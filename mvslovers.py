#!/usr/bin/env python3
'''Host side of installing the mvslovers stack with SMP during sysgen.

Pure Python, no Hercules. See documentation/MVSLOVERS_SMP_PLAN.md
("Design" and "Transport").

  fetch(product)            pinned dist from mvslovers.conf (sha256 checked)
                            or a local dist directory
  discover(dist_dir)        alloc / inst / accept JCL, xmits, FMID
  build_jobs(...)           list of Job (one per JOB card), in submit order
  build_jobstream(...)      the same as one EBCDIC byte string for the 3506
  update_manifest(...)      move the pins in mvslovers.conf to the newest
                            releases (sysgen.py --update-mvslovers)
  fetch_files(name)         pinned single release files (ZMG0001.xmit/.zip)
  zmg0001_jobs(...)         TSO integration usermod ZMG0001 (step 10)
  usermods_verify_job(...)  LIST CDS of every usermod + an EXEC test

Transport (verified in Phase 1): every .xmit travels in-stream through
IEBGENER SYSUT1 DD DATA,DLM=xx as its raw 80-byte EBCDIC records; the JCL
around it is converted to cp037 and padded to 80 columns.

CLI:
  python3 mvslovers.py ufsd -o ufsd.ebc
  python3 mvslovers.py ufsd --ascii
  python3 mvslovers.py --update [ufsd,httpd]
'''

import argparse
import configparser
import hashlib
import io
import json
import os
import platform
import re
import shutil
import struct
import sys
import tarfile
import urllib.request
import zipfile
from dataclasses import dataclass, field
from pathlib import Path

HERE = Path(__file__).resolve().parent
DEFAULT_CONF = HERE / 'mvslovers.conf'
DEFAULT_CACHE = HERE / 'temp' / 'mvslovers'

CODEPAGE = 'cp037'
CARD = 80
JCL_MAX = 71              # JCL statements must leave column 72 blank
DEFAULT_VOLUME = 'PUB000'

# Same card as sysgen.py POST_RAKF_JOB_CARD (sysgen.py runs main() on
# import, so it cannot be imported from here). USER/PASSWORD are left out
# when no user is given (before RAKF, e.g. BREXX in step 7).
JOB_CARD = ("//{JOBNAME} JOB (SYSGEN),'{DESC}',\n"
            "//             CLASS=A,\n"
            "//             MSGCLASS=A,\n"
            "//             MSGLEVEL=(1,1),\n"
            "{AUTH}"
            "//             REGION={REGION}\n")
JOB_CARD_AUTH = ("//             USER={USER},\n"
                 "//             PASSWORD={PASSWORD},\n")

DLM_CANDIDATES = (['AA'] +
                  [a + b for a in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
                   for b in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
                   if a + b != 'AA'] +
                  ['@@', '$$', '##'])


class MvsloversError(Exception):
    pass


# ---------------------------------------------------------------- manifest

def load_manifest(path=DEFAULT_CONF):
    cp = configparser.ConfigParser(interpolation=None)
    if not cp.read(path):
        raise MvsloversError('cannot read {}'.format(path))
    return {s: dict(cp[s]) for s in cp.sections()}


def dist_products(manifest):
    '''Products installed from a dist (everything but the git-pinned RAKF).'''
    return [p for p, v in manifest.items() if 'asset' in v]


def sha256_file(path):
    h = hashlib.sha256()
    with open(path, 'rb') as f:
        for chunk in iter(lambda: f.read(1 << 20), b''):
            h.update(chunk)
    return h.hexdigest()


def _download(url, dest):
    tmp = str(dest) + '.part'
    with urllib.request.urlopen(url) as r, open(tmp, 'wb') as out:
        shutil.copyfileobj(r, out)
    os.replace(tmp, dest)
    return dest


def _safe_extract(tar_path, dest):
    dest = Path(dest).resolve()
    with tarfile.open(tar_path) as tf:
        for m in tf.getmembers():
            target = (dest / m.name).resolve()
            if dest not in target.parents and target != dest:
                raise MvsloversError('{}: unsafe path {}'.format(tar_path, m.name))
            if not (m.isfile() or m.isdir()):
                raise MvsloversError('{}: unexpected member type {}'.format(tar_path, m.name))
        if hasattr(tarfile, 'data_filter'):
            tf.extractall(dest, filter='data')
        else:
            tf.extractall(dest)


def _find_dist_root(base, product):
    '''Directory under base that holds <product>-<ver>-inst.jcl.'''
    base = Path(base)
    hits = sorted({p.parent for p in base.rglob('{}-*-inst.jcl'.format(product))})
    if not hits:
        raise MvsloversError('no {}-*-inst.jcl under {}'.format(product, base))
    if len(hits) > 1:
        raise MvsloversError('more than one {} dist under {}: {}'.format(
            product, base, ', '.join(map(str, hits))))
    return hits[0]


def fetch(product, conf=DEFAULT_CONF, dist_dir=None, cache=DEFAULT_CACHE, log=print):
    '''Return the directory of the product's unpacked dist.

    dist_dir: --mvslovers-dist override -- a directory holding either the
    unpacked dist (<product>-<ver>/...) or <product>-*-dist.tar.gz.
    Otherwise the pinned asset is downloaded into cache (reused if its
    sha256 still matches) and unpacked.
    '''
    if dist_dir:
        dist_dir = Path(dist_dir)
        try:
            return _find_dist_root(dist_dir, product)
        except MvsloversError:
            tars = sorted(dist_dir.glob('{}-*-dist.tar.gz'.format(product)))
            if len(tars) != 1:
                raise MvsloversError('{}: no unpacked {} dist and {} {}-*-dist.tar.gz'.format(
                    dist_dir, product, len(tars), product))
            out = Path(cache) / 'local' / product
            shutil.rmtree(out, ignore_errors=True)
            out.mkdir(parents=True)
            _safe_extract(tars[0], out)
            return _find_dist_root(out, product)

    manifest = load_manifest(conf)
    if product not in manifest or 'asset' not in manifest[product]:
        raise MvsloversError('{} is not a dist product in {}'.format(product, conf))
    pin = manifest[product]
    cache = Path(cache)
    cache.mkdir(parents=True, exist_ok=True)
    tar_path = cache / pin['asset']
    if not tar_path.exists() or sha256_file(tar_path) != pin['sha256']:
        url = 'https://github.com/{}/releases/download/v{}/{}'.format(
            pin['repo'], pin['version'], pin['asset'])
        log('Downloading {}'.format(url))
        _download(url, tar_path)
    got = sha256_file(tar_path)
    if got != pin['sha256']:
        raise MvsloversError('{}: sha256 {} does not match the pin {} in {}'.format(
            tar_path, got, pin['sha256'], conf))
    out = cache / '{}-{}'.format(product, pin['version'])
    shutil.rmtree(out, ignore_errors=True)
    out.mkdir()
    _safe_extract(tar_path, out)
    return _find_dist_root(out, product)


# ---------------------------------------------------------------- discover

@dataclass
class Dist:
    product: str
    version: str
    path: Path
    fmid: str
    alloc: Path
    inst: Path
    accept: object            # Path or None (ufsd/httpd releases accept in inst)
    xmits: dict = field(default_factory=dict)   # CHANGE.ME suffix -> Path, inst order


_XMIT_COMMENT = re.compile(r'^//\*\s+(\S+\.xmit)\s*$')
_CHANGE_ME_DSN = re.compile(r"INDSN\('CHANGE\.ME\.([A-Z0-9@#$]{1,8})'\)")


def discover(dist_dir):
    dist_dir = Path(dist_dir)
    insts = sorted(dist_dir.glob('*-inst.jcl'))
    if len(insts) != 1:
        raise MvsloversError('{}: expected one *-inst.jcl, found {}'.format(dist_dir, len(insts)))
    inst = insts[0]
    m = re.match(r'(.+)-(\d[^-]*)-inst\.jcl$', inst.name)
    if not m:
        raise MvsloversError('cannot get product/version from {}'.format(inst.name))
    product, version = m.groups()
    stem = '{}-{}'.format(product, version)
    alloc = dist_dir / (stem + '-alloc.jcl')
    if not alloc.exists():
        raise MvsloversError('{} missing'.format(alloc))
    accept = dist_dir / (stem + '-accept.jcl')
    accept = accept if accept.exists() else None

    text = inst.read_text()
    fm = re.search(r'^\+\+FUNCTION\((\w+)\)', text, re.M)
    if not fm:
        raise MvsloversError('{}: no ++FUNCTION'.format(inst))

    xmits = {}
    last_xmit = None
    for line in text.splitlines():
        c = _XMIT_COMMENT.match(line)
        if c:
            last_xmit = c.group(1)
            continue
        if line.startswith('//*'):
            continue
        d = _CHANGE_ME_DSN.search(line)
        if d:
            if last_xmit is None:
                raise MvsloversError('{}: CHANGE.ME.{} without a preceding "//* <file>.xmit" '
                                     'comment'.format(inst.name, d.group(1)))
            xp = dist_dir / last_xmit
            if not xp.exists():
                raise MvsloversError('{} names {}, which is not in the dist'.format(inst.name, last_xmit))
            if d.group(1) in xmits:
                raise MvsloversError('{}: CHANGE.ME.{} used twice'.format(inst.name, d.group(1)))
            xmits[d.group(1)] = xp
            last_xmit = None
    unused = {p.name for p in dist_dir.glob('*.xmit')} - {p.name for p in xmits.values()}
    if unused:
        raise MvsloversError('{}: xmits not received by the inst job: {}'.format(
            dist_dir, ', '.join(sorted(unused))))
    if not xmits:
        raise MvsloversError('{}: inst job receives no xmit'.format(inst.name))
    return Dist(product, version, dist_dir, fm.group(1), alloc, inst, accept, xmits)


# ---------------------------------------------------------------- JCL helpers

def choose_dlm(records):
    '''First DLM candidate that no record starts with (compared in EBCDIC).'''
    starts = {bytes(r[:2]) for r in records}
    for c in DLM_CANDIDATES:
        if c.encode(CODEPAGE) not in starts:
            return c
    raise MvsloversError('no usable DLM: every candidate starts some record')


def read_records(path):
    data = Path(path).read_bytes()
    if not data or len(data) % CARD:
        raise MvsloversError('{}: size {} is not a multiple of {}'.format(path, len(data), CARD))
    return [data[i:i + CARD] for i in range(0, len(data), CARD)]


def job_card(jobname, desc, user=None, password=None, region='8192K'):
    auth = JOB_CARD_AUTH.format(USER=user, PASSWORD=password) if user else ''
    desc = desc.replace("'", '')[:20]
    return JOB_CARD.format(JOBNAME=jobname, DESC=desc, AUTH=auth, REGION=region)


_JOB_STMT = re.compile(r"^//([A-Z@#$][A-Z0-9@#$]{0,7})\s+JOB\b(.*)$")


def replace_job_card(jcl, user=None, password=None, region='8192K'):
    '''Swap the leading JOB statement (with its continuations) for ours.

    Only the first statement is touched: inner JOB cards (the JCLIN inside
    the SYSMOD) are in-stream data. Returns (jobname, new_jcl).
    '''
    lines = jcl.splitlines()
    if not lines:
        raise MvsloversError('empty JCL')
    m = _JOB_STMT.match(lines[0])
    if not m:
        raise MvsloversError('first card is not a JOB statement: {!r}'.format(lines[0]))
    jobname = m.group(1)
    desc = re.search(r"'([^']*)'", m.group(2))
    desc = desc.group(1) if desc else jobname
    i = 0
    while lines[i].rstrip().endswith(','):
        i += 1
        if i >= len(lines) or not lines[i].startswith('// '):
            raise MvsloversError('{}: JOB statement continuation missing'.format(jobname))
    rest = '\n'.join(lines[i + 1:])
    return jobname, job_card(jobname, desc, user, password, region) + rest + '\n'


def patch_receives(jcl, dsn_map, volume=DEFAULT_VOLUME):
    '''CHANGE.ME.<x> -> dsn_map[x]; VOLUME('CHANGE.ME') -> volume.

    A RECEIVE without a VOLUME line (ufsd/httpd 1.5.2/4.5.1) gets one, so
    placement is explicit everywhere.
    '''
    out = []
    lines = jcl.splitlines()
    for n, line in enumerate(lines):
        if line.startswith('//*'):
            out.append(line)
            continue
        d = _CHANGE_ME_DSN.search(line)
        if d:
            if d.group(1) not in dsn_map:
                raise MvsloversError('no staging dataset for CHANGE.ME.{}'.format(d.group(1)))
            line = line.replace("CHANGE.ME.{}".format(d.group(1)), dsn_map[d.group(1)])
            out.append(line)
            nxt = lines[n + 1] if n + 1 < len(lines) else ''
            if volume and 'VOLUME(' not in nxt:
                if not line.rstrip().endswith('-'):
                    raise MvsloversError('cannot add VOLUME after {!r}'.format(line))
                indent = len(line) - len(line.lstrip())
                out.append(' ' * (indent + 2) + "VOLUME('{}') -".format(volume))
            continue
        if "VOLUME('CHANGE.ME')" in line:
            if volume:
                line = re.sub(r"/\*\s*VOLUME\('CHANGE\.ME'\)\s*\*/",
                              "VOLUME('{}')".format(volume), line)
            else:
                line = None
        if line is not None:
            if 'CHANGE.ME' in line:
                raise MvsloversError('unpatched CHANGE.ME: {!r}'.format(line))
            out.append(line)
    return '\n'.join(out) + '\n'


def jcl_to_cards(jcl):
    '''ASCII JCL -> list of 80-byte cp037 cards; enforces card rules.'''
    cards = []
    for n, line in enumerate(jcl.rstrip('\n').split('\n'), 1):
        line = line.rstrip()
        if '\t' in line:
            raise MvsloversError('line {}: tab in JCL'.format(n))
        if len(line) > CARD:
            raise MvsloversError('line {}: {} columns: {!r}'.format(n, len(line), line))
        if line.startswith('//') and len(line) > JCL_MAX:
            raise MvsloversError('line {}: JCL past column {}: {!r}'.format(n, JCL_MAX, line))
        try:
            cards.append(line.ljust(CARD).encode(CODEPAGE))
        except UnicodeEncodeError as e:
            raise MvsloversError('line {}: not representable in {}: {}'.format(n, CODEPAGE, e))
    return cards


# ---------------------------------------------------------------- jobs

@dataclass
class Job:
    '''One job of the stream. parts: ('jcl', text) or ('raw', [records], label).'''
    name: str
    parts: list

    def cards(self):
        out = []
        for p in self.parts:
            out.extend(jcl_to_cards(p[1]) if p[0] == 'jcl' else p[1])
        return out

    def bytes(self):
        return b''.join(self.cards())

    def ascii(self):
        out = []
        for p in self.parts:
            if p[0] == 'jcl':
                out.append(p[1].rstrip('\n'))
            else:
                out.append('<<< {} raw records of {} >>>'.format(len(p[1]), p[2]))
        return '\n'.join(out)


def qualifier(product):
    q = re.sub(r'[^A-Z0-9@#$]', '', product.upper())[:8]
    if not q or not q[0].isalpha():
        raise MvsloversError('cannot make a DSN qualifier from {!r}'.format(product))
    return q


def staging_dsn(product, suffix, hlq='SYSGEN'):
    return '{}.{}.{}.XMIT'.format(hlq, qualifier(product), suffix)


def _short(product, tail):
    return (qualifier(product)[:8 - len(tail)] + tail)


def stage_parts(stepname, recs, label, dsn, unit='SYSDA', cond='COND=(0,NE)'):
    '''Job parts of one IEBGENER step that writes recs (raw 80-byte EBCDIC
    records, sent in-stream behind DD DATA,DLM=) to a new FB/80/3120 dsn.'''
    dlm = choose_dlm(recs)
    trk = len(recs) // 150 + 2          # 3120-byte blocks, 3330 worst case
    return [('jcl',
             '//* {} -> {}\n'
             '//{:<8}EXEC PGM=IEBGENER{}\n'
             '//SYSPRINT DD  SYSOUT=*\n'
             '//SYSIN    DD  DUMMY\n'
             '//SYSUT2   DD  DSN={},\n'
             '//             DISP=(NEW,CATLG,DELETE),\n'
             '//             UNIT={},SPACE=(TRK,({},10),RLSE),\n'
             '//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)\n'
             '//SYSUT1   DD  DATA,DLM={}\n'.format(
                 label, dsn, stepname, ',' + cond if cond else '', dsn, unit, trk, dlm)),
            ('raw', recs, label),
            ('jcl', dlm + '\n')]


def build_jobs(dist, user=None, password=None, volume=DEFAULT_VOLUME,
               hlq='SYSGEN', unit='SYSDA', region='8192K'):
    '''Jobs in submit order: XMIT staging, alloc, inst, [accept], cleanup.

    Each is a separate JOB; sysgen must wait for one to end before the
    next starts (JES2 may run same-class jobs on several initiators).
    '''
    if isinstance(dist, (str, Path)):
        dist = discover(dist)
    dsn_map = {s: staging_dsn(dist.product, s, hlq) for s in dist.xmits}
    card = lambda name, desc: job_card(name, desc, user, password, region)
    jobs = []

    # 1. stage the xmits
    xname = _short(dist.product, 'XMIT')
    parts = [('jcl', card(xname, '{} XMIT'.format(dist.product.upper())) +
              '//* Stage the {} {} xmits in-stream (DD DATA,DLM=)\n'
              '//DELOLD  EXEC PGM=IDCAMS\n'
              '//SYSPRINT DD  SYSOUT=*\n'
              '//SYSIN    DD  *\n'.format(dist.product, dist.version) +
              ''.join('  DELETE {} NONVSAM SCRATCH PURGE\n'.format(d) for d in dsn_map.values()) +
              '  SET MAXCC=0\n/*\n')]
    for n, (suffix, path) in enumerate(dist.xmits.items(), 1):
        parts.extend(stage_parts('X{}'.format(n), read_records(path), path.name,
                                 dsn_map[suffix], unit))
    parts.append(('jcl', '//\n'))
    jobs.append(Job(xname, parts))

    # 2./3. alloc, inst, accept from the dist
    for path in (dist.alloc, dist.inst, dist.accept):
        if path is None:
            continue
        name, jcl = replace_job_card(path.read_text(), user, password, region)
        jcl = patch_receives(jcl, dsn_map, volume)
        jobs.append(Job(name, [('jcl', jcl)]))

    # 4. remove the staging sets
    cname = _short(dist.product, 'CLN')
    jobs.append(Job(cname, [('jcl', card(cname, '{} CLEANUP'.format(dist.product.upper())) +
                             '//CLEANUP EXEC PGM=IDCAMS\n'
                             '//SYSPRINT DD  SYSOUT=*\n'
                             '//SYSIN    DD  *\n' +
                             ''.join('  DELETE {} NONVSAM SCRATCH PURGE\n'.format(d)
                                     for d in dsn_map.values()) +
                             '/*\n//\n')]))
    names = [j.name for j in jobs]
    if len(set(names)) != len(names):
        raise MvsloversError('duplicate job names: {}'.format(names))
    return jobs


def build_jobstream(product, user=None, password=None, conf=DEFAULT_CONF,
                    dist_dir=None, volume=DEFAULT_VOLUME, **kw):
    '''Whole install of one product as EBCDIC cards for the 3506 reader.'''
    path = fetch(product, conf=conf, dist_dir=dist_dir)
    return b''.join(j.bytes() for j in build_jobs(discover(path), user, password, volume, **kw))


# ---------------------------------------------------------------- host tools

def tool_platform():
    '''<os>-<arch> as the ufsd-utils release assets name it, or None.'''
    osname = {'linux': 'linux', 'darwin': 'darwin'}.get(sys.platform)
    arch = {'x86_64': 'amd64', 'amd64': 'amd64',
            'aarch64': 'arm64', 'arm64': 'arm64'}.get(platform.machine().lower())
    return '{}-{}'.format(osname, arch) if osname and arch else None


def fetch_tool(name='ufsd-utils', conf=DEFAULT_CONF, cache=DEFAULT_CACHE, log=print):
    '''Path of the pinned host tool binary (downloaded, sha256 checked).

    mvslovers.conf: [ufsd-utils] repo, version, sha256-<os>-<arch> per
    platform; the asset is <name>-<os>-<arch>.tar.gz holding one file,
    <name>-<os>-<arch>.'''
    pin = load_manifest(conf).get(name)
    if not pin:
        raise MvsloversError('{} is not pinned in {}'.format(name, conf))
    plat = tool_platform()
    key = 'sha256-{}'.format(plat)
    if key not in pin:
        raise MvsloversError('{} {}: no binary pinned for this host ({}); pinned: {}'.format(
            name, pin['version'], plat,
            ', '.join(k[7:] for k in pin if k.startswith('sha256-'))))
    asset = '{}-{}.tar.gz'.format(name, plat)
    cache = Path(cache)
    cache.mkdir(parents=True, exist_ok=True)
    tar_path = cache / '{}-{}-{}'.format(name, pin['version'], asset)
    if not tar_path.exists() or sha256_file(tar_path) != pin[key]:
        url = 'https://github.com/{}/releases/download/v{}/{}'.format(
            pin['repo'], pin['version'], asset)
        log('Downloading {}'.format(url))
        _download(url, tar_path)
    got = sha256_file(tar_path)
    if got != pin[key]:
        raise MvsloversError('{}: sha256 {} does not match the pin {} in {}'.format(
            tar_path, got, pin[key], conf))
    out = cache / '{}-{}-{}'.format(name, pin['version'], plat)
    shutil.rmtree(out, ignore_errors=True)
    out.mkdir()
    _safe_extract(tar_path, out)
    exe = out / '{}-{}'.format(name, plat)
    if not exe.is_file():
        raise MvsloversError('{} has no {}'.format(tar_path, exe.name))
    exe.chmod(0o755)
    return exe


def release_files(pin):
    '''Keys of a pin that name a single release file: <key> = file name
    with a <key>-sha256 next to it (e.g. xmit/xmit-sha256).'''
    return [k for k in pin if not k.endswith('-sha256') and k + '-sha256' in pin]


def fetch_files(name, conf=DEFAULT_CONF, cache=DEFAULT_CACHE, log=print):
    '''{key: Path} of the pinned single release files of section name,
    downloaded from release v<version> of <repo> into cache (reused while
    the sha256 matches) and checked against <key>-sha256.'''
    pin = load_manifest(conf).get(name)
    if not pin or not release_files(pin):
        raise MvsloversError('{} pins no release files in {}'.format(name, conf))
    cache = Path(cache)
    cache.mkdir(parents=True, exist_ok=True)
    out = {}
    for key in release_files(pin):
        path = cache / '{}-{}-{}'.format(name, pin['version'], pin[key])
        want = pin[key + '-sha256']
        if not path.exists() or sha256_file(path) != want:
            url = 'https://github.com/{}/releases/download/v{}/{}'.format(
                pin['repo'], pin['version'], pin[key])
            log('Downloading {}'.format(url))
            _download(url, path)
        got = sha256_file(path)
        if got != want:
            raise MvsloversError('{}: sha256 {} does not match the pin {} in {}'.format(
                path, got, want, conf))
        out[key] = path
    return out


# ---------------------------------------------------------------- NETDATA

def _tu(key, *values):
    '''One text unit: key, count, then length + value per item.'''
    out = struct.pack('>HH', key, len(values))
    for v in values:
        out += struct.pack('>H', len(v)) + v
    return out


def _segments(payload, flag_first, flag_last, flag_only, flag_mid):
    chunks = [payload[i:i + 253] for i in range(0, len(payload), 253)] or [b'']
    if len(chunks) == 1:
        return bytes([len(chunks[0]) + 2, flag_only]) + chunks[0]
    out = bytes([len(chunks[0]) + 2, flag_first]) + chunks[0]
    for c in chunks[1:-1]:
        out += bytes([len(c) + 2, flag_mid]) + c
    return out + bytes([len(chunks[-1]) + 2, flag_last]) + chunks[-1]


def _control(name, tus, filenum=None):
    payload = name.encode(CODEPAGE) + (struct.pack('>I', filenum) if filenum else b'') + tus
    # control records: bit 0x20 on every segment, first 0x80, last 0x40
    return _segments(payload, 0xA0, 0x60, 0xE0, 0x20)


RECFM_BITS = {'U': 0xC000, 'F': 0x8000, 'FB': 0x9000, 'V': 0x4000, 'VB': 0x5000}


def netdata_seq(blocks, dsn, recfm='U', blksize=4096, lrecl=0, node='SYSGEN',
                user='SYSGEN', when='20260101000000'):
    '''TSO TRANSMIT (NETDATA) image of one sequential dataset whose records
    are `blocks` (for RECFM=U: one block each), as 80-byte EBCDIC cards.

    RECEIVE allocates dsn with DSORG=PS and the given RECFM/BLKSIZE/LRECL
    and writes each data record as it comes -- for a UFS disk that is one
    4096-byte block per filesystem block, which is what UFSD needs.'''
    if recfm not in RECFM_BITS:
        raise MvsloversError('unsupported RECFM {}'.format(recfm))
    size = sum(len(b) for b in blocks)
    e = lambda s: s.encode(CODEPAGE)
    dsorg_ps = b'\x40\x00'
    rf = struct.pack('>H', RECFM_BITS[recfm])
    out = _control('INMR01',
                   _tu(0x0042, struct.pack('>H', CARD)) +      # INMLRECL
                   _tu(0x1011, e(node)) + _tu(0x1012, e(user)) +  # INMFNODE/INMFUID
                   _tu(0x1001, e(node)) + _tu(0x1002, e(user)) +  # INMTNODE/INMTUID
                   _tu(0x1024, e(when)) +                       # INMFTIME
                   _tu(0x102F, struct.pack('>H', 1)))           # INMNUMF
    out += _control('INMR02',
                    _tu(0x1028, e('INMCOPY')) +                  # INMUTILN
                    _tu(0x102C, struct.pack('>I', size)) +       # INMSIZE
                    _tu(0x003C, dsorg_ps) +                      # INMDSORG
                    _tu(0x0042, struct.pack('>H', lrecl)) +      # INMLRECL
                    _tu(0x0030, struct.pack('>H', blksize)) +    # INMBLKSZ
                    _tu(0x0049, rf) +                            # INMRECFM
                    _tu(0x0002, *[e(q) for q in dsn.split('.')]),  # INMDSNAM
                    filenum=1)
    out += _control('INMR03',
                    _tu(0x102C, struct.pack('>I', size)) +
                    _tu(0x003C, dsorg_ps) +
                    _tu(0x0042, struct.pack('>H', lrecl)) +
                    _tu(0x0049, rf))
    for b in blocks:
        out += _segments(b, 0x80, 0x40, 0xC0, 0x00)
    out += _control('INMR06', b'')
    if len(out) % CARD:
        out += b'\x00' * (CARD - len(out) % CARD)
    return out


def image_blocks(path, blksize):
    data = Path(path).read_bytes()
    if not data or len(data) % blksize:
        raise MvsloversError('{}: size {} is not a multiple of {}'.format(path, len(data), blksize))
    return [data[i:i + blksize] for i in range(0, len(data), blksize)]


# ---------------------------------------------------------------- UFS disks

UFS_BLKSIZE = 4096
WEBROOT_IMAGE = 'httpd-webroot.img'
WEBROOT_DSN = 'HTTPD.WEBROOT.UFS'


def home_dsn(user):
    '''UFS home disk of a user (UFSD guide: IBMUSER.UFSHOME).'''
    return '{}.UFSHOME'.format(user)


@dataclass
class UfsDisk:
    dsn: str
    path: str                 # mount point
    mode: str                 # ROOT, RO or RW
    image: Path               # host image file
    owner: object = None      # OWNER() of the MOUNT, or None
    size: object = None       # ufsd-utils create --size; None = shipped image
    img_owner: str = ''       # owner/group of the image's root directory
    img_group: str = ''


def plan_ufs_disks(rakf_users_text, admin, webroot, workdir,
                   root_size='1M', home_size='2M', scratch_size='2M'):
    '''The disks sysgen creates, in UFSDPRM0 order: root, the HTTPD webroot,
    one home per user (users.conf + sysgen --username; not the started task
    ids), scratch. Same set and sizes as the old MVP UFSD package, with the
    dataset names of the UFSD and HTTPD installation guides.

    The UFSD started task runs as RAKF user UFSD and opens every disk for
    update. Under its own HLQ (UFSD.*) RAKF allows that; HTTPD.WEBROOT.UFS
    and the homes need the DATASET profiles rakf_profiles() adds for group
    UFSROOT (without them: IEC150I 913-38). OWNER() on the MOUNT keeps a
    home writable only by its user.'''
    workdir = Path(workdir)
    disks = [UfsDisk('UFSD.ROOT', '/', 'ROOT', workdir / 'root.img', None,
                     root_size, admin, 'ADMIN'),
             UfsDisk(WEBROOT_DSN, '/www', 'RO', Path(webroot))]
    seen = set()
    for user, group, _ in parse_rakf_users(rakf_users_text):
        if user in STC_USERS or user in seen:
            continue
        seen.add(user)                      # first line = default group
        disks.append(UfsDisk(home_dsn(user), '/u/{}'.format(user.lower()), 'RW',
                             workdir / '{}.img'.format(user.lower()), user,
                             home_size, user, group))
    disks.append(UfsDisk('UFSD.SCRATCH', '/tmp', 'RW', workdir / 'scratch.img', None,
                         scratch_size, admin, 'ADMIN'))
    return disks


def build_ufs_images(disks, ufsd_utils, run=None):
    '''Create the empty images with ufsd-utils (shipped ones are checked).'''
    import subprocess
    run = run or (lambda args: subprocess.run(args, check=True, capture_output=True, text=True))
    for d in disks:
        if d.size is None:
            if not d.image.is_file():
                raise MvsloversError('{} missing (for {})'.format(d.image, d.dsn))
        else:
            d.image.parent.mkdir(parents=True, exist_ok=True)
            if d.image.exists():
                d.image.unlink()
            # always name owner/group: ufsd-utils 1.0.1 otherwise takes the
            # host's user name
            run([str(ufsd_utils), 'create', str(d.image), '--size', d.size,
                 '--blksize', str(UFS_BLKSIZE), '--owner', d.img_owner, '--group', d.img_group])
        image_blocks(d.image, UFS_BLKSIZE)       # size check
    return disks


def ufsdprm0(disks):
    lines = ['/* SYS2.PARMLIB(UFSDPRM0) - written by MVS/CE sysgen (step 12a). */',
             '/* One disk per users.conf user; add a MOUNT for each new disk.  */']
    for d in disks:
        if d.mode == 'ROOT':
            lines.append('ROOT     DSN({})'.format(d.dsn))
        else:
            line = 'MOUNT    DSN({}) PATH({}) MODE({}){}'.format(
                d.dsn, d.path, d.mode, ' OWNER({})'.format(d.owner) if d.owner else '')
            if len(line) > JCL_MAX:          # long userid: drop the alignment
                line = 'MOUNT ' + line[9:]
            lines.append(line)
    for l in lines:
        if len(l) > JCL_MAX:
            raise MvsloversError('UFSDPRM0 line too long: {!r}'.format(l))
    return '\n'.join(lines) + '\n'


def disk_jobs(disks, user=None, password=None, volume=DEFAULT_VOLUME, hlq='SYSGEN',
              unit='SYSDA', region='8192K', jobprefix='UFSDSK'):
    """One job per disk: stage its TRANSMIT image in-stream, RECEIVE it as
    RECFM=F 4096 (NJE38 RECEIVE refuses RECFM=U data whatever LRECL/BLKSIZE
    it is told: 'The incoming file contains logical records that exceed the
    LRECL of the dataset'), then IEBGENER each 4096-byte record into a
    block of the real disk: DSORG=PS, RECFM=U, BLKSIZE=4096, one contiguous
    primary extent, DISP=NEW so a disk that exists is never overwritten."""
    jobs = []
    for n, d in enumerate(disks, 1):
        name = '{}{:02d}'.format(jobprefix, n)
        stage = '{}.UFSDSK{:02d}.XMIT'.format(hlq, n)
        recv = '{}.UFSDSK{:02d}.IMG'.format(hlq, n)
        blocks = image_blocks(d.image, UFS_BLKSIZE)
        nd = netdata_seq(blocks, recv, 'F', UFS_BLKSIZE, lrecl=UFS_BLKSIZE)
        recs = [nd[i:i + CARD] for i in range(0, len(nd), CARD)]
        parts = [('jcl', job_card(name, 'UFS DISK {}'.format(n), user, password, region) +
                  '//* {} ({}) from {}\n'
                  '//DELOLD  EXEC PGM=IDCAMS\n'
                  '//SYSPRINT DD  SYSOUT=*\n'
                  '//SYSIN    DD  *\n'
                  '  DELETE {} NONVSAM SCRATCH PURGE\n'
                  '  DELETE {} NONVSAM SCRATCH PURGE\n'
                  '  SET MAXCC=0\n/*\n'.format(d.dsn, d.path, d.image.name, stage, recv))]
        parts.extend(stage_parts('STAGE', recs, d.image.name, stage, unit))
        parts.append(('jcl',
                      '//RECEIVE EXEC PGM=IKJEFT01,DYNAMNBR=20,COND=(0,NE)\n'
                      '//SYSTSPRT DD  SYSOUT=*\n'
                      '//SYSTSIN  DD  *\n'
                      "  RECEIVE INDSN('{stage}') -\n"
                      "    VOLUME('{vol}') -\n"
                      "    DATASET('{recv}')\n"
                      '/*\n'
                      '//DISK    EXEC PGM=IEBGENER,COND=(0,NE)\n'
                      '//SYSPRINT DD  SYSOUT=*\n'
                      '//SYSIN    DD  DUMMY\n'
                      '//SYSUT1   DD  DISP=SHR,DSN={recv}\n'
                      '//SYSUT2   DD  DSN={dsn},DISP=(NEW,CATLG,DELETE),\n'
                      '//             UNIT={unit},VOL=SER={vol},\n'
                      '//             SPACE=({blk},({n}),,CONTIG),\n'
                      '//             DCB=(DSORG=PS,RECFM=U,BLKSIZE={blk})\n'
                      '//CLEANUP EXEC PGM=IDCAMS,COND=EVEN\n'
                      '//SYSPRINT DD  SYSOUT=*\n'
                      '//SYSIN    DD  *\n'
                      '  DELETE {stage} NONVSAM SCRATCH PURGE\n'
                      '  DELETE {recv} NONVSAM SCRATCH PURGE\n'
                      '  SET MAXCC=0\n'
                      '/*\n//\n'.format(stage=stage, recv=recv, vol=volume, dsn=d.dsn,
                                         unit=unit, blk=UFS_BLKSIZE, n=len(blocks))))
        jobs.append(Job(name, parts))
    return jobs


# ---------------------------------------------------------------- site config

# (SAMPLIB member, target library) copied as shipped. HTTPD and HTTPPRM0 are
# edited by the BREXX step instead, UFSDPRM0 is generated from users.conf.
SITE_COPIES = [('UFSD.SAMPLIB(UFSD)', 'SYS2.PROCLIB(UFSD)'),
               ('UFSD.SAMPLIB(UFSDCLNP)', 'SYS2.PROCLIB(UFSDCLNP)'),
               ('FTPD.SAMPLIB(FTPD)', 'SYS2.PROCLIB(FTPD)'),
               ('FTPD.SAMPLIB(FTPDPRM0)', 'SYS2.PARMLIB(FTPDPRM0)')]

# Runs as PGM=BREXX. Exit 8 (step CC 8) when the shipped sample no longer
# looks the way the edits expect, so a changed sample stops the build
# instead of installing a half-edited procedure.
HTTPD_EDIT_REXX = r'''/* REXX - HTTPD proc + HTTPPRM0 for MVS/CE (sysgen step 12a) */
"EXECIO * DISKR PROCIN (STEM P. FINIS"
"EXECIO * DISKR PRMIN (STEM M. FINIS"
"EXECIO * DISKR MVSMFPRM (STEM R. FINIS"
o = 0; grp = 0; lib = 0; usr = 0
do i = 1 to p.0
  l = strip(p.i, 'T')
  if left(l, 3) <> '//*' then do
    if pos('STCUSER=HTTPD', l) > 0 then usr = usr + 1
    if pos('STCGRP=USER', l) > 0 then do
      /* group USER has no READ on FACILITY SVC244; STARTED has */
      /* not 'b'/'x' after a string: BREXX takes '...' b for a binary
         string, Error 15 */
      parse var l pre 'STCGRP=USER' post
      l = pre'STCGRP=STARTED'post
      grp = grp + 1
    end
    if left(l, 9) = '//STEPLIB' & pos('DSN=HTTPD.LINKLIB', l) > 0 then do
      /* mvsMF first, as MVSMF.SAMPLIB(MVSMFSTC) shows */
      o = o + 1; q.o = '//STEPLIB  DD  DISP=SHR,DSN=MVSMF.LINKLIB'
      l = '//         DD  DISP=SHR,DSN=HTTPD.LINKLIB'
      lib = lib + 1
    end
  end
  o = o + 1; q.o = l
end
q.0 = o
if usr <> 1 | grp <> 1 | lib <> 1 then do
  say 'MLSCFG HTTPD SAMPLE NOT AS EXPECTED: STCUSER' usr 'STCGRP' grp ,
      'STEPLIB' lib
  exit 8
end
"EXECIO" q.0 "DISKW PROCOUT (STEM Q. FINIS"
say 'MLSCFG HTTPD PROC: STCGRP=STARTED, STEPLIB MVSMF.LINKLIB'
routes = 0
do i = 1 to m.0
  if left(m.i, 10) = 'MOD=MVSMF ' then routes = routes + 1
end
o = m.0
if routes = 0 then do
  do i = 1 to r.0
    if left(r.i, 10) = 'MOD=MVSMF ' then do
      o = o + 1; m.o = r.i; routes = routes + 1
    end
  end
end
m.0 = o
if routes < 4 then do
  say 'MLSCFG HTTPPRM0 HAS' routes 'MVSMF ROUTES, EXPECTED 4'
  exit 8
end
"EXECIO" m.0 "DISKW PRMOUT (STEM M. FINIS"
say 'MLSCFG HTTPPRM0:' routes 'MVSMF ROUTES'
exit 0
'''


def site_config_job(disks, user=None, password=None, region='8192K', name='MLSCFG'):
    '''Copy the procedures and Parmlib members SMP does not own into
    SYS2.PROCLIB / SYS2.PARMLIB (each product's install guide, "Install the
    procedures and the configuration").'''
    temp = ('//             DISP=(NEW,PASS),UNIT=SYSDA,SPACE=(TRK,(2,2)),\n'
            '//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)\n')
    jcl = job_card(name, 'MVSLOVERS CONFIG', user, password, region)
    for n, (src, dst) in enumerate(SITE_COPIES, 1):
        jcl += ('//COPY{:<4}EXEC PGM=IEBGENER,COND=(0,NE)\n'
                '//SYSPRINT DD  SYSOUT=*\n'
                '//SYSIN    DD  DUMMY\n'
                '//SYSUT1   DD  DISP=SHR,DSN={}\n'
                '//SYSUT2   DD  DISP=SHR,DSN={}\n').format(n, src, dst)
    prm = ufsdprm0(disks)
    dlm = 'AA' if not any(l.startswith('AA') for l in prm.splitlines()) else '@@'
    jcl += ('//UFSDPRM EXEC PGM=IEBGENER,COND=(0,NE)\n'
            '//SYSPRINT DD  SYSOUT=*\n'
            '//SYSIN    DD  DUMMY\n'
            '//SYSUT2   DD  DISP=SHR,DSN=SYS2.PARMLIB(UFSDPRM0)\n'
            '//SYSUT1   DD  DATA,DLM={}\n'.format(dlm) + prm + dlm + '\n')
    jcl += ('//HTTPDCFG EXEC PGM=BREXX,PARM=\'RXRUN\',REGION=8192K,COND=(0,NE)\n'
            '//RXLIB    DD  DISP=SHR,DSN=BREXX.CURRENT.RXLIB\n'
            '//STDIN    DD  DUMMY\n'
            '//STDOUT   DD  SYSOUT=*\n'
            '//STDERR   DD  SYSOUT=*\n'
            '//PROCIN   DD  DISP=SHR,DSN=HTTPD.SAMPLIB(HTTPD)\n'
            '//PRMIN    DD  DISP=SHR,DSN=HTTPD.SAMPLIB(HTTPPRM0)\n'
            '//MVSMFPRM DD  DISP=SHR,DSN=MVSMF.SAMPLIB(MVSMFPRM)\n'
            '//PROCOUT  DD  DSN=&&HTTPD,\n' + temp +
            '//PRMOUT   DD  DSN=&&HTTPPRM,\n' + temp +
            '//RXRUN    DD  DATA,DLM=@@\n' + HTTPD_EDIT_REXX + '@@\n')
    for step, tmp, dst in (('HTTPPROC', '&&HTTPD', 'SYS2.PROCLIB(HTTPD)'),
                           ('HTTPPRM', '&&HTTPPRM', 'SYS2.PARMLIB(HTTPPRM0)')):
        jcl += ('//{:<8} EXEC PGM=IEBGENER,COND=(0,NE)\n'
                '//SYSPRINT DD  SYSOUT=*\n'
                '//SYSIN    DD  DUMMY\n'
                '//SYSUT1   DD  DSN={},DISP=(OLD,DELETE)\n'
                '//SYSUT2   DD  DISP=SHR,DSN={}\n').format(step, tmp, dst)
    return Job(name, [('jcl', jcl + '//\n')])


def verify_job(fmids, user=None, password=None, region='8192K', name='MLSVER'):
    '''LIST CDS / ACDS of the installed functions (check_smp_status).'''
    jcl = job_card(name, 'MVSLOVERS VERIFY', user, password, region)
    jcl += ('//SMPLIST  EXEC SMPAPP,WORK=\'SYSALLDA\'\n'
            '//SMPCNTL  DD  *\n'
            ' LIST CDS SYSMOD({}) .\n'
            ' LIST ACDS SYSMOD({}) .\n'
            '/*\n').format(' '.join(fmids), ' '.join(fmids))
    return Job(name, [('jcl', jcl + '//\n')])


# ---------------------------------------------------------------- SDSF

# sdsf docs/installation.md sections 5 and 7. TGTVOL of the dist's alloc job
# (SMPALLOC PROC TGTVOL=MVSRES): the APF entry must name the volume.
SDSF_TGTVOL = 'MVSRES'
SDSF_LINKLIB = 'ISF.LINKLIB'

# Runs as PGM=BREXX, reads the four members as they are on the system and
# writes the edited copies to temporary data sets; IEBGENER steps (COND=
# (0,NE)) put them back only if all four edits passed. Exit 8 (step CC 8)
# when a member no longer looks the way the edits expect, or already has
# the entry, so a changed system stops the build instead of being half
# edited. Lines are compared in columns 1-71; IEAAPF00 and LNKLST00 carry
# sequence numbers in 73-80. (No variable named b or x after a string:
# BREXX reads '...' b as a binary string, Error 15.)
SDSF_EDIT_REXX = r'''/* REXX - SDSF system changes for MVS/CE (sysgen step 12b) */
parse arg vol
bad = 0
"EXECIO * DISKR APFIN (STEM L. FINIS"
call addentry 2, ' ISF.LINKLIB' vol, 'IEAAPF00'
"EXECIO" l.0 "DISKW APFOUT (STEM L. FINIS"
"EXECIO * DISKR LNKIN (STEM L. FINIS"
call addentry 1, 'ISF.LINKLIB', 'LNKLST00'
"EXECIO" l.0 "DISKW LNKOUT (STEM L. FINIS"
/* SYS1.CMDPROC(ISPF): ISF.ISFPLIB at the end of the ISPPLIB ALLOC */
"EXECIO * DISKR ISPFIN (STEM L. FINIS"
hits = 0; o = 0
do i = 1 to l.0
  t = strip(left(l.i, 71), 'T')
  if pos('ISF.ISFPLIB', t) > 0 then hits = hits + 10
  if left(t, 20) = 'ALLOC F(ISPPLIB) DA(' & right(t, 6) = "') SHR" then do
    hits = hits + 1
    o = o + 1; q.o = left(t, length(t) - 5)', +'
    o = o + 1; q.o = "               'ISF.ISFPLIB') SHR"
  end
  else do
    o = o + 1; q.o = l.i
  end
end
if hits <> 1 then do
  say 'SDSFCFG SYS1.CMDPROC(ISPF) NOT AS EXPECTED: ONE LINE',
      "ALLOC F(ISPPLIB) DA(...') SHR AND NO ISF.ISFPLIB WANTED"
  bad = 1
end
q.0 = o
"EXECIO" q.0 "DISKW ISPFOUT (STEM Q. FINIS"
/* SYS1.PROCLIB(IKJACCNT): ISF.ISFCLST after SYS2.CMDPROC in SYSPROC */
"EXECIO * DISKR ACCTIN (STEM L. FINIS"
hits = 0; o = 0; prev = ''
do i = 1 to l.0
  t = strip(left(l.i, 71), 'T')
  if pos('ISF.ISFCLST', t) > 0 then hits = hits + 10
  o = o + 1; q.o = l.i
  if t = '//         DD  DISP=SHR,DSN=SYS2.CMDPROC' & ,
     left(prev, 10) = '//SYSPROC ' then do
    hits = hits + 1
    o = o + 1; q.o = '//         DD  DISP=SHR,DSN=ISF.ISFCLST'
  end
  prev = t
end
if hits <> 1 then do
  say 'SDSFCFG SYS1.PROCLIB(IKJACCNT) NOT AS EXPECTED: SYSPROC WITH',
      'SYS2.CMDPROC SECOND AND NO ISF.ISFCLST WANTED'
  bad = 1
end
q.0 = o
"EXECIO" q.0 "DISKW ACCTOUT (STEM Q. FINIS"
if bad then exit 8
say 'SDSFCFG IEAAPF00 + LNKLST00 ISF.LINKLIB, ISPF ISPPLIB, IKJACCNT SYSPROC'
exit 0

/* A list member of entries 'name[ vol],' with the last entry without a
   comma: put the comma on the last one (into the blank after it, so the
   comment stays in place) and add new as the new last entry. w is the
   word that carries the comma (2 for IEAAPF00, 1 for LNKLST00). */
addentry:
  parse arg w, new, mem
  last = 0; ok = 1
  do i = 1 to l.0
    t = strip(left(l.i, 71), 'T')
    if t = '' | left(t, 2) = '/*' then iterate
    if pos('ISF.LINKLIB', t) > 0 then ok = 0
    if last > 0 then do
      p = strip(left(l.last, 71), 'T')
      if right(word(p, w), 1) <> ',' then ok = 0
    end
    last = i
  end
  if last = 0 then ok = 0
  else do
    t = strip(left(l.last, 71), 'T')
    e = wordindex(t, w) + length(word(t, w)) - 1
    if right(word(t, w), 1) = ',' | e = 0 then ok = 0
    else if e < length(t) & substr(t, e + 1, 2) <> '  ' then ok = 0
    else if e < length(t) then l.last = left(t, e)','substr(t, e + 2)
    else l.last = t','
  end
  if \ok then do
    say 'SDSFCFG' mem 'NOT AS EXPECTED: ENTRIES name,... WITH THE LAST',
        'WITHOUT A COMMA, AND NO ISF.LINKLIB'
    bad = 1
    return
  end
  n = l.0 + 1
  do i = l.0 to last + 1 by -1
    j = i + 1; l.j = l.i
  end
  j = last + 1; l.j = new
  l.0 = n
  return
'''


def sdsf_config_job(user=None, password=None, tgtvol=SDSF_TGTVOL, region='8192K',
                    name='SDSFCFG'):
    '''IEAAPF00 and LNKLST00 entries for ISF.LINKLIB, ISF.ISFPLIB into the
    ISPPLIB ALLOC of SYS1.CMDPROC(ISPF), ISF.ISFCLST into SYSPROC of
    SYS1.PROCLIB(IKJACCNT) (sdsf installation.md sections 5 and 7). Takes
    effect at the next IPL with CLPA / logon.'''
    temp = ('//             DISP=(NEW,PASS),UNIT=SYSDA,SPACE=(TRK,(2,2)),\n'
            '//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)\n')
    members = (('APF', 'SYS1.PARMLIB(IEAAPF00)'), ('LNK', 'SYS1.PARMLIB(LNKLST00)'),
               ('ISPF', 'SYS1.CMDPROC(ISPF)'), ('ACCT', 'SYS1.PROCLIB(IKJACCNT)'))
    jcl = job_card(name, 'SDSF CONFIG', user, password, region)
    jcl += ('//EDIT     EXEC PGM=BREXX,PARM=\'RXRUN {}\',REGION=8192K\n'
            '//RXLIB    DD  DISP=SHR,DSN=BREXX.CURRENT.RXLIB\n'
            '//STDIN    DD  DUMMY\n'
            '//STDOUT   DD  SYSOUT=*\n'
            '//STDERR   DD  SYSOUT=*\n').format(tgtvol)
    for dd, dsn in members:
        jcl += '//{:<8} DD  DISP=SHR,DSN={}\n'.format(dd + 'IN', dsn)
        jcl += '//{:<8} DD  DSN=&&{},\n'.format(dd + 'OUT', dd) + temp
    jcl += '//RXRUN    DD  DATA,DLM=@@\n' + SDSF_EDIT_REXX + '@@\n'
    for dd, dsn in members:
        jcl += ('//PUT{:<5} EXEC PGM=IEBGENER,COND=(0,NE)\n'
                '//SYSPRINT DD  SYSOUT=*\n'
                '//SYSIN    DD  DUMMY\n'
                '//SYSUT1   DD  DSN=&&{},DISP=(OLD,DELETE)\n'
                '//SYSUT2   DD  DISP=SHR,DSN={}\n').format(dd, dd, dsn)
    # the result, for the log and for the next one to look at
    for n, (dd, dsn) in enumerate(members):
        jcl += ('//SHOW{:<4} EXEC PGM=IEBGENER,COND=(0,NE)\n'
                '//SYSPRINT DD  DUMMY\n'
                '//SYSIN    DD  DUMMY\n'
                '//SYSUT1   DD  DISP=SHR,DSN={}\n'
                '//SYSUT2   DD  SYSOUT=*\n').format(dd, dsn)
    return Job(name, [('jcl', jcl + '//\n')])


# SDSFPRB prints, tagged with its PARM (the job's user): every entry of the
# APF table in storage ('APF volser dsname'), whether SVC 109 code 11 has an
# ESR routine of its own (IGX00011 loaded from the LPA, same test as sdsf
# tools/isfsvct.c), and RAKF's answer to RACHECK FACILITY SDSF.OPER for READ
# and UPDATE -- the check SDSF makes before LOG and / commands
# (src/isfauth.c auth_oper). Its name must not begin with ISF.
SDSF_PROBE_ASM = r'''SDSFPRB  CSECT
         STM   14,12,12(13)
         BALR  12,0
         USING *,12
         ST    13,SAVE+4
         LA    13,SAVE
         MVC   TAG,=CL8' '
         L     1,0(,1)
         LA    1,0(,1)
         LH    2,0(,1)
         LTR   2,2
         BNP   NOPARM
         CH    2,=H'8'
         BNH   PARMLEN
         LA    2,8
PARMLEN  BCTR  2,0
         EX    2,MVCTAG
NOPARM   OPEN  (PRINT,OUTPUT)
*        APF TABLE: HALFWORD COUNT, THEN AL1(LEN) CL6'VOLSER' DSNAME
         L     2,16
         USING CVT,2
         L     3,CVTAUTHL
         DROP  2
         LA    3,0(,3)
         LH    4,0(,3)
         LA    3,2(,3)
         LTR   4,4
         BNP   APFBAD
         CH    4,=H'200'
         BH    APFBAD
APFLOOP  SR    5,5
         IC    5,0(,3)
         CH    5,=H'7'
         BL    APFBAD
         CH    5,=H'50'
         BH    APFBAD
         BAL   14,CLEAR
         MVC   LINE+17(4),=C'APF '
         MVC   LINE+21(6),1(3)
         LR    6,5
         SH    6,=H'7'
         EX    6,MVCDSN
         BAL   14,WRITE
         LA    3,1(5,3)
         BCT   4,APFLOOP
         B     ESR
APFBAD   BAL   14,CLEAR
         MVC   LINE+17(15),=C'APF TABLE ERROR'
         BAL   14,WRITE
*        ESR TABLE OF SVC 109: CVT -> SCVT -> SVC TABLE -> ENTRY 109
ESR      L     2,16
         L     3,X'C8'(,2)
         LA    3,0(,3)
         L     3,X'84'(,3)
         LA    3,0(,3)
         L     3,872(,3)
         LA    3,0(,3)
         BAL   14,CLEAR
         MVC   LINE+17(9),=C'ESR11 OFF'
         LTR   3,3
         BZ    ESRPUT
         L     4,0(,3)
         LA    4,0(,4)
         L     5,88(,3)
         LA    5,0(,5)
         CR    4,5
         BE    ESRPUT
         MVC   LINE+17(9),=C'ESR11 ON '
ESRPUT   BAL   14,WRITE
*        RAKF: FACILITY SDSF.OPER
         RACHECK ENTITY=OPERPRF,CLASS='FACILITY',ATTR=READ
         ST    15,RCREAD
         RACHECK ENTITY=OPERPRF,CLASS='FACILITY',ATTR=UPDATE
         ST    15,RCUPD
         BAL   14,CLEAR
         MVC   LINE+17(15),=C'SDSF.OPER READ='
         L     15,RCREAD
         CVD   15,DW
         OI    DW+7,X'0F'
         UNPK  LINE+32(2),DW+6(2)
         MVC   LINE+34(8),=C' UPDATE='
         L     15,RCUPD
         CVD   15,DW
         OI    DW+7,X'0F'
         UNPK  LINE+42(2),DW+6(2)
         BAL   14,WRITE
         CLOSE PRINT
         L     13,SAVE+4
         LM    14,12,12(13)
         SR    15,15
         BR    14
CLEAR    MVI   LINE,C' '
         MVC   LINE+1(79),LINE
         MVC   LINE(8),=C'SDSFPRB '
         MVC   LINE+8(8),TAG
         BR    14
WRITE    ST    14,WSAVE
         PUT   PRINT,LINE
         L     14,WSAVE
         BR    14
MVCTAG   MVC   TAG(0),2(1)
MVCDSN   MVC   LINE+28(0),7(3)
SAVE     DS    18F
WSAVE    DS    F
RCREAD   DS    F
RCUPD    DS    F
DW       DS    D
TAG      DS    CL8
LINE     DS    CL80
OPERPRF  DC    CL39'SDSF.OPER'
PRINT    DCB   DDNAME=SYSPRINT,DSORG=PS,MACRF=PM
         LTORG
         CVT   DSECT=YES
         END   SDSFPRB
'''


def sdsf_probe_steps(tag):
    '''Assemble, link and run SDSFPRB with PARM=tag (job steps).'''
    if not re.match(r'^[A-Z0-9@#$]{1,8}$', tag):
        raise MvsloversError('bad SDSFPRB tag {!r}'.format(tag))
    return ('//ASM      EXEC PGM=IFOX00,PARM=\'LOAD,NODECK\',REGION=1024K\n'
            '//SYSPRINT DD  SYSOUT=*\n'
            '//SYSUT1   DD  UNIT=SYSDA,SPACE=(1700,(600,100))\n'
            '//SYSUT2   DD  UNIT=SYSDA,SPACE=(1700,(300,50))\n'
            '//SYSUT3   DD  UNIT=SYSDA,SPACE=(1700,(300,50))\n'
            '//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB,DCB=BLKSIZE=32720\n'
            '//         DD  DISP=SHR,DSN=SYS1.AMODGEN\n'
            '//SYSGO    DD  DSN=&&OBJ,DISP=(NEW,PASS),UNIT=SYSDA,\n'
            '//             SPACE=(TRK,(5,5)),DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)\n'
            '//SYSIN    DD  DATA,DLM=@@\n' + SDSF_PROBE_ASM + '@@\n'
            '//LKED     EXEC PGM=IEWL,PARM=\'LIST,MAP\',COND=(0,NE,ASM)\n'
            '//SYSPRINT DD  SYSOUT=*\n'
            '//SYSUT1   DD  UNIT=SYSDA,SPACE=(TRK,(10,10))\n'
            '//SYSLIN   DD  DSN=&&OBJ,DISP=(OLD,DELETE)\n'
            '//SYSLMOD  DD  DSN=&&LOAD(SDSFPRB),DISP=(NEW,PASS),UNIT=SYSDA,\n'
            '//             SPACE=(TRK,(5,5,2))\n'
            '//GO       EXEC PGM=SDSFPRB,PARM=\'{}\',\n'
            '//             COND=((0,NE,ASM),(0,NE,LKED))\n'
            '//STEPLIB  DD  DSN=&&LOAD,DISP=(OLD,DELETE)\n'
            '//SYSPRINT DD  SYSOUT=*,DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)\n').format(tag)


def sdsf_verify_job(fmid, user=None, password=None, region='8192K', name='SDSFVER'):
    '''After the IPL with CLPA: LIST CDS/ACDS, the members of the target
    libraries, ISFISP from the link list (no STEPLIB; without ISPF it ends
    with ISF001E and RC 8, a S806 means it is not in the link list), and
    SDSFPRB as this job's user.'''
    jcl = job_card(name, 'SDSF VERIFY', user, password, region)
    jcl += ('//SMPLIST  EXEC SMPAPP,WORK=\'SYSALLDA\'\n'
            '//SMPCNTL  DD  *\n'
            ' LIST CDS SYSMOD({0}) .\n'
            ' LIST ACDS SYSMOD({0}) .\n'
            '/*\n'
            '//LISTDS   EXEC PGM=IKJEFT01\n'
            '//SYSTSPRT DD  SYSOUT=*\n'
            '//SYSTSIN  DD  *\n'
            "  LISTDS 'ISF.LINKLIB' MEMBERS\n"
            "  LISTDS 'ISF.ISFPLIB' MEMBERS\n"
            "  LISTDS 'ISF.ISFCLST' MEMBERS\n"
            '/*\n'
            '//RUNISF   EXEC PGM=ISFISP\n'
            '//STDOUT   DD  SYSOUT=*\n'
            '//STDERR   DD  SYSOUT=*\n'
            '//SYSPRINT DD  SYSOUT=*\n'
            '//SYSTERM  DD  SYSOUT=*\n').format(fmid)
    jcl += sdsf_probe_steps((user or 'NOUSER')[:8])
    return Job(name, [('jcl', jcl + '//\n')])


def sdsf_probe_job(user, password, region='8192K', name='SDSFPRB'):
    '''SDSFPRB alone, as another user (one without SDSF.OPER).'''
    jcl = job_card(name, 'SDSF PROBE', user, password, region)
    return Job(name, [('jcl', jcl + sdsf_probe_steps(user[:8]) + '//\n')])


# Condition codes of the SDSF jobs other than 0000 (check_maxcc; for an
# EXEC SMPAPP step IEF142I says 'HMASMP APPLY', the step name is APPLY).
# installation.md section 3: the APPLY and the ACCEPT end with 4 when
# IGX00011 is first linked into SYS1.LPALIB / SYS1.AOST4 (IEW0342,
# HMA2471). RUNISF: ISF001E without ISPF.
SDSF_STEPS_CC = {'SDSFINS': {'APPLY': '0004'},
                 'SDSFACC': {'ACCEPT': '0004'},
                 'SDSFVER': {'RUNISF': '0008'}}


# ---------------------------------------------------------------- ZMG0001
#
# BREXX/370 TSO integration (brexx370 guide, "The TSO Integration:
# ZMG0001"): EXEC and the implicit call of a name run REXX through the
# BREXX in the link list. The release carries the library USERMOD.ZMG0001
# as ZMG0001.xmit and the same members as ZMG0001.zip; the jobs assemble
# IKJCT430/IKJCT437 on the target. ZMG0001 changes EXEC in SYS1.CMDLIB.

ZMG_LIBRARY = 'USERMOD.ZMG0001'
ZMG_MEMBERS = ('@CVRLET', 'IKJCT430', 'IKJCT437', 'ZMG01CK', 'ZMG01BK',
               '$INSTALL', 'ZMG01AP', 'ZMG01RS')
ZMG_STAGE = 'SYSGEN.ZMG0001.XMIT'
# Shipped jobs sysgen submits, in order (zip member -> job name). ZMG01BK
# (IEBCOPY backup of EXEC/EX) is left out: sysgen accepts UY16532 first,
# so ZMG01RS's RESTORE removes the usermod (the guide's "Removal").
ZMG_JOBS = (('ZMG01CK.jcl', 'ZMG01CK'), ('$INSTALL.jcl', 'ZMG01IN'),
            ('ZMG01AP.jcl', 'ZMG01AP'))
# Condition codes other than 0000 (check_maxcc keys EXEC SMPAPP steps by
# the job step name):
#   ZMGPREP ACCEPT  4  the accept relinks IKJCT430/431 with NCAL (guide)
#   ZMG01CK LIST    4  ZMG0001/ZMG0002/ZMG0003 not found (expected)
#   ZMG01IN REJ    12  HMA2462, ZMG0001 not on SMPPTS on the first run
ZMG_STEPS_CC = {'ZMGPREP': {'ACCEPT': '0004'},
                'ZMG01CK': {'LIST': '0004'},
                'ZMG01IN': {'REJ': '0012'}}


def zmg0001_jobs(files, user=None, password=None, volume=DEFAULT_VOLUME,
                 region='8192K'):
    '''Jobs in submit order, one at a time (guide, "Installation"):

      ZMGXMIT  ZMG0001.xmit in-stream -> SYSGEN.ZMG0001.XMIT, RECEIVE it
               into USERMOD.ZMG0001, LISTDS its members, delete the stage
      ZMGPREP  compress SYS1.CMDLIB (one extent, ~93 % full on MVS/CE; the
               apply writes EXEC behind the last member), then ACCEPT
               UY16532 (applied, not accepted on MVS/CE) so that RESTORE
               can take ZMG0001 off again
      ZMG01CK  shipped: LIST UY16532 ZMG0001 ZMG0002 ZMG0003
      ZMG01IN  shipped $INSTALL: assemble, REJECT, RECEIVE, APPLY CHECK
      ZMG01AP  shipped: assemble again, APPLY (not accepted: cover letter)

    files: fetch_files('zmg0001') -> {'xmit': Path, 'zip': Path}.
    '''
    card = lambda name, desc: job_card(name, desc, user, password, region)
    jobs = []
    vol = "    VOLUME('{}') -\n".format(volume) if volume else ''

    parts = [('jcl', card('ZMGXMIT', 'ZMG0001 XMIT') +
              '//* ZMG0001.xmit -> {lib}\n'
              '//DELOLD  EXEC PGM=IDCAMS\n'
              '//SYSPRINT DD  SYSOUT=*\n'
              '//SYSIN    DD  *\n'
              '  DELETE {stage} NONVSAM SCRATCH PURGE\n'
              '  DELETE {lib} NONVSAM SCRATCH PURGE\n'
              '  SET MAXCC=0\n/*\n'.format(lib=ZMG_LIBRARY, stage=ZMG_STAGE))]
    parts.extend(stage_parts('X1', read_records(files['xmit']), Path(files['xmit']).name,
                             ZMG_STAGE))
    parts.append(('jcl',
                  '//RECV    EXEC PGM=IKJEFT01,DYNAMNBR=20,COND=(0,NE)\n'
                  '//SYSTSPRT DD  SYSOUT=*\n'
                  '//SYSTSIN  DD  *\n'
                  "  RECEIVE INDSN('{stage}') -\n"
                  '{vol}'
                  "    DATASET('{lib}')\n"
                  '/*\n'
                  '//LIST    EXEC PGM=IKJEFT01,COND=(0,NE)\n'
                  '//SYSTSPRT DD  SYSOUT=*\n'
                  '//SYSTSIN  DD  *\n'
                  "  LISTDS '{lib}' MEMBERS\n"
                  '/*\n'
                  '//CLEANUP EXEC PGM=IDCAMS\n'
                  '//SYSPRINT DD  SYSOUT=*\n'
                  '//SYSIN    DD  *\n'
                  '  DELETE {stage} NONVSAM SCRATCH PURGE\n'
                  '/*\n//\n'.format(stage=ZMG_STAGE, lib=ZMG_LIBRARY, vol=vol)))
    jobs.append(Job('ZMGXMIT', parts))

    jobs.append(Job('ZMGPREP', [('jcl', card('ZMGPREP', 'ZMG0001 PREREQ') +
        '//* Compress SYS1.CMDLIB in place (guide: nobody logged on to TSO)\n'
        '//COMPRESS EXEC PGM=IEBCOPY,REGION=4096K\n'
        '//SYSPRINT DD  SYSOUT=*\n'
        '//SYSUT3   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))\n'
        '//SYSUT4   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))\n'
        '//CMD      DD  DSN=SYS1.CMDLIB,DISP=OLD\n'
        '//SYSIN    DD  *\n'
        '  COPY INDD=CMD,OUTDD=CMD\n'
        '/*\n'
        '//* Accept UY16532 (IKJCT430/431 into SYS1.AOST4): RESTORE of\n'
        '//* ZMG0001 needs it accepted (HMA3022 ... UY16532 PRE otherwise)\n'
        '//ACCEPT   EXEC SMPAPP,COND=(0,NE,COMPRESS)\n'
        '//HMASMP.CMDLIB   DD DSN=SYS1.CMDLIB,DISP=SHR\n'
        '//HMASMP.AOST4    DD DSN=SYS1.AOST4,DISP=SHR\n'
        '//HMASMP.SMPCNTL  DD *\n'
        ' ACCEPT SELECT(UY16532) .\n'
        '/*\n//\n')]))

    with zipfile.ZipFile(files['zip']) as z:
        names = set(z.namelist())
        for member, want in ZMG_JOBS:
            if member not in names:
                raise MvsloversError('{} has no {}'.format(files['zip'], member))
            text = z.read(member).decode('ascii')
            name, jcl = replace_job_card(text, user, password, region)
            if name != want:
                raise MvsloversError('{}: job {} expected, found {}'.format(member, want, name))
            if 'YOUR.' in jcl:
                raise MvsloversError('{}: unpatched YOUR. data set name'.format(member))
            jobs.append(Job(name, [('jcl', jcl)]))
    return jobs


_ZMG_LINK = re.compile(r'HMA2390\s+LINK SUCCESSFUL - MOD=(\w+) - LMOD=(\w+)\b.*SYSMOD=ZMG0001')


def zmg_links(text):
    '''{module: load module} that ZMG01AP's APPLY linked (HMA2390).'''
    return {m.group(1): m.group(2) for m in _ZMG_LINK.finditer(text)}


# ---------------------------------------------------------------- usermods

_USERMOD_MCS = re.compile(r'^\+\+\s*USERMOD\s*\(\s*([A-Z0-9@#$]{1,7})\s*\)', re.M)


def usermod_ids(path):
    '''++USERMOD ids in a JCL file (ASCII, or EBCDIC cards like ZP60025).'''
    data = Path(path).read_bytes()
    if b'++' in data or b'//' in data[:2]:
        text = data.decode('latin-1')
    else:
        text = '\n'.join(data[i:i + CARD].decode(CODEPAGE) for i in range(0, len(data), CARD))
    return _USERMOD_MCS.findall(text)


def parse_smp_list(text, zone='SMPCDS'):
    '''{sysmod: {'type', 'status', 'lines'}} of the SYSMOD entries of LIST
    CDS (zone SMPCDS) or LIST ACDS (SMPACDS); the last LIST wins. status is
    None for an entry without one (TYPE = SUPERSEDED).'''
    out = {}
    cur = None
    in_zone = False
    for line in text.splitlines():
        x = line.split()
        if x[:3] in (['SMPCDS', 'SYSMOD', 'ENTRIES'], ['SMPACDS', 'SYSMOD', 'ENTRIES']):
            # a page header repeats this line, also inside an entry (TYPE on
            # one page, STATUS on the next): keep the entry in that case
            if x[0] != zone or not in_zone:
                cur = None
            in_zone = x[0] == zone
            continue
        if not in_zone:
            continue
        if x[:2] == ['HMA2050', 'LIST'] or (x and x[0] == 'LIST' and 'SUMMARY' in x):
            in_zone = False
            cur = None
            continue
        if len(x) >= 3 and x[1] == 'TYPE' and x[2] == '=':
            cur = x[0]
            # the type is one word; a page header may follow on the line
            out[cur] = {'type': x[3] if len(x) > 3 else '', 'status': None, 'lines': [line]}
            continue
        if cur:
            out[cur]['lines'].append(line)
            if x[:2] == ['STATUS', '=']:
                out[cur]['status'] = ' '.join(x[2:])
    return out


ZMG_TEST_EXECS = {
    # line 1 is a REXX comment: REXX from SYSEXEC and with EXEC 'ds(m)'
    'ZMGTST': "/* REXX - sysgen step 10: ZMG0001 test */\n"
              "SAY 'ZMGTEST' ARG(1) VERSION()\n",
    # no REXX comment: REXX only with the EXEC keyword
    'ZMGNOC': "SAY 'ZMGTEST' ARG(1) VERSION()\n",
    # a CLIST still runs as one
    'ZMGCLST': "PROC 0\nSET A = CLIST\nWRITE ZMGTEST &A RAN\n",
}
ZMG_TEST_DSN = 'SYSGEN.ZMGTEST.EXEC'
# (TSO command, expected first word after ZMGTEST)
ZMG_TEST_CMDS = (("%ZMGTST IMPLICIT", 'IMPLICIT'),
                 ("EXEC '{}(ZMGTST)' 'EXPLICIT'".format(ZMG_TEST_DSN), 'EXPLICIT'),
                 ("EXEC '{}(ZMGNOC)' 'KEYWORD' EXEC".format(ZMG_TEST_DSN), 'KEYWORD'),
                 ("EXEC '{}(ZMGCLST)'".format(ZMG_TEST_DSN), 'CLIST'))


def _smp_operands(names, width=50):
    '''SYSMOD(...) operand wrapped onto SMP control statement lines
    (' LIST ACDS SYSMOD(' + width + ',' stays inside column 72).'''
    lines, cur = [], ''
    for n in names:
        if cur and len(cur) + len(n) + 1 > width:
            lines.append(cur + ',')
            cur = ''
        cur = n if not cur else cur + ',' + n
    lines.append(cur)
    return '\n   '.join(lines)


def usermods_verify_job(sysmods, user=None, password=None, region='8192K',
                        name='UMODVER', acds=('UY16532',)):
    '''LIST CDS SYSMOD of every usermod (+ LIST ACDS of acds), the extents
    of SYS1.CMDLIB, and the EXEC test of ZMG0001 in a batch TMP: execs in
    a cataloged library (EXEC 'ds(m)' needs one) that is also SYSEXEC.'''
    jcl = job_card(name, 'USERMOD VERIFY', user, password, region)
    jcl += ('//SMPLIST  EXEC SMPAPP,WORK=\'SYSALLDA\'\n'
            '//SMPCNTL  DD  *\n'
            ' LIST CDS SYSMOD({}) .\n'
            ' LIST ACDS SYSMOD({}) .\n'
            '/*\n'.format(_smp_operands(sysmods), _smp_operands(acds)))
    jcl += ('//CMDLIB   EXEC PGM=IEHLIST\n'
            '//SYSPRINT DD  SYSOUT=*\n'
            '//DD1      DD  UNIT=3350,VOL=SER=MVSRES,DISP=OLD\n'
            '//SYSIN    DD  *\n'
            '  LISTVTOC FORMAT,VOL=3350=MVSRES,DSNAME=(SYS1.CMDLIB)\n'
            '/*\n')
    jcl += ('//DELOLD   EXEC PGM=IDCAMS\n'
            '//SYSPRINT DD  SYSOUT=*\n'
            '//SYSIN    DD  *\n'
            '  DELETE {0} NONVSAM SCRATCH PURGE\n'
            '  SET MAXCC=0\n'
            '/*\n'
            '//EXECS    EXEC PGM=IEBUPDTE,PARM=NEW\n'
            '//SYSPRINT DD  SYSOUT=*\n'
            '//SYSUT2   DD  DSN={0},DISP=(NEW,CATLG,DELETE),\n'
            '//             UNIT=SYSDA,SPACE=(TRK,(2,1,2)),\n'
            '//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)\n'
            '//SYSIN    DD  DATA,DLM=@@\n'.format(ZMG_TEST_DSN))
    for member, text in ZMG_TEST_EXECS.items():
        jcl += './ ADD NAME={}\n{}'.format(member, text)
    jcl += ('@@\n'
            '//EXECTEST EXEC PGM=IKJEFT01,DYNAMNBR=20,COND=(0,NE,EXECS)\n'
            '//SYSTSPRT DD  SYSOUT=*\n'
            '//SYSEXEC  DD  DSN={0},DISP=SHR\n'
            '//SYSPROC  DD  DSN=SYS1.CMDPROC,DISP=SHR\n'
            '//SYSTSIN  DD  *\n'.format(ZMG_TEST_DSN))
    jcl += ''.join('  {}\n'.format(c) for c, _ in ZMG_TEST_CMDS)
    jcl += ('/*\n'
            '//CLEANUP  EXEC PGM=IDCAMS\n'
            '//SYSPRINT DD  SYSOUT=*\n'
            '//SYSIN    DD  *\n'
            '  DELETE {} NONVSAM SCRATCH PURGE\n'
            '/*\n//\n'.format(ZMG_TEST_DSN))
    return Job(name, [('jcl', jcl)])


_ZMG_OUT = re.compile(r'\bZMGTEST (\S+) (\S+)')


def parse_zmg_test(text):
    '''{first word after ZMGTEST: second word} of the EXECTEST output
    (REXX: arg + VERSION(), the CLIST: CLIST RAN). Lines that echo the
    source or the commands do not match: they quote ZMGTEST or show &A.'''
    return {m.group(1): m.group(2) for m in _ZMG_OUT.finditer(text)}


def check_usermods(entries, sysmods, exact=None, superseded=()):
    '''Problems (list of str) with the LIST CDS entries of sysmods:
    missing, not applied, or not the status in exact {sysmod: status}.
    A sysmod in superseded may be TYPE = SUPERSEDED instead.'''
    exact = exact or {}
    bad = []
    for s in sysmods:
        e = entries.get(s)
        if e is None:
            bad.append('{} not in LIST CDS'.format(s))
        elif e['status'] is None:
            if not (s in superseded and e['type'] == 'SUPERSEDED'):
                bad.append('{} is {} (no STATUS)'.format(s, e['type']))
        elif s in exact:
            if e['status'] != exact[s]:
                bad.append('{} is {}, expected {}'.format(s, e['status'], exact[s]))
        elif e['status'] not in ('REC APP', 'REC APP ACC'):
            bad.append('{} is {}, not applied'.format(s, e['status']))
    return bad


def parse_sdsf_probe(text):
    '''{tag: {'apf': [(vol, dsn)], 'esr11': bool|None, 'read': rc, 'update': rc}}
    from SDSFPRB output; a later run of the same tag wins.'''
    out = {}
    for line in text.splitlines():
        m = re.search(r'SDSFPRB (\S+)\s+(APF TABLE ERROR|APF (\S{6}) (\S+)|ESR11 (ON|OFF)|'
                      r'SDSF\.OPER READ=(\d+) UPDATE=(\d+))', line)
        if not m:
            continue
        tag = m.group(1)
        if m.group(2) == 'APF TABLE ERROR':
            r = out[tag] = {'apf': [], 'esr11': None, 'read': None, 'update': None,
                            'apf_error': True}
            continue
        if m.group(3):
            r = out.get(tag)
            if r is None or r.get('done'):
                r = out[tag] = {'apf': [], 'esr11': None, 'read': None, 'update': None}
            r['apf'].append((m.group(3), m.group(4)))
        elif m.group(5):
            out.setdefault(tag, {'apf': [], 'esr11': None, 'read': None, 'update': None})
            out[tag]['esr11'] = m.group(5) == 'ON'
        else:
            out.setdefault(tag, {'apf': [], 'esr11': None, 'read': None, 'update': None})
            out[tag]['read'] = int(m.group(6))
            out[tag]['update'] = int(m.group(7))
            out[tag]['done'] = True
    return out


# ------------------------------------------------------------------ RAKF users

# Started tasks that RAKF 2.0.0 maps by procedure name to a USERS entry, and
# their group (RAKF grants access to groups, never to a user). Operations N.
#   STARTED  FTPD, HTTPD: only READ on FACILITY SVC244 (profiles.conf)
#   UFSROOT  UFSD: SVC244 READ plus UPDATE on the UFS disk datasets that are
#            not under its own HLQ (rakf_profiles); nothing else has that
# No password: generate_release.py writes no shadow record for a blank
# password, so nobody can log on as these ids (they RACINIT without one).
STC_USERS = {'UFSD': 'UFSROOT', 'FTPD': 'STARTED', 'HTTPD': 'STARTED'}
UFS_GROUP = 'UFSROOT'


def _rakf_line(user, group, first, password, oper, special):
    # cols 1-8 userid, 10-17 group, 18 '*' on the first of several entries,
    # 19-26 password (hashed into SYS1.SECURE.SHADOW, then blanked),
    # 28 Operations, 30 Special (RAKF 2.0.0: needed for ADDUSER/LISTUSER/...)
    return '{:8.8} {:8.8}{}{:8.8} {} {}'.format(
        user, group, '*' if first else ' ', password, oper, special).rstrip()


def rakf_users(users_text, extra_admin=None, stc_users=STC_USERS):
    '''users.conf text -> RAKF 2.0.0 users.txt text (input of generate_release.py -u).

       users.conf: user password logonproc OPER|NOOPER acct JCL mount ADMIN|NOADMIN.
       An ADMIN gets ADMIN + RAKFADM entries and Special Y; others group USER.
       extra_admin: (user, password) of sysgen's -u/-p admin, if any.'''
    out, seen = [], set()

    def add(user, password, oper, admin):
        user, password = user.upper(), password.upper()
        if len(user) > 7 or len(password) > 8:
            raise MvsloversError('RAKF user {}: userid max 7, password max 8'.format(user))
        if user in seen:
            raise MvsloversError('RAKF user {} defined twice'.format(user))
        seen.add(user)
        if admin:
            out.append(_rakf_line(user, 'ADMIN', True, password, oper, 'Y'))
            out.append(_rakf_line(user, 'RAKFADM', False, password, oper, 'Y'))
        else:
            out.append(_rakf_line(user, 'USER', True, password, oper, 'N'))

    for line in users_text.splitlines():
        if not line.strip() or line[0] in '#*':
            continue
        f = line.split()
        add(f[0], f[1], 'Y' if len(f) > 3 and f[3].upper() == 'OPER' else 'N',
            len(f) > 7 and f[7].upper() == 'ADMIN')
    if extra_admin and extra_admin[0]:
        add(extra_admin[0], extra_admin[1], 'Y', True)
    for stc in stc_users:
        if stc in seen:
            raise MvsloversError('users.conf must not define started task id ' + stc)
        out.append(_rakf_line(stc, stc_users[stc], True, '', 'N', 'N'))
    return '\n'.join(out) + '\n'


def _profile_line(cls, resource, group, access):
    # cols 1-8 class, 9-52 resource, 53-60 group (blank: universal), 61 access
    if len(resource) > 44:
        raise MvsloversError('RAKF resource too long: ' + resource)
    return '{:8.8}{:44}{:8.8}{}'.format(cls, resource, group, access)


def rakf_profiles(profiles_text, users_text):
    '''profiles.conf + the DATASET profiles of the UFS disks named after the
    users in users_text (rakf_users() output): universal READ, as DATASET *
    gives, and UPDATE for UFSROOT, for WEBROOT_DSN and each home_dsn().
    Sorted in EBCDIC order like profiles.conf; lines already there are kept
    once.'''
    lines = [l.rstrip() for l in profiles_text.splitlines() if l.strip()]
    users = []
    for user, _, _ in parse_rakf_users(users_text):
        if user not in STC_USERS and user not in users:
            users.append(user)
    for dsn in [WEBROOT_DSN] + [home_dsn(u) for u in users]:
        lines.append(_profile_line('DATASET', dsn, '', 'READ'))
        lines.append(_profile_line('DATASET', dsn, UFS_GROUP, 'UPDATE'))
    out = []
    for l in sorted(set(lines), key=lambda l: l.encode(CODEPAGE)):
        out.append(l)
    return '\n'.join(out) + '\n'


def parse_rakf_users(text):
    '''RAKF 2.0.0 users.txt text (as rakf_users() writes it) -> list of
       (user, group, password) by column, one per line. Started task ids
       have a blank password.'''
    out = []
    for line in text.splitlines():
        if not line.strip():
            continue
        out.append((line[0:8].strip(), line[9:17].strip(), line[18:26].strip()))
    return out


# ---------------------------------------------------------------- update pins

def _api(url, opener):
    with opener(url) as r:
        return json.load(r)


def _release_sums(assets, opener):
    '''{file name: sha256} from the release's SHA256SUMS ({} if none).'''
    sums = {}
    if 'SHA256SUMS' in assets:
        with opener(assets['SHA256SUMS']) as r:
            for line in r.read().decode().splitlines():
                parts = line.split()
                if len(parts) == 2:
                    sums[parts[1].lstrip('*')] = parts[0].lower()
    return sums


def _hash_url(url, opener):
    h = hashlib.sha256()
    with opener(url) as r:
        for chunk in iter(lambda: r.read(1 << 20), b''):
            h.update(chunk)
    return h.hexdigest()


def update_manifest(conf=DEFAULT_CONF, products=None, opener=urllib.request.urlopen,
                    log=print):
    '''Move pins to the newest releases; rewrite conf in place keeping
    comments/order. Returns [(product, old, new)] for changed products.'''
    manifest = load_manifest(conf)
    if products:
        bad = [p for p in products if p not in manifest]
        if bad:
            raise MvsloversError('unknown product(s): {} (known: {})'.format(
                ', '.join(bad), ', '.join(manifest)))
    else:
        products = list(manifest)

    new = {}
    for p in products:
        pin = manifest[p]
        rel = _api('https://api.github.com/repos/{}/releases/latest'.format(pin['repo']), opener)
        tag = rel['tag_name']
        if 'ref' in pin:
            new[p] = ({'ref': tag}, pin['ref'], tag, None)
            continue
        version = tag[1:] if tag.startswith('v') else tag
        assets = {a['name']: a['browser_download_url'] for a in rel.get('assets', [])}
        plats = [k[len('sha256-'):] for k in pin if k.startswith('sha256-')]
        if plats:
            # host tool: one binary tarball per pinned platform
            if version == pin['version']:
                new[p] = ({}, pin['version'], version, None)
                continue
            vals = {'version': version}
            for plat in plats:
                name = '{}-{}.tar.gz'.format(p, plat)
                if name not in assets:
                    raise MvsloversError('{} {}: no asset {}'.format(pin['repo'], tag, name))
                h = hashlib.sha256()
                with opener(assets[name]) as r:
                    for chunk in iter(lambda: r.read(1 << 20), b''):
                        h.update(chunk)
                vals['sha256-' + plat] = h.hexdigest()
            new[p] = (vals, pin['version'], version,
                      "# computed: not listed in the release's SHA256SUMS")
            continue
        files = release_files(pin)
        if files:
            # single release files (ZMG0001.xmit/.zip), same names each release
            if version == pin['version']:
                new[p] = ({}, pin['version'], version, None)
                continue
            sums = _release_sums(assets, opener)
            vals = {'version': version}
            note = None
            for key in files:
                fname = pin[key]
                if fname not in assets:
                    raise MvsloversError('{} {}: no asset {}'.format(pin['repo'], tag, fname))
                if fname in sums:
                    vals[key + '-sha256'] = sums[fname]
                else:
                    log('WARNING: {} SHA256SUMS does not list {}; hashing the download'.format(p, fname))
                    vals[key + '-sha256'] = _hash_url(assets[fname], opener)
                    note = "# computed: not listed in the release's SHA256SUMS"
            new[p] = (vals, pin['version'], version, note)
            continue
        dist = [a for a in assets if a.endswith('-dist.tar.gz')]
        if len(dist) != 1:
            raise MvsloversError('{} {}: expected one *-dist.tar.gz, found {}'.format(
                pin['repo'], tag, dist))
        asset = dist[0]
        if version == pin['version'] and asset == pin['asset']:
            new[p] = ({}, pin['version'], version, None)
            continue
        sums = {}
        if 'SHA256SUMS' in assets:
            with opener(assets['SHA256SUMS']) as r:
                for line in r.read().decode().splitlines():
                    parts = line.split()
                    if len(parts) == 2:
                        sums[parts[1].lstrip('*')] = parts[0].lower()
        note = None
        if asset in sums:
            sha = sums[asset]
        else:
            log('WARNING: {} SHA256SUMS does not list {}; hashing the download'.format(p, asset))
            h = hashlib.sha256()
            with opener(assets[asset]) as r:
                for chunk in iter(lambda: r.read(1 << 20), b''):
                    h.update(chunk)
            sha = h.hexdigest()
            note = "# computed: not listed in the release's SHA256SUMS"
        new[p] = ({'version': version, 'asset': asset, 'sha256': sha},
                  pin['version'], version, note)

    changed = [(p, old, nv) for p, (vals, old, nv, _) in new.items()
               if any(manifest[p].get(k) != v for k, v in vals.items())]
    if changed:
        _rewrite(conf, {p: (new[p][0], new[p][3]) for p, _, _ in changed})
    width = max(len(p) for p in products)
    for p in products:
        _, old, nv, _ = new[p]
        log('{:<{w}}  {} -> {}{}'.format(p, old, nv, '' if old != nv else '  (current)', w=width))
    return changed


def _rewrite(conf, updates):
    '''Replace key values inside sections; refresh the "# computed" note.'''
    lines = Path(conf).read_text().splitlines()
    out = []
    section = None
    noted = False
    for line in lines:
        s = re.match(r'^\[([^\]]+)\]\s*$', line)
        if s:
            section = s.group(1)
            noted = False
        elif section in updates:
            vals, note = updates[section]
            if line.startswith('# computed:'):
                continue
            k = re.match(r'^([\w-]+)\s*=', line)
            if k and k.group(1) in vals:
                if 'sha256' in k.group(1) and note and not noted:
                    out.append(note)
                    noted = True
                line = '{} = {}'.format(k.group(1), vals[k.group(1)])
        out.append(line)
    Path(conf).write_text('\n'.join(out) + '\n')


# ---------------------------------------------------------------- CLI

def main(argv=None):
    ap = argparse.ArgumentParser(description='Build the SMP install jobstream for an mvslovers product.')
    ap.add_argument('product', nargs='?', help='product section in mvslovers.conf (e.g. ufsd)')
    ap.add_argument('-o', '--output', help='write the EBCDIC jobstream here')
    ap.add_argument('--ascii', action='store_true', help='print the JCL (xmit data summarised)')
    ap.add_argument('--user', help='USER= on the job cards (omit before RAKF)')
    ap.add_argument('--password', help='PASSWORD= on the job cards')
    ap.add_argument('--volume', default=DEFAULT_VOLUME, help='RECEIVE VOLUME()')
    ap.add_argument('--conf', default=str(DEFAULT_CONF))
    ap.add_argument('--dist', help='use this dist directory instead of the pinned download')
    ap.add_argument('--update', nargs='?', const='', metavar='P1,P2',
                    help='move pins in --conf to the newest releases and exit')
    a = ap.parse_args(argv)

    try:
        if a.update is not None:
            update_manifest(a.conf, [p for p in a.update.split(',') if p] or None)
            return 0
        if not a.product:
            ap.error('product required')
        if a.password and not a.user:
            ap.error('--password needs --user')
        jobs = build_jobs(discover(fetch(a.product, a.conf, a.dist)),
                          a.user, a.password, a.volume)
        if a.ascii:
            print('\n'.join(j.ascii() for j in jobs))
        if a.output:
            data = b''.join(j.bytes() for j in jobs)
            Path(a.output).write_bytes(data)
            print('{}: {} cards, jobs {}'.format(a.output, len(data) // CARD,
                                                 ' '.join(j.name for j in jobs)), file=sys.stderr)
    except MvsloversError as e:
        print('mvslovers: {}'.format(e), file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
