'''Offline tests for mvslovers.py.  Run: python3 -m unittest discover tests

The per-product tests use the pinned dists cached in temp/mvslovers/
(filled by `python3 mvslovers.py <product>` or a sysgen run); they verify
the sha256 pin and are skipped, not failed, when a dist is not cached.
'''

import io
import json
import shutil
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
import mvslovers as m  # noqa: E402

E = lambda s: s.encode('cp037')
PRODUCTS = ['brexx370', 'ufsd', 'ftpd', 'httpd', 'mvsmf', 'sdsf']


def rec(prefix):
    return E(prefix).ljust(80, b'\x00')


class DlmTest(unittest.TestCase):
    def test_default_is_aa(self):
        self.assertEqual(m.choose_dlm([rec('XY'), rec('//')]), 'AA')

    def test_skips_used(self):
        self.assertEqual(m.choose_dlm([rec('AA'), rec('AB')]), 'AC')

    def test_compares_ebcdic_not_ascii(self):
        # ASCII 'AA' bytes must not block DLM=AA
        self.assertEqual(m.choose_dlm([b'AA' + b'\x00' * 78]), 'AA')

    def test_none_left(self):
        recs = [rec(c) for c in m.DLM_CANDIDATES]
        with self.assertRaises(m.MvsloversError):
            m.choose_dlm(recs)

    def test_record_size_checked(self):
        with tempfile.NamedTemporaryFile(suffix='.xmit') as f:
            f.write(b'x' * 81)
            f.flush()
            with self.assertRaises(m.MvsloversError):
                m.read_records(f.name)


INST = """//FOOINS  JOB (SYS),'FOO INSTALL',
//             CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),
//             REGION=4096K
//*   3. replace every CHANGE.ME.* below
//* foo-1.0.0-load.xmit
//*   -> FOO.LOAD
//RECV1   EXEC PGM=IKJEFT01
//SYSTSIN  DD  *
  RECEIVE INDSN('CHANGE.ME.FOOLOAD') -
    /* VOLUME('CHANGE.ME') */ -
    DATASET('FOO.LOAD')
/*
//* foo-1.0.0-samplib.xmit
//RECV2   EXEC PGM=IKJEFT01
//SYSTSIN  DD  *
  RECEIVE INDSN('CHANGE.ME.SAMPLIB') -
    DATASET('FOO.SAMPLIB')
/*
//RECV    EXEC SMPREC
//SMPPTFIN DD  DATA,DLM=@@
++FUNCTION(TFOO100) .
++JCLIN .
//TFOO100 JOB 1,'FOO JCLIN',MSGLEVEL=1,CLASS=A
@@
//
"""


class PatchTest(unittest.TestCase):
    MAP = {'FOOLOAD': 'SYSGEN.FOO.FOOLOAD.XMIT', 'SAMPLIB': 'SYSGEN.FOO.SAMPLIB.XMIT'}

    def test_change_me_and_volume(self):
        out = m.patch_receives(INST, self.MAP, 'PUB000')
        self.assertIn("RECEIVE INDSN('SYSGEN.FOO.FOOLOAD.XMIT') -\n    VOLUME('PUB000') -\n", out)
        # RECEIVE without a VOLUME line gets one
        self.assertIn("RECEIVE INDSN('SYSGEN.FOO.SAMPLIB.XMIT') -\n    VOLUME('PUB000') -\n"
                      "    DATASET('FOO.SAMPLIB')", out)
        body = [l for l in out.splitlines() if not l.startswith('//*')]
        self.assertFalse(any('CHANGE.ME' in l for l in body))
        self.assertEqual(out.count('VOLUME('), 2)
        # comments are left alone
        self.assertIn('//*   3. replace every CHANGE.ME.* below', out)

    def test_unknown_suffix(self):
        with self.assertRaises(m.MvsloversError):
            m.patch_receives(INST, {'FOOLOAD': 'X.Y'}, 'PUB000')

    def test_no_volume(self):
        out = m.patch_receives(INST, self.MAP, None)
        self.assertNotIn('VOLUME', '\n'.join(l for l in out.splitlines() if not l.startswith('//*')))


class JobCardTest(unittest.TestCase):
    def test_replace(self):
        name, out = m.replace_job_card(INST, 'IBMUSER', 'SYS1')
        self.assertEqual(name, 'FOOINS')
        lines = out.splitlines()
        self.assertEqual(lines[0], "//FOOINS JOB (SYSGEN),'FOO INSTALL',")
        self.assertIn('//             USER=IBMUSER,', lines)
        self.assertIn('//             PASSWORD=SYS1,', lines)
        self.assertIn('//             REGION=8192K', lines)
        self.assertNotIn('MSGCLASS=H', out)
        self.assertEqual(lines[7], '//*   3. replace every CHANGE.ME.* below')
        # the JCLIN job card inside the SYSMOD is data and stays
        self.assertIn("//TFOO100 JOB 1,'FOO JCLIN',MSGLEVEL=1,CLASS=A", out)
        self.assertEqual(out.count(' JOB '), 2)

    def test_without_user(self):
        _, out = m.replace_job_card(INST)
        self.assertNotIn('USER=', out)
        self.assertNotIn('PASSWORD=', out)

    def test_not_a_job(self):
        with self.assertRaises(m.MvsloversError):
            m.replace_job_card('//STEP EXEC PGM=X\n')


class CardTest(unittest.TestCase):
    def test_length_and_codepage(self):
        cards = m.jcl_to_cards("//A JOB\n  data\n")
        self.assertTrue(all(len(c) == 80 for c in cards))
        self.assertEqual(cards[0].decode('cp037').rstrip(), '//A JOB')

    def test_col72(self):
        with self.assertRaises(m.MvsloversError):
            m.jcl_to_cards('//' + 'X' * 70)
        m.jcl_to_cards('++' + 'X' * 78)          # in-stream data may use 80

    def test_too_long(self):
        with self.assertRaises(m.MvsloversError):
            m.jcl_to_cards('X' * 81)


class SyntheticDistTest(unittest.TestCase):
    def setUp(self):
        self.d = Path(tempfile.mkdtemp())
        (self.d / 'foo-1.0.0-inst.jcl').write_text(INST)
        (self.d / 'foo-1.0.0-alloc.jcl').write_text("//FOOALC  JOB (SYS),'A'\n//S EXEC PGM=IEFBR14\n//\n")
        (self.d / 'foo-1.0.0-load.xmit').write_bytes(rec('AA') + rec('AB') + rec('//'))
        (self.d / 'foo-1.0.0-samplib.xmit').write_bytes(rec('/*'))

    def tearDown(self):
        shutil.rmtree(self.d)

    def test_discover_without_accept(self):
        dist = m.discover(self.d)
        self.assertEqual((dist.product, dist.version, dist.fmid), ('foo', '1.0.0', 'TFOO100'))
        self.assertIsNone(dist.accept)
        self.assertEqual(list(dist.xmits), ['FOOLOAD', 'SAMPLIB'])

    def test_stream(self):
        jobs = m.build_jobs(m.discover(self.d), 'IBMUSER', 'SYS1')
        self.assertEqual([j.name for j in jobs], ['FOOXMIT', 'FOOALC', 'FOOINS', 'FOOCLN'])
        data = b''.join(j.bytes() for j in jobs)
        self.assertEqual(len(data) % 80, 0)
        cards = [data[i:i + 80] for i in range(0, len(data), 80)]
        dlm_i = cards.index(E('//SYSUT1   DD  DATA,DLM=AC').ljust(80, E(' ')))
        self.assertEqual(cards[dlm_i + 1:dlm_i + 4], [rec('AA'), rec('AB'), rec('//')])
        self.assertEqual(cards[dlm_i + 4], E('AC').ljust(80, E(' ')))

    def test_unreferenced_xmit(self):
        (self.d / 'foo-1.0.0-extra.xmit').write_bytes(rec('X'))
        with self.assertRaises(m.MvsloversError):
            m.discover(self.d)


class _Resp(io.BytesIO):
    def __enter__(self):
        return self

    def __exit__(self, *a):
        self.close()


class UpdateTest(unittest.TestCase):
    CONF = """# top comment
[ufsd]
repo = mvslovers/ufsd
version = 1.5.2
asset = ufsd-1.5.2-dist.tar.gz
sha256 = old

[httpd]
repo = mvslovers/httpd
version = 4.5.1
asset = httpd-4.5.1-dist.tar.gz
sha256 = same

[rakf]
repo = mvslovers/RAKF
ref = v2.0.0
"""

    def opener(self, url):
        rel = lambda tag, names: {'tag_name': tag, 'assets': [
            {'name': n, 'browser_download_url': 'dl/' + n} for n in names]}
        table = {
            'https://api.github.com/repos/mvslovers/ufsd/releases/latest':
                json.dumps(rel('v1.6.0', ['ufsd-1.6.0-dist.tar.gz', 'SHA256SUMS'])).encode(),
            'https://api.github.com/repos/mvslovers/httpd/releases/latest':
                json.dumps(rel('v4.5.1', ['httpd-4.5.1-dist.tar.gz'])).encode(),
            'https://api.github.com/repos/mvslovers/RAKF/releases/latest':
                json.dumps(rel('v2.1.0', [])).encode(),
            'dl/SHA256SUMS': b'abc123  ufsd-1.6.0-dist.tar.gz\nfff  other.pdf\n',
        }
        return _Resp(table[url])

    def setUp(self):
        self.f = Path(tempfile.mkdtemp()) / 'm.conf'
        self.f.write_text(self.CONF)

    def test_update_all(self):
        out = []
        changed = m.update_manifest(self.f, opener=self.opener, log=out.append)
        self.assertEqual(changed, [('ufsd', '1.5.2', '1.6.0'), ('rakf', 'v2.0.0', 'v2.1.0')])
        man = m.load_manifest(self.f)
        self.assertEqual(man['ufsd'], {'repo': 'mvslovers/ufsd', 'version': '1.6.0',
                                       'asset': 'ufsd-1.6.0-dist.tar.gz', 'sha256': 'abc123'})
        self.assertEqual(man['httpd']['sha256'], 'same')
        self.assertEqual(man['rakf']['ref'], 'v2.1.0')
        self.assertTrue(self.f.read_text().startswith('# top comment\n'))
        self.assertIn('ufsd   1.5.2 -> 1.6.0', out)

    def test_update_selected(self):
        m.update_manifest(self.f, ['rakf'], opener=self.opener, log=lambda *_: None)
        man = m.load_manifest(self.f)
        self.assertEqual(man['ufsd']['version'], '1.5.2')
        self.assertEqual(man['rakf']['ref'], 'v2.1.0')

    def test_current_is_noop(self):
        before = self.f.read_text()
        m.update_manifest(self.f, ['httpd'], opener=self.opener, log=lambda *_: None)
        self.assertEqual(self.f.read_text(), before)

    def test_unknown(self):
        with self.assertRaises(m.MvsloversError):
            m.update_manifest(self.f, ['nope'], opener=self.opener)


class RakfUsersTest(unittest.TestCase):
    CONF = ('# comment\n'
            'IBMUSER SYS1   IKJACCNT OPER   ACCT   JCL MOUNT   ADMIN\n'
            'MVSCE02 PASS4U IKJACCNT NOOPER NOACCT JCL NOMOUNT NOADMIN\n')

    def test_columns(self):
        lines = m.rakf_users(self.CONF).splitlines()
        self.assertEqual(lines[:3], [
            'IBMUSER  ADMIN   *SYS1     Y Y',
            'IBMUSER  RAKFADM  SYS1     Y Y',
            'MVSCE02  USER    *PASS4U   N N'])
        # RAKF 2.0.0 shipped users.txt layout: oper col 28, special col 30
        self.assertEqual(lines[0][27] + lines[0][29], 'YY')
        self.assertEqual(lines[3:], ['UFSD     UFSROOT *         N N',
                                     'FTPD     STARTED *         N N',
                                     'HTTPD    STARTED *         N N'])

    def test_extra_admin(self):
        lines = m.rakf_users(self.CONF, ('herc01', 'cul8tr')).splitlines()
        self.assertIn('HERC01   ADMIN   *CUL8TR   Y Y', lines)
        self.assertIn('HERC01   RAKFADM  CUL8TR   Y Y', lines)

    def test_rejects(self):
        for bad in ('TOOLONGU X a b c d e f\n', 'FTPD X a b c d e f\n',
                    self.CONF + 'IBMUSER X a b c d e f\n'):
            with self.assertRaises(m.MvsloversError):
                m.rakf_users(bad)


class RealDistTest(unittest.TestCase):
    '''Every pinned product's dist parses and yields a clean stream.'''

    def test_manifest_pins_all(self):
        man = m.load_manifest()
        self.assertEqual(sorted(m.dist_products(man)), sorted(PRODUCTS))
        self.assertEqual(man['rakf']['ref'], 'v2.0.0')

    def test_products(self):
        man = m.load_manifest()
        for p in PRODUCTS:
            with self.subTest(product=p):
                if not (m.DEFAULT_CACHE / man[p]['asset']).exists():
                    self.skipTest('{} not cached in {}'.format(man[p]['asset'], m.DEFAULT_CACHE))
                with tempfile.TemporaryDirectory() as cache:
                    shutil.copy(m.DEFAULT_CACHE / man[p]['asset'], cache)
                    dist = m.discover(m.fetch(p, cache=cache, log=lambda *_: None))
                    self.check(p, dist)

    def check(self, p, dist):
        self.assertEqual(dist.product, p)
        self.assertTrue(dist.fmid.startswith('T'))
        self.assertTrue(dist.xmits)
        jobs = m.build_jobs(dist, 'IBMUSER', 'SYS1')
        names = [j.name for j in jobs]
        self.assertEqual(len(names), 5 if dist.accept else 4)
        self.assertTrue(all(len(n) <= 8 for n in names))
        raw = {id(r) for j in jobs for part in j.parts if part[0] == 'raw' for r in part[1]}
        for j in jobs:
            for part in j.parts:
                if part[0] == 'jcl':
                    for line in part[1].splitlines():
                        if line.startswith('//') and not line.startswith('//*'):
                            self.assertLessEqual(len(line), 71, line)
                        if not line.startswith('//*'):
                            self.assertNotIn('CHANGE.ME', line)
            cards = j.cards()
            self.assertTrue(all(len(c) == 80 for c in cards))
            self.assertTrue(cards[0].decode('cp037').startswith('//{} JOB '.format(j.name)))
        # xmit bytes pass through untouched
        xjob = jobs[0]
        for suffix, path in dist.xmits.items():
            recs = next(pt[1] for pt in xjob.parts if pt[0] == 'raw' and pt[2] == path.name)
            self.assertEqual(b''.join(recs), path.read_bytes())
        inst = next(j for j in jobs if j.name.endswith('INS')).ascii()
        self.assertEqual(inst.count("VOLUME('PUB000')"), len(dist.xmits))


def parse_netdata(data):
    '''Minimal NETDATA reader: [(kind, payload)] with kind 'INMRxx' or 'data'.'''
    out, pos, cur, ctl = [], 0, b'', None
    while pos < len(data) and data[pos]:
        n, flag = data[pos], data[pos + 1]
        cur += data[pos + 2:pos + n]
        pos += n
        if flag & 0x40:                       # last segment of a record
            if flag & 0x20:
                out.append((cur[:6].decode('cp037'), cur[6:]))
            else:
                out.append(('data', cur))
            cur = b''
    return out


RAKF_TXT = ('IBMUSER  ADMIN   *SYS1     Y Y\n'
            'IBMUSER  RAKFADM  SYS1     Y Y\n'
            'MVSCE02  USER    *PASS4U   N N\n'
            'UFSD     UFSROOT *         N N\n')


class NetdataTest(unittest.TestCase):
    def test_roundtrip(self):
        blocks = [bytes([i]) * 4096 for i in range(3)]
        nd = m.netdata_seq(blocks, 'UFSD.ROOT')
        self.assertEqual(len(nd) % 80, 0)
        recs = parse_netdata(nd)
        self.assertEqual([k for k, _ in recs],
                         ['INMR01', 'INMR02', 'INMR03', 'data', 'data', 'data', 'INMR06'])
        self.assertEqual([p for k, p in recs if k == 'data'], blocks)
        inmr02 = recs[1][1]
        self.assertEqual(inmr02[:4], b'\x00\x00\x00\x01')          # file 1
        self.assertIn(b'\x00\x49\x00\x01\x00\x02\xc0\x00', inmr02)  # RECFM=U
        self.assertIn(b'\x00\x30\x00\x01\x00\x02\x10\x00', inmr02)  # BLKSIZE 4096
        self.assertIn(b'\x00\x02\x00\x02\x00\x04' + E('UFSD') + b'\x00\x04' + E('ROOT'), inmr02)

    def test_xmi_reader(self):
        try:
            import xmi
        except ImportError:
            self.skipTest('xmi-reader not installed')
        d = Path(tempfile.mkdtemp())
        (d / 'x.xmit').write_bytes(m.netdata_seq([b'\x01' * 4096] * 2, 'HTTPD.WEBROOT.UFS'))
        o = xmi.XMIT(filename=str(d / 'x.xmit'))
        o.open()
        meta = json.loads(o.get_xmit_json())['INMR02']['1']
        self.assertEqual((meta['INMDSNAM'], meta['INMRECFM'], meta['INMBLKSZ'], meta['INMSIZE']),
                         ('HTTPD.WEBROOT.UFS', 'U', 4096, 8192))

    def test_image_size_checked(self):
        f = Path(tempfile.mkdtemp()) / 'bad.img'
        f.write_bytes(b'\x00' * 5000)
        with self.assertRaises(m.MvsloversError):
            m.image_blocks(f, 4096)


class UfsDiskTest(unittest.TestCase):
    def setUp(self):
        self.d = Path(tempfile.mkdtemp())
        (self.d / 'web.img').write_bytes(b'\x00' * 8192)
        self.disks = m.plan_ufs_disks(RAKF_TXT, 'IBMUSER', self.d / 'web.img', self.d)

    def test_plan(self):
        self.assertEqual([(x.dsn, x.path, x.mode, x.owner) for x in self.disks], [
            ('UFSD.ROOT', '/', 'ROOT', None),
            ('HTTPD.WEBROOT.UFS', '/www', 'RO', None),
            ('IBMUSER.UFSHOME', '/u/ibmuser', 'RW', 'IBMUSER'),
            ('MVSCE02.UFSHOME', '/u/mvsce02', 'RW', 'MVSCE02'),
            ('UFSD.SCRATCH', '/tmp', 'RW', None)])
        home = self.disks[3]
        self.assertEqual((home.img_owner, home.img_group), ('MVSCE02', 'USER'))

    def test_build_passes_owner(self):
        calls = []

        def run(args):
            calls.append(args)
            Path(args[2]).write_bytes(b'\x00' * 4096)
        m.build_ufs_images(self.disks, '/x/ufsd-utils', run)
        self.assertEqual(len(calls), 4)                  # webroot is shipped
        self.assertEqual(calls[0][:3], ['/x/ufsd-utils', 'create', str(self.d / 'root.img')])
        self.assertIn('--owner', calls[0])
        self.assertEqual(calls[2][calls[2].index('--group') + 1], 'USER')

    def test_ufsdprm0(self):
        text = m.ufsdprm0(self.disks)
        self.assertIn('ROOT     DSN(UFSD.ROOT)\n', text)
        self.assertIn('MOUNT    DSN(MVSCE02.UFSHOME) PATH(/u/mvsce02) MODE(RW) OWNER(MVSCE02)\n', text)
        self.assertIn('MOUNT    DSN(UFSD.SCRATCH) PATH(/tmp) MODE(RW)\n', text)

    def test_disk_jobs(self):
        for x in self.disks:
            x.image.write_bytes(b'\x00' * 4096)
        jobs = m.disk_jobs(self.disks, 'IBMUSER', 'SYS1')
        self.assertEqual([j.name for j in jobs], ['UFSDSK0{}'.format(i) for i in range(1, 6)])
        text = jobs[3].ascii()
        self.assertIn("DATASET('SYSGEN.UFSDSK04.IMG')", text)
        self.assertIn('DSN=MVSCE02.UFSHOME,DISP=(NEW,CATLG,DELETE)', text)
        self.assertIn('DCB=(DSORG=PS,RECFM=U,BLKSIZE=4096)', text)
        self.assertIn("VOLUME('PUB000')", text)
        self.assertTrue(all(len(c) == 80 for j in jobs for c in j.cards()))

    def test_site_config(self):
        job = m.site_config_job(self.disks, 'IBMUSER', 'SYS1')
        text = job.ascii()
        for src, dst in m.SITE_COPIES:
            self.assertIn('DSN={}'.format(dst), text)
        self.assertIn('SYS2.PARMLIB(UFSDPRM0)', text)
        self.assertIn("'STCGRP=STARTED'", text)
        cards = job.cards()                 # enforces col 71 / 80 / cp037
        # the REXX starts with /* and must not end DD DATA early
        self.assertIn('//RXRUN    DD  DATA,DLM=@@', text)

    def test_parse_rakf_users(self):
        self.assertEqual(m.parse_rakf_users(RAKF_TXT)[0], ('IBMUSER', 'ADMIN', 'SYS1'))
        self.assertEqual(m.parse_rakf_users(RAKF_TXT)[3], ('UFSD', 'UFSROOT', ''))


class RakfProfilesTest(unittest.TestCase):
    def test_ufs_profiles(self):
        base = ('DATASET *                                                   READ\n'
                'FACILITYSVC244                                      UFSROOT READ\n')
        out = m.rakf_profiles(base, RAKF_TXT).splitlines()
        self.assertIn('DATASET HTTPD.WEBROOT.UFS                           UFSROOT UPDATE', out)
        self.assertIn('DATASET MVSCE02.UFSHOME                             UFSROOT UPDATE', out)
        self.assertIn('DATASET MVSCE02.UFSHOME                                     READ', out)
        self.assertFalse(any('UFSD.UFSHOME' in l for l in out))      # no home for STC ids
        self.assertEqual(out, sorted(out, key=lambda l: l.encode('cp037')))
        self.assertTrue(all(len(l) <= 67 for l in out))
        # universal line before its group lines
        i = out.index('DATASET IBMUSER.UFSHOME                                     READ')
        self.assertTrue(out[i + 1].startswith('DATASET IBMUSER.UFSHOME') and 'UFSROOT' in out[i + 1])
        self.assertEqual(m.rakf_profiles('\n'.join(out), RAKF_TXT).splitlines(), out)

    def test_profiles_conf_has_ufsroot(self):
        text = (Path(__file__).resolve().parent.parent / 'profiles.conf').read_text()
        self.assertIn('FACILITYSVC244                                      UFSROOT READ', text)


class ToolTest(unittest.TestCase):
    def test_update_tool_pin(self):
        f = Path(tempfile.mkdtemp()) / 'm.conf'
        f.write_text('[ufsd-utils]\nrepo = mvslovers/ufsd-utils\nversion = 1.0.1\n'
                     '# computed: x\nsha256-linux-amd64 = a\nsha256-linux-arm64 = b\n')

        def opener(url):
            if url.startswith('https://api.github.com'):
                return _Resp(json.dumps({'tag_name': 'v1.1.0', 'assets': [
                    {'name': n, 'browser_download_url': 'dl/' + n}
                    for n in ('ufsd-utils-linux-amd64.tar.gz', 'ufsd-utils-linux-arm64.tar.gz')]}).encode())
            return _Resp(url.encode())
        m.update_manifest(f, opener=opener, log=lambda *_: None)
        man = m.load_manifest(f)['ufsd-utils']
        import hashlib
        self.assertEqual(man['version'], '1.1.0')
        self.assertEqual(man['sha256-linux-amd64'],
                         hashlib.sha256(b'dl/ufsd-utils-linux-amd64.tar.gz').hexdigest())
        self.assertEqual(f.read_text().count('# computed:'), 1)

    def test_not_a_dist_product(self):
        self.assertNotIn('ufsd-utils', m.dist_products(m.load_manifest()))


class SdsfTest(unittest.TestCase):
    def test_config_job(self):
        job = m.sdsf_config_job('IBMUSER', 'SYS1')
        text = job.ascii()
        job.cards()                         # enforces col 71 / 80 / cp037
        for dsn in ('SYS1.PARMLIB(IEAAPF00)', 'SYS1.PARMLIB(LNKLST00)',
                    'SYS1.CMDPROC(ISPF)', 'SYS1.PROCLIB(IKJACCNT)'):
            self.assertEqual(text.count('DSN=' + dsn), 3)   # in, put, show
        self.assertIn("PARM='RXRUN MVSRES'", text)
        # nothing is written back unless the edit step ended 0
        self.assertEqual(text.count('EXEC PGM=IEBGENER,COND=(0,NE)'), 8)
        self.assertNotIn('\n@@', m.SDSF_EDIT_REXX)
        self.assertTrue(all(len(l) <= 80 for l in m.SDSF_EDIT_REXX.splitlines()))

    def test_probe_jobs(self):
        self.assertTrue(all(len(l) <= 71 for l in m.SDSF_PROBE_ASM.splitlines()))
        v = m.sdsf_verify_job('TISF120', 'IBMUSER', 'SYS1')
        text = v.ascii()
        v.cards()
        self.assertIn(' LIST CDS SYSMOD(TISF120) .', text)
        self.assertIn('//RUNISF   EXEC PGM=ISFISP\n', text)
        self.assertNotIn('STEPLIB  DD  DISP=SHR,DSN=ISF', text)
        self.assertIn("PARM='IBMUSER'", text)
        p = m.sdsf_probe_job('MVSCE02', 'PASS4U')
        p.cards()
        self.assertIn("PARM='MVSCE02'", p.ascii())
        self.assertIn('USER=MVSCE02', p.ascii())
        with self.assertRaises(m.MvsloversError):
            m.sdsf_probe_steps("X'Y")

    def test_parse_probe(self):
        out = m.parse_sdsf_probe(
            'SDSFPRB IBMUSER  APF MVSRES SYS1.LINKLIB\n'
            'SDSFPRB IBMUSER  APF MVSRES ISF.LINKLIB\n'
            'SDSFPRB IBMUSER  ESR11 ON\n'
            'SDSFPRB IBMUSER  SDSF.OPER READ=00 UPDATE=00\n'
            'SDSFPRB MVSCE02  APF MVSRES SYS1.LINKLIB\n'
            'SDSFPRB MVSCE02  ESR11 OFF\n'
            'SDSFPRB MVSCE02  SDSF.OPER READ=08 UPDATE=08\n'
            # a later run of the same tag wins
            'SDSFPRB MVSCE02  APF MVSRES SYS1.SVCLIB\n'
            'SDSFPRB MVSCE02  ESR11 ON\n'
            'SDSFPRB MVSCE02  SDSF.OPER READ=04 UPDATE=08\n')
        a = out['IBMUSER']
        self.assertIn(('MVSRES', 'ISF.LINKLIB'), a['apf'])
        self.assertTrue(a['esr11'])
        self.assertEqual((a['read'], a['update']), (0, 0))
        o = out['MVSCE02']
        self.assertEqual(o['apf'], [('MVSRES', 'SYS1.SVCLIB')])
        self.assertEqual((o['esr11'], o['read'], o['update']), (True, 4, 8))
        self.assertTrue(m.parse_sdsf_probe('SDSFPRB X  APF TABLE ERROR')['X']['apf_error'])

    def test_steps_cc(self):
        self.assertEqual(m.SDSF_STEPS_CC['SDSFINS'], {'APPLY': '0004'})
        self.assertEqual(m.SDSF_STEPS_CC['SDSFACC'], {'ACCEPT': '0004'})


# LIST CDS output as SMP 04.48 prints it on MVS/CE (Phase 7 probe, trimmed)
SMP_LIST = """\
SMPCDS   SYSMOD ENTRIES
  NAME
JLM0003   TYPE            = USERMOD
          STATUS          = REC  APP
          FMID            = EBB1102
          SUPING VER(001) = NJE0001
          MOD             = IKJEFTE2
NJE0001   TYPE            = SUPERSEDED
          LASTSUP         = JLM0003
SMPCDS   SYSMOD ENTRIES
  NAME
UY16532   TYPE            = PTF
          STATUS          = REC  APP  ACC
          FMID            = EBB1102
ZMG0001   TYPE            = USERMOD
          STATUS          = REC  APP
LIST     SELECT SUMMARY REPORT FOR SMPCDS
THE FOLLOWING SELECTED ENTRIES WERE NOT FOUND OR WERE NOT ELIGIBLE FOR PROCESSING
 TYPE        NAME
 SYSMOD      ZMG0002
HMA2050    LIST PROCESSING COMPLETED - HIGHEST RETURN CODE IS 04
SMPACDS  SYSMOD ENTRIES
  NAME
UY16532   TYPE            = PTF
          STATUS          = REC  ACC
"""


class ZmgTest(unittest.TestCase):
    def files(self):
        pin = m.load_manifest()['zmg0001']
        out = {}
        for key in m.release_files(pin):
            p = m.DEFAULT_CACHE / 'zmg0001-{}-{}'.format(pin['version'], pin[key])
            if not p.exists():
                self.skipTest('{} not cached in {}'.format(p.name, m.DEFAULT_CACHE))
            out[key] = p
        return out

    def test_pin(self):
        man = m.load_manifest()
        self.assertEqual(m.release_files(man['zmg0001']), ['xmit', 'zip'])
        self.assertNotIn('zmg0001', m.dist_products(man))
        self.assertEqual(man['zmg0001']['version'], man['brexx370']['version'])

    def test_fetch_checks_sha(self):
        files = self.files()
        conf = Path(tempfile.mkdtemp()) / 'm.conf'
        conf.write_text('[zmg0001]\nrepo = x/y\nversion = 3.1.0\nxmit = ZMG0001.xmit\n'
                        'xmit-sha256 = {}\n'.format(m.sha256_file(files['xmit'])))
        self.assertEqual(m.fetch_files('zmg0001', conf, m.DEFAULT_CACHE, log=self.fail),
                         {'xmit': files['xmit']})
        conf.write_text(conf.read_text().replace('xmit-sha256 = ', 'xmit-sha256 = 0'))
        cache = Path(tempfile.mkdtemp())
        shutil.copy(files['xmit'], cache / files['xmit'].name)
        with mock_download(cache / files['xmit'].name):
            with self.assertRaises(m.MvsloversError):
                m.fetch_files('zmg0001', conf, cache, log=lambda *_: None)

    def test_jobs(self):
        jobs = m.zmg0001_jobs(self.files(), 'IBMUSER', 'SYS1')
        self.assertEqual([j.name for j in jobs],
                         ['ZMGXMIT', 'ZMGPREP', 'ZMG01CK', 'ZMG01IN', 'ZMG01AP'])
        for j in jobs:
            cards = j.cards()
            self.assertTrue(all(len(c) == 80 for c in cards))
            text = j.ascii()
            self.assertTrue(text.startswith('//{} JOB (SYSGEN)'.format(j.name)), text[:40])
            self.assertIn('USER=IBMUSER', text)
            self.assertNotIn('YOUR.', text)
        x = jobs[0].ascii()
        self.assertIn("RECEIVE INDSN('SYSGEN.ZMG0001.XMIT')", x)
        self.assertIn("VOLUME('PUB000')", x)
        self.assertIn("DATASET('USERMOD.ZMG0001')", x)
        self.assertIn('ACCEPT SELECT(UY16532)', jobs[1].ascii())
        self.assertIn('APPLY SELECT(ZMG0001) .', jobs[4].ascii())
        # xmit records go through untouched
        raw = [p for p in jobs[0].parts if p[0] == 'raw'][0][1]
        self.assertEqual(b''.join(raw), self.files()['xmit'].read_bytes())

    def test_jobs_without_user(self):
        for j in m.zmg0001_jobs(self.files()):
            self.assertNotIn('USER=', j.ascii())

    def test_steps_cc_keys(self):
        self.assertEqual(set(m.ZMG_STEPS_CC), {'ZMGPREP', 'ZMG01CK', 'ZMG01IN'})

    def test_links(self):
        text = ('HMA2390    LINK SUCCESSFUL - MOD=IKJCT430 - LMOD=EXEC - LIBRARY=CMDLIB'
                ' - SYSMOD=ZMG0001 - RETURN CODE=00\n'
                '//* THE LISTING MUST SHOW "HMA2390 LINK SUCCESSFUL" FOR IKJCT430 AND\n'
                'HMA2390    LINK SUCCESSFUL - MOD=IKJCT437 - LMOD=EXEC - LIBRARY=CMDLIB'
                ' - SYSMOD=ZMG0001 - RETURN CODE=00\n')
        self.assertEqual(m.zmg_links(text), {'IKJCT430': 'EXEC', 'IKJCT437': 'EXEC'})

    def test_update_pin(self):
        f = Path(tempfile.mkdtemp()) / 'm.conf'
        f.write_text('[zmg0001]\nrepo = mvslovers/brexx370\nversion = 3.1.0\n'
                     'xmit = ZMG0001.xmit\nxmit-sha256 = a\nzip = ZMG0001.zip\nzip-sha256 = b\n')

        def opener(url):
            if url.startswith('https://api.github.com'):
                return _Resp(json.dumps({'tag_name': 'v3.2.0', 'assets': [
                    {'name': n, 'browser_download_url': 'dl/' + n}
                    for n in ('ZMG0001.xmit', 'ZMG0001.zip', 'SHA256SUMS')]}).encode())
            if url == 'dl/SHA256SUMS':
                return _Resp(b'1111  ZMG0001.xmit\n')
            return _Resp(url.encode())
        m.update_manifest(f, opener=opener, log=lambda *_: None)
        man = m.load_manifest(f)['zmg0001']
        import hashlib
        self.assertEqual(man['version'], '3.2.0')
        self.assertEqual(man['xmit-sha256'], '1111')
        self.assertEqual(man['zip-sha256'], hashlib.sha256(b'dl/ZMG0001.zip').hexdigest())
        self.assertEqual(f.read_text().count('# computed:'), 1)


class mock_download:
    '''Make _download a no-op (the file is already where it would go).'''
    def __init__(self, path):
        self.path = path

    def __enter__(self):
        self.orig = m._download
        m._download = lambda url, dest: dest

    def __exit__(self, *a):
        m._download = self.orig


class UsermodTest(unittest.TestCase):
    def test_ids_ascii_and_ebcdic(self):
        d = Path(tempfile.mkdtemp())
        (d / 'a.jcl').write_text('//X JOB\n++USERMOD(ZP60039)   REWORK(1) .\n'
                                 '++ USERMOD (#DYP001)  .\n  ++USERMOD(NOPE)\n')
        self.assertEqual(m.usermod_ids(d / 'a.jcl'), ['ZP60039', '#DYP001'])
        cards = m.jcl_to_cards('//X JOB\n++USERMOD(ZP60025) .\n')
        (d / 'e.jcl').write_bytes(b''.join(cards))
        self.assertEqual(m.usermod_ids(d / 'e.jcl'), ['ZP60025'])

    def test_parse_list(self):
        cds = m.parse_smp_list(SMP_LIST)
        self.assertEqual(cds['JLM0003']['status'], 'REC APP')
        self.assertEqual(cds['UY16532']['status'], 'REC APP ACC')
        self.assertEqual(cds['NJE0001'], {'type': 'SUPERSEDED', 'status': None,
                                          'lines': cds['NJE0001']['lines']})
        self.assertIn('ZMG0001', cds)
        self.assertNotIn('ZMG0002', cds)
        self.assertEqual(m.parse_smp_list(SMP_LIST, 'SMPACDS')['UY16532']['status'], 'REC ACC')

    def test_parse_list_page_break(self):
        text = ('SMPCDS   SYSMOD ENTRIES\n  NAME\n'
                'ZP60003   TYPE            = USERMOD     DATE 26.317  TIME 18:23:11      HMASMP LVL 04.48 '
                'MESSAGES AND CONTROL STATEMENTS   PAGE 0007\n\n'
                'SMPCDS   SYSMOD ENTRIES\n\n\n  NAME\n\n'
                '          STATUS          = REC  APP\n          FMID            = EAS1102\n')
        e = m.parse_smp_list(text)['ZP60003']
        self.assertEqual((e['type'], e['status']), ('USERMOD', 'REC APP'))

    def test_check(self):
        cds = m.parse_smp_list(SMP_LIST)
        names = ['JLM0003', 'NJE0001', 'UY16532', 'ZMG0001']
        self.assertEqual(m.check_usermods(cds, names, {'ZMG0001': 'REC APP'}, ['NJE0001']), [])
        bad = m.check_usermods(cds, names + ['ZMG0002'], {'ZMG0001': 'REC APP ACC'})
        self.assertEqual(len(bad), 3, bad)

    def test_verify_job(self):
        names = ['AY12275', '#DYP001'] + ['ZP6{:04}'.format(n) for n in range(30)]
        j = m.usermods_verify_job(names, 'IBMUSER', 'SYS1')
        text = j.ascii()
        self.assertTrue(all(len(c) == 80 for c in j.cards()))
        for n in names:
            self.assertIn(n, text)
        for cmd, _ in m.ZMG_TEST_CMDS:
            self.assertIn('  ' + cmd + '\n', text)
        # SMP control statements stay inside columns 1-72
        self.assertTrue(all(len(l) <= 72 for l in text.splitlines() if 'SYSMOD(' in l or l.startswith('   ')))

    def test_parse_zmg_test(self):
        out = ("  %ZMGTST IMPLICIT\nZMGTEST IMPLICIT V3R1M0 (Nov 2026)\n"
               "SAY 'ZMGTEST' ARG(1) VERSION()\nZMGTEST EXPLICIT V3R1M0\n"
               "ZMGTEST KEYWORD V3R1M0\nZMGTEST CLIST RAN\n")
        self.assertEqual(m.parse_zmg_test(out), {'IMPLICIT': 'V3R1M0', 'EXPLICIT': 'V3R1M0',
                                                 'KEYWORD': 'V3R1M0', 'CLIST': 'RAN'})


def load_sysgen():
    '''sysgen.py runs main() on import: exec it without the trailing call.'''
    p = Path(__file__).resolve().parent.parent / 'sysgen.py'
    src = p.read_text()
    g = {'__file__': str(p), '__name__': 'sysgen_test'}
    exec(compile(src[:src.rindex('\nmain()')], str(p), 'exec'), g)
    return g


class Step5Test(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.g = load_sysgen()

    def test_backup_names(self):
        g = self.g
        self.assertEqual(g['usermod_backup_name'](0), '24_SYSGEN06')
        self.assertEqual(g['usermod_backup_name'](1), 'usermod_01_AY12275')
        i = g['usermods'].index('ZP60038')
        self.assertEqual(g['usermod_backup_name'](i), 'usermod_{:02}_ZP60037'.format(i))

    def test_restart_index(self):
        g = self.g
        u = g['usermods']
        self.assertEqual(g['usermod_restart_index'](u.index('ZP60038')), 0)
        g['USERMOD_IPL_POINTS'] = ('ZP60014', 'ZP60030')
        try:
            r = g['usermod_restart_index']
            self.assertEqual(r(u.index('ZP60013')), 0)
            self.assertEqual(r(u.index('ZP60014')), u.index('ZP60014'))
            self.assertEqual(r(u.index('ZP60029')), u.index('ZP60014'))
            self.assertEqual(r(u.index('SLB0001')), u.index('ZP60030'))
        finally:
            g['USERMOD_IPL_POINTS'] = ()

    def test_dasdls_extents(self):
        out = ('Dsname                                       CREDT ORG RECFM LRECL BLKSZ Key  Trks%Use#Ext 2ndry_alloc\n'
               'SYS1.LINKLIB                                 26193 PO  U           19069   0   600  90   1 CYL       1\n'
               'SYS1.MACLIB                                  26193 PO  FB       80 19040   0   900  82   2 TRK      10\n'
               'VOLSER:  MVSRES    "x"\n')
        self.assertEqual(self.g['dasdls_extents'](out), {'SYS1.LINKLIB': 1, 'SYS1.MACLIB': 2})

    def test_ipl_points_are_usermods(self):
        for p in self.g['USERMOD_IPL_POINTS']:
            self.assertIn(p, self.g['usermods'])


if __name__ == '__main__':
    unittest.main()
