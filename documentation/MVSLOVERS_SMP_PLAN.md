# Plan: install the mvslovers stack with SMP during sysgen

Status: **Phases 1–9 done** (written 2026-11-13; Phase 9 2026-11-15, release tag pending). Work through the phases in order;
each one is sized for a single context window. At the start of a phase, read
this file, then the "Inputs" of that phase. At the end, tick its checklist and
append to the "Phase log" at the bottom so the next window can pick up.

## Goal

1. Remove from MVP the software that is now installed with SMP.
2. Confirm the SMP level sysgen builds is right for the whole stack.
3. Get the built mvslovers packages onto the system during sysgen, without
   mvsMF/FTP (only the Hercules card readers are available).
4. Apply the extra usermods the stack needs (BREXX: MS00100, ZMG0001).

Products: **BREXX/370, RAKF, UFSD, FTPD, HTTPD, mvsMF, SDSF**.

---

## Findings (verified 2026-11-13)

### How each product installs now

| Product | Latest release | FMID | Install mechanism | Notes |
|---|---|---|---|---|
| BREXX/370 | v3.1.0 | `TBRX310` | dist: alloc / inst / accept, 6 xmits | Not APF. Needs link list entry (MS00100) and TSO setup. ZMG0001 optional. |
| RAKF | v2.0.0 | `TRKF200` | `generate_release.py` → one EBCDIC jobstream, SMPREC/APPLY + assembles on target | **Is SMP** (always was). Tested on SMP 04.48. Its jobstream runs `PGM=BREXX` with `RXLIB DD DSN=BREXX.CURRENT.RXLIB`. |
| UFSD | v1.5.2 (main 1.6.0-dev) | `TUFS152` | dist: alloc / inst (1.5.2 still accepts in inst) | Auth via RAKF SVC 244 or APF. |
| FTPD | v1.3.4 | `TFTP134`-ish | dist: alloc / inst / accept | Needs RAKF. Alloc uses `TGTVOL=MVSRES,DLBVOL=SMP000`. |
| HTTPD | v4.5.1 | | dist: alloc / inst (+ `httpd-webroot.img`) | Needs RAKF; UFSD optional (static files). |
| mvsMF | v1.4.1 | `TZMF141` | dist: alloc / inst / accept | Needs httpd ≥ 4.5.1, RAKF; uses `SYZJ201` (already in sysgen). Concatenate into HTTPD STEPLIB + routes. |
| SDSF | v1.2.0 | `TISF120` | dist: alloc / inst (assembles SVC `IGX00011` into LPALIB) / accept | Needs ISPF, IPL **CLPA**, APF + LNKLST for `ISF.LINKLIB`, edits `SYS1.CMDPROC(ISPF)` and `IKJACCNT`. |

Every dist install job has the same shape:
`DELOLD → RECVn (IKJEFT01 TSO RECEIVE of each xmit) → RECV (SMPREC, ++FUNCTION inline) → APPLYCHK → APPLY`,
with `CHANGE.ME.*` placeholders for the uploaded xmit names and a commented
`VOLUME()`. In Phase 1, RECEIVE with and without `VOLUME('PUB000')` both
landed on PUB000; set it anyway so placement is explicit.

### SMP level — OK

sysgen installs SMP 4.44 then takes ESY1400 to 4.48 with UR13349 UR15994
UR17644 UR19590 (`jcl/smpjob03.jcl:38-39`). RAKF documents exactly that
level. The other packages use only SMP4 statements (`RESETRC`, `RECEIVE
SELECT`, `APPLY CHECK`, `ACCEPT`, `LIST CDS/ACDS`, copy-style `++JCLIN`,
`++MOD LKLIB`, `++MAC`) and were run on stock MVS/CE by their authors (brexx,
sdsf). **Confirmed in Phase 1** on a full build: `HMASMP LVL 04.48`, ESY1400 +
the four URs `REC ACC`; TNIP800, UY16532, SYZJ201 `REC APP` (not accepted).

### Problems found

1. **sysgen step 8 (RAKF) is broken against RAKF HEAD.** `sysgen.py`
   `step_08_rakf` passes `-x rakf-tools.xmit --recv370` and downloads the
   `rakf-tools.xmit` release asset. Current `generate_release.py`
   (MVS-sysgen/RAKF == mvslovers/RAKF == `a4d101e`) has neither flag and the
   tools are now SMP source assembled on target. Also git_clone() follows
   HEAD; pin it to a tag in `mvslovers.conf` like the other products
   (today `v2.0.0` == HEAD == `a4d101e`). `-u`/`-p` are resolved relative to
   the RAKF directory, so pass absolute paths (sysgen already does).
2. **TSO RECEIVE ordering loop.** Every dist uses TSO `RECEIVE`, which on
   MVS/CE comes from NJE38 (step 11, installed by MVP, which needs RAKF, which
   runs BREXX, which must now be RECEIVEd). Break it by installing the
   standalone **TRANSMIT** package (`MVP/packages/TRANSMIT`, plain JCL, part
   of NJE38) directly in step 7, before BREXX. **Verified in Phase 1:** it
   works without RAKF, but RECEIVE needs APF authorization, which it gets from
   the `IKJEFTE2` usermod TRANSMIT applies — that only takes effect after an
   **IPL with CLPA**. Before the CLPA, RECEIVE ends RC 8 with `The RECEIVE
   command is not APF-authorized`. (It is not SVC 244 that makes it work.)
3. **`BREXX.CURRENT.RXLIB` is hard-coded everywhere**: logon CLIST
   (`jcl/sysgen05.jcl:529`), the `MVP` proc (`jcl/sysgen05.jcl:777`), RAKF's
   install stream and tools, ~10 MVP packages. BREXX 3.1 uses
   `BREXX.RXLIB`. Keep the name working with
   `DEFINE ALIAS(NAME(BREXX.CURRENT.RXLIB) RELATE(BREXX.RXLIB))` (the current
   `jcl/brexx.jcl` already does this trick for V2R5M3).
4. **BREXX moves out of SYS2.LINKLIB.** Today MVS/CE copies the *APF*
   V2R5M3 build (`APFLLIB`) into `SYS2.LINKLIB`. 3.1 lives in
   `BREXX.LINKLIB`, is not authorized, and privileged functions are gated by
   RAKF FACILITY profiles. It must be in LNKLST **before** `SYS2.LINKLIB`
   (MVP runs `IKJEFT01 PARM='BREXX …'`). Recommended: usermod **MS00100**
   (PRE `TNIP800`, which sysgen applies) + `BREXX.LINKLIB(******)` or
   `(MVSRES)`. Check MVP and MVP packages for reliance on the APF build.
5. **No FTP/mvsMF during sysgen** → transfer xmits via the reader (see
   "Transport" below).
6. **Volumes/space.** Dist alloc jobs default to `TGTVOL=MVSRES`,
   `DLBVOL=SMP000` (3350s). Free after a full build (Phase 1): MVSRES 276
   cyl, SMP000 158 cyl, PUB000 824 cyl, PUB001 391 cyl.
7. Upstream release lag: ufsd/httpd releases still accept in the inst job;
   `main` splits accept out. The installer must handle both (accept job
   present or not).

### What MVP loses

`UFSD`, `FTPD`, `HTTPD`, `MVSMF` packages (+ their `cache`/`desc` lines).
Only these four depend on each other; nothing else in MVP depends on them.
`MINIZIP` and `NJE38` stay (other users). The old packages also did
site config that the SMP packages deliberately do not (SMP doesn't own
PROCLIB/PARMLIB): STC procs into `SYS2.PROCLIB`, `*PRM0` members into
`SYS2.PARMLIB`, six UFS disks (`UFSD.ROOT`, `HTTPD.WEBROOT`, user homes,
`UFSD.SCRATCH`), RAKF users/profiles. sysgen must take that over (Phase 5).

---

## Design

### Package source ("build")

Versions are **pinned** in a manifest `mvslovers.conf` (INI, like
`users.conf`), committed to the repo, so every sysgen build is reproducible:

```
[ufsd]
repo = mvslovers/ufsd
version = 1.5.2
asset = ufsd-1.5.2-dist.tar.gz
sha256 = <from the release's SHA256SUMS>
```

sysgen downloads each pinned `-dist.tar.gz` from its GitHub release and
verifies the sha256 (no mbt needed on the build host).

**`./sysgen.py --update-mvslovers`** moves the pins to the newest releases
and exits without building:
- for each product, query `releases/latest` (GitHub API, same as
  `github_release_asset`), find the `*-dist.tar.gz` asset, download the
  release's `SHA256SUMS`, take the sum from there;
- rewrite `mvslovers.conf` in place (keep comments/order), print a
  `product: old -> new` table, change nothing if already current;
- `--update-mvslovers ufsd,httpd` updates only the named products;
- the user reviews the diff and commits it — bumping the stack is a
  normal commit, so a release of sysgen says exactly what it ships.

Overrides for development:
- **`--mvslovers-dist DIR`:** use already-built dists (e.g. from
  `mbt package` on `main`) instead of the pins.

Record what was installed (product, version, FMID) in
`SYS1.PARMLIB(RELEASE)` / the sysgen log.

The installer must still not hard-code FMIDs or job layout: a version bump
can change both (ufsd/httpd releases accept inside the inst job today; their
`main` splits accept into its own job). Read them from the dist.

RAKF is pinned the same way: `[rakf]` with `ref = v2.0.0` (a git tag, since
RAKF is installed from a clone via `generate_release.py`);
`--update-mvslovers` moves it to RAKF's newest release tag.

### Transport (no FTP/mvsMF)

Generate on the host, per product, one jobstream submitted to the **EBCDIC
reader (3506)** — the same mechanism RAKF already uses:

1. For each `.xmit`: `IEBGENER` with `SYSUT1 DD DATA,DLM=xx` followed by the
   raw 80-byte EBCDIC records, `SYSUT2` = `SYSGEN.<P>.<n>.XMIT`
   (`RECFM=FB,LRECL=80,BLKSIZE=3120`). Choose a `DLM` that no record starts
   with (scan; fail if none).
2. The dist's alloc job, then inst job, with: job cards replaced by
   `POST_RAKF_JOB_CARD`; `CHANGE.ME.*` → the step-1 names;
   `/* VOLUME('CHANGE.ME') */` → `VOLUME('PUB000')`.
3. The accept job (if the dist has one) — a fresh sysgen should end with
   everything `REC APP ACC` so later PTFs can RESTORE.
4. A cleanup step deleting the `SYSGEN.<P>.*.XMIT` staging sets.

JCL is ASCII→EBCDIC converted on the host (cp037), xmit bytes passed through
untouched. Fallback if in-stream binary misbehaves: `devinit 01c <file>.xmit`
and IEBGENER from `UNIT=01C` (what `jcl/brexx.jcl` does today).

Verification is by **member listing + `LIST CDS SYSMOD()`**, not just CC —
every product guide warns that `NOT SEL` ends RC 0 with nothing copied. Add a
`check_smp_installed(fmid, members)` helper that parses the printer output for
`HMA2380 COPY SUCCESSFUL` / `STATUS = REC APP`.

### Step order (new)

| Step | Change |
|---|---|
| 1–6 | unchanged |
| 7 customization | `brexx` sub-step replaced: TRANSMIT pkg → BREXX 3.1 SMP → MS00100 → LNKLST00 edit + `BREXX.CURRENT.RXLIB` alias + RXTSO/RXBATCH into SYS2.PROCLIB → IPL CLPA |
| 8 RAKF | new `generate_release.py` interface, pinned tag, profiles for the stack |
| 9 MVP | unchanged mechanism; MVP repo no longer carries the four packages |
| 10 extras | + ZMG0001 (ZP60038 rework in step 5) — done in Phase 7 |
| 11 NJE38, 12 ISPF | unchanged (NJE38 re-installs TRANSMIT/RECEIVE — check it tolerates existing members) |
| **12a mvslovers** (new) | UFSD → FTPD → HTTPD → mvsMF, then site config |
| **12b sdsf** (new) | SDSF + APF/LNKLST + ISPF clist/IKJACCNT + IPL CLPA |
| 13 customize | `customize.jcl` keeps only OPNTERSE (done in Phase 5) |
| 14 cleanup | unchanged |

Backup names: keep the existing numbering, add `36a_MVSLOVERS`, `36b_SDSF`
and change step 13's restore point.

---

## Phases

### Phase 1 — Baseline facts on a real system (no code changes yet)

Inputs: a completed MVS/CE build from current `main` (or the latest release),
this file.

- [x] `LIST CDS SYSMOD(ESY1400,UR13349,UR15994,UR17644,UR19590,TNIP800,UY16532,SYZJ201)` — confirm levels/status.
- [x] Free space on MVSRES, SMP000, PUB000, PUB001 (`IEHLIST LISTVTOC` or `DISKMAP`).
- [x] Confirm `step_08_rakf` fails against RAKF HEAD (expected: argparse error) — just run `generate_release.py` with sysgen's args.
- [x] On a step-7 backup (before RAKF): submit `MVP/packages/TRANSMIT` and a batch `RECEIVE` of any xmit. Does it need an IPL/CLPA? Does it work without RAKF?
- [x] Prove the in-stream transport: one job that IEBGENERs `brexx370-3.1.0-load.xmit` in-stream via 3506 and RECEIVEs it.
- [x] Grep `MVP.rexx` and all MVP packages for anything that needs **authorized** BREXX (privileged functions); list them.
- Done when: the answers above are recorded in the Phase log.

### Phase 2 — Host tooling (pure Python, testable offline)

Inputs: Design sections above; `sysgen.py` helpers `submit_file_binary`,
`submit_file_binary_post_rakf`, `POST_RAKF_JOB_CARD`, `github_release_asset`.

- [x] `mvslovers.conf` manifest pinned to the current releases (version, asset, sha256; RAKF tag).
- [x] `--update-mvslovers [p1,p2]`: resolve latest releases, rewrite the pins, print old -> new, exit.
- [x] `mvslovers.py` module: `fetch(product)` (download pinned dist + sha256 check, or `--mvslovers-dist` dir), `discover(dist_dir)` (alloc/inst/accept/xmits, tolerate missing accept), `build_jobstream(product, user, password) -> bytes` (Transport steps 1–4).
- [x] CLI: `python3 mvslovers.py ufsd -o out.ebc` to inspect output; a `--ascii` dump for review.
- [x] Unit tests: DLM selection, CHANGE.ME/VOLUME patching, card length = 80, job card replacement, every product's dist parses.
- Done when: tests pass for all six dists and the generated JCL reads correctly in the ASCII dump.

### Phase 3 — BREXX 3.1 via SMP (step 7)

Inputs: brexx370 `docs/books/guide/ug-install.typ` (install, link list Way 1,
TSO setup); `jcl/brexx.jcl`; `sysgen.py` `brexx()`; `jcl/sysgen05.jcl`.

- [x] Install TRANSMIT (direct submit of a vendored copy of the MVP TRANSMIT JCL) before BREXX, **then IPL with CLPA** before any RECEIVE.
- [x] Replace `brexx()` with the Phase-2 installer for `brexx370`.
- [x] Vendor MS00100 as `usermods/MS00100.jcl` (from prince-webdesign ms00100.zip; check redistribution terms with Rob Prins) and apply it here, or in step 5 with the other usermods (it only needs TNIP800).
- [x] LNKLST00: add `BREXX.LINKLIB(MVSRES),` above `SYS2.LINKLIB` (edit `jcl/sysgen04.jcl` only if the IPLs between step 4 and 7 tolerate a missing entry — otherwise edit LNKLST00 in this step).
- [x] `DEFINE ALIAS(NAME(BREXX.CURRENT.RXLIB) RELATE(BREXX.RXLIB))`.
- [x] Copy RXTSO/RXBATCH from `BREXX.PROCLIB` into `SYS2.PROCLIB`; `SYS2.EXEC` still created.
- [x] Delete `xmi/brexx.xmi`, `jcl/brexx.jcl`; drop V2R5M3 references.
- [x] RAKF FACILITY profiles BREXX needs (ug-install-facility) → `profiles.conf` (Phase 4). *Done in Phase 4: only SVC244/DIAG8CMD, already present.*
- Done when: after IPL CLPA, `PGM=BREXX`/`REXX`/`RX` without STEPLIB print `V3R1M0`, and `LIST CDS SYSMOD(TBRX310)` is `REC APP ACC`.

### Phase 4 — RAKF 2.0.0 (step 8)

Inputs: RAKF `README.md` (Installation, 2.0.0 notes), `generate_release.py
-h`, `sysgen.py step_08_rakf`, `users.conf`, `profiles.conf`.

- [x] Add a `ref=` param to `git_clone` and clone RAKF at the pinned tag from `mvslovers.conf`.
- [x] Drop `github_release_asset(... rakf-tools.xmit)`, `-x`, `--recv370`; add `--userid`/`--password` if the job card needs them.
- [x] Use the `check_maxcc("RAKFINST", steps_cc=…)` measured in Phase 1 (see Phase log); re-check it when the RAKF pin moves.
- [x] Confirm the stream's `PGM=BREXX` steps work (needs Phase 3 link list + alias).
- [x] `profiles.conf`: add `FTPAUTH`, `JESSPOOL *`, `SDSF.OPER`, `MVSMF.CONSOLE`, BREXX FACILITY profiles; STC user ids for UFSD/FTPD/HTTPD in group `STARTED`/`STCGROUP` with READ on `SVC244` (see ftpd `docs/FTPD_RAKF_SETUP.md`, httpd install §8).
- [x] Check `users.conf` → `rakf_users.txt` format still matches 2.0.0 (`generate_release.py` hashes passwords into the shadow file).
- Done when: step 8 runs clean, logon with each `users.conf` user works, `LISTUSER`/`LISTP` work.

### Phase 5 — mvslovers stack step (UFSD, FTPD, HTTPD, mvsMF)

Inputs: each product's `docs/installation.md` (sections: Install, procedures &
configuration, RAKF, start/verify); old MVP `desc/UFSD|FTPD|HTTPD|MVSMF` for
the config they used to create; `ufsd-utils` README.

- [x] New `step_12a_mvslovers` (after ISPF, before customize) running the Phase-2 installer for ufsd, ftpd, httpd, mvsmf in that order.
- [x] Site-config job: copy procs from each `*.SAMPLIB` to `SYS2.PROCLIB`, `*PRM0` to `SYS2.PARMLIB`; mvsMF into HTTPD STEPLIB + routes in `HTTPPRM0`.
- [x] UFS disks: build images on the host with `ufsd-utils` (root, scratch, one home per `users.conf` user) + `httpd-webroot.img`; transfer like xmits; add to `UFSDPRM0`.
- [x] Decide APF vs SVC 244 for UFSD/FTPD/HTTPD (default: SVC 244, no APF entries) — document.
- [x] Not started by default (matches current README), or add to COMMND00 — decide.
- [x] Smoke test in sysgen: start UFSD/HTTPD/FTPD, look for `UFSD007I`, `HTTPD001I`, FTPD ready, then stop.
- Done when: all four `REC APP ACC`, servers start, `curl :8080/zosmf/info` answers, FTP login as MVSCE01 works.

### Phase 6 — SDSF

Inputs: sdsf `docs/installation.md`.

- [x] `step_12b_sdsf`: installer for sdsf; IEAAPF00 + LNKLST00 entries for `ISF.LINKLIB MVSRES`; `ISF.ISFPLIB` into `SYS1.CMDPROC(ISPF)` ALLOC; `ISF.ISFCLST` into `SYS1.PROCLIB(IKJACCNT)` SYSPROC; IPL with CLPA (`custjobs_ipl(clpa=True)`).
- [x] Profiles `SDSF.OPER` (Phase 4).
- Done when: `TSO SDSF` works in ISPF, `LOG` and `/D T` work for an ADMIN user.

### Phase 7 — Extra usermods

- [x] ZMG0001 from the brexx370 release (`ZMG0001.xmit`) — install via Phase-2 transport + its `$INSTALL`/`ZMG01AP` jobs (assembled on target). Note UY16532 is applied-not-accepted on MVS/CE; accept it first if ZMG0001 must be removable. *Step 10; UY16532 accepted.*
- [x] ZP60038: sysgen already applies an older one; replace `usermods/ZP60038.jcl` with the current level (brexx ISPF VPUT/VGET). Check sha256 from the guide. *REWORK(20261106), zip sha256 matches.*
- [x] Re-check the RAKF `USERMODS/` (`JLM0003`, `RAK0001`, `ZJW0003`) don't collide with sysgen's own `JLM0003`. Phase 1: RAKF's `RESTORE` of JLM0003 says `HAS NOT BEEN APPLIED`, then re-receives and applies its own JLM0003 (RC 0) — find out what that does to sysgen's JLM0003. *RAKF's replaces it (superset table); documented + checked in step 10.*
- Done when: `EXEC` of a REXX member runs BREXX; LIST shows usermods applied.

### Phase 8 — MVP and docs

Inputs: `MVP/` repo, `jcl/customize.jcl`, `README.md`.

- [x] MVP repo: remove `packages/` + `desc/` + `cache` lines for UFSD, FTPD, HTTPD, MVSMF (use the `mvp-package` skill's rebuild flow for cache). Consider leaving a stub desc that says "installed with SMP on MVS/CE; see <repo>".
- [x] Fix MVP packages that relied on APF BREXX (from Phase 1 list: `MVP.rexx`, `NJE38`, `RANDOMPW`; RAKFCL is no longer installed).
- [x] Upstream the Phase 5 MVP fixes (ADDUSER, `CHECK_DEPENDS`, `SUBMIT_WORK_JCL`, NJE38's NJE0001 APPLY). NJE38 must **drop** the NJE0001 APPLY, not RESTORE RAKF's JLM0003 to make room: RAKF's JLM0003 SUPs NJE0001 and its IKJEFTE2 table is the only one with LISTUSER/LU/LISTP (Phase 7).
- [x] SHUTDOWN/SHUTFAST clash (ZMG0001, Phase 7): `jcl/finalize.jcl` puts REXX `SHUTDOWN`/`SHUTFAST` into `SYS2.EXEC` (the logon SYSEXEC) and CLISTs of the same names into `SYS1.CMDPROC`. With ZMG0001 the SYSEXEC member wins, so TSO `SHUTDOWN` runs the 5-minute countdown inside the user's session instead of submitting `SYS2.JCLLIB(SHUTDOWN)`. Move the two execs to a library that is not a SYSEXEC (and change `EXECLIB=` of the `SHUTDOWN` proc in `jcl/sysgen05.jcl`), or rename them; test with step 14.
- [x] `customize.jcl`: OPNTERSE only. README: the new step list, what is installed, SMP verification commands, how to upgrade (download new dist, run inst + accept).
- Done when: MVP `list` no longer shows the four; `README.md` matches the build.

### Phase 9 — Full build and release

- [x] `until ./sysgen.py --CONTINUE; do …; done` from scratch, then Docker image build. *Steps 1–6 on `dd62a74`, 7–14 continued on `84dffc7` (see log).*
- [x] Acceptance: `LIST CDS` of every FMID, member counts, server smoke tests, logon each user, `RX MVP list`. *Plus SHUTFAST as admin/MVSCE02 and a scripted 3270 ISPF + SDSF session.*
- [ ] Tag a release. *Will be `v3.1.0`, after one more major upgrade (decided 2026-11-15).*

---

## Open decisions (defaults chosen; change here if wrong)

- Accept in sysgen: **yes** (clean `REC APP ACC`).
- Source of packages: **pinned releases** in `mvslovers.conf`; `--update-mvslovers` bumps them to the newest releases; `--mvslovers-dist` for local builds.
- Target volume: dist defaults (`MVSRES`/`SMP000`) unless Phase 1 shows no space.
- Servers auto-start: **no** (as today).
- Authorization: **SVC 244 via RAKF**, no APF entries except SDSF (required).

## Phase log

(append: date, phase, what was done, what's next, gotchas)

### 2026-11-13 — Phase 1 done

Tested on scratch copies of `backup/38_FINAL.tar` (full build) and
`backup/31_BREXX.tar` (step 7, before RAKF); originals untouched. Ports
3505/3506/3270.

1. **SMP levels** (`LIST CDS`/`LIST ACDS`, `HMASMP LVL 04.48`):

   | SYSMOD | CDS | ACDS |
   |---|---|---|
   | ESY1400, UR13349, UR15994, UR17644, UR19590 | REC ACC RGN | REC ACC RGN |
   | TNIP800, UY16532, SYZJ201 | REC APP | not found |

   No SMP change needed. ZMG0001 (Phase 7) needs UY16532 accepted if it
   must be removable.
2. **Free space** (IEHLIST): MVSRES 276 cyl, SMP000 158 cyl + 26 trk,
   PUB000 824 cyl + 22 trk, PUB001 391 cyl + 25 trk. Dist defaults are fine.
3. **RAKF:** sysgen's args fail: `generate_release.py: error: unrecognized
   arguments: -x rakf-tools.xmit --recv370` (rc 2). New interface works:
   `generate_release.py -u <abs users> -p <abs profiles> -o out.ebc
   --userid IBMUSER --password SYS1` → 25,022 EBCDIC cards (`$$` DLM for the
   shadow). Real run on 31_BREXX (CLPA IPL, submitted to 3506): job
   `RAKFINST`, 53 steps, all `0000` except:

   ```
   steps_cc = {'DELETE': '0008', 'APPLYR': '0004', 'RAK00013': '0012',
               'RAK00014': '0012', 'ZJW00031': '0012', 'ZJW00032': '0004',
               'RESTORE': '0012', 'RECEIVE': '0012'}
   ```

   The RC 12s are defensive RESTORE/REJECT of usermods not yet on the system
   (`RAK0001`, `ZJW0003`, `JLM0003`: `HMA2462 NOT FOUND ON SMPPTS` /
   `HMA2452 HAS NOT BEEN APPLIED`); the real TRKF200 RECEIVE/APPLY/ACCEPT
   are RC 0/4 (`ZJW00032` 4 = `HMA3190 ZJW0003 DOES NOT PRE OR SUP ...
UMID TMVS804`). All `PGM=BREXX` steps
   RC 0 with V2R5M3. `ACCEPTR` runs (`COND=(4,LT,APPLYR)`).
4. **TRANSMIT before RAKF:** submitted `MVP/packages/TRANSMIT` as-is to 3505
   on 31_BREXX: every step RC 0 (assembles into `SYS2.CMDLIB`, SMP applies
   the `IKJEFTE2` usermod). Batch RECEIVE right after (no IPL): RC 8,
   `The RECEIVE command is not APF-authorized`. After IPL with CLPA: works.
   No RAKF needed. **Order for step 7: TRANSMIT → IPL CLPA → RECEIVE.**
5. **In-stream transport works.** One job to 3506: IEBGENER `SYSUT1 DD
   DATA,DLM=AA` + the 11,504 raw records of `brexx370-3.1.0-load.xmit` →
   `SYSGEN.BRXLOAD.XMIT` (FB 80/3120) RC 0, then `RECEIVE INDSN(...)
   DATASET(...)`: `NJE38 RECEIVE v2.3.0 / Receiving 'BREXX370.V3R1M0.LINKLIB'
   ... successful`; members `BREXX ALIAS(REXX,RX)`, `BRXVSMIO`, `BRXVSMTR`,
   `BRXVTOC`. Same result with and without `VOLUME('PUB000')`. No fallback
   to `devinit` needed. Dist sha256 matched `SHA256SUMS`.
6. **MVP vs authorized BREXX** (3.1 gates these by FACILITY profiles, not APF):
   - `MVP.rexx`: `ADDRESS COMMAND 'CP SH MVP/MVP …'` (DIAG8CMD), `MTT()`
     (SVC244).
   - `NJE38` package: `ADDRESS COMMAND` (DIAG8CMD).
   - `RAKFCL`: `CONSOLE()` — **not installed any more, skip**.
   - `profiles.conf` already gives ADMIN READ on `DIAG8CMD` and `SVC244`, so
     MVP run as IBMUSER keeps working; a non-ADMIN MVP user would get error 43.
   - `RANDOMPW` writes a member into `BREXX.CURRENT.RXLIB` — with 3.1 that
     is the SMP-owned `BREXX.RXLIB`; consider a separate library.
   - No use of removed/renamed 3.1 functions (`LISTCAT()`, `LISTALC()`,
     `CALL PRINT`, `IRXVTOC`, `PUTSMF`, `IRXEXCOM`).
   - `BREXX.CURRENT.RXLIB` hard-coded in `MVP` proc, `RANDOMPW`, `BSPHRCMD`,
     `STEPLIB`, `NJE38`, `EDIT`, `AUPGM`, `SHOWSS` (alias covers it).

**Changes for later phases:** step 7 needs a CLPA IPL between TRANSMIT and
BREXX (Phase 3); step 8 `steps_cc` above (Phase 4); RAKF re-applies its own
JLM0003 (Phase 7); RAKFCL dropped from Phase 8.

**Harness gotchas** (for whoever drives Hercules outside sysgen.py):
`sysgen.py` runs `main()` on import; 38_FINAL auto-starts JES2 (no
`$HASP426`, no `$HASP099` — wait for `$HASP373 TSO`); JES2 pads job names in
`$HASP395` (`P1LIST   ENDED`), match `IEF404I <job> - ENDED`; `$p jes2`
never ends while TSO/VTAM run; Hercules restarts truncate `prt00e.txt`;
JCL cards must stay within column 71.

Next: Phase 2.

### 2026-11-13 — Phase 2 done

Pure host code, no Hercules, sysgen step code untouched.

- **`mvslovers.conf`**: brexx370 3.1.0, ufsd 1.5.2, ftpd 1.3.4, httpd 4.5.1,
  mvsmf 1.4.1, sdsf 1.2.0 (`-dist.tar.gz` + sha256), `[rakf] repo =
  mvslovers/RAKF`, `ref = v2.0.0`. **Gotcha:** the httpd 4.5.1 and mvsmf
  1.4.1 `SHA256SUMS` list only the PDFs, not the dist; those two sums were
  computed from the download and are marked `# computed:` in the file.
  `--update-mvslovers` does the same (with a warning) when a release's
  SHA256SUMS lacks the dist.
- **`sysgen.py`**: only `main()` changed — `--update-mvslovers [p1,p2]`
  (calls `mvslovers.update_manifest`, prints `product old -> new`,
  exits) and `--mvslovers-dist DIR` (stored as `mvsce.mvslovers_dist`, not
  used by any step yet). RAKF update = `releases/latest` tag_name.
- **`mvslovers.py`** (must not import sysgen: it runs `main()` on import,
  so the job card is a copy of `POST_RAKF_JOB_CARD`):
  - `fetch(product, conf, dist_dir, cache=temp/mvslovers)` → unpacked dist
    dir; download from `releases/download/v<ver>/<asset>`, sha256 check,
    safe tar extract. `dist_dir` accepts an unpacked dist or a tarball.
  - `discover(dir)` → `Dist(product, version, fmid, alloc, inst,
    accept|None, xmits)`. FMID from `++FUNCTION`; `CHANGE.ME.<x>` is mapped
    to the xmit named in the `//* <file>.xmit` comment before its RECEIVE.
    Fails on an unmapped CHANGE.ME or an xmit the inst job doesn't use.
  - `build_jobs(dist, user, password, volume='PUB000')` → list of `Job`:
    `<P>XMIT` (IDCAMS delete old staging + one IEBGENER per xmit, `SYSUT1
    DD DATA,DLM=xx`, raw records, `SYSUT2 SYSGEN.<P>.<x>.XMIT` FB/80/3120
    UNIT=SYSDA TRK sized for a 3330), dist alloc, inst, accept (if any),
    `<P>CLN` (explicit DELETE per staging set — no generic delete on 3.8).
    Job cards replaced (USER/PASSWORD omitted when `user=None`, i.e. before
    RAKF); `/* VOLUME('CHANGE.ME') */` → `VOLUME('PUB000')`, and a VOLUME
    line is **inserted** for ufsd/httpd whose RECEIVEs have none. DLM = first
    of `AA, AB … Z9, @@, $$, ##` that no record starts with (EBCDIC); brexx
    rxsamp needs `AB`.
  - `build_jobstream(product, user, password)` = all jobs as cp037 bytes.
  - Job names: BREXXMIT/BREXXALC/BREXXINS/BREXXACC/BREXXCLN, UFSD*,
    FTPD*, HTTPXMIT/HTTPD*, MVSMXMIT/MVSMF*, SDSF*.
- **CLI:** `python3 mvslovers.py ufsd [-o x.ebc] [--ascii] [--user U
  --password P] [--dist DIR] [--update [p1,p2]]`. Card counts:
  brexx370 36,571, ufsd 5,407, ftpd 5,518, httpd 11,055, mvsmf 5,963,
  sdsf 8,214.
- **Tests:** `python3 -m unittest discover -s tests` (23 tests). Real-dist
  tests use `temp/mvslovers/*.tar.gz` and skip if not cached.

**For Phase 3:** the jobs are separate JOBs; submit them **one at a time**
and wait (`IEF404I`/`check_maxcc`) before the next — JES2 may run
same-class jobs in parallel, and alloc must finish before inst. For
BREXX in step 7 pass `user=None` (pre-RAKF). Unverified on MVS:
staging `UNIT=SYSDA` (no VOL=SER) placement, and whether the
brexx accept job's own CLEANUP overlaps `BREXXCLN` (harmless if so).

Next: Phase 3.

### 2026-11-13 — Phase 3 done

Ran for real in a copy of the tree (`../sysgen-p3test`, own copy of
`backup/30_MVS02.tar`; originals untouched):
`./sysgen.py --step step_07_customization --substep brexx`, twice (second
run with the final code). Every step of every job RC 0000 (48 steps), all
checks below pass, new `31_BREXX` written; step 8 then fails on the known
RAKF `rakf-tools.xmit` problem (Phase 4).

**`brexx()` now** (`sysgen.py`), from `30_MVS02`:
1. IPL (no CLPA): `jcl/transmit.jcl` (job `TRANSMIT`, vendored
   `MVP/packages/TRANSMIT` @ MVP af59943, USER/PASSWORD removed), then
   `jcl/brexxpre.jcl` (`BRXXPRE`: `DEFINE ALIAS(NAME(BREXX)
   RELATE(UCPUB001))` — BREXX.* stays out of the master catalog).
2. IPL **CLPA** (activates RECEIVE's APF via IKJEFTE2):
   `mvslovers.fetch('brexx370', dist_dir=self.mvslovers_dist)` →
   `build_jobs(dist, user=None)`; each job sent alone with
   `submit_binary()` to 3506, `$HASP099`, `check_maxcc(job.name)` — no
   `steps_cc` needed: BREXXMIT, BREXXALC (CHECK 0 on a fresh system),
   BREXXINS, BREXXACC (GUARD/ACCEPT/CLEANUP 0), BREXXCLN. Then
   `usermods/MS00100.jcl` (RECEIVE / APPLY CHECK / APPLY, not accepted:
   TNIP800 is only applied) and `jcl/brexxcfg.jcl` (`BRXXCFG`: IEBUPDTE
   REPL LNKLST00 with `BREXX.LINKLIB(MVSRES),` between SYS1.LINKLIB and
   SYS2.LINKLIB; `DEFINE ALIAS BREXX.CURRENT.RXLIB -> BREXX.RXLIB` in
   UCPUB001; IEBCOPY RXTSO/RXBATCH → SYS2.PROCLIB; allocate SYS2.EXEC).
3. IPL CLPA: `jcl/brexxver.jcl` (`BRXXVER`: PGM=BREXX/REXX/RX without
   STEPLIB `say 'BRXVER-<pgm>' version()`, TSO LISTDS MEMBERS of the three
   libraries, SMP `LIST CDS SYSMOD(TBRX310 MS00100)` + `LIST ACDS`), then
   `check_brexx(dist)`: all three print `V3R1M0`; BREXX.LINKLIB has BREXX
   ALIAS(REXX,RX), BRXVSMIO, BRXVSMTR, BRXVTOC; RXLIB 112 / SAMPLIB 119
   members (= guide); CDS TBRX310 `REC APP ACC` (ACDS `REC APP ACC RGN`),
   MS00100 `REC APP`. Backup `31_BREXX`.

New helpers: `submit_binary(bytes, port=3506)`, `check_brexx`,
`listds_members(dsn)`, `check_smp_status(sysmod, status)` (exact match,
CDS section only, last LIST wins) — reuse them in Phases 5/6.

Removed: `jcl/brexx.jcl`, `xmi/` (brexx.xmi + readme), V2R5M3 refs.
README step list updated. MS00100 vendored (owner OK'd redistribution;
the zip has no licence text — header credits Purdy/Moseley/Prins + URL).

**Gotchas:** the old APF V2R5M3 BREXX is simply no longer installed (no
SYS2.LINKLIB copy to remove on a fresh build). `LISTDS 'X' MEMBERS` echoes
the next command indented like a member — the parser stops at a line with
a quote. The logon CLIST and MVP proc keep `BREXX.CURRENT.RXLIB` (alias
covers it). Driving sysgen.py outside `main()`: exec the source without the
trailing `main()` and set `skip_steps` (see the Phase 1 harness notes).

**For Phase 4:** `31_BREXX` now has BREXX 3.1 *unauthorized* in the link
list — RAKF's stream `PGM=BREXX` steps were only ever run on V2R5M3 (APF);
check them, and that anything they do privileged is covered by FACILITY
SVC244/DIAG8CMD (which don't exist until RAKF itself loads its profiles).
The test copy `../sysgen-p3test` has a fresh `backup/31_BREXX.tar` to start
Phase 4 from without rebuilding.

Next: Phase 4.

### 2026-11-13 — Phase 4 done

Ran for real in `../sysgen-p4test` (copy of the p3test tree + its
`31_BREXX.tar`; originals untouched) by calling `step_08_rakf()` alone (the
harness execs `sysgen.py` without `main()`, so it does not run on into step 9).
Step 8 ran clean, `32_RAKF` written.

**Code:**
- `git_clone(repo, out_folder, ref=None)`: new clone gets `--branch <ref>`; an
  existing clone with a ref gets `remote set-url origin <repo>`, `fetch --tags
  --force`, `checkout --force --detach <ref>` (no pull). The old p3test clone
  was of MVS-sysgen/RAKF, and it switched cleanly to mvslovers/RAKF `v2.0.0` =
  `a4d101e`.
- `step_08_rakf`: reads `[rakf] repo/ref` from `mvslovers.conf`; no
  `rakf-tools.xmit`, `-x` or `--recv370`; runs `generate_release.py` with
  `sys.executable -I` and absolute `-u`/`-p`. **No `--userid/--password`**:
  RAKF is not active yet when RAKFINST runs. `check_maxcc("RAKFINST",
  steps_cc=RAKF_STEPS_CC)` (module constant) = the Phase 1 values. Re-measured
  here, they are identical (`DELETE` 8, `APPLYR` 4, `ZJW00032` 4, `RAK00013`,
  `RAK00014`, `ZJW00031`, `RESTORE`, `RECEIVE` 12). The old `ZJW00042` entry
  was dropped because no such step exists.
- `mvslovers.rakf_users(users_text, extra_admin)` replaces the inline
  users.txt writer (3 new unit tests, 26 total pass). It writes the RAKF 2.0.0
  layout, the same as RAKF's own `users.txt`: oper in col 28, **Special in
  col 30** (`Y` for ADMIN; the old writer left it blank, so in 2.0.0 nobody
  could have run LISTUSER/ADDUSER/PERMIT/...), and `*` only on the first entry
  of a user. It also adds the STC ids **UFSD, FTPD, HTTPD** in group
  `STARTED`, with Operations N, Special N and a **blank password**
  (generate_release.py writes no shadow record, so nobody can log on as them).
  It rejects ids over 7 characters, duplicates, and users.conf entries that
  reuse an STC id.
- `profiles.conf`, now sorted in EBCDIC order (letters before digits:
  `PAGELPA` < `PAGEL01`): removed the `FACILITYBRX*AUTH` lines (pre-3.1
  names; BREXX 3.1 checks only `SVC244` and `DIAG8CMD`, which ADMIN and
  STCGROUP already have). Added `FTPAUTH` (NONE; ADMIN and USER READ, so
  every normal user can FTP as before, and STC ids cannot), `MVSMF.CONSOLE`
  (NONE; ADMIN READ), `SDSF.OPER` (NONE; ADMIN UPDATE), `JESSPOOL *` (NONE;
  ADMIN UPDATE), and `SVC244` STARTED READ.

**BREXX 3.1 in RAKF's stream:** the `PGM=BREXX` steps (`EXECSORT` in
RAKFCUST, `EXECCAT`/`EXECCAT2` in VSAMSRAC, `EXEC` in VTOCSRAC) use only
EXECIO, PARSE and SAY, nothing privileged. All four ended RC 0 with the
unauthorized link-list BREXX and the `BREXX.CURRENT.RXLIB` alias. JLM0003
puts BREXX/REXX/RX in the authorized command table; that has no effect on an
unauthorized library (Phase 7 still checks the collision).

**Checks on `32_RAKF`** (IPL, one batch job per user with `USER=/PASSWORD=`,
i.e. RAKF RACINIT, the same check as a TSO logon; there is no s3270 on the
host for a scripted 3270 logon):
- IBMUSER, MVSCE01 and MVSCE02 run (`IEF404I ENDED`). A wrong password, and
  FTPD/HTTPD with any password or none, get `IEF722I INVALID PASSWORD GIVEN`.
- As ADMIN: `LISTUSER IBMUSER` shows `OPERATIONS, SPECIAL`; `LU MVSCE` shows
  MVSCE01 `OPERATIONS, SPECIAL` and MVSCE02 `NOOPERATIONS,NOSPECIAL`; `LU
  HTTPD`/`LU UFSD`/`LU FTPD` show `NOOPERATIONS,NOSPECIAL`. `LISTP SVC244`
  lists ADMIN, STARTED and STCGROUP READ; `LISTP *` shows `JESSPOOL * UACC=NONE`,
  ADMIN UPDATE; FTPAUTH, SDSF.OPER and MVSMF.CONSOLE are all defined. As
  MVSCE02, every command says `Not authorized, no SPECIAL attribute`.
- BREXX FACILITY gate: `SYSVAR('SYSCP')` in a batch `PGM=BREXX` step returns
  "failed, TSO required" for ADMINs and "not authorized" for MVSCE02, so the
  SVC244 check works. `version()` = `V3R1M0`.

**Gotchas:** `LISTUSER` with no operand is a parse error in batch, so give a
(partial) userid. `LISTP` matches the *profile name*, not the class: use
`LISTP *` for `JESSPOOL *`. A job refused by RAKF ends with `$HASP396 ...
TERMINATED`, not `$HASP395`, so wait for both. Do not use `pkill -f
<script>` from a shell whose command line contains the script name, because
it kills that shell.

**For Phase 5:** HTTPD's install docs default to `STCGRP=USER`. In the HTTPD
proc copied to SYS2.PROCLIB set `STCUSER=HTTPD,STCGRP=STARTED` (USER has no
SVC244). Confirm the STC mapping on the console when the servers start (FTPD
must *not* say `FTPD004I STC RUNS AS STC/STCGROUP`). `MVSMF.CONSOLE` and
`SDSF.OPER` are already defined, so Phase 5 and 6 need no RAKF work. Test tree
`../sysgen-p4test` has `backup/32_RAKF.tar`; steps 9–12 still need to be run
to get a 35/36 backup to start Phase 5 from.

Next: Phase 5.

### 2026-11-13 — Phase 5 done

Ran for real in `../sysgen-p5test` (copy of p4test; `backup/` of the repo
untouched): steps 8 → 12 from `31_BREXX`, then `step_12a_mvslovers` from
`36_ISPF` → new `36a_MVSLOVERS`. Final run, every job RC 0:

```
TUFS152, TFTP134, THTP451, TZMF141 are REC APP ACC
UFSD007I AUTHORIZED BY SVC (MODULE KEY 8)
UFSD040I 6 FILESYSTEM(S) MOUNTED
HTTPD002I AUTHORIZED BY SVC (MODULE KEY 8)
HTTPD004I STC IDENTITY SET TO HTTPD/STARTED VIA RACINIT
FTPD008I AUTHORIZED BY SVC (MODULE KEY 8)
FTPD004I STC RUNS AS FTPD/STARTED
GET http://127.0.0.1:8080/zosmf/info: zosmf_version 1, 1.4.1
GET /: 200 3573 bytes from /www
FTP login IBMUSER / MVSCE01 / MVSCE02 OK
```

**Step 12a** (`sysgen.py`, after step 12, restores `36_ISPF`, writes
`36a_MVSLOVERS`; step 13 now restores `36a_MVSLOVERS`):
1. Host: fetch/discover ufsd, ftpd, httpd, mvsmf (`MVSLOVERS_STACK`), fetch
   the pinned `ufsd-utils` binary, build the UFS images — before the IPL.
2. `custjobs_ipl(..., tcpip=True)`: enables `HERC_TCPIP_EXTENSION`,
   `HERC_TCPIP_PROB_STATE`, `herc_370_extension` **before** `ipl 150`
   (after IPL: `HHC00889E`, and HTTPD abends S0C1 on opcode X'75').
3. Per product `build_jobs(dist, admin, pw)`, one job at a time,
   `check_maxcc` each (`submit_mvslovers_job`). No `steps_cc` needed.
   ufsd/httpd 1.5.2/4.5.1 have no accept job (accept in inst), ftpd/mvsmf do.
4. `UFSDSKnn` per disk, `MLSCFG` site config, `MLSVER` (`LIST CDS/ACDS`),
   `check_smp_status(fmid, 'REC APP ACC')` for all four.
5. `mvslovers_smoke_test`: `S UFSD/HTTPD/FTPD`, checks the messages above
   (fails on `FTPD004I … STC/STCGROUP` or anything but `FTPD/STARTED`),
   `/zosmf/info` with basic auth, `GET /`, FTP login of every users.conf
   user, then `P` all three. Fails early if 8080/2121 are busy on the build
   host (Hercules' TCP/IP binds there).

**UFS disks** (`mvslovers.plan_ufs_disks`, names from the product guides):
`UFSD.ROOT` 1M `/`, `HTTPD.WEBROOT.UFS` (the dist's `httpd-webroot.img`)
`/www` RO, `<USER>.UFSHOME` 2M `/u/<user>` RW `OWNER(<USER>)` per users.conf
user (+ `--username`), `UFSD.SCRATCH` 2M `/tmp`. Images: `ufsd-utils create
--owner/--group` always given (1.0.1 defaults to the host user name).
Transport: `netdata_seq()` (own NETDATA writer, one PS dataset) wrapped as
**RECFM=F LRECL=BLKSIZE=4096**, staged in-stream, RECEIVEd to
`SYSGEN.UFSDSKnn.IMG`, then IEBGENER into the real disk `DSORG=PS,RECFM=U,
BLKSIZE=4096`, `SPACE=(4096,(n),,CONTIG)`, `DISP=(NEW,CATLG)` on PUB000 (an
existing disk is never overwritten). **NJE38 RECEIVE refuses RECFM=U data**
whatever LRECL/BLKSIZE (0, 4096, 8192): `The incoming file contains logical
records that exceed the LRECL of the dataset`. `UFSDPRM0` is generated
(`ufsdprm0()`), not copied.

**RAKF:** UFSD opens every disk for update; RAKF allowed that only under
its own HLQ (`HTTPD.WEBROOT.UFS`, `<user>.UFSHOME`: `IEC150I 913-38`). RAKF
grants only to groups, so UFSD got its own group **`UFSROOT`** (users.txt;
FTPD/HTTPD stay in `STARTED`): `profiles.conf` + `FACILITY SVC244 UFSROOT
READ`; step 8 now passes `temp/rakf_profiles.txt` = profiles.conf +
`rakf_profiles()` (per disk: universal READ + `UFSROOT UPDATE`, EBCDIC
sorted). A user added later needs `PERMIT <user>.UFSHOME ID(UFSROOT)
ACCESS(UPDATE)` + a MOUNT line.

**Site config `MLSCFG`:** IEBGENER `UFSD`, `UFSDCLNP`, `FTPD` →
SYS2.PROCLIB, `FTPDPRM0` → SYS2.PARMLIB (as shipped); `UFSDPRM0` in-stream;
a `PGM=BREXX` step edits `HTTPD.SAMPLIB(HTTPD)` (`STCGRP=USER` →
`STARTED`, `MVSMF.LINKLIB` first in STEPLIB) and checks HTTPPRM0 has the 4
`MOD=MVSMF` routes (appends MVSMFPRM's if none); exit 8 if the sample does
not match. **BREXX gotcha:** `parse var l a 'X' b` is Error 15 (`'…' b` read
as a binary string) — no `b`/`x` variable after a string.

**Decisions (documented):** SVC 244, no IEAAPF00 entries, no IPL. Servers
not started at IPL (no COMMND00 change); README says `/S UFSD`, `/S HTTPD`,
`/S FTPD`.

**Other changes:** `mvslovers.conf` `[ufsd-utils]` (version +
`sha256-<os>-<arch>`, computed: release has no SHA256SUMS);
`--update-mvslovers` handles it. `jcl/customize.jcl` → OPNTERSE only (MVP
would otherwise reinstall the old UFSD/FTPD/HTTPD/MVSMF over 12a). Fixed
`get_rakf_admin` / step 14 README table: parsed users.txt by `[18:-1]`,
which returns `     Y` with the 2.0.0 flags (`parse_rakf_users`). 39 unit
tests.

**Found on the way, not sysgen (MVP repo, upstream HEAD `36fa757`), patched
only in the test tree's `MVSCE/MVP` clone — must be fixed in MVP before a
real build passes step 9–12:**
1. `MVP` INSTALL_JOB: `CALL 'SYS2.CMDLIB(ADDUSER)'` — RAKF 2.0.0 puts
   ADDUSER in `SYS1.CMDLIB`, and as a CP it abends 0C4 under CALL. Use the
   TSO command `ADDUSER MVP PASSWORD(...) DFLTGRP(ADMIN) GROUP(RAKFADM)
   OPERATIONS`.
2. `MVP.rexx` CHECK_DEPENDS: BREXX 3.1 keeps the trailing blanks of FB
   records, so an empty `Depends:` loops into `CHECK_DEPENDS('')` →
   `MVP.PACKAGES()` RC 8. `DEPENDS = STRIP(DEPENDS)`, `DO WHILE
   WORDS(DEPENDS) > 0`.
3. `MVP.rexx` SUBMIT_WORK_JCL: same cause, `RIGHT(line,1) == ","` never
   matches, job card continuation broken (IEF605I). Strip trailing blanks
   first.
4. `packages/NJE38`: re-applies `++USERMOD(NJE0001)` that step 7's
   `jcl/transmit.jcl` already applied → APPLY RC 12 (`HMA2483`). Drop the
   APPLY or guard it.

**Gotchas:** the build host here had 8080 taken (a container); the test
ran in `unshare -rn` + nested `unshare --map-user=1000` (tar must chown).
One JES2 cold start hung after `format,cold,noreq` (0 MIPS) — killed and
reran, not seen again. `HTTPD026W UNRECOGNIZED CONFIGURATION LINE:` comes
from an empty line 82 in the shipped HTTPPRM0 (upstream, harmless). JES
blanks `PASSWORD=` in listings — not a bug.

**For Phase 6:** SDSF uses the same helpers (`build_jobs`,
`submit_mvslovers_job`, `check_smp_status`). Test tree `../sysgen-p5test`
has `backup/36a_MVSLOVERS.tar` and the patched MVP clone.

Next: Phase 6.

### 2026-11-13 — Phase 6 done

Ran for real in `../sysgen-p6test` (copy of p5test, its `36a_MVSLOVERS.tar`
and patched MVP clone; repo `backup/` untouched): `step_12b_sdsf()` alone
through the harness → new `36b_SDSF`. Final run, every check passes:

```
TISF120 is REC APP ACC, ISF.LINKLIB APF and link list, IGX00011 in the LPA,
IBMUSER has SDSF.OPER, MVSCE02 has not
```

Then IPL of `36b_SDSF` (no CLPA), `S NET`, 3270 logon as IBMUSER (by hand):
ISPF → `TSO SDSF` → `LOG` shows the system log, `/D T` → `IEE136I` on the
console. MVSCE02 was checked in batch only (RACHECK below), not over 3270.

**Step 12b** (`sysgen.py`, after 12a, restores `36a_MVSLOVERS`, writes
`36b_SDSF`; step 13 now restores `36b_SDSF`):
1. IPL (no CLPA): `build_jobs(sdsf, admin, pw)` one at a time,
   `submit_mvslovers_job(job, steps_cc)` with
   `mvslovers.SDSF_STEPS_CC`: SDSFINS `APPLY` 0004 (IEW0342, IGX00011 not
   yet in LPALIB), SDSFACC `ACCEPT` 0004 (HMA2471 AOST4), the rest 0000.
   **Gotcha:** for `EXEC SMPAPP` steps IEF142I prints `HMASMP APPLY`
   (procstep first), so `check_maxcc` already keys them by the step name;
   no change to `check_maxcc` was needed.
2. `SDSFCFG` (`mvslovers.sdsf_config_job`): a `PGM=BREXX` step reads
   IEAAPF00, LNKLST00, `SYS1.CMDPROC(ISPF)`, `SYS1.PROCLIB(IKJACCNT)`,
   writes edited copies to temp sets; four IEBGENER `COND=(0,NE)` put them
   back, four more print the result. Exit 8 (nothing written) if a member
   no longer looks as expected or already has the entry: list members must
   be `entry,` lines with only the last without a comma (cols 1-71; seq
   numbers in 73-80, the changed line loses its number); exactly one
   `ALLOC F(ISPPLIB) DA(...') SHR`; exactly one `SYS2.CMDPROC` DD right
   after `//SYSPROC`. Results: ` SYSC.LINKLIB SYSCPK,` + ` ISF.LINKLIB
   MVSRES` (TGTVOL of the alloc job = `mvslovers.SDSF_TGTVOL`);
   `SYS2.CMDLIB,` + `ISF.LINKLIB` (master catalog, no volume needed);
   `...'SYSGEN.ISPF.RFEPLIB', +` / `'ISF.ISFPLIB') SHR`; `//  DD
   DISP=SHR,DSN=ISF.ISFCLST` after SYS2.CMDPROC.
3. IPL **CLPA**: `SDSFVER` (as admin) — `LIST CDS/ACDS SYSMOD(TISF120)`,
   `LISTDS` of ISF.LINKLIB/ISFPLIB/ISFCLST, `RUNISF` = `PGM=ISFISP`
   without STEPLIB (RC 8 + `ISF001E ISFISP RUNS UNDER ISPF`; S806 would
   mean not in the link list), then the probe `SDSFPRB`; and `SDSFPRB` as
   the first users.conf `USER`-group user (MVSCE02). `check_sdsf()`.

**SDSFPRB** (`mvslovers.SDSF_PROBE_ASM`, assembled/linked/run in-job into
`&&LOAD`, PARM = user as tag): prints the APF table from storage
(`CVTAUTHL`: halfword count, then `AL1(len) CL6 vol dsn`), `ESR11 ON|OFF`
(SVC 109 ESR entry 11 != entry 0, as sdsf `tools/isfsvct.c`), and `RACHECK
ENTITY=CL39'SDSF.OPER',CLASS='FACILITY'` READ/UPDATE RCs — the check SDSF
itself makes (`isfauth.c auth_oper`). Before install (36a): `ESR11 OFF`, no
ISF entry. After: `APF MVSRES ISF.LINKLIB`, `ESR11 ON`, IBMUSER `READ=00
UPDATE=00`, MVSCE02 `READ=08 UPDATE=08`. Parsed by
`mvslovers.parse_sdsf_probe`. **Gotcha:** LKED/GO need `COND=(0,NE,ASM)` /
`((0,NE,ASM),(0,NE,LKED))`, not `(0,NE)`, because `RUNISF` ends RC 8.

**Why no real SDSF in batch:** SDSF has no batch mode (needs ISPLINK, else
`ISF001E`), and Wally ISPF does not run in batch. The LOG and `/D T` check
needs a 3270. In sysgen it is replaced by: ISFISP loads from the link list +
APF entry in storage + ESR 11 installed + RAKF's answer on `SDSF.OPER` per user.
Note that ADMIN also has `SVC244` READ, so an ADMIN passing LOG and `/D T`
alone does not prove that IGX00011 is used; `ESR11 ON` covers that.

**Tests:** 4 new (`SdsfTest`), 43 total. README step list has 12b.

**3270 gotchas** (for scripting a TSO session in a test tree): the
`customization2` config only has TSO terminals in device group `tso`
(`c3270 tso@127.0.0.1:3270`); `S NET` is not in COMMND00 at this stage
(finalize adds it), so wait for `IKT005I` / `$HASP373 TSO`; the screen stays
on the Hercules splash, locked, until a **Clear** AID.

**For Phase 7:** start from `../sysgen-p6test/backup/36b_SDSF.tar`.

Next: Phase 7.

### 2026-11-13 — Phase 7 done

Ran for real in `../sysgen-p7test` (copy of p6test with its patched MVP
clone). p6test had no step-5 backup, so `usermod_47_ZP60037.tar` was copied
(read only) from `sysgen/backup`, the same July build the earlier phases
started from. Also copied `dasd/`, which step 7 needs and the test trees
lacked. Run: step 5 from ZP60038 → 6 → 7 → … → 12 → 12a + 12b (12a/12b in
`unshare -rn` because 8080 is taken on this host). Every step passed, and
there is a new `36b_SDSF`. Then a final `UMODVER` on `36b_SDSF`: 71 usermods
+ UY16532 as expected, all steps RC 0, EXEC runs BREXX:

```
ZMGTEST IMPLICIT V3R1M0     %ZMGTST IMPLICIT          (SYSEXEC member)
ZMGTEST EXPLICIT V3R1M0     EXEC 'ds(ZMGTST)' ...     (/* REXX */ line 1)
ZMGTEST KEYWORD V3R1M0      EXEC 'ds(ZMGNOC)' ... EXEC (no comment, keyword)
ZMGTEST CLIST RAN           EXEC 'ds(ZMGCLST)'        (CLIST still a CLIST)
```

**ZP60038** (step 5, `usermods/ZP60038.jcl` replaced): the guide's sum
`d0120eaf…bc64` matches `zp60038.zip` from both prince-webdesign and the
brexx370 v3.1.0 release asset. The zip holds `zp60038.xmi` = USERMOD.ZP60038
(`$INSTALL`, `@CVRLET`, `IKJCT441`). It is now one job in the old shape:
in-stream IKJCT441 source → IFOX00 → IDENTIFY card appended to `&&OBJ`
(IEBGENER `DISP=MOD`) → `&&TXLIB(IKJCT441)` → RECEIVE (`@CVRLET` as is,
`TXLIB(IKJCT441)`) → APPLY CHECK → APPLY with `AGENLIB` DD. Result:
`REWORK(20261106)` received, `HMA2390 LINK SUCCESSFUL - MOD=IKJCT441 -
LMOD=IKJPTGT - LIBRARY=LPALIB`, `MUPD SGIKJ441 APPLIED`, RC 0 everywhere.
Same PREs as before (UY01301, ZP60014), so it stays in its place in step 5.
The generator is not in the repo; the JCL header records the source and the
sum. Extract the xmi with `unnum=False`, because the `./ CHANGE` of
SGIKJ441 needs its sequence numbers. **Gotcha:** IEBGENER `SYSUT1` = `&&OBJ`
(FB 3200) concatenated with an in-stream `DD DDNAME=ADDREC` ends `IEB311I
CONFLICTING DCB PARAMETERS` + S0C4 on MVS/CE, so append with `DISP=MOD`
instead. (The shipped ZMG01IN/ZMG01AP use exactly that concatenation and
**do** work, with `DSORG=PO`/no DCB on the MOD member; not chased further.)

**ZMG0001** (step 10, after ZP60041; decision: step 10, not step 7, because
it needs RECEIVE + BREXX in the link list, and step 10 is the "needs the
whole system" step; it runs with the RAKF admin as user):
- `mvslovers.conf [zmg0001]`: `repo` brexx370, `version` 3.1.0, `xmit` /
  `xmit-sha256`, `zip` / `zip-sha256` (both sums from SHA256SUMS). It is not
  a dist (`dist_products` skips it). `fetch_files('zmg0001')` downloads and
  checks the files; `--update-mvslovers` moves `version` and both sums
  together (`release_files()`).
- `mvslovers.zmg0001_jobs(files, user, pw)`, one job at a time with
  `submit_mvslovers_job`, `ZMG_STEPS_CC`:
  `ZMGXMIT` (stage `ZMG0001.xmit` in-stream → `RECEIVE … VOLUME('PUB000')
  DATASET('USERMOD.ZMG0001')`, LISTDS, delete stage; sysgen checks the 8
  members), `ZMGPREP` (IEBCOPY compress of `SYS1.CMDLIB` onto itself, then
  `ACCEPT SELECT(UY16532)` with CMDLIB/AOST4 DDs, RC 4 as the guide says),
  shipped `ZMG01CK` (LIST RC 4, sysgen checks UY16532 `REC APP ACC`),
  shipped `$INSTALL` = `ZMG01IN` (REJ 12 = HMA2462; sysgen checks `MOD
  IKJCT430/IKJCT437 APPLIED` in the apply check), shipped `ZMG01AP` (sysgen
  checks `HMA2390 … MOD=IKJCT430/437 - LMOD=EXEC … SYSMOD=ZMG0001`). Jobs
  come from `ZMG0001.zip`, with job cards replaced; the library on MVS comes
  from the xmit.
- **Decision: accept UY16532** (the guide's "On MVS/CE, update TSO first"),
  so `ZMG01RS`'s RESTORE can take ZMG0001 off. That is needed for a later
  BREXX release with a new ZMG0001. ZMG0001 itself stays `REC APP` (cover
  letter: DO NOT ACCEPT). `ZMG01BK` (IEBCOPY backup of EXEC/EX) is left out:
  with UY16532 accepted, RESTORE is the way back. `USERMOD.ZMG0001` stays
  on PUB000 (master catalog) for `ZMG01RS`/reinstall.
- SYS1.CMDLIB: one extent before and after (compress + apply leave it at 6
  free tracks), so no new extent and no IPL needed. Step 10 still verifies
  after a re-IPL.
- Then IPL → `UMODVER` (`mvslovers.usermods_verify_job`): `LIST CDS` of
  every usermod (`sysgen.all_usermods()`: `++USERMOD` ids parsed from the JCL
  sysgen submits — step 5 list, DYNPROC, transmit, MS00100, ZP60041 —
  plus RAKF's 3 and ZMG0001; `#DYPDMY` left out, DYNPROC restores it on
  purpose), LIST ACDS UY16532, LISTVTOC of SYS1.CMDLIB, and the EXEC test
  above in a batch TMP (`SYSGEN.ZMGTEST.EXEC`, created and deleted).
  `check_usermods`: all `REC APP`/`REC APP ACC`, ZMG0001 exactly `REC APP`,
  UY16532 `REC APP ACC`, JLM0003 has `SUPING NJE0001` (= RAKF's), NJE0001
  `SUPERSEDED`.

**JLM0003 collision — what happens** (RAKFINST listing, step 8 rerun):
`RESTORE S(NJE0001,JLM0003)` **succeeds**. It takes both sysgen's JLM0003
(step 5) and NJE0001 (TRANSMIT, step 7) off the system, and IKJEFTE2 is
relinked from AOST4. The next `RESTORE S(JLM0003)` ends HMA2452 (RC 12, the
message Phase 1 saw) and `REJECT` HMA2462. Then RAKF's JLM0003
`REWORK(20261008) SUP(NJE0001)` is received and applied into
`IKJEFT02`/LPALIB (RC 0, active after the step-8 CLPA). RAKF's table =
sysgen's names + NJE0001's (RECEIVE, RECV, TRANSMIT, XMIT) + LISTP,
LISTUSER, LU, RPFV, NJE38, so nothing is lost. Sysgen's JLM0003 still has to
stay in step 5 (NJE0001 has PRE(JLM0003)). Fix = documentation + a guard: a
comment in `usermods/JLM0003.jcl` (add a name there **and** in RAKF's
table), `RAKF_STEPS_CC` explains the 12s, and step 10 fails if JLM0003 is
not RAKF's.

**Pre-existing bug found by the LIST check: ZP60041/42/43 were never
installed.** `jcl/ZP60041.jcl` U41S03 had `SYSLIB … DSN=SYSGEN.RAKF.V1R2M6.MACLIB`
(RAKF 1.2.6). That library is gone since RAKF 2.0.0 (Phase 4), so the job
ended `IEF453I JOB FAILED - JCL ERROR` (`IEF212I … SYSLIB +003 - DATA SET NOT
FOUND`), and `check_maxcc` passed it because it saw no IEF142I/IEF450I for
the later steps. Fixed: 5 × `RAKF.MACLIB` (byte replace; the file is
CRLF/latin-1, do not edit in an editor), and `check_maxcc` now fails a job
with `IEF453I`. Now all three are `REC APP`.

**Other code:** `parse_smp_list` keeps an entry across a page header (TYPE
on one page, STATUS on the next, seen for ZP60003). README step list has
ZMG0001/ZP60038. Tests: 13 new (`ZmgTest`, `UsermodTest`), 56 total.

**Harness:** the scratchpad `harness.py` now takes `method:arg` and copies
`prt00e.txt` to `$PRTSAVE/<n>_<method>.prt` before each IPL/quit (Hercules
truncates it at restart; the step-8 RAKFINST listing was lost without it).

**For Phase 8:** see the two new checklist items (MVP NJE38 must not
re-apply NJE0001; the SHUTDOWN/SHUTFAST clash that ZMG0001 creates, not
fixed here because it needs a step-14 test). Test tree `../sysgen-p7test`
has a full chain of new backups `usermod_48…`, `25_USERMODS` … `36b_SDSF`.

Next: Phase 8.

### 2026-11-13 — Phase 8 done

Ran for real in `../sysgen-p8test` (copy of p7test, repo `backup/`
untouched; `MVSCE/MVP` replaced by a clone of the local MVP branch below,
`main` = the branch, so step 9's `git pull --ff-only` is a no-op). Steps 9 →
10 → 11 → 12 → 12a → 12b → 13 → finalize from `32_RAKF`, all in `unshare
-rn` (8080 taken here): every step passed, new `38_FINAL`. Then a verify run
(restore `37_CUSTOM`, IPL, FINALIZE, two test jobs): all RC 0.

**MVP** (`../MVP`, local branches only, nothing pushed; base `36fa757` =
origin/main):
- `remove-smp-stack` (`ba96b2d`): `packages/` + `desc/` + `cache` lines of
  UFSD, FTPD, HTTPD, MVSMF removed. **No stub descs:** MVP `list`/`search`
  read only the `cache` (copied to MVS at INSTALL_MVP), so a desc without a
  cache line is never shown, and `install` would say "unable to locate". The
  MVP README says instead where they went; its FTPD examples now use RPF.
  Nothing to rebuild with the skill's flow: removal only touches `cache`.
- `brexx31-rakf2` (`c8c653f`, **stacked on** `remove-smp-stack`: both touch
  adjacent `cache` lines, as separate PRs they conflict): the Phase 5 fixes
  (ADDUSER as TSO command, CHECK_DEPENDS `STRIP`/`WORDS`, SUBMIT_WORK_JCL
  strip; not the `GET_PW` debug line, not `MVP.ini`, which is runtime state:
  INSTALL_MVP writes the password and configparser lowercases `MVP_repo`);
  `MVP.rexx` INSTALL checks `RACCHECK('FACILITY','DIAG8CMD'|'SVC244','READ')`
  first (skipped when `VERSION()` < V3) and says what is missing; NJE38 drops
  the NJE0001 APPLY (RECEIVE kept, comment why) and its config exec ends RC 8
  with a message when a `CP` command fails (was RC 0); RANDOMPW 1.1 = one
  self-contained exec in `SYS2.EXEC` (no write into the SMP-owned
  `BREXX.RXLIB`; BREXX 3.1 `PARSE SOURCE` is always `COMMAND`, so one member
  cannot be both command and function; nothing calls `RANDOMPW()`).
- Checked in the run: step 9 (MVPINSTL incl. ADDUSER), step 11 (NJE38MVP,
  no HMA2483), step 12 (MVPISPF), step 13 (OPNTERSE); `EXEC MVP,ACTION='LIST'`
  RC 0, lists NJE38/OPNTERSE/RPF/..., none of the four.

**SHUTDOWN/SHUTFAST** (`jcl/finalize.jcl`, `jcl/sysgen05.jcl`):
- Execs moved to new PDS **`SYS2.STCEXEC`** (MVS000, IEFBR14 in finalize):
  not a SYSEXEC, so no clash with ZMG0001. The proc moved from
  `sysgen05.jcl` (step 4) into finalize (IEBGENER to
  `SYS2.PROCLIB(SHUTDOWN)`, `EXECLIB='SYS2.STCEXEC'`; was named `MVP PROC`).
- **Second problem found by the test:** with the clash gone, TSO SHUTFAST
  submitted the job (`IKJ56250I JOB SHUTFAST(JOB00003) SUBMITTED`), but the
  job ended `CC 0043`, `Error 43.1 ... Could not find routine "CONSOLE"`: a
  job SUBMITted from TSO has no RAKF user, and BREXX 3.1 gives CONSOLE/CP
  only with FACILITY SVC244/DIAG8CMD (V2R5M3 was APF). A `/*$VS` card does
  not help either: `INTRDR ... AUTH=0` in JES2PM00.
- Fix: the `SYS1.CMDPROC` members SHUTDOWN/SHUTFAST are now `/* REXX */`
  (ZMG0001 runs them through BREXX) that check RACCHECK SVC244 and do `CALL
  CONSOLE "S SHUTDOWN[,TYPE='SHUTFAST']"`: the proc runs as a started task,
  `RAKF0010I STC SHUTDOWN STARTED USING DEFAULT STC ACCOUNT` (STCGROUP has
  SVC244 + DIAG8CMD). Only ADMINs can shut down from TSO now (before 3.1 any
  user could). The `SYS2.JCLLIB` jobs stay, with a comment that they need an
  admin `USER=`/`PASSWORD=`.
- Verified (batch TMP with the logon's SYSPROC=SYS1.CMDPROC +
  SYSEXEC=SYS2.EXEC, as with a 3270 logon): as MVSCE02 `SHUTFAST: NOT
  AUTHORIZED (RAKF FACILITY SVC244)`, RC 0, nothing started; as IBMUSER
  `SHUTFAST: STARTED`, STC SHUTDOWN → `SHUT001` → 10 s → `SHUT002` →
  `SCRIPTS/SHUTDOWN.RC` → JES2 ended → `IEE334I HALT EOD SUCCESSFUL`.
  `SYS2.EXEC` holds only `MVP` now. Not tested over a real 3270 session, and
  not the 5-minute SHUTDOWN (same code path, other TYPE).

**README:** step table (1–14 with 12a/12b), table of what is installed with
SMP (version, FMID, status, libraries, from `mvslovers.conf`), what that means
on the system (BREXX authorization, servers, SDSF), `LIST CDS/ACDS SYSMOD(...)`
job, upgrading (dist: inst + accept, no alloc; ZMG0001: `ZMG01RS` without its
COPY step, RECEIVE the new xmit, `$INSTALL` + `ZMG01AP`; RAKF: its own
stream), shutting down. `customize.jcl` was already OPNTERSE only (Phase 5).

**For Phase 9:** the MVP branches must be pushed and merged (or the build
pointed at them) **before** a full build: step 9 clones
`MVS-sysgen/MVP` main, which still has the four packages, `CALL ADDUSER`
(step 9 fails) and the NJE0001 APPLY (step 11 fails). Test tree
`../sysgen-p8test` has a full chain up to `38_FINAL` (with the first,
CLIST-based finalize; rerun finalize for the final one).

Next: Phase 9.

### 2026-11-15 — Phase 9 done (release tag pending)

**Precondition:** MVP PRs #12 (remove UFSD/FTPD/HTTPD/MVSMF) and #14
(BREXX 3.1 / RAKF 2.0.0 fixes) merged; MVS-sysgen/MVP `main` = `642cc98`
contains `ba96b2d` and `c8c653f`. (#13 had been merged into
`remove-smp-stack` instead of `main`; #14 carried the same commit. Its
conflict with #11's `WORDS(DEPENDS)` fix was resolved by keeping main's
line, `STRIP` dropped.)

**sysgen commits** (branch `mvslovers-smp`, not pushed): phases 1–8 as
`d3df805` (mvslovers module + manifest + tests), `58d5ffd` (BREXX 3.1 via
SMP), `d483338` (usermods, profiles, customize, finalize), `dae8477`
(sysgen steps), `a932659` (README + plan); then three found in this phase:

- `2079634`/`dd62a74`/`84dffc7` **console reply resend.** The build hung
  twice: after the `/` to IEA101A (step 3) and after `r 0,noreq` to
  `$HASP426`. Hercules logs HHC00013I and echoes the input, MVS prints
  nothing and sits in a wait state (CP thread idle). Now `ipl_reply()`
  (the five IEA101A sites: 1 s delay after `HHC00010A`) and every `r nn,`
  reply via `send_oper`/`send_reply` resend the reply when **no MVS line at
  all** comes for `IPL_REPLY_RETRY` = 15 s, up to `IPL_REPLY_TRIES` = 3.
  Hercules' `HHC` lines and its echo of the reply do not count. Lines read
  meanwhile are pushed back (`next_line`, `stdout_pushback`) for the
  caller's next wait. **Gotcha (why `dd62a74` was wrong):** waiting for
  `IEE600I REPLY TO nn` is not safe: MVS 3.8 (step 7, console 0009) answers
  the IEA101A `r 0,clpa` with NIP messages, not IEE600I, so it was resent,
  the copy answered `IFB010D` and every later reply hit the wrong prompt
  (JES2 `$HASP479` checkpoint I/O error). A resend to an already answered
  WTOR id is rejected by MVS (harmless); a resend during NIP is not.

**Build** (`../sysgen-p9test`, fresh clone, `--release --keep-backup
--keep-temp --CONTINUE` in `pasta --config-net -t none -u none -T none -U
none -- unshare --map-user=1000 --map-group=1000`: own netns with internet
for the GitHub downloads/clones, 8080 free inside; `unshare -rn` has no
network): steps 1–6 15:43–16:22 on `dd62a74` (one SMPJOB03 `DLBUCL5` RC 16,
before any resend, retried OK; 5 resends, all to WTORs), then the step 7
failure above, then `84dffc7` pulled into the tree and `--CONTINUE` from
`26_FDZ1D02`: steps 7–14 16:26–16:42 with **no resends**, `BUILD OK`.
Output `MVSCE.release.84dffc7.tar` (190 MB) + `backup/38_FINAL.tar`.
**Side effect:** the TSO logon screen says `ver: dd62a74` (stamped in an
early step), the release README `84dffc7`; a from-scratch build (e.g. the
release CI on the tag) has one version.

**Docker:** `Docker/Dockerfile` (as release.yml) with the release tar as
`MVSCE.release.tar` → `mvsce:p9test` (619 MB); a container IPLs to
`IKT005I` in 8 s and listens on 3270/3271/3505/3506/3221/3223/8888.

**Acceptance** (`../sysgen-p9accept`, copy of `38_FINAL`, own IPL: the
final system starts JES2/NET/TSO itself, so no `$HASP426`, and JES2 prints
**no `$HASP099` after a job** there; wait for `$HASP250 <job> IS PURGED`):
- LIST CDS: TBRX310, TUFS152, TFTP134, THTP451, TZMF141, TISF120, TRKF200
  `REC APP ACC`; MS00100 `REC APP` (LIST ACDS of it ends RC 4, not found:
  never accepted).
- Members (LISTCAT of BREXX/UFSD/FTPD/HTTPD/MVSMF/ISF, LISTDS MEMBERS):
  BREXX.LINKLIB 6, RXLIB 112, SAMPLIB 119, CMDLIB 10, JCL 15, PROCLIB 2;
  UFSD.LINKLIB 4, FTPD.LINKLIB 1, HTTPD.LINKLIB 5, MVSMF.LINKLIB 1,
  ISF.LINKLIB 1, ISF.ISFCLST 1, *.SAMPLIB 1–3. Every target library has the
  count of its distribution library **except `ISF.ISFPLIB` 82 vs
  `ISF.AISFPLIB` 24** — not chased; check against the sdsf dist.
- `check_usermods`: 71 usermods applied, EXEC runs BREXX V3R1M0, CLISTs run.
- 12a smoke test: UFSD/HTTPD/FTPD authorized by SVC, 6 file systems,
  `/zosmf/info` 1.4.1, `GET /` 200, FTP login of all three users.
- Batch job as IBMUSER, MVSCE01, MVSCE02 runs; wrong password `IEF722I`.
- `EXEC MVP,ACTION='LIST'`: 119 packages, none of UFSD FTPD HTTPD MVSMF.
- 3270 (c3270 4.3 `-scriptport` on a pty inside the netns, driven through a
  file relay): MVSCE02 logon → `SHUTFAST` = `NOT AUTHORIZED (RAKF FACILITY
  SVC244)`, nothing started; `RX MVP LIST` works for MVSCE02, none of the
  four; IBMUSER logon → ISPF → `TSO SDSF` → `LOG` shows the syslog, `/D T`
  → `IEE136I`; ISPF `X` → `SHUTFAST` → `SHUT001`/`SHUT002` → `$HASP085` →
  `IEE334I HALT EOD SUCCESSFUL`.

**Found, not fixed:**
1. **The job after a RAKF-refused job fails.** After a job ends `IEF722I
   INVALID PASSWORD GIVEN`, the **next** job in the same initiator gets
   `IEA911E COMPLETE DUMP` + `IEF174I SYSTEM ERROR BEFORE JOB INITIATION`
   (`$HASP396 TERMINATED`); the one after runs. Reproduced twice (first
   with MVP LIST as the victim, then with IEFBR14). Not checked whether
   RAKF 1.2.6 builds do the same; look at `SYS1.DUMP00` (RAKF 2.0.0 RACINIT
   cleanup in the initiator?).
2. *Fixed in `45b9ad8` (ports table, EXPOSE 2121 8080 22000-22200; tested HTTP + EPSV/PASV FTP through the image; PASV clients need `PASVADR`).* README Docker section: `-p 2121:21` maps nothing (FTPD is on 2121 in the
   container, TLS on 3221); 8080 (HTTPD/mvsMF) is neither in the `docker
   run` example nor `EXPOSE`d in `Docker/Dockerfile*`; it still points to
   `MVS-sysgen/docker-mvsce` for the Dockerfile, which is now `Docker/`.
3. `MVSCE/README.md` (release) is the generated stub: release, version,
   `start_mvs.sh`, users (STC ids correctly left out), link. It says nothing
   about the SMP stack, `/S UFSD|HTTPD|FTPD`, or that only admins can
   SHUTDOWN now.

**Harness gotchas:** `pkill -f <pattern>` / `pgrep -f` from a shell whose
own command line contains the pattern kills that shell (Phase 4 note, hit
again): kill by PID. A `tail -f` on a tree that was deleted and recloned
shows the old file; use `tail -F`. The acceptance code is in the session
scratchpad (`harness.py` + `p9accept.py`, `p9run.sh`, `relay3270.py`).

Next: one more major upgrade, then tag `v3.1.0`; the items above.
