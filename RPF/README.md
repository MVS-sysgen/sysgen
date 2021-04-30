# Review Panel Frontend (RPF)

This installs the newest version of RPF (1.8.8+) on to the Automated Sysgen MVS 3.8J

- **Install**
  - `install_rpf.sh` main script that updates and installs RPF automatically
  - `install_rpf.rc` Hercules script file
  - `install_rpf_automation.txt` semi-colon seperated challenge response used by `automate.rexx`
  - `update_rpf.sh` updates RPF and generates new jcl files based on RPF version
  - **JCL**
  - :warning: **do not edit these files, edit the templates in `./templates` instead. These JCL files are overwritten by the install script**
  - `00_rpf_alias_profile.jcl` creates `RPF` HLQ alias and `RPF.PROFILE`
  - `01_rpf_install_jcl.jcl` installs `JCL.xmi` to `RPF.V#R#M#.SRPFJCL`
  - `02_rpf_install_help.jcl` installs `HELP.xmi` to `RPF.V#R#M#.SRPFHELP`
  - `03_rpf_install_loadlib.jcl` installs `LOADLIB.xmi` to `RPF.V#R#M#.SRPFLOAD`
  - `04_rpf_tso_help.jcl` installs the RPF help files to `SYS2.HELP`
  - `05_rpf_tso_cmdlib_parmlib.jcl` Submits the `$CMDLIB` and `$PARMLIB` jobs from `RPF.V#R#M#.SRPFJCL` using `IKJEFT01` and the `SUBMIT` command
- **XMI files**
  - `HELP.xmi` renamed from `rpf188he.xmi`
  - `JCL.xmi` renamed from `rpf188jc.zip`
  - `LOADLIB.xmi` renamed from `rpf188mv.zip`
- **Directories**
  - `templates/` contains JCL templates **edit these not the JCL**
  - `zip/` Contains zip files of most recent RPF