## NJE for MVS 3.8j

To install type `INSTALL NJE38` :warning: Do not exit/logoff TSO during the install, the install script will do it for you :warning:

The configuration file is located at `SYS2.PARMLIB(NJE38C00)`

This install comes with a simple configuration, make sure you edit both `SYS2.PARMLIB(NJE38C00)` and `sysgen/conf/local.cnf` to match your enfironment.

To start NJE38 run `/S NJE38` on the master console

## Install Info

- The NJE38 spool file is located at: `PUB001.NJE38.NETSPOOL`
- The authorized programs are located at: `SYSGEN.NJE38.AUTHLIB`