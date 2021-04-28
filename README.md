# Automated MVS3.8 Sysgen

## Changes

* Edited `create.dasd.sh`: removed read to ask for compression. Always compress now.
* Edited `condcode.rexx`: returns non-zero if any jobs have a return code greater than 0004. Can now be used in scripts.
* Edited `smp1.cnf`, `smp2.cnf`, and `sysgen.cnf` to change 3215 console to 3215-C for automation

## Info

This repo is heavily based on Jay Moseley sysgen. His writeup is a wonderful resource and you should read the site here: http://www.jaymoseley.com/hercules/installMVS/iMVSintroV7.htm

A lot of the information contained on this repo is directly from his sysgen walkthrough.

There are lots of files and folders. Each folder has a readme explaining from a high level what each file does.

## System Generation

To build your own sysgen mvs 3.8j system launch: `sysgen.sh`

