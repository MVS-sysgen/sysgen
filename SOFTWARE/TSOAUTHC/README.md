# TSOAUTHC

BREXX script to automate adding TSO Authorized commands to IKJEFTE2 and submit the usermods to SMP.

For use on MVS/CE only, this may break tk4-

## Install

On MVS/CE type `INSTALL TSOAUTHC` and it will be installed to `SYS2.EXEC`.

## Usage

TSOAUTHC takes two required positional arguments and one optional argument:

- `RMID` This is obtained by running the tso command LISTCDS MOD(IKJEFTE2) if LISTCDS is not installed you can install it with `INSTALL LISTCDS`
- `PROGRAM` This is the program to be added to the TSO Authorized command list
- `COMMENT` A comment for use in the CSECT

:warning: **Using this script requires an IPL and a cold start using `R 00,CLPA`** :warning:

## Example

To add the TSO program `MVS4EVA` to the Authorized commands table do the following:

```
LISTCDS MOD(IKJEFTE2)
IKJEFTE2  FMID EBB1102  RMID TSA0006  DISTLIB AOST4     LMOD   IKJEFT02
RX TSOAUTHC 'TSA0006 MVS4EVA This is the mvs for ever program'
```

