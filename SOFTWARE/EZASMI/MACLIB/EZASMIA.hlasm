.*------------------------------------------------------------------- *
.*                                                                    *
.*                   EZASMIA Macro                                    *
.*                                                                    *
.*  Function:  The EZASMIA macro processes lists of required and      *
.*             optional keyword parameter names producing a bit map   *
.*             for each parameter type which EZASMI (the caller) can  *
.*             use to validate what the user has coded.               *
.*                                                                    *
.*             The first parameter (&LIST) must specify:              *
.*             'OPTN' or 'REQD'.                                      *
.*                                                                    *
.*  Author:    Shelby Beach, Ms. Kitty Programming Werks              *
.*                                                                    *
.*  Change Log:                                                       *
.*                                                                    *
.*    Date     Pgmr   Change                                          *
.*  12/01/2016  SLB   Initial release                                 *
.*                                                                    *
.*------------------------------------------------------------------- *
         MACRO
         EZASMIA &LIST,&P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,       +
               &P11,&P12,&P13,&P14,&P15,&P16,&P17,&P18,&P19,&P20,      +
               &P21,&P22,&P23,&P24,&P25,&P26,&P27,&P28,&P29,&P30
.*
.*------------------------------------------------------------------- *
.* The size of the following arrays must be consistent with those     *
.* specified in EZASMI.                                               *
.*------------------------------------------------------------------- *
.*
         GBLB  &REQD(31)
         GBLB  &OPTN(31)
         GBLC  &KW(31)
.*
.* Loop indices
         LCLA  &I,&J
.*
.* Clear residual array bits
.*
&I       SETA  1
.CLOOP   ANOP
         AIF   ('&LIST' NE 'REQD').CLROPTN
&REQD(&I) SETB 0
         AGO   .NEXTC
.CLROPTN ANOP
         AIF   ('&LIST' NE 'OPTN').BADLIST
&OPTN(&I) SETB 0
.NEXTC   ANOP
&I       SETA  &I+1
         AIF   (&I LE 31).CLOOP
.*
.* Process keyword names parameter list
.*
&I       SETA  2
.PLOOP   ANOP
         AIF   (&I GT N'&SYSLIST).DONE
&J       SETA  1
.KLOOP   ANOP
         AIF   ('&SYSLIST(&I)' EQ '&KW(&J)').SETP
&J       SETA  &J+1
         AIF   (&J LE 31).KLOOP
         MNOTE 16,'Unknown keyword &SYSLIST(&I)'
         AGO   .NEXTP
.*
.SETP    ANOP
         AIF   ('&LIST' NE 'REQD').OPTLIST
.*
.* Set required parameter
.*
&REQD(&J) SETB 1
         AGO   .NEXTP
.OPTLIST ANOP
.*
.* Set optional parameter
.*
&OPTN(&J) SETB 1
.NEXTP   ANOP
&I       SETA  &I+1
         AGO   .PLOOP
.*
.BADLIST ANOP
         MNOTE 16,'Invalid list name, &LIST'
.*
.DONE    ANOP
         MEND
