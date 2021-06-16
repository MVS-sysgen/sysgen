.*------------------------------------------------------------------- *
.*                                                                    *
.*                   EZASMIC Macro                                    *
.*                                                                    *
.*  Function:  The EZASMIC macro validates character constant         *
.*             keyword values, generating a load into R0 of the       *
.*             the address of the numeric constant literal.           *
.*                                                                    *
.*  Parameters:                                                       *
.*             &KWVAL - The keyword value to be processed             *
.*             &Pn    - The valid values for &KWVAL                   *
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
         EZASMIC &KWVAL,&P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,      +
               &P11,&P12,&P13,&P14,&P15,&P16,&P17,&P18,&P19,&P20
.*
         GBLC  &GPR            GPR containing addr of EZASOKET parm
.*
&GPR     SETC  '0'             Set to register to store in EZASMI
.*
.* Character string value/numeric constant array
.*
         LCLC  &STR(46)
.*
.* Initialize character string constant array
.*   Entry n is the character string value
.*   Entry n+1 is the numeric equivalent value
.*
&STR(1)  SETC 'INET'
&STR(2)  SETC 'F''02'''
&STR(3)  SETC 'STREAM'
&STR(4)  SETC 'F''01'''
&STR(5)  SETC 'DATAGRAM'
&STR(6)  SETC 'F''02'''
&STR(7)  SETC 'RAW'
&STR(8)  SETC 'F''03'''
&STR(9)  SETC 'FIONBIO'
&STR(10) SETC 'X''00000001'''          Changed from X'8004A77E' for MVS
&STR(11) SETC 'FIONREAD'
&STR(12) SETC 'X''4004A77F'''
&STR(13) SETC 'SIOCATMARK'
&STR(14) SETC 'X''4004A707'''
&STR(15) SETC 'SIOCGIFADDR'
&STR(16) SETC 'X''C020A70D'''
&STR(17) SETC 'SIOCGIFBRDADDR'
&STR(18) SETC 'X''C020A712'''
&STR(19) SETC 'SIOCGIFCONF'
&STR(20) SETC 'X''C008A714'''
&STR(21) SETC 'SIOCGIFDSTADDR'
&STR(22) SETC 'X''C020A70F'''
&STR(23) SETC 'SIOCGIFMTU'
&STR(24) SETC 'X''C020A726'''
&STR(25) SETC 'SIOCGIFNAMEINDEX'
&STR(26) SETC 'X''4000F603'''
&STR(27) SETC 'SIOCGIPMSFILTER'
&STR(28) SETC 'X''C000A724'''
&STR(29) SETC 'SIOCGMONDATA'
&STR(30) SETC 'X''C018D902'''
&STR(31) SETC 'SIOCGMSFILTER'
&STR(32) SETC 'X''C000F610'''
&STR(33) SETC 'SIOCGPARTNERINFO'
&STR(34) SETC 'X''C000F612'''
&STR(35) SETC 'SIOCGSPLXFQDN'
&STR(36) SETC 'X''C018D905'''
&STR(37) SETC 'SIOCSAPPLDATA'
&STR(38) SETC 'X''8018D90C'''
&STR(39) SETC 'SIOCSIPMSFILTER'
&STR(40) SETC 'X''8000A725'''
&STR(41) SETC 'SIOCSMSFILTER'
&STR(42) SETC 'X''8000F611'''
&STR(43) SETC 'SIOCSPARTNERINFO'
&STR(44) SETC 'X''8004F613'''
&STR(45) SETC 'SIOCTTLSCTL'
&STR(46) SETC 'X''C038D90B'''
.*
         LCLA  &I              Loop index
.*
.* Validate input keyword value
.*
&I       SETA  2
.VLOOP   ANOP
         AIF   (&I GT N'&SYSLIST).ERROR
         AIF   ('&SYSLIST(&I)' EQ '&KWVAL').LOOKUP
&I       SETA  &I+1
         AGO   .VLOOP
.*
.LOOKUP  ANOP
.*
.* Lookup keyword value numeric equivalent
.*
&I       SETA  1
.LLOOP   ANOP
         AIF   (&I GT 46).MISS
         AIF   ('&STR(&I)' EQ '&KWVAL').GENER
&I       SETA  &I+2
         AGO   .LLOOP
.*
.GENER   ANOP
.*
.* Load R0 with ptr to string's numeric equivalent
.*
         LA    0,=&STR(&I+1)
         MEXIT
.*
.ERROR   ANOP
         MNOTE 16,'Invalid parameter value, &KWVAL'
         MEXIT
.*
.MISS    ANOP
         MNOTE 16,'Missing value string, &KWVAL'
         MEND
