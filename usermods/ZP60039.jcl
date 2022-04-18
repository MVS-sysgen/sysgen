//ZP60039  JOB (SYSGEN),'J04 M50/51: ZP60039',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ADD TEXT= OPERAND SUPPORT TO WTO AND WTOR
//*
//STEP01  EXEC PGM=IEBUPDTE,PARM='NEW'
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=&&MACLIB,DISP=(NEW,PASS),
//             UNIT=SYSALLDA,SPACE=(CYL,(1,1,10)),
//             DCB=(DSORG=PO,RECFM=FB,LRECL=80,BLKSIZE=6160)
//SYSIN    DD  *
./ ADD NAME=IEZWPL
         MACRO                                                          00001000
         IEZWPL &DSECT=YES                                              00002000
*                                                                       00003000
*/********************************************************************/ 00004000
*/*                                                                  */ 00005000
*/*      WTO/WTOR/WTOR31/MLWTO/XWPL/WTP PARAMETER LIST DEFINITION    */ 00006000
*/*                                                                  */ 00007000
*/*      STATUS -                                                    */ 00008000
*/*      LASTUPD         = EBB1102  TYPE=ADD                         */ 00009000
*/*      LIBRARIES       = DISTLIB=AMODGEN                           */ 00010000
*/*      FMID            = FBB1221                                   */ 00011000
*/*      RMID            = FBB1221                                   */ 00012000
*/*      SYSMOD HISTORY  = SYSMOD   TYPE       DATE   MCS  STATUS    */ 00013000
*/*                        EBB1102  FUNCTION  00.000  MAC  ACC RGN   */ 00014000
*/*                        FBB1221  FUNCTION  00.000  MAC  ACC RGN   */ 00015000
*/*                                                                  */ 00016000
*/*      MACRO IEZWPL HAS BEEN UPDATED TO PROVIDE MAPPING FOR:       */ 00017000
*/*       - WTOR 31 BIT PARAMETER LIST                               */ 00018000
*/*       - EXTENDED WPL PARAMETER LIST                              */ 00019000
*/*      THIS UPDATED MACRO MUST BE USED TO ASSEMBLE USERMODS        */ 00020000
*/*      ZP60039 AND ZP60040 THAT PROVIDE WTO[R] TEXT= SUPPORT       */ 00021000
*/*                                                                  */ 00022000
*/*      METHOD OF ACCESS                                            */ 00023000
*/*      BAL  - DSECT IS PRODUCED UNLESS DSECT=NO IS SPECIFIED.      */ 00024000
*/*             USING ON WPLRF GIVES ADDRESSABILITY FOR ALL SYMBOLS  */ 00025000
*/*      PL/S - DCL WPLPTR PTR                                       */ 00026000
*/*                                                                  */ 00027000
*/* **************************************************************** */ 00028000
*                                                                       00029000
         IEZBITS  ,           SYMBOLIC BIT DEFINITIONS                  00030000
*                                                                       00031000
*********************************************************************** 00032000
*                                                                     * 00033000
*        WTOR PREFIX                                                  * 00034000
*                                                                     * 00035000
*********************************************************************** 00036000
*                                                                       00037000
         AIF   ('&DSECT' EQ 'NO').WPL0000                               00038000
WPLRF    DSECT ,              START OF WTOR PREFIX                      00039000
         AGO   .WPL0020                                                 00040000
.WPL0000 ANOP                                                           00041000
         DS    0D                                                       00042000
WPLRF    EQU   *              START OF WTOR PREFIX                      00043000
.WPL0020 ANOP                                                           00044000
WPLRPTR  DS    0A             POINTER TO REPLY BUFFER                   00045000
WPLRLN   DS    FL1            MAXIMUM LENGTH OF REPLY                   00046000
WPLRPTRA DS    AL3            ADDRESS OF REPLY BUFFER                   00047000
WPLRECB  DS    A              ADDRESS OF REPLY ECB                      00048000
*                                                                       00049000
*********************************************************************** 00050000
*                                                                     * 00051000
*        WTOR PREFIX (31-BIT PARAMETER LIST)                          * 00052000
*                                                                     * 00053000
*********************************************************************** 00054000
*                                                                       00055000
         ORG   WPLRF                                                    00056000
WPL31RF  EQU   *              START OF WTOR PREFIX (31-BIT PARM LIST)   00057000
WPL31RRP DS    A              ADDRESS OF REPLY BUFFER                   00058000
WPL31RFG EQU   X'80'          INDICATES A WTOR                          00059000
WPL31REP DS    A              ADDRESS OF REPLY ECB                      00060000
*                                                                       00061000
*********************************************************************** 00062000
*                                                                     * 00063000
*        COMMON SECTION                                               * 00064000
*                                                                     * 00065000
*********************************************************************** 00066000
*                                                                       00067000
         ORG   WPLRF                                                    00068000
WPL      EQU   *              START OF COMMON SECTION                   00069000
WPLLGH   DS    0H             MESSAGE LENGTH (COMBINED LENGTH OF        00070000
*                             MESSAGE TEXT, MESSAGE LENGTH FIELD,       00071000
*                             AND MCS FLAGS FIELD) FOR 24 BIT WPL       00072000
WPL31RLN DS    FL1            WTOR 31 BIT PARMLIST ONLY, L'REPLY BUFFER 00073000
*                             ZERO FOR ALL OTHER WPL FORMATS            00074000
WPLLPTXT DS    FL1            MESSAGE LENGTH (COMBINED LENGTH OF MSG    00075000
*                             TEXT, MSG LENGTH FIELD AND MCS FLAGS      00076000
WPLMCSF  DS    0BL2           MCS FLAGS                                 00077000
WPLMCSF1 DS    B              FIRST BYTE OF MCS FLAGS                   00078000
WPLMCSFA EQU   BIT0           ROUTE/DESCRIPTOR CODE FIELDS PRESENT      00079000
WPLMCSFB EQU   BIT1           QUEUE TO CONSOLE IF ACTIVE (ID IN R0)     00080000
WPLMCSFC EQU   BIT2           COMMAND RESPONSE                          00081000
WPLMCSFD EQU   BIT3           MESSAGE TYPE FIELD EXISTS                 00082000
WPLMCSFE EQU   BIT4           THIS WPL IS A REPLY TO A WTOR             00083000
WPLMCSFF EQU   BIT5           BROADCAST THIS MSG TO ALL ACTIVE          00084000
*                             CONSOLES                                  00085000
WPLMCSFG EQU   BIT6           QUEUE TO HARD COPY ONLY                   00086000
WPLMCSFH EQU   BIT7           QUEUE UNCONDITIONALLY TO CONSOLE (ID IN   00087000
*                             REG 0)                                    00088000
WPLMCSF2 DS    B              SECOND BYTE OF MCS FLAGS                  00089000
WPLMCSFI EQU   BIT0           DO NOT TIME STAMP THIS MESSAGE            00090000
WPLMCSFJ EQU   BIT1           MLWTO INDICATOR                           00091000
WPLMCSFK EQU   BIT2           THE TEXT IN THIS WPL IS TO BE             00092000
*                             ROUTED TO THE JOB'S PRINTER ONLY          00093000
*                             (OS/VS1 ONLY)                             00094000
WPLMCSFL EQU   BIT3           EXTENDED WPL FORMAT (WPX) EXISTS          00095000
*                             WAS WPLNOWTP SUPPRESS WTP (OS/VS1 ONLY)   00096000
WPLMCSFM EQU   BIT4           THE MESSAGE IS AN OPERATOR COMMAND        00097000
WPLMCSFN EQU   BIT5           BYPASS QUEUING MESSAGE TO HARD COPY       00098000
WPLRSV04 EQU   BIT6           RESERVED                                  00099000
WPLRSV05 EQU   BIT7           RESERVED                                  00100000
*                                                                       00101000
WPLTXT   DS    0CL126         MESSAGE TEXT (MAXIMUM 126 CHARACTERS)     00102000
         DS    CL125          MESSAGE TEXT                              00103000
WPLTXTL  DS    C              LAST BYTE OF MESSAGE TEXT                 00104000
*                                                                       00105000
*        THE FOLLOWING FIELDS BEGIN IMMEDIATELY FOLLOWING THE           00106000
*        LAST BYTE OF MESSAGE TEXT                                      00107000
*                                                                       00108000
         ORG   WPLRF                                                    00109000
*                                                                       00110000
WPLFLGS  EQU   *              START OF WPL FLAGS FIELDS                 00111000
WPLDESC  DS    0BL2           DESCRIPTOR CODES                          00112000
WPLDESC1 DS    B              FIRST BYTE OF DESCRIPTOR CODES            00113000
WPLDESCA EQU   BIT0           SYSTEM FAILURE MESSAGE                    00114000
WPLDESCB EQU   BIT1           IMMEDIATE ACTION REQUIRED MESSAGE         00115000
WPLDESCC EQU   BIT2           EVENTUAL ACTION REQUIRED MESSAGE          00116000
WPLDESCD EQU   BIT3           SYSTEM STATUS MESSAGE                     00117000
WPLDESCE EQU   BIT4           IMMEDIATE COMMAND RESPONSE MESSAGE        00118000
WPLDESCF EQU   BIT5           JOB STATUS MESSAGE                        00119000
WPLDESCG EQU   BIT6           APPLICATION PROGRAM/PROCESSOR MESSAGE     00120000
WPLDESCH EQU   BIT7           OUT-OF-LINE MESSAGE                       00121000
WPLDESC2 DS    B              SECOND BYTE OF DESCRIPTOR CODES           00122000
WPLDESCI EQU   BIT0           DESCRIPTOR CODE 9                         00123000
WPLDESCJ EQU   BIT1           DESCRIPTOR CODE 10 (OS/VS2)               00124000
WPLDESCK EQU   BIT2           CRITICAL EVENTUAL ACTION MESSAGE -        00125000
*                             DESCRIPTOR CODE 11                        00126000
WPLRSV08 EQU   BIT3           RESERVED                                  00127000
WPLRSV09 EQU   BIT4           RESERVED                                  00128000
WPLRSV10 EQU   BIT5           RESERVED                                  00129000
WPLRSV11 EQU   BIT6           RESERVED                                  00130000
WPLRSV12 EQU   BIT7           RESERVED                                  00131000
*                                                                       00132000
*        ROUTING CODES                                                  00133000
*        THESE CODES INDICATE THE FUNCTIONAL AREA OR AREAS TO           00134000
*        WHICH A MESSAGE IS TO BE SENT                                  00135000
*                                                                       00136000
WPLROUT  DS    0BL2           ROUTING CODES                             00137000
WPLROUT1 DS    B              1ST BYTE OF ROUTING CODES                 00138000
WPLROUTA EQU   BIT0           MASTER CONSOLE                            00139000
WPLROUTB EQU   BIT1           MASTER CONSOLE INFORMATIONAL              00140000
WPLROUTC EQU   BIT2           TAPE POOL                                 00141000
WPLROUTD EQU   BIT3           DIRECT ACCESS POOL                        00142000
WPLROUTE EQU   BIT4           TAPE LIBRARY                              00143000
WPLROUTF EQU   BIT5           DISK LIBRARY                              00144000
WPLROUTG EQU   BIT6           UNIT RECORD POOL                          00145000
WPLROUTH EQU   BIT7           TELEPROCESSING CONTROL                    00146000
WPLROUT2 DS    B              2ND BYTE OF ROUTING CODES                 00147000
WPLROUTI EQU   BIT0           SYSTEM SECURITY                           00148000
WPLROUTJ EQU   BIT1           SYSTEM/ERROR MAINTENANCE                  00149000
WPLROUTK EQU   BIT2           PROGRAMMER INFORMATION                    00150000
WPLROUTL EQU   BIT3           EMULATOR INFORMATION                      00151000
WPLROUTM EQU   BIT4           USER ROUTING CODE                         00152000
WPLROUTN EQU   BIT5           USER ROUTING CODE                         00153000
WPLROUTO EQU   BIT6           USER ROUTING CODE                         00154000
WPLRSV13 EQU   BIT7           RESERVED                                  00155000
*                                                                       00156000
WPLMSGTY DS    0BL2           MESSAGE TYPE FLAGS                        00157000
WPLMSGT1 DS    B              FIRST BYTE OF MESSAGE TYPE FLAGS          00158000
WPLMSGTA EQU   BIT0           DISPLAY JOBNAMES                          00159000
WPLMSGTB EQU   BIT1           DISPLAY STATUS                            00160000
WPLMSGTC EQU   BIT2           MONITOR ACTIVE (OS/VS1)                   00161000
WPLMSGTD EQU   BIT3           INDICATES EXISTENCE OF QID FIELD IN       00162000
*                             WPL (OS/VS1 ONLY)                         00163000
WPLRSV14 EQU   BIT4           RESERVED                                  00164000
WPLMSGTF EQU   BIT5           MONITOR SESS                              00165000
WPLRSV15 EQU   BIT6           RESERVED                                  00166000
WPLRSV16 EQU   BIT7           RESERVED                                  00167000
WPLMSGT2 DS    B              SECOND BYTE OF MESSAGE TYPE FLAGS         00168000
WPLRSV25 EQU   BIT0           RESERVED                                  00169000
WPLRSV26 EQU   BIT1           RESERVED                                  00170000
WPLRSV27 EQU   BIT2           RESERVED                                  00171000
WPLRSV28 EQU   BIT3           RESERVED                                  00172000
WPLRSV29 EQU   BIT4           RESERVED                                  00173000
WPLRSV30 EQU   BIT5           RESERVED                                  00174000
WPLRSV31 EQU   BIT6           RESERVED                                  00175000
WPLRSV32 EQU   BIT7           RESERVED                                  00176000
WPLQID   DS    H              STATION IDENTIFICATION FOR RES SUPPORT    00177000
*                             (OS/VS1 ONLY)                             00178000
*                                                                       00179000
*********************************************************************** 00180000
*                                                                     * 00181000
*        MLWTO EXTENSION                                              * 00182000
*                                                                     * 00183000
*********************************************************************** 00184000
*                                                                       00185000
*        THE FOLLOWING FIELDS ARE ALWAYS PRESENT WHEN MLWTO             00186000
*        IS SPECIFIED                                                   00187000
*                                                                       00188000
         ORG   WPLRF                                                    00189000
*                                                                       00190000
WPLLTF   DS    0BL2           LINE TYPE FLAGS FOR WPLTXT                00191000
WPLLTF1  DS    B              1ST BYTE OF WPLTXT LINE TYPE FLAGS        00192000
WPLLTFA  EQU   BIT0           CONTROL LINE                              00193000
WPLLTFB  EQU   BIT1           LABEL LINE                                00194000
WPLLTFC  EQU   BIT2           DATA LINE                                 00195000
WPLLTFD  EQU   BIT3           END LINE                                  00196000
WPLRSV17 EQU   BIT4           RESERVED                                  00197000
WPLRSV18 EQU   BIT5           RESERVED                                  00198000
WPLRSV19 EQU   BIT6           RESERVED                                  00199000
WPLRSV20 EQU   BIT7           RESERVED                                  00200000
WPLLTF2  DS    B              2ND BYTE OF WPLTXT LINE TYPE FLAGS        00201000
WPLAREA  DS    C              AREA IDENTIFICATION                       00202000
WPLLINES DS    FL1            NUMBER OF LINES (1 + NUMBER OF WPLMLTXT   00203000
*                             LINES)                                    00204000
*                                                                       00205000
*        THE FOLLOWING FIELDS ARE OPTIONAL FOR MLWTO                    00206000
*        THEY REPRESENT A MAPPING OF THE ENTRIES DESCRIBING THE         00207000
*        MINOR MESSAGE TEXT LINES CREATED IN ADDITION TO THE            00208000
*        MAJOR WPLTXT MESSAGE TEXT LINE                                 00209000
*                                                                       00210000
         ORG   WPLRF                                                    00211000
*                                                                       00212000
WPLML    EQU   *              START OF ADDITIONAL MLWTO LINE ENTRY      00213000
WPLML0   DS    FL1            ALWAYS ZERO                               00214000
WPLMLLEN DS    FL1            MESSAGE LENGTH FOR THIS LINE (LENGTH OF   00215000
*                             MESSAGE TEXT + 4)                         00216000
WPLMLLTF DS    0BL2           TYPE FLAGS FOR THIS LINE (WPLMLTXT)       00217000
WPLMLLT1 DS    B              1ST BYTE OF LINE TYPE FLAGS FOR           00218000
*                             WPLMLTXT                                  00219000
WPLMLLTA EQU   BIT0           CONTROL LINE                              00220000
WPLMLLTB EQU   BIT1           LABEL LINE                                00221000
WPLMLLTC EQU   BIT2           DATA LINE                                 00222000
WPLMLLTD EQU   BIT3           END LINE                                  00223000
WPLRSV21 EQU   BIT4           RESERVED                                  00224000
WPLRSV22 EQU   BIT5           RESERVED                                  00225000
WPLRSV23 EQU   BIT6           RESERVED                                  00226000
WPLRSV24 EQU   BIT7           RESERVED                                  00227000
WPLMLLT2 DS    B              2ND BYTE OF LINE TYPE FLAGS FOR           00228000
*                             WPLMLTXT                                  00229000
WPLMLTXT DS    CL126          MESSAGE TEXT FOR THIS LINE (MAXIMUM 126   00230000
*                             CHARACTERS)                               00231000
*********************************************************************** 00232000
*                                                                       00233000
*        XWPL FIELDS                                                    00234000
*        AN XWPL IS INDICATED BY THE MCS FLAG WPLMCSFL                  00235000
*                                                                       00236000
*********************************************************************** 00237000
*                                                                       00238000
         ORG   WPLRF                                                    00239000
*                                                                       00240000
WPX      EQU   *              START OF WPL EXTENSION                    00241000
WPXVRSN  DS    AL1            VERSION LEVEL                             00242000
WPXFLAGS DS    X              FLAGS, RESERVED                           00243000
WPXRPYLN DS    AL1            LENGTH OF REPLY BUFFER                    00244000
WPXLNGTH DS    AL1            LENGTH OF WPX (ZERO FOR VER 1)            00245000
*********************************************************************** 00246000
*                                                                       00247000
*        EXTENDED MCS FLAGS                                             00248000
*                                                                       00249000
*********************************************************************** 00250000
WPXMCSF1 DS    0XL2                                                     00251000
WPXMCS1  DS    X              FIRST BYTE OF EXTENDED MCS FLAGS          00252000
WPXRSV68 EQU   BIT0           RESERVED                                  00253000
WPXCONS  EQU   BIT1           FOUR BYTE CONSOLE ID WAS SPECIFIED        00254000
WPXRSV71 EQU   BIT2           RESERVED                                  00255000
WPXCONN  EQU   BIT3           CONNECT ID WAS SPECIFIED                  00256000
WPXWTOR  EQU   BIT4           WTOR WITH EXTENDED PARM LIST              00257000
WPXRSV72 EQU   BIT5           RESERVED                                  00258000
WPXCNM   EQU   BIT6           CONSOLE NAME WAS SPECIFIED                00259000
WPXMCS2  DS    X              2ND BYTE OF EXTENDED MCS FLAGS            00260000
WPXTXTAD EQU   BIT0           TEXT ADDRESS WAS SPECIFIED                00261000
WPXRSV1A EQU   BIT1           RESERVED                                  00262000
WPXRSV48 EQU   BIT2           RESERVED                                  00263000
WPXRSV49 EQU   BIT3           RESERVED                                  00264000
WPXRSV50 EQU   BIT4           RESERVED                                  00265000
WPXSYNC  EQU   BIT5           PROCESS SYNCHRONOUS                       00266000
WPXRSV51 EQU   BIT6           RESERVED                                  00267000
WPXRSV52 EQU   BIT7           RESERVED                                  00268000
WPXCPFLG DS    0XL2           FLAGS FOR CONTROL PROGRAM USE ONLY        00269000
*                                                                       00270000
WPXCPFL1 DS    X              FLAGS FOR CONTROL PROGRAM USE BYTE 1      00271000
WPXRROK  EQU   BIT0                                                     00272000
WPXNOHO  EQU   BIT1                                                     00273000
WPXNLCK  EQU   BIT2                                                     00274000
WPXACLW  EQU   BIT3                                                     00275000
WPXSPVD  EQU   BIT4                                                     00276000
WPXQNLY  EQU   BIT5                                                     00277000
WPXRSV56 EQU   BIT6                                                     00278000
WPXRSV57 EQU   BIT7                                                     00279000
WPXCPFL2 DS    X              FLAGS FOR CONTROL PROGRAM USE BYTE 2      00280000
WPXRSV60 EQU   BIT0                                                     00281000
WPXRSV61 EQU   BIT1                                                     00282000
WPXRSV62 EQU   BIT2                                                     00283000
WPXRSV63 EQU   BIT3                                                     00284000
WPXRSV64 EQU   BIT4                                                     00285000
WPXRSV65 EQU   BIT5                                                     00286000
WPXRSV66 EQU   BIT6                                                     00287000
WPXRSV67 EQU   BIT7                                                     00288000
*                                                                       00289000
WPXRPBUF DS    AL4            REPLY BUFFER ADDR - WTOR ONLY             00290000
WPXECBP  DS    AL4            REPLY ECB ADDRESS - WTOR ONLY             00291000
WPXSEQN  DS    0F             DOM/CONNECT ID                            00292000
WPXSYSID DS    AL1            SYSTEM ID                                 00293000
WPXSQID  DS    AL3            DOM SEQUENCE NUMBER                       00294000
*                                                                       00295000
*********************************************************************** 00296000
*                                                                       00297000
*        DESCRIPTOR CODES                                               00298000
*                                                                       00299000
*********************************************************************** 00300000
*                                                                       00301000
WPXDESC  DS    0XL4           DESCRIPTOR CODES                          00302000
WPXDESC1 DS    X              FIRST BYTE OF DESCRIPTOR CODES            00303000
WPXDESCA EQU   BIT0           SYSTEM FAILURE MESSAGE                    00304000
WPXDESCB EQU   BIT1           IMMEDIATE ACTION REQUIRED MESSAGE         00305000
WPXDESCC EQU   BIT2           EVENTUAL ACTION REQUIRED MESSAGE          00306000
WPXDESCD EQU   BIT3           SYSTEM STATUS MESSAGE                     00307000
WPXDESCE EQU   BIT4           IMMEDIATE COMMAND RESPONSE MESSAGE        00308000
WPXDESCF EQU   BIT5           JOB STATUS MESSAGE                        00309000
WPXDESCG EQU   BIT6           APPL PROGRAM - PROCESSOR MESSAGE          00310000
WPXDESCH EQU   BIT7           OUT-OF-LINE MESSAGE                       00311000
WPXDESC2 DS    X              SECOND BYTE OF DESCRIPTOR CODES           00312000
WPXDESCI EQU   BIT0           DESCRIPTOR CODE 9                         00313000
WPXDESCJ EQU   BIT1           DESCRIPTOR CODE 10                        00314000
WPXDESCK EQU   BIT2           DESCRIPTOR CODE 11                        00315000
WPXDESCL EQU   BIT3           DELIVERED BUT NOT HELD                    00316000
WPXRSV4  EQU   BIT4           RESERVED                                  00317000
WPXRSV5  EQU   BIT5           RESERVED                                  00318000
WPXRSV6  EQU   BIT6           RESERVED                                  00319000
WPXRSV7  EQU   BIT7           RESERVED                                  00320000
WPXDESC3 DS    X              RESERVED                                  00321000
WPXDESC4 DS    X              RESERVED                                  00322000
*                                                                       00323000
*********************************************************************** 00324000
*                                                                       00325000
*        ROUTING CODES 1-128                                            00326000
*                                                                       00327000
*        THESE CODES INDICATE THE FUNCTIONAL AREA OR AREAS TO           00328000
*        WHICH A MESSAGE IS TO BE SENT                                  00329000
*                                                                       00330000
*********************************************************************** 00331000
*                                                                       00332000
WPXROUT  DS    0XL16          ROUTING CODES                             00333000
WPXR001  DS    X              1ST BYTE OF ROUTING CODES                 00334000
WPXR01   EQU   BIT0           MASTER CONSOLE                            00335000
WPXR02   EQU   BIT1           MASTER CONSOLE INFORMATIONAL              00336000
WPXR03   EQU   BIT2           TAPE POOL                                 00337000
WPXR04   EQU   BIT3           DIRECT ACCESS POOL                        00338000
WPXR05   EQU   BIT4           TAPE LIBRARY                              00339000
WPXR06   EQU   BIT5           DISK LIBRARY                              00340000
WPXR07   EQU   BIT6           UNIT RECORD POOL                          00341000
WPXR08   EQU   BIT7           TELEPROCESSING CONTROL                    00342000
*                                                                       00343000
WPXR002  DS    X              2ND BYTE OF ROUTING CODES                 00344000
WPXR09   EQU   BIT0           SYSTEM SECURITY                           00345000
WPXR10   EQU   BIT1           SYSTEM/ERROR MAINTENANCE                  00346000
WPXR11   EQU   BIT2           PROGRAMMER INFORMATION                    00347000
WPXR12   EQU   BIT3           EMULATOR INFORMATION                      00348000
WPXR13   EQU   BIT4           USER ROUTING CODE                         00349000
WPXR14   EQU   BIT5           USER ROUTING CODE                         00350000
WPXR15   EQU   BIT6           USER ROUTING CODE                         00351000
WPXR16   EQU   BIT7           USER ROUTING CODE                         00352000
*                                                                       00353000
WPXR003  DS    X              3RD TO 16TH BYTE OF ROUTING CODES         00354000
WPXR004  DS    X                                                        00355000
WPXR005  DS    X                                                        00356000
WPXR006  DS    X                                                        00357000
WPXR007  DS    X                                                        00358000
WPXR008  DS    X                                                        00359000
WPXR009  DS    X                                                        00360000
WPXR010  DS    X                                                        00361000
WPXR011  DS    X                                                        00362000
WPXR012  DS    X                                                        00363000
WPXR013  DS    X                                                        00364000
WPXR014  DS    X                                                        00365000
WPXR015  DS    X                                                        00366000
WPXR016  DS    X                                                        00367000
*                                                                       00368000
*********************************************************************** 00369000
*                                                                       00370000
*        MESSAGE TYPE FLAGS                                             00371000
*                                                                       00372000
*********************************************************************** 00373000
*                                                                       00374000
WPXMSGTY DS    0XL2           MESSAGE TYPE FLAGS                        00375000
WPXMSGT1 DS    X              FIRST BYTE OF MESSAGE TYPE FLAGS          00376000
WPXMSGTA EQU   BIT0           MONITOR JOBNAMES                          00377000
WPXMSGTB EQU   BIT1           MONITOR STATUS                            00378000
WPXRSV9  EQU   BIT2           RESERVED                                  00379000
WPXRSV10 EQU   BIT3           RESERVED                                  00380000
WPXRSV11 EQU   BIT4           RESERVED                                  00381000
WPXMSGTF EQU   BIT5           MONITOR SESS                              00382000
WPXRSV12 EQU   BIT6           RESERVED                                  00383000
WPXRSV13 EQU   BIT7           RESERVED                                  00384000
WPXMSGT2 DS    X              SECOND BYTE OF MESSAGE TYPE FLAGS         00385000
WPXRSV14 EQU   BIT0           RESERVED                                  00386000
WPXRSV15 EQU   BIT1           RESERVED                                  00387000
WPXRSV16 EQU   BIT2           RESERVED                                  00388000
WPXRSV17 EQU   BIT3           RESERVED                                  00389000
WPXRSV18 EQU   BIT4           RESERVED                                  00390000
WPXRSV19 EQU   BIT5           RESERVED                                  00391000
WPXRSV20 EQU   BIT6           RESERVED                                  00392000
WPXRSV21 EQU   BIT7           RESERVED                                  00393000
*                                                                       00394000
WPXRSV73 DS    AL2            RESERVED                                  00395000
WPXJOBID DS    CL8            JOB ID                                    00396000
WPXJOBNM DS    CL8            JOBNAME                                   00397000
WPXKEY   DS    CL8            RETRIEVAL KEY                             00398000
WPXTOKN  DS    AL4            TOKEN FOR DOM                             00399000
WPXCNID  DS    AL4            CONSOLE ID                                00400000
WPXSYSNA DS    CL8            SYSTEM NAME                               00401000
WPXCNNME DS    CL8            CONSOLE NAME                              00402000
WPXRCNA  DS    AL4            ADDRESS OF 12 BYTE FIELD FOR REPLYING     00403000
*                             CONSOLE NAME/ID                           00404000
WPXCART  DS    AL4            ADDRESS OF CART                           00405000
WPXWSPRM DS    AL4            ADDRESS OF WAIT STATE PARM LIST           00406000
WPXASCB  DS    AL4            ASCB ADDRESS                              00407000
WPXRSV30 DS    CL16           RESERVED                                  00408000
WPXLEN   EQU   *-WPX          LENGTH OF THE WPX                         00409000
WPX2LEN  EQU   104            LENGTH OF VERSION 2 WPX                   00410000
WPX4LEN  EQU   124            LENGTH OF VERSION 4 WPX                   00411000
         MEND  ,              */                                        00412000
*%WPLL2: ;                                                              00413000
*                                                                       00414000
* @SPACE(2);                             /*                  @G64DP9A*/ 00415000
*/********************************************************************/ 00416000
*/*                                                                  */ 00417000
*/*                      WTOR PREFIX                                 */ 00418000
*/*                                                                  */ 00419000
*/********************************************************************/ 00420000
*                                                                       00421000
*DECLARE                                                                00422000
*  1 WPLRF  BASED(WPLPTR) BDY(WORD),     /* START OF WTOR PREFIX     */ 00423000
*   2 WPLRPTR     PTR(31),               /* POINTER TO REPLY BUFFER  */ 00424000
*    3 WPLRLN      PTR(8),               /* MAXIMUM LENGTH OF REPLY  */ 00425000
*    3 WPLRPTRA    PTR(24),              /* ADDRESS OF REPLY BUFFER  */ 00426000
*   2 WPLRECB     PTR(31);               /* ADDRESS OF REPLY ECB     */ 00427000
*                                                                       00428000
*/********************************************************************/ 00429000
*/*                                                                  */ 00430000
*/*                      COMMON SECTION                              */ 00431000
*/*                                                                  */ 00432000
*/********************************************************************/ 00433000
*                                                                       00434000
*DECLARE                                                                00435000
*  1 WPL  BASED(WPLPTR) BDY(WORD),       /* START OF COMMON SECTION  */ 00436000
*   2 WPLLGH      FIXED(15),             /* MESSAGE LENGTH (4 +         00437000
*                                           MESSAGE TEXT LENGTH      */ 00438000
*   2 WPLMCSF     CHAR(2),               /* MCS FLAGS                */ 00439000
*    3 WPLMCSF1    BIT(8),               /* 1ST BYTE OF MCS FLAGS    */ 00440000
*     4 WPLMCSFA BIT(1),                 /* ROUTE/DESCRIPTOR CODE       00441000
*                                           FIELDS PRESENT           */ 00442000
*     4 WPLMCSFB BIT(1),                 /* QUEUE TO CONSOLE IF ACTIVE  00443000
*                                           (ID IN REG 0)            */ 00444000
*     4 WPLMCSFC BIT(1),                 /* COMMAND RESPONSE         */ 00445000
*     4 WPLMCSFD BIT(1),                 /* MESSAGE TYPE FIELD EXISTS*/ 00446000
*     4 WPLMCSFE BIT(1),                 /* THIS WPL IS A REPLY TO A    00447000
*                                           WTOR                     */ 00448000
*     4 WPLMCSFF BIT(1),                 /* BROADCAST THIS MESSAGE      00449000
*                                           TO ALL ACTIVE CONSOLES   */ 00450000
*     4 WPLMCSFG BIT(1),                 /* QUEUE TO HARD COPY ONLY  */ 00451000
*     4 WPLMCSFH BIT(1),                 /* QUEUE UNCONDITIONALLY       00452000
*                                           TO CONSOLE (ID IN REG 0) */ 00453000
*    3 WPLMCSF2    BIT(8),               /* 2ND BYTE OF MCS FLAGS    */ 00454000
*     4 WPLMCSFI BIT(1),                 /* DO NOT TIME STAMP THIS      00455000
*                                           MESSAGE                  */ 00456000
*     4 WPLMCSFJ BIT(1),                 /* MLWTO INDICATOR          */ 00457000
*     4 WPLMCSFK BIT(1),                 /* IF ON, THE TEXT IN THIS     00458000
*                                           WPL IS TO BE ROUTED TO      00459000
*                                           THE JOB'S PRINTER ONLY      00460000
*                                           (OS/VS1)           MDC003*/ 00461000
*     4 WPLMCSFL BIT(1),                 /* EXTENDED WPL FORMAT EXITS   00462000
*                                           WAS WPLNOWTP SUPPRESS       00463000
*                                           WTP (VS1 ONLY)           */ 00464000
*     4 WPLRSV03 BIT(1),                 /* RESERVED                 */ 00465000
*     4 WPLMCSFN BIT(1),                 /* BYPASS QUEUING MESSAGE      00466000
*                                           TO HARD COPY             */ 00467000
*     4 WPLRSV04 BIT(1),                 /* RESERVED                 */ 00468000
*     4 WPLRSV05 BIT(1),                 /* RESERVED                 */ 00469000
*   2 WPLTXT      CHAR(126) BDY(WORD),   /* MESSAGE TEXT (MAXIMUM       00470000
*                                           126 CHARACTERS           */ 00471000
*    3 *           CHAR(125),            /* MESSAGE TEXT             */ 00472000
*    3 WPLTXTL     CHAR(1);              /* LAST BYTE OF MESSAGE        00473000
*                                           TEXT                     */ 00474000
*                                                                       00475000
*/*      THE FOLLOWING FIELDS BEGIN IMMEDIATELY FOLLOWING THE LAST   */ 00476000
*/*      CHARACTER (BYTE) OF MESSAGE TEXT IN WPLTXT                  */ 00477000
*                                                                       00478000
*DECLARE                                                                00479000
*  1 WPLFLGS  BASED(WPLPTR) BDY(BYTE),   /* START OF WPL FLGS FIELDS */ 00480000
*   2 WPLDESC     CHAR(2),               /* DESCRIPTOR CODES         */ 00481000
*                                                                       00482000
*/*      DESCRIPTOR CODES ARE USED TO FUNCTIONALLY CLASSIFY WTO AND  */ 00483000
*/*      WTOR MESSAGES.                                              */ 00484000
*                                                                       00485000
*    3 WPLDESC1    BIT(8),               /* 1ST BYTE OF DESCRIPTOR      00486000
*                                           CODES                    */ 00487000
*     4 WPLDESCA BIT(1),                 /* SYSTEM FAILURE MESSAGE   */ 00488000
*     4 WPLDESCB BIT(1),                 /* IMMEDIATE ACTION            00489000
*                                           REQUIRED MESSAGE         */ 00490000
*     4 WPLDESCC BIT(1),                 /* EVENTUAL ACTION             00491000
*                                           REQUIRED MESSAGE         */ 00492000
*     4 WPLDESCD BIT(1),                 /* SYSTEM STATUS MESSAGE    */ 00493000
*     4 WPLDESCE BIT(1),                 /* IMMEDIATE COMMAND           00494000
*                                           RESPONSE MESSAGE         */ 00495000
*     4 WPLDESCF BIT(1),                 /* JOB STATUS MESSAGE       */ 00496000
*     4 WPLDESCG BIT(1),                 /* APPLICATION PROGRAM/        00497000
*                                           PROCESSOR MESSAGE        */ 00498000
*     4 WPLDESCH BIT(1),                 /* OUT-OF-LINE MESSAGE      */ 00499000
*    3 WPLDESC2    BIT(8),               /* 2ND BYTE OF DESCRIPTOR      00500000
*                                           CODES                    */ 00501000
*     4 WPLDESCI BIT(1),                 /* DESCRIPTOR CODE 9        */ 00502000
*     4 WPLDESCJ BIT(1),                 /* DESCRIPTOR CODE 10          00503000
*                                           (OS/VS2)           MDC002*/ 00504000
*     4 WPLDESCK BIT(1),                 /* CRITICAL EVENTUAL ACTION    00505000
*                                           MESSAGE - DESCRIPTOR CODE   00506000
*                                           11  (MDC300)     @G64DP9A*/ 00507000
*     4 WPLRSV08 BIT(1),                 /* RESERVED                 */ 00508000
*     4 WPLRSV09 BIT(1),                 /* RESERVED                 */ 00509000
*     4 WPLRSV10 BIT(1),                 /* RESERVED                 */ 00510000
*     4 WPLRSV11 BIT(1),                 /* RESERVED                 */ 00511000
*     4 WPLRSV12 BIT(1),                 /* RESERVED                 */ 00512000
*                                                                       00513000
*   2 WPLROUT     CHAR(2),               /* ROUTING CODES            */ 00514000
*                                                                       00515000
*/*      THESE CODES INDICATE THE FUNCTIONAL AREA OR AREAS TO WHICH  */ 00516000
*/*      A MESSAGE IS TO BE SENT                                     */ 00517000
*                                                                       00518000
*    3 WPLROUT1    BIT(8),               /* 1ST BYTE OF ROUTING CODES*/ 00519000
*     4 WPLROUTA BIT(1),                 /* MASTER CONSOLE           */ 00520000
*     4 WPLROUTB BIT(1),                 /* MASTER CONSOLE INFO      */ 00521000
*     4 WPLROUTC BIT(1),                 /* TAPE POOL                */ 00522000
*     4 WPLROUTD BIT(1),                 /* DIRECT ACCESS POOL       */ 00523000
*     4 WPLROUTE BIT(1),                 /* TAPE LIBRARY             */ 00524000
*     4 WPLROUTF BIT(1),                 /* DISK LIBRARY             */ 00525000
*     4 WPLROUTG BIT(1),                 /* UNIT RECORD POOL         */ 00526000
*     4 WPLROUTH BIT(1),                 /* TELEPROCESSING CONTROL   */ 00527000
*    3 WPLROUT2    BIT(8),               /* 2ND BYTE OF ROUTING CODES*/ 00528000
*     4 WPLROUTI BIT(1),                 /* SYSTEM SECURITY          */ 00529000
*     4 WPLROUTJ BIT(1),                 /* SYSTEM/ERROR MAINTENANCE */ 00530000
*     4 WPLROUTK BIT(1),                 /* PROGRAMMER INFORMATION   */ 00531000
*     4 WPLROUTL BIT(1),                 /* EMULATOR INFORMATION     */ 00532000
*     4 WPLROUTM BIT(1),                 /* USER ROUTING CODE        */ 00533000
*     4 WPLROUTN BIT(1),                 /* USER ROUTING CODE        */ 00534000
*     4 WPLROUTO BIT(1),                 /* USER ROUTING CODE        */ 00535000
*     4 WPLRSV13 BIT(1),                 /* RESERVED                 */ 00536000
*                                                                       00537000
*   2 WPLMSGTY    CHAR(2),               /* MESSAGE TYPE FLAGS       */ 00538000
*    3 WPLMSGT1    BIT(8),               /* 1ST BYTE OF MESSAGE         00539000
*                                           TYPE FLAGS               */ 00540000
*     4 WPLMSGTA BIT(1),                 /* DISPLAY JOBNAMES         */ 00541000
*     4 WPLMSGTB BIT(1),                 /* DISPLAY STATUS           */ 00542000
*     4 WPLMSGTC BIT(1),                 /* MONITOR ACTIVE              00543000
*                                           (OS/VS1)           MDC001*/ 00544000
*     4 WPLMSGTD BIT(1),                 /* INDICATES EXISTENCE OF      00545000
*                                           QID FIELD IN WPL            00546000
*                                           (OS/VS1)           ICB467*/ 00547000
*     4 WPLRSV14 BIT(1),                 /* RESERVED                 */ 00548000
*     4 WPLMSGTF BIT(1),                 /* MONITOR SESS             */ 00549000
*     4 WPLRSV15 BIT(1),                 /* RESERVED                 */ 00550000
*     4 WPLRSV16 BIT(1),                 /* RESERVED                 */ 00551000
*    3 WPLMSGT2    BIT(8),               /* 2ND BYTE OF MESSAGE         00552000
*                                           TYPE FLAGS               */ 00553000
*     4 WPLRSV25 BIT(1),                 /* RESERVED                 */ 00554000
*     4 WPLRSV26 BIT(1),                 /* RESERVED                 */ 00555000
*     4 WPLRSV27 BIT(1),                 /* RESERVED                 */ 00556000
*     4 WPLRSV28 BIT(1),                 /* RESERVED                 */ 00557000
*     4 WPLRSV29 BIT(1),                 /* RESERVED                 */ 00558000
*     4 WPLRSV30 BIT(1),                 /* RESERVED                 */ 00559000
*     4 WPLRSV31 BIT(1),                 /* RESERVED                 */ 00560000
*     4 WPLRSV32 BIT(1),                 /* RESERVED                 */ 00561000
*   2 WPLQID      FIXED(15) BDY(BYTE);   /* STATION IDENTIFICATION      00562000
*                                           FOR RES SUPPORT             00563000
*                                           (OS/VS1)           ICB467*/ 00564000
*                                                                       00565000
*/********************************************************************/ 00566000
*/*                                                                  */ 00567000
*/*                      MLWTO EXTENSION                             */ 00568000
*/*                                                                  */ 00569000
*/********************************************************************/ 00570000
*                                                                       00571000
* /*     THE FOLLOWING FIELDS ARE ALWAYS PRESENT WHEN MLWTO IS       */ 00572000
* /*     SPECIFIED.                                                  */ 00573000
*                                                                       00574000
*DECLARE                                                                00575000
*  1 WPLLS01  BASED(WPLPTR) BDY(BYTE),                                  00576000
*   2 WPLLTF      CHAR(2),               /* LINE TYPE FLAGS FOR MSG     00577000
*                                           TEXT IN WPLTXT           */ 00578000
*    3 WPLLTF1     BIT(8),               /* 1ST BYTE OF LINE TYPE       00579000
*                                           FLAGS FOR WPLTXT         */ 00580000
*     4 WPLLTFA  BIT(1),                 /* CONTROL LINE             */ 00581000
*     4 WPLLTFB  BIT(1),                 /* LABEL LINE               */ 00582000
*     4 WPLLTFC  BIT(1),                 /* DATA LINE                */ 00583000
*     4 WPLLTFD  BIT(1),                 /* END LINE                 */ 00584000
*     4 WPLRSV17 BIT(1),                 /* RESERVED                 */ 00585000
*     4 WPLRSV18 BIT(1),                 /* RESERVED                 */ 00586000
*     4 WPLRSV19 BIT(1),                 /* RESERVED                 */ 00587000
*     4 WPLRSV20 BIT(1),                 /* RESERVED                 */ 00588000
*    3 WPLLTF2     BIT(8),               /* 2ND BYTE OF LINE TYPE       00589000
*                                           FLAGS FOR WPLTXT         */ 00590000
*   2 WPLAREA     CHAR(1),               /* AREA IDENTIFICATION      */ 00591000
*   2 WPLLINES    PTR(8);                /* NUMBER OF LINES (1 +        00592000
*                                           NUMBER OF WPLMLTXT LINES)*/ 00593000
*                                                                       00594000
* /*     THE FOLLOWING FIELDS ARE OPTIONAL FOR MLWTO.  THEY REPRE-   */ 00595000
* /*     SENT A MAPPING OF THE ENTRIES DESCRIBING MESSAGE TEXT LINES */ 00596000
* /*     CREATED IN ADDITION TO THE WPLTXT MESSAGE TEXT LINE.        */ 00597000
*                                                                       00598000
*DECLARE                                                                00599000
*  1 WPLML  BASED(WPLPTR) BDY(WORD),     /* START OF ADDITIONAL MSG     00600000
*                                           TEXT LINE ENTRY          */ 00601000
*   2 WPLML0      PTR(8),                /* HIGH-ORDER BYTE OF LENGTH   00602000
*                                           FIELD FOR WPLMLTXT (ALWAYS  00603000
*                                           ZERO)                    */ 00604000
*   2 WPLMLLEN    PTR(8),                /* LENGTH OF ADDITIONAL MSG    00605000
*                                           TEXT LINE ENTRY (4 +        00606000
*                                           LENGTH OF WPLMLTXT TEXT) */ 00607000
*   2 WPLMLLTF    CHAR(2),               /* LINE TYPE FLAGS FOR         00608000
*                                           WPLMLTXT TEXT            */ 00609000
*    3 WPLMLLT1    BIT(8),               /* 1ST BYTE OF LINE TYPE       00610000
*                                           FLAGS FOR WPLMLTXT       */ 00611000
*     4 WPLMLLTA BIT(1),                 /* CONTROL LINE             */ 00612000
*     4 WPLMLLTB BIT(1),                 /* LABEL LINE               */ 00613000
*     4 WPLMLLTC BIT(1),                 /* DATA LINE                */ 00614000
*     4 WPLMLLTD BIT(1),                 /* END LINE                 */ 00615000
*     4 WPLRSV21 BIT(1),                 /* RESERVED                 */ 00616000
*     4 WPLRSV22 BIT(1),                 /* RESERVED                 */ 00617000
*     4 WPLRSV23 BIT(1),                 /* RESERVED                 */ 00618000
*     4 WPLRSV24 BIT(1),                 /* RESERVED                 */ 00619000
*    3 WPLMLLT2    BIT(8),               /* 2ND BYTE OF LINE TYPE       00620000
*                                           FLAGS FOR WPLMLTXT       */ 00621000
*                                                                       00622000
*   2 WPLMLTXT    CHAR(126);             /* ADDITIONAL LINE MESSAGE     00623000
*                                           TEXT (MAX 126 CHARS)     */ 00624000
*                                                                       00625000
* /************************** END OF WPL *****************************/ 00626000
./ ADD NAME=WTO
*  %/*                                                                  00000100
         MACRO                                                          00000200
&NAME   WTO    &MESG,&MF=I,&ROUTCDE=,&DESC=,&MSGTYP=,&MCSFLAG=,&QID=,&AX00000300
               REAID=,&TEXT=^                                           00000400
.********************************************************************** 00000500
.*                                                                      00000600
.*       WTO - WRITE TO OPERATOR                                        00000700
.*                                                                      00000800
.*       INVOCATION - SPECIFY -  WTO MESG,ROUTCDE=,DESC=,MSGTYP=,       00000900
.*                            MCSFLAG=,QID=,AREAID=,MF=,TEXT=           00001000
.*              WHERE -                                                 00001100
.*                MESG     THE MESSAGE TEXT FOR A SINGLE OR             00001200
.*                         MULTIPLE LINE MESSAGE TO BE                  00001300
.*                         WRITTEN                                      00001400
.*                                                                      00001500
.*                ROUTCDE= ROUTING CODES TO BE ASSIGNED TO THE          00001600
.*                         MESSAGE                                      00001700
.*                                                                      00001800
.*                DESC=    DESCRIPTOR CODES TO BE ASSIGNED TO           00001900
.*                         THE MESSAGE                                  00002000
.*                                                                      00002100
.*                MSGTYP=  SPECIFIES HOW THE MESSAGE IS TO BE           00002200
.*                         ROUTED. VALID VALUES ARE:                    00002300
.*                         N,Y,SESS,JOBNAMES,STATUS,ACTIVE,             00002400
.*                         SHOW                                         00002500
.*                                                                      00002600
.*                MCSFLAG= SPECIFIES ATTRIBUTES OF THE                  00002700
.*                         MESSAGE. VALID VALUES ARE:                   00002800
.*                         REG0,RESP,REPLY,BRDCST,HRDCPY,               00002900
.*                         QREG0,NOTIME,NOCPY                           00003000
.*                                                                      00003100
.*                QID=     REMOTE ENTRY SERVICES (RSS) QUEUE            00003200
.*                         ID (OS/VS1 ONLY). PARAMETER ACCEPTED         00003300
.*                         BUT IGNORED BY MVS                           00003400
.*                                                                      00003500
.*                AREAID=  SPECIFIES A DISPLAY AREA ON THE              00003600
.*                         CONSOLE WHERE THE MESSAGE IS TO BE           00003700
.*                         WRITTEN.                                     00003800
.*                                                                      00003900
.*                MF=      SPECIFIES THE TYPE OF EXPANSION              00004000
.*                         REQUIRED. VALID VALUES ARE:                  00004100
.*                           I,L,E                                      00004200
.*                                                                      00004300
.*                TEXT=    SPECIFIES A DATA AREA CONTAINING A 2         00004400
.*                         BYTE MESSAGE LENGTH FIELD FOLLOWED BY        00004500
.*                         THE ACTUAL MESSAGE TEXT. IF A REGISTER       00004600
.*                         IS USED, THE REGISTER CONTAINS THE           00004700
.*                         ADDRESS OF THE DATA AREA. IF A FIELD         00004800
.*                         IS USED, THE ADDRESS OF THE FIELD IS         00004900
.*                         STORED IN THE WPL. THE TEXT KEYWORD IS       00005000
.*                         MUTUALLY EXCLUSIVE WITH INLINE               00005100
.*                         MESSAGE TEXT. EITHER TEXT OR INLINE          00005200
.*                         TEXT (BUT NOT BOTH) IS REQUIRED ON THE       00005300
.*                         STANDARD OR LIST FORM OF WTO                 00005400
.*                                                                      00005500
.*       FUNCTION - BUILDS ONE OF THREE POSSIBLE PARAMETER LIST         00005600
.*                  FORMATS AND/OR THE CODE WHICH WILL INVOKE           00005700
.*                  SVC 35 TO ISSUE THE MESSAGE.                        00005800
.*                  1. A STANDARD WPL IS BUILT IF INLINE TEXT IS        00005900
.*                     SPECIFIED AND NO ROUTING CODE GREATER THAN 16    00006000
.*                     IS SPECIFIED.                                    00006100
.*                  2. IF INLINE TEXT IS SPECIFIED AND A ROUTING CODE   00006200
.*                     GREATER THAN 16 IS SPECIFIED THEN AN EXTENDED    00006300
.*                     WPL VERSION 1 IS BUILT.                          00006400
.*                  3. IF THE TEXT OPERAND IS SPECIFIED THEN AN         00006500
.*                     EXTENDED WPL VERSION 2 IS GENERATED.             00006600
.*                                                                      00006700
.*       STATUS -                                                       00006800
.*       LASTUPD         = EBB1102  TYPE=ADD                            00006900
.*       LIBRARIES       = DISTLIB=AMACLIB                              00007000
.*       FMID            = FBB1221                                      00007100
.*       RMID            = UZ31484                                      00007200
.*       SYSMOD HISTORY  = SYSMOD   TYPE       DATE   MCS  STATUS       00007300
.*                         EBB1102  FUNCTION  00.000  MAC  ACC RGN      00007400
.*                         FBB1221  FUNCTION  00.000  MAC  ACC RGN      00007500
.*                                                                      00007600
.*       CHANGE ACTIVITY -                                              00007700
.*              ZP60039  - EXTENSIVE REVISION OF THE MACRO AND          00007800
.*                         SUPPORT FOR THE TEXT OPERAND WHICH           00007900
.*                         REQUIRES GENERATION OF AN EXTENDED WPL       00008000
.*                                                                      00008100
.********************************************************************** 00008200
         GBLB  &IHBWTL                                                  00008300
         LCLA  &LT(256),&H,&I,&II,&N,&J,&K,&LEN,&LLCNT                  00008400
         LCLA  &NUMENT                 NUMBER OF SUBLIST ENTRIES        00008500
         LCLA  &LENENT                 L'ENTRY                          00008600
         LCLB  &AD,&E,&E1,&E2,&E3,&E4,&E5,&E6,&MLW  MLWTO FLAGS         00008700
         LCLB  &SECONDL,&PAIR                                           00008800
         LCLC  &GNAME                                                   00008900
         LCLB  &RO(128),&DE(16),&MC(32),&MT(16)  FLAGS FOR THE FIELDS   00009000
         LCLC  &LOWNUM,&HIGHNUM        ROUTE RANGE NUMBERS              00009100
         LCLB  &GENRD                  GENERATE ROUT AND DESC FIELDS    00009200
         LCLB  &GENMT                  GENERATE MSGTYP FIELD            00009300
         LCLB  &GENXWPL                GENERATE AN XWPL                 00009400
         LCLA  &WPLXLEV                LEVEL OF XWPL TO GENERATE        00009500
         LCLB  &MTY,&MTN               FLAGS FOR MSGTYP PROCESSING      00009600
         ACTR  30000                                                    00009700
.*                                                                      00009800
.*       INITIALIZATION                                                 00009900
.*                                                                      00010000
&GNAME   SETC  'IHB'.'&SYSNDX'         FOR BAL INSTRUCTION              00010100
&WPLXLEV SETA  1                                                        00010200
.********************************************************************** 00010300
.*                                                                      00010400
.*       BEGIN PROCESSING PARAMETERS                                    00010500
.*                                                                      00010600
.********************************************************************** 00010700
.********************************************************************** 00010800
.*                                                                      00010900
.*        PROCESS ROUTING CODES                                         00011000
.*        DETERMINE IF ROUTING CODES HAVE BEEN SPECIFIED                00011100
.*                                                                      00011200
.*        ROUTING CODES 1-128 MAY BE SET HOWEVER FOR MVS 3.8 ONLY       00011300
.*        ONLY ROUTING CODES 1-16 WILL BE ACTIONED                      00011400
.*        ANY ROUTING CODE GREATER THAN 16 WILL BE IGNORED              00011500
.*        WITHOUT ERROR HOWEVER SPECIFYING A ROUTING CODE GREATER       00011600
.*        THAN 16 WILL CAUSE THE GENERATION OF AN EXTENDED FORMAT       00011700
.*        VERSION 1 WPL (XWPL).                                         00011800
.*                                                                      00011900
.********************************************************************** 00012000
         AIF   (T'&ROUTCDE EQ 'O').ENDROUT   ROUTING CODES APECIFIED ?  00012100
&GENRD   SETB  1                    ROUTING CODES PRESENT               00012200
&I       SETA  1                    INITIALIZE LIST INDEX               00012300
&NUMENT  SETA  N'&ROUTCDE           TOTAL NUMBER OF ENTRIES IN &ROUTCDE 00012400
.*       ROUTING CODE LOOP                                              00012500
.ROULOOP ANOP                                                           00012600
&LENENT  SETA  K'&ROUTCDE(&I)       SET L'CURRENT ELEMENT               00012700
         AIF   (&LENENT EQ 0).BADENT  NULL ENTRY, ERROR                 00012800
         AIF   (T'&ROUTCDE(&I) EQ 'N').ROUVAL   SINGLE NUMERIC VALUE    00012900
.*                                                                      00013000
.*       RANGE OF ROUTING CODES HAS BEEN SPECIFIED                      00013100
.*                                                                      00013200
.*       SCAN FOR DASH SEPERATOR                                        00013300
.*                                                                      00013400
&II      SETA  1                    INITIALIZE ELEMENT INDEX            00013500
.DASHL   ANOP                       DASH SCANNING LOOP                  00013600
         AIF   ('&ROUTCDE(&I)'(&II,1) EQ '-').DASHFND                   00013700
&II      SETA  &II+1                INCREMENT ELEMENT INDEX             00013800
         AIF   (&II GE &LENENT).BADRANG   CHECK INDEX POSITION          00013900
         AGO   .DASHL               LOOP AROUND FOR NEXT CHAR           00014000
.*       FOUND A DASH SEPERATOR                                         00014100
.DASHFND ANOP                                                           00014200
         AIF   (&II EQ 1).BADRANG         DASH UP FRONT ? ERROR         00014300
         AIF   (&II EQ &LENENT).BADRANG   DASH AT THE END ? ERROR       00014400
&LOWNUM  SETC  '&ROUTCDE(&I)'(1,&II-1) SET FIRST NUMBER OF RANGE        00014500
&HIGHNUM SETC  '&ROUTCDE(&I)'(&II+1,&LENENT-&II) SET SECOND NUMBER      00014600
.*       FIRST NUMBER > LAST NUMBER ? ERROR                             00014700
         AIF   ('&LOWNUM' GT '&HIGHNUM').BADRANG                        00014800
.*       OUTSIDE THE RANGE OF VALID ROUTING CODES ? ERROR               00014900
         AIF   ('&LOWNUM' LT '1' OR '&LOWNUM' GT '128' OR              X00015000
               '&HIGHNUM' LT '1' OR '&HIGHNUM' GT '128').BADRANG        00015100
&J       SETA  &LOWNUM              INITIALIZE START OF RANGE           00015200
&K       SETA  &HIGHNUM             INITIALIZE END OF RANGE             00015300
.*       LOOP TO SET ON ROUTING CODES                                   00015400
.*       IF ALREADY SET ON THEN ERROR                                   00015500
.RANLOOP ANOP                                                           00015600
         AIF   (&RO(&J)).BADRANG                                        00015700
&RO(&J)  SETB  1                    SET ROUTING CODE &J ON              00015800
         AIF   (&J LT 17).RANLOPA                                       00015900
&GENXWPL SETB  1                    ANY VALUE > 16 FORCES A XWPL        00016000
.RANLOPA ANOP                                                           00016100
&J       SETA  &J+1                 INCREMENT LOOP INDEX                00016200
         AIF   (&J LE &K).RANLOOP   LOOP THROUGH RANGE                  00016300
.*       RANGE PROCESSED                                                00016400
         AGO   .NEXTROU             DETERMINE NEXT ROUTING CODE         00016500
.*                                                                      00016600
.*       ROUTING CODE VALUE HAS BEEN SPECIFIED. VERIFY THAT IT IS       00016700
.*       WITHIN THE PROPER RANGE AND, IF SO, SET THE CORRECT BIT        00016800
.*                                                                      00016900
.ROUVAL  ANOP                                                           00017000
&J       SETA  &ROUTCDE(&I)         GET NEXT ROUTING CODE VALUE         00017100
         AIF   (&J GE 1 AND &J LE 128).ROUOK   VALID VALUE ?            00017200
         MNOTE 4,'ROUTCDE &J IS AN INVALID ROUTING CODE'                00017300
         AGO   .NEXTROU                                                 00017400
.ROUOK   ANOP                       VALID ROUTING CODE SPECIFIED        00017500
         AIF   (&RO(&J)).BADRANG                                        00017600
&RO(&J)  SETB  1                    SET ROUTING CODE &J ON              00017700
         AIF   (&J LT 17).NEXTROU                                       00017800
&GENXWPL SETB  1                    ANY VALUE > 16 FORCES A XWPL        00017900
&MC(12)  SETB  1                    SET MCS FLAG FOR XWPL (WPLMCSFL)    00018000
         AGO   .NEXTROU             LOOP TO NEXT VALUE                  00018100
.*       A NULL ROUTING CODE HAS BEEN SPECIFIED                         00018200
.*       ISSUE AN MNOTE AND SKIP TO THE NEXT ROUTING CODE               00018300
.BADENT  ANOP                                                           00018400
         MNOTE 4,'NULL ROUTING CODE SPECIFIED'                          00018500
         AGO   .NEXTROU                                                 00018600
.*       AN INVALID ROUTING CODE RANGE HAS BEEN SPECIFIED               00018700
.*       ISSUE AN MNOTE AND SKIP TO THE NEXT ROUTING CODE               00018800
.BADRANG ANOP                                                           00018900
         MNOTE 4,'INVALID RANGE OF ROUTING CODES IGNORED.'              00019000
.*       FALL THROUGH TO SETUP FOR NEXT VALUE                           00019100
.*                                                                      00019200
.*       SETUP TO PROCESS NEXT ROUTE CODE ENTRY                         00019300
.NEXTROU ANOP                                                           00019400
&I       SETA  &I+1                 INCREMENT INDEX TO ROUT CODE VALUES 00019500
         AIF   (&I LE &NUMENT).ROULOOP IF MORE CODES, CHECK NEXT ONE    00019600
.ENDROUT ANOP                       DONE WITH ROUTING CODES             00019700
.********************************************************************** 00019800
.*                                                                      00019900
.*        PROCESS DESCRIPTOR CODES                                      00020000
.*        DETERMINE IF DESCRIPTOR CODES HAVE BEEN SPECIFIED             00020100
.*                                                                      00020200
.********************************************************************** 00020300
         AIF   (T'&DESC EQ 'O').ENDDESC   NO DESCRIPTOR, BRANCH         00020400
&GENRD   SETB  1                 FORCE GENERATION OF DESC AND ROUTCDES  00020500
&NUMENT  SETA  N'&DESC           NUMBER OF ENTRIES                      00020600
&I       SETA  1                                                        00020700
&J       SETA  0                 COUNTER TO DETECT MUTUALLY EXCL VALUE  00020800
.DESCLOP ANOP                                                           00020900
&II      SETA  &DESC(&I)                                                00021000
         AIF   (&II GE 1 AND &II LE 16).DESCOK                          00021100
.DESCERR MNOTE 8,'DESC &N IS INVALID DESCRIPTOR - IGNORED'              00021200
         AGO   .NEXTDES                                                 00021300
.DESCOK  ANOP                                                           00021400
         AIF   (&DE(&II)).DESERR            ALREADY SPECIFIED           00021500
         AIF   (&II GT 6).DESCJMP                                       00021600
&J       SETA  &J+1                         A VALUE LOWER THAN 7 SPEC   00021700
.DESCJMP ANOP                                                           00021800
&DE(&II) SETB  1                                                        00021900
.NEXTDES ANOP                                                           00022000
&I       SETA  &I+1                         INCR ENTRY COUNTER          00022100
         AIF   (&I LE &NUMENT).DESCLOP      PROCESS NEXT ENTRY          00022200
.*       ALL ENTRIES PROCESSED                                          00022300
.*       TEST FOR ANY MUTUALLY EXCLUSIVE ENTRIES                        00022400
         AIF   (&J LT 2).ENDDESC            ALL OK                      00022500
         MNOTE 8,'DESC MUTUALLY EXCLUSIVE VALUE SPECIFIED'              00022600
         MEXIT                                                          00022700
.DESERR  MNOTE 8,'DUPLICATE DESCRIPTOR VALUE SPECIFIED'                 00022800
         MEXIT                                                          00022900
.ENDDESC ANOP                                                           00023000
.********************************************************************** 00023100
.*                                                                      00023200
.*       PROCESS MSGTYP PARAMETERS                                      00023300
.*       DETERMINE IF MSGTYP PARAMETERS HAVE BEEN SPECIFIED             00023400
.*       PARAMETERS -                                                   00023500
.*       Y        - GENERATES MSGTYP FIELD BUT IGNORES OTHER            00023600
.*                  PARAMETERS (ALL MSGTYP FLAGS ZERO)                  00023700
.*                  THIS MUST BE THE FIRST PARAMETER                    00023800
.*       N        - SUPPESSSES THE GENERATION OF THE MSGTYP             00023900
.*                  FIELD EVEN THOUGH OTHER PARAMETERS HAVE BEEN        00024000
.*                  SPECIFIED                                           00024100
.*                  THIS MUST BE THE FIRST PARAMETER                    00024200
.*       JOBNAMES -                                                     00024300
.*       STATUS   -                                                     00024400
.*       ACTIVE   -                                                     00024500
.*       SHOW     - FOR VS/1 CRJE SUPPORT                               00024600
.*       SESS     -                                                     00024700
.*       NOTE THAT THE MSGTYP FIELD WILL BE GENERATED AND THE           00024800
.*       WPLMSGTD FLAG WILL BE SET IF THE QID PARAMETER IS              00024900
.*       CODED ON THE WTO MACRO                                         00025000
.*                                                                      00025100
.********************************************************************** 00025200
         AIF   (T'&MSGTYP EQ 'O').ENDMSGT   MSGTYP PARAMS SPECIFIED ?   00025300
.*                                          YES, USE OF MSGTYPE FORCES  00025400
&GENRD   SETB  1                            GENERATION OF ROUT/DESC     00025500
&NUMENT  SETA  N'&MSGTYP                    NUMBER OF ENTRIES           00025600
         AIF   ('&MSGTYP(1)' NE 'N').MSGL1                              00025700
&MTN     SETB  1                            MSGTYP=N CODED              00025800
         AGO   .MSGL2                                                   00025900
.MSGL1   AIF   ('&MSGTYP(1)' NE 'Y').MSGL3  MSGTYP=Y CODED              00026000
&MTY     SETB  1                                                        00026100
.MSGL2   ANOP                               EITHER MSGTYP =Y/N CODED    00026200
&I       SETA  1                            FIRST PARAMETER PROCESSED   00026300
         AGO   .NEXTMSG                                                 00026400
.MSGL3   ANOP                                                           00026500
&I       SETA  0                                                        00026600
         AGO   .NEXTMSG                                                 00026700
.MSGTLOP ANOP                               LOOP THROUGH PARAMETERS     00026800
         AIF   ('&MSGTYP(&I)' NE 'JOBNAMES').MSGL4                      00026900
         AIF   (&MT(1)).MTERR               JOBNAMES ALREADY CODED ?    00027000
&MT(1)   SETB  1                            TURN ON JOBNAMES            00027100
         AGO   .NEXTMSG                                                 00027200
.MSGL4   ANOP                                                           00027300
         AIF   ('&MSGTYP(&I)' NE 'STATUS').MSGL5                        00027400
         AIF   (&MT(2)).MTERR               STATUS ALREADY CODED ?      00027500
&MT(2)   SETB  1                            TURN ON STATUS              00027600
         AGO   .NEXTMSG                                                 00027700
.MSGL5   ANOP                                                           00027800
         AIF   ('&MSGTYP(&I)' NE 'ACTIVE').MSGL6                        00027900
         AIF   (&MT(3)).MTERR               ACTIVE ALREADY CODED ?      00028000
&MT(3)   SETB  1                            TURN ON ACTIVE              00028100
         AGO   .NEXTMSG                                                 00028200
.MSGL6   ANOP                                                           00028300
         AIF   ('&MSGTYP(&I)' NE 'SHOW').MSGL7                          00028400
         AIF   (&MT(5)).MTERR               SHOW ALREADY CODED ?        00028500
&MT(5)   SETB  1                            TURN ON SHOW                00028600
         AGO   .NEXTMSG                                                 00028700
.MSGL7   ANOP                                                           00028800
         AIF   ('&MSGTYP(&I)' NE 'SESS').MTERR                          00028900
         AIF   (&MT(6)).MTERR               SESS ALREADY CODED ?        00029000
&MT(6)   SETB  1                            TURN ON SESS                00029100
.NEXTMSG ANOP                                                           00029200
&GENMT   SETB  1                            GENERATE MSGTYP FIELD       00029300
&I       SETA  &I+1                         INCR ENTRY COUNTER          00029400
         AIF   (&I LE &NUMENT).MSGTLOP      PROCESS NEXT ENTRY          00029500
.*       ALL ENTRIES PROCESSED                                          00029600
.*       PROCESS FLAGS                                                  00029700
&GENMT   SETB  (NOT &MTN)                   SUPRESS MT GEN IF MSGTYP=N  00029800
         AIF   (&MTN OR &MTY).MSGL8                                     00029900
         AGO   .ENDMSGT                                                 00030000
.*       ERROR PROCESSING                                               00030100
.MTERR   MNOTE 8,'MESSAGE TYPE FIELD INVALID - N IS ASSUMED'            00030200
&MTN     SETB  1                                                        00030300
&MTY     SETB  0                                                        00030400
.MSGL8   ANOP                               TURN OFF ALL FLAGS          00030500
&MT(1)   SETB  0                                                        00030600
&MT(2)   SETB  0                                                        00030700
&MT(3)   SETB  0                                                        00030800
&MT(5)   SETB  0                                                        00030900
&MT(6)   SETB  0                                                        00031000
.ENDMSGT ANOP                                                           00031100
.********************************************************************** 00031200
.*                                                                      00031300
.*       PROCESS MCSFLAG PARAMETERS                                     00031400
.*       DETERMINE IF MCSFLAG PARAMETERS HAVE BEEN SPECIFIED            00031500
.*       PARAMETERS -                                                   00031600
.*       REG0     - MSG TO CONSOLE WITH SOURCE ID IN R0                 00031700
.*       RESP     - WTO IS IMMEDIATE COMMAND RESPONSE                   00031800
.*       REPLY    - WTO IS A REPLY TO A WTOR                            00031900
.*       BRDCAST  - MSG TO BE BROADCAST TO ALL ACTIVE CONSOLES          00032000
.*       HRDCOPY  - MSG TO BE QUEUED FOR HARDCOPY ONLY                  00032100
.*       QREG0    - MSG TO BE QUEUED UNCONDITIONALLY TO CON ID IN R0    00032200
.*       NOTIME   - TIME IS NOT TO BE APPENDED TO MSG  TER              00032300
.*       NOCPY    - IF WTO OR WTOR IS ISSUED IN SUPERVISOR STATE        00032400
.*                  THEN DO NOT QUEUE FOR HARDCOPY                      00032500
.*                                                                      00032600
.********************************************************************** 00032700
         AIF   (T'&MCSFLAG EQ 'O').ENDMCS    MCSFLAG PARAMS SPECIFIED ? 00032800
&I       SETA  1                    SET LIST INDEX                      00032900
&NUMENT  SETA  N'&MCSFLAG           NUMBER OF ENTRIES                   00033000
.MCLOOP  ANOP                       START LOOP THROUGH PARAMETERS       00033100
         AIF   ('&MCSFLAG(&I)' NE 'REG0').MCL1                          00033200
         AIF   (&MC(2)).MCERR       REG0 ALREADY CODED ?                00033300
&MC(2)   SETB  1                    TURN ON REG0 (WPLMCSFB)             00033400
         AGO   .NEXTMC                                                  00033500
.MCL1    ANOP                                                           00033600
         AIF   ('&MCSFLAG(&I)' NE 'RESP').MCL2                          00033700
         AIF   (&MC(3)).MCERR       RESP ALREADY CODED ?                00033800
&MC(3)   SETB  1                    TURN ON RESP (WPLMCSFC)             00033900
         AGO   .NEXTMC                                                  00034000
.MCL2    ANOP                                                           00034100
         AIF   ('&MCSFLAG(&I)' NE 'REPLY').MCL3                         00034200
         AIF   (&MC(5)).MCERR       REPLYG0 ALREADY CODED ?             00034300
&MC(5)   SETB  1                    TURN ON REPLY (WPLMCSFE)            00034400
         AGO   .NEXTMC                                                  00034500
.MCL3    ANOP                                                           00034600
         AIF   ('&MCSFLAG(&I)' NE 'BRDCAST').MCL4                       00034700
         AIF   (&MC(6)).MCERR       BRDCAST ALREADY CODED ?             00034800
&MC(6)   SETB  1                    TURN ON BRDCAST (WPLMCSFF)          00034900
         AGO   .NEXTMC                                                  00035000
.MCL4    ANOP                                                           00035100
         AIF   ('&MCSFLAG(&I)' NE 'HRDCPY').MCL5                        00035200
         AIF   (&MC(7)).MCERR       HRDCPY ALREADY CODED ?              00035300
&MC(7)   SETB  1                    TURN ON HRDCPY (WPLMCSFG)           00035400
         AGO   .NEXTMC                                                  00035500
.MCL5    ANOP                                                           00035600
         AIF   ('&MCSFLAG(&I)' NE 'QREG0').MCL6                         00035700
         AIF   (&MC(8)).MCERR       QREG0 ALREADY CODED ?               00035800
&MC(8)   SETB  1                    TURN ON QREG0 (WPLMCSFH)            00035900
         AGO   .NEXTMC                                                  00036000
.MCL6    ANOP                                                           00036100
         AIF   ('&MCSFLAG(&I)' NE 'NOTIME').MCL7                        00036200
         AIF   (&MC(9)).MCERR       NOTIME ALREADY CODED ?              00036300
&MC(9)   SETB  1                    TURN ON NOTIME (WPLMCSFI)           00036400
         AGO   .NEXTMC                                                  00036500
.MCL7    ANOP                                                           00036600
         AIF   ('&MCSFLAG(&I)' NE 'NOCPY').MCERR                        00036700
         AIF   (&MC(14)).MCERR      NOTIME ALREADY CODED ?              00036800
&MC(14)  SETB  1                    TURN ON NOTTIME (WPLMCSFN)          00036900
         AGO   .NEXTMC                                                  00037000
.MCERR   ANOP                                                           00037100
         MNOTE 8,'&MCSFLAG(&I) IS INVALID - IGNORED'                    00037200
.NEXTMC  ANOP                       INCR ENTRY INDEX                    00037300
&I       SETA  &I+1                                                     00037400
         AIF   (&I LE &NUMENT).MCLOOP                                   00037500
.*       TEST FOR ERRORS                                                00037600
         AIF   (&MC(7) AND &MC(14)).MCEXCL    HRDCOPY AND NOCPY ?       00037700
         AGO   .ENDMCS                                                  00037800
.MCEXCL  MNOTE 8,'HRDCPY AND NOCPY MUTUALLY EXCLUSIVE, HRDCPY ASSUMED'  00037900
&MC(14)  SETB  0                      TURN OFF NOCPY                    00038000
.ENDMCS  ANOP                                                           00038100
.********************************************************************** 00038200
.*                                                                      00038300
.*       ALL PARAMETERS EXCEPT MESSAGE TEXT AND MF HAVE BEEN PROCESSED  00038400
.*       PROCESS ANY PARAMETER INTERACTIONS                             00038500
.*                                                                      00038600
.********************************************************************** 00038700
.*       DETERMINE IF A DEFAULT ROUTING CODE OF 2 HAS TO BE SET         00038800
.*                                                                      00038900
.*       IF THE GENERATION OF ROUTING AND DESCRIPTOR CODES FLAG         00039000
.*       HAS BEEN SET HOWEVER NO ROUTING AND DESCRIPTOR CODES           00039100
.*       HAVE BEEN PROVIDED THEN SET ROUTING CODE 2 AS A DEFAULT        00039200
         AIF   ((&GENRD) AND (T'&ROUTCDE EQ 'O')).INT1A                 00039300
         AIF   (NOT &GENMT).INT1    NO MSGTYP, BRANCH                   00039400
         AIF   (&MC(2) OR &MC(8)).INT1   MCS REG0 OR QREG0 CODED ?      00039500
.INT1A   ANOP                                                           00039600
.*       SET A DEFAULT ROUTING CODE OF 2                                00039700
&GENRD   SETB  1                                                        00039800
&RO(2)   SETB  1                                                        00039900
.INT1    ANOP                                                           00040000
.*       QID FORCES GENERATION OF DESC AND ROUTE FIELDS                 00040100
&GENRD   SETB  (&GENRD OR (T'&QID NE 'O'))                              00040200
.*       DETERMINE IF THE MCS FLAG FOR ROUT/DESC PRESENT SHOULD BE ON   00040300
&MC(1)   SETB  (&GENRD)                                                 00040400
.*       DETERMINE IF MCS FLAG FOR MSGTYP PRESENT SHOULD BE ON          00040500
&MC(4)   SETB  (&GENMT)                                                 00040600
.********************************************************************** 00040700
.*                                                                      00040800
.*       DETERMINE SOURCE OF TEXT FOR MESSAGE                           00040900
.*                                                                      00041000
.********************************************************************** 00041100
.*       MESSAGE TEXT AND INLINE TEXT ARE MUTUALLY EXCLUSIVE            00041200
.*       ISSUE ERROR MESSAGE IF CODED EXCEPT IF MF=E                    00041300
         AIF   (('&MESG' NE '') AND ('&TEXT' NE '^')).INT2              00041400
         AGO   .INT3                                                    00041500
.INT2    MNOTE 12,'''TEXT'' AND INLINE TEXT ARE MUTUALLY EXCLUSIVE'     00041600
         AGO   .END                                                     00041700
.*       EITHER MESG OR TEXT MUST BE SPECIFIED UNLESS MF=E              00041800
.INT3    AIF   ('&MF(1)' EQ 'E').INT5                                   00041900
         AIF   (('&MESG' EQ '') AND ('&TEXT' EQ '^')).INT4              00042000
         AGO   .INT5                                                    00042100
.INT4    MNOTE 12,'NO MESSAGE TEXT OR TEXT PARAMETER SPECIFIED'         00042200
         AGO   .END                                                     00042300
.INT5    ANOP                                                           00042400
.*       USE OF TEXT= PARAMETER FORCES GENERATION OF TYPE 2 XWPL        00042500
         AIF   ('&TEXT' EQ '^').INT6                                    00042600
&GENXWPL SETB  1                       GENERATE AN XWPL                 00042700
&WPLXLEV SETA  2                       LEVEL OF XWPL TO GENERATE        00042800
&MC(12)  SETB  1                       SET MCS FLAG FOR XWPL (WPLMCSFL) 00042900
&MC(25)  SETB  1                       TEXT ADDR SPECIFIED (WPXTXTAD)   00043000
.INT6    ANOP                                                           00043100
.********************************************************************** 00043200
.*                                                                      00043300
.*       PROCESS MF PARAMETER                                           00043400
.*                                                                      00043500
.********************************************************************** 00043600
         AIF    ('&MF' EQ 'I' OR '&MF' EQ 'L').WPLGEN                   00043700
.*                                                                      00043800
.*       MF=(E,X) PROCESSING                                            00043900
.*                                                                      00044000
         AIF   ('&MF(1)' NE 'E').ERROR1                                 00044100
         AIF   (N'&MF NE 2).ERROR1                                      00044200
&NAME    IHBINNRA &MF(2)            GENERATE INITIAL LR OR LA IF NEEDED 00044300
         AIF   (&IHBWTL).END                                            00044400
         AIF   (NOT &GENXWPL).MFESVC                                    00044500
.*       UPDATE MCS FLAGS                                               00044600
         OI    2(1),B'&MC(1)&MC(2)&MC(3)&MC(4)&MC(5)&MC(6)&MC(7)&MC(8)'X00044700
                                        MCS FLAGS                       00044800
         OI    3(1),B'&MC(9)&MC(10)&MC(11)&MC(12)&MC(13)&MC(14)&MC(15)&X00044900
               MC(16)'                  MCS FLAGS                       00045000
.*       ANY NEED TO UPDATE ADDITIONAL WPLX FIELDS ?                    00045100
         AIF   ((NOT &GENRD) AND (NOT &GENMT) AND (NOT &MC(25))).MFESVC 00045200
         AIF   ('&TEXT' EQ '^').MFE1                                    00045300
         AIF   ('&TEXT'(1,1) EQ '(').MFE2   TEXT=REG ?                  00045400
         LA    15,&TEXT                 R15 -> TEXT                     00045500
         ST    15,4(,1)                 STORE TEXT ADDR INTO XWPL       00045600
         AGO   .MFE1                                                    00045700
.MFE2    ST    &TEXT(1),4(,1)           STORE TEXT ADDR INTO XWPL       00045800
.MFE1    ANOP                                                           00045900
         LA    14,0(,1)                                                 00046000
         AH    14,0(,14)            ADD L'TEXT + LEN FIELD + MCS FIELD  00046100
         AIF   ('&TEXT' EQ '^').MFE3    NEED TO SET EXTENDED MCS ?      00046200
         OI    4(14),B'&MC(17)&MC(18)&MC(19)&MC(20)&MC(21)&MC(22)&MC(23X00046300
               )&MC(24)'                EXTENDED MCS                    00046400
         OI    5(14),B'&MC(25)&MC(26)&MC(27)&MC(28)&MC(29)&MC(30)&MC(31X00046500
               )&MC(32)'                EXTENDED MCS                    00046600
.MFE3    AIF   (NOT &GENRD).MFE4        NEED TO GEN ROUTE/DESC ?        00046700
         MVI   20(14),B'&DE(1)&DE(2)&DE(3)&DE(4)&DE(5)&DE(6)&DE(7)&DE(8X00046800
               )'                       DESCRIPTOR CODES                00046900
         MVI   21(14),B'&DE(9)&DE(10)&DE(11)&DE(12)&DE(13)&DE(14)&DE(15X00047000
               )&DE(16)'                DESCRIPTOR CODES                00047100
         MVI   24(14),B'&RO(1)&RO(2)&RO(3)&RO(4)&RO(5)&RO(6)&RO(7)&RO(8X00047200
               )'                       ROUTING CODES                   00047300
         MVI   25(14),B'&RO(9)&RO(10)&RO(11)&RO(12)&RO(13)&RO(14)&RO(15X00047400
               )&RO(16)'                ROUTING CODES                   00047500
.MFE4    AIF   (NOT &GENMT).MFESVC      NEED TO GEN MSGTYP FLAGS ?      00047600
         MVI   40(14),B'&MT(1)&MT(2)&MT(3)&MT(4)&MT(5)&MT(6)&MT(7)&MT(8X00047700
               )'                       MSGTYP                          00047800
         MVI   41(14),B'&MT(9)&MT(10)&MT(11)&MT(12)&MT(13)&MT(14)&MT(15X00047900
               )&MT(16)'                MSGTYP                          00048000
.MFESVC  ANOP                                                           00048100
         SVC   35                                                       00048200
         AGO   .END                 EXIT MACRO, PROCESSING COMPLETE     00048300
.*                                                                      00048400
.*       MF=I AND MF=L PROCESSING                                       00048500
.*                                                                      00048600
.WPLGEN  ANOP                                                           00048700
.*       TEXT FOR PRESENCE OF INLINE TEXT                               00048800
         AIF   (((N'&SYSLIST EQ 0) OR (N'&SYSLIST GT 255)) AND         X00048900
               ('&TEXT' EQ '^')).NOTXT                                  00049000
         AIF   ('&MF' EQ 'L').DCNAME                                    00049100
.*       MF=I PROCESSING                                                00049200
         CNOP  0,4                                                      00049300
&NAME    BAL   1,&GNAME.A                                               00049400
         AGO   .MESCHK                                                  00049500
.DCNAME  ANOP                                                           00049600
&NAME    DC    0F'0'                                                    00049700
.MESCHK  ANOP                                                           00049800
         AIF   (&GENXWPL).XWPL01         GENERATE XWPL                  00049900
         AIF   ('&MESG' EQ '').NOTXT     TEXT SPECIFIED ? NO, ERROR     00050000
.*       TEST FOR MULTI-LINE WTO                                        00050100
&MLW     SETB  ((N'&SYSLIST NE 1) OR (N'&SYSLIST(1) NE 1))              00050200
&MC(10)  SETB  (&MLW)                    SET MLWTO IF APPROPRIATE       00050300
         AIF   (NOT &MLW).HDCYOK                                        00050400
         AIF   (NOT &MC(7)).HDCYOK                                      00050500
         IHBERMAC 248           MCSFLAG=HRDCPY INVALID FOR MLWTO        00050600
&GNAME.A DS    0H                                                       00050700
         MEXIT                  TERMINATE MACRO                         00050800
.HDCYOK  ANOP                                                           00050900
.GENDCS  AIF   (NOT &MLW).NOTMLW1  GENERATE REGULAR WTO                 00051000
.********************************************************************** 00051100
.*                                                                      00051200
.*       SETUP TO GENERATE MLWTO                                        00051300
.*                                                                      00051400
.********************************************************************** 00051500
.*                                                                      00051600
.*       SET LINETYPE                                                   00051700
.*                                                                      00051800
&H       SETA  1                                                        00051900
         AIF   ('&SYSLIST(1,1)' EQ '').MLW04                            00052000
         AIF   (N'&SYSLIST(1) GT 2).MLW05                               00052100
         AIF   ('&SYSLIST(1,2)' NE 'C').MLW02                           00052200
&LT(1)   SETA  80                                                       00052300
.MLW01   AIF   (N'&SYSLIST LE &H).MLW11                                 00052400
&H       SETA  &H+1                                                     00052500
         AIF   (N'&SYSLIST(&H) GT 2).MLW05                              00052600
.MLW02   AIF   ('&SYSLIST(&H,2)' NE 'L' OR '&SYSLIST(&H,1)' EQ '').MLW0X00052700
               4                                                        00052800
&LT(&H)  SETA  40                                                       00052900
         AIF   (&SECONDL).MLW03                                         00053000
&SECONDL SETB  1                                                        00053100
         AGO   .MLW01                                                   00053200
.MLW03   AIF   (N'&SYSLIST LE &H).MLW11                                 00053300
&H       SETA  &H+1                                                     00053400
         AIF   (N'&SYSLIST(&H) GT 2).MLW05                              00053500
.MLW04   AIF   ('&SYSLIST(&H,2)' EQ 'E').MLW06                          00053600
         AIF   ('&SYSLIST(&H,1)' EQ '').MLW05                           00053700
         AIF   ('&SYSLIST(&H,2)' EQ 'DE').MLW08                         00053800
         AIF   ('&SYSLIST(&H,2)' EQ 'L' OR '&SYSLIST(&H,2)' EQ 'C').MLWX00053900
               09                                                       00054000
         AIF   ('&SYSLIST(&H,2)' NE 'D' AND '&SYSLIST(&H,2)' NE '').MLW*00054100
               10                                                       00054200
&LT(&H)  SETA  20                                                       00054300
         AGO   .MLW03                                                   00054400
.MLW05   ANOP                                                           00054500
&E5      SETB  1                                                        00054600
&LT(&H)  SETA  10                                                       00054700
         AGO   .MLW11                                                   00054800
.MLW06   ANOP                                                           00054900
&LT(&H)  SETA  10                                                       00055000
.MLW07   ANOP                                                           00055100
&E4      SETB  (&H NE N'&SYSLIST)                                       00055200
         AGO   .MLW11                                                   00055300
.MLW08   ANOP                                                           00055400
&LT(&H)  SETA  30                                                       00055500
         AGO   .MLW07                                                   00055600
.MLW09   ANOP                                                           00055700
&E3      SETB  1                                                        00055800
&LT(&H)  SETA  30                                                       00055900
         AGO   .MLW11                                                   00056000
.MLW10   ANOP                                                           00056100
&E5      SETB  1                                                        00056200
&LT(&H)  SETA  30                                                       00056300
.MLW11   ANOP                                                           00056400
&LLCNT   SETA  &H                                                       00056500
&H       SETA  1                                                        00056600
.********************************************************************** 00056700
.*                                                                      00056800
.*       GENERATE START OF WPL                                          00056900
.*                                                                      00057000
.********************************************************************** 00057100
.NOTMLW1 ANOP                                                           00057200
&I       SETA  1                                                        00057300
&LEN     SETA  K'&SYSLIST(1,1)-2                                        00057400
&PAIR    SETB  0                                                        00057500
.QLOOP1  ANOP                                                           00057600
&I       SETA  &I+1+&PAIR                                               00057700
         AIF   (&I GE K'&SYSLIST(1,1)).QDONE1                           00057800
&PAIR    SETB  ('&SYSLIST(1,1)'(&I,2) EQ '''''' OR '&SYSLIST(1,1)'(&I,2X00057900
               ) EQ '&&')                                               00058000
&LEN     SETA  &LEN-&PAIR                                               00058100
         AGO   .QLOOP1                                                  00058200
.QDONE1  ANOP                                                           00058300
&AD      SETB  (&LT(1) NE 10)      0 IF E-TYPE LINE, 1 IF NOT           00058400
&LEN     SETA  4+&LEN*&AD                                               00058500
         DC    AL2(&LEN)               TEXT LENGTH                      00058600
         DC    B'&MC(1)&MC(2)&MC(3)&MC(4)&MC(5)&MC(6)&MC(7)&MC(8)&MC(9)X00058700
               &MC(10)&MC(11)&MC(12)&MC(13)&MC(14)&MC(15)&MC(16)'      X00058800
                                       MCS FLAGS                        00058900
         AIF   (&LEN EQ 4).SKIPDC                                       00059000
         DC    C&SYSLIST(1,1)          MESSAGE TEXT                     00059100
.SKIPDC  AIF   (NOT &GENRD).OLDEXIT    GENERATING ROUT/DESC/MSGTYP ?    00059200
         DC    B'&DE(1)&DE(2)&DE(3)&DE(4)&DE(5)&DE(6)&DE(7)&DE(8)&DE(9)X00059300
               &DE(10)&DE(11)&DE(12)&DE(13)&DE(14)&DE(15)&DE(16)'      X00059400
                                       DESCRIPTOR CODES                 00059500
         DC    B'&RO(1)&RO(2)&RO(3)&RO(4)&RO(5)&RO(6)&RO(7)&RO(8)&RO(9)X00059600
               &RO(10)&RO(11)&RO(12)&RO(13)&RO(14)&RO(15)&RO(16)'      X00059700
                                       ROUTING CODES                    00059800
         AIF   (NOT &GENMT).OLDEXIT    GENERATING MSGTYP ?              00059900
         DC    B'&MT(1)&MT(2)&MT(3)&MT(4)&MT(5)&MT(6)&MT(7)&MT(8)&MT(9)X00060000
               &MT(10)&MT(11)&MT(12)&MT(13)&MT(14)&MT(15)&MT(16)'      X00060100
                                       MSGTYP                           00060200
.OLDEXIT AIF   (NOT &MLW).NOTMLW2                                       00060300
.********************************************************************** 00060400
.*                                                                      00060500
.*       GENERATE MLWTO FIELDS                                          00060600
.*                                                                      00060700
.********************************************************************** 00060800
         DC    XL2'&LT(1)00'           LINE TYPE                        00060900
         AIF   ('&AREAID' EQ '').ID0                                    00061000
         DC    CL1'&AREAID'            AREA ID                          00061100
         AGO   .IDA                                                     00061200
.ID0     DC    X'00'                   NULL AREA ID                     00061300
.IDA     ANOP                                                           00061400
         DC    AL1(&LLCNT)             TOTAL NUMBER OF LINES            00061500
.MLW12   AIF   (&H GE &LLCNT).MLW14                                     00061600
&H       SETA  &H+1                                                     00061700
&I       SETA  1                                                        00061800
&LEN     SETA  K'&SYSLIST(&H,1)-2                                       00061900
&PAIR    SETB  0                                                        00062000
.QLOOPH  ANOP                                                           00062100
&I       SETA  &I+1+&PAIR                                               00062200
         AIF   (&I GE K'&SYSLIST(&H,1)).QDONEH                          00062300
&PAIR    SETB  ('&SYSLIST(&H,1)'(&I,2) EQ '''''' OR '&SYSLIST(&H,1)'(&IX00062400
               ,2) EQ '&&')                                             00062500
&LEN     SETA  &LEN-&PAIR                                               00062600
         AGO   .QLOOPH                                                  00062700
.QDONEH  ANOP                                                           00062800
&AD      SETB  (&LT(&H) NE 10)     0 IF E-TYPE LINE, 1 IF NOT           00062900
&LEN     SETA  4+&LEN*&AD                                               00063000
         DC    AL2(&LEN)               LENGTH                           00063100
         DC    XL2'&LT(&H)00'          LINE TYPE                        00063200
         AIF   (&LEN EQ 4).MLW12                                        00063300
         DC    C&SYSLIST(&H,1)                                          00063400
         AGO   .MLW12                                                   00063500
.MLW14   AIF   (NOT &E4).MLW15                                          00063600
         IHBERMAC 242         GEN TERMINATED BY E OR DE LINE TYPE       00063700
.MLW15   AIF   (NOT &E5).MLW17                                          00063800
         IHBERMAC 243                                                   00063900
.MLW17   AIF   (NOT &E3).NOTMLW2                                        00064000
         IHBERMAC 244                                                   00064100
.NOTMLW2 AIF   ('&MF' NE 'I').END                                       00064200
&GNAME.A DS    0H                                                       00064300
.NOTMLW3 ANOP                                                           00064400
         AIF   (&E6 OR &E3 OR &IHBWTL).END                              00064500
         SVC   035                                                      00064600
         MEXIT                                                          00064700
.********************************************************************** 00064800
.*                                                                      00064900
.*       GENERATE XWPL                                                  00065000
.*                                                                      00065100
.********************************************************************** 00065200
.XWPL01  ANOP                                                           00065300
         AIF   ('&TEXT' NE '^').XWPL04     HAVE MESSAGE TEXT TO PROCESS 00065400
&I       SETA  1                                                        00065500
&LEN     SETA  K'&SYSLIST(1,1)-2    L'TEXT                              00065600
&PAIR    SETB  0                                                        00065700
.*       LOOP THROUGH TEXT STRING AND PROCESS DUPLICATE QUOTES          00065800
.*       AND AMPERSANDS                                                 00065900
.XWPL02  ANOP                                                           00066000
&I       SETA  &I+1+&PAIR                                               00066100
         AIF   (&I GE K'&SYSLIST(1,1)).XWPL03   GOTO LOOP EXIT          00066200
&PAIR    SETB  ('&SYSLIST(1,1)'(&I,2) EQ '''''' OR '&SYSLIST(1,1)'(&I,2X00066300
               ) EQ '&&')                                               00066400
&LEN     SETA  &LEN-&PAIR                                               00066500
         AGO   .XWPL02                 EXIT FROM LOOP                   00066600
.*       GENERATE XWPL FIELDS                                           00066700
.XWPL03  ANOP                                                           00066800
.*       GENERATE TEXT XWPL VERSION 1                                   00066900
&LEN     SETA  4+&LEN                                                   00067000
         AIF   (&LEN EQ 4).NOTXT                                        00067100
         DC    AL2(&LEN)                TEXT LENGTH                     00067200
         DC    B'&MC(1)&MC(2)&MC(3)&MC(4)&MC(5)&MC(6)&MC(7)&MC(8)&MC(9)X00067300
               &MC(10)&MC(11)&MC(12)&MC(13)&MC(14)&MC(15)&MC(16)'      X00067400
                                        MCS FLAGS                       00067500
         DC    C&SYSLIST(1,1)           MESSAGE TEXT                    00067600
         DC    AL1(1)                   XWPL VERSION LEVEL              00067700
         AGO   .XWPL05                                                  00067800
.*       GENERATE TEXT ADDR XWPL VERSION 2                              00067900
.XWPL04  ANOP                                                           00068000
         DC    AL2(8)                   TEXT LENGTH                     00068100
         DC    B'&MC(1)&MC(2)&MC(3)&MC(4)&MC(5)&MC(6)&MC(7)&MC(8)&MC(9)X00068200
               &MC(10)&MC(11)&MC(12)&MC(13)&MC(14)&MC(15)&MC(16)'      X00068300
                                        MCS FLAGS                       00068400
         DC    AL4(0)                   ADDR OF MESSAGE TEXT            00068500
         DC    AL1(2)                   XWPL VERSION LEVEL              00068600
.*       GENERATE FIELDS COMMON TO XWPL VER 1 AND XWPL VER 2            00068700
.XWPL05  ANOP                                                           00068800
         DC    B'00000000'              MISCELLANEOUS FLAGS             00068900
         DC    AL1(0)                   L'REPLY FOR WTOR                00069000
         AIF   (&WPLXLEV EQ 2).XWPL06   DETERMINE LEVEL OF XWPL         00069100
.*       XWPL LENGTH VALUE FOR XWPL VERSION 1                           00069200
         DC    AL1(0)                   RESERVED                        00069300
         AGO   .XWPL07                                                  00069400
.*       XWPL LENGTH VALUE FOR XWPL VERSION 2                           00069500
.XWPL06  ANOP                                                           00069600
         DC    AL1(104)                 LENGTH OF XWPL VERSION 2        00069700
.XWPL07  ANOP                                                           00069800
.*       GENERATE EXTENDED MCS FLAGS                                    00069900
         DC    B'&MC(17)&MC(18)&MC(19)&MC(20)&MC(21)&MC(22)&MC(23)&MC(2X00070000
               4)&MC(25)&MC(26)&MC(27)&MC(28)&MC(29)&MC(30)&MC(31)&MC(3X00070100
               2)'                      EXTENDED MCS FLGS               00070200
         DC    XL2'0000'                MCS FLAGS FOR CNTL PROGRAM USE  00070300
         DC    AL4(0)                   ADDR OF REPLY BUFFER FOR WTOR   00070400
         DC    AL4(0)                   ADDR OF REPLY ECB FOR WTOR      00070500
         DC    AL4(0)                   RESERVED - CONNECT ID           00070600
         DC    B'&DE(1)&DE(2)&DE(3)&DE(4)&DE(5)&DE(6)&DE(7)&DE(8)&DE(9)X00070700
               &DE(10)&DE(11)&DE(12)&DE(13)&DE(14)&DE(15)&DE(16)'      X00070800
                                        DESCRIPTOR CODES                00070900
         DC    AL2(0)                   RESERVED                        00071000
         DC    B'&RO(1)&RO(2)&RO(3)&RO(4)&RO(5)&RO(6)&RO(7)&RO(8)&RO(9)X00071100
               &RO(10)&RO(11)&RO(12)&RO(13)&RO(14)&RO(15)&RO(16)'      X00071200
                                        EXTENDED ROUTING CODES          00071300
         DC    B'&RO(17)&RO(18)&RO(19)&RO(20)&RO(21)&RO(22)&RO(23)&RO(2X00071400
               4)&RO(25)&RO(26)&RO(27)&RO(28)&RO(29)&RO(30)&RO(31)&RO(3X00071500
               2)'                                                      00071600
         DC    B'&RO(33)&RO(34)&RO(35)&RO(36)&RO(37)&RO(38)&RO(39)&RO(4X00071700
               0)&RO(41)&RO(42)&RO(43)&RO(44)&RO(45)&RO(46)&RO(47)&RO(4X00071800
               8)'                                                      00071900
         DC    B'&RO(49)&RO(50)&RO(51)&RO(52)&RO(53)&RO(54)&RO(55)&RO(5X00072000
               6)&RO(57)&RO(58)&RO(59)&RO(60)&RO(61)&RO(62)&RO(63)&RO(6X00072100
               4)'                                                      00072200
         DC    B'&RO(65)&RO(66)&RO(67)&RO(68)&RO(69)&RO(70)&RO(71)&RO(7X00072300
               2)&RO(73)&RO(74)&RO(75)&RO(76)&RO(77)&RO(78)&RO(79)&RO(8X00072400
               0)'                                                      00072500
         DC    B'&RO(81)&RO(82)&RO(83)&RO(84)&RO(85)&RO(86)&RO(87)&RO(8X00072600
               8)&RO(89)&RO(90)&RO(91)&RO(92)&RO(93)&RO(94)&RO(95)&RO(9X00072700
               6)'                                                      00072800
         DC    B'&RO(97)&RO(98)&RO(99)&RO(100)&RO(101)&RO(102)&RO(103)&X00072900
               RO(104)&RO(105)&RO(106)&RO(107)&RO(108)&RO(109)&RO(110)&X00073000
               RO(111)&RO(112)'                                         00073100
         DC    B'&RO(113)&RO(114)&RO(115)&RO(116)&RO(117)&RO(118)&RO(11X00073200
               9)&RO(120)&RO(121)&RO(122)&RO(123)&RO(124)&RO(125)&RO(12X00073300
               6)&RO(127)&RO(128)'                                      00073400
.*                                                                      00073500
         DC    B'&MT(1)&MT(2)&MT(3)&MT(4)&MT(5)&MT(6)&MT(7)&MT(8)&MT(9)X00073600
               &MT(10)&MT(11)&MT(12)&MT(13)&MT(14)&MT(15)&MT(16)'      X00073700
                                        MSGTYPE                         00073800
         DC    AL2(0)                   MESSAGE PRIORITY                00073900
         DC    CL8'        '            JOB ID                          00074000
         DC    CL8'        '            JOB NAME                        00074100
         DC    CL8'        '            RETRIEVAL KEY                   00074200
         DC    AL4(0)                   TOKEN FOR DOM                   00074300
         DC    AL4(0)                   CONSOLE ID                      00074400
         DC    CL8'        '            SYSTEM NAME                     00074500
         AIF   (&WPLXLEV EQ 1).XWPL08   BRANCH TO GENERATE XWPL VER 1   00074600
         DC    CL8'        '            CONSOLE NAME                    00074700
         DC    AL4(0)                   REPLY CONSOLE NAME/ID ADDR      00074800
         DC    AL4(0)                   CART ADDRESS                    00074900
         DC    AL4(0)                   WSPARM ADDRESS                  00075000
         AIF   ('&MF' EQ 'L').NOTMLW2                                   00075100
.*       STORE ADDR OF TEXT INTO XWPL (VERSION 2 XWPL ONLY)             00075200
.*       GENERATE LABEL FOR BAL BRANCH                                  00075300
&GNAME.A DS    0H                                                       00075400
         AIF   ('&TEXT' EQ '^').NOTMLW3  SHOULD NOT HAPPEN              00075500
         AIF   ('&TEXT'(1,1) EQ '(').XWPL10   TEXT=(REG) ?              00075600
         LA    15,&TEXT                 R15 -> TEXT                     00075700
         ST    15,4(,1)                 STORE TEXT ADDR INTO XWPL       00075800
         AGO   .NOTMLW3                                                 00075900
.XWPL10  ANOP                                                           00076000
         ST    &TEXT(1),4(,1)           STORE TEXT ADDR INTO XWPL       00076100
         AGO   .NOTMLW3                                                 00076200
.*       XWPL VERSION 1 FIELDS                                          00076300
.XWPL08  ANOP                                                           00076400
         DC    AL4(0)                   RESERVED                        00076500
         DC    AL4(0)                   RESERVED                        00076600
         AGO   .NOTMLW2                                                 00076700
.********************************************************************** 00076800
.*       ISSUE ERROR MESSAGE - 12,INVALID MF OPERAND SPECIFIED-MF       00076900
.********************************************************************** 00077000
         IHBERMAC 35,,&MF                                               00077100
.ERROR1  ANOP                                                           00077200
         MEXIT                                                          00077300
.NOTXT   ANOP                                                           00077400
         MNOTE 12,'NUMBER OF LINES REQUESTED IS 0 OR GREATER THAN 255 -X00077500
                GENERATION TERMINATED'                                  00077600
.END     MEND                                                           00077700
./ ADD NAME=WTOR
         MACRO                                                          00000100
&NAME    WTOR  &MESG,&RYAD,&LENGTH,&ECB,&MF=I,&MSGTYP=,&ROUTCDE=,      X00000200
               &DESC=,&MCSFLAG=,&QID=,&TEXT=^                           00000300
.*                                                                      00000400
.********************************************************************** 00000500
.*                                                                      00000600
.*       WTOR - WRITE TO OPERATOR WITH REPLY                            00000700
.*                                                                      00000800
.*       INVOCATION - SPECIFY WTOR MESG,RYAD,LENGTH,ECB,ROUTCDE=,       00000900
.*                                 DESC=,MSGTYP=,MCSFLAG=,MF=,          00001000
.*                                 TEXT=(TEXTADDR,RYAD,LENGTH,ECB)      00001100
.*              WHERE:                                                  00001200
.*                MESG     THE MESSAGE TEXT FOR A SINGLE LINE MESSAGE   00001300
.*                         TO BE WRITTEN TO A CONSOLE                   00001400
.*                                                                      00001500
.*                RYAD     THE ADDRESS OF THE REPLY TEXT                00001600
.*                                                                      00001700
.*                LENGTH   MAXIMUM LENGTH OF THE REPLY TEXT FIELD       00001800
.*                                                                      00001900
.*                ECB      THE ADDRESS OF THE ECB TO BE POSTED ON       00002000
.*                         COMPLETION OF ENTRY OF THE REPLY TEXT        00002100
.*                                                                      00002200
.*                ROUTCDE= ROUTING CODES TO BE ASSIGNED TO THE          00002300
.*                         MESSAGE                                      00002400
.*                                                                      00002500
.*                DESC=    DESCRIPTOR CODES TO BE ASSIGNED TO           00002600
.*                         THE MESSAGE                                  00002700
.*                                                                      00002800
.*                MSGTYP=  SPECIFIES HOW THE MESSAGE IS TO BE           00002900
.*                         ROUTED. VALID VALUES ARE -                   00003000
.*                         N,Y,SESS,JOBNAMES,STATUS,ACTIVE,SHOW         00003100
.*                                                                      00003200
.*                MCSFLAG= SPECIFIES ATTRIBUTES OF THE                  00003300
.*                         MESSAGE. VALID VALUES ARE -                  00003400
.*                         REG0,RESP,REPLY,BRDCST,HRDCPY,               00003500
.*                         QREG0,NOTIME,NOCPY                           00003600
.*                                                                      00003700
.*                QID=     REMOTE ENTRY SERVICES (RSS) QUEUE            00003800
.*                         ID (OS/VS1 ONLY). PARAMETER ACCEPTED         00003900
.*                         BUT IGNORED BY MVS                           00004000
.*                                                                      00004100
.*                MF=      SPECIFIES THE TYPE OF EXPANSION              00004200
.*                         REQUIRED. VALID VALUES ARE -                 00004300
.*                         I  -  DEFAULT VALUE                          00004400
.*                         L                                            00004500
.*                         (E,[ADDR OF WPL])                            00004600
.*                         (E,[ADDR OF WPL],EXTENDED)                   00004700
.*                         THE THIRD MF PARAMETER MUST BE USED          00004800
.*                         WHEN THE TARGET WPL IS AN XWPL AND           00004900
.*                         THE WTOR REPLY FIELDS ARE PROVIDED TO        00005000
.*                         UPDATE THE TARGET XWPL                       00005100
.*                                                                      00005200
.*                TEXT=    FOR A WTOR THE FIRST PARAMETER               00005300
.*                         SPECIFIES A DATA AREA CONTAINING A 2         00005400
.*                         BYTE MESSAGE LENGTH FIELD FOLLOWED BY        00005500
.*                         THE ACTUAL MESSAGE TEXT. THE                 00005600
.*                         ADDITIONAL POSITIONAL PARAMETERS,            00005700
.*                         RYAD, LENGTH AND ECB ADDRESS MUST BE         00005800
.*                         SPECIFIED AS THE SECOND, THIRD AND           00005900
.*                         FOURTH POSITIONAL PARAMETERS ON THE          00006000
.*                         TEXT KEYWORD. REGISTER NOTATION CAN BE       00006100
.*                         USED FOR ALL THE FIELDS. IF USED, THE        00006200
.*                         REGISTER CONTAINS THE ADDRESS OF THE         00006300
.*                         DATA AREA. IF A FIELD IS USED, THE           00006400
.*                         ADDRESS OF THE FIELD IS STORED IN THE        00006500
.*                         EXTENDED WPL. THE TEXT KEYWORD IS            00006600
.*                         MUTUALLY EXCLUSIVE WITH INLINE MESSAGE       00006700
.*                         TEXT. EITHER TEXT OR INLINE TEXT (BUT        00006800
.*                         NOT BOTH) IS REQUIRED ON THE STANDARD        00006900
.*                         OR LIST FORM OF WTOR.                        00007000
.*                                                                      00007100
.*       FUNCTION - BUILDS ONE OF THREE POSSIBLE PARAMETER LIST         00007200
.*                  FORMATS AND/OR THE CODE WHICH WILL INVOKE           00007300
.*                  SVC 35 TO ISSUE THE MESSAGE.                        00007400
.*                  1. A STANDARD WPL IS BUILT IF INLINE TEXT IS        00007500
.*                     SPECIFIED AND NO ROUTING CODE GREATER THAN 16    00007600
.*                     IS SPECIFIED.                                    00007700
.*                  2. IF INLINE TEXT IS SPECIFIED AND A ROUTING CODE   00007800
.*                     GREATER THAN 16 IS SPECIFIED THEN AN EXTENDED    00007900
.*                     WPL VERSION 1 IS BUILT.                          00008000
.*                  3. IF THE TEXT OPERAND IS SPECIFIED THEN AN         00008100
.*                     EXTENDED WPL VERSION 2 IS GENERATED OR           00008200
.*                     ASSUMED AS THE TARGET OF AN MF=E SPECIFICATION.  00008300
.*                     THE ADDITIONAL POSITIONAL PARAMETERS,            00008400
.*                     RYAD, LENGTH AND ECB ADDRESS, MUST BE            00008500
.*                     SPECIFIED AS THE SECOND, THIRD AND               00008600
.*                     FOURTH POSITIONAL PARAMETERS ON THE              00008700
.*                     TEXT KEYWORD.                                    00008800
.*                     IF MF=(E,TARGET.... IS SPECIFIED, USE OF THE     00008900
.*                     MF=(E,TARGET,EXTENDED) KEYWORD IS OPTIONAL AS    00009000
.*                     SPECIFYING THE TEXT PARAMETER ASSUMES THE        00009100
.*                     USE OF AN EXTENDED WPL VERSION 2.                00009200
.*                                                                      00009300
.*       STATUS -                                                       00009400
.*       LASTUPD         = EBB1102  TYPE=ADD                            00009500
.*       LIBRARIES       = DISTLIB=AMACLIB                              00009600
.*       FMID            = EBB1102                                      00009700
.*       RMID            = EBB1102                                      00009800
.*       SYSMOD HISTORY  = SYSMOD   TYPE       DATE   MCS  STATUS       00009900
.*                         EBB1102  FUNCTION  00.000  MAC  ACC RGN      00010000
.*                                                                      00010100
.*       CHANGE ACTIVITY -                                              00010200
.*              ZP600040 - EXTENSIVE REVISION OF THE MACRO AND          00010300
.*                         SUPPORT FOR THE TEXT OPERAND WHICH           00010400
.*                         REQUIRES GENERATION OF AN EXTENDED WPL       00010500
.*                                                                      00010600
.********************************************************************** 00010700
         LCLA  &H,&I,&II,&N,&J,&K,&LEN,&LLCNT                           00010800
         LCLA  &NUMENT                 NUMBER OF SUBLIST ENTRIES        00010900
         LCLA  &LENENT                 L'ENTRY                          00011000
         LCLB  &SECONDL,&PAIR                                           00011100
         LCLC  &GNAME,&WORKT,&WORKA,&WORKL,&WORKE                       00011200
         LCLB  &RO(128),&DE(16),&MC(32),&MT(16)  FLAGS FOR THE FIELDS   00011300
         LCLC  &LOWNUM,&HIGHNUM        ROUTE RANGE NUMBERS              00011400
         LCLB  &GENRD                  GENERATE ROUT AND DESC FIELDS    00011500
         LCLB  &GENMT                  GENERATE MSGTYP FIELD            00011600
         LCLB  &GENXWPL                GENERATE AN XWPL                 00011700
         LCLA  &WPLXLEV                LEVEL OF XWPL TO GENERATE        00011800
         LCLB  &MTY,&MTN               FLAGS FOR MSGTYP PROCESSING      00011900
         ACTR  30000                                                    00012000
.*                                                                      00012100
.*       INITIALIZATION                                                 00012200
.*                                                                      00012300
&GNAME   SETC  'IHB'.'&SYSNDX'         FOR BAL INSTRUCTION              00012400
&WPLXLEV SETA  1                                                        00012500
.********************************************************************** 00012600
.*                                                                      00012700
.*       BEGIN PROCESSING PARAMETERS                                    00012800
.*                                                                      00012900
.********************************************************************** 00013000
.********************************************************************** 00013100
.*                                                                      00013200
.*        PROCESS ROUTING CODES                                         00013300
.*        DETERMINE IF ROUTING CODES HAVE BEEN SPECIFIED                00013400
.*                                                                      00013500
.*        ROUTING CODES 1-128 MAY BE SET HOWEVER FOR MVS 3.8 ONLY       00013600
.*        ONLY ROUTING CODES 1-16 WILL BE ACTIONED                      00013700
.*        ANY ROUTING CODE GREATER THAN 16 WILL BE IGNORED              00013800
.*        WITHOUT ERROR HOWEVER SPECIFYING A ROUTING CODE GREATER       00013900
.*        THAN 16 WILL CAUSE THE GENERATION OF AN EXTENDED FORMAT       00014000
.*        VERSION 1 WPL (XWPL).                                         00014100
.*                                                                      00014200
.********************************************************************** 00014300
         AIF   (T'&ROUTCDE EQ 'O').ENDROUT   ROUTING CODES SPECIFIED ?  00014400
&GENRD   SETB  1                    ROUTING CODES PRESENT               00014500
&I       SETA  1                    INITIALIZE LIST INDEX               00014600
&NUMENT  SETA  N'&ROUTCDE           TOTAL NUMBER OF ENTRIES IN &ROUTCDE 00014700
.*       ROUTING CODE LOOP                                              00014800
.ROULOOP ANOP                                                           00014900
&LENENT  SETA  K'&ROUTCDE(&I)       SET L'CURRENT ELEMENT               00015000
         AIF   (&LENENT EQ 0).BADENT  NULL ENTRY, ERROR                 00015100
         AIF   (T'&ROUTCDE(&I) EQ 'N').ROUVAL   SINGLE NUMERIC VALUE    00015200
.*                                                                      00015300
.*       RANGE OF ROUTING CODES HAS BEEN SPECIFIED                      00015400
.*                                                                      00015500
.*       SCAN FOR DASH SEPERATOR                                        00015600
.*                                                                      00015700
&II      SETA  1                    INITIALIZE ELEMENT INDEX            00015800
.DASHL   ANOP                       DASH SCANNING LOOP                  00015900
         AIF   ('&ROUTCDE(&I)'(&II,1) EQ '-').DASHFND                   00016000
&II      SETA  &II+1                INCREMENT ELEMENT INDEX             00016100
         AIF   (&II GE &LENENT).BADRANG   CHECK INDEX POSITION          00016200
         AGO   .DASHL               LOOP AROUND FOR NEXT CHAR           00016300
.*       FOUND A DASH SEPERATOR                                         00016400
.DASHFND ANOP                                                           00016500
         AIF   (&II EQ 1).BADRANG         DASH UP FRONT ? ERROR         00016600
         AIF   (&II EQ &LENENT).BADRANG   DASH AT THE END ? ERROR       00016700
&LOWNUM  SETC  '&ROUTCDE(&I)'(1,&II-1) SET FIRST NUMBER OF RANGE        00016800
&HIGHNUM SETC  '&ROUTCDE(&I)'(&II+1,&LENENT-&II) SET SECOND NUMBER      00016900
.*       FIRST NUMBER > LAST NUMBER ? ERROR                             00017000
         AIF   ('&LOWNUM' GT '&HIGHNUM').BADRANG                        00017100
.*       OUTSIDE THE RANGE OF VALID ROUTING CODES ? ERROR               00017200
         AIF   ('&LOWNUM' LT '1' OR '&LOWNUM' GT '128' OR              X00017300
               '&HIGHNUM' LT '1' OR '&HIGHNUM' GT '128').BADRANG        00017400
&J       SETA  &LOWNUM              INITIALIZE START OF RANGE           00017500
&K       SETA  &HIGHNUM             INITIALIZE END OF RANGE             00017600
.*       LOOP TO SET ON ROUTING CODES                                   00017700
.*       IF ALREADY SET ON THEN ERROR                                   00017800
.RANLOOP ANOP                                                           00017900
         AIF   (&RO(&J)).BADRANG                                        00018000
&RO(&J)  SETB  1                    SET ROUTING CODE &J ON              00018100
         AIF   (&J LT 17).RANLOPA                                       00018200
&GENXWPL SETB  1                    ANY VALUE > 16 FORCES A XWPL        00018300
.RANLOPA ANOP                                                           00018400
&J       SETA  &J+1                 INCREMENT LOOP INDEX                00018500
         AIF   (&J LE &K).RANLOOP   LOOP THROUGH RANGE                  00018600
.*       RANGE PROCESSED                                                00018700
         AGO   .NEXTROU             DETERMINE NEXT ROUTING CODE         00018800
.*                                                                      00018900
.*       ROUTING CODE VALUE HAS BEEN SPECIFIED. VERIFY THAT IT IS       00019000
.*       WITHIN THE PROPER RANGE AND, IF SO, SET THE CORRECT BIT        00019100
.*                                                                      00019200
.ROUVAL  ANOP                                                           00019300
&J       SETA  &ROUTCDE(&I)         GET NEXT ROUTING CODE VALUE         00019400
         AIF   (&J GE 1 AND &J LE 128).ROUOK   VALID VALUE ?            00019500
         MNOTE 4,'ROUTCDE &J IS AN INVALID ROUTING CODE'                00019600
         AGO   .NEXTROU                                                 00019700
.ROUOK   ANOP                       VALID ROUTING CODE SPECIFIED        00019800
         AIF   (&RO(&J)).BADRANG                                        00019900
&RO(&J)  SETB  1                    SET ROUTING CODE &J ON              00020000
         AIF   (&J LT 17).NEXTROU                                       00020100
.*       ROUTCDE GT 16 DOES NOT FORCE XWPL IF MF=E                      00020200
         AIF   ('&MF(1)' EQ 'E').NEXTROU                                00020300
&GENXWPL SETB  1                    ANY VALUE > 16 FORCES A XWPL        00020400
&MC(12)  SETB  1                    SET MCS FLAG FOR XWPL (WPLMCSFL)    00020500
&MC(21)  SETB  1                    WTOR WITH EXT PARM    (WPXWTOR)     00020600
         AGO   .NEXTROU             LOOP TO NEXT VALUE                  00020700
.*       A NULL ROUTING CODE HAS BEEN SPECIFIED                         00020800
.*       ISSUE AN MNOTE AND SKIP TO THE NEXT ROUTING CODE               00020900
.BADENT  ANOP                                                           00021000
         MNOTE 4,'NULL ROUTING CODE SPECIFIED'                          00021100
         AGO   .NEXTROU                                                 00021200
.*       AN INVALID ROUTING CODE RANGE HAS BEEN SPECIFIED               00021300
.*       ISSUE AN MNOTE AND SKIP TO THE NEXT ROUTING CODE               00021400
.BADRANG ANOP                                                           00021500
         MNOTE 4,'INVALID RANGE OF ROUTING CODES IGNORED'               00021600
.*       FALL THROUGH TO SETUP FOR NEXT VALUE                           00021700
.*                                                                      00021800
.*       SETUP TO PROCESS NEXT ROUTE CODE ENTRY                         00021900
.NEXTROU ANOP                                                           00022000
&I       SETA  &I+1                 INCREMENT INDEX TO ROUT CODE VALUES 00022100
         AIF   (&I LE &NUMENT).ROULOOP IF MORE CODES, CHECK NEXT ONE    00022200
.ENDROUT ANOP                       DONE WITH ROUTING CODES             00022300
.********************************************************************** 00022400
.*                                                                      00022500
.*       PROCESS DESCRIPTOR CODES                                       00022600
.*       DETERMINE IF DESCRIPTOR CODES HAVE BEEN SPECIFIED              00022700
.*                                                                      00022800
.********************************************************************** 00022900
         AIF   (T'&DESC EQ 'O').ENDDESC   NO DESCRIPTOR, BRANCH         00023000
&NUMENT  SETA  N'&DESC           NUMBER OF ENTRIES                      00023100
&I       SETA  1                                                        00023200
&J       SETA  0                 COUNTER TO DETECT MUTUALLY EXCL VALUE  00023300
.DESCLOP ANOP                                                           00023400
&II      SETA  &DESC(&I)                                                00023500
         AIF   (&II GE 1 AND &II LE 16).DESCOK                          00023600
.DESCERR MNOTE 8,'DESC &N IS INVALID DESCRIPTOR - IGNORED'              00023700
         AGO   .NEXTDES                                                 00023800
.DESCOK  ANOP                                                           00023900
         AIF   (&DE(&II)).DESERR             ALREADY SPECIFIED          00024000
         AIF   (&II GT 6).DESCJMP                                       00024100
&J       SETA  &J+1                         A VALUE LOWER THAN 7 SPEC   00024200
.DESCJMP ANOP                                                           00024300
&DE(&II) SETB  1                                                        00024400
.NEXTDES ANOP                                                           00024500
&I       SETA  &I+1                         INCR ENTRY COUNTER          00024600
         AIF   (&I LE &NUMENT).DESCLOP      PROCESS NEXT ENTRY          00024700
.*       ALL ENTRIES PROCESSED                                          00024800
.*       TEST FOR ANY MUTUALLY EXCLUSIVE ENTRIES                        00024900
         AIF   (&J EQ 1).ENDDESC            ALL OK                      00025000
         MNOTE 8,'DESC MUTUALLY EXCLUSIVE VALUE SPECIFIED'              00025100
         MEXIT                                                          00025200
.DESERR  MNOTE 8,'DUPLICATE DESCRIPTOR VALUE SPECIFIED'                 00025300
         MEXIT                                                          00025400
.ENDDESC ANOP                                                           00025500
.********************************************************************** 00025600
.*                                                                      00025700
.*       PROCESS MSGTYP PARAMETERS                                      00025800
.*       DETERMINE IF MSGTYP PARAMETERS HAVE BEEN SPECIFIED             00025900
.*       PARAMETERS -                                                   00026000
.*       Y        - GENERATES MSGTYP FIELD BUT IGNORES OTHER            00026100
.*                  PARAMETERS (ALL MSGTYP FLAGS ZERO)                  00026200
.*                  THIS MUST BE THE FIRST PARAMETER                    00026300
.*       N        - SUPPESSSES THE GENERATION OF THE MSGTYP             00026400
.*                  FIELD EVEN THOUGH OTHER PARAMETERS HAVE BEEN        00026500
.*                  SPECIFIED                                           00026600
.*                  THIS MUST BE THE FIRST PARAMETER                    00026700
.*       JOBNAMES -                                                     00026800
.*       STATUS   -                                                     00026900
.*       ACTIVE   -                                                     00027000
.*       SHOW     - FOR VS/1 CRJE SUPPORT                               00027100
.*       SESS     -                                                     00027200
.*       NOTE THAT THE MSGTYP FIELD WILL BE GENERATED AND THE           00027300
.*       WPLMSGTD FLAG WILL BE SET IF THE QID PARAMETER IS              00027400
.*       CODED ON THE WTO MACRO                                         00027500
.*                                                                      00027600
.********************************************************************** 00027700
         AIF   (T'&MSGTYP EQ 'O').ENDMSGT   MSGTYP PARAMS SPECIFIED ?   00027800
&GENRD   SETB  1                            MSGTYPE CAUSED ROUT/DESC    00027900
&NUMENT  SETA  N'&MSGTYP                    NUMBER OF ENTRIES           00028000
         AIF   ('&MSGTYP(1)' NE 'N').MSGL1                              00028100
&MTN     SETB  1                            MSGTYP=N CODED              00028200
         AGO   .MSGL2                                                   00028300
.MSGL1   AIF   ('&MSGTYP(1)' NE 'Y').MSGL3  MSGTYP=Y CODED              00028400
&MTY     SETB  1                                                        00028500
.MSGL2   ANOP                               EITHER MSGTYP =Y/N CODED    00028600
&I       SETA  1                            FIRST PARAMETER PROCESSED   00028700
         AGO   .NEXTMSG                                                 00028800
.MSGL3   ANOP                                                           00028900
&I       SETA  0                                                        00029000
         AGO   .NEXTMSG                                                 00029100
.MSGTLOP ANOP                               LOOP THROUGH PARAMETERS     00029200
         AIF   ('&MSGTYP(&I)' NE 'JOBNAMES').MSGL4                      00029300
         AIF   (&MT(1)).MTERR               JOBNAMES ALREADY CODED ?    00029400
&MT(1)   SETB  1                            TURN ON JOBNAMES            00029500
         AGO   .NEXTMSG                                                 00029600
.MSGL4   ANOP                                                           00029700
         AIF   ('&MSGTYP(&I)' NE 'STATUS').MSGL5                        00029800
         AIF   (&MT(2)).MTERR               STATUS ALREADY CODED ?      00029900
&MT(2)   SETB  1                            TURN ON STATUS              00030000
         AGO   .NEXTMSG                                                 00030100
.MSGL5   ANOP                                                           00030200
         AIF   ('&MSGTYP(&I)' NE 'ACTIVE').MSGL6                        00030300
         AIF   (&MT(3)).MTERR               ACTIVE ALREADY CODED ?      00030400
&MT(3)   SETB  1                            TURN ON ACTIVE              00030500
         AGO   .NEXTMSG                                                 00030600
.MSGL6   ANOP                                                           00030700
         AIF   ('&MSGTYP(&I)' NE 'SHOW').MSGL7                          00030800
         AIF   (&MT(5)).MTERR               SHOW ALREADY CODED ?        00030900
&MT(5)   SETB  1                            TURN ON SHOW                00031000
         AGO   .NEXTMSG                                                 00031100
.MSGL7   ANOP                                                           00031200
         AIF   ('&MSGTYP(&I)' NE 'SESS').MTERR                          00031300
         AIF   (&MT(6)).MTERR               SESS ALREADY CODED ?        00031400
&MT(6)   SETB  1                            TURN ON SESS                00031500
.NEXTMSG ANOP                                                           00031600
&GENMT   SETB  1                            GENERATE MSGTYP FIELD       00031700
&I       SETA  &I+1                         INCR ENTRY COUNTER          00031800
         AIF   (&I LE &NUMENT).MSGTLOP      PROCESS NEXT ENTRY          00031900
.*       ALL ENTRIES PROCESSED                                          00032000
.*       PROCESS FLAGS                                                  00032100
&GENMT   SETB  (NOT &MTN)                   SUPRESS MT GEN IF MSGTYP=N  00032200
         AIF   (&MTN OR &MTY).MSGL8                                     00032300
         AGO   .ENDMSGT                                                 00032400
.*       ERROR PROCESSING                                               00032500
.MTERR   MNOTE 8,'MESSAGE TYPE FIELD INVALID - N IS ASSUMED'            00032600
&MTN     SETB  1                                                        00032700
&MTY     SETB  0                                                        00032800
.MSGL8   ANOP                               TURN OFF ALL FLAGS          00032900
&MT(1)   SETB  0                                                        00033000
&MT(2)   SETB  0                                                        00033100
&MT(3)   SETB  0                                                        00033200
&MT(5)   SETB  0                                                        00033300
&MT(6)   SETB  0                                                        00033400
.ENDMSGT ANOP                                                           00033500
.********************************************************************** 00033600
.*                                                                      00033700
.*       PROCESS MCSFLAG PARAMETERS                                     00033800
.*       DETERMINE IF MCSFLAG PARAMETERS HAVE BEEN SPECIFIED            00033900
.*       PARAMETERS -                                                   00034000
.*       REG0     - MSG TO CONSOLE WITH SOURCE ID IN R0                 00034100
.*       RESP     - WTO IS IMMEDIATE COMMAND RESPONSE                   00034200
.*       REPLY    - WTO IS A REPLY TO A WTOR                            00034300
.*       BRDCAST  - MSG TO BE BROADCAST TO ALL ACTIVE CONSOLES          00034400
.*       HRDCOPY  - MSG TO BE QUEUED FOR HARDCOPY ONLY                  00034500
.*       QREG0    - MSG TO BE QUEUED UNCONDITIONALLY TO CON ID IN R0    00034600
.*       NOTIME   - TIME IS NOT TO BE APPENDED TO MSG  TER              00034700
.*       NOCPY    - IF WTO OR WTOR IS ISSUED IN SUPERVISOR STATE        00034800
.*                  THEN DO NOT QUEUE FOR HARDCOPY                      00034900
.*                                                                      00035000
.********************************************************************** 00035100
         AIF   (T'&MCSFLAG EQ 'O').ENDMCS    MCSFLAG PARAMS SPECIFIED ? 00035200
&I       SETA  1                    SET LIST INDEX                      00035300
&NUMENT  SETA  N'&MCSFLAG           NUMBER OF ENTRIES                   00035400
.MCLOOP  ANOP                       START LOOP THROUGH PARAMETERS       00035500
         AIF   ('&MCSFLAG(&I)' NE 'REG0').MCL1                          00035600
         AIF   (&MC(2)).MCERR       REG0 ALREADY CODED ?                00035700
&MC(2)   SETB  1                    TURN ON REG0 (WPLMCSFB)             00035800
         AGO   .NEXTMC                                                  00035900
.MCL1    ANOP                                                           00036000
         AIF   ('&MCSFLAG(&I)' NE 'RESP').MCL2                          00036100
         AIF   (&MC(3)).MCERR       RESP ALREADY CODED ?                00036200
&MC(3)   SETB  1                    TURN ON RESP (WPLMCSFC)             00036300
         AGO   .NEXTMC                                                  00036400
.MCL2    ANOP                                                           00036500
         AIF   ('&MCSFLAG(&I)' NE 'REPLY').MCL3                         00036600
         AIF   (&MC(5)).MCERR       REPLYG0 ALREADY CODED ?             00036700
&MC(5)   SETB  1                    TURN ON REPLY (WPLMCSFE)            00036800
         AGO   .NEXTMC                                                  00036900
.MCL3    ANOP                                                           00037000
         AIF   ('&MCSFLAG(&I)' NE 'BRDCAST').MCL4                       00037100
         AIF   (&MC(6)).MCERR       BRDCAST ALREADY CODED ?             00037200
&MC(6)   SETB  1                    TURN ON BRDCAST (WPLMCSFF)          00037300
         AGO   .NEXTMC                                                  00037400
.MCL4    ANOP                                                           00037500
         AIF   ('&MCSFLAG(&I)' NE 'HRDCPY').MCL5                        00037600
         AIF   (&MC(7)).MCERR       HRDCPY ALREADY CODED ?              00037700
&MC(7)   SETB  1                    TURN ON HRDCPY (WPLMCSFG)           00037800
         AGO   .NEXTMC                                                  00037900
.MCL5    ANOP                                                           00038000
         AIF   ('&MCSFLAG(&I)' NE 'QREG0').MCL6                         00038100
         AIF   (&MC(8)).MCERR       QREG0 ALREADY CODED ?               00038200
&MC(8)   SETB  1                    TURN ON QREG0 (WPLMCSFH)            00038300
         AGO   .NEXTMC                                                  00038400
.MCL6    ANOP                                                           00038500
         AIF   ('&MCSFLAG(&I)' NE 'NOTIME').MCL7                        00038600
         AIF   (&MC(9)).MCERR       NOTIME ALREADY CODED ?              00038700
&MC(9)   SETB  1                    TURN ON NOTIME (WPLMCSFI)           00038800
         AGO   .NEXTMC                                                  00038900
.MCL7    ANOP                                                           00039000
         AIF   ('&MCSFLAG(&I)' NE 'NOCPY').MCERR                        00039100
         AIF   (&MC(14)).MCERR      NOTIME ALREADY CODED ?              00039200
&MC(14)  SETB  1                    TURN ON NOTTIME (WPLMCSFN)          00039300
         AGO   .NEXTMC                                                  00039400
.MCERR   ANOP                                                           00039500
         MNOTE 8,'&MCSFLAG(&I) IS INVALID - IGNORED'                    00039600
.NEXTMC  ANOP                       INCR ENTRY INDEX                    00039700
&I       SETA  &I+1                                                     00039800
         AIF   (&I LE &NUMENT).MCLOOP                                   00039900
.*       TEST FOR ERRORS                                                00040000
         AIF   (&MC(7) AND &MC(14)).MCEXCL    HRDCOPY AND NOCPY ?       00040100
         AGO   .ENDMCS                                                  00040200
.MCEXCL  MNOTE 8,'HRDCPY AND NOCPY MUTUALLY EXCLUSIVE, HRDCPY ASSUMED'  00040300
&MC(14)  SETB  0                      TURN OFF NOCPY                    00040400
.ENDMCS  ANOP                                                           00040500
.********************************************************************** 00040600
.*                                                                      00040700
.*       ALL PARAMETERS EXCEPT MESSAGE TEXT AND MF HAVE BEEN PROCESSED  00040800
.*       PROCESS ANY PARAMETER INTERACTIONS                             00040900
.*                                                                      00041000
.********************************************************************** 00041100
.*       DETERMINE IF A DEFAULT ROUTING CODE OF 2 HAS TO BE SET         00041200
         AIF   (&GENRD).INT1        ROUTCDE SPECIFIED, BRANCH           00041300
         AIF   (NOT &GENMT).INT1    NO MSGTYP, BRANCH                   00041400
         AIF   (&MC(2) OR &MC(8)).INT1   MCS REG0 OR QREG0 CODED ?      00041500
.*       SET A DEFAULT ROUTING CODE OF 2                                00041600
&GENRD   SETB  1                                                        00041700
&RO(2)   SETB  1                                                        00041800
.INT1    ANOP                                                           00041900
.*       QID FORCES GENERATION OF DESC AND ROUTE FIELDS                 00042000
&GENRD   SETB  (&GENRD OR (T'&QID NE 'O'))                              00042100
.*       DETERMINE IF THE MCS FLAG FOR ROUT/DESC PRESENT SHOULD BE ON   00042200
&MC(1)   SETB  (&GENRD)                                                 00042300
.*       DETERMINE IF MCS FLAG FOR MSGTYP PRESENT SHOULD BE ON          00042400
&MC(4)   SETB  (&GENMT)                                                 00042500
.********************************************************************** 00042600
.*                                                                      00042700
.*       DETERMINE SOURCE OF TEXT FOR MESSAGE                           00042800
.*                                                                      00042900
.********************************************************************** 00043000
.*       TEST &MESG FOR MULTI-LINE                                      00043100
.*       MULTI-LINE WTOR NOT SUPPORTED                                  00043200
         AIF   ((N'&SYSLIST(1) EQ 0) OR (N'&SYSLIST(1) EQ 1)).INT1A     00043300
         AGO   .ERROR7                 GENERATE MSG MLWTO/WTOR ERROR    00043400
.INT1A   ANOP                                                           00043500
.*       MESSAGE TEXT AND INLINE TEXT ARE MUTUALLY EXCLUSIVE            00043600
.*       ISSUE ERROR MESSAGE IF BOTH PROVIDED                           00043700
         AIF   (('&TEXT' NE '^') AND ('&MESG' NE '')).ERROR9            00043800
.*       EITHER MESG OR TEXT MUST BE SPECIFIED UNLESS MF=E              00043900
.INT3    AIF   ('&MF(1)' EQ 'E').INT5                                   00044000
         AIF   (('&MESG' EQ '') AND ('&TEXT' EQ '^')).INT4              00044100
         AGO   .INT5                                                    00044200
.INT4    MNOTE 12,'NO INLINE MESSAGE TEXT OR TEXT PARAMETER SPECIFIED'  00044300
         MEXIT                                                          00044400
.INT5    ANOP                                                           00044500
.*       USE OF TEXT= PARAMETER FORCES GENERATION OF TYPE 2 XWPL        00044600
         AIF   ('&TEXT' EQ '^').INT6                                    00044700
&GENXWPL SETB  1                       GENERATE AN XWPL                 00044800
&WPLXLEV SETA  2                       LEVEL OF XWPL TO GENERATE        00044900
&MC(12)  SETB  1                       SET MCS FLAG FOR XWPL (WPLMCSFL) 00045000
&MC(21)  SETB  1                       WTOR WITH EXT PARM  (WPXWTOR)    00045100
&MC(25)  SETB  1                       TEXT ADDR SPECIFIED (WPXTXTAD)   00045200
.********************************************************************** 00045300
.*                                                                      00045400
.*       PROCESS MF PARAMETER                                           00045500
.*                                                                      00045600
.********************************************************************** 00045700
.INT6    AIF    ('&MF' EQ 'I' OR '&MF' EQ 'L').WPLGEN                   00045800
.*                                                                      00045900
.*       MF=(E,X) PROCESSING                                            00046000
.*                                                                      00046100
         AIF   ('&MF(1)' NE 'E').ERROR1    CONFIRM THAT IT IS MF=E      00046200
         AIF   (N'&MF EQ 2).MFE5                                        00046300
         AIF   ((N'&MF EQ 3) AND ('&MF(3)' NE 'EXTENDED')).ERROR1       00046400
&GENXWPL SETB  ((N'&MF EQ 3) AND ('&MF(3)' EQ 'EXTENDED'))              00046500
&MC(12)  SETB  1                    SET MCS FLAG FOR XWPL (WPLMCSFL)    00046600
&MC(21)  SETB  1                       WTOR WITH EXT PARM  (WPXWTOR)    00046700
.MFE5    ANOP                                                           00046800
&NAME    IHBINNRA &MF(2)            GENERATE INITIAL LR OR LA IF NEEDED 00046900
         AIF   (&GENXWPL).MFE6                                          00047000
.*       PROCESS STANDARD WPL FOR MF=E WTOR                             00047100
         AIF   ('&RYAD' EQ '').MFE6A                                    00047200
         AIF   ('&LENGTH' NE '').MFE6B                                  00047300
         IC    14,0(1,0)                SAVE REPLY LENGTH               00047400
.MFE6B   AIF   ('&RYAD'(1,1) EQ '(').MFE6C                              00047500
         LA    15,&RYAD                 LOAD REPLY ADDR                 00047600
         ST    15,0(1,0)                STORE RPLY ADDR                 00047700
         AGO   .MFE6H                                                   00047800
         ANOP                                                           00047900
.MFE6C   ST    &RYAD(1),0(1,0)          STORE REPLY ADDR                00048000
.MFE6H   AIF   ('&LENGTH' NE '').MFE6E                                  00048100
         STC   14,0(1,0)                RESTORE REPLY LENGTH            00048200
         AGO   .MFE6G                                                   00048300
.MFE6E   AIF   ('&LENGTH'(1,1) EQ '(').MFE6D                            00048400
         MVI   0(1),&LENGTH             MOVE IN REPLY LENGTH            00048500
         AGO   .MFE6G                                                   00048600
.MFE6D   STC   &LENGTH(1),0(1,0)        STORE REPLY LENGTH              00048700
         AGO   .MFE6G                                                   00048800
.MFE6A   AIF   ('&LENGTH' NE '').MFE6E                                  00048900
.MFE6G   AIF   ('&ECB' EQ '').MFE6Z                                     00049000
         AIF   ('&ECB'(1,1) EQ '(').MFE6F                               00049100
         LA    14,&ECB                  LOAD ADDRESS OF ECB             00049200
         ST    14,4(1,0)                STORE ECB ADDRESS               00049300
         AGO   .MFE6Z                                                   00049400
.MFE6F   ST    &ECB(1),4(1,0)           STORE ECB ADDRESS               00049500
.MFE6Z   AGO   .MFESVC                                                  00049600
.*       PROCESS XWPL FOR MF=E WTOR                                     00049700
.*       EITHER USE OF A PARAMETER OR PROVIDING MF=(E,..,EXTENDED) HAS  00049800
.*       FORCED USE OF AN XWPL                                          00049900
.*       UPDATE MCS FLAGS                                               00050000
         OI    2(1),B'&MC(1)&MC(2)&MC(3)&MC(4)&MC(5)&MC(6)&MC(7)&MC(8)'X00050100
                                        MCS FLAGS                       00050200
         OI    3(1),B'&MC(9)&MC(10)&MC(11)&MC(12)&MC(13)&MC(14)&MC(15)&X00050300
               MC(16)'                  MCS FLAGS                       00050400
.MFE6    ANOP                                                           00050500
         AIF   ('&TEXT' EQ '^').MFE1A                                   00050600
         AIF   ('&TEXT' EQ '').ERROR2   TEXT= CANNOT BE NULL            00050700
&WORKT   SETC  '&TEXT(1)'                                               00050800
         AIF   ('&WORKT'(1,1) EQ '(').MFE2   TEXT=REG ?                 00050900
         LA    15,&WORKT                R15 -> TEXT                     00051000
         ST    15,4(,1)                 STORE TEXT ADDR INTO XWPL       00051100
         AGO   .MFE1                                                    00051200
.MFE2    ANOP                                                           00051300
&I       SETA  K'&WORKT                                                 00051400
&WORKT   SETC  '&WORKT'(2,&I-2)         ASSIGN REGISTER NOTATION        00051500
         ST    &WORKT,4(,1)             STORE TEXT ADDR INTO XWPL       00051600
.*       TEXT= PARAMETER HAS BEEN PROVIDED. THE SECOND, THIRD AND       00051700
.*       FORTH POSITIONAL PARAMETERS MUST NOT BE PROVIDED               00051800
.MFE1    AIF   (('&RYAD' NE '') OR ('&LENGTH' NE '') OR                X00051900
               ('&ECB' NE '')).ERROR6                                   00052000
&WORKA   SETC  '&TEXT(2)'               XWPL VERSION 2                  00052100
&WORKL   SETC  '&TEXT(3)'               XWPL VERSION 2                  00052200
&WORKE   SETC  '&TEXT(4)'               XWPL VERSION 2                  00052300
         AGO   .MFE1B                                                   00052400
.*       PROCESS XWPL VERSION 1 WTOR PARAMETERS                         00052500
.MFE1A   ANOP                                                           00052600
&WORKA   SETC  '&RYAD'                  XWPL VERSION 1                  00052700
&WORKL   SETC  '&LENGTH'                XWPL VERSION 1                  00052800
&WORKE   SETC  '&ECB'                   XWPL VERSION 1                  00052900
.MFE1B   ANOP                                                           00053000
.*       ANY NEED TO UPDATE ADDITIONAL WPLX FIELDS ?                    00053100
         AIF   ((NOT &GENRD) AND (NOT &GENMT) AND (NOT &MC(25))        X00053200
               AND ('&WORKA' EQ '') AND ('&WORKL' EQ '') AND           X00053300
               ('&WORKE' EQ '')).MFESVC  EXIT TO SVC                    00053400
         LA    14,0(,1)                                                 00053500
         AH    14,0(,14)            ADD L'TEXT + LEN FIELD + MCS FIELD  00053600
         AIF   ('&WORKL' EQ '').MFE1C   L'REPLY PROVIDED ?              00053700
         AIF   ('&WORKL'(1,1) EQ '(').MFE1D   REGISTER NOTATION ?       00053800
         MVI   2(14),&WORKL             STORE REPLY LENGTH              00053900
         AGO   .MFE1C                                                   00054000
.MFE1D   ANOP                                                           00054100
&I       SETA  K'&WORKL                                                 00054200
&WORKL   SETC  '&WORKL'(2,&I-2)         ASSIGN REGISTER NOTATION        00054300
         STC   &WORKL,2(,14)            STORE REPLY LENGTH              00054400
.MFE1C   ANOP                                                           00054500
         AIF   ('&TEXT' EQ '^').MFE4    NEED TO SET EXTENDED MCS ?      00054600
         OI    4(14),B'&MC(17)&MC(18)&MC(19)&MC(20)&MC(21)&MC(22)&MC(23X00054700
               )&MC(24)'                EXTENDED MCS                    00054800
         OI    5(14),B'&MC(25)&MC(26)&MC(27)&MC(28)&MC(29)&MC(30)&MC(31X00054900
               )&MC(32)'                EXTENDED MCS                    00055000
.MFE4    AIF   ('&WORKA' EQ '').MFE4A   REPLY ADDRESS PROVIDED ?        00055100
         AIF   ('&WORKA'(1,1) EQ '(').MFE4D   REGISTER NOTATION ?       00055200
         LA    15,&WORKA                15 -> REPLY ADDRESS             00055300
         ST    15,8(,14)                STORE REPLY ADDR                00055400
         AGO   .MFE4A                                                   00055500
.MFE4D   ANOP                                                           00055600
&I       SETA  K'&WORKA                                                 00055700
&WORKA   SETC  '&WORKA'(2,&I-2)                                         00055800
         ST    &WORKA,8(,14)            STORE REPLY ADDR                00055900
.*                                                                      00056000
.MFE4A   AIF   ('&WORKE' EQ '').MFE4G   ECB ADDRESS PROVIDED ?          00056100
         AIF   ('&WORKE'(1,1) EQ '(').MFE4F   REGISTER NOTATION ?       00056200
         LA    15,&WORKE                15 -> ECB ADDRESS               00056300
         ST    15,12(,14)               STORE ECB ADDR                  00056400
         AGO   .MFE4G                                                   00056500
.MFE4F   ANOP                                                           00056600
&I       SETA  K'&WORKE                                                 00056700
&WORKE   SETC  '&WORKE'(2,&I-2)                                         00056800
         ST    &WORKE,12(,14)           STORE ECB ADDR                  00056900
.*                                                                      00057000
.MFE4G   AIF   (NOT &GENMT).MFESVC      NEED TO GEN MSGTYP FLAGS ?      00057100
         MVI   40(14),B'&MT(1)&MT(2)&MT(3)&MT(4)&MT(5)&MT(6)&MT(7)&MT(8X00057200
               )'                       MSGTYP                          00057300
         MVI   41(14),B'&MT(9)&MT(10)&MT(11)&MT(12)&MT(13)&MT(14)&MT(15X00057400
               )&MT(16)'                MSGTYP                          00057500
.MFE3    AIF   (NOT &GENRD).MFESVC      NEED TO GEN ROUTE/DESC ?        00057600
         MVI   20(14),B'&DE(1)&DE(2)&DE(3)&DE(4)&DE(5)&DE(6)&DE(7)&DE(8X00057700
               )'                       DESCRIPTOR CODES                00057800
         MVI   21(14),B'&DE(9)&DE(10)&DE(11)&DE(12)&DE(13)&DE(14)&DE(15X00057900
               )&DE(16)'                DESCRIPTOR CODES                00058000
         MVI   24(14),B'&RO(1)&RO(2)&RO(3)&RO(4)&RO(5)&RO(6)&RO(7)&RO(8X00058100
               )'                       ROUTING CODES                   00058200
         MVI   25(14),B'&RO(9)&RO(10)&RO(11)&RO(12)&RO(13)&RO(14)&RO(15X00058300
               )&RO(16)'                ROUTING CODES                   00058400
.MFESVC  ANOP                                                           00058500
         SVC   35                                                       00058600
         MEXIT                      EXIT MACRO, PROCESSING COMPLETE     00058700
.********************************************************************** 00058800
.*                                                                      00058900
.*       MF=I AND MF=L PROCESSING                                       00059000
.*                                                                      00059100
.*       GENERATE A STANDARD WPL OR AN XWPL VERSION 1 OR VERSION 2      00059200
.*       DEPENDING ON PARAMETERS PROVIDED                               00059300
.*                                                                      00059400
.********************************************************************** 00059500
.WPLGEN  ANOP                                                           00059600
         AIF   ('&MF' EQ 'L').DCNAME                                    00059700
.*       MF=I PROCESSING                                                00059800
         CNOP  0,4                                                      00059900
&NAME    BAL   1,&GNAME.A                                               00060000
         AGO   .MFIL01                                                  00060100
.*       MF=L PROCESSING                                                00060200
.DCNAME  ANOP                                                           00060300
&NAME    DC    0F'0'                                                    00060400
.MFIL01  ANOP                                                           00060500
         AIF   (&GENXWPL).XWPL01         GO GENERATE XWPL               00060600
.********************************************************************** 00060700
.*                                                                      00060800
.*       GENERATE STANDARD WPL FOR MF=I OR MF=L                         00060900
.*                                                                      00061000
.********************************************************************** 00061100
.*       LENGTH MUST BE PROVIDED FOR MF=I                               00061200
         AIF   (('&LENGTH' EQ '') AND ('&MF' EQ 'I')).ERROR4            00061300
         AIF   (('&LENGTH' EQ '') AND ('&MF' EQ 'L')).MFIL02            00061400
         AIF   ('&LENGTH'(1,1) EQ '(').MFIL02                           00061500
         DC    AL1(&LENGTH)             REPLY LENGTH                    00061600
         AGO   .MFIL03                                                  00061700
.MFIL02  DC    AL1(0)                   REPLY LENGTH                    00061800
.*       REPLY ADDR MUST BE PROVIDED FOR MF=I                           00061900
.MFIL03  AIF   (('&RYAD' EQ '') AND ('&MF' EQ 'I')).ERROR5              00062000
         AIF   (('&RYAD' EQ '') AND ('&MF' EQ 'L')).MFIL04              00062100
         AIF   ('&RYAD'(1,1) EQ '(').MFIL04                             00062200
         DC    AL3(&RYAD)               REPLY ADDRESS                   00062300
         AGO   .MFIL05                                                  00062400
.MFIL04  DC    AL3(0)                   REPLY ADDRESS                   00062500
.*       ECB ADDR MUST BE PROVIDED FOR MF=I                             00062600
.MFIL05  AIF   (('&ECB' EQ '') AND ('&MF' EQ 'I')).ERROR6               00062700
         AIF   (('&ECB' EQ '') AND ('&MF' EQ 'L')).MFIL06               00062800
         AIF   ('&ECB'(1,1) EQ '(').MFIL06                              00062900
         DC    A(&ECB)                  ECB ADDRESS                     00063000
         AGO   .MFIL07                                                  00063100
.MFIL06  DC    A(0)                     ECB ADDRESS                     00063200
.MFIL07  ANOP                                                           00063300
.*       GENERATE INLINE MESSAGE TEXT                                   00063400
&I       SETA  1                                                        00063500
&LEN     SETA  K'&SYSLIST(1,1)-2                                        00063600
&PAIR    SETB  0                                                        00063700
.QLOOP1  ANOP                                                           00063800
&I       SETA  &I+1+&PAIR                                               00063900
         AIF   (&I GE K'&SYSLIST(1,1)).QDONE1                           00064000
&PAIR    SETB  ('&SYSLIST(1,1)'(&I,2) EQ '''''' OR '&SYSLIST(1,1)'(&I,2X00064100
               ) EQ '&&')                                               00064200
&LEN     SETA  &LEN-&PAIR                                               00064300
         AGO   .QLOOP1                                                  00064400
.QDONE1  ANOP                                                           00064500
&LEN     SETA  4+&LEN                                                   00064600
         DC    AL2(&LEN)                TEXT LENGTH                     00064700
         DC    B'&MC(1)&MC(2)&MC(3)&MC(4)&MC(5)&MC(6)&MC(7)&MC(8)&MC(9)X00064800
               &MC(10)&MC(11)&MC(12)&MC(13)&MC(14)&MC(15)&MC(16)'      X00064900
                                        MCS FLAGS                       00065000
         AIF   (&LEN EQ 4).MFE6BDC                                      00065100
         DC    C&SYSLIST(1,1)           MESSAGE TEXT                    00065200
.*       GENERATE FLAG BYTES                                            00065300
.MFE6BDC AIF   (NOT &GENRD).MFIL08      GENERATING ROUT/DESC/MSGTYP ?   00065400
         DC    B'&DE(1)&DE(2)&DE(3)&DE(4)&DE(5)&DE(6)&DE(7)&DE(8)&DE(9)X00065500
               &DE(10)&DE(11)&DE(12)&DE(13)&DE(14)&DE(15)&DE(16)'      X00065600
                                        DESCRIPTOR CODES                00065700
         DC    B'&RO(1)&RO(2)&RO(3)&RO(4)&RO(5)&RO(6)&RO(7)&RO(8)&RO(9)X00065800
               &RO(10)&RO(11)&RO(12)&RO(13)&RO(14)&RO(15)&RO(16)'      X00065900
                                        ROUTING CODES                   00066000
         AIF   (NOT &GENMT).MFIL08      GENERATING MSGTYP ?             00066100
         DC    B'&MT(1)&MT(2)&MT(3)&MT(4)&MT(5)&MT(6)&MT(7)&MT(8)&MT(9)X00066200
               &MT(10)&MT(11)&MT(12)&MT(13)&MT(14)&MT(15)&MT(16)'      X00066300
                                        MSGTYP                          00066400
.MFIL08  ANOP                                                           00066500
         AIF   ('&MF' EQ 'I').MFIL09                                    00066600
.*       MF=L, ENSURE NO REGISTER VALUES HAVE BEEN PROVIDED             00066700
         AIF   ('&RYAD' EQ '').MFIL08A                                  00066800
         AIF   ('&RYAD'(1,1) EQ '(').ERROR3                             00066900
.MFIL08A AIF   ('&LENGTH' EQ '').MFIL08B                                00067000
         AIF   ('&LENGTH'(1,1) EQ '(').ERROR3                           00067100
.MFIL08B AIF   ('&ECB' EQ '').MFIL08C                                   00067200
         AIF   ('&ECB'(1,1) EQ '(').ERROR3                              00067300
.MFIL08C MEXIT                                                          00067400
.*       MF=I, STORE ANY VALUES PROVIDED IN REGISTERS                   00067500
.MFIL09  ANOP                                                           00067600
&GNAME.A DS    0H                                                       00067700
         AIF   ('&RYAD'(1,1) NE '(').MFIL10                             00067800
         AIF   ('&LENGTH'(1,1) EQ '(').MFIL11                           00067900
         IC    14,0(1,0)                SAVE REPLY LENGTH               00068000
         ST    &RYAD(1),0(1,0)          STORE REPLY ADDRESS             00068100
         STC   14,0(1,0)                RESTORE REPLY LENGTH            00068200
         AGO   .MFIL12                                                  00068300
.MFIL11  ST    &RYAD(1),0(1,0)          STORE REPLY ADDRESS             00068400
.MFIL13  STC   &LENGTH(1),0(1,0)        STORE REPLY LENGTH              00068500
         AGO   .MFIL12                                                  00068600
.MFIL10  AIF   ('&LENGTH'(1,1) EQ '(').MFIL13                           00068700
.MFIL12  AIF   ('&ECB'(1,1) NE '(').MFIL14                              00068800
         ST    &ECB(1),4(1,0)           STORE ECB ADDRESS               00068900
.MFIL14  SVC   35                                                       00069000
         MEXIT                                                          00069100
.********************************************************************** 00069200
.*                                                                      00069300
.*       GENERATE XWPL VERSION 1 OR VERSION 2 FOR WTOR                  00069400
.*                                                                      00069500
.********************************************************************** 00069600
.*                                                                      00069700
.*       A VERSION 1 XWPL WILL BE GENERATED IF INLINE TEXT IS           00069800
.*       PROVIDED. THE WTOR MACRO POSITIONAL PARAMETERS OF &RYAD,       00069900
.*       &LENGTH AND &ECB WILL BE PLACED IN THE VERSION 1 XWPL FIELDS   00070000
.*       IF THE TEXT= PARAMETER IS PROVIDED THEN A VERSION 2 XWPL       00070100
.*       WILL BE GENERATED. THE &RYAD, &LENGTH AND &ECB FIELDS          00070200
.*       MUST BE PROVIDED AS SUB-PARAMETERS IN THE TEXT= PARAMETER.     00070300
.*       THE POSITIONAL PARAMETERS &RYAD, &LENGTH AND &ECB MUST NOT     00070400
.*       BE PROVIDED.                                                   00070500
.*                                                                      00070600
.XWPL01  AIF   ('&TEXT' NE '^').XWPL04     HAVE TEXT= PARM TO PROCESS ? 00070700
.*       GENERATE INLINE TEXT FOR A XWPL VERSION 1                      00070800
.XWPL01A ANOP                                                           00070900
&I       SETA  1                                                        00071000
&LEN     SETA  K'&SYSLIST(1,1)-2    L'TEXT                              00071100
&PAIR    SETB  0                                                        00071200
.*       LOOP THROUGH TEXT STRING AND PROCESS DUPLICATE QUOTES AND      00071300
.*       AMPERSANDS                                                     00071400
.XWPL02  ANOP                                                           00071500
&I       SETA  &I+1+&PAIR                                               00071600
         AIF   (&I GE K'&SYSLIST(1,1)).XWPL03   GOTO LOOP EXIT          00071700
&PAIR    SETB  ('&SYSLIST(1,1)'(&I,2) EQ '''''' OR '&SYSLIST(1,1)'(&I,2X00071800
               ) EQ '&&')                                               00071900
&LEN     SETA  &LEN-&PAIR                                               00072000
         AGO   .XWPL02                 EXIT FROM LOOP                   00072100
.*       GENERATE XWPL FIELDS                                           00072200
.XWPL03  ANOP                                                           00072300
.*       GENERATE TEXT FOR XWPL VERSION 1                               00072400
&LEN     SETA  4+&LEN                                                   00072500
         AIF   (&LEN EQ 4).ERROR8                                       00072600
         DC    AL2(&LEN)                TEXT LENGTH                     00072700
         DC    B'&MC(1)&MC(2)&MC(3)&MC(4)&MC(5)&MC(6)&MC(7)&MC(8)&MC(9)X00072800
               &MC(10)&MC(11)&MC(12)&MC(13)&MC(14)&MC(15)&MC(16)'      X00072900
                                        MCS FLAGS                       00073000
         DC    C&SYSLIST(1,1)           MESSAGE TEXT                    00073100
         DC    AL1(1)                   XWPL VERSION LEVEL              00073200
         AGO   .XWPL05                                                  00073300
.*                                                                      00073400
.*       GENERATE TEXT ADDR XWPL VERSION 2                              00073500
.*                                                                      00073600
.*       TEXT= PARAMETER HAS BEEN PROVIDED. THE SECOND, THIRD AND       00073700
.*       FORTH POSITIONAL PARAMETERS MUST NOT BE PROVIDED               00073800
.XWPL04  AIF   ('&TEXT' EQ '').ERROR2     TEXT= CANNOT BE NULL          00073900
         AIF   (('&RYAD' NE '') OR ('&LENGTH' NE '') OR                X00074000
               ('&ECB' NE '')).ERROR6                                   00074100
         DC    AL2(8)                   TEXT LENGTH                     00074200
         DC    B'&MC(1)&MC(2)&MC(3)&MC(4)&MC(5)&MC(6)&MC(7)&MC(8)&MC(9)X00074300
               &MC(10)&MC(11)&MC(12)&MC(13)&MC(14)&MC(15)&MC(16)'      X00074400
                                        MCS FLAGS                       00074500
         AIF   ('&MF' NE 'I').XWPL04A                                   00074600
.*       MF=I GENERATE ADDR IF PROVIDED                                 00074700
         AIF   ('&TEXT(1)' EQ '').ERROR2  MUST BE PROVIDED FOR MF=I     00074800
         AIF   ('&TEXT(1)'(1,1) EQ '(').XWPL04A  REG NOTATION ?         00074900
         DC    AL4(&TEXT(1))            ADDR OF MESSAGE TEXT            00075000
         AGO   .XWPL05                                                  00075100
.XWPL04A ANOP                                                           00075200
         DC    AL4(0)                   ADDR OF MESSAGE TEXT            00075300
         DC    AL1(2)                   XWPL VERSION LEVEL              00075400
.*       GENERATE FIELDS COMMON TO XWPL VER 1 AND XWPL VER 2            00075500
.XWPL05  ANOP                                                           00075600
         DC    B'00000000'              MISCELLANEOUS FLAGS             00075700
.*       REPLY LENGTH PROCESSING                                        00075800
&WORKL   SETC  '&LENGTH'                                                00075900
         AIF   (&WPLXLEV EQ 1).XWPL05C                                  00076000
&WORKL   SETC  '&TEXT(3)'               XWPL VERSION 2                  00076100
.XWPL05C AIF   (('&MF' EQ 'L') AND ('&WORKL' EQ '')).XWPL05A            00076200
         AIF   (('&MF' EQ 'L') AND ('&WORKL'(1,1) EQ '(')).ERROR3       00076300
         AIF   (('&MF' EQ 'I') AND ('&WORKL' EQ '')).ERROR4             00076400
         AIF   ('&WORKL'(1,1) EQ '(').XWPL05A  REGISTER NOTATION ?      00076500
         DC    AL1(&WORKL)              L'REPLY FOR WTOR                00076600
         AGO   .XWPL05B                                                 00076700
.XWPL05A DC    AL1(0)                   L'REPLY FOR WTOR                00076800
.*       DETERMINE LEVEL OF XWPL                                        00076900
.XWPL05B AIF   (&WPLXLEV EQ 2).XWPL06                                   00077000
.*       XWPL LENGTH VALUE FOR XWPL VERSION 1                           00077100
         DC    AL1(0)                   RESERVED                        00077200
         AGO   .XWPL07                                                  00077300
.*       XWPL LENGTH VALUE FOR XWPL VERSION 2                           00077400
.XWPL06  ANOP                                                           00077500
         DC    AL1(104)                 LENGTH OF XWPL VERSION 2        00077600
.XWPL07  ANOP                                                           00077700
.*       GENERATE EXTENDED MCS FLAGS                                    00077800
         DC    B'&MC(17)&MC(18)&MC(19)&MC(20)&MC(21)&MC(22)&MC(23)&MC(2X00077900
               4)&MC(25)&MC(26)&MC(27)&MC(28)&MC(29)&MC(30)&MC(31)&MC(3X00078000
               2)'                      EXTENDED MCS FLAGS              00078100
         DC    XL2'0000'                MCS FLAGS FOR CNTL PROGRAM USE  00078200
.*       REPLY BUFFER PROCESSING                                        00078300
&WORKA   SETC  '&RYAD'                                                  00078400
         AIF   (&WPLXLEV EQ 1).XWPL07C                                  00078500
&WORKA   SETC  '&TEXT(2)'               XWPL VERSION 2                  00078600
.XWPL07C AIF   (('&MF' EQ 'L') AND ('&WORKA' EQ '')).XWPL07A            00078700
         AIF   (('&MF' EQ 'L') AND ('&WORKA'(1,1) EQ '(')).ERROR3       00078800
         AIF   (('&MF' EQ 'I') AND ('&WORKA' EQ '')).ERROR5             00078900
         AIF   ('&WORKA'(1,1) EQ '(').XWPL07A  REGISTER NOTATION ?      00079000
         DC    AL4(&WORKA)              ADDR OF REPLY BUFFER FOR WTOR   00079100
         AGO   .XWPL07B                                                 00079200
.XWPL07A DC    AL4(0)                   ADDR OF REPLY BUFFER FOR WTOR   00079300
.XWPL07B ANOP                                                           00079400
.*       REPLY ECB PROCESSING                                           00079500
&WORKE   SETC  '&ECB'                                                   00079600
         AIF   (&WPLXLEV EQ 1).XWPL08C                                  00079700
&WORKE   SETC  '&TEXT(4)'               XWPL VERSION 2                  00079800
.XWPL08C AIF   (('&MF' EQ 'L') AND ('&WORKE' EQ '')).XWPL08A            00079900
         AIF   (('&MF' EQ 'L') AND ('&WORKE'(1,1) EQ '(')).ERROR3       00080000
         AIF   (('&MF' EQ 'I') AND ('&WORKE' EQ '')).ERROR6             00080100
         AIF   ('&WORKE'(1,1) EQ '(').XWPL08A  REGISTER NOTATION ?      00080200
         DC    AL4(&WORKE)              ADDR OF REPLY ECB FOR WTOR      00080300
         AGO   .XWPL08B                                                 00080400
.XWPL08A DC    AL4(0)                   ADDR OF REPLY ECB FOR WTOR      00080500
.XWPL08B ANOP                                                           00080600
         DC    AL4(0)                   RESERVED - CONNECT ID           00080700
         DC    B'&DE(1)&DE(2)&DE(3)&DE(4)&DE(5)&DE(6)&DE(7)&DE(8)&DE(9)X00080800
               &DE(10)&DE(11)&DE(12)&DE(13)&DE(14)&DE(15)&DE(16)'      X00080900
                                        DESCRIPTOR CODES                00081000
         DC    AL2(0)                   RESERVED                        00081100
         DC    B'&RO(1)&RO(2)&RO(3)&RO(4)&RO(5)&RO(6)&RO(7)&RO(8)&RO(9)X00081200
               &RO(10)&RO(11)&RO(12)&RO(13)&RO(14)&RO(15)&RO(16)'      X00081300
                                        EXTENDED ROUTING CODES          00081400
         DC    B'&RO(17)&RO(18)&RO(19)&RO(20)&RO(21)&RO(22)&RO(23)&RO(2X00081500
               4)&RO(25)&RO(26)&RO(27)&RO(28)&RO(29)&RO(30)&RO(31)&RO(3X00081600
               2)'                                                      00081700
         DC    B'&RO(33)&RO(34)&RO(35)&RO(36)&RO(37)&RO(38)&RO(39)&RO(4X00081800
               0)&RO(41)&RO(42)&RO(43)&RO(44)&RO(45)&RO(46)&RO(47)&RO(4X00081900
               8)'                                                      00082000
         DC    B'&RO(49)&RO(50)&RO(51)&RO(52)&RO(53)&RO(54)&RO(55)&RO(5X00082100
               6)&RO(57)&RO(58)&RO(59)&RO(60)&RO(61)&RO(62)&RO(63)&RO(6X00082200
               4)'                                                      00082300
         DC    B'&RO(65)&RO(66)&RO(67)&RO(68)&RO(69)&RO(70)&RO(71)&RO(7X00082400
               2)&RO(73)&RO(74)&RO(75)&RO(76)&RO(77)&RO(78)&RO(79)&RO(8X00082500
               0)'                                                      00082600
         DC    B'&RO(81)&RO(82)&RO(83)&RO(84)&RO(85)&RO(86)&RO(87)&RO(8X00082700
               8)&RO(89)&RO(90)&RO(91)&RO(92)&RO(93)&RO(94)&RO(95)&RO(9X00082800
               6)'                                                      00082900
         DC    B'&RO(97)&RO(98)&RO(99)&RO(100)&RO(101)&RO(102)&RO(103)&X00083000
               RO(104)&RO(105)&RO(106)&RO(107)&RO(108)&RO(109)&RO(110)&X00083100
               RO(111)&RO(112)'                                         00083200
         DC    B'&RO(113)&RO(114)&RO(115)&RO(116)&RO(117)&RO(118)&RO(11X00083300
               9)&RO(120)&RO(121)&RO(122)&RO(123)&RO(124)&RO(125)&RO(12X00083400
               6)&RO(127)&RO(128)'                                      00083500
.*                                                                      00083600
         DC    B'&MT(1)&MT(2)&MT(3)&MT(4)&MT(5)&MT(6)&MT(7)&MT(8)&MT(9)X00083700
               &MT(10)&MT(11)&MT(12)&MT(13)&MT(14)&MT(15)&MT(16)'      X00083800
                                        MSGTYPE                         00083900
         DC    AL2(0)                   MESSAGE PRIORITY                00084000
         DC    CL8'        '            JOB ID                          00084100
         DC    CL8'        '            JOB NAME                        00084200
         DC    CL8'        '            RETRIEVAL KEY                   00084300
         DC    AL4(0)                   TOKEN FOR DOM                   00084400
         DC    AL4(0)                   CONSOLE ID                      00084500
         DC    CL8'        '            SYSTEM NAME                     00084600
         AIF   (&WPLXLEV EQ 1).XWPL08   BRANCH TO GENERATE XWPL VER 1   00084700
         DC    CL8'        '            CONSOLE NAME                    00084800
         DC    AL4(0)                   REPLY CONSOLE NAME/ID ADDR      00084900
         DC    AL4(0)                   CART ADDRESS                    00085000
         DC    AL4(0)                   WSPARM ADDRESS                  00085100
         AGO   .XWPL09                                                  00085200
.*       XWPL VERSION 1 FIELDS                                          00085300
.XWPL08  ANOP                                                           00085400
         DC    AL4(0)                   RESERVED                        00085500
         DC    AL4(0)                   RESERVED                        00085600
.XWPL09  ANOP                                                           00085700
         AIF   ('&MF' EQ 'L').XWPL99    MF=L ALL FINISHED               00085800
.*       MF=I PROCESSING FOR XWPL VERSION 1 AND VERSION 2               00085900
&GNAME.A DS    0H                                                       00086000
.*       STORE ADDR OF TEXT INTO XWPL                                   00086100
         AIF   ('&TEXT' EQ '^').XWPL11                                  00086200
         AIF   ('&TEXT(1)'(1,1) EQ '(').XWPL10   TEXT=(REG) ?           00086300
         LA    15,&TEXT(1)              R15 -> TEXT                     00086400
         ST    15,4(,1)                 STORE TEXT ADDR INTO XWPL       00086500
         AGO   .XWPL11                                                  00086600
.XWPL10  ANOP                                                           00086700
         ST    &TEXT(1),4(,1)           STORE TEXT ADDR INTO XWPL       00086800
.*       DETERMINE IF REGISTER NOTATION HAS BEEN USED FOR ANY PARAMETER 00086900
.XWPL11  AIF   (('&WORKA'(1,1) NE '(') AND ('&WORKL'(1,1) NE '(') AND  X00087000
               ('&WORKE'(1,1) NE '(')).XWPL98   N REGISTER NOTATION     00087100
.*       REGISTER NOTATION HAS BEEN USED, SAVE VALUES IN REGISTERS      00087200
         LA    14,0(,1)                                                 00087300
         AH    14,0(,14)            ADD L'TEXT + LEN FIELD + MCS FIELD  00087400
         AIF   ('&WORKL'(1,1) NE '(').XWPL12                            00087500
         STC   &WORKL,2(,14)            STORE L'REPLY                   00087600
.XWPL12  AIF   ('&WORKA'(1,1) NE '(').XWPL13                            00087700
         ST    &WORKA,8(,14)            STORE ADDR OF REPLY             00087800
.XWPL13  AIF   ('&WORKE'(1,1) NE '(').XWPL98                            00087900
         ST    &WORKE,12(,14)           STORE ADDR OF ECB               00088000
.XWPL98  SVC   35                                                       00088100
.XWPL99  MEXIT                                                          00088200
.*                                                                      00088300
.********************************************************************** 00088400
.*       ISSUE ERROR MESSAGES                                           00088500
.********************************************************************** 00088600
.*                                                                      00088700
.*       INVALID MF OPERAND SPECIFIED-MF                                00088800
.*                                                                      00088900
.ERROR1  IHBERMAC 35,,&MF                                               00089000
         MEXIT                                                          00089100
.*       MESSAGE OPERAND REQ'D-NOT SPECIFIED                            00089200
.ERROR2  IHBERMAC 19                                                    00089300
         MEXIT                                                          00089400
.*       INVALID REGISTER NOTATION WITH MF=L SPECIFIED                  00089500
.ERROR3  IHBERMAC 69                                                    00089600
         MEXIT                                                          00089700
.*       REQUIRED LENGTH OPERAND NOT SPECIFIED                          00089800
.ERROR4  IHBERMAC 14                                                    00089900
         MEXIT                                                          00090000
.*       REQUIRED SECOND OPERAND NOT SPECIFIED                          00090100
.ERROR5  IHBERMAC 03                                                    00090200
         MEXIT                                                          00090300
.*       REQUIRED ECB OPERAND NOT SPECIFIED                             00090400
.ERROR6  IHBERMAC 11                                                    00090500
         MEXIT                                                          00090600
.*       MLWTO/WTOR MUTUALLY EXCLUSIVE                                  00090700
.ERROR7  IHBERMAC 246                                                   00090800
         MEXIT                                                          00090900
.*       MISSING OR EXCESSIVE MESSAGE TEXT                              00091000
.ERROR8  MNOTE 12,'NUMBER OF LINES REQUESTED IS 0 OR GREATER THAN 255 -X00091100
                GENERATION TERMINATED'                                  00091200
         MEXIT                                                          00091300
.*       MESG AND TEXT BOTH PROVIDED                                    00091400
.ERROR9  MNOTE 12,'INLINE TEXT AND ''TEXT'' ARE MUTUALLY EXCLUSIVE'     00091500
.*                                                                      00091600
         MEND                                                           00091700
./ ENDUP
/*
//*
//STEP02  EXEC PGM=IEBUPDTE,PARM='NEW'
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=&&SOURCE,DISP=(NEW,PASS),
//             UNIT=SYSALLDA,SPACE=(CYL,(1,1,10)),
//             DCB=(DSORG=PO,RECFM=FB,LRECL=80,BLKSIZE=6160)
//SYSIN    DD  *
./ ADD NAME=IEAVMWTO
VMW      TITLE 'IEAVMWTO - SVC 35 - MLWTO SERVICE ROUTINE'              00001000
*                                                                       00002000
IEAVMWTO CSECT                                                          00003000
*                                                                       00004000
*        STATUS -                                                       00005000
*        LASTUPD         = JCLIN    TYPE=UPD                            00006000
*        LIBRARIES       = DISTLIB=AOSC5                                00007000
*        FMID            = FBB1221                                      00008000
*        RMID            = UZ62088                                      00009000
*        LMOD            = IGC0003E                                     00010000
*        SYSMOD HISTORY  = SYSMOD   TYPE       DATE   MCS  STATUS       00011000
*                          EBB1102  FUNCTION  00.000  MOD      ACC RGN  00012000
*                          FBB1221  FUNCTION  00.000  MOD      ACC RGN  00013000
*                          UZ62088  PTF       74.164  MOD  APP ACC      00014000
*                                                                       00015000
*        ORIGINAL SOURCE FROM MVSSRC.SYM101.F20 WAS UPDATED             00016000
*        BY DISASSEMBLY TO MATCH THE LOAD MODULE PROVIDED BY            00017000
*        UZ62088.                                                       00018000
*                                                                       00019000
*        CHANGE ACTIVITY -                                              00020000
*               ZP60039  - 1. SUPPORT FOR THE EXTENDED WPL              00021000
*                             PARAMETER LIST TO ENABLE THE USE OF       00022000
*                             THE TEXT OPERAND                          00023000
*                          2. NUMEROUS CHANGES TO IMPROVE SOURCE        00024000
*                             READABILITY                               00025000
*                          3. LOCALIZED CODE OPTIMZATION                00026000
*                          4. TRANSLATION OF UNPRINTABLE CHARACTERS     00027000
*                             IN WPL TEXT TO HEX A1 (TILDE CHARACTER)   00028000
*               18 AUG 20  -  CHANGE TO COMMON INTERNAL WORKAREA        00029000
*                             MAPPING IN SUPPORT OF MESSAGE             00030000
*                             TRUNCATION IN IEAVVWTO                    00031000
*                                                                       00032000
*        DESCRIPTIVE NAME - MULTI-LINE WTO SERVICE ROUTINE              00033000
*                                                                       00034000
*        FUNCTION -                                                     00035000
*        THE MLWTO SERVICE ROUTINE IS PART OF IGC0003E OR SVC 35.       00036000
*        IT RECEIVES CONTROL WHEN A MLWTO IS ISSUED. A MULTIPLE         00037000
*        LINE WTO CAUSES A MAJOR WQE AND ONE OR MORE MINOR WQES         00038000
*        TO BE BUILT. THE WQES, MAJOR AND MINOR, CONTAIN MESSAGES       00039000
*        FOR THE CONSOLE OPERATOR AND ASSOCIATED CONTROL                00040000
*        INFORMATION                                                    00041000
*                                                                       00042000
*        THE MODULE CONSTRUCTS A MAJOR WQE AND MINOR WQES FOR           00043000
*        EACH MLWTO. IT WILL ALSO CONNECT ADDITIONAL DATA LINES         00044000
*        (OR MINOR WQES) TO AN EXISTING MLWTO. CONNECTING IS            00045000
*        RESERVED FOR KEY ZERO TO SEVEN OR SUPERVISOR MODE USERS        00046000
*        OF MLWTO ONLY.                                                 00047000
*                                                                       00048000
*        NON PP SVC 35 CALLERS (SUPERVISOR OR KEY 0-7 USERS)            00049000
*        ISSUING A STANDARD MULTI-LINE WTO MUST ZERO R0 UNLESS THEY     00050000
*        ARE PASSING A CONNECTING MESSAGE ID OR CONSOLE ID IN R0        00051000
*                                                                       00052000
*        THE ROUTINE PERFORMS THE FOLLOWING MAJOR FUNCTIONS IN          00053000
*        HANDLING AN MLWTO REQUEST                                      00054000
*                                                                       00055000
*     1. SET UP ADDRESSABILITY AND OBTAIN STORAGE FOR A WORKAREA        00056000
*                                                                       00057000
*     2. CHECK THAT THE LINE TYPES FOR EACH LINE OF THE MESSAGE         00058000
*        IS CORRECT. THE MESSAGE WILL BE TERMINATED AT THE LAST         00059000
*        CORRECT LINE IF AN INVALID LINE TYPE SITUATION IS FOUND        00060000
*                                                                       00061000
*     3. GET STORAGE FOR A MAJOR WQE AND ONE MINOR WQE IF THIS REQUEST  00062000
*        IS NOT ASKING TO ADD MORE LINES TO AN EXISTING MESSAGE         00063000
*                                                                       00064000
*     4. FILL IN THE MAJOR WQE WITH THE FIRST LINE OF THE MESSAGE.      00065000
*        IF THE MESSAGE HAS A DESCRIPTOR CODE OF 9 AND NO CONTROL       00066000
*        LINE AS THE LINE THEN A DEFAULT CONTROL MESSAGE IEE932I        00067000
*        IS SUPPLIED AS THE FIRST LINE OF THE MESSAGE                   00068000
*                                                                       00069000
*     5. PASS THE FIRST LINE TO THE SUBSYSTEM EXIT.                     00070000
*        THE TEXT IN WQETEXT IS TRANSLATED TO INSURE ONLY               00071000
*        PRINTABLE AND DISPLAYABLE CHARACTERS ARE PASSED FROM           00072000
*        THIS POINT                                                     00073000
*                                                                       00074000
*     6. XMPOST THE UCMOECB SO THAT THE COMMUNICATIONS TASK CAN START   00075000
*        PROCESSING THE MLWTO                                           00076000
*                                                                       00077000
*     7. CHECK IF THE MAJOR WQE EXISTS SO THAT THE NEXT LINE CAN BE     00078000
*        ADDED. IF THE SERVICE REQUEST WAS TO ADD EXTRA LINES THEN      00079000
*        THE PROCESSING WOULD SKIP STEPS 4 TO 7. THE MODULE IS          00080000
*        WRITTEN SO THAT EACH ADDITIONAL LINE IS HANDLED IN THE         00081000
*        SAME MANNER                                                    00082000
*                                                                       00083000
*     8. CHECK IF THERE IS SPACE IN THE MINOR WQE/WQES FOR THIS LINE.   00084000
*        GET A NEW MINOR WQE IF THERE IS NO SPACE. THERE IS ALWAYS      00085000
*        AT LEAST ONE MINOR POINTED TO BY THE MAJOR. THIS IS DONE TO    00086000
*        MINIMIZE GETMAIN/FREEMAIN USAGE.                               00087000
*                                                                       00088000
*     9. FILL IN THE NEXT LINE                                          00089000
*                                                                       00090000
*     10. PASS THE MAJOR WQE AND THE MINOR WQE WITH THE NEW LINE        00091000
*         TO THE SUBSYSTEM WTO EXIT.                                    00092000
*         ON RETURN THE TEXT IS TRANSLATED AS IN STEP 6                 00093000
*                                                                       00094000
*     11. POST THE UCMOECB                                              00095000
*                                                                       00096000
*     12. IF THERE ARE ANY MORE LINES IN THE WPL THEN GO BACK TO        00097000
*         STEP 8. IF NOT THEN RETURN TO IEAVVWTO                        00098000
*                                                                       00099000
*        OPERATION -                                                    00100000
*        THIS MODULE USES THE EXTENDED SAVEAREA IN THE SVRB.            00101000
*        SOME OF THE INFORMATION IN THIS AREA IS SET UP BY              00102000
*        IEAVVWTO PRIOR TO THE CALL TO IEAVMWTO                         00103000
*                                                                       00104000
*        NOTES -                                                        00105000
*        THE MODULE USES A NUMBER OF INTERNAL SUBROUTINES.              00106000
*        THE ROUTINES ARE ORGANIZED SO THAT THEY HAVE ONLY ONE          00107000
*        ENTRY POINT AND ONE EXIT POINT. MOST ROUTINES ARE ALSO         00108000
*        WELL STRUCTURED IN TERMS OF INSTRUCTION FLOW. THE              00109000
*        SEGMENTS USED IN THIS MODULE ARE LISTED.                       00110000
*                                                                       00111000
*                                                                       00112000
*     1. IEASGETL - DETERMINES THE NUMBER OF LINES IN THE WPL           00113000
*                                                                       00114000
*     2. IEASTLMX - LIMITS SERVICE REQUEST TO 10 LINES AT A TIME        00115000
*                                                                       00116000
*     3. CHKTYPE - CHECKS THAT LINES HAVE THE PROPER LINE TYPE          00117000
*        AND IN THE PROPER ORDER                                        00118000
*                                                                       00119000
*     4. INCRMNT - LOCATES THE BEGINNING OF THE NEXT LINE IN THE        00120000
*        WPL                                                            00121000
*                                                                       00122000
*     5. IEASTORE - SAVES THE NUMBER OF LINES FOUND IN THE WPL          00123000
*        SETS XVX0UDCL IF THE DEFAULT CONTROL LINE IS TO BE USED        00124000
*                                                                       00125000
*     6. BLDMAJ - GET SPACE FOR THE MAJOR AND MINOR WQE. BUILD MAJOR    00126000
*                                                                       00127000
*     7. GETMAJ - OBTAINS SPACE FOR THE WQE                             00128000
*                                                                       00129000
*     8. FREEMAJ - FREES THE MAJOR WQE IF A MINOR COULDN'T BE           00130000
*        OBTAINED                                                       00131000
*                                                                       00132000
*     9. BMAJINIT - INITIALIZE THE MAJOR WQE                            00133000
*                                                                       00134000
*     10.BMAJFSTL - SET UP TO MOVE TEXT INTO MAJOR                      00135000
*                                                                       00136000
*     11.BMAJMVMS - MOVE TEXT INTO MAJOR. TRUNCATE TRAILING BLANKS      00137000
*                                                                       00138000
*     12.BLDMIN - FILL IN MINOR LINES                                   00139000
*                                                                       00140000
*     13.MIN1INIT/MIN2INIT - INITIALIZE THE FIRST/SECOND LINE IN        00141000
*        THE MINOR WQE                                                  00142000
*                                                                       00143000
*     14.MIN1MOV/MIN2MOV - MOVE THE TEXT INTO THE FIRST/SECOND LINE     00144000
*        OF THE MINOR WQE                                               00145000
*                                                                       00146000
*     15.SUBSEXIT - PASS MAJOR/MINOR WQE TO SUBSYSTEM WTO EXIT.         00147000
*        ON RETURN FROM THE SUBSYSTEM EXIT THE USER IS A                00148000
*        PROBLEM PROGRAM THE WQE TEXT -MAJOR/MINOR- IS TRANSLATED       00149000
*        TO ALLOW ONLY PRINTABLE AND DISPLAYABLE CHARACTERS             00150000
*        FROM THIS POINT.                                               00151000
*                                                                       00152000
*     16.POSTOECB - XMPOST UCMOECB TO WAKE UP COMM TASK                 00153000
*                                                                       00154000
*     17.FREESAV - FREE THE MODULES DYNAMIC WORKAREA                    00155000
*                                                                       00156000
*        THE INTERNAL ROUTINES USED IN THE MODULE ARE DESCRIBED BELOW   00157000
*                                                                       00158000
*     1. GETMINOR - GETS SPACE FOR A MINOR WQE AND ADDS IT TO           00159000
*        QUEUE OF MINORS POINTED TO BY THE MAJOR.                       00160000
*                                                                       00161000
*     2. ENDUP - DECREMENTS THE NUMBER OF LINES TO BE DONE. SETS        00162000
*        LINE TYPE TO DATA-END IF NEEDED                                00163000
*                                                                       00164000
*     3. FINDID - LOCATES MAJOR TO WHICH THE MINOR LINE IS TO BE        00165000
*                 ADDED                                                 00166000
*                                                                       00167000
*     6. GETWQE - GETS A WQE FROM THE WQE CELLPOOL                      00168000
*                                                                       00169000
*     7. WAITWQE - WAIT FOR A WQE TO BE FREED                           00170000
*                                                                       00171000
*     8. TEXTLINE - INCREMENTS R4 TO THE NEXT LINE IN THE               00172000
*                   INPUT TEXT STRING                                   00173000
*                                                                       00174000
*     9. FRELCKS - FREES THE CMS AND LOCAL LOCK                         00175000
*                                                                       00176000
*     10.SETLCKS - OBTAINS THE LOCAL AND CMS LOCK                       00177000
*                                                                       00178000
*        DEPENDENCIES -                                                 00179000
*        THIS MODULE IS LINKED WITH IEAVVWTO, IGC0203E AND              00180000
*        IEECVXIT TO FORM ONE LOAD MODULE                               00181000
*        ENTRY POINT IS IEAVVWTO. THE LINK IS DONE AT SYSGEN            00182000
*                                                                       00183000
*        ATTRIBUTES -                                                   00184000
*        PAGED-LPA, ZERO PROTECT KEY, REENTERABLE, SUPERVISOR MODE      00185000
*                                                                       00186000
*        ENTRY-POINT - IEAVMWTO                                         00187000
*                                                                       00188000
*        PURPOSE -                                                      00189000
*        THIS IS THE ONLY ENTRY POINT TO THE ROUTINE. IT IS GIVEN       00190000
*        CONTROL BY IEAVVWTO TO PROCESS A MULTI-LINE WTO REQUEST        00191000
*                                                                       00192000
*        LINKAGE -                                                      00193000
*        IEAVMWTO IS CALLED BY IEAVVWTO USING STANDARD SYSTEM           00194000
*        LINKAGE CONVENTIONS                                            00195000
*                                                                       00196000
*        INPUT -                                                        00197000
*        THE FOLLOWING REGISTERS ARE INITIALIZED BY IEAVVWTO            00198000
*        R4   -> TCB                                                    00199000
*        R5   -> SVRB                                                   00200000
*        R7   -> ASCB                                                   00201000
*        R13  -> CALLER'S SAVEAREA                                      00202000
*        R14   = RETURN ADDRESS                                         00203000
*        R15   = ENTRY POINT ADDRESS                                    00204000
*                                                                       00205000
*        THE WPL HAS BEEN CHECKED FOR VALIDITY BY IEAVVWTO.             00206000
*        A MULT-LINE WTOR IS NOT PERMITTED. THIS IS CHECKED FOR         00207000
*        BY IEAVVWTO PRIOR TO IEAVMWTO BEING CALLED                     00208000
*                                                                       00209000
*        THE XVSAV AREA HAS BEEN INITIALIZED BY IEAVVWTO -              00210000
*        XVA8    =  TIME TAKEN BY IEAVVWTO                              00211000
*        XVD1PRIV = ON IF CALLER IS PRIVILEGED                          00212000
*        XVD1PP   = ON IF CALLER IS A PROBLEM PROGRAM                   00213000
*        XVD1AUTH = ON IF CALLER IS AUTHORIZED                          00214000
*        XVD2VALD = ON                                                  00215000
*        XVWQEID  = CONTENTS OF R0 PROVIDED BY SVC 35 ISSUER            00216000
*         XVWQEIDA = A NEW LINE IS TO BE CONNECTED TO THE MESSAGE       00217000
*                    WITH THIS MESSAGE ID.  MULTI-LINE WTO ONLY         00218000
*         XVCONID  = CONSOLE ID PASSED IN R0 TO SVC 35                  00219000
*                                                                       00220000
*        OUTPUT -                                                       00221000
*        THIS ROUTINE CREATES NO STREAM OR LIST OUTPUT                  00222000
*                                                                       00223000
*        REGISTERS SAVED -                                              00224000
*        THE CALLER'S REGISTERS ARE SAVED IN THE PROVIDED SAVEAREA      00225000
*                                                                       00226000
*        REGISTER USAGE -                                               00227000
*        THE REGISTER USAGE AND THE ASSOCIATED NAMES ARE GIVEN IN       00228000
*        THE FOLLOWING TABLE                                            00229000
*         REG      USAGE                                                00230000
*          0       WORK REG                                             00231000
*                                                                       00232000
*          1       WORK REG                                             00233000
*                                                                       00234000
*          2       WORK REG                                             00235000
*                                                                       00236000
*          3       -> CVT                                               00237000
*                  COUNTER FOR HANDLING TEXT LENGTH                     00238000
*                                                                       00239000
*          4       ADDR OF THE TCB                                      00240000
*                  ADDR OF TEXT LINE CURRENTLY BEING PROCESSED          00241000
*                                                                       00242000
*          6       -> WPL                                               00243000
*                                                                       00244000
*          7       ADDR OF THE ASCB                                     00245000
*                  ADDR OF MINOR WQE                                    00246000
*                                                                       00247000
*          8       ADDR OF THE MAJOR WQE                                00248000
*                  USED ONLY TO DEVELOP COUNT OF NUMBER OF LINES        00249000
*                  TO BE PROCESSED.                                     00250000
*                                                                       00251000
*          9       -> WORKAREA USED BY IEAVVWTO                         00252000
*                                                                       00253000
*          10      BASE FOR UCM                                         00254000
*                  COMPLETION CODE FOR XMPOST                           00255000
*          11      PROGRAM BASE                                         00256000
*                  ECB ADDR FOR XMPOST                                  00257000
*          12      BASE FOR XVSAV AREA                                  00258000
*                  ERRET ADDR FOR XMPOST                                00259000
*          13      SAVE AREA BASE AND WORK REG                          00260000
*                  ADDR ASCB FOR XMPOST                                 00261000
*          14      WORK REG                                             00262000
*          15      WORK REG                                             00263000
*                                                                       00264000
*                                                                       00265000
*        REGISTERS RESTORED -                                           00266000
*        ALL OF THE CALLER'S REGISTERS ARE RESTORED                     00267000
*                                                                       00268000
*        EXIT NORMAL -                                                  00269000
*        THIS MODULE HAS ONLY ONE EXIT POINT                            00270000
*                                                                       00271000
*        CONDITIONS -                                                   00272000
*        IEAVMWTO ALWAYS RETURNS TO ITS CALLER, IEAVVWTO.               00273000
*        IT RETURNS AFTER SERVICING THE MULTI-LINE REQUEST              00274000
*                                                                       00275000
*        OUTPUT -                                                       00276000
*        THE RETURN TO IEAVVWTO IS TAKEN FOR SUCCESSFUL AND             00277000
*        UNSUCCESSFUL SERVICING OF THE MULTI-LINE REQUEST.              00278000
*        THE OUTPUT FOR A SUCCESSFUL SERVICING IS -                     00279000
*         XVWQEID = 3 BYTE MESSAGE SEQUENCE NUMBER, RIGHT JUSTIFIED.    00280000
*         XVRETCOD = 0 THEN NOTHING FOUND IN ERROR.                     00281000
*                                                                       00282000
*                    4 AN ERROR WAS FOUND IN THE NUMBER OF LINES.       00283000
*                      MESSAGE WAS TRUNCATED TO TEN LINES AND LAST      00284000
*                      LINE SET TO DATA/END. - OR -                     00285000
*                      THE ACTUAL TEXT LENGTH FOR ONE LINE WAS ZERO.    00286000
*                      THE MESSAGE TRUNCATED AT THE PREVIOUS LINE.      00287000
*                                                                       00288000
*                   12 THE LINE TYPE FOR A LINE WAS INVALID. THE        00289000
*                      MESSAGE WAS TO THE LAST VALID LINE AND ITS       00290000
*                      LINE TYPE WAS SET TO DATA/END.                   00291000
*                                                                       00292000
*                   16 THE MLWTO HAD A WTP ROUTE CODE AND OTHER ROUTE   00293000
*                      CODES. THE WTP ROUTE CODE (ROUTE CODE 11)        00294000
*                      WAS IGNORED.                                     00295000
*                                                                       00296000
*        THE OUTPUT FOR AN UNSUCCESSFUL SERVICING IS:                   00297000
*        XVWQEID = 0. NO MESSAGE WAS PUT OUT, THEREFORE MSGID IS ZERO   00298000
*        XVRETCOD = 4 -THE NUMBER OF LINES IN THE WPL WAS ZERO.         00299000
*                                                                       00300000
*                     -MESSAGE TEXT LENGTH FOR A LINE WAS               00301000
*                      GREATER THAN 1; ALL LINES UP TO                  00302000
*                      ERROR LINE ARE PROCESSED                         00303000
*                                                                       00304000
*                   8 THE MESSAGE ID PASSED IN R0 DID NOT MATCH         00305000
*                     ANY ID FOR A WQE CURRENTLY IN THE SYSTEM.         00306000
*                     THIS OCCURS ONLY WHEN IEAVMWTO IS ATTEMPTING      00307000
*                     TO CONNECT NEW LINES TO AN EXISTING MESSAGE.      00308000
*                     THIS PROBLEM CAN ARISE IN THE FOLLOWING WAYS.     00309000
*                     1. REG 0 IS NOT ZERO FOR THE FIRST SERVICE        00310000
*                        REQUEST OF A MULTI-LINE WTO.                   00311000
*                                                                       00312000
*                     2. THE MULTI-LINE MSG WAS GOING TO A CONSOLE      00313000
*                        THAT ENCOUNTERED AN I/O ERROR. CONSOLE         00314000
*                        SWTICH DELETED THE MSG AS MULTI-LINE MSGS      00315000
*                        CANT BE SWITCHED.                              00316000
*                                                                       00317000
*                     3. THE USER LOST THE MESSAGE ID PASSED BACK       00318000
*                        IN R1 BY SVC 35.                               00319000
*                                                                       00320000
*                  12 THE NEW MULTI-LINE MSG CONSISTS OF ONLY AN        00321000
*                     END LINE.                                         00322000
*                                                                       00323000
*                  16 ROUTE CODE 11 (WTP) WAS THE ONLY ROUTE CODE       00324000
*                     SPECIFIED.                                        00325000
*                                                                       00326000
*                  20 THE MULTI-LINE MSG WAS TO BE SET TO HARD COPY     00327000
*                     ONLY.                                             00328000
*                                                                       00329000
*        RETURN CODES -                                                 00330000
*        THE RETURN CODES ARE SET IN XVRETCOD AND THEIR MEANINGS        00331000
*        ARE DESCRIBED ABOVE                                            00332000
*                                                                       00333000
*        EXIT ERROR -                                                   00334000
*        THERE IS NO ERROR EXIT FROM IEAVMWTO. WHEN AN ABEND            00335000
*        CONDITION IS FOUND THE MODULE SETS XVD1PERR AND RETURNS        00336000
*        NORMALLY TO IEAVVWTO. IEAVVWTO WILL THEN ABEND THE             00337000
*        CALLER WITH A D23 ABEND CODE                                   00338000
*                                                                       00339000
*        CONDITIONS -                                                   00340000
*        THE ERROR BIT, XVD1PERR, IS SET FOR THE FOLLOWING              00341000
*        REASONS -                                                      00342000
*         1. NO SPACE COULD BE OBTAINED IN SUBPOOL 229 FOR THE          00343000
*            WORKAREA FOR IEAVMWTO                                      00344000
*         2. THE ESTAE EXIT WAS ENTERED                                 00345000
*         3. SPACE COULD NOT BE OBTAINED FOR A WQE FROM THE WQE         00346000
*            CELL POOL                                                  00347000
*                                                                       00348000
*        RETURN CODES - NONE                                            00349000
*                                                                       00350000
*        EXTERNAL REFERENCES - SEE THE FOLLOWING                        00351000
*                                                                       00352000
*        ROUTINES -                                                     00353000
*        THIS MODULE CALLS SUBSYSTEM WTO EXIT VIA THE IEFSSREQ          00354000
*        MACRO                                                          00355000
*                                                                       00356000
*        DATA AREAS - THE FOLLOWING EXTERNAL DATA AREAS ARE USED        00357000
*        OR REFERENCED BY THIS MODULE                                   00358000
*        ASCBASID = MEMORY ID                                           00359000
*        CVTCRMN  = ADDR OF GETMAIN BRANCH ENTRY                        00360000
*        CVTBLDCP = ADDR OF BLD CPOOL ENTRY POINT                       00361000
*        CVTFRECL/CVTGETCL = ADDR OF GET/FREE CELL ROUTINE              00362000
*        CVTRMBR  = ADDR OF FREEMAIN ENTRY POINT                        00363000
*        CVTVWAIT = ADDR OF BRANCH ENTRY TO WAIT                        00364000
*        PSALITA  = ADDR OF LOCK INTERFACE TABLE                        00365000
*        TCBFLGS1 = BIT TCBFX IS SET TO PREVENT ASYNCHRONOUS EVENTS     00366000
*                   DURING A WAIT FOR A MINOR WQE                       00367000
*        UCMASCB  = ADDR OF ASCB FOR COMMTASK'S MEMORY                  00368000
*        UCMCMID  = MESSAGE SEQUENCE NUMBER                             00369000
*        UCMOECB  = OUTPUT ECB FOR COMM TASK                            00370000
*        UCMSYST  = THIS BIT IS SET IF MLWTO WAS DELETED BY THE         00371000
*                   SUBSYSTEM                                           00372000
*        UCMWQECP = WQE CELL POOL NUMBER                                00373000
*        UCMWQEND = ADDR OF LAST WQE ON THE CHAIN                       00374000
*        UCMWQLM  = MAX NUMBER OF WQES ALLOWED                          00375000
*        UCMWQNR  = CURRENT NUMBER OF WQES                              00376000
*        UCMWTOQ  = ADDR OF START OF WQE CHAIN                          00377000
*                                                                       00378000
*        TABLES -                                                       00379000
*        THE FOLLOWING TABLES ARE USED IN THIS ROUTINE -                00380000
*        TRTAB -  TABLE OF CHARACTERS USED TO INSURE PRINTABLE          00381000
*                 AND NON DISPLAY CONTROL CHARACTERS IN THE WQE         00382000
*                 THIS TABLE IS RESIDENT IN IEEVVWTO                    00383000
*                                                                       00384000
*        SERIALIZATION -                                                00385000
*        THE LOCAL AND CMS LOCKS ARE USED TO SERIALIZE THE              00386000
*        RESOURCES USED IN THIS MODULE                                  00387000
*                                                                       00388000
IEAVMLWO SAVE  (14,12),,'IEAVMWTO ZP60039 &SYSDATE &SYSTIME'            00389000
*                                                                       00390000
         LA    R10,0(,R15)             R15 HAS ENTRY POINT ADDR         00391000
         LA    R11,2048(,R10)                                           00392000
         LA    R11,2048(,R11)                                           00393000
         USING IEAVMLWO,R10,R11                                         00394000
         USING RBBASIC,R12                                              00395000
         LR    R12,R5                  R12 -> SVRB                      00396000
         XC    XVX,XVX                 ZERO OUT THE XVX FLAGS           00397000
         XC    XVWWB,XVWWB             ZERO OUT XVWWB FOR ADDR OF WWB   00398000
*                                                                       00399000
*                                                                       00400000
*        GETMAIN FOR SAVE AREA AND WORKAREA FROM SUBPOOL 229            00401000
*                                                                       00402000
         L     R0,WKSIZE               GET SIZE OF WORK AREA            00403000
         LR    R9,R0                   SAVE LENGTH FOR LATER AREA ZERO  00404000
         L     R1,WKSUBPL              GET SUBPOOL NUMBER OF WORKAREA   00405000
*                                                                       00406000
         GETMAIN  RC,LV=(0),SP=(1)                                      00407000
*                                                                       00408000
*        CHECK IF GETMAIN WAS SUCCESSFUL                                00409000
*        IF NOT SET ERROR AND STOP BITS                                 00410000
*                                                                       00411000
         LTR   R15,R15                 CHECK GETMAIN RETURN CODE        00412000
         BZ    IEAVSAVE                CONTINUE IF ZERO RETURN CODE     00413000
*                                                                       00414000
*        GETMAIN FAILED                                                 00415000
*                                                                       00416000
         MVI   XVAMOD,MWTOID                                            00417000
         MVI   XVFNCT,D23DYN           DYNAMIC AREA GETMAIN FAILURE     00418000
         STC   R15,XVREASON                                             00419000
         OI    XVD1,XVD1PERR           SEVERE ERROR                     00420000
         OI    XVX1,XVX1STOP           ERROR, IGNORE MLWTO              00421000
         B     IEAMGRET                BRANCH TO RETURN TO USER         00422000
*                                                                       00423000
IEAVSAVE LR    R8,R1                   R8 -> GETMAINED AREA             00424000
*                                      R9  = L'GETMAINED AREA           00425000
         SLR   R15,R15                 NO COPY, PAD OF ZERO             00426000
         MVCL  R8,R14                  ZERO WORKAREA                    00427000
         ST    R1,8(,R13)              CHAIN SAVE AREAS                 00428000
         ST    R13,4(,R1)                                               00429000
         LR    R9,R13                  R9 -> IEAVVWTO WORKAREA          00430000
         USING WORKVV,R9               ESTABLISH ADDRESSABILITY         00431000
*                                      TO IEAVVWTO WORKAREA             00432000
         LR    R13,R1                                                   00433000
         USING WORKAREA,R13            SET ADDRESSABILITY FOR WORKAREA  00434000
         L     R3,CVTPTR                                                00435000
         USING CVT,R3                                                   00436000
         MVC   ADDRUCM,CVTCUCB         SAVE -> UCM IN ADDRUCM           00437000
         DROP  R3                                                       00438000
*                                                                       00439000
*        SAVE USER'S ASCB ADDR (R7) IN OUR SAVEAREA                     00440000
*        SAVE THE ADDR TO THE CURRENT RB                                00441000
*                                                                       00442000
         ST    R7,ASCBSAVE             SAVE ASCB ADDR                   00443000
         ST    R4,TCBSAVE              SAVE TCB ADDR                    00444000
*                                                                       00445000
*        CALCULATE ADDR OF LAST BYTE OF TEXT IN PROTECTED STORAGE       00446000
*                                                                       00447000
         L     R1,WKAADTXT             R1 -> MAJOR WQE TEXT IN          00448000
*                                            PROTECTED STORAGE          00449000
         AH    R1,WKALGH               ADD L'MAJOR WQE TEXT             00450000
         A     R1,LENMWQE              ADD L'OF ALL MINOR WQES          00451000
         ST    R1,LASTWPLB             STORE ADDR OF BYTE AFTER END OF  00452000
*                                      MAJOR AND MINOR WQES             00453000
*                                                                       00454000
*        CHECK FOR PP USER ISSUING MLWTO                                00455000
*        PP USERS CAN'T CONNECT MINOR LINES TO THE MAJOR WQE            00456000
*        CHECK XVD1PP AND XVD1AUTH TO INSURE                            00457000
*        R0 ISN'T AN IMPLIED PARAMETER TO SVC 35                        00458000
*                                                                       00459000
IEAVMLWS TM    XVD1,XVD1PP             USER A PROBLEM PROGRAM ?         00460000
         BZ    IEAVTCON                NO, GO CHECK FOR MSG ID          00461000
         TM    XVD1,XVD1AUTH           KEY 0, SUPVR STATE OR APF AUTH ? 00462000
         BO    IEAVTCON                YES, BRANCH                      00463000
*                                                                       00464000
*        PROBLEM PROGRAM, NO CONNECTION ALLOWED                         00465000
*                                                                       00466000
         XC    XVWQEIDA,XVWQEIDA       ZERO MESSAGE ID                  00467000
         B     IEAVSETE                BYPASS MSG CONNECTING PROCESS    00468000
*                                                                       00469000
*        NOT PROBLEM PROGRAM                                            00470000
*        INDICATE PRIVILEGED TASK TO AVOID MLWTO HANGUP BECAUSE         00471000
*        OF UNAVAILABLE WQE'S                                           00472000
*        INDICATE CONNECTING IF REQUESTED                               00473000
*                                                                       00474000
IEAVTCON OI    XVD1,XVD1PRIV           INDICATE PRIVILEGED TASK         00475000
         ICM   R15,B'1110',XVWQEIDA    CALLER CONNECTING ?              00476000
         BZ    IEAVSETE                NO, THE MSG ID FIELD IS EMPTY    00477000
         OI    XVD2,XVD2CON            YES, INDICATE CONNECTING         00478000
*                                                                       00479000
*        SET UP THE ESTAE PROTECTION FOR THIS MODULE                    00480000
*                                                                       00481000
IEAVSETE XC    EPARM(L'EPARM),EPARM    CLEAR THE ESTAE PARM LIST        00482000
         L     R3,ADDRUCM              R3 -> UCM                        00483000
         USING UCM,R3                                                   00484000
         L     R2,UCMFRRAD             R2 -> COM TASKS RECOVERY         00485000
*                                            ROUTINE IEEFRRAD           00486000
         LA    R8,EPARM                R8 -> ESTAE PARM AREA            00487000
         MVC   SUBSLIST(ELISTL),ELIST  MOVE IN ESTAE PARM LIST          00488000
         LA    R1,SUBSLIST             R1 -> PARAMETER LIST             00489000
         DROP  R3                                                       00490000
*                                                                       00491000
         ESTAE (R2),CT,PARAM=(R8),RECORD=YES,MF=(E,(1))                 00492000
*                                                                       00493000
         LA    R2,EPARM                R2 -> PARMLIST AREA              00494000
         ST    R2,PARMPTR              PT TO PARM AREA                  00495000
         USING PARMLIST,R2                                              00496000
         LA    R1,IEAVRETY             MOVE IN RETRY ADDR               00497000
         ST    R1,PARMRTAD                                              00498000
         LA    R1,RECSAVE              R1 -> REG SAVE AREA              00499000
         ST    R1,PARMRGAD             SET ESTAE PARM LIST PTR TO SA    00500000
         MVC   PARMID,MODULEID         IDENTIFY FAILING MODULE          00501000
         MVC   RECSAVE,ALLREGS         RELOAD ALL REGS ON RETRY         00502000
         STM   R0,R15,RECREGS          SAVE REGS FOR A RETRY            00503000
         DROP  R2                                                       00504000
*                                                                       00505000
*        CHECK IF MLWTO IS QUEUED TO HARDCOPY ONLY                      00506000
*        THIS IS AN ERROR CONDITION WITH A RETURN CODE OF 20(DEC)       00507000
*                                                                       00508000
         TM    WKAMCSF,WPLMCSFG        QUEUE TO HARD COPY ONLY ?        00509000
         BZ    IEASRTDC                NO, CHECK FOR ROUT/DESC CODES    00510000
         MVI   XVRETCOD,HCONLY         YES, SET RETURN CODE 20          00511000
         BAL   R0,IEASTOPA             IGNORE REQUEST                   00512000
*                                                                       00513000
IEASRTDC TM    WKAROC+1,WPLROUTK       WTP SPECIFIED ?                  00514000
         BZ    IEASGETL                NOT WTP, SKIP ERROR CHECK        00515000
*                                                                       00516000
*        THIS MLWTO INCLUDES A WTP ROUTE CODE                           00517000
*        CHECK IF IT IS A WTP ONLY                                      00518000
*                                                                       00519000
         CLC   WKAROC,WTPONLY          ANY ROUTE CODES OTHER THAN       00520000
*                                      WPLROUTK ?                       00521000
         BNE   IEASWTPP                YES, WTP NOT THE ONLY ROUTE CODE 00522000
*                                      SPECIFIED                        00523000
*        ONLY WTP ROUTE CODE. SET STOP FLAG                             00524000
*                                                                       00525000
         OI    XVX1,XVX1STOP                                            00526000
         B     IEASRETC                                                 00527000
*                                                                       00528000
IEASWTPP NI    WKAROC+1,255-WPLROUTK   TURN OFF WTP ROUTING CODE        00529000
IEASRETC MVI   XVRETCOD,RCWTP          SET RETURN CODE 16               00530000
         TM    XVX1,XVX1STOP           STOP PROCESSING SET ?            00531000
         BZ    IEASGETL                NO, CONTINUE PROCESSING          00532000
         BAL   R0,IEASTOPA             YES, THEN SKIP TO STOP           00533000
*                                                                       00534000
*        GET THE NUMBER OF LINES TO BE PROCESSED                        00535000
*                                                                       00536000
*        INPUT -                                                        00537000
*        WKAADTXT -> TEXT FOR MAJOR WQE AND MINOR WQES                  00538000
*                                                                       00539000
*        OUTPUT -                                                       00540000
*        XVX3 = NUMBER OF LINES FROM WPLLINES FIELD                     00541000
*        R2 -> WPLLTF (MLWTO EXTENSION HDR) AND ADDRESSABILITY IS SET   00542000
*        ADDRMLHR IS SET TO ADDR OF WPLLTF                              00543000
*                                                                       00544000
IEASGETL L     R2,WKAADTXT             R2 -> MAJOR WQE AND MINOR WQES   00545000
         AH    R2,WKALGH               ADD L'MAJOR WQE                  00546000
         ST    R2,ADDRMLHR             SAVE ADDR TO MULTILINE EXTENSION 00547000
*                                      HEADER WPLLTF                    00548000
         USING WPLRF,R2                R2 -> MLWTO EXTENSION WPLLTF     00549000
*                                                                       00550000
         TM    XVX1,XVX1STOP           ERROR IN GETLINES ?              00551000
         BZ    IEASGETC                NO, CONTINUE  PROCESSING         00552000
         BAL   R0,IEASTOPA             YES, THEN SKIP PROCESSING        00553000
*                                                                       00554000
*        CHECK THE TEXT LENGTH OF THE MAJOR WQE FOR ZERO                00555000
*                                                                       00556000
IEASGETC CLC   WKALGH,KH4              LENGTH > 4 ?                     00557000
         BH    IEASTLMK                YES, GO CHECK NO OF LINES        00558000
         BL    IEA00212                < 4, ERROR, BRANCH               00559000
*                                      LENGTH = 4                       00560000
*                                                                       00561000
*        VALIDATE MLWTO EXTENSION HEADER FLAGS                          00562000
*                                                                       00563000
         TM    WPLLTF,WPLLTFD          END LINE ?                       00564000
         BZ    IEA00212                NO, BRANCH                       00565000
         TM    WPLLTF,WPLLTFC          YES, END PLUS DATA LINE ?        00566000
         BZ    IEASTLMK                NO, BRANCH                       00567000
IEA00212 MVI   XVRETCOD,LINERR         ZERO LINE LENGTH - ERROR 04      00568000
         BAL   R0,IEASTOPA             RETURN TO CALLER                 00569000
*                                                                       00570000
IEASTLMK CLI   WPLLINES,0              ANY LINES TO PROCESS ?           00571000
         BNE   IEASTLMX                YES, CONTINUE CHECKING WPL       00572000
         MVI   XVRETCOD,LINERR         NO, ZERO NO OF LINES, ERROR      00573000
         BAL   R0,IEASTOPA             RETURN TO CALLER                 00574000
*                                                                       00575000
*        VALIDATE THE NUMBER OF LINES                                   00576000
*        ALLOW NO MORE THAN 10 LINES TO BE PROCESSED                    00577000
*        UNLESS THE CALLER IS AUTHORIZED                                00578000
*                                                                       00579000
IEASTLMX MVC   XVX3,WPLLINES           SAVE THE NUMBER OF LINES         00580000
         CLI   XVX3,10                 NO LINES > 10 ?                  00581000
         BNH   IEASETC1                NO, VALID                        00582000
         TM    XVD1,XVD1AUTH           KEY 0, SUPVR STATE OR APF AUTH ? 00583000
         BO    IEASETC1                YES, BRANCH                      00584000
         TM    XVD1,XVD1PP             PROBLEM PROGRAM CALLER ?         00585000
         BZ    IEASETC1                NO, BRANCH                       00586000
         MVI   XVX3,10                 YES, TRUNCATE NO LINES TO 10 MAX 00587000
         MVI   XVRETCOD,LINERR         SET RETURN CODE = 4 BUT          00588000
*                                      CONTINUE PROCESSING              00589000
*                                                                       00590000
*        BEGIN PROCESSING THE MAJOR WQE LINE                            00591000
*                                                                       00592000
*        R2 -> WPLLTF                                                   00593000
*        R14 = LINE COUNT                                               00594000
*                                                                       00595000
IEASETC1 LA    R14,1                   SET COUNT=1                      00596000
         MVI   NUMLINES,1              SET LINE COUNT TO 1              00597000
         MVC   LINETYPE,WPLLTF         MOVE LINE CONTROL FLAGS FROM     00598000
*                                      MLWTO EXTENSION HEADER TO        00599000
*                                      LINETYPE                         00600000
         TM    LINETYPE,WPLLTFD        END LINE ?                       00601000
         BZ    IEASTCLN                NO, GO CHECK FOR CONTROL LINE    00602000
         TM    LINETYPE,WPLLTFC        YES, DATA LINE AS WELL ?         00603000
         BO    IEASTCLN                YES, START LINE TYPE CHECKING    00604000
*                                                                       00605000
*        THE FIRST LINE CAN JUST BE AN END LINE                         00606000
*        SET FLAG INDICATING END LINE                                   00607000
*        ONLY VALID IF CONNECTING                                       00608000
*                                                                       00609000
         OI    XVX0,XVX0FLJE           SET LINE 1 JUST END              00610000
         TM    XVD2,XVD2CON            CONNECTING                       00611000
         BO    IEASTORE                YES, SKIP TO STORE LINE COUNT    00612000
         MVI   XVRETCOD,INVLDLT        NO, SET INVALID LINE TYPE RC     00613000
         BAL   R0,IEASTOPA                                              00614000
*                                                                       00615000
*        CHKTYPE SEGMENT                                                00616000
*                                                                       00617000
*        ALL LINES ARE PROCESSED THROUGH THIS SECTION OF CODE           00618000
*                                                                       00619000
*        THE LINE CONTROL FLAGS ARE IN A DIFFERENT OFFSET FOR THE       00620000
*        MAJOR WQE COMPARED TO THE MINOR WQE LINES.                     00621000
*        FOR CODE SIMPLIFICATION THE LINE CONTROL FLAGS FROM THE TWO    00622000
*        DIFFERENT LOCATIONS ARE MOVED TO LINETYPE FOR COMMON TESTING   00623000
*                                                                       00624000
IEASTCLN TM    LINETYPE,WPLLTFA        CONTROL LINE ?                   00625000
         BO    IEASCNT0                YES, CHECK COUNT                 00626000
         TM    LINETYPE,WPLLTFB        LABEL LINE ?                     00627000
         BO    IEASTLAB                YES, BRANCH                      00628000
         TM    LINETYPE,WPLLTFC        DATA LINE ?                      00629000
         BO    IEASUPCT                YES, BRANCH                      00630000
         TM    LINETYPE,WPLLTFD        END LINE ONLY ?                  00631000
         BO    IEASFEND                YES, GOTO FORCE END              00632000
         B     IEASRC12                NO, ERROR                        00633000
*                                                                       00634000
*        PROCESS CONTROL LINE                                           00635000
*        CONTROL LINE MUST BE THE FIRST LINE                            00636000
*                                                                       00637000
IEASCNT0 CH    R14,KH1                 COUNT=1 (MAJOR WQE LINE) ?       00638000
         BNE   IEASRC12                NO, ERROR                        00639000
         TM    LINETYPE,WPLLTFB+WPLLTFC+WPLLTFD CONTROL LINE ONLY ?     00640000
         BNZ   IEASRC12                NO, ERROR AS OTHER LINE TYPES ON 00641000
         TM    XVD2,XVD2CON            CONNECTING ?                     00642000
         BO    IEASRC12                YES, ERROR                       00643000
         OI    XVX0,XVX0FLCL           SET FIRST LINE CONTROL LINE      00644000
         B     IEASTEST                                                 00645000
*                                                                       00646000
*        PROCESS LABEL LINE                                             00647000
*        LABEL FLAG MUST BE THE ONLY FLAG FOR THE LINE                  00648000
*                                                                       00649000
IEASTLAB TM    LINETYPE,WPLLTFA+WPLLTFC+WPLLTFD LABEL LINE ONLY ?       00650000
         BNZ   IEASRC12                NO, OTHER FLAGS ON, ERROR        00651000
         TM    XVX0,XVX0LL2F           LABEL LINE 2 BIT ON ?            00652000
         BO    IEASRC12                YES, DUPLICATE LABEL, ERROR      00653000
         TM    XVX0,XVX0LL1F           LABEL LINE 1 BIT ON ?            00654000
         BO    IEAS1LAB                YES, BRANCH                      00655000
         OI    XVX0,XVX0LL1F           SET LABEL LINE 1 BIT ON          00656000
         TM    XVX0,XVX0FLCL           CONTROL LINE PREVIOUSLY FOUND ?  00657000
         BO    IEASCLF1                YES, BRANCH                      00658000
         CH    R14,KH1                 NO, COUNT = 1 ?                  00659000
         BE    IEASTEST                YES, BRANCH                      00660000
         B     IEASRC12                LABEL LINE INCORRECTLY PLACED    00661000
*                                                                       00662000
IEASCLF1 CH    R14,KH2                 COUNT = 2 ?                      00663000
         BE    IEASTEST                                                 00664000
         B     IEASRC12                NO, ERROR                        00665000
*                                                                       00666000
*        PROCESS SECOND LABEL LINE                                      00667000
*                                                                       00668000
IEAS1LAB TM    XVX0,XVX0FLCL           FIRST LINE CONTROL LINE ?        00669000
         BZ    IEASTC2                 NO, BRANCH                       00670000
         CH    R14,KH3                 COUNT = 3 ?                      00671000
         BNE   IEASRC12                NO, ERROR                        00672000
         B     IEASETL2                                                 00673000
*                                                                       00674000
IEASTC2  CH    R14,KH2                 COUNT = 2 ?                      00675000
         BNE   IEASRC12                NO, ERROR                        00676000
IEASETL2 OI    XVX0,XVX0LL2F           SET LABEL LINE 2 BIT ON          00677000
         B     IEASTEST                                                 00678000
*                                                                       00679000
*        PROCESS DATA LINE                                              00680000
*                                                                       00681000
IEASUPCT TM    LINETYPE,WPLLTFA+WPLLTFB+WPLLTFD  JUST DATA LINE ?       00682000
         BZ    IEASTEST                YES, BRANCH                      00683000
         TM    LINETYPE,WPLLTFA+WPLLTFB  OTHER FLAGS ON ?               00684000
         BNZ   IEASRC12                YES, ERROR                       00685000
         OI    XVX0,XVX0FEDE           SET FORCED END FLAG              00686000
         B     IEASTEST                CHECK FOR END OF LOOP            00687000
*                                                                       00688000
*        INVALID LINE TYPE FLAGS DETECTED                               00689000
*                                                                       00690000
IEASRC12 MVI   XVRETCOD,INVLDLT        SET RETURN CODE TO INDICATE      00691000
*                                      BAD LINETYP                      00692000
*                                                                       00693000
*        PROCESS END LINE                                               00694000
*                                                                       00695000
IEASFEND OI    XVX0,XVX0FEDE           SET FORCE END FLAG               00696000
*                                                                       00697000
*        CHECK IF ALL DONE PROCESSING THIS MLWTO                        00698000
*        CHECK FOR AN END FOUND OR FORCED OR FOR THE LINE               00699000
*        COUNT MET. IF ALL DONE THEN STORE THE LINE COUNT               00700000
*                                                                       00701000
IEASTEST TM    XVX0,XVX0FLJE+XVX0FEDE  END FOUND OR FORCED ?            00702000
         BNZ   IEASTORE                YES, WRAP UP WPL PROCESSING      00703000
         CLM   R14,B'0001',XVX3        ALL MINOR WQE LINES PROCESSED ?  00704000
         BE    IEASTORE                YES                              00705000
*                                      NO, FALL THROUGH TO ADVANCE      00706000
*                                          TO NEXT LINE IN WPL          00707000
*                                                                       00708000
*        INCRMNT                                                        00709000
*                                                                       00710000
*        STEP TO THE NEXT MINOR WQE LINE IN THE WPL                     00711000
*        INSURE THAT THE MINOR WQE TEXT IS CONTAINED WITHIN             00712000
*        THE TEXT STORAGE AREA PASSED BY IEAVVWTO                       00713000
*                                                                       00714000
         LA    R15,4                   SET L'MLWTO EXTENSION HEADER     00715000
         CH    R14,KH1                 FIRST LINE ?                     00716000
         BE    INCRMNTA                YES, USE CURRENT VALUE IN R15    00717000
         LH    R15,WPLML0              GET CURRENT L'LINE               00718000
INCRMNTA LA    R14,1(,R14)             INCR LINE COUNT                  00719000
         AR    R2,R15                  INCR TO REFERENCE NEXT MINOR     00720000
*                                      CONTROL FIELD                    00721000
         LR    R1,R2                   R1 -> NEXT MINOR WQE             00722000
         AH    R1,WPLML0               ADD L'NEXT MINOR WQE             00723000
         C     R1,LASTWPLB             THIS NEXT MINOR WQE ENTIRELY     00724000
*                                      CONTAINED IN THE GETMAINED AREA? 00725000
         BNH   IEASINCC                WITHIN WPL, BRANCH               00726000
*                                      OUTSIDE WPL, ERROR               00727000
         MVI   XVAMOD,MWTOID           IEAVMWTO'S ID FOR D23            00728000
         MVI   XVFNCT,D23VALID         PARMLIST VALIDITY CHECK          00729000
         MVI   XVREASON,D23SIZE        CALLER MODIFIED WPL              00730000
         OI    XVD1,XVD1PERR           ABEND USER                       00731000
         BAL   R0,IEASTOPA             SKIP TO END OF TSTMLWTO          00732000
*                                                                       00733000
IEASINCC MVC   LINETYPE,WPLMLLTF       MOVE LINE TYPE FLAGS FROM        00734000
*                                      MLWTO LINE ENTRY TO LINETYPE     00735000
         TM    LINETYPE,WPLLTFD        END LINE ?                       00736000
         BZ    IEASCKLL                NO, CHECK LINE LENGTH            00737000
         TM    LINETYPE,WPLLTFC        DATA END LINE ?                  00738000
         BO    IEASCKLL                YES, CHECK LINE LENGTH           00739000
         B     IEASTORE                CHECK IF ALL DONE                00740000
*                                                                       00741000
IEASCKLL CLC   WPLML0(2),KH4           LINE LENGTH > 4 ?                00742000
         BH    IEASTCLN                YES, GO CHECK LINE TYPE FLAGS    00743000
         BCTR  R14,0                   DO NOT PUT OUT BAD LINE          00744000
         MVI   XVRETCOD,LINERR         SET RETURN CODE TO ERROR         00745000
*                                      IN THE NO OF LINES               00746000
         OI    XVX0,XVX0FEDE           FORCE END TO MLWTO               00747000
*                                                                       00748000
*        STORE THE COUNT AND SET UP FOR CREATING THE FIRST LINE         00749000
*                                                                       00750000
*        INPUT -                                                        00751000
*        R14 = NUMBER OF LINES TO PROCESS                               00752000
*                                                                       00753000
*        OUTPUT -                                                       00754000
*        COUNT IS STORED IN XVXD0 AND XVX2                              00755000
*        XVD3TXT1 IS SET TO INDICATE THE FIRST LINE IS BEING PROCESSED  00756000
*        XVX0UDCL IS SET IF THE DEFAULT CONTROL LINE IS TO BE USED      00757000
*                                                                       00758000
*                                                                       00759000
IEASTORE STC   R14,XVX3                UPDATE ACTUAL LINE COUNT         00760000
         STC   R14,XVX2                SET UP NO OF LINES TO DO         00761000
         OI    XVD3,XVD3TXT1           INDICATE FIRST LINE PROCESSING   00762000
         TM    XVD1,XVD1PP             PROBLEM PROGRAM ?                00763000
         BZ    IEASSSET                NO, BRANCH                       00764000
         TM    XVD1,XVD1AUTH           KEY 0, SUPVR STATE OR APF AUTH ? 00765000
         BO    IEASSSET                YES, BRANCH                      00766000
         OI    XVX0,XVX0FEDE           NO, SET FORCE END                00767000
*                                                                       00768000
*        DETERMINE IF THE DEFAULT LINE HAS TO BE USED                   00769000
*                                                                       00770000
IEASSSET TM    XVX0,XVX0FLCL           FIRST LINE CONTROL LINE          00771000
         BO    IEASEXIT                YES, NOT GOING TO USE DEFAULT    00772000
         TM    XVD2,XVD2CON            CONNECTING                       00773000
         BO    IEASEXIT                YES, SKIP                        00774000
         TM    WKADSC+1,WPLDESCI       DESC CODE 9 OPERATORS REQUEST ?  00775000
         BZ    IEASEXIT                NO, BRANCH                       00776000
         OI    XVX0,XVX0UDCL           YES, USE DEFAULT CONTROL LINE    00777000
*                                                                       00778000
         DROP  R2                      WPL ADDRESSABILITY DROPPED       00779000
*                                                                       00780000
*        END OF LINE VALIDATION CODE                                    00781000
*                                                                       00782000
*        SET THE LOCAL AND CMS LOCKS                                    00783000
*                                                                       00784000
IEASEXIT L     R1,PARMPTR              R1 -> ESTAE PARMAREA             00785000
         USING PARMLIST,R1                                              00786000
         XC    PARMRTAD,PARMRTAD       CLEAR RETRY ADDRESS              00787000
         DROP  R1                                                       00788000
         BAL   R15,SETLCKS             CALL THE SET LOCK ROUTINE        00789000
         STM   R0,R15,RECREGS          SAVE REGS FOR FRR RECOVERY       00790000
         XC    XVCMAJOR,XVCMAJOR       ZERO MAJOR WQE ADDR              00791000
         TM    XVD2,XVD2CON            CONNECTING MINOR LINES  ?        00792000
         BO    IEAMBMIN                YES, GO BUILD THE MINOR          00793000
*                                      NO, BUILDING THE MAJOR WQE       00794000
*                                                                       00795000
*        BLDMAJ                                                         00796000
*                                                                       00797000
*        THIS SEGMENT CREATES AND BUILDS THE MAJOR WQE OR FIRST         00798000
*        LINE OF THE MLWTO                                              00799000
*                                                                       00800000
*        INPUT -                                                        00801000
*        THE LOCAL AND CMS LOCKS ARE HELD                               00802000
*        THE WPL HAS BEEN CHECKED FOR PROPER LINE TYPES                 00803000
*                                                                       00804000
*        OUTPUT -                                                       00805000
*        A MAJOR WQE IS CONSTRUCTED AND PUT ON THE WQE CHAIN. THE       00806000
*        MAJOR HAS AN EMPTY MINOR CHAINED TO IT                         00807000
*                                                                       00808000
*        THIS SEGMENT GETS A MAJOR WQE                                  00809000
*        INPUT -                                                        00810000
*        UNCMWQNR, UCMWQLM, XVD1PRIV, XVD2CON                           00811000
*        OUTPUT -                                                       00812000
*        R1 PTS AT THE MAJOR WQE IF ONE WAS AVAILABLE                   00813000
*        R1 IS ZERO IF ONE WASN'T AVAILABLE                             00814000
*        THE WQE IS ZEROED OUT                                          00815000
*                                                                       00816000
*        CHECK IF TWO WQES ARE AVAILABLE                                00817000
*                                                                       00818000
IEAJGET0 L     R3,ADDRUCM              R3 -> UCM                        00819000
         USING UCM,R3                                                   00820000
         LH    R2,UCMWQNR              NUMBER OF WQES USED              00821000
         LA    R2,2(,R2)               TWO WQES NEEDED                  00822000
         CH    R2,UCMWQLM              COMPARE WITH LIMIT ON WQES       00823000
         BNH   IEAJGET1                WQES AVAIL, DON'T WAIT           00824000
         DROP  R3                                                       00825000
*                                                                       00826000
*        TWO WQES AREN'T AVAILABLE, CHECK IF USER IS PRIVILEGED         00827000
*                                                                       00828000
         TM    XVD1,XVD1PRIV           CALLER PRIVILEGED ?              00829000
         BO    IEAJGET1                YES, GET WQES                    00830000
*                                                                       00831000
*        USER ISN'T PRIVILEGED. WAIT FOR WQES TO BE FREED               00832000
*                                                                       00833000
         BAL   R14,WAITWQE             WAIT FOR A WQE                   00834000
         B     IEAJGET0                CHECK IF TWO WQES ARE AVAIL      00835000
*                                                                       00836000
IEAJGET1 BAL   R14,GETWQE              GET AND ZERO A WQE               00837000
*                                                                       00838000
*        CHECK IF WQE WAS AVAILABLE                                     00839000
*                                                                       00840000
         LTR   R1,R1                   ADDR RETURNED ?                  00841000
         BNZ   IEAJGET2                YES, STORE ADDR OF MAJOR         00842000
*                                                                       00843000
*        WQE WASN'T AVAILABLE. SET ERROR AND STOP FLAGS                 00844000
*                                                                       00845000
         OI    XVX1,XVX1STOP           STOP PROCESSING WPL              00846000
         OI    XVD1,XVD1PERR           ABEND USER                       00847000
         B     IEAJGET3                SKIP TO CHECK IF WWB ALLOCATED   00848000
*                                                                       00849000
*        SET UP MAJOR FOR PROCESSING                                    00850000
*                                                                       00851000
IEAJGET2 ST    R1,XVCMAJOR             SAVE ADDR OF MAJOR               00852000
         OI    XVD3,XVD3BLDJ           SET BUILD MAJOR FLAG             00853000
*                                                                       00854000
*        CHECK IF A WWB HAS BEEN OBTAINED, IF SO, THEN FREE IT          00855000
*                                                                       00856000
IEAJGET3 ICM   R2,B'1111',XVWWB        WWB ADDR ZERO ?                  00857000
         BZ    IEAJGET4                YES, SKIP FREEING WWB            00858000
*                                                                       00859000
*        FREE THE WWB POINTED AT BY XVWWB                               00860000
*                                                                       00861000
         USING WWB,R2                                                   00862000
         L     R1,WWBFWDPT             R1 -> NEXT WWB FORWARD ON CHAIN  00863000
*                                                                       00864000
*        CONNECT FORWARD WWB TO BACK WWB                                00865000
*                                                                       00866000
         MVC   WWBBCKPT-WWB(4,R1),WWBBCKPT                              00867000
         L     R1,WWBBCKPT                                              00868000
*                                                                       00869000
*        CONNECT BACK WWB TO FORWARD WWB                                00870000
*                                                                       00871000
         MVC   WWBFWDPT-WWB(4,R1),WWBFWDPT                              00872000
         DROP  R2                                                       00873000
*                                                                       00874000
*        OUR WWB IS NOW OUT OF THE CHAIN. FREE IT                       00875000
*                                                                       00876000
         L     R0,WKGETWWB             SET SUBPOOL NO AND SIZE          00877000
         LR    R1,R2                   ADDR OF WWB                      00878000
*                                                                       00879000
         FREEMAIN R,LV=(0),A=(1),BRANCH=YES  FREE THE WWB               00880000
*                                                                       00881000
         XC    XVWWB,XVWWB             ZERO WWB ADDR                    00882000
*                                                                       00883000
*        CHECK IF MAJOR WAS OBTAINED                                    00884000
*                                                                       00885000
IEAJGET4 TM    XVX1,XVX1STOP           STOP PROCESSING SET?             00886000
         BO    IEAJOUT                 YES, SKIP TO END AS NO WQE       00887000
*                                                                       00888000
*        A MAJOR WQE WAS OBTAINED. GET A MINOR                          00889000
*                                                                       00890000
         BAL   R14,GETMINOR                                             00891000
*                                                                       00892000
*        CHECK IF A MINOR WAS OBTAINED                                  00893000
*                                                                       00894000
         TM    XVX1,XVX1STOP           STOP PROC FLAG SET               00895000
         BZ    IEAJBINT                NO, GO INIT THE MAJOR            00896000
*                                                                       00897000
*        A MINOR WASN'T AVAILABLE                                       00898000
*        FREE THE MAJOR WQE                                             00899000
*        THIS SEGMENT FREES THE MAJOR WQE OBTAINED BY BLDMAJ            00900000
*        INPUT XVCMAJOR CONTAINS THE WQE ADDR                           00901000
*                                                                       00902000
         L     R3,ADDRUCM              R3 -> UCM                        00903000
         USING UCM,R3                                                   00904000
         L     R0,UCMWQECP             GET WQE CELL POOL ID             00905000
         L     R1,XVCMAJOR             ADDR OF WQE CELL                 00906000
*                                                                       00907000
         FREECELL  CPID=(0),CELL=(1),BRANCH=YES                         00908000
*                                                                       00909000
*        CHECK IF THIS WQE WAS THE LAST ONE IN AN EXTENSION             00910000
*        IF SO FREE THE EXTENSION                                       00911000
*                                                                       00912000
         CH    R15,KH20                EXTENSION EMPTY ?                00913000
         BNE   IEAJFRE2                NO, SKIP FREEING EXTENSION       00914000
*                                      YES, THE PARAMETERS TO FREE THE  00915000
*                                      EXTENSION WERE SETUP BY FREECELL 00916000
         FREEMAIN  R,LV=(0),A=(1),BRANCH=YES                            00917000
*                                                                       00918000
IEAJFRE2 LH    R1,UCMWQNR              DECREMENT COUNT OF WQES          00919000
         BCTR  R1,0                                                     00920000
         STH   R1,UCMWQNR                                               00921000
         XC    XVCMAJOR,XVCMAJOR       INSURE MAJOR ADDR IS ZERO FOR    00922000
*                                      RECOVERY REASONS                 00923000
         DROP  R3                                                       00924000
         B     IEAJOUT                 GET OUT OF THE SEGMENT           00925000
*                                                                       00926000
*        BMAJINIT                                                       00927000
*                                                                       00928000
*        INITIALIZE THE MAJOR WQE SEGMENT                               00929000
*                                                                       00930000
*        INPUT -                                                        00931000
*        XVCMAJOR HAS ADDR OF MAJOR WQE                                 00932000
*        R4       -> TCB                                                00933000
*        ASCBSAVE -> ASCB                                               00934000
*        XVA8 CONTAINS THE TIME OF DAY                                  00935000
*        XVSAV HAS VARIOUS FLAGS SET TO SHOW CONDITION OF WPL           00936000
*                                                                       00937000
*        OUTPUT -                                                       00938000
*        INITIAL PART OF MAJOR WQE IS CONSTRUCTED AND WQE IS ON         00939000
*        THE QUEUE                                                      00940000
*        THE SUSPEND BIT IS SET ON                                      00941000
*        FIRST CHAR OF TEXT IS BLANK                                    00942000
*        TIME AND ROUTE CODES ARE DECODED AND FILLED IN                 00943000
*                                                                       00944000
IEAJBINT L     R8,XVCMAJOR             R8 -> MAJOR WQE                  00945000
         USING WMJMEXT,R8                                               00946000
         OI    WMJMMLW,WMJMMLWB        SET MAJOR FLAG                   00947000
         OI    WMJMBUF,WMJMBUFB+WMJMBUFD  SET IN USE AND GETMAINED      00948000
         USING TCB,R4                                                   00949000
         ST    R4,WMJMTCB              STORE TCB ADDR                   00950000
         MVC   WMJMJTCB,TCBJSTCB       STORE ADDR OF JOB STEP'S TCB     00951000
*                                      INTO MAJOR WQE                   00952000
         L     R7,ASCBSAVE             RESTORE ASCB ADDR                00953000
         USING ASCB,R7                                                  00954000
         MVC   WMJMASID,ASCBASID       MOVE IN ASID OF CALLER           00955000
         DROP  R7                                                       00956000
         L     R3,ADDRUCM              R3 -> UCM                        00957000
         USING UCM,R3                                                   00958000
         ICM   R2,B'1111',UCMWQEND     END OF OUTPUT QUEUE, ZERO ?      00959000
         BNZ   IEAJLNK                 NO, CHAIN TO LAST WQE            00960000
         LA    R2,UCMWTOQ              R2 -> OUTPUT QUEUE               00961000
IEAJLNK  ST    R8,UCMWQEND             PUT NEW WQE AT END OF QUEUE      00962000
         DROP  R8                                                       00963000
         USING WMJMEXT,R2                                               00964000
         MVC   WMJMNXT(3),UCMWQEND+1   PUT NEW WQE ON QUEUE             00965000
         DROP  R2                                                       00966000
         DROP  R3                                                       00967000
         USING WMJMEXT,R8                                               00968000
         MVC   WMJMCS,WKAMCSF          MOVE MCS FLAGS TO WQE            00969000
         TM    XVD1,XVD1AUTH           USER AUTHORIZED ?                00970000
         BZ    IEAJQ0CK                NO, CHECK ON USE OF QREG0        00971000
         OI    WMJMDSP,WMJMDSPH        YES, SET USER AUTHORIZED FLAG    00972000
         B     IEAJTIME                SKIP TO UNPACK THE TIME          00973000
*                                                                       00974000
IEAJQ0CK TM    WMJMCS1,WMJMCS1H        QREG0 BIT ON ?                   00975000
         BZ    IEAJTIME                NO, SKIP TO UNPACK               00976000
*                                                                       00977000
*        QREG0 FLAG IS ON                                               00978000
*        AN UNAUTHORIZED USER CAN'T USE THE QREG0 MCS FUNCTION          00979000
*        TURN THE FLAG OFF                                              00980000
*                                                                       00981000
         OI    WMJMCS1,WMJMCS1B        TURN ON REG0 BIT INSTEAD         00982000
         NI    WMJMCS1,255-WMJMCS1H    TURN OFF QREG0 FLAG              00983000
IEAJTIME MVC   WQEPAD(9),PATTIME       MOVE IN THE TIME EDIT PATTERN    00984000
         ED    WQEPAD(9),XVA8          FORMAT TIME HH.MM.SS INTO WQETS  00985000
         MVI   WMJMPAD1,C' '           BLANK CHAR AFTER TIME            00986000
*                     WQEPAD2=' ';  /* INSERT BLANK AFTER THE JOBNAME*/ 00987000
         MVI   WQEPAD2,C' '                                             00988000
*                     WQEJOBNM=' '; /* BLANK OUT THE JOB NAME FIELD  */ 00989000
         MVI   WQEJOBNM,C' '                                            00990000
         MVC   WQEJOBNM+1(L'WQEJOBNM-1),WQEJOBNM                        00991000
         MVC   WMJMRR,KC0000           INIT ROUTE CODES TO CHAR ZERO    00992000
         TM    WKAMCSF,WPLMCSFA        ROUT/DESC CODES PROVIDED ?       00993000
         BZ    IEAJTID1                NO, CHECK MSG TYPE               00994000
         MVC   WMJMRTC(2),WKAROC       MOVE ROUTING CODES INTO WQE      00995000
         MVC   WMJMDEC(2),WKADSC       MOVE DESCRIPTOR CODES INTO WQE   00996000
*                                                                       00997000
*        LOOP TO CONVERT ROUTING CODES TO CHARACTER FORMAT              00998000
*                                                                       00999000
         LR    R0,R4                   SAVE TCB ADDR                    01000000
         LA    R1,4                    SET LOOP COUNTER                 01001000
         L     R5,WMJMRTC              ROUTING CODES                    01002000
IEAJRCLP SR    R4,R4                   SET UP FOR SHIFT                 01003000
         SLL   R15,8                   MOVE ROUTING CODES TO R15        01004000
         SLDL  R4,4                                                     01005000
         IC    R15,HEXCHAR(R4)         CONVERT ROUTING CODES            01006000
         BCT   R1,IEAJRCLP             TO PRINTABLE FORM                01007000
         STCM  R15,B'1111',WMJMRR      STORE IN MAJOR WQE               01008000
         LR    R4,R0                   RESTORE TCB ADR                  01009000
*                                                                       01010000
*        END OF ROUTING CODE CONVERSION LOOP                            01011000
*                                                                       01012000
         MVI   WMJMPAD,C' '            BLANK AFTER ROUT CODES           01013000
         MVC   WMJMMT1,WKAMSGTY        MOVE MESSAGE TYPE TO WQE         01014000
IEAJTID1 TM    WKAMCSF,WPLMCSFB+WPLMCSFH  CONSOLE ID PASSED ?           01015000
*                                         QREG0 OR REG0                 01016000
         BZ    IEAJAREA                NO, GO TO AREA ID                01017000
         MVC   WMJMUID,XVCONID         YES, MOVE CONSOLE ID             01018000
IEAJAREA L     R2,ADDRMLHR             R2 -> MLWTO EXTENSION HEADER     01019000
         USING WPLLTF,R2                                                01020000
         MVC   WMJMAREA,WPLAREA        MOVE AREA ID INTO MAJOR          01021000
         TM    XVD1,XVD1AUTH           CALLER AUTHORIZED ?              01022000
         BO    IEAJMBLK                YES. DONT INSERT DEFAULT FOR P/P 01023000
*                                                                       01024000
*        NON AUTHORIZED USERS OF MLWTO GET AN AREA ID OF 'Z' OR ONLINE  01025000
*                                                                       01026000
*        THIS IS TO PREVENT AN UNAUTHORIZED CALLER FROM MESSING         01027000
*        WITH AN AREA BEING USED BY A DYNAMIC STATUS DISPLAY MLWTO      01028000
*                                                                       01029000
         MVI   WMJMAREA,C'Z'           AREA GETS SET TO INLINE (Z)      01030000
IEAJMBLK MVI   WMJMTXT,C' '            BLANK FIRST CHARACTER            01031000
         MVC   WORKBYTE,WMJMAREA                                        01032000
         NI    WORKBYTE,X'0F'                                           01033000
         TM    WMJMAREA,X'F0'                                           01034000
         BO    IEA00690                                                 01035000
         TM    WMJMAREA,X'E0'                                           01036000
         BO    IEA00668                                                 01037000
         TM    WMJMAREA,X'D0'                                           01038000
         BO    IEA00678                                                 01039000
         TM    WMJMAREA,X'C0'                                           01040000
         BO    IEA00678                                                 01041000
         B     IEA00690                                                 01042000
*                                                                       01043000
IEA00668 CLI   WORKBYTE,X'02'                                           01044000
         BL    IEA00690                                                 01045000
         BE    IEA00694                                                 01046000
         B     IEA00684                                                 01047000
*                                                                       01048000
IEA00678 CLI   WORKBYTE,X'01'                                           01049000
         BL    IEA00690                                                 01050000
         BE    IEA00694                                                 01051000
IEA00684 CLI   WORKBYTE,X'09'                                           01052000
         BH    IEA00690                                                 01053000
         B     IEA00694                                                 01054000
*                                                                       01055000
IEA00690 MVI   WMJMAREA,C'Z'                                            01056000
IEA00694 TM    XVD2,XVD2QFHC           QUEUE MESSAGE TO HARD COPY ?     01057000
         BZ    IEA006A0                NO, BRANCH                       01058000
         OI    WMJMDSP,WMJMDSPB        YES, QUEUE WQE TO HARDCOPY       01059000
IEA006A0 TM    WMJMCS2,WMJMCS2F        BYPASS HARD COPY QUEUING ?       01060000
         BZ    IEAJEND                 NO, BRANCH                       01061000
         OI    WMJMBUF,WMJMTRCD                                         01062000
*                                                                       01063000
*        CHECK IF THIS MLWTO SHOULD BE QUEUED TO HC DUE TO CHANGE       01064000
*        IN THE ROUTE CODES BY THE INSTALLATION EXIT                    01065000
*                                                                       01066000
*        END OF BMAJINIT SEGMENT                                        01067000
*                                                                       01068000
IEAJEND  TM    XVX0,XVX0FLCL           FIRST LINE CONTROL LINE ?        01069000
         BO    IEAJLEN4                YES, SKIP TO SET UP FIRST LINE   01070000
         TM    XVX0,XVX0UDCL           USE DEFAULT CONTROL LINE         01071000
         BZ    IEAJLEN4                NO, SKIP TO SET UP FIRST LINE    01072000
*                                                                       01073000
*        SET UP TO MOVE IN THE DEFAULT CONTROL LINE                     01074000
*        THIS IN EFFECT ADDS AN EXTRA LINE TO THE WTO                   01075000
*        THE FIRST LINE IN THE WPL IS NOW PROCESSED AS A LABEL          01076000
*        OR DATA LINE                                                   01077000
*                                                                       01078000
         LA    R15,KIEE932I            USE DEFAULT CONTROL LINE         01079000
         LA    R3,L'KIEE932I           R3 = L' DEFAULT CONTROL LINE     01080000
         LA    R14,WMJMTXT+2           R14 -> TEXT POSITION IN MAJOR    01081000
         B     IEAJMOVM                                                 01082000
*                                                                       01083000
*        BMAJFSTL                                                       01084000
*                                                                       01085000
*        SET UP THE FIRST LINE FOR THE MOVE INTO THE MAJOR              01086000
*        INPUT -                                                        01087000
*        R8 -> MAJOR WQE                                                01088000
*        R2 -> WPLLTF FIELD (MLWTO EXTENSION )                          01089000
*                                                                       01090000
*        OUTPUT -                                                       01091000
*        R15 -> TEXT SOURCE                                             01092000
*        R14 -> TARGET                                                  01093000
*        R3   = L'TEXT TO BE MOVED                                      01094000
*                                                                       01095000
*        PROCESS CONTROL LINE                                           01096000
*                                                                       01097000
IEAJLEN4 LH    R3,WKALGH               R3 = L'MAJOR WQE TEXT + 4        01098000
         SH    R3,KH4                  R3 = L'MAJOR WQE TEXT            01099000
         LA    R14,WMJMTXT+2           R14 -> TEXT POSITION IN WQE      01100000
         L     R15,WKAADTXT            R15 -> LL + MCS + TEXT           01101000
         LA    R15,4(,R15)             R15 -> TEXT                      01102000
         TM    WPLLTF,WPLLTFA          CONTROL LINE ?                   01103000
         BZ    IEAJLNLD                NO                               01104000
         LA    R5,34                   YES, SET L'MAX FOR CONTROL LINE  01105000
         TM    XVD1,XVD1AUTH           USER AUTHORIZED ?                01106000
         BZ    IEAJCHK1                NO, USE LENGTH OF 34 CHAR        01107000
         LA    R5,35                   AUTH USERS CAN HAVE 35 CHARS     01108000
IEAJCHK1 CR    R3,R5                   LENGTH > ALLOWED FOR CNTL LINE ? 01109000
         BNH   IEAJMOVM                NO, MOVE TEXT                    01110000
         LR    R3,R5                   YES, TRUNCATE TO MAX LENGTH      01111000
         B     IEAJMOVM                MOVE TEXT INTO MAJOR             01112000
*                                                                       01113000
*        PROCESS LABEL OR DATA LINE                                     01114000
*                                                                       01115000
IEAJLNLD LA    R5,70                   SET L'MAX FOR LABELA AND DATA    01116000
         TM    XVD1,XVD1AUTH           USER AUTHORIZED ?                01117000
         BZ    IEAJCHK2                NO, USE LENGTH OF 70 CHAR        01118000
         LA    R5,71                   YES,AUTH USERS CAN HAVE 71 CHARS 01119000
IEAJCHK2 CR    R3,R5                   LENGTH > MAX DATA OR LABEL LINE? 01120000
         BNH   IEAJMOVM                NO, MOVE TEXT                    01121000
         LR    R3,R5                   YES, TRUNCATE TO MAX LENGTH      01122000
*                                                                       01123000
*        BMAJMVMS                                                       01124000
*                                                                       01125000
*        MOVE THE MESSAGE INTO THE MAJOR WQE                            01126000
*        INPUT -                                                        01127000
*        R3   = L'MESSAGE                                               01128000
*        R8  -> MAJOR WQE                                               01129000
*        R14 -> TEXT AREA IN WQE                                        01130000
*        R15 -> MESSAGE TEXT                                            01131000
*                                                                       01132000
*        OUTPUT -                                                       01133000
*        THE TEXT WITH AUTHORIZATION FLAGS IS MOVED INTO THE WQE        01134000
*        THE LENGTH AND SEQUENCE NUBMER ARE ALSO ADDED TO THE           01135000
*        WQE                                                            01136000
*                                                                       01137000
IEAJMOVM LR    R5,R3                   LENGTH FOR EXECUTE               01138000
         BCTR  R5,0                    DECR FOR EX                      01139000
         EX    R5,IEAMEXMV             MVC   0(0,R14),0(R15)            01140000
*                                                                       01141000
*        TRUNCATE MESSAGE TO REMOVE TRAILING BLANKS                     01142000
*                                                                       01143000
IEAJBLKL LTR   R5,R5                   R5 ZERO YET ?                    01144000
         BZ    IEAJBLKE                YES, THEN TEXT WAS ALL BLANK     01145000
         LA    R1,0(R5,R14)            R1 -> TEXT CHAR                  01146000
         CLI   R1,C' '                 CHARACTER BLANK ?                01147000
         BNE   IEAJBLKE                NO, GET OUT OF LOOP              01148000
         BCTR  R5,0                    YES DECR INDEX INTO TEXT         01149000
         B     IEAJBLKL                LOOP AND TEST FOR END            01150000
*                                                                       01151000
*        IF THE TEXT WAS ALL BLANK THEN ONLY ONE BLANK IS USED          01152000
*        THE FOLLOWING INSTRUCTION PUTS THE CORRECT LENGTH INTO         01153000
*        R3 WHEN THE TEXT IS ALL BLANK OR NOT ALL BLANKS                01154000
*                                                                       01155000
IEAJBLKE LA    R3,1(,R5)               SET R3 TO L'TEXT                 01156000
*                                      WITH TRAILING BLANKS TRUNCATED   01157000
         TM    WMJMDEC1,WMJMDECA+WMJMDECB  DESC CODE 1 OR 2 ?           01158000
         BNZ   IEA0076E                YES, GO CHECK AUTHORIZATION      01159000
         TM    WMJMDEC2,WMJMDECK       CRITICAL EVENTUAL ACTION MSG ?   01160000
         BNZ   IEAJAUT1                NO, BRANCH                       01161000
         TM    XVD1,XVD1AUTH           APF AUTHORIZED ?                 01162000
         BO    IEAJSTXT                YES, OMIT P/P FLAG               01163000
         MVI   WMJMTXT+1,PPWTOFLG      + FLAG, PROB PGM - NO ACTION MSG 01164000
         LA    R3,2(,R3)               UPDATE LENGTH                    01165000
         B     IEAJSTOR                                                 01166000
*                                                                       01167000
IEAJAUT1 TM    XVD1,XVD1AUTH           AUTHORIZED ?                     01168000
         BZ    IEAJMVAT                NO, BRANCH                       01169000
         B     IEA00776                                                 01170000
*                                                                       01171000
IEA0076E TM    XVD1,XVD1AUTH           AUTHORIZED ?                     01172000
         BZ    IEA0077E                NO, BRANCH                       01173000
IEA00776 MVI   WMJMTXT,SUPACFLG        * FLAG, SUPERVISOR ACTION MSG    01174000
         B     IEAJSTXT                                                 01175000
*                                                                       01176000
IEA0077E OI    WMJMDEC1,WMJMDECG       TURN ON DESC CODE 7              01177000
*                                      APPLICATION PROGRAM MESSAGE      01178000
*                                                                       01179000
*        ISSUER IS A NON-AUTHORIZED PROGRAM PROBLEM                     01180000
*        DESC CODE 1 OR 2 SPECIFIED                                     01181000
*        FORCE DESC CODE 7 ON SO MLWTO WILL BE DOMMED AT TASK           01182000
*        TERMINATION                                                    01183000
*                                                                       01184000
IEAJMVAT MVI   WMJMTXT,PPACTFLG        @ FLAG, PROB PGM ACTION MSG FLAG 01185000
*                                                                       01186000
*        SHUFFLE TEXT DOWN ONE BYTE                                     01187000
*                                                                       01188000
IEAJSTXT LA    R14,WMJMTXT+1           SHIFT TEXT TO CHAR POS 2         01189000
         LA    R15,WMJMTXT+2           SET REGS FOR TEXT SHIFT          01190000
         LR    R5,R3                   LENGTH FOR EXECUTE               01191000
         BCTR  R5,0                    DECR FOR EXECUTE                 01192000
         EX    R5,IEAMSHFT             SHIFT TEXT 1 CHAR TO THE LEFT    01193000
*                                      MVC   0(0,R14),0(R15)            01194000
         LA    R14,WMJMTXT+1           R14 -> TEXT+1                    01195000
         AR    R14,R3                  ADD L'TEXT                       01196000
         MVI   0(R14),0                ZERO FIRST CHAR AFTER TEXT       01197000
         LA    R3,1(,R3)               UPDATE LENGTH                    01198000
IEAJSTOR STH   R3,WMJMTXTL             STORE TEXT LENGTH                01199000
         L     R1,ADDRUCM              R1 -> UCM                        01200000
         SH    R1,KH4                  SUBTRACT TO REFERENCE PREFIX PTR 01201000
         L     R1,0(,R1)               LOAD UCM PREFIX ADDR             01202000
*                                                                       01203000
*        PLACE UCM SEQUENCE NUMBER INTO THE WQE                         01204000
*        SETUP TO PASS BACK THE UCM SEQUENCE NUMBER TO THE CALLER       01205000
*        INCREMENT THE UCM SEQUENCE NUMBER                              01206000
*                                                                       01207000
         USING UCMPRFX,R1                                               01208000
         MVC   WMJMSEQ(3),UCMCMID+1    MOVE SEQ NO TO MAJOR             01209000
         MVC   WMJMMSGN+1(3),UCMCMID+1  MLWTO ID                        01210000
         MVC   XVWQEIDA,UCMCMID+1      SAVE MSG ID FOR NEXT LINES       01211000
*                                      FOR PASS BACK TO THE CALLER      01212000
         L     R5,UCMCMID                                               01213000
         CVD   R5,WORK8                CONVERT ID TO DECIMAL            01214000
         LA    R5,1(,R5)               INCREMENT SEQ NUMBER             01215000
         ST    R5,UCMCMID              STORE UPDATED SEQ NO IN UCM      01216000
         MVC   WMJMHCID,PATUCMID       MOVE IN PATTERN FOR ED INST      01217000
         ED    WMJMHCID,WORK8+6        CONVERT TO PRINTABLE FORM        01218000
         DROP  R1                                                       01219000
         TM    WMJMDEC2,WMJMDECI       DESCRIPTOR CODE 9 ?              01220000
         BZ    IEAJONNN                NO, NO UCM SEQ ON CONTROL LINE   01221000
*                                                                       01222000
*        DESCRIPTOR CODE 9 PROCESSING                                   01223000
*        PLACE UCM SEQUENCE NUMBER AT END OF TEXT                       01224000
*                                                                       01225000
         LA    R15,WMJMTXT             R15 -> TEXT IN MAJOR             01226000
         AH    R15,WMJMTXTL            ADD LENGTH TO TEXT ADDR          01227000
         MVC   0(L'WMJMHCID,R15),WMJMHCID  MOVE ID TO END OF TEXT       01228000
         MVI   L'WMJMHCID(R15),C' '    PUT BLANK AFTER ID IN MSG        01229000
         LH    R3,WMJMTXTL             R3 = L'TEXT                      01230000
         LA    R3,L'WMJMHCID+1(,R3)    INCR LENGTH FOR ID               01231000
         STH   R3,WMJMTXTL             SAVE UPDATED L'TEXT              01232000
*                                                                       01233000
IEAJONNN OI    WMJMDSP,WMJMDSPG        SET MAJOR SUSPENDED              01234000
         TM    XVX0,XVX0UDCL           USE DEFAULT CONTROL LINE ?       01235000
         BZ    IEAJMVLT                NO, MOVE IN LINE TYPE FLAGS      01236000
*                                                                       01237000
*        THE DEFAULT CONTROL LINE IS BEING USED                         01238000
*                                                                       01239000
*        THE FIRST MESSAGE TEXT IN THE WPL WILL BE PROCESSED AS         01240000
*        A LABEL OR DATA MESSAGE                                        01241000
*                                                                       01242000
         OI    WMJMLTYP,WMJMLTYA       YES, SET LINE TYPE TO CONTROL    01243000
         B     IEAJOUT                 END OF SEGMENT                   01244000
*                                                                       01245000
IEAJMVLT MVC   WMJMLTYP,WPLLTF         MOVE LINE TYPE FLAGS TO MAJOR    01246000
         DROP  R2                                                       01247000
         BAL   R14,TEXTLINE            INCR TO NEXT LINE                01248000
         BAL   R14,ENDUP                                                01249000
IEAJOUT  OI    XVD2,XVD2CON            INSURE CONNECTING BIT IS SET     01250000
         B     IEAMSTTS                GO TEST IF STOP WAS SET          01251000
*                                                                       01252000
*        START OF BLDMIN SEGMENT                                        01253000
*        BUILD THE MINOR WQE                                            01254000
*                                                                       01255000
IEAMBMIN BAL   R14,FINDID              FIND ID FOR CONNECTING TO        01256000
         TM    XVX1,XVX1NOID           ID FOUND ?                       01257000
         BZ    IEA1YID                 YES, CHECK IF MINOR NEEDED       01258000
*                                                                       01259000
*        NO ID FOUND. STOP PROCESSING                                   01260000
*                                                                       01261000
         OI    XVX1,XVX1STOP           SET STOP PROCESSING FLAG         01262000
         B     IEAMSTTS                                                 01263000
*                                                                       01264000
IEA1YID  L     R8,XVCMAJOR             R8 -> MAJOR WQE                  01265000
         USING WMJMEXT,R8                                               01266000
         OI    WMJMMLW,WMJMMLWD        SET CHAIN ALTERED FLAG           01267000
*                                                                       01268000
*        THERE WILL ALWAYS BE A MINOR WQE QUEUED OFF THE MAJOR          01269000
*        FINDID WILL STORE THE ADDRESS OF THE MINOR TO USE IN           01270000
*        XVCMINOR FOR US                                                01271000
*                                                                       01272000
         L     R7,XVCMINOR             R7 ->MINOR WQE FOR CONNECTING TO 01273000
         TM    XVD3,XVD3BLD1+XVD3BLD2  BOTH LINES AVAILABLE ?           01274000
         BNZ   IEA1TLN1                BRANCH, EITHER LINE AVAILABLE    01275000
*                                                                       01276000
*        NO LINE AVAILABLE IN LAST MINOR. GET ANOTHER ONE               01277000
*                                                                       01278000
IEA1ALOC L     R3,ADDRUCM              R3 -> UCM                        01279000
         USING UCM,R3                                                   01280000
         CLC   UCMWQNR,UCMWQLM         WQE AVAILABLE ?                  01281000
         BL    IEA1GETN                YES, GET A MINOR                 01282000
         TM    XVD1,XVD1PRIV           PRIVILEGED USER ?                01283000
         BO    IEA1GETN                YES, WQES ALWAYS AVAILABLE       01284000
         DROP  R3                                                       01285000
*                                                                       01286000
*        BRANCH TO THE WAIT ROUTINE AND WAIT WITHOUT AN ECB             01287000
*        THIS TYPE OF WAIT IS DONE SO THAT CONTROL WILL RETURN          01288000
*        BACK AFTER OUT MAJOR HAS BEEN DELETED BY IEAVMDSV              01289000
*        THE BRANCH ENTRY IS USED SO THAT WE HOLD THE LOCAL             01290000
*        LOCK. THIS PREVENTS US FROM BEING POSTED BEFORE WE ISSUE       01291000
*        THE WAIT TURN ON THE WAITING BIT IN THE MAJOR WQE              01292000
*                                                                       01293000
         OI    WMJMECBF,WMJMWAIT                                        01294000
*                                                                       01295000
*        DONT ALLOW ASYNCHRONOUS EXITS TO BE DISPATCHED WHILE           01296000
*        WAITING FOR A WQE. THIS COULD CAUSE AN INTERLOCK IF THE        01297000
*        RB ISSUED A WTO THAT THEN WAITED                               01298000
*        SET ADDRESSABILITY TO TCB AND SAVE R4                          01299000
*                                                                       01300000
         ST    R4,REG4SAV              SAVE ADDR OF TEXT LINE           01301000
         L     R4,TCBSAVE              RESTORE TCB ADDR                 01302000
         USING TCB,R4                                                   01303000
         NI    XVD3,255-XVD3TFX        TURN OFF 'MWTO SET TFX' FLAG     01304000
         TM    TCBFLGS1,TCBFX          ASYNCH EXITS NOT ALLOWED ?       01305000
         BO    IEA1NSET                YES, DONT SET THE TCBFX FLAG     01306000
*                                                                       01307000
*        THE ASYNCHRONOUS EXITS ARENT ALLOWED BY SOME OTHER             01308000
*        ROUTINE SO WE SHOULD LEAVE THE TCBFX BIT ALONE                 01309000
*                                                                       01310000
         OI    TCBFLGS1,TCBFX          NO, FLAG NOT SET SO SET IT       01311000
         OI    XVD3,XVD3TFX            SET FLAG SHOWING THAT            01312000
*                                                                       01313000
*        SET THE TCBFX FLAG SO CAN TURN IT OFF FREE THE CMS LOCK        01314000
*                                                                       01315000
IEA1NSET ST    R11,REG11SAV            SAVE BASE REG                    01316000
         ST    R12,REG12SAV            SAVE XV BASE                     01317000
         ST    R9,REG9SAV                                               01318000
         LR    R0,R13                  SAVE ADDR OF MODULE WORKAREA     01319000
*                                                                       01320000
*        FREE THE FRR                                                   01321000
*                                                                       01322000
         SETFRR D,WRKREGS=(9,12)                                        01323000
         LA    R12,EPARM               R12 -> ESTAE PARM AREA           01324000
         ST    R12,PARMPTR             PT TO ESTAE PARM AREA            01325000
*                                                                       01326000
         SETLOCK RELEASE,TYPE=CMS,RELATED=(UCM,IEAVMWTO(SETLCKS))       01327000
*                                                                       01328000
         LR    R13,R0                  RESTORE WORKAREA BASE            01329000
         L     R9,REG9SAV              RESTORE R9, R11 AND R12          01330000
         L     R11,REG11SAV                                             01331000
         L     R12,REG12SAV                                             01332000
*                                                                       01333000
*        SET UP FOR BRANCH ENTRY TO WAIT                                01334000
*                                                                       01335000
*        SET UP RETURN ADDR FROM WAIT BY STORING THE RETURN POINT       01336000
*        ADDR IN THE OLD PSW IN OUR RB                                  01337000
*                                                                       01338000
         LA    R15,IEA1WTRT            LOAD ADDR OF RETURN POINT        01339000
         ST    R15,RBOPSW+4            SAVE IN RB                       01340000
         ST    R12,WMJMAECB            STORE RB ADDR IN MAJOR           01341000
*                                                                       01342000
*        IEAVMDSV USES ADDR IN WMJMAECB AS THE RB TO POST               01343000
*                                                                       01344000
         L     R1,CVTPTR               ADDR THE CVT                     01345000
         USING CVT,R1                                                   01346000
         L     R15,CVTVWAIT            BRANCH ENTRY TO WAIT             01347000
         DROP  R1                                                       01348000
         SR    R1,R1                   WAIT WITHOUT ECB                 01349000
         LA    R0,1                    WAIT COUNT OF ONE                01350000
         STM   R0,R15,TCBGRS           STORE ALL REGS IN TCB            01351000
         BR    R15                     BRANCH ENTRY TO WAIT             01352000
*                                                                       01353000
*        THE REGISTERS ARE RESTORED BY WAIT                             01354000
*        WAIT WILL FREE THE LOCAL LOCK                                  01355000
*        GET BOTH LOCKS BEFORE RESUMING PROCESSING                      01356000
*                                                                       01357000
IEA1WTRT BAL   R15,SETLCKS             SET LOCAL AND CMS LOCKS          01358000
*                                                                       01359000
*        CHECK IF NEED TO TURN OFF THE TCBFX FLAG                       01360000
*        R4 -> TCB                                                      01361000
*                                                                       01362000
         TM    XVD3,XVD3TFX            TCBFX FLAG SET ?                 01363000
         BZ    IEA1RSET                NO, LEAVE FLAG ALONE             01364000
         NI    TCBFLGS1,255-TCBFX      YES, TURN TCBFX FLAG OFF         01365000
         DROP  R4                                                       01366000
IEA1RSET L     R4,REG4SAV              RESTORE R4 -> CURRENT LINE       01367000
         B     IEAMBMIN                FIND ID AGAIN TO INSURE THAT IT  01368000
*                                      HAS NOT BEEN PURGED WHILE SYSTEM 01369000
*                                      WAS ENABLED                      01370000
IEA1GETN BAL   R14,GETMINOR            GET A MINOR WQE                  01371000
*                                                                       01372000
*        BUILD LINE 1 OF A MINOR WQE                                    01373000
*                                                                       01374000
IEA1TLN1 OI    WMJMDSP,WMJMDSPG        SET MAJOR SUSPENDED              01375000
         TM    XVD3,XVD3BLD1           BUILD LINE 1 OF MINOR ?          01376000
         BZ    IEA2BLN2                NO, CHECK IF LINE 2              01377000
*                                                                       01378000
*        INITIALIZE LINE 1 OF THE MINOR                                 01379000
*                                                                       01380000
         L     R7,XVCMINOR             R7 -> MINOR                      01381000
         NI    WMJMMLW,255-WMJMMLWH    TURN OFF DUMMY MINOR FLAG        01382000
         DROP  R8                                                       01383000
         USING WMNMEXT,R7                                               01384000
         MVC   WMNMUC1(1),0(R8)        MOVE USE COUNT TO MINOR LINE 1   01385000
         NI    XVD3,255-XVD3BLD1       RESET BUILD LINE 1 FLAG          01386000
         OI    WMNMML2,WMNMML2H        SET 2ND LINE AVAILABLE           01387000
         TM    XVD3,XVD3TXT1           TEXT LINE 1 BEING USED ?         01388000
         BZ    IEA1TXL2                NO, MOVE LINE TYPE TO WQE        01389000
*                                      YES, SETUP TO MOVE FIRST         01390000
*                                      LINE INTO WQE                    01391000
         L     R15,WKAADTXT            R15 -> FIRST TEXT LINE           01392000
         LA    R15,4(,R15)             R15 -> TEXT                      01393000
         LH    R3,WKALGH               R3 = L'TEXT + 4                  01394000
         L     R2,ADDRMLHR             R2 -> MLWTO EXTENSION HEADER     01395000
*                                            FOR ACCESS TO LINE TYPE    01396000
         B     IEA1VTYP                                                 01397000
*                                                                       01398000
*        SETUP TO USE SECOND OR SUBSEQUENT TEXT LINES                   01399000
*                                                                       01400000
         USING WPLML,R4                R4 -> SUBSEQUENT LINE TO USE     01401000
IEA1TXL2 LA    R15,WPLMLTXT            R15 -> TEXT                      01402000
         LH    R3,WPLML0               R3 =L'THIS LINE                  01403000
         LA    R2,WPLMLLTF             R2 -> LINE TYPE FLAGS            01404000
         DROP  R4                                                       01405000
IEA1VTYP MVC   WMNMLT1(1),0(R2)        MOVE LINE TYPE TO MINOR          01406000
*                                      FROM EITHER THE MLWTO            01407000
*                                      EXTENSION HEADER OR FROM THE     01408000
*                                      SECOND OR SUBSEQUENT LINE        01409000
         TM    XVX0,XVX0FLJE           FIRST LINE JUST AN END LINE ?    01410000
         BO    IEA1DECR                YES, SKIP MOVING THE MSG         01411000
         TM    WMNMLT1,WMNMLT1D        THIS LINE END LINE ?             01412000
         BZ    IEA1TXTL                NO, GET TEXT LENGTH              01413000
         TM    WMNMLT1,WMNMLT1C        DATA AND END LINE ?              01414000
         BZ    IEA1DECR                NO, JUST END LINE                01415000
*                                                                       01416000
*        MIN1MOV                                                        01417000
*                                                                       01418000
*        MOVE IN LINE 1 OF THE MINOR                                    01419000
*        SET UP FOR MESSAGE LENGTH TEST                                 01420000
*                                                                       01421000
IEA1TXTL SH    R3,KH4                  ADJUST TEXT LENGTH               01422000
*                                                                       01423000
*        CHECK IF USER IS AUTHORIZED. IF SO ALLOW 71 CHARS IN TEXT      01424000
*                                                                       01425000
         LA    R1,70                   R1 =L'MAX OF UNAUTHORIZED CALLER 01426000
         TM    XVD1,XVD1AUTH           CALLER AUTHORIZED ?              01427000
         BZ    IEA1TXT2                NO, THEN ONLY ALLOW 70 CHARS     01428000
         LA    R1,71                   YES, LET USER PUT OUT 71 CHARS   01429000
IEA1TXT2 CR    R3,R1                   EXCEED MAXIMUM ALLOWED L'LINE    01430000
         BNH   IEA1BLK1                NO, USE TEXT LENGTH              01431000
         LR    R3,R1                   OTHERWISE, TRUNCATE              01432000
IEA1BLK1 MVI   WMNMTXT1,C' '           MOVE BLANK AS FIRST CHAR         01433000
         TM    XVD1,XVD1AUTH           USER APF AUTHORIZED ?            01434000
         BZ    IEA1OVE3                NO, BRANCH                       01435000
         LA    R14,WMNMTXT1+1          R14 -> MINOR WQE TEXT AREA       01436000
         LR    R5,R3                   LENGTH FOR EXECUTE               01437000
         BCTR  R5,0                    DECR FOR EXECUTE                 01438000
         EX    R5,IEAMVTXT             MOVE TEXT TO MINOR WQE           01439000
*                                      MVC   0(0,R14),0(R15)            01440000
         LA    R3,1(,R3)               UPDATE TEXT LENGTH               01441000
         B     IEA1STL1                                                 01442000
*                                                                       01443000
*        PROCESS UNAUTHORIZED REQUEST                                   01444000
*                                                                       01445000
IEA1OVE3 MVI   WMNMTXT1+1,C' '         BLANK SECOND CHAR                01446000
         LA    R14,WMNMTXT1+2          R14 -> MINOR WQE TEXT AREA       01447000
         LR    R5,R3                   LENGTH FOR EXECUTE               01448000
         BCTR  R5,0                    DECR  FOR EX                     01449000
         EX    R5,IEAMVTXT             MOVE TEXT                        01450000
*                                      MVC   0(0,R14),0(R15)            01451000
         LA    R3,2(,R3)               UPDATE TEXT LENGTH               01452000
IEA1STL1 STC   R3,WMNMTL1              STORE TEXT LENGTH IN MINOR       01453000
         LA    R14,WMNMTXT1            R14 -> TEXT AREA 2               01454000
         L     R8,XVCMAJOR             R8 -> MAJOR WQE                  01455000
         MVC   WMNMHCT1,WMJMHCID-WMJMEXT(R8)  MOVE HARD COPY ID         01456000
*                                      FROM MAJOR T0 MINOR WQE          01457000
*                                                                       01458000
*        TRUNCATE ANY TRAILING BLANKS                                   01459000
*                                                                       01460000
IEA1LOOP BCT   R3,IEA1LOOA             DECR INDEX INTO TEXT             01461000
         B     IEA1DONE                ZERO, LOOP COMPLETED             01462000
*                                                                       01463000
IEA1LOOA LA    R15,0(R3,R14)           R15 -> NEXT CHAR                 01464000
         CLI   0(R15),C' '             BLANK ?                          01465000
         BE    IEA1LOOP                YES, RE-ENTER LOOP               01466000
*                                                                       01467000
IEA1DONE LA    R3,1(,R3)               R3 NOW HAS CORRECT LENGTH        01468000
         STC   R3,WMNMTL1                                               01469000
         BAL   R14,TEXTLINE            UPDATE TEXT POINTER              01470000
         B     IEA1DECR                SKIP TO ENDUP ROUTINE            01471000
*                                                                       01472000
*        MIN2INIT                                                       01473000
*                                                                       01474000
*        BUILD LINE 2 OF A MINOR WQE                                    01475000
*                                                                       01476000
IEA2BLN2 L     R7,XVCMINOR             R7 -> MINOR WQE                  01477000
         MVC   WMNMUC2(1),0(R8)        COPY USE COUNT FROM MAJOR        01478000
         LA    R2,WMNMUC2              R2 -> 2ND MINOR LINE             01479000
         O     R2,WMNMUC1              PRESERVE USE COUNT               01480000
         ST    R2,WMNMUC1              CHAIN TO FIRST LINE              01481000
         OI    WMNMML2,WMNMML2C        INDICATE MINOR                   01482000
         NI    XVD3,255-XVD3BLD2       TURN OFF BUILD 2ND LINE FLAG     01483000
         NI    WMNMML2,255-WMNMML2H    RESET 2ND LINE AVAILABLE         01484000
         TM    XVD3,XVD3TXT1           TEXT LINE 1 BEING USED ?         01485000
         BZ    IEA2TX2A                NO, BRANCH                       01486000
*                                                                       01487000
*        PROCESS TEXT LINE 1                                            01488000
*                                                                       01489000
         L     R15,WKAADTXT            R15 -> MCS HEADER + TEXT         01490000
         LA    R15,4(,R15)             R15 -> TEXT                      01491000
         LH    R3,WKALGH               R3 = L'TEXT +4                   01492000
         L     R2,ADDRMLHR             R2 -> MLWTO EXTENSION HEADER     01493000
         B     IEA2MVL2                                                 01494000
*                                                                       01495000
*        PROCESS SECOND AND SUBSEQUENT LINES                            01496000
*                                                                       01497000
         USING WPLML,R4                                                 01498000
IEA2TX2A LA    R15,WPLMLTXT            R15 -> TEXT                      01499000
         LH    R3,WPLML0               R3 = L'MESSAGE                   01500000
         LA    R2,WPLMLLTF             POINT TO LINE TYPE               01501000
         DROP  R4                                                       01502000
IEA2MVL2 MVC   WMNMLT2(1),0(R2)        MOVE LINE TYPE TO 2ND LINE       01503000
         TM    XVX0,XVX0FLJE           FIRST LINE JUST END              01504000
         BO    IEA1DECR                YES, POST WTO ECB                01505000
         TM    WMNMLT2,WMNMLT2D        END LINE ?                       01506000
         BZ    IEA2TXTL                NO, BRANCH                       01507000
         TM    WMNMLT2,WMNMLT2C        YES, DATA AND END LINE ?         01508000
         BZ    IEA1DECR                NO, JUST END LINE, POST ECB      01509000
*                                                                       01510000
*        MIN2MOV                                                        01511000
*                                                                       01512000
*        MOVE IN LINE 2 OF THE MINOR                                    01513000
*        CHECK WHETHER TO ALLOW 70 OR 71 CHARACTERS OF TEXT             01514000
*                                                                       01515000
IEA2TXTL LA    R1,70                   R1 = MAX NON-AUTHORIZED L'TEXT   01516000
         TM    XVD1,XVD1AUTH           USER AUTHORIZED ?                01517000
         BZ    IEA2GLEN                NO, BRANCH                       01518000
         LA    R1,71                   YES, ALLOW MAX 71 CHARS          01519000
IEA2GLEN SH    R3,KH4                  ADJUST FOR FLAGS                 01520000
         CR    R3,R1                   L'TEXT > MAXIMUM ALLOWED ?       01521000
         BNH   IEA2BTXT                NO, BRANCH                       01522000
         LR    R3,R1                   YES, TRUNCATE L'TEXT             01523000
IEA2BTXT MVI   WMNMTXT2,C' '           BLANK FIRST CHAR                 01524000
         TM    XVD1,XVD1AUTH           CALLER APF AUTHORIZED ?          01525000
         BZ    IEA2OVE4                NO, BRANCH                       01526000
         LA    R14,WMNMTXT2+1          R14 -> MINOR TEXT AREA           01527000
         LR    R5,R3                   LENGTH FOR EXECUTE               01528000
         BCTR  R5,0                    ADJUST FOR EXECUTE               01529000
         EX    R5,IEAMVTXT             MOVE TEXT TO MINOR               01530000
*                                      MVC   0(0,R14),0(R15)            01531000
         LA    R3,1(,R3)               INC MINOR LENGTH BY 1            01532000
         B     IEA2STL2                                                 01533000
*                                                                       01534000
IEA2OVE4 MVI   WMNMTXT2+1,C' '         BLANK SECOND CHAR                01535000
         LA    R14,WMNMTXT2+2          R14 -> MINOR TEXT AREA           01536000
         LR    R5,R3                   LENGTH FOR EXECUTE               01537000
         BCTR  R5,0                    ADJUST FOR EXECUTE               01538000
         EX    R5,IEAMVTXT             MVC   0(0,R14),0(R15)            01539000
*                                                                       01540000
         LA    R3,2(,R3)               UPDATE LENGTH                    01541000
IEA2STL2 STC   R3,WMNMTL2              STORE MINOR LENGTH               01542000
         LA    R14,WMNMTXT2            R14 -> TEXT AREA 2               01543000
         MVC   WMNMHCT2,WMNMHCT1       MOVE IN HARD COPY ID             01544000
*                                                                       01545000
*        TRUNCATE ANY TRAILING BLANKS                                   01546000
*                                                                       01547000
IEA2LOOP BCT   R3,IEA2LOOA             DECR INDEX INTO TEXT             01548000
         B     IEA2DONE                BRANCH, LOOP COMPLETED           01549000
IEA2LOOA LA    R15,0(R3,R14)           R15 -> NEXT CHAR                 01550000
         CLI   0(R15),C' '             BLANK ?                          01551000
         BE    IEA2LOOP                RE-ENTER LOOP                    01552000
*                                                                       01553000
IEA2DONE LA    R3,1(,R3)               R3 NOW HAS CORRECT LENGTH        01554000
         STC   R3,WMNMTL2                                               01555000
         BAL   R14,TEXTLINE            UPDATE TEXT POINTER              01556000
IEA1DECR BAL   R14,ENDUP                                                01557000
*                                                                       01558000
IEAMSTTS TM    XVX1,XVX1STOP           ERROR FOUND ?                    01559000
         BO    IEAMFREL                YES, DON'T TAKE EXIT OR POST     01560000
         TM    XVD2,XVD2DELW           MESSAGE TO BE DELETED ?          01561000
         BZ    IEAMNOST                NO, TAKE SUBSYSTEM EXIT          01562000
         DROP  R7                      RELEASE MINOR BASE               01563000
         USING WMJMEXT,R8              ADDRESSABILITY FOR MAJOR         01564000
*                                                                       01565000
*        SET UP THE FLAGS SO THAT THIS MLWTO IS DELETED AND SENT        01566000
*        TO HARDCOPY                                                    01567000
*                                                                       01568000
         OI    WMJMECBF,WMJMMAJD       INDICATE MAJOR IS DELETED        01569000
         L     R1,ADDRUCM              R1 -> UCM                        01570000
         SH    R1,KH4                  ADDR UCM PREFIX                  01571000
         L     R1,0(,R1)               R1 -> UCM PREFIX                 01572000
         USING UCMPRFX,R1              ADDRESSABILITY FOR PREFIX        01573000
         OI    UCMSFLG2,UCMSYSI        INDICATE CLEANUP NEEDED          01574000
         OI    WMJMBUF,WMJMBUFC+WMJMBUFE  INDICATE WQE SERVICED         01575000
*                                         AND READY FOR HARDCOPY        01576000
         OI    WMJMDSP,WMJMDSPB        INDICATE QUEUE TO H.C.           01577000
         DROP  R1                                                       01578000
*                                                                       01579000
*        SUBSYSTEM EXIT SEGMENT                                         01580000
*                                                                       01581000
*        THIS SEGMENT WILL PASS THE MAJOR AND MINOR LINES TO THE        01582000
*        SUBSYSTEM EXIT. THE SUBSYSTEM MAY CHANGE THE MSG AND/OR        01583000
*        ASK THAT THE MESSAGE BE DELETED                                01584000
*                                                                       01585000
*        INPUT -                                                        01586000
*        R8 -> MAJOR WQE                                                01587000
*        XVCMINOR -> THE MINOR WQE                                      01588000
*        XVC3BLD1 AND BLD2 WILL BOTH BE ON IF THE MAJOR WQE WAS         01589000
*        JUST BUILT                                                     01590000
*                                                                       01591000
*        OUTPUT -                                                       01592000
*        IF THE SUBSYSTEM ASKS TO DELETE THE MAJOR WQE THEN             01593000
*        XVD2DELW WILL BE TURNED ON AND WMJMMAJD WILL BE SET ON         01594000
*        IN THE MAJOR WQE                                               01595000
*                                                                       01596000
*        THE MINOR MAY ALSO BE ASKED TO BE DELETED, BUT THE             01597000
*        REQUEST WILL ONLY BE HONORED IF WMJMMAJD IS ON                 01598000
*                                                                       01599000
*        FREE THE LOCKS                                                 01600000
*                                                                       01601000
IEAMNOST BAL   R15,FRELCKS             CALL FRELCKS ROUTINE             01602000
*                                                                       01603000
*        SET UP RETRY ADDRESS IN CASE SUBSYSTEM EXIT HAS PROBLEMS       01604000
*                                                                       01605000
         L     R1,PARMPTR              R1 -> PARM LIST                  01606000
         USING PARMLIST,R1                                              01607000
         LA    R2,IEAHSSRT             R2 -> RETRY ADDR                 01608000
         ST    R2,PARMRTAD             SWITCH ADDR IN ESTAE PARMLIST    01609000
         MVI   PARMFTPT,FTSSOB         SET FOOTPRINT                    01610000
         LA    R2,SUBSLIST             BUILD BLOCKS IN OUR STORAGE      01611000
         STM   R0,R15,RECREGS          SAVE THE REGS AT THIS POINT      01612000
         DROP  R1                                                       01613000
*                                                                       01614000
*        SET UP SSOB AND SSWT                                           01615000
*                                                                       01616000
         ST    R2,SUBSPARM             BUILD PARM LIST PTR              01617000
         USING SSOB,R2                                                  01618000
         MVC   SSOBID,KCSSOB           IDENTIFY BLOCK AS AN SSOB        01619000
         LA    R1,SSOBHSIZ             LENGTH OF SSOB                   01620000
         STH   R1,SSOBLEN              STORE INTO SSOB LENGTH FIELD     01621000
         LA    R1,SSOBWTO              IDENTIFY THAT THIS CALL IS       01622000
         STH   R1,SSOBFUNC             FOR A WTO                        01623000
         SR    R5,R5                   CLEAR REG FOR ZEROING FIELDS     01624000
         ST    R5,SSOBSSIB             NO SSIB FOR THIS CALL            01625000
         ST    R5,SSOBRETN             INSURE RETN IS INITIALLY ZERO    01626000
         LA    R1,SSWTBGN              POINT SSOB AT SSWT               01627000
         ST    R1,SSOBINDV                                              01628000
         LA    R1,SSWTSIZE             PUT IN SIZE OF SSWT              01629000
         STH   R1,SSWTLEN                                               01630000
         ST    R8,SSWTWQE              PUT IN ADDR OF MAJOR             01631000
         ST    R5,SSWTORE              NO ORE THIS TRIP                 01632000
         TM    XVD3,XVD3BLD1+XVD3BLD2  MAJOR WQE ?                      01633000
         BNO   IEAHMINS                NO, BRANCH                       01634000
         ST    R5,SSWTMIN              NO MINOR YET                     01635000
         B     IEAHSSGO                                                 01636000
*                                                                       01637000
*        RETRY ROUTINE IF SUBSYSTEM EXIT ERROR REACHED ESTAE            01638000
*                                                                       01639000
IEAHSSRT XC    SSOBRETN,SSOBRETN       ZERO RETURN CODE FROM SUBSYSTEM  01640000
         L     R1,PARMPTR                                               01641000
         USING PARMLIST,R1                                              01642000
         XC    PARMRTAD,PARMRTAD       ZERO RETRY ADDR                  01643000
         DROP  R1                                                       01644000
         B     IEAHLOCK                                                 01645000
*                                                                       01646000
IEAHMINS MVC   SSWTMIN,XVCMINOR        MOVE IN ADDR OF MINOR            01647000
IEAHSSGO LA    R1,SUBSPARM             R1 -> PARM LIST POINTER          01648000
*                                                                       01649000
         IEFSSREQ  ,                   CALL THE SUBSYSTEM               01650000
         LR    R5,R15                  SAVE RETURN CODE FROM EXIT       01651000
*                                                                       01652000
*        SET THE LOCKS AGAIN                                            01653000
*                                                                       01654000
IEAHLOCK BAL   R15,SETLCKS                                              01655000
*                                                                       01656000
*        CHECK TO SEE IF THE SUBSYSTEM HAS SPECIFIED HARDCOPY BYPASS    01657000
*                                                                       01658000
         TM    WMJMCS2,WMJMCS2F        HARDCOPY BYPASS SET ?            01659000
         BZ    IEAHHCRD                NO, BRANCH                       01660000
         NI    WMJMDSP,255-WMJMDSPB    YES, INDICATE DO NOT HC          01661000
*                                                                       01662000
*        CHECK TO SEE IF THIS WQE IS FOR A MINOR                        01663000
*                                                                       01664000
IEAHHCRD TM    XVD3,XVD3BLD1+XVD3BLD2  BUILDING MAJOR ?                 01665000
         BNO   IEAHSUSP                NO, GO TURN OFF SUSPEND FLAG     01666000
*                                                                       01667000
*        CHECK IF SYBSYSTEM WANTS MESSAGE DELETED                       01668000
*        IF THE EXIT WAS SUCCESSFUL THEN R15 WAS ZERO ON RETURN         01669000
*        FROM THE SUBSYSTEM                                             01670000
*        CHECK R1 WHICH HAS RETURN INDICATION                           01671000
*                                                                       01672000
         LTR   R5,R5                   (R15) EXIT SUCCESSFUL ?          01673000
         BNZ   IEAHSUSP                NO, CHECK IF USER WANTS MSG OUT  01674000
*                                                                       01675000
*        CHECK FOR DELETION REQUEST                                     01676000
*                                                                       01677000
         LA    R5,SSWTNDSP             R5 = DON'T DISPLAY MESSAGE RC    01678000
         CL    R5,SSOBRETN             SUBSYSTEM REQ DON'T DISPLAY ?    01679000
         BNZ   IEAHSUSP                NO, CHECK USER                   01680000
         DROP  R2                                                       01681000
         USING WMJMEXT,R8              SET ADDRESSING TO MAJOR          01682000
*                                                                       01683000
*        SET UP THE FLAGS SO THAT THIS MLWTO IS DELETED AND SENT        01684000
*        TO HARDCOPY                                                    01685000
*                                                                       01686000
         OI    WMJMECBF,WMJMMAJD       MAJOR IS DELETED                 01687000
*                                                                       01688000
*        SET UP ADDRESSABILITY FOR UCMPREFIX                            01689000
*                                                                       01690000
         L     R1,ADDRUCM              R1 -> UCM                        01691000
         SH    R1,KH4                  ADDR UCM PREFIX                  01692000
         L     R1,0(,R1)               R1 -> UCM PREFIX                 01693000
         USING UCMPRFX,R1              SET ADDRESSABILITY               01694000
         OI    UCMSFLG2,UCMSYSI        SET WQE HOUSEKEEEPING REQUIRED   01695000
         OI    WMJMBUF,WMJMBUFE        MARK WQE AS SERVICED             01696000
*                                                                       01697000
*        CHECK TO SEE IF THE SUBSYSTEM HAS SPECIFIED HARDCOPY           01698000
*        BYPASS                                                         01699000
*                                                                       01700000
         TM    WMJMCS2,WMJMCS2F        BYPASS HARDCOPY QUEUEING ?       01701000
         BZ    IEA00C2A                NO, BRANCH                       01702000
         B     IEAHSUSP                TURN OFF SUSPEND MODE            01703000
*                                                                       01704000
IEA00C2A OI    WMJMBUF,WMJMBUFC        INDICATE READY FOR HC            01705000
         OI    WMJMDSP,WMJMDSPB        INDICATE SEND TO HC              01706000
         DROP  R1                                                       01707000
*                                                                       01708000
*        TRANSLATE UNPRINTABLE AND NON DISPLAY CHARACTERS IN THE WQE    01709000
*                                                                       01710000
IEAHSUSP TM    XVD3,XVD3BLD1+XVD3BLD2  A MAJOR WQE ?                    01711000
         BNO   IEAHTRMI                NO, GO DO MINOR                  01712000
         SR    R1,R1                   HANDLE MAJOR TEXT                01713000
         LH    R1,WMJMTXTL             LENGTH OF MAJOR TEXT             01714000
         BCTR  R1,0                    DECR FOR EX                      01715000
         LA    R15,WMJMTXT             R15 -> TEXT                      01716000
         L     R14,VTRTAB              R14 -> TRTAB IN IEEVVWTO         01717000
         EX    R1,TRINST               TRANSLATE UNPRINTABLES TO TILDE  01718000
*                                      TR    0(0,R15),0(R14)            01719000
         B     IEAHTRND                DONE WITH MAJOR WAIT FOR MINOR   01720000
*                                                                       01721000
IEAHTRMI SR    R1,R1                   DO MINOR WQE                     01722000
         DROP  R8                                                       01723000
         USING WMNMEXT,R7                                               01724000
         ICM   R1,B'0001',WMNMTL1      L'1ST MINOR TEXT                 01725000
         BZ    IEA00C64                LENGTH ZERO, BRANCH              01726000
         BCTR  R1,0                    DECR FOR EX                      01727000
         LA    R15,WMNMTXT1            R15 -> TEXT                      01728000
         L     R14,VTRTAB              R14 -> TRTAB IN IEEVVWTO         01729000
         EX    R1,TRINST               TRANSLATE UNPRINTABLES TO TILDE  01730000
*                                      TR    0(0,R15),0(R14)            01731000
IEA00C64 TM    WMNMML2,WMNMML2H        LINE 2 AVAILABLE ?               01732000
         BO    IEAHTRND                YES, BRANCH AS NOT USED          01733000
         ICM   R1,B'0001',WMNMTL2      R1 = L'TEXT                      01734000
         BZ    IEAHTRND                LENGTH ZERO, BRANCH              01735000
         BCTR  R1,0                    DECR FOR EX                      01736000
         LA    R15,WMNMTXT2            R15 -> TEXT                      01737000
         L     R14,VTRTAB              R14 -> TRTAB IN IEEVVWTO         01738000
         EX    R1,TRINST               TRANSLATE UNPRINTABLES TO TILDE  01739000
*                                      TR    0(0,R15),0(R14)            01740000
         DROP  R7                                                       01741000
         USING WMJMEXT,R8                                               01742000
IEAHTRND NI    WMJMDSP,255-WMJMDSPG    TURN OFF SUSPEND BIT             01743000
         DROP  R8                                                       01744000
*                                                                       01745000
*        FREE THE LOCKS                                                 01746000
*                                                                       01747000
         BAL   R15,FRELCKS                                              01748000
         CLI   NUMLINES,10             PROCESSED 10 LINES ?             01749000
         BNL   IEA00CAA                YES, BRANCH                      01750000
         CLI   XVX2,0                  MORE LINES TO PROCESS ?          01751000
         BNH   IEA00CAA                NO, BRANCH                       01752000
         SR    R1,R1                   INCREMENT LINE COUNTER           01753000
         IC    R1,NUMLINES                                              01754000
         LA    R1,1(,R1)                                                01755000
         STC   R1,NUMLINES                                              01756000
         B     IEAMMORE                BRANCH TO PROCESS NEXT LINE      01757000
*                                                                       01758000
*        THE LOCKS WILL BE FREE'D BY THE SUBSYSTEM EXIT SEGMENT         01759000
*        THIS SEGMENT POSTS THE UCMOECB USING A CROSS MEMORY POST       01760000
*                                                                       01761000
IEA00CAA STM   R14,R12,SAVEREGS+12     SAVE REGS AROUND XMPOST          01762000
         LR    R9,R13                  SAVE SAVEAREA ADDRESS IN R9      01763000
*                                      R9 AND R14 ARE THE ONLY          01764000
*                                      REGS PRESERVED BY XMPOST         01765000
         L     R3,ADDRUCM              R3 -> UCM                        01766000
         USING UCM,R3                                                   01767000
*                                      SETUP R1 AS BIT MASK SO          01768000
         L     R1,KX800000             THAT NO REFERENCES TO            01769000
*                                      STORAGE NEED BE MADE AFTER       01770000
*                                      BASE REG IS UPDATED              01771000
         L     R12,UCMWAKUP            R12 -> ROUTINE TO GET            01772000
*                                      CONTROL IF XMPOST FAILS          01773000
         OR    R12,R1                  INDICATE ERRET IS TO RUN         01774000
*                                      MASTER SCHEDULER MEMORY          01775000
         L     R13,UCMASCB             ASCB OF MEMORY CONTAINING        01776000
*                                      ECB TO POST                      01777000
         SLR   R10,R10                 R10 CONTAINS COMPLETION          01778000
*                                      CODE TO POST WITH                01779000
         LA    R11,UCMOECB             R11 -> ECB TO BE POSTED          01780000
         OR    R11,R1                  SET END OF LIST BIT ON           01781000
         L     R15,CVTPTR                                               01782000
         USING CVT,R15                                                  01783000
         L     R15,CVT0PT01            R15 -> XMPOST BRANCH ENTRY       01784000
         DROP  R15                                                      01785000
         BALR  R14,R15                 CALL XMPOST                      01786000
*                                                                       01787000
         LA    R15,8                   DISPATCHER CALL                  01788000
         SVC   116                     ESR SVC TYPE 1                   01789000
*                                                                       01790000
         LR    R13,R9                  RESTORE SAVEAREA ADDRESS         01791000
         LM    R14,R12,SAVEREGS+12     RESTORE REGS AFTER XMPOST        01792000
         MVI   NUMLINES,1              SET NUMLINES TO 1                01793000
         DROP  R3                                                       01794000
IEAMMORE TM    XVX1,XVX1STOP           NEED TO STOP ?                   01795000
         BO    IEAMSTOP                                                 01796000
         CLI   XVX2,0                  MORE LINES TO DO ?               01797000
         BH    IEASEXIT                YES, GO PROCESS                  01798000
         B     IEAMSTOP                GO CHECK IF WE SHOULD STOP       01799000
*                                                                       01800000
*        THIS CODE WILL GET CONTROL ONLY IF AN ERROR WAS                01801000
*        ENCOUNTERED WHILE RUNNING UNDER ESTAE                          01802000
*                                                                       01803000
*        IF THERE IS AN ERROR THEN WE ASSUME THAT THE WPL               01804000
*        CAUSED THE ERROR. THE USER WILL BE ABENDED WITH A D23          01805000
*        ABEND CODE.                                                    01806000
*                                                                       01807000
IEAVRETY OI    XVD1,XVD1PERR           SET ERROR BIT TO ABEND THE USER  01808000
         MVI   XVAMOD,MWTOID           IEAVMWTO'S ID FOR D23            01809000
         MVI   XVFNCT,D23VALID         PARMLIST VALIDITY CHECK          01810000
         MVI   XVREASON,D23PARM        PARMLIST NOT ADDRESSABLE BY USER 01811000
*                                                                       01812000
*        ERROR PROCESSING EXIT                                          01813000
*                                                                       01814000
IEASTOPA OI    XVX1,XVX1STOP           STOP PROCESSING                  01815000
         B     IEAMSTOP                                                 01816000
*                                                                       01817000
*        FREE THE LOCAL AND CMS LOCKS                                   01818000
*                                                                       01819000
IEAMFREL BAL   R15,FRELCKS             CALL THE FREE LOCK SUBROUTINE    01820000
*                                                                       01821000
*        FREE THE ESTAE RECOVERY                                        01822000
*                                                                       01823000
IEAMSTOP ESTAE 0                       FREE IEAVMWTO STAE               01824000
*                                                                       01825000
*        FREE THE WORKAREA FOR THIS MODULE                              01826000
*                                                                       01827000
         LR    R1,R13                  MOVE ADDR OF WORKAREA TO R1      01828000
         L     R0,WKFREECN             GET SUBPOOL AND SIZE PARM        01829000
         L     R13,4(,R13)             RESTORE PTR TO CALLER'S AREA     01830000
*                                                                       01831000
         FREEMAIN R,LV=(0),A=(1)       FREE WORKAREA FOR THIS MODULE    01832000
*                                                                       01833000
IEAMGRET ICM   R1,B'1111',XVCMAJOR     THERE A MAJOR WQE ?              01834000
         BZ    IEAMRETN                NO, BRANCH                       01835000
         L     R1,XVWQEID              YES, GET MSG ID NUM NO           01836000
*                                      FROM RB SAVE AREA                01837000
         SRL   R1,8                    SHIFT OUT CONSOLE ID             01838000
*                                                                       01839000
*        PUT MSG ID OR ZERO INTO XWQEID FIELD IN XVSAV                  01840000
*        THIS FIELD WILL BE RETURNED TO THE USER BY IEAVVWTO            01841000
*                                                                       01842000
IEAMRETN ST    R1,XVWQEID              PUT MSG ID BACK INTO XVSAV       01843000
*                                                                       01844000
         RETURN (14,12)                RETURN TO VWTO                   01845000
*                                                                       01846000
*        MLWTO CLEANUP ROUTINE                                          01847000
*                                                                       01848000
*        THIS ROUTINE GETS CONTROL IF THERE IS AN ERROR UNDER           01849000
*        THE FRR PROTECTION. IF SO, THEN WE WERE BUILDING A MAJOR       01850000
*        OR MINOR WQE AND NO ERROR WAS EXPECTED. THIS ROUTINE           01851000
*        WILL INSURE THAT THE SUSPENDED BIT IS TURNED OFF IN THE        01852000
*        MAJOR WQE AND WILL LET THE ERROR CONTINUE                      01853000
*                                                                       01854000
*        INPUT -                                                        01855000
*        R2  -> FRR PARM LIST                                           01856000
*        R13 -> CALLERS REG SAVE AREA                                   01857000
*        R14-R15 ARE THE RETURN AND ENTRY REGS                          01858000
*        PARMRGAD IN THE PARM AREA IS THE ADDR OF OUR REG               01859000
*        RESTORE AREA                                                   01860000
*        REGS 3,4,6,7,10,11,12,13 ARE VALID IN THE RESTORE AREA         01861000
*                                                                       01862000
MWTOCLNP SAVE  (14,12),,'IEAVMWTO CLEANUP'                              01863000
         USING PARMLIST,R2                                              01864000
         LR    R0,R13                  MOVE CALLERS SAVEAREA PTR        01865000
         L     R1,PARMRGAD                                              01866000
         LM    R3,R13,14(R1)           RELOAD R3 TO R13 FROM REG SAVE   01867000
         ICM   R1,B'1111',XVCMAJOR     R1 -> MAJOR WQE                  01868000
         BZ    IEAVCLOT                NO MAJOR WQE, RETURN             01869000
*                                                                       01870000
*        THERE IS A MAJOR WQE, IT SHOULD BE ON THE WQE QUEUE            01871000
*                                                                       01872000
*        INSURE THAT IT ISNT IN THE SUSPENDED STATE                     01873000
*                                                                       01874000
         USING WMJM,R1                                                  01875000
         NI    WMJMDSP,255-WMJMDSPG    TURN OFF SUSPEND FLAG            01876000
*                                                                       01877000
*        THE MAJOR WILL BE CLEANED UP BY IEAVMED2 AS LONG AS THE        01878000
*        SUSPEND FLAG IS OFF                                            01879000
*                                                                       01880000
IEAVCLOT LR    R13,R0                  RESTORE CALLERS SAVE AREA PTR    01881000
*                                                                       01882000
         RETURN (14,12)                RETURN TO CALLER                 01883000
*                                                                       01884000
         DROP  R1,R2                                                    01885000
*                                                                       01886000
*        CONSTANTS FOR IEAVMWTO                                         01887000
*                                                                       01888000
IEAMEXMV MVC   0(0,R14),0(R15)         MOVE TEXT TO MAJOR               01889000
IEAMSHFT MVC   0(0,R14),0(R15)         SHIFT TEXT 1 CHAR TO LEFT        01890000
IEAMVTXT MVC   0(0,R14),0(R15)         MOVE TEXT TO MINOR               01891000
TRINST   TR    0(0,R15),0(R14)         TRANSLATE UNPRINTABLES           01892000
*                                      USING TRTAB IN IEEVVWTO          01893000
*                                                                       01894000
KIEE932I DC    C'IEE932I'              DEFAULT CONTROL LINE TEXT        01895000
*                                                                       01896000
VTRTAB   DC    V(TRTAB)                -> TRTAB IN IEEVVWTO             01897000
WKSIZE   DC    A(WRKSIZE)              SIZE OF MWTO'S SAVE AND WORKAREA 01898000
WKSUBPL  DC    AL4(229)                STORAGE FROM SUBPOOL 229         01899000
WKFREECN DC    AL1(229),AL3(WRKSIZE)   FREEMAIN PARAMETER               01900000
WQEPLSZ  DC    F'4096'                 SIZE OF WQE CELL EXTENSION       01901000
SPLFRECN DC    AL1(231),XL3'1000'      FREE WQE EXTENSION PARM          01902000
WKGETWWB DC    AL1(231),AL3(WWBSIZE)   WWB GETMAIN/FREEMAIN PARM        01903000
*                                                                       01904000
KH1      DC    H'1'                                                     01905000
KH2      DC    H'2'                                                     01906000
KH3      DC    H'3'                                                     01907000
KH4      DC    H'4'                                                     01908000
KH20     DC    H'20'                   USED TO CHECK FOR EXTENSION      01909000
*                                      OF WQE CELL POOL                 01910000
         DC    0F'0'                                                    01911000
KX800000 DC    X'80000000'             END OF LIST INDICATOR            01912000
WTPONLY  DC    AL2(WPLROUTK)           CHECK FOR ONLY WTP ROUTE CODE    01913000
KCSSOB   DC    C'SSOB'                 SUBSYSTEM IDENTIFIER             01914000
MODULEID DC    C'MWTO'                 FRR/ESTAE MODULE ID              01915000
KC0000   DC    C'0000'                 CHAR ZEROS FOR ROUTE CODE INIT   01916000
KCZ      EQU   C'Z'                    INLINE AREA DESIGNATOR           01917000
PPWTOFLG EQU   C'+'                    PROB PGM - NO ACTION MSG         01918000
PPACTFLG EQU   C'@'                    PROB PGM - ACTION MSG FLAG       01919000
SUPACFLG EQU   C'*'                    SUPERVISOR - ACTION MSG          01920000
*                                                                       01921000
*        RETURN CODE EQUATES                                            01922000
*                                                                       01923000
LINERR   EQU   X'04'                   ERROR FOUND IN NUMBER OF LINES   01924000
NOIDMCH  EQU   X'08'                   NO MATCH FOR MSG ID IN R0        01925000
INVLDLT  EQU   X'0C'                   LINE TYPE WAS INVALID            01926000
RCWTP    EQU   X'10'                   WTP ROUTE CODE WAS USED          01927000
HCONLY   EQU   X'14'                   MLWTO WAS FOR HARDCOPY ONLY      01928000
*                                                                       01929000
HEXCHAR  DC    C'0123456789ABCDEF'     TABLE TO CONVERT ROUT CODES      01930000
PATTIME  DC    XL9'4021214B21214B2121'  ED PATTERN FOR TIME             01931000
PATUCMID DC    XL4'40212121'           ED PATTERN FOR UCM MSG NUMBER    01932000
*                                                                       01933000
ALLREGS  DC    X'FFFF'                 RESTORE ALL REGS MAP             01934000
*                                                                       01935000
         DC    0F'0'                                                    01936000
*                                                                       01937000
ELIST    ESTAE ,MF=L                   CREATE ESTAE PARM LIST           01938000
ELISTL   EQU   *-ELIST                 FOR LENTH CALULATION             01939000
*                                                                       01940000
FTLOCK   EQU   X'04'                   LOCK FOOT PRINT                  01941000
FTSSOB   EQU   X'03'                   SUSBYSTEM EXIT FOOT PRINT        01942000
*                                                                       01943000
*        GETMINOR                                                       01944000
*                                                                       01945000
*        INPUT -                                                        01946000
*        XVCMAJOR -> MAJOR WQE                                          01947000
*        XVCMINOR -> LAST MINOR ON THE CHAIN IF WE                      01948000
*                    ARE CREATING THE NEXT MINOR                        01949000
*        THIS ROUTINE ASSUMES THAT A WQE IS AVAILABLE UNDER THE         01950000
*        UCMWQLM COUNT                                                  01951000
*                                                                       01952000
*        OUTPUT -                                                       01953000
*        A NEW MINOR IS QUEUED OFF THE MAJOR WQE                        01954000
*        THE MINOR IS ZEROED                                            01955000
*                                                                       01956000
GETMINOR ST    R14,WORK8               SAVE THE CALLER'S ADDR           01957000
         USING WMJM,R8                                                  01958000
         BAL   R14,GETWQE              GET A MINOR WQE                  01959000
*                                                                       01960000
*        CHECK IF A MINOR WAS OBTAINED                                  01961000
*                                                                       01962000
         LTR   R1,R1                   WAS A WQE OBTAINED               01963000
         BNZ   IEANGET1                YES, ADD IT TO THE CHAIN         01964000
*                                                                       01965000
*        NO A WQE WASN'T OBTAINED. SET ERROR FLAGS                      01966000
*                                                                       01967000
         OI    XVX1,XVX1STOP           STOP PROCCESSING                 01968000
         OI    XVD1,XVD1PERR           SET ERROR FLAGS                  01969000
         B     IEANGET2                                                 01970000
*                                                                       01971000
IEANGET1 L     R8,XVCMAJOR             R8 -> MAJOR WQE                  01972000
         ICM   R7,B'1111',WMJMMIN      MINOR QUEUED TO MAJOR ?          01973000
         BNZ   IEAMINR2                YES, QUEUE TO LAST MINOR         01974000
         ST    R1,WMJMMIN              QUEUE NEW MINOR TO MAJOR         01975000
         OI    WMJMMLW,WMJMMLWH        SET DUMMY MINOR QUEUED           01976000
         B     IEAMBL12                SET MINOR FLAGS                  01977000
*                                                                       01978000
         DROP  R8                                                       01979000
         USING WMNMEXT,R7                                               01980000
IEAMINR2 L     R7,XVCMINOR             R7 -> LAST MINOR QUEUED          01981000
         O     R1,WMNMUC2              PRESERVE USE COUNT FOR 2ND LINE  01982000
         ST    R1,WMNMUC2              QUEUE MINOR                      01983000
IEAMBL12 LR    R7,R1                   R7 -> NEW MINOR                  01984000
         OI    XVD3,XVD3BLD1+XVD3BLD2     BUILD LINE 1 AND 2            01985000
         OI    WMNMML1,WMNMML1C+WMNMML1H  SET MINOR WQE AND GETMAINED   01986000
         DROP  R7                                                       01987000
         ST    R7,XVCMINOR             STORE ADDR OF CURRENT MINOR      01988000
IEANGET2 L     R14,WORK8               RESTORE RETURN ADDR              01989000
         BR    R14                     RETURN TO CALLER                 01990000
*                                                                       01991000
*        ENDUP                                                          01992000
*                                                                       01993000
ENDUP    L     R8,XVCMAJOR             R8 -> MAJOR WQE FOR FORCE END    01994000
         SR    R15,R15                 ZERO REG                         01995000
         IC    R15,XVX2                R15 = COUNT OF LINES REMAINING   01996000
         BCTR  R15,0                   DECREMENT                        01997000
         STC   R15,XVX2                UPDATE COUNT IN XVX2             01998000
         LTR   R15,R15                 ANY MORE LINES TO PROCESS ?      01999000
         BNZ   ENDUPRTN                YES, RETURN TO CALLER            02000000
         TM    XVX0,XVX0FEDE           FORCE END ?                      02001000
         BZ    ENDUPRTN                NO, RETURN TO CALLER             02002000
         USING WMJMEXT,R8                                               02003000
         TM    WMJMMLW,WMJMMLWH        DUMMY MINOR QUEUED ?             02004000
         BO    IEAEJEND                YES, FLAG MAJOR AS END           02005000
         ICM   R7,B'1111',XVCMINOR     R7 -> MINOR WQE                  02006000
*                                      THERE A MINOR WQE ?              02007000
         BZ    IEAEJEND                NO, FLAG MAJOR AS END            02008000
         DROP  R8                                                       02009000
         USING WMNMEXT,R7                                               02010000
IEAEP2ND ICM   R15,B'0111',WMNMNX1     NEXT LINE PTR = 0 ?              02011000
         BZ    IEAE1END                YES, FLAG LINE 1 AS END          02012000
         ICM   R15,B'0111',WMNMNX2     PTR TO NEXT MINOR WQE = 0 ?      02013000
         BZ    IEAE2END                YES, FLAG LINE 2 AS END          02014000
         L     R7,WMNMUC2              R7 -> NEXT MINOR WQE             02015000
         B     IEAEP2ND                FIND END OF CHARIN               02016000
*                                                                       02017000
IEAE1END CLI   WMNMTL1,0               MINOR CONTAIN TEXT ?             02018000
         BNE   IEAD1END                YES, THEN DATA END               02019000
         MVI   WMNMLT1,WMNMLT1D        NO, FLAG END LINE ONLY           02020000
         B     ENDUPRTN                RETURN TO CALLER                 02021000
*                                                                       02022000
IEAD1END MVI   WMNMLT1,WMNMLT1C+WMNMLT1D  SET DATA END FLAG             02023000
         B     ENDUPRTN                RETURN TO CALLER                 02024000
*                                                                       02025000
IEAE2END CLI   WMNMTL2,0               TEXT IN SECOND MINOR WQE ?       02026000
         BNE   IEAD2END                YES, THEN DATA END               02027000
         MVI   WMNMLT2,WMNMLT2D        NO, FLAG END LINE ONLY           02028000
         B     ENDUPRTN                RETURN TO CALLER                 02029000
*                                                                       02030000
IEAD2END MVI   WMNMLT2,WMNMLT2C+WMNMLT2D  SET DATA END FLAG             02031000
         B     ENDUPRTN                RETURN TO CALLER                 02032000
*                                                                       02033000
         DROP  R7                                                       02034000
         USING WMJMEXT,R8                                               02035000
IEAEJEND MVI   WMJMLTYP,WMJMLTYC+WMJMLTYD  FLAG MAJOR AS DATA END       02036000
ENDUPRTN BR    R14                     RETURN TO CALLER                 02037000
*                                                                       02038000
*        FINDID                                                         02039000
*                                                                       02040000
*        FIND AN EXISTING ID FOR A CONNECTING MLWTO                     02041000
*                                                                       02042000
*        PROTECT CONTENTS OF R4 AROUND CHECK FOR JOBSTEP AND MEMORY     02043000
*                                                                       02044000
FINDID   ST    R4,REG4SAV              SAVE IN R4 SAVEAREA              02045000
         L     R3,ADDRUCM              R3 -> UCM                        02046000
         USING UCM,R3                                                   02047000
         L     R1,UCMWTOQ              R1 -> FIRST WQE ON SYSTEM QUEUE  02048000
IEAISEAR LTR   R1,R1                   END OF QUEUE ?                   02049000
         BZ    IEAINOID                YES, NO ID FOUND                 02050000
         DROP  R8                                                       02051000
         DROP  R3                                                       02052000
         USING WMJMEXT,R1                                               02053000
         TM    WMJMMLW,WMJMMLWB        MAJOR WQE ?                      02054000
         BZ    IEAINXWQ                NO, GET NEXT WQE                 02055000
         CLC   XVWQEIDA,WMJMMSGN+1     YES, MSG IDS MATCH ?             02056000
         BNE   IEAINXWQ                NO, BRANCH                       02057000
         TM    WMJMLTYP,WMJMLTYD       END LINE ?                       02058000
         BO    IEAINOID                YES, ERROR                       02059000
*                                                                       02060000
*        CONFIRM THAT THE MSG WAS ISSUED BY A PROGRAM IN THE SAME       02061000
*        MEMORY AND JOB STEP AS THE CALLER                              02062000
*                                                                       02063000
         L     R4,TCBSAVE              R4 -> CALLER'S TCB               02064000
         L     R7,ASCBSAVE             R7 -> CALLER'S ASCB              02065000
         USING ASCB,R7                                                  02066000
         USING TCB,R4                                                   02067000
         CLC   WMJMASID,ASCBASID       ADDRESS SPACE ID MATCH ?         02068000
         BNE   IEAINOID                NO, THEN NO ID MATCH             02069000
         CLC   WMJMJTCB,TCBJSTCB       SAME JOB STEP ?                  02070000
         BNE   IEAINOID                NO, THEN NO ID MATCH             02071000
         DROP  R4,R7                                                    02072000
*                                                                       02073000
*        THE MESSAGE WAS ISSUED BY A USER IN THE SAME ADDRESS           02074000
*        SPACE AND JOB STEP                                             02075000
*                                                                       02076000
         ST    R1,XVCMAJOR             STORE MAJOR ADDR                 02077000
         L     R2,WMJMMIN              R2-> FIRST MINOR                 02078000
         ST    R2,XVCMINOR             STORE MINOR ADDR                 02079000
         TM    WMJMMLW,WMJMMLWH        MINOR HAVE TEXT ?                02080000
         BO    IEAIDUMM                NO, DUMMY                        02081000
         B     IEAITST1                CHECK IF MINOR LINE 2 AVAIL      02082000
*                                                                       02083000
IEAINXWQ LA    R1,0(,R1)               CLEAR HIORDER BYTE               02084000
         ICM   R1,B'0111',WMJMEXTA     R1 -> NEXT WQE                   02085000
         B     IEAISEAR                                                 02086000
*                                                                       02087000
         DROP  R1                                                       02088000
*                                                                       02089000
         USING WMNMEXT,R2                                               02090000
IEAITST1 TM    WMNMLT1,WMNMLT1D        FIRST LINE AN END ?              02091000
         BO    IEAINOID                YES, ERROR                       02092000
         TM    WMNMLT2,WMNMLT2D        SECOND LINE END ?                02093000
         BO    IEAINOID                YES, ERROR                       02094000
         LA    R2,0(,R2)               ENSURE HIORDER BYTE IS ZERO      02095000
         ICM   R2,B'0111',WMNMNX2      R2 -> NEXT MINOR WQE OR ZERO     02096000
*                                      THERE A MINOR ?                  02097000
         BNZ   IEAISADD                NO, CONTINUE SEARCH              02098000
         L     R2,XVCMINOR             R2 -> LAST MINOR WQE             02099000
         TM    WMNMML2,WMNMML2H        SECOND LINE AVAILABLE ?          02100000
         BZ    FINDIRET                NO, RETURN                       02101000
         OI    XVD3,XVD3BLD2           SET BUILD SECOND LINE FLAG       02102000
         B     FINDIRET                RETURN TO CALLER                 02103000
*                                                                       02104000
IEAISADD ST    R2,XVCMINOR             SAVE MINOR WQE ADDR              02105000
         B     IEAITST1                GO CHECK IF LINE 2 IS AVAILABLE  02106000
*                                                                       02107000
         DROP  R2                                                       02108000
         USING WMJMEXT,R1                                               02109000
IEAIDUMM OI    WMJMMLW,WMJMMLWF        SET CHAIN REUSABLE FLAG          02110000
         L     R2,XVCMINOR             R2 -> MINOR WQE                  02111000
         DROP  R1                                                       02112000
         USING WMNMEXT,R2                                               02113000
         XC    WMNM(WMNMSIZE),WMNM     ZERO MINOR                       02114000
         OI    WMNMML1,WMNMML1C+WMNMML1H   SET MINOR AND GETMAINED FLAG 02115000
         OI    WMNMML2,WMNMML2H            INDICATE LINE 2 AVAILABLE    02116000
         OI    XVD3,XVD3BLD1+XVD3BLD2      SET BUILD LINE 1 AND 2       02117000
         B     FINDIRET                RETURN TO CALLER                 02118000
*                                                                       02119000
         DROP  R2                                                       02120000
IEAINOID MVI   XVRETCOD,NOIDMCH        SET RETURN CODE                  02121000
         OI    XVX1,XVX1NOID           INDICATE NO ID FOUND             02122000
         XC    XVCMAJOR(8),XVCMAJOR    ZERO MAJOR AND MINOR PTRS        02123000
FINDIRET L     R4,REG4SAV              RESTORE R4                       02124000
         BR    R14                                                      02125000
*                                                                       02126000
*        GETWQE                                                         02127000
*                                                                       02128000
*        GET A WQE FROM THE WQE CELL POOL                               02129000
*        THE WQE WILL BE ZEROED                                         02130000
*        INPUT -                                                        02131000
*        UCMWQECP IS THE CELL POOL ID                                   02132000
*        UCMWQNR IS THE NUMBER OF WQES CURRENTLY IN USE                 02133000
*                                                                       02134000
*        OUTPUT -                                                       02135000
*        R1 -> THE ZEROED WQE IF ONE HAS BEEN OBTAINED                  02136000
*        R1 WILL BE ZERO IF A WQE WASN'T AVAILABLE                      02137000
*                                                                       02138000
*        PROCESS -                                                      02139000
*        REGS 14,15,0,1,2 ARE SAVED AND USED IN THIS ROUTINE IF A       02140000
*        CELL IS NOT AVAILABLE, THE ROUTINE CHECKS IF EXTENSION         02141000
*        IS NEEDED. IF ONE IS NEEDED IT IS OBTAIND AND WE ATTEMPT       02142000
*        TO GET A CELL AGAIN.                                           02143000
*                                                                       02144000
GETWQE   STM   R14,R2,GETSAVE                                           02145000
         L     R3,ADDRUCM              R3 -> UCM                        02146000
         USING UCM,R3                                                   02147000
         LA    R2,KH1                  SET NOT DONE INDICATOR IN R2     02148000
IEAGLOOP LTR   R2,R2                   TEST END OF GET LOOP             02149000
         BZ    IEAGRETN                YES, RETURN TO USER              02150000
*                                                                       02151000
*        NOT DONE                                                       02152000
*        GET A WQE FROM THE CELLPOOL                                    02153000
*                                                                       02154000
         L     R0,UCMWQECP             GET CELLPOOL ID FOR WQES         02155000
*                                                                       02156000
         GETCELL CPID=(0),BRANCH=YES   GET A WQE CELL                   02157000
*                                                                       02158000
*        CHECK IF CELL WAS OBTAINED                                     02159000
*        RETURN CODE IS IN R15                                          02160000
*                                                                       02161000
         LTR   R15,R15                 GET A CELL ?                     02162000
         BNZ   IEAGECHK                NO, CHECK TYPE OF RETURN CODE    02163000
*                                                                       02164000
*        YES, GOT A WQE                                                 02165000
*                                                                       02166000
         XC    0(WMJMSIZE,R1),0(R1)    ZERO OUT THE WQE                 02167000
         LH    R2,UCMWQNR              INCREMENT NO OF WQES             02168000
         LA    R2,1(,R2)                                                02169000
         STH   R2,UCMWQNR                                               02170000
         SR    R2,R2                   INDICATE ALL DONE                02171000
         B     IEAGLOOP                                                 02172000
*                                                                       02173000
*        CHECK IF CPOOL SHOULD BE EXTENDED                              02174000
*                                                                       02175000
IEAGECHK CH    R15,KH4                 EXTENSION NEEDED ?               02176000
         BE    IEA01220                YES, BRANCH                      02177000
         MVI   XVAMOD,MWTOID           IEAVMWTO'S ID FOR D23            02178000
         MVI   XVFNCT,D23WQEGC         WQE GETCELL FAILURE              02179000
         STC   R15,XVREASON                                             02180000
         B     IEAGNOWQ                                                 02181000
*                                                                       02182000
*        YES, EXTENDED THE WQE CELL POOL                                02183000
*        PROTECT THE CONTENTS OF R4 AROUND THE GETMAIN                  02184000
*                                                                       02185000
IEA01220 ST    R4,REG4SAV              STORE IN OUR SAVEAREA            02186000
         LA    R1,231                  USE SUBPOOL 231                  02187000
         L     R0,WQEPLSZ              GET SIZE OF EXTENSION            02188000
         L     R4,TCBSAVE              GET PTRS TO TCB AND ASCB         02189000
         L     R7,ASCBSAVE                                              02190000
*                                                                       02191000
         GETMAIN RC,LV=(0),SP=(1),BRANCH=YES                            02192000
*                                                                       02193000
         L     R4,REG4SAV              RESTORE THE CURRENT LINE PTR     02194000
*                                                                       02195000
*        CHECK IF GETMAIN WAS SUCCESSFUL                                02196000
*                                                                       02197000
         LTR   R15,R15                 EXTENSION SUCCESSFUL ?           02198000
         BZ    IEA01272                YES, BRANCH                      02199000
         MVI   XVAMOD,MWTOID           NO, IEAVMWTO'S ID FOR D23        02200000
         MVI   XVFNCT,D23WQEGM         WQE GETMAIN FAILURE, SP231       02201000
         STC   R15,XVREASON                                             02202000
         B     IEAGNOWQ                                                 02203000
*                                                                       02204000
*        GETMAIN OK, EXTEND CELLPOOL                                    02205000
*                                                                       02206000
IEA01272 ST    R1,SPLEXTAD             SAVE ADDR OF THE EXTENSION       02207000
         L     R3,ADDRUCM              RESTORE BASE FOR UCM             02208000
         L     R0,UCMWQECP             GET WQE CELL POOL ID             02209000
         LA    R15,WMJMSIZE            SIZE OF CELLS REQUIRED           02210000
*                                                                       02211000
*        ADDR OF EXTENSION IS IN R1                                     02212000
*                                                                       02213000
         BLDCPOOL  CPID=(0),SP=231,CSIZE=(15),CPADDR=(1),              X02214000
               AUTODEL=YES,POOLSIZ=4,BRANCH=YES,SERIAL=YES              02215000
*                                                                       02216000
*        CHECK IF EXTENSION IS OK                                       02217000
*                                                                       02218000
         LA    R2,KH1                  INSURE R2 SET TO LOOP            02219000
         LTR   R15,R15                 CELL POOL EXTENDED ?             02220000
         BZ    IEAGLOOP                YES, GO BACK AND GET A WQE       02221000
*                                                                       02222000
*        EXTENSION WASN'T SUCCESSFUL                                    02223000
*        FREE THE STORAGE FOR THE EXTENSION                             02224000
*                                                                       02225000
         MVI   XVAMOD,MWTOID           IEAVMWTO'S ID FOR D23            02226000
         MVI   XVFNCT,D23WQEBC         WQE BLDCPOOL FAILURE             02227000
         STC   R15,XVREASON                                             02228000
         L     R0,SPLFRECN             LOAD SUBPOOL AND SIZE            02229000
         L     R1,SPLEXTAD             GET ADDR OF EXTENSION            02230000
*                                                                       02231000
         FREEMAIN  R,LV=(0),A=(1),BRANCH=YES                            02232000
*                                                                       02233000
IEAGNOWQ SR    R1,R1                   SET R1 TO SHOW NO WQE            02234000
         LR    R2,R1                   GET OUT OF LOOP                  02235000
         B     IEAGLOOP                                                 02236000
*                                                                       02237000
IEAGRETN LM    R14,R0,GETSAVE          RESTORE R14 TO R0                02238000
         L     R2,GETSAVE+16           RESTORE R2                       02239000
         BR    R14                     RETURN TO CALLER                 02240000
*                                                                       02241000
         DROP  R3                                                       02242000
*                                                                       02243000
*        WAITWQE                                                        02244000
*                                                                       02245000
*        WAIT FOR A WQE TO BE FREED                                     02246000
*        INPUT -                                                        02247000
*        XVWWB IS SET TO ZERO BY INITIAL SEGMENT AND TO ADDR OF         02248000
*        WWB BY THIS ROUTINE                                            02249000
*        OUTPUT -                                                       02250000
*        XVWWB -> WWB. AT LEAST ONE WQE HAS BEEN FREED                  02251000
*                                                                       02252000
WAITWQE  STM   R14,R2,WAITSAVE         SAVE CALLER'S REGISTERS          02253000
*                                                                       02254000
*        CHECK IF A WWB EXISTS. IF NOT THEN GET ONE                     02255000
*                                                                       02256000
         ICM   R1,B'1111',XVWWB        HAS A WWB BEEN OBTAINED ?        02257000
         BNZ   IEABWAIT                YES, WAIT ON THE ECB             02258000
*                                                                       02259000
*        NO, GET A WWB AND CHAIN IT TO WQE WWB CHAIN                    02260000
*                                                                       02261000
         L     R0,WKGETWWB             GETMAIN PARM OF SUBPL & SIZE     02262000
*                                                                       02263000
         GETMAIN R,LV=(0),BRANCH=YES                                    02264000
*                                                                       02265000
         ST    R1,XVWWB                SAVE ADDR OF WWB                 02266000
         USING WWB,R1                  ADDRESSABILITY FOR WWB           02267000
*                                                                       02268000
*        SET UP THE WWB                                                 02269000
*                                                                       02270000
         MVC   WWBASCB,ASCBSAVE        PUT IN CALLER'S ASCB ADDR        02271000
         ST    R4,WWBTCBAD             CALLER'S TCB ADDR                02272000
         L     R3,ADDRUCM              R3 -> UCM                        02273000
         USING UCM,R3                                                   02274000
*                                                                       02275000
*        CONNECT THIS WWB IN TO END OF THE CHAIN                        02276000
*                                                                       02277000
         L     R2,UCMWECBT             R2 -> END OF CHAIN               02278000
*                                                                       02279000
*        R2 -> LAST WWB ON THE CHAIN                                    02280000
*        MOVE END OF CHAIN ENDICATOR FROM PREVIOUS END TO NEW WWB       02281000
*                                                                       02282000
         MVC   WWBFWDPT,WWBFWDPT-WWB(R2)                                02283000
*                                                                       02284000
*        POINT LAST WWB ON CHAIN TO NEW WWB                             02285000
*                                                                       02286000
         ST    R1,WWBFWDPT-WWB(,R2)                                     02287000
*                                                                       02288000
*        POINT NEW WWB BACK TO LAST CHAIN MEMBER                        02289000
*                                                                       02290000
         ST    R2,WWBBCKPT                                              02291000
*                                                                       02292000
*        POINT CHAIN TAIL AT NEW WWB                                    02293000
*                                                                       02294000
         ST    R1,UCMWECBT                                              02295000
*                                                                       02296000
*        THE NEW WWB IS NOW AT THE END OF THE CHAIN                     02297000
*                                                                       02298000
IEABWAIT L     R1,XVWWB                INSURE ADDRESSABILITY OF WWB     02299000
         NI    WWBFLAGS,255-WWBPOSTD   TURN OFF POSTED BIT              02300000
         XC    WWBECB,WWBECB           ZERO OUT THE ECB                 02301000
*                                                                       02302000
*        FREE THE LOCKS BEFORE ISSUING THE WAIT                         02303000
*                                                                       02304000
         BAL   R15,FRELCKS             CALL THE FREE LOCK SUBROUTINE    02305000
*                                                                       02306000
*        CLEAR RETRY ADDRESS IN PARM AREA                               02307000
*                                                                       02308000
         L     R1,PARMPTR              ADDR OF ESTAE PARM AREA          02309000
         USING PARMLIST,R1                                              02310000
         XC    PARMRTAD,PARMRTAD       CLEAR RETRY ADDR                 02311000
         DROP  R1                                                       02312000
         L     R1,XVWWB                                                 02313000
         USING WWB,R1                                                   02314000
*                                                                       02315000
*        CHECK IF WE ARE IN MEMORY ONE                                  02316000
*        IF NOT THEN TAKE A LONG WAIT                                   02317000
*                                                                       02318000
         L     R7,ASCBSAVE             LOAD ADDR OF OUR ASCB            02319000
         USING ASCB,R7                                                  02320000
         CLC   ASCBASID,KH1            IN MEMORY ONE WITH COMMTASK ?    02321000
         DROP  R7                                                       02322000
         BNE   IEABLONG                NO, GO TAKE A LONG WAIT          02323000
         LA    R1,WWBECB               GET ADDR OF ECB IN WAIT BLOCK    02324000
*                                                                       02325000
         WAIT  ,ECB=(1)                WAIT FOR A WQE TO BE FREED       02326000
*                                                                       02327000
         B     IEABRETN                                                 02328000
*                                                                       02329000
IEABLONG LA    R1,WWBECB               R1 -> ECB IN WAIT BLOCK          02330000
*                                                                       02331000
         WAIT ,ECB=(1),LONG=YES        TAKE A LONG WAIT                 02332000
*                                                                       02333000
*        SET THE LOCAK AND CMS LOCKS                                    02334000
*                                                                       02335000
IEABRETN BAL   R15,SETLCKS                                              02336000
         LM    R14,R2,WAITSAVE                                          02337000
         BR    R14                     RETURN TO USER                   02338000
*                                                                       02339000
         DROP  R1                                                       02340000
         DROP  R3                                                       02341000
*                                                                       02342000
*        TEXTLINE                                                       02343000
*                                                                       02344000
*        THIS ROUTINE SETS R4 -> THE NEXT LINE IN THE                   02345000
*        WPL TO BE PROCESSED                                            02346000
*                                                                       02347000
*        INPUT -                                                        02348000
*        ADDRMLHR -> WPLLTF                                             02349000
*        XVD3TXT1 IF ON INDICATES THAT THE FIRST LINE OF THE WPL        02350000
*        WAS JUST PROCESSED                                             02351000
*        R4 -> WPLML0 OF CURRENT LINE IF NOT THE FIRST LINE             02352000
*                                                                       02353000
*        OUTPUT -                                                       02354000
*        XVD3TXT1 IS SET OFF                                            02355000
*        XVX0UDCL IS SET OFF                                            02356000
*        R4 -> WPLML0 FOR THE NEXT LINE                                 02357000
*                                                                       02358000
         USING WPLML,R4                                                 02359000
*                                                                       02360000
TEXTLINE TM    XVD3,XVD3TXT1           FIRST LINE JUST PROCESSED        02361000
         BZ    IEAXNFST                NO, INCREMENT FOR ML EXTENSION   02362000
*                                      HEADER                           02363000
*                                                                       02364000
*        THE FIRST LINE HAS BEEN PROCESSED                              02365000
*        ADVANCE TO THE FIRST LINE IN THE MULTI LINE EXTENSION          02366000
*                                                                       02367000
         L     R4,ADDRMLHR             R4 -> WPLLTF                     02368000
         LA    R4,4(,R4)               INCR TO FIRST LINE IN WPLML0     02369000
         NI    XVD3,255-XVD3TXT1       TURN OFF LINE ONE FLAG           02370000
         NI    XVX0,255-XVX0UDCL       TURN OFF DEFAULT CNTL LINE FLAG  02371000
         B     IEAXRETN                RETURN TO CALLER                 02372000
*                                                                       02373000
*        R4 -> L'CURRENT LINE                                           02374000
*        INCREMENT R4 TO THE NEXT LINE                                  02375000
*                                                                       02376000
IEAXNFST AH    R4,WPLML0               ALL LENGTH OF CURRENT LINE       02377000
*                                      R4 -> WPLML0 FOR THE NEXT LINE   02378000
*        DROP  R4                                                       02379000
IEAXRETN BR    R14                     RETURN TO USER                   02380000
*                                                                       02381000
*        FRELCKS                                                        02382000
*                                                                       02383000
*        THE CMS AND LOCAL LOCKS ARE RELEASED                           02384000
*        R11, R12 AND R13 ARE SAVED                                     02385000
*        R11 AND R12 GO IN MLWTO'S WORKAREA                             02386000
*        R13 IS SAVED IN R0                                             02387000
*        R15 IS THE RETURN REGISTER                                     02388000
*                                                                       02389000
FRELCKS  ST    R11,REG11SAV            BASE REG SAVED IN WORKAREA       02390000
         ST    R12,REG12SAV            EXTENDED SAVE AREA BASE          02391000
         LR    R0,R13                  MOVE PTR TO SAVEAREA             02392000
*                                                                       02393000
*        FREE THE FRR AND RESET THE PARM AREA PTR                       02394000
*                                                                       02395000
         SETFRR D,WRKREGS=(11,12)                                       02396000
*                                                                       02397000
         LA    R12,EPARM               ADDR OF ESTAE PARM AREA          02398000
         ST    R12,PARMPTR             SAVE NEW ADDR                    02399000
*                                                                       02400000
         SETLOCK RELEASE,TYPE=CMS,                                     *02401000
               RELATED=(UCM,IEAVMWTO(SETLCKS))                          02402000
*                                                                       02403000
         SETLOCK RELEASE,TYPE=LOCAL,                                   *02404000
               RELATED=(UCM,IEAVMWTO(SETLCKS))                          02405000
*                                                                       02406000
         LR    R13,R0                  RESTORE PTR TO SAVEAREA          02407000
         L     R11,REG11SAV            RESTORE BASE REG                 02408000
         LA    R12,EPARM               GET ADDR OF ESTAE PARMLIST       02409000
         USING PARMLIST,R12            ESTABLISH BASE                   02410000
         NI    PARMFLAG,255-PARMFRID   TURN OFF FRR INDICATOR           02411000
         DROP  R12                     RELEASE BASE                     02412000
         L     R12,REG12SAV            RESTORE PTR TO EXTENDED AREA     02413000
         BR    R15                     RETURN TO CALLER                 02414000
*                                                                       02415000
*        SETLCKS                                                        02416000
*                                                                       02417000
*        THE LOCAL AND CMS LOCKS ARE OBTAINED                           02418000
*        R11, R12 AND R13 ARE SAVED                                     02419000
*        R11 AND R12 GO IN MLWTO'S WORKAREA                             02420000
*        R13 IS SAVED IN R0                                             02421000
*        R15 IS THE RETURN REGISTER                                     02422000
*                                                                       02423000
SETLCKS  ST    R11,REG11SAV            BASE REG SAVED IN WORKAREA       02424000
         ST    R12,REG12SAV            EXTENDED SAVE AREA BASE          02425000
         LR    R0,R13                  MOVE PTR TO SAVEAREA             02426000
*                                                                       02427000
         SETLOCK OBTAIN,TYPE=LOCAL,MODE=UNCOND,                        *02428000
               RELATED=(UCM,IEAVMWTO(FRELCKS))                          02429000
*                                                                       02430000
         SETLOCK OBTAIN,TYPE=CMS,MODE=UNCOND,                          *02431000
               RELATED=(UCM,IEAVMWTO(FRELCKS))                          02432000
*                                                                       02433000
         LR    R13,R0                  RESTORE PTR TO SAVEAREA          02434000
         L     R11,REG11SAV            RESTORE BASE REG                 02435000
         L     R12,REG12SAV            RESTORE PTR TO EXTENDED AREA     02436000
*                                                                       02437000
*        SET UP THE FRR FOR THIS ROUTINE                                02438000
*        LET SETFRR USE R2 AND R3 AS WORK REGS                          02439000
*        SAVE THE CONTENTS OF R2 AND R3 IN SUBSLIST                     02440000
*                                                                       02441000
         STM   R2,R3,SUBSLIST          SAVE R2 AND R3                   02442000
         L     R3,ADDRUCM              R3 -> UCM                        02443000
         USING UCM,R3                                                   02444000
         L     R1,UCMFRRAD             R1 -> COMM TASK FRR IEAVMFRR     02445000
         DROP  R3                                                       02446000
*                                                                       02447000
         SETFRR A,FRRAD=(R1),PARMAD=PARMPTR,WRKREGS=(R2,R3)             02448000
*                                                                       02449000
         L     R1,PARMPTR              R1 -> FRR PARMAREA               02450000
         USING PARMLIST,R1                                              02451000
         LA    R2,RECSAVE              R2 -> REG SAVE AREA              02452000
         ST    R2,PARMRGAD             PT TO REG AREA FOR RETRY         02453000
         MVC   PARMID,MODULEID         IDENTIFY AS IEAVMWTO             02454000
         DROP  R1                      DROP PRESENT PARMLIST BASE       02455000
         LA    R2,EPARM                GET PTR TO ESTAE PARMLIST        02456000
         USING PARMLIST,R2             SET ADDRESS TO ESTAE PARM        02457000
*                                                                       02458000
*********************************************************************** 02459000
*                                                                       02460000
*        THE FOLLOWING PARMFRID FIELD IN THE ESTAE PARAMETER LIST       02461000
*        IS SET TO ZERO TO GUARANTEE THAT RECOVERY WILL BE DONE         02462000
*        ON BOTH THE ESTAE AND FRR LEVEL                                02463000
*                                                                       02464000
*********************************************************************** 02465000
         NI    PARMFLAG,255-PARMFRID   TURN OFF FRR INDICATOR           02466000
         DROP  R2                      DROP TEMPORARY BASE              02467000
         USING PARMLIST,R1             RESET BASE TO FRR PARMLIST       02468000
         MVI   PARMFTPT,FTLOCK         SET FOOTPRINT                    02469000
         LA    R2,MWTOCLNP             R2 -> MLWTO CLEANUP ROUTINE      02470000
         ST    R2,PARMCLAD                                              02471000
         LM    R2,R3,SUBSLIST          RESTORE R2 AND R3                02472000
         DROP  R1                                                       02473000
         BR    R15                     RETURN TO CALLER                 02474000
*                                                                       02475000
*********************************************************************** 02476000
*                                                                       02477000
*        WORKAREA FOR THIS MODULE IEAVMWTO                              02478000
*                                                                       02479000
*********************************************************************** 02480000
*                                                                       02481000
WORKAREA DSECT                                                          02482000
SAVEREGS DS    18F                     STANDARD SAVEAREA                02483000
REG4SAV  DS    F                       SAVEAREA FOR R4                  02484000
REG9SAV  DS    F                       USED TO SAVE R9                  02485000
REG11SAV DS    F                       USED TO SAVE R11                 02486000
REG12SAV DS    F                       USED TO SAVE R12                 02487000
WORK8    DS    D                                                        02488000
GETSAVE  DS    5F                      SAVEAREA FOR GETWQE ROUTINE      02489000
SPLEXTAD DS    F                       -> WQE EXTENSION                 02490000
WAITSAVE DS    6F                      SAVE AREA FOR WIITWQE ROUTINE    02491000
ASCBSAVE DS    F                       SAVE AREA FOR ASCB ADDR          02492000
ADDRMLHR DS    F                       -> MULTI LINE EXTENSION HEADER   02493000
*                                         PART OF WPL                   02494000
TCBSAVE  DS    F                       -> CALLER'S TCB                  02495000
SUBSPARM DS    F                       PTS AT THE SUBSLIST              02496000
SUBSLIST DS    9F                      CONTAINS THE SSOB & SSWT         02497000
*                                                                       02498000
*        ESTAE SVC PARAMETER AREA                                       02499000
*                                                                       02500000
EPARM    DS    CL24                    ESTAE PARM AREA                  02501000
*                                                                       02502000
ADDRUCM  DS    F                       -> UCM                           02503000
PARMPTR  DS    F                       -> RECOVERY PARM AREA            02504000
NUMLINES DS    XL1                     COUNT OF LINES PROCESSED         02505000
RECSAVE  DS    H                       RESTORE MAP                      02506000
RECREGS  DS    16F                     RESTORE AREA FOR RETRY           02507000
LASTWPLB DS    F                       -> LAST BYTE +1 OF WPL IN SP 229 02508000
WORKBYTE DS    XL1                                                      02509000
LINETYPE DS    XL1                     LINE TYPE FLAGS FROM WPLLTF      02510000
*                                      OR WPLMLLTF                      02511000
         DS    0D                      ROUND UP TO DOUBLE WORD          02512000
WRKSIZE  EQU   *-WORKAREA              CALC SIZE OF WORKAREA            02513000
*                                                                       02514000
*********************************************************************** 02515000
*                                                                       02516000
*        WORKAREA USED IN MODULE IEAVVWTO                               02517000
*                                                                       02518000
*********************************************************************** 02519000
*                                                                       02520000
WORKVV   DSECT                                                          02521000
@SA00001 DS    18F                                                      02522000
@SAGETB  DS    18F                                                      02523000
@PC00003 DS    F                                                        02524000
@SA00004 DS    F                                                        02525000
@PC00004 DS    F                                                        02526000
@SA00008 DS    15F                                                      02527000
@SA00002 DS    15F                                                      02528000
@PC00002 DS    F                                                        02529000
PARMPTRV DS    A                                                        02530000
@TF00001 DS    F                                                        02531000
REG1SAV  DS    F                                                        02532000
REG2SAV  DS    A                  SAVE AREA FOR R2 WHEN DEALING WITH    02533000
*                                 UCM                                   02534000
LONGTXT  DS    A                  -> AREA GETMAINED FOR LONG WPL        02535000
*                                    IF ZERO THEN NO STORAGE GETMAINED  02536000
LONGLEN  DS    F                  L'GETMAINED AREA FOR LONG TEXT        02537000
*                                                                       02538000
LENMWQE  DS    F                  L'OF ALL MINOR WQES                   02539000
*                                 VALID ONLY FOR MLWTO                  02540000
MLWTOXH  DS    A                  -> MLWTO EXTENT HEADER IN CALLERS WPL 02541000
*                                 VALID ONLY FOR MLWTO                  02542000
*                                                                       02543000
*        VERSION 2 XWPL TO STORE ALL USER PROVIDED WPL FIELDS           02544000
*                                                                       02545000
WKALGH   DS    AL2                MESSAGE LENGTH OF MAJOR WQE TEXT FROM 02546000
*                                 CALLER'S WPL                          02547000
WKAMCSF  DS    XL2                MCS FLAGS COPIED FROM CALLERS WPL     02548000
WKAADTXT DS    AL4                -> MESSAGE TEXT                       02549000
WKAVRSN  DS    AL1                XWPL VERSION NUMBER                   02550000
WKAFLAGS DS    XL1                MISC FLAGS                            02551000
WKARPYLN DS    AL1                L'REPLY FOR WTOR                      02552000
WKALNGTH DS    AL1                L'XWPL, ZERO FOR VERSION 1            02553000
WKAMCSF1 DS    XL2                EXTENDED MCS FLAGS                    02554000
WKACPFLF DS    XL2                MCS FLAGS FOR CNTL PROGRAM USE        02555000
WKARPBUF DS    AL4                -> REPLY BUFFER                       02556000
WKAECBP  DS    AL4                -> ECB                                02557000
WKASEQN  DS    AL4                DOM/CONNECT ID                        02558000
WKADSC   DS    XL2             *  DESCRIPTOR CODE TO BE USED IN WTO     02559000
WKAROC   DS    XL2             V  ROUTE CODES TO BE USED IN WTO         02560000
WKAROUT  DS    XL14               EXTENDED ROUTING CODES                02561000
WKAMSGTY DS    XL2                MSGTYPE FLAGS COPIED FROM CALLERS WPL 02562000
WKAPRTY  DS    XL2                MESSAGE PRIORITY                      02563000
WKAJOBID DS    CL8                JOB ID                                02564000
WKAJOBNM DS    CL8                JOB NAME                              02565000
WKAKEY   DS    CL8                RETRIEVAL KEY                         02566000
WKATOKN  DS    AL4                TOKEN FOR DOM                         02567000
WKACNID  DS    AL4                CONSOLE ID                            02568000
WKASYSNA DS    CL8                SYSTEM NAME                           02569000
WKACNNME DS    CL8                CONSOLE NAME                          02570000
WKARCNA  DS    AL4                -> REPLY CONSOLE NAME/ID              02571000
WKACART  DS    AL4                -> CART                               02572000
WKAWSPRM DS    AL4                -> WAIT STATE PARM LIST               02573000
WKAASCB  DS    AL4                -> ASCB                               02574000
WKARSV30 DS    XL16               RESERVED                              02575000
*                                                                       02576000
WKATEXT  DS    CL130              MESSAGE TEXT (MAXIMUM 126 CHARS) +4   02577000
*                                                                       02578000
ESTAEPRM DS    A                                                        02579000
@TS00001 DS    CL1                                                      02580000
STARTID  DS    CL1                                                      02581000
CVDAREA  DS    D                                                        02582000
*                                                                       02583000
PFLAG    DS    XL1                ADDITIONAL FLAGS                      02584000
REQCMSL  EQU   X'40'              REQUEST OR HOLD CMS LOCK              02585000
*                                                                       02586000
*        THE DSECT HAS BEEN TRUNCATED TO AVOID DUPLICATE NAME ISSUES    02587000
*                                                                       02588000
IEAVMWTO CSECT                                                          02589000
*                                                                       02590000
         IHAWQE DSECT=YES                                               02591000
*                                                                       02592000
         IHACTM WWB                                                     02593000
*                                                                       02594000
         IHACTM FTPT                                                    02595000
*                                                                       02596000
         IHAFRRS                                                        02597000
*                                                                       02598000
         IEZWPL DSECT=YES                                               02599000
*                                                                       02600000
         PRINT NOGEN                                                    02601000
*                                                                       02602000
         IHAASCB                                                        02603000
*                                                                       02604000
         IEFJSSOB  (WT),CONTIG=YES                                      02605000
*                                                                       02606000
         IEFJESCT                                                       02607000
*                                                                       02608000
         IHAPSA                                                         02609000
*                                                                       02610000
         IKJRB   DSECT=YES                                              02611000
*                                                                       02612000
         PRINT GEN                                                      02613000
*                                                                       02614000
*/********************************************************************/ 02615000
*/*                                                                  */ 02616000
*/*          EXTENDED SAVEAREA MAPPING FOR SVC 35                    */ 02617000
*/*                                                                  */ 02618000
*/********************************************************************/ 02619000
*                                                                       02620000
*        DEFINED IN THE RBEXSAVE AREA                                   02621000
*                                                                       02622000
         ORG   RBEXSAVE                                                 02623000
XVSAV    DS    0A                                                       02624000
XVA4     DS    0F                      ERROR PROCESSING FIELDS          02625000
XVFNCT   DS    C                       D23 PROCESS CODE                 02626000
D23VALID EQU   X'10'                   PARMLIST VALIDITY CHECK          02627000
D23OREGC EQU   X'20'                   ORE GETCELL FAILURE              02628000
D23OREBC EQU   X'21'                   ORE BLDCPOOL FAILURE             02629000
D23OREGM EQU   X'22'                   ORE GETMAIN FAILURE, SP231       02630000
D23WQEGC EQU   X'30'                   WQE GETCELL FAILURE              02631000
D23WQEBC EQU   X'31'                   WQE BLDCPOOL FAILURE             02632000
D23WQEGM EQU   X'32'                   WQE GETMAIN FAILURE, SP231       02633000
D23DYN   EQU   X'42'                   DYNAMIC AREA GETMAIN FAILURE     02634000
XVAMOD   DS    C                       D23 MODULE ID                    02635000
VWTOID   EQU   X'01'                   IEAVVWTO'S ID FOR D23            02636000
MWTOID   EQU   X'02'                   IEAVMWTO'S ID FOR D23            02637000
XVA41    DS    C                       RESERVED                         02638000
XVREASON DS    C                       D23 REASON CODE                  02639000
D23BNDY  EQU   X'01'                   WTOR PARMLIST NOT ON WORD BNDY   02640000
D23MLWTR EQU   X'02'                   MULTILINE WTOR SPECIFIED         02641000
D23PARM  EQU   X'03'                   WPL NOT ADDRESSABLE BY USER      02642000
D23ZERO  EQU   X'04'                   ZERO TEXT LENGTH WTOR            02643000
D23SIZE  EQU   X'05'                   CALLER MODIFIED WPL              02644000
*                                      DURING WTO PROCESSING            02645000
D23LTXT  EQU   X'06'                   WTO/WTOR TEXT= CODED AND         02646000
*                                      WPLLGH ^=8                       02647000
*                                                                       02648000
XVA8     DS    AL4                     STORE TIME                       02649000
XVWPLADR DS    AL4                     -> CALLERS WPL                   02650000
XVWQEAD  DS    AL4                                                      02651000
*                                                                       02652000
*        FLAGS                                                          02653000
*                                                                       02654000
XVDFLAGS DS    0XL4                                                     02655000
XVD0     DS    X                                                        02656000
XVD0RPFD EQU   BIT1                    HARD COPY ONLY                   02657000
XVD0NWQE EQU   BIT2                    GET WQE                          02658000
XVD0NORE EQU   BIT3                    GET THE ORE FIRST                02659000
XVD0QID  EQU   BIT4                    QID FIELD IS PRESENT IN THE WPL  02660000
XVD0WWB  EQU   BIT5                    WWB WTO WAIT BLOCK OBTAINED      02661000
XVD0USER EQU   BIT6                                                     02662000
XVD0HDCY EQU   BIT7                                                     02663000
*                                                                       02664000
*        XVDFLAGS+1                                                     02665000
*                                                                       02666000
XVD1     DS    X                                                        02667000
XVD1PRIV EQU   BIT0                    PRIVILEGED USER                  02668000
XVD1ENQW EQU   BIT1                    ENQ'D ON A WQE  (VMWTO)          02669000
XVD1ENQO EQU   BIT2                    ENQ'D ON AN ORE (VMWTO)          02670000
XVD1ALDN EQU   BIT2                    ALL NEEDED CONTROL BLKS OBTAINED 02671000
XVD1PP   EQU   BIT5                    PROBLEM PROGRAM CALLER           02672000
XVD1AUTH EQU   BIT6                    KEY 0, SUPVR STATE OR APF AUTH   02673000
XVD1PERR EQU   BIT7                    SEVERE ERROR FOUND. ABEND USER   02674000
*                                                                       02675000
*        XVDFLAGS+2                                                     02676000
*                                                                       02677000
XVD2     DS    X                                                        02678000
XVD2CON  EQU   BIT0                    CONNECTING                       02679000
XVD2VALD EQU   BIT3                    PARAMETER LIST IS VALID          02680000
XVD2DELW EQU   BIT4                    SEND MSG TO HARDCOPY ONLY        02681000
XVD2ZERO EQU   BIT5                    ZERO MSG ID TO USER              02682000
XVD2WTOR EQU   BIT6                    WTOR REQUEST                     02683000
XVD2QFHC EQU   BIT7                    QUEUE THIS MSG TO HARD COPY      02684000
*                                                                       02685000
*        XVDFLAGS+3                                                     02686000
*                                                                       02687000
XVD3     DS    X                                                        02688000
XVD3BLDJ EQU   BIT0                    BUILD MAJOR WQE                  02689000
XVD3BLD1 EQU   BIT1                    BUILD LINE 1                     02690000
XVD3BLD2 EQU   BIT2                    BUILD LINE 2                     02691000
XVD3TXT1 EQU   BIT3                    TEXT LINE 1 BEING PROCESSED      02692000
XVD3TFX  EQU   BIT4                    TCBTFX WAS SET ON,TURN IT OFF    02693000
*        END OF FLAGS                                                   02694000
XVOREAD  DS    AL4                                                      02695000
         ORG   XVOREAD                                                  02696000
XVX      DS    0F                      USED AS WORK AREA BY VMWTO       02697000
XVX0     DS    X                       LINE CONTROL FLAGS - MLWTO       02698000
XVX0FLCL EQU   BIT0                    FIRST LINE IS CONTROL LINE       02699000
XVX0LL1F EQU   BIT1                    LABEL LINE 1 FOUND               02700000
XVX0LL2F EQU   BIT2                    LABEL LINE 2 FOUND               02701000
XVX0UDCL EQU   BIT3                    USE DEFAULT CONTROL LINE         02702000
XVX0FLJE EQU   BIT4                    FIRST LINE JUST 'E'              02703000
XVX0FEDE EQU   BIT5                    FORCE END (LAST LINE TO BE DE)   02704000
XVX1     DS    X                       ERROR FLAGS - MLWTO              02705000
XVX1STOP EQU   BIT0                    ERROR IN PARM LIST; IGNORE MLWTO 02706000
XVX1NOID EQU   BIT1                    NO ID FOR CONNECTING MLWTO       02707000
XVX2     DS    AL1                     NO OF LINES STILL TO DO          02708000
XVX3     DS    AL1                     NO OF LINES FROM MLWTO EXT HDR   02709000
*                                                                       02710000
XVCMAJOR DS    AL4                  *                                   02711000
XVCMINOR DS    AL4                  V                                   02712000
*                                                                       02713000
XVWWB    DS    AL4                                                      02714000
*                                                                       02715000
XVWQEID  DS    0AL4           *        CALLERS R0                       02716000
XVWQEIDA DS    AL3            |        A NEW LINE IS TO BE              02717000
*                             |        CONNECTED TO THE MESSAGE         02718000
*                             |        WITH THIS 3 BYTE MESSAGE ID      02719000
*                             |        MLWTO ONLY                       02720000
XVCONID  DS    XL1            V        CONSOLE ID FOR THIS MESSAGE      02721000
*                                                                       02722000
XVRET    DS    0AL4                                                     02723000
XVRETCOD DS    AL4                                                      02724000
XVLEN    EQU   *-XVSAV                 MUST NOT EXCEED 48 BYTES AS      02725000
*                                      RB EXTENDED SAVE AREA USED       02726000
*                                                                       02727000
         PRINT NOGEN                                                    02728000
*                                                                       02729000
         IKJTCB  DSECT=YES                                              02730000
*                                                                       02731000
         CVT   DSECT=YES,LIST=YES                                       02732000
*                                                                       02733000
         IHASCVT LIST=YES                                               02734000
*                                                                       02735000
         IEZJSCB                                                        02736000
*                                                                       02737000
         PRINT GEN                                                      02738000
*                                                                       02739000
         IEECUCM FORMAT=NEW                                             02740000
*                                                                       02741000
*                                                                       02742000
R0       EQU   0                                                        02743000
R1       EQU   1                                                        02744000
R2       EQU   2                                                        02745000
R3       EQU   3                                                        02746000
R4       EQU   4                                                        02747000
R5       EQU   5                                                        02748000
R6       EQU   6                                                        02749000
R7       EQU   7                                                        02750000
R8       EQU   8                                                        02751000
R9       EQU   9                                                        02752000
R10      EQU   10                                                       02753000
R11      EQU   11                                                       02754000
R12      EQU   12                                                       02755000
R13      EQU   13                                                       02756000
R14      EQU   14                                                       02757000
R15      EQU   15                                                       02758000
*                                                                       02759000
         END                                                            02760000
./ ADD NAME=IEAVVWTO
VVW      TITLE 'IEAVVWTO - SVC 35 - WTO AND WTOR PROCESSOR'             00001000
*                                                                       00002000
IEAVVWTO CSECT                                                          00003000
*                                                                       00004000
         ENTRY  TRTAB                                                   00005000
*                                                                       00006000
*        STATUS -                                                       00007000
*        LASTUPD         = JCLIN    TYPE=UPD                            00008000
*        LIBRARIES       = DISTLIB=AOSC5                                00009000
*        FMID            = FBB1221                                      00010000
*        RMID            = UZ62088                                      00011000
*        LKED ATTRIBUTES = RENT      REFR                               00012000
*        LMOD            = IGC0003E                                     00013000
*        SYSMOD HISTORY  = SYSMOD   TYPE       DATE   MCS  STATUS       00014000
*                          EBB1102  FUNCTION  00.000  MOD      ACC RGN  00015000
*                          FBB1221  FUNCTION  00.000  MOD      ACC RGN  00016000
*                          UZ60302  PTF       00.000  MOD      ACC RGN  00017000
*                          UZ62088  PTF       74.164  MOD  APP ACC      00018000
*                                                                       00019000
*        ORIGINAL SOURCE FROM MVSSRC.SYM101.F20 WAS UPDATED             00020000
*        BY DISASSEMBLY TO MATCH THE LOAD MODULE AT THE UZ62088         00021000
*        LEVEL.                                                         00022000
*                                                                       00023000
*        MANY INSTALLATIONS HAVE APPLIED USERMOD TMVS805 CONTRIBUTED    00024000
*        BY KEVIN LEONARD AS THIS USERMOD SIGNIFICANTLY INCREASES       00025000
*        THE USEFULNESS OF THE IEECVXIT TO AUTOMATE OPERATIONS BY       00026000
*        ALLOWING IEECVXIT TO -                                         00027000
*                (1) SEE "INTERNAL" MESSAGES;                           00028000
*                (2) PREVENT HARDCOPY OF DELETED MESSAGES.              00029000
*        THIS USERMOD ZP60039 CHANGES THE OFFSET OF THE CODE BEING      00030000
*        ZAPPED BY TMVS805 SO TMVS805 WOULD REQUIRE REWORK TO BE        00031000
*        APPLIED TO ZP60039. HOWEVER AS TMVS805 IS SO COMMONLY USED     00032000
*        IT HAS BEEN INCLUDED IN ZP60039.                               00033000
*        ZP60039 THEREFORE SUPERCEDES TMVS805.                          00034000
*        FOR THOSE USERS WHO DO NOT WISH TO INCLUDE THE FUNCTIONALITY   00035000
*        OF TMVS805 IN THIS USERMOD THEN SET THE VARIABLE &TMVS805      00036000
*        TO ZERO BEFORE RECEIVING AND APPLYING THIS USERMOD.            00037000
*                                                                       00038000
         LCLB  &TMVS805                                                 00039000
&TMVS805 SETB  1             <------- ENABLE TMVS805 FUNCTIONALITY      00040000
*                                                                       00041000
*        CHANGE ACTIVITY -                                              00042000
*               ZP60039  - 1. SUPPORT FOR THE EXTENDED WPL              00043000
*                             PARAMETER LIST TO ENABLE THE USE OF       00044000
*                             THE TEXT OPERAND                          00045000
*                          2. THE WPL OR XWPL IS NOW COPIED TO          00046000
*                             PROTECTED STORAGE AFTER VALIDATION        00047000
*                             FOR AUTHORIZED CALLERS WHEREAS            00048000
*                             PREVIOUSLY IT WAS ONLY COPIED FOR NON     00049000
*                             AUTHORIZED USERS                          00050000
*                          3. INCLUDE USERMOD TMVS805 (OPTIONAL)        00051000
*                          4. NUMEROUS CHANGES TO IMPROVE SOURCE        00052000
*                             READABILITY                               00053000
*                          5. LOCALIZED CODE OPTIMZATION                00054000
*                          6. THE IEAVVWTO CODE AT THE UZ62088 LEVEL    00055000
*                             FAILED TO HANDLE AN INVALID WPL PASSED    00056000
*                             BY THE CALLER RESULTING IN MULTIPLE       00057000
*                             IEAVMFRR DUMPS. THIS HAS BEEN RESOLVED.   00058000
*                             AN INVALID WPL NOW RESULTS IN A           00059000
*                             D23 ABEND WITH A REASON CODE IN R15       00060000
*                             AS PER THE DESCRIPTION PROVIDED IN        00061000
*                             GC38-1008 OS/VS ML: SYSTEM CODES          00062000
*                             ABEND D23                                 00063000
*                          7. TRANSLATION OF UNPRINTABLE CHARACTERS     00064000
*                             IN WPL TEXT TO HEX A1 (TILDE CHARACTER)   00065000
*               18 AUG 20  -  TRUNCATE MESSAGE TEXT THAT EXCEEDS        00066000
*                             THE REMAINING AVAILABLE TEXT BYTES        00067000
*                             AFTER THE SPECIFIC MESSAGE PREFIX         00068000
*                             CHARS HAVE BEEN PLACED IN THE WQE.        00069000
*                             LATER OS VERSIONS IMPLEMENT THE SAME      00070000
*                             TRUNCATION HOWEVER THE TRUNCATION         00071000
*                             IS LOGGED WITH A D23 ABEND IN LOGREC      00072000
*                             ONLY. THE WTO/WTOR/WTL ISSUER IS NOT      00073000
*                             ABENDED.                                  00074000
*                             THIS USERMOD PERFORMS A SILENT            00075000
*                             TRUNCATION WITH NO INDICATION OF THE      00076000
*                             TRUNCATION AND NO LOGREC RECORDING.       00077000
*        FUNCTION -                                                     00078000
*        SVC 35 WTO AND WTOR PROCESSOR                                  00079000
*        1. SETUP THE RECOVERY/TERMINATION ENVIRONMENT                  00080000
*        2. DETERMINE THE STATUS OF THE CALLER - PP/APF/SUP             00081000
*        3. DETERMINE THE FORMAT OF THE CALLER PROVIDED WPL.            00082000
*           THERE ARE A NUMBER OF WTO AND WTOR WPL PARAMETER FORMATS    00083000
*           FOR WTO THERE ARE -                                         00084000
*           1. STANDARD 370 WTO WPL                                     00085000
*           2. EXTENDED WPL VERSION 1                                   00086000
*           3. EXTENDED WPL VERSION 2 (SUPPORT FOR TEXT= PARAMETER)     00087000
*           FOR WTOR THERE ARE -                                        00088000
*           1. STANDARD 370 WTOR WPL                                    00089000
*           2. 31 BIT WTOR WPL                                          00090000
*           3. EXTENDED WPL VERSION 1                                   00091000
*           4. EXTENDED WPL VERSION 2 (SUPPORT FOR TEXT= PARAMETER)     00092000
*        4. VALIDATE CALLER ACCESS TO THE WPL FIELDS AND VALIDATE       00093000
*           WPL PARAMETERS FOR CONSISTENCY                              00094000
*        5. MOVE PARAMETERS FROM CALLER PROVIDED WPL TO PROTECTED       00095000
*           STORAGE                                                     00096000
*        6. VALIDATE KEY PARAMETERS MOVED TO PROTECTED STORAGE WERE     00097000
*           NOT CHANGED IN THE CALLERS WPL DURING THE MOVE.             00098000
*           THIS STEP IS REQUIRED TO PREVENT ANY INTEGRITY EXPOSURES    00099000
*                                                                       00100000
*        NOTES -                                                        00101000
*                                                                       00102000
*        DUE TO THE LIMITATIONS OF THE PARAMETER LIST PASSED            00103000
*        TO IEECVXIT BY IEAVVWTO SOME IEECVXIT EXITS MAKE               00104000
*        ASSUMPTIONS ABOUT THE CONTENTS OF THE REGISTERS INHERITED      00105000
*        BY IEECVXIT THAT ARE NOT DEFINED BY THE DOCUMENTED             00106000
*        IEECVXIT PARAMETER LIST.                                       00107000
*                                                                       00108000
*        THE IEECVXIT PROVIDED AS A COMPONENT OF THE BSPPILOT           00109000
*        USERMOD ZUM0003 CONTRIBUTED BY VOLKER BANDKE WILL              00110000
*        AUTOMATICALLY REPLY TO SOME WTOR MESSAGES.                     00111000
*                                                                       00112000
*        TO ENSURE THIS IEECVXIT CODE HAS LOCATED THE CORRECT ORE       00113000
*        FOR A REPLY THE CODE CAPTURES THE USER PROVIDED ECB            00114000
*        ADDRESS TO BE POSTED BY THE OPERATOR REPLY AND USES IT         00115000
*        AS A VALIDATION OF THE CORRECT ORE. THE CODE IN THE            00116000
*        BSPPILOT IEECVXIT ASSUMES THAT R5 WILL POINT TO THE SVRB       00117000
*        FOR SVC 35 AND THAT THE ADDRESS OF THE WPL PASSED TO           00118000
*        SVC 35 WILL BE LOCATED IN THE SVRB+X'74'. THE ADDRESS OF       00119000
*        THE ECB FOR A WTOR WILL BE LOCATED AT AN OFFSET 4 BYTES        00120000
*        INTO THE WPL. FOR AN XWPL THIS IS NOT CORRECT.                 00121000
*                                                                       00122000
*        THEREFORE TO ENSURE THE IEECVXIT PROVIDED BY USERMOD           00123000
*        ZUM0003 WILL CONTINUE TO FUNCTION CORRECTLY WHEN THIS          00124000
*        VERSION OF IEAVVWTO IS INSTALLED R5 IS INITIALIZED TO A        00125000
*        VALUE THAT WHEN PROCESSED BY THE IEECVXT CODE THE              00126000
*        IEECVXIT CODE WILL HAVE OBTAINED THE ECB ADDRESS.              00127000
*                                                                       00128000
*        THERE ARE OTHER IEECVXIT ROUTINES PROVIDED AS USERMODS.        00129000
*        THE CODE IN THEIR IEECVXIT ROUTINES WILL HAVE TO BE INSPECTED  00130000
*        TO ENSURE IT COMPLIES WITH THE DOCUMENTED IEECVXIT             00131000
*        PARAMETER LIST. IF DATA NOT PROVIDED IN THE DOCUMENTED         00132000
*        IEECVXIT PARAMETER IS REQUIRED THEN R9 HAS THE ADDRESS         00133000
*        OF THE IEAVVWTO WORKAREA WHICH CONTAINS ALL THE                00134000
*        REQUIRED DATA.                                                 00135000
*                                                                       00136000
*        KNOWN IEEVCXIT USERMODS -                                      00137000
*                                                                       00138000
*        USERMOD    COMPLIANT  ACTION                                   00139000
*        -------    ---------  -------------------------------------    00140000
*        ZP60001    YES        NO ACTION REQUIRED                       00141000
*        ZUM0003    NO         CODE PROVIDED IN IEAVVWTO TO SUPPORT     00142000
*                              THIS USERMOD SO NO CHANGE IS REQUIRED    00143000
*                              TO ZUM0003                               00144000
*                                                                       00145000
*        STANDARD SVC REGISTER CONVENTIONS -                            00146000
*        ON ENTRY -                                                     00147000
*        R0  USED WHEN THE CALLER ADDS ADDITIONAL MULTIPLE LINE         00148000
*            WTO MESSAGES TO A PREVIOUS STRING OF EXISTING MESSAGES,    00149000
*            IE THE LONG MLWTO ISSUED IN REPLY FOR A D U,DASD COMMAND.  00150000
*            IT CONTAINS THE MESSAGE IDENTIFICATION OF THE ORIGINAL     00151000
*            MESSAGE. R0 ALSO CONTAINS THE UCMID FOR ANY PROGRAM        00152000
*            SPECIFYING REG0 AND FOR PRIVILEGED PROGRAMS SPECIFYING     00153000
*            QREG0.                                                     00154000
*            HI-ORDER 3 BYTES, MESSAGE ID                               00155000
*            LOW ORDER BYTE, CONSOLE ID                                 00156000
*        R1  CALLER INPUT PARAMETER REGISTER (-> WPL OR XWPL)           00157000
*        R2     UNDEFINED                                               00158000
*        R3  -> CVT                                                     00159000
*        R4  -> TCB                                                     00160000
*        R5  -> SVRB                                                    00161000
*        R6  -> SVC ENTRY POINT                                         00162000
*        R7  -> ASCB                                                    00163000
*        R8-R12 UNDEFINED                                               00164000
*        R13    CALLERS VALUE                                           00165000
*        R14    RETURN ADDR TO SVC FLIH EXIT CODE                       00166000
*        R15    CALLERS VALUE                                           00167000
*                                                                       00168000
*        ON EXIT -                                                      00169000
*        R1     MESSAGE IDENTIFICATION NUMBER                           00170000
*        R15    RETURN CODE (ONLY FOR ML-WTO)                           00171000
*                                                                       00172000
         USING *,R6                                                     00173000
         USING CVT,R3                                                   00174000
         USING TCB,R4                                                   00175000
         USING RBBASIC,R5                                               00176000
         USING ASCB,R7                                                  00177000
         B     AROUND                                                   00178000
*                                                                       00179000
         DC    C'IEAVVWTO ZP60039 &SYSDATE &SYSTIME'                    00180000
*                                                                       00181000
AROUND   LA    R11,0(,R6)             CLEANUP R11                       00182000
         LA    R12,2048(,R11)                                           00183000
         LA    R12,2048(,R12)                                           00184000
         DROP  R6                                                       00185000
         USING IEAVVWTO,R11,R12                                         00186000
         XC    RBEXSAVE,RBEXSAVE      ENSURE ENTIRE RBEXSAVE IS ZERO    00187000
*                                                                       00188000
*        SAVE THE CONTENTS OF R0, R1 AND R14                            00189000
*                                                                       00190000
         ST    R0,XVWQEID             SAVE R0 IN EXTENDED SAVEAREA      00191000
*                                     IN CASE A MESSAGE NO/CONSOLE ID   00192000
*                                     HAS BEEN PROVIDED FOR A MLWTO     00193000
         STCM  R1,B'0111',XVWPLADR+1  SAVE R1 IN EXTENDED SAVEAREA      00194000
         STCM  R14,B'0111',XVRET+1    SAVE R14 IN EXTENDED SAVEAREA     00195000
*                                                                       00196000
*        GETMAIN WORKAREA FROM SUBPOOL 229                              00197000
*        INITIALIZE IT TO ZERO                                          00198000
*                                                                       00199000
         L     R0,@SIZDATD                                              00200000
         LR    R9,R0                                                    00201000
*                                                                       00202000
         GETMAIN  R,LV=(0)                                              00203000
*                                                                       00204000
         LR    R8,R1                                                    00205000
         SR    R15,R15                ZERO PAD CHAR AND SOURCE LENGTH   00206000
         MVCL  R8,R14                 ZERO WORKAREA                     00207000
         LR    R9,R1                                                    00208000
         USING @DATD,R9                                                 00209000
         ST    R13,@SA00001+4                                           00210000
         LR    R13,R9                                                   00211000
*                                                                       00212000
*/********************************************************************/ 00213000
*/*                                                                  */ 00214000
*/*  SET UP THE ESTAE ENVIRONMENT                                    */ 00215000
*/*                                                                  */ 00216000
*/*  SET THE FIRST FOOTPRINT FOR IEAVVWTO                            */ 00217000
*/*                                                                  */ 00218000
*/********************************************************************/ 00219000
*   R2=UCMFRRAD;                    /* ADDR OF IEAVMFRR, OUR RECOVERY   00220000
*                                      ROUTINE                       */ 00221000
         L     R2,CVTCUCB                                               00222000
         USING UCM,R2                                                   00223000
         L     R2,UCMFRRAD             R2 -> COMM TASK FRR IEAVMFRR     00224000
         DROP  R2                                                       00225000
*   REGRECOV=ESTAELST;              /* MOVE ESTAE PARM LIST TO WORK     00226000
*                                      AREA                          */ 00227000
         MVC   REGRECOV(ESTAELEN),ESTAELST                              00228000
*   R1=ADDR(REGRECOV);              /* GET ADDR OF ESTAE PARM LIST      00229000
*                                      AREA                          */ 00230000
         LA    R1,REGRECOV                                              00231000
*                                                                       00232000
*   R8=ADDR(EPARM);                 /* PROVIDE ADDR OF ESTAE PARM       00233000
*                                      AREA                          */ 00234000
         LA    R10,EPARM                                                00235000
*                                   /* SET UP ESTAE ENVIRONMENT USING*/ 00236000
*                                   /* COMM TASK FRR FOR RECOVERY    */ 00237000
         ESTAE (R2),CT,PARAM=(R10),RECORD=YES,MF=(E,(1))                00238000
*                                                                       00239000
         USING PARMLIST,R10         /* SET ADDR TO ESTAE PARM AREA   */ 00240000
*                                                                       00241000
*        INITIALIZE ESTAE PARM AREA                                     00242000
*                                                                       00243000
*   PARMRGAD=ADDR(REGSAVE);         /* R6 -> REG RESTORE FLAGS       */ 00244000
         LA    R6,REGSAVE           /*       AND SAVE AREA           */ 00245000
         ST    R6,PARMRGAD                                              00246000
*   PARMID=MODULEID;                /* IDENTIFY MODULE TO FRR        */ 00247000
         MVC   PARMID,KCVWTO                                            00248000
*   REGRGMAP=ALLREGS;               /* RESTORE ALL REGS AT RETRY     */ 00249000
         MVC   REGRGMAP,KXFFFF                                          00250000
*                                   /* SAVE REGS FOR RETRY IF NEEDED */ 00251000
         STM   R0,R15,REGRECOV                                          00252000
*   PARMFTPT=FTVALCHK;              /* SET FOOT PRINT TO SHOW           00253000
*                                      WORKING ON CHECKING THE          00254000
*                                      VALIDITY OF THE USER'S WPL    */ 00255000
         MVI   PARMFTPT,X'01'                                           00256000
*   PARMRTAD=0;                     /* ZERO RETRY ADDR               */ 00257000
         XC    PARMRTAD,PARMRTAD                                        00258000
*/*******************************************************************   00259000
*/*                                                                     00260000
*/*      SET UP THE EXTENDED SAVEAREA IN THE SVRB                       00261000
*/*                                                                     00262000
*/*******************************************************************   00263000
*        INPUT -                                                        00264000
*        XVWPLADR -> WPL, USERS PARM LIST (R1)                          00265000
*        XVWQEID   = THE CONTENTS OF R0 MESSAGE NUMBER/CONSOLE ID       00266000
*        XVRET     = RETURN ADDR                                        00267000
*        R3 -> CVT                                                      00268000
*        R4 -> TCB                                                      00269000
*        R5 -> SVRB WHICH CONTAINS THE EXTENDED SAVEAREA                00270000
*        R7 -> ASCB                                                     00271000
*                                                                       00272000
*        OUTPUT -                                                       00273000
*        THE EXTENDED SAVE AREA IS INITIALIZED                          00274000
*        VARIOUS BITS OR SETTINGS IN THE PARM LIST NOW APPEAR IN XSA    00275000
*        R6 -> MESSAGE PART OF THE WPL                                  00276000
*        XVA8 CONTAINS THE TIME IN DECIMAL FORMAT AS HHMMSSTH           00277000
*                                                                       00278000
*        THE MESSAGE LENGTH IS SAVED FOR INTEGRITY REASONS              00279000
*        THE LENGTH IS USED TO COMPUTE THE LENGTH OF THE PARM LIST      00280000
*        BY SAVING THE LENGTH, THE USER CANNOT CHANGE THE               00281000
*        LENGTH AFTER THE VALIDITY OF THE PARM LIST HAS BEEN CHECKED    00282000
*        LIST                                                           00283000
*                                                                       00284000
*/*******************************************************************   00285000
*                                                                       00286000
*/********************************************************************/ 00287000
*                                                                       00288000
*        DETERMINE IF THE CALLER IS AUTHORIZED                          00289000
*        DETERMINE IF THE CALLER IS A PROBLEM PROGRAM                   00290000
*        THIS INFO IS USED TO SELECT THE PROPER MESSAGE FLAGGING        00291000
*        AND ERROR RECOVERY PROCEDURES                                  00292000
*                                                                       00293000
*/********************************************************************/ 00294000
         TESTAUTH  STATE=YES,KEY=YES                                    00295000
*   IF R15^=0 THEN                  /* USER NOT STATE OR KEY            00296000
*                                      AUTHORIZED ?                  */ 00297000
         LTR   R15,R15                                                  00298000
         BZ    @RF00171                                                 00299000
*     DO;                           /* YES, THE USER IS A PROBLEM       00300000
*                                      PROGRAM                       */ 00301000
*       XVD1PP='1'B;                /* TURN ON PROBLEM PROGRAM BIT      00302000
*                                      IN XSA                        */ 00303000
         OI    XVD1,XVD1PP                                              00304000
*                                   /* CHECK IF APF AUTHORIZED       */ 00305000
         TESTAUTH FCTN=1                                                00306000
*       IF R15=0 THEN               /* USER APF AUTHORIZED ?         */ 00307000
         LTR   R15,R15                                                  00308000
         BNZ   @RC00174             /* NO, DO NOTHING                */ 00309000
*         XVD1AUTH='1'B;            /* YES, TURN ON AUTHORIZED BIT IN   00310000
*                                      XSA                           */ 00311000
@RF00171 OI    XVD1,XVD1AUTH                                            00312000
*                                                                       00313000
*/********************************************************************/ 00314000
*                                                                       00315000
*        CHECK IF USER IS A PRIVILEGED TASK                             00316000
*        A PRIVILEGED USER IS -                                         00317000
*        A. THE COMMUNICATIONS TASK                                     00318000
*        B. ANY TASK RUNNING UNDER AN SIRB                              00319000
*        C. A DAUGHTER TASK OF THE COMMUNICATIONS TASK                  00320000
*        D. AN AUTHORIZED SUBSYSTEM SUCH AS JES3                        00321000
*        A PRIVILEGED USER MAY PUT OUT A WTO ANY TIME                   00322000
*        IT DOES NOT HAVE TO WAIT FOR SPACE IF ALL THE WQES ARE         00323000
*        TAKEN. FIRST CHECK IF THE USER IS COMMTASK                     00324000
*                                                                       00325000
*        NOTE -                                                         00326000
*        THE SPECIAL CHECK FOR A DAUGHTER TASK IS ADDED TO PERMIT       00327000
*        TASK ATTACHED BY THE COMMUNICATIONS TASK TO EXCEED THE         00328000
*        WQE LIMITS IF NECESSARY                                        00329000
*                                                                       00330000
*/********************************************************************/ 00331000
*                                                                       00332000
*   IF ASCBASID=UCMCTID&            /* IN THE COMTASK'S MEMORY ?     */ 00333000
*       R4=UCMPXA                   /* AND COMTASK'S TCB             */ 00334000
*       UCMPXA=TCBOTC THEN          /* OR DAUGHTER TASK              */ 00335000
@RC00174 L     R15,CVTCUCB                                              00336000
         USING UCM,R15                                                  00337000
         CLC   ASCBASID,UCMCTID        COMPARE CURRENT ASID WITH        00338000
         BNE   @GL00001                COMTASK ASIB                     00339000
         C     R4,UCMPXA               COMPARE CURRENT TCB WITH         00340000
         BE    @RT00184                COMTASK TCB                      00341000
@GL00001 CLC   UCMPXA,TCBOTC           COMPARE COMTASK TCB WITH         00342000
         BNE   @RF00184                PARENT OF CURRENT TCB            00343000
         DROP  R15                                                      00344000
*     XVD1PRIV='1'B;                /* YES, THEN INDICATE A             00345000
*                                      PRIVILEGED TASK               */ 00346000
@RT00184 OI    XVD1,XVD1PRIV                                            00347000
         B     @RC00184                                                 00348000
*                                                                       00349000
*   ELSE                            /* NO, CHECK IF THIS RB IS AN       00350000
*                                      SIRB                          */ 00351000
*     DO;                           /* IS THE USER'S RB AN SIRB         00352000
*/********************************************************************/ 00353000
*                                                                       00354000
*        IF THE TASK IS NOT RUNNING IN SIRB MODE, THEN CHECK IF         00355000
*        SVC 35 WAS ENTERED TO ISSUE A LOG MESSAGE, OR SUBSYSTEM        00356000
*                                                                       00357000
*/********************************************************************/ 00358000
*       IF RBLINK->RBFTP='100'B TCBTID='F8'X THEN                       00359000
@RF00184 L     R15,RBLINK                                               00360000
         TM    RBSTAB1-RBBASIC(R15),RBFTSIRB                            00361000
         BNO   @GL00003                                                 00362000
         TM    RBSTAB1-RBBASIC(R15),RBFTTIRB                            00363000
         BZ    @RT00187                                                 00364000
@GL00003 CLI   TCBTID,TCBLOGID         LOG TASK ?                       00365000
         BNE   @RF00187                                                 00366000
*         XVD1PRIV='1'B;            /* YES, INDICATE A PRIVILEGED       00367000
*                                      TASK                          */ 00368000
@RT00187 OI    XVD1,XVD1PRIV                                            00369000
         B     @RC00184                                                 00370000
*                                                                       00371000
*       ELSE                                                            00372000
*         IF UCMJES3T=R7 THEN       /* IF SUBSYSTEM ISSUED WTO       */ 00373000
@RF00187 L     R15,CVTCUCB                                              00374000
         USING UCM,R15                                                  00375000
         C     R7,UCMJES3T                                              00376000
         BNE   IEA00152                                                 00377000
*           XVD1PRIV='1'B;          /* SET PRIVILEGED BIT            */ 00378000
         OI    XVD1,XVD1PRIV                                            00379000
         B     @RC00184                                                 00380000
*                                                                       00381000
*     END;                                                              00382000
*     END;                                                              00383000
IEA00152 LH    R8,UCMWQNR              R8 = CURRENT WQE COUNT           00384000
         CH    R8,UCMWQLM              COMPARE TO WQE BUFFER LIMIT      00385000
         BL    @RC00184                LESS THAN LIMIT, BRANCH          00386000
         L     R2,RBLINK               R2 -> PREVIOUS RB                00387000
         LA    R2,0(,R2)               CLEAR HI-ORDER BYTE              00388000
         B     IEA00178                                                 00389000
*                                                                       00390000
         DROP  R15                                                      00391000
IEA0016E L     R15,RBLINK-RBBASIC(,R2)                                  00392000
         LA    R2,0(,R15)              ZERO HI-ORDER BYTE               00393000
IEA00178 TM    RBSTAB1-RBBASIC(R2),RBFTIRB  IRB ?                       00394000
         BNO   IEA00188                                                 00395000
         TM    RBSTAB1-RBBASIC(R2),RBFTSIRB+RBFTTIRB-RBFTIRB            00396000
         BZ    IEA0018E                                                 00397000
IEA00188 CR    R2,R4                                                    00398000
         BNE   IEA0016E                                                 00399000
IEA0018E CR    R2,R4                                                    00400000
         BE    @RC00184                                                 00401000
*     XVD1PRIV='1'B;                /* THEN USER IS PRIVILEGED TASK  */ 00402000
         OI    XVD1,XVD1PRIV                                            00403000
*/********************************************************************/ 00404000
*/*                                                                  */ 00405000
*/*  RECORD THE TIME AND SAVE IT IN XVA8                             */ 00406000
*/*                                                                  */ 00407000
*/********************************************************************/ 00408000
*                                   /* SAVE THE REGS AT THIS POINT IN*/ 00409000
*                                   /* CASE THERE IS AN ERROR IN TIME*/ 00410000
*                                   /* SVC                           */ 00411000
@RC00184 STM   R0,R15,REGRECOV                                          00412000
*   PARMFTPT=FTTIME;                /* TIME WAS TAKEN                */ 00413000
         MVI   PARMFTPT,X'02'                                           00414000
*   PARMRTAD=ADDR(TIMERET);                                             00415000
         LA    R8,TIMERET                                               00416000
         ST    R8,PARMRTAD                                              00417000
*                                   /* TAKE THE TIME                 */ 00418000
         TIME  DEC,ERRET=TIMERET                                        00419000
*   XVA8=R0;                        /* SAVE THE TIME IN XVSAV AREA   */ 00420000
         ST    R0,XVA8                                                  00421000
*TIMERET:                           /* IF ERROR IN TIME THEN DONT       00422000
*                                      STORE R0                      */ 00423000
*   PARMFTPT=FTVALCHK;              /* RESUME CHECK OF WPL           */ 00424000
TIMERET  MVI   PARMFTPT,X'01'                                           00425000
*   PARMRTAD=0;                     /* ZERO RETRY ADDR               */ 00426000
         XC    PARMRTAD,PARMRTAD                                        00427000
         NI    PFLAG,255-NOCMSLOK      TURN OFF NO CMS LOCK REQUESTED   00428000
         OI    XVD2,XVD2VALD           SET ON THE DEFAULT OF THE        00429000
*                                      WPL BEING VALID                  00430000
*/********************************************************************/ 00431000
*/*                                                                  */ 00432000
*/*      SETUP ENVIRONMENT FOR PROCESSING THE WPL                    */ 00433000
*/*                                                                  */ 00434000
*/********************************************************************/ 00435000
*                                                                       00436000
*        OBTAIN THE LOCKS FOR BOTH AUTHORIZED AND                       00437000
*        NON AUTHORIZED CALLERS TO ALLOW FOR A BRANCH ENTRY             00438000
*        TO GETMAIN TO OBTAIN A STORAGE AREA FOR A LARGE                00439000
*        ML-WTO WPL OR XWPL                                             00440000
*                                                                       00441000
*               IF XVD1AUTH='0'B THEN/* CALLER AUTHORIZED ?          */ 00442000
*        TM    XVD1,XVD1AUTH                                            00443000
*        NOP   IEA001F4                YES, BRANCH                      00444000
         OI    PFLAG,NOCMSLOK          TURN ON NO CMS LOCK REQUESTED    00445000
*                                                                       00446000
*        OBTAIN THE LOCAL AND CMS LOCK                                  00447000
*        INSTALL AN FRR                                                 00448000
*        THE FRR HAS CREATED ITS OWN PARMLIST                           00449000
*        NOTE - NO RETRY ADDR IS PROVIDED IN THE FRR PARMLIST           00450000
*                                                                       00451000
         BAL   R14,SETLOCK                                              00452000
         NI    PFLAG,255-NOCMSLOK      TURN OFF NO CMS LOCK REQUESTED   00453000
*                                                                       00454000
*        PRIOR TO ACESSING THE CALLERS WPL/XWPL SETUP THE ESTAE         00455000
*        EXIT ROUTINE TO CATCH AND IDENTIFY ANY ERRORS IF THE           00456000
*        WPL/XWPL IS NOT ADDRESSABLE BY THE CALLER.                     00457000
*                                                                       00458000
*        ROUTINE VWTOVALR IS SET AS THE PARMLIST RETRY ADDR.            00459000
*        THE NO DUMP OPTION IS TURNED ON IN THE FRR PARMLIST            00460000
*        AS ANY ERROR IN ACCESSING OR PROCESSING THE WPL PROVIDED       00461000
*        BY THE CALLER IS NOT A SYSTEM ERROR BUT A USER ERROR           00462000
*        AND WILL BE REFLECTED TO THE CALLER WITH A D23 ABEND           00463000
*        WITH A REASON CODE IN R15                                      00464000
*                                                                       00465000
IEA001F4 STM   R0,R15,REGRECOV                                          00466000
         LA    R15,VWTOVALR            R15 -> WPL ERROR ROUTINE         00467000
         ST    R15,PARMRTAD            SAVE ADDR OF RTN IN FRR PARMLIST 00468000
         OI    PARMFLAG,PARMNDMP       SUPRESS DUMPS                    00469000
         MVI   PARMFTPT,X'01'       /* CHECKING OF WPL               */ 00470000
*                                                                       00471000
*                                   /* USE R6 TO POINT AT PARTS OF      00472000
*                                      PARM LIST                     */ 00473000
*                                   /* R6 IS BASE FOR WPL            */ 00474000
*   R6=XVWPLADR;                    /* PICK UP ADDR OF PARM LIST     */ 00475000
         L     R6,XVWPLADR             R6 = CALLERS R1 -> WPL/XWPL      00476000
         USING WPLRF,R6                                                 00477000
*                                                                       00478000
*        CHANGE TO USERS KEY PRIOR TO ACCESSING THE WPL/XWPL            00479000
*                                                                       00480000
         MODESET EXTKEY=RBT234,WORKREG=15                               00481000
*                                                                       00482000
*********************************************************************** 00483000
*                                                                       00484000
*        DETERMINE THE FORMAT OF THE PARAMETER LIST                     00485000
*        THERE ARE A NUMBER OF WTO AND WTOR WPL/XWPL PARAMETER LISTS    00486000
*        FOR WTO THERE ARE -                                            00487000
*        1. STANDARD 370 WTO WPL                                        00488000
*        2. EXTENDED WPL VERSION 1                                      00489000
*        3. EXTENDED WPL VERSION 2 (SUPPORT FOR TEXT= PARAMETER)        00490000
*        FOR WTOR THERE ARE -                                           00491000
*        1. STANDARD 370 WTOR WPL                                       00492000
*        2. 31 BIT WTOR WPL                                             00493000
*        3. EXTENDED WPL VERSION 1                                      00494000
*        4. EXTENDED WPL VERSION 2 (SUPPORT FOR TEXT= PARAMETER)        00495000
*                                                                       00496000
*        A CHECK IS MADE BY CHECKING THE WPLRLN (THE WTOR REPLY         00497000
*        LENGTH) FOR ZERO. IF THE WPL IS FOR A WTOR THEN WPLRLN         00498000
*        WILL BE THE FIRST BYTE IN THE PARM LIST. WPLRLN MUST           00499000
*        ALWAYS BE NON ZERO IF THE WPL IS FOR A WTOR A NON ZERO         00500000
*        VALUE DETERMINES THAT THE WPL IS A WTOR REQUEST. IF THE        00501000
*        HI-ORDER BIT IS ON THEN WPL IS A WTOR 31 BIT WPL. IF THE       00502000
*        WPL IS FOR A WTO THEN THERE WILL BE NO WTO PREFIX AND          00503000
*        WPLLGH WILL BE THE FIRST 2 BYTES. THE TEXT HAS A MAX           00504000
*        LENGTH OF 128 BYTES SO THE FIRST BYTE OF WPLLGH WILL BE        00505000
*        ZERO. AN EXTENDED WPL (XWPL) FOR EITHER A WTO OR A WTOR        00506000
*        IS INDICATED BY AN MCS FLAG. NO FLAGS CAN BE SET IN            00507000
*        IEAVVWTO STORAGE AT THIS STAGE BECAUSE THE KEY IS SET TO       00508000
*        THE CALLERS KEY TO ENSURE THE CALLER HAS VALID ACCESS TO       00509000
*        THE CALLER PROVIDED WPL/XWPL.                                  00510000
*                                                                       00511000
*********************************************************************** 00512000
*                                                                       00513000
         XR    R15,R15                                                  00514000
         IC    R15,WPLLGH              REFERENCE FIRST CHAR IN WPL/XWPL 00515000
         LTR   R15,R15                                                  00516000
         BZ    VALWPL01                ZERO, WTO WPL/XWPL OR WTOR XWPL  00517000
*                                                                       00518000
*        WTOR 24/31 BIT WPL PROCESSING                                  00519000
*                                                                       00520000
         LM    R14,R0,WPLRPTR          VALIDATE CALLER ACCESS TO -      00521000
*                                       - REPLY ADDRESS                 00522000
*                                       - ECB ADDRESS                   00523000
*                                       - L'TEXT AND MCS FLAGS          00524000
         LA    R6,8(,R6)               WTOR, SO INCR R6 PAST WTOR FLDS  00525000
         B     VALWPL03                                                 00526000
*                                                                       00527000
*        FIRST BYTE OF WPL/XWPL IS ZERO                                 00528000
*        DETERMINE IF THE PARAMETER LIST IS:-                           00529000
*        A. 24 BIT WTO  -OR-                                            00530000
*        B. XWPL (VERSION 1 OR VERSION 2) WTO OR WTOR                   00531000
*                                                                       00532000
VALWPL01 TM    WPLMCSF2,WPLMCSFL       XWPL PRESENT ?                   00533000
         BO    XWPL001                 YES, BRANCH, TEST FOR WTO/WTOR   00534000
*                                                                       00535000
*        PROCESS WPL FOR BOTH WTO AND WTOR                              00536000
*        R6 -> WPLLGH                                                   00537000
*                                                                       00538000
VALWPL03 ICM   R15,B'1111',0(R6)       VERIFY ACCESS TO WPL FIELDS      00539000
         LR    R8,R6                   TAKE A COPY OF -> WPL @ WPLLGH   00540000
*                                      IE AFTER THE WTOR FIELDS         00541000
         SLR   R15,R15                                                  00542000
         IC    R15,WPLLGH+1            PICK UP L'TEXT, IGNORE L'REPLY   00543000
*                                      WHICH MAY BE PRESENT IF WTOR 31  00544000
*                                                                       00545000
         AR    R8,R15                  ADD L'MSG TEXT + MCS FLAGS       00546000
*                                      R8 -> FIRST BYTE PAST MSG TEXT   00547000
         LR    R2,R8                   R2/R8-> FIRST BYTE PAST WPL TEXT 00548000
*                                                                       00549000
         TM    WPLMCSF,WPLMCSFA        DESCR/ROUTE CODES PRESENT ?      00550000
         BZ    IEA00244                NO, BRANCH                       00551000
         ICM   R15,B'1111',0(R8)       ATTEMPT TO ACCESS DESC/ROUTE FLD 00552000
         TM    WPLMCSF,WPLMCSFD        MSGTYP TYPE FIELD EXISTS ?       00553000
         BZ    IEA00244                NO, BRANCH                       00554000
*                                      MSGTYP CAUSES THE AUTOMATIC      00555000
*                                      GEN OF ROUTE AND DESC FIELDS     00556000
         ICM   R15,B'0011',4(R8)       VERIFY ACCESS TO MSGTYP FIELD    00557000
*                                                                       00558000
*        THE WPL FIELDS, TEXT LENGTH, MCS FLAGS, DESCRIPTOR AND         00559000
*        ROUTING CODES HAVE BEEN ACCESSED USING THE CALLERS KEY         00560000
*        FOR WTO/WTOR/WTOR31 FORMAT WPLS                                00561000
*                                                                       00562000
*        RETURN TO KEY ZERO TO COPY THESE FIELDS TO PROTECTED           00563000
*        STORAGE                                                        00564000
*                                                                       00565000
IEA00244 MODESET  EXTKEY=SUPR                                           00566000
*                                                                       00567000
         C     R6,XVWPLADR             WPL ADDR = CALLERS R1 VALUE ?    00568000
*                                      IE R6 NOT INCREMENTED OVER       00569000
*                                      WTOR FIELDS AT BEGINNING OF WPL  00570000
         BE    IEA00262                YES, PROCESS WTO                 00571000
*                                                                       00572000
*        PROCESS WTOR WPL                                               00573000
*                                                                       00574000
         OI    XVD2,XVD2WTOR           SET ON WTOR FLAG                 00575000
         LR    R0,R6                   SAVE CURRENT VALUE OF R6         00576000
         L     R6,XVWPLADR             R6 -> WTOR PREFIX                00577000
         MVC   WKARPBUF+1(3),WPLRPTRA  MOVE REPLY BUFFER ADDR           00578000
         MVC   WKAECBP,WPLRECB         MOVE ACROSS ECB ADDR             00579000
         MVC   WKARPYLN,WPLRLN         MOVE ACROSS L'REPLY              00580000
         TM    WPL31RRP,WPL31RFG       WTOR 31 BIT FORMAT ?             00581000
         LR    R6,R0                   YES, RESTORE R6                  00582000
         BZ    IEA00262                NO, BRANCH                       00583000
         MVC   WKARPYLN,WPL31RLN       MOVE ACROSS 31 BIT L'REPLY       00584000
*                                                                       00585000
*        COMMON WTO/WTOR PROCESSING                                     00586000
*                                                                       00587000
IEA00262 MVC   WKALGH+1(1),WPLLPTXT    SAVE L'MSG TEXT IN WKALGH        00588000
         MVC   WKAMCSF,WPLMCSF         COPY MCS FLAGS FROM WPL          00589000
*                                      THIS FIELD MAY BE ZERO FOR OLD   00590000
*                                      FORMAT (PRE MCS) WPL             00591000
*/********************************************************************/ 00592000
*                                                                       00593000
*        DETERMINE IF DESCRIPTOR AND ROUTE CODES WERE SPECIFIED         00594000
*        IN THE WPL                                                     00595000
*                                                                       00596000
*/********************************************************************/ 00597000
         TM    WKAMCSF,WPLMCSFA     /* DESC/ROUT SPECIFIED ?         */ 00598000
         BZ    IEA00288                NO, BRANCH, ALSO NO MSGTYPE      00599000
         MVC   WKADSC(4),0(R8)         YES,MOVE ACROSS DESC/ROUTE CODES 00600000
         LA    R2,4(,R2)               INCR FOR DESC/ROUTE FIELDS       00601000
*/********************************************************************/ 00602000
*                                                                       00603000
*        DETERMINE IF MSGTYP WAS SPECIFIED IN THE WPL                   00604000
*                                                                       00605000
*/********************************************************************/ 00606000
         TM    WKAMCSF,WPLMCSFD     /* MSGTYP FIELD SPECIFIED ?         00607000
         BZ    IEA00288                NO, BRANCH                       00608000
         MVC   WKAMSGTY,4(R8)          COPY THE MSGTYP FIELD FROM WPL   00609000
         LA    R2,2(,R2)               INCR PAST MSGTYP FIELD           00610000
IEA00288 TM    WKAMSGTY,WPLMSGTD       QID FIELD PROVIDED ?             00611000
         BNO   IEA002B8                NO, BRANCH                       00612000
         OI    XVD0,XVD0QID            YES, SET QID PROVIDED FLAG       00613000
*/********************************************************************/ 00614000
*                                                                       00615000
*        DETERMINE IF MULTI-LINE WTO/WTOR                               00616000
*                                                                       00617000
*/********************************************************************/ 00618000
IEA002B8 TM    WKAMCSF+1,WPLMCSFJ      MULTI-LINE WTO/WTOR ?            00619000
         BZ    IEA00348                NO, BRANCH TO COMMON PROCESSING  00620000
*                                          FOR WTO AND WTOR             00621000
         TM    XVD2,XVD2WTOR           YES, WTOR ?                      00622000
         BZ    IEA002E2                NO, BRANCH TO PROCESS MLWTO      00623000
         MVI   XVREASON,D23MLWTR       ERROR - MULTILINE WTOR SPECIFIED 00624000
         B     VWTOVALB                PROCESS ERROR                    00625000
*                                                                       00626000
*/********************************************************************/ 00627000
*                                                                       00628000
*        EXTENDED WPL VALIDATION                                        00629000
*                                                                       00630000
*/********************************************************************/ 00631000
*                                                                       00632000
*        DETERMINE THE FORMAT OF THE PARAMETER LIST FOR VALIDATION      00633000
*        THERE ARE 3 WTO/WTOR XWPL PARAMETER LISTS                      00634000
*        1. EXTENDED WPL VERSION 1                                      00635000
*        2. EXTENDED WPL VERSION 2 (SUPPORT FOR TEXT= PARAMETER)        00636000
*        3. EXTENDED WPL VERSION 4 ONWARDS(SUPPORT FOR TEXT= PARAMETER) 00637000
*                                                                       00638000
*        XWPL ACCESS VALIDATION USING THE CALLERS KEY                   00639000
*        ON ENTRY -                                                     00640000
*        R6 -> XWPL                                                     00641000
*                                                                       00642000
XWPL001  LR    R0,R6                   SAVE R6 -> XWPL                  00643000
         LH    R1,WPLLGH               R1 = L'MAJOR WQE TEXT + 4        00644000
         AR    R6,R1                   R6 -> FIRST BYTE PAST TEXT       00645000
*                                            (START OF XWPL FIELDS)     00646000
         LA    R15,92                  R15 = L'XWPL VERSION 1           00647000
         CLI   WPXVRSN,1               XWPL VERSION 1 ?                 00648000
         BE    XWPL002                 YES, BRANCH                      00649000
         IC    R15,WPXLNGTH            R15 = L'XWPL VER 2 AND ONWARDS   00650000
XWPL002  AR    R15,R1                  R15 = L'XWPL (TEXT + XWPL DATA)  00651000
         LR    R6,R0                   RESTORE R6 -> XWPL               00652000
         TM    WPLMCSF+1,WPLMCSFJ      MULTI-LINE WTO/WTOR ?            00653000
         BZ    XWPL002A                NO, BRANCH                       00654000
*                                                                       00655000
*        VALIDATE MULTI-LINE XWPL                                       00656000
*                                                                       00657000
         AR    R6,R15                  R6 -> MLWTO EXTENT HEADER        00658000
         LR    R2,R6                   R2 -> MLWTO EXTENT HEADER        00659000
         SLR   R1,R1                                                    00660000
         IC    R1,WPLLINES             R1 = TOTAL NUMBER OF LINES       00661000
         BCTR  R1,0                    R1 = NUMBER OF MINOR WQES        00662000
         LA    R6,4(,R6)               R6 -> WPLML MLWTO LINE ENTRY     00663000
         B     XWPL002B                                                 00664000
*                                                                       00665000
XWPL002C AH    R6,WPLML0               ADD L'LINE TO TOTAL IN R6        00666000
         BCTR  R1,0                    DECR MINOR WQE COUNT             00667000
XWPL002B LTR   R1,R1                   MORE MINOR WQES TO PROCESS ?     00668000
         BP    XWPL002C                LOOP TO PROCESS NEXT MINOR WQE   00669000
*                                                                       00670000
*        ALL MINOR WQES PROCESSED                                       00671000
*                                                                       00672000
         SR    R6,R2                   R6 = L'MLWTO MINOR WQE LINES     00673000
         LR    R3,R6                   R3 = L'MLWTO MINOR WQE LINES     00674000
         AR    R15,R6                  R15 = L'XWPL + ML WQE TEXT LINES 00675000
*                                      (TOTAL LENGTH OF XWPL)           00676000
*                                                                       00677000
*        VALIDATE CALLER ACCESS TO XWPL AND OPTIONAL MINOR WQES         00678000
*                                                                       00679000
XWPL002A LR    R14,R0                  R0 AND R14 -> XWPL               00680000
         LR    R1,R15                  R1 AND R15 = L'XWPL              00681000
         CLCL  R0,R14                  VALIDATE CALLER ACCESS TO XWPL   00682000
*                                      STAE EXIT WILL CATCH ANY         00683000
*                                      ATTEMPT TO ACCESS NON            00684000
*                                      ADDRESSABLE STORAGE              00685000
         L     R6,XVWPLADR             REFRESH R6 -> XWPL               00686000
         LR    R0,R6                   SAVE -> XWPL                     00687000
         AH    R6,WPLLGH               R6 -> XWPL FIELDS                00688000
         TM    WPXMCS2,WPXTXTAD        TEXT ADDR SPECIFIED ?            00689000
         LR    R6,R0                   RESTORE -> XWPL                  00690000
         BZ    XWPL003                 NO, BRANCH                       00691000
*                                                                       00692000
*        VALIDATE CALLER ACCESS TO THE STORAGE ADDRESSED BY TEXT= ADDR  00693000
*                                                                       00694000
         CLC   WPLLGH,KH8              L'TEXT = 8 ?                     00695000
         BNE   XWPLE06                 NO, ERROR                        00696000
         L     R14,WPLTXT              R14 -> L'MESSAGE TEXT            00697000
         LH    R15,0(,R14)             R15 = L'MESSAGE TEXT             00698000
         LA    R14,2(,R14)             R14 -> MESSAGE TEXT              00699000
         LR    R0,R14                  R0 -> MESSAGE TEXT               00700000
         LR    R1,R15                  R1 = L'MESSAGE TEXT              00701000
         CLCL  R0,R14                  VALIDATE CALLER ACCESS TO TEXT   00702000
*                                      STAE EXIT WILL CATCH ANY         00703000
*                                      ATTEMPT TO ACCESS NON            00704000
*                                      ACCESSABLE STORAGE               00705000
*        CHANGE TO KEY 0                                                00706000
*        COPY XWPL DATA TO PROTECTED STORAGE                            00707000
*                                                                       00708000
XWPL003  MODESET  EXTKEY=SUPR                                           00709000
*                                                                       00710000
*        COPY FIELDS FROM XWPL TO WORK AREA IN PROTECTED STORAGE        00711000
*                                                                       00712000
         MVC   WKAMCSF,WPLMCSF         MCS FLAGS                        00713000
         TM    WKAMCSF+1,WPLMCSFJ      MULTI-LINE WTO/WTOR ?            00714000
         BZ    XWPL003A                NO, BRANCH                       00715000
         ST    R2,MLWTOXH              R2 -> MLWTO EXTENT HEADER        00716000
         ST    R3,LENMWQE              R3 = L'OF ALL MINOR WQES         00717000
XWPL003A MVC   WKALGH,WPLLGH           L'MSG (8 IF XWPL V2 AND TEXT=)   00718000
         AH    R6,WKALGH               R6 -> FIRST BYTE PAST MSG TEXT   00719000
*                                      (XWPL FIELDS)                    00720000
         MVC   WKAVRSN,WPXVRSN         VERSION                          00721000
         MVC   WKARPYLN,WPXRPYLN       L'REPLY (WTOR ONLY)              00722000
         MVC   WKALNGTH,WPXLNGTH       L'XWPL (ZERO FOR V1)             00723000
         MVC   WKAMCSF1,WPXMCSF1       EXTENDED MCS FLAGS               00724000
         MVC   WKARPBUF,WPXRPBUF       REPLY BUFFER ADDR - WTOR ONLY    00725000
         MVC   WKAECBP,WPXECBP         REPLY ECB ADDRESS - WTOR ONLY    00726000
         MVC   WKADSC,WPXDESC          DESCRIPTOR CODES, 2 BYTES ONLY   00727000
         MVC   WKAROC,WPXROUT          ROUTING CODES, 2 BYTES ONLY      00728000
         MVC   WKAMSGTY,WPXMSGTY       MSGTYP FLAGS                     00729000
*                                                                       00730000
*        ADDITIONAL PROCESSING FOR XWPL WTOR                            00731000
*                                                                       00732000
         CLI   WKARPYLN,0              WTOR ?                           00733000
         BE    XWPL004A                NO, BRANCH                       00734000
         OI    XVD2,XVD2WTOR           YES, SET ON WTOR FLAG            00735000
         TM    WKAMCSF+1,WPLMCSFJ      MULTI-LINE WTOR ?                00736000
         BO    XWPLE02                 YES, ERROR                       00737000
         TM    XVWPLADR+3,X'03'        XWPL ON WORD BOUNDARY ?          00738000
         BNZ   XWPLE01                 NO, ERROR                        00739000
*                                                                       00740000
XWPL004A L     R6,XVWPLADR             R6 -> CALLERS PROVIDED WPL       00741000
         TM    WKAMCSF1+1,WPXTXTAD     TEXT ADDR SPECIFIED ?            00742000
         BZ    XWPL004                 NO, BRANCH                       00743000
*                                                                       00744000
*        PROCESS TEXT= PARAMETER                                        00745000
*                                                                       00746000
         L     R8,WPLTXT               R8 -> 2 BYTE L'TEXT FOLLOWED BY  00747000
*                                      THE TEXT                         00748000
         LH    R3,0(,R8)               R3 = L'MESSAGE TEXT              00749000
         LA    R3,4(,R3)               ADD 4 FOR COMPATIBILTY WITH      00750000
*                                      INLINE TEXT                      00751000
         STH   R3,WKALGH               SET THE CORRECT L'TEXT           00752000
         LA    R8,2(,R8)               R8 -> MESSAGE TEXT               00753000
         B     XWPL006                                                  00754000
*                                                                       00755000
*        PROCESS INLINE TEXT                                            00756000
*                                                                       00757000
XWPL004  LH    R3,WKALGH               R3 = L'TEXT +4                   00758000
         LA    R8,WPLTXT               R8 -> MESSAGE TEXT               00759000
*                                                                       00760000
*        COMMMON PROCESSING FOR INLINE AND TEXT= PARAMETERS             00761000
*                                                                       00762000
XWPL006  A     R3,LENMWQE              ADD L'MINOR WQES (IF ANY)        00763000
         C     R3,LENMAXT              EXCEED 130 ?                     00764000
         BNH   XWPL007                 NO, BRANCH                       00765000
*                                                                       00766000
*        GETMAIN AREA FOR LONG TEXT WPL FROM SUBPOOL 229                00767000
*                                                                       00768000
         ST    R3,LONGLEN              SAVE LENGTH OF LONG TEXT         00769000
         LR    R0,R3                                                    00770000
*                                                                       00771000
         GETMAIN RU,LV=(0),SP=229,BRANCH=YES                            00772000
*                                                                       00773000
         ST    R1,LONGTXT              STORE ADDR OF GETMAINED STORAGE  00774000
*                                      FOR LONG TXT                     00775000
         LR    R2,R1                   R2 -> LONG TEXT STORAGE          00776000
         B     XWPL008                                                  00777000
*                                                                       00778000
XWPL007  LA    R2,WKATEXT              R2 -> STANDARD TEXT AREA         00779000
*                                                                       00780000
*        R2 WILL EITHER POINT TO THE STANDARD TEXT AREA                 00781000
*        -OR-                                                           00782000
*        IF THAT IS NOT LONG ENOUGH THE GETMAINED STORAGE FOR AN        00783000
*        EXTENDED TEXT AREA                                             00784000
*                                                                       00785000
XWPL008  ST    R2,WKAADTXT             WKAADTXT -> TEXT FLD(S)          00786000
         MVC   0(R2,2),WKALGH          SET THE LENGTH                   00787000
         LA    R0,4(,R2)                                                00788000
         LH    R1,WKALGH                                                00789000
         S     R1,KF4                  R1 = ACTUAL L'TEXT               00790000
         LR    R15,R1                                                   00791000
         LR    R14,R8                                                   00792000
         MVCL  R0,R14                  MOVE MAJOR WQE TEXT FROM CALLER  00793000
*                                      WPL TO PROTECTED STORAGE         00794000
*                                      THIS MAY BE EITHER INLINE TEXT   00795000
*                                      OR TEXT POINTED TO BY THE        00796000
*                                      TEXT= PARAMETER                  00797000
         ICM   R1,B'1111',LENMWQE      R1 = L' MLWTO HDR + L'MINOR WQES 00798000
*                                      FROM ADDR OF LAST BYTE OF WPL    00799000
         BZ    XWPL009                 NO MINOR WQES, BRANCH            00800000
         L     R14,MLWTOXH             R14 -> MLWTO EXTENT HDR          00801000
         LR    R15,R1                                                   00802000
*                                                                       00803000
*        COPY CALLER PROVIDED MLWTO EXTENT HEADER AND MINOR WQES        00804000
*        TO PROTECTED STORAGE                                           00805000
*        THE MLWTO EXTENT HEADER AND MINOR WQES ARE NOW CONTIGOUS       00806000
*        TO MAJOR WQE MESSAGE TEXT                                      00807000
         MVCL  R0,R14                                                   00808000
*                                                                       00809000
*        COMPARE FIELDS IN USER PROVIDED WPL TO FIELDS PREVIOUSLY       00810000
*        BROUGHT INTO PROTECTED STORAGE TO DETERMINE IF THE             00811000
*        USER PROVIDED WPL HAS BEEN SUBSEQUENTLY MODIFIED               00812000
*                                                                       00813000
XWPL009  L     R3,CVTPTR               RESTORE R3 TO CVT ADDR           00814000
         L     R6,XVWPLADR             R6 -> CALLERS WPL                00815000
         TM    WKAMCSF1+1,WPXTXTAD     TEXT ADDR SPECIFIED ?            00816000
         BZ    XWPL009A                NO, BRANCH                       00817000
*                                                                       00818000
*        VALIDATE TEXT= TEXT                                            00819000
*                                                                       00820000
         CLC   WPLLGH,KH8              LENGTH CORRECT ?                 00821000
         BNE   XWPLE07                 NO, ERROR, LENGTH CHANGED        00822000
         L     R1,WPLTXT               R1 -> 2 BYTE L'TEXT FOLLOWED BY  00823000
*                                      THE TEXT                         00824000
         LH    R1,0(,R1)               R1 = L'TEXT                      00825000
         LA    R1,4(,R1)               ADD 4 FOR COMPATIBILTY WITH      00826000
*                                      INLINE TEXT                      00827000
         CH    R1,WKALGH               LENGTH REMAINS UNCHANGED ?       00828000
         BNE   XWPLE07                 NO, ERROR, XWPL CHANGED          00829000
         B     XWPL009B                                                 00830000
*                                                                       00831000
*        VALIDATE INLINE TEXT                                           00832000
*                                                                       00833000
XWPL009A CLC   WPLLGH,WKALGH           LENGTH REMAINS UNCHANGED ?       00834000
         BNE   XWPLE07                 NO, ERROR, XWPL CHANGED          00835000
XWPL009B CLC   WPLMCSF,WKAMCSF         MCS FLAGS MATCH ?                00836000
         BNE   XWPLE07                 NO, ERROR                        00837000
         AH    R6,WPLLGH               R6 -> XWPL FIELDS                00838000
         CLC   WKAMCSF1,WPXMCSF1       EXTENDED MCS FLAGS MATCH ?       00839000
         BNE   XWPLE07                 NO, ERROR                        00840000
         TM    XVD2,XVD2WTOR           ALREADY IDENTIFIED AS WTOR ?     00841000
         BO    XWPL010                 YES, BRANCH                      00842000
         CLI   WKARPYLN,0              WTO, WKARPYLN MUST BE ZERO       00843000
         BNE   XWPLE07                 NO, ERROR                        00844000
         B     XWPL011                 GO CHECK MSGTYP FIELD            00845000
*                                                                       00846000
XWPL010  CLI   WKARPYLN,0              WTOR, WKARPYLN NOT MUST BE ZERO  00847000
         BE    XWPLE07                 ZERO, ERROR                      00848000
         TM    WPXMCSF1,WPXWTOR        WTOR WITH XWPL PARAMETER LIST ?  00849000
         BZ    XWPLE07                 NO, ERROR                        00850000
XWPL011  CLC   WKAMSGTY,WPXMSGTY       MSGTYP FLAGS MATCH ?             00851000
         BNE   XWPLE07                 NO, ERROR                        00852000
         B     IEA004BE                YES, GOTO FINAL WPL ERROR CHECKS 00853000
*                                                                       00854000
*        ERROR EXIT OUT OF XWPL VALIDATION                              00855000
*        COMPLETE REASON CODE FIELD                                     00856000
*                                                                       00857000
XWPLE01  MVI   XVREASON,D23BNDY        WTOR PARMLIST NOT ON WORD BNDY   00858000
         B     XWPLEEE                                                  00859000
*                                                                       00860000
XWPLE02  MVI   XVREASON,D23MLWTR       ERROR - MULTILINE WTOR SPECIFIED 00861000
         B     XWPLEEE                                                  00862000
*                                                                       00863000
XWPLE06  MVI   XVREASON,D23LTXT        WTO/WTOR TEXT= AND WPLLGH ^=8    00864000
         B     XWPLEEE                                                  00865000
*                                                                       00866000
XWPLE07  MVI   XVREASON,D23CMOD        CALLER MODIFIED WPL              00867000
*                                                                       00868000
*        THE REASON CODE HAS BEEN COMPLETED FOR                         00869000
*        A VALIDITY CHECK FAILURE                                       00870000
*                                                                       00871000
XWPLEEE  B     VWTOVALB                BRANCH TO ERROR ROUTINE          00872000
*                                                                       00873000
*                                                                       00874000
*/********************************************************************/ 00875000
*                                                                       00876000
*        MULTI-LINE WTO PROCESSING                                      00877000
*                                                                       00878000
*/********************************************************************/ 00879000
*                                                                       00880000
*        R2 -> FIRST BYTE PAST DESC/ROUT/MSGTYP FIELDS                  00881000
*                                                                       00882000
IEA002E2 TM    XVD0,XVD0QID            QID PRESENT ?                    00883000
         BZ    IEA002EE                NO, BRANCH                       00884000
         LA    R2,2(,R2)               INCR OVER QID BYTES              00885000
*                                      R2 -> MLWTO EXTENSION HEADER     00886000
IEA002EE ST    R2,MLWTOXH              STORE ADDR OF MLWTO EXTENT HDR   00887000
*                                                                       00888000
*        CHANGE TO CALLERS KEY                                          00889000
*                                                                       00890000
         MODESET EXTKEY=RBT234,WORKREG=15                               00891000
*                                                                       00892000
*        PROCESS THE MLWTO EXTENSION HEADER                             00893000
*        LOOP THROUGH ALL THE MINOR WQES                                00894000
*        SUM THE LENGTH OF ALL THE MINOR WQES                           00895000
*        NOTE -                                                         00896000
*        THE NUMBER OF LINES IN THE WPL IS NOT CHECKED HERE             00897000
*        THIS IS DONE IN IEAVMWTO                                       00898000
*                                                                       00899000
         LR    R6,R2                   ADJUST WPLRF ADDR TO             00900000
*                                      -> MLWTO EXTENT HEADER           00901000
         SLR   R1,R1                                                    00902000
         IC    R1,WPLLINES             R1 = TOTAL NUMBER OF LINES       00903000
         BCTR  R1,0                    R1 = NUMBER OF MINOR WQES        00904000
         LA    R6,4(,R6)               R6 -> WPLML MLWTO LINE ENTRY     00905000
         B     IEA00330                                                 00906000
*                                                                       00907000
IEA00326 AH    R6,WPLML0               ADD L'LINE TO TOTAL IN R6        00908000
         BCTR  R1,0                    DECR MINOR WQE COUNT             00909000
IEA00330 LTR   R1,R1                   MORE MINOR WQES TO PROCESS ?     00910000
         BP    IEA00326                LOOP TO PROCESS NEXT MINOR WQE   00911000
         LR    R14,R6                  R14 -> LAST BYTE +1 OF WPL       00912000
         SR    R14,R2                  R14 = L'MLWTO EXTENSION          00913000
         LR    R2,R6                   R2 -> LAST BYTE +1 OF ENTIRE WPL 00914000
*                                                                       00915000
*        R2 -> END OF MLWTO WQE TEXT+1                                  00916000
*                                                                       00917000
*        CHANGE BACK TO SUPERVISOR KEY                                  00918000
*                                                                       00919000
         MODESET  EXTKEY=SUPR          CHANGE BACK TO SUPERVISOR KEY    00920000
*                                                                       00921000
         ST    R14,LENMWQE             SAVE L'MINOR WQES                00922000
*                                                                       00923000
*/********************************************************************/ 00924000
*                                                                       00925000
*        COMMON PROCESSING FOR WTO/WTOR AND MLWTO                       00926000
*        VALIDATE CALLER ACCESS TO WPL                                  00927000
*                                                                       00928000
*/********************************************************************/ 00929000
*                                                                       00930000
*        R2 -> LAST BYTE +1 OF ENTIRE WPL (WTO/WTOR/MLWTO)              00931000
*                                                                       00932000
IEA00348 L     R6,XVWPLADR             R6 -> WPL (WTO/WTOR/WTOR31)      00933000
         LR    R14,R6                  COPY WPL ADDR                    00934000
         LR    R3,R2                   R3 -> LAST BYTE +1 OF ENTIRE WPL 00935000
         SR    R3,R14                  SUBTRACT START OF WPL FROM END   00936000
*                                      R3 = L'WPL INCLUDING OPT FIELDS  00937000
*                                           AND MLWTO WQES IF PRESENT   00938000
         LR    R2,R14                  R2 AND R14 -> WPL                00939000
*                                                                       00940000
         MODESET EXTKEY=RBT234,WORKREG=15  CHANGE TO CALLERS KEY        00941000
*                                                                       00942000
         LR    R15,R3                  R3 = L'WPL INCLUDING OPT FIELDS  00943000
*                                           AND MLWTO WQES IF PRESENT   00944000
*                                      R2 = R14, SOURCE = TARGET        00945000
*                                                                       00946000
*        REFERENCE THE ENTIRE WPL TO ENSURE IT IS ADDRESSABLE BY        00947000
*        THE CALLER                                                     00948000
*                                                                       00949000
         CLCL  R2,R14                  VALIDATE CALLER ACCESS TO TEXT   00950000
*                                      STAE EXIT WILL CATCH ANY         00951000
*                                      ATTEMPT TO ACCESS NON            00952000
*                                      ADDRESSABLE STORAGE              00953000
*                                                                       00954000
*        TO PROCEED PAST THIS POINT THE ENTIRE WPL INCLUDING ANY        00955000
*        OPTIONAL MLWTO MINOR WQES ARE PROVEN TO BE ACCESSABLE BY       00956000
*        THE CALLER                                                     00957000
*VWTOCONT:                                                              00958000
VWTOCONT MODESET  EXTKEY=SUPR                                           00959000
*                                                                       00960000
         L     R3,CVTPTR               RESTORE CVT ADDR                 00961000
*       IF XVD2WTOR='1'B&           /* A VALID WTOR AND THE PARM        00962000
*                                      LIST ON A WORD BOUNDARY ?     */ 00963000
*           XVWPLADR//4^=0                                              00964000
*         THEN                                                          00965000
         TM    XVD2,XVD2WTOR           WTOR ?                           00966000
         BNO   @RF00231                NO, BRANCH                       00967000
         TM    XVWPLADR+3,X'03'        WORD BOUNDARY ?                  00968000
         BZ    @RF00231                YES, BRANCH                      00969000
*                                      NO, ERROR, NOT ON WORD BOUNDARY  00970000
         MVI   XVREASON,D23BNDY        WTOR WPL NOT ON WORD BOUNDARY    00971000
         B     VWTOVALB                                                 00972000
*                                                                       00973000
*     END;                          /* END OF CHECK OF PARM LIST     */ 00974000
*/******* END OF VALIDITY CHECK **************************************/ 00975000
*/********************************************************************/ 00976000
*/                                                                   */ 00977000
*/       THE WPL WAS CHECKED AND FOUND TO BE VALID                   */ 00978000
*/                                                                   */ 00979000
*/       COPY THE TEXT PROVIDED IN THE WPL, MAJOR WQE TEXT AND       */ 00980000
*/       MINOR WQE TEXT (IF A MLWTO) TO PROTECTED STORAGE.           */ 00981000
*/       IF THE STANDARD WKATEXT AREA IS NOT SUFFICIENTLY LONG       */ 00982000
*/       ENOUGH TO STORE THE COMBINED TEXT THEN GETMAIN AN AREA      */ 00983000
*/       SUFFICIENTLY LARGE ENOUGH TO STORE THE COMBINED TEXT IN     */ 00984000
*/       A CONTIGUOUS MANNER, MAJOR WQE TEXT FOLLOWED BY OPTIONAL    */ 00985000
*/       MULTIPLE MINOR WQE TEXT                                     */ 00986000
*/                                                                   */ 00987000
*/********************************************************************/ 00988000
*                                                                       00989000
*        ANY ERROR NOW DETECTED WILL RESULT IN AN FRR ABEND WITH DUMP   00990000
*                                                                       00991000
*       PARMRTAD=0;                 /* ZERO RETRY ADDR               */ 00992000
@RF00231 XC    PARMRTAD,PARMRTAD                                        00993000
*       PARMNDMP='0'B;              /* RESET DUMP INDICATOR          */ 00994000
         NI    PARMFLAG,255-PARMNDMP                                    00995000
         LH    R3,WKALGH               R3 = L'MAJOR WQE                 00996000
         A     R3,LENMWQE              ADD L'MINOR WQES (IF ANY)        00997000
         C     R3,LENMAXT              EXCEED 130 ? MUST BE LONG TEXT   00998000
         BNH   IEA00412                NO, BRANCH                       00999000
*                                                                       01000000
*        GETMAIN AREA FOR LONG TEXT AREA FROM SUBPOOL 229               01001000
*                                                                       01002000
         LR    R0,R3                                                    01003000
         ST    R3,LONGLEN              SAVE LENGTH OF LONG TEXT         01004000
*                                                                       01005000
         GETMAIN RU,LV=(0),SP=229,BRANCH=YES                            01006000
*                                                                       01007000
         ST    R1,LONGTXT              STORE ADDR OF GETMAINED STORAGE  01008000
*                                      FOR LONG TXT                     01009000
         LR    R2,R1                   R2 -> LONG TXT STORAGE           01010000
         B     IEA00416                                                 01011000
*                                                                       01012000
IEA00412 LA    R2,WKATEXT              R2 -> STANDARD TEXT AREA         01013000
*                                                                       01014000
*        R2 WILL EITHER POINT TO THE STANDARD TEXT AREA                 01015000
*        -OR-                                                           01016000
*        IF THAT IS NOT LONG ENOUGH THE GETMAINED STORAGE FOR AN        01017000
*        EXTENDED LENGTH TEXT AREA                                      01018000
*                                                                       01019000
IEA00416 ST    R2,WKAADTXT             WKAADTXT -> TEXT FLD(S)          01020000
         L     R14,XVWPLADR            R14 -> CALLERS PROVIDED WPL      01021000
         TM    XVD2,XVD2WTOR           WTOR WPL ?                       01022000
         BZ    IEAA0416                NO, BRANCH                       01023000
         LA    R14,8(,R14)             INCR PAST WTOR FIELDS            01024000
IEAA0416 LH    R15,WKALGH              R15 AND R3 = L'MAJOR WQE TEXT    01025000
         LR    R3,R15                                                   01026000
         MVCL  R2,R14                  MOVE MAJOR WQE TEXT FROM CALLER  01027000
*                                      WPL TO PROTECTED STORAGE         01028000
         ICM   R3,B'1111',LENMWQE      R3 = L' MLWTO HDR + L'MINOR WQES 01029000
*                                      FROM ADDR OF LAST BYTE OF WPL    01030000
         BZ    IEA00442                NO MINOR WQES, BRANCH            01031000
         L     R14,MLWTOXH             R14 -> MLWTO EXTENT HDR          01032000
         LR    R15,R3                                                   01033000
*                                                                       01034000
*        COPY CALLER PROVIDED MLWTO EXTENT HEADER AND MINOR WQES        01035000
*        TO PROTECTED STORAGE                                           01036000
*        THE MLWTO EXTENT HEADER AND MINOR WQES ARE NOW CONTIGUOUS      01037000
*        TO MAJOR WQE MESSAGE TEXT                                      01038000
         MVCL  R2,R14                                                   01039000
*                                                                       01040000
*        COMPARE FIELDS IN USER PROVIDED WPL TO FIELDS PREVIOUSLY       01041000
*        BROUGHT INTO PROTECTED STORAGE TO DETERMINE IF THE             01042000
*        USER PROVIDED WPL HAS BEEN SUBSEQUENTLY MODIFIED               01043000
*                                                                       01044000
IEA00442 L     R3,CVTPTR               RESTORE CVT ADDR                 01045000
         L     R6,XVWPLADR             R6 -> CALLERS WPL                01046000
         CLI   WPLRLN,0                FIRST BYTE OF WPL ZERO ?         01047000
         BE    IEA00462                YES, MUST BE WTO, BRANCH         01048000
         LA    R6,8(,R6)               WTOR, INCR PAST WTOR FIELDS      01049000
*                     IF XVD2WTOR='1'B THEN                             01050000
         TM    XVD2,XVD2WTOR           ALREADY IDENTIFIED AS WTOR ?     01051000
         BO    IEA0046E                YES, BRANCH                      01052000
         B     IEA00BAD                NO, BAD WPL                      01053000
*                                                                       01054000
*                     IF XVD2WTOR='1'B THEN                             01055000
IEA00462 TM    XVD2,XVD2WTOR           ALREADY IDENTIFIED AS WTOR ?     01056000
         BZ    IEA0046E                NO, BRANCH                       01057000
         B     IEA00BAD                YES, WPL BAD                     01058000
*                                                                       01059000
IEA0046E CLC   WPLLPTXT,WKALGH+1       L'MSG TEXT MATCH ?               01060000
         BNE   IEA00BAD                NO, BAD WPL                      01061000
         CLC   WPLMCSF,WKAMCSF         MCS FLAGS MATCH ?                01062000
         BNE   IEA00BAD                NO, BAD WPL                      01063000
*           IF WPLMCSFD='1'B THEN   /* MSGTYP SPECIFIED ?            */ 01064000
IEA0048A TM    WKAMCSF,WPLMCSFD     /* MSGTYP FIELD SPECIFIED ?         01065000
         BZ    IEA004BE                NO, BRANCH                       01066000
         CLC   4(2,R8),WKAMSGTY        YES, MSGTYPE FIELDS MATCH ?      01067000
         BE    IEA004BE                YES, WPL OK                      01068000
*                                                                       01069000
*        END OF WPL CHECKS                                              01070000
*                                                                       01071000
*        COMMON CODE FOR BAD WPL AND XWPL PROCESSING                    01072000
*                                                                       01073000
IEA00BAD MVI   XVREASON,D23CMOD        CALLER MODIFIED WPL              01074000
         B     VWTOVALB                                                 01075000
*                                                                       01076000
*        COMMON CODE FOR WPL AND XWPL PROCESSING                        01077000
*                                                                       01078000
*        FREE THE LOCKS OBTAINED FOR ALL CALLERS                        01079000
*                                                                       01080000
*               IF XVD1AUTH='0'B THEN/* CALLER AUTHORIZED ?             01081000
*        TM    XVD1,XVD1AUTH           AUTHORIZED ?                     01082000
*        BO    IEA004D2                YES, BRANCH                      01083000
IEA004BE OI    PFLAG,NOCMSLOK          TURN ON NO CMS LOCK REQUESTED    01084000
*                                                                       01085000
*        FREE THE LOCKS AND REMOVE THE FRR                              01086000
*        ORIGINAL ESTAE IS NOW IN PLACE                                 01087000
*                                                                       01088000
         BAL   R14,FREELOCK                                             01089000
         NI    PFLAG,255-NOCMSLOK      TURN OFF NO CMS LOCK REQUESTED   01090000
IEA004D2 EQU   *                                                        01091000
*                                                                       01092000
*   ELSE                            /* PARM LIST WAS VALID PROCESS IT*/ 01093000
*     DO;                                                               01094000
*/********************************************************************/ 01095000
*/*                                                                  */ 01096000
*/*  TAKE THE INSTALLATION EXIT                                      */ 01097000
*/*                                                                  */ 01098000
*/********************************************************************/ 01099000
*/*** START OF USEREXIT **********************************************/ 01100000
*/********************************************************************/ 01101000
*/*                                                                  */ 01102000
*/*   USEREXIT - SET UP AND TAKE INSTALLATION WTO EXIT               */ 01103000
*/*                                                                  */ 01104000
*/*  INPUT -                                                         */ 01105000
*/*    R2 -> LAST BYTE IN THE WQE                                    */ 01106000
*/*    R6 -> BEGINNING OF THE MESSAGE PART OF THE WPL                */ 01107000
*/*                                                                  */ 01108000
*/*  OUTPUT -                                                        */ 01109000
*/*    WKAROC AND WKADSC CONTAIN THE ROUTE AND DESCRIPTOR CODES      */ 01110000
*/*    BE USED IN THE WQE                                            */ 01111000
*/*    XVD2DELW IS ON IF THE MESSAGE IS NOT TO GO TO THE CONSOLE BUT */ 01112000
*/*    TO HARDCOPY ONLY.                                             */ 01113000
*/*                                                                  */ 01114000
*/********************************************************************/ 01115000
*/********************************************************************/ 01116000
*/*                                                                  */ 01117000
*/* CHECK THAT THIS IS NOT A CONNECTING MLWTO, CONNECTING MLWTOS DO  */ 01118000
*/* NOT GET SENT TO THE INSTALLATION EXIT UNLESS ISSUED              */ 01119000
*/* FROM A PROBLEM PROGRAM                                           */ 01120000
*/*                                                                  */ 01121000
*/* THE INSTALLATION USER EXIT WILL BE TAKEN FOR ANY OF              */ 01122000
*/* THESE THREE SITUATIONS:                                          */ 01123000
*/*                                                                  */ 01124000
*/*    1. THE WTO IS NOT A MULTI-LINE REQUEST                        */ 01125000
*/*         OR                                                       */ 01126000
*/*    2. THE MLWTO IS NOT A CONNECTING MLWTO REQUEST                */ 01127000
*/*         OR                                                       */ 01128000
*/*    3. THE CONNECTING MLWTO REQUEST IS ISSUED FROM A              */ 01129000
*/*           PROBLEM PROGRAM                                        */ 01130000
*/*                                                                  */ 01131000
*/* THE ONLY OTHER SITUATION IS IF A CONNECTING MLWTO IS             */ 01132000
*/* ISSUED FROM SUPERVISOR STATE                                     */ 01133000
*/*      FOR THIS CASE, DO NOT TAKE THE USER EXIT                    */ 01134000
*/*                                                                  */ 01135000
*/********************************************************************/ 01136000
*                                                                       01137000
*       IF WPLMCSFJ^='1'B           /* SITUATION 1. ABOVE            */ 01138000
*           XVWQEIDA=0              /* SITUATION 2. ABOVE            */ 01139000
*           XVD1PP='1'B THEN        /* SITUATION 3. ABOVE            */ 01140000
@RF00247 TM    WKAMCSF+1,WPLMCSFJ      MLWTO ?                          01141000
         BZ    @RT00250                NO, BRANCH                       01142000
         ICM   R15,B'0111',XVWQEIDA    YES, ANY MESSAGE ID PROVIDED ?   01143000
         BZ    @RT00250                NO, BRANCH                       01144000
         TM    XVD1,XVD1PP             PROBLEM PROGRAM ?                01145000
         BZ    @RF00250                NO, BRANCH                       01146000
*         DO;                       /* THIS IS NOT A CONNECTING         01147000
*                                      MLWTO UNLESS ISSUED FROM A       01148000
*                                      PROBLEM PROGRAM               */ 01149000
*/********************************************************************/ 01150000
*/*                                                                  */ 01151000
*/* CHECK IF WE SHOULD USE THE DEFAULTS FOR ROUTE & DESC CODES BY    */ 01152000
*/* CHECKING IF THIS IS AN OLD STYLE MESSAGE WHICH HAS NO MCS FLAGS  */ 01153000
*/*                                                                  */ 01154000
*/********************************************************************/ 01155000
*           IF WKAMCSF=0 THEN       /* ANY MCS FLAGS SET IN THE WPL ?*/ 01156000
@RT00250 ICM   R15,B'0011',WKAMCSF                                      01157000
         BNZ   @RF00264                YES, BRANCH                      01158000
*             DO;                   /* NO, USE DEFAULTS              */ 01159000
*               WKAROC=UCMOWTOR;    /* GET DEFAULT ROUTE CODES FROM     01160000
*                                      UCM                           */ 01161000
         L     R15,CVTCUCB                                              01162000
         S     R15,KF4                                                  01163000
         L     R15,0(,R15)              R10 -> UCM PREFIX               01164000
         USING UCMPRFX,R15                                              01165000
         MVC   WKAROC,UCMOWTOR          MOVE IN DEFAULT ROUTING CODES   01166000
         OI    WKAMCSF,WPLMCSFA      /* MARK R/D CODES PRESENT       */ 01167000
         DROP  R15                                                      01168000
*                                                                       01169000
*             END;                                                      01170000
*/********************************************************************/ 01171000
*/*                                                                  */ 01172000
*/* CHECK FOR AN INTERNAL MESSAGE (ONE THAT IS AUTHORIZED AND HAS ANY*/ 01173000
*/* MCSFLAGS(2-8) ON). IF IT IS AN INTERNAL MESSAGE THEN SKIP THE    */ 01174000
*/* INSTALLATION EXIT. CHECK ALSO THAT THE MSG LENGTH IS GREATER THAN*/ 01175000
*/* ZERO (THAT WKALGH > 4)                                           */ 01176000
*/*                                                                  */ 01177000
*/********************************************************************/ 01178000
*           IF^(XVD1AUTH='1'B&      /* DO WE NOT HAVE THE FOLLOWING:    01179000
*                                      (IS THE USER AUTHORIZED AND      01180000
*                                      ARE ANY SYSTEM RELATED           01181000
*                                      MCSFLAGS BITS TURNED ON (BITS    01182000
*                                      2 TO 8) ) AND THAT THE TEXT      01183000
*                                      LENGTH IS > FOUR              */ 01184000
*               WPLMCSF1(2:8)^='0000000'B)&WKALGH>4 THEN                01185000
*                                                                       01186000
@RF00264 TM    XVD1,XVD1AUTH           AUTHORIZED ?                     01187000
         AIF   (&TMVS805).U805A                                         01188000
         BZ    @GL00007                NO, BRANCH                       01189000
         AGO   .U805B                                                   01190000
.U805A   NOP   @GL00007                IGNORE AUTHORIZED STATUS         01191000
.U805B   ANOP                                                           01192000
         TM    WKAMCSF,255-WPLMCSFA    ANY SYSTEM RELATED MCS FLAGS ?   01193000
         AIF   (&TMVS805).U805C                                         01194000
         BNZ   @RF00250                YES, BRANCH                      01195000
         AGO   .U805D                                                   01196000
.U805C   NOP   @RF00250                IGNORE SYSTEM RELATED MCS FLAGS  01197000
.U805D   ANOP                                                           01198000
@GL00007 LH    R14,WKALGH              NO SYSTEM RELATED MCS FLAGS      01199000
         C     R14,KF4                 L'MCS + TEXT > 4                 01200000
         AIF   (&TMVS805).U805E                                         01201000
         BNH   @RF00250                NO, BRANCH                       01202000
         AGO   .U805F                                                   01203000
.U805E   NOP   @RF00250                IGNORE LENGTH CHECK              01204000
.U805F   ANOP                                                           01205000
*             DO;                   /* YES, THEN MESSAGE IS NOT A       01206000
*                                      SYSTEM INTERNALLY GENERATED      01207000
*                                      ONE. TAKE EXIT                */ 01208000
*               /*****************************************************/ 01209000
*               /*                                                   */ 01210000
*               /* FILL IN THS INSTALLATION EXIT'S PARAMETER LIST    */ 01211000
*               /* MOVE IN THE MESSAGE FROM THE WPL                  */ 01212000
*               /*                                                   */ 01213000
*               /*****************************************************/ 01214000
*               USERTEXT=WPLTXT(1:WKALGH-4);                            01215000
         MVI   USERTEXT,C' '                                            01216000
         MVC   USERTEXT+1(L'USERTEXT-1),USERTEXT                        01217000
         S     R14,KF5                 SUBTRACT 4 BYTES FOR L'TEXT      01218000
*                                      AND MCS FLAGS AND ONE FOR EX     01219000
         C     R14,KF127               EXCEED L'USERTEXT ? (128-1)      01220000
         BNH   @GL00008                                                 01221000
         L     R14,KF127               YES, TRUNCATE TEXT MOVE TO 128   01222000
@GL00008 L     R15,WKAADTXT                                             01223000
         EX    R14,@SM03493            MOVE WPL MAJOR WQE TEXT ONLY     01224000
*                                      TO USER EXIT TEXT                01225000
*                                      MVC   USERTEXT(0),4(R15)         01226000
*               USERDC=WKADSC;      /* PARM LIST FOR THE USER EXIT   */ 01227000
*               USERRC=WKAROC;      /* MOVE THE ROUTE/DESC CODES INTO*/ 01228000
         MVC   USERDC,WKADSC                                            01229000
         MVC   USERRC,WKAROC                                            01230000
*               /*****************************************************/ 01231000
*               /*                                                   */ 01232000
*               /* SET UP FOOT PRINT AROUND USER EXIT                */ 01233000
*               /*                                                   */ 01234000
*               /*****************************************************/ 01235000
*                                   /* SAVE CURRENT REGS         */     01236000
         STM   R0,R15,REGRECOV                                          01237000
*               PARMFTPT=FTUSERX;   /* INSTALLATION'S EXIT WAS CALLED*/ 01238000
         MVI   PARMFTPT,X'03'                                           01239000
*               PARMCLAD=0;         /* NO CLEAN UP ROUTINE           */ 01240000
         XC    PARMCLAD,PARMCLAD                                        01241000
*               PARMRTAD=0;         /* NO RETRY ADDR                 */ 01242000
         XC    PARMRTAD,PARMRTAD                                        01243000
*                                                                       01244000
*        /*****************************************************/        01245000
*        /*                                                   */        01246000
*        /* CALL INSTALLATION EXIT ROUTINE                    */        01247000
*        /*                                                   */        01248000
*        /*****************************************************/        01249000
*                                                                       01250000
*        CALL IEECVXIT(USERPARM);                                       01251000
*                                                                       01252000
*        THE IEECVXIT PROVIDED AS A COMPONENT OF THE BSPPILOT           01253000
*        USERMOD ZUM0003 CONTRIBUTED BY VOLKER BANDKE WILL              01254000
*        AUTOMATICALLY REPLY TO SELECTED WTOR MESSAGES. THE             01255000
*        CORRECT ORE FOR THE MESSAGE (AND HENCE THE REPLY NUMBER)       01256000
*        IS LOCATED BY THE IEECVXIT CODE BY COMPARING THE ECB           01257000
*        ADDRESS IN THE ORE WITH THAT PROVIDED IN THE CALLERS           01258000
*        WPL. THE CODE IN THE BSPPILOT IEECVXIT ASSUMES THAT R5         01259000
*        WILL POINT TO THE SVRB FOR SVC 35 AND THAT THE ADDRESS         01260000
*        OF THE WPL PASSED TO SVC 35 WILL BE LOCATED IN THE             01261000
*        SVRB+X'74'. THE ADDRESS OF THE ECB FOR A WTOR WILL BE          01262000
*        LOCATED AT AN OFFSET 4 BYTES INTO THE WPL. THIS IS NOT         01263000
*        CORRECT WITH THIS CODE. TO AVOID CHANGING THE IEECVXIT         01264000
*        USERMOD A DUMMY ECB WPL ADDRESS FIELD IS BUILT AND             01265000
*        PASSED VIA R5 TO THE USERMOD CODE                              01266000
*                                                                       01267000
         LR    R8,R5                   SAVE CORRECT SVRB ADDR           01268000
         LA    R15,WKAECBP-4           R15 -> FAKE WPL ADDR             01269000
         ST    R15,CVDAREA             SAVE FAKE WPL ADDR IN CVDAREA    01270000
         LA    R5,CVDAREA-X'74'        R5 -> WPL ADDR IN FAKE SVRB      01271000
*                                      PASS TO IEECVXIT CODE            01272000
         LA    R15,USERPARM                                             01273000
         ST    R15,PARMPTR                                              01274000
         L     R15,VIEECVXT                                             01275000
         LA    R1,PARMPTR                                               01276000
         BALR  R14,R15                                                  01277000
         LR    R5,R8                   RESTORE CORRECT SVRB ADDR        01278000
*               /*****************************************************/ 01279000
*               /*                                                   */ 01280000
*               /* RESTORE THE FOOTPRINTS AFTER THE EXIT ROUTINE     */ 01281000
*               /*                                                   */ 01282000
*               /*****************************************************/ 01283000
*               PARMFTPT=FTAFUX;                                        01284000
         MVI   PARMFTPT,X'04'                                           01285000
*/********************************************************************/ 01286000
*/*                                                                  */ 01287000
*/* CHECK IF THE ROUTE CODES WERE CHANGED                            */ 01288000
*/*                                                                  */ 01289000
*/********************************************************************/ 01290000
*               IF WKAROC^=         /* ROUTE CODES CHANGED ?         */ 01291000
*                   USERRC  THEN                                        01292000
         CLC   WKAROC,USERRC                                            01293000
         BE    @RF00282                NO, BRANCH                       01294000
*                 DO;               /* YES, PROCESS CHANGED ROUTE       01295000
*                                      CODES                         */ 01296000
*                   XVD2QFHC='1'B;  /* INDICATE MSG IS TO BE QUEUED     01297000
*                                      FOR HARD COPY                 */ 01298000
         OI    XVD2,XVD2QFHC                                            01299000
*/********************************************************************/ 01300000
*/*                                                                  */ 01301000
*/* CHECK IF ROUTE CODE WAS CHANGED TO ZERO, INDICATING THAT THIS    */ 01302000
*/*  MESSAGE SHOULDN'T GO TO THE CONSOLE                             */ 01303000
*/*                                                                  */ 01304000
*/********************************************************************/ 01305000
*                   IF USERRC=0 THEN                                    01306000
         ICM   R15,B'0011',USERRC                                       01307000
         BNZ   @RF00285                                                 01308000
*/********************************************************************/ 01309000
*/*                                                                  */ 01310000
*/* CHECK IF THIS IS A WTOR. IF SO THEN IGNORE REQUEST TO DELETE     */ 01311000
*/* MESSAGE (ZERO ROUTE CODE. IF WTO, DELETE MESSAGE. SEND JUST TO   */ 01312000
*/* HARDCOPY                                                         */ 01313000
*/*                                                                  */ 01314000
*/********************************************************************/ 01315000
*                     IF XVD2WTOR='1'B THEN                             01316000
         TM    XVD2,XVD2WTOR                                            01317000
         BZ    @RF00286                                                 01318000
*                       DO;         /* PUT IN WTOR ROUTE CODES       */ 01319000
*                         /*******************************************/ 01320000
*                         /*                                         */ 01321000
*                         /* ROUTE WTOR TO MASTER CONSOLE            */ 01322000
*                         /*                                         */ 01323000
*                         /*******************************************/ 01324000
*                         WKAROC='8000'X;                               01325000
         MVC   WKAROC,KX8000                                            01326000
*                       WPLMCSFA='1'B;/* TURN ON ROUTE CODES PRESENT    01327000
*                                      FLAGS                         */ 01328000
         OI    WKAMCSF,WPLMCSFA                                         01329000
         B     @RF00282                                                 01330000
*                                                                       01331000
*                       END;                                            01332000
*                     ELSE          /* NOT A WTOR. INDICATE SEND MSG    01333000
*                                      TO HARDCOPY ONLY              */ 01334000
*                       XVD2DELW='1'B;                                  01335000
@RF00286 OI    XVD2,XVD2DELW                                            01336000
         B     @RF00282                                                 01337000
*                                                                       01338000
*/********************************************************************/ 01339000
*/*                                                                  */ 01340000
*/* NO, THE ROUTE CODE WASNT SET TO ZERO, BUT IT IS CHANGED. PICK    */ 01341000
*/* UP THE NEW ROUTE CODES FOR USE LATER IN BUILDING THE WQE         */ 01342000
*/* ALSO PASS THE NEW ROUTE CODES TO WTP IF ROUTE CODE 11 IS SET     */ 01343000
*/*                                                                  */ 01344000
*/********************************************************************/ 01345000
*                   ELSE                                                01346000
*                     WKAROC=USERRC ;/* MOVE NEW ROUTE CODES TO         01347000
*                                      WKAROC                        */ 01348000
*                 END;              /* END OF PROCESSING OF CHANGED     01349000
*                                      ROUTE CODES                   */ 01350000
@RF00285 MVC   WKAROC,USERRC                                            01351000
*               IF WKADSC^=USERDC THEN/* DESC CODE CHANGED ?        */  01352000
@RF00282 CLC   WKADSC,USERDC                                            01353000
         BE    @RF00250                NO, BRANCH                       01354000
*                 DO;               /* YES, SAVE DESC CODES          */ 01355000
*                   WKADSC=USERDC ; /* SAVE USER CHANGES             */ 01356000
         MVC   WKADSC,USERDC                                            01357000
*                   XVD2QFHC='1'B;  /* INDICATE HARDCOPY MSG         */ 01358000
         OI    XVD2,XVD2QFHC                                            01359000
*                 END;                                                  01360000
*             END;                  /* OF PROCESSING NON-INTERNAL       01361000
*                                      MSGS                          */ 01362000
*         END;                      /* OF PROCESSING NON-CONNENCTING    01363000
*                                      MSGS                          */ 01364000
*/********************************************************************/ 01365000
*/*                                                                  */ 01366000
*/* WKAROC AND WKADSC CONTAIN THE POSSIBLY UPDATED ROUTE AND         */ 01367000
*/* DESCRIPTOR CODES AND MSGTYP FIELD TO BE USED BY THIS MESSAGE     */ 01368000
*/*                                                                  */ 01369000
*/********************************************************************/ 01370000
*                                                                       01371000
*/***** END OF USER EXIT SEGMENT *************************************/ 01372000
*                                                                       01373000
*/********************************************************************/ 01374000
*/*                                                                  */ 01375000
*/*  DETERMINE IF A MULTI-LINE WTO WAS ISSUED. IF SO CALL IEAVMWTO   */ 01376000
*/*                                                                  */ 01377000
*/********************************************************************/ 01378000
*                                                                       01379000
*       IF WPLMCSFJ='1'B THEN       /* MULTI-LINE FLAG SET IN WPL ?  */ 01380000
@RF00250 TM    WKAMCSF+1,WPLMCSFJ      NO, BRANCH                       01381000
         BZ    @RF00303                                                 01382000
*         DO;                       /* YES, GO TO PROCESS MULTI-LINE */ 01383000
*           PARMFTPT=FTMLWTO;       /* START OF MLWTO PROCESSING     */ 01384000
         MVI   PARMFTPT,X'05'                                           01385000
*           PARMRTAD=0;             /* DON'T RETRY. MLWTO DETECTS THE   01386000
*                                      ERROR                         */ 01387000
         XC    PARMRTAD,PARMRTAD                                        01388000
*           CALL IEAVMWTO;          /* PROCESS MULTI-LINE WTO        */ 01389000
         L     R15,VIEAVMWT                                             01390000
         BALR  R14,R15                                                  01391000
*           PARMFTPT=FTSWTO;        /* START OF WTO BLOCK PROCESSING */ 01392000
         MVI   PARMFTPT,X'06'                                           01393000
         B     VWTOERRT                GOTO EXIT PROCESSING             01394000
*                                                                       01395000
*         END;                                                          01396000
*       ELSE                        /* NO, SINGLE LINE SPECIFIED     */ 01397000
*/********************************************************************/ 01398000
*/*                                                                  */ 01399000
*/* DETERMINE IF PARM LIST LENGTH IS LESS THAN OR EQUAL TO FOUR      */ 01400000
*/* IF SO THEN THE MSGLENGTH IS ZERO. THIS IS AN ABEND               */ 01401000
*/* SITUATION FOR WTORS                                              */ 01402000
*/*                                                                  */ 01403000
*/********************************************************************/ 01404000
*         IF WKALGH<=4 THEN         /* DOES LENGTH SHOW ZERO MSG        01405000
*                                      LENGTH ?                      */ 01406000
@RF00303 CLC   WKALGH,KH4                                               01407000
         BH    @RF00314                                                 01408000
*/********************************************************************/ 01409000
*/*                                                                  */ 01410000
*/* CHECK FOR WTOR. IF WTOR, THEN SET ABEND BIT. IF WTO, THEN SET    */ 01411000
*/* THE NO MESSAGE BIT ON (XVD2ZERO)                                 */ 01412000
*/*                                                                  */ 01413000
*/********************************************************************/ 01414000
*           IF XVD2WTOR='1'B THEN   /* WTOR REQUEST ?                */ 01415000
         TM    XVD2,XVD2WTOR                                            01416000
         BZ    @RF00315                NO, BRANCH                       01417000
*                                      YES, WTOR                        01418000
         MVI   XVREASON,D23ZERO        ZERO TEXT LENGTH WTOR            01419000
         B     VWTOVALB                                                 01420000
*                                                                       01421000
*           ELSE                    /* NO, INDICATE RETURN ZERO MSG     01422000
*                                      ID TO                         */ 01423000
*             XVD2ZERO='1'B;        /* THE USER                      */ 01424000
@RF00315 OI    XVD2,XVD2ZERO                                            01425000
         B     VWTOERRT                                                 01426000
*                                                                       01427000
*/********************************************************************/ 01428000
*/*                                                                  */ 01429000
*/* MESSAGE LENGTH WAS NOT ZERO. PROCESS GOOD MESSAGE                */ 01430000
*/*                                                                  */ 01431000
*/********************************************************************/ 01432000
*         ELSE                                                          01433000
*           DO;                                                         01434000
*/********************************************************************/ 01435000
*/*                                                                  */ 01436000
*/*      CHECK FOR WRITE TO PROGRAMMER (ROUTE CODE 11).              */ 01437000
*/*      IF WTP WAS SPECIFIED THEN CALL WTP. WTP WILL RETURN         */ 01438000
*/*      CONTROL TO VWTO. ON RETURN FROM WTP THE XVD0USER BIT        */ 01439000
*/*      WILL BE ON IF THE MESSAGE WAS A WTP MESSAGE ONLY.           */ 01440000
*/*      THIS MEANS THAT THERE IS NOTHING FOR VWTO TO DO.            */ 01441000
*/*                                                                  */ 01442000
*/********************************************************************/ 01443000
*             XVD0USER='0'B;        /* INSURE THAT RETURN BIT FROM      01444000
*                                      WTP IS SET TO INDICATE           01445000
*                                      CONTINUE PROCESSING           */ 01446000
@RF00314 NI    XVD0,255-XVD0USER                                        01447000
*             IF WPLMCSFA = '1'B &     /* ROUTE CODES SPECIFIED ?    */ 01448000
*                 WPLROUTK='1'B THEN/* AND WTP SPECIFIED             */ 01449000
         TM    WKAMCSF,WPLMCSFA        ROUTING AND DESC PROVIDED ?      01450000
         BZ    @RF00320                NO, BRANCH                       01451000
         TM    WKAROC+1,WPLROUTK       WTP ROUTCDE=11 ?                 01452000
         BZ    @RF00320                NO, BRANCH                       01453000
*               DO;                 /* YES, CALL WTP. WTP WILL RETURN   01454000
*                                      CONTROL AFTER PROCESSING      */ 01455000
*                 CALL IGC0203E;    /* CALL WTP (IGC0203E)           */ 01456000
         L     R15,VIGC0203                                             01457000
         BALR  R14,R15                                                  01458000
*               END;                                                    01459000
*             /*******************************************************/ 01460000
*             /*                                                     */ 01461000
*             /* CHECK IF WTP FOUND THAT THERE WAS NO MORE PROCESSING*/ 01462000
*             /* FOR US TO DO WITH THIS MESSAGE. IF SO SET XVD2ZERO  */ 01463000
*             /* ON SO THAT A ZERO MESSAGE ID GETS PASSED TO THE     */ 01464000
*             /* USER.                                               */ 01465000
*             /*                                                     */ 01466000
*             /*******************************************************/ 01467000
*             IF XVD0USER='1'B THEN /* DID RETURN TO USER BIT GET SET   01468000
*                                      BY THE WTP ROUTINE            */ 01469000
@RF00320 TM    XVD0,XVD0USER                                            01470000
         BZ    @RF00327                NO, BRANCH                       01471000
*               XVD2ZERO='1'B;      /* YES. INDICATE ZERO MSG ID TO     01472000
*                                      BE RETURNED TO CALLER AND SKIP   01473000
*                                      THE REST OF THE PROCESSING    */ 01474000
         OI    XVD2,XVD2ZERO                                            01475000
         B     VWTOERRT                SETUP FOR RETURN TO CALLER       01476000
*                                                                       01477000
*             ELSE                  /* NO, PROCESS THE MESSAGE       */ 01478000
*               DO;                                                     01479000
*/********************************************************************/ 01480000
*/*                                                                  */ 01481000
*/* SET UP NEEDED LOCKS AND FRR                                      */ 01482000
*/*                                                                  */ 01483000
*/********************************************************************/ 01484000
*                 CALL SETLOCK;                                         01485000
@RF00327 BAL   R14,SETLOCK                                              01486000
*                 /***************************************************/ 01487000
*                 /*                                                 */ 01488000
*                 /* SET UP THE FOOTPRINT TO COVER THE BUILDING OF   */ 01489000
*                 /* THE ORE AND THE WQES. IF THERE IS A PROBLEM THEN*/ 01490000
*                 /* VWTOCLNP WILL ATTEMPT TO CLEAN UP THE CONTROL   */ 01491000
*                 /* BLOCKS                                          */ 01492000
*                 /*                                                 */ 01493000
*                 /***************************************************/ 01494000
*                 PARMFTPT=FTCTLBLK;/* START OF CONTROL BLOCK           01495000
*                                      PROCESSING                    */ 01496000
         MVI   PARMFTPT,X'07'                                           01497000
*                 PARMCLAD=ADDR(VWTOCLNP);/* ADDRESS OF CLEAN UP        01498000
*                                      ROUTINE                       */ 01499000
         LA    R8,VWTOCLNP                                              01500000
         ST    R8,PARMCLAD             SAVE ADDR IN FRR/ESTAE PARMLIST  01501000
*/********************************************************************/ 01502000
*/*                                                                  */ 01503000
*/*      GET THE NEEDED CONTROL BLOCKS                               */ 01504000
*/*                                                                  */ 01505000
*/********************************************************************/ 01506000
*/*                                                                  */ 01507000
*/*   GETBLKS - GET AN ORE AND/OR A WQE                              */ 01508000
*/*                                                                  */ 01509000
*/*   INPUT - XVD2WTOR IF ON INDICATES A WQE AND AN ORE ARE NEEDED   */ 01510000
*/*           IF OFF THEN JUST A WQE IS NEEDED.                      */ 01511000
*/*           XVD1PRIV INDICATES WHETHER THIS USER IS A PRIVILEGED   */ 01512000
*/*           TASK                                                   */ 01513000
*/*                                                                  */ 01514000
*/*   OUTPUT - XVOREAD POINTS AT THE ZEROED ORE                      */ 01515000
*/*            XVWQEAD POINTS AT THE ZEROED WQE                      */ 01516000
*/*            XVD1PERR IS ON IF CORE WAS NOT AVAILABLE              */ 01517000
*/*                                                                  */ 01518000
*/********************************************************************/ 01519000
*/*                                                                  */ 01520000
*/*  TURN OFF DO LOOP AND ECB INDICATORS                             */ 01521000
*/*                                                                  */ 01522000
*/********************************************************************/ 01523000
*                 XVD1ALDN='0'B;    /* INIT TO NOT ALL DONE          */ 01524000
         NI    XVD1,255-XVD1ALDN                                        01525000
*                 XVD0WWB='0'B;     /* INDICATE NO WTO WAIT BLOCK       01526000
*                                      OBTAINED                      */ 01527000
         NI    XVD0,255-XVD0WWB                                         01528000
*/********************************************************************/ 01529000
*/*                                                                  */ 01530000
*/*  CHECK FOR A WTOR. IF SO INDICATE THAT AN ORE IS NEEDED FIRST    */ 01531000
*/*                                                                  */ 01532000
*/********************************************************************/ 01533000
*                 IF XVD2WTOR='1'B THEN/* IS THIS REQUEST A WTOR     */ 01534000
         TM    XVD2,XVD2WTOR                                            01535000
         BZ    @RF00335                NO, BRANCH                       01536000
*                   DO;             /* YES                           */ 01537000
*                     XVD0NORE='1'B;/* GET THE ORE FIRST             */ 01538000
*                     XVD0NWQE='0'B;/* THEN GET THE WQE              */ 01539000
         OI    XVD0,XVD0NORE                                            01540000
         NI    XVD0,255-XVD0NWQE                                        01541000
         B     @RC00335                                                 01542000
*                                                                       01543000
*                   END;                                                01544000
*                 ELSE              /* NO, THIS IS A WTO             */ 01545000
*                   DO;                                                 01546000
*                     XVD0NORE='0'B;/* WE DONT NEED AN ORE           */ 01547000
*                     XVD0NWQE='1'B;/* BUT WE DO NEED A WQE          */ 01548000
@RF00335 OI    XVD0,XVD0NWQE                                            01549000
         NI    XVD0,255-XVD0NORE                                        01550000
@RC00335 B     @DE00344                                                 01551000
*                                                                       01552000
*                   END;                                                01553000
*/********************************************************************/ 01554000
*/*                                                                  */ 01555000
*/*  WE WILL ATTEMPT TO GET A WQE OR AN ORE, A REPLY ID AND A WQE,   */ 01556000
*/*  WE WILL LOOP UNTIL ALL NEEDED BLOCKS HAVE BEEN OBTAINED.        */ 01557000
*/*                                                                  */ 01558000
*/********************************************************************/ 01559000
*                 DO WHILE(XVD1ALDN='0'B);/* LOOP UNTIL WE ARE DONE  */ 01560000
*/********************************************************************/ 01561000
*/*                                                                  */ 01562000
*/*  CHECK IF WE STILL NEED TO GET AN ORE. IF SO GET ONE             */ 01563000
*/*                                                                  */ 01564000
*/********************************************************************/ 01565000
*                                                                       01566000
*                   IF XVD0NORE='1'B THEN                               01567000
*                                                                       01568000
@DL00344 TM    XVD0,XVD0NORE                                            01569000
         BZ    @RF00345                                                 01570000
*/********************************************************************/ 01571000
*/*                                                                  */ 01572000
*/*  CHECK IF AN ORE IS AVAILABLE                                    */ 01573000
*/*                                                                  */ 01574000
*/********************************************************************/ 01575000
*                     IF UCMRQNR<UCMRQLM /* IS THE NUMBER ORES LESS     01576000
*                                      THAN LIMIT                    */ 01577000
*                         XVD1PRIV='1'B THEN/* OR IS THE USER A         01578000
*                                      PRIVILEGED TASK               */ 01579000
         L     R15,CVTCUCB                                              01580000
         USING UCM,R15                                                  01581000
         LH    R8,UCMRQNR                                               01582000
         SLR   R2,R2                                                    01583000
         IC    R2,UCMRQLM                                               01584000
         CR    R8,R2                                                    01585000
         BL    @RT00346                                                 01586000
         TM    XVD1,XVD1PRIV           PRIVILIGED ?                     01587000
         BZ    @RF00346                NO, BRANCH                       01588000
*                       DO;         /* YES, GET AN ORE AND ID        */ 01589000
*                         CALL VWTOGETB(2);/* GET AN ORE AND ZERO IT */ 01590000
@RT00346 LA    R1,@AL00348                                              01591000
         BAL   R14,VWTOGETB                                             01592000
*/********************************************************************/ 01593000
*/*                                                                  */ 01594000
*/* CHECK TO SEE IF WE SUCCESSFULLY GOT AN ORE. IF NOT THEN SET      */ 01595000
*/* THE ERROR FLAG AND SET OTHER FLAGS SO THAT WE GET OUT OF         */ 01596000
*/* GET CONTROL BLOCKS LOOP.                                         */ 01597000
*/*                                                                  */ 01598000
*/********************************************************************/ 01599000
*                         IF XVOREAD=1 THEN/* WAS ERROR CONDITION SET   01600000
*                                      BY VWTOGETB                   */ 01601000
         CLC   XVOREAD,KF1                                              01602000
         BNE   @RF00349                                                 01603000
*                           DO;     /* YES. SET FLAGS TO GET OUT OF     01604000
*                                      LOOP                          */ 01605000
*                             XVD1ALDN='1'B;/* INDICATE THAT LOOP IS    01606000
*                                      OVER                          */ 01607000
*                             XVD1PERR='1'B;/* INDICATE AN ERROR        01608000
*                                      CONDITION FOUND               */ 01609000
         OI    XVD1,XVD1ALDN+XVD1PERR                                   01610000
*                             XVD0RPFD='1'B;/* SET SO THAT WE SKIP TO   01611000
*                                      THE WQE TEST AND THEN OUT     */ 01612000
         OI    XVD0,XVD0RPFD                                            01613000
         B     @RC00349                                                 01614000
*                                                                       01615000
*                           END;                                        01616000
*                         ELSE      /* AN ORE WAS OBTAINED. GET AN ID*/ 01617000
*                           DO;                                         01618000
*/********************************************************************/ 01619000
*/*  GETID - GET A REPLY ID SEGMENT                                  */ 01620000
*/*                                                                  */ 01621000
*/*  FUNCTION - TO OBTAIN ONE OF THE 100 REPLY IDS AND INSERT THE    */ 01622000
*/*     ID IN THE ORE.                                               */ 01623000
*/*                                                                  */ 01624000
*/*  INPUT - XVOREAD -> ORE                                          */ 01625000
*/*          UCMRPYL CONTAINS THE NEXT REPLY ID IN UCMRPYI TO TRY    */ 01626000
*/*          UCMRPYI IS THE BIT MAP OF AVAILABLE IDS                 */ 01627000
*/*                                                                  */ 01628000
*/*  OUTPUT - XVD0RPFD IS ON IF ID WAS STORED IN THE ORE             */ 01629000
*/*           XVD0RPFD IS OFF IF ID WAS NOT AVAILABLE                */ 01630000
*/*                                                                  */ 01631000
*/********************************************************************/ 01632000
*                             XVD0RPFD='0'B;/* DON'T TURN REPLY FOUND   01633000
*                                      ON UNTIL ID IS AVAILABLE      */ 01634000
@RF00349 NI    XVD0,255-XVD0RPFD                                        01635000
*                             STARTID=UCMRPYL;/* INIT THE STARTING ID*/ 01636000
         L     R15,CVTCUCB                                              01637000
         L     R15,UCMLSTP                                              01638000
         DROP  R15                                                      01639000
         USING UCMEIL,R15                                               01640000
         MVC   STARTID,UCMRPYL                                          01641000
         DROP  R15                                                      01642000
*                             DO UNTIL(XVD0RPFD='1'B /* DO UNTIL AN     01643000
*                                      ID IS FOUND                   */ 01644000
*                                   UCMRPYL=STARTID);/* OR ALL THE      01645000
*                                      ID'S HAVE BEEN SEARCHED       */ 01646000
*/********************************************************************/ 01647000
*/*                                                                  */ 01648000
*/* WE ADD ONE IN THE FOLLOWING TWO INSTRUCTIONS IN ORDER TO CONVERT */ 01649000
*/* THE UCMRPYL FROM ZERO ORIGIN INDEX TO A ONE ORIGIN INDEX VALUE   */ 01650000
*/* USED BY PL/S II                                                  */ 01651000
*/*                                                                  */ 01652000
*/********************************************************************/ 01653000
*                               R1=UCMRPYL/8+1;/* DEVELOP BYTE          01654000
*                                      OFFSET INTO UCMRPYI           */ 01655000
         DROP  R10                     USE R10 AS CODE TOO MESSY TO     01656000
*                                      CHANGE TO ANOTHER REG            01657000
@DL00358 LA    R10,1                                                    01658000
         L     R8,CVTCUCB                                               01659000
         USING UCM,R8                                                   01660000
         L     R15,UCMLSTP                                              01661000
         DROP  R8                                                       01662000
         SLR   R14,R14                                                  01663000
         USING UCMEIL,R15                                               01664000
         IC    R14,UCMRPYL                                              01665000
         DROP  R15                                                      01666000
         LR    R1,R14                                                   01667000
         SRL   R1,3                                                     01668000
         ALR   R1,R10                                                   01669000
*                               R2=UCMRPYL//8+1;/* DEVELOP BYTE         01670000
*                                      OFFSET INTO BIT MAP           */ 01671000
         ST    R15,@TF00001                                             01672000
         SRDA  R14,32                                                   01673000
         D     R14,KF8                                                  01674000
         ALR   R14,R10                                                  01675000
         LR    R2,R14                                                   01676000
*/********************************************************************/ 01677000
*/*                                                                  */ 01678000
*/* CHECK IF THE UCMRPYL-TH BIT IS OFF. IF SO TURN IT ON AND CONVERT */ 01679000
*/* THE ID                                                           */ 01680000
*/*                                                                  */ 01681000
*/********************************************************************/ 01682000
*                               IF(UCMRPYI(R1)&IDMAP(R2))^=IDMAP(R2)    01683000
*                                   THEN                                01684000
         LR    R10,R8                                                   01685000
         ALR   R10,R1                                                   01686000
         USING UCM,R10                                                  01687000
         MVC   @TS00001(1),UCMRPYI-1                                    01688000
         DROP  R10                                                      01689000
         LA    R10,IDMAP-1(R2)                                          01690000
         NC    @TS00001(1),0(R10)                                       01691000
         LA    R10,IDMAP-1(R2)                                          01692000
         CLC   @TS00001(1),0(R10)                                       01693000
         BE    @RF00362                                                 01694000
*                                 DO;/* YES, ID IS AVAILABLE         */ 01695000
*                                   CVD(UCMRPYL,CVDAREA);/* CONVERT     01696000
*                                      BIT ID INTO DECIMAL IN 8 BYTE    01697000
*                                      AREA -CVDAREA                 */ 01698000
*                                                                       01699000
         L     R10,@TF00001                                             01700000
         USING UCMEIL,R10                                               01701000
         SLR   R0,R0                                                    01702000
         IC    R0,UCMRPYL                                               01703000
         DROP  R10                                                      01704000
         CVD   R0,CVDAREA                                               01705000
*                                   /*********************************/ 01706000
*                                   /*                               */ 01707000
*                                   /* UNPACK ONLY THE LAST TWO BYTES*/ 01708000
*                                   /* OF CONVERTED NUMBER           */ 01709000
*                                   /*                               */ 01710000
*                                   /*********************************/ 01711000
*                                                                       01712000
*                                   UNPK(OREID,CVDAREA(7:8));           01713000
*                                                                       01714000
         L     R10,XVOREAD                                              01715000
         USING OREF,R10                                                 01716000
         UNPK  OREID,CVDAREA+6(2)                                       01717000
*                                   /*********************************/ 01718000
*                                   /*                               */ 01719000
*                                   /* REPLACE SIGN WITH 'F0' ZONE   */ 01720000
*                                   /* BITS                          */ 01721000
*                                   /*                               */ 01722000
*                                   /*********************************/ 01723000
*                                                                       01724000
*                                   OREID(2)=OREID(2) 'F0'X;            01725000
         OI    OREID+1,X'F0'                                            01726000
         DROP  R10                                                      01727000
*                                   XVD0RPFD='1'B;/* INDICATE AN ID     01728000
*                                      WAS FOUND                     */ 01729000
         OI    XVD0,XVD0RPFD                                            01730000
*                                   /*********************************/ 01731000
*                                   /*                               */ 01732000
*                                   /* TURN ON ID BIT IN BIT MAP     */ 01733000
*                                   /*                               */ 01734000
*                                   /*********************************/ 01735000
*                                   UCMRPYI(R1)=UCMRPYI(R1) IDMAP(R2);  01736000
         ALR   R8,R1                                                    01737000
         LA    R10,IDMAP-1(R2)                                          01738000
         USING UCM,R8                                                   01739000
         OC    UCMRPYI-1(1),0(R10)                                      01740000
         DROP  R8                                                       01741000
*                                   REG1SAV=R1;/* SAVE CONTENTS OF      01742000
*                                      R1 IN CASE WE HAVE TO GIVE       01743000
*                                      BACK THE ID                   */ 01744000
         ST    R1,REG1SAV                                               01745000
*                                   REG2SAV=R2;/* SAVE CONTENTS OF      01746000
*                                      R2 FOR SAME REASON            */ 01747000
         ST    R2,REG2SAV                                               01748000
*                                 END;                                  01749000
*                               IF UCMRPYL=NINTY9 THEN/* END OF ID'S    01750000
*                                      REACHED?                      */ 01751000
@RF00362 L     R10,CVTCUCB                                              01752000
         USING UCM,R10                                                  01753000
         L     R10,UCMLSTP                                              01754000
         DROP  R10                                                      01755000
         USING UCMEIL,R10                                               01756000
         CLI   UCMRPYL,99                                               01757000
         BNE   @RF00372                                                 01758000
*                                 UCMRPYL=0;/* YES, GO BACK TO 1ST ID   01759000
*                                                                    */ 01760000
         MVI   UCMRPYL,X'00'                                            01761000
         DROP  R10                                                      01762000
*                               ELSE/* NO, INCREMENT UCMRPYL. THIS IS   01763000
*                                      DONE EVEN THOUGH AN ID MAY BE    01764000
*                                      PICKED, SO THAT UCMRPYL IS       01765000
*                                      INITIALIZED FOR THE NEXT ID      01766000
*                                      SCAN                          */ 01767000
*                                 UCMRPYL=UCMRPYL+1;                    01768000
         B     @DE00358                                                 01769000
*                                                                       01770000
@RF00372 L     R10,CVTCUCB                                              01771000
         USING UCM,R10                                                  01772000
         L     R10,UCMLSTP                                              01773000
         DROP  R10                                                      01774000
         LA    R8,1                                                     01775000
         SLR   R0,R0                                                    01776000
         USING UCMEIL,R10                                               01777000
         IC    R0,UCMRPYL                                               01778000
         ALR   R8,R0                                                    01779000
         STC   R8,UCMRPYL                                               01780000
         DROP  R10                                                      01781000
*                             END;  /* OF DO UNTIL (FINDING A FREE      01782000
*                                      ID)                           */ 01783000
@DE00358 TM    XVD0,XVD0RPFD                                            01784000
         BO    @RC00349                                                 01785000
         L     R10,CVTCUCB                                              01786000
         USING UCM,R10                                                  01787000
         L     R10,UCMLSTP                                              01788000
         DROP  R10                                                      01789000
         USING UCMEIL,R10                                               01790000
         CLC   UCMRPYL,STARTID                                          01791000
         BNE   @DL00358                LOOP BACK                        01792000
         DROP  R10                                                      01793000
*/****** END OF GETID SEGMENT ****************************************/ 01794000
*                           END;                                        01795000
*/********************************************************************/ 01796000
*/*                                                                  */ 01797000
*/*  CHECK TO SEE IF AN ID WAS AVAILABLE                             */ 01798000
*/*                                                                  */ 01799000
*/********************************************************************/ 01800000
*                                                                       01801000
*        RESTORE PARMLIST ADDRESSABILITY                                01802000
@RC00349 L     R10,ESTAEPRM                                             01803000
         USING PARMLIST,R10                                             01804000
*                         IF XVD0RPFD='1'B THEN/* WAS A REPLY ID        01805000
*                                      FOUND                         */ 01806000
         TM    XVD0,XVD0RPFD                                            01807000
         BZ    @RF00378                NO, BRANCH                       01808000
*                           DO;     /* YES, INDICATE WE NOW NEED A      01809000
*                                      WQE                           */ 01810000
*                             XVD0NORE='0'B;/* PROCESSING DONE FOR AN   01811000
*                                      ORE                           */ 01812000
*                             XVD0NWQE='1'B;/* GET A WQE             */ 01813000
         OI    XVD0,XVD0NWQE                                            01814000
         NI    XVD0,255-XVD0NORE                                        01815000
         B     @RF00345                                                 01816000
*                                                                       01817000
*                           END;                                        01818000
*                         ELSE      /* NO, A REPLY WASNT AVAILABLE   */ 01819000
*                           DO;     /* FREE THE ORE JUST OBTAINED    */ 01820000
*                             CALL VWTOFORE;/* FREE THE ORE          */ 01821000
@RF00378 BAL   R14,VWTOFORE                                             01822000
*                             /***************************************/ 01823000
*                             /*                                     */ 01824000
*                             /* WAIT FOR AN ORE TO BE FREED         */ 01825000
*                             /*                                     */ 01826000
*                             /***************************************/ 01827000
*                             CALL VWTOWAIT(UCMOECBT);                  01828000
         L     R15,CVTCUCB                                              01829000
         USING UCM,R15                                                  01830000
         LA    R15,UCMOECBT                                             01831000
         ST    R15,PARMPTR                                              01832000
         LA    R1,PARMPTR                                               01833000
         BAL   R14,VWTOWAIT                                             01834000
         B     @RF00345                                                 01835000
*                                                                       01836000
*                           END;                                        01837000
*                       END;        /* OF GET ORE AND ID PART        */ 01838000
*/********************************************************************/ 01839000
*/*                                                                  */ 01840000
*/*  NO, AN ORE WASN'T AVAILABLE. WAIT FOR AN ORE TO BE FREE'D       */ 01841000
*/*                                                                  */ 01842000
*/********************************************************************/ 01843000
*                     ELSE                                              01844000
*                       CALL VWTOWAIT(UCMOECBT);                        01845000
@RF00346 L     R15,CVTCUCB                                              01846000
         LA    R15,UCMOECBT                                             01847000
         ST    R15,PARMPTR                                              01848000
         LA    R1,PARMPTR                                               01849000
         BAL   R14,VWTOWAIT                                             01850000
*/********************************************************************/ 01851000
*/*                                                                  */ 01852000
*/*  CHECK IF WE NEED A WQE                                          */ 01853000
*/*                                                                  */ 01854000
*/********************************************************************/ 01855000
*                   IF XVD0NWQE='1'B&/* IS A WQE NEEDED NOW          */ 01856000
*                       XVD1PERR='0'B THEN/* AND WAS THERE NO ERROR     01857000
*                                      IN GETTING AN ORE             */ 01858000
@RF00345 TM    XVD0,XVD0NWQE                                            01859000
         BZ    @DE00344                                                 01860000
         TM    XVD1,XVD1PERR                                            01861000
         BNZ   @DE00344                                                 01862000
*/********************************************************************/ 01863000
*/*                                                                  */ 01864000
*/*  YES, CHECK IF ONE IS AVAILABLE, IF SO, GET IT. IF NOT WAIT      */ 01865000
*/*  UNTIL ONE IS FREE'D,                                            */ 01866000
*/*                                                                  */ 01867000
*/********************************************************************/ 01868000
*                     IF UCMWQNR<UCMWQLM /* IS THE NUMBER OF WQES       01869000
*                                      LESS THAN THE LIMIT OR        */ 01870000
*                         XVD1PRIV='1'B THEN/* IS THE USER A            01871000
*                                      PRIVILEGED TASK               */ 01872000
         L     R15,CVTCUCB                                              01873000
         LH    R8,UCMWQNR                                               01874000
         CH    R8,UCMWQLM                                               01875000
         BL    @RT00390                                                 01876000
         DROP  R15                                                      01877000
         TM    XVD1,XVD1PRIV           PRIVILEGED CALLER ?              01878000
         BZ    @RF00390                NO, BRANCH                       01879000
*                       DO;         /* YES, GET THE WQE              */ 01880000
*                         CALL VWTOGETB(1);/* GET A WQE AND ZERO IT  */ 01881000
@RT00390 LA    R1,@AL00392                                              01882000
         BAL   R14,VWTOGETB                                             01883000
*/********************************************************************/ 01884000
*/*                                                                  */ 01885000
*/* CHECK IF A WQE WAS OBTAINED BY VWTOGETB.  IF NOT THEN CHECK IF   */ 01886000
*/* THIS IS A WTOR. IF SO THEN FREE THE ORE AND REPLY ID THAT WAS    */ 01887000
*/* OBTAINED.                                                        */ 01888000
*/*                                                                  */ 01889000
*/********************************************************************/ 01890000
*                         IF XVWQEAD=1 THEN/* WAS THERE AN ERROR IN     01891000
*                                      GETTING THE WQE?              */ 01892000
         CLC   XVWQEAD,KF1                                              01893000
         BNE   @RF00393                                                 01894000
*                           DO;     /* YES, CHECK IF THIS IS A WTOR     01895000
*                                      REQUEST                       */ 01896000
*                             IF XVD2WTOR='1'B THEN/* IS THIS A WTOR?*/ 01897000
         TM    XVD2,XVD2WTOR                                            01898000
         BZ    @RF00395                NO, BRANCH                       01899000
*                               DO; /* YES. FREE THE ORE AND REPLY ID*/ 01900000
*                                 CALL VWTOFORE;/* FREE THE ORE      */ 01901000
*                                                                       01902000
         BAL   R14,VWTOFORE                                             01903000
*                                 /***********************************/ 01904000
*                                 /*                                 */ 01905000
*                                 /* FREE THE REPLY ID JUST PICKED   */ 01906000
*                                 /* THE INDEXS TO UCMRPYI AND IDMAP */ 01907000
*                                 /* WERE SAVED BY GETID IN REG1SAV  */ 01908000
*                                 /* AND REG2SAV                     */ 01909000
*                                 /*                                 */ 01910000
*                                 /***********************************/ 01911000
*                             UCMRPYI(REG1SAV)=UCMRPYI(REG1SAV)&(-1&&   01912000
*                                     IDMAP(REF2SAV));                  01913000
         L     R14,REG1SAV                                              01914000
         L     R8,CVTCUCB                                               01915000
         USING UCM,R8                                                   01916000
         L     R2,REG2SAV             RESTORE R2                        01917000
         SLR   R15,R15                                                  01918000
         IC    R15,IDMAP-1(R2)                                          01919000
         X     R15,KFM1                                                 01920000
         SLR   R2,R2                                                    01921000
         IC    R2,UCMRPYI-1(R14)                                        01922000
         NR    R15,R2                                                   01923000
         STC   R15,UCMRPYI-1(R14)                                       01924000
         DROP  R8                                                       01925000
*                               END;/* OF FREEING ORE                */ 01926000
*                             XVD1PERR='1'B;/* INDICATE AN ERROR        01927000
*                                      CONDITION FOUND               */ 01928000
*                           END;    /* IN ERROR GETTING WQE          */ 01929000
@RF00395 OI    XVD1,XVD1PERR                                            01930000
*                         XVD1ALDN='1'B;/* INDICATE WE ARE ALL DONE  */ 01931000
@RF00393 OI    XVD1,XVD1ALDN                                            01932000
         B     @DE00344                                                 01933000
*                                                                       01934000
*                       END;                                            01935000
*                     ELSE          /* NO, A WQE IS NOT AVAILABLE    */ 01936000
*                       DO;                                             01937000
*                                                                       01938000
*/********************************************************************/ 01939000
*/*                                                                  */ 01940000
*/*  CHECK IF THIS IS A WTOR. IF SO FREE THE ORE AND REPLY ID. THIS  */ 01941000
*/*  IS DONE SO WE DON'T GET IN AN INTERLOCK SITUATION               */ 01942000
*/*                                                                  */ 01943000
*/********************************************************************/ 01944000
*                         IF XVD2WTOR='1'B THEN/* IS THIS A WTOR     */ 01945000
*                           DO;     /* YES                           */ 01946000
@RF00390 TM    XVD2,XVD2WTOR                                            01947000
         BZ    @RF00405                NO, BRANCH                       01948000
*                             CALL VWTOFORE;/* FREE THE ORE          */ 01949000
         BAL   R14,VWTOFORE                                             01950000
*                             /***************************************/ 01951000
*                             /*                                     */ 01952000
*                             /* FREE THE REPLY ID JUST PICKED. THE  */ 01953000
*                             /* INDEXS TO UCMRPYI AND IDMAP WERE    */ 01954000
*                             /* SAVED BY GETID IN REG1SAV           */ 01955000
*                             /* AND REG2SAV                         */ 01956000
*                             /***************************************/ 01957000
*                       UCMRPYI(REG1SAV)=UCMRPYI(REG1SAV)&(-1&&IDMAP(   01958000
*                             REG2SAV));                                01959000
         L     R14,REG1SAV                                              01960000
         L     R8,CVTCUCB                                               01961000
         L     R2,REG2SAV              RESTORE R2                       01962000
         SLR   R15,R15                                                  01963000
         IC    R15,IDMAP-1(R2)                                          01964000
         X     R15,KFM1                                                 01965000
         SLR   R2,R2                                                    01966000
         USING UCM,R8                                                   01967000
         IC    R2,UCMRPYI-1(R14)                                        01968000
         NR    R15,R2                                                   01969000
         STC   R15,UCMRPYI-1(R14)                                       01970000
*                             /***************************************/ 01971000
*                             /*                                     */ 01972000
*                             /* NOW BACK UP UCMRPYL SO WE WILL TRY  */ 01973000
*                             /* TO GET THE SAME REPLY               */ 01974000
*                             /*                                     */ 01975000
*                             /***************************************/ 01976000
*                             IF UCMRPYL=0 THEN/* ARE WE AT BEGINNING   01977000
*                                      OF IDS                        */ 01978000
         L     R15,UCMLSTP                                              01979000
         DROP  R8                                                       01980000
         USING UCMEIL,R15                                               01981000
         CLI   UCMRPYL,0                                                01982000
         BNE   @RF00409                                                 01983000
*                               UCMRPYL=NINTY9;/* YES. BACK UP TO       01984000
*                                      99TH ID                       */ 01985000
         MVI   UCMRPYL,99                                               01986000
         DROP  R15                                                      01987000
         B     @RC00409                                                 01988000
*                                                                       01989000
*                             ELSE  /* NO WE ARE NOT AT ID 0         */ 01990000
*                               UCMRPYL=UCMRPYL-1;/* BACK UP JUST ONE*/ 01991000
@RF00409 L     R15,CVTCUCB                                              01992000
         USING UCM,R15                                                  01993000
         L     R15,UCMLSTP                                              01994000
         DROP  R15                                                      01995000
         USING UCMEIL,R15                                               01996000
         SLR   R8,R8                                                    01997000
         IC    R8,UCMRPYL                                               01998000
         BCTR  R8,0                                                     01999000
         STC   R8,UCMRPYL                                               02000000
         DROP  R15                                                      02001000
*                             XVD0NORE='1'B;/* INDICATE START OVER IN   02002000
*                                      GETTING ORE                   */ 02003000
*                             XVD0NWQE='0'B;                            02004000
@RC00409 OI    XVD0,XVD0NORE                                            02005000
         NI    XVD0,255-XVD0NWQE                                        02006000
*                           END;                                        02007000
*/********************************************************************/ 02008000
*/*                                                                  */ 02009000
*/* NOW WAIT FOR A WQE TO BE FREE'D. THEN TRY AGAIN                  */ 02010000
*/*                                                                  */ 02011000
*/********************************************************************/ 02012000
*                         CALL VWTOWAIT(UCMWECBT);                      02013000
@RF00405 L     R15,CVTCUCB                                              02014000
         USING UCM,R15                                                  02015000
         LA    R15,UCMWECBT                                             02016000
         DROP  R15                                                      02017000
         ST    R15,PARMPTR                                              02018000
         LA    R1,PARMPTR                                               02019000
         BAL   R14,VWTOWAIT                                             02020000
*                       END;                                            02021000
*                 END;              /* OF DO WHILE LOOP              */ 02022000
@DE00344 TM    XVD1,XVD1ALDN                                            02023000
         BZ    @DL00344                                                 02024000
*/* **** END GETBLKS SEGMENT *****************************************/ 02025000
*                                                                       02026000
*/********************************************************************/ 02027000
*/*                                                                  */ 02028000
*/* NOW CHECK IF THE CONTROL BLOCKS WERE OBTAINED. IF SO THEN GO     */ 02029000
*/* AHEAD AND BUILD THEM. IF NOT THEN ABEND WITH A D23 ABEND CODE.   */ 02030000
*/*                                                                  */ 02031000
*/********************************************************************/ 02032000
*                 IF XVD1PERR='0'B THEN/* SUCCESSFUL IN GETTING THE     02033000
*                                         CONTROL BLOCKS (NO ERRORS)?*/ 02034000
         TM    XVD1,XVD1PERR                                            02035000
         BO    @RF00418                                                 02036000
*                   DO;             /* YES. BUILD THEM               */ 02037000
*/********************************************************************/ 02038000
*/*                                                                  */ 02039000
*/* CHECK FOR WTOR. IF THIS REQUEST IS A WTOR THEN BUILD AN ORE      */ 02040000
*/*                                                                  */ 02041000
*/********************************************************************/ 02042000
*                     IF XVD2WTOR='1'B THEN/* IS THIS REQUEST A WTOR */ 02043000
         TM    XVD2,XVD2WTOR                                            02044000
         BZ    @RF00420                       NO, BRANCH                02045000
*                       DO;                                             02046000
*/*** START OF BUILDORE **********************************************/ 02047000
*/********************************************************************/ 02048000
*/*                                                                  */ 02049000
*/*   BUILDORE - CONSTRUCT THE ORE AND PUT IT ON THE ORE CHAIN       */ 02050000
*/*                                                                  */ 02051000
*/*   INPUT - XVWPLADR POINTS AT THE USER'S PARM LIST                */ 02052000
*/*           XVOREAD  POINTS AT A ZEROED ORE                        */ 02053000
*/*                                                                  */ 02054000
*/*   OUTPUT - THE FILLED IN ORE IS ADDED TO THE ORE CHAIN           */ 02055000
*/*            AND IS MARKED SUSPENDED FOR THE DURATION OF THE       */ 02056000
*/*            SUBSYSTEM EXIT.                                       */ 02057000
*/*                                                                  */ 02058000
*/********************************************************************/ 02059000
*/********************************************************************/ 02060000
*/*                                                                  */ 02061000
*/*           THE OREID WILL HAVE ALREADY BEEN FILLED IN BY GETID    */ 02062000
*/*                                                                  */ 02063000
*/********************************************************************/ 02064000
*                                                                       02065000
*                         ORETCB=R4;/* INSERT TCB ADDR               */ 02066000
         L     R15,XVOREAD                                              02067000
         USING OREF,R15                                                 02068000
         ST    R4,ORETCB                                                02069000
*                                        /* MOVE IN REPLY LENGTH AND    02070000
*                                      BUFFER ADDR                   */ 02071000
         MVC   ORELNTH,WKARPYLN                                         02072000
         MVC   ORERPYA,WKARPBUF+1                                       02073000
*                                        /* MOVE IN THE REPLY ECB       02074000
*                                      ADDR                          */ 02075000
         MVC   OREECB,WKAECBP                                           02076000
*                         OREASID=ASCBASID;/* MOVE ASID OF THIS         02077000
*                                      MEMORY                        */ 02078000
         MVC   OREASID,ASCBASID                                         02079000
*                         OREWQE=XVWQEAD;/* MOVE IN ADDR OF             02080000
*                                      ASSOCIATED WQE                */ 02081000
         MVC   OREWQE,XVWQEAD                                           02082000
*                         OREBUFB='1'B;/* INDICATE THIS ORE IS IN USE*/ 02083000
*                         OREBUFD='1'B;/* INDICATE ORE OBTAINED VIA A   02084000
*                                      GETMAIN                       */ 02085000
         OI    OREXC,OREBUFB+OREBUFD                                    02086000
*/********************************************************************/ 02087000
*/*                                                                  */ 02088000
*/*  CHECK IF USER IS A PROBLEM PROGRAM. IF NOT THEN INDICATE THAT   */ 02089000
*/*  VALIDITY CHECKING OF REPLY BUFFER AND ECB CAN BE BYPASSED BY    */ 02090000
*/*  REPLY PROCESSING.                                               */ 02091000
*/*                                                                  */ 02092000
*/********************************************************************/ 02093000
*                         IF XVD1PP='0'B THEN/* IS USER NOT A PROBLEM   02094000
*                                      PROGRAM                       */ 02095000
         TM    XVD1,XVD1PP                                              02096000
         BNZ   @RF00429                                                 02097000
*                           OREKEY0='1'B;/* INDICATE BYPASS VALIDITY    02098000
*                                      CHECK                         */ 02099000
         OI    OREXA,OREKEY0                                            02100000
*                         ORESUSP='1'B;/* INDICATE ORE SUSPENDED FOR    02101000
*                                      SUBSYSTEM EXIT                */ 02102000
@RF00429 L     R15,XVOREAD                                              02103000
         OI    OREXA,ORESUSP           PROCESSING TEMPORARILY SUSPENDED 02104000
*/********************************************************************/ 02105000
*/*                                                                  */ 02106000
*/*  NOW PUT THE ORE ON THE FRONT OF THE QUEUE. THE ORDER OF         */ 02107000
*/*  APPEARANCE ON THE QUEUE IS IMMATERIAL. THE OPERATOR MAY RESPOND */ 02108000
*/*  IN ANY ORDER AND REPLY PROCESSING WILL SCAN THE QUEUE FOR MATCH-*/ 02109000
*/*  ING IDS.                                                        */ 02110000
*/*                                                                  */ 02111000
*/********************************************************************/ 02112000
*                         ORELKP=UCMRPYQ;/* POINT THIS ORE AT NEXT      02113000
*                                      ORE ON QUEUE                  */ 02114000
         L     R8,CVTCUCB                                               02115000
         USING UCM,R8                                                   02116000
         L     R2,UCMRPYQ                                               02117000
         ST    R2,ORELKP               LINKAGE POINTER                  02118000
         DROP  R15                                                      02119000
*                         UCMRPYQ=XVOREAD;/* POINT HEAD OF QUEUE AT     02120000
*                                      THIS ORE                      */ 02121000
         MVC   UCMRPYQ,XVOREAD                                          02122000
         DROP  R8                                                       02123000
*/********************************************************************/ 02124000
*/*                                                                  */ 02125000
*/*  WE ASSUME UCMRPYQ IS ZERO IF QUEUE IS EMPTY AT ENTRY TO VWTO    */ 02126000
*/*                                                                  */ 02127000
*/********************************************************************/ 02128000
*/****** END OF BUILD ORE SEGMENT ************************************/ 02129000
*                                                                       02130000
*                       END;                                            02131000
*/********************************************************************/ 02132000
*/*                                                                  */ 02133000
*/* NOW BUILD THE WQE                                                */ 02134000
*/*                                                                  */ 02135000
*/********************************************************************/ 02136000
*/*** START OF BUILDWQE **********************************************/ 02137000
*/********************************************************************/ 02138000
*/*                                                                  */ 02139000
*/*   BUILDWQE - CONSTRUCT THE WQE AND PUT IT ON THE WQE CHAIN       */ 02140000
*/*                                                                  */ 02141000
*/*   INPUT - WKAADTXT -> MSG TEXT + LEN +MCS FLAGS                  */ 02142000
*/*           WKALGH    = L'TEXT + 4                                 */ 02143000
*/*           XVWQEAD  POINTS AT A ZEROED WQE                        */ 02144000
*/*                                                                  */ 02145000
*/*   OUTPUT - THE FILLED IN WQE IS ADDED TO THE WQE CHAIN           */ 02146000
*/*                                                                  */ 02147000
*/*  START BY MOVING MESSAGE INTO THE WQE. THE FIRST CHARACTER OF    */ 02148000
*/*  TEXT IN THE WQE WILL BE THE AUTHORIZATION FLAG. THE MESSAGE IS  */ 02149000
*/*  SCANNED FOR 'NL' CHARACTER AND TO ELIMINATE TRAILING BLANKS.    */ 02150000
*/*  THE LENGTH OF THE MSG IS IN WKALGH AND IS NON-ZERO              */ 02151000
*/*  THE SECURITY MESSAGE FLAGGING IN THE MESSAGE TEXT IS:           */ 02152000
*/*   MSG TYPE           MSG FORMAT                                  */ 02153000
*/*   --------           ----------                                  */ 02154000
*/*   WTO,P/P,NONACTION  BLANK,+,TEXT                                */ 02155000
*/*   WTO,P/P,ACTION     @,TEXT                                      */ 02156000
*/*   WTO,SYS,NONACTION  BLANK,TEXT                                  */ 02157000
*/*   WTO,SYS,ACTION     *,TEXT                                      */ 02158000
*/*   WTOR,P/P           @,ID,BLANK,TEXT                             */ 02159000
*/*   WTOR,SYSTEM        *,ID,BLANK,TEXT                             */ 02160000
*/*                                                                  */ 02161000
*/*   AN ACTION MSG IS A WTOR OR A WTO WITH DESCRIPTOR CODE 1 OR 2.  */ 02162000
*/* NOTE: P/P IN THE ABOVE TABLE MEANS THAT THE USER IS NOT          */ 02163000
*/*       AUTHORIZED.                                                */ 02164000
*/*                                                                  */ 02165000
*/********************************************************************/ 02166000
*                     WQETXT='';    /* BLANK OUT TEXT PART OF THE WQE*/ 02167000
@RF00420 L     R15,XVWQEAD                                              02168000
         USING WQE,R15                                                  02169000
         MVI   WQETXT,C' '                                              02170000
         MVC   WQETXT+1(L'WQETXT-1),WQETXT                              02171000
*/********************************************************************/ 02172000
*/*                                                                  */ 02173000
*/*  CHECK FOR TYPE OF MESSAGE                                       */ 02174000
*/*                                                                  */ 02175000
*/********************************************************************/ 02176000
*                     IF XVD2WTOR='1'B THEN/*  WTOR ?                */ 02177000
         TM    XVD2,XVD2WTOR                                            02178000
         BZ    @RF00437                NO, BRANCH                       02179000
*                       DO;         /* YES, IS USER NOT AUTHORIZED?  */ 02180000
*                         IF XVD1AUTH='0'B THEN                         02181000
         TM    XVD1,XVD1AUTH                                            02182000
         BO    @RF00439                                                 02183000
*                           WQETXT='@';/* YES, MOVE IN P/P ACTION       02184000
*                                      FLAG                          */ 02185000
         MVI   WQETXT,C'@'                                              02186000
         B     @RC00439                                                 02187000
*                                                                       02188000
*                         ELSE      /* NO, MOVE IN SYSTEM ACTION FLAG*/ 02189000
*                           WQETXT='*';                                 02190000
@RF00439 MVI   WQETXT,C'*'                                              02191000
*                      WQETXT+1=OREID;/* MOVE IN REPLY ID FROM ORE   */ 02192000
@RC00439 L     R8,XVOREAD                                               02193000
         USING OREF,R8                                                  02194000
         MVC   WQETXT+1(2),OREID                                        02195000
         DROP  R8                                                       02196000
*                                                                       02197000
*                         MSGBLNK=' ';/* MOVE IN A BLANK AFTER REPLY    02198000
*                                      ID                            */ 02199000
         MVI   WQETXT+3,C' '                                            02200000
         DROP  R15                                                      02201000
*                         /*******************************************/ 02202000
*                         /*                                         */ 02203000
*                         /* POINT AT FIRST LOCATION OF MSG TEXT     */ 02204000
*                         /*                                         */ 02205000
*                         /*******************************************/ 02206000
         LA    R2,4                    4 BYTES OF WQETXT USED           02207000
         B     @RC00437                                                 02208000
*                                                                       02209000
*                       END;                                            02210000
*                     ELSE          /* NO, NOT A WTOR. CHECK FURTHER */ 02211000
*/********************************************************************/ 02212000
*/*                                                                  */ 02213000
*/* ACTION MSG BY REASON OF DESCRIPTOR CODES ?                       */ 02214000
*/* CHECK THE DESC CODES RETURNED BY THE INSTALLATION EXIT           */ 02215000
*/*                                                                  */ 02216000
*/********************************************************************/ 02217000
*                       DO;                                             02218000
*                         IF WPLDESCA='1'B /* DESC CODE 1 OR         */ 02219000
*                             WPLDESCB='1'B /* DESC CODE 2 OR           02220000
*                             WPLDESCK='1'B THEN/* DESC CODE 11?     */ 02221000
@RF00437 TM    WKADSC,WPLDESCA+WPLDESCB  DESC CODE 1 OR 2 ?             02222000
         BNZ   @RT00447                  YES, BRANCH                    02223000
         TM    WKADSC+1,WPLDESCK         DESC CODE 11 CRITICAL INFO ?   02224000
         BZ    @RF00447                  NO, BRANCH                     02225000
*                           IF XVD1AUTH='0'B THEN/* YES, CHECK FOR      02226000
*                                      NOT AUTHORIZED.               */ 02227000
@RT00447 TM    XVD1,XVD1AUTH                                            02228000
         BO    @RF00448                                                 02229000
*                             DO;   /* YES, USER IS NOT AUTHORIZED   */ 02230000
*                               WQETXT='@';/* MOVE IN P/P ACTION        02231000
*                                      FLAG                          */ 02232000
         L     R15,XVWQEAD                                              02233000
         USING WQE,R15                                                  02234000
         MVI   WQETXT,C'@'                                              02235000
         LA    R2,1                    1 BYTES OF WQETXT USED           02236000
         TM    WKADSC+1,WPLDESCK       CRITICAL EVENTUAL ACTION REQ ?   02237000
         BO    @RC00437                YES, BRANCH                      02238000
*                               WPLDESCG='1'B;/* TURN ON DESC 7 SO      02239000
*                                      THE P/P ACTION MSG WILL BE       02240000
*                                      DOMMED AT TASK TERMINATION    */ 02241000
         OI    WKADSC,WPLDESCG                                          02242000
         B     @RC00437                                                 02243000
*                                                                       02244000
*                             END;                                      02245000
*                           ELSE    /* NO, THIS IS A SYSTEM ACTION      02246000
*                                      MSG                           */ 02247000
*                             DO;                                       02248000
*                               WQETXT='*';/* MOVE IN SYSTEM ACTION     02249000
*                                      FLAG                          */ 02250000
@RF00448 MVI   WQETXT,C'*'                                              02251000
         LA    R2,1                    1 BYTES OF WQETXT USED           02252000
         B     @RC00437                                                 02253000
*                                                                       02254000
*                             END;                                      02255000
*                         ELSE      /* NO, THIS IS NOT AN ACTION        02256000
*                                      MESSAGE                       */ 02257000
*                           IF XVD1AUTH='0'B THEN/* IS THIS USER NOT    02258000
*                                      AUTHORIZED?                   */ 02259000
@RF00447 TM    XVD1,XVD1AUTH                                            02260000
         BNZ   @RF00458                                                 02261000
*                             DO;   /* YES                           */ 02262000
*                               PPMSGFLG=' +';/* MOVE IN P/P NO         02263000
*                                      ACTION MSG FLAG               */ 02264000
         MVC   WQETXT(L'KCPLUS),KCPLUS                                  02265000
         LA    R2,2                    2 BYTES OF WQETXT USED           02266000
         B     @RC00437                                                 02267000
*                                                                       02268000
*                             END;                                      02269000
*                           ELSE    /* NO, THIS IS A SYSTEM NO ACTION   02270000
*                                      MSG                           */ 02271000
*                             DO;                                       02272000
*                               MSGACTN=' ';/* MOVE IN SYSTEM NO        02273000
*                                      ACTION FLAG                   */ 02274000
@RF00458 MVI   WQETXT,C' '                                              02275000
         LA    R2,1                    1 BYTES OF WQETXT USED           02276000
*                             END;                                      02277000
*                       END;        /* OF WTO MSG FLAGGING SECTION   */ 02278000
*/********************************************************************/ 02279000
*/*                                                                  */ 02280000
*/*  R2 CONTAINS THE NUMBER OF BYTES OF THE WPLTXT THAT HAVE         */ 02281000
*/*  BEEN USED FOR THE PREFIX MESSAGE FIELDS                         */ 02282000
*/*                                                                  */ 02283000
*/*  SILENTLY TRUNCATE TEXT THAT WILL NOT FIT INTO THE WQE           */ 02284000
*/*  BASED ON THE WTO/R PREFIX TEXT ALREADY MOVED INTO THE WQE       */ 02285000
*/*                                                                  */ 02286000
*/********************************************************************/ 02287000
*                                                                       02288000
*        ROUTINE MODIFIED TO IMPLEMENT SILENT TRUNCATION OF        @@@  02289000
*        MESSAGE TEXT - 18-08-20                                   @@@  02290000
*                                                                       02291000
*                       TEXTLNGT=MIN((WKALGH-4),128);                   02292000
@RC00437 LH    R8,WKALGH               R8 = L'MAJOR WQE TEXT +4         02293000
         S     R8,KF4                  R8 = L'MAJOR WQE TEXT       @@@  02294000
         LA    R1,L'WQETXT             R1 = MAXIMUM L'TEXT IN WQE  @@@  02295000
         SR    R1,R2                   MINUS CHARS ALREADY IN WQE  @@@  02296000
         CR    R8,R1                   L'WQE TEXT > MAXIMUM ?      @@@  02297000
         BNH   @RC00470                NO, BRANCH TO MOVE TEXT     @@@  02298000
         LR    R8,R1                   TRUNCATE L'TEXT TO MAX      @@@  02299000
*/********************************************************************/ 02300000
*/*                                                                  */ 02301000
*/* MOVE MESSAGE TEXT INTO WQE                                       */ 02302000
*/*                                                                  */ 02303000
*/********************************************************************/ 02304000
*                     WQETXT(R2:TEXTLNGT+R2-1)=WPLTXT;                  02305000
@RC00470 LA    R1,WQETXT(R2)           R1 -> FIRST AVAIL BYTE IN WQETXT 02306000
         L     R14,WKAADTXT            R14 -> LEN FLD(2)+MCS(2) + TEXT  02307000
         LA    R14,4(,R14)             R14 -> TEXT                      02308000
         BCTR  R8,0                                                     02309000
         EX    R8,@SM03498             MOVE WPLTXT INTO WQETEXT         02310000
*                                      MVC 0(0,R1),0(R14)               02311000
*/********************************************************************/ 02312000
*/*                                                                  */ 02313000
*/*  SCAN BACKWARDS LOOKING FOR FIRST NON BLANK CHARACTER            */ 02314000
*/*                                                                  */ 02315000
*/********************************************************************/ 02316000
         LA    R1,WQETXT(R2)           R1 -> FIRST BYTE OF MESSAGE TEXT 02317000
         AR    R1,R8                   R1 -> LAST BYTE OF MESSAGE       02318000
         LA    R8,1(R2,R8)             INTERATION COUNTER               02319000
*                                                                       02320000
@DL00475 CLI   0(R1),C' '              BLANK CHAR ?                     02321000
         BNE   @RC00477                NO, EXIT LOOP TO STORE LENGTH    02322000
         BCTR  R1,0                    YES, DECR POINTER                02323000
         BCT   R8,@DL00475             CONTINUE LOOP                    02324000
*                                                                       02325000
*/********************************************************************/ 02326000
*/*                                                                  */ 02327000
*/*      MESSAGE WAS ALL BLANK                                       */ 02328000
*/*                                                                  */ 02329000
*/********************************************************************/ 02330000
         LA    R8,1                    TEXT LENGTH SET TO 1             02331000
*                     WQENBR=TEXTLNGT;/* STORE LENGTH OF MESSAGE     */ 02332000
*                     WQESEQN=UCMCMID;/* MOVE IN MSG SEQUENCE NUMBER */ 02333000
@RC00477 ST    R8,WQENBR               STORE LENGTH OF MESSAGE          02334000
         L     R1,CVTCUCB                                               02335000
         S     R1,KF4                                                   02336000
         L     R1,0(,R1)               R1 -> UCM PREFIX                 02337000
         USING UCMPRFX,R1                                               02338000
         MVC   WQESEQN(3),UCMCMID+1                                     02339000
         DROP  R1                                                       02340000
*/********************************************************************/ 02341000
*/* CHECK IF THIS IS AN OLD STYLE MESSAGE WHICH HAS NO MCS FLAGS     */ 02342000
*/*                                                                  */ 02343000
*/********************************************************************/ 02344000
*                     IF WKAMCSF=0 THEN/* MCS FLAGS IN THE WPL ?     */ 02345000
         ICM   R0,B'0011',WKAMCSF                                       02346000
         BNZ   @RF00482                YES, BRANCH                      02347000
*                       WQEMCSA='1'B;/* IND R/D CODES IN WQ          */ 02348000
         OI    WQEMCSF1,WQEMCSA                                         02349000
         B     @RC00482                                                 02350000
*                                                                       02351000
*                     /***********************************************/ 02352000
*                     /*                                             */ 02353000
*                     /* NOTE - THE MCS FIELD IN THE WPL IS SET TO   */ 02354000
*                     /* ALLOW TOTAL MESSAGE PROCESSING BY ALL       */ 02355000
*                     /* CONSOLES RECEIVING THE ORIGINAL MESSAGE     */ 02356000
*                     /*                                             */ 02357000
*                     /***********************************************/ 02358000
*                     ELSE                                              02359000
*                       WQEMCSF=WPLMCSF;/* MOVE IN THE MCS FLAGS     */ 02360000
@RF00482 MVC   WQEMCSF,WKAMCSF                                          02361000
*                     /***********************************************/ 02362000
*                     /*                                             */ 02363000
*                     /* CHECK IF THIS IS A WTP THAT IS TO GO TO     */ 02364000
*                     /* HARDCOPY ONLY. IGC0203E WOULD HAVE SET      */ 02365000
*                     /* XVD0HDCY ON IF THIS SITUATION EXISTS        */ 02366000
*                     /*                                             */ 02367000
*                     /***********************************************/ 02368000
*                     IF XVD0HDCY='1'B THEN/* DO WE SEND THIS MSG TO    02369000
*                                      HC ONLY                       */ 02370000
@RC00482 TM    XVD0,XVD0HDCY                  NO, BRANCH                02371000
         BZ    @RF00485                                                 02372000
*                       WQEMCSG='1'B;/* YES, TURN ON HC ONLY MCS FLAG*/ 02373000
         OI    WQEMCSF1,WQEMCSG                                         02374000
*                     IF WQEMCSB='1'B /* IS R0 OR QREG0 MCS FLAG ON*/   02375000
*                         WQEMCSH='1'B THEN                             02376000
@RF00485 TM    WQEMCSF1,WQEMCSB+WQEMCSH                                 02377000
         BZ    @RF00487                                                 02378000
*                       WQEUCMID=XVCONID;/* YES. MOVE IN CONSOLE ID  */ 02379000
         MVC   WQEUCMID,XVCONID                                         02380000
*                     XVWQEID=UCMCMID;/* SAVE ID FOR RETURN TO THE      02381000
*                                        USER                        */ 02382000
@RF00487 L     R8,CVTCUCB                                               02383000
         S     R8,KF4                                                   02384000
         L     R8,0(,R8)                 R8 -> UCM PREFIX               02385000
         USING UCMPRFX,R8                                               02386000
         L     R14,UCMCMID               R15 = UCM MESSAGE ID NUMBER    02387000
         ST    R14,XVWQEID                                              02388000
*                     UCMCMID=UCMCMID+1;/* INCREMENT MESSAGE ID NUM */  02389000
         LA    R14,1(,R14)                                              02390000
         ST    R14,UCMCMID                                              02391000
         DROP  R8                                                       02392000
*                     WQEROUT=WKAROC;/* MOVE IN ROUTE CODES          */ 02393000
         MVC   WQEROUT,WKAROC                                           02394000
*                     WQEDESCD=WKADSC;/* MOVE IN DESC CODES          */ 02395000
         MVC   WQEDESCD,WKADSC                                          02396000
*                       WQEMSGTP=WKAMSGTY;/* MOVE IN MSGTYP FLAGS    */ 02397000
         MVC   WQEMSGTP,WKAMSGTY                                        02398000
*                     WQEBUFB='1'B; /* INDICATE THE WQE IS IN USE    */ 02399000
*                     WQEBUFD='1'B; /* INDICATE THE QWE                 02400000
*                                      DYNAMICALLY GOTTEN            */ 02401000
         OI    WQEAVAIL,WQEBUFB+WQEBUFD                                 02402000
*/********************************************************************/ 02403000
*/*                                                                  */ 02404000
*/* IF WTOR FILL IN REPLY ID AND SET DESC CODES                      */ 02405000
*/*                                                                  */ 02406000
*/********************************************************************/ 02407000
*                     IF XVD2WTOR='1'B THEN                             02408000
         TM    XVD2,XVD2WTOR                                            02409000
         BZ    @RF00497                NO, BRANCH                       02410000
*                       DO;                                             02411000
*                         WQERPYID=OREID;/* MOVE IN REPLY ID         */ 02412000
         L     R1,XVOREAD                                               02413000
         USING OREF,R1                                                  02414000
         MVC   WQERPYID,OREID                                           02415000
         DROP  R1                                                       02416000
*                         WQEORE='1'B;/* TURN ON ORE EXISTS BIT      */ 02417000
*                         WQEWTOR='1'B;/* INDICATE THAT THIS WQE IS     02418000
*                                      FOR A WTOR                    */ 02419000
         OI    WQEXA,WQEORE+WQEWTOR                                     02420000
*                         WQEDESCD='0200'X;/* MOVE IN DESC CODE OF 7    02421000
*                                      ONLY FOR WTOR                 */ 02422000
         MVC   WQEDESCD,KX0200                                          02423000
*                       END;        /* OF WTOR HANDLING              */ 02424000
*/********************************************************************/ 02425000
*/*                                                                  */ 02426000
*/* CHECK IF USER IS P/P. IF SO DON'T ALLOW BYPASS OF HARD COPY      */ 02427000
*/*                                                                  */ 02428000
*/********************************************************************/ 02429000
*                     IF XVD1PP='1'B THEN/* CALLER A P/P             */ 02430000
@RF00497 TM    XVD1,XVD1PP                                              02431000
         BZ    @RF00504                 NO, BRANCH                      02432000
*                       WQEMCSN='0'B;/* YES, USER CAN'T BY PASS HC   */ 02433000
         NI    WQEMCSF2,255-WQEMCSN                                     02434000
*                     IF WQEMCSH='1'B&/* DID NOT AUTH USER SPECIFY      02435000
*                                      QREG0                         */ 02436000
*                         XVD1AUTH='0'B THEN                            02437000
@RF00504 TM    WQEMCSF1,WQEMCSH                                         02438000
         BZ    @RF00506                                                 02439000
         TM    XVD1,XVD1AUTH                                            02440000
         BO    @RF00506                                                 02441000
*                       DO;         /* YES. SWITCH USER TO R0           02442000
*                                      CONDITIONAL QUEUE TO THAT        02443000
*                                      CONSOLE                       */ 02444000
*                         WQEMCSH='0'B;/* TURN OFF QREG0 MCS FLAG    */ 02445000
*                         WQEMCSB='1'B;/* TURN ON R0 MCS FLAG        */ 02446000
         OI    WQEMCSF1,WQEMCSB                                         02447000
         NI    WQEMCSF1,255-WQEMCSH                                     02448000
*                       END;                                            02449000
*                     WQEASID=ASCBASID;/* MOVE IN ASID OF THIS MEMORY*/ 02450000
@RF00506 MVC   WQEASID,ASCBASID                                         02451000
*                     WQETCB=R4;    /* MOVE IN ADDR OF CURRENT TCB   */ 02452000
         ST    R4,WQETCB                                                02453000
*                     WQEJSTCB=TCBJSTCB;/* MOVE IN ADDR OF JOB STEPS    02454000
*                                      TCB                           */ 02455000
         MVC   WQEJSTCB,TCBJSTCB                                        02456000
*/********************************************************************/ 02457000
*/*                                                                  */ 02458000
*/* PUT IN USER AUTHORIZATION                                        */ 02459000
*/*                                                                  */ 02460000
*/********************************************************************/ 02461000
*                                                                       02462000
*                     IF XVD1AUTH='1'B THEN/* USER AUTHORIZED ?      */ 02463000
         TM    XVD1,XVD1AUTH                                            02464000
         BZ    @RF00514                                                 02465000
*                       WQEAUTH='1'B;/* YES, INDICATE SO             */ 02466000
         OI    WQEXA,WQEAUTH                                            02467000
         B     @RC00514                                                 02468000
*                                                                       02469000
*                     ELSE                                              02470000
*                       WQEAUTH='0'B;/* NO, MAKE SURE BIT IS OFF     */ 02471000
@RF00514 NI    WQEXA,255-WQEAUTH                                        02472000
*/********************************************************************/ 02473000
*/*                                                                  */ 02474000
*/*  NOW SET QUEUE FOR HC IF ALREADY INDICATED                       */ 02475000
*/*                                                                  */ 02476000
*/********************************************************************/ 02477000
*                     IF XVD2QFHC='1'B THEN/* IS QUEUE FOR HC           02478000
*                                      INDICATED                     */ 02479000
@RC00514 TM    XVD2,XVD2QFHC                                            02480000
         BZ    @RF00517                                                 02481000
*                       WQEQFHC='1'B;/* YES, SET SAVE BIT ON IN WQE  */ 02482000
         OI    WQEXA,WQEQFHC                                            02483000
         B     @RC00517                                                 02484000
*                                                                       02485000
*                     ELSE                                              02486000
*                       WQEQFHC='0'B;/* NO, INSURE BIT IS OFF        */ 02487000
@RF00517 NI    WQEXA,255-WQEQFHC                                        02488000
*/********************************************************************/ 02489000
*/*                                                                  */ 02490000
*/* CHECK IF TIME IS TO BE TAKEN                                     */ 02491000
*/*                                                                  */ 02492000
*/********************************************************************/ 02493000
*                     IF WPLMCSFI='1'B THEN/* DO NOT TIME STAMP ON ? */ 02494000
@RC00517 TM    WKAMCSF+1,WPLMCSFI                                       02495000
         BZ    @RF00520                                                 02496000
*                       WQETS='';   /* YES, FILL TIME STAMP WITH        02497000
*                                      BLANKS                        */ 02498000
         MVI   WQETS,C' '                                               02499000
         MVC   WQETS+1(L'WQETS-1),WQETS                                 02500000
         B     @RC00520                                                 02501000
*                                                                       02502000
*                     ELSE          /* NO, UNPACK THE TIME STAMP     */ 02503000
*                       DO;         /* FILL IN THE TIME FIELD        */ 02504000
@RF00520 MVC   WQEPAD(9),PATTIME       MOVE IN THE TIME EDIT PATTERN    02505000
         ED    WQEPAD(9),XVA8          FORMAT TIME HH.MM.SS INTO WQETS  02506000
*                       END;                                            02507000
*                     WQEPAD1=' ';  /* INSERT BLANK BETWEEN TS AND      02508000
*                                      JOBNAME                       */ 02509000
@RC00520 MVI   WQEPAD1,C' '                                             02510000
*                     WQEPAD2=' ';  /* INSERT BLANK AFTER THE JOBNAME*/ 02511000
         MVI   WQEPAD2,C' '                                             02512000
*                     WQEJOBNM=' '; /* BLANK OUT THE JOB NAME FIELD  */ 02513000
         MVI   WQEJOBNM,C' '                                            02514000
         MVC   WQEJOBNM+1(L'WQEJOBNM-1),WQEJOBNM                        02515000
*/********************************************************************/ 02516000
*/*                                                                  */ 02517000
*/* NOW DECODE THE ROUTE CODES                                       */ 02518000
*/*                                                                  */ 02519000
*/********************************************************************/ 02520000
*                     R3=WQEROUT; /* PICK UP UNDECODED ROUTE CODES */   02521000
         L     R3,WQEROUT                                               02522000
*                     DO R10=1 TO 4;/* CONVERT FOUR ROUTE               02523000
*                                      CHARACTERS                    */ 02524000
         LA    R8,1                                                     02525000
*                       R2=0;       /* PREPARE FOR SHIFT             */ 02526000
@DL00535 SLR   R2,R2                                                    02527000
*                                   /* MOVE IN FOUR BITS OF ROUTE       02528000
*                                      CODE                          */ 02529000
         SLDL  R2,4                                                     02530000
*                       R2=R2+1;/* MAKE INDEX TO EBCIDIC TABLE A        02531000
*                                      ONE ORIGIN INDEX              */ 02532000
         LA    R2,1(,R2)                                                02533000
*/********************************************************************/ 02534000
*/*                                                                  */ 02535000
*/* NOW USE THE FOUR BITS IN R2 TO PICK UP THE PROPER CHARACTER      */ 02536000
*/* FROM THE CHARACTER TABLE                                         */ 02537000
*/*                                                                  */ 02538000
*/********************************************************************/ 02539000
*                                                                       02540000
*                       WQERR(R10)=HEXCHAR(R2);                         02541000
         L     R1,XVWQEAD                                               02542000
         ALR   R1,R8                                                    02543000
         LA    R14,HEXCHAR-1(R2)                                        02544000
         MVC   WQERR-1-WQE(1,R1),0(R14)                                 02545000
*                     END;                                              02546000
         LA    R8,1(,R8)                                                02547000
         C     R8,KF4                                                   02548000
         BNH   @DL00535                                                 02549000
*                     WQEPAD=' ';   /* INSERT BLANK AFTER ROUTE      */ 02550000
         MVI   WQEPAD,C' '                                              02551000
*                     R3=CVTPTR;    /* RESET R3 TO POINT AT CVTMAP */   02552000
         L     R3,CVTPTR                                                02553000
*                     WQESUSP='1'B; /* SUSPEND OPERATION OF WQE FOR     02554000
*                                      SUBSYSTEM EXIT                */ 02555000
         OI    WQEXA,WQESUSP                                            02556000
*/********************************************************************/ 02557000
*/*                                                                  */ 02558000
*/*  THIS SECTION OF CODE TEST FOR BYPASS HARDCOPY REQUESTED BY      */ 02559000
*/*  THE ISSUER AND IF ON, SETS MASTER TRACED BIT ON IN ORDER TO     */ 02560000
*/*  PREVENT PROCESSING FOR MASTER TRACING BY IEAVMWSV               */ 02561000
*/*                                                                  */ 02562000
*/********************************************************************/ 02563000
*                     IF WQEMCSN='1'B THEN/* BYPASS HARDCOPY            02564000
*                                      REQUESTED                     */ 02565000
         TM    WQEMCSF2,WQEMCSN                                         02566000
         BZ    @RF00545                NO, BRANCH                       02567000
*                       WQEMTRCD='1'B;/* YES, SET MASTER TRACED BIT  */ 02568000
         OI    WQEAVAIL,WQEMTRCD                                        02569000
*/********************************************************************/ 02570000
*/*                                                                  */ 02571000
*/*  PUT WQE ON THE QUEUE. CHECK IF QUEUE IS EMPTY                   */ 02572000
*/*                                                                  */ 02573000
*/********************************************************************/ 02574000
*                     IF UCMWQEND=0 THEN/* IF QUEUE IS EMPTY SET UP     02575000
*                                      FOR INSERT                    */ 02576000
@RF00545 L     R8,CVTCUCB                                               02577000
         USING UCM,R8                                                   02578000
         ICM   R2,B'1111',UCMWQEND                                      02579000
         BNZ   @RF00547                                                 02580000
*                       UCMWQEND=ADDR(UCMWTOQ);/* PT AT HEAD OF CHAIN*/ 02581000
         LA    R2,UCMWTOQ                                               02582000
         ST    R2,UCMWQEND                                              02583000
*                     UCMWQEND->WQELKPA=XVWQEAD;/* PT LAST WQE AT NEW   02584000
*                                      WQE                           */ 02585000
@RF00547 L     R2,UCMWQEND                                              02586000
         STCM  R15,B'0111',WQELKPA-WQE(R2)                              02587000
*                     UCMWQEND=XVWQEAD;/* PT END OF CHAIN AT NEW WQE */ 02588000
         ST    R15,UCMWQEND                                             02589000
*                     WQELKP=0;     /* INDICATE LAST ON THE CHAIN    */ 02590000
         XC    WQELKP,WQELKP                                            02591000
*/*   CHECK IF WTO HAS BEEN MARKED FOR DELETION. IF SO MARK          */ 02592000
*/*   IT AS SERVICED AND SEND IT TO HARDCOPY                         */ 02593000
*                     IF XVD2DELW='1'B THEN/* IS WQE TO BE DELETED?  */ 02594000
         TM    XVD2,XVD2DELW                                            02595000
         BZ    @RF00552                NO, BRANCH                       02596000
*                       DO;         /* YES, DELETE THIS MESSAGE      */ 02597000
*                         WQEBUFE='1'B;/* MARK WQE AS SERVICED       */ 02598000
         OI    WQEAVAIL,WQEBUFE                                         02599000
*                         UCMSYSI='1'B;/* IND. CLEAN UP IS NEEDED    */ 02600000
         S     R8,KF4                                                   02601000
         L     R8,0(,R8)               R8 -> UCM PREFIX                 02602000
         DROP  R8                                                       02603000
         USING UCMPRFX,R8                                               02604000
         OI    UCMSFLG2,UCMSYSI                                         02605000
         AIF   (&TMVS805).U805G                                         02606000
*                         WQEQFHC='1'B;/* SEND MESSAGE TO HARDCOPY   */ 02607000
         OI    WQEXA,WQEQFHC                                            02608000
*                         WQEBUFC='1'B;/* IND. READY FOR HARDCOPY    */ 02609000
         OI    WQEAVAIL,WQEBUFC                                         02610000
         AGO   .U805H                                                   02611000
.U805G   NI    WQEXA,255-WQEQFHC       NO HARDCOPY                      02612000
         NOP   0                       WAS OI WQEAVAIL,WQEBUFC          02613000
.U805H   ANOP                                                           02614000
*                       END;        /* END IF DELETE IS SPECIFIED    */ 02615000
         DROP  R8,R15                                                   02616000
*/*** END OF BUILDWQE ************************************************/ 02617000
*/********************************************************************/ 02618000
*/*                                                                  */ 02619000
*/* FREE THE HELD LOCKS AND FRR PRIOR TO TAKING THE SUBSYSTEM EXIT   */ 02620000
*/*                                                                  */ 02621000
*/********************************************************************/ 02622000
*                     CALL FREELOCK;                                    02623000
@RF00552 BAL   R14,FREELOCK                                             02624000
*/********************************************************************/ 02625000
*/*                                                                  */ 02626000
*/* TAKE THE SUBSYSTEM EXIT                                          */ 02627000
*/*                                                                  */ 02628000
*/********************************************************************/ 02629000
*                     PARMFTPT=FTSSEXIT;/* START OF SUBSYSTEM EXIT      02630000
*                                      CODE                          */ 02631000
         MVI   PARMFTPT,X'08'                                           02632000
*                     PARMRTAD=ADDR(VWTOSSRT);/* IF ERROR TURN OFF      02633000
*                                      DELETE BIT                    */ 02634000
         LA    R2,VWTOSSRT                                              02635000
         ST    R2,PARMRTAD                                              02636000
*/********************************************************************/ 02637000
*/*                                                                  */ 02638000
*/*   HASPEXIT - EXIT TO THE JOB ENTRY SUBSYSTEM                     */ 02639000
*/*                                                                  */ 02640000
*/*  THE SPACE FOR THE SSOB AND SSOBWT IS OBTAINED FROM SUBPOOL 231  */ 02641000
*/*  WITH THE AUTOMATIC VARIABLES.                                   */ 02642000
*/*                                                                  */ 02643000
*/********************************************************************/ 02644000
*                                                                       02645000
*/********************************************************************/ 02646000
*/*                                                                  */ 02647000
*/*   SET UP THE SSOB                                                */ 02648000
*/*                                                                  */ 02649000
*/********************************************************************/ 02650000
*                                                                       02651000
*                     SSOBID='SSOB';/* FILL IN THE ID FIELD          */ 02652000
         MVC   SSOBID(L'KCSSOB),KCSSOB                                  02653000
*                     SSOBLEN=LENGTH(SSOB);/* AND THE LENGTH FIELD   */ 02654000
         MVC   SSOBLEN(2),KH20                                          02655000
*                     SSOBFUNC=SSOBWTO;/* SPECIFY WTO EXIT TO THE       02656000
*                                      SUBSYSTEM                     */ 02657000
         MVC   SSOBFUNC(2),KH9                                          02658000
*                     SSOBSSIB=0;   /* INDICATE SUBSYSTEM WHICH         02659000
*                                      INITIATED THE USER            */ 02660000
         SLR   R14,R14                                                  02661000
         ST    R14,SSOBSSIB                                             02662000
*                     SSOBINDV=ADDR(SSWT);/* POINT AT WTO INTERFACE     02663000
*                                      BLOCK                         */ 02664000
         LA    R14,SSWT                                                 02665000
         ST    R14,SSOBINDV                                             02666000
*/********************************************************************/ 02667000
*/*                                                                  */ 02668000
*/*   NOW FILL IN THE SSWT BLOCK                                     */ 02669000
*/*                                                                  */ 02670000
*/********************************************************************/ 02671000
*                     SSWTLEN=LENGTH(SSWT);/* PUT LENGTH OF SSWT INTO   02672000
*                                      THE SSWT                      */ 02673000
         MVC   SSWTLEN,KH16                                             02674000
*                     SSWTWQE=XVWQEAD;/* PUT IN THE ADDR OF THE WQE  */ 02675000
         MVC   SSWTWQE,XVWQEAD        /* INTO THE SSOB               */ 02676000
*/********************************************************************/ 02677000
*/*                                                                  */ 02678000
*/*   CHECK FOR A WTOR. IF SO THEN FILL IN ADDR OF THE ORE. IF NOT   */ 02679000
*/*   THEN SET THE SSWTORE FIELD TO ZERO.                            */ 02680000
*/*                                                                  */ 02681000
*/********************************************************************/ 02682000
*                     IF XVD2WTOR='1'B THEN/* WTOR REQUEST ?         */ 02683000
         TM    XVD2,XVD2WTOR                                            02684000
         BZ    @RF00570                    NO, BRANCH                   02685000
*                       SSWTORE=XVOREAD;/* YES, PUT IN THE ADDR OF      02686000
*                                      THE ORE                       */ 02687000
         MVC   SSWTORE,XVOREAD                                          02688000
         B     @RC00570                                                 02689000
*                                                                       02690000
*                     ELSE                                              02691000
*                       SSWTORE=0;  /* NO, SET FIELD TO ZERO         */ 02692000
@RF00570 XC    SSWTORE,SSWTORE                                          02693000
*                     SSWTMIN=0;    /* THERE IS NO MINOR FOR A SINGLE   02694000
*                                      WTO                           */ 02695000
@RC00570 XC    SSWTMIN,SSWTMIN                                          02696000
*/*                                                                  */ 02697000
*/*      IEFSSREQ  (SSOB);                                           */ 02698000
*/*                                                                  */ 02699000
*/*      PASS CONTROL TO JOB ENTRY SUBSYSTEM TO                      */ 02700000
*/*      PROCESS REQUEST -                                           */ 02701000
*/*                                                                  */ 02702000
*/*      INPUT - R1 -> ONE WORD PARAMETER LIST WHICH                 */ 02703000
*/*                    POINTS TO THE SSOB                            */ 02704000
*/*      CALL SSREQ(SSOB);              /* CALL ON THE SUBSYSTEM     */ 02705000
*/*                                                                  */ 02706000
         LA    R14,SSOB                                                 02707000
         ST    R14,PARMPTR                                              02708000
         MVI   PARMPTR,X'80'           SET END OF PARAMETER LIST        02709000
         L     R8,CVTJESCT                                              02710000
         USING JESCT,R8                                                 02711000
         L     R15,JESSSREQ            R15 -> IEFSSREQ RTN              02712000
         DROP  R8                                                       02713000
         LA    R1,PARMPTR                                               02714000
         BALR  R14,R15                 CALL IEFSSERQ                    02715000
         L     R8,XVWQEAD                                               02716000
         USING WQE,R8                                                   02717000
         TM    WQEMCSF2,WQEMCSN        BYPASS QUEUE TO HARD COPY ?      02718000
         BZ    IEA00D94                NO, BRANCH                       02719000
         NI    WQEXA,255-WQEQFHC       YES,TURN OFF QUEUE FOR HARD COPY 02720000
*/********************************************************************/ 02721000
*/*                                                                  */ 02722000
*/*   CHECK FOR A REQUEST TO DELETE MESSAGE BY SUBSYSTEM. CHECK FOR  */ 02723000
*/*   GOOD EXIT AND DELETION REQUEST                                 */ 02724000
*/*                                                                  */ 02725000
*/********************************************************************/ 02726000
*                     IF R15=SSRTOK&/* WAS EXIT SUCCESSFUL           */ 02727000
*                         SSOBRETN=SSWTNDSP/* AND IS DON'T DISPLAY      02728000
*                                      REQUESTED                     */ 02729000
*                       THEN        /* YES                           */ 02730000
IEA00D94 LTR   R15,R15                                                  02731000
         BNZ   @RF00575                                                 02732000
         LA    R14,4                                                    02733000
         C     R14,SSOBRETN                                             02734000
         BNE   @RF00575                                                 02735000
*                       DO;         /* DELETE MESSAGE                */ 02736000
*                         WQEBUFE='1'B;/* MARK WQE AS SERVICED       */ 02737000
         OI    WQEAVAIL,WQEBUFE                                         02738000
*                         UCMSYSI='1'B;/* IND. CLEAN UP IS NEEDED    */ 02739000
         L     R14,CVTCUCB                                              02740000
         S     R14,KF4                                                  02741000
         L     R14,0(,R14)             R14 -> UCM PREFIX                02742000
         USING UCMPRFX,R14                                              02743000
         OI    UCMSFLG2,UCMSYSI                                         02744000
         DROP  R14                                                      02745000
*                         /*******************************************/ 02746000
*                         /*                                         */ 02747000
*                         /* CHECK IF HARDCOPY IS TO BE BYPASSED     */ 02748000
*                         /*                                         */ 02749000
*                         /*******************************************/ 02750000
*                         IF WQEMCSN='0'B THEN/* BYPASS HARDCOPY SET?*/ 02751000
         TM    WQEMCSF2,WQEMCSN                                         02752000
         BO    @RF00579                                                 02753000
*                           DO;     /* NO,SEND MESSAGE TO HARDCOPY   */ 02754000
*                             WQEBUFC='1'B;/* IND. READY FOR HARDCOPY*/ 02755000
         OI    WQEAVAIL,WQEBUFC                                         02756000
*                             WQEQFHC='1'B;/* SEND MESSAGE TO           02757000
*                                      HARDCOPY                      */ 02758000
         OI    WQEXA,WQEQFHC                                            02759000
         B     @RF00575                                                 02760000
*                                                                       02761000
*                           END;    /* END IF BYPASS NOT SET         */ 02762000
*                         ELSE      /* IF BYPASS IS SET              */ 02763000
*                           DO;     /* DO NOT HARDCOPY MESSAGE       */ 02764000
*                             IF XVD2WTOR='1'B THEN/* THIS A WTOR?   */ 02765000
@RF00579 TM    XVD2,XVD2WTOR           NO, BRANCH                       02766000
         BZ    @RF00585                                                 02767000
*                               WQEBUFC='1'B;/* YES, SET READY FOR      02768000
*                                      HDCPY                         */ 02769000
         OI    WQEAVAIL,WQEBUFC                                         02770000
*                             WQEQFHC='0'B;/* TURN OFF HARDCOPY FLAG */ 02771000
@RF00585 NI    WQEXA,255-WQEQFHC                                        02772000
*                           END;    /* END IF BYPASS IS SET          */ 02773000
*                       END;        /* END IF DON'T DISPLAY SPEC     */ 02774000
*/********************************************************************/ 02775000
*/*                                                                  */ 02776000
*/* REMOVE ALL UNPRINTABLE AND NONDISPLAY CHARACTERS IN WQE          */ 02777000
*/*                                                                  */ 02778000
*/********************************************************************/ 02779000
*                                   /* TRANSLATE TEXT TO PRINTABLE   */ 02780000
*                                   /* AND DISPLAYABLE ONLY          */ 02781000
@RF00575 L     R1,WQENBR               R1 = L'WQETXT                    02782000
         BCTR  R1,0                                                     02783000
         EX    R1,TRINST               TR WQETXT(0),TRTAB               02784000
*                       END;                                            02785000
*                     /***********************************************/ 02786000
*                     /*                                             */ 02787000
*                     /* WE ARE DONE WITH THE SUBSYSTEM EXIT. REGAIN */ 02788000
*                     /* THE LOCKS AND TAKE THE BLOCKS OUT OF THE    */ 02789000
*                     /* SUSPENDED STATE                             */ 02790000
*                     /*                                             */ 02791000
*                     /***********************************************/ 02792000
*VWTOSSLB:                                                              02793000
*                     CALL SETLOCK;                                     02794000
VWTOSSLB BAL   R14,SETLOCK                                              02795000
*/********************************************************************/ 02796000
*/*                                                                  */ 02797000
*/* TAKE THE WQE AND ORE OUT OF THE SUSPENDED STATE                  */ 02798000
*/*                                                                  */ 02799000
*/********************************************************************/ 02800000
*                     PARMFTPT=FTEND;/* END OF THE MODULE            */ 02801000
         MVI   PARMFTPT,X'09'                                           02802000
*                     PARMRTAD=0;   /* NO RETRY NEEDED               */ 02803000
         XC    PARMRTAD,PARMRTAD                                        02804000
*                                                                       02805000
*                     WQESUSP='0'B; /* TURN OFF SUSPENDED BIT        */ 02806000
         NI    WQEXA,255-WQESUSP                                        02807000
         DROP  R8                                                       02808000
*                     IF XVD2WTOR='1'B THEN/* IS THERE AN ORE?       */ 02809000
         TM    XVD2,XVD2WTOR                                            02810000
         BZ    @RF00605                 NO, BRANCH                      02811000
*                       ORESUSP='0'B;/* YES, TURN OFF THE SUSPENDED     02812000
*                                      BIT                           */ 02813000
         L     R8,XVOREAD                                               02814000
         USING OREF,R8                                                  02815000
         NI    OREXA,255-ORESUSP                                        02816000
         DROP  R8                                                       02817000
*                     CALL FREELOCK;/* FREE ALL LOCKS                */ 02818000
@RF00605 BAL   R14,FREELOCK                                             02819000
*/********************************************************************/ 02820000
*/*                                                                  */ 02821000
*/*     POST THE COMMUNICATIONS TASK TO INDICATE THAT IT HAS WORK    */ 02822000
*/*     TO DO. NAMELY A WQE, AND POSSIBLY AN ORE, TO PROCESS         */ 02823000
*/*                                                                  */ 02824000
*/********************************************************************/ 02825000
*                     CALL POSTOECB;/* XMPOST UCMOECB                */ 02826000
         BAL   R14,POSTOECB                                             02827000
         B     VWTOERRT                                                 02828000
*                                                                       02829000
*                   END;            /* OF TEST FOR VALID CONTROL        02830000
*                                      BLOCKS                        */ 02831000
*                 ELSE              /* NO THE CONTROL BLOCKS WERE NOT   02832000
*                                      OBTAINED. FREE THE LOCKS.     */ 02833000
*                   CALL FREELOCK;                                      02834000
@RF00418 BAL   R14,FREELOCK                                             02835000
*               END;                /* OF RETURN TO USER FROM WTP       02836000
*                                      TEST                          */ 02837000
*           END;                    /* END OF ELSE CLAUSE OF ZERO MSG   02838000
*                                      LENGTH TEST                   */ 02839000
*     END;                          /* END OF VALID PARM LIST ELSE      02840000
*                                      CLAUSE                        */ 02841000
*/********************************************************************/ 02842000
*/*                                                                  */ 02843000
*/*      EXIT FROM IEAVVWTO                                          */ 02844000
*/*                                                                  */ 02845000
*/********************************************************************/ 02846000
*                                                                       02847000
*        ON ENTRY TO VWTOERRT NO LOCKS MUST BE HELD AS                  02848000
*        THE FREEMAIN SVC IS ISSUED TO FREE UP STORAGE                  02849000
*                                                                       02850000
*VWTOERRT:                                                              02851000
VWTOERRT ICM   R1,B'1111',LONGTXT      STORAGE GETMAINED FOR LONG WPL ? 02852000
         BZ    VWTOERRA                NO, BRANCH AROUND FREEMAIN       02853000
         L     R0,LONGLEN                                               02854000
*                                                                       02855000
         FREEMAIN RU,LV=(0),A=(1),SP=229                                02856000
*                                                                       02857000
VWTOERRA L     R0,@SIZDATD             GET SIZE OF DYNAMIC AREA         02858000
         LR    R1,R9                   R1 -> DYNAMIC AREA               02859000
*                                                                       02860000
         FREEMAIN R,LV=(0),A=(1)       FREE THE AREA                    02861000
*                                                                       02862000
*/********************************************************************/ 02863000
*/*                                                                  */ 02864000
*/*     FREE THE ESTAE SO THAT ABENDS WILL BE SEEN BY THE USER       */ 02865000
*/*                                                                  */ 02866000
*/********************************************************************/ 02867000
*                                                                       02868000
         ESTAE 0                                                        02869000
*                                                                       02870000
*/********************************************************************/ 02871000
*/*                                                                  */ 02872000
*/* CHECK FOR ABEND ERROR SITUATION                                  */ 02873000
*/*                                                                  */ 02874000
*/********************************************************************/ 02875000
*   IF XVD1PERR='1'B THEN           /* WAS ERROR BIT TURNED ON FOR      02876000
*                                      ABEND                         */ 02877000
         TM    XVD1,XVD1PERR                                            02878000
         BZ    @RF00618                NO, BRANCH                       02879000
*     /***************************************************************/ 02880000
*     /*                                                             */ 02881000
*     /* YES, ABEND THE USER WITH CODE D23 WITH REASON CODE IN R15   */ 02882000
*     /* THAT IDENTIFIES THE REASON FOR THE ABEND                    */ 02883000
*     /*                                                             */ 02884000
*     /***************************************************************/ 02885000
*     DO;                                                               02886000
*       R1=ABNDCODE;                                                    02887000
         L     R15,XVA4                LOAD REASON CODE INTO R15        02888000
         L     R1,ABNDCODE          /* LOAD R1 WITH D23 CODE         */ 02889000
*       /*************************************************************/ 02890000
*       /*                                                           */ 02891000
*       /* ISSUE SYSTEM ABEND WITH DUMP                              */ 02892000
*       /*                                                           */ 02893000
*       /*************************************************************/ 02894000
*                                                                       02895000
         ABEND (1),DUMP,,SYSTEM                                         02896000
*                                                                       02897000
*     END;                                                              02898000
*   ELSE                            /* NO, CHECK FOR VALID MESSAGE ID*/ 02899000
*     IF XVD2ZERO='1'B THEN         /* WAS MESSAGE NOT PROCESSED        02900000
*                                      BECAUSE OF ZERO LENGTH           02901000
*                                      MESSAGE?                      */ 02902000
@RF00618 TM    XVD2,XVD2ZERO                                            02903000
         BZ    @RC00618                                                 02904000
*       XVWQEID=0;                  /* YES, PASS BACK A ZERO MSG ID  */ 02905000
         XC    XVWQEID,XVWQEID                                          02906000
*/********************************************************************/ 02907000
*/*                                                                  */ 02908000
*/*      EPILOG CODE                                                 */ 02909000
*/*                                                                  */ 02910000
*/****************************************************************** */ 02911000
*                                                                       02912000
@RC00618 SLR   R15,R15                 CLEAR REGISTER FOR IC            02913000
         IC    R15,XVRETCOD            PICK UP RETURN CODE              02914000
         L     R1,XVWQEID              GET MSG ID TO RETURN             02915000
*   R14=XVRET;                      /* GET RETURN ADDRESS OF CALLER  */ 02916000
         L     R14,XVRET                                                02917000
         BR    R14                     EXIT FROM IEEVVWTO TO SVC RETURN 02918000
*                                                                       02919000
         DROP  R10                                                      02920000
*                                                                       02921000
*/********************************************************************/ 02922000
*/*                                                                  */ 02923000
*/*   VWTOLREC - LOG RECOVERY ROUTINE                                */ 02924000
*/*                                                                  */ 02925000
*/*   FUNCTION - THIS ROUTINE IS ENTERED WHEN WE CAN NOT GET A CELL  */ 02926000
*/*   POOL EXTENSION FROM CSA, SUBPOOL 231. CHECK TO SEE IF THE      */ 02927000
*/*   REASON FOR THE GETMAIN FAILURE IS DUE TO THE SYSTEM LOG NOT    */ 02928000
*/*   BEING INITIALIZED YET AND CSA HAS BEEN FILLED WITH MESSAGES    */ 02929000
*/*   WAITING TO GO OUT TO THE LOG.  IF THAT IS THE CASE THEN WE     */ 02930000
*/*   RECOVER FROM THE SITUATION BY SWITCHING HARDCOPY TO A CONSOLE  */ 02931000
*/*   AND COMMTASK WILL FLUSH THE MESSAGES. WE WAIT UNTIL THE WQE    */ 02932000
*/*   LIMIT IS DOWN AND THEN PROCEED.  IF THE CAUSE ISN'T DUE TO     */ 02933000
*/*   LOG PROBLEMS THEN WE CAUSE THE CALLER TO BE ABENDED.           */ 02934000
*/*                                                                  */ 02935000
*/*   INPUT - THE INPUT PARM IS THE PTR IN XVSAV THAT WILL HOLD THE  */ 02936000
*/*            CONTROL BLOCKS ADDR. EITHER XVOREAD OR XVWQEAD. THIS  */ 02937000
*/*            FIELD IS ZERO ON INPUT.                               */ 02938000
*/*            UCMWQLM AND UCMRQLM ARE THE LIMITS ON THE NUMBER OF   */ 02939000
*/*            WQES AND ORES THAT WERE SET BY SYSGEN                 */ 02940000
*/*            UCMWQLM1 AND UCMRQLM1 WERE THE LIMITS SET AT IPL      */ 02941000
*/*            UCMFMSGN IS A FLAG WHICH WHEN ON TELLS                */ 02942000
*/*            IEAVMQWR NOT TO ISSUE ANY WQE THRESHOLD MSGS.         */ 02943000
*/*                                                                  */ 02944000
*/*   OUTPUT - BLKADDR IS RETURNED AS ZERO OR ONE. ZERO MEANS THAT   */ 02945000
*/*            THE ERROR WAS RECOVERED, TRY TO GET ANOTHER CELL.     */ 02946000
*/*            ONE MEANS THAT THE ERROR COULD NOT BE RECOVERED,      */ 02947000
*/*            SO ABEND THE CALLER.                                  */ 02948000
*/*            FLAG UCMFMSGN WILL BE ON                              */ 02949000
*/*                                                                  */ 02950000
*/********************************************************************/ 02951000
*VWTOLREC:                                                              02952000
VWTOLREC STM   R14,R12,@SA00002                                         02953000
         MVC   @PC00002(4),0(R1)                                        02954000
*                                      VWTOGETB WHEN PROBLEM OCCURED */ 02955000
*   /*****************************************************************/ 02956000
*   /*                                                               */ 02957000
*   /* SET FLAG TO INFORM IEAVMQWR NOT TO ISSUE ANY MESSAGES ABOUT   */ 02958000
*   /* WQE USE                                                       */ 02959000
*   /*                                                               */ 02960000
*   /*****************************************************************/ 02961000
*   UCMFMSGN='1'B;                  /* FLAG IS IN UCM FIXED EXTENSION*/ 02962000
         L     R10,CVTCUCB                                              02963000
         USING UCM,R10                                                  02964000
         L     R8,UCMBFEXT             R8 -> UCM FIXED EXTENSION BASE   02965000
         USING UCMFEXTA,R8                                              02966000
         OI    UCMFFLG1,UCMFMSGN       NO WQE THRESHOLD MSG             02967000
         DROP  R8                                                       02968000
*   /*****************************************************************/ 02969000
*   /*                                                               */ 02970000
*   /* CHECK IF ERROR DUE TO HC GOING TO UNINITIALIZED SYSLOG        */ 02971000
*   /*                                                               */ 02972000
*   /*****************************************************************/ 02973000
*   IF UCMSYSG='1'B &               /* SYSLOG THE HARDCOPY DEVICE    */ 02974000
*       BALOGINT='0'B THEN          /* AND LOG NOT INITIALIZED YET ? */ 02975000
         LR    R8,R10                                                   02976000
         S     R8,KF4                                                   02977000
         L     R8,0(,R8)               R8 -> UCM PREFIX                 02978000
         USING UCMPRFX,R8                                               02979000
         TM    UCMSFLG1,UCMSYSG        HARD COPY DEVICE IS SYSLOG ?     02980000
         BZ    @RF00634                NO, BRANCH                       02981000
         L     R2,CVTMSER                                               02982000
         USING BASE,R2                 MASTER SCHED RESIDENT DATA AREA  02983000
         TM    BALOG,BALOGINT          LOG AREA INITIALIZED ?           02984000
         BNZ   @RF00634                                                 02985000
         DROP  R2                                                       02986000
*     DO;                                                               02987000
*       /*************************************************************/ 02988000
*       /*                                                           */ 02989000
*       /* SWITCH THE LIMITS FOR NUMBER OF ORES AND WQES ALLOWED IN  */ 02990000
*       /* THE SYSTEM AT ONE TIME                                    */ 02991000
*       /*                                                           */ 02992000
*       /*************************************************************/ 02993000
*       UCMWQLM=UCMWQLM1;           /* SET SYSGENED LIMIT TO IPL        02994000
*                                      LIMIT                         */ 02995000
         LH    R2,UCMWQLM1                                              02996000
         STH   R2,UCMWQLM                                               02997000
*       UCMRQLM=UCMRQLM1;           /* FOR WQES AND ORES             */ 02998000
         IC    R2,UCMRQLM1                                              02999000
         STC   R2,UCMRQLM                                               03000000
*       /*************************************************************/ 03001000
*       /*                                                           */ 03002000
*       /* POST COMMTASK TO SWITCH HARDCOPY                          */ 03003000
*       /*                                                           */ 03004000
*       /*************************************************************/ 03005000
*       UCMSYSO='1'B;               /* THIS IS A DUMMY ATTN INTERUPT    03006000
*                                      BY SVC 35                     */ 03007000
         OI    UCMSFLG2,UCMSYSO                                         03008000
*       CALL FREELOCK;              /* FREE THE FRR AND LOCKS        */ 03009000
         BAL   R14,FREELOCK                                             03010000
*       REG1SAV=ADDR(UCMAECB);      /* POST ATTN ECB                 */ 03011000
         L     R10,CVTCUCB                                              03012000
         LA    R8,UCMAECB                                               03013000
         ST    R8,REG1SAV                                               03014000
*       XVA4=UCMASCB;               /* COMMTASK'S ASCB ADDR          */ 03015000
         L     R8,UCMASCB                                               03016000
         ST    R8,XVA4                                                  03017000
*       XVA8=POSTRECY;              /* ADDR OF IEAVMEST              */ 03018000
         L     R10,UCMWAKUP                                             03019000
         DROP  R10                                                      03020000
         L     R10,0(,R10)                                              03021000
         ST    R10,XVA8                                                 03022000
*       R1=ADDR(REG1SAV);           /* LOAD PTR TO 3 WORD PARM LIST  */ 03023000
         LA    R1,REG1SAV                                               03024000
*                                   /* POST WITH CODE OF 35 FOR WTO  */ 03025000
         POST  ,35,MF=(E,(1))                                           03026000
*                                                                       03027000
*       CALL SETLOCK;               /* REGAIN THE LOCKS              */ 03028000
         BAL   R14,SETLOCK                                              03029000
*       IF XVD1PRIV='0'B THEN       /* MSG WASN'T ISSUED BY THE         03030000
*                                      COMMTASK OR AN SIRB. THIS        03031000
*                                      CHECK IS SO WE DON'T CAUSE AN    03032000
*                                      SIRB OR COMMTASK TO WAIT         03033000
*                                      INSTEAD THEY ARE ABENDED WITH    03034000
*                                      A D23 ABEND CODE              */ 03035000
         TM    XVD1,XVD1PRIV                                            03036000
         BNZ   @RF00648                                                 03037000
*         CALL VWTOWAIT(UCMWECBT);  /* WAIT FOR THE WQE LIMIT TO BE     03038000
*                                      REDUCED BELOW THE NEW LIMITS  */ 03039000
         L     R10,CVTCUCB                                              03040000
         USING UCM,R10                                                  03041000
         LA    R10,UCMWECBT                                             03042000
         DROP  R10                                                      03043000
         ST    R10,PARMPTR                                              03044000
         LA    R1,PARMPTR                                               03045000
         BAL   R14,VWTOWAIT                                             03046000
         B     @ER00002                                                 03047000
*                                                                       03048000
*       ELSE                        /* MSG ISSUER WAS COMMTASK OR       03049000
*                                      SIRB                          */ 03050000
*         BLKADDR=1;                /* SET PARAMETER ADDR TO ERROR      03051000
*                                      COND SO ISSUER WILL BE ABENDED*/ 03052000
@RF00648 L     R10,@PC00002                                             03053000
         MVC   0(4,R10),KF1                                             03054000
         B     @ER00002                                                 03055000
*                                                                       03056000
*     END;                                                              03057000
*   ELSE                            /* ERROR NOT DUE TO SYSLOG. SET     03058000
*                                      ERROR RETURN INDICATOR.       */ 03059000
*     BLKADDR=1;                    /* SET PARAMETER ADDR TO ERROR      03060000
*                                      COND                          */ 03061000
@RF00634 L     R10,@PC00002                                             03062000
         MVC   0(4,R10),KF1                                             03063000
*   RETURN;                         /* GO BACK TO CALLER             */ 03064000
@ER00002 LM    R14,R12,@SA00002                                         03065000
         BR    R14                                                      03066000
*   END;                                                                03067000
         B     @ER00002                                                 03068000
*                                                                       03069000
*/********************************************************************/ 03070000
*/*                                                                  */ 03071000
*/*  VWTOCLNP - VWTO'S CLEAN UP ROUTINE                              */ 03072000
*/*                                                                  */ 03073000
*/*   FUNCTION - THIS ROUTINE WILL BE CALLED IF THERE IS AN ERROR    */ 03074000
*/*   IN VWTO WHILE IT WAS LOCKED AND BUILDING THE ORE OR WQE. THIS  */ 03075000
*/*   ROUTINE WILL ATTEMP TO FREE THE ORE AND WQE. IT WILL THEN      */ 03076000
*/*   RETURN TO IEAVMFRR WHICH WILL LET THE ERROR CONTINUE.          */ 03077000
*/*                                                                  */ 03078000
*/*  INPUT - R2 -> THE USERS FRR PARMLIST                            */ 03079000
*/*          PARMRGAD WILL POINT TO THIS MODULES REG FRR SAVEAREA    */ 03080000
*/*          R13 -> IEAVMFRR'S SAVEAREA                              */ 03081000
*/*          R14 AND R15 WILL BE THE RETURN AND ENTRY REGS           */ 03082000
*/*          XVSAV WILL CONTAIN INFORMATION ON LOCATION OF ORE/WQE   */ 03083000
*/*                                                                  */ 03084000
*/*    OUTPUT - IEAVMFRR'S REGS WILL BE RESTORED                     */ 03085000
*/*             THE ORE/WQE, IF ANY WILL BE FREED                    */ 03086000
*/*                                                                  */ 03087000
*/********************************************************************/ 03088000
*VWTOCLNP:                                                              03089000
         DROP  R11,R12                                                  03090000
*                           /* SET UP THIS ENTRY POINT AND ALL REGS */  03091000
         USING *,R15           TEMPORARY BASE REGISTER                  03092000
VWTOCLNP SAVE  (14,12),,'IEAVVWTO CLEANUP'                              03093000
         USING PARMLIST,R2                                              03094000
         L     R1,PARMRGAD             PICK UP ADDR OF OUR SAVE AREA    03095000
         DROP  R2                                                       03096000
         LM    R3,R12,14(R1)           RESTORE OUR REGISTERS            03097000
         DROP  R15                     MODULE BASE REGS NOW AVAILABLE   03098000
         USING IEAVVWTO,R11,R12        RESTORE ADDRESSABILITY           03099000
*   /*****************************************************************/ 03100000
*   /*                                                               */ 03101000
*   /* NOW SAVE THE CALLER'S SAVE AREA ADDRESS                       */ 03102000
*   /*                                                               */ 03103000
*   /*****************************************************************/ 03104000
*   XVA8=R13;                       /* USE THE XVSAV WORK AREA       */ 03105000
         ST    R13,XVA8                OVERLAY SAVED TIME               03106000
*   R13=R9;                         /* LOAD OUR OWN SAVEAREA ADDRESS */ 03107000
         LR    R13,R9                                                   03108000
*   /*****************************************************************/ 03109000
*   /*                                                               */ 03110000
*   /* NOW CHECK IF ORE WAS OBTAINED. IF SO THEN CHECK IF IS WAS ON  */ 03111000
*   /* THE ORE QUEUE                                                 */ 03112000
*   /*                                                               */ 03113000
*   /*****************************************************************/ 03114000
*   IF XVD1ALDN='1'B&               /* WERE WE THRU GETTING BLOCKS   */ 03115000
*       XVOREAD>1 THEN              /* AND VALID ADDRESS IN THE         03116000
*                                      OREADDR FLD                   */ 03117000
         TM    XVD1,XVD1ALDN                                            03118000
         BZ    @RF00659                                                 03119000
         CLC   XVOREAD,KF1                                              03120000
         BNH   @RF00659                                                 03121000
*     DO;                           /* YES. CHECK IF ORE WAS ON THE     03122000
*                                      CHAIN                         */ 03123000
*       DO R1=ADDR(UCMRPYQ) BY 0 WHILE(R1^=0);                          03124000
         L     R1,CVTCUCB                                               03125000
         USING UCM,R1                                                   03126000
         LA    R1,UCMRPYQ                                               03127000
         DROP  R1                                                       03128000
         B     @DE00661                                                 03129000
*                                                                       03130000
*         IF R1->ORELKP=XVOREAD THEN/* DOES THIS LINK POINTER POINT     03131000
*                                      TO OUR ORE                    */ 03132000
@DL00661 L     R10,XVOREAD                                              03133000
         C     R10,ORELKP-OREF(,R1)                                     03134000
         BNE   @RF00662                                                 03135000
*           DO;                     /* YES. DECHAIN OUR ORE          */ 03136000
*             R1->ORELKP=XVOREAD->ORELKP;/* PUT PTR TO NEXT ORE IN      03137000
*                                      PREVIOUS ORE LINK FIELD       */ 03138000
         L     R10,ORELKP-OREF(,R10)                                    03139000
         ST    R10,ORELKP-OREF(,R1)                                     03140000
*             R1=0;                 /* GET OUT OF LOOP               */ 03141000
         SLR   R1,R1                                                    03142000
         B     @DE00661                                                 03143000
*                                                                       03144000
*           END;                                                        03145000
*         ELSE                      /* NO, NOT OURS. PT TO NEXT ORE  */ 03146000
*           R1=R1->ORELKP;                                              03147000
@RF00662 L     R1,ORELKP-OREF(,R1)                                      03148000
*       END;                        /* THE ORE IS NOW OFF THE ORE       03149000
*                                      CHAIN                         */ 03150000
@DE00661 LTR   R1,R1                                                    03151000
         BNZ   @DL00661                                                 03152000
*       /*************************************************************/ 03153000
*       /*                                                           */ 03154000
*       /* FREE THE ORE                                              */ 03155000
*       /*                                                           */ 03156000
*       /*************************************************************/ 03157000
*       R0=UCMORECP;                /* CELL POOL ID FOR IRES         */ 03158000
         L     R10,CVTCUCB                                              03159000
         USING UCM,R10                                                  03160000
         L     R0,UCMORECP                                              03161000
         DROP  R10                                                      03162000
*       R1=XVOREAD;                 /* ADDR OF THE ORE               */ 03163000
         L     R1,XVOREAD                                               03164000
*                                                  /* FREE ORE CELL  */ 03165000
         FREECELL CPID=(0),CELL=(1),BRANCH=YES                          03166000
*                                                                       03167000
*/********************************************************************/ 03168000
*/*                                                                  */ 03169000
*/* THIS ORE CELL MAY HAVE BEEN THE LAST ONE IN AN EXTENSION.        */ 03170000
*/* IF SO THEN R15 IS SET TO 20 BY FREECELL. IF THIS CONDITION       */ 03171000
*/* IS FOUND THEN FREE THE EXTENSION. R0 AND R1 ARE                  */ 03172000
*/* ARE SET BY FREECELL.                                             */ 03173000
*/*                                                                  */ 03174000
*/********************************************************************/ 03175000
*       IF R15=20 THEN              /* WAS CELL THE LAST IN AN          03176000
*                                      EXTENSION                     */ 03177000
         C     R15,KF20                                                 03178000
         BNE   @RF00659                                                 03179000
*         /***********************************************************/ 03180000
*         /*                                                         */ 03181000
*         /* YES, FREE THE EXTENSION                                 */ 03182000
*         /*                                                         */ 03183000
*         /***********************************************************/ 03184000
*                                                                       03185000
         FREEMAIN R,LV=(0),A=(1),BRANCH=YES                             03186000
*                                                                       03187000
*     END;                                                              03188000
*   /*****************************************************************/ 03189000
*   /*                                                               */ 03190000
*   /* NOW CHECK IF WQE WAS OBTAINED                                 */ 03191000
*   /*                                                               */ 03192000
*   /*****************************************************************/ 03193000
*   IF XVD1ALDN='1'B&               /* WERE WE THRU GETTING BLOCKS   */ 03194000
*       XVWQEAD>1 THEN              /* AND VALID ADDRESS IN THE         03195000
*                                      WQEADDR FLD                   */ 03196000
@RF00659 TM    XVD1,XVD1ALDN                                            03197000
         BZ    @RF00677                                                 03198000
         CLC   XVWQEAD,KF1                                              03199000
         BNH   @RF00677                                                 03200000
*     DO;                           /* YES. CHECK IF WQE WAS ON THE     03201000
*                                      CHAIN                         */ 03202000
*       DO R1=ADDR(UCMWTOQ) BY 0 WHILE(R1^=0);                          03203000
         L     R1,CVTCUCB                                               03204000
         USING UCM,R1                                                   03205000
         LA    R1,UCMWTOQ                                               03206000
         DROP  R1                                                       03207000
         B     @DE00679                                                 03208000
*                                                                       03209000
*         IF R1->WQELKPA=XVWQEAD THEN/* DOES THIS LINK POINTER          03210000
*                                      POINT TO OUR WQE              */ 03211000
@DL00679 L     R10,XVWQEAD                                              03212000
         L     R8,WQELKPA-1-WQE(,R1)                                    03213000
         LA    R8,0(,R8)                                                03214000
         CR    R10,R8                                                   03215000
         BNE   @RF00680                                                 03216000
*           DO;                     /* YES. DECHAIN OUR WQE          */ 03217000
*             R1->WQELKPA=XVWQEAD->WQELKPA;/* PUT PTR TO NEXT WQE       03218000
*                                      IN PREVIOUS WQE LINK FIELD    */ 03219000
         MVC   WQELKPA-WQE(3,R1),WQELKPA-WQE(R10)                       03220000
*             IF UCMWQEND=XVWQEAD THEN/* WAS THIS WQE THE LAST ON THE   03221000
*                                      CHAIN?                        */ 03222000
         L     R8,CVTCUCB                                               03223000
         USING UCM,R8                                                   03224000
         C     R10,UCMWQEND                                             03225000
         BNE   @RF00683                                                 03226000
*               UCMWQEND=R1;        /* YES. UPDATE PTR IN UCM TO        03227000
*                                      POINT TO NEW END OF WQE CHAIN */ 03228000
         ST    R1,UCMWQEND                                              03229000
         DROP  R8                                                       03230000
*             R1=0;                 /* GET OUT OF LOOP               */ 03231000
@RF00683 SLR   R1,R1                                                    03232000
         B     @DE00679                                                 03233000
*                                                                       03234000
*           END;                                                        03235000
*         ELSE                      /* NO, NOT OURS. PT TO NEXT WQE  */ 03236000
*           R1=R1->WQELKPA;                                             03237000
@RF00680 L     R10,WQELKPA-1-WQE(,R1)                                   03238000
         LA    R10,0(,R10)                                              03239000
         LR    R1,R10                                                   03240000
*       END;                        /* THE WQE IS NOW OFF THE WQE       03241000
*                                      CHAIN                         */ 03242000
@DE00679 LTR   R1,R1                                                    03243000
         BNZ   @DL00679                                                 03244000
*       /*************************************************************/ 03245000
*       /*                                                           */ 03246000
*       /* FREE THE WQE                                              */ 03247000
*       /*                                                           */ 03248000
*       /*************************************************************/ 03249000
*       R0=UCMWQECP;                /* CELL POOL ID FOR WQES         */ 03250000
         L     R10,CVTCUCB                                              03251000
         USING UCM,R10                                                  03252000
         L     R0,UCMWQECP                                              03253000
         DROP  R10                                                      03254000
*       R1=XVWQEAD;                 /* ADDR OF THE WQE               */ 03255000
         L     R1,XVWQEAD                                               03256000
*                                                  /* FREE WQE CELL  */ 03257000
         FREECELL CPID=(0),CELL=(1),BRANCH=YES                          03258000
*                                                                       03259000
*/********************************************************************/ 03260000
*/*                                                                  */ 03261000
*/* THIS WQE CELL MAY HAVE BEEN THE LAST ONE IN AN EXTENSION.        */ 03262000
*/* IF SO THEN R15 IS SET TO 20 BY FREECELL. IF THIS CONDITION       */ 03263000
*/* IS FOUND THEN FREE THE EXTENSION. R0 AND R1 ARE                  */ 03264000
*/* SET BY FREECELL                                                  */ 03265000
*/*                                                                  */ 03266000
*/********************************************************************/ 03267000
*       IF R15=20 THEN              /* WAS CELL THE LAST IN AN          03268000
*                                      EXTENSION                     */ 03269000
         C     R15,KF20                                                 03270000
         BNE   @RF00677                                                 03271000
*         /***********************************************************/ 03272000
*         /*                                                         */ 03273000
*         /* YES, FREE THE EXTENSION                                 */ 03274000
*         /*                                                         */ 03275000
*         /***********************************************************/ 03276000
*                                                                       03277000
         FREEMAIN R,LV=(0),A=(1),BRANCH=YES                             03278000
*                                                                       03279000
*     END;                                                              03280000
*   R13=XVA8;                       /* RESTORE CALLER'S SAVE AREA       03281000
*                                      ADDR                          */ 03282000
@RF00677 L     R13,XVA8                                                 03283000
         RETURN (14,12)             /* GO BACK TO IEAVMFRR ROUTINE*/    03284000
*                                                                       03285000
*/********************************************************************/ 03286000
*/*                                                                  */ 03287000
*/*      VWTOVALR                                                    */ 03288000
*/*                                                                  */ 03289000
*/*      THIS ROUTINE GETS CONTROL VIA THE FRR OR ESTAE PARMLIST     */ 03290000
*/*      ENTRY PARMRTAD IF AN ERROR IS DETECTED WHILE VALIDATING     */ 03291000
*/*      THE WPL.                                                    */ 03292000
*/*      REGISTERS ARE RESTORED BY SETRP REQUEST IN IEAVMFRR         */ 03293000
*/*                                                                  */ 03294000
*/********************************************************************/ 03295000
*VWTOVALR:                                                              03296000
         USING PARMLIST,R10                                             03297000
VWTOVALR MVI   XVREASON,D23PARM        PARMLIST NOT ADDRESSABLE BY USER 03298000
*                                                                       03299000
*        ENTRY POINT FOR VALIDATION ERROR DETECTED BY THE CODE          03300000
*        REASON CODE ALREADY SET                                        03301000
*                                                                       03302000
VWTOVALB MVI   XVFNCT,D23VALID         PARMLIST VALIDITY CHECK          03303000
         MVI   XVAMOD,VWTOID           IEAVVWTO'S ID FOR D23            03304000
*   XVD2VALD='0'B;                  /* WPL IS NOT VALID              */ 03305000
         NI    XVD2,255-XVD2VALD                                        03306000
         OI    XVD1,XVD1PERR           SET ERROR FLAG                   03307000
*   PARMRTAD=0;                     /* CLEAR RETRY ADDR TO AVOID        03308000
*                                      LOOPING                       */ 03309000
         XC    PARMRTAD,PARMRTAD                                        03310000
*                                                                       03311000
*        FREE THE LOCKS AND REMOVE THE FRR                              03312000
*        ORIGINAL ESTAE IS NOW IN PLACE                                 03313000
*                                                                       03314000
         OI    PFLAG,NOCMSLOK          TURN ON NO CMS LOCK REQUESTED    03315000
         BAL   R14,FREELOCK                                             03316000
         NI    PFLAG,255-NOCMSLOK      TURN OFF NO CMS LOCK REQUESTED   03317000
*                                   /* GOTO EXIT CODE                */ 03318000
         B     VWTOERRT                                                 03319000
*                                                                       03320000
*/********************************************************************/ 03321000
*/*                                                                  */ 03322000
*/*  VWTOSSRT - RETRY ROUTINE FOR SUBSYTEM EXIT                      */ 03323000
*/*                                                                  */ 03324000
*/*  FUNCTION - THIS ROUTINE WILL GET CONTROL IF THERE WAS AN ERROR  */ 03325000
*/*   IN THE SUBSYSTEM WTO EXIT ROUTINE. WE WILL TURN OFF XVD2DELW   */ 03326000
*/*  SO THAT THE MESSAGE WILL BE SURE TO GO OUT.  THEN BRANCH TO     */ 03327000
*/*  VWTOSSLB TO CONTINUE THE PROCESSING                             */ 03328000
*/*                                                                  */ 03329000
*/********************************************************************/ 03330000
*VWTOSSRT:                                                              03331000
*   XVD2DELW='0'B;                  /* DONT DELETE THE MESSAGE       */ 03332000
VWTOSSRT NI    XVD2,255-XVD2DELW                                        03333000
*   GO TO VWTOSSLB;                 /* CONTINUE WITH PROCESSING      */ 03334000
         B     VWTOSSLB                                                 03335000
*                                                                       03336000
*/********************************************************************/ 03337000
*/*  GETBLOCK                                                        */ 03338000
*/*                                                                  */ 03339000
*/*  FUNCTION: THIS ROUTINE WILL GET STORAGE FOR EITHER A WQE OR ORE */ 03340000
*/*     THE OBTAINED STORAGE WILL BE ZEROED OUT                      */ 03341000
*/*                                                                  */ 03342000
*/*  INPUT: TYPE INDICATES THE TYPE OF BLOCK TO GET. WQE OR ORE      */ 03343000
*/*          XVD0WWB MAY BE ON INDICATING THAT A WWB HAS BEEN        */ 03344000
*/*     OBTAINED.                                                    */ 03345000
*/*         XVWWB POINTS AT THE WWB                                  */ 03346000
*/*                                                                  */ 03347000
*/*  OUTPUT: A BLOCK OF THE PROPER SIZE AND ZEROED IS RETURNED TO THE*/ 03348000
*/*     CALLER. IF THE USER WAS ON THE WAIT CHAIN, THE WAIT ECB IS   */ 03349000
*/*     REMOVED.                                                     */ 03350000
*/*     IF THE GETCELL WAS UNSUCCESSFUL THEN THE ADDR PTR IS SET TO  */ 03351000
*/*     ONE INSTEAD OF BEING THE ADDR OF THE NEW CELL.               */ 03352000
*/*                                                                  */ 03353000
*/********************************************************************/ 03354000
*VWTOGETB:                                                              03355000
*                                                                       03356000
VWTOGETB ST    R14,12(,R13)           SAVE RETURN ADDR                  03357000
         ST    R13,@SAGETB+4          BACKCHAIN SAVE AREA               03358000
         LA    R14,@SAGETB                                              03359000
         LR    R13,R14                                                  03360000
         MVC   @PC00003(4),0(R1)                                        03361000
*/********************************************************************/ 03362000
*/*                                                                  */ 03363000
*/* CHECK IF REQUEST IS FOR AN ORE OR FOR A WQE                      */ 03364000
*/*                                                                  */ 03365000
*/********************************************************************/ 03366000
*                                                                       03367000
*   IF TYPE=2 THEN                  /* THIS REQUEST FOR AN ORE   ?   */ 03368000
         L     R14,@PC00003                                             03369000
         CLC   0(4,R14),KF2                                             03370000
         BNE   @RF00709                                                 03371000
*     DO;                           /* YES. GET AN ORE CELL          */ 03372000
*       XVOREAD=0;                  /* SET ADDR TO ZERO SO IT CAN BE    03373000
*                                      USED TO INDICATE SUCCESS OF      03374000
*                                      GETCELL                       */ 03375000
         SLR   R14,R14                                                  03376000
         ST    R14,XVOREAD                                              03377000
*       DO WHILE(XVOREAD=0);        /* LOOP UNTILL XVOREAD GETS         03378000
*                                      FILLED IN                     */ 03379000
         B     @DE00712                                                 03380000
*                                                                       03381000
*         R0=UCMORECP;              /* PICK UP ORE CELL POOL ID      */ 03382000
@DL00712 L     R14,CVTCUCB                                              03383000
         USING UCM,R14                                                  03384000
         L     R0,UCMORECP                                              03385000
*                                                                       03386000
         GETCELL CPID=(0),BRANCH=YES,SAVE=YES                           03387000
*                                                                       03388000
*/********************************************************************/ 03389000
*/*                                                                  */ 03390000
*/*      SEE IF GETCELL WAS SUCCESSFUL                               */ 03391000
*/*                                                                  */ 03392000
*/********************************************************************/ 03393000
*                                                                       03394000
*         IF R15=0 THEN             /* DID WE GET A CELL             */ 03395000
         LTR   R15,R15                                                  03396000
         BNZ   @RF00715                NO, BRANCH                       03397000
*           DO;                                                         03398000
*             XVOREAD=R1;           /* YES, SAVE ADDRESS TO ORE      */ 03399000
         ST    R1,XVOREAD                                               03400000
*             /*******************************************************/ 03401000
*             /*                                                     */ 03402000
*             /* ZERO OUT THE CONTROL BLOCK                          */ 03403000
*             /*                                                     */ 03404000
*             /*******************************************************/ 03405000
*             R1->0(1:LENGTH(OREF)*8)=''B;                              03406000
         XC    0(32,R1),0(R1)                                           03407000
*             UCMRQNR=UCMRQNR+1;    /* INCREMENT ORE COUNT BY ONE    */ 03408000
         L     R14,CVTCUCB                                              03409000
         LA    R10,1                                                    03410000
         AH    R10,UCMRQNR                                              03411000
         STH   R10,UCMRQNR                                              03412000
         DROP  R14                                                      03413000
         B     @DE00712                                                 03414000
*                                                                       03415000
*           END;                                                        03416000
*         ELSE                      /* NO THE GETCELL WAS NOT           03417000
*                                      SUCCESSFUL                    */ 03418000
*/********************************************************************/ 03419000
*/*                                                                  */ 03420000
*/* WE DID NOT GET A CELL. CHECK IF WE ARE OUT OF CELLS. IF SO       */ 03421000
*/* THEN GET AN EXTENSION.                                           */ 03422000
*/*                                                                  */ 03423000
*/********************************************************************/ 03424000
*           IF R15=4 THEN           /* DO WE NEED TO EXTEND THE CELL    03425000
*                                      POOL                          */ 03426000
@RF00715 C     R15,KF4                                                  03427000
         BNE   @RF00721                                                 03428000
*             DO;                   /* YES                           */ 03429000
*               R1=SUBPL231;        /* SET UP FOR GETMAIN FROM          03430000
*                                      SUBPOOL 231                   */ 03431000
         LA    R1,231                                                   03432000
*               R0=OREPLSZ;         /* PICK UP SIZE OF ORE POOL         03433000
*                                      EXTENSION                     */ 03434000
         LA    R0,1024                                                  03435000
*                                                                       03436000
         GETMAIN  RC,LV=(0),SP=(1),BRANCH=YES                           03437000
*                                                                       03438000
*               R3=CVTPTR;          /* RELOAD R3 WHICH WAS              03439000
*                                      DESTROYED BY THE GETMAIN         03440000
*                                      BRANCH ENTRY                  */ 03441000
         L     R3,CVTPTR                                                03442000
         LR    R2,R15                                                   03443000
*               /*****************************************************/ 03444000
*               /*                                                   */ 03445000
*               /* SEE IF GETMAIN WAS SUCCESSFUL. IF SO BUILD EXTEN  */ 03446000
*               /*                                                   */ 03447000
*               /*****************************************************/ 03448000
*               IF R15=0 THEN       /* STORAGE OBTAINED ?            */ 03449000
         LTR   R15,R15                                                  03450000
         BNZ   @RF00727                                                 03451000
*                 DO;               /* YES, EXTEND THE ORE POOL      */ 03452000
*                   R2=R1;          /* SAVE ADDR OF POOL EXTENSION   */ 03453000
         LR    R2,R1                                                    03454000
*                   R15=LENGTH(OREF);/* GET SIZE OF CELL REQUIRED       03455000
*                                      FOR BLD                       */ 03456000
         LA    R15,ORESIZE                                              03457000
*                   R0=UCMORECP;    /* PICK UP ORE CELL POOL ID      */ 03458000
         L     R14,CVTCUCB                                              03459000
         USING UCM,R14                                                  03460000
         L     R0,UCMORECP                                              03461000
         DROP  R14                                                      03462000
*                                                                       03463000
         BLDCPOOL  CPID=(0),CSIZE=(15),SP=231,CPADDR=(1),              C03464000
               BRANCH=YES,POOLSIZ=1,AUTODEL=YES,SERIAL=YES              03465000
*                                                                       03466000
*/********************************************************************/ 03467000
*/*                                                                  */ 03468000
*/* CHECK IF EXTENSION WAS SUCCESSFUL. IF NOT SET ERROR FLAG ON      */ 03469000
*/*                                                                  */ 03470000
*/********************************************************************/ 03471000
*                   IF R15^=0 THEN/* WAS EXTENSION UNSUCCESSFUL      */ 03472000
         LTR   R15,R14                                                  03473000
         BZ    @DE00712                                                 03474000
         MVI   XVFNCT,D23OREBC         ORE BLDCPOOL FAILURE             03475000
         MVI   XVAMOD,VWTOID           IEAVVWTO'S ID FOR D23            03476000
         STC   R15,XVREASON            STORE RETURN CODE IN REASON CODE 03477000
*                     DO;           /* YES. INDICATE SO IN XVOREAD   */ 03478000
*                       /*********************************************/ 03479000
*                       /*                                           */ 03480000
*                       /* FREE THE CORE FOR THE EXTENSION           */ 03481000
*                       /*                                           */ 03482000
*                       /*********************************************/ 03483000
*                       R0=FROREXT;/* GET SUBPOOL AND LENGTH PARM    */ 03484000
         L     R0,FROREXT                                               03485000
*                       R1=R2 ;/* GET ADDR OF EXTENSION              */ 03486000
         LR    R1,R2                                                    03487000
*                                                                       03488000
         FREEMAIN R,LV=(0),A=(1),BRANCH=YES                             03489000
*                       XVOREAD=1;  /* INDICATE NO ORE OBTAINED      */ 03490000
         MVC   XVOREAD,KF1                                              03491000
         B     @DE00712                                                 03492000
*                                                                       03493000
*                     END;                                              03494000
*                 END;                                                  03495000
*               ELSE                                                    03496000
*                 /***************************************************/ 03497000
*                 /*                                                 */ 03498000
*                 /* GETMAIN WAS NOT SUCCESSFUL. CALL ROUTINE TO     */ 03499000
*                 /* CHECK IF ERROR IS DUE TO SYSLOG NOT YET         */ 03500000
*                 /* INITIALIZED                                     */ 03501000
*                 /*                                                 */ 03502000
*                 /***************************************************/ 03503000
*                 DO;                                                   03504000
*                   CALL VWTOLREC(XVOREAD);                             03505000
@RF00727 LA    R14,XVOREAD                                              03506000
         ST    R14,PARMPTR                                              03507000
         LA    R1,PARMPTR                                               03508000
         BAL   R14,VWTOLREC                                             03509000
         CLC   XVOREAD,KF1                                              03510000
         BNE   @DE00712                                                 03511000
         MVI   XVFNCT,D23OREGM         ORE GETMAIN FAILURE, SP231       03512000
         MVI   XVAMOD,VWTOID           IEAVVWTO'S ID FOR D23            03513000
         STC   R2,XVREASON                                              03514000
         B     @DE00712                                                 03515000
*                                                                       03516000
*                 END;                                                  03517000
*             END;                  /* OF TEST FOR EXTENSION NEEDED  */ 03518000
*           ELSE                                                        03519000
*/********************************************************************/ 03520000
*/*                                                                  */ 03521000
*/* THE ERROR WAS NOT DUE TO END OF EXTENSION. THE ERROR CAN NOT     */ 03522000
*/* BE CORRECTED SO SET ERROR INDICATOR FOR CALLER                   */ 03523000
*/*                                                                  */ 03524000
*/********************************************************************/ 03525000
*             XVOREAD=1;            /* INDICATE GET ORE UNSUCCESSFUL */ 03526000
@RF00721 MVI   XVFNCT,D23OREGC         ORE GETCELL FAILURE              03527000
         MVI   XVAMOD,VWTOID           IEAVVWTO'S ID FOR D23            03528000
         STC   R15,XVREASON            STORE RETURN CODE IN REASON CODE 03529000
         MVC   XVOREAD,KF1                                              03530000
*       END;                        /* OF DO WHILE LOOP              */ 03531000
@DE00712 ICM   R14,B'1111',XVOREAD                                      03532000
         BZ    @DL00712                                                 03533000
         B     @RC00709                                                 03534000
*                                                                       03535000
*     END;                                                              03536000
*   /*****************************************************************/ 03537000
*   /*                                                               */ 03538000
*   /* WQE GET CELL PART OF THIS SEGMENT                             */ 03539000
*   /*                                                               */ 03540000
*   /*****************************************************************/ 03541000
*   ELSE                            /* NO THIS REQUEST IS FOR A WQE  */ 03542000
*     DO;                                                               03543000
*       XVWQEAD=0;                  /* ZERO OUT ADDR SO IT CAN BE       03544000
*                                      USED TO INDICATE SUCCESS OR      03545000
*                                      FAILURE OF GETCELL            */ 03546000
@RF00709 SLR   R14,R14                                                  03547000
         ST    R14,XVWQEAD                                              03548000
*       DO WHILE(XVWQEAD=0);        /* LOOP UNTIL ADDR GETS FILLED      03549000
*                                      IN                            */ 03550000
         B     @DE00752                                                 03551000
*                                                                       03552000
*         R0=UCMWQECP;              /* PICK UP WQE CELL POOL ID      */ 03553000
@DL00752 L     R14,CVTCUCB                                              03554000
         USING UCM,R14                                                  03555000
         L     R0,UCMWQECP                                              03556000
*                                                                       03557000
         GETCELL CPID=(0),BRANCH=YES                                    03558000
*                                                                       03559000
*/********************************************************************/ 03560000
*/*                                                                  */ 03561000
*/* SEE IF GETCELL WAS SUCCESSFUL                                    */ 03562000
*/*                                                                  */ 03563000
*/********************************************************************/ 03564000
*         IF R15=0 THEN             /* DID WE GET A CELL ?           */ 03565000
         LTR   R15,R15                                                  03566000
         BNZ   @RF00755                                                 03567000
*           DO;                                                         03568000
*             XVWQEAD=R1;           /* SAVE ADDRESS TO WQE           */ 03569000
         ST    R1,XVWQEAD                                               03570000
*             /*******************************************************/ 03571000
*             /*                                                     */ 03572000
*             /* ZERO OUT THE CONTROL BLOCK                          */ 03573000
*             /*                                                     */ 03574000
*             /*******************************************************/ 03575000
*             R1->0(1:LENGTH(WQE)*8)=''B;                               03576000
         XC    0(WQESIZE,R1),0(R1)                                      03577000
*             UCMWQNR=UCMWQNR+1;    /* INCREMENT WQE COUNT           */ 03578000
         L     R14,CVTCUCB                                              03579000
         LA    R10,1                                                    03580000
         AH    R10,UCMWQNR                                              03581000
         STH   R10,UCMWQNR                                              03582000
         DROP  R14                                                      03583000
         B     @DE00752                                                 03584000
*                                                                       03585000
*           END;                                                        03586000
*/********************************************************************/ 03587000
*/*                                                                  */ 03588000
*/* WE DID NOT GET A CELL. CHECK IF WE ARE OUT OF CELLS. IF SO       */ 03589000
*/* THEN GET AN EXTENSION.                                           */ 03590000
*/*                                                                  */ 03591000
*/********************************************************************/ 03592000
*         ELSE                                                          03593000
*           IF R15=4 THEN           /* DO WE NEED TO EXTEND THE CELL    03594000
*                                      POOL                          */ 03595000
@RF00755 C     R15,KF4                                                  03596000
         BNE   @RF00761                                                 03597000
*             DO;                   /* YES                           */ 03598000
*               R1=SUBPL231;        /* SET UP FOR GETMAIN FROM 231   */ 03599000
         LA    R1,231                                                   03600000
*               R0=WQEPLSZ;         /* PICK UP SIZE OF WQE EXTENSION */ 03601000
         L     R0,KF4096                                                03602000
*                                                                       03603000
         GETMAIN  RC,LV=(0),SP=(1),BRANCH=YES                           03604000
*                                                                       03605000
*               R3=CVTPTR;          /* RELOAD R3 WHICH WAS              03606000
*                                      DESTROYED BY THE GETMAIN         03607000
*                                      CALLING SEQUENCE              */ 03608000
         L     R3,CVTPTR                                                03609000
         LR    R2,R15                                                   03610000
*               /*****************************************************/ 03611000
*               /*                                                   */ 03612000
*               /* SEE IF GETMAIN WAS SUCCESSFUL. IF SO BUILD        */ 03613000
*               /* EXTENSION.                                        */ 03614000
*               /*                                                   */ 03615000
*               /*****************************************************/ 03616000
*               IF R15=0 THEN       /* GET SPACE FOR THE EXTENSION ? */ 03617000
         LTR   R15,R15                                                  03618000
         BNZ   @RF00767                                                 03619000
*                 DO;               /* YES, BUILD THE WQE EXTENSION  */ 03620000
*                   R10=R1;         /* SAVE ADDRES OF WQE EXTENSION  */ 03621000
         LR    R2,R1                                                    03622000
*                   R0=UCMWQECP;    /* PICK UP THE WQE CELL POOL ID  */ 03623000
         L     R14,CVTCUCB                                              03624000
         USING UCM,R14                                                  03625000
         L     R0,UCMWQECP                                              03626000
         DROP  R14                                                      03627000
*                   R15=LENGTH(WQE);/* GET SIZE OF CELL REQUIRED     */ 03628000
         LA    R15,192                                                  03629000
*                                                                       03630000
         BLDCPOOL CPID=(0),SP=231,CSIZE=(15),CPADDR=(1),BRANCH=YES,    C03631000
               AUTODEL=YES,POOLSIZ=4,SERIAL=YES                         03632000
*                                                                       03633000
*/********************************************************************/ 03634000
*/*                                                                  */ 03635000
*/* CHECK IF EXTENSION WAS SUCCESSFUL. IF NOT SET ERROR FLAG ON      */ 03636000
*/*                                                                  */ 03637000
*/********************************************************************/ 03638000
*                   IF R15^=0 THEN/* WAS EXTENSION UNSUCCESSFUL      */ 03639000
         LTR   R15,R15                                                  03640000
         BZ    @DE00752                                                 03641000
         MVI   XVFNCT,D23WQEBC         WQE BLDCPOOL FAILURE             03642000
         MVI   XVAMOD,VWTOID           IEAVVWTO'S ID FOR D23            03643000
         STC   R15,XVREASON            STORE RETURN CODE IN REASON CODE 03644000
*                     DO;           /* YES, INDICATE SO IN XVWQEAD   */ 03645000
*                       /*********************************************/ 03646000
*                       /*                                           */ 03647000
*                       /* FREE THE WQE POOL EXTENSION               */ 03648000
*                       /*                                           */ 03649000
*                       /*********************************************/ 03650000
*                       R0=FRWQEXT;/* GET SUBPOOL AND LENGTH         */ 03651000
         L     R0,FRWQEXT                                               03652000
*                       R1=R2 ;/* GET ADDRESS OF WQE EXTENSION       */ 03653000
         LR    R1,R2                                                    03654000
*                                                                       03655000
         FREEMAIN R,LV=(0),A=(1),BRANCH=YES                             03656000
*                                                                       03657000
*                       XVWQEAD=1;  /* INDICATE NO WQE OBTAINED      */ 03658000
         MVC   XVWQEAD,KF1                                              03659000
         B     @DE00752                                                 03660000
*                                                                       03661000
*                     END;                                              03662000
*                 END;                                                  03663000
*               /*****************************************************/ 03664000
*               /*                                                   */ 03665000
*               /* THE GETMAIN WAS NOT SUCCESSFUL. CALL THE LOG      */ 03666000
*               /* RECOVERY ROUTINE TO SEE IF ERROR DUE TO SYSLOG    */ 03667000
*               /* INIT                                              */ 03668000
*               /*                                                   */ 03669000
*               /*****************************************************/ 03670000
*               ELSE                                                    03671000
*                 DO;                                                   03672000
*                   CALL VWTOLREC(XVWQEAD);/* LOG RECOVERY ROUTINE   */ 03673000
@RF00767 LA    R14,XVWQEAD                                              03674000
         ST    R14,PARMPTR                                              03675000
         LA    R1,PARMPTR                                               03676000
         BAL   R14,VWTOLREC                                             03677000
         CLC   XVWQEAD,KF1                                              03678000
         BNE   @DE00752                                                 03679000
         MVI   XVFNCT,D23WQEGM         WQE GETMAIN FAILURE, SP231       03680000
         MVI   XVAMOD,VWTOID           IEAVVWTO'S ID FOR D23            03681000
         STC   R2,XVREASON             STORE RETURN CODE IN REASON CODE 03682000
         B     @DE00752                                                 03683000
*                                                                       03684000
*                 END;                                                  03685000
*             END;                                                      03686000
*/********************************************************************/ 03687000
*/*                                                                  */ 03688000
*/* THE ERROR WAS NOT DUE TO END OF EXTENSION. THE ERROR CAN NOT     */ 03689000
*/* BE CORRECTED SO SET ERROR INDICATOR FOR CALLER                   */ 03690000
*/*                                                                  */ 03691000
*/********************************************************************/ 03692000
*           ELSE                                                        03693000
*             XVWQEAD=1;            /* INDICATE GET WQE UNSUCCESSFUL */ 03694000
@RF00761 MVI   XVFNCT,D23WQEGC         WQE GETCELL FAILURE              03695000
         MVI   XVAMOD,VWTOID           IEAVVWTO'S ID FOR D23            03696000
         STC   R15,XVREASON            STORE RET CODE INTO REASON CODE  03697000
         MVC   XVWQEAD,KF1                                              03698000
*       END;                        /* OF DO WHILE LOOP              */ 03699000
@DE00752 ICM   R14,B'1111',XVWQEAD                                      03700000
         BZ    @DL00752                                                 03701000
*     END;                                                              03702000
*/********************************************************************/ 03703000
*/*                                                                  */ 03704000
*/* CHECK IF WE HAVE TO FREE THE ECB                                 */ 03705000
*/*                                                                  */ 03706000
*/********************************************************************/ 03707000
*   IF XVD0WWB='1'B THEN            /* A WWB ON THE CHAIN ?          */ 03708000
@RC00709 TM    XVD0,XVD0WWB                                             03709000
         BZ    @ER00003                                                 03710000
*     DO;                           /* YES, FREE IT AND TURN OFF        03711000
*                                      XVD0WWB                       */ 03712000
*       /*************************************************************/ 03713000
*       /*                                                           */ 03714000
*       /* POINT NEXT WWB AT BACK WWB                                */ 03715000
*       /*                                                           */ 03716000
*       /*************************************************************/ 03717000
*       WWBFWDPT->WWBBCKPT=WWBBCKPT;                                    03718000
         L     R14,XVWWB                                                03719000
         USING WWB,R14                                                  03720000
         L     R10,WWBFWDPT                                             03721000
         L     R8,WWBBCKPT                                              03722000
         ST    R8,WWBBCKPT-WWB(R10)                                     03723000
         DROP  R14                                                      03724000
*       /*************************************************************/ 03725000
*       /*                                                           */ 03726000
*       /* POINT BACK WWB AT NEXT WWB                                */ 03727000
*       /*                                                           */ 03728000
*       /*************************************************************/ 03729000
*       WWBBCKPT->WWBFWDPT=WWBFWDPT;                                    03730000
         ST    R10,WWBFWDPT-WWB(,R8)                                    03731000
*/********************************************************************/ 03732000
*/*                                                                  */ 03733000
*/* WWB IS NOW OFF THE QUEUE. FREE IT                                */ 03734000
*/*                                                                  */ 03735000
*/********************************************************************/ 03736000
*       R0=GETWWB;                  /* PICK PARMS FOR FREEMAIN       */ 03737000
         L     R0,GETWWB                                                03738000
*       R1=XVWWB;                   /* GET ADDRESS OF WWB            */ 03739000
         LR    R1,R14                                                   03740000
*                                                                       03741000
         FREEMAIN R,LV=(0),A=(1),BRANCH=YES                             03742000
*                                                                       03743000
*       XVWWB=0;                    /* ZERO OUT ADDRESS OF WWB IN XSA*/ 03744000
         SLR   R10,R10                                                  03745000
         ST    R10,XVWWB                                                03746000
*       XVD0WWB='0'B;               /* INDICATE NO WWB ON CHAIN      */ 03747000
         NI    XVD0,255-XVD0WWB                                         03748000
*     END;                                                              03749000
*   RETURN;                                                             03750000
@ER00003 L     R13,4(,R13)                                              03751000
         L     R14,12(,R13)                                             03752000
         BR    R14                                                      03753000
*                                                                       03754000
*   END VWTOGETB;                                                       03755000
         B     @ER00003                                                 03756000
*                                                                       03757000
*/*** START OF WAITCHN  **********************************************/ 03758000
*/********************************************************************/ 03759000
*/*                                                                  */ 03760000
*/*   WAITCHN - WAIT ON THE ECB IN THE WWB                           */ 03761000
*/*                                                                  */ 03762000
*/*   FUNCTION - THIS ROUTINE WILL CREATE A WWB AND PUT IT ON THE    */ 03763000
*/*     CHAIN POINTED AT BY THE INPUT PARAMETER. IF A WWB ALREADY    */ 03764000
*/*     EXISTS THE ECB PART IS ZEROED. THE ROUTINE FREES THE LOCKS   */ 03765000
*/*     AND THEN WAITS FOR THE ECB TO BE POSTED. THE LOCKS ARE       */ 03766000
*/*     THEN OBTAINED.                                               */ 03767000
*/*                                                                  */ 03768000
*/********************************************************************/ 03769000
*VWTOWAIT:                                                              03770000
*                                                                       03771000
VWTOWAIT ST    R14,@SA00004                                             03772000
         MVC   @PC00004(4),0(R1)                                        03773000
*/********************************************************************/ 03774000
*/*                                                                  */ 03775000
*/*  CHECK IF A WWB ALREADY EXISTS. IF ONE DOES THEN IT IS ON THE    */ 03776000
*/*  PROPER CHAIN.                                                   */ 03777000
*/*                                                                  */ 03778000
*/********************************************************************/ 03779000
*   IF XVD0WWB='0'B THEN            /* DO WE NEED TO GET A WWB       */ 03780000
         TM    XVD0,XVD0WWB                                             03781000
         BNZ   @RF00805                                                 03782000
*     DO;                           /* YES, GET IT AND PUT IT ON THE    03783000
*                                      CHAIN                         */ 03784000
*                                   /* BOTH R0 AND R1 WILL BE USED      03785000
*                                      FOR THE GETMAIN               */ 03786000
*       R0=GETWWB;                  /* PICK UP GETMAIN PARMS FOR WWB */ 03787000
         L     R0,GETWWB                                                03788000
*                                                                       03789000
         GETMAIN R,LV=(0),BRANCH=YES                                    03790000
*                                                                       03791000
*       XVWWB=R1;                   /* SAVE ADDR OF WWB              */ 03792000
         ST    R1,XVWWB                                                 03793000
*       XVD0WWB='1'B;               /* INDICATE WWB AVAILABLE        */ 03794000
         OI    XVD0,XVD0WWB                                             03795000
*                                   /* R1 DOES PT AT WWB. USE R1 */     03796000
*       WWBASCB=R7;                 /* STORE ADDR OF ASCB            */ 03797000
         USING WWB,R1                                                   03798000
         ST    R7,WWBASCB                                               03799000
*       WWBTCBAD=R4;                /* STORE ADDR OF TCB             */ 03800000
         ST    R4,WWBTCBAD                                              03801000
         DROP  R1                                                       03802000
*/********************************************************************/ 03803000
*/*                                                                  */ 03804000
*/* PUT WWB ON THE CHAIN                                             */ 03805000
*/*                                                                  */ 03806000
*/********************************************************************/ 03807000
*       /*************************************************************/ 03808000
*       /*                                                           */ 03809000
*       /* POINT THIS WWB AT WHATEVER OLD LAST WWB WAS POINTING AT   */ 03810000
*       /*                                                           */ 03811000
*       /*************************************************************/ 03812000
*       WWBFWDPT=CHNTAIL->WWBFWDPT;                                     03813000
         L     R10,@PC00004                                             03814000
         L     R8,0(,R10)                                               03815000
         L     R2,WWBFWDPT-WWB(,R8)                                     03816000
         ST    R2,WWBFWDPT-WWB(,R1)                                     03817000
*       CHNTAIL->WWBFWDPT=R1;       /* LINK LAST CHN MEMBER TO THIS     03818000
*                                      WWB                           */ 03819000
         ST    R1,WWBFWDPT-WWB(,R8)                                     03820000
*       WWBBCKPT=CHNTAIL;           /* PT OUR WWB BACK AT OLD LAST      03821000
*                                      WWB                           */ 03822000
         ST    R8,WWBBCKPT-WWB(,R1)                                     03823000
*       CHNTAIL=R1;                 /* PT END OF CHAIN AT OUR WWB    */ 03824000
         ST    R1,0(,R10)                                               03825000
*     END;                                                              03826000
*/********************************************************************/ 03827000
*/*                                                                  */ 03828000
*/*  NOW INDICATE ECB IS WAITING                                     */ 03829000
*/*                                                                  */ 03830000
*/********************************************************************/ 03831000
*   WWBPOSTD='0'B;                  /* INDICATE NOT POSTED           */ 03832000
@RF00805 L     R10,XVWWB                                                03833000
         USING WWB,R10                                                  03834000
         NI    WWBFLAGS,255-WWBPOSTD                                    03835000
*   WWBECB=0;                       /* READY TO BE POSTED            */ 03836000
         SLR   R8,R8                                                    03837000
         ST    R8,WWBECB                                                03838000
         DROP  R10                                                      03839000
*   CALL FREELOCK;                                                      03840000
         BAL   R14,FREELOCK                                             03841000
*   R1=ADDR(WWBECB);                /* LOAD ADDRESS OF ECB FOR WAIT  */ 03842000
         L     R1,XVWWB                                                 03843000
         LA    R1,WWBECB-WWB(,R1)                                       03844000
*   /*****************************************************************/ 03845000
*   /*                                                               */ 03846000
*   /* NOW CHECK WHICH MEMORY WE ARE IN AND ISSUE APPROPRIATE WAIT,  */ 03847000
*   /* LONG OR SHORT                                                 */ 03848000
*   /*                                                               */ 03849000
*   /*****************************************************************/ 03850000
*   IF ASCBASID=1 THEN              /* IF IN MEMORY ONE THEN ISSUE      03851000
*                                      SHORT WAIT AS WE ARE IN          03852000
*                                      COMMTASKS MEMORY              */ 03853000
         CLC   ASCBASID,KH1                                             03854000
         BNE   @RF00825                                                 03855000
*                                                                       03856000
         WAIT  ,ECB=(R1)                                                03857000
         B     @RC00825                                                 03858000
*                                                                       03859000
*   ELSE                            /* WE ARE NOT IN MEMORY ONE SO      03860000
*                                      ISSUE A LONG WAIT TO LET         03861000
*                                      COMMTASK PUT OUT SOME MESSAGES   03862000
*                                      AND FREE UP SOME WQES.        */ 03863000
@RF00825 DS    0H                                                       03864000
         WAIT  ,ECB=(R1),LONG=YES                                       03865000
*   CALL SETLOCK;                                                       03866000
@RC00825 BAL   R14,SETLOCK                                              03867000
*   RETURN;                                                             03868000
@ER00004 L     R14,@SA00004                                             03869000
         BR    R14                                                      03870000
*                                                                       03871000
*   END VWTOWAIT;                                                       03872000
*/*** END OF WAITCHN *************************************************/ 03873000
*                                                                       03874000
*/********************************************************************/ 03875000
*/*                                                                  */ 03876000
*/* THIS ROUTINE WILL FREE THE ORE POINTED AT BY XVOREAD. IT WILL    */ 03877000
*/* ALSO DECREASE THE COUNT OF ORES BY ONE TO ACCOUNT FOR THE ORE    */ 03878000
*/* JUST FREED.                                                      */ 03879000
*/*                                                                  */ 03880000
*/********************************************************************/ 03881000
*VWTOFORE:                                                              03882000
*                                                                       03883000
VWTOFORE ST    R14,12(,R13)                                             03884000
*   R0=UCMORECP;                    /* GET ORE CELL POOL ID          */ 03885000
         L     R14,CVTCUCB                                              03886000
         USING UCM,R14                                                  03887000
         L     R0,UCMORECP                                              03888000
         DROP  R14                                                      03889000
*   R1=XVOREAD;                     /* GET ADDRESS OF ORE            */ 03890000
         L     R1,XVOREAD                                               03891000
*   /*****************************************************************/ 03892000
*   /*                                                               */ 03893000
*   /* FREE THE ORE                                                  */ 03894000
*   /*                                                               */ 03895000
*   /*****************************************************************/ 03896000
*                                                                       03897000
         FREECELL CPID=(0),CELL=(1),BRANCH=YES                          03898000
*                                                                       03899000
*/********************************************************************/ 03900000
*/*                                                                  */ 03901000
*/* THIS ORE CELL MAY HAVE BEEN THE LAST ONE IN AN EXTENSION         */ 03902000
*/* IF SO THEN R15 IS SET TO 20 BY FREECELL. IF THIS CONDITON IS     */ 03903000
*/* FOUND THEN FREE THE EXTENSION. R0 AND R1 ARE SET BY FREECELL     */ 03904000
*/*                                                                  */ 03905000
*/********************************************************************/ 03906000
*   IF R15=20 THEN                  /* WAS CELL THE LAST IN AN          03907000
*                                      EXTENSION                     */ 03908000
         C     R15,KF20                                                 03909000
         BNE   @RF00837                                                 03910000
*     /***************************************************************/ 03911000
*     /*                                                             */ 03912000
*     /* YES, FREE THE EXTENSION                                     */ 03913000
*     /*                                                             */ 03914000
*     /***************************************************************/ 03915000
*                                                                       03916000
         FREEMAIN R,LV=(0),A=(1),BRANCH=YES                             03917000
*                                                                       03918000
*   UCMRQNR=UCMRQNR-1;              /* DECREASE COUNT OF ORES        */ 03919000
@RF00837 L     R10,CVTCUCB                                              03920000
         USING UCM,R10                                                  03921000
         LH    R8,UCMRQNR                                               03922000
         BCTR  R8,0                                                     03923000
         STH   R8,UCMRQNR                                               03924000
         DROP  R10                                                      03925000
*   END VWTOFORE;                                                       03926000
@ER00005 L     R14,12(,R13)                                             03927000
         BR    R14                                                      03928000
*                                                                       03929000
*/*** START OF SETLOCK  **********************************************/ 03930000
*/*                                                                  */ 03931000
*/*   SETLOCK - OBTAIN THE LOCAL AND CMS LOCK AND INSTALL AN FRR     */ 03932000
*/*                                                                  */ 03933000
*/********************************************************************/ 03934000
*/*                                                                  */ 03935000
*/*  SAVE R13 ACROSS SETLOCK                                         */ 03936000
*/*                                                                  */ 03937000
*/********************************************************************/ 03938000
*                                                                       03939000
*SETLOCK:                                                               03940000
*                                                                       03941000
SETLOCK  ST    R14,12(,R13)                                             03942000
         STM   R0,R1,20(R13)                                            03943000
         LR    R0,R11              SAVE BASE REGS                       03944000
         LR    R1,R12                                                   03945000
         LR    R2,R13              SAVE R13                             03946000
*                                                                       03947000
         SETLOCK OBTAIN,TYPE=LOCAL,MODE=UNCOND,                        X03948000
               RELATED=(UCM,IEAVVWTO(FREELOCK))                         03949000
*                                                                       03950000
         LR    R11,R0              RESTORE BASE REGS                    03951000
         LR    R12,R1                                                   03952000
         TM    PFLAG,NOCMSLOK      CMS LOCK NOT REQUESTED ?             03953000
         BNZ   SETLOCKA            YES, BRANCH                          03954000
*                                                                       03955000
         SETLOCK OBTAIN,TYPE=CMS,MODE=UNCOND,                          X03956000
               RELATED=(UCM,IEAVVWTO(FREELOCK))                         03957000
*                                                                       03958000
         LR    R11,R0              RESTORE BASE REGS                    03959000
         LR    R12,R1                                                   03960000
*   /*****************************************************************/ 03961000
*   /*                                                               */ 03962000
*   /* FRR USES R11 AND R12 AS WORK REGS. THE PARM LIST POINTED TO   */ 03963000
*   /* BY ESTAEPRM IS ZEROED BY SETFRR                               */ 03964000
*   /*                                                               */ 03965000
*   /*****************************************************************/ 03966000
*   R13=UCMFRRAD;                   /* GET ADDR OF IEAVMFRR FROM UCM */ 03967000
SETLOCKA L     R13,CVTCUCB                                              03968000
         USING UCM,R13                                                  03969000
         L     R13,UCMFRRAD                                             03970000
         DROP  R13                                                      03971000
*                                                                       03972000
*   /*****************************************************************/ 03973000
*   /*                                                               */ 03974000
*   /* SET UP THE FRR PROTECTION FOR THIS MODULE                     */ 03975000
*   /*                                                               */ 03976000
*   /*****************************************************************/ 03977000
*                                                                       03978000
         SETFRR A,FRRAD=(R13),PARMAD=ESTAEPRM,WRKREGS=(R11,R12)         03979000
*                                                                       03980000
         LR    R11,R0                  RESTORE BASE REGS                03981000
         LR    R12,R1                                                   03982000
*                                                                       03983000
*        A NEW PARMLIST HAS BEEN CREATED BY THE SETFRR THAT             03984000
*        REPLACES THE ONE CREATED BY ESTAE                              03985000
*        INITIALIZE THE NEW PARMLIST                                    03986000
*                                                                       03987000
         L     R10,ESTAEPRM            R10 -> NEW PARMLIST IN FRR       03988000
         USING PARMLIST,R10                                             03989000
*   PARMRGAD=ADDR(REGSAVE);         /* STORE ADDR OF REGISTER SAVE      03990000
*                                      AREA INTO PARM LIST           */ 03991000
         LA    R1,REGSAVE                                               03992000
         ST    R1,PARMRGAD                                              03993000
*   PARMID=MODULEID;                /* INSERT LAST FOUR LETTERS OF      03994000
*                                      MODULE NAME                   */ 03995000
         MVC   PARMID,KCVWTO                                            03996000
         OI    PARMFLAG,PARMFRID       PROTECTED BY FRR                 03997000
*                                   /* SET BASE BACK TO ESTAE           03998000
*                                      PARMLIST                      */ 03999000
*   /*****************************************************************/ 04000000
*   /*                                                               */ 04001000
*   /* THE FOLLOWING PARMFRID FIELD IN THE ESTAE PARAMETER           */ 04002000
*   /* LIST IS SET TO ZERO TO GUARANTEE THAT RECOVERY WILL BE        */ 04003000
*   /* DONE ON BOTH THE ESTAE AND FRR LEVEL                          */ 04004000
*   /*                                                               */ 04005000
*   /*****************************************************************/ 04006000
*   PARMFRID='0'B;                  /* MODULE PROTECTED BY FRR AND      04007000
*                                      ESTAE                         */ 04008000
         LA    R1,EPARM                                                 04009000
         NI    PARMFLAG-PARMLIST(R1),255-PARMFRID                       04010000
*                                   /* RESTORE BASE TO FRR PARMLIST  */ 04011000
*   /*****************************************************************/ 04012000
*   /*                                                               */ 04013000
*   /* RESTORE THE REGISTERS FROM THE SETLOCK                        */ 04014000
*   /*                                                               */ 04015000
*   /*****************************************************************/ 04016000
*   R13=R2                          /* RESTORE SAVEAREA PTR          */ 04017000
         LR    R13,R2                                                   04018000
         L     R14,12(,R13)                                             04019000
         LM    R0,R1,20(R13)                                            04020000
         STM   R0,R15,REGRECOV         REFRESH STORED RECOVERY REGS     04021000
         BR    R14                     RETURN TO CALLER                 04022000
*                                                                       04023000
*/*** START OF FREELOCK **********************************************/ 04024000
*/*                                                                  */ 04025000
*/*   FREELOCK - FREE THE LOCAL AND CMS LOCKS AND REMOVE THE FRR     */ 04026000
*/*                                                                  */ 04027000
*/********************************************************************/ 04028000
*/********************************************************************/ 04029000
*/*                                                                  */ 04030000
*/*  SAVE R13 ACROSS FREEING OF LOCKS                                */ 04031000
*/*                                                                  */ 04032000
*/********************************************************************/ 04033000
*FREELOCK:                                                              04034000
FREELOCK STM   R14,R15,12(R13)                                          04035000
         ST    R1,24(,R13)                                              04036000
         STM   R11,R12,64(R13)                                          04037000
         LR    R2,R13                  SAVE R13 IN R2                   04038000
*                                   /* FREE THE FRR, USE R1 & R15 AS    04039000
*                                      WORK REGS                     */ 04040000
         SETFRR D,WRKREGS=(R1,R15)                                      04041000
*                                                                       04042000
         TM    PFLAG,NOCMSLOK          CMS LOCK HELD ?                  04043000
         BNZ   FREELOKA                NO, BRANCH                       04044000
*                                                                       04045000
         SETLOCK RELEASE,TYPE=CMS,RELATED=(UCM,IEAVVWTO(SETLOCK))       04046000
*                                                                       04047000
FREELOKA SETLOCK RELEASE,TYPE=LOCAL,RELATED=(UCM,IEAVVWTO(SETLOCK))     04048000
*                                                                       04049000
*        NOW RELYING ON ESTAE PROTECTION AS FRR DELETED                 04050000
*                                                                       04051000
*   ESTAEPRM=ADDR(EPARM);           /* RESTORE PARMLIST ADDR TO      */ 04052000
         LA    R10,EPARM            /* ORIGINAL ESTAE PARAMETER LIST */ 04053000
         ST    R10,ESTAEPRM                                             04054000
*   PARMFRID='0'B;                  /* SIGNAL FRR NO LONGER IN PLACE    04055000
*                                      TO ESTAE                      */ 04056000
         NI    PARMFLAG,255-PARMFRID                                    04057000
         LR    R13,R2                  RESTORE R13                      04058000
*   END FREELOCK;                                                       04059000
         LM    R14,R15,12(R13)                                          04060000
         L     R1,24(,R13)                                              04061000
         LM    R11,R12,64(R13)                                          04062000
         STM   R0,R15,REGRECOV         REFRESH STORED RECOVERY REGS     04063000
         BR    R14                                                      04064000
*                                                                       04065000
*/********************************************************************/ 04066000
*/* POSTOECB - THIS ROUTINE SETS UP THE INTERFACE NECESSARY          */ 04067000
*/*            TO BRANCH ENTER XMPOST WITH AN INDICATOR SET          */ 04068000
*/*            TO RUN THE ERRET ROUTINE IN THE MASTER                */ 04069000
*/*            SCHEDULER ADDRESS SPACE                               */ 04070000
*/********************************************************************/ 04071000
*POSTOECB:                                                              04072000
POSTOECB STM   R14,R12,@SA00008                                         04073000
*   R15=CVT0PT01;                   /* GET ENTRY POINT ADDRESS FOR      04074000
*                                      BRANCH ENTRY TO XMPOST        */ 04075000
         L     R15,CVT0PT01                                             04076000
*   R1=0;                           /* CLEAR R1                      */ 04077000
         SLR   R1,R1                                                    04078000
*   R1=R1 '80000000'X;              /* SET UP R1 FOR USE AS BIT         04079000
*                                      MASK TO PREVENT THE NEED FOR A   04080000
*                                      REFERENCE TO STORAGE ONCE THE    04081000
*                                      BASEREG HAS BEEN UPDATED      */ 04082000
         O     R1,KX800000                                              04083000
*   R10=0;                          /* COMPLETION CODE WITH WHICH TO    04084000
*                                      POST UCMOECB                  */ 04085000
         SLR   R10,R10                                                  04086000
*   R13=UCMASCB;                    /* ASCB OF MEMORY CONTAINING ECB    04087000
*                                      TO BE POSTED                  */ 04088000
         L     R14,CVTCUCB                                              04089000
         USING UCM,R14                                                  04090000
         L     R13,UCMASCB                                              04091000
*   R11=ADDR(UCMOECB);              /* POST THE WTO ECB IN COMTASK   */ 04092000
         LA    R11,UCMOECB                                              04093000
*   R11=R11 R1;                     /* SET END OF LIST               */ 04094000
         OR    R11,R1                                                   04095000
*   R12=UCMWAKUP;                   /* IF XMPOST ERROR LET IEAVMEST     04096000
*                                      GET CONTROL                   */ 04097000
         L     R12,UCMWAKUP                                             04098000
         DROP  R14                                                      04099000
*   R12=R12 R1;                     /* SET BIT TO SHOW ERRET IS TO      04100000
*                                      RUN IN MASTER SCHEDULER MEMORY*/ 04101000
         OR    R12,R1                                                   04102000
*                                   /* POST COMTASK                  */ 04103000
         BALR  R14,R15                                                  04104000
*   RETURN;                         /* RETURN TO MAINLINE            */ 04105000
         LM    R14,R12,@SA00008                                         04106000
         BR    R14                                                      04107000
*                                                                       04108000
*   END POSTOECB;                                                       04109000
*   END                                                                 04110000
*                                                                       04111000
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM      */ 04112000
*/*%INCLUDE SYSLIB  (IHACTM  )                                       */ 04113000
*/*%INCLUDE SYSLIB  (IEFJSSOB)                                       */ 04114000
*/*%INCLUDE SYSLIB  (IEFSSOBH)                                       */ 04115000
*/*%INCLUDE SYSLIB  (IEFSSWT )                                       */ 04116000
*/*%INCLUDE SYSLIB  (IEFJESCT)                                       */ 04117000
*/*%INCLUDE SYSLIB  (IHAORE  )                                       */ 04118000
*/*%INCLUDE SYSLIB  (IHAWQE  )                                       */ 04119000
*/*%INCLUDE SYSLIB  (IEZWPL  )                                       */ 04120000
*/*%INCLUDE SYSLIB  (IEECUCM )                                       */ 04121000
*/*%INCLUDE SYSLIB  (IHAFRRS )                                       */ 04122000
*/*%INCLUDE SYSLIB  (IEEBASEA)                                       */ 04123000
*/*%INCLUDE SYSLIB  (IHAESTA )                                       */ 04124000
*/*%INCLUDE SYSLIB  (IHARB   )                                       */ 04125000
*/*%INCLUDE SYSLIB  (IKJRB   )                                       */ 04126000
*/*%INCLUDE SYSLIB  (IKJTCB  )                                       */ 04127000
*/*%INCLUDE SYSLIB  (IHAASVT )                                       */ 04128000
*/*%INCLUDE SYSLIB  (IHAASCB )                                       */ 04129000
*/*%INCLUDE SYSLIB  (IHAPSA  )                                       */ 04130000
*/*%INCLUDE SYSLIB  (CVT     )                                       */ 04131000
*/*%INCLUDE SYSLIB  (IHASCVT )                                       */ 04132000
*                                                                       04133000
@SM03493 MVC   USERTEXT(0),4(R15)      MOVE MESSAGE TEXT TO EXIT PARM   04134000
*                                                                       04135000
@SM03498 MVC   0(0,R1),0(R14)          MOVE TEXT INTO WQETXT            04136000
         DROP  R6                                                       04137000
*                                                                       04138000
@AL00348 DC    A(KF2)                  LIST WITH   1 ARGUMENT(S)        04139000
@AL00392 DC    A(KF1)                  LIST WITH   1 ARGUMENT(S)        04140000
*                                                                       04141000
KF1      DC    F'1'                                                     04142000
KH1      EQU   KF1+2                                                    04143000
KF2      DC    F'2'                                                     04144000
KF4      DC    F'4'                                                     04145000
KH4      EQU   KF4+2                                                    04146000
KF5      DC    F'5'                                                     04147000
KF6      DC    F'6'                                                     04148000
KF8      DC    F'8'                                                     04149000
KH8      EQU   KF8+2                                                    04150000
KH9      DC    H'9'                                                     04151000
KH16     DC    H'16'                                                    04152000
KF20     DC    F'20'                                                    04153000
KH20     EQU   KF20+2                                                   04154000
KF127    DC    F'127'                                                   04155000
KF4096   DC    F'4096'                                                  04156000
KFM1     DC    F'-1'                                                    04157000
KX800000 DC    XL4'80000000'                                            04158000
         DC    0F'0'                                                    04159000
@SIZDATD DC    AL1(229)                SUBPOOL 229 PRIVATE AREA         04160000
         DC    AL3(@ENDDATD-@DATD)     WORK AREA SIZE FOR GETMAIN       04161000
*                                                                       04162000
*        ROUTINES CALLED BY IEAVVWTO                                    04163000
*                                                                       04164000
VIEAVMWT DC    V(IEAVMWTO)             MULTI-LINE WTO                   04165000
VIEECVXT DC    V(IEECVXIT)             USER EXIT                        04166000
VIGC0203 DC    V(IGC0203E)             WTP                              04167000
*                                                                       04168000
LENMAXT  DC    A(L'WKATEXT)            L'WKATEXT                        04169000
ABNDCODE DC    XL4'00000D23'                                            04170000
KCVWTO   DC    C'VWTO'                                                  04171000
KCSSOB   DC    C'SSOB'                                                  04172000
KCPLUS   DC    C' +'                                                    04173000
KXFFFF   DC    X'FFFF'                                                  04174000
KX8000   DC    X'8000'                                                  04175000
KX0200   DC    X'0200'                                                  04176000
HEXCHAR  DC    CL16'0123456789ABCDEF'                                   04177000
PATTIME  DC    XL9'4021214B21214B2121'  ED PATTERN FOR TIME             04178000
*                                                                       04179000
*        GETMAIN PARAMETER LIST FOR WWB                                 04180000
*                                                                       04181000
GETWWB   DC    0F'0'                   GETMAIN/FREEMAIN FOR WWB EXTENT  04182000
         DC    AL1(231)                SUBPOOL                          04183000
         DC    AL3(WWBSIZE)            SIZE                             04184000
*                                                                       04185000
*        GETMAIN/FREEMAIN FOR ORE EXTENT                                04186000
*                                                                       04187000
FROREXT  DC    0F'0'                   GETMAIN/FREEMAIN FOR ORE EXTENT  04188000
         DC    AL1(231)                                                 04189000
         DC    AL3(1024)                                                04190000
*                                                                       04191000
*        GETMAIN/FREEMAIN FOR WQE EXTENT                                04192000
*                                                                       04193000
FRWQEXT  DC    0F'0'                   GETMAIN/FREEMAIN FOR WQE EXTENT  04194000
         DC    AL1(231)                                                 04195000
         DC    AL3(4096)                                                04196000
*                                                                       04197000
GETSP231 DC    0F'0'                                                    04198000
         DC    AL1(231)                                                 04199000
         DC    AL3(20+16)                                               04200000
*                                                                       04201000
IDMAP    DC    X'80'                                                    04202000
         DC    X'40'                                                    04203000
         DC    X'20'                                                    04204000
         DC    X'10'                                                    04205000
         DC    X'08'                                                    04206000
         DC    X'04'                                                    04207000
         DC    X'02'                                                    04208000
         DC    X'01'                                                    04209000
*                                                                       04210000
*        TRANSLATE ALL UNPRINTABLE CHARS TO HEX A1                      04211000
*                                                                       04212000
TRTAB    DC    X'40A1A1A1A1A1A1A1'      ~~~~~~~   00 -> 07              04213000
         DC    X'A1A1A1A1A1A1A1A1'     ~~~~~~~~   08 -> 0F              04214000
         DC    X'A1A1A1A1A1A1A1A1'     ~~~~~~~~   10 -> 17              04215000
         DC    X'A1A1A1A1A1A1A1A1'     ~~~~~~~~   18 -> 1F              04216000
         DC    X'A1A1A1A1A1A1A1A1'     ~~~~~~~~   20 -> 27              04217000
         DC    X'A1A1A1A1A1A1A1A1'     ~~~~~~~~   28 -> 2F              04218000
         DC    X'A1A1A1A1A1A1A1A1'     ~~~~~~~~   30 -> 37              04219000
         DC    X'A1A1A1A1A1A1A1A1'     ~~~~~~~~   38 -> 3F              04220000
         DC    X'40A1A1A1A1A1A1A1'      ~~~~~~~   40 -> 47              04221000
         DC    X'A1A14A4B4C4D4E4F'     ~~>.<(+|   48 -> 4F              04222000
         DC    X'50A1A1A1A1A1A1A1'     &~~~~~~~   50 -> 57              04223000
         DC    X'A1A15A5B5C5D5E5F'     ~~!$*);^   58 -> 5F              04224000
         DC    X'6061A1A1A1A1A1A1'     -/~~~~~~   60 -> 67              04225000
         DC    X'A1A16A6B6C6D6E6F'     ~~|,%_>?   68 -> 6F              04226000
         DC    X'A1A1A1A1A1A1A1A1'     ~~~~~~~~   70 -> 77              04227000
         DC    X'A1797A7B7C7D7E7F'     ~`:#@'="   78 -> 7F              04228000
         DC    X'A181828384858687'     ~ABCDEFG   80 -> 87              04229000
         DC    X'8889A1A1A1A1A1A1'     HI~~~~~~   88 -> 8F              04230000
         DC    X'A191929394959697'     ~JKLMNOP   90 -> 97              04231000
         DC    X'9899A1A1A1A1A1A1'     QR~~~~~~   98 -> 9F              04232000
         DC    X'A1A1A2A3A4A5A6A7'     ~~STUVWX   A0 -> A7              04233000
         DC    X'A8A9A1A1A1ADA1A1'     YZ~~~[~~   A8 -> AF              04234000
         DC    X'A1A1A1A1A1A1A1A1'     ~~~~~~~~   B0 -> B7              04235000
         DC    X'A1A1A1A1A1BDA1A1'     ~~~~~]~~   B8 -> BF              04236000
         DC    X'C0C1C2C3C4C5C6C7'     {ABCDEFG   C0 -> C7              04237000
         DC    X'C8C9A1A1A1A1A1A1'     HI~~~~~~   C8 -> CF              04238000
         DC    X'D0D1D2D3D4D5D6D7'     }JKLMNOP   D0 -> D7              04239000
         DC    X'D8D9A1A1A1A1A1A1'     QR~~~~~~   D8 -> DF              04240000
         DC    X'E0A1E2E3E4E5E6E7'     \~STUVWX   E0 -> E7              04241000
         DC    X'E8E9A1A1A1A1A1A1'     YZ~~~~~~   E8 -> EF              04242000
         DC    X'F0F1F2F3F4F5F6F7'     01234567   F0 -> F7              04243000
         DC    X'F8F9A1A1A1A1A1A1'     89~~~~~~   F8 -> FF              04244000
*                                                                       04245000
*        ESTAE SVC PARAMETER LIST                                       04246000
*                                                                       04247000
ESTAELST ESTAE  ,MF=L                                                   04248000
ESTAELEN EQU   *-ESTAELST              L'ESTAE PARAMETER LIST           04249000
*                                                                       04250000
MVCTEXT  MVC   WKATEXT+4(0),0(R8)                                       04251000
*                                                                       04252000
         USING WQE,R8                                                   04253000
TRINST   TR    WQETXT(0),TRTAB         TRANSLATE ALL UNPRINTABLES       04254000
         DROP  R8                                                       04255000
*                                                                       04256000
*        WORK AREA IN GETMAINED PRIVATE AREA STORAGE SUBPOOL 229        04257000
*                                                                       04258000
@DATD    DSECT                                                          04259000
@SA00001 DS    18F                                                      04260000
@SAGETB  DS    18F                                                      04261000
@PC00003 DS    F                                                        04262000
@SA00004 DS    F                                                        04263000
@PC00004 DS    F                                                        04264000
@SA00008 DS    15F                                                      04265000
@SA00002 DS    15F                                                      04266000
@PC00002 DS    F                                                        04267000
PARMPTR  DS    A                                                        04268000
@TF00001 DS    F                                                        04269000
REG1SAV  DS    F                                                        04270000
REG2SAV  DS    F                  SAVE AREA FOR R2 WHEN DEALING WITH    04271000
*                                 UCM (PREVIOUSLY USED XVA4)            04272000
LONGTXT  DS    A                  -> AREA GETMAINED FOR LONG WPL        04273000
*                                    IF ZERO THEN NO STORAGE GETMAINED  04274000
LONGLEN  DS    F                  L'GETMAINED AREA FOR LONG TEXT        04275000
*                                                                       04276000
LENMWQE  DS    F                  L'OF ALL MINOR WQES                   04277000
*                                 VALID ONLY FOR MLWTO                  04278000
MLWTOXH  DS    A                  -> MLWTO EXTENT HEADER IN CALLERS WPL 04279000
*                                 VALID ONLY FOR MLWTO                  04280000
*                                                                       04281000
*        VERSION 2 XWPL TO STORE ALL USER PROVIDED WPL FIELDS           04282000
*                                                                       04283000
WKALGH   DS    AL2                MESSAGE LENGTH OF MAJOR WQE TEXT FROM 04284000
*                                 CALLER'S WPL                          04285000
WKAMCSF  DS    XL2                MCS FLAGS COPIED FROM CALLERS WPL     04286000
WKAADTXT DS    AL4                -> MESSAGE TEXT                       04287000
WKAVRSN  DS    AL1                XWPL VERSION NUMBER                   04288000
WKAFLAGS DS    XL1                MISC FLAGS                            04289000
WKARPYLN DS    AL1                L'REPLY FOR WTOR                      04290000
WKALNGTH DS    AL1                L'XWPL, ZERO FOR VERSION 1            04291000
WKAMCSF1 DS    XL2                EXTENDED MCS FLAGS                    04292000
WKACPFLF DS    XL2                MCS FLAGS FOR CNTL PROGRAM USE        04293000
WKARPBUF DS    AL4                -> REPLY BUFFER                       04294000
WKAECBP  DS    AL4                -> ECB                                04295000
WKASEQN  DS    AL4                DOM/CONNECT ID                        04296000
WKADSC   DS    XL2             *  DESCRIPTOR CODE TO BE USED IN WTO     04297000
WKAROC   DS    XL2             V  ROUTE CODES TO BE USED IN WTO         04298000
WKAROUT  DS    XL14               EXTENDED ROUTING CODES                04299000
WKAMSGTY DS    XL2                MSGTYPE FLAGS COPIED FROM CALLERS WPL 04300000
WKAPRTY  DS    XL2                MESSAGE PRIORITY                      04301000
WKAJOBID DS    CL8                JOB ID                                04302000
WKAJOBNM DS    CL8                JOB NAME                              04303000
WKAKEY   DS    CL8                RETRIEVAL KEY                         04304000
WKATOKN  DS    AL4                TOKEN FOR DOM                         04305000
WKACNID  DS    AL4                CONSOLE ID                            04306000
WKASYSNA DS    CL8                SYSTEM NAME                           04307000
WKACNNME DS    CL8                CONSOLE NAME                          04308000
WKARCNA  DS    AL4                -> REPLY CONSOLE NAME/ID              04309000
WKACART  DS    AL4                -> CART                               04310000
WKAWSPRM DS    AL4                -> WAIT STATE PARM LIST               04311000
WKAASCB  DS    AL4                -> ASCB                               04312000
WKARSV30 DS    XL16               RESERVED                              04313000
*                                                                       04314000
WKATEXT  DS    CL130              MESSAGE TEXT (MAXIMUM 126 CHARS) +4   04315000
*                                                                       04316000
ESTAEPRM DS    A                  -> USER PARAMETER LIST PASSED TO      04317000
*                                    IEAVMFRR ESTAE/FRR EXIT            04318000
@TS00001 DS    CL1                                                      04319000
STARTID  DS    CL1                                                      04320000
CVDAREA  DS    D                                                        04321000
*                                                                       04322000
PFLAG    DS    XL1                ADDITIONAL FLAGS                      04323000
NOCMSLOK EQU   X'40'              DO NOT REQUEST OR FREE CMS LOCK       04324000
*                                                                       04325000
*        PARAMETER LIST FOR WTO USER EXIT IEECVXIT                      04326000
*                                                                       04327000
         DS    0F                                                       04328000
USERPARM DS    0CL136                                                   04329000
USERTEXT DS    CL128                                                    04330000
USERROUT DS    0CL4                                                     04331000
USERRC   DS    XL2                                                      04332000
         DS    XL2                      RESERVED                        04333000
USERDESC DS    0CL4                                                     04334000
USERDC   DS    XL2                                                      04335000
         DS    XL2                      RESERVED                        04336000
*                                                                       04337000
*        MAPPED BY IEFSSOBH                                             04338000
*                                                                       04339000
SSOB     DS    0CL20                                                    04340000
SSOBID   DS    CL4                                                      04341000
SSOBLEN  DS    FL2                                                      04342000
SSOBFUNC DS    FL2                                                      04343000
SSOBSSIB DS    AL4                                                      04344000
SSOBRETN DS    AL4                                                      04345000
SSOBINDV DS    AL4                                                      04346000
*                                                                       04347000
SSWT     DS    0CL16                                                    04348000
SSWTLEN  DS    FL2                                                      04349000
@NM00012 DS    FL2                                                      04350000
SSWTWQE  DS    AL4                                                      04351000
SSWTMIN  DS    AL4                                                      04352000
SSWTORE  DS    AL4                                                      04353000
*                                                                       04354000
*        ESTAE ROUTINE PARAMETER AREA                                   04355000
*                                                                       04356000
*        MAPPED BY PARMLIST DSECT                                       04357000
*                                                                       04358000
*        PASSED TO ESTAE/FRR EXIT IN SDWAPARM                           04359000
*                                                                       04360000
EPARM    DS    6F                                                       04361000
*                                                                       04362000
*        REGISTER RESTORE FLAGS AND REGISTER SAVE AREA                  04363000
*        USED BY COMM TASK FRR IEAVMFRR                                 04364000
*                                                                       04365000
         CNOP  2,4                *    FORCE HALFWORD ALIGNMENT         04366000
REGSAVE  DS    0CL66              |                                     04367000
REGRGMAP DS    H                  |                                     04368000
REGRECOV DS    16F                V                                     04369000
*                                                                       04370000
@ENDDATD EQU   *                                                        04371000
*                                                                       04372000
*        CVT                                                            04373000
*                                                                       04374000
         CVT   DSECT=YES                                                04375000
*                                                                       04376000
*        TCB                                                            04377000
*                                                                       04378000
         IKJTCB LIST=YES                                                04379000
*                                                                       04380000
*        RB                                                             04381000
*                                                                       04382000
         IKJRB   DSECT=YES                                              04383000
*/********************************************************************/ 04384000
*/*                                                                  */ 04385000
*/*          EXTENDED SAVEAREA MAPPING FOR SVC 35                    */ 04386000
*/*                                                                  */ 04387000
*/********************************************************************/ 04388000
*                                                                       04389000
*        DEFINED IN THE RBEXSAVE AREA                                   04390000
*                                                                       04391000
         ORG   RBEXSAVE                                                 04392000
XVSAV    DS    0A                                                       04393000
XVA4     DS    0F                      ERROR PROCESSING FIELDS          04394000
XVFNCT   DS    C                       D23 PROCESS CODE                 04395000
D23VALID EQU   X'10'                   PARMLIST VALIDITY CHECK          04396000
D23OREGC EQU   X'20'                   ORE GETCELL FAILURE              04397000
D23OREBC EQU   X'21'                   ORE BLDCPOOL FAILURE             04398000
D23OREGM EQU   X'22'                   ORE GETMAIN FAILURE, SP231       04399000
D23WQEGC EQU   X'30'                   WQE GETCELL FAILURE              04400000
D23WQEBC EQU   X'31'                   WQE BLDCPOOL FAILURE             04401000
D23WQEGM EQU   X'32'                   WQE GETMAIN FAILURE, SP231       04402000
D23DYN   EQU   X'42'                   DYNAMIC AREA GETMAIN FAILURE     04403000
XVAMOD   DS    C                       D23 MODULE ID                    04404000
VWTOID   EQU   X'01'                   IEAVVWTO'S ID FOR D23            04405000
MWTOID   EQU   X'02'                   IEAVMWTO'S ID FOR D23            04406000
XVA41    DS    C                       RESERVED                         04407000
XVREASON DS    C                       D23 REASON CODE                  04408000
D23BNDY  EQU   X'01'                   WTOR PARMLIST NOT ON WORD BNDY   04409000
D23MLWTR EQU   X'02'                   MULTILINE WTOR SPECIFIED         04410000
D23PARM  EQU   X'03'                   WPL NOT ADDRESSABLE BY USER      04411000
D23ZERO  EQU   X'04'                   ZERO TEXT LENGTH WTOR            04412000
D23CMOD  EQU   X'05'                   CALLER MODIFIED WPL              04413000
*                                      DURING WTO PROCESSING            04414000
D23LTXT  EQU   X'06'                   WTO/WTOR TEXT= CODED AND         04415000
*                                      WPLLGH ^=8                       04416000
*                                                                       04417000
XVA8     DS    AL4                     STORE TIME                       04418000
XVWPLADR DS    AL4                     -> CALLERS WPL                   04419000
XVWQEAD  DS    AL4                                                      04420000
*                                                                       04421000
*        FLAGS                                                          04422000
*                                                                       04423000
XVDFLAGS DS    0XL4                                                     04424000
XVD0     DS    X                                                        04425000
XVD0RPFD EQU   BIT1                    HARD COPY ONLY                   04426000
XVD0NWQE EQU   BIT2                    GET WQE                          04427000
XVD0NORE EQU   BIT3                    GET THE ORE FIRST                04428000
XVD0QID  EQU   BIT4                    QID FIELD IS PRESENT IN THE WPL  04429000
XVD0WWB  EQU   BIT5                    WWB WTO WAIT BLOCK OBTAINED      04430000
XVD0USER EQU   BIT6                                                     04431000
XVD0HDCY EQU   BIT7                                                     04432000
*                                                                       04433000
*        XVDFLAGS+1                                                     04434000
*                                                                       04435000
XVD1     DS    X                                                        04436000
XVD1PRIV EQU   BIT0                    PRIVILEGED USER                  04437000
XVD1ENQW EQU   BIT1                    ENQ'D ON A WQE  (VMWTO)          04438000
XVD1ENQO EQU   BIT2                    ENQ'D ON AN ORE (VMWTO)          04439000
XVD1ALDN EQU   BIT2                    ALL NEEDED CONTROL BLKS OBTAINED 04440000
XVD1PP   EQU   BIT5                    PROBLEM PROGRAM CALLER           04441000
XVD1AUTH EQU   BIT6                    KEY 0, SUPVR STATE OR APF AUTH   04442000
XVD1PERR EQU   BIT7                    SEVERE ERROR FOUND. ABEND USER   04443000
*                                                                       04444000
*        XVDFLAGS+2                                                     04445000
*                                                                       04446000
XVD2     DS    X                                                        04447000
XVD2CON  EQU   BIT0                    CONNECTING                       04448000
XVD2VALD EQU   BIT3                    PARAMETER LIST IS VALID          04449000
XVD2DELW EQU   BIT4                    SEND MSG TO HARDCOPY ONLY        04450000
XVD2ZERO EQU   BIT5                    ZERO MSG ID TO USER              04451000
XVD2WTOR EQU   BIT6                    WTOR REQUEST                     04452000
XVD2QFHC EQU   BIT7                    QUEUE THIS MSG TO HARD COPY      04453000
*                                                                       04454000
*        XVDFLAGS+3                                                     04455000
*                                                                       04456000
XVD3     DS    X                                                        04457000
XVD3BLDJ EQU   BIT0                    BUILD MAJOR WQE                  04458000
XVD3BLD1 EQU   BIT1                    BUILD LINE 1                     04459000
XVD3BLD2 EQU   BIT2                    BUILD LINE 2                     04460000
XVD3TXT1 EQU   BIT3                    TEXT LINE 1 BEING USED           04461000
XVD3TFX  EQU   BIT4                    TCBTFX WAS SET ON,TURN IT OFF    04462000
*        END OF FLAGS                                                   04463000
XVOREAD  DS    AL4                                                      04464000
         ORG   XVOREAD                                                  04465000
XVX      DS    0F                      USED AS WORK AREA BY VMWTO       04466000
XVX0     DS    X                       LINE CONTROL FLAGS - MLWTO       04467000
XVX0FLCL EQU   BIT0                    FIRST LINE IS CONTROL LINE       04468000
XVX0LL1F EQU   BIT1                    LABEL LINE 1 FOUND               04469000
XVX0LL2F EQU   BIT2                    LABEL LINE 2 FOUND               04470000
XVX0UDCL EQU   BIT3                    USE DEFAULT CONTROL LINE         04471000
XVX0FLJE EQU   BIT4                    FIRST LINE JUST 'E'              04472000
XVX0FEDE EQU   BIT5                    FORCE END (LAST LINE TO BE DE)   04473000
XVX1     DS    X                       ERROR FLAGS - MLWTO              04474000
XVX1STOP EQU   BIT0                    ERROR IN PARM LIST; IGNORE MLWTO 04475000
XVX1NOID EQU   BIT1                    NO ID FOR CONNECTING MLWTO       04476000
XVX2     DS    AL1                     NO OF LINES STILL TO DO          04477000
XVX3     DS    AL1                     NO OF LINES FROM MLWTO EXT HDR   04478000
*                                                                       04479000
XVCMAJOR DS    AL4                                                      04480000
XVCMINOR DS    AL4                                                      04481000
*                                                                       04482000
XVWWB    DS    AL4                                                      04483000
*                                                                       04484000
XVWQEID  DS    0AL4       *                                             04485000
XVWQEIDA DS    AL3        |            A NEW LINE IS TO BE              04486000
*                         |            CONNECTED TO THE MESSAGE         04487000
*                         |            WITH THIS 3 BYTE MESSAGE ID      04488000
*                         |            MLWTO ONLY                       04489000
XVCONID  DS    XL1        V            CONSOLE ID FOR THIS MESSAGE      04490000
*                                                                       04491000
XVRET    DS    0AL4                                                     04492000
XVRETCOD DS    AL4                                                      04493000
XVLEN    EQU   *-XVSAV                 MUST NOT EXCEED 48 BYTES AS      04494000
*                                      RB EXTENDED SAVE AREA USED       04495000
*                                                                       04496000
*        CONSOLE UNIT CONTROL MODULE                                    04497000
*                                                                       04498000
         IEECUCM DSECT=YES,FORMAT=NEW                                   04499000
*                                                                       04500000
*        IEAVMFRR USER'S PARM LIST                                      04501000
*                                                                       04502000
         IHACTM FTPT                                                    04503000
*                                                                       04504000
*        WRITE TO OPERATOR WAIT BLOCK MAPPING                           04505000
*                                                                       04506000
         IHACTM WWB                                                     04507000
*                                                                       04508000
*        FUNCTIONAL RECOVERY ROUTINE STACK                              04509000
*                                                                       04510000
         IHAFRRS                                                        04511000
*                                                                       04512000
*        OPERATOR REPLY ELEMENT                                         04513000
*                                                                       04514000
         IHAORE                                                         04515000
*                                                                       04516000
*        WRITE-TO-OPERATOR QUEUE ELEMENT                                04517000
*                                                                       04518000
         IHAWQE FORMAT=NEW                                              04519000
*                                                                       04520000
*        WTO/WTOR/MLWTO/WTP PARAMETER LIST                              04521000
*                                                                       04522000
         IEZWPL                                                         04523000
*                                                                       04524000
         PRINT NOGEN                                                    04525000
*                                                                       04526000
*        MASTER SCHEDULER RESIDENT DATA AREA                            04527000
*                                                                       04528000
         IEEBASEA                                                       04529000
*                                                                       04530000
*        PREFIXED SAVE AREA                                             04531000
*                                                                       04532000
         IHAPSA                                                         04533000
*                                                                       04534000
*        ADDRESS SPACE CONTROL BLOCK                                    04535000
*                                                                       04536000
         IHAASCB                                                        04537000
*                                                                       04538000
*        JOB ENTRY SUBSYSTEM COMMUNICATION TABLE                        04539000
*                                                                       04540000
         IEFJESCT                                                       04541000
*                                                                       04542000
         PRINT GEN                                                      04543000
*                                                                       04544000
IEAVVWTO CSECT                                                          04545000
R0       EQU   0                                                        04546000
R1       EQU   1                                                        04547000
R2       EQU   2                                                        04548000
R3       EQU   3                                                        04549000
R4       EQU   4                                                        04550000
R5       EQU   5                                                        04551000
R6       EQU   6                                                        04552000
R7       EQU   7                                                        04553000
R8       EQU   8                                                        04554000
R9       EQU   9                                                        04555000
R10      EQU   10                                                       04556000
R11      EQU   11                                                       04557000
R12      EQU   12                                                       04558000
R13      EQU   13                                                       04559000
R14      EQU   14                                                       04560000
R15      EQU   15                                                       04561000
*                                                                       04562000
         END   IEAVVWTO,(C'PLS1557',0802,83042)                         04563000
./ ADD NAME=IGC0203E
IGC      TITLE 'IGC0203E/IEEJB840 - WRITE TO PROGRAMMER'                00001000
*                                                                       00002000
IGC0203E CSECT                                                          00003000
*                                                                       00004000
*        STATUS -                                                       00005000
*        LASTUPD         = JCLIN    TYPE=UPD                            00006000
*        LIBRARIES       = DISTLIB=AOSB3                                00007000
*        FMID            = EBB1102                                      00008000
*        RMID            = UY13810                                      00009000
*        CSECT           = IGC0203E                                     00010000
*        LMOD            = IGC0003E                                     00011000
*        SYSMOD HISTORY  = SYSMOD   TYPE       DATE   MCS  STATUS       00012000
*                          EBB1102  FUNCTION  00.000  MOD      ACC RGN  00013000
*                          UY13810  PTF       74.164  MOD  APP ACC      00014000
*                                                                       00015000
*        ORIGINAL SOURCE FROM MVSSRC.SYM101.F13 WAS UPDATED             00016000
*        BY DISASSEMBLY TO MATCH THE LOAD MODULE AT THE UY13810         00017000
*        LEVEL                                                          00018000
*                                                                       00019000
*        CHANGE ACTIVITY -                                              00020000
*               ZP60040  - 1. SUPPORT FOR THE EXTENDED WPL              00021000
*                             PARAMETER LIST TO ENABLE THE USE OF       00022000
*                             THE TEXT OPERAND                          00023000
*                          2. LOCALIZED CODE OPTIMZATION                00024000
*               22 MAY 19  -  PROVIDE CORRECT ADDR OF WTP TEXT          00025000
*                             TO TPUT FOR WTO ROUTCDE=11 ISSUED         00026000
*                             BY TSO APPLICATION WITH                   00027000
*                             PROFILE WTPMSG SETTING                    00028000
*               18 AUG 20  -  CHANGE TO COMMON INTERNAL WORKAREA        00029000
*                             MAPPING IN SUPPORT OF MESSAGE             00030000
*                             TRUNCATION IN IEAVVWTO                    00031000
*                                                                       00032000
*        FUNCTION -                                                     00033000
*        CALLED BY IEAVVWTO (SVC 35) TO PROCESS WRITE TO PROGRAMMER     00034000
*        ROUTE CODE 11 MESSAGES BY WRITING THE MESSAGE TO THE           00035000
*        SYSTEM MESSAGE DATA SET. IT THE CALLER IS A TSO USER AND       00036000
*        HAS REQUESTED WTP MESSAGES IN THE USERS PROFILE TABLE THEN     00037000
*        THE MESSAGE IS ISSUED AS A TPUT TO THE USER                    00038000
*                                                                       00039000
*        REGISTERS ON ENTRY -                                           00040000
*        R4  -> TCB                                                     00041000
*        R5  -> SVRB WITH EXTENDED SAVE AREA USED                       00042000
*        R7  -> ASCB                                                    00043000
*        R14    RETURN ADDR TO IEAVVWTO                                 00044000
*        R15  = ENTRY                                                   00045000
*                                                                       00046000
         SAVE  (14,12),,'IGC0203E ZP60040 &SYSDATE &SYSTIME'            00047000
*                                                                       00048000
         LA    R11,0(,R15)                                              00049000
         USING IGC0203E,R11                                             00050000
         L     R0,WORKIGCG             R0 = L'WORKIGC                   00051000
         LR    R9,R0                   SAVE LENGTH FOR LATER AREA ZERO  00052000
*                                                                       00053000
*        GETMAIN WORKAREA IN SUBPOOL 231                                00054000
*                                                                       00055000
         GETMAIN R,LV=(0)                                               00056000
*                                                                       00057000
         LR    R8,R1                   R8 -> GETMAINED AREA             00058000
*                                      R9  = L'GETMAINED AREA           00059000
         SLR   R15,R15                 NO COPY, PAD OF ZERO             00060000
         MVCL  R8,R14                  ZERO WORKAREA                    00061000
         ST    R1,8(,R13)              CHAIN SAVE AREAS                 00062000
         ST    R13,4(,R1)                                               00063000
         LR    R10,R13                 R10 -> IEAVVWTO WORKAREA         00064000
         USING WORKVV,R10                                               00065000
         LR    R13,R1                                                   00066000
         USING WORKIGC,R13                                              00067000
*                                                                       00068000
         LR    R12,R5                                                   00069000
         USING RBBASIC,R12                                              00070000
         L     R3,CVTPTR                                                00071000
         USING CVT,R3                                                   00072000
         MVC   ADDRUCM,CVTCUCB         SAVE -> UCM IN ADDRUCM           00073000
         DROP  R3                                                       00074000
         ST    R4,TCBADDR                                               00075000
         L     R1,ADDRUCM                                               00076000
         USING UCM,R1                                                   00077000
         CLC   UCMCTID,ASCBASID-ASCB(R7)  CALLER WAS COMM TASK ?        00078000
         BE    @RF00148                YES, BRANCH                      00079000
         DROP  R1                                                       00080000
*   CALL GETESTAE;                  /* NO, CREATE STAE ENVIRONMENT   */ 00081000
         BAL   R14,GETESTAE                                             00082000
*   IF R15=0 THEN                   /* IF STAE ENVIRONMENT CREATED      00083000
*                                      THEN CONTINUE PROCESSING         00084000
*                                      OTHERWISE RETURN TO CALLER    */ 00085000
         LTR   R15,R15                 ESTAE ENVIRONMENT ESTABLISHED ?  00086000
         BNE   @RF00148                NO, ERROR RETURN                 00087000
*     DO;                                                               00088000
*       IF TCBJSCBB^=0 THEN         /* IF NO JSCB PTR RETURN TO         00089000
*                                      CALLER                        */ 00090000
         L     R4,TCBADDR              R1 -> TCB                        00091000
         USING TCB,R4                                                   00092000
         SLR   R2,R2                                                    00093000
         ICM   R2,B'0111',TCBJSCBB     R2 -> JSCB                       00094000
         BZ    @RF00151                NO JSCB, BRANCH                  00095000
         DROP  R4                                                       00096000
*         DO;                                                           00097000
*                                                                       00098000
*           /*********************************************************/ 00099000
*           /*                                                       */ 00100000
*           /* GET ACTIVE JSCB ADDRESS FROM JSCBACT. CHECK IF TSO    */ 00101000
*           /* USER(FIELD ASCBTSB IN THE ASCB HAS AN ADDRESS IF A TSO*/ 00102000
*           /* USER). IF SO THE MESSAGE IS PUT TO HIS TERMINAL VIA   */ 00103000
*           /* TPUT MACRO. IF THE TPUT IS SUCCESSFUL BYPASS ALL      */ 00104000
*           /* PROCESSING UP TO THE SCAN OF THE UCM TABLE FOR MORE   */ 00105000
*           /* CONSOLES THAT RECEIVE ROUTE CODE 11 MESSAGES. IF NOT  */ 00106000
*           /* SUCCESSFUL TRY TO PUT THE MESSAGE TO HIS MESSAGE DATA */ 00107000
*           /* SET.                                                  */ 00108000
*           /*                                                       */ 00109000
*           /*********************************************************/ 00110000
*                                                                       00111000
*           JSCBADDR=JSCBACT;       /* PUT ACTIVE JSCB ADDR IN ACTJSCB  00112000
         USING IEZJSCB,R2                                               00113000
         MVC   ACTJSCB,JSCBACT         ACTJSCB -> ACTIVE JSCB           00114000
         DROP  R2                                                       00115000
*           R15=4;                  /* INITIALIZE R15                */ 00116000
         LA    R15,4                                                    00117000
*           IF ASCBTSB^=0 THEN      /* CHECK FOR TSO USER            */ 00118000
         ICM   R1,B'1111',ASCBTSB-ASCB(R7)   TSB PRESENT ?              00119000
         BZ    @RF00156                NO, BRANCH                       00120000
*             CALL ISSUTPUT;        /* YES, PUT MSG TO TSO TERMINAL  */ 00121000
         BAL   R14,ISSUTPUT                                             00122000
*           IF R15^=0 THEN          /* IF PUT WASNT SUCCESSFUL THEN  */ 00123000
@RF00156 LTR   R15,R15                                                  00124000
         BZ    @RF00151                                                 00125000
*             DO;                   /* PUT MSG TO SYSTEM MSG DATASET */ 00126000
*                                                                       00127000
*               /*****************************************************/ 00128000
*               /*                                                   */ 00129000
*               /* CHECK IF THIS JOBSTEP HAS ISSUED PREVIOUS WTP     */ 00130000
*               /* MSGS. IF NOT THEN INITIALIZE THE JSCB. ENQUEUE TO */ 00131000
*               /* SERIALIZE PROCESSING OF THE PUT TO HASP. ALSO     */ 00132000
*               /* CHECK FOR THE PRESENCE OF AN RPL POINTER.         */ 00133000
*               /*                                                   */ 00134000
*               /*****************************************************/ 00135000
*                                                                       00136000
*               CALL CHECKJOB;      /* CHECK IF JOB HAS ISSUED          00137000
*                                      PREVIOUS WTP MSGS             */ 00138000
         BAL   R14,CHECKJOB                                             00139000
*               CALL ISSUEENQ;      /* ENQ TO SERIALIZE PROCESSING      00140000
*                                      AND CHECK RPL POINTER         */ 00141000
         BAL   R14,ISSUEENQ                                             00142000
*               IF R15=0 THEN       /* IF ENQ SUCCESSFUL CONTINUE       00143000
*                                      PROCESSING                    */ 00144000
         LTR   R15,R15                                                  00145000
         BNZ   @RF00151                                                 00146000
*                 DO;                                                   00147000
*                                                                       00148000
*                   /*************************************************/ 00149000
*                   /*                                               */ 00150000
*                   /* IF ENQUEUE IS SUCCESSFUL THEN R3 IS           */ 00151000
*                   /* INITIALIZED WITH THE ADDRESS OF THE MESSAGE   */ 00152000
*                   /* AND R8 WITH THE LENGTH OF THE MESSAGE. THE    */ 00153000
*                   /* MESSAGE IS CHECKED TO SEE IF IT EXCEEDS 126   */ 00154000
*                   /* BYTES (MAX LENGTH). IF IT DOES IT IS BROKEN UP*/ 00155000
*                   /* INTO 126 BYTE SEGMENTS OR LESS DEPENDING WHERE*/ 00156000
*                   /* THE LAST FULL WORD ENDS. EACH SEGMENT WILL BE */ 00157000
*                   /* PUT TO THE SYSTEM MSG DATA SET UNTIL ALL      */ 00158000
*                   /* SEGMENTS HAVE BEEN EXHAUSTED. THE RPL IS      */ 00159000
*                   /* INITIALIZED AND THE PUT TO THE SYSTEM MSG DATA*/ 00160000
*                   /* SET ISSUED. RETURN CODES FROM JES2 ARE CHECKED*/ 00161000
*                   /* FOR SUCCESSFUL COMPLETION OF THE PUT          */ 00162000
*                   /*                                               */ 00163000
*                   /*************************************************/ 00164000
*                                                                       00165000
*                                   /* R3 -> MCS + MESSAGE TEXT      */ 00166000
         L     R3,WKAADTXT                                              00167000
         LA    R3,4(,R3)               R3 -> MESSAGE TEXT               00168000
*                                   /* GET MSG TEXT LENGTH INTO R8   */ 00169000
         LH    R8,WKALGH                                                00170000
         SH    R8,KH4                  SUBTRACT FOR MCS HEADER          00171000
*                   DO WHILE(R8^=0);/* AS LONG AS A MSG SEGMENT         00172000
*                                      EXIST CONTINUE PROCESSING     */ 00173000
         B     @DE00167                                                 00174000
*                                                                       00175000
*                     CALL CHECKMSG;/* CHECK IF MSG > 126 BYTES      */ 00176000
@DL00167 BAL   R14,CHECKMSG                                             00177000
*                     CALL BUILDRPL;/* INITIALZE RPL FOR PUT         */ 00178000
         BAL   R14,BUILDRPL            RPL ADDR RETURNED IN R1          00179000
*                                   /* ISSUE PUT TO SYSTEM MSG DATASET  00180000
         LTR   R1,R1                   RPL ADDR ZERO ?                  00181000
         BZ    IGC000F6                YES, BRANCH                      00182000
         USING IFGRPL,R1                                                00183000
         ICM   R5,B'1111',RPLDACB      R5 -> DATA ACB                   00184000
         BZ    IGC000F6                ACB ADDRESS ZERO ? YES, BRANCH   00185000
         USING IFGACB,R5                                                00186000
         TM    ACBOFLGS,ACBOPEN        ACB OPEN ?                       00187000
         BZ    IGC000F6                NO, BRANCH                       00188000
         ICM   R15,B'1111',ACBINRTN    INTERFACE ROUTINE ADDR ZERO ?    00189000
         BZ    IGC000F6                YES, BRANCH                      00190000
*                                                                       00191000
         PUT   RPL=(1)                 WRITE MESSAGE TO SYSTEM MSG DS   00192000
*                                                                       00193000
         DROP  R1,R5                                                    00194000
*                                                                       00195000
         B     IGC000FA                                                 00196000
*                                                                       00197000
IGC000F6 LA    R15,4                   SET ERROR RETURN CODE            00198000
IGC000FA BAL   R14,CKRETURN                                             00199000
*                     CALL CKRETURN;/* CHECK IF PUT SUCCESSFUL       */ 00200000
*                   END;                                                00201000
@DE00167 LTR   R8,R8                   ANY MORE MESSAGE TEXT TO PROC ?  00202000
         BP    @DL00167                YES, LOOP BACK                   00203000
*                   R8=R15;         /* SAVE RETURN CODE              */ 00204000
         LR    R8,R15                                                   00205000
*                   CALL ISSUEDEQ;  /* RELEASE RESOURCE              */ 00206000
         BAL   R14,ISSUEDEQ                                             00207000
*                   R15=R8;         /* RESTORE RETURN CODE           */ 00208000
         LR    R15,R8                                                   00209000
*                 END;                                                  00210000
*             END;                                                      00211000
*         END;                                                          00212000
*                                                                       00213000
*       /*************************************************************/ 00214000
*       /*                                                           */ 00215000
*       /* IF AN ERROR OCCURRED DURING THE PUT TO JES2 OR THE RPL    */ 00216000
*       /* POINTER WAS ZERO AN ERROR MSG WILL BE ISSUED VIA WTO TO   */ 00217000
*       /* HARD COPY                                                 */ 00218000
*       /*                                                           */ 00219000
*       /*************************************************************/ 00220000
*                                                                       00221000
*       IF R15^=0 THEN              /* CK IF I/O ERROR OCCURRED      */ 00222000
@RF00151 LTR   R15,R15                                                  00223000
         BZ    @RF00180                                                 00224000
*         DO;                                                           00225000
*           CALL BUILDMSG;          /* CREATE WTO MSG FOR HARDCOPY   */ 00226000
         BAL   R14,BUILDMSG                                             00227000
*           CALL ISSUEMSG;          /* ISSUE WTO TO HARD COPY        */ 00228000
         BAL   R14,ISSUEMSG                                             00229000
*         END;                                                          00230000
*                                                                       00231000
*/********************************************************************* 00232000
*                                                                       00233000
*        TO DETERMINE WHETHER TO TURN ON XVD0USER BIT (RETURN TO        00234000
*        USER BIT) CHECKS ARE MADE FOR ADDITIONAL ROUTE CODES ON        00235000
*        THE MESSAGE AND IF ANY CONSOLES RECEIVE ROUTE CODE 11          00236000
*        MESSAGES.                                                      00237000
*        FLAG BITS IN THE WPL ARE ALSO CHECKED                          00238000
*        IF ANY OF THE ABOVE CONDITIONS ARE POSITIVE RETURN TO          00239000
*        WTO WITHOUT TURNING ON XVD0USER BIT.                           00240000
*                                                                       00241000
*/********************************************************************* 00242000
*                                                                       00243000
*       R1=0;                                                           00244000
@RF00180 SLR   R1,R1                                                    00245000
*STAERETY:                                                              00246000
*       IF R1^=0 THEN               /* STAE RETRY WILL ENTER HERE    */ 00247000
STAERETY LTR   R1,R1                                                    00248000
         BZ    @RF00189                                                 00249000
*         DO;                       /* R1 WILL POINT TO MY REGS      */ 00250000
*                                                                       00251000
*        RESTORE NEEDED REGISTER FROM STAEPARM AREA                     00252000
*                                                                       00253000
         LM    R9,R13,0(R1)                                             00254000
*         END;                                                          00255000
*       CALL FINALCK;               /* CK FOR ADDITIONAL WTO WORK    */ 00256000
@RF00189 BAL   R14,FINALCK                                              00257000
*                                   /* CLEAN UP ESTAE ENVIRONMENT    */ 00258000
         ESTAE 0                                                        00259000
*                                                                       00260000
         B     @EL00001                                                 00261000
*                                                                       00262000
*     END;                                                              00263000
*   ELSE                                                                00264000
*     CALL FINALCK;                 /* NO ESTAE, CK IF MORE WTO WK   */ 00265000
@RF00148 BAL   R14,FINALCK                                              00266000
*   RETURN;                         /* RETURN WTO                    */ 00267000
@EL00001 LR    R1,R13                                                   00268000
         L     R13,4(,R13)                                              00269000
         L     R0,WORKIGCG                                              00270000
*                                                                       00271000
         FREEMAIN R,LV=(0),A=(1)                                        00272000
*                                                                       00273000
         RETURN (14,12)                                                 00274000
*                                                                       00275000
*   /*****************************************************************/ 00276000
*   /*                                                               */ 00277000
*   /* GETESTAE                                                      */ 00278000
*   /*                                                               */ 00279000
*   /* BUILD A PARAMETER LIST FOR ESTAE AND ISSUE THE ESTAE MACRO    */ 00280000
*   /*                                                               */ 00281000
*   /*****************************************************************/ 00282000
*                                                                       00283000
*GETESTAE:                                                              00284000
*   MSGID='4';                      /* INITIALIZE MSG ISSUER ID TO AN   00285000
*                                      UNPREDICATABLE ERROR          */ 00286000
GETESTAE MVI   MSGID,C'4'                                               00287000
*                                   /* SAVE REGISTERS REQUIRED       */ 00288000
*                                   /* IN STAE EXIT                  */ 00289000
         STM   R9,R13,REGSTAE                                           00290000
*          0(1:ESTAELEN)=ELIST(1:ESTAELEN);/* MOVE ESTAE LIST FORM      00291000
*                                      INTO WORKAREA                 */ 00292000
         MVC   STAELIST(ESTAELEN),ELIST                                 00293000
*   R3=ADDR(STAE000);               /* GET STAE EXIT ROUTINE ADDR    */ 00294000
         LA    R3,STAE000                                               00295000
*   R2=ADDR(STAEPARM);              /* GET PARM LIST ADDR            */ 00296000
         LA    R2,STAEPARM                                              00297000
*                                                                       00298000
         ESTAE 0                                                        00299000
*                                                                       00300000
         SLR   R15,R15                                                  00301000
         B     @ER00002                                                 00302000
*                                                                       00303000
         ESTAE (3),CT,RECORD=YES,PARAM=(2),MF=(E,STAELIST)              00304000
*                                                                       00305000
*   END;                                                                00306000
@ER00002 BR    R14                     RETURN TO CALLER WITH RC IN R15  00307000
*                                                                       00308000
*   /*****************************************************************/ 00309000
*   /*                                                               */ 00310000
*   /* CHECKMSG                                                      */ 00311000
*   /*                                                               */ 00312000
*   /* CHECK IF THE MESSAGE EXCEEDS 126 CHARACTERS                   */ 00313000
*   /* IF IT DOES THE MESSAGE WILL BE BROKEN UP INTO 126 OR LESS     */ 00314000
*   /* SEGMENTS DEPENDING ON WHERE THE LAST FULL WORD ENDS. THE      */ 00315000
*   /* MESSAGE WILL BE ISSUED SEGMENT BY SEGMENT UNTIL THE ENTIRE    */ 00316000
*   /* MESSAGE HAS BE PUT TO THE SYSTEM MESSAGE DATA SET             */ 00317000
*   /*                                                               */ 00318000
*   /* ON ENTRY -                                                    */ 00319000
*   /* R3 -> MESSAGE TEXT                                            */ 00320000
*   /* R8  = L'MESSAGE TEXT                                          */ 00321000
*   /*                                                               */ 00322000
*   /* ON EXIT                                                       */ 00323000
*   /* R7 -> MESSAGE TEXT                                            */ 00324000
*   /* R8  = L'MESSAGE TEXT                                          */ 00325000
*   /*                                                               */ 00326000
*   /*****************************************************************/ 00327000
*                                                                       00328000
*CHECKMSG:                                                              00329000
*   R7=R3;                          /* R7 -> MSG                     */ 00330000
CHECKMSG LR    R7,R3                                                    00331000
*   IF R8>126 THEN                  /* L'MSG > 126 THEN DO           */ 00332000
         CH    R8,KH126                                                 00333000
         BNH   @RF00218                                                 00334000
*     DO;                                                               00335000
         LA    R2,125(,R7)             R2 -> 126TH BYTE OF MESSAGE TEXT 00336000
*       DO WHILE(R2->MESSAGE(1)^=' '&R2^=R7);/* SCAN FOR BLANK          00337000
*                                      CHAR OR BEGINNING ADDR OF MSG */ 00338000
         B     @DE00221                                                 00339000
*                                                                       00340000
*         R2=R2-1;                  /* DECREMENT R2 FOR BACKWARD SCAN*/ 00341000
@DL00221 BCTR  R2,0                                                     00342000
*       END;                                                            00343000
@DE00221 CLI   0(R2),C' '              FOUND A BLANK ?                  00344000
         BE    @DC00221                YES, BRANCH                      00345000
         CR    R2,R7                   NO, R2 -> FIRST BYTE ?           00346000
         BNE   @DL00221                NO, CONTINUE TO SCAN BACKWARDS   00347000
*       IF R2=R7 THEN               /* CK IF SCAN WAS TERMINATED        00348000
*                                      BECAUSE BLANK WASNT FOUND     */ 00349000
@DC00221 CR    R2,R7                   R2 -> FIRST BYTE ?               00350000
         BNE   @RF00224                NO, BRANCH                       00351000
*         R2=R7+125;                /* YES, THEN POINT R2 AT THE        00352000
*                                      126TH BYTE OF MSG             */ 00353000
         LA    R2,125(,R7)                                              00354000
*       R3=R2+1;                    /* ELSE R3 POINTS TO CHAR           00355000
@RF00224 LA    R3,1(,R2)               R3 -> NEXT CHAR TO BE PROCESSED  00356000
         LR    R5,R2                                                    00357000
         SLR   R5,R7                   SUBTRACT START ADDR FROM LAST    00358000
*                                      CHAR + 1                         00359000
         LA    R2,1(,R5)               CALC PROCESSED LENGTH            00360000
         SLR   R8,R2                   SUBTRACT PROCESSED LENGTH        00361000
*                                      FROM TOTAL LENGTH OF MESSAGE     00362000
*       R2=R2-R7+1;                 /* COMPUTE LENGTH TO PUT OUT     */ 00363000
         B     @ER00003                                                 00364000
*                                                                       00365000
*     END;                                                              00366000
*   ELSE                                                                00367000
*     DO;                                                               00368000
*       R2=R8;                      /* IF LENGTH <126 THEN PUT LENGTH   00369000
*                                      INTO R2                       */ 00370000
@RF00218 LR    R2,R8                                                    00371000
*       R8=0;                       /* NO MORE MSG SEGMENTS LEFT PUT    00372000
*                                      ZERO LENGTH INTO R8           */ 00373000
         SLR   R8,R8                                                    00374000
*     END;                                                              00375000
*   END;                                                                00376000
@ER00003 BR    R14                                                      00377000
*                                                                       00378000
*   /*****************************************************************/ 00379000
*   /*                                                               */ 00380000
*   /* CKROUTCD                                                      */ 00381000
*   /*                                                               */ 00382000
*   /* THE MESSAGE IS CHECKED FOR ROUTE CODES OTHER THAN 11. IF THEY */ 00383000
*   /* EXIST THIS MODULE RETURNS TO WTO WITHOUT TURNING ON XVD0USER  */ 00384000
*   /* (RETURN TO USER BIT IN XSA). IF NO OTHER ROUTE CODES EXIST    */ 00385000
*   /* THEN THE UCM TABLE IS SCANNED TO DETERMINE IF ANY CONSOLES ARE*/ 00386000
*   /* TO RECEIVE ROUTE CODE 11 MESSAGES IF SO THEN THIS MODULE      */ 00387000
*   /* RETURNS TO WTO WITHOUT TURNING ON XVD0USER BIT                */ 00388000
*   /*                                                               */ 00389000
*   /*****************************************************************/ 00390000
*                                                                       00391000
*CKROUTCD:                                                              00392000
*   IF XVRCSAVE='0020'X THEN        /* OTHER ROUTE CODES WITH MSG ?  */ 00393000
CKROUTCD CLC   WKAROC,WTPONLY          YES, BRANCH                      00394000
         BNE   @RF00237                                                 00395000
*     DO;                           /* NO, CHECK CONSOLES FOR ANY       00396000
*                                      THAT RECEIVE ROUTE CODE 11 MSG*/ 00397000
         L     R15,ADDRUCM                                              00398000
         USING UCM,R15                                                  00399000
*       R7=UCMVEA;                  /* -> FIRST UCM                  */ 00400000
         L     R7,UCMVEA                                                00401000
*       R8=UCMVEZ;                  /* L'EACH UCM                    */ 00402000
         L     R8,UCMVEZ                                                00403000
*       R3=UCMVEL;                  /* -> LAST UCM                   */ 00404000
         L     R3,UCMVEL                                                00405000
         DROP  R15                                                      00406000
*       DO WHILE(WTPROUTE^='1'B&R7<R3);/* SCAN ALL UCMS FOR ANY         00407000
*                                      THAT RECEIVE ROUTE CODE 11       00408000
*                                      MSGS                          */ 00409000
         B     @DE00242                                                 00410000
*                                                                       00411000
*         R7=R7+R8;                 /* INCREMENT R7 TO NEXT UCM      */ 00412000
@DL00242 LA    R7,0(R7,R8)                                              00413000
*       END;                                                            00414000
         USING UCMLIST,R7              ADDRESSABILITY FOR UCM DEVICE    00415000
@DE00242 TM    UCMRTCD+1,WPLROUTK      WTP ROUTE CODE 11 FOR THIS CON ? 00416000
         BO    @DC00242                                                 00417000
         CR    R7,R3                                                    00418000
         BL    @DL00242                                                 00419000
*       IF WTPROUTE='1'B THEN       /* ANY CONSOLES RECEIVE CODE 11  */ 00420000
@DC00242 TM    UCMRTCD+1,WPLROUTK                                       00421000
         BNO   @RF00245                                                 00422000
         DROP  R7                                                       00423000
*         R15=0;                    /* YES, THEN RC = RETURN TO WTO  */ 00424000
         SLR   R15,R15                                                  00425000
         B     @ER00004                                                 00426000
*                                                                       00427000
*       ELSE                                                            00428000
*         R15=4;                    /* NO, RC = RETURN TO USER       */ 00429000
@RF00245 LA    R15,4                                                    00430000
         B     @ER00004                                                 00431000
*                                                                       00432000
*     END;                                                              00433000
*   ELSE                                                                00434000
*     R15=0;                        /* OTHER ROUTE CODES RC = RETURN    00435000
*                                      TO WTO                        */ 00436000
@RF00237 SLR   R15,R15                                                  00437000
*   END;                                                                00438000
@ER00004 BR    R14                                                      00439000
*                                                                       00440000
*   /*****************************************************************/ 00441000
*   /*                                                               */ 00442000
*   /* ISSUTPUT                                                      */ 00443000
*   /*                                                               */ 00444000
*   /* A CHECK IS MADE TO SEE IF THE TSO USER WANTS WTP MESSAGES     */ 00445000
*   /* SENT TO THE TERMINAL. IF YES THEN THE TPUT MACRO IS ISSUED TO */ 00446000
*   /* THE TERMINAL. THE RC FROM TPUT IS PASSED BACK TO MAINLINE     */ 00447000
*   /* CODE. IF THE USER DOES NOT WANT WTP MESSAGES,     RETURN TO   */ 00448000
*   /* MAINLINE WITH A RC OF 4 IN R15                                */ 00449000
*   /*                                                               */ 00450000
*   /* ON RENTRY -                                                   */ 00451000
*   /* R2 -> ACTIVE JSCB                                             */ 00452000
*   /*                                                               */ 00453000
*   /*****************************************************************/ 00454000
*                                                                       00455000
*ISSUTPUT:                                                              00456000
*   IF JSCBPSCB^=0&UPTWTP='1'B THEN /* TSO USER WANT WTP MSGS ?      */ 00457000
         USING IEZJSCB,R2                                               00458000
ISSUTPUT ICM   R1,B'1111',JSCBPSCB     R1 -> TSO PROTECTED STEP CNTL    00459000
         BZ    @RF00252                PRESENT ? ,NO, NOT TSO CALLER    00460000
         DROP  R2                                                       00461000
         USING PSCB,R1                 YES                              00462000
         L     R1,PSCBUPT              R1 -> USER PROFILE TABLE         00463000
         DROP  R1                                                       00464000
         USING UPT,R1                                                   00465000
         TM    UPTSWS,UPTWTP           WTP MSGS DESIRED ?               00466000
         DROP  R1                                                       00467000
         BNO   @RF00252                NO, BRANCH                       00468000
*     DO;                           /* YES, ISSUE TPUT               */ 00469000
*       R1=ADDR(WPLTXT);            /* R1 -> MCS + MESSAGE TEXT      */ 00470000
         L     R1,WKAADTXT             @@ CHANGED FROM LA 22 MAY 19 @@  00471000
         LA    R1,4(,R1)               R1 -> MSG TEXT  @@ 22 MAY 19 @@  00472000
*                                                                       00473000
*       R0=XVMSGLGH-4;              /* R0 = L'MCS + MESSAGE          */ 00474000
         LH    R0,WKALGH               R0 = L'MESSAGE                   00475000
         SH    R0,KH4                                                   00476000
*                                   /* ISSUE TPUT TO TERMINAL        */ 00477000
         TPUT  (1),(0),R                                                00478000
*                                                                       00479000
         B     @ER00005                                                 00480000
*                                                                       00481000
*     END;                                                              00482000
*   ELSE                                                                00483000
*     R15=4;                        /* PUT TO JOB'S MSG DATA SET     */ 00484000
@RF00252 LA    R15,4                                                    00485000
*   END;                                                                00486000
@ER00005 BR    R14                     RETURN TO CALLER WITH RC IN R15  00487000
*                                                                       00488000
*   /*****************************************************************/ 00489000
*   /*                                                               */ 00490000
*   /* CHECKJOB                                                      */ 00491000
*   /*                                                               */ 00492000
*   /* IF THIS IS THE FIRST TIME A WTP IS ISSUED BY THIS JOB STEP    */ 00493000
*   /* THEN THE JSCB MUST BE INITIALIZED. IF AN OLD STEP THAT HAS    */ 00494000
*   /* ISSUED PREVIOUS WTPS THEN RETURN TO MAINLINE CODE             */ 00495000
*   /*                                                               */ 00496000
*   /*****************************************************************/ 00497000
*                                                                       00498000
*CHECKJOB:                                                              00499000
*   IF JSCBWTSP^=JSCBSTEP THEN      /* JOB BEEN HERE BEFORE ?        */ 00500000
CHECKJOB L     R15,ACTJSCB                                              00501000
         USING IEZJSCB,R15                                              00502000
         CLC   JSCBWTSP,JSCBSTEP                                        00503000
         BE    @ER00006                YES, BRANCH                      00504000
*     DO;                           /* NO, THEN INITIALIZE JSCB      */ 00505000
*       JSCBWTP=0;                  /* ZERO OUT SWITCHES             */ 00506000
         XC    JSCBWTP,JSCBWTP                                          00507000
*       JSCBWTSP=JSCBSTEP;          /* MOVE STEP NUM INTO JSCB       */ 00508000
         MVC   JSCBWTSP,JSCBSTEP                                        00509000
         DROP  R15                                                      00510000
*     END;                                                              00511000
*   END;                                                                00512000
@ER00006 BR    R14                     RETURN TO CALLER                 00513000
*                                                                       00514000
*   /*****************************************************************/ 00515000
*   /*                                                               */ 00516000
*   /* BUILDRPL                                                      */ 00517000
*   /*                                                               */ 00518000
*   /* THIS ROUTINE OBTAINS THE RPL ADDRESS FROM THE JSCB. THE       */ 00519000
*   /* REQUIRED FIELDS WITHIN THE RPL ARE INITIALIZED                */ 00520000
*   /*                                                               */ 00521000
*   /* ON ENTRY -                                                    */ 00522000
*   /* R7 -> MESSAGE TEXT                                            */ 00523000
*   /* R2  = L'MESSAGE TEXT                                          */ 00524000
*   /*                                                               */ 00525000
*   /* ON EXIT                                                       */ 00526000
*   /* R1 -> RPL                                                     */ 00527000
*   /*                                                               */ 00528000
*   /*****************************************************************/ 00529000
*                                                                       00530000
*BUILDRPL:                                                              00531000
BUILDRPL L     R15,ACTJSCB                                              00532000
         USING IEZJSCB,R15                                              00533000
*   R1=JSCBSMLR;                    /* GET PTR TO RPL                */ 00534000
         ICM   R1,B'1111',JSCBSMLR     R1 -> SYSTEM MESSAGE DATA SET    00535000
         BZ    @ER00007                SYSTEM MESSAGE DATA SET AVAIL ?  00536000
         DROP  R15                                                      00537000
         USING IFGRPL,R1                                                00538000
*   RPLAREA=R7;                     /* MSG LOCATION INTO RPL         */ 00539000
         ST    R7,RPLAREA                                               00540000
*   RPLRLEN=R2;                     /* MSG LENGTH INTO RPL           */ 00541000
         ST    R2,RPLRLEN                                               00542000
*   RPLREQ=RPLPUT;                  /* INDICATE PUT REQUEST          */ 00543000
         MVI   RPLREQ,RPLPUT                                            00544000
*   RPLSEQ='1'B;                    /* PUT AFTER LAST MSG IN SYSTEM     00545000
*                                      MSG DATA SET                  */ 00546000
         OI    RPLOPTCD,RPLSEQ                                          00547000
         DROP  R1                                                       00548000
*   RETURN;                                                             00549000
@ER00007 BR    R14                                                      00550000
*   END;                                                                00551000
*                                                                       00552000
*   /*****************************************************************/ 00553000
*   /*                                                               */ 00554000
*   /* THIS ROUTINE CHECKS THE RETURN CODES FROM HASP. IF REG 15 IS  */ 00555000
*   /* NOT 0 THEN AN ERROR OCCURRED. REG 8 IS ZEROED TO TERMINATE ANY*/ 00556000
*   /* FURTHER PROCESSING OF MESSAGES AND THE ERROR MSG ID FIELD IS  */ 00557000
*   /* CHANGED TO 3 TO INDICATE A PUT ERROR IN THE WTO MSG.          */ 00558000
*   /*                                                               */ 00559000
*   /*****************************************************************/ 00560000
*                                                                       00561000
*CKRETURN:                                                              00562000
*   IF R15^=0 THEN                  /* IF PROBLEMS OCCURRED          */ 00563000
CKRETURN LTR   R15,R15                                                  00564000
         BZ    @ER00008                                                 00565000
*     DO;                                                               00566000
*       R8=0;                       /* ZERO OUT R8 TO TERMINATE DO      00567000
*                                      LOOP                          */ 00568000
         SLR   R8,R8                                                    00569000
*       MSGID='3';                  /* SET MSG ID TO INDICATE A PUT     00570000
*                                      FAILURE                       */ 00571000
         MVI   MSGID,C'3'                                               00572000
*     END;                                                              00573000
*   RETURN;                                                             00574000
@ER00008 BR    R14                                                      00575000
*   END;                                                                00576000
*                                                                       00577000
*   /*****************************************************************/ 00578000
*   /*                                                               */ 00579000
*   /* ISSUEENQ                                                      */ 00580000
*   /*                                                               */ 00581000
*   /* THIS ROUTINE CHECKS FOR A ZERO RPL POINTER. IF ZERO THE ENQ IS*/ 00582000
*   /* NOT ISSUED AND THE ERROR MSG ID IS SET TO 1 TO INDICATE A ZERO*/ 00583000
*   /* RPL POINTER ERROR IN THE WTO MESSAGE TO HARD COPY. IF AN RPL  */ 00584000
*   /* POINTER IS PRESENT THIS ROUTINE MOVES THE ENQUEUE PARM LIST   */ 00585000
*   /* INTO THIS MODULES WORKAREA. IT THEN BUILDS AN RNAME CONSISTING*/ 00586000
*   /* OF THE RPL POINTER (JSCBSMLR) AND THE AND THE ASCBASID.IT THEN*/ 00587000
*   /* ISSUES A UNCONDITIONAL ENQUEUE                                */ 00588000
*   /*                                                               */ 00589000
*   /*****************************************************************/ 00590000
*                                                                       00591000
*ISSUEENQ:                                                              00592000
*   R1=ADDR(WORKAREA);              /* WORKAREA ADDR INTO R1         */ 00593000
ISSUEENQ LA    R1,WORKAREA                                              00594000
*   ENQAREA(1:LENENQ)=ENQLIST(1:LENENQ);/* MOVE ENQ LIST INTO WORK   */ 00595000
         MVC   WORKAREA(LENENQ),ENQLIST                                 00596000
         L     R15,ACTJSCB             R15 -> ACTIVE JSCB               00597000
         USING IEZJSCB,R15                                              00598000
*   IF JSCBSMLR^=0 THEN             /* RPL AVAILABLE FOR SYSTEM MSG ?*/ 00599000
         ICM   R15,B'1111',JSCBSMLR                                     00600000
         BZ    @RF00293                NO, BRANCH                       00601000
         DROP  R15                                                      00602000
*     DO;                                                               00603000
*       ENQRNAME(1:4)=JSCBSMLR;     /* YES,BUILD RNAME WITH RPL PTR  */ 00604000
         ST    R15,ENQRNAME                                             00605000
*       ENQRNAME(5:6)=ASCBASID;     /* AND ASID FROM ASCB            */ 00606000
         MVC   ENQRNAME+4(2),ASCBASID-ASCB(R7)                          00607000
*       RNAMEFLD=ADDR(ENQRNAME);    /* PUT RNAME ADDR INTO LIST      */ 00608000
         LA    R15,ENQRNAME                                             00609000
         ST    R15,8(,R1)                                               00610000
*                                   /* ISSUE UNCONDITIONAL ENQ       */ 00611000
         ENQ   ,MF=(E,(1))                                              00612000
*                                                                       00613000
         LTR   R15,R15                 TEST RETURN CODE FROM ENQ        00614000
         BZ    @ER00009                ZERO, ALL OK                     00615000
         CLI   3(R15),8                RETURN CODE 8 ?                  00616000
         BE    IGC0032A                                                 00617000
         CLI   3(R15),20               RETURN CODE 20 ?                 00618000
         BNE   @ER00009                                                 00619000
IGC0032A MVI   XVRETCOD,48                                              00620000
         MVI   MSGID,C'2'                                               00621000
         B     @ER00009                                                 00622000
*                                                                       00623000
*     END;                                                              00624000
*   ELSE                                                                00625000
*     DO;                           /* NO RPL PTR MEANS NO ENQ       */ 00626000
*       R15=4;                      /* RC INDICATES ENQ WOULD FAIL   */ 00627000
@RF00293 LA    R15,4                                                    00628000
*       MSGID='1';                  /* INDICATES WHY ENQ FAILED      */ 00629000
         MVI   MSGID,C'1'                                               00630000
*     END;                                                              00631000
*   END;                                                                00632000
@ER00009 BR    R14                                                      00633000
*                                                                       00634000
*   /*****************************************************************/ 00635000
*   /*                                                               */ 00636000
*   /* BUILDMSG                                                      */ 00637000
*   /*                                                               */ 00638000
*   /* GETS 53 BYTES OF THE MESSAGE THAT WAS PASSED TO WTP           */ 00639000
*   /* THE LENGTH WILL BE ADJUSTED TO A MAXIMUM OF 53 BYTES          */ 00640000
*   /*                                                               */ 00641000
*   /* ON EXIT -                                                     */ 00642000
*   /* R3 -> MESSAGE TEXT                                            */ 00643000
*   /* R6  = L'MESSAGE TEXT                                          */ 00644000
*   /*                                                               */ 00645000
*   /*****************************************************************/ 00646000
*                                                                       00647000
*BUILDMSG:                                                              00648000
*   R3=ADDR(WPLTXT);                /* R3 -> MCS + MESSAGE TEXT      */ 00649000
BUILDMSG L     R3,WKAADTXT             R3 -> MESSAGE TEXT               00650000
         LA    R3,4(,R3)                                                00651000
*   R6=XVMSGLGH-4;                  /* GET MSG LENGTH                */ 00652000
         LH    R6,WKALGH                                                00653000
         SH    R6,KH4                  SUBTRACT FOR MCS OVERHEAD        00654000
*   IF R6>53 THEN                   /* MSG EXCEED MAX LENGTH            00655000
*                                      THAT WE CAN PUT OUT           */ 00656000
         LA    R15,53                                                   00657000
         CR    R6,R15                  L'TEXT > 53 ?                    00658000
         BNH   @ER00011                NO, BRANCH TO EXIT               00659000
*     DO;                                                               00660000
         LR    R6,R15                  TRUNCATE TO 53 BYTES             00661000
*   END;                                                                00662000
@ER00011 BR    R14                                                      00663000
*                                                                       00664000
*   /*****************************************************************/ 00665000
*   /*                                                               */ 00666000
*   /* ISSUEMSG                                                      */ 00667000
*   /*                                                               */ 00668000
*   /* THIS ROUTINE MOVES THE LIST FORM OF THE MSG TO WTP WORKAREA   */ 00669000
*   /* AND THEN MOVES IN THE 55 BYTES FROM THE MSG SENT TO WTP BEFORE*/ 00670000
*   /* THE ABEND OCCURRED. IT THEN ISSUES THE WTO                    */ 00671000
*   /*                                                               */ 00672000
*   /* ON ENTRY -                                                    */ 00673000
*   /* R3 -> MESSAGE TEXT                                            */ 00674000
*   /* R6  = L'MESSAGE TEXT (MAX 53 BYTES)                           */ 00675000
*   /*                                                               */ 00676000
*   /*                                                               */ 00677000
*   /*****************************************************************/ 00678000
*                                                                       00679000
*ISSUEMSG:                                                              00680000
ISSUEMSG MVC   WORKAREA(IEF170IL),IEF170I                               00681000
*   LENGTH=R6+L'IEF170I;            /* UPDATE LENGTH                 */ 00682000
         LR    R15,R6                                                   00683000
         AH    R15,WORKAREA            ADD L'IEF170I MESSAGE            00684000
         STH   R15,WORKAREA            UPDATE L'WTO MESSAGE             00685000
*   MSGID2(1:R6)=R3->MESSAGE(1:R6);/* MOVE MSG TO WORKAREA           */ 00686000
         LR    R15,R6                                                   00687000
         BCTR  R15,0                                                    00688000
         EX    R15,@SM02290            MVC   WORKAREA+23(0),0(R3)       00689000
*   R7=ADDR(MSGID2)+R6;             /* R7 -> END OF MESSAGE          */ 00690000
         LA    R7,WORKAREA+23(R6)                                       00691000
*   R7->MESSAGE=IEF170I+23;         /* MOVE IN DESC AND ROUTE CODES  */ 00692000
         MVC   0(4,R7),IEF170I+23                                       00693000
*   IF TCBTIO^=0 THEN               /* TIOT EXIST ?                  */ 00694000
         L     R15,TCBADDR                                              00695000
         USING TCB,R15                                                  00696000
         ICM   R15,B'1111',TCBTIO      HAVE TIOT ADDR ?                 00697000
         BZ    @RF00337                NO, BRANCH                       00698000
         DROP  R15                                                      00699000
*     JOBNAME=TIOCNJOB;             /* YES, MOVE JOBNAME TO MSG      */ 00700000
         USING TIOT,R15                                                 00701000
         MVC   WORKAREA+14(L'TIOCNJOB),TIOCNJOB                         00702000
         DROP  R15                                                      00703000
*   IDENT=MSGID;                    /* INDICATE WHY MSG OCCURRED     */ 00704000
@RF00337 MVC   WORKAREA+12(L'MSGID),MSGID                               00705000
*                                   /* ISSUE WTO                     */ 00706000
         WTO   MF=(E,WORKAREA)                                          00707000
*   END;                                                                00708000
@ER00012 BR    R14                                                      00709000
*                                                                       00710000
*   /*****************************************************************/ 00711000
*   /*                                                               */ 00712000
*   /* THIS ROUTINE IS CALLED BY THE STAE EXIT ROUTINE TO DEQUEUE ANY*/ 00713000
*   /* RESOURCES THAT MAY HAVE BEEN ENQUEUED ON                      */ 00714000
*   /*                                                               */ 00715000
*   /*****************************************************************/ 00716000
*                                                                       00717000
*ISSUEDEQ:                                                              00718000
*   R1=ADDR(WORKAREA);                                                  00719000
ISSUEDEQ LA    R1,WORKAREA             R1 -> WORKAREA                   00720000
*   WORKAREA(1:LENDEQ)=DEQLIST(1:LENDEQ);/* MOVE IN LIST FORM OF DEQ */ 00721000
         MVC   WORKAREA(LENDEQ),DEQLIST                                 00722000
*   RNAMEFLD=ADDR(ENQRNAME);        /* GET ADDR OF RNAME INTO LIST   */ 00723000
         LA    R15,ENQRNAME                                             00724000
         ST    R15,8(,R1)                                               00725000
*                                   /* ISSUE CONDITIONAL DEQ         */ 00726000
         DEQ   ,MF=(E,(1))                                              00727000
*   END;                                                                00728000
@ER00013 BR    R14                                                      00729000
*                                                                       00730000
*   /*****************************************************************/ 00731000
*   /*                                                               */ 00732000
*   /* CKMCSFLG                                                      */ 00733000
*   /*                                                               */ 00734000
*   /* CHECK THE MCS FLAGS IN THE WPL. FLAGS                         */ 00735000
*   /* WPLMCSFB,WPLMCSFD, AND WPLMCSFH ARE CHECKED. ADDITIONALLY THE */ 00736000
*   /* WTOR BIT IN CXSA (XVD2WTOR) IS                                */ 00737000
*   /* CHECKED TO SEE IF THIS WPL IS FOR A WTOR                      */ 00738000
*   /*                                                               */ 00739000
*   /* IF ANY OF THESE ARE ON MORE PROCESSING IS TO BE DONE BY WTO.  */ 00740000
*   /* IF THESE FLAGS ARE NOT ON, A CHECK IS MADE TO SEE IF THIS WTP */ 00741000
*   /* SHOULD BE HARD COPIED. IF YES THEN WTO IS THUSLY INFORMED.    */ 00742000
*   /*                                                               */ 00743000
*   /*****************************************************************/ 00744000
*                                                                       00745000
*CKMCSFLG:                                                              00746000
*   IF WPLMCSFB^='1'B&WPLMCSFD^='1'B&WPLMCSFH^='1'B&XVD2WTOR^='1'B      00747000
CKMCSFLG TM    WKAMCSF,WPLMCSFB+WPLMCSFD+WPLMCSFH                       00748000
         BNZ   @ER00014                                                 00749000
         TM    XVD2,XVD2WTOR                                            00750000
         BO    @ER00014                                                 00751000
*     DO;                           /* IF ALL FLAGS ARE OFF THEN        00752000
*                                      CHECK IF WTP ARE GOING TO HARD   00753000
*                                      COPY                          */ 00754000
*       IF UCMSYSG='1'B|UCMHCUCM^=0 THEN/* CHECK IF HARD COPY EXIST  */ 00755000
         L     R15,ADDRUCM                                              00756000
         SH    R15,KH4                                                  00757000
         L     R15,UCMPRFXP-UCMPRFXP(,R15)    -> UCM PREFIX             00758000
         USING UCMPRFX,R15                                              00759000
         TM    UCMSFLG1,UCMSYSG        HARD COPY DEVICE IS SYSLOG ?     00760000
         BO    @RT00352                YES, BRANCH                      00761000
         ICM   R0,B'1111',UCMHCUCM     -> HARD COPY UCM ENTRY           00762000
*                                      HARD COPY UCM ENTRY PRESENT ?    00763000
         BZ    @RF00352                NO, BRANCH                       00764000
*         DO;                                                           00765000
*           IF RTCODE11='1'B THEN   /* ARE WTP MSGS BEING HARD COPIED*/ 00766000
@RT00352 TM    UCMHRDRT+1,WPLROUTK     ROUTE CODE 11 ?                  00767000
         BZ    @RF00354                NO, BRANCH                       00768000
         DROP  R15                                                      00769000
*             XVD0HDCY='1'B;        /* YES, THEN TELL WTO            */ 00770000
         OI    XVD0,XVD0HDCY                                            00771000
         B     @ER00014                                                 00772000
*                                                                       00773000
*           ELSE                                                        00774000
*             XVD0USER='1'B;        /* NO FLAGS OR NO HARD TELL WTO     00775000
*                                      TO RETURN TO RETURN TO USER   */ 00776000
@RF00354 OI    XVD0,XVD0USER                                            00777000
         B     @ER00014                                                 00778000
*                                                                       00779000
*         END;                                                          00780000
*       ELSE                                                            00781000
*         XVD0USER='1'B;            /* OTHERWISE TELL WTO TO            00782000
*                                      RETURN TO USER                */ 00783000
@RF00352 OI    XVD0,XVD0USER                                            00784000
*     END;                                                              00785000
*   END;                                                                00786000
@ER00014 BR    R14                                                      00787000
*                                                                       00788000
*   /*****************************************************************/ 00789000
*   /*                                                               */ 00790000
*   /* FINALCK                                                       */ 00791000
*   /*                                                               */ 00792000
*   /* CHECK IF ANY CONSOLES RECEIVE ROUTE CODE 11 MESSAGES          */ 00793000
*   /* IF MCS FLAGS B,D,H, ARE ON OR IF ROUTE CODE 11                */ 00794000
*   /* MESSAGES ARE HARDCOPIED. IF ANY OF THE ABOVE CHECKS ARE       */ 00795000
*   /* POSITIVE WTO IS FLAGGED FOR ADDITIONAL PROCESSING. OTHERWISE  */ 00796000
*   /* WTO RETURNS TO CALLER                                         */ 00797000
*   /*                                                               */ 00798000
*   /*****************************************************************/ 00799000
*                                                                       00800000
*FINALCK:                                                               00801000
*   REG14SAV=R14;                   /* SAVE MAINLINE RETURN ADDR     */ 00802000
FINALCK  ST    R14,REG14SAV                                             00803000
*   CALL CKROUTCD;                  /* CK FOR OTHER ROUTE CODES ON      00804000
*                                      MSG OR FOR CONSOLES THAT         00805000
*                                      RECEIVE ROUTE CODE 11 MSGS    */ 00806000
         BAL   R14,CKROUTCD                                             00807000
*   IF R15=4 THEN                   /* IF NONE THEN CK WPL FLAGS     */ 00808000
         CH    R15,KH4                                                  00809000
         BNE   @RF00364                                                 00810000
*     CALL CKMCSFLG;                /* GO CK MCS FLAGS AND HARDCOPY  */ 00811000
         BAL   R14,CKMCSFLG                                             00812000
*   R14=REG14SAV;                   /* RESTORE RETURN ADDR           */ 00813000
@RF00364 L     R14,REG14SAV                                             00814000
*   END;                                                                00815000
@ER00015 BR    R14                                                      00816000
*                                                                       00817000
*********************************************************************** 00818000
*                                                                       00819000
*        STAE EXIT ROUTINE                                              00820000
*                                                                       00821000
*        RECEIVES CONTROL ONLY IF AN ABEND SITUATION OCCURS.            00822000
*        WHEN ENTERED IT RESTORES THIS MODULES BASE REGISTER, XSA       00823000
*        POINTER AND DATA REG                                           00824000
*        53 BYTES OF THE MESSAGE PASSED TO WTP ARE MOVED INTO A         00825000
*        WTO AND ISSUED TO THE HARD COPY                                00826000
*        IF R0 = 0 THEN THE SETRP MACRO IS ISSUED WITH                  00827000
*        RETURN ADDR OF STAERETY                                        00828000
*        IF R0 IS NOT 0 THEN A RC OF 4 IS PLACED INTO R15, THE          00829000
*        RETRY ADDR INTO R0 AND A BR 14 TO STAE ISSUED                  00830000
*                                                                       00831000
*********************************************************************** 00832000
*                                                                       00833000
*STAE000:                                                               00834000
         USING SDWA,R1                                                  00835000
         DROP  R11                     REMOVE R11 AS BASE REG           00836000
         USING STAE000,R15             MAKE R15 BASE REG                00837000
*   R8=R14;                         /* SAVE RETURN ADDRESS IN R8     */ 00838000
STAE000  LR    R8,R14                                                   00839000
*   R11=12;                         /* NEED A 12 FOR COMPARE         */ 00840000
         LA    R11,12                                                   00841000
         CR    R0,R11                  CHECK R0 FOR A 12 RETURN CODE    00842000
         BE    NOSDWA                  IF 12 THEN                       00843000
*                                      R2 -> USER PARAMETER LIST        00844000
         L     R2,SDWAPARM             ELSE OBTAIN USER                 00845000
*                                      PARAMETER LIST FROM THE SWDA     00846000
         LR    R5,R1                   R5 -> SDWA                       00847000
         L     R1,SDWAABCC             GET ABEND COMPLETION CODE        00848000
         DROP  R1                                                       00849000
NOSDWA   SLL   R1,8                    REMOVE HIGH ORDER GARBAGE        00850000
         SRL   R1,20                   SHIFT SYSTEM ABEND CODE          00851000
*                                      INTO LOW ORDER 12 BITS           00852000
*                                                                       00853000
*        RESTORE NEEDED REGISTERS FROM SDWAPARM AREA                    00854000
*                                                                       00855000
         LM    R9,R13,0(R2)                                             00856000
         DROP  R15                     REMOVE R15 AS BASE REG           00857000
         USING IGC0203E,R11            RESTORE R11 AS BASE REG          00858000
         ST    R0,STAEIND                                               00859000
*   CALL ISSUEDEQ;                  /* REMOVE POSSIBLE ENQ           */ 00860000
@RF00378 BAL   R14,ISSUEDEQ                                             00861000
*   CALL BUILDMSG;                  /* SET UP TO ISSUE 53 BYTE ERROR    00862000
*                                      MSG                           */ 00863000
         BAL   R14,BUILDMSG                                             00864000
*   CALL ISSUEMSG;                  /* ISSUE WTO FOR ERROR MSG       */ 00865000
         BAL   R14,ISSUEMSG                                             00866000
*   R1=R5;                          /* RESTORE SDWA POINTER          */ 00867000
         LR    R1,R5                                                    00868000
         USING SDWA,R1                                                  00869000
*   R5=ADDR(STAERETY);              /* GET RETRY ADDRESS             */ 00870000
         LA    R5,STAERETY                                              00871000
*   R14=R8;                         /* RESTORE RETURN ADDR           */ 00872000
         LR    R14,R8                                                   00873000
*   IF STAEIND = 12 THEN            /* CHECK IF SDWA EXIST           */ 00874000
         CLC   STAEIND,KF12                                             00875000
         BNE   @RF00386                                                 00876000
*     DO;                           /* NO, THEN DO THE FOLLOWING     */ 00877000
*       R15=4;                      /* TELL STAE TO RETRY            */ 00878000
         LA    R15,4                                                    00879000
*       R0=R5;                      /* PUT RETRY ADDR INTO R0        */ 00880000
         LR    R0,R5                                                    00881000
         B     @ER00016                                                 00882000
*                                                                       00883000
*     END;                                                              00884000
*   ELSE                                                                00885000
*     DO;                           /* SDWA EXIST CONTINUE HERE      */ 00886000
*       SDWASR01=R2;                /* SAVE MY PARM ADDR IN SDWA     */ 00887000
@RF00386 ST    R2,SDWASR01                                              00888000
*       DO;                         /* SETRP RECORD(YES)RETADDR(STAER   00889000
*                                      ETY)RECPARM(LRECINF)RC(4)RETRE   00890000
*                                      GS(YES)FRESDWA(YES)           */ 00891000
         SETRP RECORD=YES,RETADDR=STAERETY,RECPARM=LRECINF,            X00892000
               RC=4,RETREGS=YES,FRESDWA=YES                             00893000
*                                                                       00894000
*       END;                                                            00895000
*     END;                                                              00896000
*   RETURN;                                                             00897000
@ER00016 BR    R14                                                      00898000
*   END;                                                                00899000
*                                                                       00900000
         DC    0F'0'                                                    00901000
ENQLIST  ENQ   (QN,RN,E,6,SYSTEM),RET=HAVE,MF=L                         00902000
LENENQ   EQU   *-ENQLIST                                                00903000
*                                                                       00904000
ELIST    ESTAE MF=L                                                     00905000
ESTAELEN EQU   *-ELIST                                                  00906000
*                                                                       00907000
DEQLIST  DEQ   (QN,RN,6,SYSTEM),RET=HAVE,MF=L                           00908000
LENDEQ   EQU   *-DEQLIST               L'DEQ LIST                       00909000
*                                                                       00910000
LRECINF  DC    C'IGC0203E'                                              00911000
         DC    C'IGC0203E'                                              00912000
         DC    C'        '                                              00913000
*                                                                       00914000
*   /*****************************************************************/ 00915000
*   /*                                                               */ 00916000
*   /* THIS IS THE MESSAGE SECTION OF THIS MODULE. THE MESSAGE PUT   */ 00917000
*   /* OUT BY THIS MODULE IS CONTAINED HERE AND NOT IN A SEPARATE    */ 00918000
*   /* CSECT. THE MESSAGE IS AN ERROR MESSAGE ISSUED WHEN THERE IS NO*/ 00919000
*   /* RPL POINTER, WHEN A RETURN CODE OF NON ZERO IS RETURNED FROM  */ 00920000
*   /* THE PUT, WHEN THE UNCONDITIONAL ENQUEUE FAILS OR WHEN AN      */ 00921000
*   /* UNPREDICATABLE ABEND OCCURS.                                  */ 00922000
*   /*                                                               */ 00923000
*   /*****************************************************************/ 00924000
*                                                                       00925000
*      THE FIRST BYTE OF THE MESSSAGE IS A REASON CODE FOR WHY THE      00926000
*      MESSAGE WAS ISSUED.                                              00927000
*      1 NO RPL POINTER EXISTED THEREFORE CANNOT ACCESS ACB             00928000
*      2 ENQUEUE TO SERIALIZE EXECUTION OF PUT FAILED                   00929000
*      3 PUT TO SYSTEM MESSAGE DATA SET FAILED                          00930000
*      4 UNPREDICTABLE ABEND                                            00931000
*      THIS IS FOLLOWED BY THE JOBNAME                                  00932000
*                                                                       00933000
IEF170I  WTO   'IEF170I            ',MF=L,ROUTCDE=2,DESC=5,            X00934000
               MCSFLAG=(HRDCPY)                                         00935000
IEF170IL EQU   *-IEF170I           L'WTO                                00936000
*   END                                                                 00937000
*                                                                       00938000
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM      */ 00939000
*/*%INCLUDE SYSLIB  (IHACTM  )                                       */ 00940000
*/*%INCLUDE SYSLIB  (IFGRPL  )                                       */ 00941000
*/*%INCLUDE SYSLIB  (IEECUCM )                                       */ 00942000
*/*%INCLUDE SYSLIB  (IEZWPL  )                                       */ 00943000
*/*%INCLUDE SYSLIB  (IEZJSCB )                                       */ 00944000
*/*%INCLUDE SYSLIB  (IKJTCB  )                                       */ 00945000
*/*%INCLUDE SYSLIB  (IHAASCB )                                       */ 00946000
*/*%INCLUDE SYSLIB  (IEFTIOT1)                                       */ 00947000
*/*%INCLUDE SYSLIB  (IHASDWA )                                       */ 00948000
*/*%INCLUDE SYSLIB  (IHARB   )                                       */ 00949000
*/*%INCLUDE SYSLIB  (IKJRB   )                                       */ 00950000
*/*%INCLUDE SYSLIB  (IKJUPT  )                                       */ 00951000
*/*%INCLUDE SYSLIB  (IKJPSCB )                                       */ 00952000
*                                                                       00953000
KH4      DC    H'4'                                                     00954000
KH10     DC    H'10'                                                    00955000
KH126    DC    H'126'                                                   00956000
KF12     DC    F'12'                                                    00957000
*                                                                       00958000
@SM02286 MVC   WORKAREA(0),ENQLIST                                      00959000
@SM02290 MVC   WORKAREA+23(0),0(R3)                                     00960000
*                                                                       00961000
         DC    0F'0'                                                    00962000
WORKIGCG DC    AL1(231)                SUBPOOL 231                      00963000
         DC    AL3(WORKIGCL)                                            00964000
*                                                                       00965000
WTPONLY  DC    AL2(WPLROUTK)           CHECK FOR ONLY WTP ROUTE CODE    00966000
QN       DC    CL8'SYSZJWTP'                                            00967000
RN       DC    AL1(20)                                                  00968000
*                                                                       00969000
*        WORKAREA GETMAINED BY IGC0203E IN SUBPOOL 231                  00970000
*                                                                       00971000
WORKIGC  DSECT                                                          00972000
SAVEAREA DS    18F                                                      00973000
ADDRUCM  DS    A                                                        00974000
ACTJSCB  DS    A                                                        00975000
STAEIND  DS    A                                                        00976000
TCBADDR  DS    A                                                        00977000
REG6SAVE DS    A                                                        00978000
REG14SAV DS    A                                                        00979000
         DS    0D                                                       00980000
WORKAREA DS    CL132                                                    00981000
*                                                                       00982000
         DS    0F                                                       00983000
STAEPARM DS    CL60                                                     00984000
         ORG   STAEPARM                                                 00985000
REGSTAE  DS    5F                                                       00986000
*                                                                       00987000
         DS    0F                                                       00988000
STAELIST DS    CL16                                                     00989000
ENQRNAME DS    CL8                                                      00990000
MSGID    DS    CL1                                                      00991000
         DS    0D                      ROUND TO DOUBLE WORD             00992000
WORKIGCL EQU   *-WORKIGC                                                00993000
*                                                                       00994000
*        END OF WORKAREA                                                00995000
*                                                                       00996000
         IEZBITS                                                        00997000
*                                                                       00998000
*********************************************************************** 00999000
*                                                                       01000000
*        WORKAREA USED IN MODULE IEAVVWTO                               01001000
*                                                                       01002000
*********************************************************************** 01003000
*                                                                       01004000
WORKVV   DSECT                                                          01005000
@SA00001 DS    18F                                                      01006000
@SAGETB  DS    18F                                                      01007000
@PC00003 DS    F                                                        01008000
@SA00004 DS    F                                                        01009000
@PC00004 DS    F                                                        01010000
@SA00008 DS    15F                                                      01011000
@SA00002 DS    15F                                                      01012000
@PC00002 DS    F                                                        01013000
@AL00001 DS    A                                                        01014000
@TF00001 DS    F                                                        01015000
REG1SAV  DS    F                                                        01016000
REG2SAV  DS    A                  SAVE AREA FOR R2 WHEN DEALING WITH    01017000
*                                 UCM                                   01018000
LONGTXT  DS    A                  -> AREA GETMAINED FOR LONG WPL        01019000
*                                    IF ZERO THEN NO STORAGE GETMAINED  01020000
LONGLEN  DS    F                  L'GETMAINED AREA FOR LONG TEXT        01021000
*                                                                       01022000
LENMWQE  DS    F                  L'OF ALL MINOR WQES                   01023000
*                                 VALID ONLY FOR MLWTO                  01024000
MLWTOXH  DS    A                  -> MLWTO EXTENT HEADER IN CALLERS WPL 01025000
*                                 VALID ONLY FOR MLWTO                  01026000
*                                                                       01027000
*        VERSION 2 XWPL TO STORE ALL USER PROVIDED WPL FIELDS           01028000
*                                                                       01029000
WKALGH   DS    AL2                MESSAGE LENGTH OF MAJOR WQE TEXT FROM 01030000
*                                 CALLER'S WPL                          01031000
WKAMCSF  DS    XL2                MCS FLAGS COPIED FROM CALLERS WPL     01032000
WKAADTXT DS    AL4                -> MESSAGE TEXT                       01033000
WKAVRSN  DS    AL1                XWPL VERSION NUMBER                   01034000
WKAFLAGS DS    XL1                MISC FLAGS                            01035000
WKARPYLN DS    AL1                L'REPLY FOR WTOR                      01036000
WKALNGTH DS    AL1                L'XWPL, ZERO FOR VERSION 1            01037000
WKAMCSF1 DS    XL2                EXTENDED MCS FLAGS                    01038000
WKACPFLF DS    XL2                MCS FLAGS FOR CNTL PROGRAM USE        01039000
WKARPBUF DS    AL4                -> REPLY BUFFER                       01040000
WKAECBP  DS    AL4                -> ECB                                01041000
WKASEQN  DS    AL4                DOM/CONNECT ID                        01042000
WKADSC   DS    XL2             *  DESCRIPTOR CODE TO BE USED IN WTO     01043000
WKAROC   DS    XL2             V  ROUTE CODES TO BE USED IN WTO         01044000
WKAROUT  DS    XL14               EXTENDED ROUTING CODES                01045000
WKAMSGTY DS    XL2                MSGTYPE FLAGS COPIED FROM CALLERS WPL 01046000
WKAPRTY  DS    XL2                MESSAGE PRIORITY                      01047000
WKAJOBID DS    CL8                JOB ID                                01048000
WKAJOBNM DS    CL8                JOB NAME                              01049000
WKAKEY   DS    CL8                RETRIEVAL KEY                         01050000
WKATOKN  DS    AL4                TOKEN FOR DOM                         01051000
WKACNID  DS    AL4                CONSOLE ID                            01052000
WKASYSNA DS    CL8                SYSTEM NAME                           01053000
WKACNNME DS    CL8                CONSOLE NAME                          01054000
WKARCNA  DS    AL4                -> REPLY CONSOLE NAME/ID              01055000
WKACART  DS    AL4                -> CART                               01056000
WKAWSPRM DS    AL4                -> WAIT STATE PARM LIST               01057000
WKAASCB  DS    AL4                -> ASCB                               01058000
WKARSV30 DS    XL16               RESERVED                              01059000
*                                                                       01060000
WKATEXT  DS    CL130              MESSAGE TEXT (MAXIMUM 126 CHARS) +4   01061000
*                                                                       01062000
ESTAEPRM DS    A                                                        01063000
@TS00001 DS    CL1                                                      01064000
STARTID  DS    CL1                                                      01065000
CVDAREA  DS    D                                                        01066000
*                                                                       01067000
PFLAG    DS    XL1                ADDITIONAL FLAGS                      01068000
REQCMSL  EQU   X'40'              REQUEST OR HOLD CMS LOCK              01069000
*                                                                       01070000
*        THE DSECT HAS BEEN TRUNCATED TO AVOID DUPLICATE NAME ISSUES    01071000
*                                                                       01072000
         PRINT NOGEN                                                    01073000
*                                                                       01074000
*        ACB                                                            01075000
*                                                                       01076000
         IFGACB                                                         01077000
*                                                                       01078000
*        RPL                                                            01079000
*                                                                       01080000
         IFGRPL                                                         01081000
*                                                                       01082000
*        TIOT                                                           01083000
*                                                                       01084000
TIOT     DSECT                                                          01085000
*                                                                       01086000
         IEFTIOT1                                                       01087000
*                                                                       01088000
*        JSCB                                                           01089000
*                                                                       01090000
         IEZJSCB                                                        01091000
*                                                                       01092000
*        PSCB                                                           01093000
*                                                                       01094000
         IKJPSCB                                                        01095000
*                                                                       01096000
*        TSO USER PROFILE TABLE                                         01097000
*                                                                       01098000
         IKJUPT                                                         01099000
*                                                                       01100000
*        SYSTEM DIAGNOSTIC WORK AREA                                    01101000
*                                                                       01102000
         IHASDWA DSECT=YES                                              01103000
*                                                                       01104000
*        TCB                                                            01105000
*                                                                       01106000
         IKJTCB LIST=YES                                                01107000
*                                                                       01108000
*        RB                                                             01109000
*                                                                       01110000
         IKJRB   DSECT=YES                                              01111000
*                                                                       01112000
         PRINT GEN                                                      01113000
*                                                                       01114000
*        VARIABLES DEFINED IN THE RBEXSAVE AREA                         01115000
*                                                                       01116000
*/********************************************************************/ 01117000
*/*                                                                  */ 01118000
*/*          EXTENDED SAVEAREA MAPPING FOR SVC 35                    */ 01119000
*/*                                                                  */ 01120000
*/********************************************************************/ 01121000
*                                                                       01122000
*        DEFINED IN THE RBEXSAVE AREA                                   01123000
*                                                                       01124000
         ORG   RBEXSAVE                                                 01125000
XVSAV    DS    0A                                                       01126000
XVA4     DS    0F                      ERROR PROCESSING FIELDS          01127000
XVFNCT   DS    C                       D23 PROCESS CODE                 01128000
D23VALID EQU   X'10'                   PARMLIST VALIDITY CHECK          01129000
D23OREGC EQU   X'20'                   ORE GETCELL FAILURE              01130000
D23OREBC EQU   X'21'                   ORE BLDCPOOL FAILURE             01131000
D23OREGM EQU   X'22'                   ORE GETMAIN FAILURE, SP231       01132000
D23WQEGC EQU   X'30'                   WQE GETCELL FAILURE              01133000
D23WQEBC EQU   X'31'                   WQE BLDCPOOL FAILURE             01134000
D23WQEGM EQU   X'32'                   WQE GETMAIN FAILURE, SP231       01135000
D23DYN   EQU   X'42'                   DYNAMIC AREA GETMAIN FAILURE     01136000
XVAMOD   DS    C                       D23 MODULE ID                    01137000
VWTOID   EQU   X'01'                   IEAVVWTO'S ID FOR D23            01138000
MWTOID   EQU   X'02'                   IEAVMWTO'S ID FOR D23            01139000
XVA41    DS    C                       RESERVED                         01140000
XVREASON DS    C                       D23 REASON CODE                  01141000
D23BNDY  EQU   X'01'                   WTOR PARMLIST NOT ON WORD BNDY   01142000
D23MLWTR EQU   X'02'                   MULTILINE WTOR SPECIFIED         01143000
D23PARM  EQU   X'03'                   WPL NOT ADDRESSABLE BY USER      01144000
D23ZERO  EQU   X'04'                   ZERO TEXT LENGTH WTOR            01145000
D23SIZE  EQU   X'05'                   CALLER MODIFIED WPL              01146000
*                                      DURING WTO PROCESSING            01147000
D23LTXT  EQU   X'06'                   WTO/WTOR TEXT= CODED AND         01148000
*                                      WPLLGH ^=8                       01149000
*                                                                       01150000
XVA8     DS    AL4                     STORE TIME                       01151000
XVWPLADR DS    AL4                     -> CALLERS WPL                   01152000
XVWQEAD  DS    AL4                                                      01153000
*                                                                       01154000
*        FLAGS                                                          01155000
*                                                                       01156000
XVDFLAGS DS    0XL4                                                     01157000
XVD0     DS    X                                                        01158000
XVD0RPFD EQU   BIT1                    HARD COPY ONLY                   01159000
XVD0NWQE EQU   BIT2                    GET WQE                          01160000
XVD0NORE EQU   BIT3                    GET THE ORE FIRST                01161000
XVD0QID  EQU   BIT4                    QID FIELD IS PRESENT IN THE WPL  01162000
XVD0WWB  EQU   BIT5                    WWB WTO WAIT BLOCK OBTAINED      01163000
XVD0USER EQU   BIT6                                                     01164000
XVD0HDCY EQU   BIT7                                                     01165000
*                                                                       01166000
*        XVDFLAGS+1                                                     01167000
*                                                                       01168000
XVD1     DS    X                                                        01169000
XVD1PRIV EQU   BIT0                    PRIVILEGED USER                  01170000
XVD1ENQW EQU   BIT1                    ENQ'D ON A WQE  (VMWTO)          01171000
XVD1ENQO EQU   BIT2                    ENQ'D ON AN ORE (VMWTO)          01172000
XVD1ALDN EQU   BIT2                    ALL NEEDED CONTROL BLKS OBTAINED 01173000
XVD1PP   EQU   BIT5                    PROBLEM PROGRAM CALLER           01174000
XVD1AUTH EQU   BIT6                    KEY 0, SUPVR STATE OR APF AUTH   01175000
XVD1PERR EQU   BIT7                    SEVERE ERROR FOUND. ABEND USER   01176000
*                                                                       01177000
*        XVDFLAGS+2                                                     01178000
*                                                                       01179000
XVD2     DS    X                                                        01180000
XVD2CON  EQU   BIT0                    CONNECTING                       01181000
XVD2VALD EQU   BIT3                    PARAMETER LIST IS VALID          01182000
XVD2DELW EQU   BIT4                    SEND MSG TO HARDCOPY ONLY        01183000
XVD2ZERO EQU   BIT5                    ZERO MSG ID TO USER              01184000
XVD2WTOR EQU   BIT6                    WTOR REQUEST                     01185000
XVD2QFHC EQU   BIT7                    QUEUE THIS MSG TO HARD COPY      01186000
*                                                                       01187000
*        XVDFLAGS+3                                                     01188000
*                                                                       01189000
XVD3     DS    X                                                        01190000
XVD3BLDJ EQU   BIT0                    BUILD MAJOR WQE                  01191000
XVD3BLD1 EQU   BIT1                    BUILD LINE 1                     01192000
XVD3BLD2 EQU   BIT2                    BUILD LINE 2                     01193000
XVD3TXT1 EQU   BIT3                    TEXT LINE 1 BEING USED           01194000
XVD3TFX  EQU   BIT4                    TCBTFX WAS SET ON,TURN IT OFF    01195000
*        END OF FLAGS                                                   01196000
XVOREAD  DS    AL4                                                      01197000
         ORG   XVOREAD                                                  01198000
XVX      DS    0F                      USED AS WORK AREA BY VMWTO       01199000
XVX0     DS    X                       LINE CONTROL FLAGS - MLWTO       01200000
XVX0FLCL EQU   BIT0                    FIRST LINE IS CONTROL LINE       01201000
XVX0LL1F EQU   BIT1                    LABEL LINE 1 FOUND               01202000
XVX0LL2F EQU   BIT2                    LABEL LINE 2 FOUND               01203000
XVX0UDCL EQU   BIT3                    USE DEFAULT CONTROL LINE         01204000
XVX0FLJE EQU   BIT4                    FIRST LINE JUST 'E'              01205000
XVX0FEDE EQU   BIT5                    FORCE END (LAST LINE TO BE DE)   01206000
XVX1     DS    X                       ERROR FLAGS - MLWTO              01207000
XVX1STOP EQU   BIT0                    ERROR IN PARM LIST; IGNORE MLWTO 01208000
XVX1NOID EQU   BIT1                    NO ID FOR CONNECTING MLWTO       01209000
XVX2     DS    AL1                     NO OF LINES STILL TO DO          01210000
XVX3     DS    AL1                     NO OF LINES FROM MLWTO EXT HDR   01211000
*                                                                       01212000
XVCMAJOR DS    AL4                  *                                   01213000
XVCMINOR DS    AL4                  V                                   01214000
*                                                                       01215000
XVWWB    DS    AL4                                                      01216000
*                                                                       01217000
XVWQEID  DS    0AL4       *            CALLERS R0                       01218000
XVWQEIDA DS    AL3        |            A NEW LINE IS TO BE              01219000
*                         |            CONNECTED TO THE MESSAGE         01220000
*                         |            WITH THIS 3 BYTE MESSAGE ID      01221000
*                         |            MLWTO ONLY                       01222000
XVCONID  DS    XL1        V            CONSOLE ID FOR THIS MESSAGE      01223000
*                                                                       01224000
XVRET    DS    0AL4                                                     01225000
XVRETCOD DS    AL4                                                      01226000
XVLEN    EQU   *-XVSAV                 MUST NOT EXCEED 48 BYTES AS      01227000
*                                      RB EXTENDED SAVE AREA USED       01228000
*                                                                       01229000
*        WTO/WTOR/MLWTO/WTP PARAMETER LIST                              01230000
*                                                                       01231000
         IEZWPL                                                         01232000
*                                                                       01233000
         PRINT NOGEN                                                    01234000
*                                                                       01235000
*        ADDRESS SPACE CONTROL BLOCK                                    01236000
*                                                                       01237000
         IHAASCB                                                        01238000
*                                                                       01239000
*        CONSOLE UNIT CONTROL MODULE                                    01240000
*                                                                       01241000
         IEECUCM DSECT=YES,FORMAT=NEW                                   01242000
*                                                                       01243000
*        CVT                                                            01244000
*                                                                       01245000
         CVT   DSECT=YES                                                01246000
*                                                                       01247000
         PRINT GEN                                                      01248000
*                                                                       01249000
R0       EQU   0                                                        01250000
R1       EQU   1                                                        01251000
R2       EQU   2                                                        01252000
R3       EQU   3                                                        01253000
R4       EQU   4                                                        01254000
R5       EQU   5                                                        01255000
R6       EQU   6                                                        01256000
R7       EQU   7                                                        01257000
R8       EQU   8                                                        01258000
R9       EQU   9                                                        01259000
R10      EQU   10                                                       01260000
R11      EQU   11                                                       01261000
R12      EQU   12                                                       01262000
R13      EQU   13                                                       01263000
R14      EQU   14                                                       01264000
R15      EQU   15                                                       01265000
*                                                                       01266000
         END   IGC0203E,(C'PLS1013',0802,87272)                         01267000
./ ENDUP
/*
//*
//STEP03  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DATA
++USERMOD(ZP60039)   REWORK(20220308)                      .
++VER(Z038) FMID(FBB1221)
  PRE(UZ62088,UZ31484)
  REQ(ZP60040)
  SUP(ZUM0013,TMVS805)
 /*
   PROBLEM DESCRIPTION:
     MESSAGE TEXT SUPPLIED TO THE WTO AND WTOR MACROS MUST BE INLINE.
       APPLICATIONS ISSUING WTO MESSAGES CANNOT SPECIFY DISPLAY
       TEXT WHICH IS REMOTE, THAT IS, NOT PHYSICALLY LOCATED
       WITHIN THE WTO OR WTOR PARAMETER LIST.

       CHANGES MADE BY THIS SYSMOD INCLUDE:
         - ENHANCE THE IEZWPL MACRO TO DESCRIBE THE EXTENDED WTO
           PARAMETER LIST (XWPL).
         - ENHANCE THE WTO MACRO TO ACCEPT THE TEXT= OPERAND AND
           CONSTRUCT A WPL OR XWPL AS REQUIRED.
         - ADD SUPPORT FOR THE XWPL INTO IEAVVWTO (SVC 35) AND
           IEAVMWTO (SVC 35 MULTI-LINE WTO SERVICE ROUTINE).
         - ALWAYS COPY THE WPL OR XWPL TO PROTECTED STORAGE,
           INSTEAD OF ONLY FOR UNAUTHORIZED CALLERS.
         - IMPROVED WPL VALIDATION TO AVOID INTERNAL ERRORS AND
           ISSUE SD23 ABENDS IN ACCORDANCE WITH DOCUMENTATION.
         - IMPROVE WTO MESSAGE CHARACTER RENDERING BY DISPLAYING
           MOST STANDARD KEYBOARD CHARACTERS UNTRANSLATED, AND
           SHOWING OTHER CODE POINTS AS A TILDE INSTEAD OF A BLANK.
         - SUBSUME THE FUNCTION OF USERMOD TMVS805 - ALSO WIDELY
           SHIPPED AS USERMOD ZUM0013 - BY KEVIN LEONARD, MAINLY
           BECAUSE THAT USERMOD WOULD NEED TO BE ADAPTED TO FIT THE
           UPDATES SHIPPED HERE.  THE TMVS805/ZUM0013 FUNCTION IS:
             - ALLOW IEECVXIT TO ACCESS "INTERNAL" MESSAGES, THEREBY
               FACILIATING MORE POWERFUL AUTOMATED OPERATIONS.
             - PREVENT HARDCOPY OF DELETED MESSAGES.
           NOTE THAT THE TMVS805/ZUM0013 CHANGES CAN BE DEACTIVATED
           BY ALTERING A SWITCH IN THE IEAVVWTO SOURCE CODE AND
           RECOMPILING.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 39.

     REWORK HISTORY:
       2018-08-20: INITIAL AVAILABILITY.
       2020-08-18: SILENTLY TRUNCATE EXCESSIVELY LONG TEXT.
       2022-03-08: REMOVE A BLANK LINE FROM THE WTO MACRO.

     THIS IS SYSMOD NUMBER 1 OF 2 IN A PACKAGE TO SUPPORT TEXT= ON
     WTO AND WTOR MACROS DEVELOPED BY TOM ARMSTRONG.  BOTH SYSMODS
     SHOULD BE ACTIVATED IN THE SAME IPL.  THE SYSMOD DETAILS ARE:
      +---------+---------+------+----------+-----------------------+
      | USERMOD | FMID    | COMP | PART     | DESCRIPTION           |
      +---------+---------+------+----------+-----------------------+
      | ZP60039 | FBB1221 | SU64 | IEAVMWTO | MLWTO SERVICE ROUTINE |
      |         |         |      | IEAVVWTO | WTO/WTOR PROCESSOR    |
      |         |         |      | IEZWPL   | WTO PARAMETER LIST    |
      |         |         |      | WTO      | INVOKE WTO SERVICE    |
      +---------+---------+------+----------+-----------------------+
      | ZP60040 | EBB1102 | BCP  | IGC0203E | WRITE TO PROGRAMMER   |
      |         |         |      | WTOR     | INVOKE WTOR SERVICE   |
      +---------+---------+------+----------+-----------------------+

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IEAVMWTO
       IEAVVWTO
     MACROS:
       IEZWPL
       WTO
 */.
++MAC(IEZWPL) DISTLIB(AMODGEN).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(NEW,PASS),UNIT=SYSALLDA,
//             SPACE=(CYL,3),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=4080)
//SYSIN    DD  DUMMY
//*
//STEP04  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DSN=&&MACLIB(IEZWPL),DISP=(OLD,PASS)
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP05  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
++MAC(WTO) DISTLIB(AMACLIB).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP06  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DSN=&&MACLIB(WTO),DISP=(OLD,PASS)
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP07  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
++MOD(IEAVMWTO) DISTLIB(AOSC5).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP08  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT)'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=&&MACLIB,DISP=(OLD,PASS),DCB=BLKSIZE=32720
//         DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DSN=&&SOURCE(IEAVMWTO),DISP=(OLD,PASS)
//*
//STEP09  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IEAVMWTO('ZP60039')
++MOD(IEAVVWTO) DISTLIB(AOSC5).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP10  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT)'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=&&MACLIB,DISP=(OLD,PASS),DCB=BLKSIZE=32720
//         DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DSN=&&SOURCE(IEAVVWTO),DISP=(OLD,PASS)
//*
//STEP11  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DATA
  IDENTIFY IEAVVWTO('ZP60039')
++USERMOD(ZP60040)   REWORK(20220308)                      .
++VER(Z038) FMID(EBB1102)
  PRE(UY13810) .
++IF FMID(FBB1221) REQ(ZP60039)
 /*                              ADD TEXT= TO WTO AND WTOR.
   PROBLEM DESCRIPTION:
     MESSAGE TEXT SUPPLIED TO THE WTO AND WTOR MACROS MUST BE INLINE.
       APPLICATIONS ISSUING WTO MESSAGES CANNOT SPECIFY DISPLAY
       TEXT WHICH IS REMOTE, THAT IS, NOT PHYSICALLY LOCATED
       WITHIN THE WTO OR WTOR PARAMETER LIST.

       THIS SYSMOD ENHANCES WTP (WRITE TO PROGRAMMER) TO SUPPORT
       THE EXTENDED WTO PARAMETER LIST (XWPL), AND CHANGES THE
       WTOR MACRO TO ACCEPT THE TEXT= OPERAND AND BUILD A WPL
       OR XWPL AS REQUIRED.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 40.

     REWORK HISTORY:
       2018-08-20: INITIAL AVAILABILITY.
       2019-04-22: FIX TPUT OF WTP IN IGC0203E.
       2020-08-18: SILENTLY TRUNCATE EXCESSIVELY LONG TEXT.
       2022-03-08: REMOVE A BLANK LINE FROM THE WTOR MACRO.

     THIS IS SYSMOD NUMBER 2 OF 2 IN A PACKAGE TO SUPPORT TEXT= ON
     WTO AND WTOR MACROS DEVELOPED BY TOM ARMSTRONG.  BOTH SYSMODS
     SHOULD BE ACTIVATED IN THE SAME IPL.  THE SYSMOD DETAILS ARE:
      +---------+---------+------+----------+-----------------------+
      | USERMOD | FMID    | COMP | PART     | DESCRIPTION           |
      +---------+---------+------+----------+-----------------------+
      | ZP60039 | FBB1221 | SU64 | IEAVMWTO | MLWTO SERVICE ROUTINE |
      |         |         |      | IEAVVWTO | WTO/WTOR PROCESSOR    |
      |         |         |      | IEZWPL   | WTO PARAMETER LIST    |
      |         |         |      | WTO      | INVOKE WTO SERVICE    |
      +---------+---------+------+----------+-----------------------+
      | ZP60040 | EBB1102 | BCP  | IGC0203E | WRITE TO PROGRAMMER   |
      |         |         |      | WTOR     | INVOKE WTOR SERVICE   |
      +---------+---------+------+----------+-----------------------+

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IGC0203E
     MACROS:
       WTOR
 */.
++MAC(WTOR) DISTLIB(AMACLIB).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP12  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DSN=&&MACLIB(WTOR),DISP=(OLD,PASS)
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP13  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
++MOD(IGC0203E) DISTLIB(AOSB3).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP14  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT)'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=&&MACLIB,DISP=(OLD,PASS),DCB=BLKSIZE=32720
//         DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DSN=&&SOURCE(IGC0203E),DISP=(OLD,PASS)
//*
//STEP15  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DATA
  IDENTIFY IGC0203E('ZP60040')
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//MACLIB   DD  DSN=&&MACLIB,DISP=(OLD,DELETE)
//SOURCE   DD  DSN=&&SOURCE,DISP=(OLD,DELETE)
//SYSIN    DD  DUMMY
//*
//STEP16  EXEC SMPREC
//SMPPTFIN DD  DSN=&&SMPMCS,DISP=(OLD,DELETE)
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60039,ZP60040)
          .
/*
//STEP17  EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60039,ZP60040)
        CHECK
        .
/*
//*
//STEP18   EXEC SMPAPP,WORK='SYSALLDA',COND=(0,NE)
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60039,ZP60040)
        DIS(WRITE)
        .
/*
//
