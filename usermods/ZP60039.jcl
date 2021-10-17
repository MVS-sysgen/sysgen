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
*  %/*                                                                  00001000
         MACRO                                                          00002000
&NAME   WTO    &MESG,&MF=I,&ROUTCDE=,&DESC=,&MSGTYP=,&MCSFLAG=,&QID=,&AX00003000
               REAID=,&TEXT=^                                           00004000
.********************************************************************** 00005000
.*                                                                      00006000
.*       WTO - WRITE TO OPERATOR                                        00007000
.*                                                                      00008000
.*       INVOCATION - SPECIFY -  WTO MESG,ROUTCDE=,DESC=,MSGTYP=,       00009000
.*                            MCSFLAG=,QID=,AREAID=,MF=,TEXT=           00010000
.*              WHERE -                                                 00011000
.*                MESG     THE MESSAGE TEXT FOR A SINGLE OR             00012000
.*                         MULTIPLE LINE MESSAGE TO BE                  00013000
.*                         WRITTEN                                      00014000
.*                                                                      00015000
.*                ROUTCDE= ROUTING CODES TO BE ASSIGNED TO THE          00016000
.*                         MESSAGE                                      00017000
.*                                                                      00018000
.*                DESC=    DESCRIPTOR CODES TO BE ASSIGNED TO           00019000
.*                         THE MESSAGE                                  00020000
.*                                                                      00021000
.*                MSGTYP=  SPECIFIES HOW THE MESSAGE IS TO BE           00022000
.*                         ROUTED. VALID VALUES ARE:                    00023000
.*                         N,Y,SESS,JOBNAMES,STATUS,ACTIVE,             00024000
.*                         SHOW                                         00025000
.*                                                                      00026000
.*                MCSFLAG= SPECIFIES ATTRIBUTES OF THE                  00027000
.*                         MESSAGE. VALID VALUES ARE:                   00028000
.*                         REG0,RESP,REPLY,BRDCST,HRDCPY,               00029000
.*                         QREG0,NOTIME,NOCPY                           00030000
.*                                                                      00031000
.*                QID=     REMOTE ENTRY SERVICES (RSS) QUEUE            00032000
.*                         ID (OS/VS1 ONLY). PARAMETER ACCEPTED         00033000
.*                         BUT IGNORED BY MVS                           00034000
.*                                                                      00035000
.*                AREAID=  SPECIFIES A DISPLAY AREA ON THE              00036000
.*                         CONSOLE WHERE THE MESSAGE IS TO BE           00037000
.*                         WRITTEN.                                     00038000
.*                                                                      00039000
.*                MF=      SPECIFIES THE TYPE OF EXPANSION              00040000
.*                         REQUIRED. VALID VALUES ARE:                  00041000
.*                           I,L,E                                      00042000
.*                                                                      00043000
.*                TEXT=    SPECIFIES A DATA AREA CONTAINING A 2         00044000
.*                         BYTE MESSAGE LENGTH FIELD FOLLOWED BY        00045000
.*                         THE ACTUAL MESSAGE TEXT. IF A REGISTER       00046000
.*                         IS USED, THE REGISTER CONTAINS THE           00047000
.*                         ADDRESS OF THE DATA AREA. IF A FIELD         00048000
.*                         IS USED, THE ADDRESS OF THE FIELD IS         00049000
.*                         STORED IN THE WPL. THE TEXT KEYWORD IS       00050000
.*                         MUTUALLY EXCLUSIVE WITH INLINE               00051000
.*                         MESSAGE TEXT. EITHER TEXT OR INLINE          00052000
.*                         TEXT (BUT NOT BOTH) IS REQUIRED ON THE       00053000
.*                         STANDARD OR LIST FORM OF WTO                 00054000
.*                                                                      00055000
.*       FUNCTION - BUILDS ONE OF THREE POSSIBLE PARAMETER LIST         00056000
.*                  FORMATS AND/OR THE CODE WHICH WILL INVOKE           00057000
.*                  SVC 35 TO ISSUE THE MESSAGE.                        00058000
.*                  1. A STANDARD WPL IS BUILT IF INLINE TEXT IS        00059000
.*                     SPECIFIED AND NO ROUTING CODE GREATER THAN 16    00060000
.*                     IS SPECIFIED.                                    00061000
.*                  2. IF INLINE TEXT IS SPECIFIED AND A ROUTING CODE   00062000
.*                     GREATER THAN 16 IS SPECIFIED THEN AN EXTENDED    00063000
.*                     WPL VERSION 1 IS BUILT.                          00064000
.*                  3. IF THE TEXT OPERAND IS SPECIFIED THEN AN         00065000
.*                     EXTENDED WPL VERSION 2 IS GENERATED.             00066000
.*                                                                      00067000
.*       STATUS -                                                       00068000
.*       LASTUPD         = EBB1102  TYPE=ADD                            00069000
.*       LIBRARIES       = DISTLIB=AMACLIB                              00070000
.*       FMID            = FBB1221                                      00071000
.*       RMID            = UZ31484                                      00072000
.*       SYSMOD HISTORY  = SYSMOD   TYPE       DATE   MCS  STATUS       00073000
.*                         EBB1102  FUNCTION  00.000  MAC  ACC RGN      00074000
.*                         FBB1221  FUNCTION  00.000  MAC  ACC RGN      00075000
.*                                                                      00076000
.*       CHANGE ACTIVITY -                                              00077000
.*              ZP60039  - EXTENSIVE REVISION OF THE MACRO AND          00078000
.*                         SUPPORT FOR THE TEXT OPERAND WHICH           00079000
.*                         REQUIRES GENERATION OF AN EXTENDED WPL       00080000
.*                                                                      00081000
.********************************************************************** 00082000
         GBLB  &IHBWTL                                                  00083000
         LCLA  &LT(256),&H,&I,&II,&N,&J,&K,&LEN,&LLCNT                  00084000
         LCLA  &NUMENT                 NUMBER OF SUBLIST ENTRIES        00085000
         LCLA  &LENENT                 L'ENTRY                          00086000
         LCLB  &AD,&E,&E1,&E2,&E3,&E4,&E5,&E6,&MLW  MLWTO FLAGS         00087000
         LCLB  &SECONDL,&PAIR                                           00088000
         LCLC  &GNAME                                                   00089000
         LCLB  &RO(128),&DE(16),&MC(32),&MT(16)  FLAGS FOR THE FIELDS   00090000
         LCLC  &LOWNUM,&HIGHNUM        ROUTE RANGE NUMBERS              00091000
         LCLB  &GENRD                  GENERATE ROUT AND DESC FIELDS    00092000
         LCLB  &GENMT                  GENERATE MSGTYP FIELD            00093000
         LCLB  &GENXWPL                GENERATE AN XWPL                 00094000
         LCLA  &WPLXLEV                LEVEL OF XWPL TO GENERATE        00095000
         LCLB  &MTY,&MTN               FLAGS FOR MSGTYP PROCESSING      00096000
         ACTR  30000                                                    00097000
.*                                                                      00098000
.*       INITIALIZATION                                                 00099000
.*                                                                      00100000
&GNAME   SETC  'IHB'.'&SYSNDX'         FOR BAL INSTRUCTION              00101000
&WPLXLEV SETA  1                                                        00102000
.********************************************************************** 00103000
.*                                                                      00104000
.*       BEGIN PROCESSING PARAMETERS                                    00105000
.*                                                                      00106000
.********************************************************************** 00107000
.********************************************************************** 00108000
.*                                                                      00109000
.*        PROCESS ROUTING CODES                                         00110000
.*        DETERMINE IF ROUTING CODES HAVE BEEN SPECIFIED                00111000
.*                                                                      00112000
.*        ROUTING CODES 1-128 MAY BE SET HOWEVER FOR MVS 3.8 ONLY       00113000
.*        ONLY ROUTING CODES 1-16 WILL BE ACTIONED                      00114000
.*        ANY ROUTING CODE GREATER THAN 16 WILL BE IGNORED              00115000
.*        WITHOUT ERROR HOWEVER SPECIFYING A ROUTING CODE GREATER       00116000
.*        THAN 16 WILL CAUSE THE GENERATION OF AN EXTENDED FORMAT       00117000
.*        VERSION 1 WPL (XWPL).                                         00118000
.*                                                                      00119000
.********************************************************************** 00120000
         AIF   (T'&ROUTCDE EQ 'O').ENDROUT   ROUTING CODES APECIFIED ?  00121000
&GENRD   SETB  1                    ROUTING CODES PRESENT               00122000
&I       SETA  1                    INITIALIZE LIST INDEX               00123000
&NUMENT  SETA  N'&ROUTCDE           TOTAL NUMBER OF ENTRIES IN &ROUTCDE 00124000
.*       ROUTING CODE LOOP                                              00125000
.ROULOOP ANOP                                                           00126000
&LENENT  SETA  K'&ROUTCDE(&I)       SET L'CURRENT ELEMENT               00127000
         AIF   (&LENENT EQ 0).BADENT  NULL ENTRY, ERROR                 00128000
         AIF   (T'&ROUTCDE(&I) EQ 'N').ROUVAL   SINGLE NUMERIC VALUE    00129000
.*                                                                      00130000
.*       RANGE OF ROUTING CODES HAS BEEN SPECIFIED                      00131000
.*                                                                      00132000
.*       SCAN FOR DASH SEPERATOR                                        00133000
.*                                                                      00134000
&II      SETA  1                    INITIALIZE ELEMENT INDEX            00135000
.DASHL   ANOP                       DASH SCANNING LOOP                  00136000
         AIF   ('&ROUTCDE(&I)'(&II,1) EQ '-').DASHFND                   00137000
&II      SETA  &II+1                INCREMENT ELEMENT INDEX             00138000
         AIF   (&II GE &LENENT).BADRANG   CHECK INDEX POSITION          00139000
         AGO   .DASHL               LOOP AROUND FOR NEXT CHAR           00140000
.*       FOUND A DASH SEPERATOR                                         00141000
.DASHFND ANOP                                                           00142000
         AIF   (&II EQ 1).BADRANG         DASH UP FRONT ? ERROR         00143000
         AIF   (&II EQ &LENENT).BADRANG   DASH AT THE END ? ERROR       00144000
&LOWNUM  SETC  '&ROUTCDE(&I)'(1,&II-1) SET FIRST NUMBER OF RANGE        00145000
&HIGHNUM SETC  '&ROUTCDE(&I)'(&II+1,&LENENT-&II) SET SECOND NUMBER      00146000
.*       FIRST NUMBER > LAST NUMBER ? ERROR                             00147000
         AIF   ('&LOWNUM' GT '&HIGHNUM').BADRANG                        00148000
.*       OUTSIDE THE RANGE OF VALID ROUTING CODES ? ERROR               00149000
         AIF   ('&LOWNUM' LT '1' OR '&LOWNUM' GT '128' OR              X00150000
               '&HIGHNUM' LT '1' OR '&HIGHNUM' GT '128').BADRANG        00151000
&J       SETA  &LOWNUM              INITIALIZE START OF RANGE           00152000
&K       SETA  &HIGHNUM             INITIALIZE END OF RANGE             00153000
.*       LOOP TO SET ON ROUTING CODES                                   00154000
.*       IF ALREADY SET ON THEN ERROR                                   00155000
.RANLOOP ANOP                                                           00156000
         AIF   (&RO(&J)).BADRANG                                        00157000
&RO(&J)  SETB  1                    SET ROUTING CODE &J ON              00158000
         AIF   (&J LT 17).RANLOPA                                       00159000
&GENXWPL SETB  1                    ANY VALUE > 16 FORCES A XWPL        00160000
.RANLOPA ANOP                                                           00161000
&J       SETA  &J+1                 INCREMENT LOOP INDEX                00162000
         AIF   (&J LE &K).RANLOOP   LOOP THROUGH RANGE                  00163000
.*       RANGE PROCESSED                                                00164000
         AGO   .NEXTROU             DETERMINE NEXT ROUTING CODE         00165000
.*                                                                      00166000
.*       ROUTING CODE VALUE HAS BEEN SPECIFIED. VERIFY THAT IT IS       00167000
.*       WITHIN THE PROPER RANGE AND, IF SO, SET THE CORRECT BIT        00168000
.*                                                                      00169000
.ROUVAL  ANOP                                                           00170000
&J       SETA  &ROUTCDE(&I)         GET NEXT ROUTING CODE VALUE         00171000
         AIF   (&J GE 1 AND &J LE 128).ROUOK   VALID VALUE ?            00172000
         MNOTE 4,'ROUTCDE &J IS AN INVALID ROUTING CODE'                00173000
         AGO   .NEXTROU                                                 00174000
.ROUOK   ANOP                       VALID ROUTING CODE SPECIFIED        00175000
         AIF   (&RO(&J)).BADRANG                                        00176000
&RO(&J)  SETB  1                    SET ROUTING CODE &J ON              00177000
         AIF   (&J LT 17).NEXTROU                                       00178000
&GENXWPL SETB  1                    ANY VALUE > 16 FORCES A XWPL        00179000
&MC(12)  SETB  1                    SET MCS FLAG FOR XWPL (WPLMCSFL)    00180000
         AGO   .NEXTROU             LOOP TO NEXT VALUE                  00181000
.*       A NULL ROUTING CODE HAS BEEN SPECIFIED                         00182000
.*       ISSUE AN MNOTE AND SKIP TO THE NEXT ROUTING CODE               00183000
.BADENT  ANOP                                                           00184000
         MNOTE 4,'NULL ROUTING CODE SPECIFIED'                          00185000
         AGO   .NEXTROU                                                 00186000
.*       AN INVALID ROUTING CODE RANGE HAS BEEN SPECIFIED               00187000
.*       ISSUE AN MNOTE AND SKIP TO THE NEXT ROUTING CODE               00188000
.BADRANG ANOP                                                           00189000
         MNOTE 4,'INVALID RANGE OF ROUTING CODES IGNORED.'              00190000
.*       FALL THROUGH TO SETUP FOR NEXT VALUE                           00191000
.*                                                                      00192000
.*       SETUP TO PROCESS NEXT ROUTE CODE ENTRY                         00193000
.NEXTROU ANOP                                                           00194000
&I       SETA  &I+1                 INCREMENT INDEX TO ROUT CODE VALUES 00195000
         AIF   (&I LE &NUMENT).ROULOOP IF MORE CODES, CHECK NEXT ONE    00196000
.ENDROUT ANOP                       DONE WITH ROUTING CODES             00197000
.********************************************************************** 00198000
.*                                                                      00199000
.*        PROCESS DESCRIPTOR CODES                                      00200000
.*        DETERMINE IF DESCRIPTOR CODES HAVE BEEN SPECIFIED             00201000
.*                                                                      00202000
.********************************************************************** 00203000
         AIF   (T'&DESC EQ 'O').ENDDESC   NO DESCRIPTOR, BRANCH         00204000
&GENRD   SETB  1                 FORCE GENERATION OF DESC AND ROUTCDES  00205000
&NUMENT  SETA  N'&DESC           NUMBER OF ENTRIES                      00206000
&I       SETA  1                                                        00207000
&J       SETA  0                 COUNTER TO DETECT MUTUALLY EXCL VALUE  00208000
.DESCLOP ANOP                                                           00209000
&II      SETA  &DESC(&I)                                                00210000
         AIF   (&II GE 1 AND &II LE 16).DESCOK                          00211000
.DESCERR MNOTE 8,'DESC &N IS INVALID DESCRIPTOR - IGNORED'              00212000
         AGO   .NEXTDES                                                 00213000
.DESCOK  ANOP                                                           00214000
         AIF   (&DE(&II)).DESERR            ALREADY SPECIFIED           00215000
         AIF   (&II GT 6).DESCJMP                                       00216000
&J       SETA  &J+1                         A VALUE LOWER THAN 7 SPEC   00217000
.DESCJMP ANOP                                                           00218000
&DE(&II) SETB  1                                                        00219000
.NEXTDES ANOP                                                           00220000
&I       SETA  &I+1                         INCR ENTRY COUNTER          00221000
         AIF   (&I LE &NUMENT).DESCLOP      PROCESS NEXT ENTRY          00222000
.*       ALL ENTRIES PROCESSED                                          00223000
.*       TEST FOR ANY MUTUALLY EXCLUSIVE ENTRIES                        00224000
         AIF   (&J LT 2).ENDDESC            ALL OK                      00225000
         MNOTE 8,'DESC MUTUALLY EXCLUSIVE VALUE SPECIFIED'              00226000
         MEXIT                                                          00227000
.DESERR  MNOTE 8,'DUPLICATE DESCRIPTOR VALUE SPECIFIED'                 00228000
         MEXIT                                                          00229000
.ENDDESC ANOP                                                           00230000
.********************************************************************** 00231000
.*                                                                      00232000
.*       PROCESS MSGTYP PARAMETERS                                      00233000
.*       DETERMINE IF MSGTYP PARAMETERS HAVE BEEN SPECIFIED             00234000
.*       PARAMETERS -                                                   00235000
.*       Y        - GENERATES MSGTYP FIELD BUT IGNORES OTHER            00236000
.*                  PARAMETERS (ALL MSGTYP FLAGS ZERO)                  00237000
.*                  THIS MUST BE THE FIRST PARAMETER                    00238000
.*       N        - SUPPESSSES THE GENERATION OF THE MSGTYP             00239000
.*                  FIELD EVEN THOUGH OTHER PARAMETERS HAVE BEEN        00240000
.*                  SPECIFIED                                           00241000
.*                  THIS MUST BE THE FIRST PARAMETER                    00242000
.*       JOBNAMES -                                                     00243000
.*       STATUS   -                                                     00244000
.*       ACTIVE   -                                                     00245000
.*       SHOW     - FOR VS/1 CRJE SUPPORT                               00246000
.*       SESS     -                                                     00247000
.*       NOTE THAT THE MSGTYP FIELD WILL BE GENERATED AND THE           00248000
.*       WPLMSGTD FLAG WILL BE SET IF THE QID PARAMETER IS              00249000
.*       CODED ON THE WTO MACRO                                         00250000
.*                                                                      00251000
.********************************************************************** 00252000
         AIF   (T'&MSGTYP EQ 'O').ENDMSGT   MSGTYP PARAMS SPECIFIED ?   00253000
.*                                          YES, USE OF MSGTYPE FORCES  00254000
&GENRD   SETB  1                            GENERATION OF ROUT/DESC     00255000
&NUMENT  SETA  N'&MSGTYP                    NUMBER OF ENTRIES           00256000
         AIF   ('&MSGTYP(1)' NE 'N').MSGL1                              00257000
&MTN     SETB  1                            MSGTYP=N CODED              00258000
         AGO   .MSGL2                                                   00259000
.MSGL1   AIF   ('&MSGTYP(1)' NE 'Y').MSGL3  MSGTYP=Y CODED              00260000
&MTY     SETB  1                                                        00261000
.MSGL2   ANOP                               EITHER MSGTYP =Y/N CODED    00262000
&I       SETA  1                            FIRST PARAMETER PROCESSED   00263000
         AGO   .NEXTMSG                                                 00264000
.MSGL3   ANOP                                                           00265000
&I       SETA  0                                                        00266000
         AGO   .NEXTMSG                                                 00267000
.MSGTLOP ANOP                               LOOP THROUGH PARAMETERS     00268000
         AIF   ('&MSGTYP(&I)' NE 'JOBNAMES').MSGL4                      00269000
         AIF   (&MT(1)).MTERR               JOBNAMES ALREADY CODED ?    00270000
&MT(1)   SETB  1                            TURN ON JOBNAMES            00271000
         AGO   .NEXTMSG                                                 00272000
.MSGL4   ANOP                                                           00273000
         AIF   ('&MSGTYP(&I)' NE 'STATUS').MSGL5                        00274000
         AIF   (&MT(2)).MTERR               STATUS ALREADY CODED ?      00275000
&MT(2)   SETB  1                            TURN ON STATUS              00276000
         AGO   .NEXTMSG                                                 00277000
.MSGL5   ANOP                                                           00278000
         AIF   ('&MSGTYP(&I)' NE 'ACTIVE').MSGL6                        00279000
         AIF   (&MT(3)).MTERR               ACTIVE ALREADY CODED ?      00280000
&MT(3)   SETB  1                            TURN ON ACTIVE              00281000
         AGO   .NEXTMSG                                                 00282000
.MSGL6   ANOP                                                           00283000
         AIF   ('&MSGTYP(&I)' NE 'SHOW').MSGL7                          00284000
         AIF   (&MT(5)).MTERR               SHOW ALREADY CODED ?        00285000
&MT(5)   SETB  1                            TURN ON SHOW                00286000
         AGO   .NEXTMSG                                                 00287000
.MSGL7   ANOP                                                           00288000
         AIF   ('&MSGTYP(&I)' NE 'SESS').MTERR                          00289000
         AIF   (&MT(6)).MTERR               SESS ALREADY CODED ?        00290000
&MT(6)   SETB  1                            TURN ON SESS                00291000
.NEXTMSG ANOP                                                           00292000
&GENMT   SETB  1                            GENERATE MSGTYP FIELD       00293000
&I       SETA  &I+1                         INCR ENTRY COUNTER          00294000
         AIF   (&I LE &NUMENT).MSGTLOP      PROCESS NEXT ENTRY          00295000
.*       ALL ENTRIES PROCESSED                                          00296000
.*       PROCESS FLAGS                                                  00297000
&GENMT   SETB  (NOT &MTN)                   SUPRESS MT GEN IF MSGTYP=N  00298000
         AIF   (&MTN OR &MTY).MSGL8                                     00299000
         AGO   .ENDMSGT                                                 00300000
.*       ERROR PROCESSING                                               00301000
.MTERR   MNOTE 8,'MESSAGE TYPE FIELD INVALID - N IS ASSUMED'            00302000
&MTN     SETB  1                                                        00303000
&MTY     SETB  0                                                        00304000
.MSGL8   ANOP                               TURN OFF ALL FLAGS          00305000
&MT(1)   SETB  0                                                        00306000
&MT(2)   SETB  0                                                        00307000
&MT(3)   SETB  0                                                        00308000
&MT(5)   SETB  0                                                        00309000
&MT(6)   SETB  0                                                        00310000
.ENDMSGT ANOP                                                           00311000
.********************************************************************** 00312000
.*                                                                      00313000
.*       PROCESS MCSFLAG PARAMETERS                                     00314000
.*       DETERMINE IF MCSFLAG PARAMETERS HAVE BEEN SPECIFIED            00315000
.*       PARAMETERS -                                                   00316000
.*       REG0     - MSG TO CONSOLE WITH SOURCE ID IN R0                 00317000
.*       RESP     - WTO IS IMMEDIATE COMMAND RESPONSE                   00318000
.*       REPLY    - WTO IS A REPLY TO A WTOR                            00319000
.*       BRDCAST  - MSG TO BE BROADCAST TO ALL ACTIVE CONSOLES          00320000
.*       HRDCOPY  - MSG TO BE QUEUED FOR HARDCOPY ONLY                  00321000
.*       QREG0    - MSG TO BE QUEUED UNCONDITIONALLY TO CON ID IN R0    00322000
.*       NOTIME   - TIME IS NOT TO BE APPENDED TO MSG  TER              00323000
.*       NOCPY    - IF WTO OR WTOR IS ISSUED IN SUPERVISOR STATE        00324000
.*                  THEN DO NOT QUEUE FOR HARDCOPY                      00325000
.*                                                                      00326000
.********************************************************************** 00327000
         AIF   (T'&MCSFLAG EQ 'O').ENDMCS    MCSFLAG PARAMS SPECIFIED ? 00328000
&I       SETA  1                    SET LIST INDEX                      00329000
&NUMENT  SETA  N'&MCSFLAG           NUMBER OF ENTRIES                   00330000
.MCLOOP  ANOP                       START LOOP THROUGH PARAMETERS       00331000
         AIF   ('&MCSFLAG(&I)' NE 'REG0').MCL1                          00332000
         AIF   (&MC(2)).MCERR       REG0 ALREADY CODED ?                00333000
&MC(2)   SETB  1                    TURN ON REG0 (WPLMCSFB)             00334000
         AGO   .NEXTMC                                                  00335000
.MCL1    ANOP                                                           00336000
         AIF   ('&MCSFLAG(&I)' NE 'RESP').MCL2                          00337000
         AIF   (&MC(3)).MCERR       RESP ALREADY CODED ?                00338000
&MC(3)   SETB  1                    TURN ON RESP (WPLMCSFC)             00339000
         AGO   .NEXTMC                                                  00340000
.MCL2    ANOP                                                           00341000
         AIF   ('&MCSFLAG(&I)' NE 'REPLY').MCL3                         00342000
         AIF   (&MC(5)).MCERR       REPLYG0 ALREADY CODED ?             00343000
&MC(5)   SETB  1                    TURN ON REPLY (WPLMCSFE)            00344000
         AGO   .NEXTMC                                                  00345000
.MCL3    ANOP                                                           00346000
         AIF   ('&MCSFLAG(&I)' NE 'BRDCAST').MCL4                       00347000
         AIF   (&MC(6)).MCERR       BRDCAST ALREADY CODED ?             00348000
&MC(6)   SETB  1                    TURN ON BRDCAST (WPLMCSFF)          00349000
         AGO   .NEXTMC                                                  00350000
.MCL4    ANOP                                                           00351000
         AIF   ('&MCSFLAG(&I)' NE 'HRDCPY').MCL5                        00352000
         AIF   (&MC(7)).MCERR       HRDCPY ALREADY CODED ?              00353000
&MC(7)   SETB  1                    TURN ON HRDCPY (WPLMCSFG)           00354000
         AGO   .NEXTMC                                                  00355000
.MCL5    ANOP                                                           00356000
         AIF   ('&MCSFLAG(&I)' NE 'QREG0').MCL6                         00357000
         AIF   (&MC(8)).MCERR       QREG0 ALREADY CODED ?               00358000
&MC(8)   SETB  1                    TURN ON QREG0 (WPLMCSFH)            00359000
         AGO   .NEXTMC                                                  00360000
.MCL6    ANOP                                                           00361000
         AIF   ('&MCSFLAG(&I)' NE 'NOTIME').MCL7                        00362000
         AIF   (&MC(9)).MCERR       NOTIME ALREADY CODED ?              00363000
&MC(9)   SETB  1                    TURN ON NOTIME (WPLMCSFI)           00364000
         AGO   .NEXTMC                                                  00365000
.MCL7    ANOP                                                           00366000
         AIF   ('&MCSFLAG(&I)' NE 'NOCPY').MCERR                        00367000
         AIF   (&MC(14)).MCERR      NOTIME ALREADY CODED ?              00368000
&MC(14)  SETB  1                    TURN ON NOTTIME (WPLMCSFN)          00369000
         AGO   .NEXTMC                                                  00370000
.MCERR   ANOP                                                           00371000
         MNOTE 8,'&MCSFLAG(&I) IS INVALID - IGNORED'                    00372000
.NEXTMC  ANOP                       INCR ENTRY INDEX                    00373000
&I       SETA  &I+1                                                     00374000
         AIF   (&I LE &NUMENT).MCLOOP                                   00375000
.*       TEST FOR ERRORS                                                00376000
         AIF   (&MC(7) AND &MC(14)).MCEXCL    HRDCOPY AND NOCPY ?       00377000
         AGO   .ENDMCS                                                  00378000
.MCEXCL  MNOTE 8,'HRDCPY AND NOCPY MUTUALLY EXCLUSIVE, HRDCPY ASSUMED'  00379000
&MC(14)  SETB  0                      TURN OFF NOCPY                    00380000
.ENDMCS  ANOP                                                           00381000
.********************************************************************** 00382000
.*                                                                      00383000
.*       ALL PARAMETERS EXCEPT MESSAGE TEXT AND MF HAVE BEEN PROCESSED  00384000
.*       PROCESS ANY PARAMETER INTERACTIONS                             00385000
.*                                                                      00386000
.********************************************************************** 00387000
.*       DETERMINE IF A DEFAULT ROUTING CODE OF 2 HAS TO BE SET         00388000
.*                                                                      00389000
.*       IF THE GENERATION OF ROUTING AND DESCRIPTOR CODES FLAG         00390000
.*       HAS BEEN SET HOWEVER NO ROUTING AND DESCRIPTOR CODES           00391000
.*       HAVE BEEN PROVIDED THEN SET ROUTING CODE 2 AS A DEFAULT        00392000
         AIF   ((&GENRD) AND (T'&ROUTCDE EQ 'O')).INT1A                 00393000
         AIF   (NOT &GENMT).INT1    NO MSGTYP, BRANCH                   00394000
         AIF   (&MC(2) OR &MC(8)).INT1   MCS REG0 OR QREG0 CODED ?      00395000
.INT1A   ANOP                                                           00396000
.*       SET A DEFAULT ROUTING CODE OF 2                                00397000
&GENRD   SETB  1                                                        00398000
&RO(2)   SETB  1                                                        00399000
.INT1    ANOP                                                           00400000
.*       QID FORCES GENERATION OF DESC AND ROUTE FIELDS                 00401000
&GENRD   SETB  (&GENRD OR (T'&QID NE 'O'))                              00402000
.*       DETERMINE IF THE MCS FLAG FOR ROUT/DESC PRESENT SHOULD BE ON   00403000
&MC(1)   SETB  (&GENRD)                                                 00404000
.*       DETERMINE IF MCS FLAG FOR MSGTYP PRESENT SHOULD BE ON          00405000
&MC(4)   SETB  (&GENMT)                                                 00406000
.********************************************************************** 00407000
.*                                                                      00408000
.*       DETERMINE SOURCE OF TEXT FOR MESSAGE                           00409000
.*                                                                      00410000
.********************************************************************** 00411000
.*       MESSAGE TEXT AND INLINE TEXT ARE MUTUALLY EXCLUSIVE            00412000
.*       ISSUE ERROR MESSAGE IF CODED EXCEPT IF MF=E                    00413000
         AIF   (('&MESG' NE '') AND ('&TEXT' NE '^')).INT2              00414000
         AGO   .INT3                                                    00415000
.INT2    MNOTE 12,'''TEXT'' AND INLINE TEXT ARE MUTUALLY EXCLUSIVE'     00416000
         AGO   .END                                                     00417000
.*       EITHER MESG OR TEXT MUST BE SPECIFIED UNLESS MF=E              00418000
.INT3    AIF   ('&MF(1)' EQ 'E').INT5                                   00419000
         AIF   (('&MESG' EQ '') AND ('&TEXT' EQ '^')).INT4              00420000
         AGO   .INT5                                                    00421000
.INT4    MNOTE 12,'NO MESSAGE TEXT OR TEXT PARAMETER SPECIFIED'         00422000
         AGO   .END                                                     00423000
.INT5    ANOP                                                           00424000
.*       USE OF TEXT= PARAMETER FORCES GENERATION OF TYPE 2 XWPL        00425000
         AIF   ('&TEXT' EQ '^').INT6                                    00426000
&GENXWPL SETB  1                       GENERATE AN XWPL                 00427000
&WPLXLEV SETA  2                       LEVEL OF XWPL TO GENERATE        00428000
&MC(12)  SETB  1                       SET MCS FLAG FOR XWPL (WPLMCSFL) 00429000
&MC(25)  SETB  1                       TEXT ADDR SPECIFIED (WPXTXTAD)   00430000
.INT6    ANOP                                                           00431000
.********************************************************************** 00432000
.*                                                                      00433000
.*       PROCESS MF PARAMETER                                           00434000
.*                                                                      00435000
.********************************************************************** 00436000
         AIF    ('&MF' EQ 'I' OR '&MF' EQ 'L').WPLGEN                   00437000
.*                                                                      00438000
.*       MF=(E,X) PROCESSING                                            00439000
.*                                                                      00440000
         AIF   ('&MF(1)' NE 'E').ERROR1                                 00441000
         AIF   (N'&MF NE 2).ERROR1                                      00442000
&NAME    IHBINNRA &MF(2)            GENERATE INITIAL LR OR LA IF NEEDED 00443000
         AIF   (&IHBWTL).END                                            00444000
         AIF   (NOT &GENXWPL).MFESVC                                    00445000
.*       UPDATE MCS FLAGS                                               00446000
         OI    2(1),B'&MC(1)&MC(2)&MC(3)&MC(4)&MC(5)&MC(6)&MC(7)&MC(8)'X00447000
                                        MCS FLAGS                       00448000
         OI    3(1),B'&MC(9)&MC(10)&MC(11)&MC(12)&MC(13)&MC(14)&MC(15)&X00449000
               MC(16)'                  MCS FLAGS                       00450000
.*       ANY NEED TO UPDATE ADDITIONAL WPLX FIELDS ?                    00451000
         AIF   ((NOT &GENRD) AND (NOT &GENMT) AND (NOT &MC(25))).MFESVC 00452000
         AIF   ('&TEXT' EQ '^').MFE1                                    00453000
         AIF   ('&TEXT'(1,1) EQ '(').MFE2   TEXT=REG ?                  00454000
         LA    15,&TEXT                 R15 -> TEXT                     00455000
         ST    15,4(,1)                 STORE TEXT ADDR INTO XWPL       00456000
         AGO   .MFE1                                                    00457000
.MFE2    ST    &TEXT(1),4(,1)           STORE TEXT ADDR INTO XWPL       00458000
.MFE1    ANOP                                                           00459000
         LA    14,0(,1)                                                 00460000
         AH    14,0(,14)            ADD L'TEXT + LEN FIELD + MCS FIELD  00461000
         AIF   ('&TEXT' EQ '^').MFE3    NEED TO SET EXTENDED MCS ?      00462000
         OI    4(14),B'&MC(17)&MC(18)&MC(19)&MC(20)&MC(21)&MC(22)&MC(23X00463000
               )&MC(24)'                EXTENDED MCS                    00464000
         OI    5(14),B'&MC(25)&MC(26)&MC(27)&MC(28)&MC(29)&MC(30)&MC(31X00465000
               )&MC(32)'                EXTENDED MCS                    00466000
.MFE3    AIF   (NOT &GENRD).MFE4        NEED TO GEN ROUTE/DESC ?        00467000
         MVI   20(14),B'&DE(1)&DE(2)&DE(3)&DE(4)&DE(5)&DE(6)&DE(7)&DE(8X00468000
               )'                       DESCRIPTOR CODES                00469000
         MVI   21(14),B'&DE(9)&DE(10)&DE(11)&DE(12)&DE(13)&DE(14)&DE(15X00470000
               )&DE(16)'                DESCRIPTOR CODES                00471000
         MVI   24(14),B'&RO(1)&RO(2)&RO(3)&RO(4)&RO(5)&RO(6)&RO(7)&RO(8X00472000
               )'                       ROUTING CODES                   00473000
         MVI   25(14),B'&RO(9)&RO(10)&RO(11)&RO(12)&RO(13)&RO(14)&RO(15X00474000
               )&RO(16)'                ROUTING CODES                   00475000
.MFE4    AIF   (NOT &GENMT).MFESVC      NEED TO GEN MSGTYP FLAGS ?      00476000
         MVI   40(14),B'&MT(1)&MT(2)&MT(3)&MT(4)&MT(5)&MT(6)&MT(7)&MT(8X00477000
               )'                       MSGTYP                          00478000
         MVI   41(14),B'&MT(9)&MT(10)&MT(11)&MT(12)&MT(13)&MT(14)&MT(15X00479000
               )&MT(16)'                MSGTYP                          00480000
.MFESVC  ANOP                                                           00481000
         SVC   35                                                       00482000
         AGO   .END                 EXIT MACRO, PROCESSING COMPLETE     00483000
.*                                                                      00484000
.*       MF=I AND MF=L PROCESSING                                       00485000
.*                                                                      00486000
.WPLGEN  ANOP                                                           00487000
.*       TEXT FOR PRESENCE OF INLINE TEXT                               00488000
         AIF   (((N'&SYSLIST EQ 0) OR (N'&SYSLIST GT 255)) AND         X00489000
               ('&TEXT' EQ '^')).NOTXT                                  00490000
         AIF   ('&MF' EQ 'L').DCNAME                                    00491000
.*       MF=I PROCESSING                                                00492000
         CNOP  0,4                                                      00493000
&NAME    BAL   1,&GNAME.A                                               00494000
         AGO   .MESCHK                                                  00495000
.DCNAME  ANOP                                                           00496000
&NAME    DC    0F'0'                                                    00497000
.MESCHK  ANOP                                                           00498000
         AIF   (&GENXWPL).XWPL01         GENERATE XWPL                  00499000
         AIF   ('&MESG' EQ '').NOTXT     TEXT SPECIFIED ? NO, ERROR     00500000
.*       TEST FOR MULTI-LINE WTO                                        00501000
&MLW     SETB  ((N'&SYSLIST NE 1) OR (N'&SYSLIST(1) NE 1))              00502000
&MC(10)  SETB  (&MLW)                    SET MLWTO IF APPROPRIATE       00503000
         AIF   (NOT &MLW).HDCYOK                                        00504000
         AIF   (NOT &MC(7)).HDCYOK                                      00505000
         IHBERMAC 248           MCSFLAG=HRDCPY INVALID FOR MLWTO        00506000
&GNAME.A DS    0H                                                       00507000
         MEXIT                  TERMINATE MACRO                         00508000
.HDCYOK  ANOP                                                           00509000
.GENDCS  AIF   (NOT &MLW).NOTMLW1  GENERATE REGULAR WTO                 00510000
.********************************************************************** 00511000
.*                                                                      00512000
.*       SETUP TO GENERATE MLWTO                                        00513000
.*                                                                      00514000
.********************************************************************** 00515000
.*                                                                      00516000
.*       SET LINETYPE                                                   00517000
.*                                                                      00518000
&H       SETA  1                                                        00519000
         AIF   ('&SYSLIST(1,1)' EQ '').MLW04                            00520000
         AIF   (N'&SYSLIST(1) GT 2).MLW05                               00521000
         AIF   ('&SYSLIST(1,2)' NE 'C').MLW02                           00522000
&LT(1)   SETA  80                                                       00523000
.MLW01   AIF   (N'&SYSLIST LE &H).MLW11                                 00524000
&H       SETA  &H+1                                                     00525000
         AIF   (N'&SYSLIST(&H) GT 2).MLW05                              00526000
.MLW02   AIF   ('&SYSLIST(&H,2)' NE 'L' OR '&SYSLIST(&H,1)' EQ '').MLW0X00527000
               4                                                        00528000
&LT(&H)  SETA  40                                                       00529000
         AIF   (&SECONDL).MLW03                                         00530000
&SECONDL SETB  1                                                        00531000
         AGO   .MLW01                                                   00532000
.MLW03   AIF   (N'&SYSLIST LE &H).MLW11                                 00533000
&H       SETA  &H+1                                                     00534000
         AIF   (N'&SYSLIST(&H) GT 2).MLW05                              00535000
.MLW04   AIF   ('&SYSLIST(&H,2)' EQ 'E').MLW06                          00536000
         AIF   ('&SYSLIST(&H,1)' EQ '').MLW05                           00537000
         AIF   ('&SYSLIST(&H,2)' EQ 'DE').MLW08                         00538000
         AIF   ('&SYSLIST(&H,2)' EQ 'L' OR '&SYSLIST(&H,2)' EQ 'C').MLWX00539000
               09                                                       00540000
         AIF   ('&SYSLIST(&H,2)' NE 'D' AND '&SYSLIST(&H,2)' NE '').MLW*00541000
               10                                                       00542000
&LT(&H)  SETA  20                                                       00543000
         AGO   .MLW03                                                   00544000
.MLW05   ANOP                                                           00545000
&E5      SETB  1                                                        00546000
&LT(&H)  SETA  10                                                       00547000
         AGO   .MLW11                                                   00548000
.MLW06   ANOP                                                           00549000
&LT(&H)  SETA  10                                                       00550000
.MLW07   ANOP                                                           00551000
&E4      SETB  (&H NE N'&SYSLIST)                                       00552000
         AGO   .MLW11                                                   00553000
.MLW08   ANOP                                                           00554000
&LT(&H)  SETA  30                                                       00555000
         AGO   .MLW07                                                   00556000
.MLW09   ANOP                                                           00557000
&E3      SETB  1                                                        00558000
&LT(&H)  SETA  30                                                       00559000
         AGO   .MLW11                                                   00560000
.MLW10   ANOP                                                           00561000
&E5      SETB  1                                                        00562000
&LT(&H)  SETA  30                                                       00563000
.MLW11   ANOP                                                           00564000
&LLCNT   SETA  &H                                                       00565000
&H       SETA  1                                                        00566000
.********************************************************************** 00567000
.*                                                                      00568000
.*       GENERATE START OF WPL                                          00569000
.*                                                                      00570000
.********************************************************************** 00571000
.NOTMLW1 ANOP                                                           00572000
&I       SETA  1                                                        00573000
&LEN     SETA  K'&SYSLIST(1,1)-2                                        00574000
&PAIR    SETB  0                                                        00575000
.QLOOP1  ANOP                                                           00576000
&I       SETA  &I+1+&PAIR                                               00577000
         AIF   (&I GE K'&SYSLIST(1,1)).QDONE1                           00578000
&PAIR    SETB  ('&SYSLIST(1,1)'(&I,2) EQ '''''' OR '&SYSLIST(1,1)'(&I,2X00579000
               ) EQ '&&')                                               00580000
&LEN     SETA  &LEN-&PAIR                                               00581000
         AGO   .QLOOP1                                                  00582000
.QDONE1  ANOP                                                           00583000
&AD      SETB  (&LT(1) NE 10)      0 IF E-TYPE LINE, 1 IF NOT           00584000
&LEN     SETA  4+&LEN*&AD                                               00585000
         DC    AL2(&LEN)               TEXT LENGTH                      00586000
         DC    B'&MC(1)&MC(2)&MC(3)&MC(4)&MC(5)&MC(6)&MC(7)&MC(8)&MC(9)X00587000
               &MC(10)&MC(11)&MC(12)&MC(13)&MC(14)&MC(15)&MC(16)'      X00588000
                                       MCS FLAGS                        00589000
         AIF   (&LEN EQ 4).SKIPDC                                       00590000
         DC    C&SYSLIST(1,1)          MESSAGE TEXT                     00591000
.SKIPDC  AIF   (NOT &GENRD).OLDEXIT    GENERATING ROUT/DESC/MSGTYP ?    00592000
         DC    B'&DE(1)&DE(2)&DE(3)&DE(4)&DE(5)&DE(6)&DE(7)&DE(8)&DE(9)X00593000
               &DE(10)&DE(11)&DE(12)&DE(13)&DE(14)&DE(15)&DE(16)'      X00594000
                                       DESCRIPTOR CODES                 00595000
         DC    B'&RO(1)&RO(2)&RO(3)&RO(4)&RO(5)&RO(6)&RO(7)&RO(8)&RO(9)X00596000
               &RO(10)&RO(11)&RO(12)&RO(13)&RO(14)&RO(15)&RO(16)'      X00597000
                                       ROUTING CODES                    00598000
         AIF   (NOT &GENMT).OLDEXIT    GENERATING MSGTYP ?              00599000
         DC    B'&MT(1)&MT(2)&MT(3)&MT(4)&MT(5)&MT(6)&MT(7)&MT(8)&MT(9)X00600000
               &MT(10)&MT(11)&MT(12)&MT(13)&MT(14)&MT(15)&MT(16)'      X00601000
                                       MSGTYP                           00602000
                                                                        00603000
.OLDEXIT AIF   (NOT &MLW).NOTMLW2                                       00604000
.********************************************************************** 00605000
.*                                                                      00606000
.*       GENERATE MLWTO FIELDS                                          00607000
.*                                                                      00608000
.********************************************************************** 00609000
         DC    XL2'&LT(1)00'           LINE TYPE                        00610000
         AIF   ('&AREAID' EQ '').ID0                                    00611000
         DC    CL1'&AREAID'            AREA ID                          00612000
         AGO   .IDA                                                     00613000
.ID0     DC    X'00'                   NULL AREA ID                     00614000
.IDA     ANOP                                                           00615000
         DC    AL1(&LLCNT)             TOTAL NUMBER OF LINES            00616000
.MLW12   AIF   (&H GE &LLCNT).MLW14                                     00617000
&H       SETA  &H+1                                                     00618000
&I       SETA  1                                                        00619000
&LEN     SETA  K'&SYSLIST(&H,1)-2                                       00620000
&PAIR    SETB  0                                                        00621000
.QLOOPH  ANOP                                                           00622000
&I       SETA  &I+1+&PAIR                                               00623000
         AIF   (&I GE K'&SYSLIST(&H,1)).QDONEH                          00624000
&PAIR    SETB  ('&SYSLIST(&H,1)'(&I,2) EQ '''''' OR '&SYSLIST(&H,1)'(&IX00625000
               ,2) EQ '&&')                                             00626000
&LEN     SETA  &LEN-&PAIR                                               00627000
         AGO   .QLOOPH                                                  00628000
.QDONEH  ANOP                                                           00629000
&AD      SETB  (&LT(&H) NE 10)     0 IF E-TYPE LINE, 1 IF NOT           00630000
&LEN     SETA  4+&LEN*&AD                                               00631000
         DC    AL2(&LEN)               LENGTH                           00632000
         DC    XL2'&LT(&H)00'          LINE TYPE                        00633000
         AIF   (&LEN EQ 4).MLW12                                        00634000
         DC    C&SYSLIST(&H,1)                                          00635000
         AGO   .MLW12                                                   00636000
.MLW14   AIF   (NOT &E4).MLW15                                          00637000
         IHBERMAC 242         GEN TERMINATED BY E OR DE LINE TYPE       00638000
.MLW15   AIF   (NOT &E5).MLW17                                          00639000
         IHBERMAC 243                                                   00640000
.MLW17   AIF   (NOT &E3).NOTMLW2                                        00641000
         IHBERMAC 244                                                   00642000
.NOTMLW2 AIF   ('&MF' NE 'I').END                                       00643000
&GNAME.A DS    0H                                                       00644000
.NOTMLW3 ANOP                                                           00645000
         AIF   (&E6 OR &E3 OR &IHBWTL).END                              00646000
         SVC   035                                                      00647000
         MEXIT                                                          00648000
.********************************************************************** 00649000
.*                                                                      00650000
.*       GENERATE XWPL                                                  00651000
.*                                                                      00652000
.********************************************************************** 00653000
.XWPL01  ANOP                                                           00654000
         AIF   ('&TEXT' NE '^').XWPL04     HAVE MESSAGE TEXT TO PROCESS 00655000
&I       SETA  1                                                        00656000
&LEN     SETA  K'&SYSLIST(1,1)-2    L'TEXT                              00657000
&PAIR    SETB  0                                                        00658000
.*       LOOP THROUGH TEXT STRING AND PROCESS DUPLICATE QUOTES          00659000
.*       AND AMPERSANDS                                                 00660000
.XWPL02  ANOP                                                           00661000
&I       SETA  &I+1+&PAIR                                               00662000
         AIF   (&I GE K'&SYSLIST(1,1)).XWPL03   GOTO LOOP EXIT          00663000
&PAIR    SETB  ('&SYSLIST(1,1)'(&I,2) EQ '''''' OR '&SYSLIST(1,1)'(&I,2X00664000
               ) EQ '&&')                                               00665000
&LEN     SETA  &LEN-&PAIR                                               00666000
         AGO   .XWPL02                 EXIT FROM LOOP                   00667000
.*       GENERATE XWPL FIELDS                                           00668000
.XWPL03  ANOP                                                           00669000
.*       GENERATE TEXT XWPL VERSION 1                                   00670000
&LEN     SETA  4+&LEN                                                   00671000
         AIF   (&LEN EQ 4).NOTXT                                        00672000
         DC    AL2(&LEN)                TEXT LENGTH                     00673000
         DC    B'&MC(1)&MC(2)&MC(3)&MC(4)&MC(5)&MC(6)&MC(7)&MC(8)&MC(9)X00674000
               &MC(10)&MC(11)&MC(12)&MC(13)&MC(14)&MC(15)&MC(16)'      X00675000
                                        MCS FLAGS                       00676000
         DC    C&SYSLIST(1,1)           MESSAGE TEXT                    00677000
         DC    AL1(1)                   XWPL VERSION LEVEL              00678000
         AGO   .XWPL05                                                  00679000
.*       GENERATE TEXT ADDR XWPL VERSION 2                              00680000
.XWPL04  ANOP                                                           00681000
         DC    AL2(8)                   TEXT LENGTH                     00682000
         DC    B'&MC(1)&MC(2)&MC(3)&MC(4)&MC(5)&MC(6)&MC(7)&MC(8)&MC(9)X00683000
               &MC(10)&MC(11)&MC(12)&MC(13)&MC(14)&MC(15)&MC(16)'      X00684000
                                        MCS FLAGS                       00685000
         DC    AL4(0)                   ADDR OF MESSAGE TEXT            00686000
         DC    AL1(2)                   XWPL VERSION LEVEL              00687000
.*       GENERATE FIELDS COMMON TO XWPL VER 1 AND XWPL VER 2            00688000
.XWPL05  ANOP                                                           00689000
         DC    B'00000000'              MISCELLANEOUS FLAGS             00690000
         DC    AL1(0)                   L'REPLY FOR WTOR                00691000
         AIF   (&WPLXLEV EQ 2).XWPL06   DETERMINE LEVEL OF XWPL         00692000
.*       XWPL LENGTH VALUE FOR XWPL VERSION 1                           00693000
         DC    AL1(0)                   RESERVED                        00694000
         AGO   .XWPL07                                                  00695000
.*       XWPL LENGTH VALUE FOR XWPL VERSION 2                           00696000
.XWPL06  ANOP                                                           00697000
         DC    AL1(104)                 LENGTH OF XWPL VERSION 2        00698000
.XWPL07  ANOP                                                           00699000
.*       GENERATE EXTENDED MCS FLAGS                                    00700000
         DC    B'&MC(17)&MC(18)&MC(19)&MC(20)&MC(21)&MC(22)&MC(23)&MC(2X00701000
               4)&MC(25)&MC(26)&MC(27)&MC(28)&MC(29)&MC(30)&MC(31)&MC(3X00702000
               2)'                      EXTENDED MCS FLGS               00703000
         DC    XL2'0000'                MCS FLAGS FOR CNTL PROGRAM USE  00704000
         DC    AL4(0)                   ADDR OF REPLY BUFFER FOR WTOR   00705000
         DC    AL4(0)                   ADDR OF REPLY ECB FOR WTOR      00706000
         DC    AL4(0)                   RESERVED - CONNECT ID           00707000
         DC    B'&DE(1)&DE(2)&DE(3)&DE(4)&DE(5)&DE(6)&DE(7)&DE(8)&DE(9)X00708000
               &DE(10)&DE(11)&DE(12)&DE(13)&DE(14)&DE(15)&DE(16)'      X00709000
                                        DESCRIPTOR CODES                00710000
         DC    AL2(0)                   RESERVED                        00711000
         DC    B'&RO(1)&RO(2)&RO(3)&RO(4)&RO(5)&RO(6)&RO(7)&RO(8)&RO(9)X00712000
               &RO(10)&RO(11)&RO(12)&RO(13)&RO(14)&RO(15)&RO(16)'      X00713000
                                        EXTENDED ROUTING CODES          00714000
         DC    B'&RO(17)&RO(18)&RO(19)&RO(20)&RO(21)&RO(22)&RO(23)&RO(2X00715000
               4)&RO(25)&RO(26)&RO(27)&RO(28)&RO(29)&RO(30)&RO(31)&RO(3X00716000
               2)'                                                      00717000
         DC    B'&RO(33)&RO(34)&RO(35)&RO(36)&RO(37)&RO(38)&RO(39)&RO(4X00718000
               0)&RO(41)&RO(42)&RO(43)&RO(44)&RO(45)&RO(46)&RO(47)&RO(4X00719000
               8)'                                                      00720000
         DC    B'&RO(49)&RO(50)&RO(51)&RO(52)&RO(53)&RO(54)&RO(55)&RO(5X00721000
               6)&RO(57)&RO(58)&RO(59)&RO(60)&RO(61)&RO(62)&RO(63)&RO(6X00722000
               4)'                                                      00723000
         DC    B'&RO(65)&RO(66)&RO(67)&RO(68)&RO(69)&RO(70)&RO(71)&RO(7X00724000
               2)&RO(73)&RO(74)&RO(75)&RO(76)&RO(77)&RO(78)&RO(79)&RO(8X00725000
               0)'                                                      00726000
         DC    B'&RO(81)&RO(82)&RO(83)&RO(84)&RO(85)&RO(86)&RO(87)&RO(8X00727000
               8)&RO(89)&RO(90)&RO(91)&RO(92)&RO(93)&RO(94)&RO(95)&RO(9X00728000
               6)'                                                      00729000
         DC    B'&RO(97)&RO(98)&RO(99)&RO(100)&RO(101)&RO(102)&RO(103)&X00730000
               RO(104)&RO(105)&RO(106)&RO(107)&RO(108)&RO(109)&RO(110)&X00731000
               RO(111)&RO(112)'                                         00732000
         DC    B'&RO(113)&RO(114)&RO(115)&RO(116)&RO(117)&RO(118)&RO(11X00733000
               9)&RO(120)&RO(121)&RO(122)&RO(123)&RO(124)&RO(125)&RO(12X00734000
               6)&RO(127)&RO(128)'                                      00735000
.*                                                                      00736000
         DC    B'&MT(1)&MT(2)&MT(3)&MT(4)&MT(5)&MT(6)&MT(7)&MT(8)&MT(9)X00737000
               &MT(10)&MT(11)&MT(12)&MT(13)&MT(14)&MT(15)&MT(16)'      X00738000
                                        MSGTYPE                         00739000
         DC    AL2(0)                   MESSAGE PRIORITY                00740000
         DC    CL8'        '            JOB ID                          00741000
         DC    CL8'        '            JOB NAME                        00742000
         DC    CL8'        '            RETRIEVAL KEY                   00743000
         DC    AL4(0)                   TOKEN FOR DOM                   00744000
         DC    AL4(0)                   CONSOLE ID                      00745000
         DC    CL8'        '            SYSTEM NAME                     00746000
         AIF   (&WPLXLEV EQ 1).XWPL08   BRANCH TO GENERATE XWPL VER 1   00747000
         DC    CL8'        '            CONSOLE NAME                    00748000
         DC    AL4(0)                   REPLY CONSOLE NAME/ID ADDR      00749000
         DC    AL4(0)                   CART ADDRESS                    00750000
         DC    AL4(0)                   WSPARM ADDRESS                  00751000
         AIF   ('&MF' EQ 'L').NOTMLW2                                   00752000
.*       STORE ADDR OF TEXT INTO XWPL (VERSION 2 XWPL ONLY)             00753000
.*       GENERATE LABEL FOR BAL BRANCH                                  00754000
&GNAME.A DS    0H                                                       00755000
         AIF   ('&TEXT' EQ '^').NOTMLW3  SHOULD NOT HAPPEN              00756000
         AIF   ('&TEXT'(1,1) EQ '(').XWPL10   TEXT=(REG) ?              00757000
         LA    15,&TEXT                 R15 -> TEXT                     00758000
         ST    15,4(,1)                 STORE TEXT ADDR INTO XWPL       00759000
         AGO   .NOTMLW3                                                 00760000
.XWPL10  ANOP                                                           00761000
         ST    &TEXT(1),4(,1)           STORE TEXT ADDR INTO XWPL       00762000
         AGO   .NOTMLW3                                                 00763000
.*       XWPL VERSION 1 FIELDS                                          00764000
.XWPL08  ANOP                                                           00765000
         DC    AL4(0)                   RESERVED                        00766000
         DC    AL4(0)                   RESERVED                        00767000
         AGO   .NOTMLW2                                                 00768000
.********************************************************************** 00769000
.*       ISSUE ERROR MESSAGE - 12,INVALID MF OPERAND SPECIFIED-MF       00770000
.********************************************************************** 00771000
         IHBERMAC 35,,&MF                                               00772000
.ERROR1  ANOP                                                           00773000
         MEXIT                                                          00774000
.NOTXT   ANOP                                                           00775000
         MNOTE 12,'NUMBER OF LINES REQUESTED IS 0 OR GREATER THAN 255 -X00776000
                GENERATION TERMINATED'                                  00777000
.END     MEND                                                           00778000
./ ADD NAME=WTOR
         MACRO                                                          00001000
&NAME    WTOR  &MESG,&RYAD,&LENGTH,&ECB,&MF=I,&MSGTYP=,&ROUTCDE=,      X00002000
               &DESC=,&MCSFLAG=,&QID=,&TEXT=^                           00003000
.*                                                                      00004000
.********************************************************************** 00005000
.*                                                                      00006000
.*       WTOR - WRITE TO OPERATOR WITH REPLY                            00007000
.*                                                                      00008000
.*       INVOCATION - SPECIFY WTOR MESG,RYAD,LENGTH,ECB,ROUTCDE=,       00009000
.*                                 DESC=,MSGTYP=,MCSFLAG=,MF=,          00010000
.*                                 TEXT=(TEXTADDR,RYAD,LENGTH,ECB)      00011000
.*              WHERE:                                                  00012000
.*                MESG     THE MESSAGE TEXT FOR A SINGLE LINE MESSAGE   00013000
.*                         TO BE WRITTEN TO A CONSOLE                   00014000
.*                                                                      00015000
.*                RYAD     THE ADDRESS OF THE REPLY TEXT                00016000
.*                                                                      00017000
.*                LENGTH   MAXIMUM LENGTH OF THE REPLY TEXT FIELD       00018000
.*                                                                      00019000
.*                ECB      THE ADDRESS OF THE ECB TO BE POSTED ON       00020000
.*                         COMPLETION OF ENTRY OF THE REPLY TEXT        00021000
.*                                                                      00022000
.*                ROUTCDE= ROUTING CODES TO BE ASSIGNED TO THE          00023000
.*                         MESSAGE                                      00024000
.*                                                                      00025000
.*                DESC=    DESCRIPTOR CODES TO BE ASSIGNED TO           00026000
.*                         THE MESSAGE                                  00027000
.*                                                                      00028000
.*                MSGTYP=  SPECIFIES HOW THE MESSAGE IS TO BE           00029000
.*                         ROUTED. VALID VALUES ARE -                   00030000
.*                         N,Y,SESS,JOBNAMES,STATUS,ACTIVE,SHOW         00031000
.*                                                                      00032000
.*                MCSFLAG= SPECIFIES ATTRIBUTES OF THE                  00033000
.*                         MESSAGE. VALID VALUES ARE -                  00034000
.*                         REG0,RESP,REPLY,BRDCST,HRDCPY,               00035000
.*                         QREG0,NOTIME,NOCPY                           00036000
.*                                                                      00037000
.*                QID=     REMOTE ENTRY SERVICES (RSS) QUEUE            00038000
.*                         ID (OS/VS1 ONLY). PARAMETER ACCEPTED         00039000
.*                         BUT IGNORED BY MVS                           00040000
.*                                                                      00041000
.*                MF=      SPECIFIES THE TYPE OF EXPANSION              00042000
.*                         REQUIRED. VALID VALUES ARE -                 00043000
.*                         I  -  DEFAULT VALUE                          00044000
.*                         L                                            00045000
.*                         (E,[ADDR OF WPL])                            00046000
.*                         (E,[ADDR OF WPL],EXTENDED)                   00047000
.*                         THE THIRD MF PARAMETER MUST BE USED          00048000
.*                         WHEN THE TARGET WPL IS AN XWPL AND           00049000
.*                         THE WTOR REPLY FIELDS ARE PROVIDED TO        00050000
.*                         UPDATE THE TARGET XWPL                       00051000
.*                                                                      00052000
.*                TEXT=    FOR A WTOR THE FIRST PARAMETER               00053000
.*                         SPECIFIES A DATA AREA CONTAINING A 2         00054000
.*                         BYTE MESSAGE LENGTH FIELD FOLLOWED BY        00055000
.*                         THE ACTUAL MESSAGE TEXT. THE                 00056000
.*                         ADDITIONAL POSITIONAL PARAMETERS,            00057000
.*                         RYAD, LENGTH AND ECB ADDRESS MUST BE         00058000
.*                         SPECIFIED AS THE SECOND, THIRD AND           00059000
.*                         FOURTH POSITIONAL PARAMETERS ON THE          00060000
.*                         TEXT KEYWORD. REGISTER NOTATION CAN BE       00061000
.*                         USED FOR ALL THE FIELDS. IF USED, THE        00062000
.*                         REGISTER CONTAINS THE ADDRESS OF THE         00063000
.*                         DATA AREA. IF A FIELD IS USED, THE           00064000
.*                         ADDRESS OF THE FIELD IS STORED IN THE        00065000
.*                         EXTENDED WPL. THE TEXT KEYWORD IS            00066000
.*                         MUTUALLY EXCLUSIVE WITH INLINE MESSAGE       00067000
.*                         TEXT. EITHER TEXT OR INLINE TEXT (BUT        00068000
.*                         NOT BOTH) IS REQUIRED ON THE STANDARD        00069000
.*                         OR LIST FORM OF WTOR.                        00070000
.*                                                                      00071000
.*       FUNCTION - BUILDS ONE OF THREE POSSIBLE PARAMETER LIST         00072000
.*                  FORMATS AND/OR THE CODE WHICH WILL INVOKE           00073000
.*                  SVC 35 TO ISSUE THE MESSAGE.                        00074000
.*                  1. A STANDARD WPL IS BUILT IF INLINE TEXT IS        00075000
.*                     SPECIFIED AND NO ROUTING CODE GREATER THAN 16    00076000
.*                     IS SPECIFIED.                                    00077000
.*                  2. IF INLINE TEXT IS SPECIFIED AND A ROUTING CODE   00078000
.*                     GREATER THAN 16 IS SPECIFIED THEN AN EXTENDED    00079000
.*                     WPL VERSION 1 IS BUILT.                          00080000
.*                  3. IF THE TEXT OPERAND IS SPECIFIED THEN AN         00081000
.*                     EXTENDED WPL VERSION 2 IS GENERATED OR           00082000
.*                     ASSUMED AS THE TARGET OF AN MF=E SPECIFICATION.  00083000
.*                     THE ADDITIONAL POSITIONAL PARAMETERS,            00084000
.*                     RYAD, LENGTH AND ECB ADDRESS, MUST BE            00085000
.*                     SPECIFIED AS THE SECOND, THIRD AND               00086000
.*                     FOURTH POSITIONAL PARAMETERS ON THE              00087000
.*                     TEXT KEYWORD.                                    00088000
.*                     IF MF=(E,TARGET.... IS SPECIFIED, USE OF THE     00089000
.*                     MF=(E,TARGET,EXTENDED) KEYWORD IS OPTIONAL AS    00090000
.*                     SPECIFYING THE TEXT PARAMETER ASSUMES THE        00091000
.*                     USE OF AN EXTENDED WPL VERSION 2.                00092000
.*                                                                      00093000
.*       STATUS -                                                       00094000
.*       LASTUPD         = EBB1102  TYPE=ADD                            00095000
.*       LIBRARIES       = DISTLIB=AMACLIB                              00096000
.*       FMID            = EBB1102                                      00097000
.*       RMID            = EBB1102                                      00098000
.*       SYSMOD HISTORY  = SYSMOD   TYPE       DATE   MCS  STATUS       00099000
.*                         EBB1102  FUNCTION  00.000  MAC  ACC RGN      00100000
.*                                                                      00101000
.*       CHANGE ACTIVITY -                                              00102000
.*              ZP600040 - EXTENSIVE REVISION OF THE MACRO AND          00103000
.*                         SUPPORT FOR THE TEXT OPERAND WHICH           00104000
.*                         REQUIRES GENERATION OF AN EXTENDED WPL       00105000
.*                                                                      00106000
.********************************************************************** 00107000
         LCLA  &H,&I,&II,&N,&J,&K,&LEN,&LLCNT                           00108000
         LCLA  &NUMENT                 NUMBER OF SUBLIST ENTRIES        00109000
         LCLA  &LENENT                 L'ENTRY                          00110000
         LCLB  &SECONDL,&PAIR                                           00111000
         LCLC  &GNAME,&WORKT,&WORKA,&WORKL,&WORKE                       00112000
         LCLB  &RO(128),&DE(16),&MC(32),&MT(16)  FLAGS FOR THE FIELDS   00113000
         LCLC  &LOWNUM,&HIGHNUM        ROUTE RANGE NUMBERS              00114000
         LCLB  &GENRD                  GENERATE ROUT AND DESC FIELDS    00115000
         LCLB  &GENMT                  GENERATE MSGTYP FIELD            00116000
         LCLB  &GENXWPL                GENERATE AN XWPL                 00117000
         LCLA  &WPLXLEV                LEVEL OF XWPL TO GENERATE        00118000
         LCLB  &MTY,&MTN               FLAGS FOR MSGTYP PROCESSING      00119000
         ACTR  30000                                                    00120000
.*                                                                      00121000
.*       INITIALIZATION                                                 00122000
.*                                                                      00123000
&GNAME   SETC  'IHB'.'&SYSNDX'         FOR BAL INSTRUCTION              00124000
&WPLXLEV SETA  1                                                        00125000
.********************************************************************** 00126000
.*                                                                      00127000
.*       BEGIN PROCESSING PARAMETERS                                    00128000
.*                                                                      00129000
.********************************************************************** 00130000
.********************************************************************** 00131000
.*                                                                      00132000
.*        PROCESS ROUTING CODES                                         00133000
.*        DETERMINE IF ROUTING CODES HAVE BEEN SPECIFIED                00134000
.*                                                                      00135000
.*        ROUTING CODES 1-128 MAY BE SET HOWEVER FOR MVS 3.8 ONLY       00136000
.*        ONLY ROUTING CODES 1-16 WILL BE ACTIONED                      00137000
.*        ANY ROUTING CODE GREATER THAN 16 WILL BE IGNORED              00138000
.*        WITHOUT ERROR HOWEVER SPECIFYING A ROUTING CODE GREATER       00139000
.*        THAN 16 WILL CAUSE THE GENERATION OF AN EXTENDED FORMAT       00140000
.*        VERSION 1 WPL (XWPL).                                         00141000
.*                                                                      00142000
.********************************************************************** 00143000
         AIF   (T'&ROUTCDE EQ 'O').ENDROUT   ROUTING CODES SPECIFIED ?  00144000
&GENRD   SETB  1                    ROUTING CODES PRESENT               00145000
&I       SETA  1                    INITIALIZE LIST INDEX               00146000
&NUMENT  SETA  N'&ROUTCDE           TOTAL NUMBER OF ENTRIES IN &ROUTCDE 00147000
.*       ROUTING CODE LOOP                                              00148000
.ROULOOP ANOP                                                           00149000
&LENENT  SETA  K'&ROUTCDE(&I)       SET L'CURRENT ELEMENT               00150000
         AIF   (&LENENT EQ 0).BADENT  NULL ENTRY, ERROR                 00151000
         AIF   (T'&ROUTCDE(&I) EQ 'N').ROUVAL   SINGLE NUMERIC VALUE    00152000
.*                                                                      00153000
.*       RANGE OF ROUTING CODES HAS BEEN SPECIFIED                      00154000
.*                                                                      00155000
.*       SCAN FOR DASH SEPERATOR                                        00156000
.*                                                                      00157000
&II      SETA  1                    INITIALIZE ELEMENT INDEX            00158000
.DASHL   ANOP                       DASH SCANNING LOOP                  00159000
         AIF   ('&ROUTCDE(&I)'(&II,1) EQ '-').DASHFND                   00160000
&II      SETA  &II+1                INCREMENT ELEMENT INDEX             00161000
         AIF   (&II GE &LENENT).BADRANG   CHECK INDEX POSITION          00162000
         AGO   .DASHL               LOOP AROUND FOR NEXT CHAR           00163000
.*       FOUND A DASH SEPERATOR                                         00164000
.DASHFND ANOP                                                           00165000
         AIF   (&II EQ 1).BADRANG         DASH UP FRONT ? ERROR         00166000
         AIF   (&II EQ &LENENT).BADRANG   DASH AT THE END ? ERROR       00167000
&LOWNUM  SETC  '&ROUTCDE(&I)'(1,&II-1) SET FIRST NUMBER OF RANGE        00168000
&HIGHNUM SETC  '&ROUTCDE(&I)'(&II+1,&LENENT-&II) SET SECOND NUMBER      00169000
.*       FIRST NUMBER > LAST NUMBER ? ERROR                             00170000
         AIF   ('&LOWNUM' GT '&HIGHNUM').BADRANG                        00171000
.*       OUTSIDE THE RANGE OF VALID ROUTING CODES ? ERROR               00172000
         AIF   ('&LOWNUM' LT '1' OR '&LOWNUM' GT '128' OR              X00173000
               '&HIGHNUM' LT '1' OR '&HIGHNUM' GT '128').BADRANG        00174000
&J       SETA  &LOWNUM              INITIALIZE START OF RANGE           00175000
&K       SETA  &HIGHNUM             INITIALIZE END OF RANGE             00176000
.*       LOOP TO SET ON ROUTING CODES                                   00177000
.*       IF ALREADY SET ON THEN ERROR                                   00178000
.RANLOOP ANOP                                                           00179000
         AIF   (&RO(&J)).BADRANG                                        00180000
&RO(&J)  SETB  1                    SET ROUTING CODE &J ON              00181000
         AIF   (&J LT 17).RANLOPA                                       00182000
&GENXWPL SETB  1                    ANY VALUE > 16 FORCES A XWPL        00183000
.RANLOPA ANOP                                                           00184000
&J       SETA  &J+1                 INCREMENT LOOP INDEX                00185000
         AIF   (&J LE &K).RANLOOP   LOOP THROUGH RANGE                  00186000
.*       RANGE PROCESSED                                                00187000
         AGO   .NEXTROU             DETERMINE NEXT ROUTING CODE         00188000
.*                                                                      00189000
.*       ROUTING CODE VALUE HAS BEEN SPECIFIED. VERIFY THAT IT IS       00190000
.*       WITHIN THE PROPER RANGE AND, IF SO, SET THE CORRECT BIT        00191000
.*                                                                      00192000
.ROUVAL  ANOP                                                           00193000
&J       SETA  &ROUTCDE(&I)         GET NEXT ROUTING CODE VALUE         00194000
         AIF   (&J GE 1 AND &J LE 128).ROUOK   VALID VALUE ?            00195000
         MNOTE 4,'ROUTCDE &J IS AN INVALID ROUTING CODE'                00196000
         AGO   .NEXTROU                                                 00197000
.ROUOK   ANOP                       VALID ROUTING CODE SPECIFIED        00198000
         AIF   (&RO(&J)).BADRANG                                        00199000
&RO(&J)  SETB  1                    SET ROUTING CODE &J ON              00200000
         AIF   (&J LT 17).NEXTROU                                       00201000
.*       ROUTCDE GT 16 DOES NOT FORCE XWPL IF MF=E                      00202000
         AIF   ('&MF(1)' EQ 'E').NEXTROU                                00203000
&GENXWPL SETB  1                    ANY VALUE > 16 FORCES A XWPL        00204000
&MC(12)  SETB  1                    SET MCS FLAG FOR XWPL (WPLMCSFL)    00205000
&MC(21)  SETB  1                    WTOR WITH EXT PARM    (WPXWTOR)     00206000
         AGO   .NEXTROU             LOOP TO NEXT VALUE                  00207000
.*       A NULL ROUTING CODE HAS BEEN SPECIFIED                         00208000
.*       ISSUE AN MNOTE AND SKIP TO THE NEXT ROUTING CODE               00209000
.BADENT  ANOP                                                           00210000
         MNOTE 4,'NULL ROUTING CODE SPECIFIED'                          00211000
         AGO   .NEXTROU                                                 00212000
.*       AN INVALID ROUTING CODE RANGE HAS BEEN SPECIFIED               00213000
.*       ISSUE AN MNOTE AND SKIP TO THE NEXT ROUTING CODE               00214000
.BADRANG ANOP                                                           00215000
         MNOTE 4,'INVALID RANGE OF ROUTING CODES IGNORED'               00216000
.*       FALL THROUGH TO SETUP FOR NEXT VALUE                           00217000
.*                                                                      00218000
.*       SETUP TO PROCESS NEXT ROUTE CODE ENTRY                         00219000
.NEXTROU ANOP                                                           00220000
&I       SETA  &I+1                 INCREMENT INDEX TO ROUT CODE VALUES 00221000
         AIF   (&I LE &NUMENT).ROULOOP IF MORE CODES, CHECK NEXT ONE    00222000
.ENDROUT ANOP                       DONE WITH ROUTING CODES             00223000
.********************************************************************** 00224000
.*                                                                      00225000
.*       PROCESS DESCRIPTOR CODES                                       00226000
.*       DETERMINE IF DESCRIPTOR CODES HAVE BEEN SPECIFIED              00227000
.*                                                                      00228000
.********************************************************************** 00229000
         AIF   (T'&DESC EQ 'O').ENDDESC   NO DESCRIPTOR, BRANCH         00230000
&NUMENT  SETA  N'&DESC           NUMBER OF ENTRIES                      00231000
&I       SETA  1                                                        00232000
&J       SETA  0                 COUNTER TO DETECT MUTUALLY EXCL VALUE  00233000
.DESCLOP ANOP                                                           00234000
&II      SETA  &DESC(&I)                                                00235000
         AIF   (&II GE 1 AND &II LE 16).DESCOK                          00236000
.DESCERR MNOTE 8,'DESC &N IS INVALID DESCRIPTOR - IGNORED'              00237000
         AGO   .NEXTDES                                                 00238000
.DESCOK  ANOP                                                           00239000
         AIF   (&DE(&II)).DESERR             ALREADY SPECIFIED          00240000
         AIF   (&II GT 6).DESCJMP                                       00241000
&J       SETA  &J+1                         A VALUE LOWER THAN 7 SPEC   00242000
.DESCJMP ANOP                                                           00243000
&DE(&II) SETB  1                                                        00244000
.NEXTDES ANOP                                                           00245000
&I       SETA  &I+1                         INCR ENTRY COUNTER          00246000
         AIF   (&I LE &NUMENT).DESCLOP      PROCESS NEXT ENTRY          00247000
.*       ALL ENTRIES PROCESSED                                          00248000
.*       TEST FOR ANY MUTUALLY EXCLUSIVE ENTRIES                        00249000
         AIF   (&J EQ 1).ENDDESC            ALL OK                      00250000
         MNOTE 8,'DESC MUTUALLY EXCLUSIVE VALUE SPECIFIED'              00251000
         MEXIT                                                          00252000
.DESERR  MNOTE 8,'DUPLICATE DESCRIPTOR VALUE SPECIFIED'                 00253000
         MEXIT                                                          00254000
.ENDDESC ANOP                                                           00255000
.********************************************************************** 00256000
.*                                                                      00257000
.*       PROCESS MSGTYP PARAMETERS                                      00258000
.*       DETERMINE IF MSGTYP PARAMETERS HAVE BEEN SPECIFIED             00259000
.*       PARAMETERS -                                                   00260000
.*       Y        - GENERATES MSGTYP FIELD BUT IGNORES OTHER            00261000
.*                  PARAMETERS (ALL MSGTYP FLAGS ZERO)                  00262000
.*                  THIS MUST BE THE FIRST PARAMETER                    00263000
.*       N        - SUPPESSSES THE GENERATION OF THE MSGTYP             00264000
.*                  FIELD EVEN THOUGH OTHER PARAMETERS HAVE BEEN        00265000
.*                  SPECIFIED                                           00266000
.*                  THIS MUST BE THE FIRST PARAMETER                    00267000
.*       JOBNAMES -                                                     00268000
.*       STATUS   -                                                     00269000
.*       ACTIVE   -                                                     00270000
.*       SHOW     - FOR VS/1 CRJE SUPPORT                               00271000
.*       SESS     -                                                     00272000
.*       NOTE THAT THE MSGTYP FIELD WILL BE GENERATED AND THE           00273000
.*       WPLMSGTD FLAG WILL BE SET IF THE QID PARAMETER IS              00274000
.*       CODED ON THE WTO MACRO                                         00275000
.*                                                                      00276000
.********************************************************************** 00277000
         AIF   (T'&MSGTYP EQ 'O').ENDMSGT   MSGTYP PARAMS SPECIFIED ?   00278000
&GENRD   SETB  1                            MSGTYPE CAUSED ROUT/DESC    00279000
&NUMENT  SETA  N'&MSGTYP                    NUMBER OF ENTRIES           00280000
         AIF   ('&MSGTYP(1)' NE 'N').MSGL1                              00281000
&MTN     SETB  1                            MSGTYP=N CODED              00282000
         AGO   .MSGL2                                                   00283000
.MSGL1   AIF   ('&MSGTYP(1)' NE 'Y').MSGL3  MSGTYP=Y CODED              00284000
&MTY     SETB  1                                                        00285000
.MSGL2   ANOP                               EITHER MSGTYP =Y/N CODED    00286000
&I       SETA  1                            FIRST PARAMETER PROCESSED   00287000
         AGO   .NEXTMSG                                                 00288000
.MSGL3   ANOP                                                           00289000
&I       SETA  0                                                        00290000
         AGO   .NEXTMSG                                                 00291000
.MSGTLOP ANOP                               LOOP THROUGH PARAMETERS     00292000
         AIF   ('&MSGTYP(&I)' NE 'JOBNAMES').MSGL4                      00293000
         AIF   (&MT(1)).MTERR               JOBNAMES ALREADY CODED ?    00294000
&MT(1)   SETB  1                            TURN ON JOBNAMES            00295000
         AGO   .NEXTMSG                                                 00296000
.MSGL4   ANOP                                                           00297000
         AIF   ('&MSGTYP(&I)' NE 'STATUS').MSGL5                        00298000
         AIF   (&MT(2)).MTERR               STATUS ALREADY CODED ?      00299000
&MT(2)   SETB  1                            TURN ON STATUS              00300000
         AGO   .NEXTMSG                                                 00301000
.MSGL5   ANOP                                                           00302000
         AIF   ('&MSGTYP(&I)' NE 'ACTIVE').MSGL6                        00303000
         AIF   (&MT(3)).MTERR               ACTIVE ALREADY CODED ?      00304000
&MT(3)   SETB  1                            TURN ON ACTIVE              00305000
         AGO   .NEXTMSG                                                 00306000
.MSGL6   ANOP                                                           00307000
         AIF   ('&MSGTYP(&I)' NE 'SHOW').MSGL7                          00308000
         AIF   (&MT(5)).MTERR               SHOW ALREADY CODED ?        00309000
&MT(5)   SETB  1                            TURN ON SHOW                00310000
         AGO   .NEXTMSG                                                 00311000
.MSGL7   ANOP                                                           00312000
         AIF   ('&MSGTYP(&I)' NE 'SESS').MTERR                          00313000
         AIF   (&MT(6)).MTERR               SESS ALREADY CODED ?        00314000
&MT(6)   SETB  1                            TURN ON SESS                00315000
.NEXTMSG ANOP                                                           00316000
&GENMT   SETB  1                            GENERATE MSGTYP FIELD       00317000
&I       SETA  &I+1                         INCR ENTRY COUNTER          00318000
         AIF   (&I LE &NUMENT).MSGTLOP      PROCESS NEXT ENTRY          00319000
.*       ALL ENTRIES PROCESSED                                          00320000
.*       PROCESS FLAGS                                                  00321000
&GENMT   SETB  (NOT &MTN)                   SUPRESS MT GEN IF MSGTYP=N  00322000
         AIF   (&MTN OR &MTY).MSGL8                                     00323000
         AGO   .ENDMSGT                                                 00324000
.*       ERROR PROCESSING                                               00325000
.MTERR   MNOTE 8,'MESSAGE TYPE FIELD INVALID - N IS ASSUMED'            00326000
&MTN     SETB  1                                                        00327000
&MTY     SETB  0                                                        00328000
.MSGL8   ANOP                               TURN OFF ALL FLAGS          00329000
&MT(1)   SETB  0                                                        00330000
&MT(2)   SETB  0                                                        00331000
&MT(3)   SETB  0                                                        00332000
&MT(5)   SETB  0                                                        00333000
&MT(6)   SETB  0                                                        00334000
.ENDMSGT ANOP                                                           00335000
.********************************************************************** 00336000
.*                                                                      00337000
.*       PROCESS MCSFLAG PARAMETERS                                     00338000
.*       DETERMINE IF MCSFLAG PARAMETERS HAVE BEEN SPECIFIED            00339000
.*       PARAMETERS -                                                   00340000
.*       REG0     - MSG TO CONSOLE WITH SOURCE ID IN R0                 00341000
.*       RESP     - WTO IS IMMEDIATE COMMAND RESPONSE                   00342000
.*       REPLY    - WTO IS A REPLY TO A WTOR                            00343000
.*       BRDCAST  - MSG TO BE BROADCAST TO ALL ACTIVE CONSOLES          00344000
.*       HRDCOPY  - MSG TO BE QUEUED FOR HARDCOPY ONLY                  00345000
.*       QREG0    - MSG TO BE QUEUED UNCONDITIONALLY TO CON ID IN R0    00346000
.*       NOTIME   - TIME IS NOT TO BE APPENDED TO MSG  TER              00347000
.*       NOCPY    - IF WTO OR WTOR IS ISSUED IN SUPERVISOR STATE        00348000
.*                  THEN DO NOT QUEUE FOR HARDCOPY                      00349000
.*                                                                      00350000
.********************************************************************** 00351000
         AIF   (T'&MCSFLAG EQ 'O').ENDMCS    MCSFLAG PARAMS SPECIFIED ? 00352000
&I       SETA  1                    SET LIST INDEX                      00353000
&NUMENT  SETA  N'&MCSFLAG           NUMBER OF ENTRIES                   00354000
.MCLOOP  ANOP                       START LOOP THROUGH PARAMETERS       00355000
         AIF   ('&MCSFLAG(&I)' NE 'REG0').MCL1                          00356000
         AIF   (&MC(2)).MCERR       REG0 ALREADY CODED ?                00357000
&MC(2)   SETB  1                    TURN ON REG0 (WPLMCSFB)             00358000
         AGO   .NEXTMC                                                  00359000
.MCL1    ANOP                                                           00360000
         AIF   ('&MCSFLAG(&I)' NE 'RESP').MCL2                          00361000
         AIF   (&MC(3)).MCERR       RESP ALREADY CODED ?                00362000
&MC(3)   SETB  1                    TURN ON RESP (WPLMCSFC)             00363000
         AGO   .NEXTMC                                                  00364000
.MCL2    ANOP                                                           00365000
         AIF   ('&MCSFLAG(&I)' NE 'REPLY').MCL3                         00366000
         AIF   (&MC(5)).MCERR       REPLYG0 ALREADY CODED ?             00367000
&MC(5)   SETB  1                    TURN ON REPLY (WPLMCSFE)            00368000
         AGO   .NEXTMC                                                  00369000
.MCL3    ANOP                                                           00370000
         AIF   ('&MCSFLAG(&I)' NE 'BRDCAST').MCL4                       00371000
         AIF   (&MC(6)).MCERR       BRDCAST ALREADY CODED ?             00372000
&MC(6)   SETB  1                    TURN ON BRDCAST (WPLMCSFF)          00373000
         AGO   .NEXTMC                                                  00374000
.MCL4    ANOP                                                           00375000
         AIF   ('&MCSFLAG(&I)' NE 'HRDCPY').MCL5                        00376000
         AIF   (&MC(7)).MCERR       HRDCPY ALREADY CODED ?              00377000
&MC(7)   SETB  1                    TURN ON HRDCPY (WPLMCSFG)           00378000
         AGO   .NEXTMC                                                  00379000
.MCL5    ANOP                                                           00380000
         AIF   ('&MCSFLAG(&I)' NE 'QREG0').MCL6                         00381000
         AIF   (&MC(8)).MCERR       QREG0 ALREADY CODED ?               00382000
&MC(8)   SETB  1                    TURN ON QREG0 (WPLMCSFH)            00383000
         AGO   .NEXTMC                                                  00384000
.MCL6    ANOP                                                           00385000
         AIF   ('&MCSFLAG(&I)' NE 'NOTIME').MCL7                        00386000
         AIF   (&MC(9)).MCERR       NOTIME ALREADY CODED ?              00387000
&MC(9)   SETB  1                    TURN ON NOTIME (WPLMCSFI)           00388000
         AGO   .NEXTMC                                                  00389000
.MCL7    ANOP                                                           00390000
         AIF   ('&MCSFLAG(&I)' NE 'NOCPY').MCERR                        00391000
         AIF   (&MC(14)).MCERR      NOTIME ALREADY CODED ?              00392000
&MC(14)  SETB  1                    TURN ON NOTTIME (WPLMCSFN)          00393000
         AGO   .NEXTMC                                                  00394000
.MCERR   ANOP                                                           00395000
         MNOTE 8,'&MCSFLAG(&I) IS INVALID - IGNORED'                    00396000
.NEXTMC  ANOP                       INCR ENTRY INDEX                    00397000
&I       SETA  &I+1                                                     00398000
         AIF   (&I LE &NUMENT).MCLOOP                                   00399000
.*       TEST FOR ERRORS                                                00400000
         AIF   (&MC(7) AND &MC(14)).MCEXCL    HRDCOPY AND NOCPY ?       00401000
         AGO   .ENDMCS                                                  00402000
.MCEXCL  MNOTE 8,'HRDCPY AND NOCPY MUTUALLY EXCLUSIVE, HRDCPY ASSUMED'  00403000
&MC(14)  SETB  0                      TURN OFF NOCPY                    00404000
.ENDMCS  ANOP                                                           00405000
.********************************************************************** 00406000
.*                                                                      00407000
.*       ALL PARAMETERS EXCEPT MESSAGE TEXT AND MF HAVE BEEN PROCESSED  00408000
.*       PROCESS ANY PARAMETER INTERACTIONS                             00409000
.*                                                                      00410000
.********************************************************************** 00411000
.*       DETERMINE IF A DEFAULT ROUTING CODE OF 2 HAS TO BE SET         00412000
         AIF   (&GENRD).INT1        ROUTCDE SPECIFIED, BRANCH           00413000
         AIF   (NOT &GENMT).INT1    NO MSGTYP, BRANCH                   00414000
         AIF   (&MC(2) OR &MC(8)).INT1   MCS REG0 OR QREG0 CODED ?      00415000
.*       SET A DEFAULT ROUTING CODE OF 2                                00416000
&GENRD   SETB  1                                                        00417000
&RO(2)   SETB  1                                                        00418000
.INT1    ANOP                                                           00419000
.*       QID FORCES GENERATION OF DESC AND ROUTE FIELDS                 00420000
&GENRD   SETB  (&GENRD OR (T'&QID NE 'O'))                              00421000
.*       DETERMINE IF THE MCS FLAG FOR ROUT/DESC PRESENT SHOULD BE ON   00422000
&MC(1)   SETB  (&GENRD)                                                 00423000
.*       DETERMINE IF MCS FLAG FOR MSGTYP PRESENT SHOULD BE ON          00424000
&MC(4)   SETB  (&GENMT)                                                 00425000
.********************************************************************** 00426000
.*                                                                      00427000
.*       DETERMINE SOURCE OF TEXT FOR MESSAGE                           00428000
.*                                                                      00429000
.********************************************************************** 00430000
.*       TEST &MESG FOR MULTI-LINE                                      00431000
.*       MULTI-LINE WTOR NOT SUPPORTED                                  00432000
         AIF   ((N'&SYSLIST(1) EQ 0) OR (N'&SYSLIST(1) EQ 1)).INT1A     00433000
         AGO   .ERROR7                 GENERATE MSG MLWTO/WTOR ERROR    00434000
.INT1A   ANOP                                                           00435000
.*       MESSAGE TEXT AND INLINE TEXT ARE MUTUALLY EXCLUSIVE            00436000
.*       ISSUE ERROR MESSAGE IF BOTH PROVIDED                           00437000
         AIF   (('&TEXT' NE '^') AND ('&MESG' NE '')).ERROR9            00438000
.*       EITHER MESG OR TEXT MUST BE SPECIFIED UNLESS MF=E              00439000
.INT3    AIF   ('&MF(1)' EQ 'E').INT5                                   00440000
         AIF   (('&MESG' EQ '') AND ('&TEXT' EQ '^')).INT4              00441000
         AGO   .INT5                                                    00442000
.INT4    MNOTE 12,'NO INLINE MESSAGE TEXT OR TEXT PARAMETER SPECIFIED'  00443000
         MEXIT                                                          00444000
.INT5    ANOP                                                           00445000
.*       USE OF TEXT= PARAMETER FORCES GENERATION OF TYPE 2 XWPL        00446000
         AIF   ('&TEXT' EQ '^').INT6                                    00447000
&GENXWPL SETB  1                       GENERATE AN XWPL                 00448000
&WPLXLEV SETA  2                       LEVEL OF XWPL TO GENERATE        00449000
&MC(12)  SETB  1                       SET MCS FLAG FOR XWPL (WPLMCSFL) 00450000
&MC(21)  SETB  1                       WTOR WITH EXT PARM  (WPXWTOR)    00451000
&MC(25)  SETB  1                       TEXT ADDR SPECIFIED (WPXTXTAD)   00452000
.********************************************************************** 00453000
.*                                                                      00454000
.*       PROCESS MF PARAMETER                                           00455000
.*                                                                      00456000
.********************************************************************** 00457000
.INT6    AIF    ('&MF' EQ 'I' OR '&MF' EQ 'L').WPLGEN                   00458000
.*                                                                      00459000
.*       MF=(E,X) PROCESSING                                            00460000
.*                                                                      00461000
         AIF   ('&MF(1)' NE 'E').ERROR1    CONFIRM THAT IT IS MF=E      00462000
         AIF   (N'&MF EQ 2).MFE5                                        00463000
         AIF   ((N'&MF EQ 3) AND ('&MF(3)' NE 'EXTENDED')).ERROR1       00464000
&GENXWPL SETB  ((N'&MF EQ 3) AND ('&MF(3)' EQ 'EXTENDED'))              00465000
&MC(12)  SETB  1                    SET MCS FLAG FOR XWPL (WPLMCSFL)    00466000
&MC(21)  SETB  1                       WTOR WITH EXT PARM  (WPXWTOR)    00467000
.MFE5    ANOP                                                           00468000
&NAME    IHBINNRA &MF(2)            GENERATE INITIAL LR OR LA IF NEEDED 00469000
         AIF   (&GENXWPL).MFE6                                          00470000
.*       PROCESS STANDARD WPL FOR MF=E WTOR                             00471000
         AIF   ('&RYAD' EQ '').MFE6A                                    00472000
         AIF   ('&LENGTH' NE '').MFE6B                                  00473000
         IC    14,0(1,0)                SAVE REPLY LENGTH               00474000
.MFE6B   AIF   ('&RYAD'(1,1) EQ '(').MFE6C                              00475000
         LA    15,&RYAD                 LOAD REPLY ADDR                 00476000
         ST    15,0(1,0)                STORE RPLY ADDR                 00477000
         AGO   .MFE6H                                                   00478000
         ANOP                                                           00479000
.MFE6C   ST    &RYAD(1),0(1,0)          STORE REPLY ADDR                00480000
.MFE6H   AIF   ('&LENGTH' NE '').MFE6E                                  00481000
         STC   14,0(1,0)                RESTORE REPLY LENGTH            00482000
         AGO   .MFE6G                                                   00483000
.MFE6E   AIF   ('&LENGTH'(1,1) EQ '(').MFE6D                            00484000
         MVI   0(1),&LENGTH             MOVE IN REPLY LENGTH            00485000
         AGO   .MFE6G                                                   00486000
.MFE6D   STC   &LENGTH(1),0(1,0)        STORE REPLY LENGTH              00487000
         AGO   .MFE6G                                                   00488000
.MFE6A   AIF   ('&LENGTH' NE '').MFE6E                                  00489000
.MFE6G   AIF   ('&ECB' EQ '').MFE6Z                                     00490000
         AIF   ('&ECB'(1,1) EQ '(').MFE6F                               00491000
         LA    14,&ECB                  LOAD ADDRESS OF ECB             00492000
         ST    14,4(1,0)                STORE ECB ADDRESS               00493000
         AGO   .MFE6Z                                                   00494000
.MFE6F   ST    &ECB(1),4(1,0)           STORE ECB ADDRESS               00495000
.MFE6Z   AGO   .MFESVC                                                  00496000
.*       PROCESS XWPL FOR MF=E WTOR                                     00497000
.*       EITHER USE OF A PARAMETER OR PROVIDING MF=(E,..,EXTENDED) HAS  00498000
.*       FORCED USE OF AN XWPL                                          00499000
.*       UPDATE MCS FLAGS                                               00500000
         OI    2(1),B'&MC(1)&MC(2)&MC(3)&MC(4)&MC(5)&MC(6)&MC(7)&MC(8)'X00501000
                                        MCS FLAGS                       00502000
         OI    3(1),B'&MC(9)&MC(10)&MC(11)&MC(12)&MC(13)&MC(14)&MC(15)&X00503000
               MC(16)'                  MCS FLAGS                       00504000
.MFE6    ANOP                                                           00505000
         AIF   ('&TEXT' EQ '^').MFE1A                                   00506000
         AIF   ('&TEXT' EQ '').ERROR2   TEXT= CANNOT BE NULL            00507000
&WORKT   SETC  '&TEXT(1)'                                               00508000
         AIF   ('&WORKT'(1,1) EQ '(').MFE2   TEXT=REG ?                 00509000
         LA    15,&WORKT                R15 -> TEXT                     00510000
         ST    15,4(,1)                 STORE TEXT ADDR INTO XWPL       00511000
         AGO   .MFE1                                                    00512000
.MFE2    ANOP                                                           00513000
&I       SETA  K'&WORKT                                                 00514000
&WORKT   SETC  '&WORKT'(2,&I-2)         ASSIGN REGISTER NOTATION        00515000
         ST    &WORKT,4(,1)             STORE TEXT ADDR INTO XWPL       00516000
.*       TEXT= PARAMETER HAS BEEN PROVIDED. THE SECOND, THIRD AND       00517000
.*       FORTH POSITIONAL PARAMETERS MUST NOT BE PROVIDED               00518000
.MFE1    AIF   (('&RYAD' NE '') OR ('&LENGTH' NE '') OR                X00519000
               ('&ECB' NE '')).ERROR6                                   00520000
&WORKA   SETC  '&TEXT(2)'               XWPL VERSION 2                  00521000
&WORKL   SETC  '&TEXT(3)'               XWPL VERSION 2                  00522000
&WORKE   SETC  '&TEXT(4)'               XWPL VERSION 2                  00523000
         AGO   .MFE1B                                                   00524000
.*       PROCESS XWPL VERSION 1 WTOR PARAMETERS                         00525000
.MFE1A   ANOP                                                           00526000
&WORKA   SETC  '&RYAD'                  XWPL VERSION 1                  00527000
&WORKL   SETC  '&LENGTH'                XWPL VERSION 1                  00528000
&WORKE   SETC  '&ECB'                   XWPL VERSION 1                  00529000
.MFE1B   ANOP                                                           00530000
.*       ANY NEED TO UPDATE ADDITIONAL WPLX FIELDS ?                    00531000
         AIF   ((NOT &GENRD) AND (NOT &GENMT) AND (NOT &MC(25))        X00532000
               AND ('&WORKA' EQ '') AND ('&WORKL' EQ '') AND           X00533000
               ('&WORKE' EQ '')).MFESVC  EXIT TO SVC                    00534000
         LA    14,0(,1)                                                 00535000
         AH    14,0(,14)            ADD L'TEXT + LEN FIELD + MCS FIELD  00536000
         AIF   ('&WORKL' EQ '').MFE1C   L'REPLY PROVIDED ?              00537000
         AIF   ('&WORKL'(1,1) EQ '(').MFE1D   REGISTER NOTATION ?       00538000
         MVI   2(14),&WORKL             STORE REPLY LENGTH              00539000
         AGO   .MFE1C                                                   00540000
.MFE1D   ANOP                                                           00541000
&I       SETA  K'&WORKL                                                 00542000
&WORKL   SETC  '&WORKL'(2,&I-2)         ASSIGN REGISTER NOTATION        00543000
         STC   &WORKL,2(,14)            STORE REPLY LENGTH              00544000
.MFE1C   ANOP                                                           00545000
         AIF   ('&TEXT' EQ '^').MFE4    NEED TO SET EXTENDED MCS ?      00546000
         OI    4(14),B'&MC(17)&MC(18)&MC(19)&MC(20)&MC(21)&MC(22)&MC(23X00547000
               )&MC(24)'                EXTENDED MCS                    00548000
         OI    5(14),B'&MC(25)&MC(26)&MC(27)&MC(28)&MC(29)&MC(30)&MC(31X00549000
               )&MC(32)'                EXTENDED MCS                    00550000
.MFE4    AIF   ('&WORKA' EQ '').MFE4A   REPLY ADDRESS PROVIDED ?        00551000
         AIF   ('&WORKA'(1,1) EQ '(').MFE4D   REGISTER NOTATION ?       00552000
         LA    15,&WORKA                15 -> REPLY ADDRESS             00553000
         ST    15,8(,14)                STORE REPLY ADDR                00554000
         AGO   .MFE4A                                                   00555000
.MFE4D   ANOP                                                           00556000
&I       SETA  K'&WORKA                                                 00557000
&WORKA   SETC  '&WORKA'(2,&I-2)                                         00558000
         ST    &WORKA,8(,14)            STORE REPLY ADDR                00559000
.*                                                                      00560000
.MFE4A   AIF   ('&WORKE' EQ '').MFE4G   ECB ADDRESS PROVIDED ?          00561000
         AIF   ('&WORKE'(1,1) EQ '(').MFE4F   REGISTER NOTATION ?       00562000
         LA    15,&WORKE                15 -> ECB ADDRESS               00563000
         ST    15,12(,14)               STORE ECB ADDR                  00564000
         AGO   .MFE4G                                                   00565000
.MFE4F   ANOP                                                           00566000
&I       SETA  K'&WORKE                                                 00567000
&WORKE   SETC  '&WORKE'(2,&I-2)                                         00568000
         ST    &WORKE,12(,14)           STORE ECB ADDR                  00569000
.*                                                                      00570000
.MFE4G   AIF   (NOT &GENMT).MFESVC      NEED TO GEN MSGTYP FLAGS ?      00571000
         MVI   40(14),B'&MT(1)&MT(2)&MT(3)&MT(4)&MT(5)&MT(6)&MT(7)&MT(8X00572000
               )'                       MSGTYP                          00573000
         MVI   41(14),B'&MT(9)&MT(10)&MT(11)&MT(12)&MT(13)&MT(14)&MT(15X00574000
               )&MT(16)'                MSGTYP                          00575000
.MFE3    AIF   (NOT &GENRD).MFESVC      NEED TO GEN ROUTE/DESC ?        00576000
         MVI   20(14),B'&DE(1)&DE(2)&DE(3)&DE(4)&DE(5)&DE(6)&DE(7)&DE(8X00577000
               )'                       DESCRIPTOR CODES                00578000
         MVI   21(14),B'&DE(9)&DE(10)&DE(11)&DE(12)&DE(13)&DE(14)&DE(15X00579000
               )&DE(16)'                DESCRIPTOR CODES                00580000
         MVI   24(14),B'&RO(1)&RO(2)&RO(3)&RO(4)&RO(5)&RO(6)&RO(7)&RO(8X00581000
               )'                       ROUTING CODES                   00582000
         MVI   25(14),B'&RO(9)&RO(10)&RO(11)&RO(12)&RO(13)&RO(14)&RO(15X00583000
               )&RO(16)'                ROUTING CODES                   00584000
.MFESVC  ANOP                                                           00585000
         SVC   35                                                       00586000
         MEXIT                      EXIT MACRO, PROCESSING COMPLETE     00587000
.********************************************************************** 00588000
.*                                                                      00589000
.*       MF=I AND MF=L PROCESSING                                       00590000
.*                                                                      00591000
.*       GENERATE A STANDARD WPL OR AN XWPL VERSION 1 OR VERSION 2      00592000
.*       DEPENDING ON PARAMETERS PROVIDED                               00593000
.*                                                                      00594000
.********************************************************************** 00595000
.WPLGEN  ANOP                                                           00596000
         AIF   ('&MF' EQ 'L').DCNAME                                    00597000
.*       MF=I PROCESSING                                                00598000
         CNOP  0,4                                                      00599000
&NAME    BAL   1,&GNAME.A                                               00600000
         AGO   .MFIL01                                                  00601000
.*       MF=L PROCESSING                                                00602000
.DCNAME  ANOP                                                           00603000
&NAME    DC    0F'0'                                                    00604000
.MFIL01  ANOP                                                           00605000
         AIF   (&GENXWPL).XWPL01         GO GENERATE XWPL               00606000
.********************************************************************** 00607000
.*                                                                      00608000
.*       GENERATE STANDARD WPL FOR MF=I OR MF=L                         00609000
.*                                                                      00610000
.********************************************************************** 00611000
.*       LENGTH MUST BE PROVIDED FOR MF=I                               00612000
         AIF   (('&LENGTH' EQ '') AND ('&MF' EQ 'I')).ERROR4            00613000
         AIF   (('&LENGTH' EQ '') AND ('&MF' EQ 'L')).MFIL02            00614000
         AIF   ('&LENGTH'(1,1) EQ '(').MFIL02                           00615000
         DC    AL1(&LENGTH)             REPLY LENGTH                    00616000
         AGO   .MFIL03                                                  00617000
.MFIL02  DC    AL1(0)                   REPLY LENGTH                    00618000
.*       REPLY ADDR MUST BE PROVIDED FOR MF=I                           00619000
.MFIL03  AIF   (('&RYAD' EQ '') AND ('&MF' EQ 'I')).ERROR5              00620000
         AIF   (('&RYAD' EQ '') AND ('&MF' EQ 'L')).MFIL04              00621000
         AIF   ('&RYAD'(1,1) EQ '(').MFIL04                             00622000
         DC    AL3(&RYAD)               REPLY ADDRESS                   00623000
         AGO   .MFIL05                                                  00624000
.MFIL04  DC    AL3(0)                   REPLY ADDRESS                   00625000
.*       ECB ADDR MUST BE PROVIDED FOR MF=I                             00626000
.MFIL05  AIF   (('&ECB' EQ '') AND ('&MF' EQ 'I')).ERROR6               00627000
         AIF   (('&ECB' EQ '') AND ('&MF' EQ 'L')).MFIL06               00628000
         AIF   ('&ECB'(1,1) EQ '(').MFIL06                              00629000
         DC    A(&ECB)                  ECB ADDRESS                     00630000
         AGO   .MFIL07                                                  00631000
.MFIL06  DC    A(0)                     ECB ADDRESS                     00632000
.MFIL07  ANOP                                                           00633000
.*       GENERATE INLINE MESSAGE TEXT                                   00634000
&I       SETA  1                                                        00635000
&LEN     SETA  K'&SYSLIST(1,1)-2                                        00636000
&PAIR    SETB  0                                                        00637000
.QLOOP1  ANOP                                                           00638000
&I       SETA  &I+1+&PAIR                                               00639000
         AIF   (&I GE K'&SYSLIST(1,1)).QDONE1                           00640000
&PAIR    SETB  ('&SYSLIST(1,1)'(&I,2) EQ '''''' OR '&SYSLIST(1,1)'(&I,2X00641000
               ) EQ '&&')                                               00642000
&LEN     SETA  &LEN-&PAIR                                               00643000
         AGO   .QLOOP1                                                  00644000
.QDONE1  ANOP                                                           00645000
&LEN     SETA  4+&LEN                                                   00646000
         DC    AL2(&LEN)                TEXT LENGTH                     00647000
         DC    B'&MC(1)&MC(2)&MC(3)&MC(4)&MC(5)&MC(6)&MC(7)&MC(8)&MC(9)X00648000
               &MC(10)&MC(11)&MC(12)&MC(13)&MC(14)&MC(15)&MC(16)'      X00649000
                                        MCS FLAGS                       00650000
         AIF   (&LEN EQ 4).MFE6BDC                                      00651000
         DC    C&SYSLIST(1,1)           MESSAGE TEXT                    00652000
.*       GENERATE FLAG BYTES                                            00653000
.MFE6BDC AIF   (NOT &GENRD).MFIL08      GENERATING ROUT/DESC/MSGTYP ?   00654000
         DC    B'&DE(1)&DE(2)&DE(3)&DE(4)&DE(5)&DE(6)&DE(7)&DE(8)&DE(9)X00655000
               &DE(10)&DE(11)&DE(12)&DE(13)&DE(14)&DE(15)&DE(16)'      X00656000
                                        DESCRIPTOR CODES                00657000
         DC    B'&RO(1)&RO(2)&RO(3)&RO(4)&RO(5)&RO(6)&RO(7)&RO(8)&RO(9)X00658000
               &RO(10)&RO(11)&RO(12)&RO(13)&RO(14)&RO(15)&RO(16)'      X00659000
                                        ROUTING CODES                   00660000
         AIF   (NOT &GENMT).MFIL08      GENERATING MSGTYP ?             00661000
         DC    B'&MT(1)&MT(2)&MT(3)&MT(4)&MT(5)&MT(6)&MT(7)&MT(8)&MT(9)X00662000
               &MT(10)&MT(11)&MT(12)&MT(13)&MT(14)&MT(15)&MT(16)'      X00663000
                                        MSGTYP                          00664000
                                                                        00665000
.MFIL08  ANOP                                                           00666000
         AIF   ('&MF' EQ 'I').MFIL09                                    00667000
.*       MF=L, ENSURE NO REGISTER VALUES HAVE BEEN PROVIDED             00668000
         AIF   ('&RYAD' EQ '').MFIL08A                                  00669000
         AIF   ('&RYAD'(1,1) EQ '(').ERROR3                             00670000
.MFIL08A AIF   ('&LENGTH' EQ '').MFIL08B                                00671000
         AIF   ('&LENGTH'(1,1) EQ '(').ERROR3                           00672000
.MFIL08B AIF   ('&ECB' EQ '').MFIL08C                                   00673000
         AIF   ('&ECB'(1,1) EQ '(').ERROR3                              00674000
.MFIL08C MEXIT                                                          00675000
.*       MF=I, STORE ANY VALUES PROVIDED IN REGISTERS                   00676000
.MFIL09  ANOP                                                           00677000
&GNAME.A DS    0H                                                       00678000
         AIF   ('&RYAD'(1,1) NE '(').MFIL10                             00679000
         AIF   ('&LENGTH'(1,1) EQ '(').MFIL11                           00680000
         IC    14,0(1,0)                SAVE REPLY LENGTH               00681000
         ST    &RYAD(1),0(1,0)          STORE REPLY ADDRESS             00682000
         STC   14,0(1,0)                RESTORE REPLY LENGTH            00683000
         AGO   .MFIL12                                                  00684000
.MFIL11  ST    &RYAD(1),0(1,0)          STORE REPLY ADDRESS             00685000
.MFIL13  STC   &LENGTH(1),0(1,0)        STORE REPLY LENGTH              00686000
         AGO   .MFIL12                                                  00687000
.MFIL10  AIF   ('&LENGTH'(1,1) EQ '(').MFIL13                           00688000
.MFIL12  AIF   ('&ECB'(1,1) NE '(').MFIL14                              00689000
         ST    &ECB(1),4(1,0)           STORE ECB ADDRESS               00690000
.MFIL14  SVC   35                                                       00691000
         MEXIT                                                          00692000
.********************************************************************** 00693000
.*                                                                      00694000
.*       GENERATE XWPL VERSION 1 OR VERSION 2 FOR WTOR                  00695000
.*                                                                      00696000
.********************************************************************** 00697000
.*                                                                      00698000
.*       A VERSION 1 XWPL WILL BE GENERATED IF INLINE TEXT IS           00699000
.*       PROVIDED. THE WTOR MACRO POSITIONAL PARAMETERS OF &RYAD,       00700000
.*       &LENGTH AND &ECB WILL BE PLACED IN THE VERSION 1 XWPL FIELDS   00701000
.*       IF THE TEXT= PARAMETER IS PROVIDED THEN A VERSION 2 XWPL       00702000
.*       WILL BE GENERATED. THE &RYAD, &LENGTH AND &ECB FIELDS          00703000
.*       MUST BE PROVIDED AS SUB-PARAMETERS IN THE TEXT= PARAMETER.     00704000
.*       THE POSITIONAL PARAMETERS &RYAD, &LENGTH AND &ECB MUST NOT     00705000
.*       BE PROVIDED.                                                   00706000
.*                                                                      00707000
.XWPL01  AIF   ('&TEXT' NE '^').XWPL04     HAVE TEXT= PARM TO PROCESS ? 00708000
.*       GENERATE INLINE TEXT FOR A XWPL VERSION 1                      00709000
.XWPL01A ANOP                                                           00710000
&I       SETA  1                                                        00711000
&LEN     SETA  K'&SYSLIST(1,1)-2    L'TEXT                              00712000
&PAIR    SETB  0                                                        00713000
.*       LOOP THROUGH TEXT STRING AND PROCESS DUPLICATE QUOTES AND      00714000
.*       AMPERSANDS                                                     00715000
.XWPL02  ANOP                                                           00716000
&I       SETA  &I+1+&PAIR                                               00717000
         AIF   (&I GE K'&SYSLIST(1,1)).XWPL03   GOTO LOOP EXIT          00718000
&PAIR    SETB  ('&SYSLIST(1,1)'(&I,2) EQ '''''' OR '&SYSLIST(1,1)'(&I,2X00719000
               ) EQ '&&')                                               00720000
&LEN     SETA  &LEN-&PAIR                                               00721000
         AGO   .XWPL02                 EXIT FROM LOOP                   00722000
.*       GENERATE XWPL FIELDS                                           00723000
.XWPL03  ANOP                                                           00724000
.*       GENERATE TEXT FOR XWPL VERSION 1                               00725000
&LEN     SETA  4+&LEN                                                   00726000
         AIF   (&LEN EQ 4).ERROR8                                       00727000
         DC    AL2(&LEN)                TEXT LENGTH                     00728000
         DC    B'&MC(1)&MC(2)&MC(3)&MC(4)&MC(5)&MC(6)&MC(7)&MC(8)&MC(9)X00729000
               &MC(10)&MC(11)&MC(12)&MC(13)&MC(14)&MC(15)&MC(16)'      X00730000
                                        MCS FLAGS                       00731000
         DC    C&SYSLIST(1,1)           MESSAGE TEXT                    00732000
         DC    AL1(1)                   XWPL VERSION LEVEL              00733000
         AGO   .XWPL05                                                  00734000
.*                                                                      00735000
.*       GENERATE TEXT ADDR XWPL VERSION 2                              00736000
.*                                                                      00737000
.*       TEXT= PARAMETER HAS BEEN PROVIDED. THE SECOND, THIRD AND       00738000
.*       FORTH POSITIONAL PARAMETERS MUST NOT BE PROVIDED               00739000
.XWPL04  AIF   ('&TEXT' EQ '').ERROR2     TEXT= CANNOT BE NULL          00740000
         AIF   (('&RYAD' NE '') OR ('&LENGTH' NE '') OR                X00741000
               ('&ECB' NE '')).ERROR6                                   00742000
         DC    AL2(8)                   TEXT LENGTH                     00743000
         DC    B'&MC(1)&MC(2)&MC(3)&MC(4)&MC(5)&MC(6)&MC(7)&MC(8)&MC(9)X00744000
               &MC(10)&MC(11)&MC(12)&MC(13)&MC(14)&MC(15)&MC(16)'      X00745000
                                        MCS FLAGS                       00746000
         AIF   ('&MF' NE 'I').XWPL04A                                   00747000
.*       MF=I GENERATE ADDR IF PROVIDED                                 00748000
         AIF   ('&TEXT(1)' EQ '').ERROR2  MUST BE PROVIDED FOR MF=I     00749000
         AIF   ('&TEXT(1)'(1,1) EQ '(').XWPL04A  REG NOTATION ?         00750000
         DC    AL4(&TEXT(1))            ADDR OF MESSAGE TEXT            00751000
         AGO   .XWPL05                                                  00752000
.XWPL04A ANOP                                                           00753000
         DC    AL4(0)                   ADDR OF MESSAGE TEXT            00754000
         DC    AL1(2)                   XWPL VERSION LEVEL              00755000
.*       GENERATE FIELDS COMMON TO XWPL VER 1 AND XWPL VER 2            00756000
.XWPL05  ANOP                                                           00757000
         DC    B'00000000'              MISCELLANEOUS FLAGS             00758000
.*       REPLY LENGTH PROCESSING                                        00759000
&WORKL   SETC  '&LENGTH'                                                00760000
         AIF   (&WPLXLEV EQ 1).XWPL05C                                  00761000
&WORKL   SETC  '&TEXT(3)'               XWPL VERSION 2                  00762000
.XWPL05C AIF   (('&MF' EQ 'L') AND ('&WORKL' EQ '')).XWPL05A            00763000
         AIF   (('&MF' EQ 'L') AND ('&WORKL'(1,1) EQ '(')).ERROR3       00764000
         AIF   (('&MF' EQ 'I') AND ('&WORKL' EQ '')).ERROR4             00765000
         AIF   ('&WORKL'(1,1) EQ '(').XWPL05A  REGISTER NOTATION ?      00766000
         DC    AL1(&WORKL)              L'REPLY FOR WTOR                00767000
         AGO   .XWPL05B                                                 00768000
.XWPL05A DC    AL1(0)                   L'REPLY FOR WTOR                00769000
.*       DETERMINE LEVEL OF XWPL                                        00770000
.XWPL05B AIF   (&WPLXLEV EQ 2).XWPL06                                   00771000
.*       XWPL LENGTH VALUE FOR XWPL VERSION 1                           00772000
         DC    AL1(0)                   RESERVED                        00773000
         AGO   .XWPL07                                                  00774000
.*       XWPL LENGTH VALUE FOR XWPL VERSION 2                           00775000
.XWPL06  ANOP                                                           00776000
         DC    AL1(104)                 LENGTH OF XWPL VERSION 2        00777000
.XWPL07  ANOP                                                           00778000
.*       GENERATE EXTENDED MCS FLAGS                                    00779000
         DC    B'&MC(17)&MC(18)&MC(19)&MC(20)&MC(21)&MC(22)&MC(23)&MC(2X00780000
               4)&MC(25)&MC(26)&MC(27)&MC(28)&MC(29)&MC(30)&MC(31)&MC(3X00781000
               2)'                      EXTENDED MCS FLAGS              00782000
         DC    XL2'0000'                MCS FLAGS FOR CNTL PROGRAM USE  00783000
.*       REPLY BUFFER PROCESSING                                        00784000
&WORKA   SETC  '&RYAD'                                                  00785000
         AIF   (&WPLXLEV EQ 1).XWPL07C                                  00786000
&WORKA   SETC  '&TEXT(2)'               XWPL VERSION 2                  00787000
.XWPL07C AIF   (('&MF' EQ 'L') AND ('&WORKA' EQ '')).XWPL07A            00788000
         AIF   (('&MF' EQ 'L') AND ('&WORKA'(1,1) EQ '(')).ERROR3       00789000
         AIF   (('&MF' EQ 'I') AND ('&WORKA' EQ '')).ERROR5             00790000
         AIF   ('&WORKA'(1,1) EQ '(').XWPL07A  REGISTER NOTATION ?      00791000
         DC    AL4(&WORKA)              ADDR OF REPLY BUFFER FOR WTOR   00792000
         AGO   .XWPL07B                                                 00793000
.XWPL07A DC    AL4(0)                   ADDR OF REPLY BUFFER FOR WTOR   00794000
.XWPL07B ANOP                                                           00795000
.*       REPLY ECB PROCESSING                                           00796000
&WORKE   SETC  '&ECB'                                                   00797000
         AIF   (&WPLXLEV EQ 1).XWPL08C                                  00798000
&WORKE   SETC  '&TEXT(4)'               XWPL VERSION 2                  00799000
.XWPL08C AIF   (('&MF' EQ 'L') AND ('&WORKE' EQ '')).XWPL08A            00800000
         AIF   (('&MF' EQ 'L') AND ('&WORKE'(1,1) EQ '(')).ERROR3       00801000
         AIF   (('&MF' EQ 'I') AND ('&WORKE' EQ '')).ERROR6             00802000
         AIF   ('&WORKE'(1,1) EQ '(').XWPL08A  REGISTER NOTATION ?      00803000
         DC    AL4(&WORKE)              ADDR OF REPLY ECB FOR WTOR      00804000
         AGO   .XWPL08B                                                 00805000
.XWPL08A DC    AL4(0)                   ADDR OF REPLY ECB FOR WTOR      00806000
.XWPL08B ANOP                                                           00807000
         DC    AL4(0)                   RESERVED - CONNECT ID           00808000
         DC    B'&DE(1)&DE(2)&DE(3)&DE(4)&DE(5)&DE(6)&DE(7)&DE(8)&DE(9)X00809000
               &DE(10)&DE(11)&DE(12)&DE(13)&DE(14)&DE(15)&DE(16)'      X00810000
                                        DESCRIPTOR CODES                00811000
         DC    AL2(0)                   RESERVED                        00812000
         DC    B'&RO(1)&RO(2)&RO(3)&RO(4)&RO(5)&RO(6)&RO(7)&RO(8)&RO(9)X00813000
               &RO(10)&RO(11)&RO(12)&RO(13)&RO(14)&RO(15)&RO(16)'      X00814000
                                        EXTENDED ROUTING CODES          00815000
         DC    B'&RO(17)&RO(18)&RO(19)&RO(20)&RO(21)&RO(22)&RO(23)&RO(2X00816000
               4)&RO(25)&RO(26)&RO(27)&RO(28)&RO(29)&RO(30)&RO(31)&RO(3X00817000
               2)'                                                      00818000
         DC    B'&RO(33)&RO(34)&RO(35)&RO(36)&RO(37)&RO(38)&RO(39)&RO(4X00819000
               0)&RO(41)&RO(42)&RO(43)&RO(44)&RO(45)&RO(46)&RO(47)&RO(4X00820000
               8)'                                                      00821000
         DC    B'&RO(49)&RO(50)&RO(51)&RO(52)&RO(53)&RO(54)&RO(55)&RO(5X00822000
               6)&RO(57)&RO(58)&RO(59)&RO(60)&RO(61)&RO(62)&RO(63)&RO(6X00823000
               4)'                                                      00824000
         DC    B'&RO(65)&RO(66)&RO(67)&RO(68)&RO(69)&RO(70)&RO(71)&RO(7X00825000
               2)&RO(73)&RO(74)&RO(75)&RO(76)&RO(77)&RO(78)&RO(79)&RO(8X00826000
               0)'                                                      00827000
         DC    B'&RO(81)&RO(82)&RO(83)&RO(84)&RO(85)&RO(86)&RO(87)&RO(8X00828000
               8)&RO(89)&RO(90)&RO(91)&RO(92)&RO(93)&RO(94)&RO(95)&RO(9X00829000
               6)'                                                      00830000
         DC    B'&RO(97)&RO(98)&RO(99)&RO(100)&RO(101)&RO(102)&RO(103)&X00831000
               RO(104)&RO(105)&RO(106)&RO(107)&RO(108)&RO(109)&RO(110)&X00832000
               RO(111)&RO(112)'                                         00833000
         DC    B'&RO(113)&RO(114)&RO(115)&RO(116)&RO(117)&RO(118)&RO(11X00834000
               9)&RO(120)&RO(121)&RO(122)&RO(123)&RO(124)&RO(125)&RO(12X00835000
               6)&RO(127)&RO(128)'                                      00836000
.*                                                                      00837000
         DC    B'&MT(1)&MT(2)&MT(3)&MT(4)&MT(5)&MT(6)&MT(7)&MT(8)&MT(9)X00838000
               &MT(10)&MT(11)&MT(12)&MT(13)&MT(14)&MT(15)&MT(16)'      X00839000
                                        MSGTYPE                         00840000
         DC    AL2(0)                   MESSAGE PRIORITY                00841000
         DC    CL8'        '            JOB ID                          00842000
         DC    CL8'        '            JOB NAME                        00843000
         DC    CL8'        '            RETRIEVAL KEY                   00844000
         DC    AL4(0)                   TOKEN FOR DOM                   00845000
         DC    AL4(0)                   CONSOLE ID                      00846000
         DC    CL8'        '            SYSTEM NAME                     00847000
         AIF   (&WPLXLEV EQ 1).XWPL08   BRANCH TO GENERATE XWPL VER 1   00848000
         DC    CL8'        '            CONSOLE NAME                    00849000
         DC    AL4(0)                   REPLY CONSOLE NAME/ID ADDR      00850000
         DC    AL4(0)                   CART ADDRESS                    00851000
         DC    AL4(0)                   WSPARM ADDRESS                  00852000
         AGO   .XWPL09                                                  00853000
.*       XWPL VERSION 1 FIELDS                                          00854000
.XWPL08  ANOP                                                           00855000
         DC    AL4(0)                   RESERVED                        00856000
         DC    AL4(0)                   RESERVED                        00857000
.XWPL09  ANOP                                                           00858000
         AIF   ('&MF' EQ 'L').XWPL99    MF=L ALL FINISHED               00859000
.*       MF=I PROCESSING FOR XWPL VERSION 1 AND VERSION 2               00860000
&GNAME.A DS    0H                                                       00861000
.*       STORE ADDR OF TEXT INTO XWPL                                   00862000
         AIF   ('&TEXT' EQ '^').XWPL11                                  00863000
         AIF   ('&TEXT(1)'(1,1) EQ '(').XWPL10   TEXT=(REG) ?           00864000
         LA    15,&TEXT(1)              R15 -> TEXT                     00865000
         ST    15,4(,1)                 STORE TEXT ADDR INTO XWPL       00866000
         AGO   .XWPL11                                                  00867000
.XWPL10  ANOP                                                           00868000
         ST    &TEXT(1),4(,1)           STORE TEXT ADDR INTO XWPL       00869000
.*       DETERMINE IF REGISTER NOTATION HAS BEEN USED FOR ANY PARAMETER 00870000
.XWPL11  AIF   (('&WORKA'(1,1) NE '(') AND ('&WORKL'(1,1) NE '(') AND  X00871000
               ('&WORKE'(1,1) NE '(')).XWPL98   N REGISTER NOTATION     00872000
.*       REGISTER NOTATION HAS BEEN USED, SAVE VALUES IN REGISTERS      00873000
         LA    14,0(,1)                                                 00874000
         AH    14,0(,14)            ADD L'TEXT + LEN FIELD + MCS FIELD  00875000
         AIF   ('&WORKL'(1,1) NE '(').XWPL12                            00876000
         STC   &WORKL,2(,14)            STORE L'REPLY                   00877000
.XWPL12  AIF   ('&WORKA'(1,1) NE '(').XWPL13                            00878000
         ST    &WORKA,8(,14)            STORE ADDR OF REPLY             00879000
.XWPL13  AIF   ('&WORKE'(1,1) NE '(').XWPL98                            00880000
         ST    &WORKE,12(,14)           STORE ADDR OF ECB               00881000
.XWPL98  SVC   35                                                       00882000
.XWPL99  MEXIT                                                          00883000
.*                                                                      00884000
.********************************************************************** 00885000
.*       ISSUE ERROR MESSAGES                                           00886000
.********************************************************************** 00887000
.*                                                                      00888000
.*       INVALID MF OPERAND SPECIFIED-MF                                00889000
.*                                                                      00890000
.ERROR1  IHBERMAC 35,,&MF                                               00891000
         MEXIT                                                          00892000
.*       MESSAGE OPERAND REQ'D-NOT SPECIFIED                            00893000
.ERROR2  IHBERMAC 19                                                    00894000
         MEXIT                                                          00895000
.*       INVALID REGISTER NOTATION WITH MF=L SPECIFIED                  00896000
.ERROR3  IHBERMAC 69                                                    00897000
         MEXIT                                                          00898000
.*       REQUIRED LENGTH OPERAND NOT SPECIFIED                          00899000
.ERROR4  IHBERMAC 14                                                    00900000
         MEXIT                                                          00901000
.*       REQUIRED SECOND OPERAND NOT SPECIFIED                          00902000
.ERROR5  IHBERMAC 03                                                    00903000
         MEXIT                                                          00904000
.*       REQUIRED ECB OPERAND NOT SPECIFIED                             00905000
.ERROR6  IHBERMAC 11                                                    00906000
         MEXIT                                                          00907000
.*       MLWTO/WTOR MUTUALLY EXCLUSIVE                                  00908000
.ERROR7  IHBERMAC 246                                                   00909000
         MEXIT                                                          00910000
.*       MISSING OR EXCESSIVE MESSAGE TEXT                              00911000
.ERROR8  MNOTE 12,'NUMBER OF LINES REQUESTED IS 0 OR GREATER THAN 255 -X00912000
                GENERATION TERMINATED'                                  00913000
         MEXIT                                                          00914000
.*       MESG AND TEXT BOTH PROVIDED                                    00915000
.ERROR9  MNOTE 12,'INLINE TEXT AND ''TEXT'' ARE MUTUALLY EXCLUSIVE'     00916000
.*                                                                      00917000
         MEND                                                           00918000
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
*                                                                       00029000
*        DESCRIPTIVE NAME - MULTI-LINE WTO SERVICE ROUTINE              00030000
*                                                                       00031000
*        FUNCTION -                                                     00032000
*        THE MLWTO SERVICE ROUTINE IS PART OF IGC0003E OR SVC 35.       00033000
*        IT RECEIVES CONTROL WHEN A MLWTO IS ISSUED. A MULTIPLE         00034000
*        LINE WTO CAUSES A MAJOR WQE AND ONE OR MORE MINOR WQES         00035000
*        TO BE BUILT. THE WQES, MAJOR AND MINOR, CONTAIN MESSAGES       00036000
*        FOR THE CONSOLE OPERATOR AND ASSOCIATED CONTROL                00037000
*        INFORMATION                                                    00038000
*                                                                       00039000
*        THE MODULE CONSTRUCTS A MAJOR WQE AND MINOR WQES FOR           00040000
*        EACH MLWTO. IT WILL ALSO CONNECT ADDITIONAL DATA LINES         00041000
*        (OR MINOR WQES) TO AN EXISTING MLWTO. CONNECTING IS            00042000
*        RESERVED FOR KEY ZERO TO SEVEN OR SUPERVISOR MODE USERS        00043000
*        OF MLWTO ONLY.                                                 00044000
*                                                                       00045000
*        NON PP SVC 35 CALLERS (SUPERVISOR OR KEY 0-7 USERS)            00046000
*        ISSUING A STANDARD MULTI-LINE WTO MUST ZERO R0 UNLESS THEY     00047000
*        ARE PASSING A CONNECTING MESSAGE ID OR CONSOLE ID IN R0        00048000
*                                                                       00049000
*        THE ROUTINE PERFORMS THE FOLLOWING MAJOR FUNCTIONS IN          00050000
*        HANDLING AN MLWTO REQUEST                                      00051000
*                                                                       00052000
*     1. SET UP ADDRESSABILITY AND OBTAIN STORAGE FOR A WORKAREA        00053000
*                                                                       00054000
*     2. CHECK THAT THE LINE TYPES FOR EACH LINE OF THE MESSAGE         00055000
*        IS CORRECT. THE MESSAGE WILL BE TERMINATED AT THE LAST         00056000
*        CORRECT LINE IF AN INVALID LINE TYPE SITUATION IS FOUND        00057000
*                                                                       00058000
*     3. GET STORAGE FOR A MAJOR WQE AND ONE MINOR WQE IF THIS REQUEST  00059000
*        IS NOT ASKING TO ADD MORE LINES TO AN EXISTING MESSAGE         00060000
*                                                                       00061000
*     4. FILL IN THE MAJOR WQE WITH THE FIRST LINE OF THE MESSAGE.      00062000
*        IF THE MESSAGE HAS A DESCRIPTOR CODE OF 9 AND NO CONTROL       00063000
*        LINE AS THE LINE THEN A DEFAULT CONTROL MESSAGE IEE932I        00064000
*        IS SUPPLIED AS THE FIRST LINE OF THE MESSAGE                   00065000
*                                                                       00066000
*     5. PASS THE FIRST LINE TO THE SUBSYSTEM EXIT.                     00067000
*        THE TEXT IN WQETEXT IS TRANSLATED TO INSURE ONLY               00068000
*        PRINTABLE AND DISPLAYABLE CHARACTERS ARE PASSED FROM           00069000
*        THIS POINT                                                     00070000
*                                                                       00071000
*     6. XMPOST THE UCMOECB SO THAT THE COMMUNICATIONS TASK CAN START   00072000
*        PROCESSING THE MLWTO                                           00073000
*                                                                       00074000
*     7. CHECK IF THE MAJOR WQE EXISTS SO THAT THE NEXT LINE CAN BE     00075000
*        ADDED. IF THE SERVICE REQUEST WAS TO ADD EXTRA LINES THEN      00076000
*        THE PROCESSING WOULD SKIP STEPS 4 TO 7. THE MODULE IS          00077000
*        WRITTEN SO THAT EACH ADDITIONAL LINE IS HANDLED IN THE         00078000
*        SAME MANNER                                                    00079000
*                                                                       00080000
*     8. CHECK IF THERE IS SPACE IN THE MINOR WQE/WQES FOR THIS LINE.   00081000
*        GET A NEW MINOR WQE IF THERE IS NO SPACE. THERE IS ALWAYS      00082000
*        AT LEAST ONE MINOR POINTED TO BY THE MAJOR. THIS IS DONE TO    00083000
*        MINIMIZE GETMAIN/FREEMAIN USAGE.                               00084000
*                                                                       00085000
*     9. FILL IN THE NEXT LINE                                          00086000
*                                                                       00087000
*     10. PASS THE MAJOR WQE AND THE MINOR WQE WITH THE NEW LINE        00088000
*         TO THE SUBSYSTEM WTO EXIT.                                    00089000
*         ON RETURN THE TEXT IS TRANSLATED AS IN STEP 6                 00090000
*                                                                       00091000
*     11. POST THE UCMOECB                                              00092000
*                                                                       00093000
*     12. IF THERE ARE ANY MORE LINES IN THE WPL THEN GO BACK TO        00094000
*         STEP 8. IF NOT THEN RETURN TO IEAVVWTO                        00095000
*                                                                       00096000
*        OPERATION -                                                    00097000
*        THIS MODULE USES THE EXTENDED SAVEAREA IN THE SVRB.            00098000
*        SOME OF THE INFORMATION IN THIS AREA IS SET UP BY              00099000
*        IEAVVWTO PRIOR TO THE CALL TO IEAVMWTO                         00100000
*                                                                       00101000
*        NOTES -                                                        00102000
*        THE MODULE USES A NUMBER OF INTERNAL SUBROUTINES.              00103000
*        THE ROUTINES ARE ORGANIZED SO THAT THEY HAVE ONLY ONE          00104000
*        ENTRY POINT AND ONE EXIT POINT. MOST ROUTINES ARE ALSO         00105000
*        WELL STRUCTURED IN TERMS OF INSTRUCTION FLOW. THE              00106000
*        SEGMENTS USED IN THIS MODULE ARE LISTED.                       00107000
*                                                                       00108000
*                                                                       00109000
*     1. IEASGETL - DETERMINES THE NUMBER OF LINES IN THE WPL           00110000
*                                                                       00111000
*     2. IEASTLMX - LIMITS SERVICE REQUEST TO 10 LINES AT A TIME        00112000
*                                                                       00113000
*     3. CHKTYPE - CHECKS THAT LINES HAVE THE PROPER LINE TYPE          00114000
*        AND IN THE PROPER ORDER                                        00115000
*                                                                       00116000
*     4. INCRMNT - LOCATES THE BEGINNING OF THE NEXT LINE IN THE        00117000
*        WPL                                                            00118000
*                                                                       00119000
*     5. IEASTORE - SAVES THE NUMBER OF LINES FOUND IN THE WPL          00120000
*        SETS XVX0UDCL IF THE DEFAULT CONTROL LINE IS TO BE USED        00121000
*                                                                       00122000
*     6. BLDMAJ - GET SPACE FOR THE MAJOR AND MINOR WQE. BUILD MAJOR    00123000
*                                                                       00124000
*     7. GETMAJ - OBTAINS SPACE FOR THE WQE                             00125000
*                                                                       00126000
*     8. FREEMAJ - FREES THE MAJOR WQE IF A MINOR COULDN'T BE           00127000
*        OBTAINED                                                       00128000
*                                                                       00129000
*     9. BMAJINIT - INITIALIZE THE MAJOR WQE                            00130000
*                                                                       00131000
*     10.BMAJFSTL - SET UP TO MOVE TEXT INTO MAJOR                      00132000
*                                                                       00133000
*     11.BMAJMVMS - MOVE TEXT INTO MAJOR. TRUNCATE TRAILING BLANKS      00134000
*                                                                       00135000
*     12.BLDMIN - FILL IN MINOR LINES                                   00136000
*                                                                       00137000
*     13.MIN1INIT/MIN2INIT - INITIALIZE THE FIRST/SECOND LINE IN        00138000
*        THE MINOR WQE                                                  00139000
*                                                                       00140000
*     14.MIN1MOV/MIN2MOV - MOVE THE TEXT INTO THE FIRST/SECOND LINE     00141000
*        OF THE MINOR WQE                                               00142000
*                                                                       00143000
*     15.SUBSEXIT - PASS MAJOR/MINOR WQE TO SUBSYSTEM WTO EXIT.         00144000
*        ON RETURN FROM THE SUBSYSTEM EXIT THE USER IS A                00145000
*        PROBLEM PROGRAM THE WQE TEXT -MAJOR/MINOR- IS TRANSLATED       00146000
*        TO ALLOW ONLY PRINTABLE AND DISPLAYABLE CHARACTERS             00147000
*        FROM THIS POINT.                                               00148000
*                                                                       00149000
*     16.POSTOECB - XMPOST UCMOECB TO WAKE UP COMM TASK                 00150000
*                                                                       00151000
*     17.FREESAV - FREE THE MODULES DYNAMIC WORKAREA                    00152000
*                                                                       00153000
*        THE INTERNAL ROUTINES USED IN THE MODULE ARE DESCRIBED BELOW   00154000
*                                                                       00155000
*     1. GETMINOR - GETS SPACE FOR A MINOR WQE AND ADDS IT TO           00156000
*        QUEUE OF MINORS POINTED TO BY THE MAJOR.                       00157000
*                                                                       00158000
*     2. ENDUP - DECREMENTS THE NUMBER OF LINES TO BE DONE. SETS        00159000
*        LINE TYPE TO DATA-END IF NEEDED                                00160000
*                                                                       00161000
*     3. FINDID - LOCATES MAJOR TO WHICH THE MINOR LINE IS TO BE        00162000
*                 ADDED                                                 00163000
*                                                                       00164000
*     6. GETWQE - GETS A WQE FROM THE WQE CELLPOOL                      00165000
*                                                                       00166000
*     7. WAITWQE - WAIT FOR A WQE TO BE FREED                           00167000
*                                                                       00168000
*     8. TEXTLINE - INCREMENTS R4 TO THE NEXT LINE IN THE               00169000
*                   INPUT TEXT STRING                                   00170000
*                                                                       00171000
*     9. FRELCKS - FREES THE CMS AND LOCAL LOCK                         00172000
*                                                                       00173000
*     10.SETLCKS - OBTAINS THE LOCAL AND CMS LOCK                       00174000
*                                                                       00175000
*        DEPENDENCIES -                                                 00176000
*        THIS MODULE IS LINKED WITH IEAVVWTO, IGC0203E AND              00177000
*        IEECVXIT TO FORM ONE LOAD MODULE                               00178000
*        ENTRY POINT IS IEAVVWTO. THE LINK IS DONE AT SYSGEN            00179000
*                                                                       00180000
*        ATTRIBUTES -                                                   00181000
*        PAGED-LPA, ZERO PROTECT KEY, REENTERABLE, SUPERVISOR MODE      00182000
*                                                                       00183000
*        ENTRY-POINT - IEAVMWTO                                         00184000
*                                                                       00185000
*        PURPOSE -                                                      00186000
*        THIS IS THE ONLY ENTRY POINT TO THE ROUTINE. IT IS GIVEN       00187000
*        CONTROL BY IEAVVWTO TO PROCESS A MULTI-LINE WTO REQUEST        00188000
*                                                                       00189000
*        LINKAGE -                                                      00190000
*        IEAVMWTO IS CALLED BY IEAVVWTO USING STANDARD SYSTEM           00191000
*        LINKAGE CONVENTIONS                                            00192000
*                                                                       00193000
*        INPUT -                                                        00194000
*        THE FOLLOWING REGISTERS ARE INITIALIZED BY IEAVVWTO            00195000
*        R4   -> TCB                                                    00196000
*        R5   -> SVRB                                                   00197000
*        R7   -> ASCB                                                   00198000
*        R13  -> CALLER'S SAVEAREA                                      00199000
*        R14   = RETURN ADDRESS                                         00200000
*        R15   = ENTRY POINT ADDRESS                                    00201000
*                                                                       00202000
*        THE WPL HAS BEEN CHECKED FOR VALIDITY BY IEAVVWTO.             00203000
*        A MULT-LINE WTOR IS NOT PERMITTED. THIS IS CHECKED FOR         00204000
*        BY IEAVVWTO PRIOR TO IEAVMWTO BEING CALLED                     00205000
*                                                                       00206000
*        THE XVSAV AREA HAS BEEN INITIALIZED BY IEAVVWTO -              00207000
*        XVA8    =  TIME TAKEN BY IEAVVWTO                              00208000
*        XVD1PRIV = ON IF CALLER IS PRIVILEGED                          00209000
*        XVD1PP   = ON IF CALLER IS A PROBLEM PROGRAM                   00210000
*        XVD1AUTH = ON IF CALLER IS AUTHORIZED                          00211000
*        XVD2VALD = ON                                                  00212000
*        XVWQEID  = CONTENTS OF R0 PROVIDED BY SVC 35 ISSUER            00213000
*         XVWQEIDA = A NEW LINE IS TO BE CONNECTED TO THE MESSAGE       00214000
*                    WITH THIS MESSAGE ID.  MULTI-LINE WTO ONLY         00215000
*         XVCONID  = CONSOLE ID PASSED IN R0 TO SVC 35                  00216000
*                                                                       00217000
*        OUTPUT -                                                       00218000
*        THIS ROUTINE CREATES NO STREAM OR LIST OUTPUT                  00219000
*                                                                       00220000
*        REGISTERS SAVED -                                              00221000
*        THE CALLER'S REGISTERS ARE SAVED IN THE PROVIDED SAVEAREA      00222000
*                                                                       00223000
*        REGISTER USAGE -                                               00224000
*        THE REGISTER USAGE AND THE ASSOCIATED NAMES ARE GIVEN IN       00225000
*        THE FOLLOWING TABLE                                            00226000
*         REG      USAGE                                                00227000
*          0       WORK REG                                             00228000
*                                                                       00229000
*          1       WORK REG                                             00230000
*                                                                       00231000
*          2       WORK REG                                             00232000
*                                                                       00233000
*          3       -> CVT                                               00234000
*                  COUNTER FOR HANDLING TEXT LENGTH                     00235000
*                                                                       00236000
*          4       ADDR OF THE TCB                                      00237000
*                  ADDR OF TEXT LINE CURRENTLY BEING PROCESSED          00238000
*                                                                       00239000
*          6       -> WPL                                               00240000
*                                                                       00241000
*          7       ADDR OF THE ASCB                                     00242000
*                  ADDR OF MINOR WQE                                    00243000
*                                                                       00244000
*          8       ADDR OF THE MAJOR WQE                                00245000
*                  USED ONLY TO DEVELOP COUNT OF NUMBER OF LINES        00246000
*                  TO BE PROCESSED.                                     00247000
*                                                                       00248000
*          9       -> WORKAREA USED BY IEAVVWTO                         00249000
*                                                                       00250000
*          10      BASE FOR UCM                                         00251000
*                  COMPLETION CODE FOR XMPOST                           00252000
*          11      PROGRAM BASE                                         00253000
*                  ECB ADDR FOR XMPOST                                  00254000
*          12      BASE FOR XVSAV AREA                                  00255000
*                  ERRET ADDR FOR XMPOST                                00256000
*          13      SAVE AREA BASE AND WORK REG                          00257000
*                  ADDR ASCB FOR XMPOST                                 00258000
*          14      WORK REG                                             00259000
*          15      WORK REG                                             00260000
*                                                                       00261000
*                                                                       00262000
*        REGISTERS RESTORED -                                           00263000
*        ALL OF THE CALLER'S REGISTERS ARE RESTORED                     00264000
*                                                                       00265000
*        EXIT NORMAL -                                                  00266000
*        THIS MODULE HAS ONLY ONE EXIT POINT                            00267000
*                                                                       00268000
*        CONDITIONS -                                                   00269000
*        IEAVMWTO ALWAYS RETURNS TO ITS CALLER, IEAVVWTO.               00270000
*        IT RETURNS AFTER SERVICING THE MULTI-LINE REQUEST              00271000
*                                                                       00272000
*        OUTPUT -                                                       00273000
*        THE RETURN TO IEAVVWTO IS TAKEN FOR SUCCESSFUL AND             00274000
*        UNSUCCESSFUL SERVICING OF THE MULTI-LINE REQUEST.              00275000
*        THE OUTPUT FOR A SUCCESSFUL SERVICING IS -                     00276000
*         XVWQEID = 3 BYTE MESSAGE SEQUENCE NUMBER, RIGHT JUSTIFIED.    00277000
*         XVRETCOD = 0 THEN NOTHING FOUND IN ERROR.                     00278000
*                                                                       00279000
*                    4 AN ERROR WAS FOUND IN THE NUMBER OF LINES.       00280000
*                      MESSAGE WAS TRUNCATED TO TEN LINES AND LAST      00281000
*                      LINE SET TO DATA/END. - OR -                     00282000
*                      THE ACTUAL TEXT LENGTH FOR ONE LINE WAS ZERO.    00283000
*                      THE MESSAGE TRUNCATED AT THE PREVIOUS LINE.      00284000
*                                                                       00285000
*                   12 THE LINE TYPE FOR A LINE WAS INVALID. THE        00286000
*                      MESSAGE WAS TO THE LAST VALID LINE AND ITS       00287000
*                      LINE TYPE WAS SET TO DATA/END.                   00288000
*                                                                       00289000
*                   16 THE MLWTO HAD A WTP ROUTE CODE AND OTHER ROUTE   00290000
*                      CODES. THE WTP ROUTE CODE (ROUTE CODE 11)        00291000
*                      WAS IGNORED.                                     00292000
*                                                                       00293000
*        THE OUTPUT FOR AN UNSUCCESSFUL SERVICING IS:                   00294000
*        XVWQEID = 0. NO MESSAGE WAS PUT OUT, THEREFORE MSGID IS ZERO   00295000
*        XVRETCOD = 4 -THE NUMBER OF LINES IN THE WPL WAS ZERO.         00296000
*                                                                       00297000
*                     -MESSAGE TEXT LENGTH FOR A LINE WAS               00298000
*                      GREATER THAN 1; ALL LINES UP TO                  00299000
*                      ERROR LINE ARE PROCESSED                         00300000
*                                                                       00301000
*                   8 THE MESSAGE ID PASSED IN R0 DID NOT MATCH         00302000
*                     ANY ID FOR A WQE CURRENTLY IN THE SYSTEM.         00303000
*                     THIS OCCURS ONLY WHEN IEAVMWTO IS ATTEMPTING      00304000
*                     TO CONNECT NEW LINES TO AN EXISTING MESSAGE.      00305000
*                     THIS PROBLEM CAN ARISE IN THE FOLLOWING WAYS.     00306000
*                     1. REG 0 IS NOT ZERO FOR THE FIRST SERVICE        00307000
*                        REQUEST OF A MULTI-LINE WTO.                   00308000
*                                                                       00309000
*                     2. THE MULTI-LINE MSG WAS GOING TO A CONSOLE      00310000
*                        THAT ENCOUNTERED AN I/O ERROR. CONSOLE         00311000
*                        SWTICH DELETED THE MSG AS MULTI-LINE MSGS      00312000
*                        CANT BE SWITCHED.                              00313000
*                                                                       00314000
*                     3. THE USER LOST THE MESSAGE ID PASSED BACK       00315000
*                        IN R1 BY SVC 35.                               00316000
*                                                                       00317000
*                  12 THE NEW MULTI-LINE MSG CONSISTS OF ONLY AN        00318000
*                     END LINE.                                         00319000
*                                                                       00320000
*                  16 ROUTE CODE 11 (WTP) WAS THE ONLY ROUTE CODE       00321000
*                     SPECIFIED.                                        00322000
*                                                                       00323000
*                  20 THE MULTI-LINE MSG WAS TO BE SET TO HARD COPY     00324000
*                     ONLY.                                             00325000
*                                                                       00326000
*        RETURN CODES -                                                 00327000
*        THE RETURN CODES ARE SET IN XVRETCOD AND THEIR MEANINGS        00328000
*        ARE DESCRIBED ABOVE                                            00329000
*                                                                       00330000
*        EXIT ERROR -                                                   00331000
*        THERE IS NO ERROR EXIT FROM IEAVMWTO. WHEN AN ABEND            00332000
*        CONDITION IS FOUND THE MODULE SETS XVD1PERR AND RETURNS        00333000
*        NORMALLY TO IEAVVWTO. IEAVVWTO WILL THEN ABEND THE             00334000
*        CALLER WITH A D23 ABEND CODE                                   00335000
*                                                                       00336000
*        CONDITIONS -                                                   00337000
*        THE ERROR BIT, XVD1PERR, IS SET FOR THE FOLLOWING              00338000
*        REASONS -                                                      00339000
*         1. NO SPACE COULD BE OBTAINED IN SUBPOOL 229 FOR THE          00340000
*            WORKAREA FOR IEAVMWTO                                      00341000
*         2. THE ESTAE EXIT WAS ENTERED                                 00342000
*         3. SPACE COULD NOT BE OBTAINED FOR A WQE FROM THE WQE         00343000
*            CELL POOL                                                  00344000
*                                                                       00345000
*        RETURN CODES - NONE                                            00346000
*                                                                       00347000
*        EXTERNAL REFERENCES - SEE THE FOLLOWING                        00348000
*                                                                       00349000
*        ROUTINES -                                                     00350000
*        THIS MODULE CALLS SUBSYSTEM WTO EXIT VIA THE IEFSSREQ          00351000
*        MACRO                                                          00352000
*                                                                       00353000
*        DATA AREAS - THE FOLLOWING EXTERNAL DATA AREAS ARE USED        00354000
*        OR REFERENCED BY THIS MODULE                                   00355000
*        ASCBASID = MEMORY ID                                           00356000
*        CVTCRMN  = ADDR OF GETMAIN BRANCH ENTRY                        00357000
*        CVTBLDCP = ADDR OF BLD CPOOL ENTRY POINT                       00358000
*        CVTFRECL/CVTGETCL = ADDR OF GET/FREE CELL ROUTINE              00359000
*        CVTRMBR  = ADDR OF FREEMAIN ENTRY POINT                        00360000
*        CVTVWAIT = ADDR OF BRANCH ENTRY TO WAIT                        00361000
*        PSALITA  = ADDR OF LOCK INTERFACE TABLE                        00362000
*        TCBFLGS1 = BIT TCBFX IS SET TO PREVENT ASYNCHRONOUS EVENTS     00363000
*                   DURING A WAIT FOR A MINOR WQE                       00364000
*        UCMASCB  = ADDR OF ASCB FOR COMMTASK'S MEMORY                  00365000
*        UCMCMID  = MESSAGE SEQUENCE NUMBER                             00366000
*        UCMOECB  = OUTPUT ECB FOR COMM TASK                            00367000
*        UCMSYST  = THIS BIT IS SET IF MLWTO WAS DELETED BY THE         00368000
*                   SUBSYSTEM                                           00369000
*        UCMWQECP = WQE CELL POOL NUMBER                                00370000
*        UCMWQEND = ADDR OF LAST WQE ON THE CHAIN                       00371000
*        UCMWQLM  = MAX NUMBER OF WQES ALLOWED                          00372000
*        UCMWQNR  = CURRENT NUMBER OF WQES                              00373000
*        UCMWTOQ  = ADDR OF START OF WQE CHAIN                          00374000
*                                                                       00375000
*        TABLES -                                                       00376000
*        THE FOLLOWING TABLES ARE USED IN THIS ROUTINE -                00377000
*        TRTAB -  TABLE OF CHARACTERS USED TO INSURE PRINTABLE          00378000
*                 AND NON DISPLAY CONTROL CHARACTERS IN THE WQE         00379000
*                 THIS TABLE IS RESIDENT IN IEEVVWTO                    00380000
*                                                                       00381000
*        SERIALIZATION -                                                00382000
*        THE LOCAL AND CMS LOCKS ARE USED TO SERIALIZE THE              00383000
*        RESOURCES USED IN THIS MODULE                                  00384000
*                                                                       00385000
IEAVMLWO SAVE  (14,12),,'IEAVMWTO ZP60039 &SYSDATE &SYSTIME'            00386000
*                                                                       00387000
         LA    R10,0(,R15)             R15 HAS ENTRY POINT ADDR         00388000
         LA    R11,2048(,R10)                                           00389000
         LA    R11,2048(,R11)                                           00390000
         USING IEAVMLWO,R10,R11                                         00391000
         USING RBBASIC,R12                                              00392000
         LR    R12,R5                  R12 -> SVRB                      00393000
         XC    XVX,XVX                 ZERO OUT THE XVX FLAGS           00394000
         XC    XVWWB,XVWWB             ZERO OUT XVWWB FOR ADDR OF WWB   00395000
*                                                                       00396000
*                                                                       00397000
*        GETMAIN FOR SAVE AREA AND WORKAREA FROM SUBPOOL 229            00398000
*                                                                       00399000
         L     R0,WKSIZE               GET SIZE OF WORK AREA            00400000
         LR    R9,R0                   SAVE LENGTH FOR LATER AREA ZERO  00401000
         L     R1,WKSUBPL              GET SUBPOOL NUMBER OF WORKAREA   00402000
*                                                                       00403000
         GETMAIN  RC,LV=(0),SP=(1)                                      00404000
*                                                                       00405000
*        CHECK IF GETMAIN WAS SUCCESSFUL                                00406000
*        IF NOT SET ERROR AND STOP BITS                                 00407000
*                                                                       00408000
         LTR   R15,R15                 CHECK GETMAIN RETURN CODE        00409000
         BZ    IEAVSAVE                CONTINUE IF ZERO RETURN CODE     00410000
*                                                                       00411000
*        GETMAIN FAILED                                                 00412000
*                                                                       00413000
         MVI   XVAMOD,MWTOID                                            00414000
         MVI   XVFNCT,D23DYN           DYNAMIC AREA GETMAIN FAILURE     00415000
         STC   R15,XVREASON                                             00416000
         OI    XVD1,XVD1PERR           SEVERE ERROR                     00417000
         OI    XVX1,XVX1STOP           ERROR, IGNORE MLWTO              00418000
         B     IEAMGRET                BRANCH TO RETURN TO USER         00419000
*                                                                       00420000
IEAVSAVE LR    R8,R1                   R8 -> GETMAINED AREA             00421000
*                                      R9  = L'GETMAINED AREA           00422000
         SLR   R15,R15                 NO COPY, PAD OF ZERO             00423000
         MVCL  R8,R14                  ZERO WORKAREA                    00424000
         ST    R1,8(,R13)              CHAIN SAVE AREAS                 00425000
         ST    R13,4(,R1)                                               00426000
         LR    R9,R13                  R9 -> IEAVVWTO WORKAREA          00427000
         USING WORKVV,R9               ESTABLISH ADDRESSABILITY         00428000
*                                      TO IEAVVWTO WORKAREA             00429000
         LR    R13,R1                                                   00430000
         USING WORKAREA,R13            SET ADDRESSABILITY FOR WORKAREA  00431000
         L     R3,CVTPTR                                                00432000
         USING CVT,R3                                                   00433000
         MVC   ADDRUCM,CVTCUCB         SAVE -> UCM IN ADDRUCM           00434000
         DROP  R3                                                       00435000
*                                                                       00436000
*        SAVE USER'S ASCB ADDR (R7) IN OUR SAVEAREA                     00437000
*        SAVE THE ADDR TO THE CURRENT RB                                00438000
*                                                                       00439000
         ST    R7,ASCBSAVE             SAVE ASCB ADDR                   00440000
         ST    R4,TCBSAVE              SAVE TCB ADDR                    00441000
*                                                                       00442000
*        CALCULATE ADDR OF LAST BYTE OF TEXT IN PROTECTED STORAGE       00443000
*                                                                       00444000
         L     R1,WKAADTXT             R1 -> MAJOR WQE TEXT IN          00445000
*                                            PROTECTED STORAGE          00446000
         AH    R1,WKALGH               ADD L'MAJOR WQE TEXT             00447000
         A     R1,LENMWQE              ADD L'OF ALL MINOR WQES          00448000
         ST    R1,LASTWPLB             STORE ADDR OF BYTE AFTER END OF  00449000
*                                      MAJOR AND MINOR WQES             00450000
*                                                                       00451000
*        CHECK FOR PP USER ISSUING MLWTO                                00452000
*        PP USERS CAN'T CONNECT MINOR LINES TO THE MAJOR WQE            00453000
*        CHECK XVD1PP AND XVD1AUTH TO INSURE                            00454000
*        R0 ISN'T AN IMPLIED PARAMETER TO SVC 35                        00455000
*                                                                       00456000
IEAVMLWS TM    XVD1,XVD1PP             USER A PROBLEM PROGRAM ?         00457000
         BZ    IEAVTCON                NO, GO CHECK FOR MSG ID          00458000
         TM    XVD1,XVD1AUTH           KEY 0, SUPVR STATE OR APF AUTH ? 00459000
         BO    IEAVTCON                YES, BRANCH                      00460000
*                                                                       00461000
*        PROBLEM PROGRAM, NO CONNECTION ALLOWED                         00462000
*                                                                       00463000
         XC    XVWQEIDA,XVWQEIDA       ZERO MESSAGE ID                  00464000
         B     IEAVSETE                BYPASS MSG CONNECTING PROCESS    00465000
*                                                                       00466000
*        NOT PROBLEM PROGRAM                                            00467000
*        INDICATE PRIVILEGED TASK TO AVOID MLWTO HANGUP BECAUSE         00468000
*        OF UNAVAILABLE WQE'S                                           00469000
*        INDICATE CONNECTING IF REQUESTED                               00470000
*                                                                       00471000
IEAVTCON OI    XVD1,XVD1PRIV           INDICATE PRIVILEGED TASK         00472000
         ICM   R15,B'1110',XVWQEIDA    CALLER CONNECTING ?              00473000
         BZ    IEAVSETE                NO, THE MSG ID FIELD IS EMPTY    00474000
         OI    XVD2,XVD2CON            YES, INDICATE CONNECTING         00475000
*                                                                       00476000
*        SET UP THE ESTAE PROTECTION FOR THIS MODULE                    00477000
*                                                                       00478000
IEAVSETE XC    EPARM(L'EPARM),EPARM    CLEAR THE ESTAE PARM LIST        00479000
         L     R3,ADDRUCM              R3 -> UCM                        00480000
         USING UCM,R3                                                   00481000
         L     R2,UCMFRRAD             R2 -> COM TASKS RECOVERY         00482000
*                                            ROUTINE IEEFRRAD           00483000
         LA    R8,EPARM                R8 -> ESTAE PARM AREA            00484000
         MVC   SUBSLIST(ELISTL),ELIST  MOVE IN ESTAE PARM LIST          00485000
         LA    R1,SUBSLIST             R1 -> PARAMETER LIST             00486000
         DROP  R3                                                       00487000
*                                                                       00488000
         ESTAE (R2),CT,PARAM=(R8),RECORD=YES,MF=(E,(1))                 00489000
*                                                                       00490000
         LA    R2,EPARM                R2 -> PARMLIST AREA              00491000
         ST    R2,PARMPTR              PT TO PARM AREA                  00492000
         USING PARMLIST,R2                                              00493000
         LA    R1,IEAVRETY             MOVE IN RETRY ADDR               00494000
         ST    R1,PARMRTAD                                              00495000
         LA    R1,RECSAVE              R1 -> REG SAVE AREA              00496000
         ST    R1,PARMRGAD             SET ESTAE PARM LIST PTR TO SA    00497000
         MVC   PARMID,MODULEID         IDENTIFY FAILING MODULE          00498000
         MVC   RECSAVE,ALLREGS         RELOAD ALL REGS ON RETRY         00499000
         STM   R0,R15,RECREGS          SAVE REGS FOR A RETRY            00500000
         DROP  R2                                                       00501000
*                                                                       00502000
*        CHECK IF MLWTO IS QUEUED TO HARDCOPY ONLY                      00503000
*        THIS IS AN ERROR CONDITION WITH A RETURN CODE OF 20(DEC)       00504000
*                                                                       00505000
         TM    WKAMCSF,WPLMCSFG        QUEUE TO HARD COPY ONLY ?        00506000
         BZ    IEASRTDC                NO, CHECK FOR ROUT/DESC CODES    00507000
         MVI   XVRETCOD,HCONLY         YES, SET RETURN CODE 20          00508000
         BAL   R0,IEASTOPA             IGNORE REQUEST                   00509000
*                                                                       00510000
IEASRTDC TM    WKAROC+1,WPLROUTK       WTP SPECIFIED ?                  00511000
         BZ    IEASGETL                NOT WTP, SKIP ERROR CHECK        00512000
*                                                                       00513000
*        THIS MLWTO INCLUDES A WTP ROUTE CODE                           00514000
*        CHECK IF IT IS A WTP ONLY                                      00515000
*                                                                       00516000
         CLC   WKAROC,WTPONLY          ANY ROUTE CODES OTHER THAN       00517000
*                                      WPLROUTK ?                       00518000
         BNE   IEASWTPP                YES, WTP NOT THE ONLY ROUTE CODE 00519000
*                                      SPECIFIED                        00520000
*        ONLY WTP ROUTE CODE. SET STOP FLAG                             00521000
*                                                                       00522000
         OI    XVX1,XVX1STOP                                            00523000
         B     IEASRETC                                                 00524000
*                                                                       00525000
IEASWTPP NI    WKAROC+1,255-WPLROUTK   TURN OFF WTP ROUTING CODE        00526000
IEASRETC MVI   XVRETCOD,RCWTP          SET RETURN CODE 16               00527000
         TM    XVX1,XVX1STOP           STOP PROCESSING SET ?            00528000
         BZ    IEASGETL                NO, CONTINUE PROCESSING          00529000
         BAL   R0,IEASTOPA             YES, THEN SKIP TO STOP           00530000
*                                                                       00531000
*        GET THE NUMBER OF LINES TO BE PROCESSED                        00532000
*                                                                       00533000
*        INPUT -                                                        00534000
*        WKAADTXT -> TEXT FOR MAJOR WQE AND MINOR WQES                  00535000
*                                                                       00536000
*        OUTPUT -                                                       00537000
*        XVX3 = NUMBER OF LINES FROM WPLLINES FIELD                     00538000
*        R2 -> WPLLTF (MLWTO EXTENSION HDR) AND ADDRESSABILITY IS SET   00539000
*        ADDRMLHR IS SET TO ADDR OF WPLLTF                              00540000
*                                                                       00541000
IEASGETL L     R2,WKAADTXT             R2 -> MAJOR WQE AND MINOR WQES   00542000
         AH    R2,WKALGH               ADD L'MAJOR WQE                  00543000
         ST    R2,ADDRMLHR             SAVE ADDR TO MULTILINE EXTENSION 00544000
*                                      HEADER WPLLTF                    00545000
         USING WPLRF,R2                R2 -> MLWTO EXTENSION WPLLTF     00546000
*                                                                       00547000
         TM    XVX1,XVX1STOP           ERROR IN GETLINES ?              00548000
         BZ    IEASGETC                NO, CONTINUE  PROCESSING         00549000
         BAL   R0,IEASTOPA             YES, THEN SKIP PROCESSING        00550000
*                                                                       00551000
*        CHECK THE TEXT LENGTH OF THE MAJOR WQE FOR ZERO                00552000
*                                                                       00553000
IEASGETC CLC   WKALGH,KH4              LENGTH > 4 ?                     00554000
         BH    IEASTLMK                YES, GO CHECK NO OF LINES        00555000
         BL    IEA00212                < 4, ERROR, BRANCH               00556000
*                                      LENGTH = 4                       00557000
*                                                                       00558000
*        VALIDATE MLWTO EXTENSION HEADER FLAGS                          00559000
*                                                                       00560000
         TM    WPLLTF,WPLLTFD          END LINE ?                       00561000
         BZ    IEA00212                NO, BRANCH                       00562000
         TM    WPLLTF,WPLLTFC          YES, END PLUS DATA LINE ?        00563000
         BZ    IEASTLMK                NO, BRANCH                       00564000
IEA00212 MVI   XVRETCOD,LINERR         ZERO LINE LENGTH - ERROR 04      00565000
         BAL   R0,IEASTOPA             RETURN TO CALLER                 00566000
*                                                                       00567000
IEASTLMK CLI   WPLLINES,0              ANY LINES TO PROCESS ?           00568000
         BNE   IEASTLMX                YES, CONTINUE CHECKING WPL       00569000
         MVI   XVRETCOD,LINERR         NO, ZERO NO OF LINES, ERROR      00570000
         BAL   R0,IEASTOPA             RETURN TO CALLER                 00571000
*                                                                       00572000
*        VALIDATE THE NUMBER OF LINES                                   00573000
*        ALLOW NO MORE THAN 10 LINES TO BE PROCESSED                    00574000
*        UNLESS THE CALLER IS AUTHORIZED                                00575000
*                                                                       00576000
IEASTLMX MVC   XVX3,WPLLINES           SAVE THE NUMBER OF LINES         00577000
         CLI   XVX3,10                 NO LINES > 10 ?                  00578000
         BNH   IEASETC1                NO, VALID                        00579000
         TM    XVD1,XVD1AUTH           KEY 0, SUPVR STATE OR APF AUTH ? 00580000
         BO    IEASETC1                YES, BRANCH                      00581000
         TM    XVD1,XVD1PP             PROBLEM PROGRAM CALLER ?         00582000
         BZ    IEASETC1                NO, BRANCH                       00583000
         MVI   XVX3,10                 YES, TRUNCATE NO LINES TO 10 MAX 00584000
         MVI   XVRETCOD,LINERR         SET RETURN CODE = 4 BUT          00585000
*                                      CONTINUE PROCESSING              00586000
*                                                                       00587000
*        BEGIN PROCESSING THE MAJOR WQE LINE                            00588000
*                                                                       00589000
*        R2 -> WPLLTF                                                   00590000
*        R14 = LINE COUNT                                               00591000
*                                                                       00592000
IEASETC1 LA    R14,1                   SET COUNT=1                      00593000
         MVI   NUMLINES,1              SET LINE COUNT TO 1              00594000
         MVC   LINETYPE,WPLLTF         MOVE LINE CONTROL FLAGS FROM     00595000
*                                      MLWTO EXTENSION HEADER TO        00596000
*                                      LINETYPE                         00597000
         TM    LINETYPE,WPLLTFD        END LINE ?                       00598000
         BZ    IEASTCLN                NO, GO CHECK FOR CONTROL LINE    00599000
         TM    LINETYPE,WPLLTFC        YES, DATA LINE AS WELL ?         00600000
         BO    IEASTCLN                YES, START LINE TYPE CHECKING    00601000
*                                                                       00602000
*        THE FIRST LINE CAN JUST BE AN END LINE                         00603000
*        SET FLAG INDICATING END LINE                                   00604000
*        ONLY VALID IF CONNECTING                                       00605000
*                                                                       00606000
         OI    XVX0,XVX0FLJE           SET LINE 1 JUST END              00607000
         TM    XVD2,XVD2CON            CONNECTING                       00608000
         BO    IEASTORE                YES, SKIP TO STORE LINE COUNT    00609000
         MVI   XVRETCOD,INVLDLT        NO, SET INVALID LINE TYPE RC     00610000
         BAL   R0,IEASTOPA                                              00611000
*                                                                       00612000
*        CHKTYPE SEGMENT                                                00613000
*                                                                       00614000
*        ALL LINES ARE PROCESSED THROUGH THIS SECTION OF CODE           00615000
*                                                                       00616000
*        THE LINE CONTROL FLAGS ARE IN A DIFFERENT OFFSET FOR THE       00617000
*        MAJOR WQE COMPARED TO THE MINOR WQE LINES.                     00618000
*        FOR CODE SIMPLIFICATION THE LINE CONTROL FLAGS FROM THE TWO    00619000
*        DIFFERENT LOCATIONS ARE MOVED TO LINETYPE FOR COMMON TESTING   00620000
*                                                                       00621000
IEASTCLN TM    LINETYPE,WPLLTFA        CONTROL LINE ?                   00622000
         BO    IEASCNT0                YES, CHECK COUNT                 00623000
         TM    LINETYPE,WPLLTFB        LABEL LINE ?                     00624000
         BO    IEASTLAB                YES, BRANCH                      00625000
         TM    LINETYPE,WPLLTFC        DATA LINE ?                      00626000
         BO    IEASUPCT                YES, BRANCH                      00627000
         TM    LINETYPE,WPLLTFD        END LINE ONLY ?                  00628000
         BO    IEASFEND                YES, GOTO FORCE END              00629000
         B     IEASRC12                NO, ERROR                        00630000
*                                                                       00631000
*        PROCESS CONTROL LINE                                           00632000
*        CONTROL LINE MUST BE THE FIRST LINE                            00633000
*                                                                       00634000
IEASCNT0 CH    R14,KH1                 COUNT=1 (MAJOR WQE LINE) ?       00635000
         BNE   IEASRC12                NO, ERROR                        00636000
         TM    LINETYPE,WPLLTFB+WPLLTFC+WPLLTFD CONTROL LINE ONLY ?     00637000
         BNZ   IEASRC12                NO, ERROR AS OTHER LINE TYPES ON 00638000
         TM    XVD2,XVD2CON            CONNECTING ?                     00639000
         BO    IEASRC12                YES, ERROR                       00640000
         OI    XVX0,XVX0FLCL           SET FIRST LINE CONTROL LINE      00641000
         B     IEASTEST                                                 00642000
*                                                                       00643000
*        PROCESS LABEL LINE                                             00644000
*        LABEL FLAG MUST BE THE ONLY FLAG FOR THE LINE                  00645000
*                                                                       00646000
IEASTLAB TM    LINETYPE,WPLLTFA+WPLLTFC+WPLLTFD LABEL LINE ONLY ?       00647000
         BNZ   IEASRC12                NO, OTHER FLAGS ON, ERROR        00648000
         TM    XVX0,XVX0LL2F           LABEL LINE 2 BIT ON ?            00649000
         BO    IEASRC12                YES, DUPLICATE LABEL, ERROR      00650000
         TM    XVX0,XVX0LL1F           LABEL LINE 1 BIT ON ?            00651000
         BO    IEAS1LAB                YES, BRANCH                      00652000
         OI    XVX0,XVX0LL1F           SET LABEL LINE 1 BIT ON          00653000
         TM    XVX0,XVX0FLCL           CONTROL LINE PREVIOUSLY FOUND ?  00654000
         BO    IEASCLF1                YES, BRANCH                      00655000
         CH    R14,KH1                 NO, COUNT = 1 ?                  00656000
         BE    IEASTEST                YES, BRANCH                      00657000
         B     IEASRC12                LABEL LINE INCORRECTLY PLACED    00658000
*                                                                       00659000
IEASCLF1 CH    R14,KH2                 COUNT = 2 ?                      00660000
         BE    IEASTEST                                                 00661000
         B     IEASRC12                NO, ERROR                        00662000
*                                                                       00663000
*        PROCESS SECOND LABEL LINE                                      00664000
*                                                                       00665000
IEAS1LAB TM    XVX0,XVX0FLCL           FIRST LINE CONTROL LINE ?        00666000
         BZ    IEASTC2                 NO, BRANCH                       00667000
         CH    R14,KH3                 COUNT = 3 ?                      00668000
         BNE   IEASRC12                NO, ERROR                        00669000
         B     IEASETL2                                                 00670000
*                                                                       00671000
IEASTC2  CH    R14,KH2                 COUNT = 2 ?                      00672000
         BNE   IEASRC12                NO, ERROR                        00673000
IEASETL2 OI    XVX0,XVX0LL2F           SET LABEL LINE 2 BIT ON          00674000
         B     IEASTEST                                                 00675000
*                                                                       00676000
*        PROCESS DATA LINE                                              00677000
*                                                                       00678000
IEASUPCT TM    LINETYPE,WPLLTFA+WPLLTFB+WPLLTFD  JUST DATA LINE ?       00679000
         BZ    IEASTEST                YES, BRANCH                      00680000
         TM    LINETYPE,WPLLTFA+WPLLTFB  OTHER FLAGS ON ?               00681000
         BNZ   IEASRC12                YES, ERROR                       00682000
         OI    XVX0,XVX0FEDE           SET FORCED END FLAG              00683000
         B     IEASTEST                CHECK FOR END OF LOOP            00684000
*                                                                       00685000
*        INVALID LINE TYPE FLAGS DETECTED                               00686000
*                                                                       00687000
IEASRC12 MVI   XVRETCOD,INVLDLT        SET RETURN CODE TO INDICATE      00688000
*                                      BAD LINETYP                      00689000
*                                                                       00690000
*        PROCESS END LINE                                               00691000
*                                                                       00692000
IEASFEND OI    XVX0,XVX0FEDE           SET FORCE END FLAG               00693000
*                                                                       00694000
*        CHECK IF ALL DONE PROCESSING THIS MLWTO                        00695000
*        CHECK FOR AN END FOUND OR FORCED OR FOR THE LINE               00696000
*        COUNT MET. IF ALL DONE THEN STORE THE LINE COUNT               00697000
*                                                                       00698000
IEASTEST TM    XVX0,XVX0FLJE+XVX0FEDE  END FOUND OR FORCED ?            00699000
         BNZ   IEASTORE                YES, WRAP UP WPL PROCESSING      00700000
         CLM   R14,B'0001',XVX3        ALL MINOR WQE LINES PROCESSED ?  00701000
         BE    IEASTORE                YES                              00702000
*                                      NO, FALL THROUGH TO ADVANCE      00703000
*                                          TO NEXT LINE IN WPL          00704000
*                                                                       00705000
*        INCRMNT                                                        00706000
*                                                                       00707000
*        STEP TO THE NEXT MINOR WQE LINE IN THE WPL                     00708000
*        INSURE THAT THE MINOR WQE TEXT IS CONTAINED WITHIN             00709000
*        THE TEXT STORAGE AREA PASSED BY IEAVVWTO                       00710000
*                                                                       00711000
         LA    R15,4                   SET L'MLWTO EXTENSION HEADER     00712000
         CH    R14,KH1                 FIRST LINE ?                     00713000
         BE    INCRMNTA                YES, USE CURRENT VALUE IN R15    00714000
         LH    R15,WPLML0              GET CURRENT L'LINE               00715000
INCRMNTA LA    R14,1(,R14)             INCR LINE COUNT                  00716000
         AR    R2,R15                  INCR TO REFERENCE NEXT MINOR     00717000
*                                      CONTROL FIELD                    00718000
         LR    R1,R2                   R1 -> NEXT MINOR WQE             00719000
         AH    R1,WPLML0               ADD L'NEXT MINOR WQE             00720000
         C     R1,LASTWPLB             THIS NEXT MINOR WQE ENTIRELY     00721000
*                                      CONTAINED IN THE GETMAINED AREA? 00722000
         BNH   IEASINCC                WITHIN WPL, BRANCH               00723000
*                                      OUTSIDE WPL, ERROR               00724000
         MVI   XVAMOD,MWTOID           IEAVMWTO'S ID FOR D23            00725000
         MVI   XVFNCT,D23VALID         PARMLIST VALIDITY CHECK          00726000
         MVI   XVREASON,D23SIZE        CALLER MODIFIED WPL              00727000
         OI    XVD1,XVD1PERR           ABEND USER                       00728000
         BAL   R0,IEASTOPA             SKIP TO END OF TSTMLWTO          00729000
*                                                                       00730000
IEASINCC MVC   LINETYPE,WPLMLLTF       MOVE LINE TYPE FLAGS FROM        00731000
*                                      MLWTO LINE ENTRY TO LINETYPE     00732000
         TM    LINETYPE,WPLLTFD        END LINE ?                       00733000
         BZ    IEASCKLL                NO, CHECK LINE LENGTH            00734000
         TM    LINETYPE,WPLLTFC        DATA END LINE ?                  00735000
         BO    IEASCKLL                YES, CHECK LINE LENGTH           00736000
         B     IEASTORE                CHECK IF ALL DONE                00737000
*                                                                       00738000
IEASCKLL CLC   WPLML0(2),KH4           LINE LENGTH > 4 ?                00739000
         BH    IEASTCLN                YES, GO CHECK LINE TYPE FLAGS    00740000
         BCTR  R14,0                   DO NOT PUT OUT BAD LINE          00741000
         MVI   XVRETCOD,LINERR         SET RETURN CODE TO ERROR         00742000
*                                      IN THE NO OF LINES               00743000
         OI    XVX0,XVX0FEDE           FORCE END TO MLWTO               00744000
*                                                                       00745000
*        STORE THE COUNT AND SET UP FOR CREATING THE FIRST LINE         00746000
*                                                                       00747000
*        INPUT -                                                        00748000
*        R14 = NUMBER OF LINES TO PROCESS                               00749000
*                                                                       00750000
*        OUTPUT -                                                       00751000
*        COUNT IS STORED IN XVXD0 AND XVX2                              00752000
*        XVD3TXT1 IS SET TO INDICATE THE FIRST LINE IS BEING PROCESSED  00753000
*        XVX0UDCL IS SET IF THE DEFAULT CONTROL LINE IS TO BE USED      00754000
*                                                                       00755000
*                                                                       00756000
IEASTORE STC   R14,XVX3                UPDATE ACTUAL LINE COUNT         00757000
         STC   R14,XVX2                SET UP NO OF LINES TO DO         00758000
         OI    XVD3,XVD3TXT1           INDICATE FIRST LINE PROCESSING   00759000
         TM    XVD1,XVD1PP             PROBLEM PROGRAM ?                00760000
         BZ    IEASSSET                NO, BRANCH                       00761000
         TM    XVD1,XVD1AUTH           KEY 0, SUPVR STATE OR APF AUTH ? 00762000
         BO    IEASSSET                YES, BRANCH                      00763000
         OI    XVX0,XVX0FEDE           NO, SET FORCE END                00764000
*                                                                       00765000
*        DETERMINE IF THE DEFAULT LINE HAS TO BE USED                   00766000
*                                                                       00767000
IEASSSET TM    XVX0,XVX0FLCL           FIRST LINE CONTROL LINE          00768000
         BO    IEASEXIT                YES, NOT GOING TO USE DEFAULT    00769000
         TM    XVD2,XVD2CON            CONNECTING                       00770000
         BO    IEASEXIT                YES, SKIP                        00771000
         TM    WKADSC+1,WPLDESCI       DESC CODE 9 OPERATORS REQUEST ?  00772000
         BZ    IEASEXIT                NO, BRANCH                       00773000
         OI    XVX0,XVX0UDCL           YES, USE DEFAULT CONTROL LINE    00774000
*                                                                       00775000
         DROP  R2                      WPL ADDRESSABILITY DROPPED       00776000
*                                                                       00777000
*        END OF LINE VALIDATION CODE                                    00778000
*                                                                       00779000
*        SET THE LOCAL AND CMS LOCKS                                    00780000
*                                                                       00781000
IEASEXIT L     R1,PARMPTR              R1 -> ESTAE PARMAREA             00782000
         USING PARMLIST,R1                                              00783000
         XC    PARMRTAD,PARMRTAD       CLEAR RETRY ADDRESS              00784000
         DROP  R1                                                       00785000
         BAL   R15,SETLCKS             CALL THE SET LOCK ROUTINE        00786000
         STM   R0,R15,RECREGS          SAVE REGS FOR FRR RECOVERY       00787000
         XC    XVCMAJOR,XVCMAJOR       ZERO MAJOR WQE ADDR              00788000
         TM    XVD2,XVD2CON            CONNECTING MINOR LINES  ?        00789000
         BO    IEAMBMIN                YES, GO BUILD THE MINOR          00790000
*                                      NO, BUILDING THE MAJOR WQE       00791000
*                                                                       00792000
*        BLDMAJ                                                         00793000
*                                                                       00794000
*        THIS SEGMENT CREATES AND BUILDS THE MAJOR WQE OR FIRST         00795000
*        LINE OF THE MLWTO                                              00796000
*                                                                       00797000
*        INPUT -                                                        00798000
*        THE LOCAL AND CMS LOCKS ARE HELD                               00799000
*        THE WPL HAS BEEN CHECKED FOR PROPER LINE TYPES                 00800000
*                                                                       00801000
*        OUTPUT -                                                       00802000
*        A MAJOR WQE IS CONSTRUCTED AND PUT ON THE WQE CHAIN. THE       00803000
*        MAJOR HAS AN EMPTY MINOR CHAINED TO IT                         00804000
*                                                                       00805000
*        THIS SEGMENT GETS A MAJOR WQE                                  00806000
*        INPUT -                                                        00807000
*        UNCMWQNR, UCMWQLM, XVD1PRIV, XVD2CON                           00808000
*        OUTPUT -                                                       00809000
*        R1 PTS AT THE MAJOR WQE IF ONE WAS AVAILABLE                   00810000
*        R1 IS ZERO IF ONE WASN'T AVAILABLE                             00811000
*        THE WQE IS ZEROED OUT                                          00812000
*                                                                       00813000
*        CHECK IF TWO WQES ARE AVAILABLE                                00814000
*                                                                       00815000
IEAJGET0 L     R3,ADDRUCM              R3 -> UCM                        00816000
         USING UCM,R3                                                   00817000
         LH    R2,UCMWQNR              NUMBER OF WQES USED              00818000
         LA    R2,2(,R2)               TWO WQES NEEDED                  00819000
         CH    R2,UCMWQLM              COMPARE WITH LIMIT ON WQES       00820000
         BNH   IEAJGET1                WQES AVAIL, DON'T WAIT           00821000
         DROP  R3                                                       00822000
*                                                                       00823000
*        TWO WQES AREN'T AVAILABLE, CHECK IF USER IS PRIVILEGED         00824000
*                                                                       00825000
         TM    XVD1,XVD1PRIV           CALLER PRIVILEGED ?              00826000
         BO    IEAJGET1                YES, GET WQES                    00827000
*                                                                       00828000
*        USER ISN'T PRIVILEGED. WAIT FOR WQES TO BE FREED               00829000
*                                                                       00830000
         BAL   R14,WAITWQE             WAIT FOR A WQE                   00831000
         B     IEAJGET0                CHECK IF TWO WQES ARE AVAIL      00832000
*                                                                       00833000
IEAJGET1 BAL   R14,GETWQE              GET AND ZERO A WQE               00834000
*                                                                       00835000
*        CHECK IF WQE WAS AVAILABLE                                     00836000
*                                                                       00837000
         LTR   R1,R1                   ADDR RETURNED ?                  00838000
         BNZ   IEAJGET2                YES, STORE ADDR OF MAJOR         00839000
*                                                                       00840000
*        WQE WASN'T AVAILABLE. SET ERROR AND STOP FLAGS                 00841000
*                                                                       00842000
         OI    XVX1,XVX1STOP           STOP PROCESSING WPL              00843000
         OI    XVD1,XVD1PERR           ABEND USER                       00844000
         B     IEAJGET3                SKIP TO CHECK IF WWB ALLOCATED   00845000
*                                                                       00846000
*        SET UP MAJOR FOR PROCESSING                                    00847000
*                                                                       00848000
IEAJGET2 ST    R1,XVCMAJOR             SAVE ADDR OF MAJOR               00849000
         OI    XVD3,XVD3BLDJ           SET BUILD MAJOR FLAG             00850000
*                                                                       00851000
*        CHECK IF A WWB HAS BEEN OBTAINED, IF SO, THEN FREE IT          00852000
*                                                                       00853000
IEAJGET3 ICM   R2,B'1111',XVWWB        WWB ADDR ZERO ?                  00854000
         BZ    IEAJGET4                YES, SKIP FREEING WWB            00855000
*                                                                       00856000
*        FREE THE WWB POINTED AT BY XVWWB                               00857000
*                                                                       00858000
         USING WWB,R2                                                   00859000
         L     R1,WWBFWDPT             R1 -> NEXT WWB FORWARD ON CHAIN  00860000
*                                                                       00861000
*        CONNECT FORWARD WWB TO BACK WWB                                00862000
*                                                                       00863000
         MVC   WWBBCKPT-WWB(4,R1),WWBBCKPT                              00864000
         L     R1,WWBBCKPT                                              00865000
*                                                                       00866000
*        CONNECT BACK WWB TO FORWARD WWB                                00867000
*                                                                       00868000
         MVC   WWBFWDPT-WWB(4,R1),WWBFWDPT                              00869000
         DROP  R2                                                       00870000
*                                                                       00871000
*        OUR WWB IS NOW OUT OF THE CHAIN. FREE IT                       00872000
*                                                                       00873000
         L     R0,WKGETWWB             SET SUBPOOL NO AND SIZE          00874000
         LR    R1,R2                   ADDR OF WWB                      00875000
*                                                                       00876000
         FREEMAIN R,LV=(0),A=(1),BRANCH=YES  FREE THE WWB               00877000
*                                                                       00878000
         XC    XVWWB,XVWWB             ZERO WWB ADDR                    00879000
*                                                                       00880000
*        CHECK IF MAJOR WAS OBTAINED                                    00881000
*                                                                       00882000
IEAJGET4 TM    XVX1,XVX1STOP           STOP PROCESSING SET?             00883000
         BO    IEAJOUT                 YES, SKIP TO END AS NO WQE       00884000
*                                                                       00885000
*        A MAJOR WQE WAS OBTAINED. GET A MINOR                          00886000
*                                                                       00887000
         BAL   R14,GETMINOR                                             00888000
*                                                                       00889000
*        CHECK IF A MINOR WAS OBTAINED                                  00890000
*                                                                       00891000
         TM    XVX1,XVX1STOP           STOP PROC FLAG SET               00892000
         BZ    IEAJBINT                NO, GO INIT THE MAJOR            00893000
*                                                                       00894000
*        A MINOR WASN'T AVAILABLE                                       00895000
*        FREE THE MAJOR WQE                                             00896000
*        THIS SEGMENT FREES THE MAJOR WQE OBTAINED BY BLDMAJ            00897000
*        INPUT XVCMAJOR CONTAINS THE WQE ADDR                           00898000
*                                                                       00899000
         L     R3,ADDRUCM              R3 -> UCM                        00900000
         USING UCM,R3                                                   00901000
         L     R0,UCMWQECP             GET WQE CELL POOL ID             00902000
         L     R1,XVCMAJOR             ADDR OF WQE CELL                 00903000
*                                                                       00904000
         FREECELL  CPID=(0),CELL=(1),BRANCH=YES                         00905000
*                                                                       00906000
*        CHECK IF THIS WQE WAS THE LAST ONE IN AN EXTENSION             00907000
*        IF SO FREE THE EXTENSION                                       00908000
*                                                                       00909000
         CH    R15,KH20                EXTENSION EMPTY ?                00910000
         BNE   IEAJFRE2                NO, SKIP FREEING EXTENSION       00911000
*                                      YES, THE PARAMETERS TO FREE THE  00912000
*                                      EXTENSION WERE SETUP BY FREECELL 00913000
         FREEMAIN  R,LV=(0),A=(1),BRANCH=YES                            00914000
*                                                                       00915000
IEAJFRE2 LH    R1,UCMWQNR              DECREMENT COUNT OF WQES          00916000
         BCTR  R1,0                                                     00917000
         STH   R1,UCMWQNR                                               00918000
         XC    XVCMAJOR,XVCMAJOR       INSURE MAJOR ADDR IS ZERO FOR    00919000
*                                      RECOVERY REASONS                 00920000
         DROP  R3                                                       00921000
         B     IEAJOUT                 GET OUT OF THE SEGMENT           00922000
*                                                                       00923000
*        BMAJINIT                                                       00924000
*                                                                       00925000
*        INITIALIZE THE MAJOR WQE SEGMENT                               00926000
*                                                                       00927000
*        INPUT -                                                        00928000
*        XVCMAJOR HAS ADDR OF MAJOR WQE                                 00929000
*        R4       -> TCB                                                00930000
*        ASCBSAVE -> ASCB                                               00931000
*        XVA8 CONTAINS THE TIME OF DAY                                  00932000
*        XVSAV HAS VARIOUS FLAGS SET TO SHOW CONDITION OF WPL           00933000
*                                                                       00934000
*        OUTPUT -                                                       00935000
*        INITIAL PART OF MAJOR WQE IS CONSTRUCTED AND WQE IS ON         00936000
*        THE QUEUE                                                      00937000
*        THE SUSPEND BIT IS SET ON                                      00938000
*        FIRST CHAR OF TEXT IS BLANK                                    00939000
*        TIME AND ROUTE CODES ARE DECODED AND FILLED IN                 00940000
*                                                                       00941000
IEAJBINT L     R8,XVCMAJOR             R8 -> MAJOR WQE                  00942000
         USING WMJMEXT,R8                                               00943000
         OI    WMJMMLW,WMJMMLWB        SET MAJOR FLAG                   00944000
         OI    WMJMBUF,WMJMBUFB+WMJMBUFD  SET IN USE AND GETMAINED      00945000
         USING TCB,R4                                                   00946000
         ST    R4,WMJMTCB              STORE TCB ADDR                   00947000
         MVC   WMJMJTCB,TCBJSTCB       STORE ADDR OF JOB STEP'S TCB     00948000
*                                      INTO MAJOR WQE                   00949000
         L     R7,ASCBSAVE             RESTORE ASCB ADDR                00950000
         USING ASCB,R7                                                  00951000
         MVC   WMJMASID,ASCBASID       MOVE IN ASID OF CALLER           00952000
         DROP  R7                                                       00953000
         L     R3,ADDRUCM              R3 -> UCM                        00954000
         USING UCM,R3                                                   00955000
         ICM   R2,B'1111',UCMWQEND     END OF OUTPUT QUEUE, ZERO ?      00956000
         BNZ   IEAJLNK                 NO, CHAIN TO LAST WQE            00957000
         LA    R2,UCMWTOQ              R2 -> OUTPUT QUEUE               00958000
IEAJLNK  ST    R8,UCMWQEND             PUT NEW WQE AT END OF QUEUE      00959000
         DROP  R8                                                       00960000
         USING WMJMEXT,R2                                               00961000
         MVC   WMJMNXT(3),UCMWQEND+1   PUT NEW WQE ON QUEUE             00962000
         DROP  R2                                                       00963000
         DROP  R3                                                       00964000
         USING WMJMEXT,R8                                               00965000
         MVC   WMJMCS,WKAMCSF          MOVE MCS FLAGS TO WQE            00966000
         TM    XVD1,XVD1AUTH           USER AUTHORIZED ?                00967000
         BZ    IEAJQ0CK                NO, CHECK ON USE OF QREG0        00968000
         OI    WMJMDSP,WMJMDSPH        YES, SET USER AUTHORIZED FLAG    00969000
         B     IEAJTIME                SKIP TO UNPACK THE TIME          00970000
*                                                                       00971000
IEAJQ0CK TM    WMJMCS1,WMJMCS1H        QREG0 BIT ON ?                   00972000
         BZ    IEAJTIME                NO, SKIP TO UNPACK               00973000
*                                                                       00974000
*        QREG0 FLAG IS ON                                               00975000
*        AN UNAUTHORIZED USER CAN'T USE THE QREG0 MCS FUNCTION          00976000
*        TURN THE FLAG OFF                                              00977000
*                                                                       00978000
         OI    WMJMCS1,WMJMCS1B        TURN ON REG0 BIT INSTEAD         00979000
         NI    WMJMCS1,255-WMJMCS1H    TURN OFF QREG0 FLAG              00980000
IEAJTIME MVC   WQEPAD(9),PATTIME       MOVE IN THE TIME EDIT PATTERN    00981000
         ED    WQEPAD(9),XVA8          FORMAT TIME HH.MM.SS INTO WQETS  00982000
         MVI   WMJMPAD1,C' '           BLANK CHAR AFTER TIME            00983000
*                     WQEPAD2=' ';  /* INSERT BLANK AFTER THE JOBNAME*/ 00984000
         MVI   WQEPAD2,C' '                                             00985000
*                     WQEJOBNM=' '; /* BLANK OUT THE JOB NAME FIELD  */ 00986000
         MVI   WQEJOBNM,C' '                                            00987000
         MVC   WQEJOBNM+1(L'WQEJOBNM-1),WQEJOBNM                        00988000
         MVC   WMJMRR,KC0000           INIT ROUTE CODES TO CHAR ZERO    00989000
         TM    WKAMCSF,WPLMCSFA        ROUT/DESC CODES PROVIDED ?       00990000
         BZ    IEAJTID1                NO, CHECK MSG TYPE               00991000
         MVC   WMJMRTC(2),WKAROC       MOVE ROUTING CODES INTO WQE      00992000
         MVC   WMJMDEC(2),WKADSC       MOVE DESCRIPTOR CODES INTO WQE   00993000
*                                                                       00994000
*        LOOP TO CONVERT ROUTING CODES TO CHARACTER FORMAT              00995000
*                                                                       00996000
         LR    R0,R4                   SAVE TCB ADDR                    00997000
         LA    R1,4                    SET LOOP COUNTER                 00998000
         L     R5,WMJMRTC              ROUTING CODES                    00999000
IEAJRCLP SR    R4,R4                   SET UP FOR SHIFT                 01000000
         SLL   R15,8                   MOVE ROUTING CODES TO R15        01001000
         SLDL  R4,4                                                     01002000
         IC    R15,HEXCHAR(R4)         CONVERT ROUTING CODES            01003000
         BCT   R1,IEAJRCLP             TO PRINTABLE FORM                01004000
         STCM  R15,B'1111',WMJMRR      STORE IN MAJOR WQE               01005000
         LR    R4,R0                   RESTORE TCB ADR                  01006000
*                                                                       01007000
*        END OF ROUTING CODE CONVERSION LOOP                            01008000
*                                                                       01009000
         MVI   WMJMPAD,C' '            BLANK AFTER ROUT CODES           01010000
         MVC   WMJMMT1,WKAMSGTY        MOVE MESSAGE TYPE TO WQE         01011000
IEAJTID1 TM    WKAMCSF,WPLMCSFB+WPLMCSFH  CONSOLE ID PASSED ?           01012000
*                                         QREG0 OR REG0                 01013000
         BZ    IEAJAREA                NO, GO TO AREA ID                01014000
         MVC   WMJMUID,XVCONID         YES, MOVE CONSOLE ID             01015000
IEAJAREA L     R2,ADDRMLHR             R2 -> MLWTO EXTENSION HEADER     01016000
         USING WPLLTF,R2                                                01017000
         MVC   WMJMAREA,WPLAREA        MOVE AREA ID INTO MAJOR          01018000
         TM    XVD1,XVD1AUTH           CALLER AUTHORIZED ?              01019000
         BO    IEAJMBLK                YES. DONT INSERT DEFAULT FOR P/P 01020000
*                                                                       01021000
*        NON AUTHORIZED USERS OF MLWTO GET AN AREA ID OF 'Z' OR ONLINE  01022000
*                                                                       01023000
*        THIS IS TO PREVENT AN UNAUTHORIZED CALLER FROM MESSING         01024000
*        WITH AN AREA BEING USED BY A DYNAMIC STATUS DISPLAY MLWTO      01025000
*                                                                       01026000
         MVI   WMJMAREA,C'Z'           AREA GETS SET TO INLINE (Z)      01027000
IEAJMBLK MVI   WMJMTXT,C' '            BLANK FIRST CHARACTER            01028000
         MVC   WORKBYTE,WMJMAREA                                        01029000
         NI    WORKBYTE,X'0F'                                           01030000
         TM    WMJMAREA,X'F0'                                           01031000
         BO    IEA00690                                                 01032000
         TM    WMJMAREA,X'E0'                                           01033000
         BO    IEA00668                                                 01034000
         TM    WMJMAREA,X'D0'                                           01035000
         BO    IEA00678                                                 01036000
         TM    WMJMAREA,X'C0'                                           01037000
         BO    IEA00678                                                 01038000
         B     IEA00690                                                 01039000
*                                                                       01040000
IEA00668 CLI   WORKBYTE,X'02'                                           01041000
         BL    IEA00690                                                 01042000
         BE    IEA00694                                                 01043000
         B     IEA00684                                                 01044000
*                                                                       01045000
IEA00678 CLI   WORKBYTE,X'01'                                           01046000
         BL    IEA00690                                                 01047000
         BE    IEA00694                                                 01048000
IEA00684 CLI   WORKBYTE,X'09'                                           01049000
         BH    IEA00690                                                 01050000
         B     IEA00694                                                 01051000
*                                                                       01052000
IEA00690 MVI   WMJMAREA,C'Z'                                            01053000
IEA00694 TM    XVD2,XVD2QFHC           QUEUE MESSAGE TO HARD COPY ?     01054000
         BZ    IEA006A0                NO, BRANCH                       01055000
         OI    WMJMDSP,WMJMDSPB        YES, QUEUE WQE TO HARDCOPY       01056000
IEA006A0 TM    WMJMCS2,WMJMCS2F        BYPASS HARD COPY QUEUING ?       01057000
         BZ    IEAJEND                 NO, BRANCH                       01058000
         OI    WMJMBUF,WMJMTRCD                                         01059000
*                                                                       01060000
*        CHECK IF THIS MLWTO SHOULD BE QUEUED TO HC DUE TO CHANGE       01061000
*        IN THE ROUTE CODES BY THE INSTALLATION EXIT                    01062000
*                                                                       01063000
*        END OF BMAJINIT SEGMENT                                        01064000
*                                                                       01065000
IEAJEND  TM    XVX0,XVX0FLCL           FIRST LINE CONTROL LINE ?        01066000
         BO    IEAJLEN4                YES, SKIP TO SET UP FIRST LINE   01067000
         TM    XVX0,XVX0UDCL           USE DEFAULT CONTROL LINE         01068000
         BZ    IEAJLEN4                NO, SKIP TO SET UP FIRST LINE    01069000
*                                                                       01070000
*        SET UP TO MOVE IN THE DEFAULT CONTROL LINE                     01071000
*        THIS IN EFFECT ADDS AN EXTRA LINE TO THE WTO                   01072000
*        THE FIRST LINE IN THE WPL IS NOW PROCESSED AS A LABEL          01073000
*        OR DATA LINE                                                   01074000
*                                                                       01075000
         LA    R15,KIEE932I            USE DEFAULT CONTROL LINE         01076000
         LA    R3,L'KIEE932I           R3 = L' DEFAULT CONTROL LINE     01077000
         LA    R14,WMJMTXT+2           R14 -> TEXT POSITION IN MAJOR    01078000
         B     IEAJMOVM                                                 01079000
*                                                                       01080000
*        BMAJFSTL                                                       01081000
*                                                                       01082000
*        SET UP THE FIRST LINE FOR THE MOVE INTO THE MAJOR              01083000
*        INPUT -                                                        01084000
*        R8 -> MAJOR WQE                                                01085000
*        R2 -> WPLLTF FIELD (MLWTO EXTENSION )                          01086000
*                                                                       01087000
*        OUTPUT -                                                       01088000
*        R15 -> TEXT SOURCE                                             01089000
*        R14 -> TARGET                                                  01090000
*        R3   = L'TEXT TO BE MOVED                                      01091000
*                                                                       01092000
*        PROCESS CONTROL LINE                                           01093000
*                                                                       01094000
IEAJLEN4 LH    R3,WKALGH               R3 = L'MAJOR WQE TEXT + 4        01095000
         SH    R3,KH4                  R3 = L'MAJOR WQE TEXT            01096000
         LA    R14,WMJMTXT+2           R14 -> TEXT POSITION IN WQE      01097000
         L     R15,WKAADTXT            R15 -> LL + MCS + TEXT           01098000
         LA    R15,4(,R15)             R15 -> TEXT                      01099000
         TM    WPLLTF,WPLLTFA          CONTROL LINE ?                   01100000
         BZ    IEAJLNLD                NO                               01101000
         LA    R5,34                   YES, SET L'MAX FOR CONTROL LINE  01102000
         TM    XVD1,XVD1AUTH           USER AUTHORIZED ?                01103000
         BZ    IEAJCHK1                NO, USE LENGTH OF 34 CHAR        01104000
         LA    R5,35                   AUTH USERS CAN HAVE 35 CHARS     01105000
IEAJCHK1 CR    R3,R5                   LENGTH > ALLOWED FOR CNTL LINE ? 01106000
         BNH   IEAJMOVM                NO, MOVE TEXT                    01107000
         LR    R3,R5                   YES, TRUNCATE TO MAX LENGTH      01108000
         B     IEAJMOVM                MOVE TEXT INTO MAJOR             01109000
*                                                                       01110000
*        PROCESS LABEL OR DATA LINE                                     01111000
*                                                                       01112000
IEAJLNLD LA    R5,70                   SET L'MAX FOR LABELA AND DATA    01113000
         TM    XVD1,XVD1AUTH           USER AUTHORIZED ?                01114000
         BZ    IEAJCHK2                NO, USE LENGTH OF 70 CHAR        01115000
         LA    R5,71                   YES,AUTH USERS CAN HAVE 71 CHARS 01116000
IEAJCHK2 CR    R3,R5                   LENGTH > MAX DATA OR LABEL LINE? 01117000
         BNH   IEAJMOVM                NO, MOVE TEXT                    01118000
         LR    R3,R5                   YES, TRUNCATE TO MAX LENGTH      01119000
*                                                                       01120000
*        BMAJMVMS                                                       01121000
*                                                                       01122000
*        MOVE THE MESSAGE INTO THE MAJOR WQE                            01123000
*        INPUT -                                                        01124000
*        R3   = L'MESSAGE                                               01125000
*        R8  -> MAJOR WQE                                               01126000
*        R14 -> TEXT AREA IN WQE                                        01127000
*        R15 -> MESSAGE TEXT                                            01128000
*                                                                       01129000
*        OUTPUT -                                                       01130000
*        THE TEXT WITH AUTHORIZATION FLAGS IS MOVED INTO THE WQE        01131000
*        THE LENGTH AND SEQUENCE NUBMER ARE ALSO ADDED TO THE           01132000
*        WQE                                                            01133000
*                                                                       01134000
IEAJMOVM LR    R5,R3                   LENGTH FOR EXECUTE               01135000
         BCTR  R5,0                    DECR FOR EX                      01136000
         EX    R5,IEAMEXMV             MVC   0(0,R14),0(R15)            01137000
*                                                                       01138000
*        TRUNCATE MESSAGE TO REMOVE TRAILING BLANKS                     01139000
*                                                                       01140000
IEAJBLKL LTR   R5,R5                   R5 ZERO YET ?                    01141000
         BZ    IEAJBLKE                YES, THEN TEXT WAS ALL BLANK     01142000
         LA    R1,0(R5,R14)            R1 -> TEXT CHAR                  01143000
         CLI   R1,C' '                 CHARACTER BLANK ?                01144000
         BNE   IEAJBLKE                NO, GET OUT OF LOOP              01145000
         BCTR  R5,0                    YES DECR INDEX INTO TEXT         01146000
         B     IEAJBLKL                LOOP AND TEST FOR END            01147000
*                                                                       01148000
*        IF THE TEXT WAS ALL BLANK THEN ONLY ONE BLANK IS USED          01149000
*        THE FOLLOWING INSTRUCTION PUTS THE CORRECT LENGTH INTO         01150000
*        R3 WHEN THE TEXT IS ALL BLANK OR NOT ALL BLANKS                01151000
*                                                                       01152000
IEAJBLKE LA    R3,1(,R5)               SET R3 TO L'TEXT                 01153000
*                                      WITH TRAILING BLANKS TRUNCATED   01154000
         TM    WMJMDEC1,WMJMDECA+WMJMDECB  DESC CODE 1 OR 2 ?           01155000
         BNZ   IEA0076E                YES, GO CHECK AUTHORIZATION      01156000
         TM    WMJMDEC2,WMJMDECK       CRITICAL EVENTUAL ACTION MSG ?   01157000
         BNZ   IEAJAUT1                NO, BRANCH                       01158000
         TM    XVD1,XVD1AUTH           APF AUTHORIZED ?                 01159000
         BO    IEAJSTXT                YES, OMIT P/P FLAG               01160000
         MVI   WMJMTXT+1,PPWTOFLG      + FLAG, PROB PGM - NO ACTION MSG 01161000
         LA    R3,2(,R3)               UPDATE LENGTH                    01162000
         B     IEAJSTOR                                                 01163000
*                                                                       01164000
IEAJAUT1 TM    XVD1,XVD1AUTH           AUTHORIZED ?                     01165000
         BZ    IEAJMVAT                NO, BRANCH                       01166000
         B     IEA00776                                                 01167000
*                                                                       01168000
IEA0076E TM    XVD1,XVD1AUTH           AUTHORIZED ?                     01169000
         BZ    IEA0077E                NO, BRANCH                       01170000
IEA00776 MVI   WMJMTXT,SUPACFLG        * FLAG, SUPERVISOR ACTION MSG    01171000
         B     IEAJSTXT                                                 01172000
*                                                                       01173000
IEA0077E OI    WMJMDEC1,WMJMDECG       TURN ON DESC CODE 7              01174000
*                                      APPLICATION PROGRAM MESSAGE      01175000
*                                                                       01176000
*        ISSUER IS A NON-AUTHORIZED PROGRAM PROBLEM                     01177000
*        DESC CODE 1 OR 2 SPECIFIED                                     01178000
*        FORCE DESC CODE 7 ON SO MLWTO WILL BE DOMMED AT TASK           01179000
*        TERMINATION                                                    01180000
*                                                                       01181000
IEAJMVAT MVI   WMJMTXT,PPACTFLG        @ FLAG, PROB PGM ACTION MSG FLAG 01182000
*                                                                       01183000
*        SHUFFLE TEXT DOWN ONE BYTE                                     01184000
*                                                                       01185000
IEAJSTXT LA    R14,WMJMTXT+1           SHIFT TEXT TO CHAR POS 2         01186000
         LA    R15,WMJMTXT+2           SET REGS FOR TEXT SHIFT          01187000
         LR    R5,R3                   LENGTH FOR EXECUTE               01188000
         BCTR  R5,0                    DECR FOR EXECUTE                 01189000
         EX    R5,IEAMSHFT             SHIFT TEXT 1 CHAR TO THE LEFT    01190000
*                                      MVC   0(0,R14),0(R15)            01191000
         LA    R14,WMJMTXT+1           R14 -> TEXT+1                    01192000
         AR    R14,R3                  ADD L'TEXT                       01193000
         MVI   0(R14),0                ZERO FIRST CHAR AFTER TEXT       01194000
         LA    R3,1(,R3)               UPDATE LENGTH                    01195000
IEAJSTOR STH   R3,WMJMTXTL             STORE TEXT LENGTH                01196000
         L     R1,ADDRUCM              R1 -> UCM                        01197000
         SH    R1,KH4                  SUBTRACT TO REFERENCE PREFIX PTR 01198000
         L     R1,0(,R1)               LOAD UCM PREFIX ADDR             01199000
*                                                                       01200000
*        PLACE UCM SEQUENCE NUMBER INTO THE WQE                         01201000
*        SETUP TO PASS BACK THE UCM SEQUENCE NUMBER TO THE CALLER       01202000
*        INCREMENT THE UCM SEQUENCE NUMBER                              01203000
*                                                                       01204000
         USING UCMPRFX,R1                                               01205000
         MVC   WMJMSEQ(3),UCMCMID+1    MOVE SEQ NO TO MAJOR             01206000
         MVC   WMJMMSGN+1(3),UCMCMID+1  MLWTO ID                        01207000
         MVC   XVWQEIDA,UCMCMID+1      SAVE MSG ID FOR NEXT LINES       01208000
*                                      FOR PASS BACK TO THE CALLER      01209000
         L     R5,UCMCMID                                               01210000
         CVD   R5,WORK8                CONVERT ID TO DECIMAL            01211000
         LA    R5,1(,R5)               INCREMENT SEQ NUMBER             01212000
         ST    R5,UCMCMID              STORE UPDATED SEQ NO IN UCM      01213000
         MVC   WMJMHCID,PATUCMID       MOVE IN PATTERN FOR ED INST      01214000
         ED    WMJMHCID,WORK8+6        CONVERT TO PRINTABLE FORM        01215000
         DROP  R1                                                       01216000
         TM    WMJMDEC2,WMJMDECI       DESCRIPTOR CODE 9 ?              01217000
         BZ    IEAJONNN                NO, NO UCM SEQ ON CONTROL LINE   01218000
*                                                                       01219000
*        DESCRIPTOR CODE 9 PROCESSING                                   01220000
*        PLACE UCM SEQUENCE NUMBER AT END OF TEXT                       01221000
*                                                                       01222000
         LA    R15,WMJMTXT             R15 -> TEXT IN MAJOR             01223000
         AH    R15,WMJMTXTL            ADD LENGTH TO TEXT ADDR          01224000
         MVC   0(L'WMJMHCID,R15),WMJMHCID  MOVE ID TO END OF TEXT       01225000
         MVI   L'WMJMHCID(R15),C' '    PUT BLANK AFTER ID IN MSG        01226000
         LH    R3,WMJMTXTL             R3 = L'TEXT                      01227000
         LA    R3,L'WMJMHCID+1(,R3)    INCR LENGTH FOR ID               01228000
         STH   R3,WMJMTXTL             SAVE UPDATED L'TEXT              01229000
*                                                                       01230000
IEAJONNN OI    WMJMDSP,WMJMDSPG        SET MAJOR SUSPENDED              01231000
         TM    XVX0,XVX0UDCL           USE DEFAULT CONTROL LINE ?       01232000
         BZ    IEAJMVLT                NO, MOVE IN LINE TYPE FLAGS      01233000
*                                                                       01234000
*        THE DEFAULT CONTROL LINE IS BEING USED                         01235000
*                                                                       01236000
*        THE FIRST MESSAGE TEXT IN THE WPL WILL BE PROCESSED AS         01237000
*        A LABEL OR DATA MESSAGE                                        01238000
*                                                                       01239000
         OI    WMJMLTYP,WMJMLTYA       YES, SET LINE TYPE TO CONTROL    01240000
         B     IEAJOUT                 END OF SEGMENT                   01241000
*                                                                       01242000
IEAJMVLT MVC   WMJMLTYP,WPLLTF         MOVE LINE TYPE FLAGS TO MAJOR    01243000
         DROP  R2                                                       01244000
         BAL   R14,TEXTLINE            INCR TO NEXT LINE                01245000
         BAL   R14,ENDUP                                                01246000
IEAJOUT  OI    XVD2,XVD2CON            INSURE CONNECTING BIT IS SET     01247000
         B     IEAMSTTS                GO TEST IF STOP WAS SET          01248000
*                                                                       01249000
*        START OF BLDMIN SEGMENT                                        01250000
*        BUILD THE MINOR WQE                                            01251000
*                                                                       01252000
IEAMBMIN BAL   R14,FINDID              FIND ID FOR CONNECTING TO        01253000
         TM    XVX1,XVX1NOID           ID FOUND ?                       01254000
         BZ    IEA1YID                 YES, CHECK IF MINOR NEEDED       01255000
*                                                                       01256000
*        NO ID FOUND. STOP PROCESSING                                   01257000
*                                                                       01258000
         OI    XVX1,XVX1STOP           SET STOP PROCESSING FLAG         01259000
         B     IEAMSTTS                                                 01260000
*                                                                       01261000
IEA1YID  L     R8,XVCMAJOR             R8 -> MAJOR WQE                  01262000
         USING WMJMEXT,R8                                               01263000
         OI    WMJMMLW,WMJMMLWD        SET CHAIN ALTERED FLAG           01264000
*                                                                       01265000
*        THERE WILL ALWAYS BE A MINOR WQE QUEUED OFF THE MAJOR          01266000
*        FINDID WILL STORE THE ADDRESS OF THE MINOR TO USE IN           01267000
*        XVCMINOR FOR US                                                01268000
*                                                                       01269000
         L     R7,XVCMINOR             R7 ->MINOR WQE FOR CONNECTING TO 01270000
         TM    XVD3,XVD3BLD1+XVD3BLD2  BOTH LINES AVAILABLE ?           01271000
         BNZ   IEA1TLN1                BRANCH, EITHER LINE AVAILABLE    01272000
*                                                                       01273000
*        NO LINE AVAILABLE IN LAST MINOR. GET ANOTHER ONE               01274000
*                                                                       01275000
IEA1ALOC L     R3,ADDRUCM              R3 -> UCM                        01276000
         USING UCM,R3                                                   01277000
         CLC   UCMWQNR,UCMWQLM         WQE AVAILABLE ?                  01278000
         BL    IEA1GETN                YES, GET A MINOR                 01279000
         TM    XVD1,XVD1PRIV           PRIVILEGED USER ?                01280000
         BO    IEA1GETN                YES, WQES ALWAYS AVAILABLE       01281000
         DROP  R3                                                       01282000
*                                                                       01283000
*        BRANCH TO THE WAIT ROUTINE AND WAIT WITHOUT AN ECB             01284000
*        THIS TYPE OF WAIT IS DONE SO THAT CONTROL WILL RETURN          01285000
*        BACK AFTER OUT MAJOR HAS BEEN DELETED BY IEAVMDSV              01286000
*        THE BRANCH ENTRY IS USED SO THAT WE HOLD THE LOCAL             01287000
*        LOCK. THIS PREVENTS US FROM BEING POSTED BEFORE WE ISSUE       01288000
*        THE WAIT TURN ON THE WAITING BIT IN THE MAJOR WQE              01289000
*                                                                       01290000
         OI    WMJMECBF,WMJMWAIT                                        01291000
*                                                                       01292000
*        DONT ALLOW ASYNCHRONOUS EXITS TO BE DISPATCHED WHILE           01293000
*        WAITING FOR A WQE. THIS COULD CAUSE AN INTERLOCK IF THE        01294000
*        RB ISSUED A WTO THAT THEN WAITED                               01295000
*        SET ADDRESSABILITY TO TCB AND SAVE R4                          01296000
*                                                                       01297000
         ST    R4,REG4SAV              SAVE ADDR OF TEXT LINE           01298000
         L     R4,TCBSAVE              RESTORE TCB ADDR                 01299000
         USING TCB,R4                                                   01300000
         NI    XVD3,255-XVD3TFX        TURN OFF 'MWTO SET TFX' FLAG     01301000
         TM    TCBFLGS1,TCBFX          ASYNCH EXITS NOT ALLOWED ?       01302000
         BO    IEA1NSET                YES, DONT SET THE TCBFX FLAG     01303000
*                                                                       01304000
*        THE ASYNCHRONOUS EXITS ARENT ALLOWED BY SOME OTHER             01305000
*        ROUTINE SO WE SHOULD LEAVE THE TCBFX BIT ALONE                 01306000
*                                                                       01307000
         OI    TCBFLGS1,TCBFX          NO, FLAG NOT SET SO SET IT       01308000
         OI    XVD3,XVD3TFX            SET FLAG SHOWING THAT            01309000
*                                                                       01310000
*        SET THE TCBFX FLAG SO CAN TURN IT OFF FREE THE CMS LOCK        01311000
*                                                                       01312000
IEA1NSET ST    R11,REG11SAV            SAVE BASE REG                    01313000
         ST    R12,REG12SAV            SAVE XV BASE                     01314000
         ST    R9,REG9SAV                                               01315000
         LR    R0,R13                  SAVE ADDR OF MODULE WORKAREA     01316000
*                                                                       01317000
*        FREE THE FRR                                                   01318000
*                                                                       01319000
         SETFRR D,WRKREGS=(9,12)                                        01320000
         LA    R12,EPARM               R12 -> ESTAE PARM AREA           01321000
         ST    R12,PARMPTR             PT TO ESTAE PARM AREA            01322000
*                                                                       01323000
         SETLOCK RELEASE,TYPE=CMS,RELATED=(UCM,IEAVMWTO(SETLCKS))       01324000
*                                                                       01325000
         LR    R13,R0                  RESTORE WORKAREA BASE            01326000
         L     R9,REG9SAV              RESTORE R9, R11 AND R12          01327000
         L     R11,REG11SAV                                             01328000
         L     R12,REG12SAV                                             01329000
*                                                                       01330000
*        SET UP FOR BRANCH ENTRY TO WAIT                                01331000
*                                                                       01332000
*        SET UP RETURN ADDR FROM WAIT BY STORING THE RETURN POINT       01333000
*        ADDR IN THE OLD PSW IN OUR RB                                  01334000
*                                                                       01335000
         LA    R15,IEA1WTRT            LOAD ADDR OF RETURN POINT        01336000
         ST    R15,RBOPSW+4            SAVE IN RB                       01337000
         ST    R12,WMJMAECB            STORE RB ADDR IN MAJOR           01338000
*                                                                       01339000
*        IEAVMDSV USES ADDR IN WMJMAECB AS THE RB TO POST               01340000
*                                                                       01341000
         L     R1,CVTPTR               ADDR THE CVT                     01342000
         USING CVT,R1                                                   01343000
         L     R15,CVTVWAIT            BRANCH ENTRY TO WAIT             01344000
         DROP  R1                                                       01345000
         SR    R1,R1                   WAIT WITHOUT ECB                 01346000
         LA    R0,1                    WAIT COUNT OF ONE                01347000
         STM   R0,R15,TCBGRS           STORE ALL REGS IN TCB            01348000
         BR    R15                     BRANCH ENTRY TO WAIT             01349000
*                                                                       01350000
*        THE REGISTERS ARE RESTORED BY WAIT                             01351000
*        WAIT WILL FREE THE LOCAL LOCK                                  01352000
*        GET BOTH LOCKS BEFORE RESUMING PROCESSING                      01353000
*                                                                       01354000
IEA1WTRT BAL   R15,SETLCKS             SET LOCAL AND CMS LOCKS          01355000
*                                                                       01356000
*        CHECK IF NEED TO TURN OFF THE TCBFX FLAG                       01357000
*        R4 -> TCB                                                      01358000
*                                                                       01359000
         TM    XVD3,XVD3TFX            TCBFX FLAG SET ?                 01360000
         BZ    IEA1RSET                NO, LEAVE FLAG ALONE             01361000
         NI    TCBFLGS1,255-TCBFX      YES, TURN TCBFX FLAG OFF         01362000
         DROP  R4                                                       01363000
IEA1RSET L     R4,REG4SAV              RESTORE R4 -> CURRENT LINE       01364000
         B     IEAMBMIN                FIND ID AGAIN TO INSURE THAT IT  01365000
*                                      HAS NOT BEEN PURGED WHILE SYSTEM 01366000
*                                      WAS ENABLED                      01367000
IEA1GETN BAL   R14,GETMINOR            GET A MINOR WQE                  01368000
*                                                                       01369000
*        BUILD LINE 1 OF A MINOR WQE                                    01370000
*                                                                       01371000
IEA1TLN1 OI    WMJMDSP,WMJMDSPG        SET MAJOR SUSPENDED              01372000
         TM    XVD3,XVD3BLD1           BUILD LINE 1 OF MINOR ?          01373000
         BZ    IEA2BLN2                NO, CHECK IF LINE 2              01374000
*                                                                       01375000
*        INITIALIZE LINE 1 OF THE MINOR                                 01376000
*                                                                       01377000
         L     R7,XVCMINOR             R7 -> MINOR                      01378000
         NI    WMJMMLW,255-WMJMMLWH    TURN OFF DUMMY MINOR FLAG        01379000
         DROP  R8                                                       01380000
         USING WMNMEXT,R7                                               01381000
         MVC   WMNMUC1(1),0(R8)        MOVE USE COUNT TO MINOR LINE 1   01382000
         NI    XVD3,255-XVD3BLD1       RESET BUILD LINE 1 FLAG          01383000
         OI    WMNMML2,WMNMML2H        SET 2ND LINE AVAILABLE           01384000
         TM    XVD3,XVD3TXT1           TEXT LINE 1 BEING USED ?         01385000
         BZ    IEA1TXL2                NO, MOVE LINE TYPE TO WQE        01386000
*                                      YES, SETUP TO MOVE FIRST         01387000
*                                      LINE INTO WQE                    01388000
         L     R15,WKAADTXT            R15 -> FIRST TEXT LINE           01389000
         LA    R15,4(,R15)             R15 -> TEXT                      01390000
         LH    R3,WKALGH               R3 = L'TEXT + 4                  01391000
         L     R2,ADDRMLHR             R2 -> MLWTO EXTENSION HEADER     01392000
*                                            FOR ACCESS TO LINE TYPE    01393000
         B     IEA1VTYP                                                 01394000
*                                                                       01395000
*        SETUP TO USE SECOND OR SUBSEQUENT TEXT LINES                   01396000
*                                                                       01397000
         USING WPLML,R4                R4 -> SUBSEQUENT LINE TO USE     01398000
IEA1TXL2 LA    R15,WPLMLTXT            R15 -> TEXT                      01399000
         LH    R3,WPLML0               R3 =L'THIS LINE                  01400000
         LA    R2,WPLMLLTF             R2 -> LINE TYPE FLAGS            01401000
         DROP  R4                                                       01402000
IEA1VTYP MVC   WMNMLT1(1),0(R2)        MOVE LINE TYPE TO MINOR          01403000
*                                      FROM EITHER THE MLWTO            01404000
*                                      EXTENSION HEADER OR FROM THE     01405000
*                                      SECOND OR SUBSEQUENT LINE        01406000
         TM    XVX0,XVX0FLJE           FIRST LINE JUST AN END LINE ?    01407000
         BO    IEA1DECR                YES, SKIP MOVING THE MSG         01408000
         TM    WMNMLT1,WMNMLT1D        THIS LINE END LINE ?             01409000
         BZ    IEA1TXTL                NO, GET TEXT LENGTH              01410000
         TM    WMNMLT1,WMNMLT1C        DATA AND END LINE ?              01411000
         BZ    IEA1DECR                NO, JUST END LINE                01412000
*                                                                       01413000
*        MIN1MOV                                                        01414000
*                                                                       01415000
*        MOVE IN LINE 1 OF THE MINOR                                    01416000
*        SET UP FOR MESSAGE LENGTH TEST                                 01417000
*                                                                       01418000
IEA1TXTL SH    R3,KH4                  ADJUST TEXT LENGTH               01419000
*                                                                       01420000
*        CHECK IF USER IS AUTHORIZED. IF SO ALLOW 71 CHARS IN TEXT      01421000
*                                                                       01422000
         LA    R1,70                   R1 =L'MAX OF UNAUTHORIZED CALLER 01423000
         TM    XVD1,XVD1AUTH           CALLER AUTHORIZED ?              01424000
         BZ    IEA1TXT2                NO, THEN ONLY ALLOW 70 CHARS     01425000
         LA    R1,71                   YES, LET USER PUT OUT 71 CHARS   01426000
IEA1TXT2 CR    R3,R1                   EXCEED MAXIMUM ALLOWED L'LINE    01427000
         BNH   IEA1BLK1                NO, USE TEXT LENGTH              01428000
         LR    R3,R1                   OTHERWISE, TRUNCATE              01429000
IEA1BLK1 MVI   WMNMTXT1,C' '           MOVE BLANK AS FIRST CHAR         01430000
         TM    XVD1,XVD1AUTH           USER APF AUTHORIZED ?            01431000
         BZ    IEA1OVE3                NO, BRANCH                       01432000
         LA    R14,WMNMTXT1+1          R14 -> MINOR WQE TEXT AREA       01433000
         LR    R5,R3                   LENGTH FOR EXECUTE               01434000
         BCTR  R5,0                    DECR FOR EXECUTE                 01435000
         EX    R5,IEAMVTXT             MOVE TEXT TO MINOR WQE           01436000
*                                      MVC   0(0,R14),0(R15)            01437000
         LA    R3,1(,R3)               UPDATE TEXT LENGTH               01438000
         B     IEA1STL1                                                 01439000
*                                                                       01440000
*        PROCESS UNAUTHORIZED REQUEST                                   01441000
*                                                                       01442000
IEA1OVE3 MVI   WMNMTXT1+1,C' '         BLANK SECOND CHAR                01443000
         LA    R14,WMNMTXT1+2          R14 -> MINOR WQE TEXT AREA       01444000
         LR    R5,R3                   LENGTH FOR EXECUTE               01445000
         BCTR  R5,0                    DECR  FOR EX                     01446000
         EX    R5,IEAMVTXT             MOVE TEXT                        01447000
*                                      MVC   0(0,R14),0(R15)            01448000
         LA    R3,2(,R3)               UPDATE TEXT LENGTH               01449000
IEA1STL1 STC   R3,WMNMTL1              STORE TEXT LENGTH IN MINOR       01450000
         LA    R14,WMNMTXT1            R14 -> TEXT AREA 2               01451000
         L     R8,XVCMAJOR             R8 -> MAJOR WQE                  01452000
         MVC   WMNMHCT1,WMJMHCID-WMJMEXT(R8)  MOVE HARD COPY ID         01453000
*                                      FROM MAJOR T0 MINOR WQE          01454000
*                                                                       01455000
*        TRUNCATE ANY TRAILING BLANKS                                   01456000
*                                                                       01457000
IEA1LOOP BCT   R3,IEA1LOOA             DECR INDEX INTO TEXT             01458000
         B     IEA1DONE                ZERO, LOOP COMPLETED             01459000
IEA1LOOA LA    R15,0(R3,R14)           R15 -> NEXT CHAR                 01460000
         CLI   0(R15),C' '             BLANK ?                          01461000
         BE    IEA1LOOP                YES, RE-ENTER LOOP               01462000
*                                                                       01463000
IEA1DONE LA    R3,1(,R3)               R3 NOW HAS CORRECT LENGTH        01464000
         STC   R3,WMNMTL1                                               01465000
         BAL   R14,TEXTLINE            UPDATE TEXT POINTER              01466000
         B     IEA1DECR                SKIP TO ENDUP ROUTINE            01467000
*                                                                       01468000
*        MIN2INIT                                                       01469000
*                                                                       01470000
*        BUILD LINE 2 OF A MINOR WQE                                    01471000
*                                                                       01472000
IEA2BLN2 L     R7,XVCMINOR             R7 -> MINOR WQE                  01473000
         MVC   WMNMUC2(1),0(R8)        COPY USE COUNT FROM MAJOR        01474000
         LA    R2,WMNMUC2              R2 -> 2ND MINOR LINE             01475000
         O     R2,WMNMUC1              PRESERVE USE COUNT               01476000
         ST    R2,WMNMUC1              CHAIN TO FIRST LINE              01477000
         OI    WMNMML2,WMNMML2C        INDICATE MINOR                   01478000
         NI    XVD3,255-XVD3BLD2       TURN OFF BUILD 2ND LINE FLAG     01479000
         NI    WMNMML2,255-WMNMML2H    RESET 2ND LINE AVAILABLE         01480000
         TM    XVD3,XVD3TXT1           TEXT LINE 1 BEING USED ?         01481000
         BZ    IEA2TX2A                NO, BRANCH                       01482000
*                                                                       01483000
*        PROCESS TEXT LINE 1                                            01484000
*                                                                       01485000
         L     R15,WKAADTXT            R15 -> MCS HEADER + TEXT         01486000
         LA    R15,4(,R15)             R15 -> TEXT                      01487000
         LH    R3,WKALGH               R3 = L'TEXT +4                   01488000
         L     R2,ADDRMLHR             R2 -> MLWTO EXTENSION HEADER     01489000
         B     IEA2MVL2                                                 01490000
*                                                                       01491000
*        PROCESS SECOND AND SUBSEQUENT LINES                            01492000
*                                                                       01493000
         USING WPLML,R4                                                 01494000
IEA2TX2A LA    R15,WPLMLTXT            R15 -> TEXT                      01495000
         LH    R3,WPLML0               R3 = L'MESSAGE                   01496000
         LA    R2,WPLMLLTF             POINT TO LINE TYPE               01497000
         DROP  R4                                                       01498000
IEA2MVL2 MVC   WMNMLT2(1),0(R2)        MOVE LINE TYPE TO 2ND LINE       01499000
         TM    XVX0,XVX0FLJE           FIRST LINE JUST END              01500000
         BO    IEA1DECR                YES, POST WTO ECB                01501000
         TM    WMNMLT2,WMNMLT2D        END LINE ?                       01502000
         BZ    IEA2TXTL                NO, BRANCH                       01503000
         TM    WMNMLT2,WMNMLT2C        YES, DATA AND END LINE ?         01504000
         BZ    IEA1DECR                NO, JUST END LINE, POST ECB      01505000
*                                                                       01506000
*        MIN2MOV                                                        01507000
*                                                                       01508000
*        MOVE IN LINE 2 OF THE MINOR                                    01509000
*        CHECK WHETHER TO ALLOW 70 OR 71 CHARACTERS OF TEXT             01510000
*                                                                       01511000
IEA2TXTL LA    R1,70                   R1 = MAX NON-AUTHORIZED L'TEXT   01512000
         TM    XVD1,XVD1AUTH           USER AUTHORIZED ?                01513000
         BZ    IEA2GLEN                NO, BRANCH                       01514000
         LA    R1,71                   YES, ALLOW MAX 71 CHARS          01515000
IEA2GLEN SH    R3,KH4                  ADJUST FOR FLAGS                 01516000
         CR    R3,R1                   L'TEXT > MAXIMUM ALLOWED ?       01517000
         BNH   IEA2BTXT                NO, BRANCH                       01518000
         LR    R3,R1                   YES, TRUNCATE L'TEXT             01519000
IEA2BTXT MVI   WMNMTXT2,C' '           BLANK FIRST CHAR                 01520000
         TM    XVD1,XVD1AUTH           CALLER APF AUTHORIZED ?          01521000
         BZ    IEA2OVE4                NO, BRANCH                       01522000
         LA    R14,WMNMTXT2+1          R14 -> MINOR TEXT AREA           01523000
         LR    R5,R3                   LENGTH FOR EXECUTE               01524000
         BCTR  R5,0                    ADJUST FOR EXECUTE               01525000
         EX    R5,IEAMVTXT             MOVE TEXT TO MINOR               01526000
*                                      MVC   0(0,R14),0(R15)            01527000
         LA    R3,1(,R3)               INC MINOR LENGTH BY 1            01528000
         B     IEA2STL2                                                 01529000
*                                                                       01530000
IEA2OVE4 MVI   WMNMTXT2+1,C' '         BLANK SECOND CHAR                01531000
         LA    R14,WMNMTXT2+2          R14 -> MINOR TEXT AREA           01532000
         LR    R5,R3                   LENGTH FOR EXECUTE               01533000
         BCTR  R5,0                    ADJUST FOR EXECUTE               01534000
         EX    R5,IEAMVTXT             MVC   0(0,R14),0(R15)            01535000
*                                                                       01536000
         LA    R3,2(,R3)               UPDATE LENGTH                    01537000
IEA2STL2 STC   R3,WMNMTL2              STORE MINOR LENGTH               01538000
         LA    R14,WMNMTXT2            R14 -> TEXT AREA 2               01539000
         MVC   WMNMHCT2,WMNMHCT1       MOVE IN HARD COPY ID             01540000
*                                                                       01541000
*        TRUNCATE ANY TRAILING BLANKS                                   01542000
*                                                                       01543000
IEA2LOOP BCT   R3,IEA2LOOA             DECR INDEX INTO TEXT             01544000
         B     IEA2DONE                BRANCH, LOOP COMPLETED           01545000
IEA2LOOA LA    R15,0(R3,R14)           R15 -> NEXT CHAR                 01546000
         CLI   0(R15),C' '             BLANK ?                          01547000
         BE    IEA2LOOP                RE-ENTER LOOP                    01548000
*                                                                       01549000
IEA2DONE LA    R3,1(,R3)               R3 NOW HAS CORRECT LENGTH        01550000
         STC   R3,WMNMTL2                                               01551000
         BAL   R14,TEXTLINE            UPDATE TEXT POINTER              01552000
IEA1DECR BAL   R14,ENDUP                                                01553000
*                                                                       01554000
IEAMSTTS TM    XVX1,XVX1STOP           ERROR FOUND ?                    01555000
         BO    IEAMFREL                YES, DON'T TAKE EXIT OR POST     01556000
         TM    XVD2,XVD2DELW           MESSAGE TO BE DELETED ?          01557000
         BZ    IEAMNOST                NO, TAKE SUBSYSTEM EXIT          01558000
         DROP  R7                      RELEASE MINOR BASE               01559000
         USING WMJMEXT,R8              ADDRESSABILITY FOR MAJOR         01560000
*                                                                       01561000
*        SET UP THE FLAGS SO THAT THIS MLWTO IS DELETED AND SENT        01562000
*        TO HARDCOPY                                                    01563000
*                                                                       01564000
         OI    WMJMECBF,WMJMMAJD       INDICATE MAJOR IS DELETED        01565000
         L     R1,ADDRUCM              R1 -> UCM                        01566000
         SH    R1,KH4                  ADDR UCM PREFIX                  01567000
         L     R1,0(,R1)               R1 -> UCM PREFIX                 01568000
         USING UCMPRFX,R1              ADDRESSABILITY FOR PREFIX        01569000
         OI    UCMSFLG2,UCMSYSI        INDICATE CLEANUP NEEDED          01570000
         OI    WMJMBUF,WMJMBUFC+WMJMBUFE  INDICATE WQE SERVICED         01571000
*                                         AND READY FOR HARDCOPY        01572000
         OI    WMJMDSP,WMJMDSPB        INDICATE QUEUE TO H.C.           01573000
         DROP  R1                                                       01574000
*                                                                       01575000
*        SUBSYSTEM EXIT SEGMENT                                         01576000
*                                                                       01577000
*        THIS SEGMENT WILL PASS THE MAJOR AND MINOR LINES TO THE        01578000
*        SUBSYSTEM EXIT. THE SUBSYSTEM MAY CHANGE THE MSG AND/OR        01579000
*        ASK THAT THE MESSAGE BE DELETED                                01580000
*                                                                       01581000
*        INPUT -                                                        01582000
*        R8 -> MAJOR WQE                                                01583000
*        XVCMINOR -> THE MINOR WQE                                      01584000
*        XVC3BLD1 AND BLD2 WILL BOTH BE ON IF THE MAJOR WQE WAS         01585000
*        JUST BUILT                                                     01586000
*                                                                       01587000
*        OUTPUT -                                                       01588000
*        IF THE SUBSYSTEM ASKS TO DELETE THE MAJOR WQE THEN             01589000
*        XVD2DELW WILL BE TURNED ON AND WMJMMAJD WILL BE SET ON         01590000
*        IN THE MAJOR WQE                                               01591000
*                                                                       01592000
*        THE MINOR MAY ALSO BE ASKED TO BE DELETED, BUT THE             01593000
*        REQUEST WILL ONLY BE HONORED IF WMJMMAJD IS ON                 01594000
*                                                                       01595000
*        FREE THE LOCKS                                                 01596000
*                                                                       01597000
IEAMNOST BAL   R15,FRELCKS             CALL FRELCKS ROUTINE             01598000
*                                                                       01599000
*        SET UP RETRY ADDRESS IN CASE SUBSYSTEM EXIT HAS PROBLEMS       01600000
*                                                                       01601000
         L     R1,PARMPTR              R1 -> PARM LIST                  01602000
         USING PARMLIST,R1                                              01603000
         LA    R2,IEAHSSRT             R2 -> RETRY ADDR                 01604000
         ST    R2,PARMRTAD             SWITCH ADDR IN ESTAE PARMLIST    01605000
         MVI   PARMFTPT,FTSSOB         SET FOOTPRINT                    01606000
         LA    R2,SUBSLIST             BUILD BLOCKS IN OUR STORAGE      01607000
         STM   R0,R15,RECREGS          SAVE THE REGS AT THIS POINT      01608000
         DROP  R1                                                       01609000
*                                                                       01610000
*        SET UP SSOB AND SSWT                                           01611000
*                                                                       01612000
         ST    R2,SUBSPARM             BUILD PARM LIST PTR              01613000
         USING SSOB,R2                                                  01614000
         MVC   SSOBID,KCSSOB           IDENTIFY BLOCK AS AN SSOB        01615000
         LA    R1,SSOBHSIZ             LENGTH OF SSOB                   01616000
         STH   R1,SSOBLEN              STORE INTO SSOB LENGTH FIELD     01617000
         LA    R1,SSOBWTO              IDENTIFY THAT THIS CALL IS       01618000
         STH   R1,SSOBFUNC             FOR A WTO                        01619000
         SR    R5,R5                   CLEAR REG FOR ZEROING FIELDS     01620000
         ST    R5,SSOBSSIB             NO SSIB FOR THIS CALL            01621000
         ST    R5,SSOBRETN             INSURE RETN IS INITIALLY ZERO    01622000
         LA    R1,SSWTBGN              POINT SSOB AT SSWT               01623000
         ST    R1,SSOBINDV                                              01624000
         LA    R1,SSWTSIZE             PUT IN SIZE OF SSWT              01625000
         STH   R1,SSWTLEN                                               01626000
         ST    R8,SSWTWQE              PUT IN ADDR OF MAJOR             01627000
         ST    R5,SSWTORE              NO ORE THIS TRIP                 01628000
         TM    XVD3,XVD3BLD1+XVD3BLD2  MAJOR WQE ?                      01629000
         BNO   IEAHMINS                NO, BRANCH                       01630000
         ST    R5,SSWTMIN              NO MINOR YET                     01631000
         B     IEAHSSGO                                                 01632000
*                                                                       01633000
*        RETRY ROUTINE IF SUBSYSTEM EXIT ERROR REACHED ESTAE            01634000
*                                                                       01635000
IEAHSSRT XC    SSOBRETN,SSOBRETN       ZERO RETURN CODE FROM SUBSYSTEM  01636000
         L     R1,PARMPTR                                               01637000
         USING PARMLIST,R1                                              01638000
         XC    PARMRTAD,PARMRTAD       ZERO RETRY ADDR                  01639000
         DROP  R1                                                       01640000
         B     IEAHLOCK                                                 01641000
*                                                                       01642000
IEAHMINS MVC   SSWTMIN,XVCMINOR        MOVE IN ADDR OF MINOR            01643000
IEAHSSGO LA    R1,SUBSPARM             R1 -> PARM LIST POINTER          01644000
*                                                                       01645000
         IEFSSREQ  ,                   CALL THE SUBSYSTEM               01646000
         LR    R5,R15                  SAVE RETURN CODE FROM EXIT       01647000
*                                                                       01648000
*        SET THE LOCKS AGAIN                                            01649000
*                                                                       01650000
IEAHLOCK BAL   R15,SETLCKS                                              01651000
*                                                                       01652000
*        CHECK TO SEE IF THE SUBSYSTEM HAS SPECIFIED HARDCOPY BYPASS    01653000
*                                                                       01654000
         TM    WMJMCS2,WMJMCS2F        HARDCOPY BYPASS SET ?            01655000
         BZ    IEAHHCRD                NO, BRANCH                       01656000
         NI    WMJMDSP,255-WMJMDSPB    YES, INDICATE DO NOT HC          01657000
*                                                                       01658000
*        CHECK TO SEE IF THIS WQE IS FOR A MINOR                        01659000
*                                                                       01660000
IEAHHCRD TM    XVD3,XVD3BLD1+XVD3BLD2  BUILDING MAJOR ?                 01661000
         BNO   IEAHSUSP                NO, GO TURN OFF SUSPEND FLAG     01662000
*                                                                       01663000
*        CHECK IF SYBSYSTEM WANTS MESSAGE DELETED                       01664000
*        IF THE EXIT WAS SUCCESSFUL THEN R15 WAS ZERO ON RETURN         01665000
*        FROM THE SUBSYSTEM                                             01666000
*        CHECK R1 WHICH HAS RETURN INDICATION                           01667000
*                                                                       01668000
         LTR   R5,R5                   (R15) EXIT SUCCESSFUL ?          01669000
         BNZ   IEAHSUSP                NO, CHECK IF USER WANTS MSG OUT  01670000
*                                                                       01671000
*        CHECK FOR DELETION REQUEST                                     01672000
*                                                                       01673000
         LA    R5,SSWTNDSP             R5 = DON'T DISPLAY MESSAGE RC    01674000
         CL    R5,SSOBRETN             SUBSYSTEM REQ DON'T DISPLAY ?    01675000
         BNZ   IEAHSUSP                NO, CHECK USER                   01676000
         DROP  R2                                                       01677000
         USING WMJMEXT,R8              SET ADDRESSING TO MAJOR          01678000
*                                                                       01679000
*        SET UP THE FLAGS SO THAT THIS MLWTO IS DELETED AND SENT        01680000
*        TO HARDCOPY                                                    01681000
*                                                                       01682000
         OI    WMJMECBF,WMJMMAJD       MAJOR IS DELETED                 01683000
*                                                                       01684000
*        SET UP ADDRESSABILITY FOR UCMPREFIX                            01685000
*                                                                       01686000
         L     R1,ADDRUCM              R1 -> UCM                        01687000
         SH    R1,KH4                  ADDR UCM PREFIX                  01688000
         L     R1,0(,R1)               R1 -> UCM PREFIX                 01689000
         USING UCMPRFX,R1              SET ADDRESSABILITY               01690000
         OI    UCMSFLG2,UCMSYSI        SET WQE HOUSEKEEEPING REQUIRED   01691000
         OI    WMJMBUF,WMJMBUFE        MARK WQE AS SERVICED             01692000
*                                                                       01693000
*        CHECK TO SEE IF THE SUBSYSTEM HAS SPECIFIED HARDCOPY           01694000
*        BYPASS                                                         01695000
*                                                                       01696000
         TM    WMJMCS2,WMJMCS2F        BYPASS HARDCOPY QUEUEING ?       01697000
         BZ    IEA00C2A                NO, BRANCH                       01698000
         B     IEAHSUSP                TURN OFF SUSPEND MODE            01699000
*                                                                       01700000
IEA00C2A OI    WMJMBUF,WMJMBUFC        INDICATE READY FOR HC            01701000
         OI    WMJMDSP,WMJMDSPB        INDICATE SEND TO HC              01702000
         DROP  R1                                                       01703000
*                                                                       01704000
*        TRANSLATE UNPRINTABLE AND NON DISPLAY CHARACTERS IN THE WQE    01705000
*                                                                       01706000
IEAHSUSP TM    XVD3,XVD3BLD1+XVD3BLD2  A MAJOR WQE ?                    01707000
         BNO   IEAHTRMI                NO, GO DO MINOR                  01708000
         SR    R1,R1                   HANDLE MAJOR TEXT                01709000
         LH    R1,WMJMTXTL             LENGTH OF MAJOR TEXT             01710000
         BCTR  R1,0                    DECR FOR EX                      01711000
         LA    R15,WMJMTXT             R15 -> TEXT                      01712000
         L     R14,VTRTAB              R14 -> TRTAB IN IEEVVWTO         01713000
         EX    R1,TRINST               TRANSLATE UNPRINTABLES TO TILDE  01714000
*                                      TR    0(0,R15),0(R14)            01715000
         B     IEAHTRND                DONE WITH MAJOR WAIT FOR MINOR   01716000
*                                                                       01717000
IEAHTRMI SR    R1,R1                   DO MINOR WQE                     01718000
         DROP  R8                                                       01719000
         USING WMNMEXT,R7                                               01720000
         ICM   R1,B'0001',WMNMTL1      L'1ST MINOR TEXT                 01721000
         BZ    IEA00C64                LENGTH ZERO, BRANCH              01722000
         BCTR  R1,0                    DECR FOR EX                      01723000
         LA    R15,WMNMTXT1            R15 -> TEXT                      01724000
         L     R14,VTRTAB              R14 -> TRTAB IN IEEVVWTO         01725000
         EX    R1,TRINST               TRANSLATE UNPRINTABLES TO TILDE  01726000
*                                      TR    0(0,R15),0(R14)            01727000
IEA00C64 TM    WMNMML2,WMNMML2H        LINE 2 AVAILABLE ?               01728000
         BO    IEAHTRND                YES, BRANCH AS NOT USED          01729000
         ICM   R1,B'0001',WMNMTL2      R1 = L'TEXT                      01730000
         BZ    IEAHTRND                LENGTH ZERO, BRANCH              01731000
         BCTR  R1,0                    DECR FOR EX                      01732000
         LA    R15,WMNMTXT2            R15 -> TEXT                      01733000
         L     R14,VTRTAB              R14 -> TRTAB IN IEEVVWTO         01734000
         EX    R1,TRINST               TRANSLATE UNPRINTABLES TO TILDE  01735000
*                                      TR    0(0,R15),0(R14)            01736000
         DROP  R7                                                       01737000
         USING WMJMEXT,R8                                               01738000
IEAHTRND NI    WMJMDSP,255-WMJMDSPG    TURN OFF SUSPEND BIT             01739000
         DROP  R8                                                       01740000
*                                                                       01741000
*        FREE THE LOCKS                                                 01742000
*                                                                       01743000
         BAL   R15,FRELCKS                                              01744000
         CLI   NUMLINES,10             PROCESSED 10 LINES ?             01745000
         BNL   IEA00CAA                YES, BRANCH                      01746000
         CLI   XVX2,0                  MORE LINES TO PROCESS ?          01747000
         BNH   IEA00CAA                NO, BRANCH                       01748000
         SR    R1,R1                   INCREMENT LINE COUNTER           01749000
         IC    R1,NUMLINES                                              01750000
         LA    R1,1(,R1)                                                01751000
         STC   R1,NUMLINES                                              01752000
         B     IEAMMORE                BRANCH TO PROCESS NEXT LINE      01753000
*                                                                       01754000
*        THE LOCKS WILL BE FREE'D BY THE SUBSYSTEM EXIT SEGMENT         01755000
*        THIS SEGMENT POSTS THE UCMOECB USING A CROSS MEMORY POST       01756000
*                                                                       01757000
IEA00CAA STM   R14,R12,SAVEREGS+12     SAVE REGS AROUND XMPOST          01758000
         LR    R9,R13                  SAVE SAVEAREA ADDRESS IN R9      01759000
*                                      R9 AND R14 ARE THE ONLY          01760000
*                                      REGS PRESERVED BY XMPOST         01761000
         L     R3,ADDRUCM              R3 -> UCM                        01762000
         USING UCM,R3                                                   01763000
*                                      SETUP R1 AS BIT MASK SO          01764000
         L     R1,KX800000             THAT NO REFERENCES TO            01765000
*                                      STORAGE NEED BE MADE AFTER       01766000
*                                      BASE REG IS UPDATED              01767000
         L     R12,UCMWAKUP            R12 -> ROUTINE TO GET            01768000
*                                      CONTROL IF XMPOST FAILS          01769000
         OR    R12,R1                  INDICATE ERRET IS TO RUN         01770000
*                                      MASTER SCHEDULER MEMORY          01771000
         L     R13,UCMASCB             ASCB OF MEMORY CONTAINING        01772000
*                                      ECB TO POST                      01773000
         SLR   R10,R10                 R10 CONTAINS COMPLETION          01774000
*                                      CODE TO POST WITH                01775000
         LA    R11,UCMOECB             R11 -> ECB TO BE POSTED          01776000
         OR    R11,R1                  SET END OF LIST BIT ON           01777000
         L     R15,CVTPTR                                               01778000
         USING CVT,R15                                                  01779000
         L     R15,CVT0PT01            R15 -> XMPOST BRANCH ENTRY       01780000
         DROP  R15                                                      01781000
         BALR  R14,R15                 CALL XMPOST                      01782000
*                                                                       01783000
         LA    R15,8                   DISPATCHER CALL                  01784000
         SVC   116                     ESR SVC TYPE 1                   01785000
*                                                                       01786000
         LR    R13,R9                  RESTORE SAVEAREA ADDRESS         01787000
         LM    R14,R12,SAVEREGS+12     RESTORE REGS AFTER XMPOST        01788000
         MVI   NUMLINES,1              SET NUMLINES TO 1                01789000
         DROP  R3                                                       01790000
IEAMMORE TM    XVX1,XVX1STOP           NEED TO STOP ?                   01791000
         BO    IEAMSTOP                                                 01792000
         CLI   XVX2,0                  MORE LINES TO DO ?               01793000
         BH    IEASEXIT                YES, GO PROCESS                  01794000
         B     IEAMSTOP                GO CHECK IF WE SHOULD STOP       01795000
*                                                                       01796000
*        THIS CODE WILL GET CONTROL ONLY IF AN ERROR WAS                01797000
*        ENCOUNTERED WHILE RUNNING UNDER ESTAE                          01798000
*                                                                       01799000
*        IF THERE IS AN ERROR THEN WE ASSUME THAT THE WPL               01800000
*        CAUSED THE ERROR. THE USER WILL BE ABENDED WITH A D23          01801000
*        ABEND CODE.                                                    01802000
*                                                                       01803000
IEAVRETY OI    XVD1,XVD1PERR           SET ERROR BIT TO ABEND THE USER  01804000
         MVI   XVAMOD,MWTOID           IEAVMWTO'S ID FOR D23            01805000
         MVI   XVFNCT,D23VALID         PARMLIST VALIDITY CHECK          01806000
         MVI   XVREASON,D23PARM        PARMLIST NOT ADDRESSABLE BY USER 01807000
*                                                                       01808000
*        ERROR PROCESSING EXIT                                          01809000
*                                                                       01810000
IEASTOPA OI    XVX1,XVX1STOP           STOP PROCESSING                  01811000
         B     IEAMSTOP                                                 01812000
*                                                                       01813000
*        FREE THE LOCAL AND CMS LOCKS                                   01814000
*                                                                       01815000
IEAMFREL BAL   R15,FRELCKS             CALL THE FREE LOCK SUBROUTINE    01816000
*                                                                       01817000
*        FREE THE ESTAE RECOVERY                                        01818000
*                                                                       01819000
IEAMSTOP ESTAE 0                       FREE IEAVMWTO STAE               01820000
*                                                                       01821000
*        FREE THE WORKAREA FOR THIS MODULE                              01822000
*                                                                       01823000
         LR    R1,R13                  MOVE ADDR OF WORKAREA TO R1      01824000
         L     R0,WKFREECN             GET SUBPOOL AND SIZE PARM        01825000
         L     R13,4(,R13)             RESTORE PTR TO CALLER'S AREA     01826000
*                                                                       01827000
         FREEMAIN R,LV=(0),A=(1)       FREE WORKAREA FOR THIS MODULE    01828000
*                                                                       01829000
IEAMGRET ICM   R1,B'1111',XVCMAJOR     THERE A MAJOR WQE ?              01830000
         BZ    IEAMRETN                NO, BRANCH                       01831000
         L     R1,XVWQEID              YES, GET MSG ID NUM NO           01832000
*                                      FROM RB SAVE AREA                01833000
         SRL   R1,8                    SHIFT OUT CONSOLE ID             01834000
*                                                                       01835000
*        PUT MSG ID OR ZERO INTO XWQEID FIELD IN XVSAV                  01836000
*        THIS FIELD WILL BE RETURNED TO THE USER BY IEAVVWTO            01837000
*                                                                       01838000
IEAMRETN ST    R1,XVWQEID              PUT MSG ID BACK INTO XVSAV       01839000
*                                                                       01840000
         RETURN (14,12)                RETURN TO VWTO                   01841000
*                                                                       01842000
*        MLWTO CLEANUP ROUTINE                                          01843000
*                                                                       01844000
*        THIS ROUTINE GETS CONTROL IF THERE IS AN ERROR UNDER           01845000
*        THE FRR PROTECTION. IF SO, THEN WE WERE BUILDING A MAJOR       01846000
*        OR MINOR WQE AND NO ERROR WAS EXPECTED. THIS ROUTINE           01847000
*        WILL INSURE THAT THE SUSPENDED BIT IS TURNED OFF IN THE        01848000
*        MAJOR WQE AND WILL LET THE ERROR CONTINUE                      01849000
*                                                                       01850000
*        INPUT -                                                        01851000
*        R2  -> FRR PARM LIST                                           01852000
*        R13 -> CALLERS REG SAVE AREA                                   01853000
*        R14-R15 ARE THE RETURN AND ENTRY REGS                          01854000
*        PARMRGAD IN THE PARM AREA IS THE ADDR OF OUR REG               01855000
*        RESTORE AREA                                                   01856000
*        REGS 3,4,6,7,10,11,12,13 ARE VALID IN THE RESTORE AREA         01857000
*                                                                       01858000
MWTOCLNP SAVE  (14,12),,'IEAVMWTO CLEANUP'                              01859000
         USING PARMLIST,R2                                              01860000
         LR    R0,R13                  MOVE CALLERS SAVEAREA PTR        01861000
         L     R1,PARMRGAD                                              01862000
         LM    R3,R13,14(R1)           RELOAD R3 TO R13 FROM REG SAVE   01863000
         ICM   R1,B'1111',XVCMAJOR     R1 -> MAJOR WQE                  01864000
         BZ    IEAVCLOT                NO MAJOR WQE, RETURN             01865000
*                                                                       01866000
*        THERE IS A MAJOR WQE, IT SHOULD BE ON THE WQE QUEUE            01867000
*                                                                       01868000
*        INSURE THAT IT ISNT IN THE SUSPENDED STATE                     01869000
*                                                                       01870000
         USING WMJM,R1                                                  01871000
         NI    WMJMDSP,255-WMJMDSPG    TURN OFF SUSPEND FLAG            01872000
*                                                                       01873000
*        THE MAJOR WILL BE CLEANED UP BY IEAVMED2 AS LONG AS THE        01874000
*        SUSPEND FLAG IS OFF                                            01875000
*                                                                       01876000
IEAVCLOT LR    R13,R0                  RESTORE CALLERS SAVE AREA PTR    01877000
*                                                                       01878000
         RETURN (14,12)                RETURN TO CALLER                 01879000
*                                                                       01880000
         DROP  R1,R2                                                    01881000
*                                                                       01882000
*        CONSTANTS FOR IEAVMWTO                                         01883000
*                                                                       01884000
IEAMEXMV MVC   0(0,R14),0(R15)         MOVE TEXT TO MAJOR               01885000
IEAMSHFT MVC   0(0,R14),0(R15)         SHIFT TEXT 1 CHAR TO LEFT        01886000
IEAMVTXT MVC   0(0,R14),0(R15)         MOVE TEXT TO MINOR               01887000
TRINST   TR    0(0,R15),0(R14)         TRANSLATE UNPRINTABLES           01888000
*                                      USING TRTAB IN IEEVVWTO          01889000
*                                                                       01890000
KIEE932I DC    C'IEE932I'              DEFAULT CONTROL LINE TEXT        01891000
*                                                                       01892000
VTRTAB   DC    V(TRTAB)                -> TRTAB IN IEEVVWTO             01893000
WKSIZE   DC    A(WRKSIZE)              SIZE OF MWTO'S SAVE AND WORKAREA 01894000
WKSUBPL  DC    AL4(229)                STORAGE FROM SUBPOOL 229         01895000
WKFREECN DC    AL1(229),AL3(WRKSIZE)   FREEMAIN PARAMETER               01896000
WQEPLSZ  DC    F'4096'                 SIZE OF WQE CELL EXTENSION       01897000
SPLFRECN DC    AL1(231),XL3'1000'      FREE WQE EXTENSION PARM          01898000
WKGETWWB DC    AL1(231),AL3(WWBSIZE)   WWB GETMAIN/FREEMAIN PARM        01899000
*                                                                       01900000
KH1      DC    H'1'                                                     01901000
KH2      DC    H'2'                                                     01902000
KH3      DC    H'3'                                                     01903000
KH4      DC    H'4'                                                     01904000
KH20     DC    H'20'                   USED TO CHECK FOR EXTENSION      01905000
*                                      OF WQE CELL POOL                 01906000
         DC    0F'0'                                                    01907000
KX800000 DC    X'80000000'             END OF LIST INDICATOR            01908000
WTPONLY  DC    AL2(WPLROUTK)           CHECK FOR ONLY WTP ROUTE CODE    01909000
KCSSOB   DC    C'SSOB'                 SUBSYSTEM IDENTIFIER             01910000
MODULEID DC    C'MWTO'                 FRR/ESTAE MODULE ID              01911000
KC0000   DC    C'0000'                 CHAR ZEROS FOR ROUTE CODE INIT   01912000
KCZ      EQU   C'Z'                    INLINE AREA DESIGNATOR           01913000
PPWTOFLG EQU   C'+'                    PROB PGM - NO ACTION MSG         01914000
PPACTFLG EQU   C'@'                    PROB PGM - ACTION MSG FLAG       01915000
SUPACFLG EQU   C'*'                    SUPERVISOR - ACTION MSG          01916000
*                                                                       01917000
*        RETURN CODE EQUATES                                            01918000
*                                                                       01919000
LINERR   EQU   X'04'                   ERROR FOUND IN NUMBER OF LINES   01920000
NOIDMCH  EQU   X'08'                   NO MATCH FOR MSG ID IN R0        01921000
INVLDLT  EQU   X'0C'                   LINE TYPE WAS INVALID            01922000
RCWTP    EQU   X'10'                   WTP ROUTE CODE WAS USED          01923000
HCONLY   EQU   X'14'                   MLWTO WAS FOR HARDCOPY ONLY      01924000
*                                                                       01925000
HEXCHAR  DC    C'0123456789ABCDEF'     TABLE TO CONVERT ROUT CODES      01926000
PATTIME  DC    XL9'4021214B21214B2121'  ED PATTERN FOR TIME             01927000
PATUCMID DC    XL4'40212121'           ED PATTERN FOR UCM MSG NUMBER    01928000
*                                                                       01929000
ALLREGS  DC    X'FFFF'                 RESTORE ALL REGS MAP             01930000
*                                                                       01931000
         DC    0F'0'                                                    01932000
*                                                                       01933000
ELIST    ESTAE ,MF=L                   CREATE ESTAE PARM LIST           01934000
ELISTL   EQU   *-ELIST                 FOR LENTH CALULATION             01935000
*                                                                       01936000
FTLOCK   EQU   X'04'                   LOCK FOOT PRINT                  01937000
FTSSOB   EQU   X'03'                   SUSBYSTEM EXIT FOOT PRINT        01938000
*                                                                       01939000
*        GETMINOR                                                       01940000
*                                                                       01941000
*        INPUT -                                                        01942000
*        XVCMAJOR -> MAJOR WQE                                          01943000
*        XVCMINOR -> LAST MINOR ON THE CHAIN IF WE                      01944000
*                    ARE CREATING THE NEXT MINOR                        01945000
*        THIS ROUTINE ASSUMES THAT A WQE IS AVAILABLE UNDER THE         01946000
*        UCMWQLM COUNT                                                  01947000
*                                                                       01948000
*        OUTPUT -                                                       01949000
*        A NEW MINOR IS QUEUED OFF THE MAJOR WQE                        01950000
*        THE MINOR IS ZEROED                                            01951000
*                                                                       01952000
GETMINOR ST    R14,WORK8               SAVE THE CALLER'S ADDR           01953000
         USING WMJM,R8                                                  01954000
         BAL   R14,GETWQE              GET A MINOR WQE                  01955000
*                                                                       01956000
*        CHECK IF A MINOR WAS OBTAINED                                  01957000
*                                                                       01958000
         LTR   R1,R1                   WAS A WQE OBTAINED               01959000
         BNZ   IEANGET1                YES, ADD IT TO THE CHAIN         01960000
*                                                                       01961000
*        NO A WQE WASN'T OBTAINED. SET ERROR FLAGS                      01962000
*                                                                       01963000
         OI    XVX1,XVX1STOP           STOP PROCCESSING                 01964000
         OI    XVD1,XVD1PERR           SET ERROR FLAGS                  01965000
         B     IEANGET2                                                 01966000
*                                                                       01967000
IEANGET1 L     R8,XVCMAJOR             R8 -> MAJOR WQE                  01968000
         ICM   R7,B'1111',WMJMMIN      MINOR QUEUED TO MAJOR ?          01969000
         BNZ   IEAMINR2                YES, QUEUE TO LAST MINOR         01970000
         ST    R1,WMJMMIN              QUEUE NEW MINOR TO MAJOR         01971000
         OI    WMJMMLW,WMJMMLWH        SET DUMMY MINOR QUEUED           01972000
         B     IEAMBL12                SET MINOR FLAGS                  01973000
*                                                                       01974000
         DROP  R8                                                       01975000
         USING WMNMEXT,R7                                               01976000
IEAMINR2 L     R7,XVCMINOR             R7 -> LAST MINOR QUEUED          01977000
         O     R1,WMNMUC2              PRESERVE USE COUNT FOR 2ND LINE  01978000
         ST    R1,WMNMUC2              QUEUE MINOR                      01979000
IEAMBL12 LR    R7,R1                   R7 -> NEW MINOR                  01980000
         OI    XVD3,XVD3BLD1+XVD3BLD2     BUILD LINE 1 AND 2            01981000
         OI    WMNMML1,WMNMML1C+WMNMML1H  SET MINOR WQE AND GETMAINED   01982000
         DROP  R7                                                       01983000
         ST    R7,XVCMINOR             STORE ADDR OF CURRENT MINOR      01984000
IEANGET2 L     R14,WORK8               RESTORE RETURN ADDR              01985000
         BR    R14                     RETURN TO CALLER                 01986000
*                                                                       01987000
*        ENDUP                                                          01988000
*                                                                       01989000
ENDUP    L     R8,XVCMAJOR             R8 -> MAJOR WQE FOR FORCE END    01990000
         SR    R15,R15                 ZERO REG                         01991000
         IC    R15,XVX2                R15 = COUNT OF LINES REMAINING   01992000
         BCTR  R15,0                   DECREMENT                        01993000
         STC   R15,XVX2                UPDATE COUNT IN XVX2             01994000
         LTR   R15,R15                 ANY MORE LINES TO PROCESS ?      01995000
         BNZ   ENDUPRTN                YES, RETURN TO CALLER            01996000
         TM    XVX0,XVX0FEDE           FORCE END ?                      01997000
         BZ    ENDUPRTN                NO, RETURN TO CALLER             01998000
         USING WMJMEXT,R8                                               01999000
         TM    WMJMMLW,WMJMMLWH        DUMMY MINOR QUEUED ?             02000000
         BO    IEAEJEND                YES, FLAG MAJOR AS END           02001000
         ICM   R7,B'1111',XVCMINOR     R7 -> MINOR WQE                  02002000
*                                      THERE A MINOR WQE ?              02003000
         BZ    IEAEJEND                NO, FLAG MAJOR AS END            02004000
         DROP  R8                                                       02005000
         USING WMNMEXT,R7                                               02006000
IEAEP2ND ICM   R15,B'0111',WMNMNX1     NEXT LINE PTR = 0 ?              02007000
         BZ    IEAE1END                YES, FLAG LINE 1 AS END          02008000
         ICM   R15,B'0111',WMNMNX2     PTR TO NEXT MINOR WQE = 0 ?      02009000
         BZ    IEAE2END                YES, FLAG LINE 2 AS END          02010000
         L     R7,WMNMUC2              R7 -> NEXT MINOR WQE             02011000
         B     IEAEP2ND                FIND END OF CHARIN               02012000
*                                                                       02013000
IEAE1END CLI   WMNMTL1,0               MINOR CONTAIN TEXT ?             02014000
         BNE   IEAD1END                YES, THEN DATA END               02015000
         MVI   WMNMLT1,WMNMLT1D        NO, FLAG END LINE ONLY           02016000
         B     ENDUPRTN                RETURN TO CALLER                 02017000
*                                                                       02018000
IEAD1END MVI   WMNMLT1,WMNMLT1C+WMNMLT1D  SET DATA END FLAG             02019000
         B     ENDUPRTN                RETURN TO CALLER                 02020000
*                                                                       02021000
IEAE2END CLI   WMNMTL2,0               TEXT IN SECOND MINOR WQE ?       02022000
         BNE   IEAD2END                YES, THEN DATA END               02023000
         MVI   WMNMLT2,WMNMLT2D        NO, FLAG END LINE ONLY           02024000
         B     ENDUPRTN                RETURN TO CALLER                 02025000
*                                                                       02026000
IEAD2END MVI   WMNMLT2,WMNMLT2C+WMNMLT2D  SET DATA END FLAG             02027000
         B     ENDUPRTN                RETURN TO CALLER                 02028000
*                                                                       02029000
         DROP  R7                                                       02030000
         USING WMJMEXT,R8                                               02031000
IEAEJEND MVI   WMJMLTYP,WMJMLTYC+WMJMLTYD  FLAG MAJOR AS DATA END       02032000
ENDUPRTN BR    R14                     RETURN TO CALLER                 02033000
*                                                                       02034000
*        FINDID                                                         02035000
*                                                                       02036000
*        FIND AN EXISTING ID FOR A CONNECTING MLWTO                     02037000
*                                                                       02038000
*        PROTECT CONTENTS OF R4 AROUND CHECK FOR JOBSTEP AND MEMORY     02039000
*                                                                       02040000
FINDID   ST    R4,REG4SAV              SAVE IN R4 SAVEAREA              02041000
         L     R3,ADDRUCM              R3 -> UCM                        02042000
         USING UCM,R3                                                   02043000
         L     R1,UCMWTOQ              R1 -> FIRST WQE ON SYSTEM QUEUE  02044000
IEAISEAR LTR   R1,R1                   END OF QUEUE ?                   02045000
         BZ    IEAINOID                YES, NO ID FOUND                 02046000
         DROP  R8                                                       02047000
         DROP  R3                                                       02048000
         USING WMJMEXT,R1                                               02049000
         TM    WMJMMLW,WMJMMLWB        MAJOR WQE ?                      02050000
         BZ    IEAINXWQ                NO, GET NEXT WQE                 02051000
         CLC   XVWQEIDA,WMJMMSGN+1     YES, MSG IDS MATCH ?             02052000
         BNE   IEAINXWQ                NO, BRANCH                       02053000
         TM    WMJMLTYP,WMJMLTYD       END LINE ?                       02054000
         BO    IEAINOID                YES, ERROR                       02055000
*                                                                       02056000
*        CONFIRM THAT THE MSG WAS ISSUED BY A PROGRAM IN THE SAME       02057000
*        MEMORY AND JOB STEP AS THE CALLER                              02058000
*                                                                       02059000
         L     R4,TCBSAVE              R4 -> CALLER'S TCB               02060000
         L     R7,ASCBSAVE             R7 -> CALLER'S ASCB              02061000
         USING ASCB,R7                                                  02062000
         USING TCB,R4                                                   02063000
         CLC   WMJMASID,ASCBASID       ADDRESS SPACE ID MATCH ?         02064000
         BNE   IEAINOID                NO, THEN NO ID MATCH             02065000
         CLC   WMJMJTCB,TCBJSTCB       SAME JOB STEP ?                  02066000
         BNE   IEAINOID                NO, THEN NO ID MATCH             02067000
         DROP  R4,R7                                                    02068000
*                                                                       02069000
*        THE MESSAGE WAS ISSUED BY A USER IN THE SAME ADDRESS           02070000
*        SPACE AND JOB STEP                                             02071000
*                                                                       02072000
         ST    R1,XVCMAJOR             STORE MAJOR ADDR                 02073000
         L     R2,WMJMMIN              R2-> FIRST MINOR                 02074000
         ST    R2,XVCMINOR             STORE MINOR ADDR                 02075000
         TM    WMJMMLW,WMJMMLWH        MINOR HAVE TEXT ?                02076000
         BO    IEAIDUMM                NO, DUMMY                        02077000
         B     IEAITST1                CHECK IF MINOR LINE 2 AVAIL      02078000
*                                                                       02079000
IEAINXWQ LA    R1,0(,R1)               CLEAR HIORDER BYTE               02080000
         ICM   R1,B'0111',WMJMEXTA     R1 -> NEXT WQE                   02081000
         B     IEAISEAR                                                 02082000
*                                                                       02083000
         DROP  R1                                                       02084000
*                                                                       02085000
         USING WMNMEXT,R2                                               02086000
IEAITST1 TM    WMNMLT1,WMNMLT1D        FIRST LINE AN END ?              02087000
         BO    IEAINOID                YES, ERROR                       02088000
         TM    WMNMLT2,WMNMLT2D        SECOND LINE END ?                02089000
         BO    IEAINOID                YES, ERROR                       02090000
         LA    R2,0(,R2)               ENSURE HIORDER BYTE IS ZERO      02091000
         ICM   R2,B'0111',WMNMNX2      R2 -> NEXT MINOR WQE OR ZERO     02092000
*                                      THERE A MINOR ?                  02093000
         BNZ   IEAISADD                NO, CONTINUE SEARCH              02094000
         L     R2,XVCMINOR             R2 -> LAST MINOR WQE             02095000
         TM    WMNMML2,WMNMML2H        SECOND LINE AVAILABLE ?          02096000
         BZ    FINDIRET                NO, RETURN                       02097000
         OI    XVD3,XVD3BLD2           SET BUILD SECOND LINE FLAG       02098000
         B     FINDIRET                RETURN TO CALLER                 02099000
*                                                                       02100000
IEAISADD ST    R2,XVCMINOR             SAVE MINOR WQE ADDR              02101000
         B     IEAITST1                GO CHECK IF LINE 2 IS AVAILABLE  02102000
*                                                                       02103000
         DROP  R2                                                       02104000
         USING WMJMEXT,R1                                               02105000
IEAIDUMM OI    WMJMMLW,WMJMMLWF        SET CHAIN REUSABLE FLAG          02106000
         L     R2,XVCMINOR             R2 -> MINOR WQE                  02107000
         DROP  R1                                                       02108000
         USING WMNMEXT,R2                                               02109000
         XC    WMNM(WMNMSIZE),WMNM     ZERO MINOR                       02110000
         OI    WMNMML1,WMNMML1C+WMNMML1H   SET MINOR AND GETMAINED FLAG 02111000
         OI    WMNMML2,WMNMML2H            INDICATE LINE 2 AVAILABLE    02112000
         OI    XVD3,XVD3BLD1+XVD3BLD2      SET BUILD LINE 1 AND 2       02113000
         B     FINDIRET                RETURN TO CALLER                 02114000
*                                                                       02115000
         DROP  R2                                                       02116000
IEAINOID MVI   XVRETCOD,NOIDMCH        SET RETURN CODE                  02117000
         OI    XVX1,XVX1NOID           INDICATE NO ID FOUND             02118000
         XC    XVCMAJOR(8),XVCMAJOR    ZERO MAJOR AND MINOR PTRS        02119000
FINDIRET L     R4,REG4SAV              RESTORE R4                       02120000
         BR    R14                                                      02121000
*                                                                       02122000
*        GETWQE                                                         02123000
*                                                                       02124000
*        GET A WQE FROM THE WQE CELL POOL                               02125000
*        THE WQE WILL BE ZEROED                                         02126000
*        INPUT -                                                        02127000
*        UCMWQECP IS THE CELL POOL ID                                   02128000
*        UCMWQNR IS THE NUMBER OF WQES CURRENTLY IN USE                 02129000
*                                                                       02130000
*        OUTPUT -                                                       02131000
*        R1 -> THE ZEROED WQE IF ONE HAS BEEN OBTAINED                  02132000
*        R1 WILL BE ZERO IF A WQE WASN'T AVAILABLE                      02133000
*                                                                       02134000
*        PROCESS -                                                      02135000
*        REGS 14,15,0,1,2 ARE SAVED AND USED IN THIS ROUTINE IF A       02136000
*        CELL IS NOT AVAILABLE, THE ROUTINE CHECKS IF EXTENSION         02137000
*        IS NEEDED. IF ONE IS NEEDED IT IS OBTAIND AND WE ATTEMPT       02138000
*        TO GET A CELL AGAIN.                                           02139000
*                                                                       02140000
GETWQE   STM   R14,R2,GETSAVE                                           02141000
         L     R3,ADDRUCM              R3 -> UCM                        02142000
         USING UCM,R3                                                   02143000
         LA    R2,KH1                  SET NOT DONE INDICATOR IN R2     02144000
IEAGLOOP LTR   R2,R2                   TEST END OF GET LOOP             02145000
         BZ    IEAGRETN                YES, RETURN TO USER              02146000
*                                                                       02147000
*        NOT DONE                                                       02148000
*        GET A WQE FROM THE CELLPOOL                                    02149000
*                                                                       02150000
         L     R0,UCMWQECP             GET CELLPOOL ID FOR WQES         02151000
*                                                                       02152000
         GETCELL CPID=(0),BRANCH=YES   GET A WQE CELL                   02153000
*                                                                       02154000
*        CHECK IF CELL WAS OBTAINED                                     02155000
*        RETURN CODE IS IN R15                                          02156000
*                                                                       02157000
         LTR   R15,R15                 GET A CELL ?                     02158000
         BNZ   IEAGECHK                NO, CHECK TYPE OF RETURN CODE    02159000
*                                                                       02160000
*        YES, GOT A WQE                                                 02161000
*                                                                       02162000
         XC    0(WMJMSIZE,R1),0(R1)    ZERO OUT THE WQE                 02163000
         LH    R2,UCMWQNR              INCREMENT NO OF WQES             02164000
         LA    R2,1(,R2)                                                02165000
         STH   R2,UCMWQNR                                               02166000
         SR    R2,R2                   INDICATE ALL DONE                02167000
         B     IEAGLOOP                                                 02168000
*                                                                       02169000
*        CHECK IF CPOOL SHOULD BE EXTENDED                              02170000
*                                                                       02171000
IEAGECHK CH    R15,KH4                 EXTENSION NEEDED ?               02172000
         BE    IEA01220                YES, BRANCH                      02173000
         MVI   XVAMOD,MWTOID           IEAVMWTO'S ID FOR D23            02174000
         MVI   XVFNCT,D23WQEGC         WQE GETCELL FAILURE              02175000
         STC   R15,XVREASON                                             02176000
         B     IEAGNOWQ                                                 02177000
*                                                                       02178000
*        YES, EXTENDED THE WQE CELL POOL                                02179000
*        PROTECT THE CONTENTS OF R4 AROUND THE GETMAIN                  02180000
*                                                                       02181000
IEA01220 ST    R4,REG4SAV              STORE IN OUR SAVEAREA            02182000
         LA    R1,231                  USE SUBPOOL 231                  02183000
         L     R0,WQEPLSZ              GET SIZE OF EXTENSION            02184000
         L     R4,TCBSAVE              GET PTRS TO TCB AND ASCB         02185000
         L     R7,ASCBSAVE                                              02186000
*                                                                       02187000
         GETMAIN RC,LV=(0),SP=(1),BRANCH=YES                            02188000
*                                                                       02189000
         L     R4,REG4SAV              RESTORE THE CURRENT LINE PTR     02190000
*                                                                       02191000
*        CHECK IF GETMAIN WAS SUCCESSFUL                                02192000
*                                                                       02193000
         LTR   R15,R15                 EXTENSION SUCCESSFUL ?           02194000
         BZ    IEA01272                YES, BRANCH                      02195000
         MVI   XVAMOD,MWTOID           NO, IEAVMWTO'S ID FOR D23        02196000
         MVI   XVFNCT,D23WQEGM         WQE GETMAIN FAILURE, SP231       02197000
         STC   R15,XVREASON                                             02198000
         B     IEAGNOWQ                                                 02199000
*                                                                       02200000
*        GETMAIN OK, EXTEND CELLPOOL                                    02201000
*                                                                       02202000
IEA01272 ST    R1,SPLEXTAD             SAVE ADDR OF THE EXTENSION       02203000
         L     R3,ADDRUCM              RESTORE BASE FOR UCM             02204000
         L     R0,UCMWQECP             GET WQE CELL POOL ID             02205000
         LA    R15,WMJMSIZE            SIZE OF CELLS REQUIRED           02206000
*                                                                       02207000
*        ADDR OF EXTENSION IS IN R1                                     02208000
*                                                                       02209000
         BLDCPOOL  CPID=(0),SP=231,CSIZE=(15),CPADDR=(1),              X02210000
               AUTODEL=YES,POOLSIZ=4,BRANCH=YES,SERIAL=YES              02211000
*                                                                       02212000
*        CHECK IF EXTENSION IS OK                                       02213000
*                                                                       02214000
         LA    R2,KH1                  INSURE R2 SET TO LOOP            02215000
         LTR   R15,R15                 CELL POOL EXTENDED ?             02216000
         BZ    IEAGLOOP                YES, GO BACK AND GET A WQE       02217000
*                                                                       02218000
*        EXTENSION WASN'T SUCCESSFUL                                    02219000
*        FREE THE STORAGE FOR THE EXTENSION                             02220000
*                                                                       02221000
         MVI   XVAMOD,MWTOID           IEAVMWTO'S ID FOR D23            02222000
         MVI   XVFNCT,D23WQEBC         WQE BLDCPOOL FAILURE             02223000
         STC   R15,XVREASON                                             02224000
         L     R0,SPLFRECN             LOAD SUBPOOL AND SIZE            02225000
         L     R1,SPLEXTAD             GET ADDR OF EXTENSION            02226000
*                                                                       02227000
         FREEMAIN  R,LV=(0),A=(1),BRANCH=YES                            02228000
*                                                                       02229000
IEAGNOWQ SR    R1,R1                   SET R1 TO SHOW NO WQE            02230000
         LR    R2,R1                   GET OUT OF LOOP                  02231000
         B     IEAGLOOP                                                 02232000
*                                                                       02233000
IEAGRETN LM    R14,R0,GETSAVE          RESTORE R14 TO R0                02234000
         L     R2,GETSAVE+16           RESTORE R2                       02235000
         BR    R14                     RETURN TO CALLER                 02236000
*                                                                       02237000
         DROP  R3                                                       02238000
*                                                                       02239000
*        WAITWQE                                                        02240000
*                                                                       02241000
*        WAIT FOR A WQE TO BE FREED                                     02242000
*        INPUT -                                                        02243000
*        XVWWB IS SET TO ZERO BY INITIAL SEGMENT AND TO ADDR OF         02244000
*        WWB BY THIS ROUTINE                                            02245000
*        OUTPUT -                                                       02246000
*        XVWWB -> WWB. AT LEAST ONE WQE HAS BEEN FREED                  02247000
*                                                                       02248000
WAITWQE  STM   R14,R2,WAITSAVE         SAVE CALLER'S REGISTERS          02249000
*                                                                       02250000
*        CHECK IF A WWB EXISTS. IF NOT THEN GET ONE                     02251000
*                                                                       02252000
         ICM   R1,B'1111',XVWWB        HAS A WWB BEEN OBTAINED ?        02253000
         BNZ   IEABWAIT                YES, WAIT ON THE ECB             02254000
*                                                                       02255000
*        NO, GET A WWB AND CHAIN IT TO WQE WWB CHAIN                    02256000
*                                                                       02257000
         L     R0,WKGETWWB             GETMAIN PARM OF SUBPL & SIZE     02258000
*                                                                       02259000
         GETMAIN R,LV=(0),BRANCH=YES                                    02260000
*                                                                       02261000
         ST    R1,XVWWB                SAVE ADDR OF WWB                 02262000
         USING WWB,R1                  ADDRESSABILITY FOR WWB           02263000
*                                                                       02264000
*        SET UP THE WWB                                                 02265000
*                                                                       02266000
         MVC   WWBASCB,ASCBSAVE        PUT IN CALLER'S ASCB ADDR        02267000
         ST    R4,WWBTCBAD             CALLER'S TCB ADDR                02268000
         L     R3,ADDRUCM              R3 -> UCM                        02269000
         USING UCM,R3                                                   02270000
*                                                                       02271000
*        CONNECT THIS WWB IN TO END OF THE CHAIN                        02272000
*                                                                       02273000
         L     R2,UCMWECBT             R2 -> END OF CHAIN               02274000
*                                                                       02275000
*        R2 -> LAST WWB ON THE CHAIN                                    02276000
*        MOVE END OF CHAIN ENDICATOR FROM PREVIOUS END TO NEW WWB       02277000
*                                                                       02278000
         MVC   WWBFWDPT,WWBFWDPT-WWB(R2)                                02279000
*                                                                       02280000
*        POINT LAST WWB ON CHAIN TO NEW WWB                             02281000
*                                                                       02282000
         ST    R1,WWBFWDPT-WWB(,R2)                                     02283000
*                                                                       02284000
*        POINT NEW WWB BACK TO LAST CHAIN MEMBER                        02285000
*                                                                       02286000
         ST    R2,WWBBCKPT                                              02287000
*                                                                       02288000
*        POINT CHAIN TAIL AT NEW WWB                                    02289000
*                                                                       02290000
         ST    R1,UCMWECBT                                              02291000
*                                                                       02292000
*        THE NEW WWB IS NOW AT THE END OF THE CHAIN                     02293000
*                                                                       02294000
IEABWAIT L     R1,XVWWB                INSURE ADDRESSABILITY OF WWB     02295000
         NI    WWBFLAGS,255-WWBPOSTD   TURN OFF POSTED BIT              02296000
         XC    WWBECB,WWBECB           ZERO OUT THE ECB                 02297000
*                                                                       02298000
*        FREE THE LOCKS BEFORE ISSUING THE WAIT                         02299000
*                                                                       02300000
         BAL   R15,FRELCKS             CALL THE FREE LOCK SUBROUTINE    02301000
*                                                                       02302000
*        CLEAR RETRY ADDRESS IN PARM AREA                               02303000
*                                                                       02304000
         L     R1,PARMPTR              ADDR OF ESTAE PARM AREA          02305000
         USING PARMLIST,R1                                              02306000
         XC    PARMRTAD,PARMRTAD       CLEAR RETRY ADDR                 02307000
         DROP  R1                                                       02308000
         L     R1,XVWWB                                                 02309000
         USING WWB,R1                                                   02310000
*                                                                       02311000
*        CHECK IF WE ARE IN MEMORY ONE                                  02312000
*        IF NOT THEN TAKE A LONG WAIT                                   02313000
*                                                                       02314000
         L     R7,ASCBSAVE             LOAD ADDR OF OUR ASCB            02315000
         USING ASCB,R7                                                  02316000
         CLC   ASCBASID,KH1            IN MEMORY ONE WITH COMMTASK ?    02317000
         DROP  R7                                                       02318000
         BNE   IEABLONG                NO, GO TAKE A LONG WAIT          02319000
         LA    R1,WWBECB               GET ADDR OF ECB IN WAIT BLOCK    02320000
*                                                                       02321000
         WAIT  ,ECB=(1)                WAIT FOR A WQE TO BE FREED       02322000
*                                                                       02323000
         B     IEABRETN                                                 02324000
*                                                                       02325000
IEABLONG LA    R1,WWBECB               R1 -> ECB IN WAIT BLOCK          02326000
*                                                                       02327000
         WAIT ,ECB=(1),LONG=YES        TAKE A LONG WAIT                 02328000
*                                                                       02329000
*        SET THE LOCAK AND CMS LOCKS                                    02330000
*                                                                       02331000
IEABRETN BAL   R15,SETLCKS                                              02332000
         LM    R14,R2,WAITSAVE                                          02333000
         BR    R14                     RETURN TO USER                   02334000
*                                                                       02335000
         DROP  R1                                                       02336000
         DROP  R3                                                       02337000
*                                                                       02338000
*        TEXTLINE                                                       02339000
*                                                                       02340000
*        THIS ROUTINE SETS R4 -> THE NEXT LINE IN THE                   02341000
*        WPL TO BE PROCESSED                                            02342000
*                                                                       02343000
*        INPUT -                                                        02344000
*        ADDRMLHR -> WPLLTF                                             02345000
*        XVD3TXT1 IF ON INDICATES THAT THE FIRST LINE OF THE WPL        02346000
*        WAS JUST PROCESSED                                             02347000
*        R4 -> WPLML0 OF CURRENT LINE IF NOT THE FIRST LINE             02348000
*                                                                       02349000
*        OUTPUT -                                                       02350000
*        XVD3TXT1 IS SET OFF                                            02351000
*        XVX0UDCL IS SET OFF                                            02352000
*        R4 -> WPLML0 FOR THE NEXT LINE                                 02353000
*                                                                       02354000
         USING WPLML,R4                                                 02355000
*                                                                       02356000
TEXTLINE TM    XVD3,XVD3TXT1           FIRST LINE JUST PROCESSED        02357000
         BZ    IEAXNFST                NO, INCREMENT FOR ML EXTENSION   02358000
*                                      HEADER                           02359000
*                                                                       02360000
*        THE FIRST LINE HAS BEEN PROCESSED                              02361000
*        ADVANCE TO THE FIRST LINE IN THE MULTI LINE EXTENSION          02362000
*                                                                       02363000
         L     R4,ADDRMLHR             R4 -> WPLLTF                     02364000
         LA    R4,4(,R4)               INCR TO FIRST LINE IN WPLML0     02365000
         NI    XVD3,255-XVD3TXT1       TURN OFF LINE ONE FLAG           02366000
         NI    XVX0,255-XVX0UDCL       TURN OFF DEFAULT CNTL LINE FLAG  02367000
         B     IEAXRETN                RETURN TO CALLER                 02368000
*                                                                       02369000
*        R4 -> L'CURRENT LINE                                           02370000
*        INCREMENT R4 TO THE NEXT LINE                                  02371000
*                                                                       02372000
IEAXNFST AH    R4,WPLML0               ALL LENGTH OF CURRENT LINE       02373000
*                                      R4 -> WPLML0 FOR THE NEXT LINE   02374000
*        DROP  R4                                                       02375000
IEAXRETN BR    R14                     RETURN TO USER                   02376000
*                                                                       02377000
*        FRELCKS                                                        02378000
*                                                                       02379000
*        THE CMS AND LOCAL LOCKS ARE RELEASED                           02380000
*        R11, R12 AND R13 ARE SAVED                                     02381000
*        R11 AND R12 GO IN MLWTO'S WORKAREA                             02382000
*        R13 IS SAVED IN R0                                             02383000
*        R15 IS THE RETURN REGISTER                                     02384000
*                                                                       02385000
FRELCKS  ST    R11,REG11SAV            BASE REG SAVED IN WORKAREA       02386000
         ST    R12,REG12SAV            EXTENDED SAVE AREA BASE          02387000
         LR    R0,R13                  MOVE PTR TO SAVEAREA             02388000
*                                                                       02389000
*        FREE THE FRR AND RESET THE PARM AREA PTR                       02390000
*                                                                       02391000
         SETFRR D,WRKREGS=(11,12)                                       02392000
*                                                                       02393000
         LA    R12,EPARM               ADDR OF ESTAE PARM AREA          02394000
         ST    R12,PARMPTR             SAVE NEW ADDR                    02395000
*                                                                       02396000
         SETLOCK RELEASE,TYPE=CMS,                                     *02397000
               RELATED=(UCM,IEAVMWTO(SETLCKS))                          02398000
*                                                                       02399000
         SETLOCK RELEASE,TYPE=LOCAL,                                   *02400000
               RELATED=(UCM,IEAVMWTO(SETLCKS))                          02401000
*                                                                       02402000
         LR    R13,R0                  RESTORE PTR TO SAVEAREA          02403000
         L     R11,REG11SAV            RESTORE BASE REG                 02404000
         LA    R12,EPARM               GET ADDR OF ESTAE PARMLIST       02405000
         USING PARMLIST,R12            ESTABLISH BASE                   02406000
         NI    PARMFLAG,255-PARMFRID   TURN OFF FRR INDICATOR           02407000
         DROP  R12                     RELEASE BASE                     02408000
         L     R12,REG12SAV            RESTORE PTR TO EXTENDED AREA     02409000
         BR    R15                     RETURN TO CALLER                 02410000
*                                                                       02411000
*        SETLCKS                                                        02412000
*                                                                       02413000
*        THE LOCAL AND CMS LOCKS ARE OBTAINED                           02414000
*        R11, R12 AND R13 ARE SAVED                                     02415000
*        R11 AND R12 GO IN MLWTO'S WORKAREA                             02416000
*        R13 IS SAVED IN R0                                             02417000
*        R15 IS THE RETURN REGISTER                                     02418000
*                                                                       02419000
SETLCKS  ST    R11,REG11SAV            BASE REG SAVED IN WORKAREA       02420000
         ST    R12,REG12SAV            EXTENDED SAVE AREA BASE          02421000
         LR    R0,R13                  MOVE PTR TO SAVEAREA             02422000
*                                                                       02423000
         SETLOCK OBTAIN,TYPE=LOCAL,MODE=UNCOND,                        *02424000
               RELATED=(UCM,IEAVMWTO(FRELCKS))                          02425000
*                                                                       02426000
         SETLOCK OBTAIN,TYPE=CMS,MODE=UNCOND,                          *02427000
               RELATED=(UCM,IEAVMWTO(FRELCKS))                          02428000
*                                                                       02429000
         LR    R13,R0                  RESTORE PTR TO SAVEAREA          02430000
         L     R11,REG11SAV            RESTORE BASE REG                 02431000
         L     R12,REG12SAV            RESTORE PTR TO EXTENDED AREA     02432000
*                                                                       02433000
*        SET UP THE FRR FOR THIS ROUTINE                                02434000
*        LET SETFRR USE R2 AND R3 AS WORK REGS                          02435000
*        SAVE THE CONTENTS OF R2 AND R3 IN SUBSLIST                     02436000
*                                                                       02437000
         STM   R2,R3,SUBSLIST          SAVE R2 AND R3                   02438000
         L     R3,ADDRUCM              R3 -> UCM                        02439000
         USING UCM,R3                                                   02440000
         L     R1,UCMFRRAD             R1 -> COMM TASK FRR IEAVMFRR     02441000
         DROP  R3                                                       02442000
*                                                                       02443000
         SETFRR A,FRRAD=(R1),PARMAD=PARMPTR,WRKREGS=(R2,R3)             02444000
*                                                                       02445000
         L     R1,PARMPTR              R1 -> FRR PARMAREA               02446000
         USING PARMLIST,R1                                              02447000
         LA    R2,RECSAVE              R2 -> REG SAVE AREA              02448000
         ST    R2,PARMRGAD             PT TO REG AREA FOR RETRY         02449000
         MVC   PARMID,MODULEID         IDENTIFY AS IEAVMWTO             02450000
         DROP  R1                      DROP PRESENT PARMLIST BASE       02451000
         LA    R2,EPARM                GET PTR TO ESTAE PARMLIST        02452000
         USING PARMLIST,R2             SET ADDRESS TO ESTAE PARM        02453000
*                                                                       02454000
*********************************************************************** 02455000
*                                                                       02456000
*        THE FOLLOWING PARMFRID FIELD IN THE ESTAE PARAMETER LIST       02457000
*        IS SET TO ZERO TO GUARANTEE THAT RECOVERY WILL BE DONE         02458000
*        ON BOTH THE ESTAE AND FRR LEVEL                                02459000
*                                                                       02460000
*********************************************************************** 02461000
         NI    PARMFLAG,255-PARMFRID   TURN OFF FRR INDICATOR           02462000
         DROP  R2                      DROP TEMPORARY BASE              02463000
         USING PARMLIST,R1             RESET BASE TO FRR PARMLIST       02464000
         MVI   PARMFTPT,FTLOCK         SET FOOTPRINT                    02465000
         LA    R2,MWTOCLNP             R2 -> MLWTO CLEANUP ROUTINE      02466000
         ST    R2,PARMCLAD                                              02467000
         LM    R2,R3,SUBSLIST          RESTORE R2 AND R3                02468000
         DROP  R1                                                       02469000
         BR    R15                     RETURN TO CALLER                 02470000
*                                                                       02471000
*********************************************************************** 02472000
*                                                                       02473000
*        WORKAREA FOR THIS MODULE IEAVMWTO                              02474000
*                                                                       02475000
*********************************************************************** 02476000
*                                                                       02477000
WORKAREA DSECT                                                          02478000
SAVEREGS DS    18F                     STANDARD SAVEAREA                02479000
REG4SAV  DS    F                       SAVEAREA FOR R4                  02480000
REG9SAV  DS    F                       USED TO SAVE R9                  02481000
REG11SAV DS    F                       USED TO SAVE R11                 02482000
REG12SAV DS    F                       USED TO SAVE R12                 02483000
WORK8    DS    D                                                        02484000
GETSAVE  DS    5F                      SAVEAREA FOR GETWQE ROUTINE      02485000
SPLEXTAD DS    F                       -> WQE EXTENSION                 02486000
WAITSAVE DS    6F                      SAVE AREA FOR WIITWQE ROUTINE    02487000
ASCBSAVE DS    F                       SAVE AREA FOR ASCB ADDR          02488000
ADDRMLHR DS    F                       -> MULTI LINE EXTENSION HEADER   02489000
*                                         PART OF WPL                   02490000
TCBSAVE  DS    F                       -> CALLER'S TCB                  02491000
SUBSPARM DS    F                       PTS AT THE SUBSLIST              02492000
SUBSLIST DS    9F                      CONTAINS THE SSOB & SSWT         02493000
*                                                                       02494000
*        ESTAE SVC PARAMETER AREA                                       02495000
*                                                                       02496000
EPARM    DS    CL24                    ESTAE PARM AREA                  02497000
*                                                                       02498000
ADDRUCM  DS    F                       -> UCM                           02499000
PARMPTR  DS    F                       -> RECOVERY PARM AREA            02500000
NUMLINES DS    XL1                     COUNT OF LINES PROCESSED         02501000
RECSAVE  DS    H                       RESTORE MAP                      02502000
RECREGS  DS    16F                     RESTORE AREA FOR RETRY           02503000
LASTWPLB DS    F                       -> LAST BYTE +1 OF WPL IN SP 229 02504000
WORKBYTE DS    XL1                                                      02505000
LINETYPE DS    XL1                     LINE TYPE FLAGS FROM WPLLTF      02506000
*                                      OR WPLMLLTF                      02507000
         DS    0D                      ROUND UP TO DOUBLE WORD          02508000
WRKSIZE  EQU   *-WORKAREA              CALC SIZE OF WORKAREA            02509000
*                                                                       02510000
*********************************************************************** 02511000
*                                                                       02512000
*        WORKAREA USED IN MODULE IEAVVWTO                               02513000
*                                                                       02514000
*********************************************************************** 02515000
*                                                                       02516000
WORKVV   DSECT                                                          02517000
@SA00001 DS    18F                                                      02518000
@SAGETB  DS    18F                                                      02519000
@PC00003 DS    F                                                        02520000
@SA00004 DS    F                                                        02521000
@PC00004 DS    F                                                        02522000
@SA00008 DS    15F                                                      02523000
@SA00002 DS    15F                                                      02524000
@PC00002 DS    F                                                        02525000
PARMPTRV DS    A                                                        02526000
@TF00001 DS    F                                                        02527000
REG1SAV  DS    F                                                        02528000
REG2SAV  DS    A                  SAVE AREA FOR R2 WHEN DEALING WITH    02529000
*                                 UCM                                   02530000
LONGTXT  DS    A                  -> AREA GETMAINED FOR LONG WPL        02531000
*                                    IF ZERO THEN NO STORAGE GETMAINED  02532000
LONGLEN  DS    F                  L'GETMAINED AREA FOR LONG TEXT        02533000
*                                                                       02534000
LENMWQE  DS    F                  L'OF ALL MINOR WQES                   02535000
*                                 VALID ONLY FOR MLWTO                  02536000
MLWTOXH  DS    A                  -> MLWTO EXTENT HEADER IN CALLERS WPL 02537000
*                                 VALID ONLY FOR MLWTO                  02538000
*                                                                       02539000
*        VERSION 2 XWPL TO STORE ALL USER PROVIDED WPL FIELDS           02540000
*                                                                       02541000
WKALGH   DS    AL2                MESSAGE LENGTH OF MAJOR WQE TEXT FROM 02542000
*                                 CALLER'S WPL                          02543000
WKAMCSF  DS    XL2                MCS FLAGS COPIED FROM CALLERS WPL     02544000
WKAADTXT DS    AL4                -> MESSAGE TEXT                       02545000
WKAVRSN  DS    AL1                XWPL VERSION NUMBER                   02546000
WKAFLAGS DS    XL1                MISC FLAGS                            02547000
WKARPYLN DS    AL1                L'REPLY FOR WTOR                      02548000
WKALNGTH DS    AL1                L'XWPL, ZERO FOR VERSION 1            02549000
WKAMCSF1 DS    XL2                EXTENDED MCS FLAGS                    02550000
WKACPFLF DS    XL2                MCS FLAGS FOR CNTL PROGRAM USE        02551000
WKARPBUF DS    AL4                -> REPLY BUFFER                       02552000
WKAECBP  DS    AL4                -> ECB                                02553000
WKASEQN  DS    AL4                DOM/CONNECT ID                        02554000
WKADSC   DS    XL2             *  DESCRIPTOR CODE TO BE USED IN WTO     02555000
WKAROC   DS    XL2             V  ROUTE CODES TO BE USED IN WTO         02556000
WKAROUT  DS    XL14               EXTENDED ROUTING CODES                02557000
WKAMSGTY DS    XL2                MSGTYPE FLAGS COPIED FROM CALLERS WPL 02558000
WKAPRTY  DS    XL2                MESSAGE PRIORITY                      02559000
WKAJOBID DS    CL8                JOB ID                                02560000
WKAJOBNM DS    CL8                JOB NAME                              02561000
WKAKEY   DS    CL8                RETRIEVAL KEY                         02562000
WKATOKN  DS    AL4                TOKEN FOR DOM                         02563000
WKACNID  DS    AL4                CONSOLE ID                            02564000
WKASYSNA DS    CL8                SYSTEM NAME                           02565000
WKACNNME DS    CL8                CONSOLE NAME                          02566000
WKARCNA  DS    AL4                -> REPLY CONSOLE NAME/ID              02567000
WKACART  DS    AL4                -> CART                               02568000
WKAWSPRM DS    AL4                -> WAIT STATE PARM LIST               02569000
WKAASCB  DS    AL4                -> ASCB                               02570000
WKARSV30 DS    XL16               RESERVED                              02571000
*                                                                       02572000
WKATEXT  DS    CL129              MESSAGE TEXT (MAXIMUM 125 CHARS) +4   02573000
*                                                                       02574000
ESTAEPRM DS    A                                                        02575000
@TS00001 DS    CL1                                                      02576000
STARTID  DS    CL1                                                      02577000
CVDAREA  DS    D                                                        02578000
*                                                                       02579000
PFLAG    DS    XL1                ADDITIONAL FLAGS                      02580000
REQCMSL  EQU   X'40'              REQUEST OR HOLD CMS LOCK              02581000
*                                                                       02582000
*        THE DSECT HAS BEEN TRUNCATED TO AVOID DUPLICATE NAME ISSUES    02583000
*                                                                       02584000
IEAVMWTO CSECT                                                          02585000
*                                                                       02586000
         IHAWQE DSECT=YES                                               02587000
*                                                                       02588000
         IHACTM WWB                                                     02589000
*                                                                       02590000
         IHACTM FTPT                                                    02591000
*                                                                       02592000
         IHAFRRS                                                        02593000
*                                                                       02594000
         IEZWPL DSECT=YES                                               02595000
*                                                                       02596000
         PRINT NOGEN                                                    02597000
*                                                                       02598000
         IHAASCB                                                        02599000
*                                                                       02600000
         IEFJSSOB  (WT),CONTIG=YES                                      02601000
*                                                                       02602000
         IEFJESCT                                                       02603000
*                                                                       02604000
         IHAPSA                                                         02605000
*                                                                       02606000
         IKJRB   DSECT=YES                                              02607000
*                                                                       02608000
         PRINT GEN                                                      02609000
*                                                                       02610000
*/********************************************************************/ 02611000
*/*                                                                  */ 02612000
*/*          EXTENDED SAVEAREA MAPPING FOR SVC 35                    */ 02613000
*/*                                                                  */ 02614000
*/********************************************************************/ 02615000
*                                                                       02616000
*        DEFINED IN THE RBEXSAVE AREA                                   02617000
*                                                                       02618000
         ORG   RBEXSAVE                                                 02619000
XVSAV    DS    0A                                                       02620000
XVA4     DS    0F                      ERROR PROCESSING FIELDS          02621000
XVFNCT   DS    C                       D23 PROCESS CODE                 02622000
D23VALID EQU   X'10'                   PARMLIST VALIDITY CHECK          02623000
D23OREGC EQU   X'20'                   ORE GETCELL FAILURE              02624000
D23OREBC EQU   X'21'                   ORE BLDCPOOL FAILURE             02625000
D23OREGM EQU   X'22'                   ORE GETMAIN FAILURE, SP231       02626000
D23WQEGC EQU   X'30'                   WQE GETCELL FAILURE              02627000
D23WQEBC EQU   X'31'                   WQE BLDCPOOL FAILURE             02628000
D23WQEGM EQU   X'32'                   WQE GETMAIN FAILURE, SP231       02629000
D23DYN   EQU   X'42'                   DYNAMIC AREA GETMAIN FAILURE     02630000
XVAMOD   DS    C                       D23 MODULE ID                    02631000
VWTOID   EQU   X'01'                   IEAVVWTO'S ID FOR D23            02632000
MWTOID   EQU   X'02'                   IEAVMWTO'S ID FOR D23            02633000
XVA41    DS    C                       RESERVED                         02634000
XVREASON DS    C                       D23 REASON CODE                  02635000
D23BNDY  EQU   X'01'                   WTOR PARMLIST NOT ON WORD BNDY   02636000
D23MLWTR EQU   X'02'                   MULTILINE WTOR SPECIFIED         02637000
D23PARM  EQU   X'03'                   WPL NOT ADDRESSABLE BY USER      02638000
D23ZERO  EQU   X'04'                   ZERO TEXT LENGTH WTOR            02639000
D23SIZE  EQU   X'05'                   CALLER MODIFIED WPL              02640000
*                                      DURING WTO PROCESSING            02641000
D23LTXT  EQU   X'06'                   WTO/WTOR TEXT= CODED AND         02642000
*                                      WPLLGH ^=8                       02643000
*                                                                       02644000
XVA8     DS    AL4                     STORE TIME                       02645000
XVWPLADR DS    AL4                     -> CALLERS WPL                   02646000
XVWQEAD  DS    AL4                                                      02647000
*                                                                       02648000
*        FLAGS                                                          02649000
*                                                                       02650000
XVDFLAGS DS    0XL4                                                     02651000
XVD0     DS    X                                                        02652000
XVD0RPFD EQU   BIT1                    HARD COPY ONLY                   02653000
XVD0NWQE EQU   BIT2                    GET WQE                          02654000
XVD0NORE EQU   BIT3                    GET THE ORE FIRST                02655000
XVD0QID  EQU   BIT4                    QID FIELD IS PRESENT IN THE WPL  02656000
XVD0WWB  EQU   BIT5                    WWB WTO WAIT BLOCK OBTAINED      02657000
XVD0USER EQU   BIT6                                                     02658000
XVD0HDCY EQU   BIT7                                                     02659000
*                                                                       02660000
*        XVDFLAGS+1                                                     02661000
*                                                                       02662000
XVD1     DS    X                                                        02663000
XVD1PRIV EQU   BIT0                    PRIVILEGED USER                  02664000
XVD1ENQW EQU   BIT1                    ENQ'D ON A WQE  (VMWTO)          02665000
XVD1ENQO EQU   BIT2                    ENQ'D ON AN ORE (VMWTO)          02666000
XVD1ALDN EQU   BIT2                    ALL NEEDED CONTROL BLKS OBTAINED 02667000
XVD1PP   EQU   BIT5                    PROBLEM PROGRAM CALLER           02668000
XVD1AUTH EQU   BIT6                    KEY 0, SUPVR STATE OR APF AUTH   02669000
XVD1PERR EQU   BIT7                    SEVERE ERROR FOUND. ABEND USER   02670000
*                                                                       02671000
*        XVDFLAGS+2                                                     02672000
*                                                                       02673000
XVD2     DS    X                                                        02674000
XVD2CON  EQU   BIT0                    CONNECTING                       02675000
XVD2VALD EQU   BIT3                    PARAMETER LIST IS VALID          02676000
XVD2DELW EQU   BIT4                    SEND MSG TO HARDCOPY ONLY        02677000
XVD2ZERO EQU   BIT5                    ZERO MSG ID TO USER              02678000
XVD2WTOR EQU   BIT6                    WTOR REQUEST                     02679000
XVD2QFHC EQU   BIT7                    QUEUE THIS MSG TO HARD COPY      02680000
*                                                                       02681000
*        XVDFLAGS+3                                                     02682000
*                                                                       02683000
XVD3     DS    X                                                        02684000
XVD3BLDJ EQU   BIT0                    BUILD MAJOR WQE                  02685000
XVD3BLD1 EQU   BIT1                    BUILD LINE 1                     02686000
XVD3BLD2 EQU   BIT2                    BUILD LINE 2                     02687000
XVD3TXT1 EQU   BIT3                    TEXT LINE 1 BEING PROCESSED      02688000
XVD3TFX  EQU   BIT4                    TCBTFX WAS SET ON,TURN IT OFF    02689000
*        END OF FLAGS                                                   02690000
XVOREAD  DS    AL4                                                      02691000
         ORG   XVOREAD                                                  02692000
XVX      DS    0F                      USED AS WORK AREA BY VMWTO       02693000
XVX0     DS    X                       LINE CONTROL FLAGS - MLWTO       02694000
XVX0FLCL EQU   BIT0                    FIRST LINE IS CONTROL LINE       02695000
XVX0LL1F EQU   BIT1                    LABEL LINE 1 FOUND               02696000
XVX0LL2F EQU   BIT2                    LABEL LINE 2 FOUND               02697000
XVX0UDCL EQU   BIT3                    USE DEFAULT CONTROL LINE         02698000
XVX0FLJE EQU   BIT4                    FIRST LINE JUST 'E'              02699000
XVX0FEDE EQU   BIT5                    FORCE END (LAST LINE TO BE DE)   02700000
XVX1     DS    X                       ERROR FLAGS - MLWTO              02701000
XVX1STOP EQU   BIT0                    ERROR IN PARM LIST; IGNORE MLWTO 02702000
XVX1NOID EQU   BIT1                    NO ID FOR CONNECTING MLWTO       02703000
XVX2     DS    AL1                     NO OF LINES STILL TO DO          02704000
XVX3     DS    AL1                     NO OF LINES FROM MLWTO EXT HDR   02705000
*                                                                       02706000
XVCMAJOR DS    AL4                  *                                   02707000
XVCMINOR DS    AL4                  V                                   02708000
*                                                                       02709000
XVWWB    DS    AL4                                                      02710000
*                                                                       02711000
XVWQEID  DS    0AL4           *        CALLERS R0                       02712000
XVWQEIDA DS    AL3            |        A NEW LINE IS TO BE              02713000
*                             |        CONNECTED TO THE MESSAGE         02714000
*                             |        WITH THIS 3 BYTE MESSAGE ID      02715000
*                             |        MLWTO ONLY                       02716000
XVCONID  DS    XL1            V        CONSOLE ID FOR THIS MESSAGE      02717000
*                                                                       02718000
XVRET    DS    0AL4                                                     02719000
XVRETCOD DS    AL4                                                      02720000
XVLEN    EQU   *-XVSAV                 MUST NOT EXCEED 48 BYTES AS      02721000
*                                      RB EXTENDED SAVE AREA USED       02722000
*                                                                       02723000
         PRINT NOGEN                                                    02724000
*                                                                       02725000
         IKJTCB  DSECT=YES                                              02726000
*                                                                       02727000
         CVT   DSECT=YES,LIST=YES                                       02728000
*                                                                       02729000
         IHASCVT LIST=YES                                               02730000
*                                                                       02731000
         IEZJSCB                                                        02732000
*                                                                       02733000
         PRINT GEN                                                      02734000
*                                                                       02735000
         IEECUCM FORMAT=NEW                                             02736000
*                                                                       02737000
*                                                                       02738000
R0       EQU   0                                                        02739000
R1       EQU   1                                                        02740000
R2       EQU   2                                                        02741000
R3       EQU   3                                                        02742000
R4       EQU   4                                                        02743000
R5       EQU   5                                                        02744000
R6       EQU   6                                                        02745000
R7       EQU   7                                                        02746000
R8       EQU   8                                                        02747000
R9       EQU   9                                                        02748000
R10      EQU   10                                                       02749000
R11      EQU   11                                                       02750000
R12      EQU   12                                                       02751000
R13      EQU   13                                                       02752000
R14      EQU   14                                                       02753000
R15      EQU   15                                                       02754000
*                                                                       02755000
         END                                                            02756000
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
*        APPLIED TO ZP60039. HOWEVER AS TMVS805 (ALSO SHIPPED AS        00032000
*        ZUM0013) IS SO COMMONLY USED IT HAS BEEN INCLUDED IN ZP60039.  00033000
*        ZP60039 THEREFORE SUPERCEDES TMVS805 AND ZUM0013.              00034000
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
*                                                                       00066000
*        FUNCTION -                                                     00067000
*        SVC 35 WTO AND WTOR PROCESSOR                                  00068000
*        1. SETUP THE RECOVERY/TERMINATION ENVIRONMENT                  00069000
*        2. DETERMINE THE STATUS OF THE CALLER - PP/APF/SUP             00070000
*        3. DETERMINE THE FORMAT OF THE CALLER PROVIDED WPL.            00071000
*           THERE ARE A NUMBER OF WTO AND WTOR WPL PARAMETER FORMATS    00072000
*           FOR WTO THERE ARE -                                         00073000
*           1. STANDARD 370 WTO WPL                                     00074000
*           2. EXTENDED WPL VERSION 1                                   00075000
*           3. EXTENDED WPL VERSION 2 (SUPPORT FOR TEXT= PARAMETER)     00076000
*           FOR WTOR THERE ARE -                                        00077000
*           1. STANDARD 370 WTOR WPL                                    00078000
*           2. 31 BIT WTOR WPL                                          00079000
*           3. EXTENDED WPL VERSION 1                                   00080000
*           4. EXTENDED WPL VERSION 2 (SUPPORT FOR TEXT= PARAMETER)     00081000
*        4. VALIDATE CALLER ACCESS TO THE WPL FIELDS AND VALIDATE       00082000
*           WPL PARAMETERS FOR CONSISTENCY                              00083000
*        5. MOVE PARAMETERS FROM CALLER PROVIDED WPL TO PROTECTED       00084000
*           STORAGE                                                     00085000
*        6. VALIDATE KEY PARAMETERS MOVED TO PROTECTED STORAGE WERE     00086000
*           NOT CHANGED IN THE CALLERS WPL DURING THE MOVE.             00087000
*           THIS STEP IS REQUIRED TO PREVENT ANY INTEGRITY EXPOSURES    00088000
*                                                                       00089000
*        NOTES -                                                        00090000
*                                                                       00091000
*        DUE TO THE LIMITATIONS OF THE PARAMETER LIST PASSED            00092000
*        TO IEECVXIT BY IEAVVWTO SOME IEECVXIT EXITS MAKE               00093000
*        ASSUMPTIONS ABOUT THE CONTENTS OF THE REGISTERS INHERITED      00094000
*        BY IEECVXIT THAT ARE NOT DEFINED BY THE DOCUMENTED             00095000
*        IEECVXIT PARAMETER LIST.                                       00096000
*                                                                       00097000
*        THE IEECVXIT PROVIDED AS A COMPONENT OF THE BSPPILOT           00098000
*        USERMOD ZUM0003 CONTRIBUTED BY VOLKER BANDKE WILL              00099000
*        AUTOMATICALLY REPLY TO SOME WTOR MESSAGES.                     00100000
*                                                                       00101000
*        TO ENSURE THIS IEECVXIT CODE HAS LOCATED THE CORRECT ORE       00102000
*        FOR A REPLY THE CODE CAPTURES THE USER PROVIDED ECB            00103000
*        ADDRESS TO BE POSTED BY THE OPERATOR REPLY AND USES IT         00104000
*        AS A VALIDATION OF THE CORRECT ORE. THE CODE IN THE            00105000
*        BSPPILOT IEECVXIT ASSUMES THAT R5 WILL POINT TO THE SVRB       00106000
*        FOR SVC 35 AND THAT THE ADDRESS OF THE WPL PASSED TO           00107000
*        SVC 35 WILL BE LOCATED IN THE SVRB+X'74'. THE ADDRESS OF       00108000
*        THE ECB FOR A WTOR WILL BE LOCATED AT AN OFFSET 4 BYTES        00109000
*        INTO THE WPL. FOR AN XWPL THIS IS NOT CORRECT.                 00110000
*                                                                       00111000
*        THEREFORE TO ENSURE THE IEECVXIT PROVIDED BY USERMOD           00112000
*        ZUM0003 WILL CONTINUE TO FUNCTION CORRECTLY WHEN THIS          00113000
*        VERSION OF IEAVVWTO IS INSTALLED R5 IS INITIALIZED TO A        00114000
*        VALUE THAT WHEN PROCESSED BY THE IEECVXT CODE THE              00115000
*        IEECVXIT CODE WILL HAVE OBTAINED THE ECB ADDRESS.              00116000
*                                                                       00117000
*        THERE ARE OTHER IEECVXIT ROUTINES PROVIDED AS USERMODS.        00118000
*        THE CODE IN THEIR IEECVXIT ROUTINES WILL HAVE TO BE INSPECTED  00119000
*        TO ENSURE IT COMPLIES WITH THE DOCUMENTED IEECVXIT             00120000
*        PARAMETER LIST. IF DATA NOT PROVIDED IN THE DOCUMENTED         00121000
*        IEECVXIT PARAMETER IS REQUIRED THEN R9 HAS THE ADDRESS         00122000
*        OF THE IEAVVWTO WORKAREA WHICH CONTAINS ALL THE                00123000
*        REQUIRED DATA.                                                 00124000
*                                                                       00125000
*        KNOWN IEEVCXIT USERMODS -                                      00126000
*                                                                       00127000
*        USERMOD    COMPLIANT  ACTION                                   00128000
*        -------    ---------  -------------------------------------    00129000
*        ZP60001    YES        NO ACTION REQUIRED                       00130000
*        ZUM0003    NO         CODE PROVIDED IN IEAVVWTO TO SUPPORT     00131000
*                              THIS USERMOD SO NO CHANGE IS REQUIRED    00132000
*                              TO ZUM0003                               00133000
*                                                                       00134000
*        STANDARD SVC REGISTER CONVENTIONS -                            00135000
*        ON ENTRY -                                                     00136000
*        R0  USED WHEN THE CALLER ADDS ADDITIONAL MULTIPLE LINE         00137000
*            WTO MESSAGES TO A PREVIOUS STRING OF EXISTING MESSAGES,    00138000
*            IE THE LONG MLWTO ISSUED IN REPLY FOR A D U,DASD COMMAND.  00139000
*            IT CONTAINS THE MESSAGE IDENTIFICATION OF THE ORIGINAL     00140000
*            MESSAGE. R0 ALSO CONTAINS THE UCMID FOR ANY PROGRAM        00141000
*            SPECIFYING REG0 AND FOR PRIVILEGED PROGRAMS SPECIFYING     00142000
*            QREG0.                                                     00143000
*            HI-ORDER 3 BYTES, MESSAGE ID                               00144000
*            LOW ORDER BYTE, CONSOLE ID                                 00145000
*        R1  CALLER INPUT PARAMETER REGISTER (-> WPL OR XWPL)           00146000
*        R2     UNDEFINED                                               00147000
*        R3  -> CVT                                                     00148000
*        R4  -> TCB                                                     00149000
*        R5  -> SVRB                                                    00150000
*        R6  -> SVC ENTRY POINT                                         00151000
*        R7  -> ASCB                                                    00152000
*        R8-R12 UNDEFINED                                               00153000
*        R13    CALLERS VALUE                                           00154000
*        R14    RETURN ADDR TO SVC FLIH EXIT CODE                       00155000
*        R15    CALLERS VALUE                                           00156000
*                                                                       00157000
*        ON EXIT -                                                      00158000
*        R1     MESSAGE IDENTIFICATION NUMBER                           00159000
*        R15    RETURN CODE (ONLY FOR ML-WTO)                           00160000
*                                                                       00161000
         USING *,R6                                                     00162000
         USING CVT,R3                                                   00163000
         USING TCB,R4                                                   00164000
         USING RBBASIC,R5                                               00165000
         USING ASCB,R7                                                  00166000
         B     AROUND                                                   00167000
*                                                                       00168000
         DC    C'IEAVVWTO ZP60039 &SYSDATE &SYSTIME'                    00169000
*                                                                       00170000
AROUND   LA    R11,0(,R6)             CLEANUP R11                       00171000
         LA    R12,2048(,R11)                                           00172000
         LA    R12,2048(,R12)                                           00173000
         DROP  R6                                                       00174000
         USING IEAVVWTO,R11,R12                                         00175000
         XC    RBEXSAVE,RBEXSAVE      ENSURE ENTIRE RBEXSAVE IS ZERO    00176000
*                                                                       00177000
*        SAVE THE CONTENTS OF R0, R1 AND R14                            00178000
*                                                                       00179000
         ST    R0,XVWQEID             SAVE R0 IN EXTENDED SAVEAREA      00180000
*                                     IN CASE A MESSAGE NO/CONSOLE ID   00181000
*                                     HAS BEEN PROVIDED FOR A MLWTO     00182000
         STCM  R1,B'0111',XVWPLADR+1  SAVE R1 IN EXTENDED SAVEAREA      00183000
         STCM  R14,B'0111',XVRET+1    SAVE R14 IN EXTENDED SAVEAREA     00184000
*                                                                       00185000
*        GETMAIN WORKAREA FROM SUBPOOL 229                              00186000
*        INITIALIZE IT TO ZERO                                          00187000
*                                                                       00188000
         L     R0,@SIZDATD                                              00189000
         LR    R9,R0                                                    00190000
*                                                                       00191000
         GETMAIN  R,LV=(0)                                              00192000
*                                                                       00193000
         LR    R8,R1                                                    00194000
         SR    R15,R15                ZERO PAD CHAR AND SOURCE LENGTH   00195000
         MVCL  R8,R14                 ZERO WORKAREA                     00196000
         LR    R9,R1                                                    00197000
         USING @DATD,R9                                                 00198000
         ST    R13,@SA00001+4                                           00199000
         LR    R13,R9                                                   00200000
*                                                                       00201000
*/********************************************************************/ 00202000
*/*                                                                  */ 00203000
*/*  SET UP THE ESTAE ENVIRONMENT                                    */ 00204000
*/*                                                                  */ 00205000
*/*  SET THE FIRST FOOTPRINT FOR IEAVVWTO                            */ 00206000
*/*                                                                  */ 00207000
*/********************************************************************/ 00208000
*   R2=UCMFRRAD;                    /* ADDR OF IEAVMFRR, OUR RECOVERY   00209000
*                                      ROUTINE                       */ 00210000
         L     R2,CVTCUCB                                               00211000
         USING UCM,R2                                                   00212000
         L     R2,UCMFRRAD             R2 -> COMM TASK FRR IEAVMFRR     00213000
         DROP  R2                                                       00214000
*   REGRECOV=ESTAELST;              /* MOVE ESTAE PARM LIST TO WORK     00215000
*                                      AREA                          */ 00216000
         MVC   REGRECOV(ESTAELEN),ESTAELST                              00217000
*   R1=ADDR(REGRECOV);              /* GET ADDR OF ESTAE PARM LIST      00218000
*                                      AREA                          */ 00219000
         LA    R1,REGRECOV                                              00220000
*                                                                       00221000
*   R8=ADDR(EPARM);                 /* PROVIDE ADDR OF ESTAE PARM       00222000
*                                      AREA                          */ 00223000
         LA    R10,EPARM                                                00224000
*                                   /* SET UP ESTAE ENVIRONMENT USING*/ 00225000
*                                   /* COMM TASK FRR FOR RECOVERY    */ 00226000
         ESTAE (R2),CT,PARAM=(R10),RECORD=YES,MF=(E,(1))                00227000
*                                                                       00228000
         USING PARMLIST,R10         /* SET ADDR TO ESTAE PARM AREA   */ 00229000
*                                                                       00230000
*        INITIALIZE ESTAE PARM AREA                                     00231000
*                                                                       00232000
*   PARMRGAD=ADDR(REGSAVE);         /* R6 -> REG RESTORE FLAGS       */ 00233000
         LA    R6,REGSAVE           /*       AND SAVE AREA           */ 00234000
         ST    R6,PARMRGAD                                              00235000
*   PARMID=MODULEID;                /* IDENTIFY MODULE TO FRR        */ 00236000
         MVC   PARMID,KCVWTO                                            00237000
*   REGRGMAP=ALLREGS;               /* RESTORE ALL REGS AT RETRY     */ 00238000
         MVC   REGRGMAP,KXFFFF                                          00239000
*                                   /* SAVE REGS FOR RETRY IF NEEDED */ 00240000
         STM   R0,R15,REGRECOV                                          00241000
*   PARMFTPT=FTVALCHK;              /* SET FOOT PRINT TO SHOW           00242000
*                                      WORKING ON CHECKING THE          00243000
*                                      VALIDITY OF THE USER'S WPL    */ 00244000
         MVI   PARMFTPT,X'01'                                           00245000
*   PARMRTAD=0;                     /* ZERO RETRY ADDR               */ 00246000
         XC    PARMRTAD,PARMRTAD                                        00247000
*/*******************************************************************   00248000
*/*                                                                     00249000
*/*      SET UP THE EXTENDED SAVEAREA IN THE SVRB                       00250000
*/*                                                                     00251000
*/*******************************************************************   00252000
*        INPUT -                                                        00253000
*        XVWPLADR -> WPL, USERS PARM LIST (R1)                          00254000
*        XVWQEID   = THE CONTENTS OF R0 MESSAGE NUMBER/CONSOLE ID       00255000
*        XVRET     = RETURN ADDR                                        00256000
*        R3 -> CVT                                                      00257000
*        R4 -> TCB                                                      00258000
*        R5 -> SVRB WHICH CONTAINS THE EXTENDED SAVEAREA                00259000
*        R7 -> ASCB                                                     00260000
*                                                                       00261000
*        OUTPUT -                                                       00262000
*        THE EXTENDED SAVE AREA IS INITIALIZED                          00263000
*        VARIOUS BITS OR SETTINGS IN THE PARM LIST NOW APPEAR IN XSA    00264000
*        R6 -> MESSAGE PART OF THE WPL                                  00265000
*        XVA8 CONTAINS THE TIME IN DECIMAL FORMAT AS HHMMSSTH           00266000
*                                                                       00267000
*        THE MESSAGE LENGTH IS SAVED FOR INTEGRITY REASONS              00268000
*        THE LENGTH IS USED TO COMPUTE THE LENGTH OF THE PARM LIST      00269000
*        BY SAVING THE LENGTH, THE USER CANNOT CHANGE THE               00270000
*        LENGTH AFTER THE VALIDITY OF THE PARM LIST HAS BEEN CHECKED    00271000
*        LIST                                                           00272000
*                                                                       00273000
*/*******************************************************************   00274000
*                                                                       00275000
*/********************************************************************/ 00276000
*                                                                       00277000
*        DETERMINE IF THE CALLER IS AUTHORIZED                          00278000
*        DETERMINE IF THE CALLER IS A PROBLEM PROGRAM                   00279000
*        THIS INFO IS USED TO SELECT THE PROPER MESSAGE FLAGGING        00280000
*        AND ERROR RECOVERY PROCEDURES                                  00281000
*                                                                       00282000
*/********************************************************************/ 00283000
         TESTAUTH  STATE=YES,KEY=YES                                    00284000
*   IF R15^=0 THEN                  /* USER NOT STATE OR KEY            00285000
*                                      AUTHORIZED ?                  */ 00286000
         LTR   R15,R15                                                  00287000
         BZ    @RF00171                                                 00288000
*     DO;                           /* YES, THE USER IS A PROBLEM       00289000
*                                      PROGRAM                       */ 00290000
*       XVD1PP='1'B;                /* TURN ON PROBLEM PROGRAM BIT      00291000
*                                      IN XSA                        */ 00292000
         OI    XVD1,XVD1PP                                              00293000
*                                   /* CHECK IF APF AUTHORIZED       */ 00294000
         TESTAUTH FCTN=1                                                00295000
*       IF R15=0 THEN               /* USER APF AUTHORIZED ?         */ 00296000
         LTR   R15,R15                                                  00297000
         BNZ   @RC00174             /* NO, DO NOTHING                */ 00298000
*         XVD1AUTH='1'B;            /* YES, TURN ON AUTHORIZED BIT IN   00299000
*                                      XSA                           */ 00300000
@RF00171 OI    XVD1,XVD1AUTH                                            00301000
*                                                                       00302000
*/********************************************************************/ 00303000
*                                                                       00304000
*        CHECK IF USER IS A PRIVILEGED TASK                             00305000
*        A PRIVILEGED USER IS -                                         00306000
*        A. THE COMMUNICATIONS TASK                                     00307000
*        B. ANY TASK RUNNING UNDER AN SIRB                              00308000
*        C. A DAUGHTER TASK OF THE COMMUNICATIONS TASK                  00309000
*        D. AN AUTHORIZED SUBSYSTEM SUCH AS JES3                        00310000
*        A PRIVILEGED USER MAY PUT OUT A WTO ANY TIME                   00311000
*        IT DOES NOT HAVE TO WAIT FOR SPACE IF ALL THE WQES ARE         00312000
*        TAKEN. FIRST CHECK IF THE USER IS COMMTASK                     00313000
*                                                                       00314000
*        NOTE -                                                         00315000
*        THE SPECIAL CHECK FOR A DAUGHTER TASK IS ADDED TO PERMIT       00316000
*        TASK ATTACHED BY THE COMMUNICATIONS TASK TO EXCEED THE         00317000
*        WQE LIMITS IF NECESSARY                                        00318000
*                                                                       00319000
*/********************************************************************/ 00320000
*                                                                       00321000
*   IF ASCBASID=UCMCTID&            /* IN THE COMTASK'S MEMORY ?     */ 00322000
*       R4=UCMPXA                   /* AND COMTASK'S TCB             */ 00323000
*       UCMPXA=TCBOTC THEN          /* OR DAUGHTER TASK              */ 00324000
@RC00174 L     R15,CVTCUCB                                              00325000
         USING UCM,R15                                                  00326000
         CLC   ASCBASID,UCMCTID        COMPARE CURRENT ASID WITH        00327000
         BNE   @GL00001                COMTASK ASIB                     00328000
         C     R4,UCMPXA               COMPARE CURRENT TCB WITH         00329000
         BE    @RT00184                COMTASK TCB                      00330000
@GL00001 CLC   UCMPXA,TCBOTC           COMPARE COMTASK TCB WITH         00331000
         BNE   @RF00184                PARENT OF CURRENT TCB            00332000
         DROP  R15                                                      00333000
*     XVD1PRIV='1'B;                /* YES, THEN INDICATE A             00334000
*                                      PRIVILEGED TASK               */ 00335000
@RT00184 OI    XVD1,XVD1PRIV                                            00336000
         B     @RC00184                                                 00337000
*                                                                       00338000
*   ELSE                            /* NO, CHECK IF THIS RB IS AN       00339000
*                                      SIRB                          */ 00340000
*     DO;                           /* IS THE USER'S RB AN SIRB         00341000
*/********************************************************************/ 00342000
*                                                                       00343000
*        IF THE TASK IS NOT RUNNING IN SIRB MODE, THEN CHECK IF         00344000
*        SVC 35 WAS ENTERED TO ISSUE A LOG MESSAGE, OR SUBSYSTEM        00345000
*                                                                       00346000
*/********************************************************************/ 00347000
*       IF RBLINK->RBFTP='100'B TCBTID='F8'X THEN                       00348000
@RF00184 L     R15,RBLINK                                               00349000
         TM    RBSTAB1-RBBASIC(R15),RBFTSIRB                            00350000
         BNO   @GL00003                                                 00351000
         TM    RBSTAB1-RBBASIC(R15),RBFTTIRB                            00352000
         BZ    @RT00187                                                 00353000
@GL00003 CLI   TCBTID,TCBLOGID         LOG TASK ?                       00354000
         BNE   @RF00187                                                 00355000
*         XVD1PRIV='1'B;            /* YES, INDICATE A PRIVILEGED       00356000
*                                      TASK                          */ 00357000
@RT00187 OI    XVD1,XVD1PRIV                                            00358000
         B     @RC00184                                                 00359000
*                                                                       00360000
*       ELSE                                                            00361000
*         IF UCMJES3T=R7 THEN       /* IF SUBSYSTEM ISSUED WTO       */ 00362000
@RF00187 L     R15,CVTCUCB                                              00363000
         USING UCM,R15                                                  00364000
         C     R7,UCMJES3T                                              00365000
         BNE   IEA00152                                                 00366000
*           XVD1PRIV='1'B;          /* SET PRIVILEGED BIT            */ 00367000
         OI    XVD1,XVD1PRIV                                            00368000
         B     @RC00184                                                 00369000
*                                                                       00370000
*     END;                                                              00371000
*     END;                                                              00372000
IEA00152 LH    R8,UCMWQNR              R8 = CURRENT WQE COUNT           00373000
         CH    R8,UCMWQLM              COMPARE TO WQE BUFFER LIMIT      00374000
         BL    @RC00184                LESS THAN LIMIT, BRANCH          00375000
         L     R2,RBLINK               R2 -> PREVIOUS RB                00376000
         LA    R2,0(,R2)               CLEAR HI-ORDER BYTE              00377000
         B     IEA00178                                                 00378000
*                                                                       00379000
         DROP  R15                                                      00380000
IEA0016E L     R15,RBLINK-RBBASIC(,R2)                                  00381000
         LA    R2,0(,R15)              ZERO HI-ORDER BYTE               00382000
IEA00178 TM    RBSTAB1-RBBASIC(R2),RBFTIRB  IRB ?                       00383000
         BNO   IEA00188                                                 00384000
         TM    RBSTAB1-RBBASIC(R2),RBFTSIRB+RBFTTIRB-RBFTIRB            00385000
         BZ    IEA0018E                                                 00386000
IEA00188 CR    R2,R4                                                    00387000
         BNE   IEA0016E                                                 00388000
IEA0018E CR    R2,R4                                                    00389000
         BE    @RC00184                                                 00390000
*     XVD1PRIV='1'B;                /* THEN USER IS PRIVILEGED TASK  */ 00391000
         OI    XVD1,XVD1PRIV                                            00392000
*/********************************************************************/ 00393000
*/*                                                                  */ 00394000
*/*  RECORD THE TIME AND SAVE IT IN XVA8                             */ 00395000
*/*                                                                  */ 00396000
*/********************************************************************/ 00397000
*                                   /* SAVE THE REGS AT THIS POINT IN*/ 00398000
*                                   /* CASE THERE IS AN ERROR IN TIME*/ 00399000
*                                   /* SVC                           */ 00400000
@RC00184 STM   R0,R15,REGRECOV                                          00401000
*   PARMFTPT=FTTIME;                /* TIME WAS TAKEN                */ 00402000
         MVI   PARMFTPT,X'02'                                           00403000
*   PARMRTAD=ADDR(TIMERET);                                             00404000
         LA    R8,TIMERET                                               00405000
         ST    R8,PARMRTAD                                              00406000
*                                   /* TAKE THE TIME                 */ 00407000
         TIME  DEC,ERRET=TIMERET                                        00408000
*   XVA8=R0;                        /* SAVE THE TIME IN XVSAV AREA   */ 00409000
         ST    R0,XVA8                                                  00410000
*TIMERET:                           /* IF ERROR IN TIME THEN DONT       00411000
*                                      STORE R0                      */ 00412000
*   PARMFTPT=FTVALCHK;              /* RESUME CHECK OF WPL           */ 00413000
TIMERET  MVI   PARMFTPT,X'01'                                           00414000
*   PARMRTAD=0;                     /* ZERO RETRY ADDR               */ 00415000
         XC    PARMRTAD,PARMRTAD                                        00416000
         NI    PFLAG,255-NOCMSLOK      TURN OFF NO CMS LOCK REQUESTED   00417000
         OI    XVD2,XVD2VALD           SET ON THE DEFAULT OF THE        00418000
*                                      WPL BEING VALID                  00419000
*/********************************************************************/ 00420000
*/*                                                                  */ 00421000
*/*      SETUP ENVIRONMENT FOR PROCESSING THE WPL                    */ 00422000
*/*                                                                  */ 00423000
*/********************************************************************/ 00424000
*                                                                       00425000
*        OBTAIN THE LOCKS FOR BOTH AUTHORIZED AND                       00426000
*        NON AUTHORIZED CALLERS TO ALLOW FOR A BRANCH ENTRY             00427000
*        TO GETMAIN TO OBTAIN A STORAGE AREA FOR A LARGE                00428000
*        ML-WTO WPL OR XWPL                                             00429000
*                                                                       00430000
*               IF XVD1AUTH='0'B THEN/* CALLER AUTHORIZED ?          */ 00431000
*        TM    XVD1,XVD1AUTH                                            00432000
*        NOP   IEA001F4                YES, BRANCH                      00433000
         OI    PFLAG,NOCMSLOK          TURN ON NO CMS LOCK REQUESTED    00434000
*                                                                       00435000
*        OBTAIN THE LOCAL AND CMS LOCK                                  00436000
*        INSTALL AN FRR                                                 00437000
*        THE FRR HAS CREATED ITS OWN PARMLIST                           00438000
*        NOTE - NO RETRY ADDR IS PROVIDED IN THE FRR PARMLIST           00439000
*                                                                       00440000
         BAL   R14,SETLOCK                                              00441000
         NI    PFLAG,255-NOCMSLOK      TURN OFF NO CMS LOCK REQUESTED   00442000
*                                                                       00443000
*        PRIOR TO ACESSING THE CALLERS WPL/XWPL SETUP THE ESTAE         00444000
*        EXIT ROUTINE TO CATCH AND IDENTIFY ANY ERRORS IF THE           00445000
*        WPL/XWPL IS NOT ADDRESSABLE BY THE CALLER.                     00446000
*                                                                       00447000
*        ROUTINE VWTOVALR IS SET AS THE PARMLIST RETRY ADDR.            00448000
*        THE NO DUMP OPTION IS TURNED ON IN THE FRR PARMLIST            00449000
*        AS ANY ERROR IN ACCESSING OR PROCESSING THE WPL PROVIDED       00450000
*        BY THE CALLER IS NOT A SYSTEM ERROR BUT A USER ERROR           00451000
*        AND WILL BE REFLECTED TO THE CALLER WITH A D23 ABEND           00452000
*        WITH A REASON CODE IN R15                                      00453000
*                                                                       00454000
IEA001F4 STM   R0,R15,REGRECOV                                          00455000
         LA    R15,VWTOVALR            R15 -> WPL ERROR ROUTINE         00456000
         ST    R15,PARMRTAD            SAVE ADDR OF RTN IN FRR PARMLIST 00457000
         OI    PARMFLAG,PARMNDMP       SUPRESS DUMPS                    00458000
         MVI   PARMFTPT,X'01'       /* CHECKING OF WPL               */ 00459000
*                                                                       00460000
*                                   /* USE R6 TO POINT AT PARTS OF      00461000
*                                      PARM LIST                     */ 00462000
*                                   /* R6 IS BASE FOR WPL            */ 00463000
*   R6=XVWPLADR;                    /* PICK UP ADDR OF PARM LIST     */ 00464000
         L     R6,XVWPLADR             R6 = CALLERS R1 -> WPL/XWPL      00465000
         USING WPLRF,R6                                                 00466000
*                                                                       00467000
*        CHANGE TO USERS KEY PRIOR TO ACCESSING THE WPL/XWPL            00468000
*                                                                       00469000
         MODESET EXTKEY=RBT234,WORKREG=15                               00470000
*                                                                       00471000
*********************************************************************** 00472000
*                                                                       00473000
*        DETERMINE THE FORMAT OF THE PARAMETER LIST                     00474000
*        THERE ARE A NUMBER OF WTO AND WTOR WPL/XWPL PARAMETER LISTS    00475000
*        FOR WTO THERE ARE -                                            00476000
*        1. STANDARD 370 WTO WPL                                        00477000
*        2. EXTENDED WPL VERSION 1                                      00478000
*        3. EXTENDED WPL VERSION 2 (SUPPORT FOR TEXT= PARAMETER)        00479000
*        FOR WTOR THERE ARE -                                           00480000
*        1. STANDARD 370 WTOR WPL                                       00481000
*        2. 31 BIT WTOR WPL                                             00482000
*        3. EXTENDED WPL VERSION 1                                      00483000
*        4. EXTENDED WPL VERSION 2 (SUPPORT FOR TEXT= PARAMETER)        00484000
*                                                                       00485000
*        A CHECK IS MADE BY CHECKING THE WPLRLN (THE WTOR REPLY         00486000
*        LENGTH) FOR ZERO. IF THE WPL IS FOR A WTOR THEN WPLRLN         00487000
*        WILL BE THE FIRST BYTE IN THE PARM LIST. WPLRLN MUST           00488000
*        ALWAYS BE NON ZERO IF THE WPL IS FOR A WTOR A NON ZERO         00489000
*        VALUE DETERMINES THAT THE WPL IS A WTOR REQUEST. IF THE        00490000
*        HI-ORDER BIT IS ON THEN WPL IS A WTOR 31 BIT WPL. IF THE       00491000
*        WPL IS FOR A WTO THEN THERE WILL BE NO WTO PREFIX AND          00492000
*        WPLLGH WILL BE THE FIRST 2 BYTES. THE TEXT HAS A MAX           00493000
*        LENGTH OF 125 BYTES SO THE FIRST BYTE OF WPLLGH WILL BE        00494000
*        ZERO. AN EXTENDED WPL (XWPL) FOR EITHER A WTO OR A WTOR        00495000
*        IS INDICATED BY AN MCS FLAG. NO FLAGS CAN BE SET IN            00496000
*        IEAVVWTO STORAGE AT THIS STAGE BECAUSE THE KEY IS SET TO       00497000
*        THE CALLERS KEY TO ENSURE THE CALLER HAS VALID ACCESS TO       00498000
*        THE CALLER PROVIDED WPL/XWPL.                                  00499000
*                                                                       00500000
*********************************************************************** 00501000
*                                                                       00502000
         ICM   R15,B'0001',WPLLGH      REFERENCE FIRST CHAR IN WPL/XWPL 00503000
         BZ    VALWPL01                ZERO, WTO WPL/XWPL OR WTOR XWPL  00504000
*                                                                       00505000
*        WTOR 24/31 BIT WPL PROCESSING                                  00506000
*                                                                       00507000
         LM    R14,R0,WPLRPTR          VALIDATE CALLER ACCESS TO -      00508000
*                                      REPLY ADDRESS                    00509000
*                                      ECB ADDRESS                      00510000
*                                      L'TEXT AND MCS FLAGS             00511000
         LA    R6,8(,R6)               WTOR, SO INCR R6 PAST WTOR FLDS  00512000
         B     VALWPL03                                                 00513000
*                                                                       00514000
*        FIRST BYTE OF WPL/XWPL IS ZERO                                 00515000
*        DETERMINE IF THE PARAMETER LIST IS:-                           00516000
*        A. 24 BIT WTO  -OR-                                            00517000
*        B. XWPL (VERSION 1 OR VERSION 2) WTO OR WTOR                   00518000
*                                                                       00519000
VALWPL01 TM    WPLMCSF2,WPLMCSFL       XWPL PRESENT ?                   00520000
         BO    XWPL001                 YES, BRANCH, TEST FOR WTO/WTOR   00521000
*                                                                       00522000
*        PROCESS WPL FOR BOTH WTO AND WTOR                              00523000
*        R6 -> WPLLGH                                                   00524000
*                                                                       00525000
VALWPL03 ICM   R15,B'1111',0(R6)       VERIFY ACCESS TO WPL FIELDS      00526000
         LR    R8,R6                   TAKE A COPY OF -> WPL @ WPLLGH   00527000
*                                      IE AFTER THE WTOR FIELDS         00528000
         SLR   R15,R15                                                  00529000
         IC    R15,WPLLGH+1            PICK UP L'TEXT, IGNORE L'REPLY   00530000
*                                      WHICH MAY BE PRESENT IF WTOR 31  00531000
*                                                                       00532000
         AR    R8,R15                  ADD L'MSG TEXT + MCS FLAGS       00533000
*                                      R8 -> FIRST BYTE PAST MSG TEXT   00534000
         LR    R2,R8                   R2/R8-> FIRST BYTE PAST WPL TEXT 00535000
*                                                                       00536000
         TM    WPLMCSF,WPLMCSFA        DESCR/ROUTE CODES PRESENT ?      00537000
         BZ    IEA00244                NO, BRANCH                       00538000
         ICM   R15,B'1111',0(R8)       ATTEMPT TO ACCESS DESC/ROUTE FLD 00539000
         TM    WPLMCSF,WPLMCSFD        MSGTYP TYPE FIELD EXISTS ?       00540000
         BZ    IEA00244                NO, BRANCH                       00541000
*                                      MSGTYP CAUSES THE AUTOMATIC      00542000
*                                      GEN OF ROUTE AND DESC FIELDS     00543000
         ICM   R15,B'0011',4(R8)       VERIFY ACCESS TO MSGTYP FIELD    00544000
*                                                                       00545000
*        THE WPL FIELDS, TEXT LENGTH, MCS FLAGS, DESCRIPTOR AND         00546000
*        ROUTING CODES HAVE BEEN ACCESSED USING THE CALLERS KEY         00547000
*        FOR WTO/WTOR/WTOR31 FORMAT WPLS                                00548000
*                                                                       00549000
*        RETURN TO KEY ZERO TO COPY THESE FIELDS TO PROTECTED           00550000
*        STORAGE                                                        00551000
*                                                                       00552000
IEA00244 MODESET  EXTKEY=SUPR                                           00553000
*                                                                       00554000
         C     R6,XVWPLADR             WPL ADDR = CALLERS R1 VALUE ?    00555000
*                                      IE R6 NOT INCREMENTED OVER       00556000
*                                      WTOR FIELDS AT BEGINNING OF WPL  00557000
         BE    IEA00262                YES, PROCESS WTO                 00558000
*                                                                       00559000
*        PROCESS WTOR WPL                                               00560000
*                                                                       00561000
         OI    XVD2,XVD2WTOR           SET ON WTOR FLAG                 00562000
         LR    R0,R6                   SAVE CURRENT VALUE OF R6         00563000
         L     R6,XVWPLADR             R6 -> WTOR PREFIX                00564000
         MVC   WKARPBUF+1(3),WPLRPTRA  MOVE REPLY BUFFER ADDR           00565000
         MVC   WKAECBP,WPLRECB         MOVE ACROSS ECB ADDR             00566000
         MVC   WKARPYLN,WPLRLN         MOVE ACROSS L'REPLY              00567000
         TM    WPL31RRP,WPL31RFG       WTOR 31 BIT FORMAT ?             00568000
         LR    R6,R0                   YES, RESTORE R6                  00569000
         BZ    IEA00262                NO, BRANCH                       00570000
         MVC   WKARPYLN,WPL31RLN       MOVE ACROSS 31 BIT L'REPLY       00571000
*                                                                       00572000
*        COMMON WTO/WTOR PROCESSING                                     00573000
*                                                                       00574000
IEA00262 MVC   WKALGH+1(1),WPLLPTXT    SAVE L'MSG TEXT IN WKALGH        00575000
         MVC   WKAMCSF,WPLMCSF         COPY MCS FLAGS FROM WPL          00576000
*                                      THIS FIELD MAY BE ZERO FOR OLD   00577000
*                                      FORMAT (PRE MCS) WPL             00578000
*/********************************************************************/ 00579000
*                                                                       00580000
*        DETERMINE IF DESCRIPTOR AND ROUTE CODES WERE SPECIFIED         00581000
*        IN THE WPL                                                     00582000
*                                                                       00583000
*/********************************************************************/ 00584000
         TM    WKAMCSF,WPLMCSFA     /* DESC/ROUT SPECIFIED ?         */ 00585000
         BZ    IEA00288                NO, BRANCH, ALSO NO MSGTYPE      00586000
         MVC   WKADSC(4),0(R8)         YES,MOVE ACROSS DESC/ROUTE CODES 00587000
         LA    R2,4(,R2)               INCR FOR DESC/ROUTE FIELDS       00588000
*/********************************************************************/ 00589000
*                                                                       00590000
*        DETERMINE IF MSGTYP WAS SPECIFIED IN THE WPL                   00591000
*                                                                       00592000
*/********************************************************************/ 00593000
         TM    WKAMCSF,WPLMCSFD     /* MSGTYP FIELD SPECIFIED ?         00594000
         BZ    IEA00288                NO, BRANCH                       00595000
         MVC   WKAMSGTY,4(R8)          COPY THE MSGTYP FIELD FROM WPL   00596000
         LA    R2,2(,R2)               INCR PAST MSGTYP FIELD           00597000
IEA00288 TM    WKAMSGTY,WPLMSGTD       QID FIELD PROVIDED ?             00598000
         BNO   IEA002B8                NO, BRANCH                       00599000
         OI    XVD0,XVD0QID            YES, SET QID PROVIDED FLAG       00600000
*/********************************************************************/ 00601000
*                                                                       00602000
*        DETERMINE IF MULTI-LINE WTO/WTOR                               00603000
*                                                                       00604000
*/********************************************************************/ 00605000
IEA002B8 TM    WKAMCSF+1,WPLMCSFJ      MULTI-LINE WTO/WTOR ?            00606000
         BZ    IEA00348                NO, BRANCH TO COMMON PROCESSING  00607000
*                                          FOR WTO AND WTOR             00608000
         TM    XVD2,XVD2WTOR           YES, WTOR ?                      00609000
         BZ    IEA002E2                NO, BRANCH TO PROCESS MLWTO      00610000
         MVI   XVREASON,D23MLWTR       ERROR - MULTILINE WTOR SPECIFIED 00611000
         B     VWTOVALB                PROCESS ERROR                    00612000
*                                                                       00613000
*/********************************************************************/ 00614000
*                                                                       00615000
*        EXTENDED WPL VALIDATION                                        00616000
*                                                                       00617000
*/********************************************************************/ 00618000
*                                                                       00619000
*        DETERMINE THE FORMAT OF THE PARAMETER LIST FOR VALIDATION      00620000
*        THERE ARE 3 WTO/WTOR XWPL PARAMETER LISTS                      00621000
*        1. EXTENDED WPL VERSION 1                                      00622000
*        2. EXTENDED WPL VERSION 2 (SUPPORT FOR TEXT= PARAMETER)        00623000
*        3. EXTENDED WPL VERSION 4 ONWARDS(SUPPORT FOR TEXT= PARAMETER) 00624000
*                                                                       00625000
*        XWPL ACCESS VALIDATION USING THE CALLERS KEY                   00626000
*        ON ENTRY -                                                     00627000
*        R6 -> XWPL                                                     00628000
*                                                                       00629000
XWPL001  LR    R0,R6                   SAVE R6 -> XWPL                  00630000
         LH    R1,WPLLGH               R1 = L'MAJOR WQE TEXT + 4        00631000
         AR    R6,R1                   R6 -> FIRST BYTE PAST TEXT       00632000
*                                            (START OF XWPL FIELDS)     00633000
         LA    R15,92                  R15 = L'XWPL VERSION 1           00634000
         CLI   WPXVRSN,1               XWPL VERSION 1 ?                 00635000
         BE    XWPL002                 YES, BRANCH                      00636000
         IC    R15,WPXLNGTH            R15 = L'XWPL VER 2 AND ONWARDS   00637000
XWPL002  AR    R15,R1                  R15 = L'XWPL (TEXT + XWPL DATA)  00638000
         LR    R6,R0                   RESTORE R6 -> XWPL               00639000
         TM    WPLMCSF+1,WPLMCSFJ      MULTI-LINE WTO/WTOR ?            00640000
         BZ    XWPL002A                NO, BRANCH                       00641000
*                                                                       00642000
*        VALIDATE MULTI-LINE XWPL                                       00643000
*                                                                       00644000
         AR    R6,R15                  R6 -> MLWTO EXTENT HEADER        00645000
         LR    R2,R6                   R2 -> MLWTO EXTENT HEADER        00646000
         SLR   R1,R1                                                    00647000
         IC    R1,WPLLINES             R1 = TOTAL NUMBER OF LINES       00648000
         BCTR  R1,0                    R1 = NUMBER OF MINOR WQES        00649000
         LA    R6,4(,R6)               R6 -> WPLML MLWTO LINE ENTRY     00650000
         B     XWPL002B                                                 00651000
*                                                                       00652000
XWPL002C AH    R6,WPLML0               ADD L'LINE TO TOTAL IN R6        00653000
         BCTR  R1,0                    DECR MINOR WQE COUNT             00654000
XWPL002B LTR   R1,R1                   MORE MINOR WQES TO PROCESS ?     00655000
         BP    XWPL002C                LOOP TO PROCESS NEXT MINOR WQE   00656000
*                                                                       00657000
*        ALL MINOR WQES PROCESSED                                       00658000
*                                                                       00659000
         SR    R6,R2                   R6 = L'MLWTO MINOR WQE LINES     00660000
         LR    R3,R6                   R3 = L'MLWTO MINOR WQE LINES     00661000
         AR    R15,R6                  R15 = L'XWPL + ML WQE TEXT LINES 00662000
*                                      (TOTAL LENGTH OF XWPL)           00663000
*                                                                       00664000
*        VALIDATE CALLER ACCESS TO XWPL AND OPTIONAL MINOR WQES         00665000
*                                                                       00666000
XWPL002A LR    R14,R0                  R0 AND R14 -> XWPL               00667000
         LR    R1,R15                  R1 AND R15 = L'XWPL              00668000
         CLCL  R0,R14                  VALIDATE CALLER ACCESS TO XWPL   00669000
*                                      STAE EXIT WILL CATCH ANY         00670000
*                                      ATTEMPT TO ACCESS NON            00671000
*                                      ADDRESSABLE STORAGE              00672000
         L     R6,XVWPLADR             REFRESH R6 -> XWPL               00673000
         LR    R0,R6                   SAVE -> XWPL                     00674000
         AH    R6,WPLLGH               R6 -> XWPL FIELDS                00675000
         TM    WPXMCS2,WPXTXTAD        TEXT ADDR SPECIFIED ?            00676000
         LR    R6,R0                   RESTORE -> XWPL                  00677000
         BZ    XWPL003                 NO, BRANCH                       00678000
*                                                                       00679000
*        VALIDATE CALLER ACCESS TO THE STORAGE ADDRESSED BY TEXT= ADDR  00680000
*                                                                       00681000
         CLC   WPLLGH,KH8              L'TEXT = 8 ?                     00682000
         BNE   XWPLE06                 NO, ERROR                        00683000
         L     R14,WPLTXT              R14 -> L'MESSAGE TEXT            00684000
         LH    R15,0(,R14)             R15 = L'MESSAGE TEXT             00685000
         LA    R14,2(,R14)             R14 -> MESSAGE TEXT              00686000
         LR    R0,R14                  R0 -> MESSAGE TEXT               00687000
         LR    R1,R15                  R1 = L'MESSAGE TEXT              00688000
         CLCL  R0,R14                  VALIDATE CALLER ACCESS TO TEXT   00689000
*                                      STAE EXIT WILL CATCH ANY         00690000
*                                      ATTEMPT TO ACCESS NON            00691000
*                                      ACCESSABLE STORAGE               00692000
*        CHANGE TO KEY 0                                                00693000
*        COPY XWPL DATA TO PROTECTED STORAGE                            00694000
*                                                                       00695000
XWPL003  MODESET  EXTKEY=SUPR                                           00696000
*                                                                       00697000
*        COPY FIELDS FROM XWPL TO WORK AREA IN PROTECTED STORAGE        00698000
*                                                                       00699000
         MVC   WKAMCSF,WPLMCSF         MCS FLAGS                        00700000
         TM    WKAMCSF+1,WPLMCSFJ      MULTI-LINE WTO/WTOR ?            00701000
         BZ    XWPL003A                NO, BRANCH                       00702000
         ST    R2,MLWTOXH              R2 -> MLWTO EXTENT HEADER        00703000
         ST    R3,LENMWQE              R3 = L'OF ALL MINOR WQES         00704000
XWPL003A MVC   WKALGH,WPLLGH           L'MSG (8 IF XWPL V2 AND TEXT=)   00705000
         AH    R6,WKALGH               R6 -> FIRST BYTE PAST MSG TEXT   00706000
*                                      (XWPL FIELDS)                    00707000
         MVC   WKAVRSN,WPXVRSN         VERSION                          00708000
         MVC   WKARPYLN,WPXRPYLN       L'REPLY (WTOR ONLY)              00709000
         MVC   WKALNGTH,WPXLNGTH       L'XWPL (ZERO FOR V1)             00710000
         MVC   WKAMCSF1,WPXMCSF1       EXTENDED MCS FLAGS               00711000
         MVC   WKARPBUF,WPXRPBUF       REPLY BUFFER ADDR - WTOR ONLY    00712000
         MVC   WKAECBP,WPXECBP         REPLY ECB ADDRESS - WTOR ONLY    00713000
         MVC   WKADSC,WPXDESC          DESCRIPTOR CODES, 2 BYTES ONLY   00714000
         MVC   WKAROC,WPXROUT          ROUTING CODES, 2 BYTES ONLY      00715000
         MVC   WKAMSGTY,WPXMSGTY       MSGTYP FLAGS                     00716000
*                                                                       00717000
*        ADDITIONAL PROCESSING FOR XWPL WTOR                            00718000
*                                                                       00719000
         CLI   WKARPYLN,0              WTOR ?                           00720000
         BE    XWPL004A                NO, BRANCH                       00721000
         OI    XVD2,XVD2WTOR           YES, SET ON WTOR FLAG            00722000
         TM    WKAMCSF+1,WPLMCSFJ      MULTI-LINE WTOR ?                00723000
         BO    XWPLE02                 YES, ERROR                       00724000
         TM    XVWPLADR+3,X'03'        XWPL ON WORD BOUNDARY ?          00725000
         BNZ   XWPLE01                 NO, ERROR                        00726000
*                                                                       00727000
XWPL004A L     R6,XVWPLADR             R6 -> CALLERS PROVIDED WPL       00728000
         TM    WKAMCSF1+1,WPXTXTAD     TEXT ADDR SPECIFIED ?            00729000
         BZ    XWPL004                 NO, BRANCH                       00730000
*                                                                       00731000
*        PROCESS TEXT= PARAMETER                                        00732000
*                                                                       00733000
         L     R8,WPLTXT               R8 -> 2 BYTE L'TEXT FOLLOWED BY  00734000
*                                      THE TEXT                         00735000
         LH    R3,0(,R8)               R3 = L'MESSAGE TEXT              00736000
         LA    R3,4(,R3)               ADD 4 FOR COMPATIBILTY WITH      00737000
*                                      INLINE TEXT                      00738000
         STH   R3,WKALGH               SET THE CORRECT L'TEXT           00739000
         LA    R8,2(,R8)               R8 -> MESSAGE TEXT               00740000
         B     XWPL006                                                  00741000
*                                                                       00742000
*        PROCESS INLINE TEXT                                            00743000
*                                                                       00744000
XWPL004  LH    R3,WKALGH               R3 = L'TEXT +4                   00745000
         LA    R8,WPLTXT               R8 -> MESSAGE TEXT               00746000
*                                                                       00747000
*        COMMMON PROCESSING FOR INLINE AND TEXT= PARAMETERS             00748000
*                                                                       00749000
XWPL006  C     R3,LENMAXT              L'MAJOR WQE > MAX ALLOWED ?      00750000
         BNH   XWPL005                 NO, BRANCH                       00751000
         L     R3,LENMAXT              YES, SET LENGTH TO MAX ALLOWED   00752000
         STH   R3,WKALGH               STORE TRUNCATED VALUE            00753000
XWPL005  A     R3,LENMWQE              ADD L'MINOR WQES (IF ANY)        00754000
         C     R3,LENMAXT              EXCEED 129 ? MUST BE A ML-WTO    00755000
         BNH   XWPL007                 NO, BRANCH                       00756000
*                                                                       00757000
*        GETMAIN AREA FOR LONG TEXT FOR A MLWTO WPL FROM SUBPOOL 229    00758000
*                                                                       00759000
         ST    R3,LONGLEN              SAVE LENGTH OF LONG TEXT         00760000
         LR    R0,R3                                                    00761000
*                                                                       00762000
         GETMAIN RU,LV=(0),SP=229,BRANCH=YES                            00763000
*                                                                       00764000
         ST    R1,LONGTXT              STORE ADDR OF GETMAINED STORAGE  00765000
*                                      FOR LONG TXT                     00766000
         LR    R2,R1                   R2 -> LONG TEXT STORAGE          00767000
         B     XWPL008                                                  00768000
*                                                                       00769000
XWPL007  LA    R2,WKATEXT              R2 -> STANDARD TEXT AREA         00770000
*                                                                       00771000
*        R2 WILL EITHER POINT TO THE STANDARD TEXT AREA                 00772000
*        -OR-                                                           00773000
*        IF THAT IS NOT LONG ENOUGH THE GETMAINED STORAGE FOR AN        00774000
*        EXTENDED TEXT AREA                                             00775000
*                                                                       00776000
XWPL008  ST    R2,WKAADTXT             WKAADTXT -> TEXT FLD(S)          00777000
         MVC   0(R2,2),WKALGH          SET THE LENGTH                   00778000
         LA    R0,4(,R2)                                                00779000
         LH    R1,WKALGH                                                00780000
         S     R1,KF4                  R1 = ACTUAL L'TEXT               00781000
         LR    R15,R1                                                   00782000
         LR    R14,R8                                                   00783000
         MVCL  R0,R14                  MOVE MAJOR WQE TEXT FROM CALLER  00784000
*                                      WPL TO PROTECTED STORAGE         00785000
*                                      THIS MAY BE EITHER INLINE TEXT   00786000
*                                      OR TEXT POINTED TO BY THE        00787000
*                                      TEXT= PARAMETER                  00788000
         ICM   R1,B'1111',LENMWQE      R1 = L' MLWTO HDR + L'MINOR WQES 00789000
*                                      FROM ADDR OF LAST BYTE OF WPL    00790000
         BZ    XWPL009                 NO MINOR WQES, BRANCH            00791000
         L     R14,MLWTOXH             R14 -> MLWTO EXTENT HDR          00792000
         LR    R15,R1                                                   00793000
*                                                                       00794000
*        COPY CALLER PROVIDED MLWTO EXTENT HEADER AND MINOR WQES        00795000
*        TO PROTECTED STORAGE                                           00796000
*        THE MLWTO EXTENT HEADER AND MINOR WQES ARE NOW CONTIGOUS       00797000
*        TO MAJOR WQE MESSAGE TEXT                                      00798000
         MVCL  R0,R14                                                   00799000
*                                                                       00800000
*        COMPARE FIELDS IN USER PROVIDED WPL TO FIELDS PREVIOUSLY       00801000
*        BROUGHT INTO PROTECTED STORAGE TO DETERMINE IF THE             00802000
*        USER PROVIDED WPL HAS BEEN SUBSEQUENTLY MODIFIED               00803000
*                                                                       00804000
XWPL009  L     R3,CVTPTR               RESTORE R3 TO CVT ADDR           00805000
         L     R6,XVWPLADR             R6 -> CALLERS WPL                00806000
         TM    WKAMCSF1+1,WPXTXTAD     TEXT ADDR SPECIFIED ?            00807000
         BZ    XWPL009A                NO, BRANCH                       00808000
*                                                                       00809000
*        VALIDATE TEXT= TEXT                                            00810000
*                                                                       00811000
         CLC   WPLLGH,KH8              LENGTH CORRECT ?                 00812000
         BNE   XWPLE07                 NO, ERROR, LENGTH CHANGED        00813000
         L     R1,WPLTXT               R1 -> 2 BYTE L'TEXT FOLLOWED BY  00814000
*                                      THE TEXT                         00815000
         LH    R1,0(,R1)               R1 = L'TEXT                      00816000
         LA    R1,4(,R1)               ADD 4 FOR COMPATIBILTY WITH      00817000
*                                      INLINE TEXT                      00818000
         CH    R1,WKALGH               LENGTH REMAINS UNCHANGED ?       00819000
         BNE   XWPLE07                 NO, ERROR, XWPL CHANGED          00820000
         B     XWPL009B                                                 00821000
*                                                                       00822000
*        VALIDATE INLINE TEXT                                           00823000
*                                                                       00824000
XWPL009A CLC   WPLLGH,WKALGH           LENGTH REMAINS UNCHANGED ?       00825000
         BNE   XWPLE07                 NO, ERROR, XWPL CHANGED          00826000
XWPL009B CLC   WPLMCSF,WKAMCSF         MCS FLAGS MATCH ?                00827000
         BNE   XWPLE07                 NO, ERROR                        00828000
         AH    R6,WPLLGH               R6 -> XWPL FIELDS                00829000
         CLC   WKAMCSF1,WPXMCSF1       EXTENDED MCS FLAGS MATCH ?       00830000
         BNE   XWPLE07                 NO, ERROR                        00831000
         TM    XVD2,XVD2WTOR           ALREADY IDENTIFIED AS WTOR ?     00832000
         BO    XWPL010                 YES, BRANCH                      00833000
         CLI   WKARPYLN,0              WTO, WKARPYLN MUST BE ZERO       00834000
         BNE   XWPLE07                 NO, ERROR                        00835000
         B     XWPL011                 GO CHECK MSGTYP FIELD            00836000
*                                                                       00837000
XWPL010  CLI   WKARPYLN,0              WTOR, WKARPYLN NOT MUST BE ZERO  00838000
         BE    XWPLE07                 ZERO, ERROR                      00839000
         TM    WPXMCSF1,WPXWTOR        WTOR WITH XWPL PARAMETER LIST ?  00840000
         BZ    XWPLE07                 NO, ERROR                        00841000
XWPL011  CLC   WKAMSGTY,WPXMSGTY       MSGTYP FLAGS MATCH ?             00842000
         BNE   XWPLE07                 NO, ERROR                        00843000
         B     IEA004BE                YES, GOTO FINAL WPL ERROR CHECKS 00844000
*                                                                       00845000
*        ERROR EXIT OUT OF XWPL VALIDATION                              00846000
*        COMPLETE REASON CODE FIELD                                     00847000
*                                                                       00848000
XWPLE01  MVI   XVREASON,D23BNDY        WTOR PARMLIST NOT ON WORD BNDY   00849000
         B     XWPLEEE                                                  00850000
*                                                                       00851000
XWPLE02  MVI   XVREASON,D23MLWTR       ERROR - MULTILINE WTOR SPECIFIED 00852000
         B     XWPLEEE                                                  00853000
*                                                                       00854000
XWPLE06  MVI   XVREASON,D23LTXT        WTO/WTOR TEXT= AND WPLLGH ^=8    00855000
         B     XWPLEEE                                                  00856000
*                                                                       00857000
XWPLE07  MVI   XVREASON,D23CMOD        CALLER MODIFIED WPL              00858000
*                                                                       00859000
*        THE REASON CODE HAS BEEN COMPLETED FOR                         00860000
*        A VALIDITY CHECK FAILURE                                       00861000
*                                                                       00862000
XWPLEEE  B     VWTOVALB                BRANCH TO ERROR ROUTINE          00863000
*                                                                       00864000
*                                                                       00865000
*/********************************************************************/ 00866000
*                                                                       00867000
*        MULTI-LINE WTO PROCESSING                                      00868000
*                                                                       00869000
*/********************************************************************/ 00870000
*                                                                       00871000
*        R2 -> FIRST BYTE PAST DESC/ROUT/MSGTYP FIELDS                  00872000
*                                                                       00873000
IEA002E2 TM    XVD0,XVD0QID            QID PRESENT ?                    00874000
         BZ    IEA002EE                NO, BRANCH                       00875000
         LA    R2,2(,R2)               INCR OVER QID BYTES              00876000
*                                      R2 -> MLWTO EXTENSION HEADER     00877000
IEA002EE ST    R2,MLWTOXH              STORE ADDR OF MLWTO EXTENT HDR   00878000
*                                                                       00879000
*        CHANGE TO CALLERS KEY                                          00880000
*                                                                       00881000
         MODESET EXTKEY=RBT234,WORKREG=15                               00882000
*                                                                       00883000
*        PROCESS THE MLWTO EXTENSION HEADER                             00884000
*        LOOP THROUGH ALL THE MINOR WQES                                00885000
*        SUM THE LENGTH OF ALL THE MINOR WQES                           00886000
*        NOTE -                                                         00887000
*        THE NUMBER OF LINES IN THE WPL IS NOT CHECKED HERE             00888000
*        THIS IS DONE IN IEAVMWTO                                       00889000
*                                                                       00890000
         LR    R6,R2                   ADJUST WPLRF ADDR TO             00891000
*                                      -> MLWTO EXTENT HEADER           00892000
         SLR   R1,R1                                                    00893000
         IC    R1,WPLLINES             R1 = TOTAL NUMBER OF LINES       00894000
         BCTR  R1,0                    R1 = NUMBER OF MINOR WQES        00895000
         LA    R6,4(,R6)               R6 -> WPLML MLWTO LINE ENTRY     00896000
         B     IEA00330                                                 00897000
*                                                                       00898000
IEA00326 AH    R6,WPLML0               ADD L'LINE TO TOTAL IN R6        00899000
         BCTR  R1,0                    DECR MINOR WQE COUNT             00900000
IEA00330 LTR   R1,R1                   MORE MINOR WQES TO PROCESS ?     00901000
         BP    IEA00326                LOOP TO PROCESS NEXT MINOR WQE   00902000
         LR    R14,R6                  R14 -> LAST BYTE +1 OF WPL       00903000
         SR    R14,R2                  R14 = L'MLWTO EXTENSION          00904000
         LR    R2,R6                   R2 -> LAST BYTE +1 OF ENTIRE WPL 00905000
*                                                                       00906000
*        R2 -> END OF MLWTO WQE TEXT+1                                  00907000
*                                                                       00908000
*        CHANGE BACK TO SUPERVISOR KEY                                  00909000
*                                                                       00910000
         MODESET  EXTKEY=SUPR          CHANGE BACK TO SUPERVISOR KEY    00911000
*                                                                       00912000
         ST    R14,LENMWQE             SAVE L'MINOR WQES                00913000
*                                                                       00914000
*/********************************************************************/ 00915000
*                                                                       00916000
*        COMMON PROCESSING FOR WTO/WTOR AND MLWTO                       00917000
*        VALIDATE CALLER ACCESS TO WPL                                  00918000
*                                                                       00919000
*/********************************************************************/ 00920000
*                                                                       00921000
*        R2 -> LAST BYTE +1 OF ENTIRE WPL (WTO/WTOR/MLWTO)              00922000
*                                                                       00923000
IEA00348 L     R6,XVWPLADR             R6 -> WPL (WTO/WTOR/WTOR31)      00924000
         LR    R14,R6                  COPY WPL ADDR                    00925000
         LR    R3,R2                   R3 -> LAST BYTE +1 OF ENTIRE WPL 00926000
         SR    R3,R14                  SUBTRACT START OF WPL FROM END   00927000
*                                      R3 = L'WPL INCLUDING OPT FIELDS  00928000
*                                           AND MLWTO WQES IF PRESENT   00929000
         LR    R2,R14                  R2 AND R14 -> WPL                00930000
*                                                                       00931000
         MODESET EXTKEY=RBT234,WORKREG=15  CHANGE TO CALLERS KEY        00932000
*                                                                       00933000
         LR    R15,R3                  R3 = L'WPL INCLUDING OPT FIELDS  00934000
*                                           AND MLWTO WQES IF PRESENT   00935000
*                                      R2 = R14, SOURCE = TARGET        00936000
*                                                                       00937000
*        REFERENCE THE ENTIRE WPL TO ENSURE IT IS ADDRESSABLE BY        00938000
*        THE CALLER                                                     00939000
*                                                                       00940000
         CLCL  R2,R14                  VALIDATE CALLER ACCESS TO TEXT   00941000
*                                      STAE EXIT WILL CATCH ANY         00942000
*                                      ATTEMPT TO ACCESS NON            00943000
*                                      ADDRESSABLE STORAGE              00944000
*                                                                       00945000
*        TO PROCEED PAST THIS POINT THE ENTIRE WPL INCLUDING ANY        00946000
*        OPTIONAL MLWTO MINOR WQES ARE PROVEN TO BE ACCESSABLE BY       00947000
*        THE CALLER                                                     00948000
*VWTOCONT:                                                              00949000
VWTOCONT MODESET  EXTKEY=SUPR                                           00950000
*                                                                       00951000
         L     R3,CVTPTR               RESTORE CVT ADDR                 00952000
*       IF XVD2WTOR='1'B&           /* A VALID WTOR AND THE PARM        00953000
*                                      LIST ON A WORD BOUNDARY ?     */ 00954000
*           XVWPLADR//4^=0                                              00955000
*         THEN                                                          00956000
         TM    XVD2,XVD2WTOR           WTOR ?                           00957000
         BNO   @RF00231                NO, BRANCH                       00958000
         TM    XVWPLADR+3,X'03'        WORD BOUNDARY ?                  00959000
         BZ    @RF00231                YES, BRANCH                      00960000
*                                      NO, ERROR, NOT ON WORD BOUNDARY  00961000
         MVI   XVREASON,D23BNDY        WTOR WPL NOT ON WORD BOUNDARY    00962000
         B     VWTOVALB                                                 00963000
*                                                                       00964000
*     END;                          /* END OF CHECK OF PARM LIST     */ 00965000
*/******* END OF VALIDITY CHECK **************************************/ 00966000
*/********************************************************************/ 00967000
*/                                                                   */ 00968000
*/       THE WPL WAS CHECKED AND FOUND TO BE VALID                   */ 00969000
*/                                                                   */ 00970000
*/       COPY THE TEXT PROVIDED IN THE WPL, MAJOR WQE TEXT AND       */ 00971000
*/       MINOR WQE TEXT (IF A MLWTO) TO PROTECTED STORAGE.           */ 00972000
*/       IF THE STANDARD WKATEXT AREA IS NOT SUFFICIENTLY LONG       */ 00973000
*/       ENOUGH TO STORE THE COMBINED TEXT THEN GETMAIN AN AREA      */ 00974000
*/       SUFFICIENTLY LARGE ENOUGH TO STORE THE COMBINED TEXT IN     */ 00975000
*/       A CONTIGUOUS MANNER, MAJOR WQE TEXT FOLLOWED BY OPTIONAL    */ 00976000
*/       MULTIPLE MINOR WQE TEXT                                     */ 00977000
*/                                                                   */ 00978000
*/********************************************************************/ 00979000
*                                                                       00980000
*        ANY ERROR NOW DETECTED WILL RESULT IN AN FRR ABEND WITH DUMP   00981000
*                                                                       00982000
*       PARMRTAD=0;                 /* ZERO RETRY ADDR               */ 00983000
@RF00231 XC    PARMRTAD,PARMRTAD                                        00984000
*       PARMNDMP='0'B;              /* RESET DUMP INDICATOR          */ 00985000
         NI    PARMFLAG,255-PARMNDMP                                    00986000
         LH    R3,WKALGH               R3 = L'MAJOR WQE                 00987000
         C     R3,LENMAXT              L'MAJOR WQE > MAX ALLOWED ?      00988000
         BNH   @RFA0231                NO, BRANCH                       00989000
         L     R3,LENMAXT              YES, TRUNCATE LEN TO MAX ALLOWED 00990000
         STH   R3,WKALGH               SAVE THE L'TEXT                  00991000
@RFA0231 A     R3,LENMWQE              ADD L'MINOR WQES (IF ANY)        00992000
         C     R3,LENMAXT              EXCEED 129 ? MUST BE A ML-WTO    00993000
         BNH   IEA00412                NO, BRANCH                       00994000
*                                                                       00995000
*        GETMAIN AREA FOR LONG TEXT FOR A MLWTO WPL FROM SUBPOOL 229    00996000
*                                                                       00997000
         LR    R0,R3                                                    00998000
         ST    R3,LONGLEN              SAVE LENGTH OF LONG TEXT         00999000
*                                                                       01000000
         GETMAIN RU,LV=(0),SP=229,BRANCH=YES                            01001000
*                                                                       01002000
         ST    R1,LONGTXT              STORE ADDR OF GETMAINED STORAGE  01003000
*                                      FOR LONG TXT                     01004000
         LR    R2,R1                   R2 -> LONG TXT STORAGE           01005000
         B     IEA00416                                                 01006000
*                                                                       01007000
IEA00412 LA    R2,WKATEXT              R2 -> STANDARD TEXT AREA         01008000
*                                                                       01009000
*        R2 WILL EITHER POINT TO THE STANDARD TEXT AREA                 01010000
*        -OR-                                                           01011000
*        IF THAT IS NOT LONG ENOUGH THE GETMAINED STORAGE FOR AN        01012000
*        EXTENDED LENGTH TEXT AREA                                      01013000
*                                                                       01014000
IEA00416 ST    R2,WKAADTXT             WKAADTXT -> TEXT FLD(S)          01015000
         L     R14,XVWPLADR            R14 -> CALLERS PROVIDED WPL      01016000
         TM    XVD2,XVD2WTOR           WTOR WPL ?                       01017000
         BZ    IEAA0416                NO, BRANCH                       01018000
         LA    R14,8(,R14)             INCR PAST WTOR FIELDS            01019000
IEAA0416 LH    R15,WKALGH              R15 AND R3 = L'MAJOR WQE TEXT    01020000
         LR    R3,R15                                                   01021000
         MVCL  R2,R14                  MOVE MAJOR WQE TEXT FROM CALLER  01022000
*                                      WPL TO PROTECTED STORAGE         01023000
         ICM   R3,B'1111',LENMWQE      R3 = L' MLWTO HDR + L'MINOR WQES 01024000
*                                      FROM ADDR OF LAST BYTE OF WPL    01025000
         BZ    IEA00442                NO MINOR WQES, BRANCH            01026000
         L     R14,MLWTOXH             R14 -> MLWTO EXTENT HDR          01027000
         LR    R15,R3                                                   01028000
*                                                                       01029000
*        COPY CALLER PROVIDED MLWTO EXTENT HEADER AND MINOR WQES        01030000
*        TO PROTECTED STORAGE                                           01031000
*        THE MLWTO EXTENT HEADER AND MINOR WQES ARE NOW CONTIGUOUS      01032000
*        TO MAJOR WQE MESSAGE TEXT                                      01033000
         MVCL  R2,R14                                                   01034000
*                                                                       01035000
*        COMPARE FIELDS IN USER PROVIDED WPL TO FIELDS PREVIOUSLY       01036000
*        BROUGHT INTO PROTECTED STORAGE TO DETERMINE IF THE             01037000
*        USER PROVIDED WPL HAS BEEN SUBSEQUENTLY MODIFIED               01038000
*                                                                       01039000
IEA00442 L     R3,CVTPTR               RESTORE CVT ADDR                 01040000
         L     R6,XVWPLADR             R6 -> CALLERS WPL                01041000
         CLI   WPLRLN,0                FIRST BYTE OF WPL ZERO ?         01042000
         BE    IEA00462                YES, MUST BE WTO, BRANCH         01043000
         LA    R6,8(,R6)               WTOR, INCR PAST WTOR FIELDS      01044000
*                     IF XVD2WTOR='1'B THEN                             01045000
         TM    XVD2,XVD2WTOR           ALREADY IDENTIFIED AS WTOR ?     01046000
         BO    IEA0046E                YES, BRANCH                      01047000
         B     IEA00BAD                NO, BAD WPL                      01048000
*                                                                       01049000
*                     IF XVD2WTOR='1'B THEN                             01050000
IEA00462 TM    XVD2,XVD2WTOR           ALREADY IDENTIFIED AS WTOR ?     01051000
         BZ    IEA0046E                NO, BRANCH                       01052000
         B     IEA00BAD                YES, WPL BAD                     01053000
*                                                                       01054000
IEA0046E CLC   WPLLPTXT,WKALGH+1       L'MSG TEXT MATCH ?               01055000
         BNE   IEA00BAD                NO, BAD WPL                      01056000
         CLC   WPLMCSF,WKAMCSF         MCS FLAGS MATCH ?                01057000
         BNE   IEA00BAD                NO, BAD WPL                      01058000
*           IF WPLMCSFD='1'B THEN   /* MSGTYP SPECIFIED ?            */ 01059000
IEA0048A TM    WKAMCSF,WPLMCSFD     /* MSGTYP FIELD SPECIFIED ?         01060000
         BZ    IEA004BE                NO, BRANCH                       01061000
         CLC   4(2,R8),WKAMSGTY        YES, MSGTYPE FIELDS MATCH ?      01062000
         BE    IEA004BE                YES, WPL OK                      01063000
*                                                                       01064000
*        END OF WPL CHECKS                                              01065000
*                                                                       01066000
*        COMMON CODE FOR BAD WPL AND XWPL PROCESSING                    01067000
*                                                                       01068000
IEA00BAD MVI   XVREASON,D23CMOD        CALLER MODIFIED WPL              01069000
         B     VWTOVALB                                                 01070000
*                                                                       01071000
*        COMMON CODE FOR WPL AND XWPL PROCESSING                        01072000
*                                                                       01073000
*        FREE THE LOCKS OBTAINED FOR ALL CALLERS                        01074000
*                                                                       01075000
*               IF XVD1AUTH='0'B THEN/* CALLER AUTHORIZED ?             01076000
*        TM    XVD1,XVD1AUTH           AUTHORIZED ?                     01077000
*        BO    IEA004D2                YES, BRANCH                      01078000
IEA004BE OI    PFLAG,NOCMSLOK          TURN ON NO CMS LOCK REQUESTED    01079000
*                                                                       01080000
*        FREE THE LOCKS AND REMOVE THE FRR                              01081000
*        ORIGINAL ESTAE IS NOW IN PLACE                                 01082000
*                                                                       01083000
         BAL   R14,FREELOCK                                             01084000
         NI    PFLAG,255-NOCMSLOK      TURN OFF NO CMS LOCK REQUESTED   01085000
IEA004D2 EQU   *                                                        01086000
*                                                                       01087000
*   ELSE                            /* PARM LIST WAS VALID PROCESS IT*/ 01088000
*     DO;                                                               01089000
*/********************************************************************/ 01090000
*/*                                                                  */ 01091000
*/*  TAKE THE INSTALLATION EXIT                                      */ 01092000
*/*                                                                  */ 01093000
*/********************************************************************/ 01094000
*/*** START OF USEREXIT **********************************************/ 01095000
*/********************************************************************/ 01096000
*/*                                                                  */ 01097000
*/*   USEREXIT - SET UP AND TAKE INSTALLATION WTO EXIT               */ 01098000
*/*                                                                  */ 01099000
*/*  INPUT -                                                         */ 01100000
*/*    R2 -> LAST BYTE IN THE WQE                                    */ 01101000
*/*    R6 -> BEGINNING OF THE MESSAGE PART OF THE WPL                */ 01102000
*/*                                                                  */ 01103000
*/*  OUTPUT -                                                        */ 01104000
*/*    WKAROC AND WKADSC CONTAIN THE ROUTE AND DESCRIPTOR CODES      */ 01105000
*/*    BE USED IN THE WQE                                            */ 01106000
*/*    XVD2DELW IS ON IF THE MESSAGE IS NOT TO GO TO THE CONSOLE BUT */ 01107000
*/*    TO HARDCOPY ONLY.                                             */ 01108000
*/*                                                                  */ 01109000
*/********************************************************************/ 01110000
*/********************************************************************/ 01111000
*/*                                                                  */ 01112000
*/* CHECK THAT THIS IS NOT A CONNECTING MLWTO, CONNECTING MLWTOS DO  */ 01113000
*/* NOT GET SENT TO THE INSTALLATION EXIT UNLESS ISSUED              */ 01114000
*/* FROM A PROBLEM PROGRAM                                           */ 01115000
*/*                                                                  */ 01116000
*/* THE INSTALLATION USER EXIT WILL BE TAKEN FOR ANY OF              */ 01117000
*/* THESE THREE SITUATIONS:                                          */ 01118000
*/*                                                                  */ 01119000
*/*    1. THE WTO IS NOT A MULTI-LINE REQUEST                        */ 01120000
*/*         OR                                                       */ 01121000
*/*    2. THE MLWTO IS NOT A CONNECTING MLWTO REQUEST                */ 01122000
*/*         OR                                                       */ 01123000
*/*    3. THE CONNECTING MLWTO REQUEST IS ISSUED FROM A              */ 01124000
*/*           PROBLEM PROGRAM                                        */ 01125000
*/*                                                                  */ 01126000
*/* THE ONLY OTHER SITUATION IS IF A CONNECTING MLWTO IS             */ 01127000
*/* ISSUED FROM SUPERVISOR STATE                                     */ 01128000
*/*      FOR THIS CASE, DO NOT TAKE THE USER EXIT                    */ 01129000
*/*                                                                  */ 01130000
*/********************************************************************/ 01131000
*                                                                       01132000
*       IF WPLMCSFJ^='1'B           /* SITUATION 1. ABOVE            */ 01133000
*           XVWQEIDA=0              /* SITUATION 2. ABOVE            */ 01134000
*           XVD1PP='1'B THEN        /* SITUATION 3. ABOVE            */ 01135000
@RF00247 TM    WKAMCSF+1,WPLMCSFJ      MLWTO ?                          01136000
         BZ    @RT00250                NO, BRANCH                       01137000
         ICM   R15,B'0111',XVWQEIDA    YES, ANY MESSAGE ID PROVIDED ?   01138000
         BZ    @RT00250                NO, BRANCH                       01139000
         TM    XVD1,XVD1PP             PROBLEM PROGRAM ?                01140000
         BZ    @RF00250                NO, BRANCH                       01141000
*         DO;                       /* THIS IS NOT A CONNECTING         01142000
*                                      MLWTO UNLESS ISSUED FROM A       01143000
*                                      PROBLEM PROGRAM               */ 01144000
*/********************************************************************/ 01145000
*/*                                                                  */ 01146000
*/* CHECK IF WE SHOULD USE THE DEFAULTS FOR ROUTE & DESC CODES BY    */ 01147000
*/* CHECKING IF THIS IS AN OLD STYLE MESSAGE WHICH HAS NO MCS FLAGS  */ 01148000
*/*                                                                  */ 01149000
*/********************************************************************/ 01150000
*           IF WKAMCSF=0 THEN       /* ANY MCS FLAGS SET IN THE WPL ?*/ 01151000
@RT00250 ICM   R15,B'0011',WKAMCSF                                      01152000
         BNZ   @RF00264                YES, BRANCH                      01153000
*             DO;                   /* NO, USE DEFAULTS              */ 01154000
*               WKAROC=UCMOWTOR;    /* GET DEFAULT ROUTE CODES FROM     01155000
*                                      UCM                           */ 01156000
         L     R15,CVTCUCB                                              01157000
         S     R15,KF4                                                  01158000
         L     R15,0(,R15)              R10 -> UCM PREFIX               01159000
         USING UCMPRFX,R15                                              01160000
         MVC   WKAROC,UCMOWTOR          MOVE IN DEFAULT ROUTING CODES   01161000
         OI    WKAMCSF,WPLMCSFA      /* MARK R/D CODES PRESENT       */ 01162000
         DROP  R15                                                      01163000
*                                                                       01164000
*             END;                                                      01165000
*/********************************************************************/ 01166000
*/*                                                                  */ 01167000
*/* CHECK FOR AN INTERNAL MESSAGE (ONE THAT IS AUTHORIZED AND HAS ANY*/ 01168000
*/* MCSFLAGS(2-8) ON). IF IT IS AN INTERNAL MESSAGE THEN SKIP THE    */ 01169000
*/* INSTALLATION EXIT. CHECK ALSO THAT THE MSG LENGTH IS GREATER THAN*/ 01170000
*/* ZERO (THAT WKALGH > 4)                                           */ 01171000
*/*                                                                  */ 01172000
*/********************************************************************/ 01173000
*           IF^(XVD1AUTH='1'B&      /* DO WE NOT HAVE THE FOLLOWING:    01174000
*                                      (IS THE USER AUTHORIZED AND      01175000
*                                      ARE ANY SYSTEM RELATED           01176000
*                                      MCSFLAGS BITS TURNED ON (BITS    01177000
*                                      2 TO 8) ) AND THAT THE TEXT      01178000
*                                      LENGTH IS > FOUR              */ 01179000
*               WPLMCSF1(2:8)^='0000000'B)&WKALGH>4 THEN                01180000
*                                                                       01181000
@RF00264 TM    XVD1,XVD1AUTH           AUTHORIZED ?                     01182000
         AIF   (&TMVS805).U805A                                         01183000
         BZ    @GL00007                NO, BRANCH                       01184000
         AGO   .U805B                                                   01185000
.U805A   NOP   @GL00007                IGNORE AUTHORIZED STATUS         01186000
.U805B   ANOP                                                           01187000
         TM    WKAMCSF,255-WPLMCSFA    ANY SYSTEM RELATED MCS FLAGS ?   01188000
         AIF   (&TMVS805).U805C                                         01189000
         BNZ   @RF00250                YES, BRANCH                      01190000
         AGO   .U805D                                                   01191000
.U805C   NOP   @RF00250                IGNORE SYSTEM RELATED MCS FLAGS  01192000
.U805D   ANOP                                                           01193000
@GL00007 LH    R14,WKALGH              NO SYSTEM RELATED MCS FLAGS      01194000
         C     R14,KF4                 L'MCS + TEXT > 4                 01195000
         AIF   (&TMVS805).U805E                                         01196000
         BNH   @RF00250                NO, BRANCH                       01197000
         AGO   .U805F                                                   01198000
.U805E   NOP   @RF00250                IGNORE LENGTH CHECK              01199000
.U805F   ANOP                                                           01200000
*             DO;                   /* YES, THEN MESSAGE IS NOT A       01201000
*                                      SYSTEM INTERNALLY GENERATED      01202000
*                                      ONE. TAKE EXIT                */ 01203000
*               /*****************************************************/ 01204000
*               /*                                                   */ 01205000
*               /* FILL IN THS INSTALLATION EXIT'S PARAMETER LIST    */ 01206000
*               /* MOVE IN THE MESSAGE FROM THE WPL                  */ 01207000
*               /*                                                   */ 01208000
*               /*****************************************************/ 01209000
*               USERTEXT=WPLTXT(1:WKALGH-4);                            01210000
         MVI   USERTEXT,C' '                                            01211000
         MVC   USERTEXT+1(L'USERTEXT-1),USERTEXT                        01212000
         S     R14,KF5                 SUBTRACT 4 BYTES FOR L'TEXT      01213000
*                                      AND MCS FLAGS AND ONE FOR EX     01214000
         C     R14,KF127               EXCEED L'USERTEXT ? (128-1)      01215000
         BNH   @GL00008                                                 01216000
         L     R14,KF127               YES, TRUNCATE TEXT MOVE TO 128   01217000
@GL00008 L     R15,WKAADTXT                                             01218000
         EX    R14,@SM03493            MOVE WPL MAJOR WQE TEXT ONLY     01219000
*                                      TO USER EXIT TEXT                01220000
*                                      MVC   USERTEXT(0),4(R15)         01221000
*               USERDC=WKADSC;      /* PARM LIST FOR THE USER EXIT   */ 01222000
*               USERRC=WKAROC;      /* MOVE THE ROUTE/DESC CODES INTO*/ 01223000
         MVC   USERDC,WKADSC                                            01224000
         MVC   USERRC,WKAROC                                            01225000
*               /*****************************************************/ 01226000
*               /*                                                   */ 01227000
*               /* SET UP FOOT PRINT AROUND USER EXIT                */ 01228000
*               /*                                                   */ 01229000
*               /*****************************************************/ 01230000
*                                   /* SAVE CURRENT REGS         */     01231000
         STM   R0,R15,REGRECOV                                          01232000
*               PARMFTPT=FTUSERX;   /* INSTALLATION'S EXIT WAS CALLED*/ 01233000
         MVI   PARMFTPT,X'03'                                           01234000
*               PARMCLAD=0;         /* NO CLEAN UP ROUTINE           */ 01235000
         XC    PARMCLAD,PARMCLAD                                        01236000
*               PARMRTAD=0;         /* NO RETRY ADDR                 */ 01237000
         XC    PARMRTAD,PARMRTAD                                        01238000
*                                                                       01239000
*        /*****************************************************/        01240000
*        /*                                                   */        01241000
*        /* CALL INSTALLATION EXIT ROUTINE                    */        01242000
*        /*                                                   */        01243000
*        /*****************************************************/        01244000
*                                                                       01245000
*        CALL IEECVXIT(USERPARM);                                       01246000
*                                                                       01247000
*        THE IEECVXIT PROVIDED AS A COMPONENT OF THE BSPPILOT           01248000
*        USERMOD ZUM0003 CONTRIBUTED BY VOLKER BANDKE WILL              01249000
*        AUTOMATICALLY REPLY TO SELECTED WTOR MESSAGES. THE             01250000
*        CORRECT ORE FOR THE MESSAGE (AND HENCE THE REPLY NUMBER)       01251000
*        IS LOCATED BY THE IEECVXIT CODE BY COMPARING THE ECB           01252000
*        ADDRESS IN THE ORE WITH THAT PROVIDED IN THE CALLERS           01253000
*        WPL. THE CODE IN THE BSPPILOT IEECVXIT ASSUMES THAT R5         01254000
*        WILL POINT TO THE SVRB FOR SVC 35 AND THAT THE ADDRESS         01255000
*        OF THE WPL PASSED TO SVC 35 WILL BE LOCATED IN THE             01256000
*        SVRB+X'74'. THE ADDRESS OF THE ECB FOR A WTOR WILL BE          01257000
*        LOCATED AT AN OFFSET 4 BYTES INTO THE WPL. THIS IS NOT         01258000
*        CORRECT WITH THIS CODE. TO AVOID CHANGING THE IEECVXIT         01259000
*        USERMOD A DUMMY ECB WPL ADDRESS FIELD IS BUILT AND             01260000
*        PASSED VIA R5 TO THE USERMOD CODE                              01261000
*                                                                       01262000
         LR    R8,R5                   SAVE CORRECT SVRB ADDR           01263000
         LA    R15,WKAECBP-4           R15 -> FAKE WPL ADDR             01264000
         ST    R15,CVDAREA             SAVE FAKE WPL ADDR IN CVDAREA    01265000
         LA    R5,CVDAREA-X'74'        R5 -> WPL ADDR IN FAKE SVRB      01266000
*                                      PASS TO IEECVXIT CODE            01267000
         LA    R15,USERPARM                                             01268000
         ST    R15,PARMPTR                                              01269000
         L     R15,VIEECVXT                                             01270000
         LA    R1,PARMPTR                                               01271000
         BALR  R14,R15                                                  01272000
         LR    R5,R8                   RESTORE CORRECT SVRB ADDR        01273000
*               /*****************************************************/ 01274000
*               /*                                                   */ 01275000
*               /* RESTORE THE FOOTPRINTS AFTER THE EXIT ROUTINE     */ 01276000
*               /*                                                   */ 01277000
*               /*****************************************************/ 01278000
*               PARMFTPT=FTAFUX;                                        01279000
         MVI   PARMFTPT,X'04'                                           01280000
*/********************************************************************/ 01281000
*/*                                                                  */ 01282000
*/* CHECK IF THE ROUTE CODES WERE CHANGED                            */ 01283000
*/*                                                                  */ 01284000
*/********************************************************************/ 01285000
*               IF WKAROC^=         /* ROUTE CODES CHANGED ?         */ 01286000
*                   USERRC  THEN                                        01287000
         CLC   WKAROC,USERRC                                            01288000
         BE    @RF00282                NO, BRANCH                       01289000
*                 DO;               /* YES, PROCESS CHANGED ROUTE       01290000
*                                      CODES                         */ 01291000
*                   XVD2QFHC='1'B;  /* INDICATE MSG IS TO BE QUEUED     01292000
*                                      FOR HARD COPY                 */ 01293000
         OI    XVD2,XVD2QFHC                                            01294000
*/********************************************************************/ 01295000
*/*                                                                  */ 01296000
*/* CHECK IF ROUTE CODE WAS CHANGED TO ZERO, INDICATING THAT THIS    */ 01297000
*/*  MESSAGE SHOULDN'T GO TO THE CONSOLE                             */ 01298000
*/*                                                                  */ 01299000
*/********************************************************************/ 01300000
*                   IF USERRC=0 THEN                                    01301000
         ICM   R15,B'0011',USERRC                                       01302000
         BNZ   @RF00285                                                 01303000
*/********************************************************************/ 01304000
*/*                                                                  */ 01305000
*/* CHECK IF THIS IS A WTOR. IF SO THEN IGNORE REQUEST TO DELETE     */ 01306000
*/* MESSAGE (ZERO ROUTE CODE. IF WTO, DELETE MESSAGE. SEND JUST TO   */ 01307000
*/* HARDCOPY                                                         */ 01308000
*/*                                                                  */ 01309000
*/********************************************************************/ 01310000
*                     IF XVD2WTOR='1'B THEN                             01311000
         TM    XVD2,XVD2WTOR                                            01312000
         BZ    @RF00286                                                 01313000
*                       DO;         /* PUT IN WTOR ROUTE CODES       */ 01314000
*                         /*******************************************/ 01315000
*                         /*                                         */ 01316000
*                         /* ROUTE WTOR TO MASTER CONSOLE            */ 01317000
*                         /*                                         */ 01318000
*                         /*******************************************/ 01319000
*                         WKAROC='8000'X;                               01320000
         MVC   WKAROC,KX8000                                            01321000
*                       WPLMCSFA='1'B;/* TURN ON ROUTE CODES PRESENT    01322000
*                                      FLAGS                         */ 01323000
         OI    WKAMCSF,WPLMCSFA                                         01324000
         B     @RF00282                                                 01325000
*                                                                       01326000
*                       END;                                            01327000
*                     ELSE          /* NOT A WTOR. INDICATE SEND MSG    01328000
*                                      TO HARDCOPY ONLY              */ 01329000
*                       XVD2DELW='1'B;                                  01330000
@RF00286 OI    XVD2,XVD2DELW                                            01331000
         B     @RF00282                                                 01332000
*                                                                       01333000
*/********************************************************************/ 01334000
*/*                                                                  */ 01335000
*/* NO, THE ROUTE CODE WASNT SET TO ZERO, BUT IT IS CHANGED. PICK    */ 01336000
*/* UP THE NEW ROUTE CODES FOR USE LATER IN BUILDING THE WQE         */ 01337000
*/* ALSO PASS THE NEW ROUTE CODES TO WTP IF ROUTE CODE 11 IS SET     */ 01338000
*/*                                                                  */ 01339000
*/********************************************************************/ 01340000
*                   ELSE                                                01341000
*                     WKAROC=USERRC ;/* MOVE NEW ROUTE CODES TO         01342000
*                                      WKAROC                        */ 01343000
*                 END;              /* END OF PROCESSING OF CHANGED     01344000
*                                      ROUTE CODES                   */ 01345000
@RF00285 MVC   WKAROC,USERRC                                            01346000
*               IF WKADSC^=USERDC THEN/* DESC CODE CHANGED ?        */  01347000
@RF00282 CLC   WKADSC,USERDC                                            01348000
         BE    @RF00250                NO, BRANCH                       01349000
*                 DO;               /* YES, SAVE DESC CODES          */ 01350000
*                   WKADSC=USERDC ; /* SAVE USER CHANGES             */ 01351000
         MVC   WKADSC,USERDC                                            01352000
*                   XVD2QFHC='1'B;  /* INDICATE HARDCOPY MSG         */ 01353000
         OI    XVD2,XVD2QFHC                                            01354000
*                 END;                                                  01355000
*             END;                  /* OF PROCESSING NON-INTERNAL       01356000
*                                      MSGS                          */ 01357000
*         END;                      /* OF PROCESSING NON-CONNENCTING    01358000
*                                      MSGS                          */ 01359000
*/********************************************************************/ 01360000
*/*                                                                  */ 01361000
*/* WKAROC AND WKADSC CONTAIN THE POSSIBLY UPDATED ROUTE AND         */ 01362000
*/* DESCRIPTOR CODES AND MSGTYP FIELD TO BE USED BY THIS MESSAGE     */ 01363000
*/*                                                                  */ 01364000
*/********************************************************************/ 01365000
*                                                                       01366000
*/***** END OF USER EXIT SEGMENT *************************************/ 01367000
*                                                                       01368000
*/********************************************************************/ 01369000
*/*                                                                  */ 01370000
*/*  DETERMINE IF A MULTI-LINE WTO WAS ISSUED. IF SO CALL IEAVMWTO   */ 01371000
*/*                                                                  */ 01372000
*/********************************************************************/ 01373000
*                                                                       01374000
*       IF WPLMCSFJ='1'B THEN       /* MULTI-LINE FLAG SET IN WPL ?  */ 01375000
@RF00250 TM    WKAMCSF+1,WPLMCSFJ      NO, BRANCH                       01376000
         BZ    @RF00303                                                 01377000
*         DO;                       /* YES, GO TO PROCESS MULTI-LINE */ 01378000
*           PARMFTPT=FTMLWTO;       /* START OF MLWTO PROCESSING     */ 01379000
         MVI   PARMFTPT,X'05'                                           01380000
*           PARMRTAD=0;             /* DON'T RETRY. MLWTO DETECTS THE   01381000
*                                      ERROR                         */ 01382000
         XC    PARMRTAD,PARMRTAD                                        01383000
*           CALL IEAVMWTO;          /* PROCESS MULTI-LINE WTO        */ 01384000
         L     R15,VIEAVMWT                                             01385000
         BALR  R14,R15                                                  01386000
*           PARMFTPT=FTSWTO;        /* START OF WTO BLOCK PROCESSING */ 01387000
         MVI   PARMFTPT,X'06'                                           01388000
         B     VWTOERRT                GOTO EXIT PROCESSING             01389000
*                                                                       01390000
*         END;                                                          01391000
*       ELSE                        /* NO, SINGLE LINE SPECIFIED     */ 01392000
*/********************************************************************/ 01393000
*/*                                                                  */ 01394000
*/* DETERMINE IF PARM LIST LENGTH IS LESS THAN OR EQUAL TO FOUR      */ 01395000
*/* IF SO THEN THE MSGLENGTH IS ZERO. THIS IS AN ABEND               */ 01396000
*/* SITUATION FOR WTORS                                              */ 01397000
*/*                                                                  */ 01398000
*/********************************************************************/ 01399000
*         IF WKALGH<=4 THEN         /* DOES LENGTH SHOW ZERO MSG        01400000
*                                      LENGTH ?                      */ 01401000
@RF00303 CLC   WKALGH,KH4                                               01402000
         BH    @RF00314                                                 01403000
*/********************************************************************/ 01404000
*/*                                                                  */ 01405000
*/* CHECK FOR WTOR. IF WTOR, THEN SET ABEND BIT. IF WTO, THEN SET    */ 01406000
*/* THE NO MESSAGE BIT ON (XVD2ZERO)                                 */ 01407000
*/*                                                                  */ 01408000
*/********************************************************************/ 01409000
*           IF XVD2WTOR='1'B THEN   /* WTOR REQUEST ?                */ 01410000
         TM    XVD2,XVD2WTOR                                            01411000
         BZ    @RF00315                NO, BRANCH                       01412000
*                                      YES, WTOR                        01413000
         MVI   XVREASON,D23ZERO        ZERO TEXT LENGTH WTOR            01414000
         B     VWTOVALB                                                 01415000
*                                                                       01416000
*           ELSE                    /* NO, INDICATE RETURN ZERO MSG     01417000
*                                      ID TO                         */ 01418000
*             XVD2ZERO='1'B;        /* THE USER                      */ 01419000
@RF00315 OI    XVD2,XVD2ZERO                                            01420000
         B     VWTOERRT                                                 01421000
*                                                                       01422000
*/********************************************************************/ 01423000
*/*                                                                  */ 01424000
*/* MESSAGE LENGTH WAS NOT ZERO. PROCESS GOOD MESSAGE                */ 01425000
*/*                                                                  */ 01426000
*/********************************************************************/ 01427000
*         ELSE                                                          01428000
*           DO;                                                         01429000
*/********************************************************************/ 01430000
*/*                                                                  */ 01431000
*/*      CHECK FOR WRITE TO PROGRAMMER (ROUTE CODE 11).              */ 01432000
*/*      IF WTP WAS SPECIFIED THEN CALL WTP. WTP WILL RETURN         */ 01433000
*/*      CONTROL TO VWTO. ON RETURN FROM WTP THE XVD0USER BIT        */ 01434000
*/*      WILL BE ON IF THE MESSAGE WAS A WTP MESSAGE ONLY.           */ 01435000
*/*      THIS MEANS THAT THERE IS NOTHING FOR VWTO TO DO.            */ 01436000
*/*                                                                  */ 01437000
*/********************************************************************/ 01438000
*             XVD0USER='0'B;        /* INSURE THAT RETURN BIT FROM      01439000
*                                      WTP IS SET TO INDICATE           01440000
*                                      CONTINUE PROCESSING           */ 01441000
@RF00314 NI    XVD0,255-XVD0USER                                        01442000
*             IF WPLMCSFA = '1'B &     /* ROUTE CODES SPECIFIED ?    */ 01443000
*                 WPLROUTK='1'B THEN/* AND WTP SPECIFIED             */ 01444000
         TM    WKAMCSF,WPLMCSFA        ROUTING AND DESC PROVIDED ?      01445000
         BZ    @RF00320                NO, BRANCH                       01446000
         TM    WKAROC+1,WPLROUTK       WTP ROUTCDE=11 ?                 01447000
         BZ    @RF00320                NO, BRANCH                       01448000
*               DO;                 /* YES, CALL WTP. WTP WILL RETURN   01449000
*                                      CONTROL AFTER PROCESSING      */ 01450000
*                 CALL IGC0203E;    /* CALL WTP (IGC0203E)           */ 01451000
         L     R15,VIGC0203                                             01452000
         BALR  R14,R15                                                  01453000
*               END;                                                    01454000
*             /*******************************************************/ 01455000
*             /*                                                     */ 01456000
*             /* CHECK IF WTP FOUND THAT THERE WAS NO MORE PROCESSING*/ 01457000
*             /* FOR US TO DO WITH THIS MESSAGE. IF SO SET XVD2ZERO  */ 01458000
*             /* ON SO THAT A ZERO MESSAGE ID GETS PASSED TO THE     */ 01459000
*             /* USER.                                               */ 01460000
*             /*                                                     */ 01461000
*             /*******************************************************/ 01462000
*             IF XVD0USER='1'B THEN /* DID RETURN TO USER BIT GET SET   01463000
*                                      BY THE WTP ROUTINE            */ 01464000
@RF00320 TM    XVD0,XVD0USER                                            01465000
         BZ    @RF00327                NO, BRANCH                       01466000
*               XVD2ZERO='1'B;      /* YES. INDICATE ZERO MSG ID TO     01467000
*                                      BE RETURNED TO CALLER AND SKIP   01468000
*                                      THE REST OF THE PROCESSING    */ 01469000
         OI    XVD2,XVD2ZERO                                            01470000
         B     VWTOERRT                SETUP FOR RETURN TO CALLER       01471000
*                                                                       01472000
*             ELSE                  /* NO, PROCESS THE MESSAGE       */ 01473000
*               DO;                                                     01474000
*/********************************************************************/ 01475000
*/*                                                                  */ 01476000
*/* SET UP NEEDED LOCKS AND FRR                                      */ 01477000
*/*                                                                  */ 01478000
*/********************************************************************/ 01479000
*                 CALL SETLOCK;                                         01480000
@RF00327 BAL   R14,SETLOCK                                              01481000
*                 /***************************************************/ 01482000
*                 /*                                                 */ 01483000
*                 /* SET UP THE FOOTPRINT TO COVER THE BUILDING OF   */ 01484000
*                 /* THE ORE AND THE WQES. IF THERE IS A PROBLEM THEN*/ 01485000
*                 /* VWTOCLNP WILL ATTEMPT TO CLEAN UP THE CONTROL   */ 01486000
*                 /* BLOCKS                                          */ 01487000
*                 /*                                                 */ 01488000
*                 /***************************************************/ 01489000
*                 PARMFTPT=FTCTLBLK;/* START OF CONTROL BLOCK           01490000
*                                      PROCESSING                    */ 01491000
         MVI   PARMFTPT,X'07'                                           01492000
*                 PARMCLAD=ADDR(VWTOCLNP);/* ADDRESS OF CLEAN UP        01493000
*                                      ROUTINE                       */ 01494000
         LA    R8,VWTOCLNP                                              01495000
         ST    R8,PARMCLAD             SAVE ADDR IN FRR/ESTAE PARMLIST  01496000
*/********************************************************************/ 01497000
*/*                                                                  */ 01498000
*/*      GET THE NEEDED CONTROL BLOCKS                               */ 01499000
*/*                                                                  */ 01500000
*/********************************************************************/ 01501000
*/*                                                                  */ 01502000
*/*   GETBLKS - GET AN ORE AND/OR A WQE                              */ 01503000
*/*                                                                  */ 01504000
*/*   INPUT - XVD2WTOR IF ON INDICATES A WQE AND AN ORE ARE NEEDED   */ 01505000
*/*           IF OFF THEN JUST A WQE IS NEEDED.                      */ 01506000
*/*           XVD1PRIV INDICATES WHETHER THIS USER IS A PRIVILEGED   */ 01507000
*/*           TASK                                                   */ 01508000
*/*                                                                  */ 01509000
*/*   OUTPUT - XVOREAD POINTS AT THE ZEROED ORE                      */ 01510000
*/*            XVWQEAD POINTS AT THE ZEROED WQE                      */ 01511000
*/*            XVD1PERR IS ON IF CORE WAS NOT AVAILABLE              */ 01512000
*/*                                                                  */ 01513000
*/********************************************************************/ 01514000
*/*                                                                  */ 01515000
*/*  TURN OFF DO LOOP AND ECB INDICATORS                             */ 01516000
*/*                                                                  */ 01517000
*/********************************************************************/ 01518000
*                 XVD1ALDN='0'B;    /* INIT TO NOT ALL DONE          */ 01519000
         NI    XVD1,255-XVD1ALDN                                        01520000
*                 XVD0WWB='0'B;     /* INDICATE NO WTO WAIT BLOCK       01521000
*                                      OBTAINED                      */ 01522000
         NI    XVD0,255-XVD0WWB                                         01523000
*/********************************************************************/ 01524000
*/*                                                                  */ 01525000
*/*  CHECK FOR A WTOR. IF SO INDICATE THAT AN ORE IS NEEDED FIRST    */ 01526000
*/*                                                                  */ 01527000
*/********************************************************************/ 01528000
*                 IF XVD2WTOR='1'B THEN/* IS THIS REQUEST A WTOR     */ 01529000
         TM    XVD2,XVD2WTOR                                            01530000
         BZ    @RF00335                NO, BRANCH                       01531000
*                   DO;             /* YES                           */ 01532000
*                     XVD0NORE='1'B;/* GET THE ORE FIRST             */ 01533000
*                     XVD0NWQE='0'B;/* THEN GET THE WQE              */ 01534000
         OI    XVD0,XVD0NORE                                            01535000
         NI    XVD0,255-XVD0NWQE                                        01536000
         B     @RC00335                                                 01537000
*                                                                       01538000
*                   END;                                                01539000
*                 ELSE              /* NO, THIS IS A WTO             */ 01540000
*                   DO;                                                 01541000
*                     XVD0NORE='0'B;/* WE DONT NEED AN ORE           */ 01542000
*                     XVD0NWQE='1'B;/* BUT WE DO NEED A WQE          */ 01543000
@RF00335 OI    XVD0,XVD0NWQE                                            01544000
         NI    XVD0,255-XVD0NORE                                        01545000
@RC00335 B     @DE00344                                                 01546000
*                                                                       01547000
*                   END;                                                01548000
*/********************************************************************/ 01549000
*/*                                                                  */ 01550000
*/*  WE WILL ATTEMPT TO GET A WQE OR AN ORE, A REPLY ID AND A WQE,   */ 01551000
*/*  WE WILL LOOP UNTIL ALL NEEDED BLOCKS HAVE BEEN OBTAINED.        */ 01552000
*/*                                                                  */ 01553000
*/********************************************************************/ 01554000
*                 DO WHILE(XVD1ALDN='0'B);/* LOOP UNTIL WE ARE DONE  */ 01555000
*/********************************************************************/ 01556000
*/*                                                                  */ 01557000
*/*  CHECK IF WE STILL NEED TO GET AN ORE. IF SO GET ONE             */ 01558000
*/*                                                                  */ 01559000
*/********************************************************************/ 01560000
*                                                                       01561000
*                   IF XVD0NORE='1'B THEN                               01562000
*                                                                       01563000
@DL00344 TM    XVD0,XVD0NORE                                            01564000
         BZ    @RF00345                                                 01565000
*/********************************************************************/ 01566000
*/*                                                                  */ 01567000
*/*  CHECK IF AN ORE IS AVAILABLE                                    */ 01568000
*/*                                                                  */ 01569000
*/********************************************************************/ 01570000
*                     IF UCMRQNR<UCMRQLM /* IS THE NUMBER ORES LESS     01571000
*                                      THAN LIMIT                    */ 01572000
*                         XVD1PRIV='1'B THEN/* OR IS THE USER A         01573000
*                                      PRIVILEGED TASK               */ 01574000
         L     R15,CVTCUCB                                              01575000
         USING UCM,R15                                                  01576000
         LH    R8,UCMRQNR                                               01577000
         SLR   R2,R2                                                    01578000
         IC    R2,UCMRQLM                                               01579000
         CR    R8,R2                                                    01580000
         BL    @RT00346                                                 01581000
         TM    XVD1,XVD1PRIV           PRIVILIGED ?                     01582000
         BZ    @RF00346                NO, BRANCH                       01583000
*                       DO;         /* YES, GET AN ORE AND ID        */ 01584000
*                         CALL VWTOGETB(2);/* GET AN ORE AND ZERO IT */ 01585000
@RT00346 LA    R1,@AL00348                                              01586000
         BAL   R14,VWTOGETB                                             01587000
*/********************************************************************/ 01588000
*/*                                                                  */ 01589000
*/* CHECK TO SEE IF WE SUCCESSFULLY GOT AN ORE. IF NOT THEN SET      */ 01590000
*/* THE ERROR FLAG AND SET OTHER FLAGS SO THAT WE GET OUT OF         */ 01591000
*/* GET CONTROL BLOCKS LOOP.                                         */ 01592000
*/*                                                                  */ 01593000
*/********************************************************************/ 01594000
*                         IF XVOREAD=1 THEN/* WAS ERROR CONDITION SET   01595000
*                                      BY VWTOGETB                   */ 01596000
         CLC   XVOREAD,KF1                                              01597000
         BNE   @RF00349                                                 01598000
*                           DO;     /* YES. SET FLAGS TO GET OUT OF     01599000
*                                      LOOP                          */ 01600000
*                             XVD1ALDN='1'B;/* INDICATE THAT LOOP IS    01601000
*                                      OVER                          */ 01602000
*                             XVD1PERR='1'B;/* INDICATE AN ERROR        01603000
*                                      CONDITION FOUND               */ 01604000
         OI    XVD1,XVD1ALDN+XVD1PERR                                   01605000
*                             XVD0RPFD='1'B;/* SET SO THAT WE SKIP TO   01606000
*                                      THE WQE TEST AND THEN OUT     */ 01607000
         OI    XVD0,XVD0RPFD                                            01608000
         B     @RC00349                                                 01609000
*                                                                       01610000
*                           END;                                        01611000
*                         ELSE      /* AN ORE WAS OBTAINED. GET AN ID*/ 01612000
*                           DO;                                         01613000
*/********************************************************************/ 01614000
*/*  GETID - GET A REPLY ID SEGMENT                                  */ 01615000
*/*                                                                  */ 01616000
*/*  FUNCTION - TO OBTAIN ONE OF THE 100 REPLY IDS AND INSERT THE    */ 01617000
*/*     ID IN THE ORE.                                               */ 01618000
*/*                                                                  */ 01619000
*/*  INPUT - XVOREAD -> ORE                                          */ 01620000
*/*          UCMRPYL CONTAINS THE NEXT REPLY ID IN UCMRPYI TO TRY    */ 01621000
*/*          UCMRPYI IS THE BIT MAP OF AVAILABLE IDS                 */ 01622000
*/*                                                                  */ 01623000
*/*  OUTPUT - XVD0RPFD IS ON IF ID WAS STORED IN THE ORE             */ 01624000
*/*           XVD0RPFD IS OFF IF ID WAS NOT AVAILABLE                */ 01625000
*/*                                                                  */ 01626000
*/********************************************************************/ 01627000
*                             XVD0RPFD='0'B;/* DON'T TURN REPLY FOUND   01628000
*                                      ON UNTIL ID IS AVAILABLE      */ 01629000
@RF00349 NI    XVD0,255-XVD0RPFD                                        01630000
*                             STARTID=UCMRPYL;/* INIT THE STARTING ID*/ 01631000
         L     R15,CVTCUCB                                              01632000
         L     R15,UCMLSTP                                              01633000
         DROP  R15                                                      01634000
         USING UCMEIL,R15                                               01635000
         MVC   STARTID,UCMRPYL                                          01636000
         DROP  R15                                                      01637000
*                             DO UNTIL(XVD0RPFD='1'B /* DO UNTIL AN     01638000
*                                      ID IS FOUND                   */ 01639000
*                                   UCMRPYL=STARTID);/* OR ALL THE      01640000
*                                      ID'S HAVE BEEN SEARCHED       */ 01641000
*/********************************************************************/ 01642000
*/*                                                                  */ 01643000
*/* WE ADD ONE IN THE FOLLOWING TWO INSTRUCTIONS IN ORDER TO CONVERT */ 01644000
*/* THE UCMRPYL FROM ZERO ORIGIN INDEX TO A ONE ORIGIN INDEX VALUE   */ 01645000
*/* USED BY PL/S II                                                  */ 01646000
*/*                                                                  */ 01647000
*/********************************************************************/ 01648000
*                               R1=UCMRPYL/8+1;/* DEVELOP BYTE          01649000
*                                      OFFSET INTO UCMRPYI           */ 01650000
         DROP  R10                     USE R10 AS CODE TOO MESSY TO     01651000
*                                      CHANGE TO ANOTHER REG            01652000
@DL00358 LA    R10,1                                                    01653000
         L     R8,CVTCUCB                                               01654000
         USING UCM,R8                                                   01655000
         L     R15,UCMLSTP                                              01656000
         DROP  R8                                                       01657000
         SLR   R14,R14                                                  01658000
         USING UCMEIL,R15                                               01659000
         IC    R14,UCMRPYL                                              01660000
         DROP  R15                                                      01661000
         LR    R1,R14                                                   01662000
         SRL   R1,3                                                     01663000
         ALR   R1,R10                                                   01664000
*                               R2=UCMRPYL//8+1;/* DEVELOP BYTE         01665000
*                                      OFFSET INTO BIT MAP           */ 01666000
         ST    R15,@TF00001                                             01667000
         SRDA  R14,32                                                   01668000
         D     R14,KF8                                                  01669000
         ALR   R14,R10                                                  01670000
         LR    R2,R14                                                   01671000
*/********************************************************************/ 01672000
*/*                                                                  */ 01673000
*/* CHECK IF THE UCMRPYL-TH BIT IS OFF. IF SO TURN IT ON AND CONVERT */ 01674000
*/* THE ID                                                           */ 01675000
*/*                                                                  */ 01676000
*/********************************************************************/ 01677000
*                               IF(UCMRPYI(R1)&IDMAP(R2))^=IDMAP(R2)    01678000
*                                   THEN                                01679000
         LR    R10,R8                                                   01680000
         ALR   R10,R1                                                   01681000
         USING UCM,R10                                                  01682000
         MVC   @TS00001(1),UCMRPYI-1                                    01683000
         DROP  R10                                                      01684000
         LA    R10,IDMAP-1(R2)                                          01685000
         NC    @TS00001(1),0(R10)                                       01686000
         LA    R10,IDMAP-1(R2)                                          01687000
         CLC   @TS00001(1),0(R10)                                       01688000
         BE    @RF00362                                                 01689000
*                                 DO;/* YES, ID IS AVAILABLE         */ 01690000
*                                   CVD(UCMRPYL,CVDAREA);/* CONVERT     01691000
*                                      BIT ID INTO DECIMAL IN 8 BYTE    01692000
*                                      AREA -CVDAREA                 */ 01693000
*                                                                       01694000
         L     R10,@TF00001                                             01695000
         USING UCMEIL,R10                                               01696000
         SLR   R0,R0                                                    01697000
         IC    R0,UCMRPYL                                               01698000
         DROP  R10                                                      01699000
         CVD   R0,CVDAREA                                               01700000
*                                   /*********************************/ 01701000
*                                   /*                               */ 01702000
*                                   /* UNPACK ONLY THE LAST TWO BYTES*/ 01703000
*                                   /* OF CONVERTED NUMBER           */ 01704000
*                                   /*                               */ 01705000
*                                   /*********************************/ 01706000
*                                                                       01707000
*                                   UNPK(OREID,CVDAREA(7:8));           01708000
*                                                                       01709000
         L     R10,XVOREAD                                              01710000
         USING OREF,R10                                                 01711000
         UNPK  OREID,CVDAREA+6(2)                                       01712000
*                                   /*********************************/ 01713000
*                                   /*                               */ 01714000
*                                   /* REPLACE SIGN WITH 'F0' ZONE   */ 01715000
*                                   /* BITS                          */ 01716000
*                                   /*                               */ 01717000
*                                   /*********************************/ 01718000
*                                                                       01719000
*                                   OREID(2)=OREID(2) 'F0'X;            01720000
         OI    OREID+1,X'F0'                                            01721000
         DROP  R10                                                      01722000
*                                   XVD0RPFD='1'B;/* INDICATE AN ID     01723000
*                                      WAS FOUND                     */ 01724000
         OI    XVD0,XVD0RPFD                                            01725000
*                                   /*********************************/ 01726000
*                                   /*                               */ 01727000
*                                   /* TURN ON ID BIT IN BIT MAP     */ 01728000
*                                   /*                               */ 01729000
*                                   /*********************************/ 01730000
*                                   UCMRPYI(R1)=UCMRPYI(R1) IDMAP(R2);  01731000
         ALR   R8,R1                                                    01732000
         LA    R10,IDMAP-1(R2)                                          01733000
         USING UCM,R8                                                   01734000
         OC    UCMRPYI-1(1),0(R10)                                      01735000
         DROP  R8                                                       01736000
*                                   REG1SAV=R1;/* SAVE CONTENTS OF      01737000
*                                      R1 IN CASE WE HAVE TO GIVE       01738000
*                                      BACK THE ID                   */ 01739000
         ST    R1,REG1SAV                                               01740000
*                                   REG2SAV=R2;/* SAVE CONTENTS OF      01741000
*                                      R2 FOR SAME REASON            */ 01742000
         ST    R2,REG2SAV                                               01743000
*                                 END;                                  01744000
*                               IF UCMRPYL=NINTY9 THEN/* END OF ID'S    01745000
*                                      REACHED?                      */ 01746000
@RF00362 L     R10,CVTCUCB                                              01747000
         USING UCM,R10                                                  01748000
         L     R10,UCMLSTP                                              01749000
         DROP  R10                                                      01750000
         USING UCMEIL,R10                                               01751000
         CLI   UCMRPYL,99                                               01752000
         BNE   @RF00372                                                 01753000
*                                 UCMRPYL=0;/* YES, GO BACK TO 1ST ID   01754000
*                                                                    */ 01755000
         MVI   UCMRPYL,X'00'                                            01756000
         DROP  R10                                                      01757000
*                               ELSE/* NO, INCREMENT UCMRPYL. THIS IS   01758000
*                                      DONE EVEN THOUGH AN ID MAY BE    01759000
*                                      PICKED, SO THAT UCMRPYL IS       01760000
*                                      INITIALIZED FOR THE NEXT ID      01761000
*                                      SCAN                          */ 01762000
*                                 UCMRPYL=UCMRPYL+1;                    01763000
         B     @DE00358                                                 01764000
*                                                                       01765000
@RF00372 L     R10,CVTCUCB                                              01766000
         USING UCM,R10                                                  01767000
         L     R10,UCMLSTP                                              01768000
         DROP  R10                                                      01769000
         LA    R8,1                                                     01770000
         SLR   R0,R0                                                    01771000
         USING UCMEIL,R10                                               01772000
         IC    R0,UCMRPYL                                               01773000
         ALR   R8,R0                                                    01774000
         STC   R8,UCMRPYL                                               01775000
         DROP  R10                                                      01776000
*                             END;  /* OF DO UNTIL (FINDING A FREE      01777000
*                                      ID)                           */ 01778000
@DE00358 TM    XVD0,XVD0RPFD                                            01779000
         BO    @RC00349                                                 01780000
         L     R10,CVTCUCB                                              01781000
         USING UCM,R10                                                  01782000
         L     R10,UCMLSTP                                              01783000
         DROP  R10                                                      01784000
         USING UCMEIL,R10                                               01785000
         CLC   UCMRPYL,STARTID                                          01786000
         BNE   @DL00358                LOOP BACK                        01787000
         DROP  R10                                                      01788000
*/****** END OF GETID SEGMENT ****************************************/ 01789000
*                           END;                                        01790000
*/********************************************************************/ 01791000
*/*                                                                  */ 01792000
*/*  CHECK TO SEE IF AN ID WAS AVAILABLE                             */ 01793000
*/*                                                                  */ 01794000
*/********************************************************************/ 01795000
*                                                                       01796000
*        RESTORE PARMLIST ADDRESSABILITY                                01797000
@RC00349 L     R10,ESTAEPRM                                             01798000
         USING PARMLIST,R10                                             01799000
*                         IF XVD0RPFD='1'B THEN/* WAS A REPLY ID        01800000
*                                      FOUND                         */ 01801000
         TM    XVD0,XVD0RPFD                                            01802000
         BZ    @RF00378                NO, BRANCH                       01803000
*                           DO;     /* YES, INDICATE WE NOW NEED A      01804000
*                                      WQE                           */ 01805000
*                             XVD0NORE='0'B;/* PROCESSING DONE FOR AN   01806000
*                                      ORE                           */ 01807000
*                             XVD0NWQE='1'B;/* GET A WQE             */ 01808000
         OI    XVD0,XVD0NWQE                                            01809000
         NI    XVD0,255-XVD0NORE                                        01810000
         B     @RF00345                                                 01811000
*                                                                       01812000
*                           END;                                        01813000
*                         ELSE      /* NO, A REPLY WASNT AVAILABLE   */ 01814000
*                           DO;     /* FREE THE ORE JUST OBTAINED    */ 01815000
*                             CALL VWTOFORE;/* FREE THE ORE          */ 01816000
@RF00378 BAL   R14,VWTOFORE                                             01817000
*                             /***************************************/ 01818000
*                             /*                                     */ 01819000
*                             /* WAIT FOR AN ORE TO BE FREED         */ 01820000
*                             /*                                     */ 01821000
*                             /***************************************/ 01822000
*                             CALL VWTOWAIT(UCMOECBT);                  01823000
         L     R15,CVTCUCB                                              01824000
         USING UCM,R15                                                  01825000
         LA    R15,UCMOECBT                                             01826000
         ST    R15,PARMPTR                                              01827000
         LA    R1,PARMPTR                                               01828000
         BAL   R14,VWTOWAIT                                             01829000
         B     @RF00345                                                 01830000
*                                                                       01831000
*                           END;                                        01832000
*                       END;        /* OF GET ORE AND ID PART        */ 01833000
*/********************************************************************/ 01834000
*/*                                                                  */ 01835000
*/*  NO, AN ORE WASN'T AVAILABLE. WAIT FOR AN ORE TO BE FREE'D       */ 01836000
*/*                                                                  */ 01837000
*/********************************************************************/ 01838000
*                     ELSE                                              01839000
*                       CALL VWTOWAIT(UCMOECBT);                        01840000
@RF00346 L     R15,CVTCUCB                                              01841000
         LA    R15,UCMOECBT                                             01842000
         ST    R15,PARMPTR                                              01843000
         LA    R1,PARMPTR                                               01844000
         BAL   R14,VWTOWAIT                                             01845000
*/********************************************************************/ 01846000
*/*                                                                  */ 01847000
*/*  CHECK IF WE NEED A WQE                                          */ 01848000
*/*                                                                  */ 01849000
*/********************************************************************/ 01850000
*                   IF XVD0NWQE='1'B&/* IS A WQE NEEDED NOW          */ 01851000
*                       XVD1PERR='0'B THEN/* AND WAS THERE NO ERROR     01852000
*                                      IN GETTING AN ORE             */ 01853000
@RF00345 TM    XVD0,XVD0NWQE                                            01854000
         BZ    @DE00344                                                 01855000
         TM    XVD1,XVD1PERR                                            01856000
         BNZ   @DE00344                                                 01857000
*/********************************************************************/ 01858000
*/*                                                                  */ 01859000
*/*  YES, CHECK IF ONE IS AVAILABLE, IF SO, GET IT. IF NOT WAIT      */ 01860000
*/*  UNTIL ONE IS FREE'D,                                            */ 01861000
*/*                                                                  */ 01862000
*/********************************************************************/ 01863000
*                     IF UCMWQNR<UCMWQLM /* IS THE NUMBER OF WQES       01864000
*                                      LESS THAN THE LIMIT OR        */ 01865000
*                         XVD1PRIV='1'B THEN/* IS THE USER A            01866000
*                                      PRIVILEGED TASK               */ 01867000
         L     R15,CVTCUCB                                              01868000
         LH    R8,UCMWQNR                                               01869000
         CH    R8,UCMWQLM                                               01870000
         BL    @RT00390                                                 01871000
         DROP  R15                                                      01872000
         TM    XVD1,XVD1PRIV           PRIVILEGED CALLER ?              01873000
         BZ    @RF00390                NO, BRANCH                       01874000
*                       DO;         /* YES, GET THE WQE              */ 01875000
*                         CALL VWTOGETB(1);/* GET A WQE AND ZERO IT  */ 01876000
@RT00390 LA    R1,@AL00392                                              01877000
         BAL   R14,VWTOGETB                                             01878000
*/********************************************************************/ 01879000
*/*                                                                  */ 01880000
*/* CHECK IF A WQE WAS OBTAINED BY VWTOGETB.  IF NOT THEN CHECK IF   */ 01881000
*/* THIS IS A WTOR. IF SO THEN FREE THE ORE AND REPLY ID THAT WAS    */ 01882000
*/* OBTAINED.                                                        */ 01883000
*/*                                                                  */ 01884000
*/********************************************************************/ 01885000
*                         IF XVWQEAD=1 THEN/* WAS THERE AN ERROR IN     01886000
*                                      GETTING THE WQE?              */ 01887000
         CLC   XVWQEAD,KF1                                              01888000
         BNE   @RF00393                                                 01889000
*                           DO;     /* YES, CHECK IF THIS IS A WTOR     01890000
*                                      REQUEST                       */ 01891000
*                             IF XVD2WTOR='1'B THEN/* IS THIS A WTOR?*/ 01892000
         TM    XVD2,XVD2WTOR                                            01893000
         BZ    @RF00395                NO, BRANCH                       01894000
*                               DO; /* YES. FREE THE ORE AND REPLY ID*/ 01895000
*                                 CALL VWTOFORE;/* FREE THE ORE      */ 01896000
*                                                                       01897000
         BAL   R14,VWTOFORE                                             01898000
*                                 /***********************************/ 01899000
*                                 /*                                 */ 01900000
*                                 /* FREE THE REPLY ID JUST PICKED   */ 01901000
*                                 /* THE INDEXS TO UCMRPYI AND IDMAP */ 01902000
*                                 /* WERE SAVED BY GETID IN REG1SAV  */ 01903000
*                                 /* AND REG2SAV                     */ 01904000
*                                 /*                                 */ 01905000
*                                 /***********************************/ 01906000
*                             UCMRPYI(REG1SAV)=UCMRPYI(REG1SAV)&(-1&&   01907000
*                                     IDMAP(REF2SAV));                  01908000
         L     R14,REG1SAV                                              01909000
         L     R8,CVTCUCB                                               01910000
         USING UCM,R8                                                   01911000
         L     R2,REG2SAV             RESTORE R2                        01912000
         SLR   R15,R15                                                  01913000
         IC    R15,IDMAP-1(R2)                                          01914000
         X     R15,KFM1                                                 01915000
         SLR   R2,R2                                                    01916000
         IC    R2,UCMRPYI-1(R14)                                        01917000
         NR    R15,R2                                                   01918000
         STC   R15,UCMRPYI-1(R14)                                       01919000
         DROP  R8                                                       01920000
*                               END;/* OF FREEING ORE                */ 01921000
*                             XVD1PERR='1'B;/* INDICATE AN ERROR        01922000
*                                      CONDITION FOUND               */ 01923000
*                           END;    /* IN ERROR GETTING WQE          */ 01924000
@RF00395 OI    XVD1,XVD1PERR                                            01925000
*                         XVD1ALDN='1'B;/* INDICATE WE ARE ALL DONE  */ 01926000
@RF00393 OI    XVD1,XVD1ALDN                                            01927000
         B     @DE00344                                                 01928000
*                                                                       01929000
*                       END;                                            01930000
*                     ELSE          /* NO, A WQE IS NOT AVAILABLE    */ 01931000
*                       DO;                                             01932000
*                                                                       01933000
*/********************************************************************/ 01934000
*/*                                                                  */ 01935000
*/*  CHECK IF THIS IS A WTOR. IF SO FREE THE ORE AND REPLY ID. THIS  */ 01936000
*/*  IS DONE SO WE DON'T GET IN AN INTERLOCK SITUATION               */ 01937000
*/*                                                                  */ 01938000
*/********************************************************************/ 01939000
*                         IF XVD2WTOR='1'B THEN/* IS THIS A WTOR     */ 01940000
*                           DO;     /* YES                           */ 01941000
@RF00390 TM    XVD2,XVD2WTOR                                            01942000
         BZ    @RF00405                NO, BRANCH                       01943000
*                             CALL VWTOFORE;/* FREE THE ORE          */ 01944000
         BAL   R14,VWTOFORE                                             01945000
*                             /***************************************/ 01946000
*                             /*                                     */ 01947000
*                             /* FREE THE REPLY ID JUST PICKED. THE  */ 01948000
*                             /* INDEXS TO UCMRPYI AND IDMAP WERE    */ 01949000
*                             /* SAVED BY GETID IN REG1SAV           */ 01950000
*                             /* AND REG2SAV                         */ 01951000
*                             /***************************************/ 01952000
*                       UCMRPYI(REG1SAV)=UCMRPYI(REG1SAV)&(-1&&IDMAP(   01953000
*                             REG2SAV));                                01954000
         L     R14,REG1SAV                                              01955000
         L     R8,CVTCUCB                                               01956000
         L     R2,REG2SAV              RESTORE R2                       01957000
         SLR   R15,R15                                                  01958000
         IC    R15,IDMAP-1(R2)                                          01959000
         X     R15,KFM1                                                 01960000
         SLR   R2,R2                                                    01961000
         USING UCM,R8                                                   01962000
         IC    R2,UCMRPYI-1(R14)                                        01963000
         NR    R15,R2                                                   01964000
         STC   R15,UCMRPYI-1(R14)                                       01965000
*                             /***************************************/ 01966000
*                             /*                                     */ 01967000
*                             /* NOW BACK UP UCMRPYL SO WE WILL TRY  */ 01968000
*                             /* TO GET THE SAME REPLY               */ 01969000
*                             /*                                     */ 01970000
*                             /***************************************/ 01971000
*                             IF UCMRPYL=0 THEN/* ARE WE AT BEGINNING   01972000
*                                      OF IDS                        */ 01973000
         L     R15,UCMLSTP                                              01974000
         DROP  R8                                                       01975000
         USING UCMEIL,R15                                               01976000
         CLI   UCMRPYL,0                                                01977000
         BNE   @RF00409                                                 01978000
*                               UCMRPYL=NINTY9;/* YES. BACK UP TO       01979000
*                                      99TH ID                       */ 01980000
         MVI   UCMRPYL,99                                               01981000
         DROP  R15                                                      01982000
         B     @RC00409                                                 01983000
*                                                                       01984000
*                             ELSE  /* NO WE ARE NOT AT ID 0         */ 01985000
*                               UCMRPYL=UCMRPYL-1;/* BACK UP JUST ONE*/ 01986000
@RF00409 L     R15,CVTCUCB                                              01987000
         USING UCM,R15                                                  01988000
         L     R15,UCMLSTP                                              01989000
         DROP  R15                                                      01990000
         USING UCMEIL,R15                                               01991000
         SLR   R8,R8                                                    01992000
         IC    R8,UCMRPYL                                               01993000
         BCTR  R8,0                                                     01994000
         STC   R8,UCMRPYL                                               01995000
         DROP  R15                                                      01996000
*                             XVD0NORE='1'B;/* INDICATE START OVER IN   01997000
*                                      GETTING ORE                   */ 01998000
*                             XVD0NWQE='0'B;                            01999000
@RC00409 OI    XVD0,XVD0NORE                                            02000000
         NI    XVD0,255-XVD0NWQE                                        02001000
*                           END;                                        02002000
*/********************************************************************/ 02003000
*/*                                                                  */ 02004000
*/* NOW WAIT FOR A WQE TO BE FREE'D. THEN TRY AGAIN                  */ 02005000
*/*                                                                  */ 02006000
*/********************************************************************/ 02007000
*                         CALL VWTOWAIT(UCMWECBT);                      02008000
@RF00405 L     R15,CVTCUCB                                              02009000
         USING UCM,R15                                                  02010000
         LA    R15,UCMWECBT                                             02011000
         DROP  R15                                                      02012000
         ST    R15,PARMPTR                                              02013000
         LA    R1,PARMPTR                                               02014000
         BAL   R14,VWTOWAIT                                             02015000
*                       END;                                            02016000
*                 END;              /* OF DO WHILE LOOP              */ 02017000
@DE00344 TM    XVD1,XVD1ALDN                                            02018000
         BZ    @DL00344                                                 02019000
*/* **** END GETBLKS SEGMENT *****************************************/ 02020000
*                                                                       02021000
*/********************************************************************/ 02022000
*/*                                                                  */ 02023000
*/* NOW CHECK IF THE CONTROL BLOCKS WERE OBTAINED. IF SO THEN GO     */ 02024000
*/* AHEAD AND BUILD THEM. IF NOT THEN ABEND WITH A D23 ABEND CODE.   */ 02025000
*/*                                                                  */ 02026000
*/********************************************************************/ 02027000
*                 IF XVD1PERR='0'B THEN/* SUCCESSFUL IN GETTING THE     02028000
*                                         CONTROL BLOCKS (NO ERRORS)?*/ 02029000
         TM    XVD1,XVD1PERR                                            02030000
         BO    @RF00418                                                 02031000
*                   DO;             /* YES. BUILD THEM               */ 02032000
*/********************************************************************/ 02033000
*/*                                                                  */ 02034000
*/* CHECK FOR WTOR. IF THIS REQUEST IS A WTOR THEN BUILD AN ORE      */ 02035000
*/*                                                                  */ 02036000
*/********************************************************************/ 02037000
*                     IF XVD2WTOR='1'B THEN/* IS THIS REQUEST A WTOR */ 02038000
         TM    XVD2,XVD2WTOR                                            02039000
         BZ    @RF00420                       NO, BRANCH                02040000
*                       DO;                                             02041000
*/*** START OF BUILDORE **********************************************/ 02042000
*/********************************************************************/ 02043000
*/*                                                                  */ 02044000
*/*   BUILDORE - CONSTRUCT THE ORE AND PUT IT ON THE ORE CHAIN       */ 02045000
*/*                                                                  */ 02046000
*/*   INPUT - XVWPLADR POINTS AT THE USER'S PARM LIST                */ 02047000
*/*           XVOREAD  POINTS AT A ZEROED ORE                        */ 02048000
*/*                                                                  */ 02049000
*/*   OUTPUT - THE FILLED IN ORE IS ADDED TO THE ORE CHAIN           */ 02050000
*/*            AND IS MARKED SUSPENDED FOR THE DURATION OF THE       */ 02051000
*/*            SUBSYSTEM EXIT.                                       */ 02052000
*/*                                                                  */ 02053000
*/********************************************************************/ 02054000
*/********************************************************************/ 02055000
*/*                                                                  */ 02056000
*/*           THE OREID WILL HAVE ALREADY BEEN FILLED IN BY GETID    */ 02057000
*/*                                                                  */ 02058000
*/********************************************************************/ 02059000
*                                                                       02060000
*                         ORETCB=R4;/* INSERT TCB ADDR               */ 02061000
         L     R15,XVOREAD                                              02062000
         USING OREF,R15                                                 02063000
         ST    R4,ORETCB                                                02064000
*                                        /* MOVE IN REPLY LENGTH AND    02065000
*                                      BUFFER ADDR                   */ 02066000
         MVC   ORELNTH,WKARPYLN                                         02067000
         MVC   ORERPYA,WKARPBUF+1                                       02068000
*                                        /* MOVE IN THE REPLY ECB       02069000
*                                      ADDR                          */ 02070000
         MVC   OREECB,WKAECBP                                           02071000
*                         OREASID=ASCBASID;/* MOVE ASID OF THIS         02072000
*                                      MEMORY                        */ 02073000
         MVC   OREASID,ASCBASID                                         02074000
*                         OREWQE=XVWQEAD;/* MOVE IN ADDR OF             02075000
*                                      ASSOCIATED WQE                */ 02076000
         MVC   OREWQE,XVWQEAD                                           02077000
*                         OREBUFB='1'B;/* INDICATE THIS ORE IS IN USE*/ 02078000
*                         OREBUFD='1'B;/* INDICATE ORE OBTAINED VIA A   02079000
*                                      GETMAIN                       */ 02080000
         OI    OREXC,OREBUFB+OREBUFD                                    02081000
*/********************************************************************/ 02082000
*/*                                                                  */ 02083000
*/*  CHECK IF USER IS A PROBLEM PROGRAM. IF NOT THEN INDICATE THAT   */ 02084000
*/*  VALIDITY CHECKING OF REPLY BUFFER AND ECB CAN BE BYPASSED BY    */ 02085000
*/*  REPLY PROCESSING.                                               */ 02086000
*/*                                                                  */ 02087000
*/********************************************************************/ 02088000
*                         IF XVD1PP='0'B THEN/* IS USER NOT A PROBLEM   02089000
*                                      PROGRAM                       */ 02090000
         TM    XVD1,XVD1PP                                              02091000
         BNZ   @RF00429                                                 02092000
*                           OREKEY0='1'B;/* INDICATE BYPASS VALIDITY    02093000
*                                      CHECK                         */ 02094000
         OI    OREXA,OREKEY0                                            02095000
*                         ORESUSP='1'B;/* INDICATE ORE SUSPENDED FOR    02096000
*                                      SUBSYSTEM EXIT                */ 02097000
@RF00429 L     R15,XVOREAD                                              02098000
         OI    OREXA,ORESUSP           PROCESSING TEMPORARILY SUSPENDED 02099000
*/********************************************************************/ 02100000
*/*                                                                  */ 02101000
*/*  NOW PUT THE ORE ON THE FRONT OF THE QUEUE. THE ORDER OF         */ 02102000
*/*  APPEARANCE ON THE QUEUE IS IMMATERIAL. THE OPERATOR MAY RESPOND */ 02103000
*/*  IN ANY ORDER AND REPLY PROCESSING WILL SCAN THE QUEUE FOR MATCH-*/ 02104000
*/*  ING IDS.                                                        */ 02105000
*/*                                                                  */ 02106000
*/********************************************************************/ 02107000
*                         ORELKP=UCMRPYQ;/* POINT THIS ORE AT NEXT      02108000
*                                      ORE ON QUEUE                  */ 02109000
         L     R8,CVTCUCB                                               02110000
         USING UCM,R8                                                   02111000
         L     R2,UCMRPYQ                                               02112000
         ST    R2,ORELKP               LINKAGE POINTER                  02113000
         DROP  R15                                                      02114000
*                         UCMRPYQ=XVOREAD;/* POINT HEAD OF QUEUE AT     02115000
*                                      THIS ORE                      */ 02116000
         MVC   UCMRPYQ,XVOREAD                                          02117000
         DROP  R8                                                       02118000
*/********************************************************************/ 02119000
*/*                                                                  */ 02120000
*/*  WE ASSUME UCMRPYQ IS ZERO IF QUEUE IS EMPTY AT ENTRY TO VWTO    */ 02121000
*/*                                                                  */ 02122000
*/********************************************************************/ 02123000
*/****** END OF BUILD ORE SEGMENT ************************************/ 02124000
*                                                                       02125000
*                       END;                                            02126000
*/********************************************************************/ 02127000
*/*                                                                  */ 02128000
*/* NOW BUILD THE WQE                                                */ 02129000
*/*                                                                  */ 02130000
*/********************************************************************/ 02131000
*/*** START OF BUILDWQE **********************************************/ 02132000
*/********************************************************************/ 02133000
*/*                                                                  */ 02134000
*/*   BUILDWQE - CONSTRUCT THE WQE AND PUT IT ON THE WQE CHAIN       */ 02135000
*/*                                                                  */ 02136000
*/*   INPUT - WKAADTXT -> MSG TEXT + LEN +MCS FLAGS                  */ 02137000
*/*           WKALGH    = L'TEXT + 4                                 */ 02138000
*/*           XVWQEAD  POINTS AT A ZEROED WQE                        */ 02139000
*/*                                                                  */ 02140000
*/*   OUTPUT - THE FILLED IN WQE IS ADDED TO THE WQE CHAIN           */ 02141000
*/*                                                                  */ 02142000
*/*  START BY MOVING MESSAGE INTO THE WQE. THE FIRST CHARACTER OF    */ 02143000
*/*  TEXT IN THE WQE WILL BE THE AUTHORIZATION FLAG. THE MESSAGE IS  */ 02144000
*/*  SCANNED FOR 'NL' CHARACTER AND TO ELIMINATE TRAILING BLANKS.    */ 02145000
*/*  THE LENGTH OF THE MSG IS IN WKALGH AND IS NON-ZERO              */ 02146000
*/*  THE SECURITY MESSAGE FLAGGING IN THE MESSAGE TEXT IS:           */ 02147000
*/*   MSG TYPE           MSG FORMAT                                  */ 02148000
*/*   --------           ----------                                  */ 02149000
*/*   WTO,P/P,NONACTION  BLANK,+,TEXT                                */ 02150000
*/*   WTO,P/P,ACTION     @,TEXT                                      */ 02151000
*/*   WTO,SYS,NONACTION  BLANK,TEXT                                  */ 02152000
*/*   WTO,SYS,ACTION     *,TEXT                                      */ 02153000
*/*   WTOR,P/P           @,ID,BLANK,TEXT                             */ 02154000
*/*   WTOR,SYSTEM        *,ID,BLANK,TEXT                             */ 02155000
*/*                                                                  */ 02156000
*/*   AN ACTION MSG IS A WTOR OR A WTO WITH DESCRIPTOR CODE 1 OR 2.  */ 02157000
*/* NOTE: P/P IN THE ABOVE TABLE MEANS THAT THE USER IS NOT          */ 02158000
*/*       AUTHORIZED.                                                */ 02159000
*/*                                                                  */ 02160000
*/********************************************************************/ 02161000
*                     WQETXT='';    /* BLANK OUT TEXT PART OF THE WQE*/ 02162000
@RF00420 L     R15,XVWQEAD                                              02163000
         USING WQE,R15                                                  02164000
         MVI   WQETXT,C' '                                              02165000
         MVC   WQETXT+1(L'WQETXT-1),WQETXT                              02166000
*/********************************************************************/ 02167000
*/*                                                                  */ 02168000
*/*  CHECK FOR TYPE OF MESSAGE                                       */ 02169000
*/*                                                                  */ 02170000
*/********************************************************************/ 02171000
*                     IF XVD2WTOR='1'B THEN/*  WTOR ?                */ 02172000
         TM    XVD2,XVD2WTOR                                            02173000
         BZ    @RF00437                NO, BRANCH                       02174000
*                       DO;         /* YES, IS USER NOT AUTHORIZED?  */ 02175000
*                         IF XVD1AUTH='0'B THEN                         02176000
         TM    XVD1,XVD1AUTH                                            02177000
         BO    @RF00439                                                 02178000
*                           WQETXT='@';/* YES, MOVE IN P/P ACTION       02179000
*                                      FLAG                          */ 02180000
         MVI   WQETXT,C'@'                                              02181000
         B     @RC00439                                                 02182000
*                                                                       02183000
*                         ELSE      /* NO, MOVE IN SYSTEM ACTION FLAG*/ 02184000
*                           WQETXT='*';                                 02185000
@RF00439 MVI   WQETXT,C'*'                                              02186000
*                      WQETXT+1=OREID;/* MOVE IN REPLY ID FROM ORE   */ 02187000
@RC00439 L     R8,XVOREAD                                               02188000
         USING OREF,R8                                                  02189000
         MVC   WQETXT+1(2),OREID                                        02190000
         DROP  R8                                                       02191000
*                                                                       02192000
*                         MSGBLNK=' ';/* MOVE IN A BLANK AFTER REPLY    02193000
*                                      ID                            */ 02194000
         MVI   WQETXT+3,C' '                                            02195000
         DROP  R15                                                      02196000
*                         /*******************************************/ 02197000
*                         /*                                         */ 02198000
*                         /* POINT AT FIRST LOCATION OF MSG TEXT     */ 02199000
*                         /*                                         */ 02200000
*                         /*******************************************/ 02201000
         LA    R2,4                    4 BYTES OF WQETXT USED           02202000
         B     @RC00437                                                 02203000
*                                                                       02204000
*                       END;                                            02205000
*                     ELSE          /* NO, NOT A WTOR. CHECK FURTHER */ 02206000
*/********************************************************************/ 02207000
*/*                                                                  */ 02208000
*/* ACTION MSG BY REASON OF DESCRIPTOR CODES ?                       */ 02209000
*/* CHECK THE DESC CODES RETURNED BY THE INSTALLATION EXIT           */ 02210000
*/*                                                                  */ 02211000
*/********************************************************************/ 02212000
*                       DO;                                             02213000
*                         IF WPLDESCA='1'B /* DESC CODE 1 OR         */ 02214000
*                             WPLDESCB='1'B /* DESC CODE 2 OR           02215000
*                             WPLDESCK='1'B THEN/* DESC CODE 11?     */ 02216000
@RF00437 TM    WKADSC,WPLDESCA+WPLDESCB  DESC CODE 1 OR 2 ?             02217000
         BNZ   @RT00447                  YES, BRANCH                    02218000
         TM    WKADSC+1,WPLDESCK         DESC CODE 11 CRITICAL INFO ?   02219000
         BZ    @RF00447                  NO, BRANCH                     02220000
*                           IF XVD1AUTH='0'B THEN/* YES, CHECK FOR      02221000
*                                      NOT AUTHORIZED.               */ 02222000
@RT00447 TM    XVD1,XVD1AUTH                                            02223000
         BO    @RF00448                                                 02224000
*                             DO;   /* YES, USER IS NOT AUTHORIZED   */ 02225000
*                               WQETXT='@';/* MOVE IN P/P ACTION        02226000
*                                      FLAG                          */ 02227000
         L     R15,XVWQEAD                                              02228000
         USING WQE,R15                                                  02229000
         MVI   WQETXT,C'@'                                              02230000
         LA    R2,1                    1 BYTES OF WQETXT USED           02231000
         TM    WKADSC+1,WPLDESCK       CRITICAL EVENTUAL ACTION REQ ?   02232000
         BO    @RC00437                YES, BRANCH                      02233000
*                               WPLDESCG='1'B;/* TURN ON DESC 7 SO      02234000
*                                      THE P/P ACTION MSG WILL BE       02235000
*                                      DOMMED AT TASK TERMINATION    */ 02236000
         OI    WKADSC,WPLDESCG                                          02237000
         B     @RC00437                                                 02238000
*                                                                       02239000
*                             END;                                      02240000
*                           ELSE    /* NO, THIS IS A SYSTEM ACTION      02241000
*                                      MSG                           */ 02242000
*                             DO;                                       02243000
*                               WQETXT='*';/* MOVE IN SYSTEM ACTION     02244000
*                                      FLAG                          */ 02245000
@RF00448 MVI   WQETXT,C'*'                                              02246000
         LA    R2,1                    1 BYTES OF WQETXT USED           02247000
         B     @RC00437                                                 02248000
*                                                                       02249000
*                             END;                                      02250000
*                         ELSE      /* NO, THIS IS NOT AN ACTION        02251000
*                                      MESSAGE                       */ 02252000
*                           IF XVD1AUTH='0'B THEN/* IS THIS USER NOT    02253000
*                                      AUTHORIZED?                   */ 02254000
@RF00447 TM    XVD1,XVD1AUTH                                            02255000
         BNZ   @RF00458                                                 02256000
*                             DO;   /* YES                           */ 02257000
*                               PPMSGFLG=' +';/* MOVE IN P/P NO         02258000
*                                      ACTION MSG FLAG               */ 02259000
         MVC   WQETXT(L'KCPLUS),KCPLUS                                  02260000
         LA    R2,2                    2 BYTES OF WQETXT USED           02261000
         B     @RC00437                                                 02262000
*                                                                       02263000
*                             END;                                      02264000
*                           ELSE    /* NO, THIS IS A SYSTEM NO ACTION   02265000
*                                      MSG                           */ 02266000
*                             DO;                                       02267000
*                               MSGACTN=' ';/* MOVE IN SYSTEM NO        02268000
*                                      ACTION FLAG                   */ 02269000
@RF00458 MVI   WQETXT,C' '                                              02270000
         LA    R2,1                    1 BYTES OF WQETXT USED           02271000
*                             END;                                      02272000
*                       END;        /* OF WTO MSG FLAGGING SECTION   */ 02273000
*/********************************************************************/ 02274000
*/*                                                                  */ 02275000
*/*  R2 CONTAINS THE NUMBER OF BYTES OF THE WPLTXT THAT HAVE         */ 02276000
*/*  BEEN USED FOR THE MESSAGE FLAGS                                 */ 02277000
*/*                                                                  */ 02278000
*/********************************************************************/ 02279000
*                     IF XVD2WTOR='1'B THEN/* IF A WTOR, MAX TEXT       02280000
*                                      LENGTH IS 122                 */ 02281000
@RC00437 TM    XVD2,XVD2WTOR           WTOR ?                           02282000
         BZ    @RF00470                NO, BRANCH                       02283000
*                       TEXTLNGT=MIN((WKALGH-4),122);                   02284000
         LH    R8,WKALGH               R8 = L'MAJOR WQE TEXT +4         02285000
         S     R8,KF4                  R8 = L'MAJOR WQE TEXT            02286000
         LA    R1,122                  R1 = MAX L'WTOR TEXT             02287000
         CR    R8,R1                   L'WQE TEXT > MAXIMUM ?           02288000
         BNH   @RC00470                NO, LENGTH IS NOT > MAXIMUM      02289000
         LR    R8,R1                   YES, TRUNCATE WQE TEXT TO MAX    02290000
         B     @RC00470                                                 02291000
*                                                                       02292000
*                     ELSE          /* IF WTO, MAX TEXT LENGTH IS 125*/ 02293000
*                       TEXTLNGT=MIN((WKALGH-4),125);                   02294000
@RF00470 LH    R8,WKALGH               R8 = L'MAJOR WQE TEXT +4         02295000
         S     R8,KF4                  R8 = L'MAJOR WQE TEXT            02296000
         LA    R1,125                  R1 = MAX L'WTO TEXT              02297000
         CR    R8,R1                   L'WQE TEXT > MAXIMUM ?           02298000
         BNH   @RC00470                NO, LENGTH IS NOT > MAXIMUM      02299000
         LR    R8,R1                   YES, TRUNCATE WQE TEXT TO MAX    02300000
*/********************************************************************/ 02301000
*/*                                                                  */ 02302000
*/* MOVE MESSAGE TEXT INTO WQE                                       */ 02303000
*/*                                                                  */ 02304000
*/********************************************************************/ 02305000
*                     WQETXT(R2:TEXTLNGT+R2-1)=WPLTXT;                  02306000
@RC00470 LA    R1,WQETXT(R2)           R1 -> FIRST AVAIL BYTE IN WQETXT 02307000
         L     R14,WKAADTXT            R14 -> LEN FLD + MCS + TEXT      02308000
         LA    R14,4(,R14)             R14 -> TEXT                      02309000
         BCTR  R8,0                                                     02310000
         EX    R8,@SM03498             MOVE WPLTXT INTO WQETEXT         02311000
*                                      MVC 0(0,R1),0(R14)               02312000
*/********************************************************************/ 02313000
*/*                                                                  */ 02314000
*/*  SCAN BACKWARDS LOOKING FOR FIRST NON BLANK CHARACTER            */ 02315000
*/*                                                                  */ 02316000
*/********************************************************************/ 02317000
         LA    R1,WQETXT(R2)           R1 -> START OF MSG TEXT          02318000
         AR    R1,R8                   R1 -> LAST BYTE OF MESSAGE       02319000
         LA    R8,1(R2,R8)             INTERATION COUNTER               02320000
*                                                                       02321000
@DL00475 CLI   0(R1),C' '              BLANK CHAR ?                     02322000
         BNE   @RC00477                NO, EXIT LOOP TO STORE LENGTH    02323000
         BCTR  R1,0                    YES, DECR POINTER                02324000
         BCT   R8,@DL00475             CONTINUE LOOP                    02325000
*                                                                       02326000
*/********************************************************************/ 02327000
*/*                                                                  */ 02328000
*/*      MESSAGE WAS ALL BLANK                                       */ 02329000
*/*                                                                  */ 02330000
*/********************************************************************/ 02331000
         LA    R8,1                    TEXT LENGTH SET TO 1             02332000
*                     WQENBR=TEXTLNGT;/* STORE LENGTH OF MESSAGE     */ 02333000
*                     WQESEQN=UCMCMID;/* MOVE IN MSG SEQUENCE NUMBER */ 02334000
@RC00477 ST    R8,WQENBR               STORE LENGTH OF MESSAGE          02335000
         L     R1,CVTCUCB                                               02336000
         S     R1,KF4                                                   02337000
         L     R1,0(,R1)               R1 -> UCM PREFIX                 02338000
         USING UCMPRFX,R1                                               02339000
         MVC   WQESEQN(3),UCMCMID+1                                     02340000
         DROP  R1                                                       02341000
*/********************************************************************/ 02342000
*/* CHECK IF THIS IS AN OLD STYLE MESSAGE WHICH HAS NO MCS FLAGS     */ 02343000
*/*                                                                  */ 02344000
*/********************************************************************/ 02345000
*                     IF WKAMCSF=0 THEN/* MCS FLAGS IN THE WPL ?     */ 02346000
         ICM   R0,B'0011',WKAMCSF                                       02347000
         BNZ   @RF00482                YES, BRANCH                      02348000
*                       WQEMCSA='1'B;/* IND R/D CODES IN WQ          */ 02349000
         OI    WQEMCSF1,WQEMCSA                                         02350000
         B     @RC00482                                                 02351000
*                                                                       02352000
*                     /***********************************************/ 02353000
*                     /*                                             */ 02354000
*                     /* NOTE - THE MCS FIELD IN THE WPL IS SET TO   */ 02355000
*                     /* ALLOW TOTAL MESSAGE PROCESSING BY ALL       */ 02356000
*                     /* CONSOLES RECEIVING THE ORIGINAL MESSAGE     */ 02357000
*                     /*                                             */ 02358000
*                     /***********************************************/ 02359000
*                     ELSE                                              02360000
*                       WQEMCSF=WPLMCSF;/* MOVE IN THE MCS FLAGS     */ 02361000
@RF00482 MVC   WQEMCSF,WKAMCSF                                          02362000
*                     /***********************************************/ 02363000
*                     /*                                             */ 02364000
*                     /* CHECK IF THIS IS A WTP THAT IS TO GO TO     */ 02365000
*                     /* HARDCOPY ONLY. IGC0203E WOULD HAVE SET      */ 02366000
*                     /* XVD0HDCY ON IF THIS SITUATION EXISTS        */ 02367000
*                     /*                                             */ 02368000
*                     /***********************************************/ 02369000
*                     IF XVD0HDCY='1'B THEN/* DO WE SEND THIS MSG TO    02370000
*                                      HC ONLY                       */ 02371000
@RC00482 TM    XVD0,XVD0HDCY                  NO, BRANCH                02372000
         BZ    @RF00485                                                 02373000
*                       WQEMCSG='1'B;/* YES, TURN ON HC ONLY MCS FLAG*/ 02374000
         OI    WQEMCSF1,WQEMCSG                                         02375000
*                     IF WQEMCSB='1'B /* IS R0 OR QREG0 MCS FLAG ON*/   02376000
*                         WQEMCSH='1'B THEN                             02377000
@RF00485 TM    WQEMCSF1,WQEMCSB+WQEMCSH                                 02378000
         BZ    @RF00487                                                 02379000
*                       WQEUCMID=XVCONID;/* YES. MOVE IN CONSOLE ID  */ 02380000
         MVC   WQEUCMID,XVCONID                                         02381000
*                     XVWQEID=UCMCMID;/* SAVE ID FOR RETURN TO THE      02382000
*                                        USER                        */ 02383000
@RF00487 L     R8,CVTCUCB                                               02384000
         S     R8,KF4                                                   02385000
         L     R8,0(,R8)                 R8 -> UCM PREFIX               02386000
         USING UCMPRFX,R8                                               02387000
         L     R14,UCMCMID               R15 = UCM MESSAGE ID NUMBER    02388000
         ST    R14,XVWQEID                                              02389000
*                     UCMCMID=UCMCMID+1;/* INCREMENT MESSAGE ID NUM */  02390000
         LA    R14,1(,R14)                                              02391000
         ST    R14,UCMCMID                                              02392000
         DROP  R8                                                       02393000
*                     WQEROUT=WKAROC;/* MOVE IN ROUTE CODES          */ 02394000
         MVC   WQEROUT,WKAROC                                           02395000
*                     WQEDESCD=WKADSC;/* MOVE IN DESC CODES          */ 02396000
         MVC   WQEDESCD,WKADSC                                          02397000
*                       WQEMSGTP=WKAMSGTY;/* MOVE IN MSGTYP FLAGS    */ 02398000
         MVC   WQEMSGTP,WKAMSGTY                                        02399000
*                     WQEBUFB='1'B; /* INDICATE THE WQE IS IN USE    */ 02400000
*                     WQEBUFD='1'B; /* INDICATE THE QWE                 02401000
*                                      DYNAMICALLY GOTTEN            */ 02402000
         OI    WQEAVAIL,WQEBUFB+WQEBUFD                                 02403000
*/********************************************************************/ 02404000
*/*                                                                  */ 02405000
*/* IF WTOR FILL IN REPLY ID AND SET DESC CODES                      */ 02406000
*/*                                                                  */ 02407000
*/********************************************************************/ 02408000
*                     IF XVD2WTOR='1'B THEN                             02409000
         TM    XVD2,XVD2WTOR                                            02410000
         BZ    @RF00497                NO, BRANCH                       02411000
*                       DO;                                             02412000
*                         WQERPYID=OREID;/* MOVE IN REPLY ID         */ 02413000
         L     R1,XVOREAD                                               02414000
         USING OREF,R1                                                  02415000
         MVC   WQERPYID,OREID                                           02416000
         DROP  R1                                                       02417000
*                         WQEORE='1'B;/* TURN ON ORE EXISTS BIT      */ 02418000
*                         WQEWTOR='1'B;/* INDICATE THAT THIS WQE IS     02419000
*                                      FOR A WTOR                    */ 02420000
         OI    WQEXA,WQEORE+WQEWTOR                                     02421000
*                         WQEDESCD='0200'X;/* MOVE IN DESC CODE OF 7    02422000
*                                      ONLY FOR WTOR                 */ 02423000
         MVC   WQEDESCD,KX0200                                          02424000
*                       END;        /* OF WTOR HANDLING              */ 02425000
*/********************************************************************/ 02426000
*/*                                                                  */ 02427000
*/* CHECK IF USER IS P/P. IF SO DON'T ALLOW BYPASS OF HARD COPY      */ 02428000
*/*                                                                  */ 02429000
*/********************************************************************/ 02430000
*                     IF XVD1PP='1'B THEN/* CALLER A P/P             */ 02431000
@RF00497 TM    XVD1,XVD1PP                                              02432000
         BZ    @RF00504                 NO, BRANCH                      02433000
*                       WQEMCSN='0'B;/* YES, USER CAN'T BY PASS HC   */ 02434000
         NI    WQEMCSF2,255-WQEMCSN                                     02435000
*                     IF WQEMCSH='1'B&/* DID NOT AUTH USER SPECIFY      02436000
*                                      QREG0                         */ 02437000
*                         XVD1AUTH='0'B THEN                            02438000
@RF00504 TM    WQEMCSF1,WQEMCSH                                         02439000
         BZ    @RF00506                                                 02440000
         TM    XVD1,XVD1AUTH                                            02441000
         BO    @RF00506                                                 02442000
*                       DO;         /* YES. SWITCH USER TO R0           02443000
*                                      CONDITIONAL QUEUE TO THAT        02444000
*                                      CONSOLE                       */ 02445000
*                         WQEMCSH='0'B;/* TURN OFF QREG0 MCS FLAG    */ 02446000
*                         WQEMCSB='1'B;/* TURN ON R0 MCS FLAG        */ 02447000
         OI    WQEMCSF1,WQEMCSB                                         02448000
         NI    WQEMCSF1,255-WQEMCSH                                     02449000
*                       END;                                            02450000
*                     WQEASID=ASCBASID;/* MOVE IN ASID OF THIS MEMORY*/ 02451000
@RF00506 MVC   WQEASID,ASCBASID                                         02452000
*                     WQETCB=R4;    /* MOVE IN ADDR OF CURRENT TCB   */ 02453000
         ST    R4,WQETCB                                                02454000
*                     WQEJSTCB=TCBJSTCB;/* MOVE IN ADDR OF JOB STEPS    02455000
*                                      TCB                           */ 02456000
         MVC   WQEJSTCB,TCBJSTCB                                        02457000
*/********************************************************************/ 02458000
*/*                                                                  */ 02459000
*/* PUT IN USER AUTHORIZATION                                        */ 02460000
*/*                                                                  */ 02461000
*/********************************************************************/ 02462000
*                                                                       02463000
*                     IF XVD1AUTH='1'B THEN/* USER AUTHORIZED ?      */ 02464000
         TM    XVD1,XVD1AUTH                                            02465000
         BZ    @RF00514                                                 02466000
*                       WQEAUTH='1'B;/* YES, INDICATE SO             */ 02467000
         OI    WQEXA,WQEAUTH                                            02468000
         B     @RC00514                                                 02469000
*                                                                       02470000
*                     ELSE                                              02471000
*                       WQEAUTH='0'B;/* NO, MAKE SURE BIT IS OFF     */ 02472000
@RF00514 NI    WQEXA,255-WQEAUTH                                        02473000
*/********************************************************************/ 02474000
*/*                                                                  */ 02475000
*/*  NOW SET QUEUE FOR HC IF ALREADY INDICATED                       */ 02476000
*/*                                                                  */ 02477000
*/********************************************************************/ 02478000
*                     IF XVD2QFHC='1'B THEN/* IS QUEUE FOR HC           02479000
*                                      INDICATED                     */ 02480000
@RC00514 TM    XVD2,XVD2QFHC                                            02481000
         BZ    @RF00517                                                 02482000
*                       WQEQFHC='1'B;/* YES, SET SAVE BIT ON IN WQE  */ 02483000
         OI    WQEXA,WQEQFHC                                            02484000
         B     @RC00517                                                 02485000
*                                                                       02486000
*                     ELSE                                              02487000
*                       WQEQFHC='0'B;/* NO, INSURE BIT IS OFF        */ 02488000
@RF00517 NI    WQEXA,255-WQEQFHC                                        02489000
*/********************************************************************/ 02490000
*/*                                                                  */ 02491000
*/* CHECK IF TIME IS TO BE TAKEN                                     */ 02492000
*/*                                                                  */ 02493000
*/********************************************************************/ 02494000
*                     IF WPLMCSFI='1'B THEN/* DO NOT TIME STAMP ON ? */ 02495000
@RC00517 TM    WKAMCSF+1,WPLMCSFI                                       02496000
         BZ    @RF00520                                                 02497000
*                       WQETS='';   /* YES, FILL TIME STAMP WITH        02498000
*                                      BLANKS                        */ 02499000
         MVI   WQETS,C' '                                               02500000
         MVC   WQETS+1(L'WQETS-1),WQETS                                 02501000
         B     @RC00520                                                 02502000
*                                                                       02503000
*                     ELSE          /* NO, UNPACK THE TIME STAMP     */ 02504000
*                       DO;         /* FILL IN THE TIME FIELD        */ 02505000
@RF00520 MVC   WQEPAD(9),PATTIME       MOVE IN THE TIME EDIT PATTERN    02506000
         ED    WQEPAD(9),XVA8          FORMAT TIME HH.MM.SS INTO WQETS  02507000
*                       END;                                            02508000
*                     WQEPAD1=' ';  /* INSERT BLANK BETWEEN TS AND      02509000
*                                      JOBNAME                       */ 02510000
@RC00520 MVI   WQEPAD1,C' '                                             02511000
*                     WQEPAD2=' ';  /* INSERT BLANK AFTER THE JOBNAME*/ 02512000
         MVI   WQEPAD2,C' '                                             02513000
*                     WQEJOBNM=' '; /* BLANK OUT THE JOB NAME FIELD  */ 02514000
         MVI   WQEJOBNM,C' '                                            02515000
         MVC   WQEJOBNM+1(L'WQEJOBNM-1),WQEJOBNM                        02516000
*/********************************************************************/ 02517000
*/*                                                                  */ 02518000
*/* NOW DECODE THE ROUTE CODES                                       */ 02519000
*/*                                                                  */ 02520000
*/********************************************************************/ 02521000
*                     R3=WQEROUT; /* PICK UP UNDECODED ROUTE CODES */   02522000
         L     R3,WQEROUT                                               02523000
*                     DO R10=1 TO 4;/* CONVERT FOUR ROUTE               02524000
*                                      CHARACTERS                    */ 02525000
         LA    R8,1                                                     02526000
*                       R2=0;       /* PREPARE FOR SHIFT             */ 02527000
@DL00535 SLR   R2,R2                                                    02528000
*                                   /* MOVE IN FOUR BITS OF ROUTE       02529000
*                                      CODE                          */ 02530000
         SLDL  R2,4                                                     02531000
*                       R2=R2+1;/* MAKE INDEX TO EBCIDIC TABLE A        02532000
*                                      ONE ORIGIN INDEX              */ 02533000
         LA    R2,1(,R2)                                                02534000
*/********************************************************************/ 02535000
*/*                                                                  */ 02536000
*/* NOW USE THE FOUR BITS IN R2 TO PICK UP THE PROPER CHARACTER      */ 02537000
*/* FROM THE CHARACTER TABLE                                         */ 02538000
*/*                                                                  */ 02539000
*/********************************************************************/ 02540000
*                                                                       02541000
*                       WQERR(R10)=HEXCHAR(R2);                         02542000
         L     R1,XVWQEAD                                               02543000
         ALR   R1,R8                                                    02544000
         LA    R14,HEXCHAR-1(R2)                                        02545000
         MVC   WQERR-1-WQE(1,R1),0(R14)                                 02546000
*                     END;                                              02547000
         LA    R8,1(,R8)                                                02548000
         C     R8,KF4                                                   02549000
         BNH   @DL00535                                                 02550000
*                     WQEPAD=' ';   /* INSERT BLANK AFTER ROUTE      */ 02551000
         MVI   WQEPAD,C' '                                              02552000
*                     R3=CVTPTR;    /* RESET R3 TO POINT AT CVTMAP */   02553000
         L     R3,CVTPTR                                                02554000
*                     WQESUSP='1'B; /* SUSPEND OPERATION OF WQE FOR     02555000
*                                      SUBSYSTEM EXIT                */ 02556000
         OI    WQEXA,WQESUSP                                            02557000
*/********************************************************************/ 02558000
*/*                                                                  */ 02559000
*/*  THIS SECTION OF CODE TEST FOR BYPASS HARDCOPY REQUESTED BY      */ 02560000
*/*  THE ISSUER AND IF ON, SETS MASTER TRACED BIT ON IN ORDER TO     */ 02561000
*/*  PREVENT PROCESSING FOR MASTER TRACING BY IEAVMWSV               */ 02562000
*/*                                                                  */ 02563000
*/********************************************************************/ 02564000
*                     IF WQEMCSN='1'B THEN/* BYPASS HARDCOPY            02565000
*                                      REQUESTED                     */ 02566000
         TM    WQEMCSF2,WQEMCSN                                         02567000
         BZ    @RF00545                NO, BRANCH                       02568000
*                       WQEMTRCD='1'B;/* YES, SET MASTER TRACED BIT  */ 02569000
         OI    WQEAVAIL,WQEMTRCD                                        02570000
*/********************************************************************/ 02571000
*/*                                                                  */ 02572000
*/*  PUT WQE ON THE QUEUE. CHECK IF QUEUE IS EMPTY                   */ 02573000
*/*                                                                  */ 02574000
*/********************************************************************/ 02575000
*                     IF UCMWQEND=0 THEN/* IF QUEUE IS EMPTY SET UP     02576000
*                                      FOR INSERT                    */ 02577000
@RF00545 L     R8,CVTCUCB                                               02578000
         USING UCM,R8                                                   02579000
         ICM   R2,B'1111',UCMWQEND                                      02580000
         BNZ   @RF00547                                                 02581000
*                       UCMWQEND=ADDR(UCMWTOQ);/* PT AT HEAD OF CHAIN*/ 02582000
         LA    R2,UCMWTOQ                                               02583000
         ST    R2,UCMWQEND                                              02584000
*                     UCMWQEND->WQELKPA=XVWQEAD;/* PT LAST WQE AT NEW   02585000
*                                      WQE                           */ 02586000
@RF00547 L     R2,UCMWQEND                                              02587000
         STCM  R15,B'0111',WQELKPA-WQE(R2)                              02588000
*                     UCMWQEND=XVWQEAD;/* PT END OF CHAIN AT NEW WQE */ 02589000
         ST    R15,UCMWQEND                                             02590000
*                     WQELKP=0;     /* INDICATE LAST ON THE CHAIN    */ 02591000
         XC    WQELKP,WQELKP                                            02592000
*/*   CHECK IF WTO HAS BEEN MARKED FOR DELETION. IF SO MARK          */ 02593000
*/*   IT AS SERVICED AND SEND IT TO HARDCOPY                         */ 02594000
*                     IF XVD2DELW='1'B THEN/* IS WQE TO BE DELETED?  */ 02595000
         TM    XVD2,XVD2DELW                                            02596000
         BZ    @RF00552                NO, BRANCH                       02597000
*                       DO;         /* YES, DELETE THIS MESSAGE      */ 02598000
*                         WQEBUFE='1'B;/* MARK WQE AS SERVICED       */ 02599000
         OI    WQEAVAIL,WQEBUFE                                         02600000
*                         UCMSYSI='1'B;/* IND. CLEAN UP IS NEEDED    */ 02601000
         S     R8,KF4                                                   02602000
         L     R8,0(,R8)               R8 -> UCM PREFIX                 02603000
         DROP  R8                                                       02604000
         USING UCMPRFX,R8                                               02605000
         OI    UCMSFLG2,UCMSYSI                                         02606000
         AIF   (&TMVS805).U805G                                         02607000
*                         WQEQFHC='1'B;/* SEND MESSAGE TO HARDCOPY   */ 02608000
         OI    WQEXA,WQEQFHC                                            02609000
*                         WQEBUFC='1'B;/* IND. READY FOR HARDCOPY    */ 02610000
         OI    WQEAVAIL,WQEBUFC                                         02611000
         AGO   .U805H                                                   02612000
.U805G   NI    WQEXA,255-WQEQFHC       NO HARDCOPY                      02613000
         NOP   0                       WAS OI WQEAVAIL,WQEBUFC          02614000
.U805H   ANOP                                                           02615000
*                       END;        /* END IF DELETE IS SPECIFIED    */ 02616000
         DROP  R8,R15                                                   02617000
*/*** END OF BUILDWQE ************************************************/ 02618000
*/********************************************************************/ 02619000
*/*                                                                  */ 02620000
*/* FREE THE HELD LOCKS AND FRR PRIOR TO TAKING THE SUBSYSTEM EXIT   */ 02621000
*/*                                                                  */ 02622000
*/********************************************************************/ 02623000
*                     CALL FREELOCK;                                    02624000
@RF00552 BAL   R14,FREELOCK                                             02625000
*/********************************************************************/ 02626000
*/*                                                                  */ 02627000
*/* TAKE THE SUBSYSTEM EXIT                                          */ 02628000
*/*                                                                  */ 02629000
*/********************************************************************/ 02630000
*                     PARMFTPT=FTSSEXIT;/* START OF SUBSYSTEM EXIT      02631000
*                                      CODE                          */ 02632000
         MVI   PARMFTPT,X'08'                                           02633000
*                     PARMRTAD=ADDR(VWTOSSRT);/* IF ERROR TURN OFF      02634000
*                                      DELETE BIT                    */ 02635000
         LA    R2,VWTOSSRT                                              02636000
         ST    R2,PARMRTAD                                              02637000
*/********************************************************************/ 02638000
*/*                                                                  */ 02639000
*/*   HASPEXIT - EXIT TO THE JOB ENTRY SUBSYSTEM                     */ 02640000
*/*                                                                  */ 02641000
*/*  THE SPACE FOR THE SSOB AND SSOBWT IS OBTAINED FROM SUBPOOL 231  */ 02642000
*/*  WITH THE AUTOMATIC VARIABLES.                                   */ 02643000
*/*                                                                  */ 02644000
*/********************************************************************/ 02645000
*                                                                       02646000
*/********************************************************************/ 02647000
*/*                                                                  */ 02648000
*/*   SET UP THE SSOB                                                */ 02649000
*/*                                                                  */ 02650000
*/********************************************************************/ 02651000
*                                                                       02652000
*                     SSOBID='SSOB';/* FILL IN THE ID FIELD          */ 02653000
         MVC   SSOBID(L'KCSSOB),KCSSOB                                  02654000
*                     SSOBLEN=LENGTH(SSOB);/* AND THE LENGTH FIELD   */ 02655000
         MVC   SSOBLEN(2),KH20                                          02656000
*                     SSOBFUNC=SSOBWTO;/* SPECIFY WTO EXIT TO THE       02657000
*                                      SUBSYSTEM                     */ 02658000
         MVC   SSOBFUNC(2),KH9                                          02659000
*                     SSOBSSIB=0;   /* INDICATE SUBSYSTEM WHICH         02660000
*                                      INITIATED THE USER            */ 02661000
         SLR   R14,R14                                                  02662000
         ST    R14,SSOBSSIB                                             02663000
*                     SSOBINDV=ADDR(SSWT);/* POINT AT WTO INTERFACE     02664000
*                                      BLOCK                         */ 02665000
         LA    R14,SSWT                                                 02666000
         ST    R14,SSOBINDV                                             02667000
*/********************************************************************/ 02668000
*/*                                                                  */ 02669000
*/*   NOW FILL IN THE SSWT BLOCK                                     */ 02670000
*/*                                                                  */ 02671000
*/********************************************************************/ 02672000
*                     SSWTLEN=LENGTH(SSWT);/* PUT LENGTH OF SSWT INTO   02673000
*                                      THE SSWT                      */ 02674000
         MVC   SSWTLEN,KH16                                             02675000
*                     SSWTWQE=XVWQEAD;/* PUT IN THE ADDR OF THE WQE  */ 02676000
         MVC   SSWTWQE,XVWQEAD        /* INTO THE SSOB               */ 02677000
*/********************************************************************/ 02678000
*/*                                                                  */ 02679000
*/*   CHECK FOR A WTOR. IF SO THEN FILL IN ADDR OF THE ORE. IF NOT   */ 02680000
*/*   THEN SET THE SSWTORE FIELD TO ZERO.                            */ 02681000
*/*                                                                  */ 02682000
*/********************************************************************/ 02683000
*                     IF XVD2WTOR='1'B THEN/* WTOR REQUEST ?         */ 02684000
         TM    XVD2,XVD2WTOR                                            02685000
         BZ    @RF00570                    NO, BRANCH                   02686000
*                       SSWTORE=XVOREAD;/* YES, PUT IN THE ADDR OF      02687000
*                                      THE ORE                       */ 02688000
         MVC   SSWTORE,XVOREAD                                          02689000
         B     @RC00570                                                 02690000
*                                                                       02691000
*                     ELSE                                              02692000
*                       SSWTORE=0;  /* NO, SET FIELD TO ZERO         */ 02693000
@RF00570 XC    SSWTORE,SSWTORE                                          02694000
*                     SSWTMIN=0;    /* THERE IS NO MINOR FOR A SINGLE   02695000
*                                      WTO                           */ 02696000
@RC00570 XC    SSWTMIN,SSWTMIN                                          02697000
*/*                                                                  */ 02698000
*/*      IEFSSREQ  (SSOB);                                           */ 02699000
*/*                                                                  */ 02700000
*/*      PASS CONTROL TO JOB ENTRY SUBSYSTEM TO                      */ 02701000
*/*      PROCESS REQUEST -                                           */ 02702000
*/*                                                                  */ 02703000
*/*      INPUT - R1 -> ONE WORD PARAMETER LIST WHICH                 */ 02704000
*/*                    POINTS TO THE SSOB                            */ 02705000
*/*      CALL SSREQ(SSOB);              /* CALL ON THE SUBSYSTEM     */ 02706000
*/*                                                                  */ 02707000
         LA    R14,SSOB                                                 02708000
         ST    R14,PARMPTR                                              02709000
         MVI   PARMPTR,X'80'           SET END OF PARAMETER LIST        02710000
         L     R8,CVTJESCT                                              02711000
         USING JESCT,R8                                                 02712000
         L     R15,JESSSREQ            R15 -> IEFSSREQ RTN              02713000
         DROP  R8                                                       02714000
         LA    R1,PARMPTR                                               02715000
         BALR  R14,R15                 CALL IEFSSERQ                    02716000
         L     R8,XVWQEAD                                               02717000
         USING WQE,R8                                                   02718000
         TM    WQEMCSF2,WQEMCSN        BYPASS QUEUE TO HARD COPY ?      02719000
         BZ    IEA00D94                NO, BRANCH                       02720000
         NI    WQEXA,255-WQEQFHC       YES,TURN OFF QUEUE FOR HARD COPY 02721000
*/********************************************************************/ 02722000
*/*                                                                  */ 02723000
*/*   CHECK FOR A REQUEST TO DELETE MESSAGE BY SUBSYSTEM. CHECK FOR  */ 02724000
*/*   GOOD EXIT AND DELETION REQUEST                                 */ 02725000
*/*                                                                  */ 02726000
*/********************************************************************/ 02727000
*                     IF R15=SSRTOK&/* WAS EXIT SUCCESSFUL           */ 02728000
*                         SSOBRETN=SSWTNDSP/* AND IS DON'T DISPLAY      02729000
*                                      REQUESTED                     */ 02730000
*                       THEN        /* YES                           */ 02731000
IEA00D94 LTR   R15,R15                                                  02732000
         BNZ   @RF00575                                                 02733000
         LA    R14,4                                                    02734000
         C     R14,SSOBRETN                                             02735000
         BNE   @RF00575                                                 02736000
*                       DO;         /* DELETE MESSAGE                */ 02737000
*                         WQEBUFE='1'B;/* MARK WQE AS SERVICED       */ 02738000
         OI    WQEAVAIL,WQEBUFE                                         02739000
*                         UCMSYSI='1'B;/* IND. CLEAN UP IS NEEDED    */ 02740000
         L     R14,CVTCUCB                                              02741000
         S     R14,KF4                                                  02742000
         L     R14,0(,R14)             R14 -> UCM PREFIX                02743000
         USING UCMPRFX,R14                                              02744000
         OI    UCMSFLG2,UCMSYSI                                         02745000
         DROP  R14                                                      02746000
*                         /*******************************************/ 02747000
*                         /*                                         */ 02748000
*                         /* CHECK IF HARDCOPY IS TO BE BYPASSED     */ 02749000
*                         /*                                         */ 02750000
*                         /*******************************************/ 02751000
*                         IF WQEMCSN='0'B THEN/* BYPASS HARDCOPY SET?*/ 02752000
         TM    WQEMCSF2,WQEMCSN                                         02753000
         BO    @RF00579                                                 02754000
*                           DO;     /* NO,SEND MESSAGE TO HARDCOPY   */ 02755000
*                             WQEBUFC='1'B;/* IND. READY FOR HARDCOPY*/ 02756000
         OI    WQEAVAIL,WQEBUFC                                         02757000
*                             WQEQFHC='1'B;/* SEND MESSAGE TO           02758000
*                                      HARDCOPY                      */ 02759000
         OI    WQEXA,WQEQFHC                                            02760000
         B     @RF00575                                                 02761000
*                                                                       02762000
*                           END;    /* END IF BYPASS NOT SET         */ 02763000
*                         ELSE      /* IF BYPASS IS SET              */ 02764000
*                           DO;     /* DO NOT HARDCOPY MESSAGE       */ 02765000
*                             IF XVD2WTOR='1'B THEN/* THIS A WTOR?   */ 02766000
@RF00579 TM    XVD2,XVD2WTOR           NO, BRANCH                       02767000
         BZ    @RF00585                                                 02768000
*                               WQEBUFC='1'B;/* YES, SET READY FOR      02769000
*                                      HDCPY                         */ 02770000
         OI    WQEAVAIL,WQEBUFC                                         02771000
*                             WQEQFHC='0'B;/* TURN OFF HARDCOPY FLAG */ 02772000
@RF00585 NI    WQEXA,255-WQEQFHC                                        02773000
*                           END;    /* END IF BYPASS IS SET          */ 02774000
*                       END;        /* END IF DON'T DISPLAY SPEC     */ 02775000
*/********************************************************************/ 02776000
*/*                                                                  */ 02777000
*/* REMOVE ALL UNPRINTABLE AND NONDISPLAY CHARACTERS IN WQE          */ 02778000
*/*                                                                  */ 02779000
*/********************************************************************/ 02780000
*                                   /* TRANSLATE TEXT TO PRINTABLE   */ 02781000
*                                   /* AND DISPLAYABLE ONLY          */ 02782000
@RF00575 L     R1,WQENBR               R1 = L'WQETXT                    02783000
         BCTR  R1,0                                                     02784000
         EX    R1,TRINST               TR WQETXT(0),TRTAB               02785000
*                       END;                                            02786000
*                     /***********************************************/ 02787000
*                     /*                                             */ 02788000
*                     /* WE ARE DONE WITH THE SUBSYSTEM EXIT. REGAIN */ 02789000
*                     /* THE LOCKS AND TAKE THE BLOCKS OUT OF THE    */ 02790000
*                     /* SUSPENDED STATE                             */ 02791000
*                     /*                                             */ 02792000
*                     /***********************************************/ 02793000
*VWTOSSLB:                                                              02794000
*                     CALL SETLOCK;                                     02795000
VWTOSSLB BAL   R14,SETLOCK                                              02796000
*/********************************************************************/ 02797000
*/*                                                                  */ 02798000
*/* TAKE THE WQE AND ORE OUT OF THE SUSPENDED STATE                  */ 02799000
*/*                                                                  */ 02800000
*/********************************************************************/ 02801000
*                     PARMFTPT=FTEND;/* END OF THE MODULE            */ 02802000
         MVI   PARMFTPT,X'09'                                           02803000
*                     PARMRTAD=0;   /* NO RETRY NEEDED               */ 02804000
         XC    PARMRTAD,PARMRTAD                                        02805000
*                                                                       02806000
*                     WQESUSP='0'B; /* TURN OFF SUSPENDED BIT        */ 02807000
         NI    WQEXA,255-WQESUSP                                        02808000
         DROP  R8                                                       02809000
*                     IF XVD2WTOR='1'B THEN/* IS THERE AN ORE?       */ 02810000
         TM    XVD2,XVD2WTOR                                            02811000
         BZ    @RF00605                 NO, BRANCH                      02812000
*                       ORESUSP='0'B;/* YES, TURN OFF THE SUSPENDED     02813000
*                                      BIT                           */ 02814000
         L     R8,XVOREAD                                               02815000
         USING OREF,R8                                                  02816000
         NI    OREXA,255-ORESUSP                                        02817000
         DROP  R8                                                       02818000
*                     CALL FREELOCK;/* FREE ALL LOCKS                */ 02819000
@RF00605 BAL   R14,FREELOCK                                             02820000
*/********************************************************************/ 02821000
*/*                                                                  */ 02822000
*/*     POST THE COMMUNICATIONS TASK TO INDICATE THAT IT HAS WORK    */ 02823000
*/*     TO DO. NAMELY A WQE, AND POSSIBLY AN ORE, TO PROCESS         */ 02824000
*/*                                                                  */ 02825000
*/********************************************************************/ 02826000
*                     CALL POSTOECB;/* XMPOST UCMOECB                */ 02827000
         BAL   R14,POSTOECB                                             02828000
         B     VWTOERRT                                                 02829000
*                                                                       02830000
*                   END;            /* OF TEST FOR VALID CONTROL        02831000
*                                      BLOCKS                        */ 02832000
*                 ELSE              /* NO THE CONTROL BLOCKS WERE NOT   02833000
*                                      OBTAINED. FREE THE LOCKS.     */ 02834000
*                   CALL FREELOCK;                                      02835000
@RF00418 BAL   R14,FREELOCK                                             02836000
*               END;                /* OF RETURN TO USER FROM WTP       02837000
*                                      TEST                          */ 02838000
*           END;                    /* END OF ELSE CLAUSE OF ZERO MSG   02839000
*                                      LENGTH TEST                   */ 02840000
*     END;                          /* END OF VALID PARM LIST ELSE      02841000
*                                      CLAUSE                        */ 02842000
*/********************************************************************/ 02843000
*/*                                                                  */ 02844000
*/*      EXIT FROM IEAVVWTO                                          */ 02845000
*/*                                                                  */ 02846000
*/********************************************************************/ 02847000
*                                                                       02848000
*        ON ENTRY TO VWTOERRT NO LOCKS MUST BE HELD AS                  02849000
*        THE FREEMAIN SVC IS ISSUED TO FREE UP STORAGE                  02850000
*                                                                       02851000
*VWTOERRT:                                                              02852000
VWTOERRT ICM   R1,B'1111',LONGTXT      STORAGE GETMAINED FOR LONG WPL ? 02853000
         BZ    VWTOERRA                NO, BRANCH AROUND FREEMAIN       02854000
         L     R0,LONGLEN                                               02855000
*                                                                       02856000
         FREEMAIN RU,LV=(0),A=(1),SP=229                                02857000
*                                                                       02858000
VWTOERRA L     R0,@SIZDATD             GET SIZE OF DYNAMIC AREA         02859000
         LR    R1,R9                   R1 -> DYNAMIC AREA               02860000
*                                                                       02861000
         FREEMAIN R,LV=(0),A=(1)       FREE THE AREA                    02862000
*                                                                       02863000
*/********************************************************************/ 02864000
*/*                                                                  */ 02865000
*/*     FREE THE ESTAE SO THAT ABENDS WILL BE SEEN BY THE USER       */ 02866000
*/*                                                                  */ 02867000
*/********************************************************************/ 02868000
*                                                                       02869000
         ESTAE 0                                                        02870000
*                                                                       02871000
*/********************************************************************/ 02872000
*/*                                                                  */ 02873000
*/* CHECK FOR ABEND ERROR SITUATION                                  */ 02874000
*/*                                                                  */ 02875000
*/********************************************************************/ 02876000
*   IF XVD1PERR='1'B THEN           /* WAS ERROR BIT TURNED ON FOR      02877000
*                                      ABEND                         */ 02878000
         TM    XVD1,XVD1PERR                                            02879000
         BZ    @RF00618                NO, BRANCH                       02880000
*     /***************************************************************/ 02881000
*     /*                                                             */ 02882000
*     /* YES, ABEND THE USER WITH CODE D23 WITH REASON CODE IN R15   */ 02883000
*     /* THAT IDENTIFIES THE REASON FOR THE ABEND                    */ 02884000
*     /*                                                             */ 02885000
*     /***************************************************************/ 02886000
*     DO;                                                               02887000
*       R1=ABNDCODE;                                                    02888000
         L     R15,XVA4                LOAD REASON CODE INTO R15        02889000
         L     R1,ABNDCODE          /* LOAD R1 WITH D23 CODE         */ 02890000
*       /*************************************************************/ 02891000
*       /*                                                           */ 02892000
*       /* ISSUE SYSTEM ABEND WITH DUMP                              */ 02893000
*       /*                                                           */ 02894000
*       /*************************************************************/ 02895000
*                                                                       02896000
         ABEND (1),DUMP,,SYSTEM                                         02897000
*                                                                       02898000
*     END;                                                              02899000
*   ELSE                            /* NO, CHECK FOR VALID MESSAGE ID*/ 02900000
*     IF XVD2ZERO='1'B THEN         /* WAS MESSAGE NOT PROCESSED        02901000
*                                      BECAUSE OF ZERO LENGTH           02902000
*                                      MESSAGE?                      */ 02903000
@RF00618 TM    XVD2,XVD2ZERO                                            02904000
         BZ    @RC00618                                                 02905000
*       XVWQEID=0;                  /* YES, PASS BACK A ZERO MSG ID  */ 02906000
         XC    XVWQEID,XVWQEID                                          02907000
*/********************************************************************/ 02908000
*/*                                                                  */ 02909000
*/*      EPILOG CODE                                                 */ 02910000
*/*                                                                  */ 02911000
*/****************************************************************** */ 02912000
*                                                                       02913000
@RC00618 SLR   R15,R15                 CLEAR REGISTER FOR IC            02914000
         IC    R15,XVRETCOD            PICK UP RETURN CODE              02915000
         L     R1,XVWQEID              GET MSG ID TO RETURN             02916000
*   R14=XVRET;                      /* GET RETURN ADDRESS OF CALLER  */ 02917000
         L     R14,XVRET                                                02918000
         BR    R14                     EXIT FROM IEEVVWTO TO SVC RETURN 02919000
*                                                                       02920000
         DROP  R10                                                      02921000
*                                                                       02922000
*/********************************************************************/ 02923000
*/*                                                                  */ 02924000
*/*   VWTOLREC - LOG RECOVERY ROUTINE                                */ 02925000
*/*                                                                  */ 02926000
*/*   FUNCTION - THIS ROUTINE IS ENTERED WHEN WE CAN NOT GET A CELL  */ 02927000
*/*   POOL EXTENSION FROM CSA, SUBPOOL 231. CHECK TO SEE IF THE      */ 02928000
*/*   REASON FOR THE GETMAIN FAILURE IS DUE TO THE SYSTEM LOG NOT    */ 02929000
*/*   BEING INITIALIZED YET AND CSA HAS BEEN FILLED WITH MESSAGES    */ 02930000
*/*   WAITING TO GO OUT TO THE LOG.  IF THAT IS THE CASE THEN WE     */ 02931000
*/*   RECOVER FROM THE SITUATION BY SWITCHING HARDCOPY TO A CONSOLE  */ 02932000
*/*   AND COMMTASK WILL FLUSH THE MESSAGES. WE WAIT UNTIL THE WQE    */ 02933000
*/*   LIMIT IS DOWN AND THEN PROCEED.  IF THE CAUSE ISN'T DUE TO     */ 02934000
*/*   LOG PROBLEMS THEN WE CAUSE THE CALLER TO BE ABENDED.           */ 02935000
*/*                                                                  */ 02936000
*/*   INPUT - THE INPUT PARM IS THE PTR IN XVSAV THAT WILL HOLD THE  */ 02937000
*/*            CONTROL BLOCKS ADDR. EITHER XVOREAD OR XVWQEAD. THIS  */ 02938000
*/*            FIELD IS ZERO ON INPUT.                               */ 02939000
*/*            UCMWQLM AND UCMRQLM ARE THE LIMITS ON THE NUMBER OF   */ 02940000
*/*            WQES AND ORES THAT WERE SET BY SYSGEN                 */ 02941000
*/*            UCMWQLM1 AND UCMRQLM1 WERE THE LIMITS SET AT IPL      */ 02942000
*/*            UCMFMSGN IS A FLAG WHICH WHEN ON TELLS                */ 02943000
*/*            IEAVMQWR NOT TO ISSUE ANY WQE THRESHOLD MSGS.         */ 02944000
*/*                                                                  */ 02945000
*/*   OUTPUT - BLKADDR IS RETURNED AS ZERO OR ONE. ZERO MEANS THAT   */ 02946000
*/*            THE ERROR WAS RECOVERED, TRY TO GET ANOTHER CELL.     */ 02947000
*/*            ONE MEANS THAT THE ERROR COULD NOT BE RECOVERED,      */ 02948000
*/*            SO ABEND THE CALLER.                                  */ 02949000
*/*            FLAG UCMFMSGN WILL BE ON                              */ 02950000
*/*                                                                  */ 02951000
*/********************************************************************/ 02952000
*VWTOLREC:                                                              02953000
VWTOLREC STM   R14,R12,@SA00002                                         02954000
         MVC   @PC00002(4),0(R1)                                        02955000
*                                      VWTOGETB WHEN PROBLEM OCCURED */ 02956000
*   /*****************************************************************/ 02957000
*   /*                                                               */ 02958000
*   /* SET FLAG TO INFORM IEAVMQWR NOT TO ISSUE ANY MESSAGES ABOUT   */ 02959000
*   /* WQE USE                                                       */ 02960000
*   /*                                                               */ 02961000
*   /*****************************************************************/ 02962000
*   UCMFMSGN='1'B;                  /* FLAG IS IN UCM FIXED EXTENSION*/ 02963000
         L     R10,CVTCUCB                                              02964000
         USING UCM,R10                                                  02965000
         L     R8,UCMBFEXT             R8 -> UCM FIXED EXTENSION BASE   02966000
         USING UCMFEXTA,R8                                              02967000
         OI    UCMFFLG1,UCMFMSGN       NO WQE THRESHOLD MSG             02968000
         DROP  R8                                                       02969000
*   /*****************************************************************/ 02970000
*   /*                                                               */ 02971000
*   /* CHECK IF ERROR DUE TO HC GOING TO UNINITIALIZED SYSLOG        */ 02972000
*   /*                                                               */ 02973000
*   /*****************************************************************/ 02974000
*   IF UCMSYSG='1'B &               /* SYSLOG THE HARDCOPY DEVICE    */ 02975000
*       BALOGINT='0'B THEN          /* AND LOG NOT INITIALIZED YET ? */ 02976000
         LR    R8,R10                                                   02977000
         S     R8,KF4                                                   02978000
         L     R8,0(,R8)               R8 -> UCM PREFIX                 02979000
         USING UCMPRFX,R8                                               02980000
         TM    UCMSFLG1,UCMSYSG        HARD COPY DEVICE IS SYSLOG ?     02981000
         BZ    @RF00634                NO, BRANCH                       02982000
         L     R2,CVTMSER                                               02983000
         USING BASE,R2                 MASTER SCHED RESIDENT DATA AREA  02984000
         TM    BALOG,BALOGINT          LOG AREA INITIALIZED ?           02985000
         BNZ   @RF00634                                                 02986000
         DROP  R2                                                       02987000
*     DO;                                                               02988000
*       /*************************************************************/ 02989000
*       /*                                                           */ 02990000
*       /* SWITCH THE LIMITS FOR NUMBER OF ORES AND WQES ALLOWED IN  */ 02991000
*       /* THE SYSTEM AT ONE TIME                                    */ 02992000
*       /*                                                           */ 02993000
*       /*************************************************************/ 02994000
*       UCMWQLM=UCMWQLM1;           /* SET SYSGENED LIMIT TO IPL        02995000
*                                      LIMIT                         */ 02996000
         LH    R2,UCMWQLM1                                              02997000
         STH   R2,UCMWQLM                                               02998000
*       UCMRQLM=UCMRQLM1;           /* FOR WQES AND ORES             */ 02999000
         IC    R2,UCMRQLM1                                              03000000
         STC   R2,UCMRQLM                                               03001000
*       /*************************************************************/ 03002000
*       /*                                                           */ 03003000
*       /* POST COMMTASK TO SWITCH HARDCOPY                          */ 03004000
*       /*                                                           */ 03005000
*       /*************************************************************/ 03006000
*       UCMSYSO='1'B;               /* THIS IS A DUMMY ATTN INTERUPT    03007000
*                                      BY SVC 35                     */ 03008000
         OI    UCMSFLG2,UCMSYSO                                         03009000
*       CALL FREELOCK;              /* FREE THE FRR AND LOCKS        */ 03010000
         BAL   R14,FREELOCK                                             03011000
*       REG1SAV=ADDR(UCMAECB);      /* POST ATTN ECB                 */ 03012000
         L     R10,CVTCUCB                                              03013000
         LA    R8,UCMAECB                                               03014000
         ST    R8,REG1SAV                                               03015000
*       XVA4=UCMASCB;               /* COMMTASK'S ASCB ADDR          */ 03016000
         L     R8,UCMASCB                                               03017000
         ST    R8,XVA4                                                  03018000
*       XVA8=POSTRECY;              /* ADDR OF IEAVMEST              */ 03019000
         L     R10,UCMWAKUP                                             03020000
         DROP  R10                                                      03021000
         L     R10,0(,R10)                                              03022000
         ST    R10,XVA8                                                 03023000
*       R1=ADDR(REG1SAV);           /* LOAD PTR TO 3 WORD PARM LIST  */ 03024000
         LA    R1,REG1SAV                                               03025000
*                                   /* POST WITH CODE OF 35 FOR WTO  */ 03026000
         POST  ,35,MF=(E,(1))                                           03027000
*                                                                       03028000
*       CALL SETLOCK;               /* REGAIN THE LOCKS              */ 03029000
         BAL   R14,SETLOCK                                              03030000
*       IF XVD1PRIV='0'B THEN       /* MSG WASN'T ISSUED BY THE         03031000
*                                      COMMTASK OR AN SIRB. THIS        03032000
*                                      CHECK IS SO WE DON'T CAUSE AN    03033000
*                                      SIRB OR COMMTASK TO WAIT         03034000
*                                      INSTEAD THEY ARE ABENDED WITH    03035000
*                                      A D23 ABEND CODE              */ 03036000
         TM    XVD1,XVD1PRIV                                            03037000
         BNZ   @RF00648                                                 03038000
*         CALL VWTOWAIT(UCMWECBT);  /* WAIT FOR THE WQE LIMIT TO BE     03039000
*                                      REDUCED BELOW THE NEW LIMITS  */ 03040000
         L     R10,CVTCUCB                                              03041000
         USING UCM,R10                                                  03042000
         LA    R10,UCMWECBT                                             03043000
         DROP  R10                                                      03044000
         ST    R10,PARMPTR                                              03045000
         LA    R1,PARMPTR                                               03046000
         BAL   R14,VWTOWAIT                                             03047000
         B     @ER00002                                                 03048000
*                                                                       03049000
*       ELSE                        /* MSG ISSUER WAS COMMTASK OR       03050000
*                                      SIRB                          */ 03051000
*         BLKADDR=1;                /* SET PARAMETER ADDR TO ERROR      03052000
*                                      COND SO ISSUER WILL BE ABENDED*/ 03053000
@RF00648 L     R10,@PC00002                                             03054000
         MVC   0(4,R10),KF1                                             03055000
         B     @ER00002                                                 03056000
*                                                                       03057000
*     END;                                                              03058000
*   ELSE                            /* ERROR NOT DUE TO SYSLOG. SET     03059000
*                                      ERROR RETURN INDICATOR.       */ 03060000
*     BLKADDR=1;                    /* SET PARAMETER ADDR TO ERROR      03061000
*                                      COND                          */ 03062000
@RF00634 L     R10,@PC00002                                             03063000
         MVC   0(4,R10),KF1                                             03064000
*   RETURN;                         /* GO BACK TO CALLER             */ 03065000
@ER00002 LM    R14,R12,@SA00002                                         03066000
         BR    R14                                                      03067000
*   END;                                                                03068000
         B     @ER00002                                                 03069000
*                                                                       03070000
*/********************************************************************/ 03071000
*/*                                                                  */ 03072000
*/*  VWTOCLNP - VWTO'S CLEAN UP ROUTINE                              */ 03073000
*/*                                                                  */ 03074000
*/*   FUNCTION - THIS ROUTINE WILL BE CALLED IF THERE IS AN ERROR    */ 03075000
*/*   IN VWTO WHILE IT WAS LOCKED AND BUILDING THE ORE OR WQE. THIS  */ 03076000
*/*   ROUTINE WILL ATTEMP TO FREE THE ORE AND WQE. IT WILL THEN      */ 03077000
*/*   RETURN TO IEAVMFRR WHICH WILL LET THE ERROR CONTINUE.          */ 03078000
*/*                                                                  */ 03079000
*/*  INPUT - R2 -> THE USERS FRR PARMLIST                            */ 03080000
*/*          PARMRGAD WILL POINT TO THIS MODULES REG FRR SAVEAREA    */ 03081000
*/*          R13 -> IEAVMFRR'S SAVEAREA                              */ 03082000
*/*          R14 AND R15 WILL BE THE RETURN AND ENTRY REGS           */ 03083000
*/*          XVSAV WILL CONTAIN INFORMATION ON LOCATION OF ORE/WQE   */ 03084000
*/*                                                                  */ 03085000
*/*    OUTPUT - IEAVMFRR'S REGS WILL BE RESTORED                     */ 03086000
*/*             THE ORE/WQE, IF ANY WILL BE FREED                    */ 03087000
*/*                                                                  */ 03088000
*/********************************************************************/ 03089000
*VWTOCLNP:                                                              03090000
         DROP  R11,R12                                                  03091000
*                           /* SET UP THIS ENTRY POINT AND ALL REGS */  03092000
         USING *,R15           TEMPORARY BASE REGISTER                  03093000
VWTOCLNP SAVE  (14,12),,'IEAVVWTO CLEANUP'                              03094000
         USING PARMLIST,R2                                              03095000
         L     R1,PARMRGAD             PICK UP ADDR OF OUR SAVE AREA    03096000
         DROP  R2                                                       03097000
         LM    R3,R12,14(R1)           RESTORE OUR REGISTERS            03098000
         DROP  R15                     MODULE BASE REGS NOW AVAILABLE   03099000
         USING IEAVVWTO,R11,R12        RESTORE ADDRESSABILITY           03100000
*   /*****************************************************************/ 03101000
*   /*                                                               */ 03102000
*   /* NOW SAVE THE CALLER'S SAVE AREA ADDRESS                       */ 03103000
*   /*                                                               */ 03104000
*   /*****************************************************************/ 03105000
*   XVA8=R13;                       /* USE THE XVSAV WORK AREA       */ 03106000
         ST    R13,XVA8                OVERLAY SAVED TIME               03107000
*   R13=R9;                         /* LOAD OUR OWN SAVEAREA ADDRESS */ 03108000
         LR    R13,R9                                                   03109000
*   /*****************************************************************/ 03110000
*   /*                                                               */ 03111000
*   /* NOW CHECK IF ORE WAS OBTAINED. IF SO THEN CHECK IF IS WAS ON  */ 03112000
*   /* THE ORE QUEUE                                                 */ 03113000
*   /*                                                               */ 03114000
*   /*****************************************************************/ 03115000
*   IF XVD1ALDN='1'B&               /* WERE WE THRU GETTING BLOCKS   */ 03116000
*       XVOREAD>1 THEN              /* AND VALID ADDRESS IN THE         03117000
*                                      OREADDR FLD                   */ 03118000
         TM    XVD1,XVD1ALDN                                            03119000
         BZ    @RF00659                                                 03120000
         CLC   XVOREAD,KF1                                              03121000
         BNH   @RF00659                                                 03122000
*     DO;                           /* YES. CHECK IF ORE WAS ON THE     03123000
*                                      CHAIN                         */ 03124000
*       DO R1=ADDR(UCMRPYQ) BY 0 WHILE(R1^=0);                          03125000
         L     R1,CVTCUCB                                               03126000
         USING UCM,R1                                                   03127000
         LA    R1,UCMRPYQ                                               03128000
         DROP  R1                                                       03129000
         B     @DE00661                                                 03130000
*                                                                       03131000
*         IF R1->ORELKP=XVOREAD THEN/* DOES THIS LINK POINTER POINT     03132000
*                                      TO OUR ORE                    */ 03133000
@DL00661 L     R10,XVOREAD                                              03134000
         C     R10,ORELKP-OREF(,R1)                                     03135000
         BNE   @RF00662                                                 03136000
*           DO;                     /* YES. DECHAIN OUR ORE          */ 03137000
*             R1->ORELKP=XVOREAD->ORELKP;/* PUT PTR TO NEXT ORE IN      03138000
*                                      PREVIOUS ORE LINK FIELD       */ 03139000
         L     R10,ORELKP-OREF(,R10)                                    03140000
         ST    R10,ORELKP-OREF(,R1)                                     03141000
*             R1=0;                 /* GET OUT OF LOOP               */ 03142000
         SLR   R1,R1                                                    03143000
         B     @DE00661                                                 03144000
*                                                                       03145000
*           END;                                                        03146000
*         ELSE                      /* NO, NOT OURS. PT TO NEXT ORE  */ 03147000
*           R1=R1->ORELKP;                                              03148000
@RF00662 L     R1,ORELKP-OREF(,R1)                                      03149000
*       END;                        /* THE ORE IS NOW OFF THE ORE       03150000
*                                      CHAIN                         */ 03151000
@DE00661 LTR   R1,R1                                                    03152000
         BNZ   @DL00661                                                 03153000
*       /*************************************************************/ 03154000
*       /*                                                           */ 03155000
*       /* FREE THE ORE                                              */ 03156000
*       /*                                                           */ 03157000
*       /*************************************************************/ 03158000
*       R0=UCMORECP;                /* CELL POOL ID FOR IRES         */ 03159000
         L     R10,CVTCUCB                                              03160000
         USING UCM,R10                                                  03161000
         L     R0,UCMORECP                                              03162000
         DROP  R10                                                      03163000
*       R1=XVOREAD;                 /* ADDR OF THE ORE               */ 03164000
         L     R1,XVOREAD                                               03165000
*                                                  /* FREE ORE CELL  */ 03166000
         FREECELL CPID=(0),CELL=(1),BRANCH=YES                          03167000
*                                                                       03168000
*/********************************************************************/ 03169000
*/*                                                                  */ 03170000
*/* THIS ORE CELL MAY HAVE BEEN THE LAST ONE IN AN EXTENSION.        */ 03171000
*/* IF SO THEN R15 IS SET TO 20 BY FREECELL. IF THIS CONDITION       */ 03172000
*/* IS FOUND THEN FREE THE EXTENSION. R0 AND R1 ARE                  */ 03173000
*/* ARE SET BY FREECELL.                                             */ 03174000
*/*                                                                  */ 03175000
*/********************************************************************/ 03176000
*       IF R15=20 THEN              /* WAS CELL THE LAST IN AN          03177000
*                                      EXTENSION                     */ 03178000
         C     R15,KF20                                                 03179000
         BNE   @RF00659                                                 03180000
*         /***********************************************************/ 03181000
*         /*                                                         */ 03182000
*         /* YES, FREE THE EXTENSION                                 */ 03183000
*         /*                                                         */ 03184000
*         /***********************************************************/ 03185000
*                                                                       03186000
         FREEMAIN R,LV=(0),A=(1),BRANCH=YES                             03187000
*                                                                       03188000
*     END;                                                              03189000
*   /*****************************************************************/ 03190000
*   /*                                                               */ 03191000
*   /* NOW CHECK IF WQE WAS OBTAINED                                 */ 03192000
*   /*                                                               */ 03193000
*   /*****************************************************************/ 03194000
*   IF XVD1ALDN='1'B&               /* WERE WE THRU GETTING BLOCKS   */ 03195000
*       XVWQEAD>1 THEN              /* AND VALID ADDRESS IN THE         03196000
*                                      WQEADDR FLD                   */ 03197000
@RF00659 TM    XVD1,XVD1ALDN                                            03198000
         BZ    @RF00677                                                 03199000
         CLC   XVWQEAD,KF1                                              03200000
         BNH   @RF00677                                                 03201000
*     DO;                           /* YES. CHECK IF WQE WAS ON THE     03202000
*                                      CHAIN                         */ 03203000
*       DO R1=ADDR(UCMWTOQ) BY 0 WHILE(R1^=0);                          03204000
         L     R1,CVTCUCB                                               03205000
         USING UCM,R1                                                   03206000
         LA    R1,UCMWTOQ                                               03207000
         DROP  R1                                                       03208000
         B     @DE00679                                                 03209000
*                                                                       03210000
*         IF R1->WQELKPA=XVWQEAD THEN/* DOES THIS LINK POINTER          03211000
*                                      POINT TO OUR WQE              */ 03212000
@DL00679 L     R10,XVWQEAD                                              03213000
         L     R8,WQELKPA-1-WQE(,R1)                                    03214000
         LA    R8,0(,R8)                                                03215000
         CR    R10,R8                                                   03216000
         BNE   @RF00680                                                 03217000
*           DO;                     /* YES. DECHAIN OUR WQE          */ 03218000
*             R1->WQELKPA=XVWQEAD->WQELKPA;/* PUT PTR TO NEXT WQE       03219000
*                                      IN PREVIOUS WQE LINK FIELD    */ 03220000
         MVC   WQELKPA-WQE(3,R1),WQELKPA-WQE(R10)                       03221000
*             IF UCMWQEND=XVWQEAD THEN/* WAS THIS WQE THE LAST ON THE   03222000
*                                      CHAIN?                        */ 03223000
         L     R8,CVTCUCB                                               03224000
         USING UCM,R8                                                   03225000
         C     R10,UCMWQEND                                             03226000
         BNE   @RF00683                                                 03227000
*               UCMWQEND=R1;        /* YES. UPDATE PTR IN UCM TO        03228000
*                                      POINT TO NEW END OF WQE CHAIN */ 03229000
         ST    R1,UCMWQEND                                              03230000
         DROP  R8                                                       03231000
*             R1=0;                 /* GET OUT OF LOOP               */ 03232000
@RF00683 SLR   R1,R1                                                    03233000
         B     @DE00679                                                 03234000
*                                                                       03235000
*           END;                                                        03236000
*         ELSE                      /* NO, NOT OURS. PT TO NEXT WQE  */ 03237000
*           R1=R1->WQELKPA;                                             03238000
@RF00680 L     R10,WQELKPA-1-WQE(,R1)                                   03239000
         LA    R10,0(,R10)                                              03240000
         LR    R1,R10                                                   03241000
*       END;                        /* THE WQE IS NOW OFF THE WQE       03242000
*                                      CHAIN                         */ 03243000
@DE00679 LTR   R1,R1                                                    03244000
         BNZ   @DL00679                                                 03245000
*       /*************************************************************/ 03246000
*       /*                                                           */ 03247000
*       /* FREE THE WQE                                              */ 03248000
*       /*                                                           */ 03249000
*       /*************************************************************/ 03250000
*       R0=UCMWQECP;                /* CELL POOL ID FOR WQES         */ 03251000
         L     R10,CVTCUCB                                              03252000
         USING UCM,R10                                                  03253000
         L     R0,UCMWQECP                                              03254000
         DROP  R10                                                      03255000
*       R1=XVWQEAD;                 /* ADDR OF THE WQE               */ 03256000
         L     R1,XVWQEAD                                               03257000
*                                                  /* FREE WQE CELL  */ 03258000
         FREECELL CPID=(0),CELL=(1),BRANCH=YES                          03259000
*                                                                       03260000
*/********************************************************************/ 03261000
*/*                                                                  */ 03262000
*/* THIS WQE CELL MAY HAVE BEEN THE LAST ONE IN AN EXTENSION.        */ 03263000
*/* IF SO THEN R15 IS SET TO 20 BY FREECELL. IF THIS CONDITION       */ 03264000
*/* IS FOUND THEN FREE THE EXTENSION. R0 AND R1 ARE                  */ 03265000
*/* SET BY FREECELL                                                  */ 03266000
*/*                                                                  */ 03267000
*/********************************************************************/ 03268000
*       IF R15=20 THEN              /* WAS CELL THE LAST IN AN          03269000
*                                      EXTENSION                     */ 03270000
         C     R15,KF20                                                 03271000
         BNE   @RF00677                                                 03272000
*         /***********************************************************/ 03273000
*         /*                                                         */ 03274000
*         /* YES, FREE THE EXTENSION                                 */ 03275000
*         /*                                                         */ 03276000
*         /***********************************************************/ 03277000
*                                                                       03278000
         FREEMAIN R,LV=(0),A=(1),BRANCH=YES                             03279000
*                                                                       03280000
*     END;                                                              03281000
*   R13=XVA8;                       /* RESTORE CALLER'S SAVE AREA       03282000
*                                      ADDR                          */ 03283000
@RF00677 L     R13,XVA8                                                 03284000
         RETURN (14,12)             /* GO BACK TO IEAVMFRR ROUTINE*/    03285000
*                                                                       03286000
*/********************************************************************/ 03287000
*/*                                                                  */ 03288000
*/*      VWTOVALR                                                    */ 03289000
*/*                                                                  */ 03290000
*/*      THIS ROUTINE GETS CONTROL VIA THE FRR OR ESTAE PARMLIST     */ 03291000
*/*      ENTRY PARMRTAD IF AN ERROR IS DETECTED WHILE VALIDATING     */ 03292000
*/*      THE WPL.                                                    */ 03293000
*/*      REGISTERS ARE RESTORED BY SETRP REQUEST IN IEAVMFRR         */ 03294000
*/*                                                                  */ 03295000
*/********************************************************************/ 03296000
*VWTOVALR:                                                              03297000
         USING PARMLIST,R10                                             03298000
VWTOVALR MVI   XVREASON,D23PARM        PARMLIST NOT ADDRESSABLE BY USER 03299000
*                                                                       03300000
*        ENTRY POINT FOR VALIDATION ERROR DETECTED BY THE CODE          03301000
*        REASON CODE ALREADY SET                                        03302000
*                                                                       03303000
VWTOVALB MVI   XVFNCT,D23VALID         PARMLIST VALIDITY CHECK          03304000
         MVI   XVAMOD,VWTOID           IEAVVWTO'S ID FOR D23            03305000
*   XVD2VALD='0'B;                  /* WPL IS NOT VALID              */ 03306000
         NI    XVD2,255-XVD2VALD                                        03307000
         OI    XVD1,XVD1PERR           SET ERROR FLAG                   03308000
*   PARMRTAD=0;                     /* CLEAR RETRY ADDR TO AVOID        03309000
*                                      LOOPING                       */ 03310000
         XC    PARMRTAD,PARMRTAD                                        03311000
*                                                                       03312000
*        FREE THE LOCKS AND REMOVE THE FRR                              03313000
*        ORIGINAL ESTAE IS NOW IN PLACE                                 03314000
*                                                                       03315000
         OI    PFLAG,NOCMSLOK          TURN ON NO CMS LOCK REQUESTED    03316000
         BAL   R14,FREELOCK                                             03317000
         NI    PFLAG,255-NOCMSLOK      TURN OFF NO CMS LOCK REQUESTED   03318000
*                                   /* GOTO EXIT CODE                */ 03319000
         B     VWTOERRT                                                 03320000
*                                                                       03321000
*/********************************************************************/ 03322000
*/*                                                                  */ 03323000
*/*  VWTOSSRT - RETRY ROUTINE FOR SUBSYTEM EXIT                      */ 03324000
*/*                                                                  */ 03325000
*/*  FUNCTION - THIS ROUTINE WILL GET CONTROL IF THERE WAS AN ERROR  */ 03326000
*/*   IN THE SUBSYSTEM WTO EXIT ROUTINE. WE WILL TURN OFF XVD2DELW   */ 03327000
*/*  SO THAT THE MESSAGE WILL BE SURE TO GO OUT.  THEN BRANCH TO     */ 03328000
*/*  VWTOSSLB TO CONTINUE THE PROCESSING                             */ 03329000
*/*                                                                  */ 03330000
*/********************************************************************/ 03331000
*VWTOSSRT:                                                              03332000
*   XVD2DELW='0'B;                  /* DONT DELETE THE MESSAGE       */ 03333000
VWTOSSRT NI    XVD2,255-XVD2DELW                                        03334000
*   GO TO VWTOSSLB;                 /* CONTINUE WITH PROCESSING      */ 03335000
         B     VWTOSSLB                                                 03336000
*                                                                       03337000
*/********************************************************************/ 03338000
*/*  GETBLOCK                                                        */ 03339000
*/*                                                                  */ 03340000
*/*  FUNCTION: THIS ROUTINE WILL GET STORAGE FOR EITHER A WQE OR ORE */ 03341000
*/*     THE OBTAINED STORAGE WILL BE ZEROED OUT                      */ 03342000
*/*                                                                  */ 03343000
*/*  INPUT: TYPE INDICATES THE TYPE OF BLOCK TO GET. WQE OR ORE      */ 03344000
*/*          XVD0WWB MAY BE ON INDICATING THAT A WWB HAS BEEN        */ 03345000
*/*     OBTAINED.                                                    */ 03346000
*/*         XVWWB POINTS AT THE WWB                                  */ 03347000
*/*                                                                  */ 03348000
*/*  OUTPUT: A BLOCK OF THE PROPER SIZE AND ZEROED IS RETURNED TO THE*/ 03349000
*/*     CALLER. IF THE USER WAS ON THE WAIT CHAIN, THE WAIT ECB IS   */ 03350000
*/*     REMOVED.                                                     */ 03351000
*/*     IF THE GETCELL WAS UNSUCCESSFUL THEN THE ADDR PTR IS SET TO  */ 03352000
*/*     ONE INSTEAD OF BEING THE ADDR OF THE NEW CELL.               */ 03353000
*/*                                                                  */ 03354000
*/********************************************************************/ 03355000
*VWTOGETB:                                                              03356000
*                                                                       03357000
VWTOGETB ST    R14,12(,R13)           SAVE RETURN ADDR                  03358000
         ST    R13,@SAGETB+4          BACKCHAIN SAVE AREA               03359000
         LA    R14,@SAGETB                                              03360000
         LR    R13,R14                                                  03361000
         MVC   @PC00003(4),0(R1)                                        03362000
*/********************************************************************/ 03363000
*/*                                                                  */ 03364000
*/* CHECK IF REQUEST IS FOR AN ORE OR FOR A WQE                      */ 03365000
*/*                                                                  */ 03366000
*/********************************************************************/ 03367000
*                                                                       03368000
*   IF TYPE=2 THEN                  /* THIS REQUEST FOR AN ORE   ?   */ 03369000
         L     R14,@PC00003                                             03370000
         CLC   0(4,R14),KF2                                             03371000
         BNE   @RF00709                                                 03372000
*     DO;                           /* YES. GET AN ORE CELL          */ 03373000
*       XVOREAD=0;                  /* SET ADDR TO ZERO SO IT CAN BE    03374000
*                                      USED TO INDICATE SUCCESS OF      03375000
*                                      GETCELL                       */ 03376000
         SLR   R14,R14                                                  03377000
         ST    R14,XVOREAD                                              03378000
*       DO WHILE(XVOREAD=0);        /* LOOP UNTILL XVOREAD GETS         03379000
*                                      FILLED IN                     */ 03380000
         B     @DE00712                                                 03381000
*                                                                       03382000
*         R0=UCMORECP;              /* PICK UP ORE CELL POOL ID      */ 03383000
@DL00712 L     R14,CVTCUCB                                              03384000
         USING UCM,R14                                                  03385000
         L     R0,UCMORECP                                              03386000
*                                                                       03387000
         GETCELL CPID=(0),BRANCH=YES,SAVE=YES                           03388000
*                                                                       03389000
*/********************************************************************/ 03390000
*/*                                                                  */ 03391000
*/*      SEE IF GETCELL WAS SUCCESSFUL                               */ 03392000
*/*                                                                  */ 03393000
*/********************************************************************/ 03394000
*                                                                       03395000
*         IF R15=0 THEN             /* DID WE GET A CELL             */ 03396000
         LTR   R15,R15                                                  03397000
         BNZ   @RF00715                NO, BRANCH                       03398000
*           DO;                                                         03399000
*             XVOREAD=R1;           /* YES, SAVE ADDRESS TO ORE      */ 03400000
         ST    R1,XVOREAD                                               03401000
*             /*******************************************************/ 03402000
*             /*                                                     */ 03403000
*             /* ZERO OUT THE CONTROL BLOCK                          */ 03404000
*             /*                                                     */ 03405000
*             /*******************************************************/ 03406000
*             R1->0(1:LENGTH(OREF)*8)=''B;                              03407000
         XC    0(32,R1),0(R1)                                           03408000
*             UCMRQNR=UCMRQNR+1;    /* INCREMENT ORE COUNT BY ONE    */ 03409000
         L     R14,CVTCUCB                                              03410000
         LA    R10,1                                                    03411000
         AH    R10,UCMRQNR                                              03412000
         STH   R10,UCMRQNR                                              03413000
         DROP  R14                                                      03414000
         B     @DE00712                                                 03415000
*                                                                       03416000
*           END;                                                        03417000
*         ELSE                      /* NO THE GETCELL WAS NOT           03418000
*                                      SUCCESSFUL                    */ 03419000
*/********************************************************************/ 03420000
*/*                                                                  */ 03421000
*/* WE DID NOT GET A CELL. CHECK IF WE ARE OUT OF CELLS. IF SO       */ 03422000
*/* THEN GET AN EXTENSION.                                           */ 03423000
*/*                                                                  */ 03424000
*/********************************************************************/ 03425000
*           IF R15=4 THEN           /* DO WE NEED TO EXTEND THE CELL    03426000
*                                      POOL                          */ 03427000
@RF00715 C     R15,KF4                                                  03428000
         BNE   @RF00721                                                 03429000
*             DO;                   /* YES                           */ 03430000
*               R1=SUBPL231;        /* SET UP FOR GETMAIN FROM          03431000
*                                      SUBPOOL 231                   */ 03432000
         LA    R1,231                                                   03433000
*               R0=OREPLSZ;         /* PICK UP SIZE OF ORE POOL         03434000
*                                      EXTENSION                     */ 03435000
         LA    R0,1024                                                  03436000
*                                                                       03437000
         GETMAIN  RC,LV=(0),SP=(1),BRANCH=YES                           03438000
*                                                                       03439000
*               R3=CVTPTR;          /* RELOAD R3 WHICH WAS              03440000
*                                      DESTROYED BY THE GETMAIN         03441000
*                                      BRANCH ENTRY                  */ 03442000
         L     R3,CVTPTR                                                03443000
         LR    R2,R15                                                   03444000
*               /*****************************************************/ 03445000
*               /*                                                   */ 03446000
*               /* SEE IF GETMAIN WAS SUCCESSFUL. IF SO BUILD EXTEN  */ 03447000
*               /*                                                   */ 03448000
*               /*****************************************************/ 03449000
*               IF R15=0 THEN       /* STORAGE OBTAINED ?            */ 03450000
         LTR   R15,R15                                                  03451000
         BNZ   @RF00727                                                 03452000
*                 DO;               /* YES, EXTEND THE ORE POOL      */ 03453000
*                   R2=R1;          /* SAVE ADDR OF POOL EXTENSION   */ 03454000
         LR    R2,R1                                                    03455000
*                   R15=LENGTH(OREF);/* GET SIZE OF CELL REQUIRED       03456000
*                                      FOR BLD                       */ 03457000
         LA    R15,ORESIZE                                              03458000
*                   R0=UCMORECP;    /* PICK UP ORE CELL POOL ID      */ 03459000
         L     R14,CVTCUCB                                              03460000
         USING UCM,R14                                                  03461000
         L     R0,UCMORECP                                              03462000
         DROP  R14                                                      03463000
*                                                                       03464000
         BLDCPOOL  CPID=(0),CSIZE=(15),SP=231,CPADDR=(1),              C03465000
               BRANCH=YES,POOLSIZ=1,AUTODEL=YES,SERIAL=YES              03466000
*                                                                       03467000
*/********************************************************************/ 03468000
*/*                                                                  */ 03469000
*/* CHECK IF EXTENSION WAS SUCCESSFUL. IF NOT SET ERROR FLAG ON      */ 03470000
*/*                                                                  */ 03471000
*/********************************************************************/ 03472000
*                   IF R15^=0 THEN/* WAS EXTENSION UNSUCCESSFUL      */ 03473000
         LTR   R15,R14                                                  03474000
         BZ    @DE00712                                                 03475000
         MVI   XVFNCT,D23OREBC         ORE BLDCPOOL FAILURE             03476000
         MVI   XVAMOD,VWTOID           IEAVVWTO'S ID FOR D23            03477000
         STC   R15,XVREASON            STORE RETURN CODE IN REASON CODE 03478000
*                     DO;           /* YES. INDICATE SO IN XVOREAD   */ 03479000
*                       /*********************************************/ 03480000
*                       /*                                           */ 03481000
*                       /* FREE THE CORE FOR THE EXTENSION           */ 03482000
*                       /*                                           */ 03483000
*                       /*********************************************/ 03484000
*                       R0=FROREXT;/* GET SUBPOOL AND LENGTH PARM    */ 03485000
         L     R0,FROREXT                                               03486000
*                       R1=R2 ;/* GET ADDR OF EXTENSION              */ 03487000
         LR    R1,R2                                                    03488000
*                                                                       03489000
         FREEMAIN R,LV=(0),A=(1),BRANCH=YES                             03490000
*                       XVOREAD=1;  /* INDICATE NO ORE OBTAINED      */ 03491000
         MVC   XVOREAD,KF1                                              03492000
         B     @DE00712                                                 03493000
*                                                                       03494000
*                     END;                                              03495000
*                 END;                                                  03496000
*               ELSE                                                    03497000
*                 /***************************************************/ 03498000
*                 /*                                                 */ 03499000
*                 /* GETMAIN WAS NOT SUCCESSFUL. CALL ROUTINE TO     */ 03500000
*                 /* CHECK IF ERROR IS DUE TO SYSLOG NOT YET         */ 03501000
*                 /* INITIALIZED                                     */ 03502000
*                 /*                                                 */ 03503000
*                 /***************************************************/ 03504000
*                 DO;                                                   03505000
*                   CALL VWTOLREC(XVOREAD);                             03506000
@RF00727 LA    R14,XVOREAD                                              03507000
         ST    R14,PARMPTR                                              03508000
         LA    R1,PARMPTR                                               03509000
         BAL   R14,VWTOLREC                                             03510000
         CLC   XVOREAD,KF1                                              03511000
         BNE   @DE00712                                                 03512000
         MVI   XVFNCT,D23OREGM         ORE GETMAIN FAILURE, SP231       03513000
         MVI   XVAMOD,VWTOID           IEAVVWTO'S ID FOR D23            03514000
         STC   R2,XVREASON                                              03515000
         B     @DE00712                                                 03516000
*                                                                       03517000
*                 END;                                                  03518000
*             END;                  /* OF TEST FOR EXTENSION NEEDED  */ 03519000
*           ELSE                                                        03520000
*/********************************************************************/ 03521000
*/*                                                                  */ 03522000
*/* THE ERROR WAS NOT DUE TO END OF EXTENSION. THE ERROR CAN NOT     */ 03523000
*/* BE CORRECTED SO SET ERROR INDICATOR FOR CALLER                   */ 03524000
*/*                                                                  */ 03525000
*/********************************************************************/ 03526000
*             XVOREAD=1;            /* INDICATE GET ORE UNSUCCESSFUL */ 03527000
@RF00721 MVI   XVFNCT,D23OREGC         ORE GETCELL FAILURE              03528000
         MVI   XVAMOD,VWTOID           IEAVVWTO'S ID FOR D23            03529000
         STC   R15,XVREASON            STORE RETURN CODE IN REASON CODE 03530000
         MVC   XVOREAD,KF1                                              03531000
*       END;                        /* OF DO WHILE LOOP              */ 03532000
@DE00712 ICM   R14,B'1111',XVOREAD                                      03533000
         BZ    @DL00712                                                 03534000
         B     @RC00709                                                 03535000
*                                                                       03536000
*     END;                                                              03537000
*   /*****************************************************************/ 03538000
*   /*                                                               */ 03539000
*   /* WQE GET CELL PART OF THIS SEGMENT                             */ 03540000
*   /*                                                               */ 03541000
*   /*****************************************************************/ 03542000
*   ELSE                            /* NO THIS REQUEST IS FOR A WQE  */ 03543000
*     DO;                                                               03544000
*       XVWQEAD=0;                  /* ZERO OUT ADDR SO IT CAN BE       03545000
*                                      USED TO INDICATE SUCCESS OR      03546000
*                                      FAILURE OF GETCELL            */ 03547000
@RF00709 SLR   R14,R14                                                  03548000
         ST    R14,XVWQEAD                                              03549000
*       DO WHILE(XVWQEAD=0);        /* LOOP UNTIL ADDR GETS FILLED      03550000
*                                      IN                            */ 03551000
         B     @DE00752                                                 03552000
*                                                                       03553000
*         R0=UCMWQECP;              /* PICK UP WQE CELL POOL ID      */ 03554000
@DL00752 L     R14,CVTCUCB                                              03555000
         USING UCM,R14                                                  03556000
         L     R0,UCMWQECP                                              03557000
*                                                                       03558000
         GETCELL CPID=(0),BRANCH=YES                                    03559000
*                                                                       03560000
*/********************************************************************/ 03561000
*/*                                                                  */ 03562000
*/* SEE IF GETCELL WAS SUCCESSFUL                                    */ 03563000
*/*                                                                  */ 03564000
*/********************************************************************/ 03565000
*         IF R15=0 THEN             /* DID WE GET A CELL ?           */ 03566000
         LTR   R15,R15                                                  03567000
         BNZ   @RF00755                                                 03568000
*           DO;                                                         03569000
*             XVWQEAD=R1;           /* SAVE ADDRESS TO WQE           */ 03570000
         ST    R1,XVWQEAD                                               03571000
*             /*******************************************************/ 03572000
*             /*                                                     */ 03573000
*             /* ZERO OUT THE CONTROL BLOCK                          */ 03574000
*             /*                                                     */ 03575000
*             /*******************************************************/ 03576000
*             R1->0(1:LENGTH(WQE)*8)=''B;                               03577000
         XC    0(WQESIZE,R1),0(R1)                                      03578000
*             UCMWQNR=UCMWQNR+1;    /* INCREMENT WQE COUNT           */ 03579000
         L     R14,CVTCUCB                                              03580000
         LA    R10,1                                                    03581000
         AH    R10,UCMWQNR                                              03582000
         STH   R10,UCMWQNR                                              03583000
         DROP  R14                                                      03584000
         B     @DE00752                                                 03585000
*                                                                       03586000
*           END;                                                        03587000
*/********************************************************************/ 03588000
*/*                                                                  */ 03589000
*/* WE DID NOT GET A CELL. CHECK IF WE ARE OUT OF CELLS. IF SO       */ 03590000
*/* THEN GET AN EXTENSION.                                           */ 03591000
*/*                                                                  */ 03592000
*/********************************************************************/ 03593000
*         ELSE                                                          03594000
*           IF R15=4 THEN           /* DO WE NEED TO EXTEND THE CELL    03595000
*                                      POOL                          */ 03596000
@RF00755 C     R15,KF4                                                  03597000
         BNE   @RF00761                                                 03598000
*             DO;                   /* YES                           */ 03599000
*               R1=SUBPL231;        /* SET UP FOR GETMAIN FROM 231   */ 03600000
         LA    R1,231                                                   03601000
*               R0=WQEPLSZ;         /* PICK UP SIZE OF WQE EXTENSION */ 03602000
         L     R0,KF4096                                                03603000
*                                                                       03604000
         GETMAIN  RC,LV=(0),SP=(1),BRANCH=YES                           03605000
*                                                                       03606000
*               R3=CVTPTR;          /* RELOAD R3 WHICH WAS              03607000
*                                      DESTROYED BY THE GETMAIN         03608000
*                                      CALLING SEQUENCE              */ 03609000
         L     R3,CVTPTR                                                03610000
         LR    R2,R15                                                   03611000
*               /*****************************************************/ 03612000
*               /*                                                   */ 03613000
*               /* SEE IF GETMAIN WAS SUCCESSFUL. IF SO BUILD        */ 03614000
*               /* EXTENSION.                                        */ 03615000
*               /*                                                   */ 03616000
*               /*****************************************************/ 03617000
*               IF R15=0 THEN       /* GET SPACE FOR THE EXTENSION ? */ 03618000
         LTR   R15,R15                                                  03619000
         BNZ   @RF00767                                                 03620000
*                 DO;               /* YES, BUILD THE WQE EXTENSION  */ 03621000
*                   R10=R1;         /* SAVE ADDRES OF WQE EXTENSION  */ 03622000
         LR    R2,R1                                                    03623000
*                   R0=UCMWQECP;    /* PICK UP THE WQE CELL POOL ID  */ 03624000
         L     R14,CVTCUCB                                              03625000
         USING UCM,R14                                                  03626000
         L     R0,UCMWQECP                                              03627000
         DROP  R14                                                      03628000
*                   R15=LENGTH(WQE);/* GET SIZE OF CELL REQUIRED     */ 03629000
         LA    R15,192                                                  03630000
*                                                                       03631000
         BLDCPOOL CPID=(0),SP=231,CSIZE=(15),CPADDR=(1),BRANCH=YES,    C03632000
               AUTODEL=YES,POOLSIZ=4,SERIAL=YES                         03633000
*                                                                       03634000
*/********************************************************************/ 03635000
*/*                                                                  */ 03636000
*/* CHECK IF EXTENSION WAS SUCCESSFUL. IF NOT SET ERROR FLAG ON      */ 03637000
*/*                                                                  */ 03638000
*/********************************************************************/ 03639000
*                   IF R15^=0 THEN/* WAS EXTENSION UNSUCCESSFUL      */ 03640000
         LTR   R15,R15                                                  03641000
         BZ    @DE00752                                                 03642000
         MVI   XVFNCT,D23WQEBC         WQE BLDCPOOL FAILURE             03643000
         MVI   XVAMOD,VWTOID           IEAVVWTO'S ID FOR D23            03644000
         STC   R15,XVREASON            STORE RETURN CODE IN REASON CODE 03645000
*                     DO;           /* YES, INDICATE SO IN XVWQEAD   */ 03646000
*                       /*********************************************/ 03647000
*                       /*                                           */ 03648000
*                       /* FREE THE WQE POOL EXTENSION               */ 03649000
*                       /*                                           */ 03650000
*                       /*********************************************/ 03651000
*                       R0=FRWQEXT;/* GET SUBPOOL AND LENGTH         */ 03652000
         L     R0,FRWQEXT                                               03653000
*                       R1=R2 ;/* GET ADDRESS OF WQE EXTENSION       */ 03654000
         LR    R1,R2                                                    03655000
*                                                                       03656000
         FREEMAIN R,LV=(0),A=(1),BRANCH=YES                             03657000
*                                                                       03658000
*                       XVWQEAD=1;  /* INDICATE NO WQE OBTAINED      */ 03659000
         MVC   XVWQEAD,KF1                                              03660000
         B     @DE00752                                                 03661000
*                                                                       03662000
*                     END;                                              03663000
*                 END;                                                  03664000
*               /*****************************************************/ 03665000
*               /*                                                   */ 03666000
*               /* THE GETMAIN WAS NOT SUCCESSFUL. CALL THE LOG      */ 03667000
*               /* RECOVERY ROUTINE TO SEE IF ERROR DUE TO SYSLOG    */ 03668000
*               /* INIT                                              */ 03669000
*               /*                                                   */ 03670000
*               /*****************************************************/ 03671000
*               ELSE                                                    03672000
*                 DO;                                                   03673000
*                   CALL VWTOLREC(XVWQEAD);/* LOG RECOVERY ROUTINE   */ 03674000
@RF00767 LA    R14,XVWQEAD                                              03675000
         ST    R14,PARMPTR                                              03676000
         LA    R1,PARMPTR                                               03677000
         BAL   R14,VWTOLREC                                             03678000
         CLC   XVWQEAD,KF1                                              03679000
         BNE   @DE00752                                                 03680000
         MVI   XVFNCT,D23WQEGM         WQE GETMAIN FAILURE, SP231       03681000
         MVI   XVAMOD,VWTOID           IEAVVWTO'S ID FOR D23            03682000
         STC   R2,XVREASON             STORE RETURN CODE IN REASON CODE 03683000
         B     @DE00752                                                 03684000
*                                                                       03685000
*                 END;                                                  03686000
*             END;                                                      03687000
*/********************************************************************/ 03688000
*/*                                                                  */ 03689000
*/* THE ERROR WAS NOT DUE TO END OF EXTENSION. THE ERROR CAN NOT     */ 03690000
*/* BE CORRECTED SO SET ERROR INDICATOR FOR CALLER                   */ 03691000
*/*                                                                  */ 03692000
*/********************************************************************/ 03693000
*           ELSE                                                        03694000
*             XVWQEAD=1;            /* INDICATE GET WQE UNSUCCESSFUL */ 03695000
@RF00761 MVI   XVFNCT,D23WQEGC         WQE GETCELL FAILURE              03696000
         MVI   XVAMOD,VWTOID           IEAVVWTO'S ID FOR D23            03697000
         STC   R15,XVREASON            STORE RET CODE INTO REASON CODE  03698000
         MVC   XVWQEAD,KF1                                              03699000
*       END;                        /* OF DO WHILE LOOP              */ 03700000
@DE00752 ICM   R14,B'1111',XVWQEAD                                      03701000
         BZ    @DL00752                                                 03702000
*     END;                                                              03703000
*/********************************************************************/ 03704000
*/*                                                                  */ 03705000
*/* CHECK IF WE HAVE TO FREE THE ECB                                 */ 03706000
*/*                                                                  */ 03707000
*/********************************************************************/ 03708000
*   IF XVD0WWB='1'B THEN            /* A WWB ON THE CHAIN ?          */ 03709000
@RC00709 TM    XVD0,XVD0WWB                                             03710000
         BZ    @ER00003                                                 03711000
*     DO;                           /* YES, FREE IT AND TURN OFF        03712000
*                                      XVD0WWB                       */ 03713000
*       /*************************************************************/ 03714000
*       /*                                                           */ 03715000
*       /* POINT NEXT WWB AT BACK WWB                                */ 03716000
*       /*                                                           */ 03717000
*       /*************************************************************/ 03718000
*       WWBFWDPT->WWBBCKPT=WWBBCKPT;                                    03719000
         L     R14,XVWWB                                                03720000
         USING WWB,R14                                                  03721000
         L     R10,WWBFWDPT                                             03722000
         L     R8,WWBBCKPT                                              03723000
         ST    R8,WWBBCKPT-WWB(R10)                                     03724000
         DROP  R14                                                      03725000
*       /*************************************************************/ 03726000
*       /*                                                           */ 03727000
*       /* POINT BACK WWB AT NEXT WWB                                */ 03728000
*       /*                                                           */ 03729000
*       /*************************************************************/ 03730000
*       WWBBCKPT->WWBFWDPT=WWBFWDPT;                                    03731000
         ST    R10,WWBFWDPT-WWB(,R8)                                    03732000
*/********************************************************************/ 03733000
*/*                                                                  */ 03734000
*/* WWB IS NOW OFF THE QUEUE. FREE IT                                */ 03735000
*/*                                                                  */ 03736000
*/********************************************************************/ 03737000
*       R0=GETWWB;                  /* PICK PARMS FOR FREEMAIN       */ 03738000
         L     R0,GETWWB                                                03739000
*       R1=XVWWB;                   /* GET ADDRESS OF WWB            */ 03740000
         LR    R1,R14                                                   03741000
*                                                                       03742000
         FREEMAIN R,LV=(0),A=(1),BRANCH=YES                             03743000
*                                                                       03744000
*       XVWWB=0;                    /* ZERO OUT ADDRESS OF WWB IN XSA*/ 03745000
         SLR   R10,R10                                                  03746000
         ST    R10,XVWWB                                                03747000
*       XVD0WWB='0'B;               /* INDICATE NO WWB ON CHAIN      */ 03748000
         NI    XVD0,255-XVD0WWB                                         03749000
*     END;                                                              03750000
*   RETURN;                                                             03751000
@ER00003 L     R13,4(,R13)                                              03752000
         L     R14,12(,R13)                                             03753000
         BR    R14                                                      03754000
*                                                                       03755000
*   END VWTOGETB;                                                       03756000
         B     @ER00003                                                 03757000
*                                                                       03758000
*/*** START OF WAITCHN  **********************************************/ 03759000
*/********************************************************************/ 03760000
*/*                                                                  */ 03761000
*/*   WAITCHN - WAIT ON THE ECB IN THE WWB                           */ 03762000
*/*                                                                  */ 03763000
*/*   FUNCTION - THIS ROUTINE WILL CREATE A WWB AND PUT IT ON THE    */ 03764000
*/*     CHAIN POINTED AT BY THE INPUT PARAMETER. IF A WWB ALREADY    */ 03765000
*/*     EXISTS THE ECB PART IS ZEROED. THE ROUTINE FREES THE LOCKS   */ 03766000
*/*     AND THEN WAITS FOR THE ECB TO BE POSTED. THE LOCKS ARE       */ 03767000
*/*     THEN OBTAINED.                                               */ 03768000
*/*                                                                  */ 03769000
*/********************************************************************/ 03770000
*VWTOWAIT:                                                              03771000
*                                                                       03772000
VWTOWAIT ST    R14,@SA00004                                             03773000
         MVC   @PC00004(4),0(R1)                                        03774000
*/********************************************************************/ 03775000
*/*                                                                  */ 03776000
*/*  CHECK IF A WWB ALREADY EXISTS. IF ONE DOES THEN IT IS ON THE    */ 03777000
*/*  PROPER CHAIN.                                                   */ 03778000
*/*                                                                  */ 03779000
*/********************************************************************/ 03780000
*   IF XVD0WWB='0'B THEN            /* DO WE NEED TO GET A WWB       */ 03781000
         TM    XVD0,XVD0WWB                                             03782000
         BNZ   @RF00805                                                 03783000
*     DO;                           /* YES, GET IT AND PUT IT ON THE    03784000
*                                      CHAIN                         */ 03785000
*                                   /* BOTH R0 AND R1 WILL BE USED      03786000
*                                      FOR THE GETMAIN               */ 03787000
*       R0=GETWWB;                  /* PICK UP GETMAIN PARMS FOR WWB */ 03788000
         L     R0,GETWWB                                                03789000
*                                                                       03790000
         GETMAIN R,LV=(0),BRANCH=YES                                    03791000
*                                                                       03792000
*       XVWWB=R1;                   /* SAVE ADDR OF WWB              */ 03793000
         ST    R1,XVWWB                                                 03794000
*       XVD0WWB='1'B;               /* INDICATE WWB AVAILABLE        */ 03795000
         OI    XVD0,XVD0WWB                                             03796000
*                                   /* R1 DOES PT AT WWB. USE R1 */     03797000
*       WWBASCB=R7;                 /* STORE ADDR OF ASCB            */ 03798000
         USING WWB,R1                                                   03799000
         ST    R7,WWBASCB                                               03800000
*       WWBTCBAD=R4;                /* STORE ADDR OF TCB             */ 03801000
         ST    R4,WWBTCBAD                                              03802000
         DROP  R1                                                       03803000
*/********************************************************************/ 03804000
*/*                                                                  */ 03805000
*/* PUT WWB ON THE CHAIN                                             */ 03806000
*/*                                                                  */ 03807000
*/********************************************************************/ 03808000
*       /*************************************************************/ 03809000
*       /*                                                           */ 03810000
*       /* POINT THIS WWB AT WHATEVER OLD LAST WWB WAS POINTING AT   */ 03811000
*       /*                                                           */ 03812000
*       /*************************************************************/ 03813000
*       WWBFWDPT=CHNTAIL->WWBFWDPT;                                     03814000
         L     R10,@PC00004                                             03815000
         L     R8,0(,R10)                                               03816000
         L     R2,WWBFWDPT-WWB(,R8)                                     03817000
         ST    R2,WWBFWDPT-WWB(,R1)                                     03818000
*       CHNTAIL->WWBFWDPT=R1;       /* LINK LAST CHN MEMBER TO THIS     03819000
*                                      WWB                           */ 03820000
         ST    R1,WWBFWDPT-WWB(,R8)                                     03821000
*       WWBBCKPT=CHNTAIL;           /* PT OUR WWB BACK AT OLD LAST      03822000
*                                      WWB                           */ 03823000
         ST    R8,WWBBCKPT-WWB(,R1)                                     03824000
*       CHNTAIL=R1;                 /* PT END OF CHAIN AT OUR WWB    */ 03825000
         ST    R1,0(,R10)                                               03826000
*     END;                                                              03827000
*/********************************************************************/ 03828000
*/*                                                                  */ 03829000
*/*  NOW INDICATE ECB IS WAITING                                     */ 03830000
*/*                                                                  */ 03831000
*/********************************************************************/ 03832000
*   WWBPOSTD='0'B;                  /* INDICATE NOT POSTED           */ 03833000
@RF00805 L     R10,XVWWB                                                03834000
         USING WWB,R10                                                  03835000
         NI    WWBFLAGS,255-WWBPOSTD                                    03836000
*   WWBECB=0;                       /* READY TO BE POSTED            */ 03837000
         SLR   R8,R8                                                    03838000
         ST    R8,WWBECB                                                03839000
         DROP  R10                                                      03840000
*   CALL FREELOCK;                                                      03841000
         BAL   R14,FREELOCK                                             03842000
*   R1=ADDR(WWBECB);                /* LOAD ADDRESS OF ECB FOR WAIT  */ 03843000
         L     R1,XVWWB                                                 03844000
         LA    R1,WWBECB-WWB(,R1)                                       03845000
*   /*****************************************************************/ 03846000
*   /*                                                               */ 03847000
*   /* NOW CHECK WHICH MEMORY WE ARE IN AND ISSUE APPROPRIATE WAIT,  */ 03848000
*   /* LONG OR SHORT                                                 */ 03849000
*   /*                                                               */ 03850000
*   /*****************************************************************/ 03851000
*   IF ASCBASID=1 THEN              /* IF IN MEMORY ONE THEN ISSUE      03852000
*                                      SHORT WAIT AS WE ARE IN          03853000
*                                      COMMTASKS MEMORY              */ 03854000
         CLC   ASCBASID,KH1                                             03855000
         BNE   @RF00825                                                 03856000
*                                                                       03857000
         WAIT  ,ECB=(R1)                                                03858000
         B     @RC00825                                                 03859000
*                                                                       03860000
*   ELSE                            /* WE ARE NOT IN MEMORY ONE SO      03861000
*                                      ISSUE A LONG WAIT TO LET         03862000
*                                      COMMTASK PUT OUT SOME MESSAGES   03863000
*                                      AND FREE UP SOME WQES.        */ 03864000
@RF00825 DS    0H                                                       03865000
         WAIT  ,ECB=(R1),LONG=YES                                       03866000
*   CALL SETLOCK;                                                       03867000
@RC00825 BAL   R14,SETLOCK                                              03868000
*   RETURN;                                                             03869000
@ER00004 L     R14,@SA00004                                             03870000
         BR    R14                                                      03871000
*                                                                       03872000
*   END VWTOWAIT;                                                       03873000
*/*** END OF WAITCHN *************************************************/ 03874000
*                                                                       03875000
*/********************************************************************/ 03876000
*/*                                                                  */ 03877000
*/* THIS ROUTINE WILL FREE THE ORE POINTED AT BY XVOREAD. IT WILL    */ 03878000
*/* ALSO DECREASE THE COUNT OF ORES BY ONE TO ACCOUNT FOR THE ORE    */ 03879000
*/* JUST FREED.                                                      */ 03880000
*/*                                                                  */ 03881000
*/********************************************************************/ 03882000
*VWTOFORE:                                                              03883000
*                                                                       03884000
VWTOFORE ST    R14,12(,R13)                                             03885000
*   R0=UCMORECP;                    /* GET ORE CELL POOL ID          */ 03886000
         L     R14,CVTCUCB                                              03887000
         USING UCM,R14                                                  03888000
         L     R0,UCMORECP                                              03889000
         DROP  R14                                                      03890000
*   R1=XVOREAD;                     /* GET ADDRESS OF ORE            */ 03891000
         L     R1,XVOREAD                                               03892000
*   /*****************************************************************/ 03893000
*   /*                                                               */ 03894000
*   /* FREE THE ORE                                                  */ 03895000
*   /*                                                               */ 03896000
*   /*****************************************************************/ 03897000
*                                                                       03898000
         FREECELL CPID=(0),CELL=(1),BRANCH=YES                          03899000
*                                                                       03900000
*/********************************************************************/ 03901000
*/*                                                                  */ 03902000
*/* THIS ORE CELL MAY HAVE BEEN THE LAST ONE IN AN EXTENSION         */ 03903000
*/* IF SO THEN R15 IS SET TO 20 BY FREECELL. IF THIS CONDITON IS     */ 03904000
*/* FOUND THEN FREE THE EXTENSION. R0 AND R1 ARE SET BY FREECELL     */ 03905000
*/*                                                                  */ 03906000
*/********************************************************************/ 03907000
*   IF R15=20 THEN                  /* WAS CELL THE LAST IN AN          03908000
*                                      EXTENSION                     */ 03909000
         C     R15,KF20                                                 03910000
         BNE   @RF00837                                                 03911000
*     /***************************************************************/ 03912000
*     /*                                                             */ 03913000
*     /* YES, FREE THE EXTENSION                                     */ 03914000
*     /*                                                             */ 03915000
*     /***************************************************************/ 03916000
*                                                                       03917000
         FREEMAIN R,LV=(0),A=(1),BRANCH=YES                             03918000
*                                                                       03919000
*   UCMRQNR=UCMRQNR-1;              /* DECREASE COUNT OF ORES        */ 03920000
@RF00837 L     R10,CVTCUCB                                              03921000
         USING UCM,R10                                                  03922000
         LH    R8,UCMRQNR                                               03923000
         BCTR  R8,0                                                     03924000
         STH   R8,UCMRQNR                                               03925000
         DROP  R10                                                      03926000
*   END VWTOFORE;                                                       03927000
@ER00005 L     R14,12(,R13)                                             03928000
         BR    R14                                                      03929000
*                                                                       03930000
*/*** START OF SETLOCK  **********************************************/ 03931000
*/*                                                                  */ 03932000
*/*   SETLOCK - OBTAIN THE LOCAL AND CMS LOCK AND INSTALL AN FRR     */ 03933000
*/*                                                                  */ 03934000
*/********************************************************************/ 03935000
*/*                                                                  */ 03936000
*/*  SAVE R13 ACROSS SETLOCK                                         */ 03937000
*/*                                                                  */ 03938000
*/********************************************************************/ 03939000
*                                                                       03940000
*SETLOCK:                                                               03941000
*                                                                       03942000
SETLOCK  ST    R14,12(,R13)                                             03943000
         STM   R0,R1,20(R13)                                            03944000
         LR    R0,R11              SAVE BASE REGS                       03945000
         LR    R1,R12                                                   03946000
         LR    R2,R13              SAVE R13                             03947000
*                                                                       03948000
         SETLOCK OBTAIN,TYPE=LOCAL,MODE=UNCOND,                        X03949000
               RELATED=(UCM,IEAVVWTO(FREELOCK))                         03950000
*                                                                       03951000
         LR    R11,R0              RESTORE BASE REGS                    03952000
         LR    R12,R1                                                   03953000
         TM    PFLAG,NOCMSLOK      CMS LOCK NOT REQUESTED ?             03954000
         BNZ   SETLOCKA            YES, BRANCH                          03955000
*                                                                       03956000
         SETLOCK OBTAIN,TYPE=CMS,MODE=UNCOND,                          X03957000
               RELATED=(UCM,IEAVVWTO(FREELOCK))                         03958000
*                                                                       03959000
         LR    R11,R0              RESTORE BASE REGS                    03960000
         LR    R12,R1                                                   03961000
*   /*****************************************************************/ 03962000
*   /*                                                               */ 03963000
*   /* FRR USES R11 AND R12 AS WORK REGS. THE PARM LIST POINTED TO   */ 03964000
*   /* BY ESTAEPRM IS ZEROED BY SETFRR                               */ 03965000
*   /*                                                               */ 03966000
*   /*****************************************************************/ 03967000
*   R13=UCMFRRAD;                   /* GET ADDR OF IEAVMFRR FROM UCM */ 03968000
SETLOCKA L     R13,CVTCUCB                                              03969000
         USING UCM,R13                                                  03970000
         L     R13,UCMFRRAD                                             03971000
         DROP  R13                                                      03972000
*                                                                       03973000
*   /*****************************************************************/ 03974000
*   /*                                                               */ 03975000
*   /* SET UP THE FRR PROTECTION FOR THIS MODULE                     */ 03976000
*   /*                                                               */ 03977000
*   /*****************************************************************/ 03978000
*                                                                       03979000
         SETFRR A,FRRAD=(R13),PARMAD=ESTAEPRM,WRKREGS=(R11,R12)         03980000
*                                                                       03981000
         LR    R11,R0                  RESTORE BASE REGS                03982000
         LR    R12,R1                                                   03983000
*                                                                       03984000
*        A NEW PARMLIST HAS BEEN CREATED BY THE SETFRR THAT             03985000
*        REPLACES THE ONE CREATED BY ESTAE                              03986000
*        INITIALIZE THE NEW PARMLIST                                    03987000
*                                                                       03988000
         L     R10,ESTAEPRM            R10 -> NEW PARMLIST IN FRR       03989000
         USING PARMLIST,R10                                             03990000
*   PARMRGAD=ADDR(REGSAVE);         /* STORE ADDR OF REGISTER SAVE      03991000
*                                      AREA INTO PARM LIST           */ 03992000
         LA    R1,REGSAVE                                               03993000
         ST    R1,PARMRGAD                                              03994000
*   PARMID=MODULEID;                /* INSERT LAST FOUR LETTERS OF      03995000
*                                      MODULE NAME                   */ 03996000
         MVC   PARMID,KCVWTO                                            03997000
         OI    PARMFLAG,PARMFRID       PROTECTED BY FRR                 03998000
*                                   /* SET BASE BACK TO ESTAE           03999000
*                                      PARMLIST                      */ 04000000
*   /*****************************************************************/ 04001000
*   /*                                                               */ 04002000
*   /* THE FOLLOWING PARMFRID FIELD IN THE ESTAE PARAMETER           */ 04003000
*   /* LIST IS SET TO ZERO TO GUARANTEE THAT RECOVERY WILL BE        */ 04004000
*   /* DONE ON BOTH THE ESTAE AND FRR LEVEL                          */ 04005000
*   /*                                                               */ 04006000
*   /*****************************************************************/ 04007000
*   PARMFRID='0'B;                  /* MODULE PROTECTED BY FRR AND      04008000
*                                      ESTAE                         */ 04009000
         LA    R1,EPARM                                                 04010000
         NI    PARMFLAG-PARMLIST(R1),255-PARMFRID                       04011000
*                                   /* RESTORE BASE TO FRR PARMLIST  */ 04012000
*   /*****************************************************************/ 04013000
*   /*                                                               */ 04014000
*   /* RESTORE THE REGISTERS FROM THE SETLOCK                        */ 04015000
*   /*                                                               */ 04016000
*   /*****************************************************************/ 04017000
*   R13=R2                          /* RESTORE SAVEAREA PTR          */ 04018000
         LR    R13,R2                                                   04019000
         L     R14,12(,R13)                                             04020000
         LM    R0,R1,20(R13)                                            04021000
         STM   R0,R15,REGRECOV         REFRESH STORED RECOVERY REGS     04022000
         BR    R14                     RETURN TO CALLER                 04023000
*                                                                       04024000
*/*** START OF FREELOCK **********************************************/ 04025000
*/*                                                                  */ 04026000
*/*   FREELOCK - FREE THE LOCAL AND CMS LOCKS AND REMOVE THE FRR     */ 04027000
*/*                                                                  */ 04028000
*/********************************************************************/ 04029000
*/********************************************************************/ 04030000
*/*                                                                  */ 04031000
*/*  SAVE R13 ACROSS FREEING OF LOCKS                                */ 04032000
*/*                                                                  */ 04033000
*/********************************************************************/ 04034000
*FREELOCK:                                                              04035000
FREELOCK STM   R14,R15,12(R13)                                          04036000
         ST    R1,24(,R13)                                              04037000
         STM   R11,R12,64(R13)                                          04038000
         LR    R2,R13                  SAVE R13 IN R2                   04039000
*                                   /* FREE THE FRR, USE R1 & R15 AS    04040000
*                                      WORK REGS                     */ 04041000
         SETFRR D,WRKREGS=(R1,R15)                                      04042000
*                                                                       04043000
         TM    PFLAG,NOCMSLOK          CMS LOCK HELD ?                  04044000
         BNZ   FREELOKA                NO, BRANCH                       04045000
*                                                                       04046000
         SETLOCK RELEASE,TYPE=CMS,RELATED=(UCM,IEAVVWTO(SETLOCK))       04047000
*                                                                       04048000
FREELOKA SETLOCK RELEASE,TYPE=LOCAL,RELATED=(UCM,IEAVVWTO(SETLOCK))     04049000
*                                                                       04050000
*        NOW RELYING ON ESTAE PROTECTION AS FRR DELETED                 04051000
*                                                                       04052000
*   ESTAEPRM=ADDR(EPARM);           /* RESTORE PARMLIST ADDR TO      */ 04053000
         LA    R10,EPARM            /* ORIGINAL ESTAE PARAMETER LIST */ 04054000
         ST    R10,ESTAEPRM                                             04055000
*   PARMFRID='0'B;                  /* SIGNAL FRR NO LONGER IN PLACE    04056000
*                                      TO ESTAE                      */ 04057000
         NI    PARMFLAG,255-PARMFRID                                    04058000
         LR    R13,R2                  RESTORE R13                      04059000
*   END FREELOCK;                                                       04060000
         LM    R14,R15,12(R13)                                          04061000
         L     R1,24(,R13)                                              04062000
         LM    R11,R12,64(R13)                                          04063000
         STM   R0,R15,REGRECOV         REFRESH STORED RECOVERY REGS     04064000
         BR    R14                                                      04065000
*                                                                       04066000
*/********************************************************************/ 04067000
*/* POSTOECB - THIS ROUTINE SETS UP THE INTERFACE NECESSARY          */ 04068000
*/*            TO BRANCH ENTER XMPOST WITH AN INDICATOR SET          */ 04069000
*/*            TO RUN THE ERRET ROUTINE IN THE MASTER                */ 04070000
*/*            SCHEDULER ADDRESS SPACE                               */ 04071000
*/********************************************************************/ 04072000
*POSTOECB:                                                              04073000
POSTOECB STM   R14,R12,@SA00008                                         04074000
*   R15=CVT0PT01;                   /* GET ENTRY POINT ADDRESS FOR      04075000
*                                      BRANCH ENTRY TO XMPOST        */ 04076000
         L     R15,CVT0PT01                                             04077000
*   R1=0;                           /* CLEAR R1                      */ 04078000
         SLR   R1,R1                                                    04079000
*   R1=R1 '80000000'X;              /* SET UP R1 FOR USE AS BIT         04080000
*                                      MASK TO PREVENT THE NEED FOR A   04081000
*                                      REFERENCE TO STORAGE ONCE THE    04082000
*                                      BASEREG HAS BEEN UPDATED      */ 04083000
         O     R1,KX800000                                              04084000
*   R10=0;                          /* COMPLETION CODE WITH WHICH TO    04085000
*                                      POST UCMOECB                  */ 04086000
         SLR   R10,R10                                                  04087000
*   R13=UCMASCB;                    /* ASCB OF MEMORY CONTAINING ECB    04088000
*                                      TO BE POSTED                  */ 04089000
         L     R14,CVTCUCB                                              04090000
         USING UCM,R14                                                  04091000
         L     R13,UCMASCB                                              04092000
*   R11=ADDR(UCMOECB);              /* POST THE WTO ECB IN COMTASK   */ 04093000
         LA    R11,UCMOECB                                              04094000
*   R11=R11 R1;                     /* SET END OF LIST               */ 04095000
         OR    R11,R1                                                   04096000
*   R12=UCMWAKUP;                   /* IF XMPOST ERROR LET IEAVMEST     04097000
*                                      GET CONTROL                   */ 04098000
         L     R12,UCMWAKUP                                             04099000
         DROP  R14                                                      04100000
*   R12=R12 R1;                     /* SET BIT TO SHOW ERRET IS TO      04101000
*                                      RUN IN MASTER SCHEDULER MEMORY*/ 04102000
         OR    R12,R1                                                   04103000
*                                   /* POST COMTASK                  */ 04104000
         BALR  R14,R15                                                  04105000
*   RETURN;                         /* RETURN TO MAINLINE            */ 04106000
         LM    R14,R12,@SA00008                                         04107000
         BR    R14                                                      04108000
*                                                                       04109000
*   END POSTOECB;                                                       04110000
*   END                                                                 04111000
*                                                                       04112000
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM      */ 04113000
*/*%INCLUDE SYSLIB  (IHACTM  )                                       */ 04114000
*/*%INCLUDE SYSLIB  (IEFJSSOB)                                       */ 04115000
*/*%INCLUDE SYSLIB  (IEFSSOBH)                                       */ 04116000
*/*%INCLUDE SYSLIB  (IEFSSWT )                                       */ 04117000
*/*%INCLUDE SYSLIB  (IEFJESCT)                                       */ 04118000
*/*%INCLUDE SYSLIB  (IHAORE  )                                       */ 04119000
*/*%INCLUDE SYSLIB  (IHAWQE  )                                       */ 04120000
*/*%INCLUDE SYSLIB  (IEZWPL  )                                       */ 04121000
*/*%INCLUDE SYSLIB  (IEECUCM )                                       */ 04122000
*/*%INCLUDE SYSLIB  (IHAFRRS )                                       */ 04123000
*/*%INCLUDE SYSLIB  (IEEBASEA)                                       */ 04124000
*/*%INCLUDE SYSLIB  (IHAESTA )                                       */ 04125000
*/*%INCLUDE SYSLIB  (IHARB   )                                       */ 04126000
*/*%INCLUDE SYSLIB  (IKJRB   )                                       */ 04127000
*/*%INCLUDE SYSLIB  (IKJTCB  )                                       */ 04128000
*/*%INCLUDE SYSLIB  (IHAASVT )                                       */ 04129000
*/*%INCLUDE SYSLIB  (IHAASCB )                                       */ 04130000
*/*%INCLUDE SYSLIB  (IHAPSA  )                                       */ 04131000
*/*%INCLUDE SYSLIB  (CVT     )                                       */ 04132000
*/*%INCLUDE SYSLIB  (IHASCVT )                                       */ 04133000
*                                                                       04134000
@SM03493 MVC   USERTEXT(0),4(R15)      MOVE MESSAGE TEXT TO EXIT PARM   04135000
*                                                                       04136000
@SM03498 MVC   0(0,R1),0(R14)          MOVE TEXT INTO WQETXT            04137000
         DROP  R6                                                       04138000
*                                                                       04139000
@AL00348 DC    A(KF2)                  LIST WITH   1 ARGUMENT(S)        04140000
@AL00392 DC    A(KF1)                  LIST WITH   1 ARGUMENT(S)        04141000
*                                                                       04142000
KF1      DC    F'1'                                                     04143000
KH1      EQU   KF1+2                                                    04144000
KF2      DC    F'2'                                                     04145000
KF4      DC    F'4'                                                     04146000
KH4      EQU   KF4+2                                                    04147000
KF5      DC    F'5'                                                     04148000
KF6      DC    F'6'                                                     04149000
KF8      DC    F'8'                                                     04150000
KH8      EQU   KF8+2                                                    04151000
KH9      DC    H'9'                                                     04152000
KH16     DC    H'16'                                                    04153000
KF20     DC    F'20'                                                    04154000
KH20     EQU   KF20+2                                                   04155000
KF127    DC    F'127'                                                   04156000
KF4096   DC    F'4096'                                                  04157000
KFM1     DC    F'-1'                                                    04158000
KX800000 DC    XL4'80000000'                                            04159000
         DC    0F'0'                                                    04160000
@SIZDATD DC    AL1(229)                SUBPOOL 229 PRIVATE AREA         04161000
         DC    AL3(@ENDDATD-@DATD)     WORK AREA SIZE FOR GETMAIN       04162000
*                                                                       04163000
*        ROUTINES CALLED BY IEAVVWTO                                    04164000
*                                                                       04165000
VIEAVMWT DC    V(IEAVMWTO)             MULTI-LINE WTO                   04166000
VIEECVXT DC    V(IEECVXIT)             USER EXIT                        04167000
VIGC0203 DC    V(IGC0203E)             WTP                              04168000
*                                                                       04169000
LENMAXT  DC    F'129'                  L'MAXIMUM TEXT PERMITTED + 4     04170000
ABNDCODE DC    XL4'00000D23'                                            04171000
KCVWTO   DC    C'VWTO'                                                  04172000
KCSSOB   DC    C'SSOB'                                                  04173000
KCPLUS   DC    C' +'                                                    04174000
KXFFFF   DC    X'FFFF'                                                  04175000
KX8000   DC    X'8000'                                                  04176000
KX0200   DC    X'0200'                                                  04177000
HEXCHAR  DC    CL16'0123456789ABCDEF'                                   04178000
PATTIME  DC    XL9'4021214B21214B2121'  ED PATTERN FOR TIME             04179000
*                                                                       04180000
*        GETMAIN PARAMETER LIST FOR WWB                                 04181000
*                                                                       04182000
GETWWB   DC    0F'0'                   GETMAIN/FREEMAIN FOR WWB EXTENT  04183000
         DC    AL1(231)                SUBPOOL                          04184000
         DC    AL3(WWBSIZE)            SIZE                             04185000
*                                                                       04186000
*        GETMAIN/FREEMAIN FOR ORE EXTENT                                04187000
*                                                                       04188000
FROREXT  DC    0F'0'                   GETMAIN/FREEMAIN FOR ORE EXTENT  04189000
         DC    AL1(231)                                                 04190000
         DC    AL3(1024)                                                04191000
*                                                                       04192000
*        GETMAIN/FREEMAIN FOR WQE EXTENT                                04193000
*                                                                       04194000
FRWQEXT  DC    0F'0'                   GETMAIN/FREEMAIN FOR WQE EXTENT  04195000
         DC    AL1(231)                                                 04196000
         DC    AL3(4096)                                                04197000
*                                                                       04198000
GETSP231 DC    0F'0'                                                    04199000
         DC    AL1(231)                                                 04200000
         DC    AL3(20+16)                                               04201000
*                                                                       04202000
IDMAP    DC    X'80'                                                    04203000
         DC    X'40'                                                    04204000
         DC    X'20'                                                    04205000
         DC    X'10'                                                    04206000
         DC    X'08'                                                    04207000
         DC    X'04'                                                    04208000
         DC    X'02'                                                    04209000
         DC    X'01'                                                    04210000
*                                                                       04211000
*        TRANSLATE ALL UNPRINTABLE CHARS TO HEX A1                      04212000
*                                                                       04213000
TRTAB    DC    X'40A1A1A1A1A1A1A1'      ~~~~~~~   00 -> 07              04214000
         DC    X'A1A1A1A1A1A1A1A1'     ~~~~~~~~   08 -> 0F              04215000
         DC    X'A1A1A1A1A1A1A1A1'     ~~~~~~~~   10 -> 17              04216000
         DC    X'A1A1A1A1A1A1A1A1'     ~~~~~~~~   18 -> 1F              04217000
         DC    X'A1A1A1A1A1A1A1A1'     ~~~~~~~~   20 -> 27              04218000
         DC    X'A1A1A1A1A1A1A1A1'     ~~~~~~~~   28 -> 2F              04219000
         DC    X'A1A1A1A1A1A1A1A1'     ~~~~~~~~   30 -> 37              04220000
         DC    X'A1A1A1A1A1A1A1A1'     ~~~~~~~~   38 -> 3F              04221000
         DC    X'40A1A1A1A1A1A1A1'      ~~~~~~~   40 -> 47              04222000
         DC    X'A1A14A4B4C4D4E4F'     ~~>.<(+|   48 -> 4F              04223000
         DC    X'50A1A1A1A1A1A1A1'     &~~~~~~~   50 -> 57              04224000
         DC    X'A1A15A5B5C5D5E5F'     ~~!$*);^   58 -> 5F              04225000
         DC    X'6061A1A1A1A1A1A1'     -/~~~~~~   60 -> 67              04226000
         DC    X'A1A16A6B6C6D6E6F'     ~~|,%_>?   68 -> 6F              04227000
         DC    X'A1A1A1A1A1A1A1A1'     ~~~~~~~~   70 -> 77              04228000
         DC    X'A1797A7B7C7D7E7F'     ~`:#@'="   78 -> 7F              04229000
         DC    X'A181828384858687'     ~ABCDEFG   80 -> 87              04230000
         DC    X'8889A1A1A1A1A1A1'     HI~~~~~~   88 -> 8F              04231000
         DC    X'A191929394959697'     ~JKLMNOP   90 -> 97              04232000
         DC    X'9899A1A1A1A1A1A1'     QR~~~~~~   98 -> 9F              04233000
         DC    X'A1A1A2A3A4A5A6A7'     ~~STUVWX   A0 -> A7              04234000
         DC    X'A8A9A1A1A1ADA1A1'     YZ~~~[~~   A8 -> AF              04235000
         DC    X'A1A1A1A1A1A1A1A1'     ~~~~~~~~   B0 -> B7              04236000
         DC    X'A1A1A1A1A1BDA1A1'     ~~~~~]~~   B8 -> BF              04237000
         DC    X'C0C1C2C3C4C5C6C7'     {ABCDEFG   C0 -> C7              04238000
         DC    X'C8C9A1A1A1A1A1A1'     HI~~~~~~   C8 -> CF              04239000
         DC    X'D0D1D2D3D4D5D6D7'     }JKLMNOP   D0 -> D7              04240000
         DC    X'D8D9A1A1A1A1A1A1'     QR~~~~~~   D8 -> DF              04241000
         DC    X'E0A1E2E3E4E5E6E7'     \~STUVWX   E0 -> E7              04242000
         DC    X'E8E9A1A1A1A1A1A1'     YZ~~~~~~   E8 -> EF              04243000
         DC    X'F0F1F2F3F4F5F6F7'     01234567   F0 -> F7              04244000
         DC    X'F8F9A1A1A1A1A1A1'     89~~~~~~   F8 -> FF              04245000
*                                                                       04246000
*        ESTAE SVC PARAMETER LIST                                       04247000
*                                                                       04248000
ESTAELST ESTAE  ,MF=L                                                   04249000
ESTAELEN EQU   *-ESTAELST              L'ESTAE PARAMETER LIST           04250000
*                                                                       04251000
MVCTEXT  MVC   WKATEXT+4(0),0(R8)                                       04252000
*                                                                       04253000
         USING WQE,R8                                                   04254000
TRINST   TR    WQETXT(0),TRTAB         TRANSLATE ALL UNPRINTABLES       04255000
         DROP  R8                                                       04256000
*                                                                       04257000
*        WORK AREA IN GETMAINED PRIVATE AREA STORAGE SUBPOOL 229        04258000
*                                                                       04259000
@DATD    DSECT                                                          04260000
@SA00001 DS    18F                                                      04261000
@SAGETB  DS    18F                                                      04262000
@PC00003 DS    F                                                        04263000
@SA00004 DS    F                                                        04264000
@PC00004 DS    F                                                        04265000
@SA00008 DS    15F                                                      04266000
@SA00002 DS    15F                                                      04267000
@PC00002 DS    F                                                        04268000
PARMPTR  DS    A                                                        04269000
@TF00001 DS    F                                                        04270000
REG1SAV  DS    F                                                        04271000
REG2SAV  DS    F                  SAVE AREA FOR R2 WHEN DEALING WITH    04272000
*                                 UCM (PREVIOUSLY USED XVA4)            04273000
LONGTXT  DS    A                  -> AREA GETMAINED FOR LONG WPL        04274000
*                                    IF ZERO THEN NO STORAGE GETMAINED  04275000
LONGLEN  DS    F                  L'GETMAINED AREA FOR LONG TEXT        04276000
*                                                                       04277000
LENMWQE  DS    F                  L'OF ALL MINOR WQES                   04278000
*                                 VALID ONLY FOR MLWTO                  04279000
MLWTOXH  DS    A                  -> MLWTO EXTENT HEADER IN CALLERS WPL 04280000
*                                 VALID ONLY FOR MLWTO                  04281000
*                                                                       04282000
*        VERSION 2 XWPL TO STORE ALL USER PROVIDED WPL FIELDS           04283000
*                                                                       04284000
WKALGH   DS    AL2                MESSAGE LENGTH OF MAJOR WQE TEXT FROM 04285000
*                                 CALLER'S WPL                          04286000
WKAMCSF  DS    XL2                MCS FLAGS COPIED FROM CALLERS WPL     04287000
WKAADTXT DS    AL4                -> MESSAGE TEXT                       04288000
WKAVRSN  DS    AL1                XWPL VERSION NUMBER                   04289000
WKAFLAGS DS    XL1                MISC FLAGS                            04290000
WKARPYLN DS    AL1                L'REPLY FOR WTOR                      04291000
WKALNGTH DS    AL1                L'XWPL, ZERO FOR VERSION 1            04292000
WKAMCSF1 DS    XL2                EXTENDED MCS FLAGS                    04293000
WKACPFLF DS    XL2                MCS FLAGS FOR CNTL PROGRAM USE        04294000
WKARPBUF DS    AL4                -> REPLY BUFFER                       04295000
WKAECBP  DS    AL4                -> ECB                                04296000
WKASEQN  DS    AL4                DOM/CONNECT ID                        04297000
WKADSC   DS    XL2             *  DESCRIPTOR CODE TO BE USED IN WTO     04298000
WKAROC   DS    XL2             V  ROUTE CODES TO BE USED IN WTO         04299000
WKAROUT  DS    XL14               EXTENDED ROUTING CODES                04300000
WKAMSGTY DS    XL2                MSGTYPE FLAGS COPIED FROM CALLERS WPL 04301000
WKAPRTY  DS    XL2                MESSAGE PRIORITY                      04302000
WKAJOBID DS    CL8                JOB ID                                04303000
WKAJOBNM DS    CL8                JOB NAME                              04304000
WKAKEY   DS    CL8                RETRIEVAL KEY                         04305000
WKATOKN  DS    AL4                TOKEN FOR DOM                         04306000
WKACNID  DS    AL4                CONSOLE ID                            04307000
WKASYSNA DS    CL8                SYSTEM NAME                           04308000
WKACNNME DS    CL8                CONSOLE NAME                          04309000
WKARCNA  DS    AL4                -> REPLY CONSOLE NAME/ID              04310000
WKACART  DS    AL4                -> CART                               04311000
WKAWSPRM DS    AL4                -> WAIT STATE PARM LIST               04312000
WKAASCB  DS    AL4                -> ASCB                               04313000
WKARSV30 DS    XL16               RESERVED                              04314000
*                                                                       04315000
WKATEXT  DS    CL129              MESSAGE TEXT (MAXIMUM 125 CHARS) +4   04316000
*                                                                       04317000
ESTAEPRM DS    A                  -> USER PARAMETER LIST PASSED TO      04318000
*                                    IEAVMFRR ESTAE/FRR EXIT            04319000
@TS00001 DS    CL1                                                      04320000
STARTID  DS    CL1                                                      04321000
CVDAREA  DS    D                                                        04322000
*                                                                       04323000
PFLAG    DS    XL1                ADDITIONAL FLAGS                      04324000
NOCMSLOK EQU   X'40'              DO NOT REQUEST OR FREE CMS LOCK       04325000
*                                                                       04326000
*        PARAMETER LIST FOR WTO USER EXIT IEECVXIT                      04327000
*                                                                       04328000
         DS    0F                                                       04329000
USERPARM DS    0CL136                                                   04330000
USERTEXT DS    CL128                                                    04331000
USERROUT DS    0CL4                                                     04332000
USERRC   DS    XL2                                                      04333000
         DS    XL2                      RESERVED                        04334000
USERDESC DS    0CL4                                                     04335000
USERDC   DS    XL2                                                      04336000
         DS    XL2                      RESERVED                        04337000
*                                                                       04338000
*        MAPPED BY IEFSSOBH                                             04339000
*                                                                       04340000
SSOB     DS    0CL20                                                    04341000
SSOBID   DS    CL4                                                      04342000
SSOBLEN  DS    FL2                                                      04343000
SSOBFUNC DS    FL2                                                      04344000
SSOBSSIB DS    AL4                                                      04345000
SSOBRETN DS    AL4                                                      04346000
SSOBINDV DS    AL4                                                      04347000
*                                                                       04348000
SSWT     DS    0CL16                                                    04349000
SSWTLEN  DS    FL2                                                      04350000
@NM00012 DS    FL2                                                      04351000
SSWTWQE  DS    AL4                                                      04352000
SSWTMIN  DS    AL4                                                      04353000
SSWTORE  DS    AL4                                                      04354000
*                                                                       04355000
*        ESTAE ROUTINE PARAMETER AREA                                   04356000
*                                                                       04357000
*        MAPPED BY PARMLIST DSECT                                       04358000
*                                                                       04359000
*        PASSED TO ESTAE/FRR EXIT IN SDWAPARM                           04360000
*                                                                       04361000
EPARM    DS    6F                                                       04362000
*                                                                       04363000
*        REGISTER RESTORE FLAGS AND REGISTER SAVE AREA                  04364000
*        USED BY COMM TASK FRR IEAVMFRR                                 04365000
*                                                                       04366000
         CNOP  2,4                *    FORCE HALFWORD ALIGNMENT         04367000
REGSAVE  DS    0CL66              |                                     04368000
REGRGMAP DS    H                  |                                     04369000
REGRECOV DS    16F                V                                     04370000
*                                                                       04371000
@ENDDATD EQU   *                                                        04372000
*                                                                       04373000
*        CVT                                                            04374000
*                                                                       04375000
         CVT   DSECT=YES                                                04376000
*                                                                       04377000
*        TCB                                                            04378000
*                                                                       04379000
         IKJTCB LIST=YES                                                04380000
*                                                                       04381000
*        RB                                                             04382000
*                                                                       04383000
         IKJRB   DSECT=YES                                              04384000
*/********************************************************************/ 04385000
*/*                                                                  */ 04386000
*/*          EXTENDED SAVEAREA MAPPING FOR SVC 35                    */ 04387000
*/*                                                                  */ 04388000
*/********************************************************************/ 04389000
*                                                                       04390000
*        DEFINED IN THE RBEXSAVE AREA                                   04391000
*                                                                       04392000
         ORG   RBEXSAVE                                                 04393000
XVSAV    DS    0A                                                       04394000
XVA4     DS    0F                      ERROR PROCESSING FIELDS          04395000
XVFNCT   DS    C                       D23 PROCESS CODE                 04396000
D23VALID EQU   X'10'                   PARMLIST VALIDITY CHECK          04397000
D23OREGC EQU   X'20'                   ORE GETCELL FAILURE              04398000
D23OREBC EQU   X'21'                   ORE BLDCPOOL FAILURE             04399000
D23OREGM EQU   X'22'                   ORE GETMAIN FAILURE, SP231       04400000
D23WQEGC EQU   X'30'                   WQE GETCELL FAILURE              04401000
D23WQEBC EQU   X'31'                   WQE BLDCPOOL FAILURE             04402000
D23WQEGM EQU   X'32'                   WQE GETMAIN FAILURE, SP231       04403000
D23DYN   EQU   X'42'                   DYNAMIC AREA GETMAIN FAILURE     04404000
XVAMOD   DS    C                       D23 MODULE ID                    04405000
VWTOID   EQU   X'01'                   IEAVVWTO'S ID FOR D23            04406000
MWTOID   EQU   X'02'                   IEAVMWTO'S ID FOR D23            04407000
XVA41    DS    C                       RESERVED                         04408000
XVREASON DS    C                       D23 REASON CODE                  04409000
D23BNDY  EQU   X'01'                   WTOR PARMLIST NOT ON WORD BNDY   04410000
D23MLWTR EQU   X'02'                   MULTILINE WTOR SPECIFIED         04411000
D23PARM  EQU   X'03'                   WPL NOT ADDRESSABLE BY USER      04412000
D23ZERO  EQU   X'04'                   ZERO TEXT LENGTH WTOR            04413000
D23CMOD  EQU   X'05'                   CALLER MODIFIED WPL              04414000
*                                      DURING WTO PROCESSING            04415000
D23LTXT  EQU   X'06'                   WTO/WTOR TEXT= CODED AND         04416000
*                                      WPLLGH ^=8                       04417000
*                                                                       04418000
XVA8     DS    AL4                     STORE TIME                       04419000
XVWPLADR DS    AL4                     -> CALLERS WPL                   04420000
XVWQEAD  DS    AL4                                                      04421000
*                                                                       04422000
*        FLAGS                                                          04423000
*                                                                       04424000
XVDFLAGS DS    0XL4                                                     04425000
XVD0     DS    X                                                        04426000
XVD0RPFD EQU   BIT1                    HARD COPY ONLY                   04427000
XVD0NWQE EQU   BIT2                    GET WQE                          04428000
XVD0NORE EQU   BIT3                    GET THE ORE FIRST                04429000
XVD0QID  EQU   BIT4                    QID FIELD IS PRESENT IN THE WPL  04430000
XVD0WWB  EQU   BIT5                    WWB WTO WAIT BLOCK OBTAINED      04431000
XVD0USER EQU   BIT6                                                     04432000
XVD0HDCY EQU   BIT7                                                     04433000
*                                                                       04434000
*        XVDFLAGS+1                                                     04435000
*                                                                       04436000
XVD1     DS    X                                                        04437000
XVD1PRIV EQU   BIT0                    PRIVILEGED USER                  04438000
XVD1ENQW EQU   BIT1                    ENQ'D ON A WQE  (VMWTO)          04439000
XVD1ENQO EQU   BIT2                    ENQ'D ON AN ORE (VMWTO)          04440000
XVD1ALDN EQU   BIT2                    ALL NEEDED CONTROL BLKS OBTAINED 04441000
XVD1PP   EQU   BIT5                    PROBLEM PROGRAM CALLER           04442000
XVD1AUTH EQU   BIT6                    KEY 0, SUPVR STATE OR APF AUTH   04443000
XVD1PERR EQU   BIT7                    SEVERE ERROR FOUND. ABEND USER   04444000
*                                                                       04445000
*        XVDFLAGS+2                                                     04446000
*                                                                       04447000
XVD2     DS    X                                                        04448000
XVD2CON  EQU   BIT0                    CONNECTING                       04449000
XVD2VALD EQU   BIT3                    PARAMETER LIST IS VALID          04450000
XVD2DELW EQU   BIT4                    SEND MSG TO HARDCOPY ONLY        04451000
XVD2ZERO EQU   BIT5                    ZERO MSG ID TO USER              04452000
XVD2WTOR EQU   BIT6                    WTOR REQUEST                     04453000
XVD2QFHC EQU   BIT7                    QUEUE THIS MSG TO HARD COPY      04454000
*                                                                       04455000
*        XVDFLAGS+3                                                     04456000
*                                                                       04457000
XVD3     DS    X                                                        04458000
XVD3BLDJ EQU   BIT0                    BUILD MAJOR WQE                  04459000
XVD3BLD1 EQU   BIT1                    BUILD LINE 1                     04460000
XVD3BLD2 EQU   BIT2                    BUILD LINE 2                     04461000
XVD3TXT1 EQU   BIT3                    TEXT LINE 1 BEING USED           04462000
XVD3TFX  EQU   BIT4                    TCBTFX WAS SET ON,TURN IT OFF    04463000
*        END OF FLAGS                                                   04464000
XVOREAD  DS    AL4                                                      04465000
         ORG   XVOREAD                                                  04466000
XVX      DS    0F                      USED AS WORK AREA BY VMWTO       04467000
XVX0     DS    X                       LINE CONTROL FLAGS - MLWTO       04468000
XVX0FLCL EQU   BIT0                    FIRST LINE IS CONTROL LINE       04469000
XVX0LL1F EQU   BIT1                    LABEL LINE 1 FOUND               04470000
XVX0LL2F EQU   BIT2                    LABEL LINE 2 FOUND               04471000
XVX0UDCL EQU   BIT3                    USE DEFAULT CONTROL LINE         04472000
XVX0FLJE EQU   BIT4                    FIRST LINE JUST 'E'              04473000
XVX0FEDE EQU   BIT5                    FORCE END (LAST LINE TO BE DE)   04474000
XVX1     DS    X                       ERROR FLAGS - MLWTO              04475000
XVX1STOP EQU   BIT0                    ERROR IN PARM LIST; IGNORE MLWTO 04476000
XVX1NOID EQU   BIT1                    NO ID FOR CONNECTING MLWTO       04477000
XVX2     DS    AL1                     NO OF LINES STILL TO DO          04478000
XVX3     DS    AL1                     NO OF LINES FROM MLWTO EXT HDR   04479000
*                                                                       04480000
XVCMAJOR DS    AL4                                                      04481000
XVCMINOR DS    AL4                                                      04482000
*                                                                       04483000
XVWWB    DS    AL4                                                      04484000
*                                                                       04485000
XVWQEID  DS    0AL4       *                                             04486000
XVWQEIDA DS    AL3        |            A NEW LINE IS TO BE              04487000
*                         |            CONNECTED TO THE MESSAGE         04488000
*                         |            WITH THIS 3 BYTE MESSAGE ID      04489000
*                         |            MLWTO ONLY                       04490000
XVCONID  DS    XL1        V            CONSOLE ID FOR THIS MESSAGE      04491000
*                                                                       04492000
XVRET    DS    0AL4                                                     04493000
XVRETCOD DS    AL4                                                      04494000
XVLEN    EQU   *-XVSAV                 MUST NOT EXCEED 48 BYTES AS      04495000
*                                      RB EXTENDED SAVE AREA USED       04496000
*                                                                       04497000
*        CONSOLE UNIT CONTROL MODULE                                    04498000
*                                                                       04499000
         IEECUCM DSECT=YES,FORMAT=NEW                                   04500000
*                                                                       04501000
*        IEAVMFRR USER'S PARM LIST                                      04502000
*                                                                       04503000
         IHACTM FTPT                                                    04504000
*                                                                       04505000
*        WRITE TO OPERATOR WAIT BLOCK MAPPING                           04506000
*                                                                       04507000
         IHACTM WWB                                                     04508000
*                                                                       04509000
*        FUNCTIONAL RECOVERY ROUTINE STACK                              04510000
*                                                                       04511000
         IHAFRRS                                                        04512000
*                                                                       04513000
*        OPERATOR REPLY ELEMENT                                         04514000
*                                                                       04515000
         IHAORE                                                         04516000
*                                                                       04517000
*        WRITE-TO-OPERATOR QUEUE ELEMENT                                04518000
*                                                                       04519000
         IHAWQE FORMAT=NEW                                              04520000
*                                                                       04521000
*        WTO/WTOR/MLWTO/WTP PARAMETER LIST                              04522000
*                                                                       04523000
         IEZWPL                                                         04524000
*                                                                       04525000
         PRINT NOGEN                                                    04526000
*                                                                       04527000
*        MASTER SCHEDULER RESIDENT DATA AREA                            04528000
*                                                                       04529000
         IEEBASEA                                                       04530000
*                                                                       04531000
*        PREFIXED SAVE AREA                                             04532000
*                                                                       04533000
         IHAPSA                                                         04534000
*                                                                       04535000
*        ADDRESS SPACE CONTROL BLOCK                                    04536000
*                                                                       04537000
         IHAASCB                                                        04538000
*                                                                       04539000
*        JOB ENTRY SUBSYSTEM COMMUNICATION TABLE                        04540000
*                                                                       04541000
         IEFJESCT                                                       04542000
*                                                                       04543000
         PRINT GEN                                                      04544000
*                                                                       04545000
IEAVVWTO CSECT                                                          04546000
R0       EQU   0                                                        04547000
R1       EQU   1                                                        04548000
R2       EQU   2                                                        04549000
R3       EQU   3                                                        04550000
R4       EQU   4                                                        04551000
R5       EQU   5                                                        04552000
R6       EQU   6                                                        04553000
R7       EQU   7                                                        04554000
R8       EQU   8                                                        04555000
R9       EQU   9                                                        04556000
R10      EQU   10                                                       04557000
R11      EQU   11                                                       04558000
R12      EQU   12                                                       04559000
R13      EQU   13                                                       04560000
R14      EQU   14                                                       04561000
R15      EQU   15                                                       04562000
*                                                                       04563000
         END   IEAVVWTO                                                 04564000
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
*                                                                       00025000
*        FUNCTION -                                                     00026000
*        CALLED BY IEAVVWTO (SVC 35) TO PROCESS WRITE TO PROGRAMMER     00027000
*        ROUTE CODE 11 MESSAGES BY WRITING THE MESSAGE TO THE           00028000
*        SYSTEM MESSAGE DATA SET. IT THE CALLER IS A TSO USER AND       00029000
*        HAS REQUESTED WTP MESSAGES IN THE USERS PROFILE TABLE THEN     00030000
*        THE MESSAGE IS ISSUED AS A TPUT TO THE USER                    00031000
*                                                                       00032000
*        REGISTERS ON ENTRY -                                           00033000
*        R4  -> TCB                                                     00034000
*        R5  -> SVRB WITH EXTENDED SAVE AREA USED                       00035000
*        R7  -> ASCB                                                    00036000
*        R14    RETURN ADDR TO IEAVVWTO                                 00037000
*        R15  = ENTRY                                                   00038000
*                                                                       00039000
         SAVE  (14,12),,'IGC0203E ZP60040 &SYSDATE &SYSTIME'            00040000
*                                                                       00041000
         LA    R11,0(,R15)                                              00042000
         USING IGC0203E,R11                                             00043000
         L     R0,WORKIGCG             R0 = L'WORKIGC                   00044000
         LR    R9,R0                   SAVE LENGTH FOR LATER AREA ZERO  00045000
*                                                                       00046000
*        GETMAIN WORKAREA IN SUBPOOL 231                                00047000
*                                                                       00048000
         GETMAIN R,LV=(0)                                               00049000
*                                                                       00050000
         LR    R8,R1                   R8 -> GETMAINED AREA             00051000
*                                      R9  = L'GETMAINED AREA           00052000
         SLR   R15,R15                 NO COPY, PAD OF ZERO             00053000
         MVCL  R8,R14                  ZERO WORKAREA                    00054000
         ST    R1,8(,R13)              CHAIN SAVE AREAS                 00055000
         ST    R13,4(,R1)                                               00056000
         LR    R10,R13                 R10 -> IEAVVWTO WORKAREA         00057000
         USING WORKVV,R10                                               00058000
         LR    R13,R1                                                   00059000
         USING WORKIGC,R13                                              00060000
*                                                                       00061000
         LR    R12,R5                                                   00062000
         USING RBBASIC,R12                                              00063000
         L     R3,CVTPTR                                                00064000
         USING CVT,R3                                                   00065000
         MVC   ADDRUCM,CVTCUCB         SAVE -> UCM IN ADDRUCM           00066000
         DROP  R3                                                       00067000
         ST    R4,TCBADDR                                               00068000
         L     R1,ADDRUCM                                               00069000
         USING UCM,R1                                                   00070000
         CLC   UCMCTID,ASCBASID-ASCB(R7)  CALLER WAS COMM TASK ?        00071000
         BE    @RF00148                YES, BRANCH                      00072000
         DROP  R1                                                       00073000
*   CALL GETESTAE;                  /* NO, CREATE STAE ENVIRONMENT   */ 00074000
         BAL   R14,GETESTAE                                             00075000
*   IF R15=0 THEN                   /* IF STAE ENVIRONMENT CREATED      00076000
*                                      THEN CONTINUE PROCESSING         00077000
*                                      OTHERWISE RETURN TO CALLER    */ 00078000
         LTR   R15,R15                 ESTAE ENVIRONMENT ESTABLISHED ?  00079000
         BNE   @RF00148                NO, ERROR RETURN                 00080000
*     DO;                                                               00081000
*       IF TCBJSCBB^=0 THEN         /* IF NO JSCB PTR RETURN TO         00082000
*                                      CALLER                        */ 00083000
         L     R4,TCBADDR              R1 -> TCB                        00084000
         USING TCB,R4                                                   00085000
         SLR   R2,R2                                                    00086000
         ICM   R2,B'0111',TCBJSCBB     R2 -> JSCB                       00087000
         BZ    @RF00151                NO JSCB, BRANCH                  00088000
         DROP  R4                                                       00089000
*         DO;                                                           00090000
*                                                                       00091000
*           /*********************************************************/ 00092000
*           /*                                                       */ 00093000
*           /* GET ACTIVE JSCB ADDRESS FROM JSCBACT. CHECK IF TSO    */ 00094000
*           /* USER(FIELD ASCBTSB IN THE ASCB HAS AN ADDRESS IF A TSO*/ 00095000
*           /* USER). IF SO THE MESSAGE IS PUT TO HIS TERMINAL VIA   */ 00096000
*           /* TPUT MACRO. IF THE TPUT IS SUCCESSFUL BYPASS ALL      */ 00097000
*           /* PROCESSING UP TO THE SCAN OF THE UCM TABLE FOR MORE   */ 00098000
*           /* CONSOLES THAT RECEIVE ROUTE CODE 11 MESSAGES. IF NOT  */ 00099000
*           /* SUCCESSFUL TRY TO PUT THE MESSAGE TO HIS MESSAGE DATA */ 00100000
*           /* SET.                                                  */ 00101000
*           /*                                                       */ 00102000
*           /*********************************************************/ 00103000
*                                                                       00104000
*           JSCBADDR=JSCBACT;       /* PUT ACTIVE JSCB ADDR IN ACTJSCB  00105000
         USING IEZJSCB,R2                                               00106000
         MVC   ACTJSCB,JSCBACT         ACTJSCB -> ACTIVE JSCB           00107000
         DROP  R2                                                       00108000
*           R15=4;                  /* INITIALIZE R15                */ 00109000
         LA    R15,4                                                    00110000
*           IF ASCBTSB^=0 THEN      /* CHECK FOR TSO USER            */ 00111000
         ICM   R1,B'1111',ASCBTSB-ASCB(R7)   TSB PRESENT ?              00112000
         BZ    @RF00156                NO, BRANCH                       00113000
*             CALL ISSUTPUT;        /* YES, PUT MSG TO TSO TERMINAL  */ 00114000
         BAL   R14,ISSUTPUT                                             00115000
*           IF R15^=0 THEN          /* IF PUT WASNT SUCCESSFUL THEN  */ 00116000
@RF00156 LTR   R15,R15                                                  00117000
         BZ    @RF00151                                                 00118000
*             DO;                   /* PUT MSG TO SYSTEM MSG DATASET */ 00119000
*                                                                       00120000
*               /*****************************************************/ 00121000
*               /*                                                   */ 00122000
*               /* CHECK IF THIS JOBSTEP HAS ISSUED PREVIOUS WTP     */ 00123000
*               /* MSGS. IF NOT THEN INITIALIZE THE JSCB. ENQUEUE TO */ 00124000
*               /* SERIALIZE PROCESSING OF THE PUT TO HASP. ALSO     */ 00125000
*               /* CHECK FOR THE PRESENCE OF AN RPL POINTER.         */ 00126000
*               /*                                                   */ 00127000
*               /*****************************************************/ 00128000
*                                                                       00129000
*               CALL CHECKJOB;      /* CHECK IF JOB HAS ISSUED          00130000
*                                      PREVIOUS WTP MSGS             */ 00131000
         BAL   R14,CHECKJOB                                             00132000
*               CALL ISSUEENQ;      /* ENQ TO SERIALIZE PROCESSING      00133000
*                                      AND CHECK RPL POINTER         */ 00134000
         BAL   R14,ISSUEENQ                                             00135000
*               IF R15=0 THEN       /* IF ENQ SUCCESSFUL CONTINUE       00136000
*                                      PROCESSING                    */ 00137000
         LTR   R15,R15                                                  00138000
         BNZ   @RF00151                                                 00139000
*                 DO;                                                   00140000
*                                                                       00141000
*                   /*************************************************/ 00142000
*                   /*                                               */ 00143000
*                   /* IF ENQUEUE IS SUCCESSFUL THEN R3 IS           */ 00144000
*                   /* INITIALIZED WITH THE ADDRESS OF THE MESSAGE   */ 00145000
*                   /* AND R8 WITH THE LENGTH OF THE MESSAGE. THE    */ 00146000
*                   /* MESSAGE IS CHECKED TO SEE IF IT EXCEEDS 126   */ 00147000
*                   /* BYTES (MAX LENGTH). IF IT DOES IT IS BROKEN UP*/ 00148000
*                   /* INTO 126 BYTE SEGMENTS OR LESS DEPENDING WHERE*/ 00149000
*                   /* THE LAST FULL WORD ENDS. EACH SEGMENT WILL BE */ 00150000
*                   /* PUT TO THE SYSTEM MSG DATA SET UNTIL ALL      */ 00151000
*                   /* SEGMENTS HAVE BEEN EXHAUSTED. THE RPL IS      */ 00152000
*                   /* INITIALIZED AND THE PUT TO THE SYSTEM MSG DATA*/ 00153000
*                   /* SET ISSUED. RETURN CODES FROM JES2 ARE CHECKED*/ 00154000
*                   /* FOR SUCCESSFUL COMPLETION OF THE PUT          */ 00155000
*                   /*                                               */ 00156000
*                   /*************************************************/ 00157000
*                                                                       00158000
*                                   /* R3 -> MCS + MESSAGE TEXT      */ 00159000
         L     R3,WKAADTXT                                              00160000
         LA    R3,4(,R3)               R3 -> MESSAGE TEXT               00161000
*                                   /* GET MSG TEXT LENGTH INTO R8   */ 00162000
         LH    R8,WKALGH                                                00163000
         SH    R8,KH4                  SUBTRACT FOR MCS HEADER          00164000
*                   DO WHILE(R8^=0);/* AS LONG AS A MSG SEGMENT         00165000
*                                      EXIST CONTINUE PROCESSING     */ 00166000
         B     @DE00167                                                 00167000
*                                                                       00168000
*                     CALL CHECKMSG;/* CHECK IF MSG > 126 BYTES      */ 00169000
@DL00167 BAL   R14,CHECKMSG                                             00170000
*                     CALL BUILDRPL;/* INITIALZE RPL FOR PUT         */ 00171000
         BAL   R14,BUILDRPL            RPL ADDR RETURNED IN R1          00172000
*                                   /* ISSUE PUT TO SYSTEM MSG DATASET  00173000
         LTR   R1,R1                   RPL ADDR ZERO ?                  00174000
         BZ    IGC000F6                YES, BRANCH                      00175000
         USING IFGRPL,R1                                                00176000
         ICM   R5,B'1111',RPLDACB      R5 -> DATA ACB                   00177000
         BZ    IGC000F6                ACB ADDRESS ZERO ? YES, BRANCH   00178000
         USING IFGACB,R5                                                00179000
         TM    ACBOFLGS,ACBOPEN        ACB OPEN ?                       00180000
         BZ    IGC000F6                NO, BRANCH                       00181000
         ICM   R15,B'1111',ACBINRTN    INTERFACE ROUTINE ADDR ZERO ?    00182000
         BZ    IGC000F6                YES, BRANCH                      00183000
*                                                                       00184000
         PUT   RPL=(1)                 WRITE MESSAGE TO SYSTEM MSG DS   00185000
*                                                                       00186000
         DROP  R1,R5                                                    00187000
*                                                                       00188000
         B     IGC000FA                                                 00189000
*                                                                       00190000
IGC000F6 LA    R15,4                   SET ERROR RETURN CODE            00191000
IGC000FA BAL   R14,CKRETURN                                             00192000
*                     CALL CKRETURN;/* CHECK IF PUT SUCCESSFUL       */ 00193000
*                   END;                                                00194000
@DE00167 LTR   R8,R8                   ANY MORE MESSAGE TEXT TO PROC ?  00195000
         BP    @DL00167                YES, LOOP BACK                   00196000
*                   R8=R15;         /* SAVE RETURN CODE              */ 00197000
         LR    R8,R15                                                   00198000
*                   CALL ISSUEDEQ;  /* RELEASE RESOURCE              */ 00199000
         BAL   R14,ISSUEDEQ                                             00200000
*                   R15=R8;         /* RESTORE RETURN CODE           */ 00201000
         LR    R15,R8                                                   00202000
*                 END;                                                  00203000
*             END;                                                      00204000
*         END;                                                          00205000
*                                                                       00206000
*       /*************************************************************/ 00207000
*       /*                                                           */ 00208000
*       /* IF AN ERROR OCCURRED DURING THE PUT TO JES2 OR THE RPL    */ 00209000
*       /* POINTER WAS ZERO AN ERROR MSG WILL BE ISSUED VIA WTO TO   */ 00210000
*       /* HARD COPY                                                 */ 00211000
*       /*                                                           */ 00212000
*       /*************************************************************/ 00213000
*                                                                       00214000
*       IF R15^=0 THEN              /* CK IF I/O ERROR OCCURRED      */ 00215000
@RF00151 LTR   R15,R15                                                  00216000
         BZ    @RF00180                                                 00217000
*         DO;                                                           00218000
*           CALL BUILDMSG;          /* CREATE WTO MSG FOR HARDCOPY   */ 00219000
         BAL   R14,BUILDMSG                                             00220000
*           CALL ISSUEMSG;          /* ISSUE WTO TO HARD COPY        */ 00221000
         BAL   R14,ISSUEMSG                                             00222000
*         END;                                                          00223000
*                                                                       00224000
*/********************************************************************* 00225000
*                                                                       00226000
*        TO DETERMINE WHETHER TO TURN ON XVD0USER BIT (RETURN TO        00227000
*        USER BIT) CHECKS ARE MADE FOR ADDITIONAL ROUTE CODES ON        00228000
*        THE MESSAGE AND IF ANY CONSOLES RECEIVE ROUTE CODE 11          00229000
*        MESSAGES.                                                      00230000
*        FLAG BITS IN THE WPL ARE ALSO CHECKED                          00231000
*        IF ANY OF THE ABOVE CONDITIONS ARE POSITIVE RETURN TO          00232000
*        WTO WITHOUT TURNING ON XVD0USER BIT.                           00233000
*                                                                       00234000
*/********************************************************************* 00235000
*                                                                       00236000
*       R1=0;                                                           00237000
@RF00180 SLR   R1,R1                                                    00238000
*STAERETY:                                                              00239000
*       IF R1^=0 THEN               /* STAE RETRY WILL ENTER HERE    */ 00240000
STAERETY LTR   R1,R1                                                    00241000
         BZ    @RF00189                                                 00242000
*         DO;                       /* R1 WILL POINT TO MY REGS      */ 00243000
*                                                                       00244000
*        RESTORE NEEDED REGISTER FROM STAEPARM AREA                     00245000
*                                                                       00246000
         LM    R9,R13,0(R1)                                             00247000
*         END;                                                          00248000
*       CALL FINALCK;               /* CK FOR ADDITIONAL WTO WORK    */ 00249000
@RF00189 BAL   R14,FINALCK                                              00250000
*                                   /* CLEAN UP ESTAE ENVIRONMENT    */ 00251000
         ESTAE 0                                                        00252000
*                                                                       00253000
         B     @EL00001                                                 00254000
*                                                                       00255000
*     END;                                                              00256000
*   ELSE                                                                00257000
*     CALL FINALCK;                 /* NO ESTAE, CK IF MORE WTO WK   */ 00258000
@RF00148 BAL   R14,FINALCK                                              00259000
*   RETURN;                         /* RETURN WTO                    */ 00260000
@EL00001 LR    R1,R13                                                   00261000
         L     R13,4(,R13)                                              00262000
         L     R0,WORKIGCG                                              00263000
*                                                                       00264000
         FREEMAIN R,LV=(0),A=(1)                                        00265000
*                                                                       00266000
         RETURN (14,12)                                                 00267000
*                                                                       00268000
*   /*****************************************************************/ 00269000
*   /*                                                               */ 00270000
*   /* GETESTAE                                                      */ 00271000
*   /*                                                               */ 00272000
*   /* BUILD A PARAMETER LIST FOR ESTAE AND ISSUE THE ESTAE MACRO    */ 00273000
*   /*                                                               */ 00274000
*   /*****************************************************************/ 00275000
*                                                                       00276000
*GETESTAE:                                                              00277000
*   MSGID='4';                      /* INITIALIZE MSG ISSUER ID TO AN   00278000
*                                      UNPREDICATABLE ERROR          */ 00279000
GETESTAE MVI   MSGID,C'4'                                               00280000
*                                   /* SAVE REGISTERS REQUIRED       */ 00281000
*                                   /* IN STAE EXIT                  */ 00282000
         STM   R9,R13,REGSTAE                                           00283000
*          0(1:ESTAELEN)=ELIST(1:ESTAELEN);/* MOVE ESTAE LIST FORM      00284000
*                                      INTO WORKAREA                 */ 00285000
         MVC   STAELIST(ESTAELEN),ELIST                                 00286000
*   R3=ADDR(STAE000);               /* GET STAE EXIT ROUTINE ADDR    */ 00287000
         LA    R3,STAE000                                               00288000
*   R2=ADDR(STAEPARM);              /* GET PARM LIST ADDR            */ 00289000
         LA    R2,STAEPARM                                              00290000
*                                                                       00291000
         ESTAE 0                                                        00292000
*                                                                       00293000
         SLR   R15,R15                                                  00294000
         B     @ER00002                                                 00295000
*                                                                       00296000
         ESTAE (3),CT,RECORD=YES,PARAM=(2),MF=(E,STAELIST)              00297000
*                                                                       00298000
*   END;                                                                00299000
@ER00002 BR    R14                     RETURN TO CALLER WITH RC IN R15  00300000
*                                                                       00301000
*   /*****************************************************************/ 00302000
*   /*                                                               */ 00303000
*   /* CHECKMSG                                                      */ 00304000
*   /*                                                               */ 00305000
*   /* CHECK IF THE MESSAGE EXCEEDS 126 CHARACTERS                   */ 00306000
*   /* IF IT DOES THE MESSAGE WILL BE BROKEN UP INTO 126 OR LESS     */ 00307000
*   /* SEGMENTS DEPENDING ON WHERE THE LAST FULL WORD ENDS. THE      */ 00308000
*   /* MESSAGE WILL BE ISSUED SEGMENT BY SEGMENT UNTIL THE ENTIRE    */ 00309000
*   /* MESSAGE HAS BE PUT TO THE SYSTEM MESSAGE DATA SET             */ 00310000
*   /*                                                               */ 00311000
*   /* ON ENTRY -                                                    */ 00312000
*   /* R3 -> MESSAGE TEXT                                            */ 00313000
*   /* R8  = L'MESSAGE TEXT                                          */ 00314000
*   /*                                                               */ 00315000
*   /* ON EXIT                                                       */ 00316000
*   /* R7 -> MESSAGE TEXT                                            */ 00317000
*   /* R8  = L'MESSAGE TEXT                                          */ 00318000
*   /*                                                               */ 00319000
*   /*****************************************************************/ 00320000
*                                                                       00321000
*CHECKMSG:                                                              00322000
*   R7=R3;                          /* R7 -> MSG                     */ 00323000
CHECKMSG LR    R7,R3                                                    00324000
*   IF R8>126 THEN                  /* L'MSG > 126 THEN DO           */ 00325000
         CH    R8,KH126                                                 00326000
         BNH   @RF00218                                                 00327000
*     DO;                                                               00328000
         LA    R2,125(,R7)             R2 -> 126TH BYTE OF MESSAGE TEXT 00329000
*       DO WHILE(R2->MESSAGE(1)^=' '&R2^=R7);/* SCAN FOR BLANK          00330000
*                                      CHAR OR BEGINNING ADDR OF MSG */ 00331000
         B     @DE00221                                                 00332000
*                                                                       00333000
*         R2=R2-1;                  /* DECREMENT R2 FOR BACKWARD SCAN*/ 00334000
@DL00221 BCTR  R2,0                                                     00335000
*       END;                                                            00336000
@DE00221 CLI   0(R2),C' '              FOUND A BLANK ?                  00337000
         BE    @DC00221                YES, BRANCH                      00338000
         CR    R2,R7                   NO, R2 -> FIRST BYTE ?           00339000
         BNE   @DL00221                NO, CONTINUE TO SCAN BACKWARDS   00340000
*       IF R2=R7 THEN               /* CK IF SCAN WAS TERMINATED        00341000
*                                      BECAUSE BLANK WASNT FOUND     */ 00342000
@DC00221 CR    R2,R7                   R2 -> FIRST BYTE ?               00343000
         BNE   @RF00224                NO, BRANCH                       00344000
*         R2=R7+125;                /* YES, THEN POINT R2 AT THE        00345000
*                                      126TH BYTE OF MSG             */ 00346000
         LA    R2,125(,R7)                                              00347000
*       R3=R2+1;                    /* ELSE R3 POINTS TO CHAR           00348000
@RF00224 LA    R3,1(,R2)               R3 -> NEXT CHAR TO BE PROCESSED  00349000
         LR    R5,R2                                                    00350000
         SLR   R5,R7                   SUBTRACT START ADDR FROM LAST    00351000
*                                      CHAR + 1                         00352000
         LA    R2,1(,R5)               CALC PROCESSED LENGTH            00353000
         SLR   R8,R2                   SUBTRACT PROCESSED LENGTH        00354000
*                                      FROM TOTAL LENGTH OF MESSAGE     00355000
*       R2=R2-R7+1;                 /* COMPUTE LENGTH TO PUT OUT     */ 00356000
         B     @ER00003                                                 00357000
*                                                                       00358000
*     END;                                                              00359000
*   ELSE                                                                00360000
*     DO;                                                               00361000
*       R2=R8;                      /* IF LENGTH <126 THEN PUT LENGTH   00362000
*                                      INTO R2                       */ 00363000
@RF00218 LR    R2,R8                                                    00364000
*       R8=0;                       /* NO MORE MSG SEGMENTS LEFT PUT    00365000
*                                      ZERO LENGTH INTO R8           */ 00366000
         SLR   R8,R8                                                    00367000
*     END;                                                              00368000
*   END;                                                                00369000
@ER00003 BR    R14                                                      00370000
*                                                                       00371000
*   /*****************************************************************/ 00372000
*   /*                                                               */ 00373000
*   /* CKROUTCD                                                      */ 00374000
*   /*                                                               */ 00375000
*   /* THE MESSAGE IS CHECKED FOR ROUTE CODES OTHER THAN 11. IF THEY */ 00376000
*   /* EXIST THIS MODULE RETURNS TO WTO WITHOUT TURNING ON XVD0USER  */ 00377000
*   /* (RETURN TO USER BIT IN XSA). IF NO OTHER ROUTE CODES EXIST    */ 00378000
*   /* THEN THE UCM TABLE IS SCANNED TO DETERMINE IF ANY CONSOLES ARE*/ 00379000
*   /* TO RECEIVE ROUTE CODE 11 MESSAGES IF SO THEN THIS MODULE      */ 00380000
*   /* RETURNS TO WTO WITHOUT TURNING ON XVD0USER BIT                */ 00381000
*   /*                                                               */ 00382000
*   /*****************************************************************/ 00383000
*                                                                       00384000
*CKROUTCD:                                                              00385000
*   IF XVRCSAVE='0020'X THEN        /* OTHER ROUTE CODES WITH MSG ?  */ 00386000
CKROUTCD CLC   WKAROC,WTPONLY          YES, BRANCH                      00387000
         BNE   @RF00237                                                 00388000
*     DO;                           /* NO, CHECK CONSOLES FOR ANY       00389000
*                                      THAT RECEIVE ROUTE CODE 11 MSG*/ 00390000
         L     R15,ADDRUCM                                              00391000
         USING UCM,R15                                                  00392000
*       R7=UCMVEA;                  /* -> FIRST UCM                  */ 00393000
         L     R7,UCMVEA                                                00394000
*       R8=UCMVEZ;                  /* L'EACH UCM                    */ 00395000
         L     R8,UCMVEZ                                                00396000
*       R3=UCMVEL;                  /* -> LAST UCM                   */ 00397000
         L     R3,UCMVEL                                                00398000
         DROP  R15                                                      00399000
*       DO WHILE(WTPROUTE^='1'B&R7<R3);/* SCAN ALL UCMS FOR ANY         00400000
*                                      THAT RECEIVE ROUTE CODE 11       00401000
*                                      MSGS                          */ 00402000
         B     @DE00242                                                 00403000
*                                                                       00404000
*         R7=R7+R8;                 /* INCREMENT R7 TO NEXT UCM      */ 00405000
@DL00242 LA    R7,0(R7,R8)                                              00406000
*       END;                                                            00407000
         USING UCMLIST,R7              ADDRESSABILITY FOR UCM DEVICE    00408000
@DE00242 TM    UCMRTCD+1,WPLROUTK      WTP ROUTE CODE 11 FOR THIS CON ? 00409000
         BO    @DC00242                                                 00410000
         CR    R7,R3                                                    00411000
         BL    @DL00242                                                 00412000
*       IF WTPROUTE='1'B THEN       /* ANY CONSOLES RECEIVE CODE 11  */ 00413000
@DC00242 TM    UCMRTCD+1,WPLROUTK                                       00414000
         BNO   @RF00245                                                 00415000
         DROP  R7                                                       00416000
*         R15=0;                    /* YES, THEN RC = RETURN TO WTO  */ 00417000
         SLR   R15,R15                                                  00418000
         B     @ER00004                                                 00419000
*                                                                       00420000
*       ELSE                                                            00421000
*         R15=4;                    /* NO, RC = RETURN TO USER       */ 00422000
@RF00245 LA    R15,4                                                    00423000
         B     @ER00004                                                 00424000
*                                                                       00425000
*     END;                                                              00426000
*   ELSE                                                                00427000
*     R15=0;                        /* OTHER ROUTE CODES RC = RETURN    00428000
*                                      TO WTO                        */ 00429000
@RF00237 SLR   R15,R15                                                  00430000
*   END;                                                                00431000
@ER00004 BR    R14                                                      00432000
*                                                                       00433000
*   /*****************************************************************/ 00434000
*   /*                                                               */ 00435000
*   /* ISSUTPUT                                                      */ 00436000
*   /*                                                               */ 00437000
*   /* A CHECK IS MADE TO SEE IF THE TSO USER WANTS WTP MESSAGES     */ 00438000
*   /* SENT TO THE TERMINAL. IF YES THEN THE TPUT MACRO IS ISSUED TO */ 00439000
*   /* THE TERMINAL. THE RC FROM TPUT IS PASSED BACK TO MAINLINE     */ 00440000
*   /* CODE. IF THE USER DOES NOT WANT WTP MESSAGES,     RETURN TO   */ 00441000
*   /* MAINLINE WITH A RC OF 4 IN R15                                */ 00442000
*   /*                                                               */ 00443000
*   /* ON RENTRY -                                                   */ 00444000
*   /* R2 -> ACTIVE JSCB                                             */ 00445000
*   /*                                                               */ 00446000
*   /*****************************************************************/ 00447000
*                                                                       00448000
*ISSUTPUT:                                                              00449000
*   IF JSCBPSCB^=0&UPTWTP='1'B THEN /* TSO USER WANT WTP MSGS ?      */ 00450000
         USING IEZJSCB,R2                                               00451000
ISSUTPUT ICM   R1,B'1111',JSCBPSCB     R1 -> TSO PROTECTED STEP CNTL    00452000
         BZ    @RF00252                PRESENT ? ,NO, NOT TSO CALLER    00453000
         DROP  R2                                                       00454000
         USING PSCB,R1                 YES                              00455000
         L     R1,PSCBUPT              R1 -> USER PROFILE TABLE         00456000
         DROP  R1                                                       00457000
         USING UPT,R1                                                   00458000
         TM    UPTSWS,UPTWTP           WTP MSGS DESIRED ?               00459000
         DROP  R1                                                       00460000
         BNO   @RF00252                NO, BRANCH                       00461000
*     DO;                           /* YES, ISSUE TPUT               */ 00462000
*       R1=ADDR(WPLTXT);            /* R1 -> MCS + MESSAGE TEXT      */ 00463000
         LA    R1,WKAADTXT                                              00464000
         LA    R1,4(,R4)               R1 -> MESSAGE TEXT               00465000
*       R0=XVMSGLGH-4;              /* R0 = L'MCS + MESSAGE          */ 00466000
         LH    R0,WKALGH               R0 = L'MESSAGE                   00467000
         SH    R0,KH4                                                   00468000
*                                   /* ISSUE TPUT TO TERMINAL        */ 00469000
         TPUT  (1),(0),R                                                00470000
*                                                                       00471000
         B     @ER00005                                                 00472000
*                                                                       00473000
*     END;                                                              00474000
*   ELSE                                                                00475000
*     R15=4;                        /* PUT TO JOB'S MSG DATA SET     */ 00476000
@RF00252 LA    R15,4                                                    00477000
*   END;                                                                00478000
@ER00005 BR    R14                     RETURN TO CALLER WITH RC IN R15  00479000
*                                                                       00480000
*   /*****************************************************************/ 00481000
*   /*                                                               */ 00482000
*   /* CHECKJOB                                                      */ 00483000
*   /*                                                               */ 00484000
*   /* IF THIS IS THE FIRST TIME A WTP IS ISSUED BY THIS JOB STEP    */ 00485000
*   /* THEN THE JSCB MUST BE INITIALIZED. IF AN OLD STEP THAT HAS    */ 00486000
*   /* ISSUED PREVIOUS WTPS THEN RETURN TO MAINLINE CODE             */ 00487000
*   /*                                                               */ 00488000
*   /*****************************************************************/ 00489000
*                                                                       00490000
*CHECKJOB:                                                              00491000
*   IF JSCBWTSP^=JSCBSTEP THEN      /* JOB BEEN HERE BEFORE ?        */ 00492000
CHECKJOB L     R15,ACTJSCB                                              00493000
         USING IEZJSCB,R15                                              00494000
         CLC   JSCBWTSP,JSCBSTEP                                        00495000
         BE    @ER00006                YES, BRANCH                      00496000
*     DO;                           /* NO, THEN INITIALIZE JSCB      */ 00497000
*       JSCBWTP=0;                  /* ZERO OUT SWITCHES             */ 00498000
         XC    JSCBWTP,JSCBWTP                                          00499000
*       JSCBWTSP=JSCBSTEP;          /* MOVE STEP NUM INTO JSCB       */ 00500000
         MVC   JSCBWTSP,JSCBSTEP                                        00501000
         DROP  R15                                                      00502000
*     END;                                                              00503000
*   END;                                                                00504000
@ER00006 BR    R14                     RETURN TO CALLER                 00505000
*                                                                       00506000
*   /*****************************************************************/ 00507000
*   /*                                                               */ 00508000
*   /* BUILDRPL                                                      */ 00509000
*   /*                                                               */ 00510000
*   /* THIS ROUTINE OBTAINS THE RPL ADDRESS FROM THE JSCB. THE       */ 00511000
*   /* REQUIRED FIELDS WITHIN THE RPL ARE INITIALIZED                */ 00512000
*   /*                                                               */ 00513000
*   /* ON ENTRY -                                                    */ 00514000
*   /* R7 -> MESSAGE TEXT                                            */ 00515000
*   /* R2  = L'MESSAGE TEXT                                          */ 00516000
*   /*                                                               */ 00517000
*   /* ON EXIT                                                       */ 00518000
*   /* R1 -> RPL                                                     */ 00519000
*   /*                                                               */ 00520000
*   /*****************************************************************/ 00521000
*                                                                       00522000
*BUILDRPL:                                                              00523000
BUILDRPL L     R15,ACTJSCB                                              00524000
         USING IEZJSCB,R15                                              00525000
*   R1=JSCBSMLR;                    /* GET PTR TO RPL                */ 00526000
         ICM   R1,B'1111',JSCBSMLR     R1 -> SYSTEM MESSAGE DATA SET    00527000
         BZ    @ER00007                SYSTEM MESSAGE DATA SET AVAIL ?  00528000
         DROP  R15                                                      00529000
         USING IFGRPL,R1                                                00530000
*   RPLAREA=R7;                     /* MSG LOCATION INTO RPL         */ 00531000
         ST    R7,RPLAREA                                               00532000
*   RPLRLEN=R2;                     /* MSG LENGTH INTO RPL           */ 00533000
         ST    R2,RPLRLEN                                               00534000
*   RPLREQ=RPLPUT;                  /* INDICATE PUT REQUEST          */ 00535000
         MVI   RPLREQ,RPLPUT                                            00536000
*   RPLSEQ='1'B;                    /* PUT AFTER LAST MSG IN SYSTEM     00537000
*                                      MSG DATA SET                  */ 00538000
         OI    RPLOPTCD,RPLSEQ                                          00539000
         DROP  R1                                                       00540000
*   RETURN;                                                             00541000
@ER00007 BR    R14                                                      00542000
*   END;                                                                00543000
*                                                                       00544000
*   /*****************************************************************/ 00545000
*   /*                                                               */ 00546000
*   /* THIS ROUTINE CHECKS THE RETURN CODES FROM HASP. IF REG 15 IS  */ 00547000
*   /* NOT 0 THEN AN ERROR OCCURRED. REG 8 IS ZEROED TO TERMINATE ANY*/ 00548000
*   /* FURTHER PROCESSING OF MESSAGES AND THE ERROR MSG ID FIELD IS  */ 00549000
*   /* CHANGED TO 3 TO INDICATE A PUT ERROR IN THE WTO MSG.          */ 00550000
*   /*                                                               */ 00551000
*   /*****************************************************************/ 00552000
*                                                                       00553000
*CKRETURN:                                                              00554000
*   IF R15^=0 THEN                  /* IF PROBLEMS OCCURRED          */ 00555000
CKRETURN LTR   R15,R15                                                  00556000
         BZ    @ER00008                                                 00557000
*     DO;                                                               00558000
*       R8=0;                       /* ZERO OUT R8 TO TERMINATE DO      00559000
*                                      LOOP                          */ 00560000
         SLR   R8,R8                                                    00561000
*       MSGID='3';                  /* SET MSG ID TO INDICATE A PUT     00562000
*                                      FAILURE                       */ 00563000
         MVI   MSGID,C'3'                                               00564000
*     END;                                                              00565000
*   RETURN;                                                             00566000
@ER00008 BR    R14                                                      00567000
*   END;                                                                00568000
*                                                                       00569000
*   /*****************************************************************/ 00570000
*   /*                                                               */ 00571000
*   /* ISSUEENQ                                                      */ 00572000
*   /*                                                               */ 00573000
*   /* THIS ROUTINE CHECKS FOR A ZERO RPL POINTER. IF ZERO THE ENQ IS*/ 00574000
*   /* NOT ISSUED AND THE ERROR MSG ID IS SET TO 1 TO INDICATE A ZERO*/ 00575000
*   /* RPL POINTER ERROR IN THE WTO MESSAGE TO HARD COPY. IF AN RPL  */ 00576000
*   /* POINTER IS PRESENT THIS ROUTINE MOVES THE ENQUEUE PARM LIST   */ 00577000
*   /* INTO THIS MODULES WORKAREA. IT THEN BUILDS AN RNAME CONSISTING*/ 00578000
*   /* OF THE RPL POINTER (JSCBSMLR) AND THE AND THE ASCBASID.IT THEN*/ 00579000
*   /* ISSUES A UNCONDITIONAL ENQUEUE                                */ 00580000
*   /*                                                               */ 00581000
*   /*****************************************************************/ 00582000
*                                                                       00583000
*ISSUEENQ:                                                              00584000
*   R1=ADDR(WORKAREA);              /* WORKAREA ADDR INTO R1         */ 00585000
ISSUEENQ LA    R1,WORKAREA                                              00586000
*   ENQAREA(1:LENENQ)=ENQLIST(1:LENENQ);/* MOVE ENQ LIST INTO WORK   */ 00587000
         MVC   WORKAREA(LENENQ),ENQLIST                                 00588000
         L     R15,ACTJSCB             R15 -> ACTIVE JSCB               00589000
         USING IEZJSCB,R15                                              00590000
*   IF JSCBSMLR^=0 THEN             /* RPL AVAILABLE FOR SYSTEM MSG ?*/ 00591000
         ICM   R15,B'1111',JSCBSMLR                                     00592000
         BZ    @RF00293                NO, BRANCH                       00593000
         DROP  R15                                                      00594000
*     DO;                                                               00595000
*       ENQRNAME(1:4)=JSCBSMLR;     /* YES,BUILD RNAME WITH RPL PTR  */ 00596000
         ST    R15,ENQRNAME                                             00597000
*       ENQRNAME(5:6)=ASCBASID;     /* AND ASID FROM ASCB            */ 00598000
         MVC   ENQRNAME+4(2),ASCBASID-ASCB(R7)                          00599000
*       RNAMEFLD=ADDR(ENQRNAME);    /* PUT RNAME ADDR INTO LIST      */ 00600000
         LA    R15,ENQRNAME                                             00601000
         ST    R15,8(,R1)                                               00602000
*                                   /* ISSUE UNCONDITIONAL ENQ       */ 00603000
         ENQ   ,MF=(E,(1))                                              00604000
*                                                                       00605000
         LTR   R15,R15                 TEST RETURN CODE FROM ENQ        00606000
         BZ    @ER00009                ZERO, ALL OK                     00607000
         CLI   3(R15),8                RETURN CODE 8 ?                  00608000
         BE    IGC0032A                                                 00609000
         CLI   3(R15),20               RETURN CODE 20 ?                 00610000
         BNE   @ER00009                                                 00611000
IGC0032A MVI   XVRETCOD,48                                              00612000
         MVI   MSGID,C'2'                                               00613000
         B     @ER00009                                                 00614000
*                                                                       00615000
*     END;                                                              00616000
*   ELSE                                                                00617000
*     DO;                           /* NO RPL PTR MEANS NO ENQ       */ 00618000
*       R15=4;                      /* RC INDICATES ENQ WOULD FAIL   */ 00619000
@RF00293 LA    R15,4                                                    00620000
*       MSGID='1';                  /* INDICATES WHY ENQ FAILED      */ 00621000
         MVI   MSGID,C'1'                                               00622000
*     END;                                                              00623000
*   END;                                                                00624000
@ER00009 BR    R14                                                      00625000
*                                                                       00626000
*   /*****************************************************************/ 00627000
*   /*                                                               */ 00628000
*   /* BUILDMSG                                                      */ 00629000
*   /*                                                               */ 00630000
*   /* GETS 53 BYTES OF THE MESSAGE THAT WAS PASSED TO WTP           */ 00631000
*   /* THE LENGTH WILL BE ADJUSTED TO A MAXIMUM OF 53 BYTES          */ 00632000
*   /*                                                               */ 00633000
*   /* ON EXIT -                                                     */ 00634000
*   /* R3 -> MESSAGE TEXT                                            */ 00635000
*   /* R6  = L'MESSAGE TEXT                                          */ 00636000
*   /*                                                               */ 00637000
*   /*****************************************************************/ 00638000
*                                                                       00639000
*BUILDMSG:                                                              00640000
*   R3=ADDR(WPLTXT);                /* R3 -> MCS + MESSAGE TEXT      */ 00641000
BUILDMSG L     R3,WKAADTXT             R3 -> MESSAGE TEXT               00642000
         LA    R3,4(,R3)                                                00643000
*   R6=XVMSGLGH-4;                  /* GET MSG LENGTH                */ 00644000
         LH    R6,WKALGH                                                00645000
         SH    R6,KH4                  SUBTRACT FOR MCS OVERHEAD        00646000
*   IF R6>53 THEN                   /* MSG EXCEED MAX LENGTH            00647000
*                                      THAT WE CAN PUT OUT           */ 00648000
         LA    R15,53                                                   00649000
         CR    R6,R15                  L'TEXT > 53 ?                    00650000
         BNH   @ER00011                NO, BRANCH TO EXIT               00651000
*     DO;                                                               00652000
         LR    R6,R15                  TRUNCATE TO 53 BYTES             00653000
*   END;                                                                00654000
@ER00011 BR    R14                                                      00655000
*                                                                       00656000
*   /*****************************************************************/ 00657000
*   /*                                                               */ 00658000
*   /* ISSUEMSG                                                      */ 00659000
*   /*                                                               */ 00660000
*   /* THIS ROUTINE MOVES THE LIST FORM OF THE MSG TO WTP WORKAREA   */ 00661000
*   /* AND THEN MOVES IN THE 55 BYTES FROM THE MSG SENT TO WTP BEFORE*/ 00662000
*   /* THE ABEND OCCURRED. IT THEN ISSUES THE WTO                    */ 00663000
*   /*                                                               */ 00664000
*   /* ON ENTRY -                                                    */ 00665000
*   /* R3 -> MESSAGE TEXT                                            */ 00666000
*   /* R6  = L'MESSAGE TEXT (MAX 53 BYTES)                           */ 00667000
*   /*                                                               */ 00668000
*   /*                                                               */ 00669000
*   /*****************************************************************/ 00670000
*                                                                       00671000
*ISSUEMSG:                                                              00672000
ISSUEMSG MVC   WORKAREA(IEF170IL),IEF170I                               00673000
*   LENGTH=R6+L'IEF170I;            /* UPDATE LENGTH                 */ 00674000
         LR    R15,R6                                                   00675000
         AH    R15,WORKAREA            ADD L'IEF170I MESSAGE            00676000
         STH   R15,WORKAREA            UPDATE L'WTO MESSAGE             00677000
*   MSGID2(1:R6)=R3->MESSAGE(1:R6);/* MOVE MSG TO WORKAREA           */ 00678000
         LR    R15,R6                                                   00679000
         BCTR  R15,0                                                    00680000
         EX    R15,@SM02290            MVC   WORKAREA+23(0),0(R3)       00681000
*   R7=ADDR(MSGID2)+R6;             /* R7 -> END OF MESSAGE          */ 00682000
         LA    R7,WORKAREA+23(R6)                                       00683000
*   R7->MESSAGE=IEF170I+23;         /* MOVE IN DESC AND ROUTE CODES  */ 00684000
         MVC   0(4,R7),IEF170I+23                                       00685000
*   IF TCBTIO^=0 THEN               /* TIOT EXIST ?                  */ 00686000
         L     R15,TCBADDR                                              00687000
         USING TCB,R15                                                  00688000
         ICM   R15,B'1111',TCBTIO      HAVE TIOT ADDR ?                 00689000
         BZ    @RF00337                NO, BRANCH                       00690000
         DROP  R15                                                      00691000
*     JOBNAME=TIOCNJOB;             /* YES, MOVE JOBNAME TO MSG      */ 00692000
         USING TIOT,R15                                                 00693000
         MVC   WORKAREA+14(L'TIOCNJOB),TIOCNJOB                         00694000
         DROP  R15                                                      00695000
*   IDENT=MSGID;                    /* INDICATE WHY MSG OCCURRED     */ 00696000
@RF00337 MVC   WORKAREA+12(L'MSGID),MSGID                               00697000
*                                   /* ISSUE WTO                     */ 00698000
         WTO   MF=(E,WORKAREA)                                          00699000
*   END;                                                                00700000
@ER00012 BR    R14                                                      00701000
*                                                                       00702000
*   /*****************************************************************/ 00703000
*   /*                                                               */ 00704000
*   /* THIS ROUTINE IS CALLED BY THE STAE EXIT ROUTINE TO DEQUEUE ANY*/ 00705000
*   /* RESOURCES THAT MAY HAVE BEEN ENQUEUED ON                      */ 00706000
*   /*                                                               */ 00707000
*   /*****************************************************************/ 00708000
*                                                                       00709000
*ISSUEDEQ:                                                              00710000
*   R1=ADDR(WORKAREA);                                                  00711000
ISSUEDEQ LA    R1,WORKAREA             R1 -> WORKAREA                   00712000
*   WORKAREA(1:LENDEQ)=DEQLIST(1:LENDEQ);/* MOVE IN LIST FORM OF DEQ */ 00713000
         MVC   WORKAREA(LENDEQ),DEQLIST                                 00714000
*   RNAMEFLD=ADDR(ENQRNAME);        /* GET ADDR OF RNAME INTO LIST   */ 00715000
         LA    R15,ENQRNAME                                             00716000
         ST    R15,8(,R1)                                               00717000
*                                   /* ISSUE CONDITIONAL DEQ         */ 00718000
         DEQ   ,MF=(E,(1))                                              00719000
*   END;                                                                00720000
@ER00013 BR    R14                                                      00721000
*                                                                       00722000
*   /*****************************************************************/ 00723000
*   /*                                                               */ 00724000
*   /* CKMCSFLG                                                      */ 00725000
*   /*                                                               */ 00726000
*   /* CHECK THE MCS FLAGS IN THE WPL. FLAGS                         */ 00727000
*   /* WPLMCSFB,WPLMCSFD, AND WPLMCSFH ARE CHECKED. ADDITIONALLY THE */ 00728000
*   /* WTOR BIT IN CXSA (XVD2WTOR) IS                                */ 00729000
*   /* CHECKED TO SEE IF THIS WPL IS FOR A WTOR                      */ 00730000
*   /*                                                               */ 00731000
*   /* IF ANY OF THESE ARE ON MORE PROCESSING IS TO BE DONE BY WTO.  */ 00732000
*   /* IF THESE FLAGS ARE NOT ON, A CHECK IS MADE TO SEE IF THIS WTP */ 00733000
*   /* SHOULD BE HARD COPIED. IF YES THEN WTO IS THUSLY INFORMED.    */ 00734000
*   /*                                                               */ 00735000
*   /*****************************************************************/ 00736000
*                                                                       00737000
*CKMCSFLG:                                                              00738000
*   IF WPLMCSFB^='1'B&WPLMCSFD^='1'B&WPLMCSFH^='1'B&XVD2WTOR^='1'B      00739000
CKMCSFLG TM    WKAMCSF,WPLMCSFB+WPLMCSFD+WPLMCSFH                       00740000
         BNZ   @ER00014                                                 00741000
         TM    XVD2,XVD2WTOR                                            00742000
         BO    @ER00014                                                 00743000
*     DO;                           /* IF ALL FLAGS ARE OFF THEN        00744000
*                                      CHECK IF WTP ARE GOING TO HARD   00745000
*                                      COPY                          */ 00746000
*       IF UCMSYSG='1'B|UCMHCUCM^=0 THEN/* CHECK IF HARD COPY EXIST  */ 00747000
         L     R15,ADDRUCM                                              00748000
         SH    R15,KH4                                                  00749000
         L     R15,UCMPRFXP-UCMPRFXP(,R15)    -> UCM PREFIX             00750000
         USING UCMPRFX,R15                                              00751000
         TM    UCMSFLG1,UCMSYSG        HARD COPY DEVICE IS SYSLOG ?     00752000
         BO    @RT00352                YES, BRANCH                      00753000
         ICM   R0,B'1111',UCMHCUCM     -> HARD COPY UCM ENTRY           00754000
*                                      HARD COPY UCM ENTRY PRESENT ?    00755000
         BZ    @RF00352                NO, BRANCH                       00756000
*         DO;                                                           00757000
*           IF RTCODE11='1'B THEN   /* ARE WTP MSGS BEING HARD COPIED*/ 00758000
@RT00352 TM    UCMHRDRT+1,WPLROUTK     ROUTE CODE 11 ?                  00759000
         BZ    @RF00354                NO, BRANCH                       00760000
         DROP  R15                                                      00761000
*             XVD0HDCY='1'B;        /* YES, THEN TELL WTO            */ 00762000
         OI    XVD0,XVD0HDCY                                            00763000
         B     @ER00014                                                 00764000
*                                                                       00765000
*           ELSE                                                        00766000
*             XVD0USER='1'B;        /* NO FLAGS OR NO HARD TELL WTO     00767000
*                                      TO RETURN TO RETURN TO USER   */ 00768000
@RF00354 OI    XVD0,XVD0USER                                            00769000
         B     @ER00014                                                 00770000
*                                                                       00771000
*         END;                                                          00772000
*       ELSE                                                            00773000
*         XVD0USER='1'B;            /* OTHERWISE TELL WTO TO            00774000
*                                      RETURN TO USER                */ 00775000
@RF00352 OI    XVD0,XVD0USER                                            00776000
*     END;                                                              00777000
*   END;                                                                00778000
@ER00014 BR    R14                                                      00779000
*                                                                       00780000
*   /*****************************************************************/ 00781000
*   /*                                                               */ 00782000
*   /* FINALCK                                                       */ 00783000
*   /*                                                               */ 00784000
*   /* CHECK IF ANY CONSOLES RECEIVE ROUTE CODE 11 MESSAGES          */ 00785000
*   /* IF MCS FLAGS B,D,H, ARE ON OR IF ROUTE CODE 11                */ 00786000
*   /* MESSAGES ARE HARDCOPIED. IF ANY OF THE ABOVE CHECKS ARE       */ 00787000
*   /* POSITIVE WTO IS FLAGGED FOR ADDITIONAL PROCESSING. OTHERWISE  */ 00788000
*   /* WTO RETURNS TO CALLER                                         */ 00789000
*   /*                                                               */ 00790000
*   /*****************************************************************/ 00791000
*                                                                       00792000
*FINALCK:                                                               00793000
*   REG14SAV=R14;                   /* SAVE MAINLINE RETURN ADDR     */ 00794000
FINALCK  ST    R14,REG14SAV                                             00795000
*   CALL CKROUTCD;                  /* CK FOR OTHER ROUTE CODES ON      00796000
*                                      MSG OR FOR CONSOLES THAT         00797000
*                                      RECEIVE ROUTE CODE 11 MSGS    */ 00798000
         BAL   R14,CKROUTCD                                             00799000
*   IF R15=4 THEN                   /* IF NONE THEN CK WPL FLAGS     */ 00800000
         CH    R15,KH4                                                  00801000
         BNE   @RF00364                                                 00802000
*     CALL CKMCSFLG;                /* GO CK MCS FLAGS AND HARDCOPY  */ 00803000
         BAL   R14,CKMCSFLG                                             00804000
*   R14=REG14SAV;                   /* RESTORE RETURN ADDR           */ 00805000
@RF00364 L     R14,REG14SAV                                             00806000
*   END;                                                                00807000
@ER00015 BR    R14                                                      00808000
*                                                                       00809000
*********************************************************************** 00810000
*                                                                       00811000
*        STAE EXIT ROUTINE                                              00812000
*                                                                       00813000
*        RECEIVES CONTROL ONLY IF AN ABEND SITUATION OCCURS.            00814000
*        WHEN ENTERED IT RESTORES THIS MODULES BASE REGISTER, XSA       00815000
*        POINTER AND DATA REG                                           00816000
*        53 BYTES OF THE MESSAGE PASSED TO WTP ARE MOVED INTO A         00817000
*        WTO AND ISSUED TO THE HARD COPY                                00818000
*        IF R0 = 0 THEN THE SETRP MACRO IS ISSUED WITH                  00819000
*        RETURN ADDR OF STAERETY                                        00820000
*        IF R0 IS NOT 0 THEN A RC OF 4 IS PLACED INTO R15, THE          00821000
*        RETRY ADDR INTO R0 AND A BR 14 TO STAE ISSUED                  00822000
*                                                                       00823000
*********************************************************************** 00824000
*                                                                       00825000
*STAE000:                                                               00826000
         USING SDWA,R1                                                  00827000
         DROP  R11                     REMOVE R11 AS BASE REG           00828000
         USING STAE000,R15             MAKE R15 BASE REG                00829000
*   R8=R14;                         /* SAVE RETURN ADDRESS IN R8     */ 00830000
STAE000  LR    R8,R14                                                   00831000
*   R11=12;                         /* NEED A 12 FOR COMPARE         */ 00832000
         LA    R11,12                                                   00833000
         CR    R0,R11                  CHECK R0 FOR A 12 RETURN CODE    00834000
         BE    NOSDWA                  IF 12 THEN                       00835000
*                                      R2 -> USER PARAMETER LIST        00836000
         L     R2,SDWAPARM             ELSE OBTAIN USER                 00837000
*                                      PARAMETER LIST FROM THE SWDA     00838000
         LR    R5,R1                   R5 -> SDWA                       00839000
         L     R1,SDWAABCC             GET ABEND COMPLETION CODE        00840000
         DROP  R1                                                       00841000
NOSDWA   SLL   R1,8                    REMOVE HIGH ORDER GARBAGE        00842000
         SRL   R1,20                   SHIFT SYSTEM ABEND CODE          00843000
*                                      INTO LOW ORDER 12 BITS           00844000
*                                                                       00845000
*        RESTORE NEEDED REGISTERS FROM SDWAPARM AREA                    00846000
*                                                                       00847000
         LM    R9,R13,0(R2)                                             00848000
         DROP  R15                     REMOVE R15 AS BASE REG           00849000
         USING IGC0203E,R11            RESTORE R11 AS BASE REG          00850000
         ST    R0,STAEIND                                               00851000
*   CALL ISSUEDEQ;                  /* REMOVE POSSIBLE ENQ           */ 00852000
@RF00378 BAL   R14,ISSUEDEQ                                             00853000
*   CALL BUILDMSG;                  /* SET UP TO ISSUE 53 BYTE ERROR    00854000
*                                      MSG                           */ 00855000
         BAL   R14,BUILDMSG                                             00856000
*   CALL ISSUEMSG;                  /* ISSUE WTO FOR ERROR MSG       */ 00857000
         BAL   R14,ISSUEMSG                                             00858000
*   R1=R5;                          /* RESTORE SDWA POINTER          */ 00859000
         LR    R1,R5                                                    00860000
         USING SDWA,R1                                                  00861000
*   R5=ADDR(STAERETY);              /* GET RETRY ADDRESS             */ 00862000
         LA    R5,STAERETY                                              00863000
*   R14=R8;                         /* RESTORE RETURN ADDR           */ 00864000
         LR    R14,R8                                                   00865000
*   IF STAEIND = 12 THEN            /* CHECK IF SDWA EXIST           */ 00866000
         CLC   STAEIND,KF12                                             00867000
         BNE   @RF00386                                                 00868000
*     DO;                           /* NO, THEN DO THE FOLLOWING     */ 00869000
*       R15=4;                      /* TELL STAE TO RETRY            */ 00870000
         LA    R15,4                                                    00871000
*       R0=R5;                      /* PUT RETRY ADDR INTO R0        */ 00872000
         LR    R0,R5                                                    00873000
         B     @ER00016                                                 00874000
*                                                                       00875000
*     END;                                                              00876000
*   ELSE                                                                00877000
*     DO;                           /* SDWA EXIST CONTINUE HERE      */ 00878000
*       SDWASR01=R2;                /* SAVE MY PARM ADDR IN SDWA     */ 00879000
@RF00386 ST    R2,SDWASR01                                              00880000
*       DO;                         /* SETRP RECORD(YES)RETADDR(STAER   00881000
*                                      ETY)RECPARM(LRECINF)RC(4)RETRE   00882000
*                                      GS(YES)FRESDWA(YES)           */ 00883000
         SETRP RECORD=YES,RETADDR=STAERETY,RECPARM=LRECINF,            X00884000
               RC=4,RETREGS=YES,FRESDWA=YES                             00885000
*                                                                       00886000
*       END;                                                            00887000
*     END;                                                              00888000
*   RETURN;                                                             00889000
@ER00016 BR    R14                                                      00890000
*   END;                                                                00891000
*                                                                       00892000
         DC    0F'0'                                                    00893000
ENQLIST  ENQ   (QN,RN,E,6,SYSTEM),RET=HAVE,MF=L                         00894000
LENENQ   EQU   *-ENQLIST                                                00895000
*                                                                       00896000
ELIST    ESTAE MF=L                                                     00897000
ESTAELEN EQU   *-ELIST                                                  00898000
*                                                                       00899000
DEQLIST  DEQ   (QN,RN,6,SYSTEM),RET=HAVE,MF=L                           00900000
LENDEQ   EQU   *-DEQLIST               L'DEQ LIST                       00901000
*                                                                       00902000
LRECINF  DC    C'IGC0203E'                                              00903000
         DC    C'IGC0203E'                                              00904000
         DC    C'        '                                              00905000
*                                                                       00906000
*   /*****************************************************************/ 00907000
*   /*                                                               */ 00908000
*   /* THIS IS THE MESSAGE SECTION OF THIS MODULE. THE MESSAGE PUT   */ 00909000
*   /* OUT BY THIS MODULE IS CONTAINED HERE AND NOT IN A SEPARATE    */ 00910000
*   /* CSECT. THE MESSAGE IS AN ERROR MESSAGE ISSUED WHEN THERE IS NO*/ 00911000
*   /* RPL POINTER, WHEN A RETURN CODE OF NON ZERO IS RETURNED FROM  */ 00912000
*   /* THE PUT, WHEN THE UNCONDITIONAL ENQUEUE FAILS OR WHEN AN      */ 00913000
*   /* UNPREDICATABLE ABEND OCCURS.                                  */ 00914000
*   /*                                                               */ 00915000
*   /*****************************************************************/ 00916000
*                                                                       00917000
*      THE FIRST BYTE OF THE MESSSAGE IS A REASON CODE FOR WHY THE      00918000
*      MESSAGE WAS ISSUED.                                              00919000
*      1 NO RPL POINTER EXISTED THEREFORE CANNOT ACCESS ACB             00920000
*      2 ENQUEUE TO SERIALIZE EXECUTION OF PUT FAILED                   00921000
*      3 PUT TO SYSTEM MESSAGE DATA SET FAILED                          00922000
*      4 UNPREDICTABLE ABEND                                            00923000
*      THIS IS FOLLOWED BY THE JOBNAME                                  00924000
*                                                                       00925000
IEF170I  WTO   'IEF170I            ',MF=L,ROUTCDE=2,DESC=5,            X00926000
               MCSFLAG=(HRDCPY)                                         00927000
IEF170IL EQU   *-IEF170I           L'WTO                                00928000
*   END                                                                 00929000
*                                                                       00930000
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM      */ 00931000
*/*%INCLUDE SYSLIB  (IHACTM  )                                       */ 00932000
*/*%INCLUDE SYSLIB  (IFGRPL  )                                       */ 00933000
*/*%INCLUDE SYSLIB  (IEECUCM )                                       */ 00934000
*/*%INCLUDE SYSLIB  (IEZWPL  )                                       */ 00935000
*/*%INCLUDE SYSLIB  (IEZJSCB )                                       */ 00936000
*/*%INCLUDE SYSLIB  (IKJTCB  )                                       */ 00937000
*/*%INCLUDE SYSLIB  (IHAASCB )                                       */ 00938000
*/*%INCLUDE SYSLIB  (IEFTIOT1)                                       */ 00939000
*/*%INCLUDE SYSLIB  (IHASDWA )                                       */ 00940000
*/*%INCLUDE SYSLIB  (IHARB   )                                       */ 00941000
*/*%INCLUDE SYSLIB  (IKJRB   )                                       */ 00942000
*/*%INCLUDE SYSLIB  (IKJUPT  )                                       */ 00943000
*/*%INCLUDE SYSLIB  (IKJPSCB )                                       */ 00944000
*                                                                       00945000
KH4      DC    H'4'                                                     00946000
KH10     DC    H'10'                                                    00947000
KH126    DC    H'126'                                                   00948000
KF12     DC    F'12'                                                    00949000
*                                                                       00950000
@SM02286 MVC   WORKAREA(0),ENQLIST                                      00951000
@SM02290 MVC   WORKAREA+23(0),0(R3)                                     00952000
*                                                                       00953000
         DC    0F'0'                                                    00954000
WORKIGCG DC    AL1(231)                SUBPOOL 231                      00955000
         DC    AL3(WORKIGCL)                                            00956000
*                                                                       00957000
WTPONLY  DC    AL2(WPLROUTK)           CHECK FOR ONLY WTP ROUTE CODE    00958000
QN       DC    CL8'SYSZJWTP'                                            00959000
RN       DC    AL1(20)                                                  00960000
*                                                                       00961000
*        WORKAREA GETMAINED BY IGC0203E IN SUBPOOL 231                  00962000
*                                                                       00963000
WORKIGC  DSECT                                                          00964000
SAVEAREA DS    18F                                                      00965000
ADDRUCM  DS    A                                                        00966000
ACTJSCB  DS    A                                                        00967000
STAEIND  DS    A                                                        00968000
TCBADDR  DS    A                                                        00969000
REG6SAVE DS    A                                                        00970000
REG14SAV DS    A                                                        00971000
         DS    0D                                                       00972000
WORKAREA DS    CL132                                                    00973000
*                                                                       00974000
         DS    0F                                                       00975000
STAEPARM DS    CL60                                                     00976000
         ORG   STAEPARM                                                 00977000
REGSTAE  DS    5F                                                       00978000
*                                                                       00979000
         DS    0F                                                       00980000
STAELIST DS    CL16                                                     00981000
ENQRNAME DS    CL8                                                      00982000
MSGID    DS    CL1                                                      00983000
         DS    0D                      ROUND TO DOUBLE WORD             00984000
WORKIGCL EQU   *-WORKIGC                                                00985000
*                                                                       00986000
*        END OF WORKAREA                                                00987000
*                                                                       00988000
         IEZBITS                                                        00989000
*                                                                       00990000
*********************************************************************** 00991000
*                                                                       00992000
*        WORKAREA USED IN MODULE IEAVVWTO                               00993000
*                                                                       00994000
*********************************************************************** 00995000
*                                                                       00996000
WORKVV   DSECT                                                          00997000
@SA00001 DS    18F                                                      00998000
@SAGETB  DS    18F                                                      00999000
@PC00003 DS    F                                                        01000000
@SA00004 DS    F                                                        01001000
@PC00004 DS    F                                                        01002000
@SA00008 DS    15F                                                      01003000
@SA00002 DS    15F                                                      01004000
@PC00002 DS    F                                                        01005000
@AL00001 DS    A                                                        01006000
@TF00001 DS    F                                                        01007000
REG1SAV  DS    F                                                        01008000
REG2SAV  DS    A                  SAVE AREA FOR R2 WHEN DEALING WITH    01009000
*                                 UCM                                   01010000
LONGTXT  DS    A                  -> AREA GETMAINED FOR LONG WPL        01011000
*                                    IF ZERO THEN NO STORAGE GETMAINED  01012000
LONGLEN  DS    F                  L'GETMAINED AREA FOR LONG TEXT        01013000
*                                                                       01014000
LENMWQE  DS    F                  L'OF ALL MINOR WQES                   01015000
*                                 VALID ONLY FOR MLWTO                  01016000
MLWTOXH  DS    A                  -> MLWTO EXTENT HEADER IN CALLERS WPL 01017000
*                                 VALID ONLY FOR MLWTO                  01018000
*                                                                       01019000
*        VERSION 2 XWPL TO STORE ALL USER PROVIDED WPL FIELDS           01020000
*                                                                       01021000
WKALGH   DS    AL2                MESSAGE LENGTH OF MAJOR WQE TEXT FROM 01022000
*                                 CALLER'S WPL                          01023000
WKAMCSF  DS    XL2                MCS FLAGS COPIED FROM CALLERS WPL     01024000
WKAADTXT DS    AL4                -> MESSAGE TEXT                       01025000
WKAVRSN  DS    AL1                XWPL VERSION NUMBER                   01026000
WKAFLAGS DS    XL1                MISC FLAGS                            01027000
WKARPYLN DS    AL1                L'REPLY FOR WTOR                      01028000
WKALNGTH DS    AL1                L'XWPL, ZERO FOR VERSION 1            01029000
WKAMCSF1 DS    XL2                EXTENDED MCS FLAGS                    01030000
WKACPFLF DS    XL2                MCS FLAGS FOR CNTL PROGRAM USE        01031000
WKARPBUF DS    AL4                -> REPLY BUFFER                       01032000
WKAECBP  DS    AL4                -> ECB                                01033000
WKASEQN  DS    AL4                DOM/CONNECT ID                        01034000
WKADSC   DS    XL2             *  DESCRIPTOR CODE TO BE USED IN WTO     01035000
WKAROC   DS    XL2             V  ROUTE CODES TO BE USED IN WTO         01036000
WKAROUT  DS    XL14               EXTENDED ROUTING CODES                01037000
WKAMSGTY DS    XL2                MSGTYPE FLAGS COPIED FROM CALLERS WPL 01038000
WKAPRTY  DS    XL2                MESSAGE PRIORITY                      01039000
WKAJOBID DS    CL8                JOB ID                                01040000
WKAJOBNM DS    CL8                JOB NAME                              01041000
WKAKEY   DS    CL8                RETRIEVAL KEY                         01042000
WKATOKN  DS    AL4                TOKEN FOR DOM                         01043000
WKACNID  DS    AL4                CONSOLE ID                            01044000
WKASYSNA DS    CL8                SYSTEM NAME                           01045000
WKACNNME DS    CL8                CONSOLE NAME                          01046000
WKARCNA  DS    AL4                -> REPLY CONSOLE NAME/ID              01047000
WKACART  DS    AL4                -> CART                               01048000
WKAWSPRM DS    AL4                -> WAIT STATE PARM LIST               01049000
WKAASCB  DS    AL4                -> ASCB                               01050000
WKARSV30 DS    XL16               RESERVED                              01051000
*                                                                       01052000
WKATEXT  DS    CL129              MESSAGE TEXT (MAXIMUM 125 CHARS) +4   01053000
*                                                                       01054000
ESTAEPRM DS    A                                                        01055000
@TS00001 DS    CL1                                                      01056000
STARTID  DS    CL1                                                      01057000
CVDAREA  DS    D                                                        01058000
*                                                                       01059000
PFLAG    DS    XL1                ADDITIONAL FLAGS                      01060000
REQCMSL  EQU   X'40'              REQUEST OR HOLD CMS LOCK              01061000
*                                                                       01062000
*        THE DSECT HAS BEEN TRUNCATED TO AVOID DUPLICATE NAME ISSUES    01063000
*                                                                       01064000
         PRINT NOGEN                                                    01065000
*                                                                       01066000
*        ACB                                                            01067000
*                                                                       01068000
         IFGACB                                                         01069000
*                                                                       01070000
*        RPL                                                            01071000
*                                                                       01072000
         IFGRPL                                                         01073000
*                                                                       01074000
*        TIOT                                                           01075000
*                                                                       01076000
TIOT     DSECT                                                          01077000
*                                                                       01078000
         IEFTIOT1                                                       01079000
*                                                                       01080000
*        JSCB                                                           01081000
*                                                                       01082000
         IEZJSCB                                                        01083000
*                                                                       01084000
*        PSCB                                                           01085000
*                                                                       01086000
         IKJPSCB                                                        01087000
*                                                                       01088000
*        TSO USER PROFILE TABLE                                         01089000
*                                                                       01090000
         IKJUPT                                                         01091000
*                                                                       01092000
*        SYSTEM DIAGNOSTIC WORK AREA                                    01093000
*                                                                       01094000
         IHASDWA DSECT=YES                                              01095000
*                                                                       01096000
*        TCB                                                            01097000
*                                                                       01098000
         IKJTCB LIST=YES                                                01099000
*                                                                       01100000
*        RB                                                             01101000
*                                                                       01102000
         IKJRB   DSECT=YES                                              01103000
*                                                                       01104000
         PRINT GEN                                                      01105000
*                                                                       01106000
*        VARIABLES DEFINED IN THE RBEXSAVE AREA                         01107000
*                                                                       01108000
*/********************************************************************/ 01109000
*/*                                                                  */ 01110000
*/*          EXTENDED SAVEAREA MAPPING FOR SVC 35                    */ 01111000
*/*                                                                  */ 01112000
*/********************************************************************/ 01113000
*                                                                       01114000
*        DEFINED IN THE RBEXSAVE AREA                                   01115000
*                                                                       01116000
         ORG   RBEXSAVE                                                 01117000
XVSAV    DS    0A                                                       01118000
XVA4     DS    0F                      ERROR PROCESSING FIELDS          01119000
XVFNCT   DS    C                       D23 PROCESS CODE                 01120000
D23VALID EQU   X'10'                   PARMLIST VALIDITY CHECK          01121000
D23OREGC EQU   X'20'                   ORE GETCELL FAILURE              01122000
D23OREBC EQU   X'21'                   ORE BLDCPOOL FAILURE             01123000
D23OREGM EQU   X'22'                   ORE GETMAIN FAILURE, SP231       01124000
D23WQEGC EQU   X'30'                   WQE GETCELL FAILURE              01125000
D23WQEBC EQU   X'31'                   WQE BLDCPOOL FAILURE             01126000
D23WQEGM EQU   X'32'                   WQE GETMAIN FAILURE, SP231       01127000
D23DYN   EQU   X'42'                   DYNAMIC AREA GETMAIN FAILURE     01128000
XVAMOD   DS    C                       D23 MODULE ID                    01129000
VWTOID   EQU   X'01'                   IEAVVWTO'S ID FOR D23            01130000
MWTOID   EQU   X'02'                   IEAVMWTO'S ID FOR D23            01131000
XVA41    DS    C                       RESERVED                         01132000
XVREASON DS    C                       D23 REASON CODE                  01133000
D23BNDY  EQU   X'01'                   WTOR PARMLIST NOT ON WORD BNDY   01134000
D23MLWTR EQU   X'02'                   MULTILINE WTOR SPECIFIED         01135000
D23PARM  EQU   X'03'                   WPL NOT ADDRESSABLE BY USER      01136000
D23ZERO  EQU   X'04'                   ZERO TEXT LENGTH WTOR            01137000
D23SIZE  EQU   X'05'                   CALLER MODIFIED WPL              01138000
*                                      DURING WTO PROCESSING            01139000
D23LTXT  EQU   X'06'                   WTO/WTOR TEXT= CODED AND         01140000
*                                      WPLLGH ^=8                       01141000
*                                                                       01142000
XVA8     DS    AL4                     STORE TIME                       01143000
XVWPLADR DS    AL4                     -> CALLERS WPL                   01144000
XVWQEAD  DS    AL4                                                      01145000
*                                                                       01146000
*        FLAGS                                                          01147000
*                                                                       01148000
XVDFLAGS DS    0XL4                                                     01149000
XVD0     DS    X                                                        01150000
XVD0RPFD EQU   BIT1                    HARD COPY ONLY                   01151000
XVD0NWQE EQU   BIT2                    GET WQE                          01152000
XVD0NORE EQU   BIT3                    GET THE ORE FIRST                01153000
XVD0QID  EQU   BIT4                    QID FIELD IS PRESENT IN THE WPL  01154000
XVD0WWB  EQU   BIT5                    WWB WTO WAIT BLOCK OBTAINED      01155000
XVD0USER EQU   BIT6                                                     01156000
XVD0HDCY EQU   BIT7                                                     01157000
*                                                                       01158000
*        XVDFLAGS+1                                                     01159000
*                                                                       01160000
XVD1     DS    X                                                        01161000
XVD1PRIV EQU   BIT0                    PRIVILEGED USER                  01162000
XVD1ENQW EQU   BIT1                    ENQ'D ON A WQE  (VMWTO)          01163000
XVD1ENQO EQU   BIT2                    ENQ'D ON AN ORE (VMWTO)          01164000
XVD1ALDN EQU   BIT2                    ALL NEEDED CONTROL BLKS OBTAINED 01165000
XVD1PP   EQU   BIT5                    PROBLEM PROGRAM CALLER           01166000
XVD1AUTH EQU   BIT6                    KEY 0, SUPVR STATE OR APF AUTH   01167000
XVD1PERR EQU   BIT7                    SEVERE ERROR FOUND. ABEND USER   01168000
*                                                                       01169000
*        XVDFLAGS+2                                                     01170000
*                                                                       01171000
XVD2     DS    X                                                        01172000
XVD2CON  EQU   BIT0                    CONNECTING                       01173000
XVD2VALD EQU   BIT3                    PARAMETER LIST IS VALID          01174000
XVD2DELW EQU   BIT4                    SEND MSG TO HARDCOPY ONLY        01175000
XVD2ZERO EQU   BIT5                    ZERO MSG ID TO USER              01176000
XVD2WTOR EQU   BIT6                    WTOR REQUEST                     01177000
XVD2QFHC EQU   BIT7                    QUEUE THIS MSG TO HARD COPY      01178000
*                                                                       01179000
*        XVDFLAGS+3                                                     01180000
*                                                                       01181000
XVD3     DS    X                                                        01182000
XVD3BLDJ EQU   BIT0                    BUILD MAJOR WQE                  01183000
XVD3BLD1 EQU   BIT1                    BUILD LINE 1                     01184000
XVD3BLD2 EQU   BIT2                    BUILD LINE 2                     01185000
XVD3TXT1 EQU   BIT3                    TEXT LINE 1 BEING USED           01186000
XVD3TFX  EQU   BIT4                    TCBTFX WAS SET ON,TURN IT OFF    01187000
*        END OF FLAGS                                                   01188000
XVOREAD  DS    AL4                                                      01189000
         ORG   XVOREAD                                                  01190000
XVX      DS    0F                      USED AS WORK AREA BY VMWTO       01191000
XVX0     DS    X                       LINE CONTROL FLAGS - MLWTO       01192000
XVX0FLCL EQU   BIT0                    FIRST LINE IS CONTROL LINE       01193000
XVX0LL1F EQU   BIT1                    LABEL LINE 1 FOUND               01194000
XVX0LL2F EQU   BIT2                    LABEL LINE 2 FOUND               01195000
XVX0UDCL EQU   BIT3                    USE DEFAULT CONTROL LINE         01196000
XVX0FLJE EQU   BIT4                    FIRST LINE JUST 'E'              01197000
XVX0FEDE EQU   BIT5                    FORCE END (LAST LINE TO BE DE)   01198000
XVX1     DS    X                       ERROR FLAGS - MLWTO              01199000
XVX1STOP EQU   BIT0                    ERROR IN PARM LIST; IGNORE MLWTO 01200000
XVX1NOID EQU   BIT1                    NO ID FOR CONNECTING MLWTO       01201000
XVX2     DS    AL1                     NO OF LINES STILL TO DO          01202000
XVX3     DS    AL1                     NO OF LINES FROM MLWTO EXT HDR   01203000
*                                                                       01204000
XVCMAJOR DS    AL4                  *                                   01205000
XVCMINOR DS    AL4                  V                                   01206000
*                                                                       01207000
XVWWB    DS    AL4                                                      01208000
*                                                                       01209000
XVWQEID  DS    0AL4       *            CALLERS R0                       01210000
XVWQEIDA DS    AL3        |            A NEW LINE IS TO BE              01211000
*                         |            CONNECTED TO THE MESSAGE         01212000
*                         |            WITH THIS 3 BYTE MESSAGE ID      01213000
*                         |            MLWTO ONLY                       01214000
XVCONID  DS    XL1        V            CONSOLE ID FOR THIS MESSAGE      01215000
*                                                                       01216000
XVRET    DS    0AL4                                                     01217000
XVRETCOD DS    AL4                                                      01218000
XVLEN    EQU   *-XVSAV                 MUST NOT EXCEED 48 BYTES AS      01219000
*                                      RB EXTENDED SAVE AREA USED       01220000
*                                                                       01221000
*        WTO/WTOR/MLWTO/WTP PARAMETER LIST                              01222000
*                                                                       01223000
         IEZWPL                                                         01224000
*                                                                       01225000
         PRINT NOGEN                                                    01226000
*                                                                       01227000
*        ADDRESS SPACE CONTROL BLOCK                                    01228000
*                                                                       01229000
         IHAASCB                                                        01230000
*                                                                       01231000
*        CONSOLE UNIT CONTROL MODULE                                    01232000
*                                                                       01233000
         IEECUCM DSECT=YES,FORMAT=NEW                                   01234000
*                                                                       01235000
*        CVT                                                            01236000
*                                                                       01237000
         CVT   DSECT=YES                                                01238000
*                                                                       01239000
         PRINT GEN                                                      01240000
*                                                                       01241000
R0       EQU   0                                                        01242000
R1       EQU   1                                                        01243000
R2       EQU   2                                                        01244000
R3       EQU   3                                                        01245000
R4       EQU   4                                                        01246000
R5       EQU   5                                                        01247000
R6       EQU   6                                                        01248000
R7       EQU   7                                                        01249000
R8       EQU   8                                                        01250000
R9       EQU   9                                                        01251000
R10      EQU   10                                                       01252000
R11      EQU   11                                                       01253000
R12      EQU   12                                                       01254000
R13      EQU   13                                                       01255000
R14      EQU   14                                                       01256000
R15      EQU   15                                                       01257000
*                                                                       01258000
         END   IGC0203E                                                 01259000
./ ENDUP
/*
//*
//STEP03  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DATA
++USERMOD(ZP60039)            /* ADD TEXT= TO WTO AND WTOR */  .
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
++USERMOD(ZP60040)            /* ADD TEXT= TO WTO AND WTOR */  .
++VER(Z038) FMID(EBB1102)
  PRE(UY13810) .
++IF FMID(FBB1221) REQ(ZP60039)
 /*
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
