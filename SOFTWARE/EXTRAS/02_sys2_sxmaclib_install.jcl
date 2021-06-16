//SYS2SXMC  JOB (TSO),
//             'Install SYS2 MACLIB',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*  Installs SYS2.SXMACLIB
//*
//STEP1   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.SXMACLIB,DISP=(NEW,CATLG),
//             VOL=SER=MVS000,
//             UNIT=3350,SPACE=(TRK,(30,10,100)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=19040)
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=$$DOC    0112-16043-16100-1247-00016-00004-00000-SYS210  18
                      SXMACLIB Overview

This macro library is mainly intended to provide support for the
instructions emulated by loading the Hercules S37X module when running
in S/370 mode. For more information, please see the $$S37X member.

To allow the structured programming macros provided with the original
TK3 system to utilize the additional S37X instructions, changes
were required to the structured programming macros. In general these
changes should be transparent to the user. A few additional features
have been implemented in the structured programming macros. These new
features along with the capabilities provided by the original macros
are fully documented in the $$SPDOC member.

Shelby Beach
Ms. Kitty Programming Werks
./ ADD NAME=$$SPDOC  0116-16094-16103-1725-00366-00004-00000-SYS210  56
The structured programming macros provide tools that you can use to
write structured programs in assembly language. The structured
programming macros included in SXMACLIB are based on those originally
included with TK3, and have been modified to provide some of the
functionality included in the structured programming macros that are
part of the IBM HLASM Toolkit. Although the SXMACLIB macros are still
a limited subset of those provided by IBM, the changes made to these
macros bring them more in line with the design of the IBM macros.

You can use the structured programming macros in this library even
if you do not intend to use any of the new instructions provided by
the S37X module; however, if you intend to use any of the S37X
instructions in your structured programs, you MUST use the SXMACLIB
macros. The macros originally included as part of TK3 will produce
any number of nasty errors should you attempt to code an S37X
instruction with a TK3 structured programming macro, for example:

            IF (CHI,R1,EQ,17)

$ New Structured Programming Capabilities

The structured programming macros are fully documented below; this
section briefly describes new features added to the structured
prograamming macro set.

  - DO UNTIL
    This DO macro option allows you to define a DO group which will
    be executed at least one time. Essentially, the test for loop
    termination is moved to the bottom of the loop.

  - ITERATE
    The ITERATE macro allows you to end the current execution of a
    DO group and branch to the code which tests for loop termination.
    You may specify the name of the DO group to be iterated; by default,
    the current DO group is iterated.

  - DOEXIT
    This macro provides the ability to conditionally terminate the
    execution of the current DO group.

  - ASMLEAVE
    The ASMLEAVE macro unconditionally terminates the execution of
    either the current DO group, or the specified, named DO group.

  - ASMMREL
    The ON/OFF option for this macro allows you to optionally
    specify that the structured programming macros should generate
    branch relative instructions (ON), as opposed to base register
    branch instructions (OFF). The default setting is OFF.

  - Logical expressions
    It is now possible to specify just the condition code value as the
    operand of an IF, ELSEIF, DO, or DOEXIT macro. This change allows
    you to test the previously set condition code value without having
    to code an instruction which sets the condition code.

  - Numeric Condition Code Specification
    In addition to specifying a mnemonic condition code mask (e.g. EQ,
    NE, Z, etc.), you may optionally specify a decimal number between 1
    and 14.

$ Structured Programming Macro Usage

  - ASMMREL

    Controls the generation of instructions by the structured
    programming macros. By default, the macros will generate S/370
    type base register branch instructions. If you specify the ON
    option, the macros will generate z/Architecture branch relative
    (jump) instructions.

    The ON option also controls the instruction which loads the register
    specified by the DO FROM operand. If ON is specified, an LLILF
    instruction is used, allowing a maxmimum value of 2**32 - 1;
    otherwise, an LA instruction is used, and the maximum value is 4095.

      Syntax:

          ASMMREL status

                   - status : ON generates branch relative instructions
                            : OFF generates base register branch
                              instructions

  - DO

    The DO macro specifies the start of a DO group. The DO group is
    terminated by a matching ENDDO macro. Options specified on the DO
    macro determine the conditions under which execution of the
    contained loop of code will be terminated. logical-expression is
    documented below following the descriptions of the various macros
    in which it is used. Several forms of the DO macro are available:

    The DO macro may specify an optional name. If you intend to
    terminate execution of a DO group by name using the ASMLEAVE or
    EXIT macros, then you must include a name for the DO group on the
    DO statement. Note that this is not the same as specifying a DS 0H
    or EQU statement preceding the DO statement.

      Syntax:

{name}    DO WHILE=logical-expression

          Executes the code between the DO and the corresponding ENDDO.
          The code contained in the loop will not be executed if the
          logical-expression initially evaluates as true. Execution of
          the loop ends when the specified logical-expression evaluates
          as true.

{name}    DO UNTIL=logical-expression

          Executes the code between the DO and the corresponding ENDDO.
          The code contained in the loop will be executed at least once,
          and execution of the loop ends when the specified logical-
          expression evaluates as true.

{name}    DO FROM=(reg{,int})

                   - reg    : Register specification (0 - 15), used
                              in a BCT instruction generated by the
                              corresponding ENDDO.
                   - int    : Optional integer numeric value used to
                              initialize reg. If int is not specified,
                              it is assumed that reg has already been
                              loaded with the desired value.

                              The maximum value allowed for int depends
                              upon the setting of ASMMREL. If OFF, the
                              maximum value is 4095; if ON, the maximum
                              value is 2**31 - 1.
{name}    DO INF
{name}    DO FOREVER

          Executes the code between the DO and the corresponding ENDDO.
          The code contained in the loop will execute continuously until
          some instruction contained in the loop exits. The instruction
          causing the exit may be a branch instruction, or one of the
          structured programming macros: ASMLEAVE, DOEXIT, or EXIT.

  - ASMLEAVE

    Terminates execution of a DO group. Control passes to the statement
    following the ENDDO for the loop that is terminated.

      Syntax:

          ASMLEAVE {do-group}

                 - do-group : This optional operand specifies the name
                              of the DO group whose execution is to be
                              be terminated. If the do-group name is not
                              specified, execution of the current DO
                              group is terminated.

  - DOEXIT

    Terminates execution of the current DO group. Control passes to the
    statement following the ENDDO for the current loop.

      Syntax:

          DOEXIT {logical-expression}

             - logical-expression : If the logical expression evaluates
                                    as true, execution of the current
                                    loop terminates; otherwise execution
                                    of the loop continues with the
                                    instruction following the DOEXIT.

                                    If the logical-expression is not
                                    specified, execution of the loop is
                                    unconditionally terminated.

  - EXIT

    Terminates execution of a DO group. Control passes to the statement
    following the ENDDO for the loop that is terminated.

      Syntax:

          EXIT {DO=do-group}

                 - do-group : This optional operand specifies the name
                              of the DO group whose execution is to be
                              be terminated. If the do-group name is not
                              specified, execution of the current DO
                              group is terminated.

  - ITERATE

    Stops executing in the current DO group and passes control to the
    code which determines whether or not execution of the loop should
    continue. If the DO group is controlled by the FROM option, control
    is passed to the BCT generated by the corresponding ENDDO macro.

      Syntax:

          ITERATE {do-group}

                 - do-group : This optional operand specifies the name
                              of the DO group to be iterated. If the
                              do-group name is not specified, the
                              current DO group is iterated.

  - ENDDO

    Marks the end of a DO group. Each DO macro must have a corresponding
    ENDDO. The ENDDO macro has no operands.

      Syntax:

          ENDDO

  - IF

    The IF macro allows you to conditionally execute a block of code
    based upon the true/false value of the specified logical-expression.

      Syntax:

          IF logical-expression

          Executes the code between the IF and the corresponding ELSE,
          ELSEIF, or ENDIF provided that the specified logical-
          expression evaluates as true.

  - ELSE

    The ELSE macro allows you to specify the block of code to be
    executed should the logical-expression associated with the IF or
    ELSEIF macro at the same nesting level evaluate to false.

      Syntax:

          ELSE

          Marks the beginning of the block of code to be executed if
          the logical expression associated with the IF or ELSEIF macro
          at the same nesting level evaluates as false. The block of
          code must be followed by an ENDIF macro.

  - ELSEIF

    The ELSEIF macro allows you to specify the block of code to be
    executed should the logical-expression associated with the IF or
    ELSEIF macro at the same nesting level evaluate to false, and the
    logical-expression specified by the ELSEIF evaluates as true.

      Syntax:

          ELSEIF logical-expression

          Marks the beginning of the block of code to be executed if the
          logical-expression evaluates as true. The block of code must
          be followed by an ENDIF or another ELSEIF macro.

  - ENDIF

    The ENDIF macro marks the end of a block of code. The ENDIF must
    have a corresponding IF, ELSE, or ELSEIF at the same nesting level.

      Syntax:

          ENDIF

  - Logical Expressions

    Many of the structured programming macros require an operand
    consisting of a logical expression. A logical expression can take
    several forms. In general a logical expression will contain one or
    more conditional phrases each enclosed within parentheses and
    connected by AND or OR operators.

      - Mask Conditional Phrase

        In its simplist form, a logical expression consists of just a
        condition code mask. The mask specifies what is to be tested for
        in the current value of the condition code. The following
        mnemonic mask values may be specified:

          E  - Equal
          EQ - Equal
          GE - Greater than or equal
          GT - Greater than
          H  - High
          L  - Low
          LE - Less than or equal
          LT - Less than
          M  - Minus/Mixed
          NE - Not equal
          NH - Not high
          NL - Not low
          NM - Not minus/Not mixed
          NO - Not overflow/Not ones
          NP - Not plus
          NZ - Not zero
          O  - Overflow/Ones
          P  - Plus
          Z  - Zero

        Additionally, the condition code mask can be specified as a
        numeric value between 1 and 14.

        - Examples

          IF (P)
          DO WHILE=(NZ)
          DOEXIT (8)

      - Single Conditional Phrase

        This form of a logical expression consists of a single
        instruction which sets the condition code, enclosed in
        parentheses, along with a specification for the desired
        condition code mask. The condition code mask is coded following
        the operation code and its operands unless the instruction is a
        compare operation.

        For a compare instruction, the condition code mask is coded
        preceding the second comparand address. So for CLC as an
        example, the condition code mask fits naturally between the
        first and second comparands (i.e. the only two operands). But
        for some compare instructions, other operands of the instruction
        get in the way (so to speak). This is the situation with
        instructions such as CLM or CS (see the examples below). Notice
        that even for these instructions, the placement of the condition
        code mask still precedes the second comparand address.

        - Examples

          IF (TM,SW,X'10',NO)
          IF (CLC,NAME,GT,INPUT)
          IF (CLM,R5,B'1000',EQ,=C'X')
          IF (CLCLE,R2,EQ,R4,X'40')
          IF (CS,R15,R0,NE,DATQECB)
          IF (ICM,R1,B'0111',ADDR+1,NZ)
          DO UNTIL=(CHI,R3,GT,17)
          DOEXIT (CLC,NAME,EQ,=C'NONAME')
          DO WHILE=(A,R0,INCR,M)

      - Compound Conditional Phrase

        A logical expression consisting of more than one conditional
        phrase connected by AND or OR operators constitutes a compound
        conditional phrase. If the OR operator is used, the logical
        expression is true if any of the connected phrases is true.
        For the AND operator, the logical expression is true only if
        all of the connected phrases are true. The use of AND and OR
        in the same logical expression is not supported, and the result
        is undefined.

        - Examples

          IF (TM,SW1,X'10',NO),OR,
                (TM,SW2,X'04',O)
          DO WHILE=(LTR,R15,R15,Z),AND,
                (LTR,R1,R1,P)
          DO UNTIL=(LTR,R15,R15,NZ),OR,
                (CLI,CODE,EQ,C'X'),OR,
                (CLI,CODE,EQ,C'Y'),OR,
                (CLI,CODE,EQ,C'Z')
          ELSEIF (M),OR,
                (C,R7,GT,MAXVAL)

Shelby Beach
Ms. Kitty Programming Werks
./ ADD NAME=$$S37X   0102-16094-16100-1247-00021-00020-00000-SYS210  01
This library provides macros which generate code for the z/Architecture
instructions provided by the Hercules S37X module when assembling with
the IFOX00 assembler. It is based on the SIMZ9 macro library created
by Tom Armstrong in support of the SIMZ9 simulator. Those macros are
included in this library, as the SIMZ9 instructions are a subset of
those provided by S37X.

Notes:
1. Displacement values are limited to a maximum of 12 bits. Instructions
   with 20-bit displacement values will contain zeroes in the high-order
   8 bits (DH field).
2. The second operand of SS-f format instructions (e.g. PKA, PKU)
   must be coded as a symbolic value without an explicit length. This
   restriction arises from the fact that the 8-bit length field in SS-f
   format instructions applies to the second operand, as opposed to the
   first operand in S/360 SS format instructions (e.g. MVC).
3. For instructions containing relative address references (e.g. CHRL),
   the relative address operand may not be a literal.

Shelby Beach
Ms. Kitty Programming Werks
./ ADD NAME=ADB      0104-16042-16101-1823-00007-00006-00000-SYS210  59
         MACRO
&LABEL   ADB   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,1A
         MEND
./ ADD NAME=ADBR     0103-16042-16101-1824-00008-00005-00000-SYS210  06
         MACRO
&LABEL   ADBR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B31A00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=AEB      0102-16043-16101-1824-00007-00007-00000-SYS210  50
         MACRO
&LABEL   AEB   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,0A
         MEND
./ ADD NAME=AEBR     0102-16043-16101-1824-00008-00008-00000-SYS210  54
         MACRO
&LABEL   AEBR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B30A00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=AFI      0103-08014-16101-1824-00007-00007-00000-SYS210  59
         MACRO
&LABEL   AFI   &R1,&I32
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C2OP  9,&R1,&I32
         MEND
./ ADD NAME=AGSI     0102-16043-16101-1825-00007-00007-00000-SYS210  02
         MACRO
&LABEL   AGSI  &BD,&I8
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   DC    0XL6'00',X'EB',AL1(&I8.),S(&BD.),X'007A'
         MEND
./ ADD NAME=AHI      0103-08013-16101-1825-00007-00007-00000-SYS210  07
         MACRO
&LABEL   AHI   &R1,&I16
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A7OP  A,&R1,&I16
         MEND
./ ADD NAME=AHIK     0102-16063-16101-1825-00007-00007-00000-SYS210  12
         MACRO
&LABEL   AHIK  &R1,&R3,&I16
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   DC    0XL6'00',X'EC',AL.4(&R1.,&R3.),AL2(&I16),X'00D8'
         MEND
./ ADD NAME=AHY      0102-16063-16101-1825-00007-00007-00000-SYS210  17
         MACRO
&LABEL   AHY   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,7A
         MEND
./ ADD NAME=ALC      0102-16044-16101-1825-00007-00007-00000-SYS210  27
         MACRO
&LABEL   ALC   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,98
         MEND
./ ADD NAME=ALCR     0102-16044-16101-1825-00008-00008-00000-SYS210  30
         MACRO
&LABEL   ALCR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B99800',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=ALFI     0103-08014-16101-1825-00007-00007-00000-SYS210  34
         MACRO
&LABEL   ALFI  &R1,&I32
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C2OP  B,&R1,&I32
         MEND
./ ADD NAME=ALGSI    0102-16044-16101-1825-00007-00007-00000-SYS210  38
         MACRO
&LABEL   ALGSI &BD,&I8
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   DC    0XL6'00',X'EB',AL1(&I8.),S(&BD.),X'007E'
         MEND
./ ADD NAME=ALHSIK   0101-16063-16101-1825-00007-00007-00000-SYS210  42
         MACRO
&LABEL   ALHSIK &R1,&R3,&I16
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   DC    0XL6'00',X'EC',AL.4(&R1.,&R3.),AL2(&I16),X'00DA'
         MEND
./ ADD NAME=ALRK     0102-16063-16101-1825-00008-00008-00000-SYS210  47
         MACRO
&LABEL   ALRK  &R1,&R2,&R3
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B9FA',AL.4(&R3.,0),AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=ALSI     0102-16044-16101-1826-00007-00007-00000-SYS210  05
         MACRO
&LABEL   ALSI  &BD,&I8
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   DC    0XL6'00',X'EB',AL1(&I8.),S(&BD.),X'006E'
         MEND
./ ADD NAME=ALY      0102-16063-16101-1826-00007-00007-00000-SYS210  08
         MACRO
&LABEL   ALY   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,5E
         MEND
./ ADD NAME=ARK      0102-16063-16101-1826-00008-00008-00000-SYS210  12
         MACRO
&LABEL   ARK   &R1,&R2,&R3
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B9F8',AL.4(&R3.,0),AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=ASI      0102-16044-16101-1826-00007-00007-00000-SYS210  16
         MACRO
&LABEL   ASI   &BD,&I8
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   DC    0XL6'00',X'EB',AL1(&I8.),S(&BD.),X'006A'
         MEND
./ ADD NAME=ASMLEAVE 0101-16094-16096-1851-00062-00070-00000-SYS210  47
         MACRO
&NAME    ASMLEAVE &DO                                               SLB
.*#-------------------------------------------------------------------*
.*#   ASMLEAVE     MACRO FOR STRUCTURED PROGRAMMING                   *
.*#-------------------------------------------------------------------*
.*#
.*#   FUNCTION:    UNCONDITIONAL EXIT OF ONE OR MORE DO GROUPS.
.*#
.*#   SYNTAX       ASMLEAVE  : EXITS CURRENT DO GROUP
.*#
.*#                ASMLEAVE  : DOGROUP
.*#                            EXITS THE DO GROUP WITH THIS LABEL
.*#                            EXAMPLE: ASMLEAVE OUTER
.*#
.*#--------------------------------------------------------------------
         LCLA  &I              INDEX DONAME SCANNING                SLB
         LCLC  &OPND                                                SLB
         COPY  IFGLO                                                SLB
.*                                                                  SLB
&MACNA   SETC  'ASMLEAVE'                                           SLB
.*                                                                  SLB
         AIF   (&DOLEVEL GT 0).OBR00                                SLB
    MNOTE 12,' ??????  ADIOS LOGIC !!!,     EMERGENCY MESSAGE FROM STRU*
               CTURED PROGRAMMING SYSTEM.'                          SLB
         MEXIT                                                      SLB
.OBR00   ANOP                                                       SLB
         AIF   ('&DO' EQ '').NONAME      NAME OF DO GROUP SPECIFIED SLB
.*-----------------------------------------------------------------SLB*
.*       LOOK UP NAME-TABLE FOR CURRENT NAME                       SLB*
.*-----------------------------------------------------------------SLB*
&I       SETA  1                                                    SLB
.LOOP    ANOP                                                       SLB
         AIF   (&I GT &DOLEVEL).FEHL19                              SLB
         AIF   ('&DO' EQ '&DONAME(&I)').ENDLOOP NAME IS IN TABLE    SLB
&I       SETA  &I+1                             NEXT ELEMENT        SLB
         AGO  .LOOP                                                 SLB
.ENDLOOP ANOP                                                       SLB
         AGO  .GENER                                                SLB
.*-----------------------------------------------------------------SLB*
.*       EXIT CURRENT LOOP                                         SLB*
.*-----------------------------------------------------------------SLB*
.NONAME  ANOP                                                       SLB
&I       SETA  &DOLEVEL                                             SLB
.GENER   ANOP                                                       SLB
.*                                                                  SLB
.*-----------------------------------------------------------------SLB*
.*  GENERATE CODE TO EXIT THE SPECIFIED LOOP                       SLB*
.*-----------------------------------------------------------------SLB*
&OPND    SETC  '&IFPRAEF&DOENDLB(&I)'                               SLB
         AIF   ('&ASMMREL' NE 'ON').BASE01 CHECK MACRO LEVEL        SLB
         J     &OPND                     LEAVE THE LOOP             SLB
         AGO   .MACEND                                              SLB
.BASE01  ANOP                                                       SLB
         B     &OPND                     LEAVE THE LOOP             SLB
.*                                                                  SLB
         AGO   .MACEND                                              SLB
.*                                                                  SLB
         COPY IFERR                                                 SLB
.*                                                                  SLB
.MACEND  ANOP                                                       SLB
         MEXIT                                                      SLB
         MEND                                                       SLB
./ ADD NAME=ASMMREL  0102-16089-16096-1905-00025-00024-00000-SYS210  24
         MACRO
         ASMMREL &STATUS                                            SLB
.*#-------------------------------------------------------------------*
.*#   ASMMREL     MACRO FOR STRUCTURED PROGRAMMING                    *
.*#-------------------------------------------------------------------*
.*#
.*#   FUNCTION:    DETERMINES IF S37X INSTRUCTIONS ARE GENERATED
.*#
.*#   SYNTAX       ASMMREL   : STATUS
.*#
.*#                -   STATUS: "ON" GENERATES BRANCH RELATIVE
.*#                            INSTRUCTIONS
.*#                            "OFF" GENERATES BASE REGISTER
.*#                            BRANCH INSTRUCTIONS
.*#--------------------------------------------------------------------
         COPY  IFGLO                                                SLB
.*                                                                  SLB
         AIF   ('&STATUS' EQ 'ON').STATOK                           SLB
         AIF   ('&STATUS' EQ 'OFF').STATOK                          SLB
         MNOTE 16,'ASMMREL STATUS MUST BE "ON" OR "OFF"'            SLB
&ASMMREL SETC  'OFF'                                                SLB
         MEXIT                                                      SLB
.STATOK  ANOP                                                       SLB
&ASMMREL SETC  '&STATUS'                                            SLB
         MEND                                                       SLB
./ ADD NAME=AXBR     0102-16044-16101-1826-00008-00008-00000-SYS210  29
         MACRO
&LABEL   AXBR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B34A00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=AY       0102-16063-16101-1826-00007-00007-00000-SYS210  35
         MACRO
&LABEL   AY    &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,5A
         MEND
./ ADD NAME=A5OP     0104-08013-16101-1826-00008-00014-00000-SYS210  41
         MACRO
&LABEL   A5OP  &SUBOP,&R,&I16
.*
.*       Generate A5 Opcodes in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'A5',AL.4(&R.,X'0&SUBOP'),AL2(&I16)
         MEND
./ ADD NAME=A7OP     0107-08013-16101-1826-00008-00015-00000-SYS210  53
         MACRO
&LABEL   A7OP  &SUBOP,&R,&I16
.*
.*       Generate A7 Opcodes in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'A7',AL.4(&R.,X'0&SUBOP'),AL2(&I16)
         MEND
./ ADD NAME=BASSM    0102-08013-16101-1827-00008-00008-00000-SYS210  05
         MACRO
&LABEL   BASSM &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL2'00',X'0C',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=BRAS     0104-08013-16101-1827-00007-00007-00000-SYS210  09
         MACRO
&LABEL   BRAS  &R1,&LAB
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A7OP  5,&R1,(&LAB-*+2)/2
         MEND
./ ADD NAME=BRASL    0103-08014-16101-1827-00007-00007-00000-SYS210  16
         MACRO
&LABEL   BRASL &R1,&LAB
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C0OP  5,&R1,(&LAB-*+2)/2
         MEND
./ ADD NAME=BRC      0102-08013-16101-1827-00007-00007-00000-SYS210  20
         MACRO
&LABEL   BRC   &M,&LAB
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A7OP  4,&M,(&LAB-*+2)/2
         MEND
./ ADD NAME=BRCL     0102-08014-16101-1827-00007-00007-00000-SYS210  23
         MACRO
&LABEL   BRCL  &M,&LAB
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C0OP  4,&M,(&LAB-*+2)/2
         MEND
./ ADD NAME=BRCT     0103-08013-16101-1827-00007-00007-00000-SYS210  26
         MACRO
&LABEL   BRCT  &R1,&LAB
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A7OP  6,&R1,(&LAB-*+2)/2
         MEND
./ ADD NAME=BRE      0102-08014-16101-1827-00007-00007-00000-SYS210  31
         MACRO
&LABEL   BRE   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   8,&REL
         MEND
./ ADD NAME=BREL     0102-08014-16101-1827-00007-00007-00000-SYS210  37
         MACRO
&LABEL   BREL  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  8,&REL
         MEND
./ ADD NAME=BRH      0102-08014-16101-1827-00007-00007-00000-SYS210  42
         MACRO
&LABEL   BRH   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   2,&REL
         MEND
./ ADD NAME=BRHL     0102-08014-16101-1827-00007-00007-00000-SYS210  46
         MACRO
&LABEL   BRHL  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  2,&REL
         MEND
./ ADD NAME=BRL      0102-08014-16101-1827-00007-00007-00000-SYS210  50
         MACRO
&LABEL   BRL   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   4,&REL
         MEND
./ ADD NAME=BRLL     0102-08014-16101-1827-00007-00007-00000-SYS210  53
         MACRO
&LABEL   BRLL  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  4,&REL
         MEND
./ ADD NAME=BRM      0102-08014-16101-1827-00007-00007-00000-SYS210  57
         MACRO
&LABEL   BRM   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   4,&REL
         MEND
./ ADD NAME=BRML     0102-08014-16101-1828-00007-00007-00000-SYS210  02
         MACRO
&LABEL   BRML  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  4,&REL
         MEND
./ ADD NAME=BRNE     0102-08014-16101-1828-00007-00007-00000-SYS210  05
         MACRO
&LABEL   BRNE  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   7,&REL
         MEND
./ ADD NAME=BRNEL    0102-08014-16101-1828-00007-00007-00000-SYS210  10
         MACRO
&LABEL   BRNEL &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  7,&REL
         MEND
./ ADD NAME=BRNH     0102-08014-16101-1828-00007-00007-00000-SYS210  15
         MACRO
&LABEL   BRNH  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   13,&REL
         MEND
./ ADD NAME=BRNHL    0102-08014-16101-1828-00007-00007-00000-SYS210  17
         MACRO
&LABEL   BRNHL &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  13,&REL
         MEND
./ ADD NAME=BRNL     0102-08014-16101-1828-00007-00007-00000-SYS210  20
         MACRO
&LABEL   BRNL  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   11,&REL
         MEND
./ ADD NAME=BRNLL    0102-08014-16101-1828-00007-00007-00000-SYS210  23
         MACRO
&LABEL   BRNLL &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  11,&REL
         MEND
./ ADD NAME=BRNM     0102-08014-16101-1828-00007-00007-00000-SYS210  28
         MACRO
&LABEL   BRNM  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   11,&REL
         MEND
./ ADD NAME=BRNML    0102-08014-16101-1828-00007-00007-00000-SYS210  31
         MACRO
&LABEL   BRNML &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  11,&REL
         MEND
./ ADD NAME=BRNO     0102-08014-16101-1828-00007-00007-00000-SYS210  34
         MACRO
&LABEL   BRNO  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   14,&REL
         MEND
./ ADD NAME=BRNOL    0102-08014-16101-1828-00007-00007-00000-SYS210  39
         MACRO
&LABEL   BRNOL &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  14,&REL
         MEND
./ ADD NAME=BRNP     0102-08014-16101-1828-00007-00007-00000-SYS210  43
         MACRO
&LABEL   BRNP  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   13,&REL
         MEND
./ ADD NAME=BRNPL    0102-08014-16101-1828-00007-00007-00000-SYS210  47
         MACRO
&LABEL   BRNPL &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  13,&REL
         MEND
./ ADD NAME=BRNZ     0102-08014-16101-1828-00007-00007-00000-SYS210  51
         MACRO
&LABEL   BRNZ  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   7,&REL
         MEND
./ ADD NAME=BRNZL    0102-08014-16101-1828-00007-00007-00000-SYS210  53
         MACRO
&LABEL   BRNZL &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  7,&REL
         MEND
./ ADD NAME=BRO      0102-08014-16101-1829-00007-00007-00000-SYS210  01
         MACRO
&LABEL   BRO   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   1,&REL
         MEND
./ ADD NAME=BROL     0102-08014-16101-1829-00007-00007-00000-SYS210  06
         MACRO
&LABEL   BROL  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  1,&REL
         MEND
./ ADD NAME=BRP      0102-08014-16101-1829-00007-00007-00000-SYS210  10
         MACRO
&LABEL   BRP   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   2,&REL
         MEND
./ ADD NAME=BRPL     0102-08014-16101-1829-00007-00007-00000-SYS210  13
         MACRO
&LABEL   BRPL  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  2,&REL
         MEND
./ ADD NAME=BRU      0102-08014-16101-1829-00007-00007-00000-SYS210  17
         MACRO
&LABEL   BRU   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   15,&REL
         MEND
./ ADD NAME=BRUL     0102-08014-16101-1829-00007-00007-00000-SYS210  20
         MACRO
&LABEL   BRUL  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  15,&REL
         MEND
./ ADD NAME=BRXH     0104-16046-16101-1829-00008-00005-00000-SYS210  22
         MACRO
&LABEL   BRXH  &R1,&R3,&LAB
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL.  DC    0XL4'00',X'84',AL.4(&R1.,&R3.),AL2((&LAB-*+2)/2)
         MEND
./ ADD NAME=BRXLE    0101-16046-16101-1829-00008-00008-00000-SYS210  29
         MACRO
&LABEL   BRXLE &R1,&R3,&LAB
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL.  DC    0XL4'00',X'85',AL.4(&R1.,&R3.),AL2((&LAB-*+2)/2)
         MEND
./ ADD NAME=BRZ      0102-08014-16101-1829-00007-00007-00000-SYS210  32
         MACRO
&LABEL   BRZ   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   8,&REL
         MEND
./ ADD NAME=BRZL     0102-08014-16101-1829-00007-00007-00000-SYS210  36
         MACRO
&LABEL   BRZL  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  8,&REL
         MEND
./ ADD NAME=BSM      0102-08013-16101-1829-00008-00008-00000-SYS210  39
         MACRO
&LABEL   BSM   &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL2'00',X'0B',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CDB      0102-16046-16101-1830-00007-00007-00000-SYS210  50
         MACRO
&LABEL   CDB   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,19
         MEND
./ ADD NAME=CDBR     0102-16046-16101-1830-00008-00008-00000-SYS210  54
         MACRO
&LABEL   CDBR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B31900',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CDFBR    0102-16046-16101-1830-00008-00008-00000-SYS210  59
         MACRO
&LABEL   CDFBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B39500',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CDFR     0102-16046-16101-1831-00008-00008-00000-SYS210  08
         MACRO
&LABEL   CDFR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B3B500',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CEB      0102-16046-16101-1831-00007-00007-00000-SYS210  11
         MACRO
&LABEL   CEB   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,09
         MEND
./ ADD NAME=CEBR     0102-16046-16101-1831-00008-00008-00000-SYS210  14
         MACRO
&LABEL   CEBR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B30900',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CEFBR    0102-16046-16101-1831-00008-00008-00000-SYS210  18
         MACRO
&LABEL   CEFBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B39400',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CEFR     0102-16046-16101-1831-00008-00008-00000-SYS210  21
         MACRO
&LABEL   CEFR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B3B400',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CFC      0103-16046-16101-1831-00008-00005-00000-SYS210  23
         MACRO
&LABEL   CFC   &BD
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B21A',S(&BD)
         MEND
./ ADD NAME=CFDBR    0102-16046-16101-1831-00008-00008-00000-SYS210  26
         MACRO
&LABEL   CFDBR &R1,&M3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B399',AL.4(&M3.,0,&R1.,&R2.)
         MEND
./ ADD NAME=CFDR     0102-16046-16101-1831-00008-00008-00000-SYS210  30
         MACRO
&LABEL   CFDR  &R1,&M3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B3B9',AL.4(&M3.,0,&R1.,&R2.)
         MEND
./ ADD NAME=CFEBR    0101-16046-16101-1831-00008-00008-00000-SYS210  33
         MACRO
&LABEL   CFEBR &R1,&M3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B398',AL.4(&M3.,0,&R1.,&R2.)
         MEND
./ ADD NAME=CFER     0101-16046-16101-1831-00008-00008-00000-SYS210  38
         MACRO
&LABEL   CFER  &R1,&M3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B3B8',AL.4(&M3.,0,&R1.,&R2.)
         MEND
./ ADD NAME=CFI      0103-08014-16101-1832-00007-00007-00000-SYS210  00
         MACRO
&LABEL   CFI   &R1,&I32
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C2OP  D,&R1,&I32
         MEND
./ ADD NAME=CFXBR    0101-16046-16101-1832-00008-00008-00000-SYS210  05
         MACRO
&LABEL   CFXBR &R1,&M3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B39A',AL.4(&M3.,0,&R1.,&R2.)
         MEND
./ ADD NAME=CFXR     0101-16046-16101-1832-00008-00008-00000-SYS210  08
         MACRO
&LABEL   CFXR  &R1,&M3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B3BA',AL.4(&M3.,0,&R1.,&R2.)
         MEND
./ ADD NAME=CGHSI    0101-16046-16101-1832-00008-00008-00000-SYS210  11
         MACRO
&LABEL   CGHSI &BD,&I16
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'E558',S(&BD),AL2(&I16)
         MEND
./ ADD NAME=CHHSI    0101-16046-16101-1832-00008-00008-00000-SYS210  15
         MACRO
&LABEL   CHHSI &BD,&I16
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'E554',S(&BD),AL2(&I16)
         MEND
./ ADD NAME=CHI      0103-08014-16101-1832-00007-00007-00000-SYS210  17
         MACRO
&LABEL   CHI   &R1,&I16
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A7OP  E,&R1,&I16
         MEND
./ ADD NAME=CHRL     0102-16048-16101-1832-00007-00007-00000-SYS210  20
         MACRO
&LABEL   CHRL  &R1,&LAB
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C6OP  5,&R1,(&LAB-*+2)/2
         MEND
./ ADD NAME=CHSI     0101-16048-16101-1832-00008-00008-00000-SYS210  24
         MACRO
&LABEL   CHSI  &BD,&I16
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'E55C',S(&BD),AL2(&I16)
         MEND
./ ADD NAME=CHY      0101-16069-16101-1832-00007-00007-00000-SYS210  27
         MACRO
&LABEL   CHY   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,79
         MEND
./ ADD NAME=CIB      0103-16048-16101-1832-00008-00008-00000-SYS210  29
         MACRO
&LABEL   CIB   &R1,&I8,&M3,&BD
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'EC',AL.4(&R1.,&M3.),S(&BD),AL1(&I8),X'FE'
         MEND
./ ADD NAME=CIBE     0103-16069-16101-1832-00007-00008-00000-SYS210  32
         MACRO
&LABEL   CIBE  &R1,&I8,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CIB   &R1,&I8,8,&BD
         MEND
./ ADD NAME=CIBH     0103-16069-16101-1832-00007-00008-00000-SYS210  34
         MACRO
&LABEL   CIBH  &R1,&I8,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CIB   &R1,&I8,2,&BD
         MEND
./ ADD NAME=CIBL     0103-16069-16101-1832-00007-00008-00000-SYS210  38
         MACRO
&LABEL   CIBL  &R1,&I8,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CIB   &R1,&I8,4,&BD
         MEND
./ ADD NAME=CIBNE    0103-16069-16101-1832-00007-00008-00000-SYS210  41
         MACRO
&LABEL   CIBNE &R1,&I8,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CIB   &R1,&I8,6,&BD
         MEND
./ ADD NAME=CIBNH    0103-16069-16101-1832-00007-00008-00000-SYS210  44
         MACRO
&LABEL   CIBNH &R1,&I8,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CIB   &R1,&I8,12,&BD
         MEND
./ ADD NAME=CIBNL    0103-16069-16101-1832-00007-00008-00000-SYS210  46
         MACRO
&LABEL   CIBNL &R1,&I8,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CIB   &R1,&I8,10,&BD
         MEND
./ ADD NAME=CIJ      0104-16048-16101-1832-00009-00009-00000-SYS210  49
         MACRO
&LABEL   CIJ   &R1,&I8,&M3,&REL
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'EC',AL.4(&R1.,&M3.),AL2((&REL-*+2)/2)
         DC    AL1(&I8),X'7E'
         MEND
./ ADD NAME=CIJE     0101-16069-16101-1832-00007-00007-00000-SYS210  52
         MACRO
&LABEL   CIJE  &R1,&I8,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CIJ   &R1,&I8,8,&REL
         MEND
./ ADD NAME=CIJH     0101-16069-16101-1833-00007-00007-00000-SYS210  03
         MACRO
&LABEL   CIJH  &R1,&I8,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CIJ   &R1,&I8,2,&REL
         MEND
./ ADD NAME=CIJL     0101-16069-16101-1833-00007-00007-00000-SYS210  07
         MACRO
&LABEL   CIJL  &R1,&I8,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CIJ   &R1,&I8,4,&REL
         MEND
./ ADD NAME=CIJNE    0101-16069-16101-1833-00007-00007-00000-SYS210  10
         MACRO
&LABEL   CIJNE &R1,&I8,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CIJ   &R1,&I8,6,&REL
         MEND
./ ADD NAME=CIJNH    0101-16069-16101-1833-00007-00007-00000-SYS210  13
         MACRO
&LABEL   CIJNH &R1,&I8,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CIJ   &R1,&I8,12,&REL
         MEND
./ ADD NAME=CIJNL    0101-16069-16101-1833-00007-00007-00000-SYS210  16
         MACRO
&LABEL   CIJNL &R1,&I8,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CIJ   &R1,&I8,10,&REL
         MEND
./ ADD NAME=CIT      0102-16048-16101-1833-00008-00008-00000-SYS210  19
         MACRO
&LABEL   CIT   &R1,&I16,&M3
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'EC',AL.4(&R1.,0),AL2(&I16),AL.4(&M3.,0),X'72'
         MEND
./ ADD NAME=CKSM     0102-08014-16101-1833-00008-00008-00000-SYS210  22
         MACRO
&LABEL   CKSM  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B24100',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CLCLE    0102-16048-16101-1833-00008-00003-00000-SYS210  25
         MACRO
&LABEL   CLCLE &R1,&R3,&BD
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'A9',AL.4(&R1.,&R3.),S(&BD.)
         MEND
./ ADD NAME=CLCLU    0101-16048-16101-1833-00008-00008-00000-SYS210  29
         MACRO
&LABEL   CLCLU &R1,&R3,&BD
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&BD.),X'008F'
         MEND
./ ADD NAME=CLFHSI   0101-16048-16101-1833-00008-00008-00000-SYS210  39
         MACRO
&LABEL   CLFHSI &BD,&I16
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'E55D',S(&BD),AL2(&I16)
         MEND
./ ADD NAME=CLFI     0103-08014-16101-1833-00007-00007-00000-SYS210  44
         MACRO
&LABEL   CLFI  &R1,&I32
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C2OP  F,&R1,&I32
         MEND
./ ADD NAME=CLFIT    0102-16048-16101-1833-00008-00008-00000-SYS210  47
         MACRO
&LABEL   CLFIT &R1,&I16,&M3
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'EC',AL.4(&R1.,0),AL2(&I16),AL.4(&M3.,0),X'73'
         MEND
./ ADD NAME=CLGHSI   0101-16048-16101-1833-00008-00008-00000-SYS210  49
         MACRO
&LABEL   CLGHSI &BD,&I16
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'E559',S(&BD),AL2(&I16)
         MEND
./ ADD NAME=CLHHSI   0101-16048-16101-1833-00008-00008-00000-SYS210  52
         MACRO
&LABEL   CLHHSI &BD,&I16
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'E555',S(&BD),AL2(&I16)
         MEND
./ ADD NAME=CLHRL    0102-16050-16101-1833-00007-00007-00000-SYS210  56
         MACRO
&LABEL   CLHRL &R1,&LAB
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C6OP  7,&R1,(&LAB-*+2)/2
         MEND
./ ADD NAME=CLIB     0103-16050-16101-1833-00008-00008-00000-SYS210  58
         MACRO
&LABEL   CLIB  &R1,&I8,&M3,&BD
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'EC',AL.4(&R1.,&M3.),S(&BD),AL1(&I8),X'FF'
         MEND
./ ADD NAME=CLIBE    0101-16070-16101-1834-00007-00007-00000-SYS210  01
         MACRO
&LABEL   CLIBE &R1,&I8,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLIB  &R1,&I8,8,&BD
         MEND
./ ADD NAME=CLIBH    0101-16070-16101-1834-00007-00007-00000-SYS210  04
         MACRO
&LABEL   CLIBH &R1,&I8,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLIB  &R1,&I8,2,&BD
         MEND
./ ADD NAME=CLIBL    0101-16070-16101-1834-00007-00007-00000-SYS210  06
         MACRO
&LABEL   CLIBL &R1,&I8,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLIB  &R1,&I8,4,&BD
         MEND
./ ADD NAME=CLIBNE   0101-16070-16101-1834-00007-00007-00000-SYS210  09
         MACRO
&LABEL   CLIBNE &R1,&I8,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLIB  &R1,&I8,6,&BD
         MEND
./ ADD NAME=CLIBNH   0101-16070-16101-1834-00007-00007-00000-SYS210  12
         MACRO
&LABEL   CLIBNH &R1,&I8,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLIB  &R1,&I8,12,&BD
         MEND
./ ADD NAME=CLIBNL   0101-16070-16101-1834-00007-00007-00000-SYS210  15
         MACRO
&LABEL   CLIBNL &R1,&I8,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLIB  &R1,&I8,10,&BD
         MEND
./ ADD NAME=CLIJ     0102-16050-16101-1834-00009-00009-00000-SYS210  18
         MACRO
&LABEL   CLIJ  &R1,&I8,&M3,&REL
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'EC',AL.4(&R1.,&M3.),AL2((&REL-*+2)/2)
         DC    AL1(&I8),X'7F'
         MEND
./ ADD NAME=CLIJE    0101-16070-16101-1834-00007-00007-00000-SYS210  20
         MACRO
&LABEL   CLIJE &R1,&I8,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLIJ  &R1,&I8,8,&REL
         MEND
./ ADD NAME=CLIJH    0101-16070-16101-1834-00007-00007-00000-SYS210  24
         MACRO
&LABEL   CLIJH &R1,&I8,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLIJ  &R1,&I8,2,&REL
         MEND
./ ADD NAME=CLIJL    0101-16070-16101-1834-00007-00007-00000-SYS210  26
         MACRO
&LABEL   CLIJL &R1,&I8,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLIJ  &R1,&I8,4,&REL
         MEND
./ ADD NAME=CLIJNE   0101-16070-16101-1834-00007-00007-00000-SYS210  29
         MACRO
&LABEL   CLIJNE &R1,&I8,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLIJ  &R1,&I8,6,&REL
         MEND
./ ADD NAME=CLIJNH   0101-16070-16101-1834-00007-00007-00000-SYS210  31
         MACRO
&LABEL   CLIJNH &R1,&I8,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLIJ  &R1,&I8,12,&REL
         MEND
./ ADD NAME=CLIJNL   0101-16070-16101-1834-00007-00007-00000-SYS210  35
         MACRO
&LABEL   CLIJNL &R1,&I8,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLIJ  &R1,&I8,10,&REL
         MEND
./ ADD NAME=CLRB     0103-16050-16101-1834-00009-00009-00000-SYS210  38
         MACRO
&LABEL   CLRB  &R1,&R2,&M3,&BD
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'EC',AL.4(&R1.,&R2.),S(&BD)
         DC    AL.4(&M3.,0),X'F7'
         MEND
./ ADD NAME=CLRBE    0102-16071-16101-1834-00007-00007-00000-SYS210  42
         MACRO
&LABEL   CLRBE &R1,&R2,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLRB  &R1,&R2,8,&BD
         MEND
./ ADD NAME=CLRBH    0102-16071-16101-1834-00007-00007-00000-SYS210  45
         MACRO
&LABEL   CLRBH &R1,&R2,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLRB  &R1,&R2,2,&BD
         MEND
./ ADD NAME=CLRBL    0102-16071-16101-1834-00007-00007-00000-SYS210  47
         MACRO
&LABEL   CLRBL &R1,&R2,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLRB  &R1,&R2,4,&BD
         MEND
./ ADD NAME=CLRBNE   0102-16071-16101-1834-00007-00007-00000-SYS210  50
         MACRO
&LABEL   CLRBNE &R1,&R2,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLRB  &R1,&R2,6,&BD
         MEND
./ ADD NAME=CLRBNH   0102-16071-16101-1834-00007-00007-00000-SYS210  54
         MACRO
&LABEL   CLRBNH &R1,&R2,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLRB  &R1,&R2,12,&BD
         MEND
./ ADD NAME=CLRBNL   0102-16071-16101-1834-00007-00007-00000-SYS210  59
         MACRO
&LABEL   CLRBNL &R1,&R2,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLRB  &R1,&R2,10,&BD
         MEND
./ ADD NAME=CLRJ     0101-16050-16101-1835-00009-00009-00000-SYS210  02
         MACRO
&LABEL   CLRJ  &R1,&R2,&M3,&REL
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'EC',AL.4(&R1.,&R2.),AL2((&REL-*+2)/2)
         DC    AL.4(&M3.,0),X'77'
         MEND
./ ADD NAME=CLRJE    0101-16071-16101-1835-00007-00007-00000-SYS210  05
         MACRO
&LABEL   CLRJE &R1,&R2,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLRJ  &R1,&R2,8,&REL
         MEND
./ ADD NAME=CLRJH    0101-16071-16101-1835-00007-00007-00000-SYS210  08
         MACRO
&LABEL   CLRJH &R1,&R2,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLRJ  &R1,&R2,2,&REL
         MEND
./ ADD NAME=CLRJL    0101-16071-16101-1835-00007-00007-00000-SYS210  10
         MACRO
&LABEL   CLRJL &R1,&R2,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLRJ  &R1,&R2,4,&REL
         MEND
./ ADD NAME=CLRJNE   0101-16071-16101-1835-00007-00007-00000-SYS210  13
         MACRO
&LABEL   CLRJNE &R1,&R2,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLRJ  &R1,&R2,6,&REL
         MEND
./ ADD NAME=CLRJNH   0101-16071-16101-1835-00007-00007-00000-SYS210  15
         MACRO
&LABEL   CLRJNH &R1,&R2,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLRJ  &R1,&R2,12,&REL
         MEND
./ ADD NAME=CLRJNL   0101-16071-16101-1835-00007-00007-00000-SYS210  18
         MACRO
&LABEL   CLRJNL &R1,&R2,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CLRJ  &R1,&R2,10,&REL
         MEND
./ ADD NAME=CLRL     0102-16051-16101-1835-00007-00007-00000-SYS210  20
         MACRO
&LABEL   CLRL  &R1,&LAB
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C6OP  F,&R1,(&LAB-*+2)/2
         MEND
./ ADD NAME=CLRT     0102-16051-16101-1835-00008-00008-00000-SYS210  23
         MACRO
&LABEL   CLRT  &R1,&R2,&M3
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B973',AL.4(&M3.,0),AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CLST     0101-16051-16101-1835-00008-00008-00000-SYS210  25
         MACRO
&LABEL   CLST  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B25D00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CLY      0101-16073-16101-1835-00007-00007-00000-SYS210  34
         MACRO
&LABEL   CLY   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,55
         MEND
./ ADD NAME=CMPSC    0101-16051-16101-1835-00008-00008-00000-SYS210  38
         MACRO
&LABEL   CMPSC &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B26300',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CPSDR    0102-16073-16101-1835-00008-00008-00000-SYS210  47
         MACRO
&LABEL   CPSDR &R1,&R3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B372',AL.4(&R3.,0,&R1.,&R2.)
         MEND
./ ADD NAME=CRB      0102-16051-16101-1835-00009-00009-00000-SYS210  51
         MACRO
&LABEL   CRB   &R1,&R2,&M3,&BD
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'EC',AL.4(&R1.,&R2.),S(&BD)
         DC    AL.4(&M3.,0),X'F6'
         MEND
./ ADD NAME=CRBE     0102-16071-16101-1835-00007-00007-00000-SYS210  55
         MACRO
&LABEL   CRBE  &R1,&R2,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CRB   &R1,&R2,8,&BD
         MEND
./ ADD NAME=CRBH     0102-16071-16101-1835-00007-00007-00000-SYS210  58
         MACRO
&LABEL   CRBH  &R1,&R2,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CRB   &R1,&R2,2,&BD
         MEND
./ ADD NAME=CRBL     0102-16071-16101-1836-00007-00007-00000-SYS210  00
         MACRO
&LABEL   CRBL  &R1,&R2,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CRB   &R1,&R2,4,&BD
         MEND
./ ADD NAME=CRBNE    0102-16071-16101-1836-00007-00007-00000-SYS210  02
         MACRO
&LABEL   CRBNE &R1,&R2,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CRB   &R1,&R2,6,&BD
         MEND
./ ADD NAME=CRBNH    0102-16071-16101-1836-00007-00007-00000-SYS210  06
         MACRO
&LABEL   CRBNH &R1,&R2,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CRB   &R1,&R2,12,&BD
         MEND
./ ADD NAME=CRBNL    0102-16071-16101-1836-00007-00007-00000-SYS210  10
         MACRO
&LABEL   CRBNL &R1,&R2,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CRB   &R1,&R2,10,&BD
         MEND
./ ADD NAME=CRJ      0101-16051-16101-1836-00009-00009-00000-SYS210  13
         MACRO
&LABEL   CRJ   &R1,&R2,&M3,&REL
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'EC',AL.4(&R1.,&R2.),AL2((&REL-*+2)/2)
         DC    AL.4(&M3.,0),X'76'
         MEND
./ ADD NAME=CRJE     0101-16071-16101-1836-00007-00007-00000-SYS210  15
         MACRO
&LABEL   CRJE  &R1,&R2,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CRJ   &R1,&R2,8,&REL
         MEND
./ ADD NAME=CRJH     0101-16071-16101-1836-00007-00007-00000-SYS210  17
         MACRO
&LABEL   CRJH  &R1,&R2,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CRJ   &R1,&R2,2,&REL
         MEND
./ ADD NAME=CRJL     0101-16071-16101-1836-00007-00007-00000-SYS210  21
         MACRO
&LABEL   CRJL  &R1,&R2,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CRJ   &R1,&R2,4,&REL
         MEND
./ ADD NAME=CRJNE    0101-16071-16101-1836-00007-00007-00000-SYS210  24
         MACRO
&LABEL   CRJNE &R1,&R2,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CRJ   &R1,&R2,6,&REL
         MEND
./ ADD NAME=CRJNH    0101-16071-16101-1836-00007-00007-00000-SYS210  26
         MACRO
&LABEL   CRJNH &R1,&R2,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CRJ   &R1,&R2,12,&REL
         MEND
./ ADD NAME=CRJNL    0101-16071-16101-1836-00007-00007-00000-SYS210  28
         MACRO
&LABEL   CRJNL &R1,&R2,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   CRJ   &R1,&R2,10,&REL
         MEND
./ ADD NAME=CRL      0101-16073-16101-1836-00007-00007-00000-SYS210  31
         MACRO
&LABEL   CRL   &R1,&LAB
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C6OP  D,&R1,(&LAB-*+2)/2
         MEND
./ ADD NAME=CRT      0101-16051-16101-1836-00008-00008-00000-SYS210  33
         MACRO
&LABEL   CRT   &R1,&R2,&M3
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B972',AL.4(&M3.,0),AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CSST     0102-16051-16101-1836-00007-00008-00000-SYS210  36
         MACRO
&LABEL   CSST  &BD1,&BD2,&R3
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C8OP  &BD1,&BD2,&R3,2
         MEND
./ ADD NAME=CUSE     0101-16051-16101-1836-00008-00008-00000-SYS210  39
         MACRO
&LABEL   CUSE  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B25700',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CUTFU    0102-16051-16101-1836-00012-00008-00000-SYS210  43
         MACRO
&LABEL   CUTFU &R1,&R2,&M3
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
         AIF   (K'&M3 EQ 0).NOMASK
&LABEL   DC    0XL4'00',X'B2A7',AL.4(&M3.,0),AL.4(&R1.,&R2.)
         MEXIT
.NOMASK  ANOP
&LABEL   DC    0XL4'00',X'B2A700',AL.4(&R1.,&R2)
         MEND
./ ADD NAME=CUUTF    0102-16051-16101-1836-00012-00008-00000-SYS210  48
         MACRO
&LABEL   CUUTF &R1,&R2,&M3
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
         AIF   (K'&M3 EQ 0).NOMASK
&LABEL   DC    0XL4'00',X'B2A6',AL.4(&M3.,0),AL.4(&R1.,&R2.)
         MEXIT
.NOMASK  ANOP
&LABEL   DC    0XL4'00',X'B2A600',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CU12     0102-16051-16101-1836-00012-00008-00000-SYS210  52
         MACRO
&LABEL   CU12  &R1,&R2,&M3
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
         AIF   (K'&M3 EQ 0).NOMASK
&LABEL   DC    0XL4'00',X'B2A7',AL.4(&M3.,0),AL.4(&R1.,&R2.)
         MEXIT
.NOMASK  ANOP
&LABEL   DC    0XL4'00',X'B2A700',AL.4(&R1.,&R2)
         MEND
./ ADD NAME=CU14     0102-16051-16101-1836-00012-00008-00000-SYS210  54
         MACRO
&LABEL   CU14  &R1,&R2,&M3
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
         AIF   (K'&M3 EQ 0).NOMASK
&LABEL   DC    0XL4'00',X'B9B0',AL.4(&M3.,0),AL.4(&R1.,&R2.)
         MEXIT
.NOMASK  ANOP
&LABEL   DC    0XL4'00',X'B9B000',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CU21     0102-16051-16101-1836-00012-00008-00000-SYS210  58
         MACRO
&LABEL   CU21  &R1,&R2,&M3
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
         AIF   (K'&M3 EQ 0).NOMASK
&LABEL   DC    0XL4'00',X'B2A6',AL.4(&M3.,0),AL.4(&R1.,&R2.)
         MEXIT
.NOMASK  ANOP
&LABEL   DC    0XL4'00',X'B2A600',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CU24     0102-16051-16101-1837-00012-00008-00000-SYS210  01
         MACRO
&LABEL   CU24  &R1,&R2,&M3
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
         AIF   (K'&M3 EQ 0).NOMASK
&LABEL   DC    0XL4'00',X'B9B1',AL.4(&M3.,0),AL.4(&R1.,&R2.)
         MEXIT
.NOMASK  ANOP
&LABEL   DC    0XL4'00',X'B9B100',AL.4(&R1.,&R2)
         MEND
./ ADD NAME=CU41     0101-16051-16101-1837-00008-00008-00000-SYS210  03
         MACRO
&LABEL   CU41  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B9B200',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CU42     0101-16051-16101-1837-00008-00008-00000-SYS210  06
         MACRO
&LABEL   CU42  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B9B300',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CVBY     0101-16073-16101-1837-00007-00007-00000-SYS210  08
         MACRO
&LABEL   CVBY  &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,06
         MEND
./ ADD NAME=CVDY     0101-16073-16101-1837-00007-00007-00000-SYS210  11
         MACRO
&LABEL   CVDY  &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,26
         MEND
./ ADD NAME=CXBR     0102-16051-16101-1837-00008-00005-00000-SYS210  14
         MACRO
&LABEL   CXBR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B34900',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CXFBR    0101-16051-16101-1837-00008-00008-00000-SYS210  16
         MACRO
&LABEL   CXFBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B39600',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CXFR     0101-16051-16101-1837-00008-00008-00000-SYS210  18
         MACRO
&LABEL   CXFR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B3B600',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CXR      0101-16051-16101-1837-00008-00008-00000-SYS210  21
         MACRO
&LABEL   CXR   &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B36900',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=CY       0101-16073-16101-1837-00007-00007-00000-SYS210  23
         MACRO
&LABEL   CY    &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,59
         MEND
./ ADD NAME=C0OP     0106-08014-16101-1837-00008-00016-00000-SYS210  29
         MACRO
&LABEL   C0OP  &SUBOP,&R,&I32
.*
.*       Generate C0 Opcodes in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'C0',AL.4(&R.,X'0&SUBOP'),AL4(&I32)
         MEND
./ ADD NAME=C2OP     0107-08014-16101-1837-00008-00016-00000-SYS210  33
         MACRO
&LABEL   C2OP  &SUBOP,&R,&I32
.*
.*       Generate C2 Opcodes in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'C2',AL.4(&R.,X'0&SUBOP'),AL4(&I32)
         MEND
./ ADD NAME=C4OP     0101-16054-16101-1837-00008-00008-00000-SYS210  35
         MACRO
&LABEL   C4OP  &SUBOP,&R,&REL
.*
.*       Generate C0 Opcodes in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'C4',AL.4(&R.,X'0&SUBOP'),AL4(&REL)
         MEND
./ ADD NAME=C6OP     0101-16048-16101-1837-00008-00008-00000-SYS210  38
         MACRO
&LABEL   C6OP  &SUBOP,&R,&I32
.*
.*       Generate C6 Opcodes in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'C6',AL.4(&R.,X'0&SUBOP'),AL4(&I32)
         MEND
./ ADD NAME=C8OP     0103-16079-16101-1837-00023-00022-00000-SYS210  43
         MACRO
&LABEL   C8OP  &BD1,&BD2,&R3,&SUBOP
.*
.*       Generate C8 Opcodes in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00'                Set Instruction Length of 6
.*
.*       Use CLC opcode to generate Base displacement addresses
.*       for C8 instructions. Note: MVC seems like a more logical
.*       choice, but CLC allows both operands to be literals.
.*
         CLC   &BD1,&BD2               OpCode overwritten to C8
.*
.*       Org back to reset the Opcode to C8
.*
         ORG   *-6
         DC    X'C8',AL.4(&R3.,&SUBOP.) Set C8 opcode, R3 and subop
.*
.*       Zero DH byte and set Sub Opcode
.*
         ORG ,                          Back to where we were
         MEND
./ ADD NAME=DDB      0101-16053-16101-1837-00007-00007-00000-SYS210  59
         MACRO
&LABEL   DDB   &R,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R,&BXD,1D
         MEND
./ ADD NAME=DDBR     0102-16053-16101-1838-00008-00008-00000-SYS210  01
         MACRO
&LABEL   DDBR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B31D00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=DEB      0101-16053-16101-1838-00007-00007-00000-SYS210  04
         MACRO
&LABEL   DEB   &R,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R,&BXD,0D
         MEND
./ ADD NAME=DEBR     0102-16053-16101-1838-00008-00008-00000-SYS210  07
         MACRO
&LABEL   DEBR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B30D00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=DIDBR    0101-16053-16101-1838-00008-00008-00000-SYS210  10
         MACRO
&LABEL   DIDBR &R1,&R3,&R2,&M4
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B35B',AL.4(&R3.,&M4.,&R1.,&R2.)
         MEND
./ ADD NAME=DIEBR    0101-16053-16101-1838-00008-00008-00000-SYS210  13
         MACRO
&LABEL   DIEBR &R1,&R3,&R2,&M4
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B353',AL.4(&R3.,&M4.,&R1.,&R2.)
         MEND
./ ADD NAME=DL       0101-16053-16101-1838-00007-00007-00000-SYS210  15
         MACRO
&LABEL   DL    &R,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R,&BXD,97
         MEND
./ ADD NAME=DLR      0102-16053-16101-1838-00008-00008-00000-SYS210  18
         MACRO
&LABEL   DLR   &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B99700',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=DO       8029-74021-16096-1843-00210-00151-00000-SYS210  02
         MACRO
&NAME    DO    &P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,&P11,&P12,&P13,*
               &P14,&P15,&P16,&P17,&P18,&P19,&P20,&WHILE=,&FROM=,&C=,  *
               &UNTIL=                                              SLB
.*#-------------------------------------------------------------------*
.*#      DO    MACRO FOR STRUCTURED PROGRAMMING                       *
.*#
.*#   FUNCTION:    STARTS A NEW DO GROUP
.*#
.*#   CALL(1):     DO WHILE=CC
.*#                   WHILE=COND1
.*#                   WHILE=COND1,OP1,COND2
.*#                   WHILE=COND1,OP1,COND2,OP2,COND3,...,CONDN
.*#                   UNTIL=CC
.*#                   UNTIL=COND1
.*#                   UNTIL=COND1,OP1,COND2
.*#                   UNTIL=COND1,OP1,COND2,OP2,COND3,...,CONDN
.*#
.*#                -   CC    : SIMPLY TESTS THE CURRENT CONDITION CODE
.*#                            VALUE, AND MAY BE SPECIFIED AS A
.*#                            MNEMONIC CONDITION CODE OR A NUMERIC
.*#                            VALUE BETWEEN 1 AND 14. THE CC VALUE
.*#                            CAN BE COMBINED WITH CONDI EXPRESSIONS
.*#                            AS DESCRIBED BELOW.
.*#
.*#                -   CONDI : A VALID ASSEMBLER INSTRUCTION WITH
.*#                            MNEMONIC CONDITION CODE (IN PARENTHESES)
.*#                            EXAMPLE: (TM,SWITCH,X'04',O)
.*#                            FOR COMPARE OPERATIONS THE CONDITION-
.*#                            CODE WILL BE PUT BETWEEN THE OPERANDS
.*#                            EXAMPLE: (CLC,FIELD1,EQ,FIELD2)
.*#                -   OPI:    IS ONE OF THE LOGICAL OPERANDS 'AND' OR
.*#                            'OR'
.*#                            DO NOT MIX 'AND' AND 'OR' IN THE SAME
.*#                            DO GROUP.
.*#
.*#
.*#   CALL(2):     DO  FROM=(REG,INITVAL)
.*#
.*#                -   REG:    LOOP-REGISTER. IT CONTAINS THE NUMBER
.*#                            OF TIMES THE LOOP WILL BE EXECUTED
.*#                -   INITVAL: INITAL VALUE FOR THE LOOP REGISTER
.*#                            THIS PARAMETER MAY BE OMITTED. IN THIS
.*#                            CASE THE MACRO ASSUMES, THAT THE
.*#                            REGISTER IS ALREADY LOADED.
.*#
.*#
.*#   CALL(3):     DO  INF
.*#      OR        DO  FOREVER
.*#
.*#                AN INFINITE LOOP WILL BE GENERATED.
.*#                PLEASE USE THE 'EXIT', 'DOEXIT', OR 'ASMLEAVE'
.*#                TO LEAVE THE LOOP.
.*#
.*#
.*#--------------------------------------------------------------------
         LCLA  &I              INDEX FOR STRING SCANNING
         LCLC  &OPND
         COPY  IFGLO
.*--------------------------------------------------------------------*
.*       FIRST DO/IF: INIT GLOBALS                                    *
.*--------------------------------------------------------------------*
         AIF   (&IFINIT).START            INIT ALREADY DONE
&IFINIT  SETB  1
&IFLEVEL SETA  0
&DOLEVEL SETA  0
&IFLABEL SETA  0
&UNTILLB SETA  0                                                    SLB
&DOXITLB SETA  0                                                    SLB
&IFLIMIT SETA  100000
&IFPRAEF SETC  '##'                    WAR #I
&IFDEBUG SETB  0
.START   ANOP
&MACNA   SETC  'DO'
.*--------------------------------------------------------------------*
.*       FORMAL TESTS                                                 *
.*--------------------------------------------------------23-09-80-RS-*
.FOR01   ANOP
         AIF   ('&WHILE' EQ '').FOR02     NO WHILE PARAM
         AIF   ('&FROM'  NE '').FEHL12    FROM AND WHILE SPECIFIED
         AIF   ('&UNTIL' NE '').FEHL20    UNTIL AND WHILE SPECIFIED SLB
&OPND    SETC  'WHILE='
         AGO   .FOR05
.*
.FOR02   ANOP
         AIF   ('&UNTIL' NE '').FOR03     UNTIL PARAM               SLB
         AIF   ('&FROM' EQ '').FOR03      DO W/O FROM/WHILE/UNTIL   SLB
         AIF   ('&FROM'(1,1) NE '(').FEHL13 NOT IN PARENTHESIS
         AIF   ('&P1' NE '').FEHL14       EXCESIVE PARAMETERS
&OPND    SETC  'FROM='
         AGO   .FOR05
.FOR03   ANOP                                                       SLB
         AIF   ('&UNTIL' EQ '').FOR04     DO WITHOUT FROM/UNTIL     SLB
         AIF   ('&FROM'  NE '').FEHL12    FROM AND UNTIL SPECIFIED  SLB
         AIF   ('&UNTIL'(1,1) NE '(').FEHL13 NOT IN PARENTHESES     SLB
&OPND    SETC  'UNTIL='                                             SLB
         AGO   .FOR05                                               SLB
.*
.FOR04   ANOP
         AIF   ('&P1' EQ 'FOREVER').FOR04A
         AIF   ('&P1' NE 'INF').FEHL15
.FOR04A  ANOP
         AIF   ('&P2' NE '').FEHL18
.FOR05   ANOP
.*--------------------------------------------------------------------*
.*       INCREMENT LEVEL. GENERATE LABELS FOR FALSE/TRUE              *
.*--------------------------------------------------------------------*
&DOLEVEL SETA  &DOLEVEL+1
         AIF   (&DOLEVEL EQ 50).FEHL06
         AIF ('&SYSPARM' EQ '').OBR00
         AIF ('&SYSPARM'(1,2) EQ 'NO').OBR00
         AGO .OBR00
&NAME    SVDOC   COM=START,&OPND,&WHILE&FROM,C=&C,                     *
               &P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,&P11,&P12,&P13,*
               &P14,&P15,&P16,&P17,&P18,&P19,&P20
.*
&OPND    SETC  ''
.*
.OBR00   ANOP
.*
&IFLABEL SETA  &IFLABEL+1
         AIF   (&IFLABEL GE &IFLIMIT).FEHL06
.*
.*--------------------------------------------------------------------*
.*       SAVE NAME OF DO-GROUP FOR EXIT AND ITERATE MACROS         SLB*
.*--------------------------------------------------------24-09-80-RS-*
&DONAME(&DOLEVEL) SETC '&NAME'
.*--------------------------------------------------------------------*
.*       FROM - CLAUSE                                                *
.*--------------------------------------------------------23-09-80-RS-*
         AIF   ('&P1' EQ 'INF').STA03     DO INFINITE
         AIF   ('&P1' EQ 'FOREVER').STA03 DO INFINITE
         AIF   ('&WHILE' NE '').STA03     WHILE SPECIFIED
&DOFROM(&DOLEVEL) SETC '&FROM'            GET LOOP REGISTER
         AIF   ('&FROM(2)' EQ '').STA03   NO INITIAL VALUE
&DOFROM(&DOLEVEL) SETC '&FROM(1)'         GET LOOP REGISTER
         AIF   ('&ASMMREL' NE 'ON').BASE01 CHECK MACRO LEVEL
         LLILF &FROM(1),&FROM(2)          GET INITAL LOOP COUNT (DO)
         AGO   .STA03
.BASE01  ANOP
         LA    &FROM(1),&FROM(2)          GET INITAL LOOP COUNT (DO)
.STA03   ANOP                                                       SLB
.*-----------------------------------------------------------------SLB*
.*       GENERATE BRANCH FOR UNTIL INITIAL ENTRY                   SLB*
.*--------------------------------------------------------23-09-80-SLB*
         AIF   ('&UNTIL' EQ '').NOUNTIL  SKIP IF NOT UNTIL          SLB
&UNTILLB SETA  &UNTILLB+1                BUMP UP UNTIL BRANCH LABEL SLB
&OPND    SETC  '##UN&UNTILLB'            MAKE UNTIL LABEL           SLB
         AIF   ('&ASMMREL' NE 'ON').BASE10 CHECK MACRO LEVEL        SLB
         J     &OPND                     SKIP OVER CONDITIONAL CODE SLB
         AGO   .NOUNTIL                                             SLB
.BASE10  ANOP                                                       SLB
         B     &OPND                     SKIP OVER CONDITIONAL CODE SLB
.NOUNTIL ANOP                                                       SLB
.*--------------------------------------------------------------------*
.*       GENERATE START AND END LABEL                                 *
.*--------------------------------------------------------23-09-80-RS-*
&DOSTART(&DOLEVEL) SETC '&IFLABEL'
&OPND    SETC  '&IFPRAEF&DOSTART(&DOLEVEL)'
&OPND    DS    0H                         TARGET FOR DO-LOOP
.*
&IFLABEL SETA  &IFLABEL+1
         AIF   (&IFLABEL GE &IFLIMIT).FEHL06
.*
&DOENDLB(&DOLEVEL) SETC '&IFLABEL'        TARGET FOR END OF DO-LOOP
         AIF    ('&FROM' NE '').MACEND    DON'T CALL IF-PROCESSOR
         AIF    ('&P1' EQ  'INF').MACEND  DON'T CALL IF-PROCESSOR
         AIF    ('&P1' EQ  'FOREVER').MACEND  DON'T CALL IF-PROCESSOR
.*--------------------------------------------------------------------*
.*       GENERATE LABEL FOR BRANCH ON TRUE                            *
.*--------------------------------------------------------23-09-80-RS-*
&IFLABEL SETA  &IFLABEL+1
         AIF   (&IFLABEL GE &IFLIMIT).FEHL06
.*
&DOTRUE  SETC  '&IFLABEL'
.*
.*--------------------------------------------------------------------*
.*       CALL IF-PROCESSOR TO ANALYZE CONDITION                       *
.*--------------------------------------------------------------------*
         AIF   ('&UNTIL' EQ '').DOWHILE  PROCESS DO UNTIL           SLB
         IFPRO &DOTRUE,&DOENDLB(&DOLEVEL),&UNTIL,                   SLB*
               &P1,&P2,&P3,&P4,&P5,&P6,&P7,                         SLB*
               &P8,&P9,&P10,&P11,&P12,&P13,&P14,&P15,&P16,&P17,     SLB*
               &P18,&P19,&P20                                       SLB
         AGO   .PRODONE                                             SLB
.DOWHILE ANOP                            PROCESS DO WHILE           SLB
         IFPRO &DOTRUE,&DOENDLB(&DOLEVEL),&WHILE,                      *
               &P1,&P2,&P3,&P4,&P5,&P6,&P7,                            *
               &P8,&P9,&P10,&P11,&P12,&P13,&P14,&P15,&P16,&P17,&P18,   *
               &P19,&P20
.PRODONE ANOP                                                       SLB
.*-----------------------------------------------------------------SLB*
.*       SET UNTIL LABEL                                           SLB*
.*-----------------------------------------------------------------SLB*
         AIF   ('&UNTIL' EQ '').NOUNLAB                             SLB
&OPND    SETC  '##UN&UNTILLB'                                       SLB
&OPND    DS    0H                        TARGET FOR DO UNTIL ENTRY  SLB
.NOUNLAB ANOP                                                       SLB
.*--------------------------------------------------------------------*
.*       SET TRUE LABEL                                               *
.*--------------------------------------------------------------------*
&OPND    SETC  '&IFPRAEF&DOTRUE'
&OPND    DS    0H                        TARGET FOR TRUE BRANCH
         AGO   .MACEND
.*
         COPY IFERR
.*
.MACEND  ANOP
         MEXIT
         MEND
./ ADD NAME=DOEXIT   0110-16094-16096-1853-00059-00072-00000-SYS210  05
         MACRO
&NAME    DOEXIT &EXIT,&P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,     SLB*
               &P11,&P12,&P13,&P14,&P15,&P16,&P17,&P18,&P19,&P20    SLB
.*#-------------------------------------------------------------------*
.*#   DOEXIT     MACRO FOR STRUCTURED PROGRAMMING                     *
.*#-------------------------------------------------------------------*
.*#
.*#   FUNCTION:    CONDITIONALLY EXIT FROM CURRENT DO GROUP
.*#
.*#   SYNTAX   DOEXIT       : UNCONDITIONALLY EXITS CURRENT DO GROUP
.*#
.*#            DOEXIT CC    : CONDITIONALLY EXITS CURRENT DO GROUP
.*#                   COND1
.*#                   COND1,OP1,COND2
.*#                   COND1,OP1,COND2,OP2,COND3,...,CONDN
.*#
.*#                -   CC    : SIMPLY TESTS THE CURRENT CONDITION CODE
.*#                            VALUE, AND MAY BE SPECIFIED AS A
.*#                            MNEMONIC CONDITION CODE OR A NUMERIC
.*#                            VALUE BETWEEN 1 AND 14. THE CC VALUE
.*#                            CAN BE COMBINED WITH CONDI EXPRESSIONS
.*#                            AS DESCRIBED BELOW.
.*#
.*#                -   CONDI : A VALID ASSEMBLER INSTRUCTION WITH
.*#                            MNEMONIC CONDITION CODE (IN PARENTHESES)
.*#                            EXAMPLE: (TM,SWITCH,X'04',O)
.*#                            FOR COMPARE OPERATIONS THE CONDITION-
.*#                            CODE WILL BE PUT BETWEEN THE OPERANDS
.*#                            EXAMPLE: (CLC,FIELD1,EQ,FIELD2)
.*#
.*#--------------------------------------------------------------------
         COPY  IFGLO                                                SLB
&MACNA   SETC  'DOEXIT'                                             SLB
         AIF   (&DOLEVEL GT 0).OBR00                                SLB
    MNOTE 12,' ??????  ADIOS LOGIC !!!,     EMERGENCY MESSAGE FROM STRU*
               CTURED PROGRAMMING SYSTEM.'                          SLB
         MEXIT                                                      SLB
.OBR00   ANOP                                                       SLB
.*-----------------------------------------------------------------SLB*
.*       TREAT DOEXIT SANS CONDITION AS ASMLEAVE                   SLB*
.*-----------------------------------------------------------------SLB*
         AIF   ('&EXIT' NE '').DOEXIT                               SLB
         ASMLEAVE ,                                                 SLB
         MEXIT                                                      SLB
.DOEXIT  ANOP                                                       SLB
.*-----------------------------------------------------------------SLB*
.*       GENERATE LABEL FOR BRANCH ON TRUE                         SLB*
.*-----------------------------------------------------------------SLB*
.*                                                                  SLB
&DOTRUE  SETC  '&DOENDLB(&DOLEVEL)'                                 SLB
.*                                                                  SLB
.*-----------------------------------------------------------------SLB*
.*       CALL IF-PROCESSOR TO ANALYZE CONDITION                    SLB*
.*-----------------------------------------------------------------SLB*
         IFPRO &DOTRUE,&DOTRUE,DOEXIT,&EXIT,                        SLB*
               &P1,&P2,&P3,&P4,&P5,&P6,&P7,                         SLB*
               &P8,&P9,&P10,&P11,&P12,&P13,&P14,&P15,&P16,&P17,     SLB*
               &P18,&P19,&P20                                       SLB
         MEND                                                       SLB
./ ADD NAME=DXBR     0101-16053-16101-1838-00008-00008-00000-SYS210  33
         MACRO
&LABEL   DXBR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B34D00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=EBOP     0103-16079-16101-1838-00023-00023-00000-SYS210  37
         MACRO
&LABEL   EBOP  &R1,&R3,&BD,&SUBOP
.*
.*       Generate EB Opcodes in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00'                Set Instruction Length of 6
.*
.*       Use LM opcode to generate Base Register, DL displacement,
.*       and registers for EB instructions
.*
         LM    &R1,&R3,&BD             OpCode overwritten to EB
.*
.*       Org back to reset the Opcode to EB
.*
         ORG   *-4
         DC    X'EB'                   Opcode set to EB
.*
.*       Zero DH byte and set Sub Opcode
.*
         ORG   *+3
         DC    X'00',X'&SUBOP'
         MEND
./ ADD NAME=EDOP     0104-16042-16101-1838-00023-00023-00000-SYS210  39
         MACRO
&LABEL   EDOP  &R,&BXD,&SUBOP
.*
.*       Generate ED Opcodes in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00'                Set Instruction Length of 6
.*
.*       Use IC opcode to generate Base and Index Registers and
.*       DL Displacement for ED instructions
.*
         IC    &R,&BXD                 OpCode overwritten to ED
.*
.*       Org back to reset the Opcode to ED
.*
         ORG   *-4
         DC    X'ED'                   Opcode set to ED
.*
.*       Zero reserved byte and set Sub Opcode
.*
         ORG   *+3
         DC    X'00',X'&SUBOP'
         MEND
./ ADD NAME=EDOP2    0102-16054-16101-1838-00023-00023-00000-SYS210  42
         MACRO
&LABEL   EDOP2 &R1,&R3,&BXD,&SUBOP
.*
.*       Generate ED Opcodes in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00'                Set Instruction Length of 6
.*
.*       Use IC opcode to generate Base and Index Registers and
.*       DL Displacement for ED instructions
.*
         IC    &R3,&BXD                OpCode overwritten to ED
.*
.*       Org back to reset the Opcode to ED
.*
         ORG   *-4
         DC    X'ED'                   Opcode set to ED
.*
.*       Set operand reg and Sub Opcode
.*
         ORG   *+3
         DC    AL.4(&R1.,0),X'&SUBOP'
         MEND
./ ADD NAME=EFPC     0102-16053-16101-1838-00008-00005-00000-SYS210  45
         MACRO
&LABEL   EFPC  &R1
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B38C00',AL.4(&R1.,0)
         MEND
./ ADD NAME=ELSE     8003-74024-16096-1835-00057-00053-00000-SYS210  34
         MACRO
&NAME    ELSE  &COMMENT,&C=
.*#-------------------------------------------------------------------*
.*#  ELSE: MACRO FOR STRUCTURED PROGRAMMING                           *
.*#-------------------------------------------------------------------*
.*#                                                                   *
.*#  FUNCTION: IF ALL PRECEDING CONDITIONS IN THE 'IF' OR 'ELESIF'    *
.*#            MACROS OF THE SAME NESTING LEVEL TURN OUT TO BE        *
.*#            NOT FULLFILLED, THE CODE AFTER THE 'ELSE' MACRO        *
.*#            WILL BE EXECUTED.                                      *
.*#                                                                   *
.*#  CODING:   ELSE     (NO OPERANDS)                                 *
.*#                                                                   *
.*#-------------------------------------------------------------------*
         COPY  IFGLO
         LCLC  &OPND
.*--------------------------------------------------------------------*
.*       GENERATE BRANCH TO ENDIF                                     *
.*--------------------------------------------------------23-09-80-RS-*
.*
&MACNA   SETC  'ELSE'
.*
         AIF ('&SYSPARM' EQ '').OBR00
         AIF ('&SYSPARM'(1,2) EQ 'NO').OBR00
         AGO .OBR00
&NAME    SVDOC  COM=CONT,C=&C
.OBR00   ANOP
         AIF   ('&IFENDLB(&IFLEVEL)' NE '').NOEND  KEIN ENDIF
&IFLABEL SETA  &IFLABEL+1
.*
         AIF   (&IFLABEL GE &IFLIMIT).FEHL06
&IFENDLB(&IFLEVEL) SETC '&IFLABEL'
.*
.NOEND   ANOP
&OPND    SETC  '&IFPRAEF&IFENDLB(&IFLEVEL)'
         AIF   ('&ASMMREL' NE 'ON').BASE01 CHECK MACRO LEVEL        SLB
         J     &OPND                       JUMP TO ENDIF            SLB
         AGO   .BASE02                                              SLB
.BASE01  ANOP
         B     &OPND                       BRANCH TO ENDIF
.BASE02  ANOP                                                       SLB
.*--------------------------------------------------------------------*
.*       GENERATE TARGET FOR BRANCH ON ELSE                           *
.*--------------------------------------------------------23-09-80-RS-*
&OPND    SETC  '&IFPRAEF&IFFALSE(&IFLEVEL)'
&OPND    DS    0H                          TARGET FOR BRANCH ON ELSE
.*--------------------------------------------------------------------*
.*       SIGNAL TO ENDIF: GENERATE NO ELSE LABEL                      *
.*--------------------------------------------------------23-09-80-RS-*
&IFFALSE(&IFLEVEL) SETC ''
         AGO   .MACEND
.*
         COPY  IFERR
.*
.MACEND  ANOP
         MEXIT
         MEND
./ ADD NAME=ELSEIF   8004-74024-16096-1808-00102-00089-00000-SYS210  59
         MACRO
&NAME   ELSEIF &P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,&P11,&P12,&P13,*
               &P14,&P15,&P16,&P17,&P18,&P19,&P20,&C=
.*#-------------------------------------------------------------------*
.*#     ELSEIF MACRO FOR STRUCTURED PROGRAMMING                       *
.*#----------------------------------------------------23-09-80-RS----*
.*#
.*#   FUNCTION:    USED INSTEAD OF AN 'ELSE' TO START A NEW
.*#                CONDITION CLAUSE
.*#
.*#   MODEL:       ELSEIF  CC
.*#                        CONDI
.*#                        COND1,OP1,COND2
.*#                        COND1,OP1,COND2,OP2,COND3,...,CONDN
.*#
.*#                -   CC    : SIMPLY TESTS THE CURRENT CONDITION
.*#                            CODE VALUE, AND MAY BE SPECIFIED AS A
.*#                            MNEMONIC CONDITION CODE OR A NUMERIC
.*#                            VALUE BETWEEN 1 AND 14. THE CC VALUE
.*#                            MUST BE ENCLOSED IN PARENTHESIS AND
.*#                            CAN BE COMBINED WITH CONDI EXPRESSIONS
.*#                            AS DESCRIBED BELOW.
.*#
.*#                -   CONDI : A VALID ASSEMBLE INSTRUCTION WITH
.*#                            MNEMONIC CONDITION CODE (IN PARENTHESES)
.*#                            EXAMPLE: (TM,SWITCH,X'04',O)
.*#                            FOR COMPARE OPERATIONS THE CONDITION-
.*#                            CODE WILL BE PUT BETWEEN THE OPERANDS
.*#                            EXAMPLE: (CLC,FIELD1,EQ,FIELD2)
.*#                -   OPI:    IS ONE OF THE LOGICAL OPERANDS 'AND' OR
.*#                            'OR'
.*#                            DO NOT MIX 'AND' AND 'OR' OPERANDS IN
.*#                            THE SAME ELSEIF.
.*#
.*#--------------------------------------------------------------------
         LCLA  &I              INDEX FOR STRING SCANNING
         LCLC  &OPND
         COPY  IFGLO
.*
&MACNA   SETC  'ELSEIF'
.*
         AIF   ('&P1' EQ '').FEHL17      FORMAL TEST
.*--------------------------------------------------------------------*
.*       GENERATE BRANCH TO ENDIF                                     *
.*--------------------------------------------------------------------*
         AIF ('&SYSPARM' EQ '').OBR00
         AIF ('&SYSPARM'(1,2) EQ 'NO').OBR00
         AGO .OBR00
&NAME    SVDOC   COM=CONT,C=&C,                                        *
               &P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,&P11,&P12,&P13,*
               &P14,&P15,&P16,&P17,&P18,&P19,&P20
.*
.OBR00   ANOP
         AIF   ('&IFENDLB(&IFLEVEL)' NE '').NOEND  NOT ENDIF
&IFLABEL SETA  &IFLABEL+1
         AIF   (&IFLABEL GE &IFLIMIT).FEHL06
&IFENDLB(&IFLEVEL) SETC '&IFLABEL'
.*
.NOEND   ANOP
&OPND    SETC  '&IFPRAEF&IFENDLB(&IFLEVEL)'
         AIF   ('&ASMMREL' NE 'ON').BASE01 CHECK MACRO LEVEL        SLB
         J     &OPND                       JUMP TO ENDIF            SLB
         AGO   .BASE02                                              SLB
.BASE01  ANOP                                                       SLB
         B     &OPND                       BRANCH TO ENDIF
.BASE02  ANOP                                                       SLB
.*--------------------------------------------------------------------*
.*       GENERATE TARGET FOR BRANCH ON ELSE                           *
.*--------------------------------------------------------23-09-80-RS-*
&OPND    SETC  '&IFPRAEF&IFFALSE(&IFLEVEL)'
&OPND    DS    0H                          TARGET FOR BRANCH ON ELSE
.*--------------------------------------------------------------------*
.*       GENERATE NEXT ELSE LABEL                                     *
.*--------------------------------------------------------23-09-80-RS-*
&IFLABEL SETA  &IFLABEL+1
         AIF   (&IFLABEL GE &IFLIMIT).FEHL06
.*
&IFFALSE(&IFLEVEL) SETC '&IFLABEL'         ID FOR NEXT ELSE ON LEVEL
.*--------------------------------------------------------------------*
.*       GENERATE TRUE LABEL                                          *
.*--------------------------------------------------------23-09-80-RS-*
&IFLABEL SETA  &IFLABEL+1
         AIF   (&IFLABEL GE &IFLIMIT).FEHL06
&IFTRUE  SETC  '&IFLABEL'                  TRUE LABEL FOR AND /OR
.*--------------------------------------------------------------------*
.*       CALL IF-PROCESSOR TO ANALYZE CONDITION                       *
.*--------------------------------------------------------------------*
         IFPRO &IFTRUE,&IFFALSE(&IFLEVEL),&P1,&P2,&P3,&P4,&P5,&P6,&P7, *
               &P8,&P9,&P10,&P11,&P12,&P13,&P14,&P15,&P16,&P17,&P18,   *
               &P19,&P20
.*--------------------------------------------------------------------*
.*       SET TRUE LABEL                                               *
.*--------------------------------------------------------------------*
&OPND    SETC  '&IFPRAEF&IFTRUE'
&OPND    DS    0H                        TARGET FOR BANCH ON NOT TRUE
         AGO   .MACEND
.*
         COPY IFERR
.*
.MACEND  ANOP
         MEXIT
         MEND
./ ADD NAME=ENDDO    8005-74024-16096-1831-00070-00066-00000-SYS210  20
         MACRO
&NAME    ENDDO &COMMENT,&C=
.*#-------------------------------------------------------------------*
.*# ENDDO: CLOSE A DO GROUP IN STRUCTURED PROGRAMMING                 *
.*#-------------------------------------------------------------------*
.*#                                                                   *
.*# FUNCTION: CLOSES A DO-LOOP   (= DO GROUP)                         *
.*#                                                                   *
.*#                                                                   *
.*# CODING:   ENDDO           (NO PARAMETERS)                         *
.*#                                                                   *
.*#                                                                   *
.*#-------------------------------------------------------------------*
         COPY  IFGLO
         LCLC  &OPND
.*
&MACNA   SETC  'ENDDO'
.*
         AIF   (&DOLEVEL GT 0).OBR20
    MNOTE 12,' ??????  ADIOS LOGIC !!!,     EMERGENCY MESSAGE FROM STRU*
               CTURED PROGRAMMING SYSTEM.'
         MEXIT
.*
.OBR20   ANOP
         AIF ('&SYSPARM' EQ '').OBR00
         AIF ('&SYSPARM'(1,2) EQ 'NO').OBR00
         AGO     .OBR00
&NAME    SVDOC   COM=END,C=&C
.OBR00   ANOP
.*--------------------------------------------------------------------*
.*       BRANCH BACK TO START IF FROM-REGISTER IS NOT ZERO            *
.*--------------------------------------------------------23-09-80-RS-*
.*
         AIF   ('&DOFROM(&DOLEVEL)' EQ '').WHILE
&OPND    SETC  '&DOFROM(&DOLEVEL),&IFPRAEF&DOSTART(&DOLEVEL)'
         AIF   ('&ASMMREL' NE 'ON').BASE01 CHECK MACRO LEVEL        SLB
         JCT   &DOFROM(&DOLEVEL),&IFPRAEF&DOSTART(&DOLEVEL)         SLB
         AGO   .LOOPEND                                             SLB
.BASE01  ANOP                                                       SLB
         BCT   &OPND                       BRANCH BACK TO START OF LOOP
         AGO   .LOOPEND
.*--------------------------------------------------------------------*
.*       BRANCH BACK TO START OF LOOP IN CASE OF WHILE CONTROL        *
.*--------------------------------------------------------23-09-80-RS-*
.WHILE   ANOP
&OPND    SETC  '&IFPRAEF&DOSTART(&DOLEVEL)'
         AIF   ('&ASMMREL' NE 'ON').BASE02 CHECK MACRO LEVEL        SLB
         J     &OPND                     JUMP BACK TO START OF LOOP SLB
         AGO   .LOOPEND                                             SLB
.BASE02  ANOP                                                       SLB
         B     &OPND                     BRANCH BACK TO START OF LOOP
.*--------------------------------------------------------------------*
.*       GENERATE TARGET LABEL TO LEAVE THE LOOP (PSEUDO ELSE)        *
.*--------------------------------------------------------23-09-80-RS-*
.LOOPEND ANOP
&OPND    SETC  '&IFPRAEF&DOENDLB(&DOLEVEL)'
&OPND    DS    0H                          TARGET TO LEAVE THE LOOP
.*--------------------------------------------------------------------*
.*       RESET FUNCTIONS FOR THIS DO LEVEL                            *
.*--------------------------------------------------------23-09-80-RS-*
.RESET   ANOP
&DOTRUE  SETC  ''
&DOFALSE(&DOLEVEL) SETC ''
&DOENDLB(&DOLEVEL) SETC ''
&DOSTART(&DOLEVEL) SETC ''
&DOFROM(&DOLEVEL)  SETC ''
&DONAME(&DOLEVEL)  SETC ''
&DOLEVEL SETA  &DOLEVEL-1
         MEXIT
         MEND
./ ADD NAME=ENDIF    8002-74024-16096-1827-00050-00053-00000-SYS210  25
         MACRO
&NAME    ENDIF &COMMENT,&C=
.*#-------------------------------------------------------------------*
.*# ENDIF: MACRO  CLOSE CURRENT IF LEVEL                              *
.*#-------------------------------------------------------------------*
.*#                                                                   *
.*# FUNCTION: THE ACTUAL 'IF' LEVEL WILL BE CLOSED.                   *
.*#                                                                   *
.*# CODING:   ENDIF      (NO OPERANDS)                                *
.*#                                                                   *
.*#-------------------------------------------------------------------*
         COPY  IFGLO
         LCLC  &OPND
.*
&MACNA   SETC  'ENDIF'
.*
.*--------------------------------------------------------------------*
.*       IF WITHOUT ELSE: GENERATE ELSE LABEL                         *
.*--------------------------------------------------------23-09-80-RS-*
         AIF   (&IFLEVEL GT 0).OBR20
    MNOTE 12,' ??????  ADIOS LOGIC !!!,     EMERGENCY MESSAGE FROM STRU*
               CTURED PROGRAMMING SYSTEM.'
         MEXIT
.*
.OBR20   ANOP
         AIF ('&SYSPARM' EQ '').OBR00
         AIF ('&SYSPARM'(1,2) EQ 'NO').OBR00
         AGO     .OBR00
&NAME    SVDOC   COM=END,C=&C
.OBR00   ANOP
         AIF   ('&IFFALSE(&IFLEVEL)' EQ '').NOELSE
&OPND    SETC  '&IFPRAEF&IFFALSE(&IFLEVEL)'
&OPND    DS    0H                          TARGET FOR ELSE BRANCH
.NOELSE  ANOP
.*--------------------------------------------------------------------*
.*       GENERATE ENDIF LABEL IF NECESSARY                            *
.*--------------------------------------------------------23-09-80-RS-*
         AIF   ('&IFENDLB(&IFLEVEL)' EQ '').NOENDIF
&OPND    SETC  '&IFPRAEF&IFENDLB(&IFLEVEL)'
&OPND    DS    0H                          TARGET FOR ENDIF BRANCH
.NOENDIF ANOP
.*--------------------------------------------------------------------*
.*       RESET FUNCTIONS OF THIS IF-LEVEL                             *
.*--------------------------------------------------------23-09-80-RS-*
&IFTRUE  SETC  ''
&IFFALSE(&IFLEVEL) SETC ''
&IFENDLB(&IFLEVEL) SETC ''
&IFLEVEL SETA &IFLEVEL-1
         MEXIT
         MEND
./ ADD NAME=EPSW     0102-16053-16101-1839-00008-00005-00000-SYS210  01
         MACRO
&LABEL   EPSW  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B98D00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=EXIT     8003-74024-16096-1847-00069-00068-00000-SYS210  03
         MACRO
&NAME    EXIT  &DO=,&C=
.*#-------------------------------------------------------------------*
.*#   EXIT     MACRO FOR STRUCTURED PROGRAMMING                       *
.*#-------------------------------------------------------------------*
.*#
.*#   FUNCTION:    UNCONDITIONAL EXIT OF ONE OR MORE DO GROUPS.
.*#
.*#   SYNTAX       EXIT      : EXITS CURRENT DO GROUP
.*#
.*#                EXIT  DO=DOGROUP
.*#
.*#                -   DOGROUP: AN ASSEMBLER LABEL OF A DO-GROUP
.*#                            EXITS THE DO GROUP WITH THIS LABEL
.*#                            EXAMPLE: EXIT DO=FIRST
.*#
.*#--------------------------------------------------------------------
         LCLA  &I              INDEX DONAME SCANNING
         LCLC  &OPND
         COPY  IFGLO
.*
&MACNA   SETC  'EXIT'
.*
         AIF   (&DOLEVEL GT 0).OBR20
    MNOTE 12,' ??????  ADIOS LOGIC !!!,     EMERGENCY MESSAGE FROM STRU*
               CTURED PROGRAMMING SYSTEM.'
         MEXIT
.OBR20   ANOP
         AIF ('&SYSPARM' EQ '').OBR00
         AIF ('&SYSPARM'(1,2) EQ 'NO').OBR00
         AGO     .OBR00
&NAME    SVDOC   COM=CONT,&DO,C=&C
.OBR00   ANOP
         AIF   ('&DO' EQ '').NONAME        NAME OF DO GROUP SPECIFIED
.*--------------------------------------------------------------------*
.*       LOOK UP NAME-TABLE FOR CURRENT NAME                          *
.*--------------------------------------------------------------------*
&I       SETA  1
.LOOP    ANOP
         AIF   (&I GT &DOLEVEL).FEHL19
         AIF   ('&DO' EQ '&DONAME(&I)').ENDLOOP  THE NAME IS IN TABLE
&I       SETA  &I+1                              NEXT ELEMENT
         AGO  .LOOP
.ENDLOOP ANOP
         AGO  .GENER
.*--------------------------------------------------------------------*
.*       EXIT CURRENT LOOP                                            *
.*--------------------------------------------------------------------*
.NONAME  ANOP
&I       SETA  &DOLEVEL
.GENER   ANOP
.*
.*--------------------------------------------------------------------*
.*  GENERATE CODE TO EXIT THE SPECIFIED LOOP                          *
.*--------------------------------------------------------------------*
&OPND    SETC  '&IFPRAEF&DOENDLB(&I)'
         AIF   ('&ASMMREL' NE 'ON').BASE01 CHECK MACRO LEVEL        SLB
         J     &OPND                     LEAVE THE LOOP             SLB
         AGO   .MACEND                                              SLB
.BASE01  ANOP                                                       SLB
         B     &OPND                     LEAVE THE LOOP
.*
         AGO   .MACEND
.*
         COPY IFERR
.*
.MACEND  ANOP
         MEXIT
         MEND
./ ADD NAME=EXRL     0101-16056-16101-1839-00007-00007-00000-SYS210  07
         MACRO
&LABEL   EXRL  &R,&LAB
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C6OP  0,&R,(&LAB-*+2)/2
         MEND
./ ADD NAME=E3OP     0104-08014-16101-1839-00033-00035-00000-SYS210  12
         MACRO
&LABEL   E3OP  &R,&BXD,&SUBOP
.*
.*       Generate E3 Opcodes in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00'                Set Instruction Length of 6
         AIF   ('&SUBOP' EQ '3E').GENST     STRV Opcode ?
         AIF   ('&SUBOP' EQ '3F').GENST     STRVH Opcode ?
.*
.*       Use IC opcode to generate Base and Index Registers and
.*       DL Displacement for E3 Load instructions
.*
         IC    &R,&BXD                 OpCode overwritten to E3
         AGO   .GENE3
.*
.*       Use STC opcode to generate Base and Index Registers and
.*       DL Displacement for E3 Store instructions
.*
.GENST   ANOP
         STC   &R,&BXD                 Opcode overwritten to E3
.GENE3   ANOP
.*
.*       Org back to reset the Opcode to E3
.*
         ORG   *-4
         DC    X'E3'                   Opcode set to E3
.*
.*       Zero DH and set Sub Opcode
.*
         ORG   *+3
         DC    X'00',X'&SUBOP'
         MEND
./ ADD NAME=FIDBR    0101-16053-16101-1839-00008-00008-00000-SYS210  15
         MACRO
&LABEL   FIDBR &R1,&M3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B35F',AL.4(&M3,0,&R1.,&R2.)
         MEND
./ ADD NAME=FIDR     0101-16053-16101-1839-00008-00008-00000-SYS210  20
         MACRO
&LABEL   FIDR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B37F00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=FIEBR    0101-16053-16101-1839-00008-00008-00000-SYS210  22
         MACRO
&LABEL   FIEBR &R1,&M3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B357',AL.4(&M3,0,&R1.,&R2.)
         MEND
./ ADD NAME=FIER     0101-16053-16101-1839-00008-00008-00000-SYS210  25
         MACRO
&LABEL   FIER  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B37700',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=FIXBR    0101-16053-16101-1839-00008-00008-00000-SYS210  27
         MACRO
&LABEL   FIXBR &R1,&M3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B347',AL.4(&M3,0,&R1.,&R2.)
         MEND
./ ADD NAME=FIXR     0101-16053-16101-1839-00008-00008-00000-SYS210  32
         MACRO
&LABEL   FIXR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B36700',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=IF       8008-74024-16096-1843-00092-00083-00000-SYS210  23
         MACRO
&NAME    IF    &P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,&P11,&P12,&P13,*
               &P14,&P15,&P16,&P17,&P18,&P19,&P20,&C=
.*#-------------------------------------------------------------------*
.*#      IF    MACRO FOR STRUCTURED PROGRAMMING                       *
.*#----------------------------------------------------23-09-80-RS----*
.*#
.*#   FUNCTION:    STARTS A NEW IF LEVEL
.*#
.*#   MODEL:       IF  CC
.*#                    COND1
.*#                    COND1,OP1,COND2
.*#                    COND1,OP1,COND2,OP2,COND3,...,CONDN
.*#
.*#                -   CC    : SIMPLY TESTS THE CURRENT CONDITION CODE
.*#                            VALUE, AND MAY BE SPECIFIED AS A
.*#                            MNEMONIC CONDITION CODE OR A NUMERIC
.*#                            VALUE BETWEEN 1 AND 14. THE CC VALUE
.*#                            CAN BE COMBINED WITH CONDI EXPRESSIONS
.*#                            AS DESCRIBED BELOW.
.*#
.*#                -   CONDI : A VALID ASSEMBLE INSTRUCTION WITH
.*#                            MNEMONIC CONDITION CODE (IN PARENTHESES)
.*#                            EXAMPLE: (TM,SWITCH,X'04',O)
.*#                            FOR COMPARE OPERATIONS THE CONDITION-
.*#                            CODE WILL BE PUT BETWEEN THE OPERANDS
.*#                            EXAMPLE: (CLC,FIELD1,EQ,FIELD2)
.*#                -   OPI   : IS ONE OF THE LOGICAL OPERANDS 'AND' OR
.*#                            'OR'
.*#                            DO NOT MIX 'AND' AND 'OR' OPERANDS IN
.*#                            THE SAME IF STATEMENT.
.*#
.*#--------------------------------------------------------------------
         LCLA  &I              INDEX FOR STRING SCANNING
         LCLC  &OPND
         COPY  IFGLO
.*--------------------------------------------------------------------*
.*       FIRST IF: INIT GLOBALS                                       *
.*--------------------------------------------------------------------*
         AIF   (&IFINIT).START            INIT ALREADY DONE
&IFINIT  SETB  1
&IFLEVEL SETA  0
&DOLEVEL SETA  0
&IFLABEL SETA  0
&UNTILLB SETA  0                                                    SLB
&DOXITLB SETA  0                                                    SLB
&IFLIMIT SETA  100000
&IFPRAEF SETC  '##'
&IFDEBUG SETB  0
.*--------------------------------------------------------------------*
.*       INCREMENT LEVEL. GENERATE LABELS FOR FALSE/TRUE              *
.*--------------------------------------------------------------------*
.START   ANOP
&MACNA   SETC  'IF'
&IFLEVEL SETA  &IFLEVEL+1
         AIF   (&IFLEVEL EQ 50).FEHL06
         AIF ('&SYSPARM' EQ '').OBR00
         AIF ('&SYSPARM'(1,2) EQ 'NO').OBR00
         AGO     .OBR00
&NAME    SVDOC   COM=START,C=&C,                                       *
               &P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,&P11,&P12,&P13,*
               &P14,&P15,&P16,&P17,&P18,&P19,&P20
.*
.OBR00   ANOP
&IFLABEL SETA  &IFLABEL+1
         AIF   (&IFLABEL GE &IFLIMIT).FEHL06
&IFFALSE(&IFLEVEL) SETC '&IFLABEL'    LABEL FOR BRANCH ON NOT TRUE
&IFTRUE  SETC  ''
         AIF   ('&SYSLIST(2)' EQ '').STA03 NOT TRUE LABEL REQUIRED
&IFLABEL SETA  &IFLABEL+1
         AIF   (&IFLABEL GE &IFLIMIT).FEHL06
&IFTRUE  SETC  '&IFLABEL'                  TRUE LABEL FOR AND /OR
.*--------------------------------------------------------------------*
.*       CALL IF-PROCESSOR TO ANALYZE CONDITION                       *
.*--------------------------------------------------------------------*
.STA03   ANOP
         IFPRO &IFTRUE,&IFFALSE(&IFLEVEL),&P1,&P2,&P3,&P4,&P5,&P6,&P7, *
               &P8,&P9,&P10,&P11,&P12,&P13,&P14,&P15,&P16,&P17,&P18,   *
               &P19,&P20
.*--------------------------------------------------------------------*
.*       SET TRUE LABEL IF NECESSARY                                  *
.*--------------------------------------------------------------------*
         AIF   ('&IFTRUE' EQ '').MACEND
&OPND    SETC  '&IFPRAEF&IFTRUE'
&OPND    DS    0H                        TARGET FOR BANCH ON NOT TRUE
         AGO   .MACEND
.*
         COPY IFERR
.*
.MACEND  ANOP
         MEXIT
         MEND
./ ADD NAME=IFERR    0103-16090-16096-1854-00046-00044-00000-SYS210  36
.*#-------------------------------------------------------------------*
.*# IFERR: ERROR MESSAGES FOR STRUCTURED PROGRAMMING-MACROS           *
.*#-------------------------------------------------------------------*
.FEHL01  MNOTE 8,'IFINIT MACRO NOT FIRST MACRO'
         AGO   .EREND
.FEHL02  MNOTE 8,'PRAEFIX BIGGER THAN 7 POSITIONS'
         AGO   .EREND
.FEHL03  MNOTE 8,'DEBUG NOT YES OR NO'
         AGO   .EREND
.FEHL04  MNOTE 8,'TRACE SWITCH NOT YES OR NO'
         AGO   .EREND
.FEHL05  MNOTE 8,'COMPARE CODE NOT IN PARENTHESIS'
         AGO   .EREND
.FEHL06  MNOTE 8,'TOO MANY STRUCTURED MACROS IN THIS PROGRAM'
         AGO   .EREND
.FEHL07  MNOTE 8,'SHOULD NOT OCCUR'
         AGO   .EREND
.FEHL08  MNOTE 8,'TOO MANY OPERANDS IN CONDITION'
         AGO   .EREND
.FEHL09  MNOTE 8,'NOT ENOUGH OPERANDS IN CONDITION'
         AGO   .EREND
.FEHL10  MNOTE 8,'AND/OR CONTINATION MISSING'
         AGO   .EREND
.FEHL11  MNOTE 8,'INVALID COMPARE OPERANDS'
         AGO   .EREND
.FEHL12  MNOTE 8,'FROM AND WHILE/UNTIL ARE MUTUALLY EXCLUSIVE'
         AGO   .EREND
.FEHL13  MNOTE 8,'WHILE/UNTIL PARAMETER NOT IN BRACKETS'
         AGO   .EREND
.FEHL14  MNOTE 8,'EXCESSIVE PARAMETERS IN DO STATEMENT'
         AGO   .EREND
.FEHL15  MNOTE 8,'INVALID PARAMETER'
         AGO   .EREND
.FEHL16  MNOTE 8,'EXIT NEEDS CONDITION'
         AGO   .EREND
.FEHL17  MNOTE 8,'PARAMETER MISSING'
         AGO   .EREND
.FEHL18  MNOTE 8,'INVALID OPERAND IN DO STATEMENT'
         AGO   .EREND
.FEHL19  MNOTE 8,'DONAME NOT DEFINED'
         AGO   .EREND
.FEHL20  MNOTE 8,'WHILE AND UNTIL ARE MUTUALLY EXCLUSIVE'           SLB
         AGO   .EREND                                               SLB
.EREND   ANOP
         MEXIT
.*--------------------------------------------------------------------*
./ ADD NAME=IFGLO    0105-16090-16096-1844-00031-00029-00000-SYS210  53
.*#-------------------------------------------------------------------*
.*#     GLOBALS FOR MACROS FOR STRUCTURED PROGRAMMING                 *
.*#-------------------------------------------------------------------*
.*
         GBLC  &ASMMREL        -IFGLO-     STRUCT PROG MACRO LEVEL  SLB
.*                             -IFGLO-
         GBLC  &IFFALSE(50)    -IFGLO-     TABELLE DER FALSE IDENTIF.
         GBLC  &IFENDLB(50)    -IFGLO-     TABELLE DER ENDIF ID.
         GBLC  &DOSTART(50)    -IFGLO-     TABELLE DER START ID.
         GBLC  &DOFALSE(50)    -IFGLO-     TABELLE DER FALSE ID.
         GBLC  &DOENDLB(50)    -IFGLO-     TABELLE DER ENDDO ID.
         GBLC  &DOFROM(50)     -IFGLO-     TABELLE DER DO LOOP REG
         GBLC  &DONAME(50)     -IFGLO-     TABELLE DER DO LOOP NAMEN
.*                             -IFGLO-
         GBLC  &IFPRAEF        -IFGLO-     PRAFIX ZUR LABEL ERZEUGUNG
         GBLC  &IFTRUE         -IFGLO-     TRUE LABEL FOR AND/OR
         GBLC  &DOTRUE         -IFGLO-     TRUE LABEL FOR AND/OR
.*                             -IFGLO-
         GBLA  &IFLEVEL        -IFGLO-     NESTING LEVEL
         GBLA  &DOLEVEL        -IFGLO-     NESTING LEVEL
         GBLA  &IFLABEL        -IFGLO-     ELSE ID (COUNTS UP)
         GBLA  &IFLIMIT        -IFGLO-     ENDIF ID (COUNTS DOWN)
         GBLA  &UNTILLB        -IFGLO-     UNTIL BRANCH LABEL       SLB
         GBLA  &DOXITLB        -IFGLO-     DOEXIT BRANCH LABEL      SLB
.*                             -IFGLO-
         GBLB  &IFINIT         -IFGLO-     INIT SWITCH
         GBLB  &IFDEBUG        -IFGLO-     DEBUG MODE
.*       SBU                   -IFGLO-
.*                             -IFGLO-
         GBLC  &MACNA          -IFGLO-     MACRO NAME
.*--------------------------------------------------------------------*
./ ADD NAME=IFPRO    0124-16090-16102-1932-00296-00225-00000-SYS210  09
         MACRO
         IFPRO &TRUE,&FALSE,&P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,  *
               &P11,&P12,&P13,&P14,&P15,&P16,&P17,&P18,&P19,&P20
.*#-------------------------------------------------------------------*
.*#  IFPRO  PROCESSES CONDITION STATEMENTS IN STUCTURED PROGRAMMING   *
.*#-------------------------------------------------------------------*
         COPY  IFGLO
         LCLC  &INST                       INSTRUCTION
         LCLC  &COND                       CONDITION CODE
         LCLC  &OPND                       WORK-FIELD
         LCLA  &I
         LCLC  &OP1
         LCLC  &OP2
         LCLC  &OP3
         LCLC  &OP4
         LCLA  &CCNUM                      NUMERIC CC MASK          SLB
.*--------------------------------------------------------------------*
.*       FORMAL TEST: NUMBER OF OPERANDS  AND 'AND' OR  'OR' PROCESS. *
.*--------------------------------------------------------23-09-80-RS-*
&I       SETA  2                           SKIP TRUE AND FALSE PARAM
         AIF   ('&P1' NE 'DOEXIT').FOR01   HANDLE DOEXIT IDENTIFIER SLB
&DOXITLB SETA  &DOXITLB+1                  COMPUTE FOR DOEXIT LABEL SLB
&I       SETA  3                       SKIP TRUE, FALSE, & 'DOEXIT' SLB
.FOR01   ANOP                                                       SLB
&I       SETA  &I+1
         AIF   ('&SYSLIST(&I)' EQ '').FEHL10
&I       SETA  &I+1
         AIF   ('&SYSLIST(&I)' EQ 'AND').FOR01
         AIF   ('&SYSLIST(&I)' EQ 'OR').FOR01
         AIF   ('&SYSLIST(&I)' NE '').FEHL10
.*--------------------------------------------------------------------*
.*       SCANNING OF THE OPERANDS                                     *
.*--------------------------------------------------------23-09-80-RS-*
&I       SETA  2
         AIF   ('&P1' NE 'DOEXIT').BIGLOP  HANDLE DOEXIT IDENTIFIER SLB
&I       SETA  3                       SKIP TRUE, FALSE, & 'DOEXIT' SLB
.BIGLOP  ANOP
&I       SETA  &I+1
         AIF   ('&SYSLIST(&I)' EQ '').MACEND
.*-----------------------------------------------------------------SLB*
.*       HANDLE POSSIBLE CONDITION CODE MASK                       SLB*
.*-----------------------------------------------------------------SLB*
         AIF   (N'&SYSLIST(&I) NE 1).NOTCC
&COND    SETC  '&SYSLIST(&I,1)'                 GET POSSIBLE CC VAL SLB
         AGO   .ER00                                                SLB
.NOTCC   ANOP                                                       SLB
.*--------------------------------------------------------------------*
.*       WORK ON ASSEMBLE INSTRUCTIONS AND GENERATE CODE              *
.*--------------------------------------------------------23-09-80-RS-*
         AIF   ('&SYSLIST(&I)'(1,1) NE '(').FEHL05 NOT IN PARENTHESES
         AIF   (N'&SYSLIST(&I) GT 6).FEHL08        TOO MANY OPERANDS
         AIF   (N'&SYSLIST(&I) LT 1).FEHL09        TOO FEW OPERANDS
.*--------------------------------------------------------------------*
.*       ELIMINATE OP-CODE AND BRANCH CONDITION                       *
.*--------------------------------------------------------------------*
&INST    SETC  '&SYSLIST(&I,1)'                    GET OP CODE
&OP1     SETC  '&SYSLIST(&I,2)'
         AIF   (N'&SYSLIST(&I) EQ 4).OP4
.*
&OP2     SETC  '&SYSLIST(&I,3)'
         AIF   ('&INST'(1,2) EQ 'CF').OP2C         NOT COMPARE INST SLB
         AIF   ('&INST'(1,4) EQ 'CSST').OP2C       NOT COMPARE INST SLB
         AIF   ('&INST'(1,1) EQ 'C').OP5C          COMPARE INSTRUCTION
.OP2C    ANOP                                      NOT COMPARE INST SLB
         AIF   (N'&SYSLIST(&I) NE 2).OP1C                           SLB
&COND    SETC  '&SYSLIST(&I,2)'                    OP,CC            SLB
         AGO   .OP5END                                              SLB
.OP1C    ANOP                                                       SLB
         AIF   (N'&SYSLIST(&I) NE 3).OP1CA                          SLB
&COND    SETC  '&SYSLIST(&I,3)'                    OP,OP1,CC        SLB
         AGO   .OP5END                                              SLB
.OP1CA   ANOP                                                       SLB
.*
&OP3     SETC  '&SYSLIST(&I,4)'
         AIF   (N'&SYSLIST(&I) EQ 6).OP6C                           SLB
&COND    SETC  '&SYSLIST(&I,5)'                  OP,OP1,OP2,OP3,CC  SLB
         AGO   .OP5END
.OP6C    ANOP                                                       SLB
&OP4     SETC  '&SYSLIST(&I,5)'               OP,OP1,OP2,OP3,OP4,CC SLB
&COND    SETC  '&SYSLIST(&I,6)'                                     SLB
         AGO   .OP5END                                              SLB
.*
.OP5C    ANOP
         AIF   ('&INST'(1,4) EQ 'CLCL').OP5C1      CLCLE/CLCLU      SLB
         AIF   ('&INST'(1,5) EQ 'CLFIT').OP5C1     CLFIT            SLB
         AIF   ('&INST'(1,4) EQ 'CLRT').OP5C1      CLRT             SLB
         AIF   ('&INST'(1,3) EQ 'CRT').OP5C1       CRT              SLB
&OP3     SETC  '&SYSLIST(&I,5)'                   OP,OP1,OP2,CC,OP3 SLB
&COND    SETC  '&SYSLIST(&I,4)'
         AGO   .OP5END                                              SLB
.OP5C1   ANOP                                                       SLB
&OP2     SETC  '&SYSLIST(&I,4)'                   OP,OP1,CC,OP2,OP3 SLB
&OP3     SETC  '&SYSLIST(&I,5)'                                     SLB
&COND    SETC  '&SYSLIST(&I,3)'                                     SLB
.*
.OP5END  ANOP
&OPND    SETC  '&OP1'.','.'&OP2'.','.'&OP3'
         AGO   .OPEND
.OP4     ANOP                                      OP,OP1,OP2,OP3
         AIF   ('&INST'(1,2) EQ 'CK').OP3C         NOT COMPARE INST SLB
         AIF   ('&INST'(1,2) EQ 'CM').OP3C         NOT COMPARE INST SLB
         AIF   ('&INST'(1,4) EQ 'CUSE').OP4C       COMPARE INST     SLB
         AIF   ('&INST'(1,2) EQ 'CU').OP3C         NOT COMPARE INST SLB
         AIF   ('&INST'(1,1) EQ 'C').OP4C          COMPARE INSTRUCTION
         AIF   ('&INST'(1,2) EQ 'KD').OP4C         COMPARE INST     SLB
         AIF   ('&INST'(1,2) EQ 'KE').OP4C         COMPARE INST     SLB
         AIF   ('&INST'(1,2) EQ 'KX').OP4C         COMPARE INST     SLB
.OP3C    ANOP
&OP2     SETC  '&SYSLIST(&I,3)'                       OP,OP1,OP2,CC SLB
&COND    SETC  '&SYSLIST(&I,4)'
         AGO   .OP4END
.*
.OP4C    ANOP
&OP2     SETC  '&SYSLIST(&I,4)'                       OP,OP1,CC,OP2 SLB
&COND    SETC  '&SYSLIST(&I,3)'
.*
.OP4END  ANOP
&OPND    SETC  '&OP1,&OP2'
.*
.OPEND   ANOP
.*--------------------------------------------------------------------*
.*       GENERATE ASSEMBLER INSTRUCTION                               *
.*--------------------------------------------------------23-09-80-RS-*
         COPY  S37XOPS                                              SLB
         AGO   .ER00                                                SLB
.GENIT   ANOP                                                       SLB
         &INST &OPND
.*--------------------------------------------------------------------*
.*       COMPUTE INVERTED CONDITION CODE                              *
.*--------------------------------------------------------23-09-80-RS-*
.ER00    ANOP
         AIF   ('&COND' NE 'H').ER01
&COND    SETC  '2'
         AGO   .ER99
.*
.ER01    ANOP
         AIF   ('&COND' NE 'EQ').ER02
&COND    SETC  '8'
         AGO   .ER99
.*
.ER02    ANOP
         AIF   ('&COND' NE 'L').ER03
&COND    SETC  '4'
         AGO   .ER99
.*
.ER03    ANOP
         AIF   ('&COND' NE 'LE').ER04
&COND    SETC  '13'
         AGO   .ER99
.*
.ER04    ANOP
         AIF   ('&COND' NE 'NH').ER05
&COND    SETC  '13'
         AGO   .ER99
.*
.ER05    ANOP
         AIF   ('&COND' NE 'NL').ER06
&COND    SETC  '11'
         AGO   .ER99
.*
.ER06    ANOP
         AIF   ('&COND' NE 'NE').ER07
&COND    SETC  '7'
         AGO   .ER99
.*
.ER07    ANOP
         AIF   ('&COND' NE 'O').ER08
&COND    SETC  '1'
         AGO   .ER99
.*
.ER08    ANOP
         AIF   ('&COND' NE 'P').ER09
&COND    SETC  '2'
         AGO   .ER99
.*
.ER09    ANOP
         AIF   ('&COND' NE 'M').ER10
&COND    SETC  '4'
         AGO   .ER99
.*
.ER10    ANOP
         AIF   ('&COND' NE 'NP').ER11
&COND    SETC  '13'
         AGO   .ER99
.*
.ER11    ANOP
         AIF   ('&COND' NE 'NM').ER12
&COND    SETC  '11'
         AGO   .ER99
.*
.ER12    ANOP
         AIF   ('&COND' NE 'Z').ER13
&COND    SETC  '8'
         AGO   .ER99
.*
.ER13    ANOP
         AIF   ('&COND' NE 'NZ').ER14
&COND    SETC  '7'
         AGO   .ER99
.*
.ER14    ANOP
         AIF   ('&COND' NE 'NO').ER15
&COND    SETC  '14'
         AGO   .ER99
.*
.ER15    ANOP
         AIF   ('&COND' NE 'E').ER16
&COND    SETC  '8'
         AGO   .ER99
.*
.ER16    ANOP
         AIF   ('&COND' NE 'GE').ER17
&COND    SETC  '11'
         AGO   .ER99
.*
.ER17    ANOP
         AIF   ('&COND' NE 'GT').ER18
&COND    SETC  '2'
         AGO   .ER99
.*
.ER18    ANOP
         AIF   ('&COND' NE 'LT').ER19
&COND    SETC  '4'
         AGO   .ER99
.*
.ER19    ANOP
&CCNUM   SETA  &COND                       S37X EXTENSION ALLOWS    SLB
         AIF   (&CCNUM LT 1).ER20          NUMERIC CONDITION MASK   SLB
         AIF   (&CCNUM GT 14).ER20         FOR COMPATIBLITY WITH    SLB
&COND    SETC  '&CCNUM'                    HLASM TOOLKIT            SLB
         AGO   .ER99                                                SLB
.*
.ER20    ANOP                                                       SLB
         AGO   .FEHL11
.*
.ER99    ANOP
&I       SETA  &I+1
.*-----------------------------------------------------------------SLB*
.*       HANDLE CONDITION CODE TESTING FOR DOEXIT CONDITIONS       SLB*
.*-----------------------------------------------------------------SLB*
         AIF   ('&P1' NE 'DOEXIT').NOTEXIT                          SLB
.*-----------------------------------------------------------------SLB*
.*       GENERATE BRANCH IF TRUE LABEL FOR 'OR' (OR LAST PARAM)    SLB*
.*-----------------------------------------------------------------SLB*
         AIF   ('&SYSLIST(&I)' EQ 'AND').BIGAND                     SLB
&OPND    SETC  '&COND,&IFPRAEF&TRUE'                                SLB
         AIF   ('&ASMMREL' NE 'ON').BASE02 CHECK MACRO LEVEL        SLB
         BRC   &COND,&IFPRAEF&TRUE         BRANCH IF TRUE           SLB
         AGO   .BIGLOP                                              SLB
.BASE02  ANOP                                                       SLB
         BC    &OPND                       BRANCH IF TRUE           SLB
         AGO   .BIGLOP                                              SLB
.*-----------------------------------------------------------------SLB*
.*       GENERATE BRANCH IF NOT TRUE LABEL FOR 'AND'               SLB*
.*-----------------------------------------------------------------SLB*
.BIGAND  ANOP                                                       SLB
&OPND    SETC  '15-&COND,##XT&DOXITLB'                              SLB
         AIF   ('&ASMMREL' NE 'ON').BASE01 CHECK MACRO LEVEL        SLB
         BRC   15-&COND,##XT&DOXITLB       BRANCH IF FALSE          SLB
         AGO   .BIGLOP                                              SLB
.BASE01  ANOP                                                       SLB
         BC    &OPND                       BRANCH IF FALSE          SLB
         AGO   .BIGLOP                                              SLB
.*--------------------------------------------------------------------*
.*       GENERATE BRANCH IF NOT TRUE LABEL (FOR 'AND' OR LAST PARAM)  *
.*--------------------------------------------------------23-09-80-RS-*
.NOTEXIT ANOP                                                       SLB
         AIF   ('&SYSLIST(&I)' EQ 'OR').BIGOR
&OPND    SETC  '15-&COND,&IFPRAEF&FALSE'                            SLB
         AIF   ('&ASMMREL' NE 'ON').BASE11 CHECK MACRO LEVEL        SLB
         BRC   15-&COND,&IFPRAEF&FALSE     BRANCH IF NOT TRUE       SLB
         AGO   .BIGLOP                                              SLB
.BASE11  ANOP                                                       SLB
         BC    &OPND                       BRANCH IF NOT TRUE
         AGO   .BIGLOP
.*--------------------------------------------------------------------*
.*       GENERATE BRANCH IF NOT TRUE LABEL FOR 'OR' PROCESSING        *
.*--------------------------------------------------------23-09-80-RS-*
.BIGOR   ANOP
&OPND    SETC  '&COND,&IFPRAEF&TRUE'
         AIF   ('&ASMMREL' NE 'ON').BASE12 CHECK MACRO LEVEL        SLB
         BRC   &COND,&IFPRAEF&TRUE         BRANCH IF TRUE           SLB
         AGO   .BIGLOP                                              SLB
.BASE12  ANOP                                                       SLB
         BC    &OPND                       BRANCH IF TRUE
         AGO   .BIGLOP
.*
         COPY  IFERR
.*
.MACEND  ANOP
         AIF   ('&P1' NE 'DOEXIT').PRODONE                          SLB
&OPND    SETC  '##XT&DOXITLB'                                       SLB
&OPND    DS    0H                                                   SLB
.PRODONE ANOP                                                       SLB
         MEXIT
         MEND
./ ADD NAME=IILF     0102-08014-16101-1839-00007-00007-00000-SYS210  48
         MACRO
&LABEL   IILF  &R,&I32
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C0OP  9,&R,&I32
         MEND
./ ADD NAME=IILH     0102-08013-16101-1839-00007-00007-00000-SYS210  51
         MACRO
&LABEL   IILH  &R,&I16
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A5OP  2,&R,&I16
         MEND
./ ADD NAME=IILL     0102-08013-16101-1839-00007-00007-00000-SYS210  53
         MACRO
&LABEL   IILL  &R,&I16
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A5OP  3,&R,&I16
         MEND
./ ADD NAME=IPM      0102-08014-16101-1839-00008-00008-00000-SYS210  55
         MACRO
&LABEL   IPM   &R1
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B22200',AL.4(&R1.,X'00')
         MEND
./ ADD NAME=ITERATE  0105-16088-16096-1904-00066-00044-00000-SYS210  36
         MACRO
&NAME    ITERATE &LABEL                                             SLB
.*#-------------------------------------------------------------------*
.*# ITERATE: REPEAT A DO GROUP IN STRUCTURED PROGRAMMING              *
.*#-------------------------------------------------------------------*
.*#                                                                   *
.*# FUNCTION: ITERATES A DO-LOOP   (= DO GROUP)                       *
.*#                                                                   *
.*#                                                                   *
.*# CODING:   ITERATE         (OPTIONAL DO GROUP LABEL)               *
.*#                                                                   *
.*#                                                                   *
.*#-------------------------------------------------------------------*
         COPY  IFGLO                                                SLB
         LCLC  &OPND                                                SLB
         LCLA  &I              INDEX DONAME SCANNING                SLB
.*                                                                  SLB
         AIF   (&DOLEVEL GT 0).OBR00                                SLB
    MNOTE 12,' ??????  ADIOS LOGIC !!!,     EMERGENCY MESSAGE FROM STRU*
               CTURED PROGRAMMING SYSTEM.'                          SLB
         MEXIT                                                      SLB
.*                                                                  SLB
.OBR00   ANOP                                                       SLB
         AIF   ('&LABEL' EQ '').NONAME    FIND LEVEL IF DO LABEL    SLB
.*-----------------------------------------------------------------SLB*
.*       LOOK UP NAME-TABLE FOR SPECIFIED NAME                     SLB*
.*-----------------------------------------------------------------SLB*
&I       SETA  1                                                    SLB
.LOOP    ANOP                                                       SLB
         AIF   (&I GT &DOLEVEL).FEHL19                              SLB
         AIF   ('&LABEL' EQ '&DONAME(&I)').ENDLOOP NAME IS IN TABLE SLB
&I       SETA  &I+1                                NEXT ELEMENT     SLB
         AGO  .LOOP                                                 SLB
.*-----------------------------------------------------------------SLB*
.*       EXIT CURRENT LOOP                                         SLB*
.*-----------------------------------------------------------------SLB*
.NONAME  ANOP                                                       SLB
&I       SETA  &DOLEVEL                                             SLB
.*-----------------------------------------------------------------SLB*
.*       BRANCH BACK TO START IF FROM-REGISTER IS NOT ZERO         SLB*
.*-----------------------------------------------------------------SLB*
.ENDLOOP ANOP                                                       SLB
         AIF   ('&DOFROM(&I)' EQ '').WHILE                          SLB
&OPND    SETC  '&IFPRAEF&DOENDLB(&I)-4'                             SLB
         AIF   ('&ASMMREL' NE 'ON').BASE01 CHECK MACRO LEVEL        SLB
         J     &OPND                    BRANCH TO END OF LOOP (BCT) SLB
         MEXIT                                                      SLB
.BASE01  ANOP                                                       SLB
         B     &OPND                    BRANCH TO END OF LOOP (BCT) SLB
         MEXIT                                                      SLB
.*-----------------------------------------------------------------SLB*
.*       BRANCH BACK TO START OF LOOP IN CASE OF WHILE CONTROL     SLB*
.*-----------------------------------------------------------------SLB*
.WHILE   ANOP                                                       SLB
&OPND    SETC  '&IFPRAEF&DOSTART(&I)'                               SLB
         AIF   ('&ASMMREL' NE 'ON').BASE02 CHECK MACRO LEVEL        SLB
         J     &OPND                   BRANCH BACK TO START OF LOOP SLB
         MEXIT                                                      SLB
.BASE02  ANOP                                                       SLB
         B     &OPND                   BRANCH BACK TO START OF LOOP SLB
         MEXIT                                                      SLB
.*                                                                  SLB
         COPY IFERR                                                 SLB
.*                                                                  SLB
         MEXIT                                                      SLB
         MEND                                                       SLB
./ ADD NAME=J        0103-16044-16101-1840-00007-00007-00000-SYS210  01
         MACRO
&LABEL   J     &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   15,&REL
         MEND
./ ADD NAME=JAS      0101-16045-16101-1840-00007-00007-00000-SYS210  21
         MACRO
&LABEL   JAS   &R,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRAS  &R,&REL
         MEND
./ ADD NAME=JASL     0101-16045-16101-1840-00007-00007-00000-SYS210  24
         MACRO
&LABEL   JASL  &R,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRASL &R,&REL
         MEND
./ ADD NAME=JCT      0102-16045-16101-1840-00007-00007-00000-SYS210  33
         MACRO
&LABEL   JCT   &R1,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCT  &R1,&REL
         MEND
./ ADD NAME=JE       0102-16045-16101-1840-00007-00007-00000-SYS210  35
         MACRO
&LABEL   JE    &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   8,&REL
         MEND
./ ADD NAME=JH       0102-16044-16101-1840-00007-00007-00000-SYS210  37
         MACRO
&LABEL   JH    &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   2,&REL
         MEND
./ ADD NAME=JL       0102-16045-16101-1840-00007-00007-00000-SYS210  40
         MACRO
&LABEL   JL    &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   4,&REL
         MEND
./ ADD NAME=JLE      0101-16045-16101-1840-00007-00007-00000-SYS210  43
         MACRO
&LABEL   JLE   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  8,&REL
         MEND
./ ADD NAME=JLH      0101-16045-16101-1840-00007-00007-00000-SYS210  46
         MACRO
&LABEL   JLH   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  2,&REL
         MEND
./ ADD NAME=JLL      0101-16045-16101-1840-00007-00007-00000-SYS210  48
         MACRO
&LABEL   JLL   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  4,&REL
         MEND
./ ADD NAME=JLM      0101-16045-16101-1840-00007-00007-00000-SYS210  52
         MACRO
&LABEL   JLM   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  4,&REL
         MEND
./ ADD NAME=JLNE     0101-16045-16101-1840-00007-00007-00000-SYS210  55
         MACRO
&LABEL   JLNE  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  7,&REL
         MEND
./ ADD NAME=JLNH     0101-16045-16101-1841-00007-00007-00000-SYS210  00
         MACRO
&LABEL   JLNH  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  13,&REL
         MEND
./ ADD NAME=JLNL     0101-16045-16101-1841-00007-00007-00000-SYS210  03
         MACRO
&LABEL   JLNL  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  11,&REL
         MEND
./ ADD NAME=JLNM     0101-16045-16101-1841-00007-00007-00000-SYS210  17
         MACRO
&LABEL   JLNM  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  11,&REL
         MEND
./ ADD NAME=JLNO     0101-16045-16101-1841-00007-00007-00000-SYS210  20
         MACRO
&LABEL   JLNO  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  14,&REL
         MEND
./ ADD NAME=JLNOP    0101-16045-16101-1841-00007-00007-00000-SYS210  23
         MACRO
&LABEL   JLNOP &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  0,&REL
         MEND
./ ADD NAME=JLNP     0101-16045-16101-1841-00007-00007-00000-SYS210  27
         MACRO
&LABEL   JLNP  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  13,&REL
         MEND
./ ADD NAME=JLNZ     0101-16045-16101-1841-00007-00007-00000-SYS210  29
         MACRO
&LABEL   JLNZ  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  7,&REL
         MEND
./ ADD NAME=JLO      0101-16045-16101-1841-00007-00007-00000-SYS210  47
         MACRO
&LABEL   JLO   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  1,&REL
         MEND
./ ADD NAME=JLP      0101-16045-16101-1841-00007-00007-00000-SYS210  49
         MACRO
&LABEL   JLP   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  2,&REL
         MEND
./ ADD NAME=JLU      0101-16045-16101-1841-00007-00007-00000-SYS210  52
         MACRO
&LABEL   JLU   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  15,&REL
         MEND
./ ADD NAME=JLZ      0101-16045-16101-1841-00007-00007-00000-SYS210  54
         MACRO
&LABEL   JLZ   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRCL  8,&REL
         MEND
./ ADD NAME=JM       0102-16045-16101-1841-00007-00007-00000-SYS210  57
         MACRO
&LABEL   JM    &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   4,&REL
         MEND
./ ADD NAME=JNE      0102-16045-16101-1841-00007-00007-00000-SYS210  59
         MACRO
&LABEL   JNE   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   7,&REL
         MEND
./ ADD NAME=JNH      0102-16045-16101-1842-00007-00007-00000-SYS210  01
         MACRO
&LABEL   JNH   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   13,&REL
         MEND
./ ADD NAME=JNL      0102-16045-16101-1842-00007-00007-00000-SYS210  05
         MACRO
&LABEL   JNL   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   11,&REL
         MEND
./ ADD NAME=JNM      0102-16045-16101-1842-00007-00007-00000-SYS210  07
         MACRO
&LABEL   JNM   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   11,&REL
         MEND
./ ADD NAME=JNO      0102-16045-16101-1842-00007-00007-00000-SYS210  10
         MACRO
&LABEL   JNO   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   14,&REL
         MEND
./ ADD NAME=JNOP     0102-16044-16101-1842-00007-00007-00000-SYS210  13
         MACRO
&LABEL   JNOP  &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   0,&REL
         MEND
./ ADD NAME=JNP      0102-16045-16101-1842-00007-00007-00000-SYS210  15
         MACRO
&LABEL   JNP   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   13,&REL
         MEND
./ ADD NAME=JNZ      0102-16045-16101-1842-00007-00007-00000-SYS210  17
         MACRO
&LABEL   JNZ   &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   7,&REL
         MEND
./ ADD NAME=JO       0102-16045-16101-1842-00007-00007-00000-SYS210  21
         MACRO
&LABEL   JO    &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   1,&REL
         MEND
./ ADD NAME=JP       0102-16045-16101-1842-00007-00007-00000-SYS210  23
         MACRO
&LABEL   JP    &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   2,&REL
         MEND
./ ADD NAME=JXH      0101-16045-16101-1842-00007-00007-00000-SYS210  25
         MACRO
&LABEL   JXH   &R1,&R2,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRXH  &R1,&R2,&REL
         MEND
./ ADD NAME=JXLE     0101-16045-16101-1842-00007-00007-00000-SYS210  28
         MACRO
&LABEL   JXLE  &R1,&R2,&REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRXLE &R1,&R2,&REL
         MEND
./ ADD NAME=JZ       0102-16045-16101-1842-00007-00007-00000-SYS210  31
         MACRO
&LABEL   JZ    &REL
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   BRC   8,&REL
         MEND
./ ADD NAME=KDB      0102-16053-16101-1842-00007-00007-00000-SYS210  35
         MACRO
&LABEL   KDB   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,18
         MEND
./ ADD NAME=KDBR     0103-16053-16101-1842-00008-00005-00000-SYS210  38
         MACRO
&LABEL   KDBR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B31800',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=KEB      0102-16053-16101-1842-00007-00007-00000-SYS210  41
         MACRO
&LABEL   KEB   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,08
         MEND
./ ADD NAME=KEBR     0101-16053-16101-1842-00008-00008-00000-SYS210  53
         MACRO
&LABEL   KEBR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B30800',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=KIMD     0101-16075-16101-1842-00008-00008-00000-SYS210  56
         MACRO
&LABEL   KIMD  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B93E00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=KLMD     0101-16075-16101-1842-00008-00008-00000-SYS210  58
         MACRO
&LABEL   KLMD  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B93F00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=KM       0101-16075-16101-1843-00008-00008-00000-SYS210  00
         MACRO
&LABEL   KM    &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B92E00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=KMAC     0101-16075-16101-1843-00008-00008-00000-SYS210  02
         MACRO
&LABEL   KMAC  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B91E00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=KMC      0101-16075-16101-1843-00008-00008-00000-SYS210  05
         MACRO
&LABEL   KMC   &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B92F00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=KMCTR    0101-16075-16101-1843-00008-00008-00000-SYS210  08
         MACRO
&LABEL   KMCTR &R1,&R3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B92D',AL.4(&R3.,0,&R1.,&R2.)
         MEND
./ ADD NAME=KMF      0101-16075-16101-1843-00008-00008-00000-SYS210  10
         MACRO
&LABEL   KMF   &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B92A00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=KMO      0101-16075-16101-1843-00008-00008-00000-SYS210  13
         MACRO
&LABEL   KMO   &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B92B00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=KXBR     0101-16053-16101-1843-00008-00008-00000-SYS210  19
         MACRO
&LABEL   KXBR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B34800',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LAA      0103-16076-16101-1843-00007-00008-00000-SYS210  21
         MACRO
&LABEL   LAA   &R1,&R3,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EBOP  &R1,&R3,&BD,F8
         MEND
./ ADD NAME=LAAL     0103-16076-16101-1843-00007-00008-00000-SYS210  24
         MACRO
&LABEL   LAAL  &R1,&R3,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EBOP  &R1,&R3,&BD,FA
         MEND
./ ADD NAME=LAN      0102-16076-16101-1843-00007-00008-00000-SYS210  28
         MACRO
&LABEL   LAN   &R1,&R3,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EBOP  &R1,&R3,&BD,F4
         MEND
./ ADD NAME=LAO      0102-16076-16101-1843-00007-00008-00000-SYS210  31
         MACRO
&LABEL   LAO   &R1,&R3,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EBOP  &R1,&R3,&BD,F6
         MEND
./ ADD NAME=LARL     0107-08018-16101-1843-00007-00004-00000-SYS210  34
         MACRO
&NAME    LARL  &R1,&I32
.*
.*       GENERATE OPCODE IN IFOX00
.*
&NAME    C0OP  0,&R1,(&I32-*+2)/2
         MEND
./ ADD NAME=LAX      0102-16076-16101-1843-00007-00008-00000-SYS210  36
         MACRO
&LABEL   LAX   &R1,&R3,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EBOP  &R1,&R3,&BD,F7
         MEND
./ ADD NAME=LB       0103-08014-16101-1843-00007-00007-00000-SYS210  38
         MACRO
&LABEL   LB    &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,76
         MEND
./ ADD NAME=LBR      0102-08014-16101-1843-00008-00008-00000-SYS210  41
         MACRO
&LABEL   LBR   &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B92600',AL.4(&R1.,&R2)
         MEND
./ ADD NAME=LCDBR    0101-16053-16101-1843-00008-00008-00000-SYS210  43
         MACRO
&LABEL   LCDBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B31300',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LCDFR    0101-16076-16101-1843-00008-00008-00000-SYS210  46
         MACRO
&LABEL   LCDFR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B37300',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LCEBR    0101-16053-16101-1843-00008-00008-00000-SYS210  48
         MACRO
&LABEL   LCEBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B30300',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LCXBR    0101-16053-16101-1843-00008-00008-00000-SYS210  50
         MACRO
&LABEL   LCXBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B34300',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LCXR     0101-16053-16101-1843-00008-00008-00000-SYS210  53
         MACRO
&LABEL   LCXR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B36300',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LDE      0102-16053-16101-1843-00007-00007-00000-SYS210  56
         MACRO
&LABEL   LDE   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,24
         MEND
./ ADD NAME=LDEB     0102-16053-16101-1843-00007-00007-00000-SYS210  59
         MACRO
&LABEL   LDEB  &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,04
         MEND
./ ADD NAME=LDEBR    0101-16053-16101-1844-00008-00008-00000-SYS210  01
         MACRO
&LABEL   LDEBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B30400',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LDER     0101-16053-16101-1844-00008-00008-00000-SYS210  03
         MACRO
&LABEL   LDER  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B32400',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LDXBR    0101-16053-16101-1844-00008-00008-00000-SYS210  05
         MACRO
&LABEL   LDXBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B34500',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LEDBR    0101-16053-16101-1844-00008-00008-00000-SYS210  07
         MACRO
&LABEL   LEDBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B34400',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LEXBR    0101-16053-16101-1844-00008-00008-00000-SYS210  09
         MACRO
&LABEL   LEXBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B34600',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LEXR     0101-16053-16101-1844-00008-00008-00000-SYS210  12
         MACRO
&LABEL   LEXR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B36600',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LFAS     0101-16076-16101-1844-00008-00008-00000-SYS210  14
         MACRO
&LABEL   LFAS  &BD
.*
.*       GENERATE OPCODE IN IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B2BD',S(&BD.)
         MEND
./ ADD NAME=LFPC     0102-16053-16101-1844-00008-00005-00000-SYS210  16
         MACRO
&LABEL   LFPC  &BD
.*
.*       GENERATE OPCODE IN IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B29D',S(&BD.)
         MEND
./ ADD NAME=LHI      0103-08013-16101-1844-00007-00007-00000-SYS210  18
         MACRO
&LABEL   LHI   &R1,&I16
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A7OP  8,&R1,&I16
         MEND
./ ADD NAME=LHR      0101-16054-16101-1844-00008-00008-00000-SYS210  21
         MACRO
&LABEL   LHR   &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B92700',AL.4(&R1.,&R2)
         MEND
./ ADD NAME=LHRL     0103-16054-16101-1844-00007-00007-00000-SYS210  23
         MACRO
&NAME    LHRL  &R1,&REL
.*
.*       GENERATE OPCODE IN IFOX00
.*
&NAME    C4OP  5,&R1,(&REL-*+2)/2
         MEND
./ ADD NAME=LHY      0101-16076-16101-1844-00007-00007-00000-SYS210  26
         MACRO
&LABEL   LHY   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,78
         MEND
./ ADD NAME=LLC      0103-08014-16101-1844-00007-00007-00000-SYS210  28
         MACRO
&LABEL   LLC   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,94
         MEND
./ ADD NAME=LLCR     0102-08014-16101-1844-00008-00008-00000-SYS210  31
         MACRO
&LABEL   LLCR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B99400',AL.4(&R1.,&R2)
         MEND
./ ADD NAME=LLH      0103-08014-16101-1844-00007-00007-00000-SYS210  48
         MACRO
&LABEL   LLH   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,95
         MEND
./ ADD NAME=LLHR     0102-08014-16101-1844-00008-00008-00000-SYS210  51
         MACRO
&LABEL   LLHR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B99500',AL.4(&R1.,&R2)
         MEND
./ ADD NAME=LLHRL    0102-16054-16101-1844-00007-00007-00000-SYS210  53
         MACRO
&NAME    LLHRL &R1,&REL
.*
.*       GENERATE OPCODE IN IFOX00
.*
&NAME    C4OP  2,&R1,(&REL-*+2)/2
         MEND
./ ADD NAME=LLILF    0103-08014-16101-1844-00007-00007-00000-SYS210  55
         MACRO
&LABEL   LLILF &R1,&I32
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C0OP  F,&R1,&I32
         MEND
./ ADD NAME=LLILH    0103-08013-16101-1844-00007-00007-00000-SYS210  57
         MACRO
&LABEL   LLILH &R1,&I16
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A5OP  E,&R1,&I16
         MEND
./ ADD NAME=LLILL    0103-08013-16101-1845-00007-00007-00000-SYS210  00
         MACRO
&LABEL   LLILL &R1,&I16
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A5OP  F,&R1,&I16
         MEND
./ ADD NAME=LNDBR    0102-16054-16101-1845-00008-00005-00000-SYS210  03
         MACRO
&LABEL   LNDBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B31100',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LNDFR    0101-16076-16101-1845-00008-00008-00000-SYS210  05
         MACRO
&LABEL   LNDFR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B37100',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LNEBR    0102-16054-16101-1845-00008-00005-00000-SYS210  09
         MACRO
&LABEL   LNEBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B30100',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LNXBR    0102-16054-16101-1845-00008-00005-00000-SYS210  11
         MACRO
&LABEL   LNXBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B34100',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LNXR     0102-16054-16101-1845-00008-00005-00000-SYS210  14
         MACRO
&LABEL   LNXR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B36100',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LOC      0103-16076-16101-1845-00007-00008-00000-SYS210  17
         MACRO
&LABEL   LOC   &R1,&BD,&M3
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EBOP  &R1,&M3,&BD,F2
         MEND
./ ADD NAME=LOCM     0103-16076-16101-1845-00007-00008-00000-SYS210  19
         MACRO
&LABEL   LOCM  &R1,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   LOC   &R1,&BD,4
         MEND
./ ADD NAME=LOCNM    0103-16076-16101-1845-00007-00008-00000-SYS210  21
         MACRO
&LABEL   LOCNM &R1,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   LOC   &R1,&BD,11
         MEND
./ ADD NAME=LOCNO    0102-16076-16101-1845-00007-00008-00000-SYS210  24
         MACRO
&LABEL   LOCNO &R1,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   LOC   &R1,&BD,14
         MEND
./ ADD NAME=LOCNP    0103-16076-16101-1845-00007-00008-00000-SYS210  26
         MACRO
&LABEL   LOCNP &R1,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   LOC   &R1,&BD,13
         MEND
./ ADD NAME=LOCNZ    0103-16076-16101-1845-00007-00008-00000-SYS210  34
         MACRO
&LABEL   LOCNZ &R1,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   LOC   &R1,&BD,7
         MEND
./ ADD NAME=LOCO     0102-16076-16101-1845-00007-00008-00000-SYS210  38
         MACRO
&LABEL   LOCO  &R1,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   LOC   &R1,&BD,1
         MEND
./ ADD NAME=LOCP     0103-16076-16101-1845-00007-00008-00000-SYS210  44
         MACRO
&LABEL   LOCP  &R1,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   LOC   &R1,&BD,2
         MEND
./ ADD NAME=LOCR     0101-16076-16101-1845-00008-00008-00000-SYS210  47
         MACRO
&LABEL   LOCR  &R1,&R2,&M3
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B9F2',AL.4(&M3.,0),AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LOCRM    0102-16076-16101-1845-00007-00008-00000-SYS210  49
         MACRO
&LABEL   LOCRM &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   LOCR  &R1,&R2,4
         MEND
./ ADD NAME=LOCRNM   0101-16076-16101-1845-00007-00007-00000-SYS210  51
         MACRO
&LABEL   LOCRNM &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   LOCR  &R1,&R2,11
         MEND
./ ADD NAME=LOCRNO   0101-16076-16101-1845-00007-00007-00000-SYS210  54
         MACRO
&LABEL   LOCRNO &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   LOCR  &R1,&R2,14
         MEND
./ ADD NAME=LOCRNP   0101-16076-16101-1845-00007-00007-00000-SYS210  58
         MACRO
&LABEL   LOCRNP &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   LOCR  &R1,&R2,13
         MEND
./ ADD NAME=LOCRNZ   0101-16076-16101-1846-00007-00007-00000-SYS210  01
         MACRO
&LABEL   LOCRNZ &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   LOCR  &R1,&R2,7
         MEND
./ ADD NAME=LOCRO    0101-16076-16101-1846-00007-00007-00000-SYS210  03
         MACRO
&LABEL   LOCRO &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   LOCR  &R1,&R2,1
         MEND
./ ADD NAME=LOCRP    0101-16076-16101-1846-00007-00007-00000-SYS210  07
         MACRO
&LABEL   LOCRP &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   LOCR  &R1,&R2,2
         MEND
./ ADD NAME=LOCRZ    0101-16076-16101-1846-00007-00007-00000-SYS210  09
         MACRO
&LABEL   LOCRZ &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   LOCR  &R1,&R2,8
         MEND
./ ADD NAME=LOCZ     0103-16076-16101-1846-00007-00008-00000-SYS210  11
         MACRO
&LABEL   LOCZ  &R1,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   LOC   &R1,&BD,8
         MEND
./ ADD NAME=LPD      0102-16076-16101-1846-00007-00008-00000-SYS210  15
         MACRO
&LABEL   LPD   &R3,&BD1,&BD2
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C8OP  &BD1,&BD2,&R3,4
         MEND
./ ADD NAME=LPDBR    0102-16054-16101-1846-00008-00005-00000-SYS210  18
         MACRO
&LABEL   LPDBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B31000',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LPDFR    0101-16076-16101-1846-00008-00008-00000-SYS210  21
         MACRO
&LABEL   LPDFR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B37000',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LPEBR    0102-16054-16101-1846-00008-00005-00000-SYS210  24
         MACRO
&LABEL   LPEBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B30000',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LPXBR    0102-16054-16101-1846-00008-00005-00000-SYS210  26
         MACRO
&LABEL   LPXBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B34000',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LPXR     0102-16054-16101-1846-00008-00005-00000-SYS210  28
         MACRO
&LABEL   LPXR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B36000',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LRL      0103-16054-16101-1846-00007-00007-00000-SYS210  30
         MACRO
&NAME    LRL   &R1,&REL
.*
.*       GENERATE OPCODE IN IFOX00
.*
&NAME    C4OP  D,&R1,(&REL-*+2)/2
         MEND
./ ADD NAME=LRV      0103-08014-16101-1846-00007-00007-00000-SYS210  32
         MACRO
&LABEL   LRV   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,1E
         MEND
./ ADD NAME=LRVH     0103-08014-16101-1846-00007-00007-00000-SYS210  34
         MACRO
&LABEL   LRVH  &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,1F
         MEND
./ ADD NAME=LRVR     0102-16054-16101-1846-00008-00005-00000-SYS210  42
         MACRO
&LABEL   LRVR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B91F00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LT       0103-08014-16101-1847-00007-00007-00000-SYS210  09
         MACRO
&LABEL   LT    &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,12
         MEND
./ ADD NAME=LTDBR    0102-16054-16101-1847-00008-00005-00000-SYS210  11
         MACRO
&LABEL   LTDBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B31200',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LTEBR    0102-16054-16101-1847-00008-00005-00000-SYS210  13
         MACRO
&LABEL   LTEBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B30200',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LTXBR    0102-16054-16101-1847-00008-00005-00000-SYS210  16
         MACRO
&LABEL   LTXBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B34200',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LTXR     0102-16054-16101-1847-00008-00005-00000-SYS210  19
         MACRO
&LABEL   LTXR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B36200',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LXD      0102-16054-16101-1847-00007-00007-00000-SYS210  21
         MACRO
&LABEL   LXD   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,25
         MEND
./ ADD NAME=LXDB     0102-16054-16101-1847-00007-00007-00000-SYS210  23
         MACRO
&LABEL   LXDB  &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,05
         MEND
./ ADD NAME=LXDBR    0101-16054-16101-1847-00008-00008-00000-SYS210  26
         MACRO
&LABEL   LXDBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B30500',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LXDR     0102-16054-16101-1849-00008-00005-00000-SYS210  39
         MACRO
&LABEL   LXDR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B32500',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LXE      0102-16054-16101-1849-00007-00007-00000-SYS210  42
         MACRO
&LABEL   LXE   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,26
         MEND
./ ADD NAME=LXEB     0102-16054-16101-1849-00007-00007-00000-SYS210  50
         MACRO
&LABEL   LXEB  &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,06
         MEND
./ ADD NAME=LXEBR    0101-16054-16101-1849-00008-00008-00000-SYS210  53
         MACRO
&LABEL   LXEBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B30600',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LXER     0102-16054-16101-1849-00008-00005-00000-SYS210  56
         MACRO
&LABEL   LXER  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B32600',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LXR      0102-16054-16101-1849-00008-00005-00000-SYS210  59
         MACRO
&LABEL   LXR   &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B36500',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=LY       0101-16076-16101-1850-00007-00007-00000-SYS210  02
         MACRO
&LABEL   LY    &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,58
         MEND
./ ADD NAME=LZDR     0103-16054-16101-1850-00008-00005-00000-SYS210  05
         MACRO
&LABEL   LZDR  &R1
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B37500',AL.4(&R1.,0)
         MEND
./ ADD NAME=LZER     0101-16054-16101-1850-00008-00008-00000-SYS210  10
         MACRO
&LABEL   LZER  &R1
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B37400',AL.4(&R1.,0)
         MEND
./ ADD NAME=LZXR     0101-16054-16101-1850-00008-00008-00000-SYS210  12
         MACRO
&LABEL   LZXR  &R1
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B37600',AL.4(&R1.,0)
         MEND
./ ADD NAME=MAD      0103-16054-16101-1850-00007-00007-00000-SYS210  15
         MACRO
&LABEL   MAD   &R1,&R3,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP2 &R1,&R3,&BXD,3E
         MEND
./ ADD NAME=MADB     0101-16054-16101-1850-00007-00007-00000-SYS210  19
         MACRO
&LABEL   MADB  &R1,&R3,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP2 &R1,&R3,&BXD,1E
         MEND
./ ADD NAME=MADBR    0101-16055-16101-1850-00008-00008-00000-SYS210  21
         MACRO
&LABEL   MADBR &R1,&R3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B31E',AL.4(&R1.,0),AL.4(&R3.,&R2.)
         MEND
./ ADD NAME=MADR     0101-16055-16101-1850-00008-00008-00000-SYS210  25
         MACRO
&LABEL   MADR  &R1,&R3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B33E',AL.4(&R1.,0),AL.4(&R3.,&R2.)
         MEND
./ ADD NAME=MAE      0101-16055-16101-1850-00007-00007-00000-SYS210  27
         MACRO
&LABEL   MAE   &R1,&R3,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP2 &R1,&R3,&BXD,2E
         MEND
./ ADD NAME=MAEB     0101-16055-16101-1850-00007-00007-00000-SYS210  29
         MACRO
&LABEL   MAEB  &R1,&R3,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP2 &R1,&R3,&BXD,0E
         MEND
./ ADD NAME=MAEBR    0101-16055-16101-1850-00008-00008-00000-SYS210  33
         MACRO
&LABEL   MAEBR &R1,&R3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B30E',AL.4(&R1.,0),AL.4(&R3.,&R2.)
         MEND
./ ADD NAME=MAER     0101-16055-16101-1850-00008-00008-00000-SYS210  35
         MACRO
&LABEL   MAER  &R1,&R3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B32E',AL.4(&R1.,0),AL.4(&R3.,&R2.)
         MEND
./ ADD NAME=MAY      0101-16055-16101-1850-00007-00007-00000-SYS210  37
         MACRO
&LABEL   MAY   &R1,&R3,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP2 &R1,&R3,&BXD,3A
         MEND
./ ADD NAME=MAYH     0101-16055-16101-1850-00007-00007-00000-SYS210  42
         MACRO
&LABEL   MAYH  &R1,&R3,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP2 &R1,&R3,&BXD,3C
         MEND
./ ADD NAME=MAYHR    0101-16055-16101-1850-00008-00008-00000-SYS210  45
         MACRO
&LABEL   MAYHR &R1,&R3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B33C',AL.4(&R1.,0),AL.4(&R3.,&R2.)
         MEND
./ ADD NAME=MAYL     0101-16055-16101-1850-00007-00007-00000-SYS210  54
         MACRO
&LABEL   MAYL  &R1,&R3,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP2 &R1,&R3,&BXD,38
         MEND
./ ADD NAME=MAYLR    0101-16055-16101-1850-00008-00008-00000-SYS210  56
         MACRO
&LABEL   MAYLR &R1,&R3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B338',AL.4(&R1.,0),AL.4(&R3.,&R2.)
         MEND
./ ADD NAME=MAYR     0101-16055-16101-1850-00008-00008-00000-SYS210  58
         MACRO
&LABEL   MAYR  &R1,&R3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B33A',AL.4(&R1.,0),AL.4(&R3.,&R2.)
         MEND
./ ADD NAME=MDB      0102-16055-16101-1851-00007-00007-00000-SYS210  01
         MACRO
&LABEL   MDB   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,1C
         MEND
./ ADD NAME=MDBR     0101-16055-16101-1851-00008-00008-00000-SYS210  03
         MACRO
&LABEL   MDBR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B31C00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=MDEB     0102-16055-16101-1851-00007-00007-00000-SYS210  05
         MACRO
&LABEL   MDEB  &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,0C
         MEND
./ ADD NAME=MDEBR    0101-16055-16101-1851-00008-00008-00000-SYS210  07
         MACRO
&LABEL   MDEBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B30C00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=MEE      0102-16055-16101-1851-00007-00007-00000-SYS210  09
         MACRO
&LABEL   MEE   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,37
         MEND
./ ADD NAME=MEEB     0102-16055-16101-1851-00007-00007-00000-SYS210  12
         MACRO
&LABEL   MEEB  &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,17
         MEND
./ ADD NAME=MEEBR    0101-16055-16101-1851-00008-00008-00000-SYS210  14
         MACRO
&LABEL   MEEBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B31700',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=MEER     0101-16055-16101-1851-00008-00008-00000-SYS210  27
         MACRO
&LABEL   MEER  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B33700',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=MFY      0102-16055-16101-1851-00007-00007-00000-SYS210  29
         MACRO
&LABEL   MFY   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,5C
         MEND
./ ADD NAME=MHI      0103-08014-16101-1851-00007-00007-00000-SYS210  31
         MACRO
&LABEL   MHI   &R1,&I16
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A7OP  C,&R1,&I16
         MEND
./ ADD NAME=MHY      0102-16055-16101-1851-00007-00007-00000-SYS210  34
         MACRO
&LABEL   MHY   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,7C
         MEND
./ ADD NAME=ML       0102-16055-16101-1851-00007-00007-00000-SYS210  36
         MACRO
&LABEL   ML    &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,96
         MEND
./ ADD NAME=MLR      0101-16055-16101-1851-00008-00008-00000-SYS210  40
         MACRO
&LABEL   MLR   &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B99600',AL.4(&R1.,&R2)
         MEND
./ ADD NAME=MS       0102-16055-16101-1851-00022-00022-00000-SYS210  43
         MACRO
&LABEL   MS    &R1,&BXD
.*
.*       Generate MS instruction in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00'                Set Instruction Length of 4
.*
.*       Use IC opcode to generate Base and Index Registers and
.*       DL Displacement for MS instruction
.*
         IC    &R1,&BXD                OpCode overwritten to 71
.*
.*       Org back to reset the Opcode to 71
.*
         ORG   *-4
         DC    X'71'                   Opcode set to 71
.*
.*       Get back to where we were
.*
         ORG
         MEND
./ ADD NAME=MSD      0101-16055-16101-1851-00007-00007-00000-SYS210  47
         MACRO
&LABEL   MSD   &R1,&R3,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP2 &R1,&R3,&BXD,3F
         MEND
./ ADD NAME=MSDB     0101-16055-16101-1851-00007-00007-00000-SYS210  49
         MACRO
&LABEL   MSDB  &R1,&R3,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP2 &R1,&R3,&BXD,1F
         MEND
./ ADD NAME=MSDBR    0101-16055-16101-1851-00008-00008-00000-SYS210  53
         MACRO
&LABEL   MSDBR &R1,&R3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B31F',AL.4(&R1.,0),AL.4(&R3.,&R2.)
         MEND
./ ADD NAME=MSDR     0101-16055-16101-1851-00008-00008-00000-SYS210  56
         MACRO
&LABEL   MSDR  &R1,&R3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B33F',AL.4(&R1.,0),AL.4(&R3.,&R2.)
         MEND
./ ADD NAME=MSE      0101-16055-16101-1852-00007-00007-00000-SYS210  00
         MACRO
&LABEL   MSE   &R1,&R3,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP2 &R1,&R3,&BXD,2F
         MEND
./ ADD NAME=MSEB     0101-16055-16101-1852-00007-00007-00000-SYS210  03
         MACRO
&LABEL   MSEB  &R1,&R3,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP2 &R1,&R3,&BXD,0F
         MEND
./ ADD NAME=MSEBR    0101-16055-16101-1852-00008-00008-00000-SYS210  05
         MACRO
&LABEL   MSEBR &R1,&R3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B30F',AL.4(&R1.,0),AL.4(&R3.,&R2.)
         MEND
./ ADD NAME=MSER     0101-16055-16101-1852-00008-00008-00000-SYS210  07
         MACRO
&LABEL   MSER  &R1,&R3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B32F',AL.4(&R1.,0),AL.4(&R3.,&R2.)
         MEND
./ ADD NAME=MSFI     0102-16056-16101-1852-00007-00007-00000-SYS210  10
         MACRO
&LABEL   MSFI  &R1,&I32
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C2OP  1,&R1,&I32
         MEND
./ ADD NAME=MSR      0101-16056-16101-1852-00008-00008-00000-SYS210  13
         MACRO
&LABEL   MSR   &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B25200',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=MSY      0101-16082-16101-1852-00007-00007-00000-SYS210  16
         MACRO
&LABEL   MSY   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,51
         MEND
./ ADD NAME=MVCLE    0101-16056-16101-1852-00008-00008-00000-SYS210  18
         MACRO
&LABEL   MVCLE &R1,&R3,&BD
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'A8',AL.4(&R1.,&R3.),S(&BD.)
         MEND
./ ADD NAME=MVCLU    0103-16056-16101-1852-00007-00008-00000-SYS210  20
         MACRO
&LABEL   MVCLU &R1,&R3,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EBOP  &R1,&R3,&BD,8E
         MEND
./ ADD NAME=MVGHI    0102-16046-16101-1852-00008-00008-00000-SYS210  22
         MACRO
&LABEL   MVGHI &BD,&I16
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'E548',S(&BD),AL2(&I16)
         MEND
./ ADD NAME=MVHHI    0101-16056-16101-1852-00008-00008-00000-SYS210  25
         MACRO
&LABEL   MVHHI &BD,&I16
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'E544',S(&BD),AL2(&I16)
         MEND
./ ADD NAME=MVHI     0101-16056-16101-1852-00008-00008-00000-SYS210  27
         MACRO
&LABEL   MVHI  &BD,&I16
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00',X'E54C',S(&BD),AL2(&I16)
         MEND
./ ADD NAME=MVST     0101-16056-16101-1852-00008-00008-00000-SYS210  29
         MACRO
&LABEL   MVST  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B25500',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=MXBR     0101-16056-16101-1852-00008-00008-00000-SYS210  31
         MACRO
&LABEL   MXBR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B34C00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=MXDB     0102-16056-16101-1852-00007-00007-00000-SYS210  33
         MACRO
&LABEL   MXDB  &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,07
         MEND
./ ADD NAME=MXDBR    0101-16056-16101-1852-00008-00008-00000-SYS210  35
         MACRO
&LABEL   MXDBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B30700',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=MY       0101-16056-16101-1852-00007-00007-00000-SYS210  38
         MACRO
&LABEL   MY    &R1,&R3,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP2 &R1,&R3,&BXD,3B
         MEND
./ ADD NAME=MYH      0101-16056-16101-1852-00007-00007-00000-SYS210  45
         MACRO
&LABEL   MYH   &R1,&R3,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP2 &R1,&R3,&BXD,3D
         MEND
./ ADD NAME=MYHR     0101-16056-16101-1852-00008-00008-00000-SYS210  48
         MACRO
&LABEL   MYHR  &R1,&R3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B33D',AL.4(&R1.,0),AL.4(&R3.,&R2.)
         MEND
./ ADD NAME=MYL      0101-16056-16101-1852-00007-00007-00000-SYS210  55
         MACRO
&LABEL   MYL   &R1,&R3,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP2 &R1,&R3,&BXD,39
         MEND
./ ADD NAME=MYLR     0101-16056-16101-1852-00008-00008-00000-SYS210  57
         MACRO
&LABEL   MYLR  &R1,&R3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B339',AL.4(&R1.,0),AL.4(&R3.,&R2.)
         MEND
./ ADD NAME=MYR      0101-16056-16101-1853-00008-00008-00000-SYS210  19
         MACRO
&LABEL   MYR   &R1,&R3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B33B',AL.4(&R1.,0),AL.4(&R3.,&R2.)
         MEND
./ ADD NAME=NILF     0102-08014-16101-1853-00007-00007-00000-SYS210  21
         MACRO
&LABEL   NILF  &R,&I32
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C0OP  B,&R,&I32
         MEND
./ ADD NAME=NILH     0102-08013-16101-1853-00007-00007-00000-SYS210  23
         MACRO
&LABEL   NILH  &R,&I16
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A5OP  6,&R,&I16
         MEND
./ ADD NAME=NILL     0102-08013-16101-1853-00007-00007-00000-SYS210  25
         MACRO
&LABEL   NILL  &R,&I16
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A5OP  7,&R,&I16
         MEND
./ ADD NAME=NRK      0101-16083-16101-1853-00008-00008-00000-SYS210  27
         MACRO
&LABEL   NRK   &R1,&R2,&R3
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B9F4',AL.4(&R3.,0),AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=NY       0101-16083-16101-1853-00007-00007-00000-SYS210  29
         MACRO
&LABEL   NY    &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,54
         MEND
./ ADD NAME=OILF     0102-08014-16101-1853-00007-00007-00000-SYS210  31
         MACRO
&LABEL   OILF  &R,&I32
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C0OP  D,&R,&I32
         MEND
./ ADD NAME=OILH     0102-08013-16101-1853-00007-00007-00000-SYS210  40
         MACRO
&LABEL   OILH  &R,&I16
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A5OP  A,&R,&I16
         MEND
./ ADD NAME=OILL     0102-08013-16101-1853-00007-00007-00000-SYS210  44
         MACRO
&LABEL   OILL  &R,&I16
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A5OP  B,&R,&I16
         MEND
./ ADD NAME=ORK      0101-16083-16101-1853-00008-00008-00000-SYS210  46
         MACRO
&LABEL   ORK   &R1,&R2,&R3
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B9F6',AL.4(&R3.,0),AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=OY       0101-16083-16101-1853-00007-00007-00000-SYS210  48
         MACRO
&LABEL   OY    &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,56
         MEND
./ ADD NAME=PCC      0101-16083-16101-1853-00008-00008-00000-SYS210  50
         MACRO
&LABEL   PCC
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B92C0000'
         MEND
./ ADD NAME=PCKMO    0101-16083-16101-1853-00008-00008-00000-SYS210  53
         MACRO
&LABEL   PCKMO
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B9280000'
         MEND
./ ADD NAME=PFD      0101-16056-16101-1854-00007-00007-00000-SYS210  00
         MACRO
&LABEL   PFD   &M,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &M,&BXD,36
         MEND
./ ADD NAME=PFDRL    0101-16056-16101-1854-00007-00007-00000-SYS210  02
         MACRO
&LABEL   PFDRL &M,&LAB
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C6OP  2,&M,(&LAB-*+2)/2
         MEND
./ ADD NAME=PKA      0105-16057-16101-1854-00022-00022-00000-SYS210  04
         MACRO
&LABEL   PKA   &BD1,&BD2
.*
.*       Generate PKA instruction in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00'                Set Instruction Length of 6
.*
.*       Use MVC opcode to generate Base and Displacement fields
.*
         MVC   &BD1,&BD2               OpCode overwritten to E9
.*
.*       Org back to reset the Opcode to E9
.*
         ORG   *-6
         DC    X'E9'                   Opcode set to E9
         DC    AL1(L'&BD2-1)           Set 2nd operand length
.*
.*       Get back to where we were
.*
         ORG
         MEND
./ ADD NAME=PKU      0103-16058-16101-1854-00022-00023-00000-SYS210  08
         MACRO
&LABEL   PKU   &BD1,&BD2
.*
.*       Generate PKU instruction in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00'                Set Instruction Length of 6
.*
.*       Use MVC opcode to generate Base and Displacement fields
.*
         MVC   &BD1,&BD2               OpCode overwritten to E1
.*
.*       Org back to reset the Opcode to E1
.*
         ORG   *-6
         DC    X'E1'                   Opcode set to E1
         DC    AL1(L'&BD2-1)           Set 2nd operand length
.*
.*       Get back to where we were
.*
         ORG
         MEND
./ ADD NAME=RLL      0102-16058-16101-1854-00007-00008-00000-SYS210  10
         MACRO
&LABEL   RLL   &R1,&R3,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EBOP  &R1,&R3,&BD,1D
         MEND
./ ADD NAME=SAM24    0102-08013-16101-1854-00008-00008-00000-SYS210  17
         MACRO
&LABEL   SAM24
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL2'00',X'010C'
         MEND
./ ADD NAME=SAM31    0102-08013-16101-1854-00008-00008-00000-SYS210  19
         MACRO
&LABEL   SAM31
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL2'00',X'010D'
         MEND
./ ADD NAME=SDB      0102-16058-16101-1854-00007-00007-00000-SYS210  21
         MACRO
&LABEL   SDB   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,1B
         MEND
./ ADD NAME=SDBR     0101-16058-16101-1854-00008-00008-00000-SYS210  23
         MACRO
&LABEL   SDBR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B31B00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=SEB      0102-16058-16101-1854-00007-00007-00000-SYS210  25
         MACRO
&LABEL   SEB   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,0B
         MEND
./ ADD NAME=SEBR     0101-16058-16101-1854-00008-00008-00000-SYS210  27
         MACRO
&LABEL   SEBR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B30B00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=SFASR    0102-16084-16101-1854-00008-00008-00000-SYS210  29
         MACRO
&LABEL   SFASR &R1
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B38500',AL.4(&R1.,0)
         MEND
./ ADD NAME=SFPC     0101-16058-16101-1854-00008-00008-00000-SYS210  31
         MACRO
&LABEL   SFPC  &R1
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B38400',AL.4(&R1.,0)
         MEND
./ ADD NAME=SHY      0101-16084-16101-1854-00007-00007-00000-SYS210  33
         MACRO
&LABEL   SHY   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,7B
         MEND
./ ADD NAME=SLAK     0101-16079-16101-1854-00007-00007-00000-SYS210  37
         MACRO
&LABEL   SLAK  &R1,&R3,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EBOP  &R1,&R3,&BD,DD
         MEND
./ ADD NAME=SLB      0101-16058-16101-1854-00007-00007-00000-SYS210  40
         MACRO
&LABEL   SLB   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,99
         MEND
./ ADD NAME=SLBR     0101-16058-16101-1854-00008-00008-00000-SYS210  42
         MACRO
&LABEL   SLBR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B99900',AL.4(&R1.,&R2)
         MEND
./ ADD NAME=SLFI     0103-08014-16101-1854-00007-00007-00000-SYS210  45
         MACRO
&LABEL   SLFI  &R1,&I32
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C2OP  5,&R1,&I32
         MEND
./ ADD NAME=SLLK     0101-16079-16101-1854-00007-00007-00000-SYS210  48
         MACRO
&LABEL   SLLK  &R1,&R3,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EBOP  &R1,&R3,&BD,DF
         MEND
./ ADD NAME=SLRK     0101-16084-16101-1854-00008-00008-00000-SYS210  52
         MACRO
&LABEL   SLRK  &R1,&R2,&R3
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B9FB',AL.4(&R3.,0),AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=SLY      0101-16084-16101-1854-00007-00007-00000-SYS210  54
         MACRO
&LABEL   SLY   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,5F
         MEND
./ ADD NAME=SQD      0102-16058-16101-1855-00007-00007-00000-SYS210  06
         MACRO
&LABEL   SQD   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,35
         MEND
./ ADD NAME=SQDB     0102-16058-16101-1855-00007-00007-00000-SYS210  08
         MACRO
&LABEL   SQDB  &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,15
         MEND
./ ADD NAME=SQDBR    0101-16058-16101-1855-00008-00008-00000-SYS210  10
         MACRO
&LABEL   SQDBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B31500',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=SQDR     0101-16058-16101-1855-00008-00008-00000-SYS210  27
         MACRO
&LABEL   SQDR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B24400',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=SQE      0102-16058-16101-1855-00007-00007-00000-SYS210  29
         MACRO
&LABEL   SQE   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,34
         MEND
./ ADD NAME=SQEB     0102-16058-16101-1855-00007-00007-00000-SYS210  32
         MACRO
&LABEL   SQEB  &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R1,&BXD,14
         MEND
./ ADD NAME=SQEBR    0101-16058-16101-1855-00008-00008-00000-SYS210  34
         MACRO
&LABEL   SQEBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B31400',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=SQER     0101-16058-16101-1855-00008-00008-00000-SYS210  37
         MACRO
&LABEL   SQER  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B24500',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=SQXBR    0101-16058-16101-1855-00008-00008-00000-SYS210  39
         MACRO
&LABEL   SQXBR &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B31600',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=SQXR     0101-16058-16101-1855-00008-00008-00000-SYS210  40
         MACRO
&LABEL   SQXR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B33600',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=SRAK     0101-16079-16101-1855-00007-00007-00000-SYS210  42
         MACRO
&LABEL   SRAK  &R1,&R3,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EBOP  &R1,&R3,&BD,DC
         MEND
./ ADD NAME=SRK      0101-16084-16101-1855-00008-00008-00000-SYS210  44
         MACRO
&LABEL   SRK   &R1,&R2,&R3
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B9F9',AL.4(&R3.,0),AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=SRLK     0101-16079-16101-1857-00007-00007-00000-SYS210  04
         MACRO
&LABEL   SRLK  &R1,&R3,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EBOP  &R1,&R3,&BD,DE
         MEND
./ ADD NAME=SRNM     0101-16058-16101-1857-00008-00008-00000-SYS210  05
         MACRO
&LABEL   SRNM  &BD
.*
.*       GENERATE OPCODE IN IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B299',S(&BD.)
         MEND
./ ADD NAME=SRST     0101-16058-16101-1857-00008-00008-00000-SYS210  08
         MACRO
&LABEL   SRST  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B25E00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=SRSTU    0101-16058-16101-1857-00008-00008-00000-SYS210  10
         MACRO
&LABEL   SRSTU &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B9BE00',AL.4(&R1.,&R2)
         MEND
./ ADD NAME=STFPC    0101-16058-16101-1857-00008-00008-00000-SYS210  13
         MACRO
&LABEL   STFPC &BD
.*
.*       GENERATE OPCODE IN IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B29C',S(&BD.)
         MEND
./ ADD NAME=STHRL    0102-16058-16101-1857-00007-00007-00000-SYS210  15
         MACRO
&NAME    STHRL &R1,&REL
.*
.*       GENERATE OPCODE IN IFOX00
.*
&NAME    C4OP  7,&R1,(&REL-*+2)/2
         MEND
./ ADD NAME=STOC     0101-16079-16101-1857-00007-00007-00000-SYS210  17
         MACRO
&LABEL   STOC  &R1,&BD,&M3
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EBOP  &R1,&M3,&BD,F3
         MEND
./ ADD NAME=STOCM    0101-16084-16101-1857-00007-00007-00000-SYS210  19
         MACRO
&LABEL   STOCM &R1,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   STOC  &R1,&BD,4
         MEND
./ ADD NAME=STOCNM   0101-16084-16101-1857-00007-00007-00000-SYS210  21
         MACRO
&LABEL   STOCNM &R1,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   STOC  &R1,&BD,11
         MEND
./ ADD NAME=STOCNO   0101-16084-16101-1857-00007-00007-00000-SYS210  23
         MACRO
&LABEL   STOCNO &R1,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   STOC  &R1,&BD,14
         MEND
./ ADD NAME=STOCNP   0101-16084-16101-1857-00007-00007-00000-SYS210  25
         MACRO
&LABEL   STOCNP &R1,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   STOC  &R1,&BD,13
         MEND
./ ADD NAME=STOCNZ   0101-16084-16101-1857-00007-00007-00000-SYS210  32
         MACRO
&LABEL   STOCNZ &R1,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   STOC  &R1,&BD,7
         MEND
./ ADD NAME=STOCO    0101-16084-16101-1857-00007-00007-00000-SYS210  39
         MACRO
&LABEL   STOCO &R1,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   STOC  &R1,&BD,1
         MEND
./ ADD NAME=STOCP    0101-16084-16101-1857-00007-00007-00000-SYS210  41
         MACRO
&LABEL   STOCP &R1,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   STOC  &R1,&BD,2
         MEND
./ ADD NAME=STOCZ    0101-16084-16101-1857-00007-00007-00000-SYS210  44
         MACRO
&LABEL   STOCZ &R1,&BD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   STOC  &R1,&BD,8
         MEND
./ ADD NAME=STRL     0102-16058-16101-1857-00007-00007-00000-SYS210  47
         MACRO
&NAME    STRL  &R1,&REL
.*
.*       GENERATE OPCODE IN IFOX00
.*
&NAME    C4OP  F,&R1,(&REL-*+2)/2
         MEND
./ ADD NAME=STRV     0103-08014-16101-1857-00007-00007-00000-SYS210  52
         MACRO
&LABEL   STRV  &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,3E
         MEND
./ ADD NAME=STRVH    0103-08014-16101-1857-00007-00007-00000-SYS210  53
         MACRO
&LABEL   STRVH &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,3F
         MEND
./ ADD NAME=STY      0101-16084-16101-1857-00007-00007-00000-SYS210  59
         MACRO
&LABEL   STY   &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,50
         MEND
./ ADD NAME=SXBR     0101-16058-16101-1858-00008-00008-00000-SYS210  01
         MACRO
&LABEL   SXBR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B34B00',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=SY       0101-16084-16101-1858-00007-00007-00000-SYS210  03
         MACRO
&LABEL   SY    &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,5B
         MEND
./ ADD NAME=S37XOPS  0103-16101-16103-1737-00679-00679-00000-SYS210  05
.* THIS MEMBER DEFINES CONDITION CODE SETTING S37X INSTRUCTIONS THAT
.* CAN BE USED IN STRUCTURED PROGRAMMING MACRO LOGICAL EXPRESSIONS.
.* LIMITATIONS OF THE IFOX00 ASSEMBLER REQUIRE THAT THESE INSTRUCTIONS
.* BE INDIVIDUALLY DEFINED BECAUSE IT DOES NOT HANDLE OPERATIONS
.* THAT INVOKE MACROS BEING SPECIFIED BY MACRO CHARACTER VARIABLES.
.* S37XOPS IS COPIED INTO THE IFPRO MACRO.
         AIF   ('&INST'(1,1) GT 'A').$$NOTA
         AIF   ('&INST' NE 'ADB ').$ADBR
         ADB    &OP1,&OP2
         AGO   .S37XEND
.$ADBR   ANOP
         AIF   ('&INST' NE 'ADBR').$AEB
         ADBR   &OP1,&OP2
         AGO   .S37XEND
.$AEB    ANOP
         AIF   ('&INST' NE 'AEB ').$AEBR
         AEB    &OP1,&OP2
         AGO   .S37XEND
.$AEBR   ANOP
         AIF   ('&INST' NE 'AEBR').$AFI
         AEBR   &OP1,&OP2
         AGO   .S37XEND
.$AFI    ANOP
         AIF   ('&INST' NE 'AFI').$AGSI
         AFI    &OP1,&OP2
         AGO   .S37XEND
.$AGSI   ANOP
         AIF   ('&INST' NE 'AGSI').$AHI
         AGSI   &OP1,&OP2
         AGO   .S37XEND
.$AHI    ANOP
         AIF   ('&INST' NE 'AHI').$AHIK
         AHI    &OP1,&OP2
         AGO   .S37XEND
.$AHIK   ANOP
         AIF   ('&INST' NE 'AHIK').$AHY
         AHIK   &OP1,&OP2,&OP3
         AGO   .S37XEND
.$AHY    ANOP
         AIF   ('&INST' NE 'AHY').$ALC
         AHY    &OP1,&OP2
         AGO   .S37XEND
.$ALC    ANOP
         AIF   ('&INST' NE 'ALC').$ALCR
         ALC    &OP1,&OP2
         AGO   .S37XEND
.$ALCR   ANOP
         AIF   ('&INST' NE 'ALCR').$ALFI
         ALCR   &OP1,&OP2
         AGO   .S37XEND
.$ALFI   ANOP
         AIF   ('&INST' NE 'ALFI').$ALGSI
         ALFI   &OP1,&OP2
         AGO   .S37XEND
.$ALGSI  ANOP
         AIF   ('&INST' NE 'ALGSI').$ALHSIK
         ALGSI  &OP1,&OP2
         AGO   .S37XEND
.$ALHSIK ANOP
         AIF   ('&INST' NE 'ALHSIK').$ALRK
         ALHSIK &OP1,&OP2,&OP3
         AGO   .S37XEND
.$ALRK   ANOP
         AIF   ('&INST' NE 'ALRK').$ALSI
         ALRK   &OP1,&OP2,&OP3
         AGO   .S37XEND
.$ALSI   ANOP
         AIF   ('&INST' NE 'ALSI').$ALY
         ALSI   &OP1,&OP2
         AGO   .S37XEND
.$ALY    ANOP
         AIF   ('&INST' NE 'ALY').$ARK
         ALY    &OP1,&OP2
         AGO   .S37XEND
.$ARK    ANOP
         AIF   ('&INST' NE 'ARK').$ASI
         ARK    &OP1,&OP2,&OP3
         AGO   .S37XEND
.$ASI    ANOP
         AIF   ('&INST' NE 'ASI').$AXBR
         ASI    &OP1,&OP2
         AGO   .S37XEND
.$AXBR   ANOP
         AIF   ('&INST' NE 'AXBR').$AY
         AXBR   &OP1,&OP2
         AGO   .S37XEND
.$AY     ANOP
         AIF   ('&INST' NE 'AY').GENIT
         AY     &OP1,&OP2
         AGO   .S37XEND
.$$NOTA  ANOP
         AIF   ('&INST'(1,1) GT 'C').$$NOTC
         AIF   ('&INST' NE 'CDB ').$CDBR
         CDB    &OP1,&OP2
         AGO   .S37XEND
.$CDBR   ANOP
         AIF   ('&INST' NE 'CDBR').$CEB
         CDBR   &OP1,&OP2
         AGO   .S37XEND
.$CEB    ANOP
         AIF   ('&INST' NE 'CEB ').$CEBR
         CEB    &OP1,&OP2
         AGO   .S37XEND
.$CEBR   ANOP
         AIF   ('&INST' NE 'CEBR').$CFC
         CEBR   &OP1,&OP2
         AGO   .S37XEND
.$CFC    ANOP
         AIF   ('&INST' NE 'CFC').$CFDBR
         CFC    &OP1
         AGO   .S37XEND
.$CFDBR  ANOP
         AIF   ('&INST' NE 'CFDBR').$CFDR
         CFDBR  &OP1,&OP2,&OP3
         AGO   .S37XEND
.$CFDR   ANOP
         AIF   ('&INST' NE 'CFDR').$CFEBR
         CFDR   &OP1,&OP2,&OP3
         AGO   .S37XEND
.$CFEBR  ANOP
         AIF   ('&INST' NE 'CFEBR').$CFER
         CFEBR  &OP1,&OP2,&OP3
         AGO   .S37XEND
.$CFER   ANOP
         AIF   ('&INST' NE 'CFER').$CFI
         CFER   &OP1,&OP2,&OP3
         AGO   .S37XEND
.$CFI    ANOP
         AIF   ('&INST' NE 'CFI').$CFXBR
         CFI    &OP1,&OP2
         AGO   .S37XEND
.$CFXBR  ANOP
         AIF   ('&INST' NE 'CFXBR').$CFXR
         CFXBR  &OP1,&OP2,&OP3
         AGO   .S37XEND
.$CFXR   ANOP
         AIF   ('&INST' NE 'CFXR').$CGHSI
         CFXR   &OP1,&OP2,&OP3
         AGO   .S37XEND
.$CGHSI  ANOP
         AIF   ('&INST' NE 'CGHSI').$CHHSI
         CGHSI  &OP1,&OP2
         AGO   .S37XEND
.$CHHSI  ANOP
         AIF   ('&INST' NE 'CHHSI').$CHI
         CHHSI  &OP1,&OP2
         AGO   .S37XEND
.$CHI    ANOP
         AIF   ('&INST' NE 'CHI').$CHRL
         CHI    &OP1,&OP2
         AGO   .S37XEND
.$CHRL   ANOP
         AIF   ('&INST' NE 'CHRL').$CHSI
         CHRL   &OP1,&OP2
         AGO   .S37XEND
.$CHSI   ANOP
         AIF   ('&INST' NE 'CHSI').$CHY
         CHSI   &OP1,&OP2
         AGO   .S37XEND
.$CHY    ANOP
         AIF   ('&INST' NE 'CHY').$CKSM
         CHY    &OP1,&OP2
         AGO   .S37XEND
.$CKSM   ANOP
         AIF   ('&INST' NE 'CKSM').$CLCLE
         CKSM   &OP1,&OP2
         AGO   .S37XEND
.$CLCLE  ANOP
         AIF   ('&INST' NE 'CLCLE').$CLCLU
         CLCLE  &OP1,&OP2,&OP3
         AGO   .S37XEND
.$CLCLU  ANOP
         AIF   ('&INST' NE 'CLCLU').$CLFHSI
         CLCLU  &OP1,&OP2,&OP3
         AGO   .S37XEND
.$CLFHSI ANOP
         AIF   ('&INST' NE 'CLFHSI').$CLFI
         CLFHSI &OP1,&OP2
         AGO   .S37XEND
.$CLFI   ANOP
         AIF   ('&INST' NE 'CLFI').$CLFIT
         CLFI   &OP1,&OP2
         AGO   .S37XEND
.$CLFIT  ANOP
         AIF   ('&INST' NE 'CLFIT').$CLGHSI
         CLFIT  &OP1,&OP2,&OP3
         AGO   .S37XEND
.$CLGHSI ANOP
         AIF   ('&INST' NE 'CLGHSI').$CLHHSI
         CLGHSI &OP1,&OP2
         AGO   .S37XEND
.$CLHHSI ANOP
         AIF   ('&INST' NE 'CLHHSI').$CLHRL
         CLHHSI &OP1,&OP2
         AGO   .S37XEND
.$CLHRL  ANOP
         AIF   ('&INST' NE 'CLHRL').$CLIB
         CLHRL  &OP1,&OP2
         AGO   .S37XEND
.$CLIB   ANOP
         AIF   ('&INST' NE 'CLIB').$CLIJ
         CLIB   &OP1,&OP2
         AGO   .S37XEND
.$CLIJ   ANOP
         AIF   ('&INST' NE 'CLIJ').$CLRB
         CLIJ   &OP1,&OP2
         AGO   .S37XEND
.$CLRB   ANOP
         AIF   ('&INST' NE 'CLRB').$CLRJ
         CLRB   &OP1,&OP2
         AGO   .S37XEND
.$CLRJ   ANOP
         AIF   ('&INST' NE 'CLRJ').$CLRL
         CLRJ   &OP1,&OP2
         AGO   .S37XEND
.$CLRL   ANOP
         AIF   ('&INST' NE 'CLRL').$CLRT
         CLRL   &OP1,&OP2
         AGO   .S37XEND
.$CLRT   ANOP
         AIF   ('&INST' NE 'CLRT').$CLST
         CLRT   &OP1,&OP2,&OP3
         AGO   .S37XEND
.$CLST   ANOP
         AIF   ('&INST' NE 'CLST').$CLY
         CLST   &OP1,&OP2
         AGO   .S37XEND
.$CLY    ANOP
         AIF   ('&INST' NE 'CLY').$CMPSC
         CLY    &OP1,&OP2
         AGO   .S37XEND
.$CMPSC  ANOP
         AIF   ('&INST' NE 'CMPSC').$CRL
         CMPSC  &OP1,&OP2
         AGO   .S37XEND
.$CRL    ANOP
         AIF   ('&INST' NE 'CRL').$CSST
         CRL    &OP1,&OP2
         AGO   .S37XEND
.$CSST   ANOP
         AIF   ('&INST' NE 'CSST').$CU14
         CSST   &OP1,&OP2,&OP3
         AGO   .S37XEND
.$CU14   ANOP
         AIF   ('&INST' NE 'CU14').$CU24
         CU14   &OP1,&OP2
         AGO   .S37XEND
.$CU24   ANOP
         AIF   ('&INST' NE 'CU24').$CU41
         CU24   &OP1,&OP2
         AGO   .S37XEND
.$CU41   ANOP
         AIF   ('&INST' NE 'CU41').$CU42
         CU41   &OP1,&OP2
         AGO   .S37XEND
.$CU42   ANOP
         AIF   ('&INST' NE 'CU42').$CUSE
         CU42   &OP1,&OP2
         AGO   .S37XEND
.$CUSE   ANOP
         AIF   ('&INST' NE 'CUSE').$CUTFU
         CUSE   &OP1,&OP2
         AGO   .S37XEND
.$CUTFU  ANOP
         AIF   ('&INST' NE 'CUTFU').$CUUTF
         CUTFU  &OP1,&OP2
         AGO   .S37XEND
.$CUUTF  ANOP
         AIF   ('&INST' NE 'CUUTF').$CXBR
         CUUTF  &OP1,&OP2
         AGO   .S37XEND
.$CXBR   ANOP
         AIF   ('&INST' NE 'CXBR').$CXR
         CXBR   &OP1,&OP2
         AGO   .S37XEND
.$CXR    ANOP
         AIF   ('&INST' NE 'CXR').$CY
         CXR    &OP1,&OP2
         AGO   .S37XEND
.$CY     ANOP
         AIF   ('&INST' NE 'CY').GENIT
         CY     &OP1,&OP2
         AGO   .S37XEND
.$$NOTC  ANOP
         AIF   ('&INST'(1,1) GT 'K').$$NOTK
         AIF   ('&INST' NE 'DIDBR').$DIEBR
         DIDBR  &OP1,&OP2,&OP3,&OP4
         AGO   .S37XEND
.$DIEBR  ANOP
         AIF   ('&INST' NE 'DIEBR').$EXRL
         DIEBR  &OP1,&OP2,&OP3,&OP4
         AGO   .S37XEND
.$EXRL   ANOP
         AIF   ('&INST' NE 'EXRL').$KDB
         EXRL   &OP1,&OP2
         AGO   .S37XEND
.$KDB    ANOP
         AIF   ('&INST' NE 'KDB ').$KDBR
         KDB    &OP1,&OP2
         AGO   .S37XEND
.$KDBR   ANOP
         AIF   ('&INST' NE 'KDBR').$KEB
         KDBR   &OP1,&OP2
         AGO   .S37XEND
.$KEB    ANOP
         AIF   ('&INST' NE 'KEB ').$KEBR
         KEB    &OP1,&OP2
         AGO   .S37XEND
.$KEBR   ANOP
         AIF   ('&INST' NE 'KEBR').$KIMD
         KEBR   &OP1,&OP2
         AGO   .S37XEND
.$KIMD   ANOP
         AIF   ('&INST' NE 'KIMD').$KLMD
         KIMD   &OP1,&OP2
         AGO   .S37XEND
.$KLMD   ANOP
         AIF   ('&INST' NE 'KLMD').$KM
         KLMD   &OP1,&OP2
         AGO   .S37XEND
.$KM     ANOP
         AIF   ('&INST' NE 'KM').$KMAC
         KM     &OP1,&OP2
         AGO   .S37XEND
.$KMAC   ANOP
         AIF   ('&INST' NE 'KMAC').$KMC
         KMAC   &OP1,&OP2
         AGO   .S37XEND
.$KMC    ANOP
         AIF   ('&INST' NE 'KMC').$KMCTR
         KMC    &OP1,&OP2
         AGO   .S37XEND
.$KMCTR  ANOP
         AIF   ('&INST' NE 'KMCTR').$KMF
         KMCTR  &OP1,&OP2,&OP3
         AGO   .S37XEND
.$KMF    ANOP
         AIF   ('&INST' NE 'KMF').$KMO
         KMF    &OP1,&OP2
         AGO   .S37XEND
.$KMO    ANOP
         AIF   ('&INST' NE 'KMO').$KXBR
         KMO    &OP1,&OP2
         AGO   .S37XEND
.$KXBR   ANOP
         AIF   ('&INST' NE 'KXBR').GENIT
         KXBR   &OP1,&OP2
         AGO   .S37XEND
.$$NOTK  ANOP
         AIF   ('&INST'(1,1) GT 'L').$$NOTL
         AIF   ('&INST' NE 'LAA').$LAAL
         LAA    &OP1,&OP2,&OP3
         AGO   .S37XEND
.$LAAL   ANOP
         AIF   ('&INST' NE 'LAAL').$LAN
         LAAL   &OP1,&OP2,&OP3
         AGO   .S37XEND
.$LAN    ANOP
         AIF   ('&INST' NE 'LAN').$LAO
         LAN    &OP1,&OP2,&OP3
         AGO   .S37XEND
.$LAO    ANOP
         AIF   ('&INST' NE 'LAO').$LAX
         LAO    &OP1,&OP2,&OP3
         AGO   .S37XEND
.$LAX    ANOP
         AIF   ('&INST' NE 'LAX').$LCDBR
         LAX    &OP1,&OP2,&OP3
         AGO   .S37XEND
.$LCDBR  ANOP
         AIF   ('&INST' NE 'LCDBR').$LCDFR
         LCDBR  &OP1,&OP2
         AGO   .S37XEND
.$LCDFR  ANOP
         AIF   ('&INST' NE 'LCDFR').$LCEBR
         LCDFR  &OP1,&OP2
         AGO   .S37XEND
.$LCEBR  ANOP
         AIF   ('&INST' NE 'LCEBR').$LCXBR
         LCEBR  &OP1,&OP2
         AGO   .S37XEND
.$LCXBR  ANOP
         AIF   ('&INST' NE 'LCXBR').$LCXR
         LCXBR  &OP1,&OP2
         AGO   .S37XEND
.$LCXR   ANOP
         AIF   ('&INST' NE 'LCXR').$LNDBR
         LCXR   &OP1,&OP2
         AGO   .S37XEND
.$LNDBR  ANOP
         AIF   ('&INST' NE 'LNDBR').$LNDFR
         LNDBR  &OP1,&OP2
         AGO   .S37XEND
.$LNDFR  ANOP
         AIF   ('&INST' NE 'LNDFR').$LNEBR
         LNDFR  &OP1,&OP2
         AGO   .S37XEND
.$LNEBR  ANOP
         AIF   ('&INST' NE 'LNEBR').$LNXBR
         LNEBR  &OP1,&OP2
         AGO   .S37XEND
.$LNXBR  ANOP
         AIF   ('&INST' NE 'LNXBR').$LNXR
         LNXBR  &OP1,&OP2
         AGO   .S37XEND
.$LNXR   ANOP
         AIF   ('&INST' NE 'LNXR').$LPD
         LNXR   &OP1,&OP2
         AGO   .S37XEND
.$LPD    ANOP
         AIF   ('&INST' NE 'LPD').$LPDBR
         LPD    &OP1,&OP2,&OP3
         AGO   .S37XEND
.$LPDBR  ANOP
         AIF   ('&INST' NE 'LPDBR').$LPDFR
         LPDBR  &OP1,&OP2
         AGO   .S37XEND
.$LPDFR  ANOP
         AIF   ('&INST' NE 'LPDFR').$LPEBR
         LPDFR  &OP1,&OP2
         AGO   .S37XEND
.$LPEBR  ANOP
         AIF   ('&INST' NE 'LPEBR').$LPXBR
         LPEBR  &OP1,&OP2
         AGO   .S37XEND
.$LPXBR  ANOP
         AIF   ('&INST' NE 'LPXBR').$LPXR
         LPXBR  &OP1,&OP2
         AGO   .S37XEND
.$LPXR   ANOP
         AIF   ('&INST' NE 'LPXR').$LT
         LPXR   &OP1,&OP2
         AGO   .S37XEND
.$LT     ANOP
         AIF   ('&INST' NE 'LT').$LTDBR
         LT     &OP1,&OP2
         AGO   .S37XEND
.$LTDBR  ANOP
         AIF   ('&INST' NE 'LTDBR').$LTEBR
         LTDBR  &OP1,&OP2
         AGO   .S37XEND
.$LTEBR  ANOP
         AIF   ('&INST' NE 'LTEBR').$LTXBR
         LTEBR  &OP1,&OP2
         AGO   .S37XEND
.$LTXBR  ANOP
         AIF   ('&INST' NE 'LTXBR').$LTXR
         LTXBR  &OP1,&OP2
         AGO   .S37XEND
.$LTXR   ANOP
         AIF   ('&INST' NE 'LTXR').GENIT
         LTXR   &OP1,&OP2
         AGO   .S37XEND
.$$NOTL  ANOP
         AIF   ('&INST'(1,1) GT 'P').$$NOTP
         AIF   ('&INST' NE 'MVCLE').$MVCLU
         MVCLE  &OP1,&OP2,&OP3
         AGO   .S37XEND
.$MVCLU  ANOP
         AIF   ('&INST' NE 'MVCLU').$MVST
         MVCLU  &OP1,&OP2,&OP3
         AGO   .S37XEND
.$MVST   ANOP
         AIF   ('&INST' NE 'MVST').$NILF
         MVST   &OP1,&OP2
         AGO   .S37XEND
.$NILF   ANOP
         AIF   ('&INST' NE 'NILF').$NILH
         NILF   &OP1,&OP2
         AGO   .S37XEND
.$NILH   ANOP
         AIF   ('&INST' NE 'NILH').$NILL
         NILH   &OP1,&OP2
         AGO   .S37XEND
.$NILL   ANOP
         AIF   ('&INST' NE 'NILL').$NRK
         NILL   &OP1,&OP2
         AGO   .S37XEND
.$NRK    ANOP
         AIF   ('&INST' NE 'NRK').$NY
         NRK    &OP1,&OP2,&OP3
         AGO   .S37XEND
.$NY     ANOP
         AIF   ('&INST' NE 'NY').$OILF
         NY     &OP1,&OP2
         AGO   .S37XEND
.$OILF   ANOP
         AIF   ('&INST' NE 'OILF').$OILH
         OILF   &OP1,&OP2
         AGO   .S37XEND
.$OILH   ANOP
         AIF   ('&INST' NE 'OILH').$OILL
         OILH   &OP1,&OP2
         AGO   .S37XEND
.$OILL   ANOP
         AIF   ('&INST' NE 'OILL').$ORK
         OILL   &OP1,&OP2
         AGO   .S37XEND
.$ORK    ANOP
         AIF   ('&INST' NE 'ORK').$OY
         ORK    &OP1,&OP2,&OP3
         AGO   .S37XEND
.$OY     ANOP
         AIF   ('&INST' NE 'OY').$PCC
         OY     &OP1,&OP2
         AGO   .S37XEND
.$PCC    ANOP
         AIF   ('&INST' NE 'PCC').GENIT
         PCC    &OP1,&OP2
         AGO   .S37XEND
.$$NOTP  ANOP
         AIF   ('&INST'(1,1) GT 'S').$$NOTS
         AIF   ('&INST' NE 'SDB ').$SDBR
         SDB    &OP1,&OP2
         AGO   .S37XEND
.$SDBR   ANOP
         AIF   ('&INST' NE 'SDBR').$SEB
         SDBR   &OP1,&OP2
         AGO   .S37XEND
.$SEB    ANOP
         AIF   ('&INST' NE 'SEB ').$SEBR
         SEB    &OP1,&OP2
         AGO   .S37XEND
.$SEBR   ANOP
         AIF   ('&INST' NE 'SEBR').$SHY
         SEBR   &OP1,&OP2
         AGO   .S37XEND
.$SHY    ANOP
         AIF   ('&INST' NE 'SHY').$SLAK
         SHY    &OP1,&OP2
         AGO   .S37XEND
.$SLAK   ANOP
         AIF   ('&INST' NE 'SLAK').$SLB
         SLAK   &OP1,&OP2,&OP3
         AGO   .S37XEND
.$SLB    ANOP
         AIF   ('&INST' NE 'SLB ').$SLBR
         SLB    &OP1,&OP2
         AGO   .S37XEND
.$SLBR   ANOP
         AIF   ('&INST' NE 'SLBR').$SLFI
         SLBR   &OP1,&OP2
         AGO   .S37XEND
.$SLFI   ANOP
         AIF   ('&INST' NE 'SLFI').$SLRK
         SLFI   &OP1,&OP2
         AGO   .S37XEND
.$SLRK   ANOP
         AIF   ('&INST' NE 'SLRK').$SLY
         SLRK   &OP1,&OP2
         AGO   .S37XEND
.$SLY    ANOP
         AIF   ('&INST' NE 'SLY').$SRAK
         SLY    &OP1,&OP2
         AGO   .S37XEND
.$SRAK   ANOP
         AIF   ('&INST' NE 'SRAK').$SRK
         SRAK   &OP1,&OP2,&OP3
         AGO   .S37XEND
.$SRK    ANOP
         AIF   ('&INST' NE 'SRK').$SRST
         SRK    &OP1,&OP2,&OP3
         AGO   .S37XEND
.$SRST   ANOP
         AIF   ('&INST' NE 'SRST').$SRSTU
         SRST   &OP1,&OP2
         AGO   .S37XEND
.$SRSTU  ANOP
         AIF   ('&INST' NE 'SRSTU').$SXBR
         SRSTU  &OP1,&OP2
         AGO   .S37XEND
.$SXBR   ANOP
         AIF   ('&INST' NE 'SXBR').$SY
         SXBR   &OP1,&OP2
         AGO   .S37XEND
.$SY     ANOP
         AIF   ('&INST' NE 'SY').GENIT
         SY     &OP1,&OP2
         AGO   .S37XEND
.$$NOTS  ANOP
         AIF   ('&INST' NE 'TAM').$TBDR
         TAM
         AGO   .S37XEND
.$TBDR   ANOP
         AIF   ('&INST' NE 'TBDR').$TBEDR
         TBDR   &OP1,&OP2,&OP3
         AGO   .S37XEND
.$TBEDR  ANOP
         AIF   ('&INST' NE 'TBEDR').$TCDB
         TBEDR  &OP1,&OP2,&OP3
         AGO   .S37XEND
.$TCDB   ANOP
         AIF   ('&INST' NE 'TCDB').$TCEB
         TCDB   &OP1,&OP2
         AGO   .S37XEND
.$TCEB   ANOP
         AIF   ('&INST' NE 'TCEB').$TCXB
         TCEB   &OP1,&OP2
         AGO   .S37XEND
.$TCXB   ANOP
         AIF   ('&INST' NE 'TCXB').$THDER
         TCXB   &OP1,&OP2
         AGO   .S37XEND
.$THDER  ANOP
         AIF   ('&INST' NE 'THDER').$THDR
         THDER  &OP1,&OP2
         AGO   .S37XEND
.$THDR   ANOP
         AIF   ('&INST' NE 'THDR').$TMLH
         THDR   &OP1,&OP2
         AGO   .S37XEND
.$TMLH   ANOP
         AIF   ('&INST' NE 'TMLH').$TMLL
         TMLH   &OP1,&OP2
         AGO   .S37XEND
.$TMLL   ANOP
         AIF   ('&INST' NE 'TMLL').$TP
         TMLL   &OP1,&OP2
         AGO   .S37XEND
.$TP     ANOP
         AIF   ('&INST' NE 'TP').$TRE
         TP     &OP1
         AGO   .S37XEND
.$TRE    ANOP
         AIF   ('&INST' NE 'TRE').$TROO
         TRE    &OP1,&OP2
         AGO   .S37XEND
.$TROO   ANOP
         AIF   ('&INST' NE 'TROO').$TROT
         TROO   &OP1,&OP2
         AGO   .S37XEND
.$TROT   ANOP
         AIF   ('&INST' NE 'TROT').$TRTE
         TROT   &OP1,&OP2
         AGO   .S37XEND
.$TRTE   ANOP
         AIF   ('&INST' NE 'TRTE').$TRTO
         TRTE   &OP1,&OP2
         AGO   .S37XEND
.$TRTO   ANOP
         AIF   ('&INST' NE 'TRTO').$TRTR
         TRTO   &OP1,&OP2
         AGO   .S37XEND
.$TRTR   ANOP
         AIF   ('&INST' NE 'TRTR').$TRTRE
         TRTR   &OP1,&OP2
         AGO   .S37XEND
.$TRTRE  ANOP
         AIF   ('&INST' NE 'TRTRE').$TRTT
         TRTRE  &OP1,&OP2
         AGO   .S37XEND
.$TRTT   ANOP
         AIF   ('&INST' NE 'TRTT').$UNPKA
         TRTT   &OP1,&OP2
         AGO   .S37XEND
.$UNPKA  ANOP
         AIF   ('&INST' NE 'UNPKA').$UNPKU
         UNPKA  &OP1,&OP2
         AGO   .S37XEND
.$UNPKU  ANOP
         AIF   ('&INST' NE 'UNPKU').$UPT
         UNPKU  &OP1,&OP2
         AGO   .S37XEND
.$UPT    ANOP
         AIF   ('&INST' NE 'UPT').$XILF
         UPT    &OP1,&OP2
         AGO   .S37XEND
.$XILF   ANOP
         AIF   ('&INST' NE 'XILF').$XRK
         XILF   &OP1,&OP2
         AGO   .S37XEND
.$XRK    ANOP
         AIF   ('&INST' NE 'XRK').$XY
         XRK    &OP1,&OP2,&OP3
         AGO   .S37XEND
.$XY     ANOP
         AIF   ('&INST' NE 'XY').GENIT
         XY     &OP1,&OP2
.S37XEND ANOP
./ ADD NAME=TAM      0102-08013-16101-1858-00008-00008-00000-SYS210  12
         MACRO
&LABEL   TAM
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL2'00',X'010B'
         MEND
./ ADD NAME=TBDR     0101-16084-16101-1858-00008-00008-00000-SYS210  14
         MACRO
&LABEL   TBDR  &R1,&M3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B351',AL.4(&M3,0,&R1.,&R2.)
         MEND
./ ADD NAME=TBEDR    0101-16084-16101-1858-00008-00008-00000-SYS210  26
         MACRO
&LABEL   TBEDR &R1,&M3,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B350',AL.4(&M3,0,&R1.,&R2.)
         MEND
./ ADD NAME=TCDB     0101-16059-16101-1858-00007-00007-00000-SYS210  29
         MACRO
&LABEL   TCDB  &R,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R,&BXD,11
         MEND
./ ADD NAME=TCEB     0101-16059-16101-1858-00007-00007-00000-SYS210  32
         MACRO
&LABEL   TCEB  &R,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R,&BXD,10
         MEND
./ ADD NAME=TCXB     0101-16059-16101-1858-00007-00007-00000-SYS210  34
         MACRO
&LABEL   TCXB  &R,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   EDOP  &R,&BXD,12
         MEND
./ ADD NAME=THDER    0101-16084-16101-1858-00008-00008-00000-SYS210  37
         MACRO
&LABEL   THDER &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B35800',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=THDR     0101-16084-16101-1858-00008-00008-00000-SYS210  39
         MACRO
&LABEL   THDR  &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B35900',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=TMH      0102-08013-16101-1858-00007-00007-00000-SYS210  41
         MACRO
&LABEL   TMH   &R,&I16
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A7OP  0,&R,&I16
         MEND
./ ADD NAME=TML      0102-08013-16101-1858-00007-00007-00000-SYS210  44
         MACRO
&LABEL   TML   &R,&I16
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A7OP  1,&R,&I16
         MEND
./ ADD NAME=TMLH     0102-08013-16101-1858-00007-00007-00000-SYS210  46
         MACRO
&LABEL   TMLH  &R,&I16
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A7OP  0,&R,&I16
         MEND
./ ADD NAME=TMLL     0102-08013-16101-1858-00007-00007-00000-SYS210  47
         MACRO
&LABEL   TMLL  &R,&I16
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   A7OP  1,&R,&I16
         MEND
./ ADD NAME=TP       0103-08014-16102-1714-00020-00020-00000-SYS210  59
         MACRO
&LABEL   TP    &BLD
.*
.*       Generate Opcode in IFOX00
.*
.*       Use the Compare Packed Opcode to generate the Base
.*       Register, Length and Displacement for Operand One.
.*
&LABEL   CP    &BLD,0(0,0)             Changed to TP
.*
.*       Org back to reset the Opcode to EB - Test Packed
.*
         ORG   *-6
         DC    X'EB'                   Set to TP &BLD
.*
.*       Overlay Operand 2 with remaining part of Test Packed Opcode
.*
         ORG   *+3
         DC    X'00C0'
         MEND
./ ADD NAME=TRE      0102-08014-16101-1858-00008-00008-00000-SYS210  52
         MACRO
&LABEL   TRE   &R1,&R2
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B2A500',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=TROO     0102-08014-16101-1858-00012-00012-00000-SYS210  54
         MACRO
&LABEL   TROO  &R1,&R2,&M
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
         AIF   (K'&M EQ 0).NOMASK
&LABEL   DC    0XL4'00',X'B993',AL.4(&M.,0),AL.4(&R1.,&R2)
         MEXIT
.NOMASK  ANOP
&LABEL   DC    0XL4'00',X'B99300',AL.4(&R1.,&R2)
         MEND
./ ADD NAME=TROT     0102-08014-16101-1858-00012-00012-00000-SYS210  57
         MACRO
&LABEL   TROT  &R1,&R2,&M
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
         AIF   (K'&M EQ 0).NOMASK
&LABEL   DC    0XL4'00',X'B992',AL.4(&M.,0),AL.4(&R1.,&R2)
         MEXIT
.NOMASK  ANOP
&LABEL   DC    0XL4'00',X'B99200',AL.4(&R1.,&R2)
         MEND
./ ADD NAME=TRTE     0101-16059-16101-1858-00012-00012-00000-SYS210  59
         MACRO
&LABEL   TRTE  &R1,&R2,&M
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
         AIF   (K'&M EQ 0).NOMASK
&LABEL   DC    0XL4'00',X'B9BF',AL.4(&M.,0),AL.4(&R1.,&R2)
         MEXIT
.NOMASK  ANOP
&LABEL   DC    0XL4'00',X'B9BF00',AL.4(&R1.,&R2)
         MEND
./ ADD NAME=TRTO     0102-08014-16101-1859-00012-00012-00000-SYS210  08
         MACRO
&LABEL   TRTO  &R1,&R2,&M
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
         AIF   (K'&M EQ 0).NOMASK
&LABEL   DC    0XL4'00',X'B991',AL.4(&M.,0),AL.4(&R1.,&R2)
         MEXIT
.NOMASK  ANOP
&LABEL   DC    0XL4'00',X'B99100',AL.4(&R1.,&R2)
         MEND
./ ADD NAME=TRTR     0102-08014-16101-1859-00017-00017-00000-SYS210  10
         MACRO
&LABEL   TRTR  &BLD1,&BD2
.*
.*       Generate Opcode in IFOX00
.*
.*       Use the Translate and Test Opcode to generate the Base
.*       Register, Length and Displacement for Operands One and
.*       Two
.*
&LABEL   TRT   &BLD1,&BD2               Changed to TRTR
.*
.*       Org back to reset the Opcode to D0 - Translate and Test Rev
.*
         ORG   *-6
         DC    X'D0'                    Set to TRTR
         ORG
         MEND
./ ADD NAME=TRTRE    0101-16059-16101-1859-00012-00012-00000-SYS210  12
         MACRO
&LABEL   TRTRE &R1,&R2,&M
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
         AIF   (K'&M EQ 0).NOMASK
&LABEL   DC    0XL4'00',X'B9BD',AL.4(&M.,0),AL.4(&R1.,&R2)
         MEXIT
.NOMASK  ANOP
&LABEL   DC    0XL4'00',X'B9BD00',AL.4(&R1.,&R2)
         MEND
./ ADD NAME=TRTT     0103-08014-16101-1859-00012-00012-00000-SYS210  14
         MACRO
&LABEL   TRTT  &R1,&R2,&M
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
         AIF   (K'&M EQ 0).NOMASK
&LABEL   DC    0XL4'00',X'B990',AL.4(&M.,0),AL.4(&R1.,&R2)
         MEXIT
.NOMASK  ANOP
&LABEL   DC    0XL4'00',X'B99000',AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=UNPKA    0102-16059-16101-1859-00022-00022-00000-SYS210  16
         MACRO
&LABEL   UNPKA &BD1,&BD2
.*
.*       Generate UNPKA instruction in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00'                Set Instruction Length of 6
.*
.*       Use MVC opcode to generate Base and Displacement fields
.*       as well as the L1 length field
.*
         MVC   &BD1,&BD2               OpCode overwritten to EA
.*
.*       Org back to reset the Opcode to EA
.*
         ORG   *-6
         DC    X'EA'                   Opcode set to EA
.*
.*       Get back to where we were
.*
         ORG
         MEND
./ ADD NAME=UNPKU    0101-16059-16101-1859-00022-00022-00000-SYS210  19
         MACRO
&LABEL   UNPKU &BD1,&BD2
.*
.*       Generate UNPKU instruction in IFOX00
.*
         DS    0H
&LABEL   DC    0XL6'00'                Set Instruction Length of 6
.*
.*       Use MVC opcode to generate Base and Displacement fields
.*       as well as the L1 length field
.*
         MVC   &BD1,&BD2               OpCode overwritten to E2
.*
.*       Org back to reset the Opcode to E2
.*
         ORG   *-6
         DC    X'E2'                   Opcode set to E2
.*
.*       Get back to where we were
.*
         ORG
         MEND
./ ADD NAME=XILF     0102-08014-16101-1859-00007-00007-00000-SYS210  21
         MACRO
&LABEL   XILF  &R,&I32
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   C0OP  7,&R,&I32
         MEND
./ ADD NAME=XRK      0101-16084-16101-1859-00008-00008-00000-SYS210  23
         MACRO
&LABEL   XRK   &R1,&R2,&R3
.*
.*       Generate Opcode in IFOX00
.*
         DS    0H
&LABEL   DC    0XL4'00',X'B9F7',AL.4(&R3.,0),AL.4(&R1.,&R2.)
         MEND
./ ADD NAME=XY       0101-16084-16101-1859-00007-00007-00000-SYS210  27
         MACRO
&LABEL   XY    &R1,&BXD
.*
.*       Generate Opcode in IFOX00
.*
&LABEL   E3OP  &R1,&BXD,57
         MEND
@@
//