.*------------------------------------------------------------------- *
.*                                                                    *
.*                   EZASMIP Macro                                    *
.*                                                                    *
.*  Function:  The EZASMIP macro analyzes EZASMI keyword parameters   *
.*             to determine their type and load the address of the    *
.*             parameter into &REG. The following types are supported:*
.*             Char constants - Character string enclosed in single   *
.*                              quotes (calls EZASMIC)                *
.*             Address label: - Assembler label referencing value     *
.*             Indirect addr: - Assembler label preceded by an        *
.*                              asterisk(*) pointing to a full word   *
.*                              which contains the address of the     *
.*                              value                                 *
.*             GPR            - General purpose register number       *
.*                              enclosed in parentheses of the        *
.*                              register containing the address of    *
.*                              the value.                            *
.*             Literal value  - Literal numeric value                 *
.*                                                                    *
.*  Parameters:                                                       *
.*             &PARM - The keyword value to be analyzed               *
.*             &REG  - The register to be loaded (default R0)         *
.*             &TYPE - Type of numeric literal to generate. Numeric   *
.*                     literal not valid if this parm not specified   *
.*             &MAX  - Maximum value for numeric literal; if zero,    *
.*                     no limit check is performed                    *
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
         EZASMIP &PARM,&REG,&TYPE,&MAX
         LCLC  &R1,&R2,&IADDR,&VAL
         LCLA  &NUM,&MNUM
.*
         GBLC  &GPR            GPR containing addr of EZASOKET parm
.*
&R1      SETC  '0'             Default register to load
&GPR     SETC  '0'             Set to register to store in EZASMI
         AIF   (K'&REG EQ 0).P1
&R1      SETC  '&REG'
.P1      ANOP
         AIF   ('&PARM'(1,1) NE '(').P2
.*
.* Register points to value
.*
.* There is no need to load a register. The parm address is already in
.* a register, we just need to tell the mainline what register needs
.* to be stored into the EZASOKET parm list.
.*
&GPR     SETC  '&PARM'(2,K'&PARM-2)
         MEXIT
.P2      ANOP
         AIF   ('&PARM'(1,1) NE '*').P3
.*
.* Indirect address points to value
.*
&IADDR   SETC  '&PARM'(2,K'&PARM-1)
         L     &R1,&IADDR
         MEXIT
.P3      ANOP
.*
.* Remove quotes if specified
.*
&VAL     SETC  '&PARM'
         AIF   ('&PARM'(1,1) NE '''').P4
&VAL     SETC  '&PARM'(2,K'&PARM-2)
.P4      ANOP
.*
.* Direct address points to value
.*
         AIF   ('&VAL'(1,1) GE '0').P5
         LA    &R1,&VAL
         MEXIT
.*
.* Literal value coded
.*
.P5      ANOP
         AIF   (K'&TYPE NE 0).P6
.*
.* Numeric literal not allowed
.*
         MNOTE 16,'Invalid keyword value, &PARM'
         MEXIT
.P6      ANOP
&MNUM    SETA  &MAX
&NUM     SETA  &VAL
         AIF   (&MNUM EQ 0).P7
         AIF   (&NUM LE &MNUM).P7
.*
.* Numeric literal exceeds max allowed
.*
         MNOTE 16,'Keyword value &VAL greater than &MAX'
         MEXIT
.*
.* Generate load of ptr to literal value
.*
.P7      ANOP
         AIF   ('&TYPE' EQ 'F').P8
         LA    &R1,=&TYPE.(&VAL.)
         MEXIT
.P8      ANOP
         LA    &R1,=F'&VAL'
         MEND
