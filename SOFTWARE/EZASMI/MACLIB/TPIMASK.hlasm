.**********************************************************************
.*                                                                    *
.* This macro provides the capability to set and test bits in the     *
.* read, write, and exception bit masks used with the EZASMI          *
.* TYPE=SELECT macro call.                                            *
.*                                                                    *
.* Invocation:                                                        *
.*       TPIMASK {SET | TEST},MASK=maskaddr,SD=socket                 *
.*                                                                    *
.*             SET      - Sets the socket descriptor bit in the mask. *
.*             TEST     - Tests the socket descriptor bit in the mask.*
.*                        The TEST function sets the condition code   *
.*                        such that you may follow the TPIMASK macro  *
.*                        call with a BC instruction (BE branches if  *
.*                        bit was on, BNE branches if bit was off).   *
.*                                                                    *
.*             maskaddr - Address of the read, write, or execution    *
.*                        mask to be acted on. May be specified as a  *
.*                        storage location label or a register        *
.*                        number enclosed in parentheses.             *
.*                                                                    *
.*             socket   - Address of the socket descriptor which      *
.*                        determines the bit to be tested or set. May *
.*                        be specified as a storage location label or *
.*                        the register number enclosed in parentheses *
.*                        that contains the socket descriptor.        *
.*                                                                    *
.* Change history:                                                    *
.*                                                                    *
.* 03/04/2017 - Initial release                 Shelby Beach          *
.*                                                                    *
.**********************************************************************
.*
         MACRO
         TPIMASK &FUNC,        Function to perform (TEST or SET)       +
               &MASK=,         Mask address                            +
               &SD=            Socket descriptor
         AIF ('&FUNC' EQ 'TEST' OR '&FUNC' EQ 'SET').FUNCOK
         MNOTE 16,'Invalid TPIMASK function'
         MEXIT

.FUNCOK  ANOP
         XR    14,14           Prepare for divide.
         AIF   ('&SD'(1,1) EQ '(').SDREG
         LH    15,&SD          R15 = Socket Descriptor
         AGO   .SDOK
.SDREG   ANOP
         LR    15,&SD          R15 = Socket Descriptor
.SDOK    ANOP
         D     14,=F'32'       R15 = Word containing bit
         SLL   15,2            R15 = Offset of word
         AIF   ('&MASK'(1,1) EQ '(').MASKREG
         LA    1,&MASK         R1 = A(Mask)
         AGO   .MASKOK
.MASKREG ANOP
         LR    1,&MASK         R1 = A(Mask)
.MASKOK  ANOP
         AR    15,1            R15 = A(Word bit is in)
         LA    1,1             Need a bit in R1.
         SLL   1,0(14)         Position bit for test/set.
         O     1,0(,15)        Combine bit with others in mask.
         AIF   ('&FUNC' EQ 'SET').SETBIT
         C     1,0(,15)        Will be equal if bit was set.
         MEXIT
.SETBIT  ANOP
         ST    1,0(,15)        Store updated mask word.
         MEND
