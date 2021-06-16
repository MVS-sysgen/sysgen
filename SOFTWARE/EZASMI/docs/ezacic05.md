EZACIC05
========

The **EZACIC05** routine translates a character string from ASCII to
EBCDIC.

Invocation
----------

        CALL EZACIC05,(buffer,length),VL
        

**Parameter**
:   **Description**

**buffer**
:   The address of the data to be translated. Note that the translation
    occurs in place; if you do not want the input string destroyed, you
    must first make a copy of the string, and then translate the copy.

**length**
:   The address of a fullword binary number specifying the number of
    bytes to translate to EBCDIC.

Return Code
-----------

On return, R15 contains the return code as follows:

**00** - Translation completed successfully

**08** - Invalid number of parameters specified

**12** - Buffer length specified as zero

**16** - Buffer address specified as zero

Including Translation Code in Your Program
------------------------------------------

When any modules calling EZACIC05 are subsequently linked, EZACIC05 must
be included in the resultant load module. This is most easily
accomplished by simply including SYS2.LINKLIB in the SYSLIB DD
concatenation for your link step. Note: Ensure that you have not
specified the NCAL linkage editor option.

If you have explicitly included EZASOH03 in your load module, nothing
additional need be included; EZACIC05 is an entry point in EZASOH03.
