TPIMASK
=======

The **TPIMASK** macro provides the capability to set and test bits in
the read, write, and exception bit masks used with the EZASMI
TYPE=SELECT macro call.

Syntax:
-------

        >>-TPIMASK--{SET | TEST}--,MASK--=--+-address--+--,SD--=--+-address--+----------><
                            '-(reg)----'          '-(reg)----'
        

**Keyword**
:   **Description**

**MASK**
:   Address of the read, write, or execution mask to be acted on. May be
    specified as a storage location label or a register number enclosed
    in parentheses.

**SD**
:   Address of the socket descriptor which determines the bit to be
    tested or set. May be specified as a storage location label or the
    register number enclosed in parentheses that contains the socket
    descriptor.
