SOCKET
======

Parameter Considerations
------------------------

**PROTO**
:   The values that may be specified for this parameter with TCP/IP for
    MVS 3.8 Assembler depend upon the underlying TCP/IP package running
    on the Hercules host platform. In general you will not need to
    specify this parameter; however, should you need to specify a value
    other than 0, you should consult the related TCP/IP documentation
    for your system for more information.

Unsupported Parameters
----------------------

The following keyword parameters are not supported by TCP/IP for MVS 3.8
Assembler:

-   ECB
-   ERROR
-   NS
-   REQAREA
