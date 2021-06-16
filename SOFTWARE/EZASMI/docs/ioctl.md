IOCTL
=====

Parameter Considerations
------------------------

**COMMAND**
:   Most of the values that may be specified for this parameter are not
    supported by TCP/IP for MVS 3.8 Assembler. The actual **COMMAND**
    values that are supported are determined by the underlying TCP/IP
    package running on the Hercules host platform. For example, only the
    following values are supported by Winsock:
    -   FIONBIO
    -   FIONREAD
    -   SIOCATMARK

    You should consult the related TCP/IP documentation for your system
    for more information.

Unsupported Parameters
----------------------

The following keyword parameters are not supported by TCP/IP for MVS 3.8
Assembler:

-   ECB
-   ERROR
-   REQAREA
