BIND
====

Parameter Considerations
------------------------

**NAME**
:   There is no BPXYSOCK macro mapping the socket address structure
    input to the BIND function. The IPv4 fields are specified in the
    exact order listed in the z/OS documentation.

Unsupported Parameters
----------------------

The following keyword parameters are not supported by TCP/IP for MVS 3.8
Assembler:

-   ECB
-   ERROR
-   REQAREA
