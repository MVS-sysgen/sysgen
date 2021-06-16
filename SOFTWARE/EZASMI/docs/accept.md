ACCEPT
======

Parameter Considerations
------------------------

**NAME**
:   There is no BPXYSOCK macro mapping the socket address structure
    returned by the ACCEPT function. The returned IPv4 fields are in the
    exact order listed in the z/OS documentation.

Unsupported Parameters
----------------------

The following keyword parameters are not supported by TCP/IP for MVS 3.8
Assembler:

-   NS
-   ECB
-   ERROR
-   REQAREA
