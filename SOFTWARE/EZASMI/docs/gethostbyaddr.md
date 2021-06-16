GETHOSTBYADDR
=============

Parameter Considerations
------------------------

**HOSTADR**
:   The parameter value may not be specified as a *number*. This appears
    to be an error in the IBM documentation.

**HOSTENT**
:   TCP/IP for MVS 3.8 Assembler returns only a subset of the
    information returned by z/OS as follows:
    -   No alias names are returned; the address of the alias_list will
        be zero.
    -   Only a single hostname is returned.

    The returned *HOSTENT* structure is unique for each task using
    EZASMI functions. If any task issues multiple function requests
    returning a *HIOSTENT* structure, any prior information in the
    structure will be overlaid; you should save any required data before
    issuing subsequent requests.

Unsupported Parameters
----------------------

The following keyword parameters are not supported by TCP/IP for MVS 3.8
Assembler:

-   ERROR
