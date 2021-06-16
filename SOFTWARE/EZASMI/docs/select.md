SELECT
======

Parameter Considerations
------------------------

With the z/OS Communications Server, the largest value that may be
specified for the **MAXSOC** parameter is a function of the number of
bits in the referenced bit mask.

With MVS, **MAXSOC** must be set to the *exact* number of the
highest-numbered socket to be monitored, plus 1. If the highest socket
number to be monitored is greater than the highest socket number in use,
SELECT will return an ENOTSOCK error (decimal 38). Note that z/OS does
not return this error number for SELECT.

Unsupported Parameters
----------------------

The following keyword parameters are not supported by TCP/IP for MVS 3.8
Assembler:

-   ECB
-   ERROR
-   REQAREA
