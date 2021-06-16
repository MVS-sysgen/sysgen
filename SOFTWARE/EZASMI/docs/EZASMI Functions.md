---
title: Supported z/OS EZASMI Functions
---

Supported z/OS EZASMI Functions
===============================

Each of the EZASMI functions supported by TCP/IP for MVS 3.8 Assembler
are listed in the subsequent topics. Any differences or restrictions
related to the z/OS EZASMI macro are noted where appropriate. Links to
the IBM documentation for the various functions can be found here:

::: {style="color:#1E90FF"}
www.ibm.com/support/knowledgecenter/en/SSLTBW_2.2.0/com.ibm.zos.v2r2.hala001/mac.htm
:::

You might also find it helpful to read

::: {style="color:#1E90FF"}
www.ibm.com/support/knowledgecenter/en/SSLTBW_2.2.0/com.ibm.zos.v2r2.hala001/mparm.htm
:::

which describes the various types of operands which can be used with the
keyword parameter values.

In the function topics below, you will notice that in some cases there
are function parameters that are supported in the z/OS environment but
not the MVS 3.8 environment. Any unsupported parameters may still be
coded on EZASMI macros, however the parameter values will not be
validated and will not result in generating any object code. This
approach will allow code to be ported down to MVS 3.8 from z/OS with
minimal changes; it is the user\'s responsibility to ensure that the
lack of support for some parameters does not materially affect the
operation of the program.

Since the IBM documentation does not make this clear, it should be noted
that any character string operand or character string value returned by
EZASMI functions is an EBCDIC character string.
