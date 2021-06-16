TERMAPI
=======

When all tasks issuing EZASMI requests have completed TCP/IP processing,
the **TERMAPI** function should be issued by the same task from which
the **INITAPI** function was originally requested (either explicitly or
implicitly). Failure to do this may result in allocated storage not
being released.

Parameter Considerations
------------------------

None.

Unsupported Parameters
----------------------

None.
