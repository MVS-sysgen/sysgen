EZASMI MNOTE Messages
=====================

The following messages, listed in alphabetic order, are issued for
errors detected during assembly of the EZASMI macro.

**ERETMSK is required when ESNDMSK is specified**

:   **Meaning:** The use of the *ESNDMSK* keyword parameter of the
    **SELECT** function requires that the *WRETMSK* keyword also be
    specified.

    **User Action:** Specify the missing keyword parameter.

**Function TYPE keyword required**

:   **Meaning:** Every EZASMI macro must include the **TYPE** keyword
    parameter identifying the requested function.

    **User Action:** Correct the error by coding the **TYPE** parameter
    to indicate the desired function.

**Invalid keyword value, vvvvvvvv**

:   **Meaning:** A numeric constant has been specified for a parameter
    value where only the address of a value is allowed.

    **User Action:** Correct the keyword value.

**Invalid list name, llllllll**

:   **Meaning:** This is an internal error that should not occur.

    **User Action:** Report this error to the developers.

**Invalid MF parm**

:   **Meaning:** The only supported **MF** options are *E* and *L*.

    **User Action:** Correct the invalid option.

**Invalid option specified, kkkkkkkk**

:   **Meaning:** The keyword *kkkkkkkk* is not valid with the specified
    function **TYPE**.

    **User Action:** Correct the error by changing or removing the
    invalid keyword.

**Invalid parameter value, kkkkkkkk**

:   **Meaning:** The value specified for keyword *kkkkkkkk* is not
    valid.

    **User Action:** Correct the keyword value.

**Invalid TYPE value, tttttttt**

:   **Meaning:** The **TYPE** parameter value of *tttttttt* is not the
    name of a supported EZASMI function.

    **User Action:** Correct the invalid function name.

**Keyword value vvvvvvvv greater than mmmmmmmm**

:   **Meaning:** The value of a numeric constant keyword parameter value
    is greater than the maximum value allowed.

    **User Action:** Correct the keyword value.

**Missing label for LIST form**

:   **Meaning:** The **EZASMI MF=L** macro statement must include a
    label so that the *MF=E* form may refer to it.

    **User Action:** Provide a statement label.

**Missing required option, kkkkkkkk**

:   **Meaning:** The specified function **TYPE** requires that keyword
    *kkkkkkkk* be specified.

    **User Action:** Provide the required keyword and value.

**Missing value string, kkkkkkkk**

:   **Meaning:** This is an internal error that should not occur.

    **User Action:** Report this error to the developers.

**No LIST specified for E form of macro**

:   **Meaning:** The *E* option of the **MF** parameter requires that
    you specify the address of the parameter list storage area.

    **User Action:** Correct the **MF** parameter by specifying the
    address of the parameter list.

**RRETMSK is required when RSNDMSK is specified**

:   **Meaning:** The use of the *RSNDMSK* keyword parameter of the
    **SELECT** function requires that the *RRETMSK* keyword also be
    specified.

    **User Action:** Specify the missing keyword parameter.

**Unknown keyword kkkkkkkk**

:   **Meaning:** The valid keyword *kkkkkkkk* is not properly defined in
    the EZASMI macro set. This is an internal error that should not
    occur.

    **User Action:** Report this error to the developers.

**WRETMSK is required when WSNDMSK is specified**

:   **Meaning:** The use of the *WSNDMSK* keyword parameter of the
    **SELECT** function requires that the *WRETMSK* keyword also be
    specified.

    **User Action:** Specify the missing keyword parameter.
