REPORT zjo_adone_form_test.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_pernr TYPE pernr_d.

SELECTION-SCREEN END OF BLOCK b1.

*START-OF-SELECTION.

DATA: form            TYPE tdsfname  VALUE 'ZJO_AF_ADOBE_FORM',
      cntry           TYPE c LENGTH 2,
      lx_fp_api       TYPE REF TO cx_fp_api,
      fm_name         TYPE rs38l_fnam,      " CHAR 30 0 Name of Function Module
      fp_docparams    TYPE sfpdocparams,    " Structure  SFPDOCPARAMS Short Description  Form Parameters for Form Processing
      fp_outputparams TYPE sfpoutputparams. " Structure  SFPOUTPUTPARAMS Short Description  Form Processing Output Parameter

fp_outputparams-preview  = 'X'.
* Sets the output parameters and opens the spool job
CALL FUNCTION 'FP_JOB_OPEN'                   "& Form Processing: Call Form
  CHANGING
    ie_outputparams = fp_outputparams
  EXCEPTIONS
    cancel          = 1
    usage_error     = 2
    system_error    = 3
    internal_error  = 4
    OTHERS          = 5.
IF sy-subrc <> 0.
*            <error handling>
ENDIF.

*&---- Get the name of the generated function module
TRY.
    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
      EXPORTING
        i_name     = form
      IMPORTING
        e_funcname = fm_name.
  CATCH cx_fp_api INTO lx_fp_api.
*   exception handling
*      MESSAGE ID lx_fp_api->msgid TYPE lx_fp_api->msgty
*        NUMBER lx_fp_api->msgno
*          WITH lx_fp_api->msgv1 lx_fp_api->msgv2
*               lx_fp_api->msgv3 lx_fp_api->msgv4.
*      cf_retcode = 1.
    MESSAGE 'There was a problem with the function module name' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
ENDTRY.


*-- Fetch the Data and store it in the Internal Table
*  SELECT * FROM mari INTO TABLE it_mari UP TO 15 ROWS.


* Language and country setting
* Set form language and country (->form locale)
fp_docparams-langu = sy-langu.


*&--- Call the generated function module
CALL FUNCTION fm_name
  EXPORTING
    /1bcdwb/docparams = fp_docparams
    lv_pernr         = p_pernr
* IMPORTING
*   /1BCDWB/FORMOUTPUT       =
  EXCEPTIONS
    usage_error       = 1
    system_error      = 2
    internal_error    = 3
    OTHERS            = 4.
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

*&---- Close the spool job
CALL FUNCTION 'FP_JOB_CLOSE'
* IMPORTING
*   E_RESULT             =
  EXCEPTIONS
    usage_error    = 1
    system_error   = 2
    internal_error = 3
    OTHERS         = 4.
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.
