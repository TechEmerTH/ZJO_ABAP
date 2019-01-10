*&---------------------------------------------------------------------*
*& Report ZJO_SMARTFORM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zjo_smartform.

INFOTYPES:0001.

TABLES pernr.
DATA: gt_p0001   TYPE p0001_tab,
      gw_p0001   LIKE LINE OF gt_p0001,
      gv_fm_name TYPE rs38l_fnam,
      lv_output_option TYPE SSFCOMPOP,
      lv_control TYPE SSFCTRLOP.

NODES peras.

START-OF-SELECTION.

  GET peras.

  LOOP AT p0001.
    CLEAR: gw_p0001.
    MOVE-CORRESPONDING p0001 TO gw_p0001.
    APPEND gw_p0001 TO gt_p0001.
  ENDLOOP.

END-OF-SELECTION.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZJO_TEST'
    IMPORTING
      fm_name            = gv_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc = 0.
    lv_output_option-tddest = 'locl'.

    CALL FUNCTION GV_FM_NAME
      EXPORTING
       CONTROL_PARAMETERS = lv_control
       OUTPUT_OPTIONS   = lv_output_option
        gt_p0001         = gt_p0001
        gv_pn_begda      = pn-begda
        gv_pn_endda      = pn-endda
        user_settings    = space
      EXCEPTIONS
        formatting_error = 1
        internal_error   = 2
        send_error       = 3
        user_canceled    = 4
        OTHERS           = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


  ENDIF.
