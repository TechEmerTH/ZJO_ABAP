*----------------------------------------------------------------------
* Program ID       :
* Date(dd.mm.yyyy) :
* Author           :
* Description      :
*----------------------------------------------------------------------
*  CHANGE HISTORY
*----------------------------------------------------------------------
* Change By   :
* Change Date :
* Search Team : CH01
* Description :
*----------------------------------------------------------------------
REPORT zjo_smartform_hm.
*----------------------------------------------------------------------
* I N F O T Y P E S
*----------------------------------------------------------------------
*INFOTYPES:

*----------------------------------------------------------------------
* I N C L U D E
*----------------------------------------------------------------------
*INCLUDE: YEMWFR0000I_MAIN_INCLUDE.

*----------------------------------------------------------------------
* T A B L E S
*----------------------------------------------------------------------
*TABLES: P0001.

*----------------------------------------------------------------------
* T Y P E S
*----------------------------------------------------------------------

*TYPES: BEGIN OF TY_LINE,
*    COLUMN1 TYPE I,
*    COLUMN2 TYPE I,
*    COLUMN3 TYPE I,
* END OF TY_LINE.
*TYPES: TTY_TAB TYPE TABLE OF TY_LINE.

*----------------------------------------------------------------------
* C O N S T A N T S
*----------------------------------------------------------------------
CONSTANTS: c_address1 TYPE c LENGTH 30 VALUE '191 Silom Complex Building'.
CONSTANTS: c_address2 TYPE c LENGTH 30 VALUE 'Level 22, Unit A1-A2'.
CONSTANTS: c_address3 TYPE c LENGTH 30 VALUE 'Silom Road, Silom, Bangrak'.
CONSTANTS: c_address4 TYPE c LENGTH 30 VALUE 'Bangkok 10500 Thailand'.
CONSTANTS: c_auth TYPE c LENGTH 30 VALUE 'Patrick Stewart'.
CONSTANTS: c_dot        TYPE c LENGTH 1 VALUE '.'.
CONSTANTS: c_colon        TYPE c LENGTH 1 VALUE ':'.
CONSTANTS: c_smartform        TYPE c LENGTH 10 VALUE 'ZJO_SF'.
CONSTANTS: c_smartform_page_2        TYPE c LENGTH 18 VALUE 'ZJO_SF_PAGE2'.

*----------------------------------------------------------------------
* D A T A
*----------------------------------------------------------------------
*-> Object
*DATA : GO_P0001 TYPE REF TO CL_ABAP_TYPEDESCR.

*-> Internal tables
DATA: gt_pa0001    TYPE TABLE OF pa0001,
      gt_hrp1000_o TYPE TABLE OF hrp1000,
      gt_hrp1000_s TYPE TABLE OF hrp1000,
      gt_output    TYPE zhr_jo_tt_smartform,
      gt_output_2  TYPE zhr_jo_tt_org_info.

*-> Work areas
DATA: gw_pa0001        LIKE LINE OF gt_pa0001,
      gw_hrp1000       LIKE LINE OF gt_hrp1000_s,
      gw_output        LIKE LINE OF gt_output,
      gw_output_2      LIKE LINE OF gt_output_2,
      gw_output_detail TYPE zhr_jo_ts_smartform_detail.


*-> Ranges
*DATA : GR_P0001 TYPE RANGE OF P0001-PERNR.

*-> Variables
*DATA : GV_PERNR TYPE P0001-PERNR.

*-> Field-symbols
*FIELD-SYMBOLS : <FS_P0001> LIKE LINE OF GT_P0001.

*----------------------------------------------------------------------
* M A C R O S
*----------------------------------------------------------------------
*DEFINE MC_APPEND_DATA.
*END-OF-DEFINITION.

*----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*----------------------------------------------------------------------
PARAMETERS: p_adobe TYPE flag.
* SELECT-OPTIONS: S_DATE FOR SY-DATUM.

*----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*----------------------------------------------------------------------
INITIALIZATION.

*----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*----------------------------------------------------------------------
*AT SELECTION-SCREEN OUTPUT.
*
*AT SELECTION-SCREEN ON HELP REQUEST FOR <field>
* *AT SELECTION-SCREEN ON VALUE REQUEST FOR <field>
* *AT SELECTION-SCREEN ON <field>
* *AT SELECTION-SCREEN.

*----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*----------------------------------------------------------------------
START-OF-SELECTION.

  SELECT *
    FROM pa0001
    INTO TABLE gt_pa0001
         WHERE begda <= sy-datum
           AND endda >= sy-datum.

  IF gt_pa0001 IS NOT INITIAL.
    SELECT *
      FROM hrp1000
      INTO TABLE gt_hrp1000_o
      FOR ALL ENTRIES IN gt_pa0001
      WHERE objid = gt_pa0001-orgeh
      AND begda <= sy-datum
      AND endda >= sy-datum
      AND otype = 'O'.

    SELECT *
      FROM hrp1000
      INTO TABLE gt_hrp1000_s
      FOR ALL ENTRIES IN gt_pa0001
      WHERE objid = gt_pa0001-plans
      AND begda <= sy-datum
      AND endda >= sy-datum
      AND otype = 'S'.
  ENDIF.

*----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*----------------------------------------------------------------------
END-OF-SELECTION.

  PERFORM pf_prepare_data.
  IF p_adobe IS INITIAL.
    PERFORM pf_display.
  ELSE.
    PERFORM pf_display_adobe.
  ENDIF.
*----------------------------------------------------------------------
* T O P - O F – P A G E
*----------------------------------------------------------------------
*TOP-OF-PAGE.

*----------------------------------------------------------------------
* A T   L I N E - S E L E C T I O N
*----------------------------------------------------------------------
*AT LINE-SELECTION.

*----------------------------------------------------------------------
* F O R M S
*----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  PF_PREPARE_DATA
*&---------------------------------------------------------------------*
FORM pf_prepare_data .
  DATA: lt_pa0001 LIKE gt_pa0001.
  DATA: lw_pa0001 LIKE LINE OF gt_pa0001.
  DATA: lv_count TYPE i.
  DATA: lv_total TYPE i.
  DATA: lv_orgeh TYPE orgeh.

  FIELD-SYMBOLS <lfs_output> LIKE LINE OF gt_output.

  SORT gt_pa0001 BY orgeh pernr ASCENDING.
  lt_pa0001 = gt_pa0001.
  DELETE ADJACENT DUPLICATES FROM gt_pa0001 COMPARING orgeh.
  LOOP AT gt_pa0001 INTO gw_pa0001.
    CLEAR lv_count.
    CLEAR: gw_output.
    gw_output-org_id = gw_pa0001-orgeh.
    READ TABLE gt_hrp1000_o INTO gw_hrp1000 WITH KEY objid = gw_pa0001-orgeh.
    IF sy-subrc = 0.
      gw_output-org_name = gw_hrp1000-stext.
    ENDIF.
    LOOP AT lt_pa0001 INTO lw_pa0001 WHERE orgeh = gw_pa0001-orgeh.
      lv_count = lv_count + 1.
    ENDLOOP.
    gw_output-pos_amount = lv_count.
    lv_total = lv_total + lv_count.
    APPEND gw_output TO gt_output.
  ENDLOOP.

  LOOP AT lt_pa0001 INTO lw_pa0001.
    CLEAR gw_output_2.
    IF lw_pa0001-orgeh <> lv_orgeh.
      gw_output_2-newpage = abap_true.
    ENDIF.

    gw_output_2-emp_name = lw_pa0001-ename.
    gw_output_2-org_id = lw_pa0001-orgeh.

    READ TABLE gt_hrp1000_o INTO gw_hrp1000 WITH KEY objid = gw_pa0001-orgeh.
    IF sy-subrc = 0.
      gw_output_2-org_name = gw_hrp1000-stext.
    ENDIF.

    gw_output_2-pos_id = lw_pa0001-plans.
    READ TABLE gt_hrp1000_s INTO gw_hrp1000 WITH KEY objid = gw_pa0001-plans.
    IF sy-subrc = 0.
      gw_output_2-pos_name = gw_hrp1000-stext.
    ENDIF.
    lv_orgeh = gw_output_2-org_id.
    APPEND gw_output_2 TO gt_output_2.
  ENDLOOP.

  gw_output_detail-total = lv_total.
  gw_output_detail-address_l_1 = c_address1.
  gw_output_detail-address_l_2 = c_address2.
  gw_output_detail-address_l_3 = c_address3.
  gw_output_detail-address_l_4 = c_address4.
  gw_output_detail-auth_name = c_auth.
  gw_output_detail-print_date = sy-datum+6(2) && c_dot && sy-datum+4(2) && c_dot && sy-datum+0(4).
  gw_output_detail-print_time = sy-timlo+0(2) && c_colon && sy-timlo+2(2).
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PF_DISPLAY
*&---------------------------------------------------------------------*
FORM pf_display .
  DATA: lw_control TYPE ssfctrlop.
  DATA: lw_option TYPE ssfcompop.
  DATA: lv_fm TYPE rs38l_fnam.
  DATA: lv_fname TYPE tdsfname.
  DATA: lv_page TYPE int4.

  lv_fname = c_smartform.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lv_fname
*     VARIANT            = ' '
*     DIRECT_CALL        = ' '
    IMPORTING
      fm_name            = lv_fm
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.

  ENDIF.

*  lw_control-no_dialog = 'X'.
*  lw_control-preview = 'X'.
  lw_control-no_open = ''.
  lw_control-no_close = 'X'.
  lw_option-tddest        = 'LOCL'.
*  lw_option-tdnewid       = 'X'.
*  lw_option-tdfinal       = space.

  CALL FUNCTION lv_fm
    EXPORTING
      control_parameters = lw_control
      output_options     = lw_option
      user_settings      = space
      gw_detail          = gw_output_detail
    IMPORTING
*     document_output_info =
*     job_output_info    =
*     job_output_options =
      ev_total_page      = lv_page
    TABLES
      gt_output          = gt_output
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.

  ENDIF.

  lv_fname = c_smartform_page_2.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lv_fname
*     VARIANT            = ' '
*     DIRECT_CALL        = ' '
    IMPORTING
      fm_name            = lv_fm
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.

  ENDIF.
  lw_control-no_open = 'X'.
  lw_control-no_close = ''.
  lw_option-tdnewid       = space.
  CALL FUNCTION lv_fm
    EXPORTING
      control_parameters = lw_control
      output_options     = lw_option
      user_settings      = space
      gw_detail          = gw_output_detail
      gv_page            = lv_page
    TABLES
      gt_output          = gt_output_2
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_DISPLAY_ADOBE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_display_adobe .
  DATA: ls_outputparams TYPE sfpoutputparams.
  DATA: lv_fm_name TYPE rs38l_fnam.
  CONSTANTS: c_afname TYPE fpname VALUE 'ZJO_ADOBE_FORM'.

  CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
    EXPORTING
      i_name     = c_afname
    IMPORTING
      e_funcname = lv_fm_name.

  ls_outputparams-nodialog = 'X'.
  ls_outputparams-preview = 'X'.
  ls_outputparams-dest = 'PDF'.

  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = ls_outputparams
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
  ENDIF.

  CALL FUNCTION lv_fm_name
    EXPORTING
      gt_output      = gt_output_2
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
  ENDIF.

  CALL FUNCTION 'FP_JOB_CLOSE'
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.
