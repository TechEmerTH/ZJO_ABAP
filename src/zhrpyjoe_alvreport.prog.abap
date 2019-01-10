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
* Search Team :
* Description :
*----------------------------------------------------------------------
REPORT zhrpyjoe_alvreport.
*----------------------------------------------------------------------
* I N F O T Y P E S
*----------------------------------------------------------------------
NODES : peras.
INFOTYPES: 0001,0002,0008.

*----------------------------------------------------------------------
* I N C L U D E
*----------------------------------------------------------------------
INCLUDE: YEMWFR0000I_MAIN_INCLUDE.

*----------------------------------------------------------------------
* T A B L E S
*----------------------------------------------------------------------
TABLES: pernr.

*----------------------------------------------------------------------
* T Y P E S
*----------------------------------------------------------------------
TYPES : BEGIN OF ty_output,
          seq   TYPE i,
          pernr TYPE pa0001-pernr,
          vorna TYPE pa0002-vorna,
          nachn TYPE pa0002-nachn,
          plans TYPE pa0001-plans,
          orgeh TYPE pa0001-orgeh,
          betrg LIKE q0008-sumbb,
          waers LIKE tcurc-waers,
          note  TYPE c LENGTH 200,


        END OF ty_output.


*TYPES : begin of ty_output,
*      pernr TYPE pa0001-pernr,
*         ename TYPE pa0001-ename,
*        END OF ty_output.
*----------------------------------------------------------------------
* C O N S T A N T S
*----------------------------------------------------------------------
*CONSTANTS: C_FLAG_Y VALUE 'Y'.

*----------------------------------------------------------------------
* D A T A
*----------------------------------------------------------------------
*-> Object
*DATA : GO_P0001 TYPE REF TO CL_ABAP_TYPEDESCR.

*-> Internal tables
DATA : gt_output   TYPE TABLE OF ty_output,
       gt_fieldcat TYPE slis_t_fieldcat_alv..

*-> Work areas08
DATA: gw_output       LIKE LINE OF gt_output.


*-> Variables
DATA: i TYPE i.
DATA: i2 TYPE i.

*-> Field-symbols
FIELD-SYMBOLS: <fs-output> LIKE LINE OF gt_output.

*----------------------------------------------------------------------
* M A C R O S
*----------------------------------------------------------------------
*DEFINE MC_APPEND_DATA.
*END-OF-DEFINITION.

DEFINE mc_append_fieldcat.
  ls_fieldcat-fieldname = &1 .
ls_fieldcat-col_pos = i2 + 1.
ls_fieldcat-seltext_s = &2.
ls_fieldcat-seltext_m = &2.
ls_fieldcat-seltext_l = &2.
APPEND ls_fieldcat TO gt_fieldcat.
END-OF-DEFINITION.


*----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*----------------------------------------------------------------------
SELECTION-SCREEN: BEGIN OF BLOCK testrun WITH FRAME TITLE TEXT-a01.

PARAMETERS: textrun TYPE c  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN: END OF BLOCK testrun.
*SELECTION-SCREEN: BEGIN OF BLOCK outoff WITH FRAME TITLE TEXT-a02.
*
*PARAMETERS: textrun TYPE c  AS CHECKBOX DEFAULT 'X'.
*SELECTION-SCREEN: END OF BLOCK outoff.
*----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*----------------------------------------------------------------------
INITIALIZATION.
*  pnppernr-sign    = 'I'.
*  pnppernr-option = 'BT'.
*  pnppernr-low     = '26000021'.
*  pnppernr-high    = '26000025'.
*

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
  i = 1.
  REFRESH : gt_output.

  GET peras.
  rp_provide_from_last p0001 space pn-begda pn-endda.
  IF pnp-sw-found NE 1.
    REJECT 'PERAS'.
  ENDIF.
  rp_provide_from_last p0002 space pn-begda pn-endda.

  CLEAR gw_output.
  gw_output-seq = i.
  gw_output-pernr = p0001-pernr.
  gw_output-plans = p0001-plans.
  gw_output-orgeh = p0001-orgeh.
  gw_output-vorna = p0002-vorna.
  gw_output-nachn = p0002-nachn.
  APPEND gw_output TO gt_output.
  i = i + 1.
*----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*----------------------------------------------------------------------
END-OF-SELECTION.
  LOOP AT gt_output ASSIGNING <fs-output>.

    CALL FUNCTION 'HR_GET_TOTAL_AMOUNT_P0008'
      EXPORTING
        pernr             = <fs-output>-pernr
      IMPORTING
        amount            = <fs-output>-betrg
        currency          = <fs-output>-waers
      EXCEPTIONS
        no_entry_in_t001p = 1
        no_entry_in_t503  = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDLOOP.


  PERFORM sub_display_alv.



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
*&      Form  SUB_DISPLAY_ALV
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SUB_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_display_alv .
  DATA: ls_fieldcat LIKE LINE OF gt_fieldcat,
        ls_layout   TYPE slis_layout_alv.

  mc_append_fieldcat 'SEQ' TEXT-001 .
  mc_append_fieldcat 'PERNR' TEXT-002  .
  mc_append_fieldcat 'PLANS' TEXT-003  .
  mc_append_fieldcat 'GRGEH' TEXT-004 .
  mc_append_fieldcat 'VORNA' TEXT-005.
  mc_append_fieldcat 'NACHN' TEXT-006 .
  mc_append_fieldcat 'BETRG' TEXT-007 .
  mc_append_fieldcat 'WAERS' TEXT-008 .

  ls_layout-zebra = 'X'.

  PERFORM header.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program          = sy-repid
      i_callback_pf_status_set    = 'PF_STATUS_SET'
      i_callback_user_command     = 'PF_USER_COMMAND'
      i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
      is_layout                   = ls_layout
      it_fieldcat                 = gt_fieldcat
        I_SAVE                      = abap_true
           i_html_height_top           = 40
    TABLES
      t_outtab                    = gt_output
    EXCEPTIONS
      program_error               = 1
      OTHERS                      = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM header .
DATA: lw_header LIKE LINE OF i_extra_top.
v_top_title = sy-title.
ENDFORM.
