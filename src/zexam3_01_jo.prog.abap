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
REPORT zexam3_01_jo.
*----------------------------------------------------------------------
* I N F O T Y P E S
*----------------------------------------------------------------------
INFOTYPES: 0001,0002,0185.
NODES : peras.
*----------------------------------------------------------------------
* I N C L U D E
*----------------------------------------------------------------------
*INCLUDE: YEMWFR0000I_MAIN_INCLUDE.

*----------------------------------------------------------------------
* T A B L E S
*----------------------------------------------------------------------
TABLES: pernr.

*----------------------------------------------------------------------
* T Y P E S
*----------------------------------------------------------------------

TYPES : BEGIN OF ty_output,
          pernr     TYPE pa0001-pernr,
          ename     TYPE pa0001-ename,
          age       TYPE pa0001-ename,
          male      TYPE i,
          female    TYPE i,
          regular   TYPE i,
          contact   TYPE i,
          idcard    TYPE pa0185-icnum,
          objid_o   TYPE hrp1000-objid,
          stext_o   TYPE hrp1000-stext,
          objid_s_b TYPE hrp1000-objid,
          stext_s_b TYPE hrp1000-stext,
          ee_group  TYPE t501t-ptext,
          sex       TYPE string,

        END OF ty_output.
*TYPES: TTY_TAB TYPE TABLE OF TY_LINE.

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
       gt_objec    TYPE TABLE OF objec,
       gt_objec2   TYPE TABLE OF objec,
       gt_t501t    TYPE TABLE OF t501t,
       gt_fieldcat TYPE slis_t_fieldcat_alv,
       gt_taba     TYPE STANDARD TABLE OF dd07v,
       gt_tabn     TYPE STANDARD TABLE OF dd07v.

*-> Work areas
DATA : gw_output LIKE LINE OF gt_output.

*-> Ranges
*DATA : GR_P0001 TYPE RANGE OF P0001-PERNR.

*-> Variables
DATA : gv_col_pos TYPE i,
       gv_years   TYPE num2,
       gv_months  TYPE num2,
       gv_days    TYPE num2.

*-> Field-symbols
FIELD-SYMBOLS : <fs_output> LIKE LINE OF gt_output,
                <fs_objec>  LIKE LINE OF gt_objec,
                <fs_objec2> LIKE LINE OF gt_objec2,
                <fs_t501t>  LIKE LINE OF gt_t501t,
                <fs_taba>   LIKE LINE OF gt_taba,
                <fs_p0185>  LIKE LINE OF p0185.

*----------------------------------------------------------------------
* M A C R O S
*----------------------------------------------------------------------
*DEFINE MC_APPEND_DATA.
*END-OF-DEFINITION.

DEFINE mc_append_fieldcat.
  gv_col_pos            = gv_col_pos + 1 .
   ls_fieldcat-fieldname = &1 .
   ls_fieldcat-col_pos   = gv_col_pos.
   ls_fieldcat-seltext_s = &2.
   ls_fieldcat-seltext_m = &2.
   ls_fieldcat-seltext_l = &2.
     CASE ls_fieldcat-fieldname.
   WHEN 'MALE'.
     ls_fieldcat-do_sum = abap_true.
   WHEN 'FEMALE'.
  ls_fieldcat-do_sum = abap_true.

   WHEN 'REGULAR'.
  ls_fieldcat-do_sum = abap_true.
   WHEN 'CONTACT'.
  ls_fieldcat-do_sum = abap_true.

     ENDCASE.
     APPEND ls_fieldcat TO gt_fieldcat.
END-OF-DEFINITION.
*----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*----------------------------------------------------------------------
* PARAMETERS: P_KDATE TYPE SY-DATUM.
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
  PERFORM get_text.
  PERFORM get_sex.
  GET peras.

  rp_provide_from_last p0001 space pn-begda pn-endda.
  IF pnp-sw-found NE 1.
    REJECT .
  ENDIF.

  rp_provide_from_last p0002 space pn-begda pn-endda.
  IF pnp-sw-found NE 1.
    REJECT .
  ENDIF.

  CLEAR: gw_output.
  gw_output-pernr = p0001-pernr.
  gw_output-ename = p0001-ename.

* get age
  CALL FUNCTION 'HRCM_TIME_PERIOD_CALCULATE'
    EXPORTING
      begda         = p0002-gbdat
      endda         = sy-datum
    IMPORTING
      noyrs         = gv_years
      nomns         = gv_months
      nodys         = gv_days
    EXCEPTIONS
      invalid_dates = 1
      overflow      = 2
      OTHERS        = 3.
  CONCATENATE gv_days gv_months gv_years INTO gw_output-age .

*get idcard
  LOOP AT p0185 ASSIGNING <fs_p0185> WHERE begda <= pn-endda
                                         AND endda >= pn-begda.
    CASE <fs_p0185>-subty.
      WHEN '01'.
        gw_output-idcard = <fs_p0185>-icnum.
    ENDCASE.
  ENDLOOP.

*get ee_group for sum
  IF p0001-persg = 1.
    gw_output-regular = 1.
  ELSEIF  p0001-persg = 3.
    gw_output-contact = 1.
  ENDIF.

*get sex for sum
  IF p0002-gesch = 1.
    gw_output-male = 1.
  ELSEIF  p0002-gesch = 2.
    gw_output-female = 1.
  ENDIF.

*get ee_group
  LOOP AT gt_t501t ASSIGNING <fs_t501t> WHERE persg = p0001-persg.
    gw_output-ee_group = <fs_t501t>-ptext.
  ENDLOOP.

*get sexs
  LOOP AT gt_taba ASSIGNING <fs_taba> WHERE domvalue_l = p0002-gesch .
    gw_output-sex = <fs_taba>-ddtext.
  ENDLOOP.
  APPEND gw_output TO gt_output.
*----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*----------------------------------------------------------------------
END-OF-SELECTION.
  PERFORM pf_get_om_data.
  PERFORM pf_alv.
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
*&      Form  GET_OM_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_get_om_data .
  LOOP AT gt_output ASSIGNING <fs_output>.
    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype      = 'P'
        act_objid      = <fs_output>-pernr
        act_wegid      = 'P-S-O-O'
        act_begda      = pn-begda
        act_endda      = pn-endda
      TABLES
        result_objec   = gt_objec
      EXCEPTIONS
        no_plvar_found = 1
        no_entry_found = 2
        OTHERS         = 3.

*-- Get Organization
    LOOP AT gt_objec ASSIGNING <fs_objec>
             WHERE otype = 'O'.

*- FIND closer org
      IF gw_output-objid_o IS INITIAL
     AND <fs_objec>-objid IS NOT INITIAL.
        <fs_output>-objid_o = <fs_objec>-objid.
        <fs_output>-stext_o = <fs_objec>-stext.
      ENDIF.

*- FIND MANAGER
      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype      = 'O'
          act_objid      = <fs_objec>-objid
          act_wegid      = 'BOSSONLY'
          act_begda      = pn-begda
          act_endda      = pn-endda
        TABLES
          result_objec   = gt_objec2
        EXCEPTIONS
          no_plvar_found = 1
          no_entry_found = 2
          OTHERS         = 3.

      LOOP AT gt_objec2 ASSIGNING <fs_objec2> WHERE otype = 'P'
                                              AND  objid <> <fs_output>-pernr.
        IF sy-subrc EQ 0 .
          <fs_output>-objid_s_b = <fs_objec2>-objid.
          <fs_output>-stext_s_b = <fs_objec2>-stext.
        ENDIF.
      ENDLOOP.

      IF <fs_output>-objid_s_b IS NOT INITIAL
      AND <fs_output>-stext_s_b IS NOT INITIAL.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_alv .
  DATA: ls_fieldcat LIKE LINE OF gt_fieldcat,
        ls_layout   TYPE slis_layout_alv.

  mc_append_fieldcat 'PERNR'       TEXT-001 .
  mc_append_fieldcat 'ENAME'       TEXT-002 .
  mc_append_fieldcat 'AGE'         TEXT-011 .
  mc_append_fieldcat 'SEX'         TEXT-012 .
  mc_append_fieldcat 'MALE'        TEXT-003 .
  mc_append_fieldcat 'FEMALE'      TEXT-004 .
  mc_append_fieldcat 'EE_GROUP'    TEXT-010 .
  mc_append_fieldcat 'REGULAR'     TEXT-005 .
  mc_append_fieldcat 'CONTACT'     TEXT-006 .
  mc_append_fieldcat 'IDCARD'      TEXT-007 .
  mc_append_fieldcat 'STEXT_O'     TEXT-008 .
  mc_append_fieldcat 'STEXT_S_B'   TEXT-009 .

  ls_layout-zebra = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      i_save             = abap_true
      is_layout          = ls_layout
      it_fieldcat        = gt_fieldcat
    TABLES
      t_outtab           = gt_output
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_text .
  SELECT * FROM t501t INTO TABLE gt_t501t WHERE sprsl = sy-langu .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_SEX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sex .
CALL FUNCTION 'DD_DOMA_GET'
    EXPORTING
      domain_name   = 'GESCH'
      langu         = sy-langu
      withtext      = 'X'
    TABLES
      dd07v_tab_a   = gt_taba
      dd07v_tab_n   = gt_tabn
    EXCEPTIONS
      illegal_value = 1
      op_failure    = 2
      OTHERS        = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
