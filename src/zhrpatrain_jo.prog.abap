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
REPORT zhrpatrain_jo.
*----------------------------------------------------------------------
* I N F O T Y P E S
*----------------------------------------------------------------------
INFOTYPES: 0001,0002,0041.

*----------------------------------------------------------------------
* I N C L U D E
*----------------------------------------------------------------------
INCLUDE: YEMWFR0000I_MAIN_INCLUDE_jo.

*----------------------------------------------------------------------
* T A B L E S
*----------------------------------------------------------------------
TABLES: pernr.

NODES: peras.
*----------------------------------------------------------------------
* T Y P E S
*----------------------------------------------------------------------

TYPES: BEGIN OF TY_output,
    pernr TYPE PA0001-PERNR ,
    persg TYPE P0001-PERSG,
    persk TYPE P0001-persk,
    vorna TYPE P0002-vorna,
    nachn TYPE P0002-nachn,
    gbdat TYPE P0002-gbdat,
    dar01 TYPE p0041-dar01,
    dat01 TYPE p0041-dat01,
 END OF TY_output.
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
DATA : GT_output TYPE TABLE OF ty_output,
      gt_fieldcat TYPE  slis_t_fieldcat_alv .

*-> Work areas
DATA : GW_output LIKE LINE OF GT_output.

*-> Ranges
*DATA : GR_P0001 TYPE RANGE OF P0001-PERNR.

*-> Variables
DATA : gv_date TYPE sy-datum,
       gv_col_pos TYPE i,
       gv_file_path TYPE ibipparms-path,
       gv_lastdate TYPE sy-datum.

*-> Field-symbols
FIELD-SYMBOLS : <FS_P0041> LIKE LINE OF p0041.

*----------------------------------------------------------------------
* M A C R O S
*----------------------------------------------------------------------
DEFINE mc_append_fieldcat.
  CLEAR: ls_fieldcat.
  gv_col_pos            = gv_col_pos + 1 .
  ls_fieldcat-fieldname = &1 .
  ls_fieldcat-col_pos   = gv_col_pos.
  ls_fieldcat-seltext_s = &2.
  ls_fieldcat-seltext_m = &2.
  ls_fieldcat-seltext_l = &2.
  APPEND ls_fieldcat TO gt_fieldcat.
END-OF-DEFINITION.

*----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*----------------------------------------------------------------------
* PARAMETERS: P_KDATE TYPE SY-DATUM
* SELECT-OPTIONS: S_DATE FOR SY-DATUM.
SELECTION-SCREEN: begin of BLOCK b1 WITH FRAME TITLE text-009 .
  parameters: p_output TYPE string DEFAULT 'C:\Users\kjirakhunthaworn\Desktop\Personel_report.txt'.
  SELECTION-SCREEN: end of BLOCK b1.

*----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*----------------------------------------------------------------------
INITIALIZATION.
pnpbegda = sy-datum.
  PNPSTAT2-sign    = 'I'.
  PNPSTAT2-option = 'EQ'.
  PNPSTAT2-low     = 3.
  APPEND PNPSTAT2.
*----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*----------------------------------------------------------------------
*AT SELECTION-SCREEN OUTPUT.
*
*AT SELECTION-SCREEN ON HELP REQUEST FOR <field>
* *AT SELECTION-SCREEN ON VALUE REQUEST FOR <field>
* *AT SELECTION-SCREEN ON <field>
AT SELECTION-SCREEN.
  CASE pnptimed.
    WHEN 'K'.
      pnpbegda = sy-datum.
    WHEN 'I'.
      PERFORM pf_calldate.
      pnpendda = gv_date.
      CONCATENATE  sy-datum+0(4)  sy-datum+4(2)  '01' INTO pnpbegda.
  ENDCASE.
    AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_output.
    CLEAR gv_file_path.
  PERFORM pf_browse_locl_folder.
  MOVE gv_file_path TO p_output.
*----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*----------------------------------------------------------------------
START-OF-SELECTION.

  GET peras.
rp_provide_from_last p0001 space pn-begda pn-endda.
  IF pnp-sw-found NE 1.
    REJECT .
  ENDIF.

  rp_provide_from_last p0002 space pn-begda pn-endda.
  IF pnp-sw-found NE 1.
    REJECT .
  ENDIF.

  rp_provide_from_last p0002 space pn-begda pn-endda.
  IF pnp-sw-found NE 1.
    REJECT .
  ENDIF.

  gw_output-pernr = p0001-pernr.
  gw_output-persk = p0001-persk.
  gw_output-persg = p0001-persg.
  gw_output-vorna = p0002-vorna.
  gw_output-nachn = p0002-nachn.
  gw_output-gbdat = p0002-gbdat.

  rp_provide_from_last p0041 space pn-begda pn-endda.
  IF pnp-sw-found EQ 1.
    do 12 times varying gw_output-dar01 from p0041-dar01 next p0041-dar02
                VARYING gw_output-dat01 FROM p0041-dat01 NEXT p0041-dat02.
      if gw_output-dar01 = '01'.
        exit.
        endif.
    enddo.
  ENDIF.

APPEND gw_output to gt_output.

*----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*----------------------------------------------------------------------
END-OF-SELECTION.
perform pf_alv.
PERFORM download_to_pc.
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
*&      Form  PF_CALLDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_calldate .
CALL FUNCTION '/DSD/PR_LAST_DAY_OF_MONTHS'
  EXPORTING
    day_in                  = sy-datum
 IMPORTING
   LAST_DAY_OF_MONTH       = gv_date
 EXCEPTIONS
   DAY_IN_NO_DATE          = 1
   OTHERS                  = 2
          .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

ENDFORM.
FORM pf_alv .
  DATA: ls_fieldcat LIKE LINE OF gt_fieldcat,
        ls_layout   TYPE slis_layout_alv.

  mc_append_fieldcat 'PERNR'  TEXT-001 .
  mc_append_fieldcat 'PERSG'  TEXT-002  .
  mc_append_fieldcat 'PERSK'  TEXT-003  .
  mc_append_fieldcat 'VORNA'  TEXT-004  .
  mc_append_fieldcat 'NACHN'  TEXT-005  .
  mc_append_fieldcat 'GBDAT'  TEXT-006  .
  mc_append_fieldcat 'DAR01'  TEXT-007  .
  mc_append_fieldcat 'DAT01'  TEXT-008  .

PERFORM header.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = ls_layout
      it_fieldcat        = gt_fieldcat
      i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
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
*&      Form  HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM header .
  DATA: lw_header LIKE LINE OF i_extra_top. "เพิ่มที่หัว include
  v_top_title = sy-title.
  w_close_fields-period = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_BROWSE_LOCL_FOLDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_browse_locl_folder .
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'PATH'
    IMPORTING
      file_name     = gv_file_path.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_TO_PC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_to_pc .
 DATA: lv_codepage1 TYPE cpcodepage,
        lv_codepage  TYPE abap_encoding,
        lv_filename  TYPE string.

  CALL FUNCTION 'SCP_CODEPAGE_BY_EXTERNAL_NAME'
    EXPORTING
      external_name = 'UTF-8'
    IMPORTING
      sap_codepage  = lv_codepage1
    EXCEPTIONS
      not_found     = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  lv_codepage = lv_codepage1.
  lv_filename = p_output.

  PERFORM pf_replace_date_and_time CHANGING lv_filename.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_filename
      filetype                = 'ASC'
      append                  = 'X'
      write_field_separator   = 'X'
      codepage                = lv_codepage
    TABLES
      data_tab                = gt_output
*     FIELDNAMES              =
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
