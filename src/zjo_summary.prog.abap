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
REPORT zjo_summary.


*----------------------------------------------------------------------
* I N F O T Y P E S
*----------------------------------------------------------------------
*INFOTYPES:

*----------------------------------------------------------------------
* I N C L U D E
*----------------------------------------------------------------------
INCLUDE: yemwfr0000i_main_include.

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
TYPES: BEGIN OF ty_input,
         column1 TYPE c LENGTH 30,
         column2 TYPE c LENGTH 30,
         column3 TYPE c LENGTH 30,
       END OF ty_input.


TYPES: BEGIN OF ty_output,
         column1 TYPE c LENGTH 30,
         column2 TYPE c LENGTH 30,
         column3 TYPE c LENGTH 30,
         column4 TYPE c LENGTH 30,
       END OF ty_output.

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
*DATA : GT_P0001 TYPE TABLE OF P0001.
DATA: gt_output TYPE TABLE OF ty_output,
      gt_input  TYPE TABLE OF ty_input,
      gt_file   TYPE TABLE OF string.





*-> Work areas
*DATA : GW_P0001 LIKE LINE OF I_P0001.
DATA: gw_output LIKE LINE OF gt_output,
      gw_input  LIKE LINE OF gt_input,
      gw_file   LIKE LINE OF gt_file.
*-> Ranges
*DATA : GR_P0001 TYPE RANGE OF P0001-PERNR.

*-> Variables
*DATA : GV_PERNR TYPE P0001-PERNR.
DATA: lv_filepath TYPE string,
      gv_splite TYPE char1,
      gv_field  TYPE c LENGTH 30..
*-> Field-symbols
*FIELD-SYMBOLS : <FS_P0001> LIKE LINE OF GT_P0001.

*----------------------------------------------------------------------
* M A C R O S
*----------------------------------------------------------------------
*DEFINE MC_APPEND_DATA.
*END-OF-DEFINITION.
*zsel_scrn_interface_input space space.
*----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*----------------------------------------------------------------------
* PARAMETERS: P_KDATE TYPE SY-DATUM.
* SELECT-OPTIONS: S_DATE FOR SY-DATUM.

*----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*----------------------------------------------------------------------
INITIALIZATION.

*    zset_scrn_interface_input vm_h_in vm_h_pc vm_h_app.
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
  DATA: lv_label TYPE c LENGTH 5,
        lv_sum   TYPE i.

  gv_splite = cl_abap_char_utilities=>horizontal_tab.

  PERFORM read_from_sap.

  CLEAR: gw_file, gw_input, gw_output.
  LOOP AT gt_file INTO gw_file.
    SPLIT gw_file AT gv_splite INTO gw_input-column1
                                   gw_input-column2
                                   gw_input-column3.
    APPEND gw_input TO gt_input.
  ENDLOOP.


  LOOP AT gt_input INTO gw_input.
    gw_output-column1 = gw_input-column1.
    gw_output-column2 = gw_input-column2.
    gw_output-column3 = gw_input-column3.
    gw_output-column4 = gw_output-column2 * gw_output-column3.
    CONDENSE  gw_output-column4.
    lv_sum = lv_sum + gw_output-column4.
    APPEND gw_output TO gt_output.
  ENDLOOP.
  CLEAR: gw_output.
  lv_label = 'Total'.
  gw_output-column1 = lv_label.
  gw_output-column4 = lv_sum.
  CONDENSE  gw_output-column4.

  APPEND gw_output TO gt_output.

*----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*----------------------------------------------------------------------
END-OF-SELECTION.
  PERFORM download_to_sap .
  PERFORM download_to_pc .
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
*&      Form  PF_GET_DATA
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  PERFORM_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_from_sap.
  DATA gw_file LIKE LINE OF gt_file.

  DATA: lv_filepath  TYPE string,
        lv_filepath2 TYPE rlgrap-filename.

  lv_filepath2 = '\usr\sap\temp\Payment_JO.txt'.
  lv_filepath = lv_filepath2.
  OPEN DATASET lv_filepath FOR INPUT IN TEXT MODE
                            ENCODING UTF-8.
  WHILE sy-subrc = 0.
    CLEAR:gw_file.
    READ DATASET lv_filepath INTO gw_file.
    IF gw_file IS NOT INITIAL.
      APPEND gw_file TO gt_file.
    ENDIF.
  ENDWHILE.

  CLOSE DATASET lv_filepath.
ENDFORM.

FORM download_to_sap .
  DATA: lw_output LIKE LINE OF gt_output.
  DATA: lv_filepath1 TYPE rlgrap-filename,
*        lv_filepath2 TYPE rlgrap-filename,
*        lv_filepath3 TYPE rlgrap-filename,
        lv_filepath4 TYPE rlgrap-filename,
        lv_string    TYPE c LENGTH 200.

  lv_filepath1 = '\usr\sap\temp\Payment_jo'.
*  lv_filepath2 = sy-datum.
*  lv_filepath3 = sy-uzeit.
  lv_filepath4 = '.txt'.
  CONCATENATE lv_filepath1 sy-datum sy-uzeit lv_filepath4 INTO lv_filepath1.

  lv_filepath = lv_filepath1.

  OPEN DATASET lv_filepath FOR OUTPUT IN TEXT MODE
                            ENCODING UTF-8.
  IF sy-subrc = 0.
    CLEAR:lw_output.
    LOOP AT gt_output INTO gw_output.
      WRITE gw_output TO lv_string.
      TRANSFER lv_string TO lv_filepath.
    ENDLOOP.
  ENDIF.
  IF sy-subrc = 0.

  ENDIF.
  CLOSE DATASET lv_filepath.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PERFORM_UPLOAD_TO_PC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_to_pc .
  DATA : lv_codepage  TYPE cpcodepage,
         lv_codepage1 TYPE abap_encoding,
         lv_filename  TYPE string,
         lv_filepath1 TYPE string,
*         lv_filepath2 TYPE string,
*         lv_filepath3 TYPE string,
         lv_filepath4 TYPE string.

  lv_filepath1 = '\usr\sap\temp\Payment_jo'.
*  lv_filepath2 = sy-datum.
*  lv_filepath3 = sy-uzeit.
  lv_filepath4 = '.txt'.
  CONCATENATE lv_filepath1 sy-datum sy-uzeit lv_filepath4 INTO lv_filepath1.


  CALL FUNCTION 'SCP_CODEPAGE_BY_EXTERNAL_NAME'
    EXPORTING
      external_name = 'UTF-8'
    IMPORTING
      sap_codepage  = lv_codepage
    EXCEPTIONS
      not_found     = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  lv_codepage1 = lv_codepage.
  lv_filename = lv_filepath1.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
*     BIN_FILESIZE            =
      filename                = lv_filename
      filetype                = 'ASC'
      append                  = ' '
      write_field_separator   = 'X'
*     HEADER                  = '00'
*     TRUNC_TRAILING_BLANKS   = ' '
*     WRITE_LF                = 'X'
*     COL_SELECT              = ' '
*     COL_SELECT_MASK         = ' '
*     DAT_MODE                = ' '
*     CONFIRM_OVERWRITE       = ' '
*     NO_AUTH_CHECK           = ' '
      codepage                = lv_codepage1
*     IGNORE_CERR             = ABAP_TRUE
*     REPLACEMENT             = '#'
*     WRITE_BOM               = ' '
*     TRUNC_TRAILING_BLANKS_EOL       = 'X'
*     WK1_N_FORMAT            = ' '
*     WK1_N_SIZE              = ' '
*     WK1_T_FORMAT            = ' '
*     WK1_T_SIZE              = ' '
*     WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*     SHOW_TRANSFER_STATUS    = ABAP_TRUE
*     VIRUS_SCAN_PROFILE      = '/SCET/GUI_DOWNLOAD'
* IMPORTING
*     FILELENGTH              =
    TABLES
      data_tab                = gt_output
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
  IF sy-subrc = 0.
    WRITE: /'success2'.
  ENDIF.







ENDFORM.
