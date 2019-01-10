*----------------------------------------------------------------------
* Program ID       :
* Date(dd.mm.yyyy) : 6/9/2017
* Author           : Korntana Jirakhunthaworn
* Description      : Exam 2
*----------------------------------------------------------------------
*  CHANGE HISTORY
*----------------------------------------------------------------------
* Change By   :
* Change Date :
* Search Team : CH01
* Description :
*----------------------------------------------------------------------
REPORT zexam1_02_jo.
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

*----------------------------------------------------------------------
* T Y P E S
*----------------------------------------------------------------------
TYPES: BEGIN OF ty_input,
         column1 TYPE c LENGTH 8,
         column2 TYPE c LENGTH 40,
         column3 TYPE c LENGTH 40,
         column4 TYPE c LENGTH 8,
         column5 TYPE c LENGTH 1,
         column6 TYPE c LENGTH 3,
       END OF ty_input.


*----------------------------------------------------------------------
* C O N S T A N T S
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* D A T A
*----------------------------------------------------------------------
*-> Object
DATA: gt_input  TYPE TABLE OF ty_input,
      gt_output TYPE TABLE OF ztexam1_02_jo,
      gt_input2 TYPE TABLE OF ty_input.
*-> Internal tables

*-> Work areas
DATA: gw_input  LIKE LINE OF gt_input,
      gw_output LIKE LINE OF gt_output.
*-> Ranges

*-> Variables
DATA: gv_file_path TYPE ibipparms-path.
*-> Field-symbols
FIELD-SYMBOLS : <fs_input>  LIKE LINE OF gt_input,
                <fs_output> LIKE LINE OF gt_output.

*----------------------------------------------------------------------
* M A C R O S
*----------------------------------------------------------------------
*----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*----------------------------------------------------------------------
PARAMETERS: p_input TYPE c LENGTH 200 DEFAULT 'C:\Users\kjirakhunthaworn\Desktop\exam\Exam 2 (2).txt'.
*----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*----------------------------------------------------------------------
INITIALIZATION.

*----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_input.
  CLEAR gv_file_path.
  PERFORM pf_browse_locl_folder.
  MOVE gv_file_path TO p_input.
*----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM read_from_pc.
  PERFORM upload_to_table.

  WRITE: 'success'.
*----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*----------------------------------------------------------------------
END-OF-SELECTION.

*----------------------------------------------------------------------
* T O P - O F – P A G E
*----------------------------------------------------------------------


*----------------------------------------------------------------------
* A T   L I N E - S E L E C T I O N
*----------------------------------------------------------------------


*----------------------------------------------------------------------
* F O R M S
*----------------------------------------------------------------------
FORM read_from_pc.
  DATA: lv_codepage1 TYPE cpcodepage,
        lv_codepage2 TYPE abap_encoding,
        lv_filename  TYPE string.
  .
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

  lv_codepage2 = lv_codepage1.
  lv_filename = p_input.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_filename
      filetype                = 'ASC'
      has_field_separator     = 'X'
      codepage                = lv_codepage2
    TABLES
      data_tab                = gt_input
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_TO_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_to_table .
  DATA: lv_string TYPE string,
        lv_num1   TYPE n LENGTH 2,
        lv_num2   TYPE n LENGTH 2,
        lv_num3   TYPE n LENGTH 4,
        lv_c1     TYPE c LENGTH 2,
        lv_c2     TYPE c LENGTH 2,
        lv_c3     TYPE c LENGTH 4.


  IF gt_input[] IS INITIAL.
    EXIT.
  ENDIF.
  LOOP AT gt_input2 ASSIGNING <fs_input>.
    IF sy-tabix > 1.
      CLEAR: gw_output, lv_num1 ,lv_num2 ,lv_num3 ,lv_c1 ,lv_c2 ,lv_c3 .
      gw_output-mandt = sy-mandt.
      gw_output-pernr = <fs_input>-column1.
      gw_output-first_name = <fs_input>-column2.
      gw_output-last_name = <fs_input>-column3.

      SPLIT <fs_input>-column4 AT '.' INTO lv_c1 lv_c2 lv_c3.
      lv_num1 = lv_c1.
      lv_num2 = lv_c2.
      lv_num3 = lv_c3.
      CONCATENATE  lv_num3 lv_num2 lv_num1 INTO lv_string.

      gw_output-birth_date = lv_string.
      gw_output-gender = <fs_input>-column5.
      gw_output-nation = <fs_input>-column6.
      APPEND gw_output TO gt_output.
    ENDIF.
  ENDLOOP.
  IF sy-subrc <> 0.

  ENDIF.
  MODIFY ztexam1_02_jo FROM TABLE gt_output.

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
