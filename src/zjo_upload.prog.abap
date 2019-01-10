*----------------------------------------------------------------------
* Program ID       : ZJO_UPLOAD
* Date(dd.mm.yyyy) : 07.03.2018
* Author           : Korntana J.
* Description      : Upload infotype 2010 from data in al11.
*----------------------------------------------------------------------
*  CHANGE HISTORY
*----------------------------------------------------------------------
* Change By   :
* Change Date :
* Search Team : CH01
* Description :
*----------------------------------------------------------------------
REPORT zjo_upload.
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

TYPES: BEGIN OF ty_input,
         pernr    TYPE c LENGTH 100,
         begda    TYPE c LENGTH 100,
         lgart    TYPE c LENGTH 100,
         anzhl    TYPE c LENGTH 100,
         betrg    TYPE c LENGTH 100,
         waers    TYPE c LENGTH 100,
         filename TYPE c LENGTH 100,
       END OF ty_input.

TYPES: BEGIN OF ty_output,
         icon     TYPE icon-id,
         pernr    TYPE c LENGTH 100,
         begda    TYPE c LENGTH 100,
         lgart    TYPE c LENGTH 100,
         anzhl    TYPE c LENGTH 100,
         betrg    TYPE c LENGTH 100,
         waers    TYPE einhtxt,
         message  TYPE c LENGTH 100,
         filename TYPE c LENGTH 100,
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
DATA : gt_input  TYPE TABLE OF ty_input,
       gt_output TYPE TABLE OF ty_output,
       gt_string TYPE TABLE OF string,
       gt_p2010  TYPE TABLE OF p2010.

*-> Work areas
DATA : gw_input  LIKE LINE OF gt_input,
       gw_output LIKE LINE OF gt_output,
       gw_string LIKE LINE OF gt_string,
       gw_p2010  LIKE LINE OF gt_p2010.

*-> Ranges
*DATA : GR_P0001 TYPE RANGE OF P0001-PERNR.

*-> Variables

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
PARAMETERS: p_path TYPE rlgrap-filename DEFAULT '\usr\sap\temp\'.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X'.

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

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  PERFORM pf_get_file_name_sv CHANGING p_path.
*----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM pf_read_data.
  PERFORM pf_prepare_data.

*----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*----------------------------------------------------------------------
END-OF-SELECTION.

  IF gt_output IS NOT INITIAL.
    PERFORM pf_upload_p2010 USING p_test.
    PERFORM pf_transfer_alv.
    PERFORM pf_show_alv.
  ELSE.
    MESSAGE 'No data was selected' TYPE 'S' .
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
*&      Form  PF_GET_FILE_NAME_SV
*&---------------------------------------------------------------------*
FORM pf_get_file_name_sv CHANGING ev_file TYPE rlgrap-filename.
  DATA lv_svfile TYPE rlgrap-filename.

  CLEAR lv_svfile.
  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    IMPORTING
      serverfile       = lv_svfile
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.
  IF sy-subrc = 0 AND lv_svfile IS NOT INITIAL.
    ev_file = lv_svfile.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_PREPARE_DATA
*&---------------------------------------------------------------------*
FORM pf_prepare_data.
  LOOP AT gt_input INTO gw_input.
    CLEAR: gw_p2010, gw_output.

    MOVE-CORRESPONDING gw_input TO gw_output.
    gw_p2010-pernr = gw_input-pernr.

    PERFORM pf_check_pernr USING gw_p2010-pernr
                        CHANGING gw_output-message.

    PERFORM pf_check_date USING gw_input-begda
                       CHANGING gw_p2010-begda
                                gw_p2010-endda
                                gw_output-message.

    gw_p2010-anzhl = gw_input-anzhl.
    gw_p2010-betrg = gw_input-betrg.
    gw_p2010-waers = gw_input-waers.

    gw_p2010-lgart = gw_input-lgart.
    gw_p2010-infty = '2010'.
    IF gw_output-message IS NOT INITIAL.
      PERFORM pf_set_icon USING yem_wf_cl_constant_ext=>c_msgty-e
                       CHANGING gw_output-icon.
    ENDIF.
    APPEND gw_p2010 TO gt_p2010.
    APPEND gw_output TO gt_output.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_CHECK_PERNR
*&---------------------------------------------------------------------*
FORM pf_check_pernr  USING p_pernr TYPE pernr_d
                    CHANGING cv_message.

  DATA: ls_return TYPE bapireturn.

  CALL FUNCTION 'BAPI_EMPLOYEE_CHECKEXISTENCE'
    EXPORTING
      number = p_pernr
    IMPORTING
      return = ls_return.
  IF ls_return IS NOT INITIAL.
    CONCATENATE ls_return-type ':' ls_return-message INTO cv_message SEPARATED BY space.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_CHECK_DATE
*&---------------------------------------------------------------------*
FORM pf_check_date  USING iv_date_txt TYPE c
                 CHANGING cv_date     TYPE datum
                          cv_date2     TYPE datum
                          cv_message.
  DATA: lv_date  TYPE datum,
        lv_days  TYPE n LENGTH 2,
        lv_month TYPE n LENGTH 2,
        lv_year  TYPE n LENGTH 4,
        lv_1     TYPE c LENGTH 2,
        lv_2     TYPE c LENGTH 2,
        lv_3     TYPE c LENGTH 4.

  CHECK cv_message IS INITIAL.
  " spilte date for 2  digit (ex. 1 -> 01)
  SPLIT iv_date_txt AT '/' INTO lv_1 lv_2 lv_3.
  lv_days = lv_1.
  lv_month = lv_2.
  lv_year = lv_3.
  CONCATENATE lv_year lv_month lv_days INTO lv_date.

  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      date                      = lv_date
    EXCEPTIONS
      plausibility_check_failed = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
    cv_message = TEXT-e01.
    EXIT.
  ENDIF.

  cv_date = lv_date.
  cv_date2 = lv_date.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_SET_ICON
*&---------------------------------------------------------------------*
FORM pf_set_icon  USING p_msgty TYPE sy-msgty
               CHANGING cv_icon TYPE icon-id.
  CASE p_msgty.
    WHEN yem_wf_cl_constant_ext=>c_msgty-e.
      cv_icon = icon_red_light.
    WHEN yem_wf_cl_constant_ext=>c_msgty-w.
      cv_icon = icon_yellow_light.
    WHEN yem_wf_cl_constant_ext=>c_msgty-s.
      cv_icon = icon_green_light.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_UPLOAD_P2010
*&---------------------------------------------------------------------*
FORM pf_upload_p2010  USING p_test.

  DATA: ls_return  TYPE bapireturn1,
        ls_retunn2 TYPE bapiret2.
  FIELD-SYMBOLS <lfs_output> LIKE LINE OF gt_output.

  LOOP AT gt_p2010 INTO gw_p2010.
    READ TABLE gt_output[] ASSIGNING <lfs_output> INDEX sy-tabix.
    CHECK <lfs_output> IS ASSIGNED.

    CHECK <lfs_output>-icon NE icon_red_light.

    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = gw_p2010-pernr
      IMPORTING
        return = ls_return.
    IF ls_return-type = yem_wf_cl_constant_ext=>c_msgty-e.
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
        WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4
        INTO <lfs_output>-message.
      PERFORM pf_set_icon USING ls_return-type CHANGING <lfs_output>-icon.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.
    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty     = gw_p2010-infty
        subtype   = gw_p2010-lgart
        number    = gw_p2010-pernr
        record    = gw_p2010
        operation = 'INS'
        tclas     = 'A'
        nocommit  = 'X'
      IMPORTING
        return    = ls_return.
    IF ls_return-type = yem_wf_cl_constant_ext=>c_msgty-e.
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
        WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4
        INTO <lfs_output>-message.
      PERFORM pf_set_icon USING ls_return-type CHANGING <lfs_output>-icon.
    ENDIF.


    IF p_test IS INITIAL AND <lfs_output>-icon NE icon_red_light.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      PERFORM pf_set_icon USING yem_wf_cl_constant_ext=>c_msgty-s CHANGING <lfs_output>-icon.
      <lfs_output>-message = TEXT-s01.
    ELSEIF p_test IS NOT INITIAL AND <lfs_output>-icon NE icon_red_light.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      PERFORM pf_set_icon USING yem_wf_cl_constant_ext=>c_msgty-w CHANGING <lfs_output>-icon.
      <lfs_output>-message = TEXT-w01.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = gw_p2010-pernr.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_SHOW_ALV
*&---------------------------------------------------------------------*
FORM pf_show_alv .
  DATA: lt_fieldcat TYPE  slis_t_fieldcat_alv.

  DATA: ls_layout   TYPE  slis_layout_alv,
        ls_fieldcat LIKE LINE OF lt_fieldcat.

  ls_layout-colwidth_optimize = 'X'.
  ls_layout-zebra = 'X'.

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'ICON'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c01.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'PERNR'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c02.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'BEGDA'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c03.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'LGART'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c04.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'ANZHL'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c05.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'BETRG'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c08.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'WAERS'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c06.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'MESSAGE'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c07.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'FILENAME'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c09.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      is_layout     = ls_layout
      it_fieldcat   = lt_fieldcat
    TABLES
      t_outtab      = gt_output
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_READ_DATA
*&---------------------------------------------------------------------*
FORM pf_read_data .
  DATA: lv_filepath  TYPE rlgrap-filename,
        lv_path      TYPE salfile-longname,
        lv_count     TYPE i,
        lv_soucefile LIKE sapb-sappfad,
        lv_archive   LIKE sapb-sappfad.

  DATA: lt_path     TYPE STANDARD TABLE OF salfldir.

  DATA: lw_path     LIKE LINE OF lt_path.


  CLEAR: lt_path.
  lv_path = p_path.

* read all file in folder
  CALL FUNCTION 'RZL_READ_DIR_LOCAL'
    EXPORTING
      name               = lv_path
*     FROMLINE           = 0
*     NRLINES            = 1000
    TABLES
      file_tbl           = lt_path
    EXCEPTIONS
      argument_error     = 1
      not_found          = 2
      no_admin_authority = 3
      OTHERS             = 4.
  IF sy-subrc <> 0.
    IF lt_path IS INITIAL.
      MESSAGE 'File not exist' TYPE 'S'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
* sort by name (date)
  SORT lt_path BY name ASCENDING.


  LOOP AT lt_path INTO lw_path.
* Select only pattern and read file
    IF lw_path-name CP 'WFS-PY*'.
      CONCATENATE p_path  lw_path-name INTO lv_filepath.
*      lv_filepath =  lw_filepath.
      CLEAR: lv_count.
      OPEN DATASET lv_filepath FOR INPUT IN TEXT MODE
                                ENCODING DEFAULT.
      WHILE sy-subrc = 0.
        CLEAR:gw_string.
        READ DATASET lv_filepath INTO gw_string.
        IF gw_string IS NOT INITIAL.
          IF lv_count <> 0.
            CONCATENATE gw_string ',' lw_path-name INTO gw_string.
            APPEND gw_string TO gt_string.
          ENDIF.
        ENDIF.
        lv_count = lv_count + 1.
      ENDWHILE.
      CLOSE DATASET lv_filepath.

      lv_soucefile = lv_filepath.
* copy and delete
      "here backup
      CONCATENATE '.\' lw_path-name INTO lv_archive.
      CALL FUNCTION 'ARCHIVFILE_SERVER_TO_SERVER'
        EXPORTING
          sourcepath       = lv_soucefile
          targetpath       = lv_archive
*     IMPORTING
*         LENGTH           =
        EXCEPTIONS
          error_file       = 1
          no_authorization = 2
          OTHERS           = 3.
      IF sy-subrc = 0.
        IF p_test IS INITIAL.
*          DELETE DATASET lv_soucefile.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR: gw_string, gw_input.
  LOOP AT gt_string INTO gw_string.
    SPLIT gw_string AT ',' INTO gw_input-pernr
                                gw_input-begda
                                gw_input-lgart
                                gw_input-anzhl
                                gw_input-betrg
                                gw_input-waers
                                gw_input-filename.
    CONDENSE: gw_input-pernr, gw_input-begda, gw_input-lgart, gw_input-anzhl,gw_input-betrg,gw_input-waers.
    APPEND gw_input TO gt_input.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_TRANSFER_ALV
*&---------------------------------------------------------------------*
FORM pf_transfer_alv .
  DATA: lv_out   TYPE string,
        lv_fname TYPE string.

  "It will store the text file into Application Server Path
*  lv_fname = '\usr\sap\temp\test_alv.txt'.
  CONCATENATE '\usr\sap\temp\' 'WFS-PY' '_RESULT_' sy-datum sy-timlo INTO lv_fname.

  OPEN DATASET lv_fname FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

  "Header in Text file
  CONCATENATE TEXT-c02 TEXT-c03 TEXT-c04 TEXT-c05 TEXT-c08 TEXT-c06 TEXT-c07 TEXT-c09
              INTO lv_out SEPARATED BY ','.
  TRANSFER lv_out TO lv_fname.

  "Data in Text file
  LOOP AT gt_output INTO gw_output.
    CLEAR: lv_out.
    REPLACE ALL OCCURRENCES OF ',' IN gw_output-message WITH space.
    CONCATENATE gw_output-pernr
                gw_output-begda
                gw_output-lgart
                gw_output-anzhl
                gw_output-betrg
                gw_output-waers
                gw_output-message
                gw_output-filename
                INTO lv_out SEPARATED BY ','.
    TRANSFER lv_out TO lv_fname.
  ENDLOOP.
  CLOSE DATASET lv_fname.

ENDFORM.
