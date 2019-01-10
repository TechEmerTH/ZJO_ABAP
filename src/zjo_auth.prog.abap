*----------------------------------------------------------------------
* Program ID       : ZJO_AUTH
* Date(dd.mm.yyyy) : 08/03/2018
* Author           : Korntana J.
* Description      : authorization
*----------------------------------------------------------------------
*  CHANGE HISTORY
*----------------------------------------------------------------------
* Change By   :
* Change Date :
* Search Team : CH01
* Description :
*----------------------------------------------------------------------
REPORT zjo_auth.
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

TYPES: BEGIN OF typ_output_tcode,
         running_no    TYPE i,
         username      TYPE xubname,
         role_name     TYPE agr_name,
         function_name TYPE tcode,
         desc          TYPE ttext_stct,
       END OF typ_output_tcode.

TYPES: BEGIN OF typ_output_auth,
         running_no   TYPE i,
         username     TYPE xubname,
         role_name    TYPE agr_name,
         profile_name TYPE agauth,
         field_name   TYPE c LENGTH 30,
         value        TYPE c LENGTH 55,
         desc         TYPE intxt,
       END OF typ_output_auth.

*----------------------------------------------------------------------
* C O N S T A N T S
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* D A T A
*----------------------------------------------------------------------
*-> Object
*DATA : GO_P0001 TYPE REF TO CL_ABAP_TYPEDESCR.

*-> Internal tables
DATA : gt_agr_users    TYPE TABLE OF agr_users,
       gt_agr_1251     TYPE TABLE OF agr_1251,
       gt_output_tcode TYPE TABLE OF typ_output_tcode,
       gt_output_auth  TYPE TABLE OF typ_output_auth,
       gt_tstct        TYPE TABLE OF tstct,
       gt_t582s        TYPE STANDARD TABLE OF t582s,
       gt_t500p        TYPE STANDARD TABLE OF t500p,
       gt_t501t        TYPE STANDARD TABLE OF t501t,
       gt_t503t        TYPE STANDARD TABLE OF t503t,
       gt_t527o        TYPE STANDARD TABLE OF t527o,
       gt_xml          TYPE STANDARD TABLE OF string.

*-> Work areas
DATA : gw_agr_users    LIKE LINE OF gt_agr_users,
       gw_agr_1251     LIKE LINE OF gt_agr_1251,
       gw_output_tcode LIKE LINE OF gt_output_tcode,
       gw_output_auth  LIKE LINE OF gt_output_auth,
       gw_tstct        LIKE LINE OF gt_tstct,
       gw_t500p        LIKE LINE OF gt_t500p,
       gw_t501t        LIKE LINE OF gt_t501t,
       gw_t503t        LIKE LINE OF gt_t503t,
       gw_t527o        LIKE LINE OF gt_t527o,
       gw_t582s        LIKE LINE OF gt_t582s.

DATA: directory  TYPE dirname.
*-> Ranges
*DATA : GR_P0001 TYPE RANGE OF P0001-PERNR.

*-> Variables
DATA : gv_count TYPE i.

*-> Field-symbols
FIELD-SYMBOLS : <fs_xml> LIKE LINE OF gt_xml.

*----------------------------------------------------------------------
* M A C R O S
*----------------------------------------------------------------------
DEFINE mc_concat_xml.
  APPEND INITIAL LINE TO gt_xml ASSIGNING <fs_xml>.
  <fs_xml> = &1.
END-OF-DEFINITION.

*----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*----------------------------------------------------------------------

SELECT-OPTIONS: s_user FOR gw_agr_users-uname NO INTERVALS.
SELECT-OPTIONS: s_func FOR gw_agr_1251-low NO INTERVALS.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE  TEXT-001.
PARAMETERS: r_1 RADIOBUTTON GROUP r1 USER-COMMAND ucom DEFAULT 'X'.
PARAMETERS: r_2 RADIOBUTTON GROUP r1 .
SELECTION-SCREEN END OF BLOCK b1.


*SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME TITLE text-t01.
"PC
*
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS: r_rad1 RADIOBUTTON GROUP r2 DEFAULT 'X' USER-COMMAND ucom2.
*SELECTION-SCREEN COMMENT 4(5) FOR FIELD p_pc1.
PARAMETERS: p_pc1 TYPE rlgrap-filename DEFAULT 'c:\temp\SAP Authorize Report_****_YYYYMMDD.xls'.
*SELECTION-SCREEN END OF LINE.

*"APPLICATION SERVER
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS: r_rad2 RADIOBUTTON GROUP r2.
*SELECTION-SCREEN COMMENT 4(5) FOR FIELD p_sv1.
*PARAMETERS: p_sv1 TYPE rlgrap-filename .
*SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN END OF BLOCK s1.

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
* *AT SELECTION-SCREEN .

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pc1 .
  PERFORM pf_open_file_name_pc.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_sv1 .
*  PERFORM pf_open_file_name_sv.

AT SELECTION-SCREEN OUTPUT.
  PERFORM pf_set_screen_for_input.
*----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM pf_gettext.
  IF r_1 = abap_true.
    PERFORM pf_get_user_tcode.
    PERFORM pf_gen_excel_tcode.
    PERFORM pf_alv_tcode.
  ELSE.
    PERFORM pf_get_user_auth.
    PERFORM pf_gen_excel_auth.
    PERFORM pf_alv_auth.
  ENDIF.
*----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*----------------------------------------------------------------------
END-OF-SELECTION.

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
*&      Form  PF_GET_USER_TCODE
*&---------------------------------------------------------------------*
FORM pf_get_user_tcode .

  DATA: lr_function TYPE RANGE OF tcode,
        lw_function LIKE LINE OF lr_function.

  FIELD-SYMBOLS: <fs_output> LIKE LINE OF gt_output_tcode.

  IF gt_agr_users IS NOT INITIAL.

*    IF s_func IS NOT INITIAL.
*      SELECT *
*        FROM agr_1251
*        INTO TABLE gt_agr_1251
*        FOR ALL ENTRIES IN gt_agr_users
*        WHERE agr_name = gt_agr_users-agr_name
*        AND object = '*TCODE'
**        AND low in s_func
*      AND field = 'TCD'.
*    ELSE.
      SELECT *
      FROM agr_1251
      INTO TABLE gt_agr_1251
      FOR ALL ENTRIES IN gt_agr_users
      WHERE agr_name = gt_agr_users-agr_name
      AND field = 'TCD'
      AND object LIKE '%TCODE'
        and low in s_func.

*    ENDIF.
  ENDIF.
  DELETE gt_agr_1251 WHERE low IS INITIAL.

  LOOP AT gt_agr_1251 INTO gw_agr_1251.
    CLEAR: gw_output_tcode,gw_agr_users.
    gw_output_tcode-role_name = gw_agr_1251-agr_name.

    IF gw_agr_1251-high IS NOT INITIAL.
      CLEAR: lr_function.
      lw_function-low = gw_agr_1251-low.
      lw_function-high = gw_agr_1251-high.
      lw_function-sign = 'I'.
      lw_function-option = 'BT'.
      APPEND lw_function TO lr_function.
      LOOP AT gt_tstct INTO gw_tstct WHERE tcode IN lr_function.

        gw_output_tcode-function_name = gw_tstct-tcode.
        gw_output_tcode-desc = gw_tstct-ttext.

        LOOP AT gt_agr_users INTO gw_agr_users WHERE agr_name = gw_agr_1251-agr_name.
          gw_output_tcode-username = gw_agr_users-uname.
          APPEND gw_output_tcode TO gt_output_tcode.
        ENDLOOP.
      ENDLOOP.
    ELSE.
      gw_output_tcode-function_name = gw_agr_1251-low.
      READ TABLE gt_tstct INTO gw_tstct WITH KEY tcode = gw_agr_1251-low.
      IF sy-subrc = 0.
        gw_output_tcode-desc = gw_tstct-ttext.
      ENDIF.
      LOOP AT gt_agr_users INTO gw_agr_users WHERE agr_name = gw_agr_1251-agr_name.
        gw_output_tcode-username = gw_agr_users-uname.
        APPEND gw_output_tcode TO gt_output_tcode.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  SORT gt_output_tcode BY username role_name function_name desc ASCENDING.
  DELETE ADJACENT DUPLICATES FROM gt_output_tcode COMPARING username role_name function_name desc .
  gv_count = 1.
  LOOP AT gt_output_tcode ASSIGNING <fs_output>.
    <fs_output>-running_no = gv_count.
    gv_count = gv_count + 1 .
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_GET_USER_AUTH
*&---------------------------------------------------------------------*
FORM pf_get_user_auth .

  DATA: lr_infty    TYPE RANGE OF infty,
        lr_function TYPE RANGE OF tcode,
        lr_persk    TYPE RANGE OF persk,
        lr_persg    TYPE RANGE OF persg,
        lr_werk     TYPE RANGE OF persa.

  DATA: lw_function LIKE LINE OF lr_function,
        lw_infty    LIKE LINE OF lr_infty,
        lw_persk    LIKE LINE OF lr_persk,
        lw_persg    LIKE LINE OF lr_persg,
        lw_werk     LIKE LINE OF lr_werk.

  FIELD-SYMBOLS: <fs_output> LIKE LINE OF gt_output_auth.
  IF gt_agr_users IS NOT INITIAL.
    SELECT *
      FROM t582s
      INTO TABLE gt_t582s
    WHERE sprsl = sy-langu.
    SELECT *
      FROM t500p
      INTO TABLE gt_t500p.
    SELECT *
      FROM t501t
      INTO TABLE gt_t501t
      WHERE sprsl = sy-langu.
    SELECT *
      FROM t503t
      INTO TABLE gt_t503t
      WHERE sprsl = sy-langu.

    SELECT *
      FROM agr_1251
      INTO TABLE gt_agr_1251
      FOR ALL ENTRIES IN gt_agr_users
      WHERE agr_name = gt_agr_users-agr_name
      AND object = 'P_ORGIN'
      AND field = 'INFTY'
      OR  field = 'AUTHC'

      OR field = 'PERSA'

      OR field = 'PERSG'

      OR  field = 'PERSK'

      OR  field = 'SUBTY'

      OR  field = 'VDSK1'.
  ENDIF.
  DELETE gt_agr_1251 WHERE low IS INITIAL.


  LOOP AT gt_agr_1251 INTO gw_agr_1251.
    CLEAR: gw_output_auth,gw_t582s.

    gw_output_auth-role_name = gw_agr_1251-agr_name.
    gw_output_auth-profile_name = gw_agr_1251-auth.

    IF gw_agr_1251-field = 'INFTY'.
      gw_output_auth-field_name = 'Infotype'.
    ELSEIF gw_agr_1251-field = 'SUBTY'.
      gw_output_auth-field_name = 'Subtype'.
    ELSEIF gw_agr_1251-field = 'AUTHC'.
      gw_output_auth-field_name = 'Authorization level'.
    ELSEIF gw_agr_1251-field = 'PERSA'.
      gw_output_auth-field_name = 'Personnel Area'.
    ELSEIF gw_agr_1251-field = 'PERSG'.
      gw_output_auth-field_name = 'Employee Group'.
    ELSEIF gw_agr_1251-field = 'PERSK'.
      gw_output_auth-field_name = 'Employee Subgroup'.
    ELSEIF gw_agr_1251-field = 'VDSK1'.
      gw_output_auth-field_name = 'Organizational Key'.
    ENDIF.

    IF gw_agr_1251-high IS NOT INITIAL.

      IF gw_agr_1251-low = '*'.
        gw_output_auth-desc = TEXT-002.
      ELSE.
        IF gw_output_auth-field_name = 'Infotype'.
          CLEAR: lr_infty.
          lw_infty-low = gw_agr_1251-low.
          lw_infty-high = gw_agr_1251-high.
          lw_infty-sign = 'I'.
          lw_infty-option = 'BT'.
          APPEND lw_infty TO lr_infty.
          LOOP AT gt_t582s INTO gw_t582s WHERE infty IN lr_infty.

            gw_output_auth-value = gw_t582s-infty.
            gw_output_auth-desc = gw_t582s-itext.

            LOOP AT gt_agr_users INTO gw_agr_users WHERE agr_name = gw_agr_1251-agr_name.
              gw_output_auth-username = gw_agr_users-uname.
              APPEND gw_output_auth TO gt_output_auth.
            ENDLOOP.
          ENDLOOP.
        ELSEIF gw_output_auth-field_name = 'Subtype'.
          CLEAR: lr_function.
          lw_function-low = gw_agr_1251-low.
          lw_function-high = gw_agr_1251-high.
          lw_function-sign = 'I'.
          lw_function-option = 'BT'.
          APPEND lw_function TO lr_function.
          LOOP AT gt_tstct INTO gw_tstct WHERE tcode IN lr_function.

            gw_output_auth-value = gw_tstct-tcode.
            gw_output_auth-desc = gw_tstct-ttext.

            LOOP AT gt_agr_users INTO gw_agr_users WHERE agr_name = gw_agr_1251-agr_name.
              gw_output_auth-username = gw_agr_users-uname.
              APPEND gw_output_auth TO gt_output_auth.
            ENDLOOP.
          ENDLOOP.
        ELSEIF gw_output_auth-field_name = 'Personnel Area'.
          CLEAR: lr_werk.
          lw_werk-low = gw_agr_1251-low.
          lw_werk-high = gw_agr_1251-high.
          lw_werk-sign = 'I'.
          lw_werk-option = 'BT'.
          APPEND lw_werk TO lr_werk.
          LOOP AT gt_t500p INTO gw_t500p WHERE persa IN lr_function.

            gw_output_auth-value = gw_tstct-tcode.
            gw_output_auth-desc = gw_tstct-ttext.

            LOOP AT gt_agr_users INTO gw_agr_users WHERE agr_name = gw_agr_1251-agr_name.
              gw_output_auth-username = gw_agr_users-uname.
              APPEND gw_output_auth TO gt_output_auth.
            ENDLOOP.
          ENDLOOP.
        ELSEIF gw_output_auth-field_name = 'Employee Group'.
          CLEAR: lr_persg.
          lw_persg-low = gw_agr_1251-low.
          lw_persg-high = gw_agr_1251-high.
          lw_persg-sign = 'I'.
          lw_persg-option = 'BT'.
          APPEND lw_persg TO lr_persg.
          LOOP AT gt_t501t INTO gw_t501t WHERE persg IN lr_persg.

            gw_output_auth-value = gw_t501t-persg.
            gw_output_auth-desc = gw_t501t-ptext.

            LOOP AT gt_agr_users INTO gw_agr_users WHERE agr_name = gw_agr_1251-agr_name.
              gw_output_auth-username = gw_agr_users-uname.
              APPEND gw_output_auth TO gt_output_auth.
            ENDLOOP.
          ENDLOOP.
        ELSEIF gw_output_auth-field_name = 'Employee Subgroup'.
          CLEAR: lr_persk.
          lw_persk-low = gw_agr_1251-low.
          lw_persk-high = gw_agr_1251-high.
          lw_persk-sign = 'I'.
          lw_persk-option = 'BT'.
          APPEND lw_persk TO lr_persk.
          LOOP AT gt_t503t INTO gw_t503t WHERE persk IN lr_persk.

            gw_output_auth-value = gw_t503t-persk.
            gw_output_auth-desc = gw_t503t-ptext.

            LOOP AT gt_agr_users INTO gw_agr_users WHERE agr_name = gw_agr_1251-agr_name.
              gw_output_auth-username = gw_agr_users-uname.
              APPEND gw_output_auth TO gt_output_auth.
            ENDLOOP.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ELSE.

      gw_output_auth-value = gw_agr_1251-low.
      gw_output_auth-running_no = gv_count.
      IF gw_output_auth-field_name = 'Authorization level'.
        IF gw_output_auth-value = 'R'.
          gw_output_auth-value = 'read'.
        ELSEIF gw_output_auth-value = 'W'.
          gw_output_auth-value = 'maintain data'.
        ELSEIF gw_output_auth-value = 'A'.
          gw_output_auth-value = 'release'.
        ELSEIF gw_output_auth-value = 'B'.
          gw_output_auth-value = 'reserve in Amadeus reservation system'.
        ELSEIF gw_output_auth-value = 'C'.
          gw_output_auth-value = 'reserve released trips in Amadeus reservation system'.
        ELSEIF gw_output_auth-value = 'Q'.
          gw_output_auth-value = 'create trip template'.
        ELSEIF gw_output_auth-value = '*'.
          gw_output_auth-value = 'all operations'.
        ENDIF.
      ENDIF.

      IF gw_output_auth-value = '*'.
        gw_output_auth-desc = TEXT-002.
      ELSE.
        IF gw_output_auth-field_name = 'SUBTY'.
          READ TABLE gt_tstct INTO gw_tstct WITH KEY tcode = gw_agr_1251-low.
          IF sy-subrc = 0.
            gw_output_auth-desc = gw_tstct-ttext.
          ENDIF.
        ELSEIF gw_output_auth-field_name = 'INFTY'.
          READ TABLE gt_t582s INTO gw_t582s WITH KEY infty = gw_agr_1251-low.
          IF sy-subrc = 0.
            gw_output_auth-desc = gw_t582s-itext.
          ENDIF.
        ELSEIF gw_output_auth-field_name = 'Personnel Area'.
          READ TABLE gt_t500p INTO gw_t500p WITH KEY persa = gw_agr_1251-low.
          IF sy-subrc = 0.
            gw_output_auth-desc = gw_t582s-itext.
          ENDIF.
        ELSEIF gw_output_auth-field_name = 'Employee Group'.
          READ TABLE gt_t501t INTO gw_t501t WITH KEY persg = gw_agr_1251-low.
          IF sy-subrc = 0.
            gw_output_auth-desc = gw_t582s-itext.
          ENDIF.
        ELSEIF gw_output_auth-field_name = 'Employee Subgroup'.
          READ TABLE gt_t503t INTO gw_t503t WITH KEY persk = gw_agr_1251-low.
          IF sy-subrc = 0.
            gw_output_auth-desc = gw_t582s-itext.
          ENDIF.
        ELSEIF gw_output_auth-field_name = 'Organizational Key'.
          READ TABLE gt_t527o  INTO gw_t527o  WITH KEY orgky = gw_agr_1251-low.
          IF sy-subrc = 0.
            CONCATENATE gw_t527o-text1 gw_t527o-text2 INTO gw_output_auth-desc SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDIF.
      LOOP AT gt_agr_users INTO gw_agr_users WHERE agr_name = gw_agr_1251-agr_name.
        gw_output_auth-username = gw_agr_users-uname.
        APPEND gw_output_auth TO gt_output_auth.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  SORT gt_output_auth BY username role_name field_name value desc ASCENDING.
  DELETE ADJACENT DUPLICATES FROM gt_output_auth COMPARING username role_name field_name value desc .
  gv_count = 1.
  LOOP AT gt_output_auth ASSIGNING <fs_output>.
    <fs_output>-running_no = gv_count.
    gv_count = gv_count + 1 .
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_OPEN_FILE_NAME_PC
*&---------------------------------------------------------------------*
FORM pf_open_file_name_pc.
  DATA: lv_init_dir TYPE string,
        lv_folder   TYPE string.

  lv_init_dir = p_pc1.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title         = 'Browse Output Destination'
      initial_folder       = lv_init_dir
    CHANGING
      selected_folder      = lv_folder
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    lv_folder = 'C:\TEMP'.
  ENDIF.

  p_pc1 = lv_folder.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_ALV_TCODE
*&---------------------------------------------------------------------*
FORM pf_alv_tcode .
  DATA: lt_fieldcat TYPE  slis_t_fieldcat_alv.

  DATA: ls_layout   TYPE  slis_layout_alv,
        ls_fieldcat LIKE LINE OF lt_fieldcat.

  ls_layout-colwidth_optimize = 'X'.
  ls_layout-zebra = 'X'.

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'RUNNING_NO'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c01.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'USERNAME'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c02.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'ROLE_NAME'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c03.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'FUNCTION_NAME'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c04.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'DESC'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c05.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      is_layout     = ls_layout
      it_fieldcat   = lt_fieldcat
    TABLES
      t_outtab      = gt_output_tcode
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_ALV_AUTH
*&---------------------------------------------------------------------*
FORM pf_alv_auth .
  DATA: lt_fieldcat TYPE  slis_t_fieldcat_alv.

  DATA: ls_layout   TYPE  slis_layout_alv,
        ls_fieldcat LIKE LINE OF lt_fieldcat.

  ls_layout-colwidth_optimize = 'X'.
  ls_layout-zebra = 'X'.

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'RUNNING_NO'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c01.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'USERNAME'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c02.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'ROLE_NAME'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c03.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'PROFILE_NAME'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c06.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'FIELD_NAME'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c07.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'VALUE'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c08.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'DESC'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c05.
  APPEND ls_fieldcat TO lt_fieldcat[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      is_layout     = ls_layout
      it_fieldcat   = lt_fieldcat
    TABLES
      t_outtab      = gt_output_auth
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_GETTEXT
*&---------------------------------------------------------------------*
FORM pf_gettext .
  SELECT *
    FROM agr_users
    INTO TABLE gt_agr_users
    WHERE uname IN s_user
    AND from_dat <= sy-datum
  AND to_dat   >= sy-datum.

  SELECT *
    FROM tstct
    INTO TABLE gt_tstct
  WHERE sprsl = sy-langu.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_GEN_EXCEL_TCODE
*&---------------------------------------------------------------------*
FORM pf_gen_excel_tcode .
  DATA: lv_string   TYPE string,
        lv_field    TYPE string,
        lv_filename TYPE string.

  CHECK gt_output_tcode IS NOT INITIAL.
  CLEAR gt_xml.


  mc_concat_xml ' <?xml version="1.0"?>' .
  mc_concat_xml ' <?mso-application progid="Excel.Sheet"?>' .
  mc_concat_xml ' <Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"' .
  mc_concat_xml '  xmlns:o="urn:schemas-microsoft-com:office:office"' .
  mc_concat_xml '  xmlns:x="urn:schemas-microsoft-com:office:excel"' .
  mc_concat_xml '  xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"' .
  mc_concat_xml '  xmlns:html="http://www.w3.org/TR/REC-html40">' .
  mc_concat_xml '  <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">' .
  mc_concat_xml '   <Author>KJIRAKHUNTHAWORN</Author>' .
  mc_concat_xml '   <LastAuthor>KJIRAKHUNTHAWORN</LastAuthor>' .
  mc_concat_xml '   <Created>2018-03-12T05:30:47Z</Created>' .
  mc_concat_xml '   <Version>16.00</Version>' .
  mc_concat_xml '  </DocumentProperties>' .
  mc_concat_xml '  <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">' .
  mc_concat_xml '   <AllowPNG/>' .
  mc_concat_xml '  </OfficeDocumentSettings>' .
  mc_concat_xml '  <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">' .
  mc_concat_xml '   <WindowHeight>8100</WindowHeight>' .
  mc_concat_xml '   <WindowWidth>19530</WindowWidth>' .
  mc_concat_xml '   <WindowTopX>0</WindowTopX>' .
  mc_concat_xml '   <WindowTopY>0</WindowTopY>' .
  mc_concat_xml '   <ProtectStructure>False</ProtectStructure>' .
  mc_concat_xml '   <ProtectWindows>False</ProtectWindows>' .
  mc_concat_xml '  </ExcelWorkbook>' .
  mc_concat_xml '  <Styles>' .
  mc_concat_xml '   <Style ss:ID="Default" ss:Name="Normal">' .
  mc_concat_xml '    <Alignment ss:Vertical="Bottom"/>' .
  mc_concat_xml '    <Borders/>' .
  mc_concat_xml '    <Font ss:FontName="Calibri" x:Family="Swiss" ss:Size="11" ss:Color="#000000"/>' .
  mc_concat_xml '    <Interior/>' .
  mc_concat_xml '    <NumberFormat/>' .
  mc_concat_xml '    <Protection/>' .
  mc_concat_xml '   </Style>' .
  mc_concat_xml '   <Style ss:ID="s62" ss:Name="Normal 2">' .
  mc_concat_xml '    <Alignment ss:Vertical="Bottom"/>' .
  mc_concat_xml '    <Borders/>' .
  mc_concat_xml '    <Font ss:FontName="BrowalliaUPC" x:Family="Swiss" ss:Size="16"' .
  mc_concat_xml '     ss:Color="#000000"/>' .
  mc_concat_xml '    <Interior/>' .
  mc_concat_xml '    <NumberFormat/>' .
  mc_concat_xml '    <Protection/>' .
  mc_concat_xml '   </Style>' .
  mc_concat_xml '   <Style ss:ID="s64" ss:Parent="s62">' .
  mc_concat_xml '    <Alignment ss:Horizontal="Center" ss:Vertical="Top"/>' .
  mc_concat_xml '    <Borders>' .
  mc_concat_xml '     <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>' .
  mc_concat_xml '     <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>' .
  mc_concat_xml '     <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>' .
  mc_concat_xml '     <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>' .
  mc_concat_xml '    </Borders>' .
  mc_concat_xml '    <Font ss:FontName="TH SarabunPSK" x:Family="Swiss" ss:Size="16"' .
  mc_concat_xml '     ss:Color="#000000" ss:Bold="1"/>' .
  mc_concat_xml '    <Interior ss:Color="#E7E6E6" ss:Pattern="Solid"/>' .
  mc_concat_xml '   </Style>' .
  mc_concat_xml '   <Style ss:ID="s66" ss:Parent="s62">' .
  mc_concat_xml '    <Alignment ss:Horizontal="Center" ss:Vertical="Top"/>' .
  mc_concat_xml '    <Borders>' .
  mc_concat_xml '     <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>' .
  mc_concat_xml '     <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>' .
  mc_concat_xml '     <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>' .
  mc_concat_xml '     <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>' .
  mc_concat_xml '    </Borders>' .
  mc_concat_xml '    <Font ss:FontName="TH SarabunPSK" ss:Size="16" ss:Color="#000000"/>' .
  mc_concat_xml '    <Interior/>' .
  mc_concat_xml '   </Style>' .
  mc_concat_xml '  </Styles>' .
  mc_concat_xml '  <Worksheet ss:Name="User-Tcode">' .
  mc_concat_xml '   <Table ss:ExpandedColumnCount="5" ss:ExpandedRowCount="999999" x:FullColumns="1"' .
  mc_concat_xml '    x:FullRows="1" ss:DefaultRowHeight="15">' .
*  mc_concat_xml '    <Column ss:AutoFitWidth="0" ss:Width="46.5"/>' .
  mc_concat_xml '    <Column ss:AutoFitWidth="0" ss:Width="62.5"/>' .
  mc_concat_xml '    <Column ss:AutoFitWidth="0" ss:Width="100.25"/>' .
*  mc_concat_xml '    <Column ss:AutoFitWidth="0" ss:Width="89.25"/>' .
  mc_concat_xml '    <Column ss:AutoFitWidth="0" ss:Width="350.75"/>' .
*  mc_concat_xml '    <Column ss:AutoFitWidth="0" ss:Width="120.75"/>' .
  mc_concat_xml '    <Column ss:AutoFitWidth="0" ss:Width="111.5"/>' .
*  mc_concat_xml '    <Column ss:AutoFitWidth="0" ss:Width="118.5"/>' .
  mc_concat_xml '    <Column ss:AutoFitWidth="0" ss:Width="350"/>' .
*  mc_concat_xml '    <Column ss:AutoFitWidth="0" ss:Width="105"/>' .
  mc_concat_xml '    <Row ss:AutoFitHeight="0" ss:Height="20.25">' .
  mc_concat_xml '     <Cell ss:StyleID="s64"><Data ss:Type="String">ลำดับ</Data></Cell>' .
  mc_concat_xml '     <Cell ss:StyleID="s64"><Data ss:Type="String">ชื่อ User</Data></Cell>' .
  mc_concat_xml '     <Cell ss:StyleID="s64"><Data ss:Type="String">ชื่อ Role</Data></Cell>' .
  mc_concat_xml '     <Cell ss:StyleID="s64"><Data ss:Type="String">ชื่อฟังก์ชัน</Data></Cell>' .
  mc_concat_xml '     <Cell ss:StyleID="s64"><Data ss:Type="String">คำอธิบาย</Data></Cell>' .
  mc_concat_xml '    </Row>' .

  LOOP AT gt_output_tcode INTO gw_output_tcode.
    mc_concat_xml '   <Row ss:Height="20.25">' .

    PERFORM pf_format_text USING gw_output_tcode-running_no   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s66"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.

    PERFORM pf_format_text USING gw_output_tcode-username   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s66"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.

    PERFORM pf_format_text USING gw_output_tcode-role_name   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s66"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.

    PERFORM pf_format_text USING gw_output_tcode-function_name   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s66"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.

    PERFORM pf_format_text USING gw_output_tcode-desc   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s66"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
    mc_concat_xml '   </Row>' .
  ENDLOOP.

*  mc_concat_xml '    <Row ss:AutoFitHeight="0" ss:Height="20.25">' .
*  mc_concat_xml '     <Cell ss:StyleID="s66"><Data ss:Type="Number">1</Data></Cell>' .
*  mc_concat_xml '     <Cell ss:StyleID="s66"><Data ss:Type="String">INFOS1-01</Data></Cell>' .
*  mc_concat_xml '     <Cell ss:StyleID="s66"><Data ss:Type="String">HR_PA_HRIS</Data></Cell>' .
*  mc_concat_xml '     <Cell ss:StyleID="s66"><Data ss:Type="String">PA40</Data></Cell>' .
*  mc_concat_xml '     <Cell ss:StyleID="s66"/>' .
*  mc_concat_xml '    </Row>' .

  mc_concat_xml '   </Table>' .
  mc_concat_xml '   <WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">' .
  mc_concat_xml '    <PageSetup>' .
  mc_concat_xml '     <Header x:Margin="0.3"/>' .
  mc_concat_xml '     <Footer x:Margin="0.3"/>' .
  mc_concat_xml '     <PageMargins x:Bottom="0.75" x:Left="0.7" x:Right="0.7" x:Top="0.75"/>' .
  mc_concat_xml '    </PageSetup>' .
  mc_concat_xml '    <Unsynced/>' .
  mc_concat_xml '    <Print>' .
  mc_concat_xml '     <ValidPrinterInfo/>' .
  mc_concat_xml '     <HorizontalResolution>600</HorizontalResolution>' .
  mc_concat_xml '     <VerticalResolution>600</VerticalResolution>' .
  mc_concat_xml '    </Print>' .
  mc_concat_xml '    <Selected/>' .
  mc_concat_xml '    <Panes>' .
  mc_concat_xml '     <Pane>' .
  mc_concat_xml '      <Number>3</Number>' .
  mc_concat_xml '      <ActiveRow>12</ActiveRow>' .
  mc_concat_xml '      <ActiveCol>3</ActiveCol>' .
  mc_concat_xml '     </Pane>' .
  mc_concat_xml '    </Panes>' .
  mc_concat_xml '    <ProtectObjects>False</ProtectObjects>' .
  mc_concat_xml '    <ProtectScenarios>False</ProtectScenarios>' .
  mc_concat_xml '   </WorksheetOptions>' .
  mc_concat_xml '  </Worksheet>' .
  mc_concat_xml ' </Workbook>' .

  DATA: lv_fname TYPE string.
  lv_filename = p_pc1.
  lv_fname = '\Authorize_tcode'.
  PERFORM pf_gen_excel_file TABLES gt_xml
                            USING lv_filename
                                  abap_true
                                  lv_fname.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_GEN_EXCEL_AUTH
*&---------------------------------------------------------------------*
FORM pf_gen_excel_auth .
  DATA: lv_string   TYPE string,
        lv_field    TYPE string,
        lv_filename TYPE string.

  CHECK gt_output_auth IS NOT INITIAL.
  CLEAR gt_xml.

  mc_concat_xml '<?xml version="1.0"?>' .
  mc_concat_xml '<?mso-application progid="Excel.Sheet"?>' .
  mc_concat_xml '<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"' .
  mc_concat_xml ' xmlns:o="urn:schemas-microsoft-com:office:office"' .
  mc_concat_xml ' xmlns:x="urn:schemas-microsoft-com:office:excel"' .
  mc_concat_xml ' xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"' .
  mc_concat_xml ' xmlns:html="http://www.w3.org/TR/REC-html40">' .
  mc_concat_xml ' <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">' .
  mc_concat_xml '  <Author>KJIRAKHUNTHAWORN</Author>' .
  mc_concat_xml '  <LastAuthor>KJIRAKHUNTHAWORN</LastAuthor>' .
  mc_concat_xml '  <Created>2018-03-14T11:17:40Z</Created>' .
  mc_concat_xml '  <Version>16.00</Version>' .
  mc_concat_xml ' </DocumentProperties>' .
  mc_concat_xml ' <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">' .
  mc_concat_xml '  <AllowPNG/>' .
  mc_concat_xml ' </OfficeDocumentSettings>' .
  mc_concat_xml ' <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">' .
  mc_concat_xml '  <WindowHeight>8115</WindowHeight>' .
  mc_concat_xml '  <WindowWidth>19560</WindowWidth>' .
  mc_concat_xml '  <WindowTopX>0</WindowTopX>' .
  mc_concat_xml '  <WindowTopY>0</WindowTopY>' .
  mc_concat_xml '  <ProtectStructure>False</ProtectStructure>' .
  mc_concat_xml '  <ProtectWindows>False</ProtectWindows>' .
  mc_concat_xml ' </ExcelWorkbook>' .
  mc_concat_xml ' <Styles>' .
  mc_concat_xml '  <Style ss:ID="Default" ss:Name="Normal">' .
  mc_concat_xml '   <Alignment ss:Vertical="Bottom"/>' .
  mc_concat_xml '   <Borders/>' .
  mc_concat_xml '   <Font ss:FontName="Calibri" x:Family="Swiss" ss:Size="11" ss:Color="#000000"/>' .
  mc_concat_xml '   <Interior/>' .
  mc_concat_xml '   <NumberFormat/>' .
  mc_concat_xml '   <Protection/>' .
  mc_concat_xml '  </Style>' .
  mc_concat_xml '  <Style ss:ID="s62" ss:Name="Normal 2">' .
  mc_concat_xml '   <Alignment ss:Vertical="Bottom"/>' .
  mc_concat_xml '   <Borders/>' .
  mc_concat_xml '   <Font ss:FontName="BrowalliaUPC" x:Family="Swiss" ss:Size="16"' .
  mc_concat_xml '    ss:Color="#000000"/>' .
  mc_concat_xml '   <Interior/>' .
  mc_concat_xml '   <NumberFormat/>' .
  mc_concat_xml '   <Protection/>' .
  mc_concat_xml '  </Style>' .
  mc_concat_xml '  <Style ss:ID="s64" ss:Parent="s62">' .
  mc_concat_xml '   <Alignment ss:Horizontal="Center" ss:Vertical="Top"/>' .
  mc_concat_xml '   <Borders>' .
  mc_concat_xml '    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>' .
  mc_concat_xml '    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>' .
  mc_concat_xml '    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>' .
  mc_concat_xml '    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>' .
  mc_concat_xml '   </Borders>' .
  mc_concat_xml '   <Font ss:FontName="TH SarabunPSK" x:Family="Swiss" ss:Size="16"' .
  mc_concat_xml '    ss:Color="#000000" ss:Bold="1"/>' .
  mc_concat_xml '   <Interior ss:Color="#E7E6E6" ss:Pattern="Solid"/>' .
  mc_concat_xml '  </Style>' .
  mc_concat_xml '  <Style ss:ID="s65" ss:Parent="s62">' .
  mc_concat_xml '   <Alignment ss:Horizontal="Center" ss:Vertical="Top"/>' .
  mc_concat_xml '   <Borders>' .
  mc_concat_xml '    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>' .
  mc_concat_xml '    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>' .
  mc_concat_xml '    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>' .
  mc_concat_xml '    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>' .
  mc_concat_xml '   </Borders>' .
  mc_concat_xml '   <Font ss:FontName="TH SarabunPSK" ss:Size="16" ss:Color="#000000"/>' .
  mc_concat_xml '   <Interior/>' .
  mc_concat_xml '  </Style>' .
  mc_concat_xml ' </Styles>' .
  mc_concat_xml ' <Worksheet ss:Name="Sheet1">' .
  mc_concat_xml '  <Table ss:ExpandedColumnCount="7" ss:ExpandedRowCount="9999999" x:FullColumns="1"' .
  mc_concat_xml '   x:FullRows="1" ss:DefaultRowHeight="15">' .
  mc_concat_xml '   <Column ss:Width="46.5"/>' .
  mc_concat_xml '   <Column ss:AutoFitWidth="0" ss:Width="107.25"/>' .
  mc_concat_xml '   <Column ss:AutoFitWidth="0" ss:Width="234.75"/>' .
  mc_concat_xml '   <Column ss:Hidden="1" ss:AutoFitWidth="0" ss:Width="142.5"/>' .
  mc_concat_xml '   <Column ss:AutoFitWidth="0" ss:Width="150"/>' .
  mc_concat_xml '   <Column ss:AutoFitWidth="0" ss:Width="88.5"/>' .
  mc_concat_xml '   <Column ss:AutoFitWidth="0" ss:Width="168"/>' .
  mc_concat_xml '   <Row ss:Height="20.25">' .
  mc_concat_xml '    <Cell ss:StyleID="s64"><Data ss:Type="String">ลำดับ</Data></Cell>' .
  mc_concat_xml '    <Cell ss:StyleID="s64"><Data ss:Type="String">ชื่อ User</Data></Cell>' .
  mc_concat_xml '    <Cell ss:StyleID="s64"><Data ss:Type="String">ชื่อ Role</Data></Cell>' .
  mc_concat_xml '    <Cell ss:StyleID="s64"><Data ss:Type="String">ชื่อ Profile</Data></Cell>' .
  mc_concat_xml '    <Cell ss:StyleID="s64"><Data ss:Type="String">ชื่อ Object</Data></Cell>' .
  mc_concat_xml '    <Cell ss:StyleID="s64"><Data ss:Type="String">Value</Data></Cell>' .
  mc_concat_xml '    <Cell ss:StyleID="s64"><Data ss:Type="String">คำอธิบาย</Data></Cell>' .
  mc_concat_xml '   </Row>' .

  LOOP AT gt_output_auth INTO gw_output_auth.
    mc_concat_xml '       <Row ss:AutoFitHeight="0" ss:Height="20.25">' .

    PERFORM pf_format_text USING gw_output_auth-running_no   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s65"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*mc_concat_xml '     <Cell ss:StyleID="s65"><Data ss:Type="Number">1</Data></Cell>' .
    PERFORM pf_format_text USING gw_output_auth-username   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s65"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*mc_concat_xml '     <Cell ss:StyleID="s65"><Data ss:Type="String">INFOS1-01</Data></Cell>' .
    PERFORM pf_format_text USING gw_output_auth-role_name   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s65"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*mc_concat_xml '     <Cell ss:StyleID="s65"><Data ss:Type="String">HR_PA_HRIS</Data></Cell>' .
    PERFORM pf_format_text USING gw_output_auth-profile_name   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s65"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*mc_concat_xml '     <Cell ss:StyleID="s65"><Data ss:Type="String">T-DV72030300</Data></Cell>' .
    PERFORM pf_format_text USING gw_output_auth-field_name   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s65"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*mc_concat_xml '     <Cell ss:StyleID="s65"><Data ss:Type="String">Infotype</Data></Cell>' .
    PERFORM pf_format_text USING gw_output_auth-value   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s65"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*mc_concat_xml '     <Cell ss:StyleID="s66"><Data ss:Type="Number">0</Data></Cell>' .
    PERFORM pf_format_text USING gw_output_auth-desc   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s65"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*mc_concat_xml '     <Cell ss:StyleID="s65"><Data ss:Type="String">Action</Data></Cell>' .
    mc_concat_xml '    </Row>' .
  ENDLOOP.

  mc_concat_xml '  </Table>' .
  mc_concat_xml '  <WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">' .
  mc_concat_xml '   <PageSetup>' .
  mc_concat_xml '    <Header x:Margin="0.3"/>' .
  mc_concat_xml '    <Footer x:Margin="0.3"/>' .
  mc_concat_xml '    <PageMargins x:Bottom="0.75" x:Left="0.7" x:Right="0.7" x:Top="0.75"/>' .
  mc_concat_xml '   </PageSetup>' .
  mc_concat_xml '   <Unsynced/>' .

  mc_concat_xml '   <Selected/>' .
  mc_concat_xml '   <Panes>' .
  mc_concat_xml '    <Pane>' .
  mc_concat_xml '     <Number>3</Number>' .
  mc_concat_xml '     <ActiveRow>2</ActiveRow>' .
  mc_concat_xml '     <ActiveCol>4</ActiveCol>' .
  mc_concat_xml '    </Pane>' .
  mc_concat_xml '   </Panes>' .
  mc_concat_xml '   <ProtectObjects>False</ProtectObjects>' .
  mc_concat_xml '   <ProtectScenarios>False</ProtectScenarios>' .
  mc_concat_xml '  </WorksheetOptions>' .
  mc_concat_xml ' </Worksheet>' .
  mc_concat_xml '</Workbook>' .

  DATA: lv_fname TYPE string.

  CONCATENATE p_pc1  '\Authorize_auth' '_' sy-datum+6(2) sy-datum+4(2) sy-datum+0(4)   '.xls' INTO lv_filename.
  lv_fname = '\Authorize_auth'.
  PERFORM pf_gen_excel_file TABLES gt_xml
                            USING lv_filename
                                  abap_true
                                  lv_fname.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_SET_SCREEN_FOR_INPUT
*&---------------------------------------------------------------------*
FORM pf_set_screen_for_input .
  LOOP AT SCREEN.
    IF r_1 = abap_true.
      IF screen-name CP '*s_func*'.
        screen-input = 1.
      ENDIF.
    ELSE.
      IF screen-name CP '*s_func*'.
        screen-input = 0.
      ENDIF.
    ENDIF.
*    IF r_rad1 = abap_true.
*      IF screen-name CP '*p_pc1*'.
*        screen-input = 1.
*      ENDIF.
*      IF screen-name CP '*p_sv1*'.
*        screen-input = 0.
*      ENDIF.
*    ELSE.
*      IF screen-name CP '*p_pc1*'.
*        screen-input = 0.
*      ENDIF.
*      IF screen-name CP '*p_sv1*'.
*        screen-input = 1.
*      ENDIF.
*    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_FORMAT_TEXT
*&---------------------------------------------------------------------*
FORM pf_format_text  USING    p_value  " format special character for exel cus exel can read only value on the right
                     CHANGING p_field.
  p_field = p_value.
  REPLACE ALL OCCURRENCES OF '&' IN p_field WITH '&amp;' .
  REPLACE ALL OCCURRENCES OF '<' IN p_field WITH '&lt;' .
  REPLACE ALL OCCURRENCES OF '>' IN p_field WITH '&gt;' .
  REPLACE ALL OCCURRENCES OF '''' IN p_field WITH '&apos;' .
  REPLACE ALL OCCURRENCES OF '" " ' IN p_field WITH '&quot;'.
  CONDENSE p_field.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_GEN_EXCEL_FILE
*&---------------------------------------------------------------------*
FORM pf_gen_excel_file TABLES  it_print     LIKE gt_xml
                        USING  im_file_name TYPE string
                               im_open      TYPE abap_bool
                               im_fname     TYPE string.

  DATA: l_fpath        TYPE string.
  DATA: lv_codepage1  TYPE cpcodepage.
  DATA: lv_codepage2  TYPE abap_encoding.

  DATA: excel     TYPE ole2_object,
        workbook  TYPE ole2_object,
        worksheet TYPE ole2_object,
        row       TYPE ole2_object,
        lv_loop   TYPE abap_bool,
        lv_run_no TYPE i.

  DATA lv_sapworkdir TYPE string.
  DATA lv_tmp_path TYPE string.

  MOVE im_file_name TO l_fpath.
* get internal codepage from external codepage
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
    RETURN.
  ENDIF.

  lv_codepage2 = lv_codepage1.

  CALL METHOD cl_gui_frontend_services=>get_sapgui_workdir
    CHANGING
      sapworkdir            = lv_sapworkdir
    EXCEPTIONS
      get_sapworkdir_failed = 1
      cntl_error            = 2
      error_no_gui          = 3
      not_supported_by_gui  = 4
      OTHERS                = 5.

  CALL METHOD cl_gui_cfw=>update_view
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2
      OTHERS            = 3.

  IF lv_sapworkdir IS INITIAL.
    CALL METHOD cl_gui_frontend_services=>get_desktop_directory
      CHANGING
        desktop_directory    = lv_sapworkdir
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.

    CALL METHOD cl_gui_cfw=>update_view
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2
        OTHERS            = 3.

    IF lv_sapworkdir IS INITIAL.
      lv_sapworkdir = 'C:\TEMP'.
    ENDIF.
  ENDIF.

  CONCATENATE lv_sapworkdir im_fname '_TMP.xls' INTO lv_tmp_path.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_tmp_path
      filetype                = 'ASC'
      write_field_separator   = 'X'
      dat_mode                = 'X'
      codepage                = lv_codepage2
    TABLES
      data_tab                = it_print[]
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
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

  CREATE OBJECT excel 'Excel.Application'.
  IF im_open IS NOT INITIAL.
    SET PROPERTY OF excel 'Visible' = 1.
  ELSE.
    SET PROPERTY OF excel 'Visible' = 0.
  ENDIF.

  CALL METHOD OF excel
      'Workbooks' = workbook.
  CALL METHOD OF workbook
    'Open'
    EXPORTING
      #1 = lv_tmp_path.

  GET PROPERTY OF excel   'ActiveWorkbook' = workbook.
  CALL METHOD OF workbook
    'SAVEAS'
    EXPORTING
      #1 = l_fpath
      #2 = 1.
  IF sy-subrc NE 0.
    GET PROPERTY OF excel
    'ActiveWorkbook' = workbook.
  ENDIF.
  IF im_open IS INITIAL.
    CALL METHOD OF
      workbook 'CLOSE'.
    CALL METHOD OF
      excel 'QUIT'.
  ENDIF.
  FREE OBJECT workbook.
  FREE OBJECT excel.

  DATA : lv_rc TYPE i.
  CALL METHOD cl_gui_frontend_services=>file_delete
    EXPORTING
      filename             = lv_tmp_path
    CHANGING
      rc                   = lv_rc
    EXCEPTIONS
      file_delete_failed   = 1
      cntl_error           = 2
      error_no_gui         = 3
      file_not_found       = 4
      access_denied        = 5
      unknown_error        = 6
      not_supported_by_gui = 7
      wrong_parameter      = 8
      OTHERS               = 9.

ENDFORM.
