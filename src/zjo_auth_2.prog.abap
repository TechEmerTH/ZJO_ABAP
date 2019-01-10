*----------------------------------------------------------------------
* Program ID       : ZHRAUTHREP01
* Date(dd.mm.yyyy) : 08/03/2018
* Author           : Korntana J.
* Description      : authorization
*----------------------------------------------------------------------
*  CHANGE HISTORY
*----------------------------------------------------------------------
* Change By   : Korntana J.
* Change Date : 04/06/2018
* Search Team : CH01
* Description : Fix bugs subtype not divide.
*----------------------------------------------------------------------
REPORT zhrauthrep01.
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
         department    TYPE ad_dprtmnt,
         role_name     TYPE agr_name,
         function_name TYPE tcode,
         desc          TYPE ttext_stct,
       END OF typ_output_tcode.

TYPES: BEGIN OF typ_output_auth,
         running_no   TYPE i,
         username     TYPE xubname,
         department   TYPE ad_dprtmnt,
         role_name    TYPE agr_name,
         profile_name TYPE agauth,
         field_name   TYPE c LENGTH 30,
         value        TYPE c LENGTH 55,
         desc         TYPE intxt,
       END OF typ_output_auth.

*----------------------------------------------------------------------
* C O N S T A N T S
*----------------------------------------------------------------------
CONSTANTS: c_auth_level         TYPE c LENGTH 55 VALUE 'Authorization level',
           c_infty              TYPE c LENGTH 5 VALUE 'INFTY',
           c_authc              TYPE c LENGTH 5 VALUE 'AUTHC',
           c_persa              TYPE c LENGTH 5 VALUE 'PERSA',
           c_persg              TYPE c LENGTH 5 VALUE 'PERSG',
           c_persk              TYPE c LENGTH 5 VALUE 'PERSK',
           c_subty              TYPE c LENGTH 5 VALUE 'SUBTY',
           c_vdsk1              TYPE c LENGTH 5 VALUE 'VDSK1',
           c_infotype           TYPE c LENGTH 55 VALUE 'Infotype',
           c_subtype            TYPE c LENGTH 55 VALUE 'Subtype',
           c_personnel_area     TYPE c LENGTH 55 VALUE 'Personnel Area',
           c_employee_group     TYPE c LENGTH 55 VALUE 'Employee Group',
           c_employee_subgroup  TYPE c LENGTH 55 VALUE 'Employee Subgroup',
           c_organizational_key TYPE c LENGTH 55 VALUE 'Organizational Key',
           c_read               TYPE c LENGTH 55 VALUE 'Read',
           c_read_with          TYPE c LENGTH 55 VALUE 'Read with entry helps',
           c_write              TYPE c LENGTH 100 VALUE 'Write locked record; unlock if the last person to change the record is not the current user',
           c_write_lock         TYPE c LENGTH 100 VALUE 'Write locked record',
           c_change             TYPE c LENGTH 100 VALUE 'Change lock indicator',
           c_write_data         TYPE c LENGTH 100 VALUE 'Write data records',
           c_all                TYPE c LENGTH 100 VALUE 'All operations'.
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
       gt_user_addr    TYPE TABLE OF user_addr,
       gt_t582s        TYPE STANDARD TABLE OF t582s,
       gt_t500p        TYPE STANDARD TABLE OF t500p,
       gt_t501t        TYPE STANDARD TABLE OF t501t,
       gt_t503t        TYPE STANDARD TABLE OF t503t,
       gt_t527o        TYPE STANDARD TABLE OF t527o,
       gt_t512t        TYPE STANDARD TABLE OF t512t,
       gt_xml          TYPE STANDARD TABLE OF string.

*-> Work areas
DATA : gw_agr_users    LIKE LINE OF gt_agr_users,
       gw_agr_1251     LIKE LINE OF gt_agr_1251,
       gw_output_tcode LIKE LINE OF gt_output_tcode,
       gw_output_auth  LIKE LINE OF gt_output_auth,
       gw_tstct        LIKE LINE OF gt_tstct,
       gw_user_addr    LIKE LINE OF gt_user_addr,
       gw_t500p        LIKE LINE OF gt_t500p,
       gw_t501t        LIKE LINE OF gt_t501t,
       gw_t503t        LIKE LINE OF gt_t503t,
       gw_t527o        LIKE LINE OF gt_t527o,
       gw_t582s        LIKE LINE OF gt_t582s,
       gw_t512t        LIKE LINE OF gt_t512t.
*
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
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE  TEXT-003.
SELECT-OPTIONS: s_user FOR gw_agr_users-uname NO INTERVALS.
SELECT-OPTIONS: s_func FOR gw_tstct-tcode NO INTERVALS.

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
PARAMETERS: p_pc1 TYPE rlgrap-filename DEFAULT 'c:\temp\SAP Authorize Report_****_YYYYMMDD.xls' OBLIGATORY.
*SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK a1.
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
    IF gt_output_tcode IS NOT INITIAL.
      PERFORM pf_gen_excel_tcode.
      PERFORM pf_alv_tcode.
    ELSE.
      MESSAGE 'No data was selected' TYPE 'S'.
    ENDIF.
  ELSE.
    PERFORM pf_get_user_auth.
    IF gt_output_auth IS NOT INITIAL.
      PERFORM pf_gen_excel_auth.
      PERFORM pf_alv_auth.
    ELSE.
      MESSAGE 'No data was selected' TYPE 'S'.
    ENDIF.
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

    SELECT *
    FROM agr_1251
    INTO TABLE gt_agr_1251
    FOR ALL ENTRIES IN gt_agr_users
    WHERE agr_name = gt_agr_users-agr_name
    AND field = 'TCD'
    AND object LIKE '%TCODE'
    AND low IN s_func.

    DELETE gt_agr_1251 WHERE low IS INITIAL.

    LOOP AT gt_agr_1251 INTO gw_agr_1251.
      CLEAR: gw_output_tcode,gw_agr_users, gw_tstct, gw_user_addr.
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
            READ TABLE gt_user_addr INTO gw_user_addr WITH KEY bname = gw_output_tcode-username BINARY SEARCH.
            IF sy-subrc = 0.
              gw_output_tcode-department = gw_user_addr-department.
            ENDIF.
            APPEND gw_output_tcode TO gt_output_tcode.
          ENDLOOP.
        ENDLOOP.
      ELSE.
        gw_output_tcode-function_name = gw_agr_1251-low.
        READ TABLE gt_tstct INTO gw_tstct WITH KEY tcode = gw_agr_1251-low BINARY SEARCH.
        IF sy-subrc = 0.
          gw_output_tcode-desc = gw_tstct-ttext.
        ENDIF.
        LOOP AT gt_agr_users INTO gw_agr_users WHERE agr_name = gw_agr_1251-agr_name.
          gw_output_tcode-username = gw_agr_users-uname.
          READ TABLE gt_user_addr INTO gw_user_addr WITH KEY bname = gw_output_tcode-username BINARY SEARCH.
          IF sy-subrc = 0.
            gw_output_tcode-department = gw_user_addr-department.
          ENDIF.
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
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_GET_USER_AUTH
*&---------------------------------------------------------------------*
FORM pf_get_user_auth .

  DATA: lr_infty    TYPE RANGE OF infty,
        lr_function TYPE RANGE OF tcode, "CH01
        lr_lgart    TYPE RANGE OF lgart,
        lr_persk    TYPE RANGE OF persk,
        lr_persg    TYPE RANGE OF persg,
        lr_werk     TYPE RANGE OF persa,
        lr_field    TYPE RANGE OF agrfield.

  DATA: lw_function LIKE LINE OF lr_function,
        lw_infty    LIKE LINE OF lr_infty,
        lw_lgart    LIKE LINE OF lr_lgart,"CH01
        lw_persk    LIKE LINE OF lr_persk,
        lw_persg    LIKE LINE OF lr_persg,
        lw_werk     LIKE LINE OF lr_werk,
        lw_field    LIKE LINE OF lr_field.

  FIELD-SYMBOLS: <fs_output> LIKE LINE OF gt_output_auth.
  IF gt_agr_users IS NOT INITIAL.
    SELECT *
      FROM t582s
      INTO TABLE gt_t582s
    WHERE sprsl = sy-langu.
    SORT gt_t582s BY infty ASCENDING .
    SELECT *
      FROM t500p
      INTO TABLE gt_t500p.
    SORT gt_t500p BY persa ASCENDING .
    SELECT *
      FROM t501t
      INTO TABLE gt_t501t
      WHERE sprsl = sy-langu.
    SORT gt_t501t BY persg ASCENDING .
    SELECT *
      FROM t503t
      INTO TABLE gt_t503t
      WHERE sprsl = sy-langu.
    SORT gt_t503t BY persk ASCENDING .
    SELECT *
     FROM t512t
     INTO TABLE gt_t512t
     WHERE sprsl = sy-langu.
    SORT gt_t512t BY lgart ASCENDING .

    lw_field-low = c_infty.
    lw_field-sign = 'I'.
    lw_field-option = 'EQ'.
    APPEND lw_field TO lr_field.
    lw_field-low = c_authc.
    APPEND lw_field TO lr_field.
    lw_field-low = c_persa.
    APPEND lw_field TO lr_field.
    lw_field-low = c_persg.
    APPEND lw_field TO lr_field.
    lw_field-low = c_persk.
    APPEND lw_field TO lr_field.
    lw_field-low = c_subty.
    APPEND lw_field TO lr_field.
    lw_field-low = c_vdsk1.
    APPEND lw_field TO lr_field.

    SELECT *
      FROM agr_1251
      INTO TABLE gt_agr_1251
      FOR ALL ENTRIES IN gt_agr_users
      WHERE agr_name = gt_agr_users-agr_name
      AND object = 'P_ORGIN'
      AND field IN lr_field.
    DELETE gt_agr_1251 WHERE low IS INITIAL.


    LOOP AT gt_agr_1251 INTO gw_agr_1251.
      CLEAR: gw_output_auth,gw_t582s.

      gw_output_auth-role_name = gw_agr_1251-agr_name.
      gw_output_auth-profile_name = gw_agr_1251-auth.

      IF gw_agr_1251-field = c_infty.
        gw_output_auth-field_name = c_infotype.
      ELSEIF gw_agr_1251-field = c_subty.
        gw_output_auth-field_name = c_subtype.
      ELSEIF gw_agr_1251-field = c_authc.
        gw_output_auth-field_name = c_auth_level.
      ELSEIF gw_agr_1251-field = c_persa.
        gw_output_auth-field_name = c_personnel_area.
      ELSEIF gw_agr_1251-field = c_persg.
        gw_output_auth-field_name = c_employee_group.
      ELSEIF gw_agr_1251-field = c_persk.
        gw_output_auth-field_name = c_employee_subgroup.
      ELSEIF gw_agr_1251-field = c_vdsk1.
        gw_output_auth-field_name = c_organizational_key.
      ENDIF.

      " have high
      IF gw_agr_1251-high IS NOT INITIAL.

        IF gw_agr_1251-low = '*'.
          gw_output_auth-desc = TEXT-002.
        ELSE.
          IF gw_output_auth-field_name = c_infotype.
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
                READ TABLE gt_user_addr INTO gw_user_addr WITH KEY bname = gw_output_auth-username BINARY SEARCH.
                IF sy-subrc = 0.
                  gw_output_auth-department = gw_user_addr-department.
                ENDIF.
                APPEND gw_output_auth TO gt_output_auth.
              ENDLOOP.
            ENDLOOP.
          ELSEIF gw_output_auth-field_name = c_subtype.
*-->CH01 DEL
*            CLEAR: lr_function.
*            lw_function-low = gw_agr_1251-low.
*            lw_function-high = gw_agr_1251-high.
*            lw_function-sign = 'I'.
*            lw_function-option = 'BT'.
*            APPEND lw_function TO lr_function.
*            LOOP AT gt_tstct INTO gw_tstct WHERE tcode IN lr_function.
*<--CH01 DEL
*-->CH01 INS
            CLEAR: lr_lgart.
            lw_lgart-low = gw_agr_1251-low.
            lw_lgart-high = gw_agr_1251-high.
            lw_lgart-sign = 'I'.
            lw_lgart-option = 'BT'.
            APPEND lw_lgart TO lr_lgart.
            LOOP AT gt_t512t INTO gw_t512t WHERE lgart IN lr_lgart.
*<--CH01 INS
              gw_output_auth-value = gw_t512t-lgart.
              gw_output_auth-desc = gw_t512t-lgtxt.

              LOOP AT gt_agr_users INTO gw_agr_users WHERE agr_name = gw_agr_1251-agr_name.
                gw_output_auth-username = gw_agr_users-uname.
                READ TABLE gt_user_addr INTO gw_user_addr WITH KEY bname = gw_output_auth-username BINARY SEARCH.
                IF sy-subrc = 0.
                  gw_output_auth-department = gw_user_addr-department.
                ENDIF.
                APPEND gw_output_auth TO gt_output_auth.
              ENDLOOP.
            ENDLOOP.
          ELSEIF gw_output_auth-field_name = c_personnel_area.
            CLEAR: lr_werk.
            lw_werk-low = gw_agr_1251-low.
            lw_werk-high = gw_agr_1251-high.
            lw_werk-sign = 'I'.
            lw_werk-option = 'BT'.
            APPEND lw_werk TO lr_werk.
            LOOP AT gt_t500p INTO gw_t500p WHERE persa IN lr_werk.

              gw_output_auth-value = gw_t500p-persa.
              gw_output_auth-desc = gw_t500p-pstlz.

              LOOP AT gt_agr_users INTO gw_agr_users WHERE agr_name = gw_agr_1251-agr_name.
                gw_output_auth-username = gw_agr_users-uname.
                READ TABLE gt_user_addr INTO gw_user_addr WITH KEY bname = gw_output_auth-username BINARY SEARCH.
                IF sy-subrc = 0.
                  gw_output_auth-department = gw_user_addr-department.
                ENDIF.
                APPEND gw_output_auth TO gt_output_auth.
              ENDLOOP.
            ENDLOOP.
          ELSEIF gw_output_auth-field_name = c_employee_group.
            CLEAR: lr_persg.
            lw_persg-low = gw_agr_1251-low.
            lw_persg-high = gw_agr_1251-high.
            lw_persg-sign = 'I'.
            lw_persg-option = 'BT'.
            APPEND lw_persg TO lr_persg.
            LOOP AT gt_t501t INTO gw_t501t WHERE persg IN lr_persg.

              gw_output_auth-value = gw_t501t-persg.
              gw_output_auth-desc = gw_t501t-ptext.

              LOOP AT gt_agr_users INTO gw_agr_users WHERE agr_name = gw_agr_1251-agr_name .
                gw_output_auth-username = gw_agr_users-uname.
                READ TABLE gt_user_addr INTO gw_user_addr WITH KEY bname = gw_output_auth-username BINARY SEARCH.
                IF sy-subrc = 0.
                  gw_output_auth-department = gw_user_addr-department.
                ENDIF.
                APPEND gw_output_auth TO gt_output_auth.
              ENDLOOP.
            ENDLOOP.
          ELSEIF gw_output_auth-field_name = c_employee_subgroup.
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
                READ TABLE gt_user_addr INTO gw_user_addr WITH KEY bname = gw_output_auth-username BINARY SEARCH.
                IF sy-subrc = 0.
                  gw_output_auth-department = gw_user_addr-department.
                ENDIF.
                APPEND gw_output_auth TO gt_output_auth.
              ENDLOOP.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ELSE.
        " no high
        gw_output_auth-value = gw_agr_1251-low.
        gw_output_auth-running_no = gv_count.
        IF gw_output_auth-field_name = c_auth_level.
          IF gw_output_auth-value = 'R'.
            gw_output_auth-value = c_read.
          ELSEIF gw_output_auth-value = 'M'.
            gw_output_auth-value = c_read_with.
          ELSEIF gw_output_auth-value = 'S'.
            gw_output_auth-value = c_write.
          ELSEIF gw_output_auth-value = 'E'.
            gw_output_auth-value = c_write_lock.
          ELSEIF gw_output_auth-value = 'D'.
            gw_output_auth-value = c_change.
          ELSEIF gw_output_auth-value = 'W'.
            gw_output_auth-value = c_write_data.
          ELSEIF gw_output_auth-value = '*'.
            gw_output_auth-value = c_all.
          ENDIF.
        ENDIF.

        IF gw_output_auth-value = '*'.
          gw_output_auth-desc = TEXT-002.
        ELSE.
          IF gw_output_auth-field_name = c_subtype.
            READ TABLE gt_tstct INTO gw_tstct WITH KEY tcode = gw_agr_1251-low BINARY SEARCH.
            IF sy-subrc = 0.
              gw_output_auth-desc = gw_tstct-ttext.
            ENDIF.
          ELSEIF gw_output_auth-field_name = c_infotype.
            READ TABLE gt_t582s INTO gw_t582s WITH KEY infty = gw_agr_1251-low BINARY SEARCH.
            IF sy-subrc = 0.
              gw_output_auth-desc = gw_t582s-itext.
            ENDIF.
          ELSEIF gw_output_auth-field_name = c_personnel_area.
            READ TABLE gt_t500p INTO gw_t500p WITH KEY persa = gw_agr_1251-low BINARY SEARCH.
            IF sy-subrc = 0.
              gw_output_auth-desc = gw_t500p-name1.
            ENDIF.
          ELSEIF gw_output_auth-field_name = c_employee_group.
            READ TABLE gt_t501t INTO gw_t501t WITH KEY persg = gw_agr_1251-low BINARY SEARCH.
            IF sy-subrc = 0.
              gw_output_auth-desc = gw_t501t-ptext.
            ENDIF.
          ELSEIF gw_output_auth-field_name = c_employee_subgroup.
            READ TABLE gt_t503t INTO gw_t503t WITH KEY persk = gw_agr_1251-low BINARY SEARCH.
            IF sy-subrc = 0.
              gw_output_auth-desc = gw_t503t-ptext.
            ENDIF.
          ELSEIF gw_output_auth-field_name = c_organizational_key.
            READ TABLE gt_t527o  INTO gw_t527o  WITH KEY orgky = gw_agr_1251-low BINARY SEARCH.
            IF sy-subrc = 0.
              CONCATENATE gw_t527o-text1 gw_t527o-text2 INTO gw_output_auth-desc SEPARATED BY space.
            ENDIF.
          ENDIF.
        ENDIF.
        LOOP AT gt_agr_users INTO gw_agr_users WHERE agr_name = gw_agr_1251-agr_name.
          gw_output_auth-username = gw_agr_users-uname.
          READ TABLE gt_user_addr INTO gw_user_addr WITH KEY bname = gw_output_auth-username BINARY SEARCH.
          IF sy-subrc = 0.
            gw_output_auth-department = gw_user_addr-department.
          ENDIF.
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
  ENDIF.
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
  ls_fieldcat-fieldname = 'DEPARTMENT'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c08.
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
  ls_fieldcat-fieldname = 'DEPARTMENT'.
  ls_fieldcat-lowercase = 'X'.
  ls_fieldcat-seltext_l = TEXT-c08.
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
  SORT gt_tstct BY tcode ASCENDING.

  SELECT *
    FROM user_addr
    INTO TABLE gt_user_addr.


  SORT gt_user_addr BY bname ASCENDING.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_GEN_EXCEL_TCODE
*&---------------------------------------------------------------------*
FORM pf_gen_excel_tcode .
  DATA: lv_filename TYPE string.

  CHECK gt_output_tcode IS NOT INITIAL.
  CLEAR gt_xml.

  PERFORM pf_gen_header_tcode.
  PERFORM pf_gen_body_tcode.
  PERFORM pf_gen_footer_tcode.


  lv_filename = p_pc1.

  PERFORM pf_gen_excel_file TABLES gt_xml
                            USING  lv_filename
                                   abap_true.

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

  PERFORM pf_gen_header_auth.
  PERFORM pf_gen_body_auth.
  PERFORM pf_gen_footer_auth.

  lv_filename = p_pc1.

  PERFORM pf_gen_excel_file TABLES gt_xml
                            USING  lv_filename
                                   abap_true.

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
FORM pf_gen_excel_file TABLES  it_print
                         USING   im_file_name
                                 im_open.

  DATA: l_fpath TYPE string.
  DATA: lv_codepage1 TYPE cpcodepage.
  DATA: lv_codepage2 TYPE abap_encoding.

  DATA excel TYPE ole2_object.
  DATA workbook TYPE ole2_object.

  DATA lv_sapworkdir TYPE string.
  DATA lv_tmp_path TYPE string.

  DATA: lv_stripped_name TYPE string.
  DATA: lv_file_path TYPE string.

  DATA: lv_ret TYPE i.
  DATA: lv_wavfile TYPE rlgrap-filename.
  DATA: lv_wavfilepath TYPE rlgrap-filename.

  DATA: lv_fileformat TYPE i.
  DATA: lv_name       TYPE string.
  DATA: lv_extension  TYPE string.

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
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

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
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CALL METHOD cl_gui_cfw=>update_view
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2
        OTHERS            = 3.

    IF lv_sapworkdir IS INITIAL.
      lv_sapworkdir = 'C:\TEMP'.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = l_fpath
    IMPORTING
      stripped_name = lv_stripped_name
      file_path     = lv_file_path
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
  ENDIF.

  " C H A N G E - X L S X - T O - X L S
  CLEAR: lv_name,lv_extension.
  CALL METHOD cl_bcs_utilities=>split_name(
    EXPORTING
      iv_name      = lv_stripped_name
    IMPORTING
      ev_name      = lv_name
      ev_extension = lv_extension
                     ).
  IF lv_extension IS INITIAL.
    MESSAGE TEXT-s01 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  DATA: lv_result TYPE abap_bool.
  DATA: lv_file_path2 TYPE string.
  DATA: lt_tab TYPE TABLE OF string.

  CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name = l_fpath
    IMPORTING
*     STRIPPED_NAME       =
      file_path = lv_file_path2
    EXCEPTIONS
      x_error   = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL METHOD cl_gui_frontend_services=>directory_exist
    EXPORTING
      directory            = lv_file_path2
    RECEIVING
      result               = lv_result
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      OTHERS               = 5.
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

  IF lv_result IS INITIAL.
    CALL METHOD cl_gui_frontend_services=>directory_create
      EXPORTING
        directory                = lv_file_path2
      CHANGING
        rc                       = lv_ret
      EXCEPTIONS
        directory_create_failed  = 1
        cntl_error               = 2
        error_no_gui             = 3
        directory_access_denied  = 4
        directory_already_exists = 5
        path_not_found           = 6
        unknown_error            = 7
        not_supported_by_gui     = 8
        wrong_parameter          = 9
        OTHERS                   = 10.
    IF sy-subrc <> 0.
      MESSAGE TEXT-s02 TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  IF r_1 = 'X'.
    REPLACE ALL OCCURRENCES OF '****' IN lv_name WITH 'TCODE'.
  ELSE.
    REPLACE ALL OCCURRENCES OF '****' IN lv_name WITH 'AUTH'.
  ENDIF.
  REPLACE ALL OCCURRENCES OF 'YYYY' IN lv_name WITH sy-datum+0(4).
  REPLACE ALL OCCURRENCES OF 'MM' IN lv_name WITH sy-datum+4(2).
  REPLACE ALL OCCURRENCES OF 'DD' IN lv_name WITH sy-datum+6(2).
  CONCATENATE  lv_file_path2 lv_name '.XLS' INTO l_fpath.

  CONCATENATE lv_sapworkdir '\' lv_name '_TMP' '.XLS'
    INTO lv_tmp_path.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_tmp_path
      filetype                = 'ASC'
      write_field_separator   = 'X'
      dat_mode                = 'X'
      codepage                = lv_codepage2
*     CONFIRM_OVERWRITE       = ' '
    TABLES
      data_tab                = it_print
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

  ENDIF.

  CREATE OBJECT excel 'Excel.Application'.
  IF im_open IS NOT INITIAL.
    SET PROPERTY OF excel 'Visible' = 1.
  ELSE.
    SET PROPERTY OF excel 'Visible' = 0.
  ENDIF.

  CALL METHOD OF
    excel
      'Workbooks' = workbook.
  CALL METHOD OF
    workbook
    'Open'
    EXPORTING
      #1 = lv_tmp_path.

  IF 1 = 1 .
    lv_fileformat = 51.
  ELSE.
    lv_fileformat = 56.
  ENDIF.

  GET PROPERTY OF excel   'ActiveWorkbook' = workbook.
  CALL METHOD OF
    workbook
    'SAVEAS'
    EXPORTING
      #1 = l_fpath
*     #2 = 1.
      #2 = lv_fileformat.
  IF sy-subrc NE 0.
    GET PROPERTY OF excel   'ActiveWorkbook' = workbook.
  ENDIF.
  IF im_open IS INITIAL.
    CALL METHOD OF
      workbook
      'CLOSE'.
    CALL METHOD OF
      excel
      'QUIT'.
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
  IF sy-subrc <> 0.
  ENDIF.
ENDFORM.
INCLUDE zhrauthrep01_f01.
