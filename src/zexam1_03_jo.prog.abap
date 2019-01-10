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
REPORT zexam1_03_jo.
*----------------------------------------------------------------------
* I N F O T Y P E S
*----------------------------------------------------------------------
NODES : peras.
INFOTYPES: 0001,0002,0006,0105.
" gbdat in pdate
*----------------------------------------------------------------------
* I N C L U D E
*----------------------------------------------------------------------
INCLUDE: yemwfr0000i_main_include.

*----------------------------------------------------------------------
* T A B L E S
*----------------------------------------------------------------------
TABLES: pernr.

*----------------------------------------------------------------------
* T Y P E S
*----------------------------------------------------------------------

*TYPES: BEGIN OF TY_LINE,
*    COLUMN1 TYPE I,
*    COLUMN2 TYPE I,
*    COLUMN3 TYPE I,
* END OF TY_LINE.
*TYPES: TTY_TAB TYPE TABLE OF TY_LINE.
TYPES : BEGIN OF ty_output,
          pernr     TYPE pa0001-pernr,
          ename     TYPE pa0001-ename,
          objid_s   TYPE hrp1000-objid,
          stext_s   TYPE hrp1000-stext,
          objid_o   TYPE hrp1000-objid,
          stext_o   TYPE hrp1000-stext,
          objid_s_b TYPE hrp1000-objid,
          stext_s_b TYPE hrp1000-stext,
          city      TYPE pa0006-ort01,
          cc        TYPE pa0006-land1,
          country   TYPE t005t-landx,
          username  TYPE pa0105-usrid,
          email     TYPE pa0105-usrid_long,
          date      TYPE pa0002-gbdat,
          salary    TYPE pa0008-bet01,
          molga     TYPE t500l-molga,
          sequnce   TYPE pc261-seqnr,
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
DATA: gt_output   TYPE TABLE OF ty_output,
      gt_p0006    TYPE TABLE OF p0006,
      gt_p0002    TYPE TABLE OF p0002,
      gt_p0105    TYPE TABLE OF p0105,
      gt_t005t    TYPE TABLE OF t005t,
      gt_fieldcat TYPE slis_t_fieldcat_alv,
      gt_xml      TYPE STANDARD TABLE OF string,
      gt_objec    TYPE TABLE OF objec,
      gt_objec2   TYPE TABLE OF objec.
*-> Work areas
*DATA : GW_P0001 LIKE LINE OF I_P0001.
DATA: gw_output LIKE LINE OF gt_output.

*-> Ranges
*DATA : GR_P0001 TYPE RANGE OF P0001-PERNR.

*-> Variables
*DATA : GV_PERNR TYPE P0001-PERNR.
DATA: gv_col_pos   TYPE i,
      gv_endda     TYPE sy-datum,
      gv_begda     TYPE sy-datum,
      gv_molga     TYPE t500l-molga,
      gv_file_path TYPE ibipparms-path.

*-> Field-symbols
FIELD-SYMBOLS : <fs_p0006>  LIKE LINE OF gt_p0006,
                <fs_output> LIKE LINE OF gt_output,
                <fs_p0105>  LIKE LINE OF p0105,
                <fs_t0005t> LIKE LINE OF gt_t005t,
                <fs_xml>    LIKE LINE OF gt_xml,
                <fs_objec>  LIKE LINE OF gt_objec,
                <fs_objec2>  LIKE LINE OF gt_objec2.


*----------------------------------------------------------------------
* M A C R O S
*----------------------------------------------------------------------
DEFINE mc_concat_xml.
  APPEND INITIAL LINE TO gt_xml ASSIGNING <fs_xml>.
  <fs_xml> = &1.
END-OF-DEFINITION.
DEFINE mc_append_fieldcat.

  CLEAR: ls_fieldcat.
  gv_col_pos            = gv_col_pos + 1 .
  ls_fieldcat-fieldname = &1 .
  ls_fieldcat-col_pos   = gv_col_pos.
  ls_fieldcat-seltext_s = &2.
  ls_fieldcat-seltext_m = &2.
  ls_fieldcat-seltext_l = &2.
     CASE ls_fieldcat-fieldname .
       WHEN 'PERNR'.
             ls_fieldcat-key = abap_true.
       ENDCASE.
  APPEND ls_fieldcat TO gt_fieldcat.
END-OF-DEFINITION.

*----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*----------------------------------------------------------------------
* PARAMETERS: P_KDATE TYPE SY-DATUM.
* SELECT-OPTIONS: S_DATE FOR SY-DATUM.
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
SELECT-OPTIONS: s_date FOR sy-datum NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK b1.

PARAMETERS: p_exe TYPE c  AS CHECKBOX DEFAULT abap_true.
PARAMETERS: p_output TYPE c LENGTH 200 DEFAULT 'c:\temp\EXAM1_03_jo.xls'.
*----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*----------------------------------------------------------------------
INITIALIZATION.

*----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_output.
  CLEAR gv_file_path.
  PERFORM pf_browse_locl_folder.
  MOVE gv_file_path TO p_output.
*----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM pf_country.

  GET peras.

  rp_provide_from_last p0002 space pn-begda pn-endda .
  IF pnp-sw-found NE 1.
    REJECT .
  ENDIF.

  CHECK p0002-gbdat IN s_date.

  rp_provide_from_last p0001 space pn-begda pn-endda.
  IF pnp-sw-found NE 1.
    REJECT .
  ENDIF.

  CLEAR: gw_output.
  gw_output-date = p0002-gbdat.
  gw_output-pernr = p0001-pernr.
  gw_output-ename = p0001-ename.


  rp_provide_from_last p0006 space pn-begda pn-endda.
*
*  LOOP AT p0006 ASSIGNING <fs_p0006> WHERE begda <= pn-endda
*                                       AND endda >= pn-begda.
*    CASE <fs_p0006>-subty.
*      WHEN '1' .
*        gw_output-city = <fs_p0006>-ort01.
*        gw_output-cc = <fs_p0006>-land1.
*    ENDCASE.
*  ENDLOOP.

  IF gw_output-cc IS NOT INITIAL.
    READ TABLE gt_t005t ASSIGNING <fs_t0005t> WITH KEY land1 = gw_output-cc.
    IF sy-subrc EQ 0.
      gw_output-country = <fs_t0005t>-landx.
    ENDIF.
  ENDIF.

  LOOP AT p0105 ASSIGNING <fs_p0105> WHERE begda <= pn-endda
                                       AND endda >= pn-begda.
    CASE <fs_p0105>-subty.
      WHEN '0001' .
        gw_output-username = <fs_p0105>-usrid.
      WHEN '0010' .
        gw_output-email = <fs_p0105>-usrid_long.
    ENDCASE.
  ENDLOOP.

  APPEND gw_output TO gt_output.
*----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*----------------------------------------------------------------------
END-OF-SELECTION.

  PERFORM pf_prepare_om_data.
  PERFORM pf_prepare_py_data.
  PERFORM pf_alv.
  IF p_exe = abap_true.
    PERFORM gen_xml.
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

  mc_append_fieldcat 'PERNR'     TEXT-001 .
  mc_append_fieldcat 'ENAME'     TEXT-002  .
  mc_append_fieldcat 'CITY'      TEXT-003 .
  mc_append_fieldcat 'CC'        TEXT-004 .
  mc_append_fieldcat 'COUNTRY'   TEXT-005 .
  mc_append_fieldcat 'USERNAME'  TEXT-006 .
  mc_append_fieldcat 'EMAIL'     TEXT-007 .

  ls_layout-zebra = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
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
*&      Form  PF_COUNTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_country .
  REFRESH gt_t005t.
  SELECT * FROM t005t INTO TABLE gt_t005t WHERE spras = sy-langu.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GEN_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gen_xml .
  DATA: lv_string   TYPE string,
        lv_field    TYPE string,
        lv_filename TYPE string.
  CHECK gt_output IS NOT INITIAL.
  CLEAR gt_xml.

  mc_concat_xml '<?xml version="1.0"?>'.
  mc_concat_xml '<?mso-application progid="Excel.Sheet"?>'  .
  mc_concat_xml '<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"'  .
  mc_concat_xml ' xmlns:o="urn:schemas-microsoft-com:office:office"'  .
  mc_concat_xml ' xmlns:x="urn:schemas-microsoft-com:office:excel"'  .
  mc_concat_xml ' xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"'  .
  mc_concat_xml ' xmlns:html="http://www.w3.org/TR/REC-html40">'  .
  mc_concat_xml ' <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">'  .
  mc_concat_xml '  <Created>2017-09-05T12:23:12Z</Created>'  .
  mc_concat_xml '  <LastSaved>2017-09-05T14:03:41Z</LastSaved>'  .
  mc_concat_xml '  <Version>16.00</Version>'  .
  mc_concat_xml ' </DocumentProperties>'  .
  mc_concat_xml ' <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">'  .
  mc_concat_xml '  <AllowPNG/>'  .
  mc_concat_xml ' </OfficeDocumentSettings>'  .
  mc_concat_xml ' <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">'  .
  mc_concat_xml '  <WindowHeight>8310</WindowHeight>'  .
  mc_concat_xml '  <WindowWidth>20490</WindowWidth>'  .
  mc_concat_xml '  <WindowTopX>0</WindowTopX>'  .
  mc_concat_xml '  <WindowTopY>0</WindowTopY>'  .
  mc_concat_xml '  <ProtectStructure>False</ProtectStructure>'  .
  mc_concat_xml '  <ProtectWindows>False</ProtectWindows>'  .
  mc_concat_xml ' </ExcelWorkbook>'  .
  mc_concat_xml ' <Styles>'  .
  mc_concat_xml '  <Style ss:ID="Default" ss:Name="Normal">'  .
  mc_concat_xml '   <Alignment ss:Vertical="Bottom"/>'  .
  mc_concat_xml '   <Borders/>'  .
  mc_concat_xml '   <Font ss:FontName="Calibri" x:Family="Swiss" ss:Size="11" ss:Color="#000000"/>'  .
  mc_concat_xml '   <Interior/>'  .
  mc_concat_xml '   <NumberFormat/>'  .
  mc_concat_xml '   <Protection/>'  .
  mc_concat_xml '  </Style>'  .
  mc_concat_xml '  <Style ss:ID="s16">'  .
  mc_concat_xml '   <Font ss:FontName="Calibri" x:Family="Swiss" ss:Size="11" ss:Color="#FFFFFF"/>'  .
  mc_concat_xml '   <Interior ss:Color="#44546A" ss:Pattern="Solid"/>'  .
  mc_concat_xml '  </Style>'  .
  mc_concat_xml '  <Style ss:ID="s17">'  .
  mc_concat_xml '   <Font ss:FontName="Calibri" x:Family="Swiss" ss:Size="11" ss:Color="#FF0000"/>'  .
  mc_concat_xml '  </Style>'  .
  mc_concat_xml ' </Styles>'  .
  mc_concat_xml ' <Worksheet ss:Name="Sheet1">'  .
  mc_concat_xml '  <Table ss:ExpandedColumnCount="14" ss:ExpandedRowCount="9999" x:FullColumns="1"'  .
  mc_concat_xml '   x:FullRows="1" ss:DefaultRowHeight="15">'  .
  mc_concat_xml '   <Column ss:Width="91.5"/>'  .
  mc_concat_xml '   <Column ss:Width="156"/>'  .
  mc_concat_xml '   <Column ss:Width="65.25"/>'  .
  mc_concat_xml '   <Column ss:Width="75"/>'  .
  mc_concat_xml '   <Column ss:Width="50.25"/>'  .
  mc_concat_xml '   <Column ss:Width="66"/>'  .
  mc_concat_xml '   <Column ss:Width="270.75"/>'  .
  mc_concat_xml '   <Column ss:Width="58.5"/>'  .
  mc_concat_xml '   <Column ss:Width="114"/>'  .
  mc_concat_xml '   <Column ss:Width="82.5"/>'  .
  mc_concat_xml '   <Column ss:Width="99"/>'  .
  mc_concat_xml '   <Column ss:Width="39"/>'  .
  mc_concat_xml '   <Column ss:Width="58.5"/>'  .
  mc_concat_xml '   <Column ss:Width="156"/>'  .
  mc_concat_xml '   <Row>'  .
  mc_concat_xml '    <Cell ss:StyleID="s16"><Data ss:Type="String">Personal number</Data></Cell>'  .
  mc_concat_xml '    <Cell ss:StyleID="s16"><Data ss:Type="String">Name</Data></Cell>'  .
  mc_concat_xml '    <Cell ss:StyleID="s16"><Data ss:Type="String">City</Data></Cell>'  .
  mc_concat_xml '    <Cell ss:StyleID="s16"><Data ss:Type="String">Country Code</Data></Cell>'  .
  mc_concat_xml '    <Cell ss:StyleID="s16"><Data ss:Type="String">Country</Data></Cell>'  .
  mc_concat_xml '    <Cell ss:StyleID="s16"><Data ss:Type="String">Username</Data></Cell>'  .
  mc_concat_xml '    <Cell ss:StyleID="s16"><Data ss:Type="String">Email</Data></Cell>'  .
  mc_concat_xml '    <Cell ss:StyleID="s16"><Data ss:Type="String">Position ID</Data></Cell>'  .
  mc_concat_xml '    <Cell ss:StyleID="s16"><Data ss:Type="String">Position</Data></Cell>'  .
  mc_concat_xml '    <Cell ss:StyleID="s16"><Data ss:Type="String">Organization ID</Data></Cell>'  .
  mc_concat_xml '    <Cell ss:StyleID="s16"><Data ss:Type="String">Organization</Data></Cell>'  .
  mc_concat_xml '    <Cell ss:StyleID="s16"><Data ss:Type="String">Salary</Data></Cell>'  .
  mc_concat_xml '    <Cell ss:StyleID="s16"><Data ss:Type="String">Manager</Data></Cell>'  .
  mc_concat_xml '    <Cell ss:StyleID="s16"><Data ss:Type="String">Manager name</Data></Cell>'  .
  mc_concat_xml '   </Row>'  .

  LOOP AT gt_output INTO gw_output.
    mc_concat_xml '   <Row>'  .


    PERFORM pf_format_text USING gw_output-pernr   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s17"><Data ss:Type="Number">' lv_field '</Data></Cell>' INTO lv_string.
*  mc_concat_xml '    <Cell ss:StyleID="s17"><Data ss:Type="Number">26000021</Data></Cell>'  .
    mc_concat_xml lv_string.

    PERFORM pf_format_text USING gw_output-ename   CHANGING lv_field.
    CONCATENATE '<Cell><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
*  mc_concat_xml '    <Cell><Data ss:Type="String">Mr Yoon Surapong</Data></Cell>'  .
    mc_concat_xml lv_string.

    PERFORM pf_format_text USING gw_output-city   CHANGING lv_field.
    CONCATENATE '<Cell><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
*  mc_concat_xml '    <Cell><Data ss:Type="String">Bangkok</Data></Cell>'  .
    mc_concat_xml lv_string.

    PERFORM pf_format_text USING gw_output-cc   CHANGING lv_field.
    CONCATENATE '<Cell><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
*  mc_concat_xml '    <Cell><Data ss:Type="String">TH</Data></Cell>'  .
    mc_concat_xml lv_string.

    PERFORM pf_format_text USING gw_output-country   CHANGING lv_field.
    CONCATENATE '<Cell><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
*  mc_concat_xml '    <Cell><Data ss:Type="String">Thailand</Data></Cell>'  .
    mc_concat_xml lv_string.

    PERFORM pf_format_text USING gw_output-username   CHANGING lv_field.
    CONCATENATE '<Cell><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
*    mc_concat_xml '    <Cell><Data ss:Type="String">EMABAP03</Data></Cell>'  .
    mc_concat_xml lv_string.

    PERFORM pf_format_text USING gw_output-email   CHANGING lv_field.
    CONCATENATE '<Cell><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
*    mc_concat_xml '    <Cell><Data ss:Type="String">NARONGSAK.WISEKSOMPONG@GMAIL.COM</Data></Cell>'  .
    mc_concat_xml lv_string.

    PERFORM pf_format_text USING gw_output-objid_s   CHANGING lv_field.
    CONCATENATE '<Cell><Data ss:Type="Number">' lv_field '</Data></Cell>' INTO lv_string.
*    mc_concat_xml '    <Cell><Data ss:Type="Number">30000172</Data></Cell>'  .
    mc_concat_xml lv_string.

    PERFORM pf_format_text USING gw_output-stext_s   CHANGING lv_field.
    CONCATENATE '<Cell><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
*    mc_concat_xml '    <Cell><Data ss:Type="String">Technical Consultant</Data></Cell>'  .
    mc_concat_xml lv_string.

    PERFORM pf_format_text USING gw_output-objid_o  CHANGING lv_field.
    CONCATENATE '<Cell><Data ss:Type="Number">' lv_field '</Data></Cell>' INTO lv_string.
*    mc_concat_xml '    <Cell><Data ss:Type="Number">20000169</Data></Cell>'  .
    mc_concat_xml lv_string.

    PERFORM pf_format_text USING gw_output-stext_o   CHANGING lv_field.
    CONCATENATE '<Cell><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
*    mc_concat_xml '    <Cell><Data ss:Type="String">Technical Training</Data></Cell>'  .
    mc_concat_xml lv_string.

    PERFORM pf_format_text USING gw_output-salary  CHANGING lv_field.
    CONCATENATE '<Cell><Data ss:Type="Number">' lv_field '</Data></Cell>' INTO lv_string.
*    mc_concat_xml '    <Cell><Data ss:Type="Number">28000</Data></Cell>'  .
    mc_concat_xml lv_string.

    PERFORM pf_format_text USING gw_output-objid_s_b  CHANGING lv_field.
    CONCATENATE '<Cell><Data ss:Type="Number">' lv_field '</Data></Cell>' INTO lv_string.
*    mc_concat_xml '    <Cell><Data ss:Type="Number">26000020</Data></Cell>'  .
    mc_concat_xml lv_string.

    PERFORM pf_format_text USING gw_output-stext_s_b   CHANGING lv_field.
    CONCATENATE '<Cell><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
*    mc_concat_xml '    <Cell><Data ss:Type="String">Mr Narongsak Wisedsompong</Data></Cell>'  .
    mc_concat_xml lv_string.
    mc_concat_xml '   </Row>'  .


  ENDLOOP.
  mc_concat_xml '  </Table>'  .
  mc_concat_xml '  <WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">'  .
  mc_concat_xml '   <PageSetup>'  .
  mc_concat_xml '    <Header x:Margin="0.3"/>'  .
  mc_concat_xml '    <Footer x:Margin="0.3"/>'  .
  mc_concat_xml '    <PageMargins x:Bottom="0.75" x:Left="0.7" x:Right="0.7" x:Top="0.75"/>'  .
  mc_concat_xml '   </PageSetup>'  .
  mc_concat_xml '   <Print>'  .
  mc_concat_xml '    <ValidPrinterInfo/>'  .
  mc_concat_xml '    <PaperSizeIndex>9</PaperSizeIndex>'  .
  mc_concat_xml '    <HorizontalResolution>600</HorizontalResolution>'  .
  mc_concat_xml '    <VerticalResolution>600</VerticalResolution>'  .
  mc_concat_xml '   </Print>'  .
  mc_concat_xml '   <Zoom>70</Zoom>'  .
  mc_concat_xml '   <Selected/>'  .
  mc_concat_xml '   <Panes>'  .
  mc_concat_xml '    <Pane>'  .
  mc_concat_xml '     <Number>3</Number>'  .
  mc_concat_xml '     <ActiveRow>13</ActiveRow>'  .
  mc_concat_xml '     <ActiveCol>6</ActiveCol>'  .
  mc_concat_xml '    </Pane>'  .
  mc_concat_xml '   </Panes>'  .
  mc_concat_xml '   <ProtectObjects>False</ProtectObjects>'  .
  mc_concat_xml '   <ProtectScenarios>False</ProtectScenarios>'  .
  mc_concat_xml '  </WorksheetOptions>'  .
  mc_concat_xml ' </Worksheet>'  .
  mc_concat_xml '</Workbook>'  .
  lv_filename = p_output.
  PERFORM pf_gen_excel_file TABLES gt_xml
                            USING lv_filename
                                  abap_true. "
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_FORMAT_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GW_OUTPUT_PERNR  text
*      <--P_LV_FIELD  text
*----------------------------------------------------------------------*
FORM pf_format_text  USING    p_value
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
*&      Form  PF_PREPARE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_prepare_om_data .

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

*-- Get Position
      READ TABLE gt_objec ASSIGNING <fs_objec>
                          WITH KEY otype = 'S'.
      IF sy-subrc EQ 0.
        <fs_output>-objid_s = <fs_objec>-objid.
        <fs_output>-stext_s = <fs_objec>-stext.
      ENDIF.

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


         loop at gt_objec2 ASSIGNING <fs_objec2>
                              where otype = 'P' and  objid <> <fs_output>-pernr.
          IF sy-subrc EQ 0 .
            <fs_output>-objid_s_b = <fs_objec2>-objid.
            <fs_output>-stext_s_b = <fs_objec2>-stext.
          ENDIF.
          endloop.

        IF <fs_output>-objid_s_b IS NOT INITIAL
        AND <fs_output>-stext_s_b IS NOT INITIAL.
        EXIT.
        ENDIF.



  ENDLOOP.

ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_PREPARE_PY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_prepare_py_data .
  DATA: lt_rgdir  LIKE STANDARD TABLE OF pc261,
        lt_evpdir LIKE STANDARD TABLE OF pc261.

  DATA: lw_rgdir        TYPE pc261,
        lw_t5001        LIKE  t500l,
        lw_evpdir       LIKE LINE OF lt_evpdir,
        lw_payth_result TYPE  payth_result,
        lw_rt           LIKE LINE OF lw_payth_result-inter-rt,
        lw_crt          LIKE LINE OF lw_payth_result-inter-crt,
        lw_bt           LIKE LINE OF lw_payth_result-inter-bt,
        lw_wpbp         LIKE LINE OF lw_payth_result-inter-wpbp.

  DATA: lv_betrg  TYPE betrg.

  LOOP AT gt_output ASSIGNING <fs_output>.

*find person who pernr = pernr in selscreen
    CALL FUNCTION 'CU_READ_RGDIR'
      EXPORTING
        persnr          = <fs_output>-pernr
      IMPORTING
        molga           = gv_molga
      TABLES
        in_rgdir        = lt_rgdir
      EXCEPTIONS
        no_record_found = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      CONTINUE.
    ELSE.
      <fs_output>-molga = gv_molga.
    ENDIF.

*    READ TABLE lt_rgdir INTO lw_rgdir INDEX 1.
*    IF sy-subrc = 0.
*      <fs_output>-sequnce = lw_rgdir-seqnr .
*    ENDIF.

* for later
    LOOP AT lt_rgdir INTO lw_rgdir WHERE fpbeg <= pn-endda
                                   AND fpend >= pn-begda.
      <fs_output>-sequnce = lw_rgdir-seqnr .
    ENDLOOP.
    CHECK <fs_output>-sequnce IS NOT INITIAL.

*find by molga
    CALL FUNCTION 'HRPY_READ_T500L'
      EXPORTING
        molga          = gv_molga
      IMPORTING
        t500l_entry    = lw_t5001
      EXCEPTIONS
        no_entry_found = 1
        OTHERS         = 2.
    IF sy-subrc = 0.
      CALL FUNCTION 'CD_CHECK_PAYROLL_RESULT'  "find data by lt_rgdir
        EXPORTING
          bonus_date      = lw_rgdir-bondt
          inper_modif     = lw_rgdir-iperm
          inper           = lw_rgdir-fpper
          pay_type        = lw_rgdir-payty
          pay_ident       = lw_rgdir-payid
        TABLES
          rgdir           = lt_rgdir
        EXCEPTIONS
          no_record_found = 1
          OTHERS          = 2.
      IF sy-subrc = 0.
        CALL FUNCTION 'CD_EVALUATION_PERIODS' " insert period to pull all rerated data and change to P or A
          EXPORTING
            bonus_date      = lw_rgdir-bondt
            inper_modif     = lw_rgdir-iperm
            inper           = lw_rgdir-fpper
            pay_type        = lw_rgdir-payty
            pay_ident       = lw_rgdir-payid
          TABLES
            evpdir          = lt_evpdir " output for PYXX_READ_PAYROLL_RESULT
            rgdir           = lt_rgdir
          EXCEPTIONS
            no_record_found = 1
            OTHERS          = 2.
        IF sy-subrc = 0.

*lt_evpdir from last function (CD_EVALUATION_PERIODS)
          LOOP AT lt_evpdir INTO lw_evpdir.
            CLEAR: lw_payth_result .
            CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'  "read pull data
              EXPORTING
                clusterid                    = lw_t5001-relid
                employeenumber               = <fs_output>-pernr
                sequencenumber               = lw_evpdir-seqnr
              CHANGING
                payroll_result               = lw_payth_result
              EXCEPTIONS
                illegal_isocode_or_clusterid = 1
                error_generating_import      = 2
                import_mismatch_error        = 3
                subpool_dir_full             = 4
                no_read_authority            = 5
                no_record_found              = 6
                versions_do_not_match        = 7
                error_reading_archive        = 8
                error_reading_relid          = 9
                OTHERS                       = 10.
            IF sy-subrc = 0.
* Implement suitable error handling here
            ENDIF.

            LOOP AT lw_payth_result-inter-rt INTO lw_rt
              WHERE lgart EQ '1000'.
              IF lw_evpdir-srtza EQ 'P'.
                lv_betrg = lw_rt-betrg * -1.
              ELSE.
                lv_betrg = lw_rt-betrg.
              ENDIF.
              ADD lv_betrg TO <fs_output>-salary.
            ENDLOOP.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_GET_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_get_month .

  CALL FUNCTION '/DSD/PR_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = sy-datum
    IMPORTING
      last_day_of_month = gv_endda
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_GEN_EXCEL_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_XML  text
*      -->P_LV_FILENAME  text
*      -->P_ABAP_TRUE  text
*----------------------------------------------------------------------*
FORM pf_gen_excel_file  TABLES  it_print     LIKE gt_xml
                         USING   im_file_name TYPE string
                                 im_open      TYPE abap_bool.
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

  CONCATENATE lv_sapworkdir '\' sy-repid '_TMP.xls' INTO lv_tmp_path.

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
FORM pf_browse_locl_folder .
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'PATH'
    IMPORTING
      file_name     = gv_file_path.
ENDFORM.
