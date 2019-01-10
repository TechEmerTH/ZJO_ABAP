*&---------------------------------------------------------------------*
*&  Include           ZHRAUTHREP01_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  PF_GEN_HEADER_TCODE
*&---------------------------------------------------------------------*
FORM pf_gen_header_tcode .
  mc_concat_xml '<?xml version="1.0"?>' .
  mc_concat_xml '<?mso-application progid="Excel.Sheet"?>' .
  mc_concat_xml '<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"' .
  mc_concat_xml ' xmlns:o="urn:schemas-microsoft-com:office:office"' .
  mc_concat_xml ' xmlns:x="urn:schemas-microsoft-com:office:excel"' .
  mc_concat_xml ' xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"' .
  mc_concat_xml ' xmlns:html="http://www.w3.org/TR/REC-html40">' .
  mc_concat_xml ' <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">' .
  mc_concat_xml '  <Author>SDMaster</Author>' .
  mc_concat_xml '  <LastAuthor>SDMaster</LastAuthor>' .
  mc_concat_xml '  <Created>2018-03-16T03:04:57Z</Created>' .
  mc_concat_xml '  <Version>15.00</Version>' .
  mc_concat_xml ' </DocumentProperties>' .
  mc_concat_xml ' <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">' .
  mc_concat_xml '  <AllowPNG/>' .
  mc_concat_xml ' </OfficeDocumentSettings>' .
  mc_concat_xml ' <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">' .
  mc_concat_xml '  <WindowHeight>7905</WindowHeight>' .
  mc_concat_xml '  <WindowWidth>20490</WindowWidth>' .
  mc_concat_xml '  <WindowTopX>0</WindowTopX>' .
  mc_concat_xml '  <WindowTopY>0</WindowTopY>' .
  mc_concat_xml '  <ProtectStructure>False</ProtectStructure>' .
  mc_concat_xml '  <ProtectWindows>False</ProtectWindows>' .
  mc_concat_xml ' </ExcelWorkbook>' .
  mc_concat_xml ' <Styles>' .
  mc_concat_xml '  <Style ss:ID="Default" ss:Name="Normal">' .
  mc_concat_xml '   <Alignment ss:Vertical="Bottom"/>' .
  mc_concat_xml '   <Borders/>' .
  mc_concat_xml '   <Font ss:FontName="Tahoma" x:CharSet="222" x:Family="Swiss" ss:Size="11"' .
  mc_concat_xml '    ss:Color="#000000"/>' .
  mc_concat_xml '   <Interior/>' .
  mc_concat_xml '   <NumberFormat/>' .
  mc_concat_xml '   <Protection/>' .
  mc_concat_xml '  </Style>' .
  mc_concat_xml '  <Style ss:ID="s104" ss:Name="Normal 2">' .
  mc_concat_xml '   <Alignment ss:Vertical="Bottom"/>' .
  mc_concat_xml '   <Borders/>' .
  mc_concat_xml '   <Font ss:FontName="BrowalliaUPC" x:Family="Swiss" ss:Size="16"' .
  mc_concat_xml '    ss:Color="#000000"/>' .
  mc_concat_xml '   <Interior/>' .
  mc_concat_xml '   <NumberFormat/>' .
  mc_concat_xml '   <Protection/>' .
  mc_concat_xml '  </Style>' .
  mc_concat_xml '  <Style ss:ID="s111" ss:Parent="s104">' .
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
  mc_concat_xml '  <Style ss:ID="s112" ss:Parent="s104">' .
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
  mc_concat_xml '  <Table ss:ExpandedColumnCount="6" ss:ExpandedRowCount="999999" x:FullColumns="1"' .
  mc_concat_xml '   x:FullRows="1" ss:DefaultColumnWidth="54" ss:DefaultRowHeight="14.25">' .
  mc_concat_xml '   <Column ss:AutoFitWidth="0" ss:Width="60.75"/>' .
  mc_concat_xml '   <Column ss:AutoFitWidth="0" ss:Width="102.75"/>' .
  mc_concat_xml '   <Column ss:AutoFitWidth="0" ss:Width="112.5"/>' .
  mc_concat_xml '   <Column ss:AutoFitWidth="0" ss:Width="276"/>' .
  mc_concat_xml '   <Column ss:AutoFitWidth="0" ss:Width="115.5"/>' .
  mc_concat_xml '   <Column ss:AutoFitWidth="0" ss:Width="353.25"/>' .
  mc_concat_xml '   <Row ss:Height="21">' .
  mc_concat_xml '    <Cell ss:StyleID="s111"><Data ss:Type="String">ÅÓ´Ñº</Data></Cell>' .
  mc_concat_xml '    <Cell ss:StyleID="s111"><Data ss:Type="String">ª×èÍ User</Data></Cell>' .
  mc_concat_xml '    <Cell ss:StyleID="s111"><Data ss:Type="String">ª×èÍ á¼¹¡</Data></Cell>' .
  mc_concat_xml '    <Cell ss:StyleID="s111"><Data ss:Type="String">ª×èÍ Role</Data></Cell>' .
  mc_concat_xml '    <Cell ss:StyleID="s111"><Data ss:Type="String">ª×èÍ¿Ñ§¡ìªÑ¹</Data></Cell>' .
  mc_concat_xml '    <Cell ss:StyleID="s111"><Data ss:Type="String">¤ÓÍ¸ÔºÒÂ</Data></Cell>' .
  mc_concat_xml '   </Row>' .
ENDFORM.                    " PF_GEN_HEADER_TCODE
*&---------------------------------------------------------------------*
*&      Form  PF_GEN_BODY_TCODE
*&---------------------------------------------------------------------*
FORM pf_gen_body_tcode .
  DATA: lv_string TYPE string,
        lv_field  TYPE string.
  LOOP AT gt_output_tcode INTO gw_output_tcode.
    mc_concat_xml '   <Row ss:Height="21">' .

    PERFORM pf_format_text USING gw_output_tcode-running_no   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s112"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*mc_concat_xml '    <Cell ss:StyleID="s112"><Data ss:Type="String">1</Data></Cell>' .

    PERFORM pf_format_text USING gw_output_tcode-username   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s112"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*mc_concat_xml '    <Cell ss:StyleID="s112"><Data ss:Type="String">ACCHO1-01</Data></Cell>' .

    PERFORM pf_format_text USING gw_output_tcode-department   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s112"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*mc_concat_xml '    <Cell ss:StyleID="s112"><Data ss:Type="String">»»»¡¿Ë¡</Data></Cell>' .

    PERFORM pf_format_text USING gw_output_tcode-role_name     CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s112"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*mc_concat_xml '    <Cell ss:StyleID="s112"><Data ss:Type="String">ACCHO_APPEND-DOC_IC_C</Data></Cell>' .

    PERFORM pf_format_text USING gw_output_tcode-function_name        CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s112"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*mc_concat_xml '    <Cell ss:StyleID="s112"><Data ss:Type="String">FB03L</Data></Cell>' .

    PERFORM pf_format_text USING gw_output_tcode-desc     CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s112"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*mc_concat_xml '    <Cell ss:StyleID="s112"><Data ss:Type="String">Document Display : G/L View</Data></Cell>' .
    mc_concat_xml '   </Row>' .
  ENDLOOP.
ENDFORM.                    " PF_GEN_BODY_TCODE
*&---------------------------------------------------------------------*
*&      Form  PF_GEN_FOOTER_TCODE
*&---------------------------------------------------------------------*
FORM pf_gen_footer_tcode .

  mc_concat_xml '  </Table>' .
  mc_concat_xml '  <WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">' .
  mc_concat_xml '   <PageSetup>' .
  mc_concat_xml '    <Header x:Margin="0.3"/>' .
  mc_concat_xml '    <Footer x:Margin="0.3"/>' .
  mc_concat_xml '    <PageMargins x:Bottom="0.75" x:Left="0.7" x:Right="0.7" x:Top="0.75"/>' .
  mc_concat_xml '   </PageSetup>' .
  mc_concat_xml '   <Selected/>' .
  mc_concat_xml '   <Panes>' .
  mc_concat_xml '    <Pane>' .
  mc_concat_xml '     <Number>3</Number>' .
  mc_concat_xml '     <ActiveRow>5</ActiveRow>' .
  mc_concat_xml '     <ActiveCol>5</ActiveCol>' .
  mc_concat_xml '    </Pane>' .
  mc_concat_xml '   </Panes>' .
  mc_concat_xml '   <ProtectObjects>False</ProtectObjects>' .
  mc_concat_xml '   <ProtectScenarios>False</ProtectScenarios>' .
  mc_concat_xml '  </WorksheetOptions>' .
  mc_concat_xml ' </Worksheet>' .
  mc_concat_xml '</Workbook>' .
ENDFORM.                    " PF_GEN_FOOTER_TCODE
*&---------------------------------------------------------------------*
*&      Form  PF_GEN_HEADER_AUTH
*&---------------------------------------------------------------------*
FORM pf_gen_header_auth .
  mc_concat_xml '<?xml version="1.0"?>' .
  mc_concat_xml '<?mso-application progid="Excel.Sheet"?>' .
  mc_concat_xml '<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"' .
  mc_concat_xml ' xmlns:o="urn:schemas-microsoft-com:office:office"' .
  mc_concat_xml ' xmlns:x="urn:schemas-microsoft-com:office:excel"' .
  mc_concat_xml ' xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"' .
  mc_concat_xml ' xmlns:html="http://www.w3.org/TR/REC-html40">' .
  mc_concat_xml ' <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">' .
  mc_concat_xml '  <Author>SDMaster</Author>' .
  mc_concat_xml '  <LastAuthor>SDMaster</LastAuthor>' .
  mc_concat_xml '  <Created>2018-03-16T03:29:29Z</Created>' .
  mc_concat_xml '  <Version>15.00</Version>' .
  mc_concat_xml ' </DocumentProperties>' .
  mc_concat_xml ' <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">' .
  mc_concat_xml '  <AllowPNG/>' .
  mc_concat_xml ' </OfficeDocumentSettings>' .
  mc_concat_xml ' <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">' .
  mc_concat_xml '  <WindowHeight>7905</WindowHeight>' .
  mc_concat_xml '  <WindowWidth>20490</WindowWidth>' .
  mc_concat_xml '  <WindowTopX>0</WindowTopX>' .
  mc_concat_xml '  <WindowTopY>0</WindowTopY>' .
  mc_concat_xml '  <ProtectStructure>False</ProtectStructure>' .
  mc_concat_xml '  <ProtectWindows>False</ProtectWindows>' .
  mc_concat_xml ' </ExcelWorkbook>' .
  mc_concat_xml ' <Styles>' .
  mc_concat_xml '  <Style ss:ID="Default" ss:Name="Normal">' .
  mc_concat_xml '   <Alignment ss:Vertical="Bottom"/>' .
  mc_concat_xml '   <Borders/>' .
  mc_concat_xml '   <Font ss:FontName="Tahoma" x:CharSet="222" x:Family="Swiss" ss:Size="11"' .
  mc_concat_xml '    ss:Color="#000000"/>' .
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
  mc_concat_xml '  <Style ss:ID="s63">' .
  mc_concat_xml '   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>' .
  mc_concat_xml '  </Style>' .
  mc_concat_xml '  <Style ss:ID="s65" ss:Parent="s62">' .
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
  mc_concat_xml '  <Style ss:ID="s66" ss:Parent="s62">' .
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
  mc_concat_xml '  <Table ss:ExpandedColumnCount="8" ss:ExpandedRowCount="999999" x:FullColumns="1"' .
  mc_concat_xml '   x:FullRows="1" ss:StyleID="s63" ss:DefaultColumnWidth="54"' .
  mc_concat_xml '   ss:DefaultRowHeight="14.25">' .
  mc_concat_xml '   <Column ss:Index="2" ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="92.25"/>' .
  mc_concat_xml '   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="106.5"/>' .
  mc_concat_xml '   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="220.5"/>' .
  mc_concat_xml '   <Column ss:StyleID="s63" ss:Hidden="1" ss:AutoFitWidth="0" ss:Width="173.25"/>' .
  mc_concat_xml '   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="140.25"/>' .
  mc_concat_xml '   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="72"/>' .
  mc_concat_xml '   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="123"/>' .
  mc_concat_xml '   <Row ss:AutoFitHeight="0" ss:Height="21">' .
  mc_concat_xml '    <Cell ss:StyleID="s65"><Data ss:Type="String">ÅÓ´Ñº</Data></Cell>' .
  mc_concat_xml '    <Cell ss:StyleID="s65"><Data ss:Type="String">ª×èÍ User</Data></Cell>' .
  mc_concat_xml '    <Cell ss:StyleID="s65"><Data ss:Type="String">ª×èÍ á¼¹¡</Data></Cell>' .
  mc_concat_xml '    <Cell ss:StyleID="s65"><Data ss:Type="String">ª×èÍ Role</Data></Cell>' .
  mc_concat_xml '    <Cell ss:StyleID="s65"><Data ss:Type="String">ª×èÍ Profile</Data></Cell>' .
  mc_concat_xml '    <Cell ss:StyleID="s65"><Data ss:Type="String">ª×èÍ Object</Data></Cell>' .
  mc_concat_xml '    <Cell ss:StyleID="s65"><Data ss:Type="String">Value</Data></Cell>' .
  mc_concat_xml '    <Cell ss:StyleID="s65"><Data ss:Type="String">¤ÓÍ¸ÔºÒÂ</Data></Cell>' .
  mc_concat_xml '   </Row>' .
ENDFORM.                    " PF_GEN_HEADER_AUTH
*&---------------------------------------------------------------------*
*&      Form  PF_GEN_BODY_AUTH
*&---------------------------------------------------------------------*
FORM pf_gen_body_auth .
  DATA: lv_string TYPE string,
        lv_field  TYPE string.
  LOOP AT gt_output_auth INTO gw_output_auth.

    mc_concat_xml '   <Row ss:AutoFitHeight="0" ss:Height="21">' .
    PERFORM pf_format_text USING gw_output_auth-running_no   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s66"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*  mc_concat_xml '    <Cell ss:StyleID="s66"><Data ss:Type="String">1</Data></Cell>' .

    PERFORM pf_format_text USING gw_output_auth-username   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s66"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*  mc_concat_xml '    <Cell ss:StyleID="s66"><Data ss:Type="String">ADBCS2-01</Data></Cell>' .

    PERFORM pf_format_text USING gw_output_auth-department   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s66"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*  mc_concat_xml '    <Cell ss:StyleID="s66"><Data ss:Type="String">¿Ë¡¿Ë¡¿Ë</Data></Cell>'

    PERFORM pf_format_text USING gw_output_auth-role_name   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s66"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*  mc_concat_xml '    <Cell ss:StyleID="s66"><Data ss:Type="String">ADM_APPROVAL_BCS_C</Data></Cell>' .

    PERFORM pf_format_text USING gw_output_auth-profile_name   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s66"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.

*  mc_concat_xml '    <Cell ss:StyleID="s66"><Data ss:Type="String">T-DV72383600</Data></Cell>' .

    PERFORM pf_format_text USING gw_output_auth-field_name   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s66"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*  mc_concat_xml '    <Cell ss:StyleID="s66"><Data ss:Type="String">Authorization level</Data></Cell>' .

    PERFORM pf_format_text USING gw_output_auth-value   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s66"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*  mc_concat_xml '    <Cell ss:StyleID="s66"><Data ss:Type="String">read</Data></Cell>' .

    PERFORM pf_format_text USING gw_output_auth-desc   CHANGING lv_field.
    CONCATENATE '<Cell ss:StyleID="s66"><Data ss:Type="String">' lv_field '</Data></Cell>' INTO lv_string.
    mc_concat_xml lv_string.
*  mc_concat_xml '    <Cell ss:StyleID="s66"><Data ss:Type="String"></Data></Cell>' .

    mc_concat_xml '   </Row>' .
  ENDLOOP.
ENDFORM.                    " PF_GEN_BODY_AUTH
*&---------------------------------------------------------------------*
*&      Form  PF_GEN_FOOTER_AUTH
*&---------------------------------------------------------------------*
FORM pf_gen_footer_auth .
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
  mc_concat_xml '     <ActiveCol>4</ActiveCol>' .
  mc_concat_xml '     <RangeSelection>C5</RangeSelection>' .
  mc_concat_xml '    </Pane>' .
  mc_concat_xml '   </Panes>' .
  mc_concat_xml '   <ProtectObjects>False</ProtectObjects>' .
  mc_concat_xml '   <ProtectScenarios>False</ProtectScenarios>' .
  mc_concat_xml '  </WorksheetOptions>' .
  mc_concat_xml ' </Worksheet>' .
  mc_concat_xml '</Workbook>' .
ENDFORM.                    " PF_GEN_FOOTER_AUTH
