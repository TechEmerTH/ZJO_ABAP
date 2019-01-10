*&---------------------------------------------------------------------*
*&  Include           YEM_WF_MAIN_INCLUDE
*&---------------------------------------------------------------------*
* Change By   : Smanpoom T.
* Change Date : 17/02/2014
* Search Team : CH02
* Description : - Create new macro for interface screen
*-----------------------------------------------------------------------
* Change By   : Smanpoom T.
* Change Date : 23/04/2014
* Search Team : CH03
* Description : - Add selection screen for PC only
*               - New function for default directory path
*               - New function for default filename
*-----------------------------------------------------------------------
* Change By   : Thongchai.
* Change Date : 25/04/2014
* Search Team : CH04
* Description : - Adjust to not display number success and error record
*                 for only display report
*-----------------------------------------------------------------------
* Change By   : Preradon K.
* Change Date : 23/06/2014
* Search Team : CH06
* Description : - Adjust top of page alv
* Change Date : 24/06/2014
* Search Team : CH07
* Description : - Change box file download, upload
* Change Date : 02/07/2014
* Search Team : CH09
* Description : - pf screen
*-----------------------------------------------------------------------
* Change By   : Thongchai R.
* Change Date : 25/06/2014
* Search Team : CH08
* Description : - Set text of frame inbound
*               - Set varaint label
* Change Date : 21/07/2014
* Search Team : CH09
* Description : - Extended program check
*-----------------------------------------------------------------------
* Change By   : Thanat H.
* Change Date : 08/08/2014
* Search Team : CH10
* Description : Auto create folder when saving excel file
*-----------------------------------------------------------------------
* Change By   : Jirapat A.
* Change Date : 12/08/2014
* Search Team : CH11
* Description : Optional, add/remove total no.
*-----------------------------------------------------------------------
* Change By   : Jirapat A.
* Change Date : 28/08/2014
* Search Team : CH12
* Description : Avoid 'Selection Option' display as test (not icon)
*               Ex. @28\QSelect: Include pattern@
*-----------------------------------------------------------------------
* Change By   : Preradon K.
* Change Date : 01/09/2014
* Search Team : CH13
* Description : Message Class PF_NO_DATA_FOUND
*-----------------------------------------------------------------------
* Change By   : Ruangwut I.
* Change Date : 05/11/2015
* Search Team : CH14
* Description : Adjust  Header Text.
*-----------------------------------------------------------------------
* Change By   : Thongchai S.
* Change Date : 20/11/2015
* Search Team : CH15
* Description : Adjust Top-Of-Page ALV.
*-----------------------------------------------------------------------
* Change By   : Jirapat A.
* Change Date : 03/03/2016
* Search Team : CH16
* Description : Adjusted number format like 1938 -> 1,938
*-----------------------------------------------------------------------
* Change By   : Preradon K.
* Change Date : 24/03/2016
* Search Team : CH17
* Description : Adjust inbound,outbound
*-----------------------------------------------------------------------

*$*$------------------------------------------------------------------*
*$*$    T A B L E S                                                   *
*$*$------------------------------------------------------------------*
TABLES: T001,            " Company Codes
        ADRC,            " Addresses (central address admin.)
        SSCRFIELDS.      " Fields on selection screens.
TYPE-POOLS ABAP.
INCLUDE <LIST>.
INCLUDE OLE2INCL.

*--> CH02: BEG INS
*$*$------------------------------------------------------------------*
*$*$    Data Declarations                                             *
*$*$------------------------------------------------------------------*
TYPES: BEGIN OF TYP_EXTRA_TOP,
         KEY       TYPE STRING,
         VALUE     TYPE STRING,
         1ST_WIDTH TYPE SDYDO_VALUE,
         2ND_WIDTH TYPE SDYDO_VALUE,
         1ST_ALIGN TYPE SDYDO_ATTRIBUTE,
         2ND_ALIGN TYPE SDYDO_ATTRIBUTE,
       END OF TYP_EXTRA_TOP.

TYPES: BEGIN OF TYP_EXTRA_HEADER,
         KEY       TYPE STRING,
         VALUE     TYPE STRING,
         1ST_WIDTH TYPE SDYDO_VALUE,
         2ND_WIDTH TYPE SDYDO_VALUE,
         1ST_ALIGN TYPE SDYDO_ATTRIBUTE,
         2ND_ALIGN TYPE SDYDO_ATTRIBUTE,
       END OF TYP_EXTRA_HEADER.

TYPES: BEGIN OF TYP_CLOSE_FIELDS,
         TOT    TYPE FLAG,
         COM    TYPE FLAG,
         ERR    TYPE FLAG,
         STG    TYPE FLAG,
         PERIOD TYPE FLAG,
       END OF TYP_CLOSE_FIELDS.

TYPES: BEGIN OF TYP_CLOSE_NEWLINE,
         EXTRA_TOP_FIRST TYPE FLAG,
       END OF TYP_CLOSE_NEWLINE.

DATA: I_EXTRA_HEADER       TYPE TABLE OF TYP_EXTRA_HEADER,
      I_EXTRA_HEADER_UPPER TYPE TABLE OF TYP_EXTRA_HEADER,  "CH06 INS
      I_EXTRA_TOP          TYPE TABLE OF TYP_EXTRA_TOP.

DATA: W_VARIANT       TYPE DISVARIANT,
      W_CLOSE_FIELDS  TYPE TYP_CLOSE_FIELDS,
      W_CLOSE_NEWLINE TYPE TYP_CLOSE_NEWLINE.

DATA: V_BEGDA_1        TYPE DATUM,
      V_ENDDA_1        TYPE DATUM,
      V_DISPLAY_REPORT TYPE FLAG,               "CH04 INS
      V_VALUE          TYPE STRING,
      V_COUNT_TOT_1    TYPE I,
      V_COUNT_COM_1    TYPE I,
      V_COUNT_ERR_1    TYPE I,
      V_COUNT_STG_1    TYPE I,
      V_TOP_TITLE      TYPE STRING,
      V_PERIOD         TYPE STRING,
      V_RUN_DATE       TYPE C LENGTH 10,
      V_RUN_TIME       TYPE C LENGTH 12,
      V_FILENAME       TYPE STRING,
      V_INIT_PATH      TYPE STRING,
      V_DEF_EXTEN      TYPE STRING,
      V_DEF_FILE_N     TYPE STRING,
      V_FILE_FILTER    TYPE STRING.

DATA: T_FIELD_NAME TYPE STANDARD TABLE OF SCREEN-NAME,  "CH09 INS
      W_FIELD_NAME LIKE LINE OF T_FIELD_NAME.           "CH09 INS

FIELD-SYMBOLS: <FS_EXTRA_HEADER> TYPE TYP_EXTRA_HEADER,
               <FS_EXTRA_TOP>    TYPE TYP_EXTRA_HEADER.
*<-- CH02: END INS

*$*$------------------------------------------------------------------*
*$*$    HR CONSTANT                                                   *
*$*$------------------------------------------------------------------*
CONSTANTS: GC_PLVAR          TYPE HRP1000-PLVAR    VALUE '01',
           GC_ORGLEVEL_SUBTY TYPE HRP1002-SUBTY    VALUE '9003',
           GC_ROOT_OTYPE     TYPE HRP1001-OTYPE    VALUE 'O',
           GC_ORG_OTYPE      TYPE HRP1001-OTYPE    VALUE 'O',
           GC_POSTION_OTYPE  TYPE HRP1001-OTYPE    VALUE 'S',
           GC_PERS_OTYPE     TYPE HRP1001-OTYPE    VALUE 'P',
           GC_TASK_OTYPE     TYPE HRP1001-OTYPE    VALUE 'T',
           GC_COSCTR_OTYPE   TYPE HRP1001-OTYPE    VALUE 'K',
           GC_ROOTOBJ_COURSE TYPE HRP1000-STEXT    VALUE 'Central Course'.

CONSTANTS: GC_EXC_STAT2 TYPE PA0000-STAT2     VALUE '0',
           GC_CCODE     TYPE T001-BUKRS       VALUE '1000',
           GC_PREVIOUS  TYPE PC261-SRTZA      VALUE 'P',
           GC_DUMMY_POS TYPE PA0001-PLANS     VALUE '99999999'.
*--> CH14: BEGIN INS
*CONSTANTS: GC_RUN_DATE_H TYPE SDYDO_TEXT_ELEMENT VALUE 'วันที่ประมวลผล:',
*           GC_RUN_TIME_H TYPE SDYDO_TEXT_ELEMENT VALUE 'เวลาที่ประมวลผล:',
*           GC_USER_H     TYPE SDYDO_TEXT_ELEMENT VALUE 'ผู้ประมวลผล:',
*           GC_PERIOD_H   TYPE SDYDO_TEXT_ELEMENT VALUE 'ช่วงเวลาของข้อมูล:',
**           gc_infotype_h TYPE sdydo_text_element VALUE 'วันที่ประมวลผล:',
*           GC_TOT_REC_H  TYPE SDYDO_TEXT_ELEMENT VALUE 'รวมรายการทั้งหมด:',
*           GC_TOT_COM_H  TYPE SDYDO_TEXT_ELEMENT VALUE 'จำนวนรายการที่ดำเนินการสำเร็จ:',
*           GC_TOT_ERR_H  TYPE SDYDO_TEXT_ELEMENT VALUE 'จำนวนรายการที่ดำเนินการไม่สำเร็จ:',
*           GC_TOT_STG_H  TYPE SDYDO_TEXT_ELEMENT VALUE 'จำนวนรายการที่ดึงข้อมูลจาก Staging:',
*           GC_MODE_H     TYPE SDYDO_TEXT_ELEMENT VALUE 'โหมด',
*           GC_COL_WIDTH  TYPE SDYDO_VALUE VALUE '138'.

CONSTANTS: GC_RUN_DATE_H TYPE SDYDO_TEXT_ELEMENT VALUE 'Process Date:',
           GC_RUN_TIME_H TYPE SDYDO_TEXT_ELEMENT VALUE 'Process Time:',
           GC_USER_H     TYPE SDYDO_TEXT_ELEMENT VALUE 'Process By:',
           GC_PERIOD_H   TYPE SDYDO_TEXT_ELEMENT VALUE 'Period:',
*           gc_infotype_h TYPE sdydo_text_element VALUE 'วันที่ประมวลผล:',
           GC_TOT_REC_H  TYPE SDYDO_TEXT_ELEMENT VALUE 'Total Records:',
           GC_TOT_COM_H  TYPE SDYDO_TEXT_ELEMENT VALUE 'จำนวนรายการที่ดำเนินการสำเร็จ:',
           GC_TOT_ERR_H  TYPE SDYDO_TEXT_ELEMENT VALUE 'จำนวนรายการที่ดำเนินการไม่สำเร็จ:',
           GC_TOT_STG_H  TYPE SDYDO_TEXT_ELEMENT VALUE 'จำนวนรายการที่ดึงข้อมูลจาก Staging:',
           GC_MODE_H     TYPE SDYDO_TEXT_ELEMENT VALUE 'โหมด',
           GC_COL_WIDTH  TYPE SDYDO_VALUE VALUE '138'.


CONSTANTS: C_SAVE_A     TYPE C LENGTH 1       VALUE 'A',
           C_TOP_HEIGHT TYPE I                VALUE 31.

CONSTANTS: GC_TEXT_INPUT  TYPE C LENGTH 10 VALUE 'Input'.
CONSTANTS: GC_TEXT_OUTPUT TYPE C LENGTH 10 VALUE 'Output'.
CONSTANTS: GC_TEXT_PC     TYPE C LENGTH 10 VALUE 'PC'.
CONSTANTS: GC_TEXT_APP    TYPE C LENGTH 20 VALUE 'Application server'.
CONSTANTS: GC_TEXT_VAR    TYPE C LENGTH 20 VALUE 'Sap List Viewer'.
CONSTANTS: GC_TEXT_LAY    TYPE C LENGTH 20 VALUE 'Output format'.

DATA : GV_ROOTOBJ TYPE HRP1000-OBJID.                       "CH01
*$*$------------------------------------------------------------------*
*$*$    VARRIABLE                                                     *
*$*$------------------------------------------------------------------*
* Add for page of total page
DATA: TOT_PAGE(5) TYPE C  VALUE '9,999',          "  include
      TOTAL_PAGE  LIKE SY-PAGNO.                 "  include

*DATA: t_ziso      LIKE  ziso      OCCURS 0.
DATA: T_TEXTPOOL  LIKE  TEXTPOOL  OCCURS 0   WITH HEADER LINE.

*$*$------------------------------------------------------------------*
*$*$    M A C R O - C O M M A N D   D E F I N I T I O N S             *
*$*$------------------------------------------------------------------*
*      Selection Screen
DEFINE M%SELECTION_SCREEN_COVER_PAGE.
  PARAMETERS      PC_PRBIG   AS CHECKBOX.  " ######################
END-OF-DEFINITION.

DEFINE M%APPEND_RANGE.
  CLEAR &1.
  &1-SIGN = &2.     &1-OPTION = &3.     &1-LOW = &4.      &1-HIGH  = &5.
*  APPEND &1."CH09 DEL
  APPEND &1 TO &1.
END-OF-DEFINITION.

*      Declare work-area structure and field-symbol for working with
*      specified internal table
DEFINE M%DECLARE_WORK_AREA_FOR_ITAB.
  DATA:          &2  LIKE LINE OF &1.
  FIELD-SYMBOLS: &3  LIKE LINE OF &1.
END-OF-DEFINITION.

DEFINE ZSEL_SCRN_INTERFACE_INPUT.

  sELECTION-SCREEN BEGIN OF BLOCK S1 WITH FRAME TITLE VM_H_IN.
  "PC
  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS: R_RAD1 RADIOBUTTON GROUP R1 DEFAULT 'X' USER-COMMAND UCOMM1.
  SELECTION-SCREEN COMMENT 4(28) VM_H_PC FOR FIELD P_PC1.
  PARAMETERS: P_PC1 TYPE RLGRAP-FILENAME DEFAULT &1.
  SELECTION-SCREEN END OF LINE.

  "APPLICATION SERVER
  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS: R_RAD2 RADIOBUTTON GROUP R1.
  SELECTION-SCREEN COMMENT 4(28) VM_H_APP FOR FIELD P_SV1.
  PARAMETERS: P_SV1 TYPE RLGRAP-FILENAME DEFAULT &2.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN END OF BLOCK S1.

END-OF-DEFINITION.

DEFINE ZSET_SCRN_INTERFACE_INPUT.
  "  zset_scrn_interface_input vm_h_in vm_h_pc vm_h_app.
  &1 = GC_TEXT_INPUT.
  &2 = GC_TEXT_PC.
  &3 = GC_TEXT_APP.

END-OF-DEFINITION.

DEFINE ZSEL_SCRN_INTERFACE_INPUT_PC.

  SELECTION-SCREEN BEGIN OF BLOCK S1 WITH FRAME TITLE VM_H_IN.
  "PC
  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 4(28) VM_H_PC FOR FIELD P_PC1.
  PARAMETERS: P_PC1 TYPE RLGRAP-FILENAME DEFAULT &1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN END OF BLOCK S1.

END-OF-DEFINITION.

DEFINE ZSET_SCRN_INTERFACE_INPUT_PC.
  "  zset_scrn_interface_input_pc vm_h_in vm_h_pc.
  &1 = GC_TEXT_INPUT.
  &2 = GC_TEXT_PC.

END-OF-DEFINITION.

DEFINE ZSEL_SCRN_INTERFACE_OUTPUT.

  SELECTION-SCREEN BEGIN OF BLOCK S2 WITH FRAME TITLE VM_H_OUT.
  "PC
  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS: R_RAD3 RADIOBUTTON GROUP R2 DEFAULT 'X' USER-COMMAND UCOMM2.
  SELECTION-SCREEN COMMENT 4(28) VM_H_PC FOR FIELD P_PC2.
  PARAMETERS: P_PC2 TYPE RLGRAP-FILENAME DEFAULT &1.
  SELECTION-SCREEN END OF LINE.

  "APPLICATION SERVER
  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS: R_RAD4 RADIOBUTTON GROUP R2.
  SELECTION-SCREEN COMMENT 4(28) VM_H_APP FOR FIELD P_SV2.
  PARAMETERS: P_SV2 TYPE RLGRAP-FILENAME DEFAULT &2.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END of block s2.

END-OF-DEFINITION.

DEFINE ZSET_SCRN_INTERFACE_OUTPUT.
  "  zset_scrn_interface_output vm_h_out vm_h_pc vm_h_app.
  &1 = GC_TEXT_OUTPUT.
  &2 = GC_TEXT_PC.
  &3 = GC_TEXT_APP.

END-OF-DEFINITION.

DEFINE ZSEL_SCRN_INTERFACE_OUTPUT_PC.

  SELECTION-SCREEN BEGIN OF BLOCK S2 WITH FRAME TITLE VM_H_OUT.
  "PC
  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 4(28) VM_H_PC FOR FIELD P_PC2.
  PARAMETERS: P_PC2 TYPE RLGRAP-FILENAME DEFAULT &1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN END OF BLOCK S2.

END-OF-DEFINITION.

DEFINE ZSET_SCRN_INTERFACE_OUTPUT_PC.
  "  zset_scrn_interface_output_pc vm_h_out vm_h_pc.
  &1 = GC_TEXT_OUTPUT.
  &2 = GC_TEXT_PC.

END-OF-DEFINITION.

DEFINE ZDEFAULT_FILENAME_INPUT.
  CONCATENATE &1 &2 &3 &4 &5 INTO V_FILENAME.
  PERFORM: PF_GET_INIT_PATH CHANGING V_INIT_PATH.
  PERFORM: PF_CONCAT_PATH_AND_FILE USING V_INIT_PATH
                                         V_FILENAME
                                CHANGING P_PC1.
END-OF-DEFINITION.
DEFINE ZDEFAULT_FILENAME_OUTPUT.
  CONCATENATE &1 &2 &3 &4 &5 INTO V_FILENAME.
  PERFORM: PF_GET_INIT_PATH CHANGING V_INIT_PATH.
  PERFORM: PF_CONCAT_PATH_AND_FILE USING V_INIT_PATH
                                         V_FILENAME
                                CHANGING P_PC2.
END-OF-DEFINITION.

DEFINE ZSET_SCRN_DEFAULT_PATH_INPUT.
  PERFORM: PF_GET_INIT_PATH CHANGING V_INIT_PATH.
  P_PC1 = V_INIT_PATH.

END-OF-DEFINITION.

*---------------------------------------------------------------------*
*       DEFINE ZSEL_SCRN_ALV_SAVE_VARIANT                             *
*---------------------------------------------------------------------*
*  - Create Selection screen for input variant & Default              *
*---------------------------------------------------------------------*
*  -->  &1  : Default Variant                                         *
*---------------------------------------------------------------------*
DEFINE ZSEL_SCRN_ALV_SAVE_VARIANT.

  SELECTION-SCREEN BEGIN OF BLOCK S3 WITH FRAME TITLE VM_H_LAY.
  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(31) VM_H_VAR FOR FIELD P_VAR.
  PARAMETER P_VAR TYPE DISVARIANT-VARIANT DEFAULT &1.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK S3.

END-OF-DEFINITION.

*---------------------------------------------------------------------*
*       DEFINE ZSET_SCRN_ALV_SAVE_VARIANT                             *
*---------------------------------------------------------------------*
*  - Set Label for varint block                                       *
*  Should call macro 'ZSEL_SCRN_ALV_SAVE_VARIANT' first               *
*---------------------------------------------------------------------*
*  -->  &1  : Header Text                                             *
*  -->  &2  : Paramter Label                                          *
*---------------------------------------------------------------------*
DEFINE ZSET_SCRN_ALV_SAVE_VARIANT.
*zset_scrn_alv_save_variant vm_h_var vm_h_lay.
  &1 = GC_TEXT_VAR.
  &2 = GC_TEXT_LAY.

END-OF-DEFINITION.

*---------------------------------------------------------------------*
*       FORM PRINT_VLINE                                              *
*---------------------------------------------------------------------*
*       Subroutine for Print | (Horizontal line)                      *
*---------------------------------------------------------------------*
*  -->  COL : Position of vline                                       *
*  Pattern   PERFORM PRINT_VLINE USING: 0 , 30 , 40 , 50 , 120.       *
*                                                                     *
*---------------------------------------------------------------------*
FORM PRINT_VLINE USING COL.
  WRITE AT COL SY-VLINE.
ENDFORM.                    "print_vline

*---------------------------------------------------------------------*
*       FORM REPORT_HEADER                                            *
*---------------------------------------------------------------------*
*  - Subroutine for print Heading of report at event TOP-OF-PAGE in   *
*    report program.                                                  *
*  - If PL_HTEXT1 no maintain the routine will be read Report name    *
*    from text element                                                *
*---------------------------------------------------------------------*
*  -->  PL_BUKRS  : Company (required)                                *
*  -->  PL_HTEXT1 : Report header 1st line                            *
*  -->  PL_HTEXT2 : Report header 2nd line                            *
*---------------------------------------------------------------------*
FORM REPORT_HEADER  USING VALUE(PL_BUKRS)  LIKE  T001-BUKRS
                          VALUE(PL_HTEXT1)
                          VALUE(PL_HTEXT2).
  DATA: L_CTEXT(100)    TYPE C,
        L_STRLEN        TYPE I,
        L_POSITION      TYPE I,
        L_HTEXT1(100),
        L_HTEXT2(100),
        L_SYSCLIENT(20).
  DATA: L_DATUM_THAI   LIKE SY-DATUM,       "<-- for PWA
        L_YEAR_THAI(4) TYPE N,              "<-- for PWA
        L_DATUM_ENG    LIKE SY-DATUM.       "<-- for PWA
*--> CH09 : BEGIN INS
  DATA:
        LV_TEXT  TYPE STRING
        .
*<-- CH09 : END INS
  SUMMARY.
*        Read Company name
  CLEAR: T001, ADRC.
*  IF NOT pl_bukrs  IS  INITIAL.
*    CLEAR: t001, adrc, l_ctext.
*    SELECT SINGLE * FROM t001 WHERE bukrs = pl_bukrs.
*    SELECT        * FROM adrc WHERE addrnumber  = t001-adrnr
*                              AND   date_from  <= sy-datum
*                              AND   date_to    >= sy-datum
*                              AND   nation      = space.
*      EXIT.
*    ENDSELECT.
*    CONCATENATE adrc-name1 adrc-name2 INTO l_ctext SEPARATED BY space.
*  ELSE.
*    CLEAR: t001, l_ctext.
*    SELECT SINGLE * FROM t001 WHERE bukrs = pl_bukrs.
*    l_ctext  = t001-butxt.
*  ENDIF.
  CLEAR: T001, ADRC, L_CTEXT.
  SELECT SINGLE  BUKRS BUTXT ADRNR
                 INTO CORRESPONDING FIELDS OF T001
                 FROM T001  WHERE BUKRS = PL_BUKRS.
  SELECT  ADDRNUMBER NAME1 NAME2
          INTO CORRESPONDING FIELDS OF ADRC
          FROM ADRC WHERE ADDRNUMBER  = T001-ADRNR
                    AND   DATE_FROM  <= SY-DATUM
                    AND   DATE_TO    >= SY-DATUM
                    AND   NATION      = SPACE.
    EXIT.
  ENDSELECT.
  IF SY-SUBRC  EQ 0 AND ( NOT ADRC-NAME1 IS INITIAL OR
                          NOT ADRC-NAME2 IS INITIAL ) .
    CONCATENATE ADRC-NAME1 ADRC-NAME2 INTO L_CTEXT SEPARATED BY SPACE.
  ELSE.
    L_CTEXT  = T001-BUTXT.
  ENDIF.
*                                         Read Report name in Text-Pool
  IF T_TEXTPOOL[] IS INITIAL.
    READ TEXTPOOL SY-CPROG  INTO T_TEXTPOOL  LANGUAGE SY-LANGU.
  ENDIF.
  IF PL_HTEXT1  IS INITIAL.
    CLEAR T_TEXTPOOL.
    READ TABLE T_TEXTPOOL  WITH KEY ID = 'T'.
    IF SY-SUBRC NE 0.
      READ TABLE T_TEXTPOOL WITH KEY ID = 'R'.
    ENDIF.
    MOVE T_TEXTPOOL-ENTRY TO  L_HTEXT1.
  ELSE.
    MOVE PL_HTEXT1        TO  L_HTEXT1.
  ENDIF.
**                                 Read ISO document no. and Issued date
*  CLEAR ziso.
*  READ TABLE t_ziso  INTO ziso
*                     WITH KEY  bukrs  = pl_bukrs.

*--------------------- Write heading 1st line ------------------------*
*--> CH09 : BEGIN INS
  CLEAR : LV_TEXT.
  PERFORM PF_GET_CONST_TEXT USING 'MAIN_REPORT_ID'
                            CHANGING LV_TEXT.
*<-- CH09 : END INS
  CASE SY-LANGU.
*    WHEN 'E'.      WRITE: /1 'Report ID :', sy-cprog.
    WHEN 'E'.      WRITE: /1 LV_TEXT, SY-CPROG.
    WHEN '2'.      WRITE: /1 '###########  :', SY-CPROG.
  ENDCASE.

  L_STRLEN = STRLEN( L_CTEXT ).
*>######.
  PERFORM CUT_STRING_NUMBER USING L_STRLEN L_CTEXT.
*>######.

  L_POSITION = ( SY-LINSZ / 2 ) - ( L_STRLEN / 2 ).
  WRITE AT L_POSITION L_CTEXT.

  L_POSITION = SY-LINSZ - 20.
  L_DATUM_ENG   = SY-DATUM.
  L_YEAR_THAI   = SY-DATUM(4) + 543.
  CONCATENATE L_YEAR_THAI SY-DATUM+4(4)  INTO L_DATUM_THAI.
*--> CH09 : BEGIN INS
  CLEAR : LV_TEXT.
  PERFORM PF_GET_CONST_TEXT USING 'MAIN_DATE'
                            CHANGING LV_TEXT.
*<-- CH09 : END INS
  CASE SY-LANGU.
*    WHEN 'E'.      WRITE: AT l_position 'Date :', l_datum_eng."CH09 DEL
    WHEN 'E'.      WRITE: AT L_POSITION LV_TEXT, L_DATUM_ENG."CH09 INS
    WHEN '2'.      WRITE: AT L_POSITION '###### :', L_DATUM_THAI.
  ENDCASE.

*--------------------- Write heading 2nd line ------------------------*
*--> CH09 : BEGIN INS
  CLEAR : LV_TEXT.
  PERFORM PF_GET_CONST_TEXT USING 'MAIN_USER_NAME'
                            CHANGING LV_TEXT.
*<-- CH09 : END INS
  CASE SY-LANGU.
*    WHEN 'E'.      WRITE: /1 'User Name :', sy-uname.
    WHEN 'E'.      WRITE: /1 LV_TEXT, SY-UNAME.
    WHEN '2'.      WRITE: /1 '##########   :', SY-UNAME.
  ENDCASE.

  L_STRLEN = STRLEN( L_HTEXT1 ).
*>######.
  PERFORM CUT_STRING_NUMBER USING L_STRLEN L_HTEXT1.
*>######.
  L_POSITION = ( SY-LINSZ / 2 ) - ( L_STRLEN / 2 ).
  WRITE AT L_POSITION L_HTEXT1.

  L_POSITION = SY-LINSZ - 20.
*  write at l_position 'ISSUE FROM:'.
*  write ziso-isodat.
*--> CH09 : BEGIN INS
  CLEAR : LV_TEXT.
  PERFORM PF_GET_CONST_TEXT USING 'MAIN_TIME'
                            CHANGING LV_TEXT.
*<-- CH09 : END INS
  CASE SY-LANGU.
*    WHEN 'E'.    WRITE: AT l_position 'Time :', sy-uzeit."CH09 DEL
    WHEN 'E'.    WRITE: AT L_POSITION LV_TEXT, SY-UZEIT. "CH09 INS
    WHEN '2'.    WRITE: AT L_POSITION '####   :', SY-UZEIT.
  ENDCASE.

*--------------------- Write heading 3rd line ------------------------*
  CONCATENATE SY-SYSID '/' SY-MANDT  INTO  L_SYSCLIENT.
  CASE SY-LANGU.
    WHEN 'E'.    WRITE: /1  'Sys/Client:'   , L_SYSCLIENT.
    WHEN '2'.    WRITE: /1  '####/########:', L_SYSCLIENT.
  ENDCASE.

*  l_strlen = STRLEN( pl_htext2 ).
**>######.
*  PERFORM cut_string_number USING l_strlen pl_htext2.
**>######.
  L_HTEXT2 = PL_HTEXT2.
  L_STRLEN = STRLEN( L_HTEXT2 ).
*>######.
  PERFORM CUT_STRING_NUMBER USING L_STRLEN L_HTEXT2.
*>######.
  L_POSITION = ( SY-LINSZ / 2 ) - ( L_STRLEN / 2 ).
  WRITE AT L_POSITION PL_HTEXT2.

  L_POSITION = SY-LINSZ - 20.
*--> CH09 : BEGIN INS
  CLEAR : LV_TEXT.
  PERFORM PF_GET_CONST_TEXT USING 'MAIN_PAGE'
                            CHANGING LV_TEXT.
*<-- CH09 : END INS
  CASE SY-LANGU.
*    WHEN 'E'.    WRITE: AT l_position 'Page :', (7) sy-pagno NO-SIGN.
    WHEN 'E'.    WRITE: AT L_POSITION LV_TEXT, (7) SY-PAGNO NO-SIGN.
    WHEN '2'.    WRITE: AT L_POSITION '####   :', (7) SY-PAGNO NO-SIGN.
  ENDCASE.

ENDFORM.                    "report_header

*---------------------------------------------------------------------*
*       FORM PRINT_COL_HEAD                                           *
*---------------------------------------------------------------------*
*  Print Column header from text-element in each program. The program *
*  can 4 line maximum colunm header in 1 program. If you have more    *
*  than then should by leave this routine and write yourself.         *
*---------------------------------------------------------------------*
FORM PRINT_COL_HEAD.

  FORMAT COLOR COL_BACKGROUND  INTENSIFIED ON.
  READ TABLE T_TEXTPOOL  WITH KEY ID = 'H'.
  IF SY-SUBRC = 0.         ULINE.         ENDIF.

  FORMAT COLOR COL_HEADING  INTENSIFIED ON.
  LOOP AT T_TEXTPOOL  WHERE ID = 'H'.
    WRITE: / T_TEXTPOOL-ENTRY.
  ENDLOOP.

  CHECK SY-SUBRC = 0.
  FORMAT COLOR COL_BACKGROUND  INTENSIFIED ON.
  ULINE.

ENDFORM.                    "print_col_head

*&---------------------------------------------------------------------*
*&      Form  cut_string_number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_STRLEN  text
*      -->P_L_CTEXT  text
*----------------------------------------------------------------------*
FORM CUT_STRING_NUMBER  USING    PL_STRLEN
                                 PL_CTEXT.
  DATA: L_STRLEN1 TYPE I.
  FIELD-SYMBOLS <L_CHA_P1>.

  L_STRLEN1 = PL_STRLEN - 1.
  DO L_STRLEN1 TIMES.
    ASSIGN PL_CTEXT+SY-INDEX(1) TO <L_CHA_P1> TYPE 'X'.
    IF NOT <L_CHA_P1>  IS INITIAL.
      IF <L_CHA_P1> = '0E31' OR  "###########
         <L_CHA_P1> = '0E34' OR  "#####
         <L_CHA_P1> = '0E35' OR  "#####
         <L_CHA_P1> = '0E36' OR  "#####
         <L_CHA_P1> = '0E37' OR  "#####
         <L_CHA_P1> = '0E38' OR  "#####
         <L_CHA_P1> = '0E39' OR  "#####
         <L_CHA_P1> = '0E47' OR  "#########
         <L_CHA_P1> = '0E48' OR  "######
         <L_CHA_P1> = '0E49' OR  "#####
         <L_CHA_P1> = '0E4A' OR  "######
         <L_CHA_P1> = '0E4B' OR  "########
         <L_CHA_P1> = '0E4C'.    "#######
        PL_STRLEN = PL_STRLEN - 1.   "string
      ENDIF.
    ENDIF.
  ENDDO.
ENDFORM.                    " cut_string_number

*&---------------------------------------------------------------------*
*&      Form  modify_page_number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_PAGE_NUMBER .
  DATA: L_PAGE LIKE SY-PAGNO,
        L_LINE LIKE SY-PAGNO.
  DATA: L_TOTAL_PAGES_CHAR(5).

* Add for Page of total page
  TOTAL_PAGE = SY-PAGNO.
  WRITE: SY-PAGNO  TO  L_TOTAL_PAGES_CHAR(5)  NO-SIGN NO-GAP.

  DO TOTAL_PAGE TIMES.   " total page

    L_PAGE = SY-INDEX.

    DO.
      L_LINE = SY-INDEX.
      CLEAR TOT_PAGE.
      READ LINE L_LINE OF PAGE L_PAGE FIELD VALUE TOT_PAGE.

*     header have 1 line print 'page/total page'
      IF TOT_PAGE EQ '9,999'.
*        MODIFY LINE l_line OF PAGE l_page FIELD VALUE tot_page
*          FROM total_page.
        MODIFY LINE L_LINE OF PAGE L_PAGE FIELD VALUE TOT_PAGE
           FROM L_TOTAL_PAGES_CHAR.
        EXIT.
      ENDIF.

    ENDDO.

  ENDDO.

ENDFORM.                    " modify_page_number

*&---------------------------------------------------------------------*
*&      Form  report_header_page_of_tpage
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(PL_BUKRS)   text
*      -->VALUE(PL_HTEXT1)  text
*      -->VALUE(PL_HTEXT2)  text
*----------------------------------------------------------------------*
FORM REPORT_HEADER_PAGE_OF_TPAGE
                 USING VALUE(PL_BUKRS)  LIKE  T001-BUKRS
                       VALUE(PL_HTEXT1)
                       VALUE(PL_HTEXT2).
  DATA: L_CTEXT(100)    TYPE C,
        L_STRLEN        TYPE I,
        L_POSITION      TYPE I,
        L_HTEXT1(100),
        L_SYSCLIENT(20).
  DATA: L_DATUM_THAI   LIKE SY-DATUM,       "<-- for PWA
        L_YEAR_THAI(4) TYPE N,              "<-- for PWA
        L_DATUM_ENG    LIKE SY-DATUM.       "<-- for PWA
  DATA : LV_TEXT TYPE STRING. "CH09 INS
  SUMMARY.
*        Read Company name
  CLEAR: T001, ADRC, L_CTEXT.
  IF  NOT PL_BUKRS  IS  INITIAL.
    SELECT SINGLE  BUKRS BUTXT ADRNR
                   INTO CORRESPONDING FIELDS OF T001
                   FROM T001  WHERE BUKRS = PL_BUKRS.
    SELECT  ADDRNUMBER NAME1 NAME2
            INTO CORRESPONDING FIELDS OF ADRC
            FROM ADRC WHERE ADDRNUMBER  = T001-ADRNR
                      AND   DATE_FROM  <= SY-DATUM
                      AND   DATE_TO    >= SY-DATUM
                      AND   NATION      = SPACE.
      EXIT.
    ENDSELECT.
    IF SY-SUBRC  EQ 0.
      CONCATENATE ADRC-NAME1 ADRC-NAME2 INTO L_CTEXT SEPARATED BY SPACE.
    ELSE.
      L_CTEXT  = T001-BUTXT.
    ENDIF.
  ELSE.
    L_CTEXT =  PL_HTEXT2.
    CLEAR PL_HTEXT2.
  ENDIF.
*                                         Read Report name in Text-Pool
  IF T_TEXTPOOL[] IS INITIAL.
    READ TEXTPOOL SY-CPROG  INTO T_TEXTPOOL  LANGUAGE SY-LANGU.
  ENDIF.
  IF PL_HTEXT1  IS INITIAL.
    CLEAR T_TEXTPOOL.
    READ TABLE T_TEXTPOOL  WITH KEY ID = 'T'.
    IF SY-SUBRC NE 0.
      READ TABLE T_TEXTPOOL WITH KEY ID = 'R'.
    ENDIF.
    MOVE T_TEXTPOOL-ENTRY TO  L_HTEXT1.
  ELSE.
    MOVE PL_HTEXT1        TO  L_HTEXT1.
  ENDIF.
**                                 Read ISO document no. and Issued date
*  CLEAR ziso.
*  READ TABLE t_ziso  INTO ziso
*                     WITH KEY  bukrs  = pl_bukrs.

*--------------------- Write heading 1st line ------------------------*
*--> CH09 : BEGIN INS
  CLEAR : LV_TEXT.
  PERFORM PF_GET_CONST_TEXT USING 'MAIN_REPORT_ID'
                            CHANGING LV_TEXT.
*<-- CH09 : END INS
  CASE SY-LANGU.
*    WHEN 'E'.      WRITE: /1 'Report ID :', sy-cprog."CH09 DEL
    WHEN 'E'.      WRITE: /1 LV_TEXT, SY-CPROG. "CH09 INS
    WHEN '2'.      WRITE: /1 '###########  :', SY-CPROG.
  ENDCASE.

  L_STRLEN = STRLEN( L_CTEXT ).
*>??????
  PERFORM CUT_STRING_NUMBER USING L_STRLEN L_CTEXT.
*>??????

  L_POSITION = ( SY-LINSZ / 2 ) - ( L_STRLEN / 2 ).
  WRITE AT L_POSITION L_CTEXT.

  L_POSITION = SY-LINSZ - 20.
  L_DATUM_ENG   = SY-DATUM.
  L_YEAR_THAI   = SY-DATUM(4) + 543.
  CONCATENATE L_YEAR_THAI SY-DATUM+4(4)  INTO L_DATUM_THAI.
*--> CH09 : BEGIN INS
  CLEAR : LV_TEXT.
  PERFORM PF_GET_CONST_TEXT USING 'MAIN_DATE'
                            CHANGING LV_TEXT.
*<-- CH09 : END INS
  CASE SY-LANGU.
*    WHEN 'E'.      WRITE: AT l_position 'Date :', l_datum_eng.
    WHEN 'E'.      WRITE: AT L_POSITION LV_TEXT, L_DATUM_ENG.
    WHEN '2'.      WRITE: AT L_POSITION '###### :', L_DATUM_THAI.
  ENDCASE.

*--------------------- Write heading 2nd line ------------------------*
  CASE SY-LANGU.
    WHEN 'E'.      WRITE: /1 'User Name :', SY-UNAME.
    WHEN '2'.      WRITE: /1 '##########   :', SY-UNAME.
  ENDCASE.

  L_STRLEN = STRLEN( L_HTEXT1 ).
*>??????
  PERFORM CUT_STRING_NUMBER USING L_STRLEN L_HTEXT1.
*>??????
  L_POSITION = ( SY-LINSZ / 2 ) - ( L_STRLEN / 2 ).
  WRITE AT L_POSITION L_HTEXT1.

  L_POSITION = SY-LINSZ - 20.
*  write at l_position 'ISSUE FROM:'.
*  write ziso-isodat.
  CASE SY-LANGU.
    WHEN 'E'.    WRITE: AT L_POSITION 'Time :', SY-UZEIT.
    WHEN '2'.    WRITE: AT L_POSITION '####   :', SY-UZEIT.
  ENDCASE.

*--------------------- Write heading 3rd line ------------------------*
  CONCATENATE SY-SYSID '/' SY-MANDT  INTO  L_SYSCLIENT.
  CASE SY-LANGU.
    WHEN 'E'.    WRITE: /1  'Sys/Client:'   , L_SYSCLIENT.
    WHEN '2'.    WRITE: /1  '####/########:', L_SYSCLIENT.
  ENDCASE.

  L_STRLEN = STRLEN( PL_HTEXT2 ).
*>??????
  PERFORM CUT_STRING_NUMBER USING L_STRLEN PL_HTEXT2.
*>??????
  L_POSITION = ( SY-LINSZ / 2 ) - ( L_STRLEN / 2 ).
  WRITE AT L_POSITION PL_HTEXT2.

  L_POSITION = SY-LINSZ - 20.
*  WRITE AT l_position 'Page :'(h06).
  CASE SY-LANGU.
    WHEN 'E'.     WRITE: AT L_POSITION 'Page :'.
    WHEN '2'.     WRITE: AT L_POSITION '####  :'.
  ENDCASE.
  WRITE: (05) SY-PAGNO NO-SIGN,'/', TOT_PAGE.

ENDFORM.                    " report_header_page_of_tpage

*&---------------------------------------------------------------------*
*&      Form  end_of_report_page_of_tpage
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM END_OF_REPORT_PAGE_OF_TPAGE .

  FORMAT COLOR COL_BACKGROUND  INTENSIFIED ON.
  ULINE.
  CASE SY-LANGU.
    WHEN 'E'.
      WRITE AT /(sy-linsz) '* * * End of Report * * *'  CENTERED.
    WHEN '2'.
      WRITE AT /(sy-linsz) '* * * ######## * * *'  CENTERED.
  ENDCASE.

ENDFORM.                    " end_of_report_page_of_tpage

*&---------------------------------------------------------------------*
*&      Form  end_of_report_page_of_tpage
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM END_OF_REPORT_PAGE_OF_TPAGE01 .

  FORMAT COLOR COL_BACKGROUND  INTENSIFIED ON.
*  ULINE.          "(don't print ULINE)
  CASE SY-LANGU.
    WHEN 'E'.
      WRITE AT /(sy-linsz) '* * * End of Report * * *'  CENTERED.
    WHEN '2'.
      WRITE AT /(sy-linsz) '* * * ######## * * *'  CENTERED.
  ENDCASE.

ENDFORM.                    " end_of_report_page_of_tpage01

*&---------------------------------------------------------------------*
*&      Form  set_default_print_format
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(PL_FORMAT)  text
*----------------------------------------------------------------------*
FORM SET_DEFAULT_PRINT_FORMAT
                  USING VALUE(PL_FORMAT)  LIKE  PRI_PARAMS-PAART.

  CALL FUNCTION 'SET_PRINT_PARAMETERS'
    EXPORTING
*     ARCHIVE_ID            = C_CHAR_UNKNOWN
*     ARCHIVE_INFO          = C_CHAR_UNKNOWN
*     ARCHIVE_MODE          = C_CHAR_UNKNOWN
*     ARCHIVE_TEXT          = C_CHAR_UNKNOWN
*     AR_OBJECT             = C_CHAR_UNKNOWN
*     AUTHORITY             = C_CHAR_UNKNOWN
*     COPIES = C_NUM3_UNKNOWN
*     COVER_PAGE            = C_CHAR_UNKNOWN
*     DATA_SET              = C_CHAR_UNKNOWN
*     DEPARTMENT            = C_CHAR_UNKNOWN
*     DESTINATION           = C_CHAR_UNKNOWN
*     EXPIRATION            = C_NUM1_UNKNOWN
*     IMMEDIATELY           = C_CHAR_UNKNOWN
*     IN_ARCHIVE_PARAMETERS = ' '
*     IN_PARAMETERS         = ' '
      LAYOUT = PL_FORMAT
*     LINE_COUNT            = C_INT_UNKNOWN
*     LINE_SIZE             = C_INT_UNKNOWN
*     LIST_NAME             = C_CHAR_UNKNOWN
*     LIST_TEXT             = C_CHAR_UNKNOWN
*     NEW_LIST_ID           = C_CHAR_UNKNOWN
*     RECEIVER              = C_CHAR_UNKNOWN
*     RELEASE               = C_CHAR_UNKNOWN
*     SAP_COVER_PAGE        = C_CHAR_UNKNOWN
*     HOST_COVER_PAGE       = C_CHAR_UNKNOWN
*     PRIORITY              = C_NUM1_UNKNOWN
*     SAP_OBJECT            = C_CHAR_UNKNOWN
*     TYPE   = C_CHAR_UNKNOWN
*     foot_line =
*        C_CHAR_UNKNOWN
    .

ENDFORM.                    "set_default_print_format
*&---------------------------------------------------------------------*
*&      Form  set_default_print_format_cov
*&---------------------------------------------------------------------*
FORM SET_DEFAULT_PRINT_FORMAT_COV
                    USING VALUE(PL_FORMAT)  LIKE  PRI_PARAMS-PAART
                                PL_PRBIG    LIKE  PRI_PARAMS-PRBIG.

  CALL FUNCTION 'SET_PRINT_PARAMETERS'
    EXPORTING
*     ARCHIVE_ID = C_CHAR_UNKNOWN
*     ARCHIVE_INFO          = C_CHAR_UNKNOWN
*     ARCHIVE_MODE          = C_CHAR_UNKNOWN
*     ARCHIVE_TEXT          = C_CHAR_UNKNOWN
*     AR_OBJECT  = C_CHAR_UNKNOWN
*     AUTHORITY  = C_CHAR_UNKNOWN
*     COPIES     = C_NUM3_UNKNOWN
      COVER_PAGE = PL_PRBIG
*     DATA_SET   = C_CHAR_UNKNOWN
*     DEPARTMENT = C_CHAR_UNKNOWN
*     DESTINATION           = C_CHAR_UNKNOWN
*     EXPIRATION = C_NUM1_UNKNOWN
*     IMMEDIATELY           = C_CHAR_UNKNOWN
*     IN_ARCHIVE_PARAMETERS = ' '
*     IN_PARAMETERS         = ' '
      LAYOUT     = PL_FORMAT
*     LINE_COUNT = C_INT_UNKNOWN
*     LINE_SIZE  = C_INT_UNKNOWN
*     LIST_NAME  = C_CHAR_UNKNOWN
*     LIST_TEXT  = C_CHAR_UNKNOWN
*     NEW_LIST_ID           = C_CHAR_UNKNOWN
*     RECEIVER   = C_CHAR_UNKNOWN
*     RELEASE    = C_CHAR_UNKNOWN
*     SAP_COVER_PAGE        = C_CHAR_UNKNOWN
*     HOST_COVER_PAGE       = C_CHAR_UNKNOWN
*     PRIORITY   = C_NUM1_UNKNOWN
*     SAP_OBJECT = C_CHAR_UNKNOWN
*     TYPE       = C_CHAR_UNKNOWN
*     foot_line  =
*        C_CHAR_UNKNOWN
    .
ENDFORM.                    " set_default_print_format_cov
*&---------------------------------------------------------------------*
*&      Form  change_minus_amount
*&---------------------------------------------------------------------*
FORM CHANGE_MINUS_AMOUNT  USING    PL_AMOUNT
                                   PL_TEXT
                             VALUE(PL_NOZERO).

  IF  PL_NOZERO IS  INITIAL.
    WRITE PL_AMOUNT  TO   PL_TEXT.
  ELSE.
    WRITE PL_AMOUNT  TO   PL_TEXT NO-ZERO.
  ENDIF.

  SEARCH PL_TEXT FOR '-'.
  IF SY-SUBRC = 0 AND SY-FDPOS <> 0.
    MOVE:  '('        TO     PL_TEXT+0(1),
           ')'        TO     PL_TEXT+SY-FDPOS(1).
    CONDENSE   PL_TEXT NO-GAPS.
    SHIFT PL_TEXT RIGHT DELETING TRAILING SPACE.
  ENDIF.

ENDFORM.                    " change_minus_amount
*<<== CH01 : BEGIN
FORM SUB_GET_ROOT_ORG USING P_DATE TYPE SY-DATUM.
  DATA : BEGIN OF LT_TXTKEY OCCURS 0,
           OBJID TYPE HRP1002-OBJID,
           TABNR TYPE HRP1002-TABNR,
         END OF LT_TXTKEY.

  DATA : BEGIN OF LT_TXT OCCURS 0,
           TABNR TYPE HRT1002-TABNR,
           TLINE TYPE HRT1002-TLINE,
         END OF LT_TXT.

  REFRESH LT_TXTKEY.
  SELECT OBJID TABNR INTO TABLE LT_TXTKEY
  FROM  HRP1002
  WHERE OTYPE = 'O'
  AND   SUBTY = '9003'
  AND   BEGDA <= P_DATE
  AND   ENDDA >= P_DATE.
  IF LT_TXTKEY[] IS NOT INITIAL.
    SELECT TABNR TLINE INTO TABLE LT_TXT
    FROM  HRT1002
    FOR ALL ENTRIES IN LT_TXTKEY
    WHERE TABNR = LT_TXTKEY-TABNR
    AND   TABSEQNR = '1'.

    CLEAR GV_ROOTOBJ.
    SORT LT_TXT BY TLINE.
    READ TABLE LT_TXT WITH KEY TLINE = '00' BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      READ TABLE LT_TXTKEY WITH KEY TABNR = LT_TXT-TABNR.
      IF SY-SUBRC EQ 0.
        GV_ROOTOBJ = LT_TXTKEY-OBJID.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "SUB_GET_ROOT_ORG

*&---------------------------------------------------------------------*
*&      Form  GENERATE_EXCEL_FILE
*&---------------------------------------------------------------------*
FORM GENERATE_EXCEL_FILE TABLES  IT_PRINT
                         USING   IM_FILE_NAME
                                 IM_OPEN.

  DATA: L_FPATH TYPE STRING.
  DATA: LV_CODEPAGE1 TYPE CPCODEPAGE.
  DATA: LV_CODEPAGE2 TYPE ABAP_ENCODING.

  DATA EXCEL TYPE OLE2_OBJECT.
  DATA WORKBOOK TYPE OLE2_OBJECT.

  DATA LV_SAPWORKDIR TYPE STRING.
  DATA LV_TMP_PATH TYPE STRING.

  MOVE IM_FILE_NAME TO L_FPATH.
* get internal codepage from external codepage
  CALL FUNCTION 'SCP_CODEPAGE_BY_EXTERNAL_NAME'
    EXPORTING
      EXTERNAL_NAME = 'UTF-8'
    IMPORTING
      SAP_CODEPAGE  = LV_CODEPAGE1
    EXCEPTIONS
      NOT_FOUND     = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LV_CODEPAGE2 = LV_CODEPAGE1.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_SAPGUI_WORKDIR
    CHANGING
      SAPWORKDIR            = LV_SAPWORKDIR
    EXCEPTIONS
      GET_SAPWORKDIR_FAILED = 1
      CNTL_ERROR            = 2
      ERROR_NO_GUI          = 3
      NOT_SUPPORTED_BY_GUI  = 4
      OTHERS                = 5.
  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL METHOD CL_GUI_CFW=>UPDATE_VIEW
    EXCEPTIONS
      CNTL_SYSTEM_ERROR = 1
      CNTL_ERROR        = 2
      OTHERS            = 3.

  IF LV_SAPWORKDIR IS INITIAL.
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_DESKTOP_DIRECTORY
      CHANGING
        DESKTOP_DIRECTORY    = LV_SAPWORKDIR
      EXCEPTIONS
        CNTL_ERROR           = 1
        ERROR_NO_GUI         = 2
        NOT_SUPPORTED_BY_GUI = 3
        OTHERS               = 4.
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CALL METHOD CL_GUI_CFW=>UPDATE_VIEW
      EXCEPTIONS
        CNTL_SYSTEM_ERROR = 1
        CNTL_ERROR        = 2
        OTHERS            = 3.

    IF LV_SAPWORKDIR IS INITIAL.
      LV_SAPWORKDIR = 'C:\TEMP'.
    ENDIF.
  ENDIF.

  LV_TMP_PATH = L_FPATH.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                = LV_TMP_PATH
      FILETYPE                = 'ASC'
      WRITE_FIELD_SEPARATOR   = 'X'
      DAT_MODE                = 'X'
      CODEPAGE                = LV_CODEPAGE2
    TABLES
      DATA_TAB                = IT_PRINT
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      OTHERS                  = 22.
  IF SY-SUBRC <> 0.

  ENDIF.

  CREATE OBJECT EXCEL 'Excel.Application'.
  IF IM_OPEN IS NOT INITIAL.
    SET PROPERTY OF EXCEL 'Visible' = 1.
  ELSE.
    SET PROPERTY OF EXCEL 'Visible' = 0.
  ENDIF.

  CALL METHOD OF
    EXCEL
      'Workbooks' = WORKBOOK.
  CALL METHOD OF
    WORKBOOK
    'Open'
    EXPORTING
      #1 = LV_TMP_PATH.

  GET PROPERTY OF EXCEL   'ActiveWorkbook' = WORKBOOK.
  CALL METHOD OF
    WORKBOOK
    'SAVEAS'
    EXPORTING
      #1 = L_FPATH
      #2 = 1.
  IF SY-SUBRC NE 0.
    GET PROPERTY OF EXCEL   'ActiveWorkbook' = WORKBOOK.
  ENDIF.
  IF IM_OPEN IS INITIAL.
    CALL METHOD OF
      WORKBOOK
      'CLOSE'.
    CALL METHOD OF
      EXCEL
      'QUIT'.
  ENDIF.
  FREE OBJECT WORKBOOK.
  FREE OBJECT EXCEL.

  DATA : LV_RC TYPE I.
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
    EXPORTING
      FILENAME             = LV_TMP_PATH
    CHANGING
      RC                   = LV_RC
    EXCEPTIONS
      FILE_DELETE_FAILED   = 1
      CNTL_ERROR           = 2
      ERROR_NO_GUI         = 3
      FILE_NOT_FOUND       = 4
      ACCESS_DENIED        = 5
      UNKNOWN_ERROR        = 6
      NOT_SUPPORTED_BY_GUI = 7
      WRONG_PARAMETER      = 8
      OTHERS               = 9.
  IF SY-SUBRC <> 0.

  ENDIF.

ENDFORM.                    " GENERATE_EXCEL_FILE

*&---------------------------------------------------------------------*
*&      Form  TOP-OF-PAGE
*&---------------------------------------------------------------------*
*       Top of Page
*----------------------------------------------------------------------*
FORM TOP-OF-PAGE.

  DATA: LI_HEADER     TYPE SLIS_T_LISTHEADER.

  DATA: LV_STRING  TYPE STRING,
        LV_STRING2 TYPE STRING,
        LV_BEGDA   TYPE C LENGTH 10,
        LV_ENDDA   TYPE C LENGTH 10.
  DATA: LV_COUNT_TXT  TYPE C LENGTH 255.

  FIELD-SYMBOLS: <FS_HEADER> TYPE SLIS_LISTHEADER.

  PERFORM PF_SET_CURRENT_TIME_INFO.

  REFRESH LI_HEADER.
  APPEND INITIAL LINE TO LI_HEADER ASSIGNING <FS_HEADER>.
  <FS_HEADER>-TYP = 'H'.
  <FS_HEADER>-INFO = V_TOP_TITLE.
  UNASSIGN <FS_HEADER>.

  "--> Add - Additional Fields
  LOOP AT I_EXTRA_TOP ASSIGNING <FS_EXTRA_TOP>.

    CLEAR LV_STRING.
    LV_STRING = <FS_EXTRA_TOP>-KEY.

    CLEAR LV_STRING2.
    LV_STRING2 = <FS_EXTRA_TOP>-VALUE.

    APPEND INITIAL LINE TO LI_HEADER ASSIGNING <FS_HEADER>.
    <FS_HEADER>-TYP  = 'S'.
    CONCATENATE LV_STRING ':' INTO LV_STRING.
    CONCATENATE LV_STRING LV_STRING2
      INTO <FS_HEADER>-INFO SEPARATED BY SPACE.
    UNASSIGN <FS_HEADER>.
  ENDLOOP.

  APPEND INITIAL LINE TO LI_HEADER ASSIGNING <FS_HEADER>.
  <FS_HEADER>-TYP  = 'S'.

  CONCATENATE GC_RUN_DATE_H V_RUN_DATE INTO <FS_HEADER>-INFO SEPARATED BY SPACE.
  CONCATENATE <FS_HEADER>-INFO GC_RUN_TIME_H INTO <FS_HEADER>-INFO
                                             SEPARATED BY '   '.
  CONCATENATE <FS_HEADER>-INFO V_RUN_TIME INTO <FS_HEADER>-INFO SEPARATED BY SPACE.
  CONCATENATE <FS_HEADER>-INFO GC_USER_H INTO <FS_HEADER>-INFO
                                         SEPARATED BY '   '.
  CONCATENATE <FS_HEADER>-INFO SY-UNAME INTO <FS_HEADER>-INFO SEPARATED BY SPACE.

  UNASSIGN <FS_HEADER>.

  CLEAR: LV_BEGDA, LV_ENDDA.
  WRITE: V_BEGDA_1 TO LV_BEGDA DD/MM/YYYY,
         V_ENDDA_1 TO LV_ENDDA DD/MM/YYYY.

  "--> Period
  CLEAR LV_STRING.
  LV_STRING = V_PERIOD.
  IF V_PERIOD IS INITIAL.
    CLEAR LV_STRING.
    IF V_ENDDA_1 IS NOT INITIAL AND
       V_ENDDA_1 <> '00000000'  AND
       V_ENDDA_1 <> V_BEGDA_1.
      CONCATENATE LV_BEGDA '-' LV_ENDDA INTO LV_STRING
        SEPARATED BY SPACE.
    ELSE.
      LV_STRING = LV_BEGDA.
    ENDIF.
  ENDIF.

  IF W_CLOSE_FIELDS-PERIOD EQ ABAP_FALSE.
    APPEND INITIAL LINE TO LI_HEADER ASSIGNING <FS_HEADER>.
    <FS_HEADER>-TYP  = 'S'.

    CONCATENATE GC_PERIOD_H
                LV_STRING
                INTO <FS_HEADER>-INFO
                SEPARATED BY '   '.
    UNASSIGN <FS_HEADER>.
  ENDIF.

  "--> Add - Additional Fields
  LOOP AT I_EXTRA_HEADER_UPPER ASSIGNING <FS_EXTRA_HEADER>.
    CLEAR LV_STRING.
    LV_STRING = <FS_EXTRA_HEADER>-KEY.

    CLEAR LV_STRING2.
    LV_STRING2 = <FS_EXTRA_HEADER>-VALUE.

    APPEND INITIAL LINE TO LI_HEADER ASSIGNING <FS_HEADER>.
    <FS_HEADER>-TYP  = 'S'.
    CONCATENATE LV_STRING ':' INTO LV_STRING.
    CONCATENATE LV_STRING LV_STRING2
      INTO <FS_HEADER>-INFO SEPARATED BY SPACE.
    UNASSIGN <FS_HEADER>.
  ENDLOOP.

  IF W_CLOSE_FIELDS-TOT = ABAP_FALSE.
    APPEND INITIAL LINE TO LI_HEADER ASSIGNING <FS_HEADER>.
    <FS_HEADER>-TYP  = 'S'.

*    "--> Total Records
*    CLEAR LV_STRING.
*    CLEAR: LV_COUNT_TXT.
*    WRITE: V_COUNT_TOT_1 TO LV_COUNT_TXT.
*    CONDENSE: LV_COUNT_TXT.


*    APPEND INITIAL LINE TO LI_HEADER ASSIGNING <FS_HEADER>.
*    <FS_HEADER>-TYP  = 'S'.
*
*    CONCATENATE GC_TOT_REC_H
*                '       '
*                LV_COUNT_TXT
*                INTO <FS_HEADER>-INFO.
*    UNASSIGN <FS_HEADER>.
  ENDIF.

  "--> Completed Records
  CLEAR LV_STRING.
  CLEAR: LV_COUNT_TXT.
  WRITE: V_COUNT_COM_1 TO LV_COUNT_TXT.
  CONDENSE: LV_COUNT_TXT.

  IF W_CLOSE_FIELDS-COM = ABAP_FALSE.
    APPEND INITIAL LINE TO LI_HEADER ASSIGNING <FS_HEADER>.
    <FS_HEADER>-TYP  = 'S'.

    CONCATENATE GC_TOT_COM_H
                '       '
                LV_COUNT_TXT
                INTO <FS_HEADER>-INFO.
    UNASSIGN <FS_HEADER>.

  ENDIF.

  IF W_CLOSE_FIELDS-ERR = ABAP_FALSE.
    "--> Error Records
    CLEAR LV_STRING.
    CLEAR: LV_COUNT_TXT.
    WRITE: V_COUNT_ERR_1 TO LV_COUNT_TXT.
    CONDENSE: LV_COUNT_TXT.

    APPEND INITIAL LINE TO LI_HEADER ASSIGNING <FS_HEADER>.
    <FS_HEADER>-TYP  = 'S'.

    CONCATENATE GC_TOT_ERR_H
                LV_COUNT_TXT
      INTO <FS_HEADER>-INFO SEPARATED BY SPACE.
    UNASSIGN <FS_HEADER>.

  ENDIF.

  "--> Add - Additional Fields
  LOOP AT I_EXTRA_HEADER ASSIGNING <FS_EXTRA_HEADER>.

    CLEAR LV_STRING.
    LV_STRING = <FS_EXTRA_HEADER>-KEY.

    CLEAR LV_STRING2.
    LV_STRING2 = <FS_EXTRA_HEADER>-VALUE.

    APPEND INITIAL LINE TO LI_HEADER ASSIGNING <FS_HEADER>.
    <FS_HEADER>-TYP  = 'S'.
    CONCATENATE LV_STRING ':' INTO LV_STRING.
    CONCATENATE LV_STRING LV_STRING2
      INTO <FS_HEADER>-INFO SEPARATED BY SPACE.
    UNASSIGN <FS_HEADER>.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LI_HEADER.

ENDFORM.                    "top-of-page

*&---------------------------------------------------------------------*
*&      Form  HTML_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->DOCUMENT   text
*----------------------------------------------------------------------*
FORM HTML_TOP_OF_PAGE USING DOCUMENT TYPE REF TO CL_DD_DOCUMENT.

  DATA: R_DD_TABLE TYPE REF TO CL_DD_TABLE_AREA,
        R_DOCTABLE TYPE REF TO CL_DD_TABLE_ELEMENT.

  DATA: LV_TEXT      TYPE SDYDO_TEXT_ELEMENT,
        LV_BEGDA_1   TYPE C LENGTH 10,
        LV_ENDDA_1   TYPE C LENGTH 10,
        LV_WIDTH     TYPE SDYDO_VALUE,
        LV_SAP_ALIGN TYPE SDYDO_ATTRIBUTE.

*  FIELD-SYMBOLS: <fs_header> TYPE slis_listheader."CH08 DEL

  CONSTANTS: LC_COL_1 TYPE SDYDO_VALUE VALUE '250',
             LC_COL_2 TYPE SDYDO_VALUE VALUE '55'.

  PERFORM PF_SET_CURRENT_TIME_INFO.

  CLEAR LV_TEXT.
  LV_TEXT = V_TOP_TITLE.

  IF LV_TEXT IS NOT INITIAL.
    "add heading
    CALL METHOD DOCUMENT->ADD_TEXT
      EXPORTING
        TEXT      = LV_TEXT
        SAP_STYLE = CL_DD_AREA=>HEADING.

    "New line
    CALL METHOD DOCUMENT->NEW_LINE.
    CALL METHOD DOCUMENT->NEW_LINE.
  ENDIF.

**  "Dynamic Add Extra Field in ALV Header
**  LOOP AT I_EXTRA_TOP ASSIGNING <FS_EXTRA_TOP>.
**    AT FIRST.
**      IF W_CLOSE_NEWLINE-EXTRA_TOP_FIRST IS INITIAL.
**        CALL METHOD DOCUMENT->NEW_LINE.
**      ENDIF.
**    ENDAT.
**
**    "Create Tables
**    CLEAR: R_DOCTABLE, R_DD_TABLE.
**    CALL METHOD DOCUMENT->ADD_TABLE
**      EXPORTING
**        NO_OF_COLUMNS = 2
**        BORDER        = '0'
**      IMPORTING
**        TABLE         = R_DOCTABLE
**        TABLEAREA     = R_DD_TABLE.
**
**    "Get Width - 1st Column
**    CLEAR LV_WIDTH.
**    IF <FS_EXTRA_TOP>-1ST_WIDTH IS NOT INITIAL.
**      LV_WIDTH = <FS_EXTRA_TOP>-1ST_WIDTH.
**    ELSE.
**      LV_WIDTH = LC_COL_1.
**    ENDIF.
**
**    "Set Width - 1st Column
**    CALL METHOD R_DD_TABLE->SET_COLUMN_WIDTH
**      EXPORTING
**        COL_NO = 1
**        WIDTH  = LV_WIDTH.
**
**    "Set Align - 1st Column
**    CLEAR LV_SAP_ALIGN.
**    IF <FS_EXTRA_TOP>-1ST_ALIGN IS NOT INITIAL.
**      LV_SAP_ALIGN = <FS_EXTRA_TOP>-1ST_ALIGN.
**    ELSE.
**      LV_SAP_ALIGN = 'LEFT'.
**    ENDIF.
**
**    "Set Align - 1st Column
**    CALL METHOD R_DOCTABLE->SET_COLUMN_STYLE
**      EXPORTING
**        COL_NO    = 1
**        SAP_ALIGN = LV_SAP_ALIGN.
**
**    "Get Width - 2nd Column
**    CLEAR LV_WIDTH.
**    IF <FS_EXTRA_TOP>-2ND_WIDTH IS NOT INITIAL.
**      LV_WIDTH = <FS_EXTRA_TOP>-2ND_WIDTH.
**    ELSE.
**      LV_WIDTH = LC_COL_2.
**    ENDIF.
**
**    "Set Width - 2nd Column
**    CALL METHOD R_DD_TABLE->SET_COLUMN_WIDTH
**      EXPORTING
**        COL_NO = 2
**        WIDTH  = LV_WIDTH.
**
**    "Set Align - 2nd Column
**    CLEAR LV_SAP_ALIGN.
**    IF <FS_EXTRA_TOP>-2ND_ALIGN IS NOT INITIAL.
**      LV_SAP_ALIGN = <FS_EXTRA_TOP>-2ND_ALIGN.
**    ELSE.
**      LV_SAP_ALIGN = 'LEFT'.
**    ENDIF.
**
**    "Set Align - 2nd Column
**    CALL METHOD R_DOCTABLE->SET_COLUMN_STYLE
**      EXPORTING
**        COL_NO    = 2
**        SAP_ALIGN = LV_SAP_ALIGN.
**
**    CLEAR LV_TEXT.
**    CONCATENATE <FS_EXTRA_TOP>-KEY ':' INTO LV_TEXT.
**
**    CALL METHOD R_DD_TABLE->ADD_TEXT
**      EXPORTING
**        TEXT         = LV_TEXT
**        SAP_EMPHASIS = CL_DD_DOCUMENT=>STRONG
**        STYLE_CLASS  = SPACE.
**
**    CLEAR LV_TEXT.
**    LV_TEXT = <FS_EXTRA_TOP>-VALUE.
**
**    CALL METHOD R_DD_TABLE->ADD_TEXT
**      EXPORTING
**        TEXT = LV_TEXT.
**
**    CALL METHOD R_DD_TABLE->NEW_ROW.
**  ENDLOOP.
**
**  "add quick table with two columns
**  CALL METHOD DOCUMENT->ADD_TABLE
**    EXPORTING
**      NO_OF_COLUMNS = 6
**      BORDER        = '0'
**      WIDTH         = '50%'  "you can set any width of top area in %
**    IMPORTING
**      TABLEAREA     = R_DD_TABLE.
**
**  "Row - Header
***  CALL METHOD R_DD_TABLE->NEW_ROW.
**
****  Run Date
***  CALL METHOD R_DD_TABLE->ADD_TEXT
***    EXPORTING
***      TEXT         = GC_RUN_DATE_H
***      SAP_EMPHASIS = CL_DD_DOCUMENT=>STRONG
***      STYLE_CLASS  = SPACE.
***
***  CLEAR LV_TEXT.
***  LV_TEXT = V_RUN_DATE.
***
***  CALL METHOD R_DD_TABLE->ADD_TEXT
***    EXPORTING
***      TEXT = LV_TEXT.
***
****  Run Time
***  CALL METHOD R_DD_TABLE->ADD_TEXT
***    EXPORTING
***      TEXT         = GC_RUN_TIME_H
***      SAP_EMPHASIS = CL_DD_DOCUMENT=>STRONG
***      STYLE_CLASS  = SPACE.
***
***  CLEAR LV_TEXT.
***  LV_TEXT = V_RUN_TIME.
***
***  CALL METHOD R_DD_TABLE->ADD_TEXT
***    EXPORTING
***      TEXT = LV_TEXT.
***
****  User
***  CALL METHOD R_DD_TABLE->ADD_TEXT
***    EXPORTING
***      TEXT         = GC_USER_H
***      SAP_EMPHASIS = CL_DD_DOCUMENT=>STRONG
***      STYLE_CLASS  = SPACE.
***
***  CLEAR LV_TEXT.
***  LV_TEXT = SY-UNAME.
**
***  CALL METHOD R_DD_TABLE->ADD_TEXT
***    EXPORTING
***      TEXT = LV_TEXT.
**
***  "Append - 1st Row
***  CALL METHOD R_DD_TABLE->NEW_ROW.
***
***  CLEAR: LV_BEGDA_1, LV_ENDDA_1.
***  WRITE: V_BEGDA_1 TO LV_BEGDA_1 DD/MM/YYYY,
***         V_ENDDA_1 TO LV_ENDDA_1 DD/MM/YYYY.
***
***  CLEAR LV_TEXT.
***  IF V_ENDDA_1 IS NOT INITIAL AND
***     V_ENDDA_1 <> '00000000'  AND
***     V_ENDDA_1 <> V_BEGDA_1.
***    CONCATENATE LV_BEGDA_1 '-' LV_ENDDA_1 INTO LV_TEXT
***      SEPARATED BY SPACE.
***  ELSE.
***    LV_TEXT = LV_BEGDA_1.
***  ENDIF.
***
***  "--> Create Table - 2nd Row
***  CLEAR: R_DOCTABLE, R_DD_TABLE.
***  CALL METHOD DOCUMENT->ADD_TABLE
***    EXPORTING
***      NO_OF_COLUMNS = 2
***      BORDER        = '0'
***    IMPORTING
***      TABLE         = R_DOCTABLE
***      TABLEAREA     = R_DD_TABLE.
**
**  "Set Width - 1st Column
**  CALL METHOD R_DD_TABLE->SET_COLUMN_WIDTH
**    EXPORTING
**      COL_NO = 1
**      WIDTH  = GC_COL_WIDTH.
**
**  IF W_CLOSE_FIELDS-PERIOD = ABAP_FALSE.
**
**    "Set Width - 2nd Column
**    CALL METHOD R_DOCTABLE->SET_COLUMN_STYLE
**      EXPORTING
**        COL_NO    = 2
**        SAP_ALIGN = 'LEFT'.
**
**    CALL METHOD R_DD_TABLE->ADD_TEXT
**      EXPORTING
**        TEXT         = GC_PERIOD_H
**        SAP_EMPHASIS = CL_DD_DOCUMENT=>STRONG
**        STYLE_CLASS  = SPACE.
**
**    CLEAR LV_TEXT.
**    LV_TEXT = V_PERIOD.
**    IF V_PERIOD IS INITIAL.
**      CLEAR: LV_BEGDA_1, LV_ENDDA_1.
**      WRITE: V_BEGDA_1 TO LV_BEGDA_1 DD/MM/YYYY,
**             V_ENDDA_1 TO LV_ENDDA_1 DD/MM/YYYY.
**
**      CLEAR LV_TEXT.
**      IF V_ENDDA_1 IS NOT INITIAL AND
**         V_ENDDA_1 <> '00000000'  AND
**         V_ENDDA_1 <> V_BEGDA_1.
**        CONCATENATE LV_BEGDA_1 '-' LV_ENDDA_1 INTO LV_TEXT
**          SEPARATED BY SPACE.
**      ELSE.
**        LV_TEXT = LV_BEGDA_1.
**      ENDIF.
**    ENDIF.
**    CALL METHOD R_DD_TABLE->ADD_TEXT
**      EXPORTING
**        TEXT = LV_TEXT.
**
**    "Append - 2nd Row
**    CALL METHOD R_DD_TABLE->NEW_ROW.
**  ENDIF.
**
**  "Dynamic Add Extra Field in ALV Header
**  LOOP AT I_EXTRA_HEADER_UPPER ASSIGNING <FS_EXTRA_HEADER>.
***    AT FIRST.
***      CALL METHOD document->new_line.
***    ENDAT.
**    "Create Tables
**    CLEAR: R_DOCTABLE, R_DD_TABLE.
**    CALL METHOD DOCUMENT->ADD_TABLE
**      EXPORTING
**        NO_OF_COLUMNS = 2
**        BORDER        = '0'
**      IMPORTING
**        TABLE         = R_DOCTABLE
**        TABLEAREA     = R_DD_TABLE.
**
**    "Get Width - 1st Column
**    CLEAR LV_WIDTH.
**    IF <FS_EXTRA_HEADER>-1ST_WIDTH IS NOT INITIAL.
**      LV_WIDTH = <FS_EXTRA_HEADER>-1ST_WIDTH.
**    ELSE.
**      LV_WIDTH = LC_COL_1.
**    ENDIF.
**
**    "Set Width - 1st Column
**    CALL METHOD R_DD_TABLE->SET_COLUMN_WIDTH
**      EXPORTING
**        COL_NO = 1
**        WIDTH  = LV_WIDTH.
**
**    "Set Align - 1st Column
**    CLEAR LV_SAP_ALIGN.
**    IF <FS_EXTRA_HEADER>-1ST_ALIGN IS NOT INITIAL.
**      LV_SAP_ALIGN = <FS_EXTRA_HEADER>-1ST_ALIGN.
**    ELSE.
**      LV_SAP_ALIGN = 'LEFT'.
**    ENDIF.
**
**    "Set Align - 1st Column
**    CALL METHOD R_DOCTABLE->SET_COLUMN_STYLE
**      EXPORTING
**        COL_NO    = 1
**        SAP_ALIGN = LV_SAP_ALIGN.
**
**    "Get Width - 2nd Column
**    CLEAR LV_WIDTH.
**    IF <FS_EXTRA_HEADER>-2ND_WIDTH IS NOT INITIAL.
**      LV_WIDTH = <FS_EXTRA_HEADER>-2ND_WIDTH.
**    ELSE.
**      LV_WIDTH = LC_COL_2.
**    ENDIF.
**
**    "Set Width - 2nd Column
**    CALL METHOD R_DD_TABLE->SET_COLUMN_WIDTH
**      EXPORTING
**        COL_NO = 2
**        WIDTH  = LV_WIDTH.
**
**    "Set Align - 2nd Column
**    CLEAR LV_SAP_ALIGN.
**    IF <FS_EXTRA_HEADER>-2ND_ALIGN IS NOT INITIAL.
**      LV_SAP_ALIGN = <FS_EXTRA_HEADER>-2ND_ALIGN.
**    ELSE.
**      LV_SAP_ALIGN = 'LEFT'.
**    ENDIF.
**
**    "Set Align - 2nd Column
**    CALL METHOD R_DOCTABLE->SET_COLUMN_STYLE
**      EXPORTING
**        COL_NO    = 2
**        SAP_ALIGN = LV_SAP_ALIGN.
**
**    CLEAR LV_TEXT.
**    CONCATENATE <FS_EXTRA_HEADER>-KEY ':' INTO LV_TEXT.
**
**    CALL METHOD R_DD_TABLE->ADD_TEXT
**      EXPORTING
**        TEXT         = LV_TEXT
**        SAP_EMPHASIS = CL_DD_DOCUMENT=>STRONG
**        STYLE_CLASS  = SPACE.
**
**    CLEAR LV_TEXT.
**    LV_TEXT = <FS_EXTRA_HEADER>-VALUE.
**
**    CALL METHOD R_DD_TABLE->ADD_TEXT
**      EXPORTING
**        TEXT = LV_TEXT.
**
**    CALL METHOD R_DD_TABLE->NEW_ROW.
**  ENDLOOP.
**
**  "New Line
**  CALL METHOD DOCUMENT->NEW_LINE.
**
***  IF W_CLOSE_FIELDS-TOT = ABAP_FALSE.
***    "--> Create Table - 3rd Row
***    CLEAR: R_DOCTABLE, R_DD_TABLE.
***    CALL METHOD DOCUMENT->ADD_TABLE
***      EXPORTING
***        NO_OF_COLUMNS = 2
***        BORDER        = '0'
***      IMPORTING
***        TABLE         = R_DOCTABLE
***        TABLEAREA     = R_DD_TABLE.
***
***    "Set Width - 1st Column
***    CALL METHOD R_DD_TABLE->SET_COLUMN_WIDTH
***      EXPORTING
***        COL_NO = 1
***        WIDTH  = LC_COL_1.
***
***    "Set Width - 2nd Column
***    CALL METHOD R_DD_TABLE->SET_COLUMN_WIDTH
***      EXPORTING
***        COL_NO = 2
***        WIDTH  = LC_COL_2.
***
***    "Set Align - 2nd Column
***    CALL METHOD R_DOCTABLE->SET_COLUMN_STYLE
***      EXPORTING
***        COL_NO    = 2
***        SAP_ALIGN = 'RIGHT'.
***
***    CALL METHOD R_DD_TABLE->ADD_TEXT
***      EXPORTING
***        TEXT         = GC_TOT_REC_H
***        SAP_EMPHASIS = CL_DD_DOCUMENT=>STRONG
***        STYLE_CLASS  = SPACE.
***
***    CLEAR LV_TEXT.
***    WRITE: V_COUNT_TOT_1 TO LV_TEXT.
***    CONDENSE: LV_TEXT.
***
***    CALL METHOD R_DD_TABLE->ADD_TEXT
***      EXPORTING
***        TEXT = LV_TEXT.
***
***    "Append - 3rd Row
***    CALL METHOD R_DD_TABLE->NEW_ROW.
***  ENDIF.
***
***  IF W_CLOSE_FIELDS-COM = ABAP_FALSE.
***
***    "--> Create Table - 4th Row
***    CLEAR: R_DOCTABLE, R_DD_TABLE.
***    CALL METHOD DOCUMENT->ADD_TABLE
***      EXPORTING
***        NO_OF_COLUMNS = 2
***        BORDER        = '0'
***      IMPORTING
***        TABLE         = R_DOCTABLE
***        TABLEAREA     = R_DD_TABLE.
***
***    "Set Width - 1st Column
***    CALL METHOD R_DD_TABLE->SET_COLUMN_WIDTH
***      EXPORTING
***        COL_NO = 1
***        WIDTH  = LC_COL_1.
***
***    "Set Width - 2nd Column
***    CALL METHOD R_DD_TABLE->SET_COLUMN_WIDTH
***      EXPORTING
***        COL_NO = 2
***        WIDTH  = LC_COL_2.
***
***    "Set Align - 2nd Column
***    CALL METHOD R_DOCTABLE->SET_COLUMN_STYLE
***      EXPORTING
***        COL_NO    = 2
***        SAP_ALIGN = 'RIGHT'.
***
***    "Total Records - Success
***    CALL METHOD R_DD_TABLE->ADD_TEXT
***      EXPORTING
***        TEXT         = GC_TOT_COM_H
***        SAP_EMPHASIS = CL_DD_DOCUMENT=>STRONG
***        STYLE_CLASS  = SPACE.
***
***    CLEAR LV_TEXT.
***    WRITE: V_COUNT_COM_1 TO LV_TEXT.
***    CONDENSE: LV_TEXT.
***
***    CALL METHOD R_DD_TABLE->ADD_TEXT
***      EXPORTING
***        TEXT = LV_TEXT.
***
***    "Append - 4th Row
***    CALL METHOD R_DD_TABLE->NEW_ROW.
***  ENDIF.
***
***  IF W_CLOSE_FIELDS-ERR = ABAP_FALSE.
***
***    "Create Table - 5th
***    CLEAR: R_DOCTABLE, R_DD_TABLE.
***    CALL METHOD DOCUMENT->ADD_TABLE
***      EXPORTING
***        NO_OF_COLUMNS = 2
***        BORDER        = '0'
***      IMPORTING
***        TABLE         = R_DOCTABLE
***        TABLEAREA     = R_DD_TABLE.
***
***    "Set Width - 1st Column
***    CALL METHOD R_DD_TABLE->SET_COLUMN_WIDTH
***      EXPORTING
***        COL_NO = 1
***        WIDTH  = LC_COL_1.
***
***    "Set Width - 2nd Column
***    CALL METHOD R_DD_TABLE->SET_COLUMN_WIDTH
***      EXPORTING
***        COL_NO = 2
***        WIDTH  = LC_COL_2.
***
***    "Set Align - 2nd Column
***    CALL METHOD R_DOCTABLE->SET_COLUMN_STYLE
***      EXPORTING
***        COL_NO    = 2
***        SAP_ALIGN = 'RIGHT'.
***
***    CALL METHOD R_DD_TABLE->ADD_TEXT
***      EXPORTING
***        TEXT         = GC_TOT_ERR_H
***        SAP_EMPHASIS = CL_DD_DOCUMENT=>STRONG
***        STYLE_CLASS  = SPACE.
***
***    CLEAR LV_TEXT.
***    WRITE: V_COUNT_ERR_1 TO LV_TEXT.
***    CONDENSE: LV_TEXT.
***
***    CALL METHOD R_DD_TABLE->ADD_TEXT
***      EXPORTING
***        TEXT = LV_TEXT.
***
***    "Append - 5th Row
***    CALL METHOD R_DD_TABLE->NEW_ROW.
***  ENDIF.
**
**  "Dynamic Add Extra Field in ALV Header
**  LOOP AT I_EXTRA_HEADER ASSIGNING <FS_EXTRA_HEADER>.
**    AT FIRST.
**      CALL METHOD DOCUMENT->NEW_LINE.
**    ENDAT.
**
**    "Create Tables
**    CLEAR: R_DOCTABLE, R_DD_TABLE.
**    CALL METHOD DOCUMENT->ADD_TABLE
**      EXPORTING
**        NO_OF_COLUMNS = 2
**        BORDER        = '0'
**      IMPORTING
**        TABLE         = R_DOCTABLE
**        TABLEAREA     = R_DD_TABLE.
**
**    "Get Width - 1st Column
**    CLEAR LV_WIDTH.
**    IF <FS_EXTRA_HEADER>-1ST_WIDTH IS NOT INITIAL.
**      LV_WIDTH = <FS_EXTRA_HEADER>-1ST_WIDTH.
**    ELSE.
**      LV_WIDTH = LC_COL_1.
**    ENDIF.
**
**    "Set Width - 1st Column
**    CALL METHOD R_DD_TABLE->SET_COLUMN_WIDTH
**      EXPORTING
**        COL_NO = 1
**        WIDTH  = LV_WIDTH.
**
**    "Set Align - 1st Column
**    CLEAR LV_SAP_ALIGN.
**    IF <FS_EXTRA_HEADER>-1ST_ALIGN IS NOT INITIAL.
**      LV_SAP_ALIGN = <FS_EXTRA_HEADER>-1ST_ALIGN.
**    ELSE.
**      LV_SAP_ALIGN = 'LEFT'.
**    ENDIF.
**
**    "Set Align - 1st Column
**    CALL METHOD R_DOCTABLE->SET_COLUMN_STYLE
**      EXPORTING
**        COL_NO    = 1
**        SAP_ALIGN = LV_SAP_ALIGN.
**
**    "Get Width - 2nd Column
**    CLEAR LV_WIDTH.
**    IF <FS_EXTRA_HEADER>-2ND_WIDTH IS NOT INITIAL.
**      LV_WIDTH = <FS_EXTRA_HEADER>-2ND_WIDTH.
**    ELSE.
**      LV_WIDTH = LC_COL_2.
**    ENDIF.
**
**    "Set Width - 2nd Column
**    CALL METHOD R_DD_TABLE->SET_COLUMN_WIDTH
**      EXPORTING
**        COL_NO = 2
**        WIDTH  = LV_WIDTH.
**
**    "Set Align - 2nd Column
**    CLEAR LV_SAP_ALIGN.
**    IF <FS_EXTRA_HEADER>-2ND_ALIGN IS NOT INITIAL.
**      LV_SAP_ALIGN = <FS_EXTRA_HEADER>-2ND_ALIGN.
**    ELSE.
**      LV_SAP_ALIGN = 'LEFT'.
**    ENDIF.
**
**    "Set Align - 2nd Column
**    CALL METHOD R_DOCTABLE->SET_COLUMN_STYLE
**      EXPORTING
**        COL_NO    = 2
**        SAP_ALIGN = LV_SAP_ALIGN.
**
**    CLEAR LV_TEXT.
**    CONCATENATE <FS_EXTRA_HEADER>-KEY ':' INTO LV_TEXT.
**
**    CALL METHOD R_DD_TABLE->ADD_TEXT
**      EXPORTING
**        TEXT         = LV_TEXT
**        SAP_EMPHASIS = CL_DD_DOCUMENT=>STRONG
**        STYLE_CLASS  = SPACE.
**
**    CLEAR LV_TEXT.
**    LV_TEXT = <FS_EXTRA_HEADER>-VALUE.
**
**    CALL METHOD R_DD_TABLE->ADD_TEXT
**      EXPORTING
**        TEXT = LV_TEXT.
**
**    CALL METHOD R_DD_TABLE->NEW_ROW.
**  ENDLOOP.

ENDFORM.                    "html_top_of_page

*&---------------------------------------------------------------------*
*&      Form  pf_get_file_name_pc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->EV_FILE    text
*----------------------------------------------------------------------*
FORM PF_OPEN_FILE_NAME_PC CHANGING EV_FILE TYPE RLGRAP-FILENAME.

  DATA: LV_FILENAME TYPE STRING,
        LV_PATH     TYPE STRING,
        LV_FULLPATH TYPE STRING.

  CLEAR: EV_FILE,
         LV_FILENAME,
         LV_PATH,
         LV_FULLPATH.

  DATA: LV_DESKTOP_DIR TYPE STRING.

  IF LV_FILENAME IS NOT INITIAL.
    CLEAR : LV_FILENAME.
  ENDIF.

  IF LV_PATH IS NOT INITIAL.
    CLEAR LV_PATH.
  ENDIF.

  IF LV_FULLPATH IS NOT INITIAL.
    CLEAR : LV_FULLPATH.
  ENDIF.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_DESKTOP_DIRECTORY
    CHANGING
      DESKTOP_DIRECTORY    = LV_DESKTOP_DIR
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.

  DATA : LI_FILE_TAB TYPE FILETABLE,
         LW_FILE_TAB LIKE LINE OF LI_FILE_TAB,
         LV_RC       TYPE I.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = 'Open file'
      DEFAULT_EXTENSION       = V_DEF_EXTEN
*     default_filename        = v_def_file_n   "-CH17
      DEFAULT_FILENAME        = SPACE           "+CH17
      FILE_FILTER             = V_FILE_FILTER
*     WITH_ENCODING           =
      INITIAL_DIRECTORY       = V_INIT_PATH
      MULTISELECTION          = ABAP_FALSE
    CHANGING
      FILE_TABLE              = LI_FILE_TAB
      RC                      = LV_RC
*     USER_ACTION             =
*     FILE_ENCODING           =
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.
  IF SY-SUBRC EQ 0.
    READ TABLE LI_FILE_TAB INTO LW_FILE_TAB INDEX 1.
    EV_FILE = LW_FILE_TAB.
  ENDIF.

ENDFORM.                    "pf_get_file_name_pc

*&---------------------------------------------------------------------*
*&      Form  pf_get_file_name_pc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->EV_FILE    text
*----------------------------------------------------------------------*
FORM PF_GET_FILE_NAME_PC CHANGING EV_FILE TYPE RLGRAP-FILENAME.

  DATA: LV_FILENAME TYPE STRING,
        LV_PATH     TYPE STRING,
        LV_FULLPATH TYPE STRING.

  CLEAR: EV_FILE,
         LV_FILENAME,
         LV_PATH,
         LV_FULLPATH.

  IF V_INIT_PATH IS INITIAL OR EV_FILE IS NOT INITIAL.
    V_INIT_PATH = EV_FILE.
  ENDIF.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
      WINDOW_TITLE         = 'File Selection'
      DEFAULT_FILE_NAME    = V_DEF_FILE_N
      FILE_FILTER          = V_FILE_FILTER
*     FILE_FILTER          = CL_GUI_FRONTEND_SERVICES=>FILETYPE_TEXT
      INITIAL_DIRECTORY    = V_INIT_PATH
    CHANGING
      FILENAME             = LV_FILENAME
      PATH                 = LV_PATH
      FULLPATH             = LV_FULLPATH
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
*     invalid_default_file_name = 4
      OTHERS               = 5.
  IF SY-SUBRC = 0.
    EV_FILE = LV_FULLPATH.
  ENDIF.

ENDFORM.                    "pf_get_file_name_pc

*&---------------------------------------------------------------------*
*&      Form  pf_get_directory_name_pc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_INIT_FOLDER  text
*      -->EV_DIRECTORY    text
*----------------------------------------------------------------------*
FORM PF_GET_DIRECTORY_NAME_PC USING IV_INIT_FOLDER TYPE STRING
                           CHANGING EV_DIRECTORY TYPE RLGRAP-FILENAME.

*  DATA: li_file_tab TYPE filetable.

  DATA: LV_TITLE  TYPE STRING,
        LV_FOLDER TYPE STRING.

  "Set - Popup Title
  CLEAR LV_TITLE.
  LV_TITLE = TEXT-T01.

  IF IV_INIT_FOLDER IS INITIAL.
    PERFORM: PF_GET_INIT_PATH CHANGING IV_INIT_FOLDER.
  ENDIF.

  CLEAR LV_FOLDER.
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE
    EXPORTING
      WINDOW_TITLE         = LV_TITLE
      INITIAL_FOLDER       = IV_INIT_FOLDER
    CHANGING
      SELECTED_FOLDER      = LV_FOLDER
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC = 0.
    EV_DIRECTORY = LV_FOLDER.
  ENDIF.

ENDFORM.                    "pf_get_directory_name_pc

*&---------------------------------------------------------------------*
*&      Form  pf_get_file_name_sv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->EV_FILE  text
*----------------------------------------------------------------------*
FORM PF_GET_FILE_NAME_SV CHANGING EV_FILE TYPE RLGRAP-FILENAME.

  DATA LV_SVFILE TYPE RLGRAP-FILENAME.

  CLEAR LV_SVFILE.
  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    IMPORTING
      SERVERFILE       = LV_SVFILE
    EXCEPTIONS
      CANCELED_BY_USER = 1
      OTHERS           = 2.
  IF SY-SUBRC = 0 AND LV_SVFILE IS NOT INITIAL.
    EV_FILE = LV_SVFILE.
  ENDIF.

ENDFORM.                    "pf_get_file_name_sv

*&---------------------------------------------------------------------*
*&      Form  pf_on_request_for_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->EV_VAR     text
*----------------------------------------------------------------------*
FORM PF_ON_REQUEST_FOR_VARIANT CHANGING EV_VAR TYPE DISVARIANT-VARIANT.

  DATA: LW_IN_VARIANT  TYPE DISVARIANT,
        LW_OUT_VARIANT TYPE DISVARIANT.

  CLEAR LW_IN_VARIANT.
  LW_IN_VARIANT-REPORT = SY-REPID.

  CLEAR LW_OUT_VARIANT.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT    = LW_IN_VARIANT
      I_SAVE        = 'A'
    IMPORTING
      ES_VARIANT    = LW_OUT_VARIANT
    EXCEPTIONS
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.
  IF SY-SUBRC EQ 0 .
    EV_VAR = LW_OUT_VARIANT-VARIANT.
  ENDIF.

ENDFORM.                    "pf_on_request_for_variant

*&---------------------------------------------------------------------*
*&      Form  pf_set_screen_for_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_RAD1    text
*----------------------------------------------------------------------*
FORM PF_SET_SCREEN_FOR_INPUT USING IV_RAD1 TYPE FLAG.

  LOOP AT SCREEN.
    IF IV_RAD1 = ABAP_TRUE.
      IF SCREEN-NAME CP '*P_PC1*'.
        SCREEN-INPUT = 1.
      ELSEIF SCREEN-NAME CP '*P_SV1*'.
        SCREEN-INPUT = 0.
      ENDIF.
    ELSE.
      IF SCREEN-NAME CP '*P_PC1*'.
        SCREEN-INPUT = 0.
      ELSEIF SCREEN-NAME CP '*P_SV1*'.
        SCREEN-INPUT = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    "pf_set_screen_for_input

*&---------------------------------------------------------------------*
*&      Form  pf_set_screen_for_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_RAD3    text
*----------------------------------------------------------------------*
FORM PF_SET_SCREEN_FOR_OUTPUT USING IV_RAD3 TYPE FLAG.

  LOOP AT SCREEN.
    IF IV_RAD3 = ABAP_TRUE.
      IF SCREEN-NAME CP '*P_PC2*'.
        SCREEN-INPUT = 1.
      ELSEIF SCREEN-NAME CP '*P_SV2*'.
        SCREEN-INPUT = 0.
      ENDIF.
    ELSE.
      IF SCREEN-NAME CP '*P_PC2*'.
        SCREEN-INPUT = 0.
      ELSEIF SCREEN-NAME CP '*P_SV2*'.
        SCREEN-INPUT = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    "pf_set_screen_for_output

*&---------------------------------------------------------------------*
*&      FORM  PF_SET_GLOBAL_PARAMETER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_BEGDA      text
*      -->IV_ENDDA      text
*      -->IV_COUNT_TOT  text
*      -->IV_COUNT_COM  text
*      -->IV_COUNT_ERR  text
*      -->IV_TITLE      text
*----------------------------------------------------------------------*
FORM PF_SET_GLOBAL_PARAMETER USING IV_BEGDA     TYPE BEGDA
                                   IV_ENDDA     TYPE ENDDA
                                   IV_COUNT_TOT TYPE I
                                   IV_COUNT_COM TYPE I
                                   IV_COUNT_ERR TYPE I
                                   IV_TITLE     TYPE STRING.

  V_BEGDA_1     = IV_BEGDA.
  V_ENDDA_1     = IV_ENDDA.
  V_COUNT_TOT_1 = IV_COUNT_TOT.
  V_COUNT_COM_1 = IV_COUNT_COM.
  V_COUNT_ERR_1 = IV_COUNT_ERR.
  V_TOP_TITLE   = IV_TITLE.

ENDFORM.                    "PF_SET_GLOBAL_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  pf_set_current_time_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PF_SET_CURRENT_TIME_INFO.

  WRITE: SY-UZEIT TO V_RUN_TIME,
         SY-DATUM TO V_RUN_DATE DD/MM/YYYY.

ENDFORM.                    "pf_set_current_time_info
*<-- CH02: END INS

*&---------------------------------------------------------------------*
*&      Form  pf_get_init_directory_pc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PF_GET_INIT_DIRECTORY_PC CHANGING P_PC2.

  DATA LV_SAPWORKDIR TYPE STRING.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_DESKTOP_DIRECTORY
    CHANGING
      DESKTOP_DIRECTORY    = LV_SAPWORKDIR
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  CHECK SY-SUBRC = 0.

  CALL METHOD CL_GUI_CFW=>UPDATE_VIEW
    EXCEPTIONS
      CNTL_SYSTEM_ERROR = 1
      CNTL_ERROR        = 2
      OTHERS            = 3.

  IF LV_SAPWORKDIR IS INITIAL.
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_SAPGUI_WORKDIR
      CHANGING
        SAPWORKDIR            = LV_SAPWORKDIR
      EXCEPTIONS
        GET_SAPWORKDIR_FAILED = 1
        CNTL_ERROR            = 2
        ERROR_NO_GUI          = 3
        NOT_SUPPORTED_BY_GUI  = 4
        OTHERS                = 5.
    CHECK SY-SUBRC = 0.

    CALL METHOD CL_GUI_CFW=>UPDATE_VIEW
      EXCEPTIONS
        CNTL_SYSTEM_ERROR = 1
        CNTL_ERROR        = 2
        OTHERS            = 3.

    IF LV_SAPWORKDIR IS INITIAL.
      LV_SAPWORKDIR = 'C:\TEMP'.
    ENDIF.
  ENDIF.

  P_PC2 = LV_SAPWORKDIR.

ENDFORM.                    "pf_get_init_directory_pc

*&---------------------------------------------------------------------*
*&      Form  pf_get_init_path
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->EV_INIT_PATH    text
*----------------------------------------------------------------------*
FORM PF_GET_INIT_PATH CHANGING EV_INIT_PATH TYPE STRING.

  DATA LV_SAPWORKDIR TYPE STRING.

  CLEAR EV_INIT_PATH.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_DESKTOP_DIRECTORY
    CHANGING
      DESKTOP_DIRECTORY    = LV_SAPWORKDIR
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  CHECK SY-SUBRC = 0.

  CALL METHOD CL_GUI_CFW=>UPDATE_VIEW
    EXCEPTIONS
      CNTL_SYSTEM_ERROR = 1
      CNTL_ERROR        = 2
      OTHERS            = 3.

  IF LV_SAPWORKDIR IS INITIAL.
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_SAPGUI_WORKDIR
      CHANGING
        SAPWORKDIR            = LV_SAPWORKDIR
      EXCEPTIONS
        GET_SAPWORKDIR_FAILED = 1
        CNTL_ERROR            = 2
        ERROR_NO_GUI          = 3
        NOT_SUPPORTED_BY_GUI  = 4
        OTHERS                = 5.
    CHECK SY-SUBRC = 0.

    CALL METHOD CL_GUI_CFW=>UPDATE_VIEW
      EXCEPTIONS
        CNTL_SYSTEM_ERROR = 1
        CNTL_ERROR        = 2
        OTHERS            = 3.

    IF LV_SAPWORKDIR IS INITIAL.
      LV_SAPWORKDIR = 'C:\TEMP'.
    ENDIF.
  ENDIF.

  EV_INIT_PATH = LV_SAPWORKDIR.

ENDFORM.                    "pf_get_init_path

*&---------------------------------------------------------------------*
*&      Form  pf_concat_path_and_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_PATH    text
*      -->IV_FILE    text
*      -->EV_OUTPUT  text
*----------------------------------------------------------------------*
FORM PF_CONCAT_PATH_AND_FILE USING VALUE(IV_PATH) TYPE STRING
                                   VALUE(IV_FILE) TYPE STRING
                          CHANGING EV_OUTPUT      TYPE RLGRAP-FILENAME.

  DATA: LV_OUTPUT TYPE STRING.

  CONSTANTS: LC_DELIMITER1 TYPE C VALUE '\'.

  CLEAR: EV_OUTPUT,
         LV_OUTPUT.

  CONCATENATE IV_PATH LC_DELIMITER1 IV_FILE INTO LV_OUTPUT.

  EV_OUTPUT = LV_OUTPUT.

ENDFORM.                    "pf_concat_path_and_file
*&---------------------------------------------------------------------*
*&      Form  PF_CONCAT_PATH_FILE_SERVER                               *
*&---------------------------------------------------------------------*
*       Concatenate Server Path & Filename                             *
*----------------------------------------------------------------------*
*      -->(IV_PATH) : Server Path                                      *
*      -->(IV_FILE) : Filename                                         *
*      -->EV_OUTPUT : Path & Filename                                  *
*----------------------------------------------------------------------*
FORM PF_CONCAT_PATH_FILE_SERVER USING VALUE(IV_PATH) TYPE STRING
                                      VALUE(IV_FILE) TYPE STRING
                             CHANGING EV_OUTPUT      TYPE RLGRAP-FILENAME.

  DATA: LV_OUTPUT TYPE STRING.

  CONSTANTS: LC_DELIMITER1 TYPE C VALUE '/'.

  CLEAR: EV_OUTPUT,
         LV_OUTPUT.

  CONCATENATE IV_PATH LC_DELIMITER1 IV_FILE INTO LV_OUTPUT.

  EV_OUTPUT = LV_OUTPUT.

ENDFORM.                    "PF_CONCAT_PATH_AND_FILE

*&---------------------------------------------------------------------*
*&      Form  PF_SET_FILE_PATH                                         *
*&---------------------------------------------------------------------*
*       Concatenate Desktop Path & Filename                            *
*----------------------------------------------------------------------*
*      -->P_PATH      : Desktop Path                                   *
*      -->P_NAME      : Filename                                       *
*      -->P_FILEPATH  : Path & Filename                                *
*----------------------------------------------------------------------*
FORM PF_GET_FILE_PATH USING P_PATH
                            P_NAME
                   CHANGING P_FILEPATH.

  CONCATENATE P_PATH '\' P_NAME '_' SY-DATUM SY-UZEIT INTO P_FILEPATH.

ENDFORM.                    "PF_SET_FILE_PATH

*&---------------------------------------------------------------------*
*&      Form  PF_READ_FILE_SERVER
*&---------------------------------------------------------------------*
*       Read File From Server
*----------------------------------------------------------------------*
FORM PF_READ_FILE_SERVER  TABLES LT_DATA
                          USING  P_FILE
                                 P_SPLIT.

  TYPES: BEGIN OF TYP_RECORD ,
           DATA(5000),
         END OF   TYP_RECORD.
  FIELD-SYMBOLS : <FS>     TYPE ANY,
                  <FS_REC> TYPE TYP_RECORD.

  DATA: LV_LINE TYPE STRING.

  DATA: LT_REC TYPE STANDARD TABLE OF TYP_RECORD.

  OPEN DATASET P_FILE FOR INPUT IN TEXT MODE ENCODING UTF-8
  IGNORING CONVERSION ERRORS.
  IF SY-SUBRC = 0.
    DO.
      READ DATASET P_FILE INTO LV_LINE.
      IF SY-SUBRC NE 0.
        EXIT.
      ELSE.
        SPLIT LV_LINE AT P_SPLIT INTO TABLE LT_REC.
        LOOP AT LT_REC ASSIGNING <FS_REC>.
          ASSIGN COMPONENT  SY-TABIX OF STRUCTURE LT_DATA TO <FS>.
          IF SY-SUBRC = 0.
            <FS> = <FS_REC>.
          ENDIF.
        ENDLOOP.
        APPEND LT_DATA.
        REFRESH LT_REC.
      ENDIF.
    ENDDO.
  ENDIF.

ENDFORM.                    "PF_READ_FILE_SERVER
*&---------------------------------------------------------------------*
*&      Form  PF_MANAGE_SCREEN_REQUIRED
*&---------------------------------------------------------------------*
*       Use to Assign Manatory Field
*----------------------------------------------------------------------*
FORM PF_MANAGE_SCREEN_REQUIRED .

  DATA: LR_NAME TYPE RANGE OF SCREEN-NAME,
        LW_NAME LIKE LINE OF LR_NAME.

  CHECK T_FIELD_NAME[] IS NOT INITIAL.

  LOOP AT T_FIELD_NAME INTO W_FIELD_NAME.
    CLEAR LW_NAME.
    LW_NAME-LOW = W_FIELD_NAME.
    LW_NAME-OPTION = 'EQ'.
    LW_NAME-SIGN = 'I'.
    APPEND LW_NAME TO LR_NAME.
  ENDLOOP.

  LOOP AT SCREEN.
    IF SCREEN-NAME IN LR_NAME.
      SCREEN-REQUIRED = 1.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

  PERFORM PF_SET_ICON_SELECTION_OPTION.

ENDFORM.                    "PF_MANAGE_SCREEN_REQUIRED
*&---------------------------------------------------------------------*
*&      Form  PF_MANAGE_SCREEN_CUR_MOUTH
*&---------------------------------------------------------------------*
*       Get Start Date & End Date of Mouth
*----------------------------------------------------------------------*
FORM PF_MANAGE_SCREEN_CUR_MOUTH USING V_BEGDA
                                      V_ENDDA.
  DATA: LV_BEGIN_DATE TYPE D,
        LV_END_DATE   TYPE D.

  "Start and End Date
  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      IV_DATE             = SY-DATUM
    IMPORTING
      EV_MONTH_BEGIN_DATE = LV_BEGIN_DATE
      EV_MONTH_END_DATE   = LV_END_DATE.

  CLEAR: V_BEGDA, V_ENDDA.
  V_BEGDA = LV_BEGIN_DATE.
  V_ENDDA = LV_END_DATE.

ENDFORM.                    "PF_MANAGE_SCREEN_CUR_MOUTH
*&---------------------------------------------------------------------*
*&      Form  PF_GET_CONST_TEXT
*&---------------------------------------------------------------------*
*       Get Constant Text
*----------------------------------------------------------------------*
FORM PF_GET_CONST_TEXT USING IP_CONSTKEY TYPE YEM_WF_DE_CONSTKEY
                       CHANGING  US_TEXT TYPE STRING.

  DATA: LV_VALUE TYPE YEM_WF_DE_LOW.
  CLEAR: LV_VALUE.
  LV_VALUE = YEM_WF_CL_CONSTANT_EXT=>GET_VALUE( IV_CPROG = SPACE
                                                IV_CONSTKEY = IP_CONSTKEY ).
  US_TEXT = LV_VALUE.

ENDFORM.                    "PF_GET_CONST_TEXT
*&---------------------------------------------------------------------*
*&      Form  PF_REPLACE_DATE_AND_TIME
*&---------------------------------------------------------------------*
*       Replace Date And Time
*----------------------------------------------------------------------*
FORM PF_REPLACE_DATE_AND_TIME CHANGING EV_FILENAME.
  DATA: LV_DATE             TYPE SY-DATUM,
        LV_TIME             TYPE SY-UZEIT,
        LI_RESULT_TAB_OPEN  TYPE MATCH_RESULT_TAB,
        LI_RESULT_TAB_CLOSE TYPE MATCH_RESULT_TAB.

  DATA: LV_LOOP         TYPE I,
        LV_OFFSET       TYPE I,
        LV_OFFSET_FIRST TYPE I,
        LV_OFFSET_LAST  TYPE I,
        LV_FLAG         TYPE C,
        LV_COUNT        TYPE I.

  FIELD-SYMBOLS: <LFS_MATCH_OPEN>  LIKE LINE OF LI_RESULT_TAB_OPEN,
                 <LFS_MATCH_CLOSE> LIKE LINE OF LI_RESULT_TAB_CLOSE,
                 <LFS_MATCH_TMP>   LIKE LINE OF LI_RESULT_TAB_OPEN.

  LV_DATE = SY-DATUM.
  LV_TIME = SY-UZEIT.

  FIND ALL OCCURRENCES OF SUBSTRING '<' IN  EV_FILENAME IGNORING CASE RESULTS LI_RESULT_TAB_OPEN.
  LOOP AT LI_RESULT_TAB_OPEN ASSIGNING <LFS_MATCH_OPEN>.
    FIND FIRST OCCURRENCE OF SUBSTRING '>' IN EV_FILENAME+<LFS_MATCH_OPEN>-OFFSET IGNORING CASE RESULTS LI_RESULT_TAB_CLOSE.
    READ TABLE LI_RESULT_TAB_CLOSE ASSIGNING <LFS_MATCH_CLOSE> INDEX 1.
    IF SY-SUBRC EQ 0.
      CLEAR: LV_LOOP,LV_FLAG,LV_OFFSET_FIRST,LV_OFFSET_LAST,LV_COUNT.
      LV_OFFSET_FIRST = <LFS_MATCH_OPEN>-OFFSET.
      LV_OFFSET_LAST  = <LFS_MATCH_CLOSE>-OFFSET + LV_OFFSET_FIRST.
      LV_LOOP = LV_OFFSET_LAST - LV_OFFSET_FIRST.
      LV_FLAG = 'X'.
      DO LV_LOOP TIMES.
        CLEAR: LV_OFFSET.
        LV_OFFSET = <LFS_MATCH_OPEN>-OFFSET + SY-INDEX.
        IF LV_COUNT > 0.
          LV_COUNT = LV_COUNT - 1.
          CONTINUE.
        ENDIF.
        CASE EV_FILENAME+LV_OFFSET(1).

          WHEN 'Y'.
            IF EV_FILENAME+LV_OFFSET(4) EQ 'YYYY'.
              LV_COUNT = 3.
            ELSE.
              LV_FLAG = ''.
            ENDIF.
          WHEN 'M'.
            IF EV_FILENAME+LV_OFFSET(2) EQ 'MM'.
              LV_COUNT = 1.
            ELSE.
              LV_FLAG = ''.
            ENDIF.
          WHEN 'D'.
            IF EV_FILENAME+LV_OFFSET(2) EQ 'DD'.
              LV_COUNT = 1.
            ELSE.
              LV_FLAG = ''.
            ENDIF.
          WHEN 'h'.
            IF EV_FILENAME+LV_OFFSET(2) EQ 'hh'.
              LV_COUNT = 1.
            ELSE.
              LV_FLAG = ''.
            ENDIF.
          WHEN 'm'.
            IF EV_FILENAME+LV_OFFSET(2) EQ 'mm'.
              LV_COUNT = 1.
            ELSE.
              LV_FLAG = ''.
            ENDIF.
          WHEN 's'.
            IF EV_FILENAME+LV_OFFSET(2) EQ 'ss'.
              LV_COUNT = 1.
            ELSE.
              LV_FLAG = ''.
            ENDIF.
          WHEN '<' OR '>'.
          WHEN OTHERS.
            LV_FLAG = ''.
        ENDCASE.
      ENDDO.

      DO LV_LOOP TIMES.
        CLEAR LV_OFFSET.
        LV_OFFSET = <LFS_MATCH_OPEN>-OFFSET + SY-INDEX.
        CASE EV_FILENAME+LV_OFFSET(1).
          WHEN 'Y'.
            IF EV_FILENAME+LV_OFFSET(4) EQ 'YYYY' AND LV_FLAG EQ 'X'.
              REPLACE SECTION OFFSET LV_OFFSET LENGTH 4 OF EV_FILENAME WITH LV_DATE(4).
            ENDIF.
          WHEN 'M'.
            IF EV_FILENAME+LV_OFFSET(2) EQ 'MM' AND LV_FLAG EQ 'X'.
              REPLACE SECTION OFFSET LV_OFFSET LENGTH 2 OF EV_FILENAME WITH LV_DATE+4(2).
            ENDIF.
          WHEN 'D'.
            IF EV_FILENAME+LV_OFFSET(2) EQ 'DD' AND LV_FLAG EQ 'X'.
              REPLACE SECTION OFFSET LV_OFFSET LENGTH 2 OF EV_FILENAME WITH LV_DATE+6(2).
            ENDIF.
          WHEN 'h'.
            IF EV_FILENAME+LV_OFFSET(2) EQ 'hh' AND LV_FLAG EQ 'X'.
              REPLACE SECTION OFFSET LV_OFFSET LENGTH 2 OF EV_FILENAME WITH LV_TIME(2).
            ENDIF.
          WHEN 'm'.
            IF EV_FILENAME+LV_OFFSET(2) EQ 'mm' AND LV_FLAG EQ 'X'.
              REPLACE SECTION OFFSET LV_OFFSET LENGTH 2 OF EV_FILENAME WITH LV_TIME+2(2).
            ENDIF.
          WHEN 's'.
            IF EV_FILENAME+LV_OFFSET(2) EQ 'ss' AND LV_FLAG EQ 'X'.
              REPLACE SECTION OFFSET LV_OFFSET LENGTH 2 OF EV_FILENAME WITH LV_TIME+4(2).
            ENDIF.
        ENDCASE.
      ENDDO.
      IF LV_FLAG EQ 'X'.
        IF EV_FILENAME+LV_OFFSET_FIRST(1) EQ '<' AND EV_FILENAME+LV_OFFSET_LAST(1) EQ '>'.
          REPLACE SECTION OFFSET LV_OFFSET_FIRST LENGTH 1 OF EV_FILENAME WITH ''.
          LV_OFFSET_LAST = LV_OFFSET_LAST - 1.
          LOOP AT LI_RESULT_TAB_OPEN ASSIGNING <LFS_MATCH_TMP>.
            <LFS_MATCH_TMP>-OFFSET = <LFS_MATCH_TMP>-OFFSET - 2.
          ENDLOOP.
          REPLACE SECTION OFFSET LV_OFFSET_LAST LENGTH 1 OF EV_FILENAME WITH ''.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "PF_REPLACE_DATE_AND_TIME
*&---------------------------------------------------------------------*
*&      Form  PF_SET_ICON_SELECTION_OPTION
*&---------------------------------------------------------------------*
*       Set Icon Selection Option
*----------------------------------------------------------------------*
FORM PF_SET_ICON_SELECTION_OPTION.

  LOOP AT SCREEN.
    IF SCREEN-NAME CP '%_PNP*_%_APP_%-OPTI_PUSH'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "PF_SET_ICON_SELECTION_OPTION
*&---------------------------------------------------------------------*
*&      Form  PF_NO_DATA_FOUND
*&---------------------------------------------------------------------*
*       No Data Found
*----------------------------------------------------------------------*
FORM PF_NO_DATA_FOUND USING VALUE(IV_PROGRAM_NAME)  TYPE SY-CPROG
                            VALUE(IV_MSGID)         TYPE C
                            VALUE(IV_MSGNR)         TYPE C
                            VALUE(IV_MSGTY)         TYPE C   "DEFAULT 'E'
                            VALUE(IV_MSGDIS)        TYPE C   "DEFAULT LIKE MESSAGE TYPE
                            VALUE(IV_LEAVE)         TYPE C   "DEFAULT 'YES'
                            VALUE(IV_TEXT1)         TYPE C
                            VALUE(IV_TEXT2)         TYPE C
                            VALUE(IV_TEXT3)         TYPE C
                            VALUE(IV_TEXT4)         TYPE C.

*--------------------------------------------------------------------*
* NO DATA FOUND TEMP USING
*--------------------------------------------------------------------*
*PERFORM pf_no_data_found USING sy-cprog space space space space space space space space space.
*--------------------------------------------------------------------*
  DATA: LV_TEXT TYPE STRING.

*--------------------------------------------------------------------*
* 1. Check parameter to message
*--------------------------------------------------------------------*
  IF IV_TEXT1 IS INITIAL AND
     IV_TEXT2 IS INITIAL AND
     IV_TEXT3 IS INITIAL AND
     IV_TEXT4 IS INITIAL AND
     IV_MSGID IS INITIAL AND
     IV_MSGNR IS INITIAL.
*--------------------------------------------------------------------*
* 1.2. Message no data found
*--------------------------------------------------------------------*
    MESSAGE ID 'AQ' TYPE 'S' NUMBER '260'.

  ELSE.
*--------------------------------------------------------------------*
* 1.1. Found parameter to message another
*--------------------------------------------------------------------*
    CLEAR LV_TEXT.
*--------------------------------------------------------------------*
* 1.1.1. Check message type
*--------------------------------------------------------------------*
    TRANSLATE IV_MSGTY TO UPPER CASE.
    IF IV_MSGTY IS INITIAL OR NOT (
       IV_MSGTY EQ 'E' OR
       IV_MSGTY EQ 'S' OR
       IV_MSGTY EQ 'W' OR
       IV_MSGTY EQ 'I' OR
       IV_MSGTY EQ 'A' ).
      IV_MSGTY = 'E'.
    ENDIF.
*--------------------------------------------------------------------*
* 1.1.2. Check message display like type
*--------------------------------------------------------------------*
    TRANSLATE IV_MSGDIS TO UPPER CASE.
    IF IV_MSGDIS IS INITIAL OR NOT (
       IV_MSGDIS EQ 'E' OR
       IV_MSGDIS EQ 'S' OR
       IV_MSGDIS EQ 'W' OR
       IV_MSGDIS EQ 'I' OR
       IV_MSGDIS EQ 'A' ).
      IV_MSGDIS = IV_MSGTY.
    ENDIF.
*--------------------------------------------------------------------*
* 1.1.3. Message
*--------------------------------------------------------------------*
    IF IV_MSGID IS INITIAL OR IV_MSGNR IS INITIAL.
      CONCATENATE IV_TEXT1
                  IV_TEXT2
                  IV_TEXT3
                  IV_TEXT4 INTO LV_TEXT.
      MESSAGE LV_TEXT TYPE IV_MSGTY DISPLAY LIKE IV_MSGDIS.
    ELSE.
*      MESSAGE ID iv_msgid TYPE iv_msgty NUMBER iv_msgnr WITH iv_text1 iv_text2 iv_text3 iv_text4 DISPLAY LIKE iv_msgdis.
    ENDIF.
  ENDIF.
*--------------------------------------------------------------------*
* 2. Levae list - processing
*--------------------------------------------------------------------*
  IF IV_LEAVE IS INITIAL.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.                    "PF_NO_DATA_FOUND

*&---------------------------------------------------------------------*
*&      Form  PF_GET_FILE_NAME_FOR_PATH
*&---------------------------------------------------------------------*
*       Get file name for path + file name
*----------------------------------------------------------------------*
FORM PF_GET_FILE_NAME_FOR_PATH USING VALUE(IV_PATH)
                            CHANGING CV_FILE_N      TYPE STRING.

  DATA: LS_RESULT TYPE MATCH_RESULT.
  DATA: LV_PATH   TYPE STRING.
  DATA: LV_PC     TYPE STRING.

  CLEAR: CV_FILE_N,LS_RESULT.

  LV_PATH = IV_PATH.
  CONDENSE LV_PATH.
  FIND ALL OCCURRENCES OF '\' IN LV_PATH RESULTS LS_RESULT. " Finds last occurence of \
  IF SY-SUBRC = 0.
    LV_PC = LV_PATH+LS_RESULT-OFFSET.
    CLEAR: LS_RESULT.
    FIND ALL OCCURRENCES OF '.' IN LV_PC RESULTS LS_RESULT.
    IF SY-SUBRC = 0.
      CV_FILE_N = LV_PC+0(LS_RESULT-OFFSET).
    ENDIF.

  ENDIF.

ENDFORM.                    "PF_GET_FILE_NAME_FOR_PATH
