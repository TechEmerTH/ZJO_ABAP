*----------------------------------------------------------------------
* Program ID       : ZHRTIMEREMINDER
* Date(dd.mm.yyyy) : 8/1/2018
* Author           : Korntana J.
* Description      : Create program
*----------------------------------------------------------------------
*  CHANGE HISTORY
*----------------------------------------------------------------------
* Change By   :
* Change Date :
* Search Team : CH01
* Description :
*----------------------------------------------------------------------
REPORT zjo_email.
*----------------------------------------------------------------------
* I N F O T Y P E S
*----------------------------------------------------------------------
INFOTYPES: 0001,0002,0105.
NODES peras.
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

TYPES: BEGIN OF ty_no1,
         pernr       TYPE pernr_d,
         tvarvc      TYPE rvari_val_255,
         userid_long TYPE p0105-usrid_long,
         cname       TYPE p0002-cname,
         startdate   TYPE datum,
         enddate     TYPE datum,
         refdate     TYPE datum,
       END OF ty_no1.

TYPES: BEGIN OF ty_no2,
         pernr       TYPE pernr_d,
         tvarvc      TYPE rvari_val_255,
         userid_long TYPE p0105-usrid_long,
         cname       TYPE p0002-cname,
       END OF ty_no2.



TYPES : BEGIN OF ty_table_no3,
          pernr       TYPE pernr_d,
          tvarvc      TYPE rvari_val_255,
          userid_long TYPE p0105-usrid_long,
          cname       TYPE p0002-cname,
        END OF ty_table_no3.

TYPES: BEGIN OF ty_no3,
         pernr       TYPE pernr_d,
         tvarvc      TYPE rvari_val_255,
         userid_long TYPE p0105-usrid_long,
         cname       TYPE p0002-cname,
         refdate     TYPE  datum,
         table       TYPE STANDARD TABLE OF ty_table_no3 WITH DEFAULT KEY,
       END OF ty_no3.

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
DATA : gt_p0001            TYPE TABLE OF p0001,
       gt_p0002            TYPE TABLE OF p0002,
       gt_p0105            TYPE TABLE OF p0105,
       gt_tvarvc           TYPE TABLE OF tvarvc,
       gt_t554t            TYPE TABLE OF t554t,
       gt_ptreq_header     TYPE TABLE OF ptreq_header,
       gt_ptreq_attabsdata TYPE TABLE OF ptreq_attabsdata,
       gt_ptreq_notice     TYPE TABLE OF ptreq_notice,
       gt_tptcor_core      TYPE TABLE OF tptcor_core,
       gt_no1              TYPE TABLE OF ty_no1,
       gt_no2              TYPE TABLE OF ty_no2,
       gt_no3              TYPE TABLE OF ty_no3.
*-> Work areas
DATA : gw_p0001  LIKE LINE OF gt_p0001,
       gw_p0002  LIKE LINE OF gt_p0002,
       gw_p0105  LIKE LINE OF gt_p0105,
       gw_tvarvc LIKE LINE OF gt_tvarvc,
       gw_no1    LIKE LINE OF gt_no1,
       gw_no2    LIKE LINE OF gt_no2,
       gw_no3    LIKE LINE OF gt_no3.

*-> Ranges
*DATA : GR_P0001 TYPE RANGE OF P0001-PERNR.

*-> Variables
*DATA : GV_PERNR TYPE P0001-PERNR.

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


  GET peras.

  rp_provide_from_last p0001 space pn-begda pn-endda.
  APPEND p0001 TO gt_p0001.
  rp_provide_from_last p0002 space pn-begda pn-endda.
  APPEND p0002 TO gt_p0002.
  rp_provide_from_last p0105 space pn-begda pn-endda.
  APPEND p0105 TO gt_p0105.

*----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*----------------------------------------------------------------------
END-OF-SELECTION.
  PERFORM pf_getdata.
  PERFORM pf_prepare_data.
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
*&      Form  PF_GETDATA
*&---------------------------------------------------------------------*
FORM pf_getdata .

  SELECT *
    FROM tvarvc
    INTO TABLE gt_tvarvc.

  SELECT *
    FROM t554t
    INTO TABLE gt_t554t
    WHERE sprsl = 'E'.

  SELECT *
    FROM ptreq_header
    INTO TABLE gt_ptreq_header.

  SELECT *
    FROM ptreq_attabsdata
    INTO TABLE gt_ptreq_attabsdata.

  SELECT *
    FROM ptreq_notice
    INTO TABLE gt_ptreq_notice.

  SELECT *
    FROM tptcor_core
    INTO TABLE gt_tptcor_core.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_PREPARE_DATA
*&---------------------------------------------------------------------*
FORM pf_prepare_data .
  "read tbd
  " if sy-datum = Locking Reminder Date PC
  LOOP AT gt_p0001 INTO gw_p0001 WHERE persg <> 5.
    gw_no1-pernr = gw_p0001-pernr.
    READ TABLE gt_p0105 INTO gw_p0105 WITH KEY pernr = gw_no1-pernr.
    IF sy-subcs = 0.
      gw_no1-userid_long = gw_p0105-usrid_long.
    ENDIF.

    READ TABLE gt_p0002 INTO gw_p0002 WITH KEY pernr = gw_no1-pernr.
    IF sy-subcs = 0.
      gw_no1-cname = gw_p0002-cname.
    ENDIF.

    READ TABLE gt_tvarvc INTO gw_tvarvc WITH KEY name = 'ZEMAILSENDER'.
    IF sy-subcs = 0.
      gw_no1-tvarvc = gw_tvarvc-low.
    ENDIF.
    " get startdate , end date from tbs
    APPEND gw_no1 to gt_no1.
  ENDLOOP.
  " elseif sy-datum = Locking Reminder Date OS
  LOOP AT gt_p0001 INTO gw_p0001 WHERE persg = 5.
    " copy ด้านบน
  ENDLOOP.
  "endif



  "elseif sy-datum = Completed Reminder Date OS or sy-datum = Reminder Date PC
  LOOP AT gt_p0001 INTO gw_p0001.
    gw_no2-pernr = gw_p0001-pernr.
    READ TABLE gt_p0105 INTO gw_p0105 WITH KEY pernr = gw_no1-pernr.
    IF sy-subcs = 0.
      gw_no2-userid_long = gw_p0105-usrid_long.
    ENDIF.

    READ TABLE gt_p0002 INTO gw_p0002 WITH KEY pernr = gw_no1-pernr.
    IF sy-subcs = 0.
      gw_no2-cname = gw_p0002-cname.
    ENDIF.

    READ TABLE gt_tvarvc INTO gw_tvarvc WITH KEY name = 'ZEMAILSENDER'.
    IF sy-subcs = 0.
      gw_no2-tvarvc = gw_tvarvc-low.
    ENDIF.
    " get startdate , end date from tbs
    APPEND gw_no2 to gt_no2.
  ENDLOOP.

  "elseif  sy-datun < Changed On
ENDFORM.
