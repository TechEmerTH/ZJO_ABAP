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
REPORT zexam2_01_jo.
*----------------------------------------------------------------------
* I N F O T Y P E S
*----------------------------------------------------------------------
NODES : peras.
INFOTYPES: 0001,0022.

*----------------------------------------------------------------------
* I N C L U D E
*----------------------------------------------------------------------
*INCLUDE: yemwfr0000i_main_include.

*----------------------------------------------------------------------
* T A B L E S
*----------------------------------------------------------------------
TABLES: pernr.

*----------------------------------------------------------------------
* T Y P E S
*----------------------------------------------------------------------
TYPES : BEGIN OF ty_output,
          pernr     TYPE pa0001-pernr,
          ename     TYPE pa0001-ename,
          endda     TYPE c LENGTH 4,
          slart     TYPE pa0022-slart,
          slartt    TYPE t517t-stext, "
          ausbi     TYPE pa0022-ausbi,
          ausbit    TYPE t518b-atext, "
          institute TYPE pa0022-insti,
          grade     TYPE pa0022-emark,
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
DATA : gt_output   TYPE TABLE OF ty_output,
       gt_pa0022   TYPE STANDARD TABLE OF p0022,
       gt_t517t    TYPE STANDARD TABLE OF t517t,
       gt_t518b    TYPE STANDARD TABLE OF t518b,
       gt_fieldcat TYPE  slis_t_fieldcat_alv.

*-> Work areas
DATA : gw_output LIKE LINE OF gt_output.

*-> Ranges
*DATA : GR_P0001 TYPE RANGE OF P0001-PERNR.

*-> Variables
*DATA : GV_PERNR TYPE P0001-PERNR.
DATA: gv_col_pos TYPE i,
      gv_endda   TYPE sy-datum,
      gv_date    TYPE string,
      gv_begda   TYPE sy-datum.
*-> Field-symbols
FIELD-SYMBOLS : <fs_p0022> LIKE LINE OF gt_pa0022,
                <fs_t517t> LIKE LINE OF gt_t517t,
                <fs_t518b> LIKE LINE OF gt_t518b.

*----------------------------------------------------------------------
* M A C R O S
*----------------------------------------------------------------------
*DEFINE MC_APPEND_DATA.
*END-OF-DEFINITION.
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
* PARAMETERS: P_KDATE TYPE SY-DATUM.
* SELECT-OPTIONS: S_DATE FOR SY-DATUM.
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
SELECT-OPTIONS: s_date FOR gw_output-endda .
SELECTION-SCREEN: END OF BLOCK b1.
*----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*----------------------------------------------------------------------
INITIALIZATION.

*----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*----------------------------------------------------------------------
*AT SELECTION-SCREEN OUTPUT.

*AT SELECTION-SCREEN.
*  if PNPTIMED = 'K'.
*   pnpbegda = sy-datum.
*  elseif PNPTIMED = 'I'.
*PERFORM pf_get_month.
*CONCATENATE sy-datum+0(6) '01' into gv_begda.
*pnpbegda = gv_begda.
*pnpendda = gv_endda.
*endif.

*----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM pf_getdata.
  GET peras.
  rp_provide_from_last p0001 space pn-begda pn-endda.
  IF pnp-sw-found NE 1.
    REJECT 'PERAS'.
  ENDIF.

  CLEAR gw_output.
  gw_output-pernr  = p0001-pernr.
  gw_output-ename  = p0001-ename .

  LOOP AT p0022 ASSIGNING <fs_p0022>  where endda+0(4) in s_date .
    PERFORM get_edu_data.
  ENDLOOP.

*----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*----------------------------------------------------------------------
END-OF-SELECTION.
  SORT gt_output BY pernr endda.
  PERFORM pf_alv.
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

  mc_append_fieldcat 'PERNR'  TEXT-001 .
  mc_append_fieldcat 'ENAME'  TEXT-002  .
  mc_append_fieldcat 'ENDDA'  TEXT-003  .
  mc_append_fieldcat 'SLARTT'  TEXT-004  .
  mc_append_fieldcat 'AUSBIT'  TEXT-005  .
  mc_append_fieldcat 'INSTITUTE'  TEXT-006  .
  mc_append_fieldcat 'GRADE'  TEXT-007  .




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
*&      Form  PF_GET_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_getdata .
  REFRESH gt_t517t.
  SELECT * FROM t517t INTO TABLE gt_t517t WHERE sprsl = sy-langu.
  REFRESH gt_t518b.
  SELECT * FROM t518b INTO TABLE gt_t518b WHERE langu = sy-langu.
ENDFORM.

FORM get_edu_data.
  gw_output-endda = <fs_p0022>-endda.

if <fs_p0022>-slart is not INITIAL.
  READ TABLE gt_t517t ASSIGNING <fs_t517t> WITH KEY slart = <fs_p0022>-slart.
  IF sy-subrc EQ 0.
    gw_output-slartt = <fs_t517t>-stext.
  ENDIF.
ENDIF.

if <fs_p0022>-ausbi is not INITIAL.
  READ TABLE gt_t518b ASSIGNING <fs_t518b> WITH KEY ausbi = <fs_p0022>-ausbi.
  IF sy-subrc EQ 0.
    gw_output-ausbit = <fs_t518b>-atext.
  ENDIF.
ENDIF.

  gw_output-institute = <fs_p0022>-insti.
  gw_output-grade = <fs_p0022>-emark.
  APPEND gw_output TO gt_output.
ENDFORM.
