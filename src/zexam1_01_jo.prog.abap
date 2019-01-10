*----------------------------------------------------------------------
* Program ID       :
* Date(dd.mm.YYYY) :
* Author           :
* Description      :
*----------------------------------------------------------------------
*  CHANGE HISTORY
*----------------------------------------------------------------------
* Change BY   :
* Change Date :
* Search Team : CH01
* Description :
*----------------------------------------------------------------------
REPORT gv_zegv_xam1_01_jo.
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

*TYPES: BEGIN OF TY_LINE,
*    COLUMN1 TYPE I,
*    COLUMN2 TYPE I,
*    COLUMN3 TYPE I,
* END OF TY_LINE.
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
*DATA : GT_P0001 TYPE TABLE OF P0001.

*-> Work areas
*DATA : GW_P0001 LIKE LINE OF I_P0001.

*-> Ranges
*DATA : GR_P0001 TYPE RANGE OF P0001-PERNR.

*-> Variables
*DATA : GV_PERNR TYPE P0001-PERNR.
DATA: gv_z     TYPE string,
      gv_z2    TYPE string,
      gv_spell TYPE spell,
      spell   TYPE string,
      gv_x2    TYPE string,
      gv_y2    TYPE string.
*-> Field-sYmbols
*FIELD-SYMBOLS : <FS_P0001> LIKE LINE OF GT_P0001.

*----------------------------------------------------------------------
* M A C R O S
*----------------------------------------------------------------------
*DEFINE MC_APPEND_DATA.
*END-OF-DEFINITION.

*----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*----------------------------------------------------------------------
PARAMETERS: p_x TYPE i.
PARAMETERS: p_y TYPE i.
*----------------------------------------------------------------------
* I N I T I A L I gv_z A T I O N
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





*----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*----------------------------------------------------------------------
END-OF-SELECTION.
  gv_z = p_x + p_y.
  CALL FUNCTION 'SPELL_AMOUNT'
    EXPORTING
      amount    = gv_z
      language  = sy-langu
    IMPORTING
      in_words  = gv_spell
    EXCEPTIONS
      not_found = 1
      too_large = 2
      OTHERS    = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  gv_x2 = p_x.
  gv_y2 = p_y.

  CONCATENATE 'Z = ' ` ` gv_spell-word  INTO spell.
  CONCATENATE 'Z = ' ` ` gv_z  INTO gv_z2.
  CONCATENATE 'X = ' ` ` gv_x2  INTO gv_x2.
  CONCATENATE 'Y = ' ` ` gv_y2  INTO gv_y2.

  WRITE:  gv_x2,
        / gv_y2,
        /  ,
        / 'Z = X + Y',
        / gv_z2,
        / spell.
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
