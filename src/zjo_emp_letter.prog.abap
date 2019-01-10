*-----------------------------------------------------------------------
* Program ID       : YEMXXHRPAR0001_EMP_LETTER
* Date(dd-mmm-yyyy): 13.10.2016
* Author           : Noprada A.
* Description      : Generate Employee Letter
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
* Change By   :
* Change Date :
* Search Team : CH01
* Description :
*-----------------------------------------------------------------------

REPORT zjo_emp_letter MESSAGE-ID 38.

*-----------------------------------------------------------------------
* I N C L U D E
*-----------------------------------------------------------------------
INCLUDE yempar0001i_top.                      " Global Data
INCLUDE yemwfr0000i_main_include.             " Main Include
INCLUDE ole2incl.
INCLUDE yempar0001i_s01.                      " Selection Screen Data
INCLUDE yempar0001i_f01.                      " Subroutines
INCLUDE yempar0001i_f02.                      " Subroutines - template&merge
INCLUDE yempar0001i_f03.                      " Subroutines - fill data
INCLUDE yempar0001i_f04.                      " Certification

*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.
  PERFORM initial_listbox_letter_type.
  PERFORM initial_listbox_footer.
  PERFORM initial_path.
  PERFORM get_organization_level.
  PERFORM get_logo_by_bu.                        "...Display logo by bu
  PERFORM initial_wage_type.                     "...Default wage type

*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.           "Path
  PERFORM pf_browse_file  CHANGING  p_path.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_wlgart-low.     "Wage type
  PERFORM pf_get_wage_type  CHANGING  s_wlgart-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_wlgart-high.    "Wage type
  PERFORM pf_get_wage_type  CHANGING  s_wlgart-high.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_slgart-low.     "Wage type
  PERFORM pf_get_wage_type  CHANGING  s_slgart-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_slgart-high.    "Wage type
  PERFORM pf_get_wage_type  CHANGING  s_slgart-high.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_hlgart-low.     "Wage type
  PERFORM pf_get_wage_type  CHANGING  s_hlgart-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_hlgart-high.    "Wage type
  PERFORM pf_get_wage_type  CHANGING  s_hlgart-high.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_schcd.
  PERFORM pf_get_institution  CHANGING  p_schcd.

AT SELECTION-SCREEN  OUTPUT.

  APPEND 'PNPBUKRS-LOW' TO t_field_name.
  PERFORM pf_manage_screen_required.             "Require company code

  PERFORM initial_listbox_template.
  PERFORM disable_screen.                        "...Check table screen
  PERFORM pf_get_default_name.                   "Default singuage name & Coor. name

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM check_require_field. " check require fill by letter type
  IF v_error IS NOT INITIAL.
    LEAVE LIST-PROCESSING. " exit to selection screen
  ENDIF.

  PERFORM get_path_directory. " check path if initial assign path
  PERFORM check_template_exist. " check input letter type value exist in template table
  PERFORM initial_data. " asign emp_letter_typ value
  REFRESH i_efm.

  IF w_yemhrpa_lettyp-dokar IS INITIAL " dokar = document type
    AND p_efm IS NOT INITIAL.

    MESSAGE i000(38) WITH text-w01.
  ENDIF.

  v_endda = pn-endda.
  v_begda = pn-begda.

  GET peras.
*- Get main data
  PERFORM pf_get_data. " get data (each letter type have thier own way to get data)

  pn-endda = v_endda.
  pn-begda = v_begda.

*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
  IF pnp-sw-found = 0.
    PERFORM pf_no_data_found USING sy-cprog space
            space space space space space space space space.
  ELSE.
    IF p_typ_l = '016'.                          "Certification Verify
      PERFORM pf_fill_data_certification.
      PERFORM pf_export_word.
      PERFORM pf_generate_efm_verify.
    ELSE.
      PERFORM file_delete USING v_path.          "Detele template file
    ENDIF.
    PERFORM export_file_for_efm.
  ENDIF.

*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
*TOP-OF-PAGE.

*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
*AT LINE-SELECTION.

  INCLUDE yempar0001i_f05.
