FUNCTION zfm_jo_get_manager.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      ACTOR_TAB STRUCTURE  SWHACTOR
*"      AC_CONTAINER STRUCTURE  SWCONT
*"  EXCEPTIONS
*"      NOBODY_FOUND
*"----------------------------------------------------------------------


*ACTOR_TAB-objid
  DATA: lv_otype TYPE otype.
  DATA: lv_objid TYPE realo.
  DATA: lv_realo TYPE realo.
  DATA: lw_leader TYPE hrrootob.
  DATA: lw_actor LIKE LINE OF  actor_tab.

  LOOP AT ac_container.
    IF ac_container-element = 'IV_PERNR'.
      lv_objid = ac_container-value.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'RH_GET_LEADER'
    EXPORTING
      plvar                     = '01'
      keydate                   = sy-datum
      otype                     = 'P'
      objid                     = lv_objid
    IMPORTING
      leader_id                 = lv_realo
*    TABLES
*     leader_id                 = lv_realo
    EXCEPTIONS
      no_leader_found           = 1
      no_leading_position_found = 2
      OTHERS                    = 3.
  IF sy-subrc = 0.
    lw_actor-otype = 'P'.
    lw_actor-objid = lv_realo.
    APPEND lw_actor TO actor_tab.
  ENDIF.


ENDFUNCTION.
