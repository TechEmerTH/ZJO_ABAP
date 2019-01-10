class ZCL_WF_TEST_JO definition
  public
  final
  create public .

public section.

  interfaces BI_OBJECT .
  interfaces BI_PERSISTENT .
  interfaces IF_WORKFLOW .

  class-methods GET_AUTO_REJECT_DATE
    importing
      !IV_DATE type DATUM
    exporting
      !EV_AUTO_REJ_DATE type DATUM .
  class-methods DUMMY .
  class-methods GET_NAME
    importing
      !IV_BEGDA type DATUM
      !IV_ENDDA type DATUM
      !IV_PERNR type PERNR_D
    exporting
      !EV_NAME type EMNAM .
protected section.
private section.
ENDCLASS.



CLASS ZCL_WF_TEST_JO IMPLEMENTATION.


  method DUMMY.
  endmethod.


  method GET_AUTO_REJECT_DATE.
  endmethod.


  METHOD get_name.

    DATA: lt_pa0001 TYPE TABLE OF pa0001.
    DATA: lw_pa0001 LIKE LINE  OF lt_pa0001.

    SELECT *
      FROM pa0001
      INTO TABLE lt_pa0001
      WHERE pernr = iv_pernr
      AND begda <= iv_endda
      AND endda >= iv_begda.

    SORT lt_pa0001 BY endda DESCENDING.

    READ TABLE lt_pa0001 INTO lw_pa0001 INDEX 1.
    IF sy-subrc = 0.
      ev_name = lw_pa0001-ename.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
