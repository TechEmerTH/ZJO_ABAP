*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTEXAM1_02_JO
*   generation date: 06.09.2017 at 10:49:18
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTEXAM1_02_JO      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
