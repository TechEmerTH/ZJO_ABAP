*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 06.09.2017 at 10:49:21
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTEXAM1_02_JO...................................*
DATA:  BEGIN OF STATUS_ZTEXAM1_02_JO                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTEXAM1_02_JO                 .
CONTROLS: TCTRL_ZTEXAM1_02_JO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTEXAM1_02_JO                 .
TABLES: ZTEXAM1_02_JO                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
