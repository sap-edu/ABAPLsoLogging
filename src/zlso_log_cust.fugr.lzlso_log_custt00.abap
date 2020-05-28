*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 10.04.2019 at 11:54:33
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZLSO_LOG_CUST...................................*
DATA:  BEGIN OF STATUS_ZLSO_LOG_CUST                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLSO_LOG_CUST                 .
CONTROLS: TCTRL_ZLSO_LOG_CUST
            TYPE TABLEVIEW USING SCREEN '1011'.
*.........table declarations:.................................*
TABLES: *ZLSO_LOG_CUST                 .
TABLES: ZLSO_LOG_CUST                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
