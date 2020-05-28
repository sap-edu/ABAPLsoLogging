*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZLSO_LOG_CUST
*   generation date: 10.04.2019 at 11:54:33
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZLSO_LOG_CUST      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
