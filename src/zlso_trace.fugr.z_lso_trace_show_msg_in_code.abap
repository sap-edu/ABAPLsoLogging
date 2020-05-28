FUNCTION Z_LSO_TRACE_SHOW_MSG_IN_CODE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_MESSAGE_NUMBER) LIKE  T100-MSGNR DEFAULT '036'
*"     VALUE(IV_MESSAGE_CLASS) LIKE  RSDAG-ARBGB DEFAULT 'ZLSO_TASK'
*"----------------------------------------------------------------------
  data:
    lv_transaction_name type          c length 10,
    ls_bdcdata          type          bdcdata,
    lt_bdcdata          type table of bdcdata,
    ls_options          type          ctu_params.

  lv_transaction_name = 'SE91'.

  ls_options-dismode  = 'E'. "'A'. " display mode
  ls_options-defsize  = ' '. " do not change window's size
  ls_options-racommit = 'X'. " Continue after commit

  "initial screen
  append value bdcdata( program   = 'SAPLWBMESSAGES'
                        dynpro    = '0100'
                        dynbegin  = 'X' )            to lt_bdcdata.

  append value bdcdata( fnam      = 'BDC_OKCODE'     fval = '=WB_WHERE_USED_LIST' ) to lt_bdcdata.
  append value bdcdata( fnam      = 'RSDAG-ARBGB'    fval = iv_message_class )      to lt_bdcdata.
  append value bdcdata( fnam      = 'RSDAG-MSGFLAG'  fval = 'X' )                   to lt_bdcdata.
  append value bdcdata( fnam      = 'MSG_NUMMER'     fval = iv_message_number )     to lt_bdcdata.

  append value bdcdata( program   = 'SAPICNN_'
                        dynpro    = '0101'
                        dynbegin  = 'X' )            to lt_bdcdata.

  append value bdcdata( fnam      = 'BDC_CURSOR'     fval = 'RSEUX-CGE' )   to lt_bdcdata.
  append value bdcdata( fnam      = 'BDC_OKCODE'     fval = '=ENTR' )       to lt_bdcdata.

  append value bdcdata( fnam      = 'RSEUX-CP'       fval = 'X' )           to lt_bdcdata.
  append value bdcdata( fnam      = 'RSEUX-COCI'     fval = 'X' )           to lt_bdcdata.
  append value bdcdata( fnam      = 'RSEUX-CYT'      fval = 'X' )           to lt_bdcdata.
  append value bdcdata( fnam      = 'RSEUX-CWO'      fval = 'X' )           to lt_bdcdata.
  append value bdcdata( fnam      = 'RSEUX-CDTF'     fval = 'X' )           to lt_bdcdata.
  append value bdcdata( fnam      = 'RSEUX-CGE'      fval = 'X' )           to lt_bdcdata.

  call transaction lv_transaction_name using lt_bdcdata options from ls_options. "#EC CI_CALLTA

endfunction.
