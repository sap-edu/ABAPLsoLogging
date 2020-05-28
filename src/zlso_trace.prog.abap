*&---------------------------------------------------------------------*
*& Report  ZLSO_TRACE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

include zlso_trace_top.  " Global Data
include zlso_trace_sel.  " Selection screen
include zlso_trace_f01.  " FORM-Routines

initialization.


  "Check if the user is Technical Support
  "THIS IS intentionally done here without the zcl_lso_security class to keep it transportable to ISP
  authority-check object 'S_DEVELOP' id 'ACTVT' field '02'.
  if sy-subrc <> 0.
    message e000(zlso_log) with |Missing Authorization: Technical Support Role|.
  endif.

  perform init_screen_params.

end-of-selection.
  data: lt_logs type zlso_tt_log_objects.

  clear lt_logs.

  try.
      lt_logs = zcl_lso_log_factory=>instance( )->find( log_ids              = p_id[]
                                                        log_dates            = p_ldate[]
                                                        log_times            = p_ltime[]
                                                        changed_bys          = p_chby[]
                                                        contexts             = p_ctx[]
                                                        tcodes               = p_tcode[]
                                                        programs             = p_prog[]
                                                        msgtys               = p_msgty[]
                                                        msgids               = p_msgid[]
                                                        msgnos               = p_msgno[]
                                                        http_statuses        = p_httprc[]
                                                        request_urls         = p_url[]
                                                        request_methods      = p_meth[]
                                                        message_texts        = p_msgtx[]
                                                        request_payloads     = p_rqpay[]
                                                        response_payloads    = p_repay[]
                                                        req_header_names     = p_rqhnam[]
                                                        req_header_values    = p_rqhval[]
                                                        res_header_names     = p_rshnam[]
                                                        res_header_values    = p_rshval[] ).
    catch zcx_lso_log into data(lo_cx_lso_log).
      write: lo_cx_lso_log->get_text( ).
      exit.
  endtry.

* call FM to display
  call function 'Z_LSO_TRACE_DISPLAY'
    exporting
      it_logs = lt_logs.
