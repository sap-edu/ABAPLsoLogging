*&---------------------------------------------------------------------*
*& Report  ZLSO_TRACE_CONTEXT_TEST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
program zlso_trace_context_test.

*include zlso_trace_top                          .    " global Data
*include zlso_trace_sel.                              "selection screen


*parameters: p_con1 radiobutton group cont.
*parameters: p_con2 radiobutton group cont.

** INCLUDE ZLSO_TRACE_O01                          .  " PBO-Modules
** INCLUDE ZLSO_TRACE_I01                          .  " PAI-Modules
*include zlso_trace_f01                          .  " FORM-Routines
*
*initialization.
*
*  "Check if the user is Technical Support
*  "THIS IS intentionally done here without the zcl_lso_security class to keep it transportable to ISP
*  authority-check object 'S_DEVELOP' id 'ACTVT' field '02'.
*  if sy-subrc <> 0.
*    message e000(zlso_log) with |Missing Authorization: Technical Support Role|.
*  endif.
*
*  perform init_screen_params.
*
*end-of-selection.
*  data: lt_logs type zlso_tt_log_objects.
*
*  clear lt_logs.
*
*  try.
*      lt_logs = zcl_lso_log_factory=>instance( )->find( log_ids              = p_id[]
*                                                        log_dates            = p_ldate[]
*                                                        log_times            = p_ltime[]
*                                                        changed_bys          = p_chby[]
*                                                        tcodes               = p_tcode[]
*                                                        programs             = p_prog[]
*                                                        msgtys               = p_msgty[]
*                                                        msgids               = p_msgid[]
*                                                        msgnos               = p_msgno[]
*                                                        http_statuses        = p_httprc[]
*                                                        request_urls         = p_url[]
*                                                        request_methods      = p_meth[]
*                                                        message_texts        = p_msgtx[]
*                                                        request_payloads     = p_rqpay[]
*                                                        response_payloads    = p_repay[]
*                                                        with_real_data       = p_real
*                                                        with_simulation_data = p_sim ).
*    catch zcx_lso_log into data(lo_cx_lso_log).
*      write: lo_cx_lso_log->get_text( ).
*      exit.
*  endtry.
*
*
** just for testing
*  data: lt_context type table of zlso_s_log_context.
*  data: lt_context2 type table of zlso_s_log_context2.
*
*
*
*  append initial line to lt_context assigning field-symbol(<fs_contxt>).
*  <fs_contxt>-log_id = '051MfGM97jMvtJaT821KuW'.
*  <fs_contxt>-log_seqnr = '000001'.
*  <fs_contxt>-user_id = 'user1'.
*
*
*  append initial line to lt_context assigning <fs_contxt>.
*  <fs_contxt>-log_id = '051MfGM97jMvtJaT821KuW'.
*  <fs_contxt>-log_seqnr = '000002'.
*  <fs_contxt>-user_id = 'user2'.
*
*  append initial line to lt_context2 assigning field-symbol(<fs_contxt2>).
*  <fs_contxt2>-log_id = '051MfGM97jMvtJaT821KuW'.
*  <fs_contxt2>-log_seqnr = '000003'.
*  <fs_contxt2>-nachn = 'Brzeczyszczykiewicz'.
*
*
*
*  data lr_context type ref to data.
*  if p_con1 = abap_true.
*    get reference of lt_context into lr_context.
*  else.
*    get reference of lt_context2 into lr_context.
*  endif.
*
*
*
*
*
** call FM t o display
*  call function 'Z_LSO_TRACE_DISPLAY'
*    exporting
*      it_logs    = lt_logs
*      ir_context = lr_context.
*
*
*
** call it second time but with empty lt_logs
*
*
*  get reference of lt_context2 into lr_context.
** call FM to display
*  call function 'Z_LSO_TRACE_DISPLAY'
*    exporting
*      it_logs    = lt_logs
*      ir_context = lr_context.





*call method ZCL_LSO_TRACE_DISPLAY=>start_trace_report
*  exporting
*    it_log_id   = p_id[]
*    it_log_date = p_ldate[]
*    ir_context  = lr_context
*    .
