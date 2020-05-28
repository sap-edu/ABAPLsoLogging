*&---------------------------------------------------------------------*
*& Report zlso_log_stripper
*&---------------------------------------------------------------------*
*& Removes old logs with messages, traces, payloads and headers.
*&---------------------------------------------------------------------*
report zlso_log_stripper.

include zlso_log_stripper_top.  " Data declaration
include zlso_log_stripper_sel.  " Selection-Screen
include zlso_log_stripper_cls.  " Classes
include zlso_log_stripper_f01.  " Forms

initialization.
  lcl_main=>create( )->initialization( delete_payloads_after = ref #( p_cdelp )
                                       delete_traces_after   = ref #( p_cdelt )
                                       delete_messages_after = ref #( p_cdelm )
                                       delete_logs_after     = ref #( p_cdell ) ).

start-of-selection.
  data(report) = lcl_main=>create( ) ##NEEDED.

  try.
      report->strip( test_mode             = p_testm
                     log_ids               = s_logids[]
                     delete_logs_after     = p_cdell
                     delete_messages_after = p_cdelm
                     delete_traces_after   = p_cdelt
                     delete_payloads_after = p_cdelp
                     no_db_update          = p_nodbup ).
    catch lcx_report into data(cx_report) ##NEEDED.
      message cx_report->get_text( ) type zif_lso_log_message=>c_type-success
        display like zif_lso_log_message=>c_type-error.
  endtry.

end-of-selection.

  report->display_output( ).
