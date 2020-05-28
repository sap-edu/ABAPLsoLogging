FUNCTION Z_LSO_TRACE_DISPLAY.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_LOGS) TYPE  ZLSO_TT_LOG_OBJECTS
*"     REFERENCE(IR_CONTEXT) TYPE REF TO  DATA OPTIONAL
*"----------------------------------------------------------------------

  if go_trace_dis_app is not bound.
    create object go_trace_dis_app.
  endif.

  go_trace_dis_app->collect_data( it_logs    = it_logs
                                  ir_context = ir_context ).
  call screen 100.

endfunction.
