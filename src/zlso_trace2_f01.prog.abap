*&---------------------------------------------------------------------*
*&  Include           ZLSO_TRACE_F01
*&---------------------------------------------------------------------*

form init_screen_params.
  " fill default time/date
  append initial line to p_ldate assigning field-symbol(<fs_date>).
  if <fs_date> is assigned.
    <fs_date>-sign = 'I'.
    <fs_date>-option = 'BT'.
    <fs_date>-low = sy-datum.
    <fs_date>-high = sy-datum.
  endif.

  append initial line to p_ltime assigning field-symbol(<fs_time>).
  if <fs_time> is assigned.
    <fs_time>-sign = 'I'.
    <fs_time>-option = 'BT'.
    <fs_time>-low = '000000'.
    <fs_time>-high = '235959'.
  endif.
endform.


form show_trace.
  if go_trace_dis_app is not bound.
    create object go_trace_dis_app.
  endif.

  data: lr_context type ref to data.
*  data: lt_context TYPE table of text4096.
*  FIELD-SYMBOLS: <lt_context> type any table.
*
*  ASSIGN lt_context to <lt_context>.

*  import trace_context to lt_context from memory id 'TRACE_CONTEXT'.

*  get REFERENCE OF <lt_context> into lr_context.
*
  go_trace_dis_app->collect_data( it_logs = gt_logs
                                  ir_context = lr_context
                                  ).

  call screen 100.
endform.
