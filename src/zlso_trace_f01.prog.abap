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

form jump_to_log_stripper.
  try.
      new zcl_lso_log_stripper( )->execute( ).
    catch zcx_lso_log into data(cx_log).
      message cx_log->get_text( ) type zif_lso_log_message=>c_type-error.
  endtry.
endform.
