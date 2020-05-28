*&---------------------------------------------------------------------*
*&  Include  zlso_log_stripper_f01
*&---------------------------------------------------------------------*

form refresh_customizing.
  " Delete payloads after NNN days
  p_cdelp = lcl_main=>create( )->get_customizing_number( zif_lso_log_cust_factory=>id-delete_payload_after ).

  " Delete traces after NNN days
  p_cdelt = lcl_main=>create( )->get_customizing_number( zif_lso_log_cust_factory=>id-delete_trace_after ).

  " Delete messages after NNN days
  p_cdelm = lcl_main=>create( )->get_customizing_number( zif_lso_log_cust_factory=>id-delete_message_after ).

  " Delete logs after NNN days
  p_cdell = lcl_main=>create( )->get_customizing_number( zif_lso_log_cust_factory=>id-delete_log_after ).

  message 'Operation finished'(001) type zif_lso_log_message=>c_type-success.
endform.


form save_customizing.
  data custom type ref to zif_lso_log_cust.

  try.
      " Delete payloads after NNNN days
      custom = lcl_main=>create( )->get_customizing( zif_lso_log_cust_factory=>id-delete_payload_after ).
      custom->set_value( conv #( p_cdelp ) ).
      custom->save( ).

      " Delete traces after NNNN days
      custom = lcl_main=>create( )->get_customizing( zif_lso_log_cust_factory=>id-delete_trace_after ).
      custom->set_value( conv #( p_cdelt ) ).
      custom->save( ).

      " Delete messages after NNNN days
      custom = lcl_main=>create( )->get_customizing( zif_lso_log_cust_factory=>id-delete_message_after ).
      custom->set_value( conv #( p_cdelm ) ).
      custom->save( ).

      " Delete logs after NNNN days
      custom = lcl_main=>create( )->get_customizing( zif_lso_log_cust_factory=>id-delete_log_after ).
      custom->set_value( conv #( p_cdell ) ).
      custom->save( ).

      message 'Operation finished'(001) type zif_lso_log_message=>c_type-success.
    catch zcx_lso_log into data(cx_log).
      message cx_log->get_text( ) type zif_lso_log_message=>c_type-error.
  endtry.
endform.


form validate_test_mode_block.
  perform toggle_test_mode_screen.

  if p_testm eq abap_true and s_logids[] is initial.
    message 'Please provide Log Id!' type zif_lso_log_message=>c_type-error.
  endif.
endform.


form selection_screen_output.
  perform toggle_test_mode_screen.
endform.


form toggle_test_mode_screen.
  loop at screen.
    " Switch parameters related to test mode.
    if screen-group1 eq 'TST' and ( screen-group3 eq 'LOW' or screen-group3 eq 'VPU' or screen-group3 eq 'OPU' ).
      " Change 'Log Id' input parameter according to the selected test mode.
      screen-input  = cond #( when p_testm eq abap_true then '1' else '0' ).
      screen-output = '1'.
      modify screen.
    endif.
  endloop.
endform.


form final_confirmation.
  data(answer) = value char1( ).

  call function 'POPUP_TO_CONFIRM'
    exporting
      titlebar              = 'Final confirmation'(007)
      text_question         = 'Are you sure you want to delete the data?'(008)
      default_button        = '2'    " Cursor position
      display_cancel_button = abap_false
    importing
      answer                = answer
    exceptions
      text_not_found        = 1
      others                = 2.

  if sy-subrc ne 0 or answer ne '1'.
    leave screen.
  endif.
endform.
