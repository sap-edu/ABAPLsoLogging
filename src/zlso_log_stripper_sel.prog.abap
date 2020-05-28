*&---------------------------------------------------------------------*
*&  Include  zlso_log_stripper_sel
*&---------------------------------------------------------------------*

" Test Mode
selection-screen begin of block test-mode with frame title text-s01.
" Log Ids
select-options s_logids for zlso_log-id no intervals modif id tst.

selection-screen begin of line.
" Test-Mode
parameters p_testm as checkbox user-command testmode.
selection-screen comment (40) text-s02 for field p_testm.
selection-screen end of line.
" No DB Update
parameters p_nodbup as checkbox.
selection-screen end of block test-mode.

" Customizing
selection-screen begin of block customizing with frame title text-s03.
selection-screen begin of line.
selection-screen comment (30) text-s04 for field p_testm.
selection-screen end of line.

" Payloads after NNN days
selection-screen begin of line.
selection-screen comment 1(31) text-s05 for field p_cdelp.
parameters p_cdelp type int4 visible length 8 modif id cus obligatory.
selection-screen comment 43(6) text-s09.
selection-screen end of line.

" Traces after NNN days
selection-screen begin of line.
selection-screen comment 1(31) text-s06 for field p_cdelt.
parameters p_cdelt type int4 visible length 8 modif id cus obligatory.
selection-screen comment 43(6) text-s09.
selection-screen end of line.

" Messages after NNN days
selection-screen begin of line.
selection-screen comment 1(31) text-s07 for field p_cdelm.
parameters p_cdelm type int4 visible length 8 modif id cus obligatory.
selection-screen comment 43(6) text-s09.
selection-screen end of line.

" Logs after NNN days
selection-screen begin of line.
selection-screen comment 1(31) text-s08 for field p_cdell.
parameters p_cdell type int4 visible length 8 modif id cus obligatory.
selection-screen comment 43(6) text-s09.
selection-screen end of line.

" Refresh customization
selection-screen begin of line.
selection-screen pushbutton 33(18) text-s10 user-command refresh_customizing.
" Save customization
selection-screen pushbutton 52(18) text-s11 user-command save_customizing modif id cus.
selection-screen end of line.
selection-screen end of block customizing.


" On test mode switch...
at selection-screen on p_testm.
  syucomm = sy-ucomm.

  " On test mode parameters...
at selection-screen on block test-mode.
  perform validate_test_mode_block.

  " Delete payloads/headers after NNNN days
at selection-screen on p_cdelp.
  if p_cdelp <= 0.
    message 'Parameter value MUST be > 0'(e03) type zif_lso_log_message=>c_type-error.
  endif.

  " Delete traces after NNNN days
at selection-screen on p_cdelt.
  if p_cdelt <= 0.
    message 'Parameter value MUST be > 0'(e03) type zif_lso_log_message=>c_type-error.
  endif.

  " Delete messages after NNNN days
at selection-screen on p_cdelm.
  if p_cdelm <= 0.
    message 'Parameter value MUST be > 0'(e03) type zif_lso_log_message=>c_type-error.
  endif.

  " Delete logs after NNNN days
at selection-screen on p_cdell.
  if p_cdell <= 0.
    message 'Parameter value MUST be > 0'(e03) type zif_lso_log_message=>c_type-error.
  endif.

  " On selection-screen action...
at selection-screen.
  case sy-ucomm.
    when 'REFRESH_CUSTOMIZING'.
      perform refresh_customizing.

    when 'SAVE_CUSTOMIZING'.
      perform save_customizing.

    when 'ONLI'.
      if p_nodbup ne abap_true and sy-batch ne abap_true.
        perform final_confirmation.
      endif.

    when others.
  endcase.

  " Screen PBO...
at selection-screen output.
  perform selection_screen_output.
