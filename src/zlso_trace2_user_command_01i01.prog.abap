*----------------------------------------------------------------------*
***INCLUDE ZLSO_TRACE2_USER_COMMAND_01I01.
*----------------------------------------------------------------------*

module user_command_0100 input.
 data(lv_ok0100) = gv_ok0100.

  clear gv_ok0100.

  case lv_ok0100.
*   BACK/EXIT/CANCEL
    when c_user_command-back or
         c_user_command-exit or
         c_user_command-cancel.
      "go_trace_dis_app->free( ).

*      clear go_lc_top.
*      clear go_lc_bot_left.
*      clear go_lc_bot_right.
*      clear go_lc_bot.
*      clear go_splitter_pay.
*      clear go_splitter_all.
*      clear go_lc_all.
*      clear go_trace_dis_app.

*     Go to the selection screen.
      set screen 0.
      leave screen.


    when others.
     " cl_gui_cfw=>dispatch( ).

  endcase.
endmodule.                 " USER_COMMAND_0100  INPUT
