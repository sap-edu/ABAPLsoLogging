*----------------------------------------------------------------------*
***INCLUDE LZLSO_TRACEI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.

  data(ok0100) = gv_ok0100.

  clear gv_ok0100.

  case ok0100.

    when zcl_lso_trace_display=>c_action-where_used or
         zcl_lso_trace_display=>c_action-show_hide_details or
         zcl_lso_trace_display=>c_action-export.

      go_trace_dis_app->on_user_command( ok0100 ).


*   BACK/EXIT/CANCEL
    when c_user_command-back or
         c_user_command-exit or
         c_user_command-cancel.
      " Exit application
      go_trace_dis_app->free( ).

      go_splitter_all->free( ).
      clear go_splitter_all.

      " Go to the selection screen.
      set screen 0.
      leave screen.

    when others.
      cl_gui_cfw=>dispatch( ).

  endcase.
endmodule.                 " USER_COMMAND_0100  INPUT
