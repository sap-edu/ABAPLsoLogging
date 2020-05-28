*"* use this source file for your ABAP unit test classes
class ltd_lso_log_abstract definition for testing
  inheriting from zcl_lso_log_abstract
  create public.
endclass.

class ltd_lso_log_abstract implementation.
endclass.


class ltc_lso_log_abstract definition final for testing
  duration short
  risk level harmless.

  private section.

    constants begin of c_message.
    constants msgid type zlso_log_message-msgid value 'ZLSO_LOG'.
    constants msgno type zlso_log_message-msgno value '001'.  " &1 &2 &3 &4
    constants msgv1 type string value 'Var1'.
    constants msgv2 type string value 'Var2'.
    constants msgv3 type string value 'Var3'.
    constants msgv4 type string value 'Var4'.
    constants end of c_message.

    constants begin of c_long.
    constants msgv1 type string value 'Lorem Ipsum is simply dummy text of the printing and typesetting industry.'.
    constants end of c_long.

    " ZCL_LSO_LOG_ABSTRACT cannot be instantiated, therefore go for local one.
    data cut type ref to ltd_lso_log_abstract.

    class-methods class_setup.
    class-methods class_teardown.

    methods setup.
    methods teardown.

    methods exception for testing.
    methods exception_abort for testing.
    methods exception_long_text for testing.
    methods exception_previous for testing.
    methods add_message for testing.
    methods add_messages for testing.
    methods add_symsg for testing.
    methods get_messages for testing.
    methods get_messages_rfc for testing.
    methods get_symsgs for testing.
    methods get_program for testing.
    methods get_tcode for testing.
    methods has_error for testing.
    methods has_error_abort for testing.
    methods has_info for testing.
    methods has_success for testing.
    methods has_warning for testing.
    methods has_abort for testing.
    methods has_exception for testing.
    methods has_message for testing.
    methods has_message_type for testing.
    methods has_message_key for testing.
    methods has_messages for testing.
    methods has_error_after for testing raising cx_static_check.
    methods success for testing.
    methods error for testing.
    methods warning for testing.
    methods abort for testing.
    methods info for testing.

endclass.


class ltc_lso_log_abstract implementation.

  method class_setup.
  endmethod.


  method class_teardown.
*   Rollback is executed automatically...
  endmethod.


  method setup.
    me->cut = new #( ).
  endmethod.


  method teardown.
  endmethod.


  method exception.
    " Not found - &1 &2 &3 &4
    data(exception) = new zcx_lso_log( textid   = zcx_lso_log=>not_found
                                       mv_msgv1 = c_message-msgv1
                                       mv_msgv2 = c_message-msgv2
                                       mv_msgv3 = c_message-msgv3
                                       mv_msgv4 = c_message-msgv4 ).

    me->cut->zif_lso_log_abstract~exception( exception ).

    " Exception message was added correctly?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_messages( ) ).

    data(messages) = me->cut->zif_lso_log_abstract~get_messages( ).

    " One message found?
    cl_abap_unit_assert=>assert_equals( act = messages->size( ) exp = 1 ).

    " Message structure as expected?
    cl_abap_unit_assert=>assert_equals( act = cast zcl_lso_log_message( messages->get( 1 ) )->get_symsg( )
                                        exp = value symsg( msgty = zif_lso_log_message=>c_type-error
                                                           msgid = zcx_lso_log=>not_found-msgid
                                                           msgno = zcx_lso_log=>not_found-msgno
                                                           msgv1 = c_message-msgv1
                                                           msgv2 = c_message-msgv2
                                                           msgv3 = c_message-msgv3
                                                           msgv4 = c_message-msgv4 ) ).
  endmethod.


  method exception_abort.
    " Not found - &1 &2 &3 &4
    data(exception) = new zcx_lso_log( textid   = zcx_lso_log=>not_found
                                       mv_msgv1 = c_message-msgv1
                                       mv_msgv2 = c_message-msgv2
                                       mv_msgv3 = c_message-msgv3
                                       mv_msgv4 = c_message-msgv4
                                       severity = zif_lso_cx=>severity-abort ).

    me->cut->zif_lso_log_abstract~exception( exception ).

    " Exception message was added correctly?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_messages( ) ).

    data(messages) = me->cut->zif_lso_log_abstract~get_messages( ).

    " One message found?
    cl_abap_unit_assert=>assert_equals( act = messages->size( ) exp = 1 ).

    " Message structure as expected?
    cl_abap_unit_assert=>assert_equals( act = cast zcl_lso_log_message( messages->get( 1 ) )->get_symsg( )
                                        exp = value symsg( msgty = zif_lso_log_message=>c_type-abort
                                                           msgid = zcx_lso_log=>not_found-msgid
                                                           msgno = zcx_lso_log=>not_found-msgno
                                                           msgv1 = c_message-msgv1
                                                           msgv2 = c_message-msgv2
                                                           msgv3 = c_message-msgv3
                                                           msgv4 = c_message-msgv4 ) ).
  endmethod.


  method exception_long_text.
    " Unit Test Message :) &1 &2 &3 &4
    data(exception) = new zcx_lso_log( textid   = zcx_lso_log=>unit_test_message
                                       mv_msgv1 = c_long-msgv1
                                       severity = zif_lso_cx=>severity-abort ).

    me->cut->zif_lso_log_abstract~exception( exception ).

    data(message) = cast zcl_lso_log_message( me->cut->zif_lso_log_abstract~get_messages( zif_lso_log_message=>c_type-abort )->get( 1 ) ).

    data(act) = message->get_text( ).
    data(exp) = |Unit Test Message :) { c_long-msgv1 }|.

    cl_abap_unit_assert=>assert_equals( act = act exp = exp ).
  endmethod.


  method exception_previous.
    " Unit Test Message :) &1 &2 &3 &4
    data(previous) = new zcx_lso_log( textid   = zcx_lso_log=>unit_test_message
                                      mv_msgv1 = c_long-msgv1
                                      severity = zif_lso_cx=>severity-abort ).

    data(exception) = new zcx_lso_log( previous = previous ).

    me->cut->zif_lso_log_abstract~exception( exception ).

    data(message) = cast zcl_lso_log_message( me->cut->zif_lso_log_abstract~get_messages( zif_lso_log_message=>c_type-abort )->get( 1 ) ).

    data(act) = message->get_text( ).
    data(exp) = |Unit Test Message :) { c_long-msgv1 }|.

    cl_abap_unit_assert=>assert_equals( act = act exp = exp ).
  endmethod.


  method add_message.
    " &1 &2 &3 &4
    data(message) = new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-warning
                                             msgid = c_message-msgid
                                             msgno = c_message-msgno
                                             msgv1 = c_message-msgv1
                                             msgv2 = c_message-msgv2
                                             msgv3 = c_message-msgv3
                                             msgv4 = c_message-msgv4 ).

    me->cut->zif_lso_log_abstract~add_message( message ).

    " Message was added correctly?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_messages( ) ).

    data(messages) = me->cut->zif_lso_log_abstract~get_messages( ).

    " One message found?
    cl_abap_unit_assert=>assert_equals( act = messages->size( ) exp = 1 ).

    " Message structure as expected?
    cl_abap_unit_assert=>assert_equals( act = cast zcl_lso_log_message( messages->get( 1 ) )->get_symsg( )
                                        exp = value symsg( msgty = zif_lso_log_message=>c_type-warning
                                                           msgid = c_message-msgid
                                                           msgno = c_message-msgno
                                                           msgv1 = c_message-msgv1
                                                           msgv2 = c_message-msgv2
                                                           msgv3 = c_message-msgv3
                                                           msgv4 = c_message-msgv4 ) ).
  endmethod.


  method add_messages.
    " &1 &2 &3 &4
    data(message1) = new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-warning
                                              msgid = c_message-msgid
                                              msgno = c_message-msgno
                                              msgv1 = c_message-msgv1
                                              msgv2 = c_message-msgv2
                                              msgv3 = c_message-msgv3
                                              msgv4 = c_message-msgv4 ).

    " &1 &2 &3 &4
    data(message2) = new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-error
                                              msgid = c_message-msgid
                                              msgno = c_message-msgno
                                              msgv1 = c_message-msgv1
                                              msgv2 = c_message-msgv2
                                              msgv3 = c_message-msgv3
                                              msgv4 = c_message-msgv4 ).

    data(messages) = new cl_object_collection( ).
    messages->add( message1 ).
    messages->add( message2 ).

    me->cut->zif_lso_log_abstract~add_messages( messages ).

    " Messages were added correctly?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_messages( ) ).

    data(act_messages) = me->cut->zif_lso_log_abstract~get_messages( ).

    " All message found?
    cl_abap_unit_assert=>assert_equals( act = act_messages->size( ) exp = 2 ).
  endmethod.


  method add_symsg.
    me->cut->zif_lso_log_abstract~add_symsg( value symsg( msgty = zif_lso_log_message=>c_type-error
                                                          msgid = c_message-msgid
                                                          msgno = c_message-msgno
                                                          msgv1 = c_message-msgv1
                                                          msgv2 = c_message-msgv2
                                                          msgv3 = c_message-msgv3
                                                          msgv4 = c_message-msgv4 ) ).

    " Message was added correctly?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_messages( ) ).

    data(messages) = me->cut->zif_lso_log_abstract~get_messages( ).

    " One message found?
    cl_abap_unit_assert=>assert_equals( act = messages->size( ) exp = 1 ).

    " Message structure as expected?
    cl_abap_unit_assert=>assert_equals( act = cast zcl_lso_log_message( messages->get( 1 ) )->get_symsg( )
                                        exp = value symsg( msgty = zif_lso_log_message=>c_type-error
                                                           msgid = c_message-msgid
                                                           msgno = c_message-msgno
                                                           msgv1 = c_message-msgv1
                                                           msgv2 = c_message-msgv2
                                                           msgv3 = c_message-msgv3
                                                           msgv4 = c_message-msgv4 ) ).
  endmethod.


  method get_messages.
    me->cut->zif_lso_log_abstract~add_message( new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-success
                                                                        msgid = c_message-msgid
                                                                        msgno = c_message-msgno
                                                                        msgv1 = c_message-msgv1
                                                                        msgv2 = c_message-msgv2
                                                                        msgv3 = c_message-msgv3
                                                                        msgv4 = c_message-msgv4 ) ).

    data(messages) = me->cut->zif_lso_log_abstract~get_messages( ).

    " Are there any messages returned?
    cl_abap_unit_assert=>assert_not_initial( messages ).

    " One message found?
    cl_abap_unit_assert=>assert_equals( act = messages->size( ) exp = 1 ).

    " Message structure as expected?
    cl_abap_unit_assert=>assert_equals( act = cast zcl_lso_log_message( messages->get( 1 ) )->get_symsg( )
                                        exp = value symsg( msgty = zif_lso_log_message=>c_type-success
                                                           msgid = c_message-msgid
                                                           msgno = c_message-msgno
                                                           msgv1 = c_message-msgv1
                                                           msgv2 = c_message-msgv2
                                                           msgv3 = c_message-msgv3
                                                           msgv4 = c_message-msgv4 ) ).
  endmethod.


  method get_messages_rfc.
    me->cut->zif_lso_log_abstract~add_message( new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-success
                                                                        msgid = c_message-msgid
                                                                        msgno = c_message-msgno  "&1 &2 &3 &4
                                                                        msgv1 = c_message-msgv1
                                                                        msgv2 = c_message-msgv2
                                                                        msgv3 = c_message-msgv3
                                                                        msgv4 = c_message-msgv4 ) ).

    me->cut->zif_lso_log_abstract~add_message( new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-warning
                                                                        msgid = c_message-msgid
                                                                        msgno = '000'            "&1&2&3&4
                                                                        msgv1 = c_message-msgv1
                                                                        msgv2 = c_message-msgv2
                                                                        msgv3 = c_message-msgv3
                                                                        msgv4 = c_message-msgv4 ) ).

    data(messages) = me->cut->zif_lso_log_abstract~get_messages_rfc( ).

    " Messages for RFC created?
    cl_abap_unit_assert=>assert_not_initial( messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( messages ) exp = 2 ).

    " First RFC message as expected?
    cl_abap_unit_assert=>assert_equals( act = messages[ 1 ]
                                        exp = value zlso_s_log_rfc_message( type   = zif_lso_log_message=>c_type-success
                                                                            class  = c_message-msgid
                                                                            number = c_message-msgno
                                                                            text   = |{ c_message-msgv1 } { c_message-msgv2 } { c_message-msgv3 } { c_message-msgv4 }| ) ).

    " Second RFC message as expected?
    cl_abap_unit_assert=>assert_equals( act = messages[ 2 ]
                                        exp = value zlso_s_log_rfc_message( type   = zif_lso_log_message=>c_type-warning
                                                                            class  = c_message-msgid
                                                                            number = '000'
                                                                            text   = |{ c_message-msgv1 }{ c_message-msgv2 }{ c_message-msgv3 }{ c_message-msgv4 }| ) ).
  endmethod.


  method get_symsgs.
    me->cut->zif_lso_log_abstract~add_message( new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-success
                                                                        msgid = c_message-msgid
                                                                        msgno = c_message-msgno
                                                                        msgv1 = c_message-msgv1
                                                                        msgv2 = c_message-msgv2
                                                                        msgv3 = c_message-msgv3
                                                                        msgv4 = c_message-msgv4 ) ).

    data(symsgs) = me->cut->zif_lso_log_abstract~get_symsgs( ).

    " Are there any messages returned?
    cl_abap_unit_assert=>assert_not_initial( symsgs ).

    " One message found?
    cl_abap_unit_assert=>assert_equals( act = lines( symsgs ) exp = 1 ).

    " Message structure as expected?
    cl_abap_unit_assert=>assert_equals( act = symsgs[ 1 ]
                                        exp = value symsg( msgty = zif_lso_log_message=>c_type-success
                                                           msgid = c_message-msgid
                                                           msgno = c_message-msgno
                                                           msgv1 = c_message-msgv1
                                                           msgv2 = c_message-msgv2
                                                           msgv3 = c_message-msgv3
                                                           msgv4 = c_message-msgv4 ) ).
  endmethod.


  method get_program.
    me->cut->zif_lso_log_abstract~set_program( sy-cprog ).

    " Is program as expected?
    cl_abap_unit_assert=>assert_equals( act = me->cut->zif_lso_log_abstract~get_program( ) exp = sy-cprog ).
  endmethod.


  method get_tcode.
    me->cut->zif_lso_log_abstract~set_tcode( sy-tcode ).

    " Is transaction code as expected?
    cl_abap_unit_assert=>assert_equals( act = me->cut->zif_lso_log_abstract~get_tcode( ) exp = sy-tcode ).
  endmethod.


  method has_error.
    " &1 &2 &3 &4
    data(message) = new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-error
                                             msgid = c_message-msgid
                                             msgno = c_message-msgno
                                             msgv1 = c_message-msgv1
                                             msgv2 = c_message-msgv2
                                             msgv3 = c_message-msgv3
                                             msgv4 = c_message-msgv4 ).

    me->cut->zif_lso_log_abstract~add_message( message ).

    " Is there an expected error message in the log?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_error( ) ).
  endmethod.


  method has_error_abort.
    " &1 &2 &3 &4
    data(message) = new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-abort
                                             msgid = c_message-msgid
                                             msgno = c_message-msgno
                                             msgv1 = c_message-msgv1
                                             msgv2 = c_message-msgv2
                                             msgv3 = c_message-msgv3
                                             msgv4 = c_message-msgv4 ).

    me->cut->zif_lso_log_abstract~add_message( message ).

    " Is there an expected abort/error message in the log?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_error( ) ).
  endmethod.


  method has_info.
    " &1 &2 &3 &4
    data(message) = new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-info
                                             msgid = c_message-msgid
                                             msgno = c_message-msgno
                                             msgv1 = c_message-msgv1
                                             msgv2 = c_message-msgv2
                                             msgv3 = c_message-msgv3
                                             msgv4 = c_message-msgv4 ).

    me->cut->zif_lso_log_abstract~add_message( message ).

    " Is there an expected information message in the log?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_info( ) ).
  endmethod.


  method has_success.
    " &1 &2 &3 &4
    data(message) = new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-success
                                             msgid = c_message-msgid
                                             msgno = c_message-msgno
                                             msgv1 = c_message-msgv1
                                             msgv2 = c_message-msgv2
                                             msgv3 = c_message-msgv3
                                             msgv4 = c_message-msgv4 ).

    me->cut->zif_lso_log_abstract~add_message( message ).

    " Is there an expected success message in the log?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_success( ) ).
  endmethod.


  method has_warning.
    " &1 &2 &3 &4
    data(message) = new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-warning
                                             msgid = c_message-msgid
                                             msgno = c_message-msgno
                                             msgv1 = c_message-msgv1
                                             msgv2 = c_message-msgv2
                                             msgv3 = c_message-msgv3
                                             msgv4 = c_message-msgv4 ).

    me->cut->zif_lso_log_abstract~add_message( message ).

    " Is there an expected warning message in the log?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_warning( ) ).
  endmethod.


  method has_abort.
    " &1 &2 &3 &4
    data(message) = new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-abort
                                             msgid = c_message-msgid
                                             msgno = c_message-msgno
                                             msgv1 = c_message-msgv1
                                             msgv2 = c_message-msgv2
                                             msgv3 = c_message-msgv3
                                             msgv4 = c_message-msgv4 ).

    me->cut->zif_lso_log_abstract~add_message( message ).

    " Is there an expected warning message in the log?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_abort( ) ).
  endmethod.


  method has_exception.
    " Not found - &1 &2 &3 &4
    data(exception) = new zcx_lso_log( textid   = zcx_lso_log=>not_found
                                       mv_msgv1 = c_message-msgv1
                                       mv_msgv2 = c_message-msgv2
                                       mv_msgv3 = c_message-msgv3
                                       mv_msgv4 = c_message-msgv4 ).

    me->cut->zif_lso_log_abstract~exception( exception ).

    " Is there an expected exception in the log?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_exception( zcx_lso_log=>not_found ) ).
  endmethod.


  method has_message.
    " &1 &2 &3 &4
    data(message) = new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-warning
                                             msgid = c_message-msgid
                                             msgno = c_message-msgno
                                             msgv1 = c_message-msgv1
                                             msgv2 = c_message-msgv2
                                             msgv3 = c_message-msgv3
                                             msgv4 = c_message-msgv4 ).

    me->cut->zif_lso_log_abstract~add_message( message ).

    " Is there an expected message in the log?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_message( message ) ).
  endmethod.


  method has_message_key.
    " &1 &2 &3 &4
    data(message) = new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-warning
                                             msgid = zcx_lso_log=>zcx_lso_log-msgid
                                             msgno = zcx_lso_log=>zcx_lso_log-msgno ).

    me->cut->zif_lso_log_abstract~add_message( message ).

    " Is there an expected message with this key in the log?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_message_key( msgid = zcx_lso_log=>zcx_lso_log-msgid
                                                                                     msgno = zcx_lso_log=>zcx_lso_log-msgno ) ).
  endmethod.


  method has_message_type.
    " &1 &2 &3 &4
    data(message) = new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-warning
                                             msgid = c_message-msgid
                                             msgno = c_message-msgno
                                             msgv1 = c_message-msgv1
                                             msgv2 = c_message-msgv2
                                             msgv3 = c_message-msgv3
                                             msgv4 = c_message-msgv4 ).

    me->cut->zif_lso_log_abstract~add_message( message ).

    " Is there an expected message with the type in the log?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_message_type( zif_lso_log_message=>c_type-warning ) ).
  endmethod.


  method has_error_after.
    data(cast) = cast zif_lso_log_abstract( me->cut ).

    data(before_first_message) = value timestampl( ).
    get time stamp field before_first_message.

    cl_abap_unit_assert=>assert_false( cast->has_error( after = before_first_message ) ).

    cast->add_message( new #( msgty = zif_lso_log_message=>c_type-error
                              msgid = c_message-msgid
                              msgno = c_message-msgno ) ).

    cl_abap_unit_assert=>assert_true( cast->has_error( after = before_first_message ) ).

    data(before_second_message) = value timestampl( ).
    get time stamp field before_second_message.

    cl_abap_unit_assert=>assert_false( cast->has_error( after = before_second_message ) ).

    cast->add_message( new #( msgty = zif_lso_log_message=>c_type-error
                              msgid = c_message-msgid
                              msgno = c_message-msgno ) ).

    cl_abap_unit_assert=>assert_true( cast->has_error( after = before_second_message ) ).
    cl_abap_unit_assert=>assert_true( cast->has_error( after = before_first_message ) ).
  endmethod.


  method has_messages.
    " &1 &2 &3 &4
    data(message) = new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-warning
                                             msgid = c_message-msgid
                                             msgno = c_message-msgno
                                             msgv1 = c_message-msgv1
                                             msgv2 = c_message-msgv2
                                             msgv3 = c_message-msgv3
                                             msgv4 = c_message-msgv4 ).

    me->cut->zif_lso_log_abstract~add_message( message ).

    " Are there any messages in the log?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_messages( ) ).
  endmethod.


  method abort.
    data(message) = me->cut->zif_lso_log_abstract~abort( msgid = c_message-msgid
                                                         msgno = c_message-msgno
                                                         msgv1 = c_message-msgv1
                                                         msgv2 = c_message-msgv2
                                                         msgv3 = c_message-msgv3
                                                         msgv4 = c_message-msgv4 ).

    cl_abap_unit_assert=>assert_true( message->is_abort( ) ).
  endmethod.


  method error.
    data(message) = me->cut->zif_lso_log_abstract~error( msgid = c_message-msgid
                                                         msgno = c_message-msgno
                                                         msgv1 = c_message-msgv1
                                                         msgv2 = c_message-msgv2
                                                         msgv3 = c_message-msgv3
                                                         msgv4 = c_message-msgv4 ).

    cl_abap_unit_assert=>assert_true( message->is_error( ) ).
  endmethod.


  method info.
    data(message) = me->cut->zif_lso_log_abstract~info( msgid = c_message-msgid
                                                        msgno = c_message-msgno
                                                        msgv1 = c_message-msgv1
                                                        msgv2 = c_message-msgv2
                                                        msgv3 = c_message-msgv3
                                                        msgv4 = c_message-msgv4 ).

    cl_abap_unit_assert=>assert_true( message->is_info( ) ).
  endmethod.


  method success.
    data(message) = me->cut->zif_lso_log_abstract~success( msgid = c_message-msgid
                                                           msgno = c_message-msgno
                                                           msgv1 = c_message-msgv1
                                                           msgv2 = c_message-msgv2
                                                           msgv3 = c_message-msgv3
                                                           msgv4 = c_message-msgv4 ).

    cl_abap_unit_assert=>assert_true( message->is_success( ) ).
  endmethod.


  method warning.
    data(message) = me->cut->zif_lso_log_abstract~warning( msgid = c_message-msgid
                                                           msgno = c_message-msgno
                                                           msgv1 = c_message-msgv1
                                                           msgv2 = c_message-msgv2
                                                           msgv3 = c_message-msgv3
                                                           msgv4 = c_message-msgv4 ).

    cl_abap_unit_assert=>assert_true( message->is_warning( ) ).
  endmethod.

endclass.
