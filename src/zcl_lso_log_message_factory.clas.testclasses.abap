*"* use this source file for your ABAP unit test classes
class ltc_lso_log_message_factory definition final for testing
  inheriting from zcu_lso_log
  duration short
  risk level harmless.

  private section.
    constants begin of c_msg.
    constants log_id type zlso_log_message-log_id value 'LTC_LOG_MSG_FACTORY'.
    constants msgid type zlso_log_message-msgid value 'ZLSO_LOG'.
    constants end of c_msg.

    data mo_cut type ref to zif_lso_log_message_factory.

    class-methods class_setup.
    class-methods class_teardown.

    methods setup.
    methods teardown.

    methods get_last_error_by_log_id for testing raising zcx_lso_log.
    methods get_last_success_by_log_id for testing raising zcx_lso_log.
    methods get_last_warning_by_log_id for testing raising zcx_lso_log.
    methods get_last_by_log_id for testing raising zcx_lso_log.
    methods get_last_not_existing for testing raising zcx_lso_log.
    methods get_last_stripped for testing raising zcx_lso_log.
endclass.


class ltc_lso_log_message_factory implementation.

  method class_setup.
  endmethod.


  method class_teardown.
*   Rollback is executed automatically...
  endmethod.


  method setup.
    me->mo_cut = zcl_lso_log_message_factory=>instance( ).
  endmethod.


  method teardown.
  ##TODO
    " Just to be double sure all test data is deleted!
*    me->delete_log( new zcl_lso_log( c_msg-log_id ) ).
  endmethod.


  method get_last_error_by_log_id.
  ##TODO
*    " Main log
*    data(log) = new zcl_lso_log_builder(
*        )->set_log_id( c_msg-log_id
*        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 1 error no trace' )->build( )
*        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'W' )->set_msgv1( 'Message 2 warning no trace' )->build( )
*        )->build( ).
*
*    data(exp_message) = new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 3 error no trace' )->build( ).
*
*    log->add( exp_message ).
*    log->add( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'S' )->set_msgv1( 'Message 4 success no trace' )->build( ) ).
*
*    log->save( ).
*
*    " Add log to the buffer for further tear down.
*    me->add_log_to_teardown( log ).
*
*    " Try to find last error message.
*    data(act_message) = me->mo_cut->get_last_error_by_log_id( c_msg-log_id ).
*
*    cl_abap_unit_assert=>assert_bound( act_message ).
*
*    cl_abap_unit_assert=>assert_equals( act = act_message->get_symsg( )
*                                        exp = exp_message->get_symsg( ) ).
  endmethod.


  method get_last_success_by_log_id.
    ##TODO
*    " Main log
*    data(log) = new zcl_lso_log_builder(
*        )->set_log_id( c_msg-log_id
*        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 1 error no trace' )->build( )
*        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'W' )->set_msgv1( 'Message 2 warning no trace' )->build( )
*        )->build( ).
*
*    data(exp_message) = new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'S' )->set_msgv1( 'Message 3 success no trace' )->build( ).
*
*    log->add( exp_message ).
*    log->add( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 4 error no trace' )->build( ) ).
*
*    log->save( ).
*
*    " Add log to the buffer for further tear down.
*    me->add_log_to_teardown( log ).
*
*    " Try to find last success message.
*    data(act_message) = me->mo_cut->get_last_success_by_log_id( c_msg-log_id ).
*
*    cl_abap_unit_assert=>assert_bound( act_message ).
*
*    cl_abap_unit_assert=>assert_equals( act = act_message->get_symsg( )
*                                        exp = exp_message->get_symsg( ) ).
  endmethod.


  method get_last_warning_by_log_id.
    ##TODO
*    " Main log
*    data(log) = new zcl_lso_log_builder(
*        )->set_log_id( c_msg-log_id
*        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 1 error no trace' )->build( )
*        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'W' )->set_msgv1( 'Message 2 warning no trace' )->build( )
*        )->build( ).
*
*    data(exp_message) = new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'W' )->set_msgv1( 'Message 3 warning no trace' )->build( ).
*
*    log->add( exp_message ).
*    log->add( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 4 error no trace' )->build( ) ).
*
*    log->save( ).
*
*    " Add log to the buffer for further tear down.
*    me->add_log_to_teardown( log ).
*
*    " Try to find last success message.
*    data(act_message) = me->mo_cut->get_last_warning_by_log_id( c_msg-log_id ).
*
*    cl_abap_unit_assert=>assert_bound( act_message ).
*
*    cl_abap_unit_assert=>assert_equals( act = act_message->get_symsg( )
*                                        exp = exp_message->get_symsg( ) ).
  endmethod.


  method get_last_by_log_id.
    ##TODO
    " Main log
*    data(log) = new zcl_lso_log_builder(
*        )->set_log_id( c_msg-log_id
*        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 1 error no trace' )->build( )
*        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'W' )->set_msgv1( 'Message 2 warning no trace' )->build( )
*        )->build( ).
*
*    data(exp_message) = new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'S' )->set_msgv1( 'Message 3 success no trace' )->build( ).
*
*    log->add( exp_message ).
*
*    log->save( ).
*
*    " Add log to the buffer for further tear down.
*    me->add_log_to_teardown( log ).
*
*    " Try to find last message.
*    data(act_message) = me->mo_cut->get_last_by_log_id( c_msg-log_id ).
*
*    cl_abap_unit_assert=>assert_bound( act_message ).
*
*    cl_abap_unit_assert=>assert_equals( act = act_message->get_symsg( )
*                                        exp = exp_message->get_symsg( ) ).
  endmethod.


  method get_last_not_existing.
    data(exception_was_raised) = abap_false.

    try.
        " Try to find last not existing message.
        data(act_message) = me->mo_cut->get_last_by_log_id( c_msg-log_id ).
      catch zcx_lso_log into data(cx_log).
        cl_abap_unit_assert=>assert_equals( act = cx_log->if_t100_message~t100key
                                            exp = cx_log->not_found ).

        exception_was_raised = abap_true.
    endtry.

    cl_abap_unit_assert=>assert_true( exception_was_raised ).
  endmethod.


  method get_last_stripped.
    ##TODO
*    " Expected message
*    data(exp_message) = new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid
*                                                         )->set_msgty( 'E'
*                                                         )->set_msgv1( 'Message 1 error no trace'
*                                                         )->set_stripped_date( cl_abap_context_info=>get_system_date( )
*                                                         )->build( ).
*
*    " Main log
*    data(log) = new zcl_lso_log_builder( )->set_log_id( c_msg-log_id
*                                         )->add_message( exp_message
*                                         )->build( ).
*
*    log->save( ).
*
*    " Add log to the buffer for further tear down.
*    me->add_log_to_teardown( log ).
*
*    " Try to find last message.
*    data(act_message) = me->mo_cut->get_last_by_log_id( c_msg-log_id ).
*
*    " Is found message as expected?
*    cl_abap_unit_assert=>assert_bound( act_message ).
*    cl_abap_unit_assert=>assert_equals( act = act_message->get_symsg( )
*                                        exp = exp_message->get_symsg( ) ).
*
*    " Is log stripped?
*    cl_abap_unit_assert=>assert_true( act_message->zif_lso_log_message~is_stripped( ) ).
*
*    " Is stripped date as expected?
*    cl_abap_unit_assert=>assert_equals( act = act_message->zif_lso_log_message~get_stripped_date( ) exp = cl_abap_context_info=>get_system_date( ) ).
  endmethod.

endclass.
