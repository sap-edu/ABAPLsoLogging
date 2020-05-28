class lcl_lso_log_handler_unit definition deferred.
class zcl_lso_log_handler definition local friends lcl_lso_log_handler_unit.

class lcl_lso_log_handler_unit definition for testing
  duration short
  inheriting from zcl_lso_log_unit
  risk level harmless.

  private section.
    constants begin of c_message.
    constants msgid type zlso_log_message-msgid value 'ZLSO_LOG'.
    constants msgno type zlso_log_message-msgno value '000'.
    constants msgv1 type string value 'Var1'.
    constants msgv2 type string value 'Var2'.
    constants msgv3 type string value 'Var3'.
    constants msgv4 type string value 'Var4'.
    constants end of c_message.

    constants c_trace_id type zlso_log_trace-id value 'LTC_LOG_HANDLER'.

    data cut type ref to zcl_lso_log_handler.  "class under test

    methods setup.
    methods teardown.

    methods message for testing.
    methods trace_message for testing.
    methods exception for testing.
    methods error for testing.
    methods info for testing.
    methods success for testing.
    methods warning for testing.
    methods get_messages for testing.
    methods get_messages_rfc for testing raising cx_static_check.
    methods to_log for testing.
endclass.


class lcl_lso_log_handler_unit implementation.

  method setup.
    me->cut = new #( ).
  endmethod.


  method teardown.
    clear me->cut.
  endmethod.


  method get_messages_rfc.
    constants: c_success_text type string value 'SUCCESS',
               c_error_text   type string value 'ERROR'.

    " Given we have 1 Success
    cut->zif_lso_log_handler~message( msgty = zif_lso_log_message=>c_type-success
                                      msgid = zcx_lso_log=>zcx_lso_log-msgid
                                      msgno = zcx_lso_log=>zcx_lso_log-msgno
                                      msgv1 = c_success_text ).

    " and 1 Error in our log handler
    cut->zif_lso_log_handler~message( msgty = zif_lso_log_message=>c_type-error
                                      msgid = zcx_lso_log=>zcx_lso_log-msgid
                                      msgno = zcx_lso_log=>zcx_lso_log-msgno
                                      msgv1 = c_error_text ).

    " When we ask for RFC usable message return for all messages
    data(messages) = cut->zif_lso_log_abstract~get_messages_rfc( ).

    " Then we get all messages back
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( messages ) ).

    " When we ask for Messages of a spcecific type "error"
    messages = cut->zif_lso_log_abstract~get_messages_rfc( type = zif_lso_log_message=>c_type-error ).

    " Then we get only error messages back
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( messages ) ).

    " and that message has the expected error text
    cl_abap_unit_assert=>assert_true( cond #( when messages[ 1 ]-text ca c_error_text
                                              then abap_true ) ).
  endmethod.


  method get_messages.
    cl_abap_unit_assert=>assert_bound( me->cut->get_messages( ) ).
    cl_abap_unit_assert=>assert_bound( me->cut->get_messages( zif_lso_log_message=>c_type-error ) ).
  endmethod.


  method message.

    data(message) = cut->zif_lso_log_handler~message(
      msgty = 'S'
      msgid = 'ZLSO_LOG'
      msgno = '001'
      msgv1 = 'Test1'
      msgv2 = 'Test2'
      msgv3 = 'Lorem Ipsum is simply dummy text of the printing and typesetting industry.' "more than 50 chars
      msgv4 = 'Test4' ).

    cl_abap_unit_assert=>assert_bound( message ).
    cl_abap_unit_assert=>assert_not_initial( message->get_text( ) ).

    message = cut->zif_lso_log_handler~message(
      msgty = 'S'
      msgid = 'ZLSO_LOG'
      msgno = '001'
      msgv1 = 'Test1'
      msgv2 = 'Test2'
      msgv3 = 'Lorem Ipsum is simply dummy text.'  "less than 50 chars
      msgv4 = 'Test4' ).

    cl_abap_unit_assert=>assert_bound( message ).
    cl_abap_unit_assert=>assert_not_initial( message->get_text( ) ).

  endmethod.

  method trace_message.
    data(message) = cut->zif_lso_log_handler~trace_message(
      trace = new zcl_lso_log_trace( request_url      = 'https://saplearninghub-sbx.plateau.com/learning/oauth-api/rest/v1/token/learning/odatav4/user/v1/Users' "more than 50 chars
                                     request_method   = 'POST'
                                     http_status      = '200'
                                     request_payload  = ''
                                     response_payload = '' )
      msgid = zcx_lso_log=>lock_error-msgid
      msgno = zcx_lso_log=>lock_error-msgno
      msgv2 = |User Transfer| ).

    cl_abap_unit_assert=>assert_bound( message ).
    cl_abap_unit_assert=>assert_not_initial( message->get_text( ) ).
  endmethod.


  method exception.
*   TEST EXCEPTION WITH MESSAGE CLASS
    try.
        raise exception type zcx_lso_log
          exporting
            mv_msgv1 = 'MsgVar1'
            mv_msgv2 = '-MsgVar2'
            mv_msgv3 = '-MsgVar3'
            mv_msgv4 = '-MsgVar4'.
      catch zcx_lso_log into data(cx_t100).
        me->cut->zif_lso_log_abstract~exception( cx_t100 ).
    endtry.

    cl_abap_unit_assert=>assert_true( cut->zif_lso_log_abstract~has_messages( ) ).

    cl_abap_unit_assert=>assert_equals( act = cut->get_messages( )->size( ) exp = 1 ).

    data(message) = cast zcl_lso_log_message( cut->get_messages( )->get( 1 ) ).

    cl_abap_unit_assert=>assert_equals( act = message->get_symsg( )-msgid exp = 'ZLSO_LOG' ).

    cl_abap_unit_assert=>assert_equals( act = message->get_text( )
                                        exp = 'MsgVar1-MsgVar2-MsgVar3-MsgVar4' ).

*   TEST EXCEPTION WITHOUT MESSAGE CLASS
    try.
        raise exception type zcx_lso_log
          exporting
            mv_msgv1 = 'This is ZCX_LSO_LOG...'.
      catch zcx_lso_log into data(cx).
        cut->zif_lso_log_abstract~exception( cx ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = cut->get_messages( )->size( ) exp = 2 ).

    message = cast zcl_lso_log_message( cut->get_messages( )->get( 2 ) ).

    cl_abap_unit_assert=>assert_equals( act = message->get_symsg( )-msgid
                                        exp = 'ZLSO_LOG' ).

    cl_abap_unit_assert=>assert_equals( act = message->get_text( )
                                        exp = 'This is ZCX_LSO_LOG...' ).
  endmethod.


  method error.
    data(message) = me->cut->zif_lso_log_handler~error( msgid = c_message-msgid
                                                        msgno = c_message-msgno
                                                        msgv1 = c_message-msgv1
                                                        msgv2 = c_message-msgv2
                                                        msgv3 = c_message-msgv3
                                                        msgv4 = c_message-msgv4
                                                        trace = me->create_trace( c_trace_id ) ).

    cl_abap_unit_assert=>assert_bound( message ).
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_error( ) ).
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_message( message ) ).
  endmethod.


  method info.
    data(message) = me->cut->zif_lso_log_handler~info( msgid = c_message-msgid
                                                       msgno = c_message-msgno
                                                       msgv1 = c_message-msgv1
                                                       msgv2 = c_message-msgv2
                                                       msgv3 = c_message-msgv3
                                                       msgv4 = c_message-msgv4
                                                       trace = me->create_trace( c_trace_id ) ).

    cl_abap_unit_assert=>assert_bound( message ).
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_info( ) ).
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_message( message ) ).
  endmethod.


  method success.
    data(message) = me->cut->zif_lso_log_handler~success( msgid = c_message-msgid
                                                          msgno = c_message-msgno
                                                          msgv1 = c_message-msgv1
                                                          msgv2 = c_message-msgv2
                                                          msgv3 = c_message-msgv3
                                                          msgv4 = c_message-msgv4
                                                          trace = me->create_trace( c_trace_id ) ).

    cl_abap_unit_assert=>assert_bound( message ).
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_success( ) ).
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_message( message ) ).
  endmethod.


  method warning.
    data(message) = me->cut->zif_lso_log_handler~warning( msgid = c_message-msgid
                                                          msgno = c_message-msgno
                                                          msgv1 = c_message-msgv1
                                                          msgv2 = c_message-msgv2
                                                          msgv3 = c_message-msgv3
                                                          msgv4 = c_message-msgv4
                                                          trace = me->create_trace( c_trace_id ) ).

    cl_abap_unit_assert=>assert_bound( message ).
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_warning( ) ).
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_abstract~has_message( message ) ).
  endmethod.


  method to_log.
    data(message) = me->cut->zif_lso_log_handler~success( msgid = c_message-msgid
                                                          msgno = c_message-msgno
                                                          msgv1 = c_message-msgv1
                                                          msgv2 = c_message-msgv2
                                                          msgv3 = c_message-msgv3
                                                          msgv4 = c_message-msgv4 ).

    data(log) = me->cut->to_log( ).

    cl_abap_unit_assert=>assert_bound( log ).
    cl_abap_unit_assert=>assert_true( log->zif_lso_log_abstract~has_messages( ) ).
  endmethod.

endclass.
