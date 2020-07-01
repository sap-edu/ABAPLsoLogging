class ltc_lso_log_message definition deferred.
class zcl_lso_log_message definition local friends ltc_lso_log_message.

class ltc_lso_log_message definition for testing
  inheriting from zcu_lso_log
  duration short
  risk level harmless.

  private section.
    constants begin of c_message.
    constants log_id type zlso_log_message-log_id value 'LCT_LOG_MESSAGE'.
    constants log_seqnr type zlso_log_message-log_seqnr value '1'.
    constants msgid type zlso_log_message-msgid value 'ZLSO_LOG'.
    constants msgno type zlso_log_message-msgno value '001'.
    constants msgv1 type zlso_log_message-msgv1 value 'Var1' ##NO_TEXT.
    constants text type string value 'Lorem Ipsum is simply dummy text of the printing and type setting industry.' ##NO_TEXT.
    constants end of c_message.

    data cut type ref to zcl_lso_log_message.  "class under test
    data trace type ref to zcl_lso_log_trace.

    class-methods class_setup.
    class-methods class_teardown.

    methods setup.
    methods teardown.

    methods get_class for testing.
    methods get_date for testing.
    methods get_symsg for testing.
    methods get_number for testing.
    methods get_request_payload for testing.
    methods get_response_payload for testing.
    methods get_text for testing.
    methods get_time for testing.
    methods get_timestamp for testing.
    methods get_trace for testing.
    methods get_type for testing.
    methods get_stripped_date for testing.
    methods get_abap_stack for testing.
    methods get_object for testing.
    methods has_trace for testing.
    methods is_error for testing.
    methods is_info for testing.
    methods is_success for testing.
    methods is_warning for testing.
    methods is_abort for testing.
    methods is_stripped for testing.
    methods split_msgv for testing.
    methods set_trace for testing.
    methods clone for testing.
    methods save_collection for testing raising cx_static_check.
    methods lock_exception for testing.
endclass.


class ltc_lso_log_message implementation.

  method class_setup.
  endmethod.


  method class_teardown.
  endmethod.


  method setup.
    me->trace = me->create_trace( request_payload  = '{ "request":"payload" }'
                                  response_payload = '{ "response":"payload" }' ).

    " &1 &2 &3 &4
    me->cut = new #( msgty = zif_lso_log_message=>c_type-info
                     msgid = c_message-msgid
                     msgno = c_message-msgno
                     msgv1 = conv #( c_message-msgv1 )
                     msgv2 = c_message-text
                     trace = me->trace ).
  endmethod.


  method teardown.
    " Saved messages will be deleted with super->teardown( ) method.
  endmethod.


  method get_class.
    cl_abap_unit_assert=>assert_equals( act = me->cut->get_class( ) exp = c_message-msgid ).
  endmethod.


  method get_date.
    cl_abap_unit_assert=>assert_equals( act = me->cut->get_date( ) exp = cl_abap_context_info=>get_system_date( ) ).
  endmethod.


  method get_symsg.
    cl_abap_unit_assert=>assert_equals( act = me->cut->get_symsg( )
                                        exp = value symsg( msgty = zif_lso_log_message=>c_type-info
                                                           msgid = c_message-msgid
                                                           msgno = c_message-msgno
                                                           msgv1 = c_message-msgv1
                                                           msgv2 = 'Lorem Ipsum is simply dummy text of the printing'
                                                           msgv3 = 'and type setting industry.' ) ).
  endmethod.


  method get_number.
    cl_abap_unit_assert=>assert_equals( act = me->cut->get_number( ) exp = c_message-msgno ).
  endmethod.


  method get_request_payload.
    cl_abap_unit_assert=>assert_equals( act = me->cut->get_request_payload( )
                                        exp = me->trace->get_request_payload( )->get_payload( ) ).
  endmethod.


  method get_response_payload.
    cl_abap_unit_assert=>assert_equals( act = me->cut->get_response_payload( )
                                        exp = me->trace->get_response_payload( )->get_payload( ) ).
  endmethod.


  method get_text.
    " Message 000 from the ZLSO_LOG message - &1&2&3&4
    " Check if provided text has been correctly splitted into parts.
    cl_abap_unit_assert=>assert_text_matches( pattern =  |^{ c_message-msgv1 }(.*){ c_message-text }$|
                                              text    =  me->cut->get_text( ) ).
  endmethod.


  method get_time.
    cl_abap_unit_assert=>assert_not_initial( me->cut->get_time( ) ).
  endmethod.


  method get_timestamp.
    cl_abap_unit_assert=>assert_not_initial( me->cut->get_timestamp( ) ).
  endmethod.


  method get_trace.
    cl_abap_unit_assert=>assert_equals( act = me->cut->get_trace( ) exp = me->trace ).
  endmethod.


  method get_type.
    cl_abap_unit_assert=>assert_equals( act = me->cut->get_type( ) exp = zif_lso_log_message=>c_type-info ).
  endmethod.


  method get_abap_stack.
    " ABAP Stack is not permitted!
*    data(class_absolute) = cast cl_abap_classdescr( cl_abap_classdescr=>describe_by_object_ref( me ) )->absolute_name.
*
*    " \CLASS-POOL=ZCL_LSO_LOG_MESSAGE\CLASS=LTC_LSO_LOG_MESSAGE
*    find regex '^\\CLASS-POOL=([^\\]+)\\CLASS=([^\\]+)$'
*      in class_absolute
*      ignoring case
*      submatches data(global_class) data(local_class).
*
*    cl_abap_unit_assert=>assert_char_cp( act = me->cut->zif_lso_log_message~get_abap_stack( )-abap_program
*                                         exp = |{ condense( global_class ) }*| ).
*
*    cl_abap_unit_assert=>assert_char_cp( act = me->cut->zif_lso_log_message~get_abap_stack( )-abap_include
*                                         exp = |{ condense( global_class ) }*| ).
*
*    cl_abap_unit_assert=>assert_true( boolc( me->cut->zif_lso_log_message~get_abap_stack( )-abap_source_line > 0 ) ).
*
*    cl_abap_unit_assert=>assert_equals( act = me->cut->zif_lso_log_message~get_abap_stack( )-abap_event
*                                        exp = |{ condense( local_class ) }=>SETUP| ).
  endmethod.


  method has_trace.
    cl_abap_unit_assert=>assert_true( me->cut->has_trace( ) ).
  endmethod.


  method is_error.
    cl_abap_unit_assert=>assert_false( me->cut->is_error( ) ).
  endmethod.


  method is_info.
    cl_abap_unit_assert=>assert_true( me->cut->is_info( ) ).
  endmethod.


  method is_success.
    cl_abap_unit_assert=>assert_false( me->cut->is_success( ) ).
  endmethod.


  method is_warning.
    cl_abap_unit_assert=>assert_false( me->cut->is_warning( ) ).
  endmethod.


  method is_abort.
    cl_abap_unit_assert=>assert_false( me->cut->is_abort( ) ).
  endmethod.


  method save_collection.
    data(message1) = new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-error
                                              msgid = c_message-msgid
                                              msgno = c_message-msgno
                                              msgv1 = conv #( c_message-msgv1 )
                                              msgv2 = 'Message1 with trace'
                                              trace = me->trace ).

    data(message2) = new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-success
                                              msgid = c_message-msgid
                                              msgno = c_message-msgno
                                              msgv1 = conv #( c_message-msgv1 )
                                              msgv2 = 'Message2, no trace' ).

    data(timestamp) = value zlso_log_message-timestamp( ).
    get time stamp field timestamp.

    data(message_builder) = new zcl_lso_log_message_builder( ).
    message_builder->set_timestamp( timestamp ).
    message_builder->set_msgty( zif_lso_log_message=>c_type-warning ).
    message_builder->set_msgid( c_message-msgid ).
    message_builder->set_msgno( c_message-msgno ).
    message_builder->set_msgv1( conv #( c_message-msgv1 ) ).
    message_builder->set_msgv2( 'Created with message builder' ).

    data(message3) = message_builder->build( ).

    data(messages) = value zlso_tt_log_messages( ( message1->get_object( ) )
                                                 ( message2->get_object( ) )
                                                 ( message3->get_object( ) ) ).

    " Add messages to the buffer for further tear down.
    me->add_messages_to_teardown( messages ).

    cl_abap_unit_assert=>assert_true( zcl_lso_log_message=>save_collection( log_id    = c_message-log_id
                                                                            log_seqnr = c_message-log_seqnr
                                                                            messages  = messages ) ).
  endmethod.


  method split_msgv.
    data lv_msgv type string.
    data lv_msgv1 type symsgv.
    data lv_msgv2 type symsgv.
    data lv_msgv3 type symsgv.
    data lv_msgv4 type symsgv.

    cut->split_msgv(
      exporting
        msgv  = c_message-text
      changing
        msgv1 = lv_msgv1
        msgv2 = lv_msgv2
        msgv3 = lv_msgv3
        msgv4 = lv_msgv4 ).

    cl_abap_unit_assert=>assert_not_initial( act = lv_msgv1 ).
    cl_abap_unit_assert=>assert_not_initial( act = lv_msgv2 ).
    cl_abap_unit_assert=>assert_initial( act = lv_msgv3 ).
    cl_abap_unit_assert=>assert_initial( act = lv_msgv4 ).

    data(exp_msgv1) = 'Lorem Ipsum is simply dummy text of the printing'.
    data(exp_msgv2) = 'and type setting industry.'.

    cl_abap_unit_assert=>assert_equals( act = lv_msgv1 exp = exp_msgv1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_msgv2 exp = exp_msgv2 ).
  endmethod.


  method get_stripped_date.
    me->cut->set_stripped_date( cl_abap_context_info=>get_system_date( ) ).

    cl_abap_unit_assert=>assert_equals( act = me->cut->zif_lso_log_message~get_stripped_date( ) exp = cl_abap_context_info=>get_system_date( ) ).
  endmethod.


  method get_object.
    cl_abap_unit_assert=>assert_equals( act = me->cut->get_object( )
                                        exp = value zlso_s_log_message( log_id    = me->cut->log_id
                                                                        log_seqnr = me->cut->log_seqnr
                                                                        timestamp = me->cut->timestamp
                                                                        instance  = me->cut ) ).
  endmethod.


  method is_stripped.
    me->cut->set_stripped_date( cl_abap_context_info=>get_system_date( ) ).

    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_message~is_stripped( ) ).
  endmethod.


  method set_trace.
    me->cut->set_trace( me->trace ).

    cl_abap_unit_assert=>assert_true( me->cut->has_trace( ) ).
  endmethod.


  method clone.
    data(clone) = cast zcl_lso_log_message( me->cut->zif_lso_clone~clone( ) ).

    cl_abap_unit_assert=>assert_equals( act = me->cut->get_symsg( )     exp = clone->get_symsg( ) ).
    cl_abap_unit_assert=>assert_equals( act = me->cut->get_timestamp( ) exp = clone->get_timestamp( ) ).
  endmethod.


  method lock_exception.
    data(msgv) = |ZLSO_LOG_MESSAGE table { me->cut->log_id }/{ me->cut->log_seqnr }|.

    try.
        raise exception type zcx_lso_log
          exporting
            textid   = zcx_lso_log=>lock_error
            mv_msgv1 = msgv.
      catch zcx_lso_log into data(cx).
        cl_abap_unit_assert=>assert_equals( act = cx->get_text( )
                                            exp = |{ msgv } is locked for writing!| ).
    endtry.
  endmethod.

endclass.
