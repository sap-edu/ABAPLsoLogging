class ltc_lso_log_unit definition deferred.
class zcl_lso_log definition local friends ltc_lso_log_unit.

class ltc_lso_log_unit definition for testing
  inheriting from zcl_lso_log_unit
  duration short
  risk level harmless.

  private section.
    constants begin of c_log.
    constants id type zlso_log-id value 'LTC_LOG'.
    constants ref1_id type zlso_log-ref_id value 'LTC_LOG_REF1'.
    constants ref2_id type zlso_log-ref_id value 'LTC_LOG_REF2'.
    constants end of c_log.

    constants begin of c_message.
    constants msgid type zlso_log_message-msgid value 'ZLSO_LOG'.
    constants msgno type zlso_log_message-msgno value '001'.  " &1 &2 &3 &4
    constants msgv1 type string value 'Var1' ##NO_TEXT.
    constants msgv2 type string value 'Var2' ##NO_TEXT.
    constants msgv3 type string value 'Var3' ##NO_TEXT.
    constants msgv4 type string value 'Var4' ##NO_TEXT.
    constants end of c_message.

    data cut type ref to zcl_lso_log.  "class under test
    data ids type zif_lso_log_factory=>tt_log_id.
    data factory type ref to zif_lso_log_factory.

    class-methods class_setup.
    class-methods class_teardown.

    methods setup.
    methods teardown.

    methods get_id for testing.
    methods get_seqnr for testing.
    methods get_structure for testing.
    methods get_object for testing.
    methods get_messages for testing.
    methods get_messages_iterator for testing.
    methods get_messages_of_type for testing.
    methods get_abstract_messages for testing.
    methods get_messages_clone for testing.
    methods get_messages_rfc for testing.
    methods save for testing raising zcx_lso_log.
    methods save_with_ref for testing raising zcx_lso_log.

    "! Save main log with messages and added reference log at the same time.
    methods save_with_messages_and_ref for testing raising zcx_lso_log.

    methods multiple_save for testing raising zcx_lso_log.
    methods multiple_save_with_ref for testing raising zcx_lso_log.
    methods save_with_cross_reference for testing.
    methods save_without_messages for testing raising zcx_lso_log.
    methods delete for testing raising zcx_lso_log.
    methods has_message for testing.
    methods has_exception for testing.
    methods has_messages for testing.
    methods has_message_type for testing.
    methods has_ref_logs for testing.
endclass.


class ltc_lso_log_unit implementation.

  method class_setup.
  endmethod.


  method class_teardown.
  endmethod.


  method setup.
    me->cut = new #( c_log-id ).

    me->factory = zcl_lso_log_factory=>instance( ).

*   Unit Tests - Log Ids range...
    me->ids = value #( ( sign = zif_lso_log=>c_sign-include option = zif_lso_log=>c_option-equal low = c_log-id ) ).
  endmethod.


  method teardown.
    " Just to be double sure data is deleted!
    me->delete_log( me->cut ).
  endmethod.


  method get_id.
    cl_abap_unit_assert=>assert_equals( act = me->cut->get_id( ) exp = c_log-id ).
  endmethod.


  method get_seqnr.
    " Is sequence number initial with no save?
    cl_abap_unit_assert=>assert_initial( me->cut->zif_lso_log~get_seqnr( ) ).
  endmethod.


  method get_structure.
    " Is basic structure as expected before save?
    cl_abap_unit_assert=>assert_equals( act = me->cut->zif_lso_log~get_structure( )
                                        exp = value zlso_log( id         = me->cut->zif_lso_log~get_id( )
                                                              seqnr      = me->cut->zif_lso_log~get_seqnr( )
                                                              timestamp  = me->cut->zif_lso_log~get_timestamp( )
                                                              log_date   = me->cut->zif_lso_log~get_date( )
                                                              log_time   = me->cut->zif_lso_log~get_time( ) ) ).
  endmethod.


  method get_object.
    " Object instance with key is returned as expected?
    cl_abap_unit_assert=>assert_equals( act = me->cut->zif_lso_log~get_object( )
                                        exp = value zif_lso_log=>ts_object( id       = me->cut->zif_lso_log~get_id( )
                                                                            seqnr    = me->cut->zif_lso_log~get_seqnr( )
                                                                            instance = me->cut ) ).
  endmethod.


  method get_messages.
    me->cut->add( new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-success
                                           msgid = c_message-msgid
                                           msgno = c_message-msgno
                                           msgv1 = c_message-msgv1
                                           msgv2 = c_message-msgv2
                                           msgv3 = c_message-msgv3
                                           msgv4 = c_message-msgv4 ) ).

    " Are there any messages returned?
    cl_abap_unit_assert=>assert_not_initial( me->cut->zif_lso_log~get_messages( ) ).
  endmethod.


  method get_messages_iterator.
    me->cut->add( new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-success
                                           msgid = c_message-msgid
                                           msgno = c_message-msgno
                                           msgv1 = c_message-msgv1
                                           msgv2 = c_message-msgv2
                                           msgv3 = c_message-msgv3
                                           msgv4 = c_message-msgv4 ) ).

    " Has iterator got an element?
    cl_abap_unit_assert=>assert_true( me->cut->get_messages( )->get_iterator( )->has_next( ) ).
  endmethod.


  method get_abstract_messages.
    me->cut->add( new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-info
                                           msgid = c_message-msgid
                                           msgno = c_message-msgno
                                           msgv1 = c_message-msgv1
                                           msgv2 = c_message-msgv2
                                           msgv3 = c_message-msgv3
                                           msgv4 = c_message-msgv4 ) ).

    " Are there any messages returned?
    cl_abap_unit_assert=>assert_not_initial( me->cut->zif_lso_log_abstract~get_messages( ) ).
  endmethod.


  method get_messages_clone.
    me->cut->add( new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-info
                                           msgid = c_message-msgid
                                           msgno = c_message-msgno
                                           msgv1 = c_message-msgv1
                                           msgv2 = c_message-msgv2
                                           msgv3 = c_message-msgv3
                                           msgv4 = c_message-msgv4 ) ).

    " Get collected messages.
    data(messages) = me->cut->zif_lso_log_abstract~get_messages( ).

    " Try to add one more message to the returned collection.
    cast cl_object_collection( messages )->add( new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-error
                                                                         msgid = c_message-msgid
                                                                         msgno = c_message-msgno ) ).

    " Was original collection of messages in the log object not changed with the new one?
    cl_abap_unit_assert=>assert_equals( act = me->cut->zif_lso_log_abstract~get_messages( )->size( ) exp = 1 ).

    " Was returned collection changed with the new one?
    cl_abap_unit_assert=>assert_equals( act = messages->size( ) exp = 2 ).
  endmethod.


  method get_messages_of_type.
    me->cut->add( new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-success
                                           msgid = c_message-msgid
                                           msgno = c_message-msgno
                                           msgv1 = c_message-msgv1
                                           msgv2 = c_message-msgv2
                                           msgv3 = c_message-msgv3
                                           msgv4 = c_message-msgv4 ) ).

    me->cut->add( new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-warning
                                           msgid = c_message-msgid
                                           msgno = c_message-msgno
                                           msgv1 = c_message-msgv1
                                           msgv2 = c_message-msgv2
                                           msgv3 = c_message-msgv3
                                           msgv4 = c_message-msgv4 ) ).

    data(messages) = me->cut->zif_lso_log_abstract~get_messages( zif_lso_log_message=>c_type-success ).

    " Is there one message returned?
    cl_abap_unit_assert=>assert_bound( messages ).
    cl_abap_unit_assert=>assert_false( messages->is_empty( ) ).
    cl_abap_unit_assert=>assert_equals( act = messages->size( ) exp = 1 ).

    data(exp_message) = cast zcl_lso_log_message( messages->get( 1 ) ).

    " Is there only success message returned?
    cl_abap_unit_assert=>assert_equals( act = exp_message->get_symsg( )
                                        exp = value symsg( msgty = zif_lso_log_message=>c_type-success
                                                           msgid = c_message-msgid
                                                           msgno = c_message-msgno
                                                           msgv1 = c_message-msgv1
                                                           msgv2 = c_message-msgv2
                                                           msgv3 = c_message-msgv3
                                                           msgv4 = c_message-msgv4 ) ).
  endmethod.


  method get_messages_rfc.
    me->cut->add( new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-success
                                           msgid = c_message-msgid
                                           msgno = c_message-msgno
                                           msgv1 = c_message-msgv1
                                           msgv2 = c_message-msgv2
                                           msgv3 = c_message-msgv3
                                           msgv4 = c_message-msgv4 ) ).

    me->cut->add( new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-warning
                                           msgid = c_message-msgid
                                           msgno = c_message-msgno
                                           msgv1 = c_message-msgv1
                                           msgv2 = c_message-msgv2
                                           msgv3 = c_message-msgv3
                                           msgv4 = c_message-msgv4 ) ).

    cl_abap_unit_assert=>assert_not_initial( me->cut->zif_lso_log_abstract~get_messages_rfc( ) ).
  endmethod.


  method save.
    data(request_headers) = me->get_request_headers( ).
    data(response_headers) = me->get_response_headers( ).

    " Prepare messages that will be added to the log object later.
    " Date/time of Message1 and Message2 are less than Message3.
    data(trace) = me->create_trace( request_payload  = '{ "request":"payload" }'
                                    response_payload = '{ "response":"payload" }' ).

    data(message1) = new zcl_lso_log_message( msgty = 'E'
                                              msgid = 'ZLSO_LOG'
                                              msgno = '000'
                                              msgv1 = 'Message1'
                                              trace = trace ).

    data(message2) = new zcl_lso_log_message( msgty = 'S'
                                              msgid = 'ZLSO_LOG'
                                              msgno = '000'
                                              msgv1 = 'Message2'
                                              trace = trace ).

    " Message3 will be added directly to the log object.
    data(message3) = new zcl_lso_log_message( msgty = 'W'
                                              msgid = 'ZLSO_LOG'
                                              msgno = '000'
                                              msgv1 = 'Message3' ).

    me->cut->add( message3 ).

    " Add Message1 and Message2 as a collection.
    data(messages) = new cl_object_collection( ).
    messages->add( message1 ).
    messages->add( message2 ).

    " Message1 and Message2 were created earlier than Message3 but added later than Message3.
    " Anyway they still need to be saved in proper order - Message1, Message2, Message3.
    me->cut->zif_lso_log_abstract~add_messages( messages ).

    " Long trace...
    data(long_trace) = me->create_trace( ).

    " Message4 with long payload.
    data(message4) = new zcl_lso_log_message( msgty = 'E'
                                              msgid = 'ZLSO_LOG'
                                              msgno = '000'
                                              msgv1 = 'Message4 with LONG trace'
                                              trace = long_trace ).

    " Add Message4 with long trace payload.
    me->cut->add( message4 ).

    " Try to save the log...
    me->cut->save( ).

    " Try to find newly saved logs...
    data(logs) = me->factory->find( me->ids ).

    " Has anything been found after save?
    cl_abap_unit_assert=>assert_not_initial( logs ).

    " Main log was found?
    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 1 ).

    data(act_log) = logs[ 1 ]-instance.

    " Assert log object.
    me->assert_log( log               = act_log
                    exp_id            = c_log-id
                    exp_has_messages  = abap_true
                    exp_number_of_msg = 4
                    exp_has_ref_logs  = abap_false ).

    data(act_message1) = cast zcl_lso_log_message( act_log->zif_lso_log_abstract~get_messages( )->get( 1 ) ).

    " Assert message object.
    me->assert_message( message       = act_message1
                        exp_log_id    = act_log->zif_lso_log~get_id( )
                        exp_log_seqnr = act_log->zif_lso_log~get_seqnr( )
                        exp_msgty     = 'E'
                        exp_msgid     = 'ZLSO_LOG'
                        exp_msgno     = '000'
                        exp_has_trace = abap_true ).

    data(act_message2) = cast zcl_lso_log_message( act_log->zif_lso_log_abstract~get_messages( )->get( 2 ) ).

    " Assert message object.
    me->assert_message( message       = act_message2
                        exp_log_id    = act_log->zif_lso_log~get_id( )
                        exp_log_seqnr = act_log->zif_lso_log~get_seqnr( )
                        exp_msgty     = 'S'
                        exp_msgid     = 'ZLSO_LOG'
                        exp_msgno     = '000'
                        exp_has_trace = abap_true ).

    " Is trace the same for both messages?
    cl_abap_unit_assert=>assert_equals( act = act_message1->get_trace( ) exp = act_message2->get_trace( ) ).

    data(act_trace) = act_message1->get_trace( ).

    " Assert trace object.
    me->assert_trace( trace                       = act_trace
                      exp_http_status             = c_trace_data-http_status
                      exp_request_method          = c_trace_data-request_method
                      exp_has_payload             = abap_true
                      exp_has_request_payload     = abap_true
                      exp_has_response_payload    = abap_true
                      exp_req_payload_cs_pattern  = '"request":"payload"'
                      exp_resp_payload_cs_pattern = '"response":"payload"'
                      exp_has_headers             = abap_true
                      exp_has_request_headers     = abap_true
                      exp_has_response_headers    = abap_true
                      exp_request_header          = request_headers[ 1 ]
                      exp_response_header         = response_headers[ 1 ] ).

    data(act_message3) = cast zcl_lso_log_message( act_log->zif_lso_log_abstract~get_messages( )->get( 3 ) ).

    " Assert message object.
    me->assert_message( message       = act_message3
                        exp_log_id    = act_log->zif_lso_log~get_id( )
                        exp_log_seqnr = act_log->zif_lso_log~get_seqnr( )
                        exp_msgty     = 'W'
                        exp_msgid     = 'ZLSO_LOG'
                        exp_msgno     = '000'
                        exp_has_trace = abap_false ).

    data(act_message4) = cast zcl_lso_log_message( act_log->zif_lso_log_abstract~get_messages( )->get( 4 ) ).

    " Assert message object.
    me->assert_message( message       = act_message4
                        exp_log_id    = act_log->zif_lso_log~get_id( )
                        exp_log_seqnr = act_log->zif_lso_log~get_seqnr( )
                        exp_msgty     = 'E'
                        exp_msgid     = 'ZLSO_LOG'
                        exp_msgno     = '000'
                        exp_has_trace = abap_true ).

    data(act_long_trace) = act_message4->get_trace( ).

    " Assert trace object.
    me->assert_trace( trace                       = act_long_trace
                      exp_http_status             = c_trace_data-http_status
                      exp_request_method          = c_trace_data-request_method
                      exp_has_payload             = abap_true
                      exp_has_request_payload     = abap_true
                      exp_has_response_payload    = abap_true
                      exp_has_headers             = abap_true
                      exp_has_request_headers     = abap_true
                      exp_has_response_headers    = abap_true
                      exp_req_payload_cs_pattern  = '"loginMethod": "SSO"'
                      exp_resp_payload_cs_pattern = '"status" : "ERROR"' ).
  endmethod.


  method multiple_save.
    me->cut->add( new zcl_lso_log_message( msgty = 'E'
                                           msgid = 'ZLSO_LOG'
                                           msgno = '000'
                                           msgv1 = 'Main log, 1st save' ) ).

    " 1st save...
    me->cut->save( ).

    me->cut->add( new zcl_lso_log_message( msgty = 'E'
                                           msgid = 'ZLSO_LOG'
                                           msgno = '000'
                                           msgv1 = 'Main log, 2nd save' ) ).

    " 2nd save...
    me->cut->save( ).

    " 3rd save, one more time with no changes.
    me->cut->save( ).

    " Try to find newly saved logs...
    data(logs) = me->factory->find( me->ids ).

    " Has anything been found after save?
    cl_abap_unit_assert=>assert_not_initial( logs ).

    " Two log saves were found?
    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 2 ).

    " Assert 1st log.
    me->assert_log( log               = logs[ 1 ]-instance
                    exp_id            = c_log-id
                    exp_seqnr         = '001'
                    exp_has_messages  = abap_true
                    exp_number_of_msg = 1
                    exp_has_ref_logs  = abap_false ).

    " Assert 2nd log.
    me->assert_log( log               = logs[ 2 ]-instance
                    exp_id            = c_log-id
                    exp_seqnr         = '002'
                    exp_has_messages  = abap_true
                    exp_number_of_msg = 1
                    exp_has_ref_logs  = abap_false ).
  endmethod.


  method multiple_save_with_ref.
    data(ref_log) = new zcl_lso_log( c_log-ref1_id ).
    ref_log->add( new zcl_lso_log_message( msgty = 'E'
                                           msgid = 'ZLSO_LOG'
                                           msgno = '000'
                                           msgv1 = 'Message 1 1st ref log' ) ).
    " 1st save - reference log
    ref_log->save( ).

    me->cut->add_ref_log( ref_log ).

    " 2nd save - relation between main and the reference log
    me->cut->save( ).

    me->cut->add( new zcl_lso_log_message( msgty = 'E'
                                           msgid = 'ZLSO_LOG'
                                           msgno = '000'
                                           msgv1 = 'Message 1 main log' ) ).

    me->cut->add( new zcl_lso_log_message( msgty = 'E'
                                           msgid = 'ZLSO_LOG'
                                           msgno = '000'
                                           msgv1 = 'Message 2 main log' ) ).

    " 3rd save - add messages to the main log
    me->cut->save( ).

    " 4th save - no changes, nothing to be saved
    me->cut->save( ).

    " Try to find newly saved logs...
    data(logs) = me->factory->find( me->ids ).

    " Has anything been found after save?
    cl_abap_unit_assert=>assert_not_initial( logs ).

    " Two log saves were found?
    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 2 ).

    " Main log (0001) with message from the reference log (save 1st and 2nd)
    me->assert_log( log                        = logs[ 1 ]-instance
                    exp_id                     = c_log-id
                    exp_seqnr                  = 1
                    exp_has_messages           = abap_true
                    exp_number_of_msg          = 0
                    exp_has_ref_logs           = abap_true
                    exp_number_of_msg_with_ref = 1
                    exp_number_of_ref_logs     = 1 ).

    " Main log (0002) with messages, no reference log (save 3rd)
    me->assert_log( log               = logs[ 2 ]-instance
                    exp_id            = c_log-id
                    exp_seqnr         = 2
                    exp_has_messages  = abap_true
                    exp_number_of_msg = 2
                    exp_has_ref_logs  = abap_false ).
  endmethod.


  method save_with_cross_reference.
    me->cut->add( new zcl_lso_log_message( msgty = 'E'
                                           msgid = 'ZLSO_LOG'
                                           msgno = '000'
                                           msgv1 = 'Main log' ) ).

    data(ref_log) = new zcl_lso_log( |{ c_log-id }_CROSS| ).

    ref_log->add( new zcl_lso_log_message( msgty = 'E'
                                           msgid = 'ZLSO_LOG'
                                           msgno = '000'
                                           msgv1 = 'Ref log' ) ).

    " Set cross reference case:
    " A --> B
    " B --> A
    me->cut->zif_lso_log~add_ref_log( ref_log ).
    ref_log->zif_lso_log~add_ref_log( me->cut ).

    try.
        me->cut->save( ).
      catch zcx_lso_log into data(cx_log).
        cl_abap_unit_assert=>assert_equals( act = cx_log->if_t100_message~t100key
                                            exp = cx_log->cross_reference_error ).
    endtry.

    " Exception is expected!
    cl_abap_unit_assert=>assert_bound( cx_log ).
  endmethod.


  method save_with_ref.
    data(trace) = me->create_trace( request_payload  = '{ "request":"payload_ref" }'
                                    response_payload = '{ "response":"payload_ref" }' ).

    data(message1) = new zcl_lso_log_message( msgty = 'E'
                                              msgid = 'ZLSO_LOG'
                                              msgno = '000'
                                              msgv1 = 'Message1 Ref, with trace'
                                              trace = trace ).

    data(message2) = new zcl_lso_log_message( msgty = 'S'
                                              msgid = 'ZLSO_LOG'
                                              msgno = '000'
                                              msgv1 = 'Message2 Ref, with trace'
                                              trace = trace ).

    data(message3) = new zcl_lso_log_message( msgty = 'W'
                                              msgid = 'ZLSO_LOG'
                                              msgno = '000'
                                              msgv1 = 'Message3 Ref, no trace' ).

    " Long trace...
    data(long_trace) = me->create_trace( ).

    " Message4 with long payload.
    data(message4) = new zcl_lso_log_message( msgty = 'E'
                                              msgid = 'ZLSO_LOG'
                                              msgno = '000'
                                              msgv1 = 'Message4 with LONG trace'
                                              trace = long_trace ).

    " Message5 with the same long payload.
    data(message5) = new zcl_lso_log_message( msgty = 'E'
                                              msgid = 'ZLSO_LOG'
                                              msgno = '000'
                                              msgv1 = 'Message5 with LONG trace'
                                              trace = long_trace ).

    data(ref_log) = new zcl_lso_log( c_log-ref1_id ).
    ref_log->add( message1 ).
    ref_log->add( message2 ).
    ref_log->add( message3 ).
    ref_log->add( message4 ).
    ref_log->add( message5 ).

    me->cut->zif_lso_log~add_ref_log( ref_log ).

    " Add message directly to the log to have both direct and referenced messages.
    data(message6) = new zcl_lso_log_message( msgty = 'E'
                                              msgid = 'ZLSO_LOG'
                                              msgno = '000'
                                              msgv1 = 'Message6 with LONG trace'
                                              msgv2 = 'Direct message'
                                              trace = long_trace ).

    me->cut->add( message6 ).

    " Try to save the log...
    me->cut->save( ).

    " Try to find newly saved logs...
    data(logs) = me->factory->find( me->ids ).

    " Has anything been found after save?
    cl_abap_unit_assert=>assert_not_initial( logs ).

    " Main log was found?
    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 1 ).

    data(act_log) = logs[ 1 ]-instance.

    " Assert basic log details.
    me->assert_log( log                        = act_log
                    exp_id                     = c_log-id
                    exp_has_messages           = abap_true
                    exp_number_of_msg          = 1
                    exp_number_of_msg_with_ref = 6
                    exp_has_ref_logs           = abap_true
                    exp_number_of_ref_logs     = 1 ).

    data(act_message6) = cast zcl_lso_log_message( act_log->zif_lso_log_abstract~get_messages( )->get( 1 ) ).

    " Assert message from the main log.
    me->assert_message( message       = act_message6
                        exp_log_id    = act_log->zif_lso_log~get_id( )
                        exp_log_seqnr = act_log->zif_lso_log~get_seqnr( )
                        exp_msgty     = 'E'
                        exp_msgid     = 'ZLSO_LOG'
                        exp_msgno     = '000'
                        exp_has_trace = abap_true ).

    data(act_long_trace) = act_message6->get_trace( ).

    " Assert trace object from the log message.
    me->assert_trace( trace                      = act_long_trace
                      exp_http_status            = c_trace_data-http_status
                      exp_request_method         = c_trace_data-request_method
                      exp_has_payload            = abap_true
                      exp_has_request_payload    = abap_true
                      exp_has_response_payload   = abap_true
                      exp_has_headers            = abap_true
                      exp_has_request_headers    = abap_true
                      exp_has_response_headers   = abap_true
                      exp_req_payload_cs_pattern = '"loginMethod": "SSO"' ).

    data(act_ref_log) = cast zcl_lso_log( act_log->zif_lso_log~get_ref_logs( )->get( 1 ) ).

    " Assert reference log.
    me->assert_log( log               = act_ref_log
                    exp_id            = c_log-ref1_id
                    exp_has_messages  = abap_true
                    exp_number_of_msg = 5
                    exp_has_ref_logs  = abap_false ).

    data(act_message1) = cast zcl_lso_log_message( act_ref_log->zif_lso_log_abstract~get_messages( )->get( 1 ) ).

    " Assert message object from reference log.
    me->assert_message( message       = act_message1
                        exp_log_id    = act_ref_log->zif_lso_log~get_id( )
                        exp_log_seqnr = act_ref_log->zif_lso_log~get_seqnr( )
                        exp_msgty     = 'E'
                        exp_msgid     = 'ZLSO_LOG'
                        exp_msgno     = '000'
                        exp_has_trace = abap_true ).

    data(act_message2) = cast zcl_lso_log_message( act_ref_log->zif_lso_log_abstract~get_messages( )->get( 2 ) ).

    " Assert message object from reference log.
    me->assert_message( message       = act_message2
                        exp_log_id    = act_ref_log->zif_lso_log~get_id( )
                        exp_log_seqnr = act_ref_log->zif_lso_log~get_seqnr( )
                        exp_msgty     = 'S'
                        exp_msgid     = 'ZLSO_LOG'
                        exp_msgno     = '000'
                        exp_has_trace = abap_true ).

    " Is trace object the same?
    cl_abap_unit_assert=>assert_equals( act = act_message1->get_trace( ) exp = act_message2->get_trace( ) ).

    data(act_trace) = act_message1->get_trace( ).

    " Assert trace object from the reference log message.
    me->assert_trace( trace                      = act_trace
                      exp_http_status            = c_trace_data-http_status
                      exp_request_method         = c_trace_data-request_method
                      exp_has_payload            = abap_true
                      exp_has_request_payload    = abap_true
                      exp_has_response_payload   = abap_true
                      exp_has_headers            = abap_true
                      exp_has_request_headers    = abap_true
                      exp_has_response_headers   = abap_true
                      exp_req_payload_cs_pattern = '"request":"payload_ref"' ).

    data(act_message3) = cast zcl_lso_log_message( act_ref_log->zif_lso_log_abstract~get_messages( )->get( 3 ) ).

    " Assert message object from reference log.
    me->assert_message( message       = act_message3
                        exp_log_id    = act_ref_log->zif_lso_log~get_id( )
                        exp_log_seqnr = act_ref_log->zif_lso_log~get_seqnr( )
                        exp_msgty     = 'W'
                        exp_msgid     = 'ZLSO_LOG'
                        exp_msgno     = '000'
                        exp_has_trace = abap_false ).

    data(act_message4) = cast zcl_lso_log_message( act_ref_log->zif_lso_log_abstract~get_messages( )->get( 4 ) ).

    " Assert message object from reference log.
    me->assert_message( message       = act_message4
                        exp_log_id    = act_ref_log->zif_lso_log~get_id( )
                        exp_log_seqnr = act_ref_log->zif_lso_log~get_seqnr( )
                        exp_msgty     = 'E'
                        exp_msgid     = 'ZLSO_LOG'
                        exp_msgno     = '000'
                        exp_has_trace = abap_true ).

    data(act_message5) = cast zcl_lso_log_message( act_ref_log->zif_lso_log_abstract~get_messages( )->get( 5 ) ).

    " Assert message object from reference log.
    me->assert_message( message       = act_message5
                        exp_log_id    = act_ref_log->zif_lso_log~get_id( )
                        exp_log_seqnr = act_ref_log->zif_lso_log~get_seqnr( )
                        exp_msgty     = 'E'
                        exp_msgid     = 'ZLSO_LOG'
                        exp_msgno     = '000'
                        exp_has_trace = abap_true ).

    " Is trace object the same?
    cl_abap_unit_assert=>assert_equals( act = act_message4->get_trace( ) exp = act_message5->get_trace( ) ).

    act_trace = act_message4->get_trace( ).

    " Assert trace object from reference log message.
    me->assert_trace( trace                      = act_trace
                      exp_http_status            = c_trace_data-http_status
                      exp_request_method         = c_trace_data-request_method
                      exp_has_payload            = abap_true
                      exp_has_request_payload    = abap_true
                      exp_has_response_payload   = abap_true
                      exp_has_headers            = abap_true
                      exp_has_request_headers    = abap_true
                      exp_has_response_headers   = abap_true
                      exp_req_payload_cs_pattern = '"loginMethod": "SSO"' ).
  endmethod.


  method save_with_messages_and_ref.
    " Main log (messages)
    "  -> Ref1 (messages)
    "  -> Ref2 (messages)

    " Main log with messages...
    me->cut->zif_lso_log_abstract~add_message( new zcl_lso_log_message( msgty = 'E'
                                                                        msgid = 'ZLSO_LOG'
                                                                        msgno = '000'
                                                                        msgv1 = 'Message1 main log' ) ).

    me->cut->zif_lso_log_abstract~add_message( new zcl_lso_log_message( msgty = 'E'
                                                                        msgid = 'ZLSO_LOG'
                                                                        msgno = '000'
                                                                        msgv1 = 'Message2 main log' ) ).

    " 1st reference log with messages...
    data(ref1) = new zcl_lso_log( c_log-ref1_id ).

    ref1->zif_lso_log_abstract~add_message( new zcl_lso_log_message( msgty = 'E'
                                                                     msgid = 'ZLSO_LOG'
                                                                     msgno = '000'
                                                                     msgv1 = 'Message1 ref1 log' ) ).

    ref1->zif_lso_log_abstract~add_message( new zcl_lso_log_message( msgty = 'E'
                                                                     msgid = 'ZLSO_LOG'
                                                                     msgno = '000'
                                                                     msgv1 = 'Message2 ref1 log' ) ).

    " Add 1st reference log to the main one.
    me->cut->zif_lso_log~add_ref_log( ref1 ).

    " 2nd reference log with messages...
    data(ref2) = new zcl_lso_log( c_log-ref2_id ).

    ref2->zif_lso_log_abstract~add_message( new zcl_lso_log_message( msgty = 'E'
                                                                     msgid = 'ZLSO_LOG'
                                                                     msgno = '000'
                                                                     msgv1 = 'Message1 ref2 log' ) ).

    ref2->zif_lso_log_abstract~add_message( new zcl_lso_log_message( msgty = 'E'
                                                                     msgid = 'ZLSO_LOG'
                                                                     msgno = '000'
                                                                     msgv1 = 'Message2 ref2 log' ) ).

    " Add 2nd reference log to the main one.
    me->cut->zif_lso_log~add_ref_log( ref2 ).

    " Try to save the log...
    me->cut->save( ).

    " Try to find newly saved logs...
    data(logs) = me->factory->find( me->ids ).

    " Has anything been found after save?
    cl_abap_unit_assert=>assert_not_initial( logs ).

    " Main log was found?
    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 1 ).

    data(act_log) = logs[ 1 ]-instance.

    " Assert basic log details.
    me->assert_log( log                        = act_log
                    exp_id                     = c_log-id
                    exp_has_messages           = abap_true
                    exp_number_of_msg          = 2
                    exp_number_of_msg_with_ref = 6
                    exp_has_ref_logs           = abap_true
                    exp_number_of_ref_logs     = 2 ).


  endmethod.


  method save_without_messages.
    me->cut->save( ).

    " Try to find logs...
    data(logs) = me->factory->find( me->ids ).

    " No logs found?
    cl_abap_unit_assert=>assert_initial( logs ).
  endmethod.


  method has_exception.
    " Not found - &1 &2 &3 &4
    data(exception) = new zcx_lso_log( textid   = zcx_lso_log=>not_found
                                       mv_msgv1 = conv #( c_message-msgv1 )
                                       mv_msgv2 = conv #( c_message-msgv2 )
                                       mv_msgv3 = conv #( c_message-msgv3 )
                                       mv_msgv4 = conv #( c_message-msgv4 ) ).

    me->cut->zif_lso_log_abstract~exception( exception ).

    " Is there an expected exception in the log?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log~has_exception( zcx_lso_log=>not_found ) ).
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

    me->cut->add( message ).

    " Is there an expected message in the log?
    cl_abap_unit_assert=>assert_true( me->cut->has_message( message ) ).
  endmethod.


  method has_messages.
    " Initially there is no single message added to the log?
    cl_abap_unit_assert=>assert_false( me->cut->zif_lso_log~has_messages( ) ).

    " Add message to the log.
    me->cut->add( new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-warning
                                           msgid = c_message-msgid
                                           msgno = c_message-msgno
                                           msgv1 = c_message-msgv1
                                           msgv2 = c_message-msgv2
                                           msgv3 = c_message-msgv3
                                           msgv4 = c_message-msgv4 ) ).

    " There are messages in the log?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log~has_messages( ) ).
  endmethod.


  method has_message_type.
    " Add warning message to the log.
    me->cut->add( new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-warning
                                           msgid = c_message-msgid
                                           msgno = c_message-msgno
                                           msgv1 = c_message-msgv1
                                           msgv2 = c_message-msgv2
                                           msgv3 = c_message-msgv3
                                           msgv4 = c_message-msgv4 ) ).

    " There are warning messages in the log?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log~has_message_type( zif_lso_log_message=>c_type-warning ) ).
  endmethod.


  method has_ref_logs.
    me->cut->zif_lso_log~add_ref_log( new zcl_lso_log( c_log-ref1_id ) ).

    " There are reference logs in the log?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log~has_ref_logs( ) ).
  endmethod.


  method delete.
    me->cut->add( new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-error
                                           msgid = c_message-msgid
                                           msgno = c_message-msgno
                                           trace = me->create_trace( ) ) ).
    me->cut->save( ).

    " Deleted successfully?
    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log~delete( ) ).

    " Log cannot be found any more?
    data(logs) = me->factory->find( me->ids ).

    " Nothing has been found after delete?
    cl_abap_unit_assert=>assert_initial( logs ).
  endmethod.

endclass.
