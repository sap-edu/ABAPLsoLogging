class ltc_log_factory definition final for testing
  inheriting from zcu_lso_log
  duration long
  risk level harmless.

  private section.
    constants begin of c_log.
    constants id type zlso_log-id value 'LTC_LOG_FACTORY'.
    constants ref_id_1st type zlso_log-ref_id value 'LTC_LOG_FACTORY_REF_1'.
    constants ref_id_2nd type zlso_log-ref_id value 'LTC_LOG_FACTORY_REF_2'.
    constants ref_id_3rd type zlso_log-ref_id value 'LTC_LOG_FACTORY_REF_3'.
    constants ref_id_4th type zlso_log-ref_id value 'LTC_LOG_FACTORY_REF_4'.
    constants end of c_log.

    constants begin of c_msg.
    constants msgid type zlso_log_message-msgid value 'ZLSO_LOG'.
    constants error type zlso_log_message-msgty value 'E'.
    constants warning type zlso_log_message-msgty value 'W'.
    constants success type zlso_log_message-msgty value 'S'.
    constants end of c_msg.

    constants begin of c_trace.
    constants id type zlso_log_trace-id value 'LTC_LOG_FACTORY_TRC'.
    constants id_ref_1st type zlso_log_trace-id value 'LTC_LOG_FACTORY_TRC1'.
    constants id_ref_2nd type zlso_log_trace-id value 'LTC_LOG_FACTORY_TRC2'.
    constants end of c_trace.

    data cut type ref to zif_lso_log_factory.
    data ids type zif_lso_log_factory=>tt_log_id.
    data dates type zif_lso_log_factory=>tt_log_date.
    data times type zif_lso_log_factory=>tt_log_time.
    data changed_bys type zif_lso_log_factory=>tt_changed_by.
    data tcodes type zif_lso_log_factory=>tt_tcode.
    data programs type zif_lso_log_factory=>tt_program.

    class-methods class_setup.
    class-methods class_teardown.

    methods setup.
    methods teardown.

    "! Get log by ID
    methods get for testing.

    "! Create log objects based from the log handler.
    methods create_from_handler for testing.

    "! Create log object by its structure.
    methods create_by_structure for testing.

    "! Does log exist?
    methods exists for testing raising zcx_lso_log.

    "! Find log by id
    methods find_by_id for testing raising zcx_lso_log.

    "! Find log by IDs taking into account parent-child hierarchy.
    methods find_by_id_hierarchy for testing raising zcx_lso_log.

    "! Find log by IDs WITHOUT reference logs.
    methods find_by_id_without_ref_logs for testing raising zcx_lso_log.

    "! Find by id where messages are first stored in a reference log, relations is set and message is added to the main one.
    methods find_by_id_ref_main for testing raising zcx_lso_log.

    "! Find log contains message text
    methods find_by_message_text for testing raising zcx_lso_log.

    "! Find 1st level reference log contains message text
    methods find_by_1st_ref_message_text for testing raising zcx_lso_log.

    "! Find 2nd level reference logs contain message text
    methods find_by_2nd_ref_message_text for testing raising zcx_lso_log.

    "! Multiple log levels, find with 1st reference id
    methods find_by_1st_ref_id for testing raising zcx_lso_log.

    "! Find by request payload
    methods find_by_request_payload for testing raising zcx_lso_log.

    "! Find by response payload
    methods find_by_response_payload for testing raising zcx_lso_log.

    "! Find by request, response payload
    methods find_by_req_resp_payload for testing raising zcx_lso_log.

    "! Find by message text, request and response payload
    methods find_by_msg_req_resp_payload for testing raising zcx_lso_log.

    "! Find last messages from log without a reference one
    methods find_last for testing raising zcx_lso_log.

    "! Find log stripped of some messages in the past
    methods find_stripped for testing raising zcx_lso_log.

    "! Find last messages from log with a reference one
    methods find_last_with_ref for testing raising zcx_lso_log.

    "! Delete logs|
    methods delete for testing raising zcx_lso_log.

    "! Delete logs collection
    methods delete_collection for testing raising zcx_lso_log.
endclass.


class ltc_log_factory implementation.

  method class_setup.
  endmethod.


  method class_teardown.
*   Rollback is executed automatically...
  endmethod.


  method setup.
    " Class Under Test
    me->cut = zcl_lso_log_factory=>instance( ).

*   Unit Tests - Log IDs range...
    me->ids = value #( ( sign = zif_lso_log=>c_sign-include option = zif_lso_log=>c_option-equal low = c_log-id ) ).

*   Unit Tests - Dates range...
    me->dates = value #( ( sign   = zif_lso_log=>c_sign-include
                           option = zif_lso_log=>c_option-between
                           low    = cl_abap_context_info=>get_system_date( ) - 1
                           high   = cl_abap_context_info=>get_system_date( ) + 1 ) ).

*   Unit Tests - Time range...
    me->times = value #( ( sign   = zif_lso_log=>c_sign-include
                           option = zif_lso_log=>c_option-between
                           low    = cl_abap_context_info=>get_system_time( ) - 1
                           high   = cl_abap_context_info=>get_system_time( ) + 1 ) ).

*   Unit Tests - Changed By range...
    me->changed_bys = value #( ( sign = zif_lso_log=>c_sign-include option = zif_lso_log=>c_option-equal low = sy-uname ) ).

*   Unit Tests - Transaction code range...
    me->tcodes = value #( ( sign = zif_lso_log=>c_sign-include option = zif_lso_log=>c_option-equal low = 'sy-tcode' ) ).

*   Unit Tests - Program range...
    me->programs = value #( ( sign = zif_lso_log=>c_sign-include option = zif_lso_log=>c_option-equal low = 'sy-cprog' ) ).
  endmethod.


  method teardown.
    " Just to be double sure all test data is deleted!
    me->delete_log( new zcl_lso_log( c_log-id ) ).
    me->delete_log( new zcl_lso_log( c_log-ref_id_1st ) ).
    me->delete_log( new zcl_lso_log( c_log-ref_id_2nd ) ).
    me->delete_log( new zcl_lso_log( c_log-ref_id_3rd ) ).
  endmethod.


  method create_from_handler.
    data(log_handler) = new zcl_lso_log_handler( ).

    log_handler->zif_lso_log_abstract~add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG'
                                                                                    )->set_msgty( 'E'
                                                                                    )->set_msgv1( 'Message 1'
                                                                                    )->build( ) ).

    log_handler->zif_lso_log_abstract~add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG'
                                                                                    )->set_msgty( 'E'
                                                                                    )->set_msgv1( 'Message 2'
                                                                                    )->build( ) ).

    data(logs) = me->cut->create_from_handler( log_handler ).

    " Test log object...
    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( logs[ 1 ]-instance->get_messages( ) ) exp = 2 ).
  endmethod.


  method create_by_structure.
    data(timestamp) = value timestampl( ).
    get time stamp field timestamp.

    try.
        zcl_lso_log_utils=>utc_tstmp_2_datetime(
          exporting
            iv_tstmp                   = timestamp
          importing
            ev_date                    = data(date)
            ev_time                    = data(time) ).
      catch cx_parameter_invalid_type
            cx_parameter_invalid_range.
        date = cl_abap_context_info=>get_system_date( ).
        time = cl_abap_context_info=>get_system_time( ).
    endtry.

    data(exp_structure) = value zlso_log( id         = c_log-id
                                          seqnr      = '000001'
                                          timestamp  = timestamp
                                          log_date   = date
                                          log_time   = time
                                          log_mode   = zif_lso_log=>c_mode-dialog
                                          changed_by = sy-uname ).

    data(act_log) = me->cut->create_by_structure( exp_structure ).

    cl_abap_unit_assert=>assert_equals( act = act_log->zif_lso_log~get_structure( ) exp = exp_structure ).
  endmethod.


  method exists.
    " Main log
    data(log) = new zcl_lso_log_builder(
        )->set_log_id( c_log-id
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 1 error no trace' )->build( )
        )->build( ).

    log->save( ).

    " Add log to the buffer for further tear down.
    me->add_log_to_teardown( log ).

    cl_abap_unit_assert=>assert_true( me->cut->exists( c_log-id ) ).
  endmethod.


  method find_by_id.
    " Main log
    data(exp_log) = new zcl_lso_log_builder(
        )->set_log_id( c_log-id
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 1 error no trace' )->build( )
        )->build( ).

    exp_log->save( ).

    " Add log to the buffer for further tear down.
    me->add_log_to_teardown( exp_log ).

    " Find with log id.
    data(logs) = me->cut->find( me->ids ).

    " Test if anything has been found?
    cl_abap_unit_assert=>assert_not_initial( logs ).

    " Only one main log has been returned?
    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 1 ).

    data(act_log) = logs[ 1 ]-instance.

    " Test - main log with messages, no reference log.
    me->assert_log( log               = act_log
                    exp_id            = exp_log->zif_lso_log~get_id( )
                    exp_has_messages  = exp_log->zif_lso_log~has_messages( )
                    exp_number_of_msg = lines( exp_log->zif_lso_log~get_messages( ) )
                    exp_has_ref_logs  = exp_log->zif_lso_log~has_ref_logs( ) ).

    data(messages) = act_log->get_messages( ).

    data(act_message) = ref #( messages[ 1 ] ).

##TODO " Is it possible on the Steampunk?
*    data(class_absolute) = cast cl_abap_classdescr( cl_abap_classdescr=>describe_by_object_ref( me ) )->absolute_name.
*
*    " \CLASS-POOL=ZCL_LSO_LOG_FACTORY\CLASS=LTC_LOG_FACTORY
*    find regex '^\\CLASS-POOL=([^\\]+)\\CLASS=([^\\]+)$'
*      in class_absolute
*      ignoring case
*      submatches data(global_class) data(local_class).

    " Test found message.
    me->assert_message( message          = act_message->instance
                        exp_log_id       = act_log->get_id( )
                        exp_log_seqnr    = act_log->get_seqnr( )
                        exp_msgty        = act_message->instance->get_symsg( )-msgty
                        exp_msgid        = act_message->instance->get_symsg( )-msgid
                        exp_msgno        = act_message->instance->get_symsg( )-msgno
                        exp_has_trace    = act_message->instance->has_trace( )
##TODO???
*                        exp_abap_program = |{ condense( global_class ) }*|
*                        exp_abap_include = |{ condense( global_class ) }*|
*                        exp_abap_event   = |{ condense( local_class ) }=>FIND_BY_ID|
                        ).
  endmethod.


  method find_by_id_ref_main.
    " 1st level (reference) - with messages (1st save)
    " Main log - add 1st reference log (already saved one) (2nd save)
    " Main log - add message (3rd save)

    " 1st level reference log
    data(exp_ref_log) = new zcl_lso_log_builder(
        )->set_log_id( c_log-ref_id_1st
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message 1 1st ref log' )->build( )
        )->build( ).

    exp_ref_log->save( ).

    " Main log
    data(exp_log) = new zcl_lso_log_builder(
        )->set_log_id( c_log-id
        )->add_ref_log( exp_ref_log
        )->build( ).

    exp_log->save( ).

    " Add message to the main log.
    exp_log->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message 1 main log' )->build( ) ).

    exp_log->save( ).

    " Add log to the buffer for further tear down.
    me->add_log_to_teardown( exp_log ).

    " Find with log id.
    data(logs) = me->cut->find( me->ids ).

    " Test if anything has been found?
    cl_abap_unit_assert=>assert_not_initial( logs ).

    " Only one main log has been returned?
    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 2 ).

    data(act_log) = logs[ key object_key index 1 ]-instance.

    " Test - main log with messages from reference log (save 1st and 2nd)
    me->assert_log( log                        = act_log
                    exp_id                     = exp_log->zif_lso_log~get_id( )
                    exp_seqnr                  = 1
                    exp_has_messages           = abap_true
                    exp_number_of_msg          = 0
                    exp_has_ref_logs           = abap_true
                    exp_number_of_msg_with_ref = 1
                    exp_number_of_ref_logs     = 1 ).

    act_log = logs[ key object_key index 2 ]-instance.

    " Test - main log with messages from 2nd save, no reference log.
    me->assert_log( log               = act_log
                    exp_id            = exp_log->zif_lso_log~get_id( )
                    exp_seqnr         = 2
                    exp_has_messages  = abap_true
                    exp_number_of_msg = 1
                    exp_has_ref_logs  = abap_false ).
  endmethod.


  method find_by_message_text.
    " Main log
    data(exp_log) = new zcl_lso_log_builder(
        )->set_log_id( c_log-id
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 1 error no trace' )->build( )
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'W' )->set_msgv1( 'Message 2 warning no trace' )->build( )
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'S' )->set_msgv1( 'Message 3 success no trace' )->build( )
        )->build( ).

    exp_log->save( ).

    " Add log to the buffer for further tear down.
    me->add_log_to_teardown( exp_log ).

    " Find with message text pattern.
    data(logs) = me->cut->find( log_ids       = me->ids
                                log_dates     = me->dates
                                message_texts = value #( ( sign   = zif_lso_log=>c_sign-include
                                                           option = zif_lso_log=>c_option-contains_pattern
                                                           low    = 'Message' ) ) ).

    " Test if anything has been found?
    cl_abap_unit_assert=>assert_not_initial( logs ).

    " Only one main log has been returned?
    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 1 ).

    data(act_log) = ref #( logs[ 1 ] ).

    " Test - main log with messages, no reference log
    me->assert_log( log               = cast #( act_log->instance )
                    exp_id            = exp_log->get_id( )
                    exp_has_messages  = exp_log->zif_lso_log~has_messages( )
                    exp_number_of_msg = lines( exp_log->zif_lso_log~get_messages( ) )
                    exp_has_ref_logs  = exp_log->zif_lso_log~has_ref_logs( ) ).
  endmethod.


  method find_by_1st_ref_message_text.
    " Main log - message without trace
    "   -> 1st level (reference) log - message with trace
    data(log) = new zcl_lso_log_builder(
        " Main log
        )->set_log_id( c_log-id
        )->add_message(
            new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message 1 error main log'
                                             )->build( )
        )->add_ref_log(
            " 1st level reference log
            new zcl_lso_log_builder(
                )->set_log_id( c_log-ref_id_1st
                )->add_message(
                    new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message 2 error from ref log'
                                                     )->set_trace( me->create_trace( c_trace-id_ref_1st )
                                                     )->build( )
                )->build( )
        )->build( ).

    log->save( ).

    " Add log to the buffer for further tear down.
    me->add_log_to_teardown( log ).

    " Find with message text pattern.
    data(logs) = me->cut->find( log_ids       = me->ids
                                log_dates     = me->dates
                                message_texts = value #( ( sign   = zif_lso_log=>c_sign-include
                                                           option = zif_lso_log=>c_option-contains_pattern
                                                           low    = 'error from ref log' ) ) ).

    " Test if anything has been found?
    cl_abap_unit_assert=>assert_not_initial( logs ).

    " Only main log has been returned?
    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 1 ).

    data(log_object) = ref #( logs[ 1 ] ).

    " Test main log without messages, reference log with message.
    me->assert_log( log                        = log_object->instance
                    exp_id                     = c_log-id
                    exp_has_messages           = abap_true
                    exp_number_of_msg          = 0
                    exp_number_of_msg_with_ref = 1
                    exp_has_ref_logs           = abap_true
                    exp_number_of_ref_logs     = 1 ).


    " Main log contains 1st level reference log?
    data(ref_logs) = log_object->instance->get_ref_logs( ).
    data(ref_log_1st) = ref #( ref_logs[ 1 ] ).

    " Test if 1st level reference log exists and does not contain further reference one?
    me->assert_log( log               = ref_log_1st->instance
                    exp_id            = c_log-ref_id_1st
                    exp_has_messages  = abap_true
                    exp_number_of_msg = 1
                    exp_has_ref_logs  = abap_false ).
  endmethod.


  method find_by_2nd_ref_message_text.
    " Main log - no messages
    "   -> 1st level (reference) log - no messages
    "     -> 2nd level (reference) log - message with trace
    "     -> 2nd level (reference) log - message without trace
    data(log) = new zcl_lso_log_builder(
        " Main log
        )->set_log_id( c_log-id
        )->add_ref_log(
            " 1st level reference log
            new zcl_lso_log_builder(
                )->set_log_id( c_log-ref_id_1st
                )->add_ref_log(
                    " 2nd level 1st reference log
                    new zcl_lso_log_builder(
                        )->set_log_id( c_log-ref_id_2nd
                        )->add_message(
                            new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message 1 error from 2nd level 1st ref log'
                                                             )->set_trace( me->create_trace( c_trace-id_ref_2nd )
                                                             )->build( )
                        )->build( )
                )->add_ref_log(
                    " 2nd level 2nd reference log
                    new zcl_lso_log_builder(
                        )->set_log_id( c_log-ref_id_2nd
                        )->add_message(
                            new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message 1 error from 2nd level 2nd ref log'
                                                             )->build( )
                        )->build( )
                )->build( )
        )->build( ).

    log->save( ).

    " Add log to the buffer for further tear down.
    me->add_log_to_teardown( log ).

##TODO " T100 is not permitted :/
*    " Try to find logs...
*    data(logs) = me->cut->find( log_ids       = me->ids
*                                log_dates     = me->dates
*                                message_texts = value #( ( sign   = zif_lso_log=>c_sign-include
*                                                           option = zif_lso_log=>c_option-contains_pattern
*                                                           low    = 'error from 2nd level' ) ) ).
*
*    " Test if anything has been found?
*    cl_abap_unit_assert=>assert_not_initial( logs ).
*
*    " Only main log has been returned?
*    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 1 ).
*
*    " Main log with reference one
*    data(log_object) = ref #( logs[ 1 ] ).
*
*    " Test main log...
*    me->assert_log( log                        = log_object->instance
*                    exp_id                     = c_log-id
*                    exp_has_messages           = abap_true
*                    exp_number_of_msg          = 0
*                    exp_number_of_msg_with_ref = 2
*                    exp_has_ref_logs           = abap_true
*                    exp_number_of_ref_logs     = 1 ).
*
*    data(ref_logs) = log_object->instance->get_ref_logs( ).
*
*    " 1st reference log
*    data(ref_log_1st) = ref #( ref_logs[ 1 ] ).
*
*    " Test 1st reference log...
*    me->assert_log( log                        = ref_log_1st->instance
*                    exp_id                     = c_log-ref_id_1st
*                    exp_has_messages           = abap_true
*                    exp_number_of_msg          = 0
*                    exp_number_of_msg_with_ref = 2
*                    exp_has_ref_logs           = abap_true
*                    exp_number_of_ref_logs     = 2 ).
*
*    ref_logs = ref_log_1st->instance->get_ref_logs( ).
*
*    " Test 2nd level reference log 1...
*    me->assert_log( log               = ref_logs[ 1 ]-instance
*                    exp_id            = c_log-ref_id_2nd
*                    exp_has_messages  = abap_true
*                    exp_number_of_msg = 1
*                    exp_has_ref_logs  = abap_false ).
*
*    " Test 2nd level reference log 2...
*    me->assert_log( log               = ref_logs[ 2 ]-instance
*                    exp_id            = c_log-ref_id_2nd
*                    exp_has_messages  = abap_true
*                    exp_number_of_msg = 1
*                    exp_has_ref_logs  = abap_false ).
  endmethod.


  method find_by_1st_ref_id.
    " Main log - with message
    "   -> 1st level (reference) - with messages
    "     -> 2nd level (reference) - with messages
    "       -> 3rd level (reference) - with messages
    "     -> 2nd level (reference) - with message
    "   -> 1st level (reference) - with message
    data(log) = new zcl_lso_log_builder(
        " Main log
        )->set_log_id( c_log-id
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message 1 main log' )->build( )
        )->add_ref_log(
            " 1st level reference log
            new zcl_lso_log_builder(
                )->set_log_id( c_log-ref_id_1st
                )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message 1 1st ref log' )->build( )
                )->add_ref_log(
                    " 2nd level reference log
                    new zcl_lso_log_builder(
                        )->set_log_id( c_log-ref_id_2nd
                        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message 1 2nd ref log' )->build( )
                        )->add_ref_log(
                            " 3rd level reference log
                            new zcl_lso_log_builder(
                                )->set_log_id( c_log-ref_id_3rd
                                )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message 1 3rd ref log' )->build( )
                                )->build( )
                        )->build( )
                )->build( )
        )->build( ).

    log->save( ).

    " Add more messages to the 1st level reference log
    log = new zcl_lso_log_builder( )->set_log_id( c_log-ref_id_1st
                                   )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'S' )->set_msgv1( 'Message 2 1st ref log' )->build( )
                                   )->build( ).

    log->save( ).

    " Add more messages to the 2nd level reference log
    log = new zcl_lso_log_builder( )->set_log_id( c_log-ref_id_2nd
                                   )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'S' )->set_msgv1( 'Message 2 2nd ref log' )->build( )
                                   )->build( ).

    log->save( ).

    " Add more messages to the 3rd level reference log
    log = new zcl_lso_log_builder( )->set_log_id( c_log-ref_id_3rd
                                   )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'S' )->set_msgv1( 'Message 2 3rd ref log' )->build( )
                                   )->build( ).

    log->save( ).

    " Add log to the buffer for further tear down.
    me->add_log_to_teardown( log ).

    " Try to find logs starting with id of the 1st level reference log...
    data(logs) = me->cut->find( value #( ( sign   = zif_lso_log=>c_sign-include
                                           option = zif_lso_log=>c_option-equal
                                           low    = c_log-ref_id_1st ) ) ).

    " Expected result starting from the 1st level log:
    "   - 1st 0001 - Message1
    "     - 2nd 0001 - Message1
    "         - 3rd 0001 - Message1
    "         - 3rd 0002 - Message2
    "     - 2nd 0002 - Message2
    "   - 1st 0002 - Message2

    " Test if anything has been found?
    cl_abap_unit_assert=>assert_not_initial( logs ).

    " Two 1st level reference logs has been returned?
    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 2 ).

    " 1st 0001
    data(log_object_1st_0001) = ref #( logs[ 1 ] ).

    " Test 1st 0001...
    me->assert_log( log                        = log_object_1st_0001->instance
                    exp_id                     = c_log-ref_id_1st
                    exp_has_messages           = abap_true
                    exp_number_of_msg          = 1
                    exp_number_of_msg_with_ref = 5
                    exp_has_ref_logs           = abap_true
                    exp_number_of_ref_logs     = 2 ).

    " 1st 0002
    data(log_object_1st_0002) = ref #( logs[ 2 ] ).

    " Test 1st 0002...
    me->assert_log( log                        = log_object_1st_0002->instance
                    exp_id                     = c_log-ref_id_1st
                    exp_has_messages           = abap_true
                    exp_number_of_msg          = 1
                    exp_number_of_msg_with_ref = 1
                    exp_has_ref_logs           = abap_false ).

    " Reference logs from 1st 0001
    data(ref_logs_1st_0001) = log_object_1st_0001->instance->get_ref_logs( ).

    " 2nd 0001
    data(ref_log_2nd_0001) = ref_logs_1st_0001[ 1 ].

    " Test 2nd 0001...
    me->assert_log( log                        = ref_log_2nd_0001-instance
                    exp_id                     = c_log-ref_id_2nd
                    exp_has_messages           = abap_true
                    exp_number_of_msg          = 1
                    exp_number_of_msg_with_ref = 3
                    exp_has_ref_logs           = abap_true
                    exp_number_of_ref_logs     = 2 ).

    " 2nd 0002
    data(ref_log_2nd_0002) = ref_logs_1st_0001[ 2 ]-instance.

    " Test 2nd 0002...
    me->assert_log( log               = ref_log_2nd_0002
                    exp_id            = c_log-ref_id_2nd
                    exp_has_messages  = abap_true
                    exp_number_of_msg = 1
                    exp_has_ref_logs  = abap_false ).

    data(ref_logs_2nd_0001) = ref_log_2nd_0001-instance->get_ref_logs( ).

    " Test 3rd 0001...
    me->assert_log( log               = ref_logs_2nd_0001[ 1 ]-instance
                    exp_id            = c_log-ref_id_3rd
                    exp_has_messages  = abap_true
                    exp_number_of_msg = 1
                    exp_has_ref_logs  = abap_false ).

    " Test 3rd 0002...
    me->assert_log( log               = ref_logs_2nd_0001[ 2 ]-instance
                    exp_id            = c_log-ref_id_3rd
                    exp_has_messages  = abap_true
                    exp_number_of_msg = 1
                    exp_has_ref_logs  = abap_false ).
  endmethod.


  method find_by_id_hierarchy.
    " Main log - with message
    "   -> 1st level (reference) - with messages
    "     -> 2nd level (reference) - with messages
    "       -> 3rd level (reference) - with messages
    "         -> 4th level (reference) - with messages
    "     -> 2nd level (reference) - with message
    "   -> multiple 1st level (reference) - with message
    data(log) = new zcl_lso_log_builder(
        " Main log
        )->set_log_id( c_log-id
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message1 main log' )->build( )
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message2 main log' )->build( )
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message3 main log' )->build( )
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message4 main log' )->build( )
        )->add_ref_log(
            " 1st level reference log
            new zcl_lso_log_builder(
                )->set_log_id( c_log-ref_id_1st
                )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message1 1st ref log' )->build( )
                )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message2 1st ref log' )->build( )
                )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message3 1st ref log' )->build( )
                )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message4 1st ref log' )->build( )
                )->add_ref_log(
                    " 2nd level reference log
                    new zcl_lso_log_builder(
                        )->set_log_id( c_log-ref_id_2nd
                        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message1 2nd ref log' )->build( )
                        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message2 2nd ref log' )->build( )
                        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message3 2nd ref log' )->build( )
                        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message4 2nd ref log' )->build( )
                        )->add_ref_log(
                            " 3rd level reference log
                            new zcl_lso_log_builder(
                                )->set_log_id( c_log-ref_id_3rd
                                )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message1 3rd ref log' )->build( )
                                )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message2 3rd ref log' )->build( )
                                )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message3 3rd ref log' )->build( )
                                )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message4 3rd ref log' )->build( )
                                )->add_ref_log(
                                  " 4th level reference log
                                  new zcl_lso_log_builder(
                                    )->set_log_id( c_log-ref_id_4th
                                    )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message1 4th ref log' )->build( )
                                    )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message2 4th ref log' )->build( )
                                    )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message3 4th ref log' )->build( )
                                    )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message4 4th ref log' )->build( )
                                    )->build( )
                            )->build( )
                        )->build( )
                )->build( )
        )->build( ).

    data(exp_number_of_ref_logs) = 1.
    data(exp_number_of_msg_with_ref) = 20.
    data(log_id_prefix) = 'LTC_REF_'.

    " Add multiple reference logs to the main one.
    do 1000 times.
      data(log_id) = conv zlso_log-id( |{ log_id_prefix }{ sy-index }| ).

      log->zif_lso_log~add_ref_log(
        new zcl_lso_log_builder( )->set_log_id( log_id
                                 )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( |Message1 { log_id } ref log| )->build( )
                                 )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( |Message2 { log_id } ref log| )->build( )
                                 )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( |Message3 { log_id } ref log| )->build( )
                                 )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( |Message4 { log_id } ref log| )->build( )
                                 )->build( ) ).

      exp_number_of_ref_logs = exp_number_of_ref_logs + 1.
      exp_number_of_msg_with_ref = exp_number_of_msg_with_ref + 4.
    enddo.

    log->save( ).

    " One more message added to the reference log.
    log_id = |{ log_id_prefix }1|.

    " Save reference log, new sequence number will be generated.
    new zcl_lso_log_builder(
      )->set_log_id( log_id
      )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( |Message5 { log_id } ref log| )->build( )
      )->build(
      )->save( ).

    " Increase expected number of reference logs taking into account just saved one.
    exp_number_of_ref_logs = exp_number_of_ref_logs + 1.

    " Increase expected number of messages including all reference logs + just saved one.
    exp_number_of_msg_with_ref = exp_number_of_msg_with_ref + 1.

    " Add log to the buffer for further tear down.
    me->add_log_to_teardown( log ).

    " Try to find logs starting with the main log id.
    data(logs) = me->cut->find( me->ids ).

    " Test if anything has been found?
    cl_abap_unit_assert=>assert_not_initial( logs ).

    " Main log has been returned only?
    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 1 ).

    " Main log
    data(act_log) = ref #( logs[ 1 ] ).

    " Assert main log details.
    me->assert_log( log                        = act_log->instance
                    exp_id                     = c_log-id
                    exp_has_messages           = abap_true
                    exp_number_of_msg          = 4
                    exp_number_of_msg_with_ref = exp_number_of_msg_with_ref
                    exp_has_ref_logs           = abap_true
                    exp_number_of_ref_logs     = exp_number_of_ref_logs ).
  endmethod.


  method find_by_id_without_ref_logs.
    " Main log - with message
    "   -> 1st level (reference) - with messages
    "     -> 2nd level (reference) - with messages
    data(log) = new zcl_lso_log_builder(
        " Main log
        )->set_log_id( c_log-id
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message1 main log' )->build( )
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message2 main log' )->build( )
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message3 main log' )->build( )
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message4 main log' )->build( )
        )->add_ref_log(
            " 1st level reference log
            new zcl_lso_log_builder(
                )->set_log_id( c_log-ref_id_1st
                )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message1 1st ref log' )->build( )
                )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message2 1st ref log' )->build( )
                )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message3 1st ref log' )->build( )
                )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message4 1st ref log' )->build( )
                )->add_ref_log(
                    " 2nd level reference log
                    new zcl_lso_log_builder(
                        )->set_log_id( c_log-ref_id_2nd
                        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message1 2nd ref log' )->build( )
                        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message2 2nd ref log' )->build( )
                        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message3 2nd ref log' )->build( )
                        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message4 2nd ref log' )->build( )
                        )->build( )
                )->build( )
        )->build( ).

    log->save( ).

    " One more message in the main log with the next sequence number.
    log->zif_lso_log_abstract~add_message( new zcl_lso_log_message_builder( )->set_msgid( 'ZLSO_LOG' )->set_msgty( 'E' )->set_msgv1( 'Message5 main log' )->build( ) ).

    log->save( ).

    " Add log to the buffer for further tear down.
    me->add_log_to_teardown( log ).

    " Try to find logs starting with the main log id without reference ones.
    data(logs) = me->cut->find( log_ids       = me->ids
                                with_ref_logs = abap_false ).

    " Test if anything has been found?
    cl_abap_unit_assert=>assert_not_initial( logs ).

    " Two main logs (different sequence number) have been returned only?
    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 2 ).

    " Main log (sequence number 0001)
    data(act_log) = ref #( logs[ 1 ] ).

    " Assert main log details.
    me->assert_log( log                        = act_log->instance
                    exp_id                     = c_log-id
                    exp_has_messages           = abap_true
                    exp_number_of_msg          = 4
                    exp_number_of_msg_with_ref = 4
                    exp_has_ref_logs           = abap_false
                    exp_number_of_ref_logs     = 0 ).

    " Main log (sequence number 0002)
    act_log = ref #( logs[ 2 ] ).

    " Assert main log details.
    me->assert_log( log                        = act_log->instance
                    exp_id                     = c_log-id
                    exp_has_messages           = abap_true
                    exp_number_of_msg          = 1
                    exp_number_of_msg_with_ref = 1
                    exp_has_ref_logs           = abap_false
                    exp_number_of_ref_logs     = 0 ).
  endmethod.


  method find_by_request_payload.
    data(exp_trace) = me->create_trace( c_trace-id ).

    data(exp_message) = new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid
                                                         )->set_msgty( 'S'
                                                         )->set_msgno( '000'
                                                         )->set_msgv1( 'Message 3 success WITH trace'
                                                         )->set_trace( exp_trace
                                                         )->build( ).

    " Main log
    data(exp_log) = new zcl_lso_log_builder(
        )->set_log_id( c_log-id
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 1 error no trace' )->build( )
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'W' )->set_msgv1( 'Message 2 warning no trace' )->build( )
        )->add_message( exp_message
        )->build( ).

    exp_log->save( ).

    " Add log to the buffer for further tear down.
    me->add_log_to_teardown( exp_log ).

    " Find with request payload pattern.
    data(logs) = me->cut->find( log_ids          = me->ids
                                log_dates        = me->dates
                                request_payloads = value #( ( sign   = zif_lso_log=>c_sign-include
                                                              option = zif_lso_log=>c_option-contains_pattern
                                                              low    = '"custom01": "custom01Test1"' ) ) ).

    cl_abap_unit_assert=>assert_not_initial( logs ).
    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 1 ).

    data(act_log) = logs[ 1 ]-instance.

    me->assert_log( log               = act_log
                    exp_id            = exp_log->zif_lso_log~get_id( )
                    exp_has_messages  = exp_log->zif_lso_log~has_messages( )
                    exp_number_of_msg = 1
                    exp_has_ref_logs  = abap_false ).

    data(messages) = act_log->get_messages( ).
    data(act_message) = ref #( messages[ 1 ] ).

    me->assert_message( message       = act_message->instance
                        exp_log_id    = act_log->get_id( )
                        exp_log_seqnr = act_log->get_seqnr( )
                        exp_msgty     = act_message->instance->get_symsg( )-msgty
                        exp_msgid     = act_message->instance->get_symsg( )-msgid
                        exp_msgno     = act_message->instance->get_symsg( )-msgno
                        exp_has_trace = act_message->instance->has_trace( ) ).

    data(act_trace) = act_message->instance->get_trace( ).

    me->assert_trace( trace                    = act_trace
                      exp_http_status          = exp_trace->get_http_status( )
                      exp_request_method       = exp_trace->get_request_method( )
                      exp_has_payload          = exp_trace->has_payload( )
                      exp_has_request_payload  = exp_trace->has_request_payload( )
                      exp_has_response_payload = exp_trace->has_response_payload( )
                      exp_has_headers          = exp_trace->has_headers( )
                      exp_has_request_headers  = exp_trace->has_request_headers( )
                      exp_has_response_headers = exp_trace->has_response_headers( ) ).
  endmethod.


  method find_by_response_payload.
    data(exp_trace) = me->create_trace( c_trace-id ).

    data(exp_message) = new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid
                                                         )->set_msgty( 'S'
                                                         )->set_msgno( '000'
                                                         )->set_msgv1( 'Message 3 success WITH trace'
                                                         )->set_trace( exp_trace
                                                         )->build( ).

    " Main log
    data(exp_log) = new zcl_lso_log_builder(
        )->set_log_id( c_log-id
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 1 error no trace' )->build( )
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'W' )->set_msgv1( 'Message 2 warning no trace' )->build( )
        )->add_message( exp_message
        )->build( ).

    exp_log->save( ).

    " Add log to the buffer for further tear down.
    me->add_log_to_teardown( exp_log ).

    " Find with request payload pattern.
    data(logs) = me->cut->find( log_ids           = me->ids
                                log_dates         = me->dates
                                response_payloads = value #( ( sign   = zif_lso_log=>c_sign-include
                                                               option = zif_lso_log=>c_option-contains_pattern
                                                               low    = '"status" : "OK"' ) ) ).

    cl_abap_unit_assert=>assert_not_initial( logs ).
    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 1 ).

    data(act_log) = logs[ 1 ]-instance.

    me->assert_log( log               = act_log
                    exp_id            = exp_log->get_id( )
                    exp_has_messages  = exp_log->zif_lso_log~has_messages( )
                    exp_number_of_msg = 1
                    exp_has_ref_logs  = abap_false ).

    data(messages) = act_log->get_messages( ).
    data(act_message) = ref #( messages[ 1 ] ).

    me->assert_message( message       = act_message->instance
                        exp_msgty     = act_message->instance->get_symsg( )-msgty
                        exp_msgid     = act_message->instance->get_symsg( )-msgid
                        exp_msgno     = act_message->instance->get_symsg( )-msgno
                        exp_has_trace = act_message->instance->has_trace( ) ).

    data(act_trace) = act_message->instance->get_trace( ).

    me->assert_trace( trace                    = act_trace
                      exp_http_status          = exp_trace->get_http_status( )
                      exp_request_method       = exp_trace->get_request_method( )
                      exp_has_payload          = exp_trace->has_payload( )
                      exp_has_request_payload  = exp_trace->has_request_payload( )
                      exp_has_response_payload = exp_trace->has_response_payload( )
                      exp_has_headers          = exp_trace->has_headers( )
                      exp_has_request_headers  = exp_trace->has_request_headers( )
                      exp_has_response_headers = exp_trace->has_response_headers( ) ).
  endmethod.


  method find_by_req_resp_payload.
    data(exp_trace) = me->create_trace( c_trace-id ).

    data(exp_message) = new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid
                                                         )->set_msgty( 'S'
                                                         )->set_msgno( '000'
                                                         )->set_msgv1( 'Message 3 success WITH trace'
                                                         )->set_trace( exp_trace
                                                         )->build( ).

    " Main log
    data(exp_log) = new zcl_lso_log_builder(
        )->set_log_id( c_log-id
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 1 error no trace' )->build( )
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'W' )->set_msgv1( 'Message 2 warning no trace' )->build( )
        )->add_message( exp_message
        )->build( ).

    exp_log->save( ).

    " Add log to the buffer for further tear down.
    me->add_log_to_teardown( exp_log ).

    " Find with request payload pattern.
    data(logs) = me->cut->find( log_ids           = me->ids
                                log_dates         = me->dates
                                request_payloads  = value #( ( sign   = zif_lso_log=>c_sign-include
                                                               option = zif_lso_log=>c_option-contains_pattern
                                                               low    = '"custom01": "custom01Test1"' ) )
                                response_payloads = value #( ( sign   = zif_lso_log=>c_sign-include
                                                               option = zif_lso_log=>c_option-contains_pattern
                                                               low    = '"status" : "OK"' ) ) ).

    cl_abap_unit_assert=>assert_not_initial( logs ).
    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 1 ).

    data(act_log) = logs[ 1 ]-instance.

    me->assert_log( log               = act_log
                    exp_id            = exp_log->get_id( )
                    exp_has_messages  = exp_log->zif_lso_log~has_messages( )
                    exp_number_of_msg = 1
                    exp_has_ref_logs  = abap_false ).

    data(messages) = act_log->get_messages( ).
    data(act_message) = ref #( messages[ 1 ] ).

    me->assert_message( message       = act_message->instance
                        exp_msgty     = act_message->instance->get_symsg( )-msgty
                        exp_msgid     = act_message->instance->get_symsg( )-msgid
                        exp_msgno     = act_message->instance->get_symsg( )-msgno
                        exp_has_trace = act_message->instance->has_trace( ) ).

    data(act_trace) = act_message->instance->get_trace( ).

    me->assert_trace( trace                    = act_trace
                      exp_http_status          = exp_trace->get_http_status( )
                      exp_request_method       = exp_trace->get_request_method( )
                      exp_has_payload          = exp_trace->has_payload( )
                      exp_has_request_payload  = exp_trace->has_request_payload( )
                      exp_has_response_payload = exp_trace->has_response_payload( )
                      exp_has_headers          = exp_trace->has_headers( )
                      exp_has_request_headers  = exp_trace->has_request_headers( )
                      exp_has_response_headers = exp_trace->has_response_headers( ) ).
  endmethod.


  method find_by_msg_req_resp_payload.
    data(exp_trace) = me->create_trace( c_trace-id ).

    data(exp_message) = new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid
                                                         )->set_msgty( 'S'
                                                         )->set_msgno( '000'
                                                         )->set_msgv1( 'Message 3 success WITH trace'
                                                         )->set_trace( exp_trace
                                                         )->build( ).

    " Main log
    data(exp_log) = new zcl_lso_log_builder(
        )->set_log_id( c_log-id
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 1 error no trace' )->build( )
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'W' )->set_msgv1( 'Message 2 warning no trace' )->build( )
        )->add_message( exp_message
        )->build( ).

    exp_log->save( ).

    " Add log to the buffer for further tear down.
    me->add_log_to_teardown( exp_log ).

    " Find with request payload pattern.
    data(logs) = me->cut->find( log_ids           = me->ids
                                log_dates         = me->dates
                                message_texts     = value #( ( sign   = zif_lso_log=>c_sign-include
                                                               option = zif_lso_log=>c_option-contains_pattern
                                                               low    = 'WITH trace' ) )
                                request_payloads  = value #( ( sign   = zif_lso_log=>c_sign-include
                                                               option = zif_lso_log=>c_option-contains_pattern
                                                               low    = '"custom01": "custom01Test1"' ) )
                                response_payloads = value #( ( sign   = zif_lso_log=>c_sign-include
                                                               option = zif_lso_log=>c_option-contains_pattern
                                                               low    = '"status" : "OK"' ) ) ).

    cl_abap_unit_assert=>assert_not_initial( logs ).
    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 1 ).

    data(act_log) = logs[ 1 ]-instance.

    me->assert_log( log               = act_log
                    exp_id            = exp_log->get_id( )
                    exp_has_messages  = exp_log->zif_lso_log~has_messages( )
                    exp_number_of_msg = 1
                    exp_has_ref_logs  = abap_false ).

    data(messages) = act_log->get_messages( ).
    data(act_message) = ref #( messages[ 1 ] ).

    me->assert_message( message       = act_message->instance
                        exp_msgty     = act_message->instance->get_symsg( )-msgty
                        exp_msgid     = act_message->instance->get_symsg( )-msgid
                        exp_msgno     = act_message->instance->get_symsg( )-msgno
                        exp_has_trace = act_message->instance->has_trace( ) ).

    data(act_trace) = act_message->instance->get_trace( ).

    me->assert_trace( trace                    = act_trace
                      exp_http_status          = exp_trace->get_http_status( )
                      exp_request_method       = exp_trace->get_request_method( )
                      exp_has_payload          = exp_trace->has_payload( )
                      exp_has_request_payload  = exp_trace->has_request_payload( )
                      exp_has_response_payload = exp_trace->has_response_payload( )
                      exp_has_headers          = exp_trace->has_headers( )
                      exp_has_request_headers  = exp_trace->has_request_headers( )
                      exp_has_response_headers = exp_trace->has_response_headers( ) ).
  endmethod.


  method find_last.
    data(message1) = new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 1 error no trace' )->build( ).
    data(message2) = new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'W' )->set_msgv1( 'Message 2 warning no trace' )->build( ).

    " Main log
    data(log) = new zcl_lso_log_builder( )->set_log_id( c_log-id
                                         )->add_message( message1
                                         )->add_message( message2
                                         )->build( ).

    log->save( ).

    " Add log to the buffer for further tear down.
    me->add_log_to_teardown( log ).

    data(last_messages) = me->cut->find_last_messages( me->ids ).

    cl_abap_unit_assert=>assert_not_initial( last_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( last_messages ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = last_messages[ 1 ]
                                        exp = value zif_lso_log_factory=>ts_last_message( log_id        = message2->get_log_id( )
                                                                                          log_seqnr     = message2->get_log_seqnr( )
                                                                                          log_ref_id    = ''
                                                                                          log_ref_seqnr = ''
                                                                                          msg_timestamp = message2->get_timestamp( )
                                                                                          msg_date      = message2->get_date( )
                                                                                          msg_time      = message2->get_time( ) ) ).
  endmethod.


  method find_last_with_ref.
    " Main log 0001: messages
    "   -> 1st ref log 0001: messages
    " 1st ref log 0002: add message
    "   -> 2nd ref log 0001: messages
    " 2nd ref log 0002: add message
    data(log) = new zcl_lso_log_builder(
        )->set_log_id( c_log-id
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 1 error no trace' )->build( )
        )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'W' )->set_msgv1( 'Message 2 warning no trace' )->build( )
        )->add_ref_log( new zcl_lso_log_builder( )->set_log_id( c_log-ref_id_1st
                                                 )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 1 error 1st ref' )->build( )
                                                 )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'W' )->set_msgv1( 'Message 2 warning 1st ref' )->build( )
                                                 )->build( )
        )->build( ).

    log->save( ).

    " Add one more message to the 1st reference log.
    data(ref_log_1st) = new zcl_lso_log_builder( )->set_log_id( c_log-ref_id_1st
                                                 )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 3 error 1st ref' )->build( )
                                                 )->build( ).

    ref_log_1st->save( ).

    " Prepare 2nd reference log.
    data(ref_log_2nd) = new zcl_lso_log_builder( )->set_log_id( c_log-ref_id_2nd
                                                 )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 4 error 2nd ref' )->build( )
                                                 )->build( ).

    ref_log_1st->zif_lso_log~add_ref_log( ref_log_2nd ).

    ref_log_1st->save( ).

    " Expected latest message added.
    data(exp_message) = new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 5 error 2nd ref' )->build( ).

    " Add message to the 2nd reference log.
    ref_log_2nd->add( exp_message ).

    ref_log_2nd->save( ).

    " Add log to the buffer for further tear down.
    me->add_log_to_teardown( log ).

    " Try to find last messages...
    data(last_messages) = me->cut->find_last_messages( me->ids ).

    cl_abap_unit_assert=>assert_not_initial( last_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( last_messages ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = last_messages[ 1 ]
                                        exp = value zif_lso_log_factory=>ts_last_message( log_id        = c_log-id
                                                                                          log_seqnr     = '0001'
                                                                                          log_ref_id    = exp_message->get_log_id( )
                                                                                          log_ref_seqnr = exp_message->get_log_seqnr( )
                                                                                          msg_timestamp = exp_message->get_timestamp( )
                                                                                          msg_date      = exp_message->get_date( )
                                                                                          msg_time      = exp_message->get_time( ) ) ).
  endmethod.


  method find_stripped.
    " Expected stripped date...
    data(exp_stripped_date) = conv zlso_log-stripped_date( cl_abap_context_info=>get_system_date( ) - 10 ).

    data(log) = new zcl_lso_log_builder( )->set_log_id( c_log-id
                                         )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Message 1' )->build( )
                                         )->set_stripped_date( exp_stripped_date
                                         )->build( ).

    log->save( ).

    " Add log to the buffer for further tear down.
    me->add_log_to_teardown( log ).

    data(logs) = me->cut->find( me->ids ).

    " Any log found?
    cl_abap_unit_assert=>assert_not_initial( logs ).
    cl_abap_unit_assert=>assert_equals( act = lines( logs ) exp = 1 ).

    " Actual log instance...
    data(act_log) = logs[ 1 ]-instance.

    " Has found log been stripped of messages?
    cl_abap_unit_assert=>assert_true( act_log->is_stripped( ) ).
    cl_abap_unit_assert=>assert_equals( act = act_log->get_stripped_date( ) exp = exp_stripped_date ).
  endmethod.


  method delete.
    data(log1) = new zcl_lso_log_builder(
            )->set_log_id( |{ c_log-id }_1|
            )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Log 1; Message 1 error no trace' )->build( )
            )->build( ).

    log1->save( ).

    " Add log for further tear down in case actual deletion wasn't finished successfully.
    me->add_log_to_teardown( log1 ).

    data(log2) = new zcl_lso_log_builder(
            )->set_log_id( |{ c_log-id }_2|
            )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Log 2; Message 1 error no trace' )->build( )
            )->build( ).

    log2->save( ).

    " Add log for further tear down in case actual deletion wasn't finished successfully.
    me->add_log_to_teardown( log2 ).

    data(log3) = new zcl_lso_log_builder(
            )->set_log_id( |{ c_log-id }_3|
            )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Log 3; Message 1 error no trace' )->build( )
            )->build( ).

    log3->save( ).

    " Add log for further tear down in case actual deletion wasn't finished successfully.
    me->add_log_to_teardown( log3 ).

    data(ids) = value zif_lso_log_factory=>tt_log_id( ( sign = zif_lso_log=>c_sign-include option = zif_lso_log=>c_option-equal low = log1->get_id( ) )
                                                      ( sign = zif_lso_log=>c_sign-include option = zif_lso_log=>c_option-equal low = log2->get_id( ) )
                                                      ( sign = zif_lso_log=>c_sign-include option = zif_lso_log=>c_option-equal low = log3->get_id( ) ) ).

    " Delete logs.
    me->cut->delete( ids ).

    " Try to find just deleted logs if they are still in DB.
    data(logs) = me->cut->find( ids ).

    " Nothing found after deletion?
    cl_abap_unit_assert=>assert_initial( logs ).
  endmethod.


  method delete_collection.
    data(log1) = new zcl_lso_log_builder(
            )->set_log_id( |{ c_log-id }_1|
            )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Log 1; Message 1 error no trace' )->build( )
            )->build( ).

    log1->save( ).

    " Add log for further tear down in case actual deletion wasn't finished successfully.
    me->add_log_to_teardown( log1 ).

    data(log2) = new zcl_lso_log_builder(
            )->set_log_id( |{ c_log-id }_2|
            )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Log 2; Message 1 error no trace' )->build( )
            )->build( ).

    log2->save( ).

    " Add log for further tear down in case actual deletion wasn't finished successfully.
    me->add_log_to_teardown( log2 ).

    data(log3) = new zcl_lso_log_builder(
            )->set_log_id( |{ c_log-id }_3|
            )->add_message( new zcl_lso_log_message_builder( )->set_msgid( c_msg-msgid )->set_msgty( 'E' )->set_msgv1( 'Log 3; Message 1 error no trace' )->build( )
            )->build( ).

    log3->save( ).

    " Add log for further tear down in case actual deletion wasn't finished successfully.
    me->add_log_to_teardown( log3 ).

    data(logs2delete) = value zlso_tt_logs( ( log1->get_object( ) ) ( log2->get_object( ) ) ( log3->get_object( ) ) ).

    " Delete logs collection.
    me->cut->delete_collection( logs2delete ).

    " Try to find just deleted logs if they are still in DB.
    data(logs) = me->cut->find( me->cut->logs2ids( logs2delete ) ).

    " Nothing found after deletion?
    cl_abap_unit_assert=>assert_initial( logs ).
  endmethod.


  method get.
    try.
        me->cut->get( conv #( 'X' ) ).
      catch zcx_lso_log into data(x).
    endtry.
  endmethod.

endclass.
