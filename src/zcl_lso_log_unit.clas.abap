class zcl_lso_log_unit definition for testing
  public
  create public
  duration short
  risk level harmless.

  public section.
    methods constructor .

  protected section.
    constants c_password type string value 'ThisPasswordShouldBeRemoved' ##NO_TEXT.

    constants begin of c_trace_data.
    constants request_url type zlso_log_trace-request_url value 'https://sap.com'.
    constants request_method type zlso_log_trace-request_method value 'GET'.
    constants http_status type zlso_log_trace-http_status value '200'.
    constants end of c_trace_data.

    methods start_time_measure .

    methods finish_time_measure
      returning
        value(rv_execution_time) type decfloat34 .

    methods get_request_headers
      returning
        value(rt_headers) type tihttpnvp .

    methods get_response_headers
      returning
        value(rt_headers) type tihttpnvp .

    methods get_html_payload
      returning
        value(rv_payload) type string .

    methods get_saml_payload
      returning
        value(payload) type string .

    methods get_json_request_payload
      returning
        value(rv_payload) type string .

    methods get_json_response_payload
      returning
        value(rv_payload) type string .

    methods get_json_sensitive_payload
      returning
        value(rv_payload) type string .

    methods get_sensitive_payload
      returning
        value(rv_payload) type string .

    methods get_xml_payload
      returning
        value(rv_payload) type string .

    methods create_trace
      importing id               type zlso_log_trace-id optional
                request_url      type string optional
                request_method   type string optional
                http_status      type i optional
                request_payload  type string optional
                response_payload type string optional
                request_headers  type tihttpnvp optional
                response_headers type tihttpnvp optional
                  preferred parameter id
      returning value(trace)     type ref to zcl_lso_log_trace.

    "! Add log to the buffer for further tear down.
    "! @parameter log |
    methods add_log_to_teardown
      importing log type ref to zcl_lso_log.

    "! Add log messages to the buffer for further tear down.
    "! @parameter messages |
    methods add_messages_to_teardown
      importing messages type ref to cl_object_collection.

    "! Add log message to the buffer for further tear down.
    "! @parameter message |
    methods add_message_to_teardown
      importing message type ref to zcl_lso_log_message.

    "! Add log traces to the buffer for further tear down.
    "! @parameter traces |
    methods add_traces_to_teardown
      importing traces type ref to cl_object_collection.

    "! Add log trace to the buffer for further tear down.
    "! @parameter trace |
    methods add_trace_to_teardown
      importing trace type ref to zcl_lso_log_trace.

    "! Add log payloads to the buffer for further tear down.
    "! @parameter payloads |
    methods add_payloads_to_teardown
      importing payloads type ref to cl_object_collection.

    "! Add log payload to the buffer for further tear down.
    "! @parameter payload |
    methods add_payload_to_teardown
      importing payload type ref to zcl_lso_log_payload.

    "! Delete log data from DB
    "! @parameter log |
    methods delete_log
      importing log type ref to zcl_lso_log.

    "! Delete log messsage from DB
    "! @parameter message | <p class="shorttext synchronized" lang="en"></p>
    methods delete_message
      importing message type ref to zcl_lso_log_message.

    "! <p class="shorttext synchronized" lang="en">Delete log trace from DB</p>
    "! @parameter trace | <p class="shorttext synchronized" lang="en"></p>
    methods delete_trace
      importing trace type ref to zcl_lso_log_trace.

    "! <p class="shorttext synchronized" lang="en">Delete payload from DB</p>
    "!
    "! @parameter payload | <p class="shorttext synchronized" lang="en"></p>
    methods delete_payload
      importing payload type ref to zcl_lso_log_payload.

    "! <p class="shorttext synchronized" lang="en">Assert log object</p>
    "!
    "! @parameter log | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_id | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_seqnr | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_has_messages | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_number_of_msg | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_number_of_msg_with_ref | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_has_ref_logs | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_number_of_ref_logs | <p class="shorttext synchronized" lang="en"></p>
    methods assert_log
      importing log                        type ref to zcl_lso_log
                exp_id                     type zlso_log-id optional
                exp_seqnr                  type zlso_log-seqnr optional
                exp_has_messages           type abap_bool optional
                exp_has_ref_logs           type abap_bool optional
                exp_number_of_msg          type i optional
                exp_number_of_msg_with_ref type i optional
                exp_number_of_ref_logs     type i optional.

    "! <p class="shorttext synchronized" lang="en">Assert message object</p>
    "!
    "! @parameter message | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_msgty | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_msgid | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_msgno | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_has_trace | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_log_id | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_log_seqnr | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_abap_program | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_abap_include | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_abap_event | <p class="shorttext synchronized" lang="en"></p>
    methods assert_message
      importing message          type ref to zcl_lso_log_message
                exp_log_id       type zlso_log_message-log_id optional
                exp_log_seqnr    type zlso_log_message-log_seqnr optional
                exp_msgty        type zlso_log_message-msgty optional
                exp_msgid        type zlso_log_message-msgid optional
                exp_msgno        type zlso_log_message-msgno optional
                exp_has_trace    type abap_bool optional
                exp_abap_program type zlso_log_message-abap_program optional
                exp_abap_include type zlso_log_message-abap_include optional
                exp_abap_event   type zlso_log_message-abap_event optional.

    "! <p class="shorttext synchronized" lang="en">Assert trace object</p>
    "!
    "! @parameter trace | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_http_status | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_request_method | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_has_payload | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_has_request_payload | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_has_response_payload | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_has_headers | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_has_request_headers | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_has_response_headers | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter exp_req_payload_cs_pattern | <p class="shorttext synchronized" lang="en">Request payload contains pattern</p>
    "! @parameter exp_resp_payload_cs_pattern | <p class="shorttext synchronized" lang="en">Response payload contains pattern</p>
    methods assert_trace
      importing trace                       type ref to zcl_lso_log_trace
                exp_http_status             type zlso_log_trace-http_status optional
                exp_request_method          type zlso_log_trace-request_method optional
                exp_has_payload             type abap_bool optional
                exp_has_request_payload     type abap_bool optional
                exp_has_response_payload    type abap_bool optional
                exp_req_payload_cs_pattern  type string optional
                exp_resp_payload_cs_pattern type string optional
                exp_has_headers             type abap_bool optional
                exp_has_request_headers     type abap_bool optional
                exp_has_response_headers    type abap_bool optional
                exp_request_header          type ihttpnvp optional
                exp_response_header         type ihttpnvp optional.


  private section.
    data mv_t1 type i .
    data mv_t2 type i .
    data mo_abap_runtime type ref to if_abap_runtime .
    data logs type ref to cl_object_collection.
    data messages type ref to cl_object_collection.
    data traces type ref to cl_object_collection.
    data payloads type ref to cl_object_collection.
    data deleted_objects type zif_lso_log=>tt_objects.

    methods teardown.
endclass.


class zcl_lso_log_unit implementation.

  method constructor.
    me->mo_abap_runtime = cl_abap_runtime=>create_hr_timer( ).

    " Logs created for the unit tests purpose.
    me->logs = new #( ).

    " Messages created for the unit test purpose.
    me->messages = new #( ).

    " Traces created for the unit test purpose.
    me->traces = new #( ).

    " Payloads created for the unit test purpose.
    me->payloads = new #( ).

    " Deleted log object within test runtime.
    me->deleted_objects = value #( ).
  endmethod.


  method teardown.
    " Logs...
    if not me->logs->is_empty( ).
      data(iterator) = me->logs->get_iterator( ).

      while iterator->has_next( ).
        " Delete test data
        me->delete_log( cast zcl_lso_log( iterator->get_next( ) ) ).
      endwhile.
    endif.

    " Messages...
    if not me->messages->is_empty( ).
      data(messages_iterator) = me->messages->get_iterator( ).

      while messages_iterator->has_next( ).
        me->delete_message( cast zcl_lso_log_message( messages_iterator->get_next( ) ) ) .
      endwhile.
    endif.

    " Traces...
    if not me->traces->is_empty( ).
      data(traces_iterator) = me->traces->get_iterator( ).

      while traces_iterator->has_next( ).
        me->delete_trace( cast zcl_lso_log_trace( traces_iterator->get_next( ) ) ) .
      endwhile.
    endif.

    " Payloads...
    if not me->payloads->is_empty( ).
      data(payloads_iterator) = me->payloads->get_iterator( ).

      while payloads_iterator->has_next( ).
        me->delete_payload( cast zcl_lso_log_payload( payloads_iterator->get_next( ) ) ) .
      endwhile.
    endif.
  endmethod.


  method start_time_measure.
    me->mv_t1 = me->mo_abap_runtime->get_runtime( ).
  endmethod.


  method finish_time_measure.
    me->mv_t2 = me->mo_abap_runtime->get_runtime( ).

*   Return execution time in seconds.
    rv_execution_time = ( me->mv_t2 - me->mv_t1 ) / 1000000.
  endmethod.


  method create_trace.
    if id is not initial.
      data(trace_id) = id.
    else.
      try.
          " Generate random trace id with 'LTC_' prefix.
          data(uuid) = cl_system_uuid=>if_system_uuid_static~create_uuid_c22( ).
          trace_id = |LTC_{ uuid+4(18) }|.
        catch cx_uuid_error.
          wait up to 1 seconds.
          retry.
      endtry.
    endif.

    trace = new zcl_lso_log_trace(
        id               = trace_id
        request_url      = cond #( when request_url is not initial then request_url else c_trace_data-request_url )
        request_method   = cond #( when request_method is not initial then request_method else c_trace_data-request_method )
        http_status      = cond #( when http_status is not initial then http_status else c_trace_data-http_status )
        request_payload  = cond #( when request_payload is not initial then request_payload else me->get_json_request_payload( ) )
        response_payload = cond #( when response_payload is not initial then response_payload else me->get_json_response_payload( ) )
        request_headers  = cond #( when request_headers is not initial then request_headers else me->get_request_headers( ) )
        response_headers = cond #( when response_headers is not initial then response_headers else me->get_response_headers( ) ) ).
  endmethod.


  method add_log_to_teardown.
    me->logs->add( log ).

    if log->zif_lso_log~has_ref_logs( ).
      data(iterator) = log->zif_lso_log~get_ref_logs( )->get_iterator( ).

      while iterator->has_next( ).
        me->logs->add( cast zcl_lso_log( iterator->get_next( ) ) ).
      endwhile.
    endif.
  endmethod.


  method add_messages_to_teardown.
    data(iterator) = messages->get_iterator( ).

    while iterator->has_next( ).
      me->add_message_to_teardown( cast zcl_lso_log_message( iterator->get_next( ) ) ).
    endwhile.
  endmethod.


  method add_message_to_teardown.
    me->messages->add( message ).
  endmethod.


  method add_traces_to_teardown.
    data(iterator) = traces->get_iterator( ).

    while iterator->has_next( ).
      me->add_trace_to_teardown( cast zcl_lso_log_trace( iterator->get_next( ) ) ).
    endwhile.
  endmethod.


  method add_trace_to_teardown.
    me->traces->add( trace ).
  endmethod.


  method add_payloads_to_teardown.
    data(iterator) = payloads->get_iterator( ).

    while iterator->has_next( ).
      me->add_payload_to_teardown( cast zcl_lso_log_payload( iterator->get_next( ) ) ).
    endwhile.
  endmethod.


  method add_payload_to_teardown.
    me->payloads->add( payload ).
  endmethod.


  method delete_log.
    " Cross-reference logs protection!
    if line_exists( me->deleted_objects[ id    = log->zif_lso_log~get_id( )
                                         seqnr = log->zif_lso_log~get_seqnr( ) ] ).
      " Log has been already deleted.
      return.
    endif.

    " Mark log object as already deleted.
    insert value #( id       = log->zif_lso_log~get_id( )
                    seqnr    = log->zif_lso_log~get_seqnr( )
                    instance = log )
      into table me->deleted_objects.

    if log->zif_lso_log~has_ref_logs( ).
      data(iterator) = log->zif_lso_log~get_ref_logs( )->get_iterator( ).

      while iterator->has_next( ).
        me->delete_log( cast zcl_lso_log( iterator->get_next( ) ) ).
      endwhile.
    endif.

    data(id) = log->zif_lso_log~get_id( ).

    if id is initial.
      return.
    endif.

    delete from zlso_log_payload
      where trace_id in (
        select id
          from zlso_log_trace
         where id in (
           select trace_id
             from zlso_log_message
            where log_id eq @id )
      ).

    delete from zlso_log_headers
      where trace_id in (
        select id
          from zlso_log_trace
         where id in (
           select trace_id
             from zlso_log_message
            where log_id eq @id )
      ).

    delete from zlso_log_trace
      where id in (
        select trace_id
          from zlso_log_message
         where log_id eq @id ).

    delete from zlso_log_message where log_id eq @id.
    delete from zlso_log where id eq @id.

    commit work and wait.
  endmethod.


  method delete_message.
    data(log_id) = message->get_log_id( ).
    data(log_seqnr) = message->get_log_seqnr( ).
    data(timestamp) = message->get_timestamp( ).

    if log_id is initial or log_seqnr is initial or timestamp is initial.
      return.
    endif.

    delete from zlso_log_payload
      where trace_id in (
        select id
          from zlso_log_trace
         where id in (
           select trace_id
             from zlso_log_message
            where log_id    eq @log_id
              and log_seqnr eq @log_seqnr
              and timestamp eq @timestamp )
      ).

    delete from zlso_log_headers
      where trace_id in (
        select id
          from zlso_log_trace
         where id in (
           select trace_id
             from zlso_log_message
            where log_id    eq @log_id
              and log_seqnr eq @log_seqnr
              and timestamp eq @timestamp )
      ).

    delete from zlso_log_trace
      where id in (
        select trace_id
          from zlso_log_message
         where log_id    eq @log_id
           and log_seqnr eq @log_seqnr
           and timestamp eq @timestamp ).

    delete from zlso_log_message
      where log_id    eq @log_id
        and log_seqnr eq @log_seqnr
        and timestamp eq @timestamp.

    commit work.
  endmethod.


  method delete_trace.
    data(id) = trace->get_id( ).

    if id is initial.
      return.
    endif.

    delete from zlso_log_payload where trace_id eq @id.
    delete from zlso_log_headers where trace_id eq @id.
    delete from zlso_log_trace where id eq @id.

    commit work.
  endmethod.


  method delete_payload.
    data(trace_id) = payload->get_trace_id( ).

    if trace_id is initial.
      return.
    endif.

    delete from zlso_log_payload where trace_id eq @trace_id.

    commit work.
  endmethod.


  method assert_log.
    " Is log object is correctly bound?
    cl_abap_unit_assert=>assert_bound( log ).

    if exp_id is supplied.
      " Is log id correct?
      cl_abap_unit_assert=>assert_equals( act = log->zif_lso_log~get_id( ) exp = exp_id ).
    endif.

    if exp_seqnr is supplied.
      " Is sequence number as expected?
      cl_abap_unit_assert=>assert_equals( act = log->zif_lso_log~get_seqnr( ) exp = exp_seqnr ).
    endif.

    if exp_has_messages is supplied.
      " Has log got messages?
      cl_abap_unit_assert=>assert_equals( act = log->zif_lso_log~has_messages( ) exp = exp_has_messages ).
    endif.

    if exp_number_of_msg is supplied.
      " Is number of messages without reference log correct?
      cl_abap_unit_assert=>assert_equals( act = log->zif_lso_log~get_messages( abap_false )->size( ) exp = exp_number_of_msg ).
    endif.

    if exp_number_of_msg_with_ref is supplied.
      " Is number of messages with reference log correct?
      cl_abap_unit_assert=>assert_equals( act = log->zif_lso_log~get_messages( abap_true )->size( ) exp = exp_number_of_msg_with_ref ).
    endif.

    if exp_has_ref_logs is supplied.
      " Has log got a reference one?
      cl_abap_unit_assert=>assert_equals( act = log->zif_lso_log~has_ref_logs( ) exp = exp_has_ref_logs ).
    endif.

    if exp_number_of_ref_logs is supplied.
      " Is number of reference logs correct?
      cl_abap_unit_assert=>assert_equals( act = log->zif_lso_log~get_ref_logs( )->size( ) exp = exp_number_of_ref_logs ).
    endif.
  endmethod.


  method assert_message.
    " Is log message correctly bound?
    cl_abap_unit_assert=>assert_bound( message ).

    if exp_log_id is supplied.
      " Is log id as expected?
      cl_abap_unit_assert=>assert_equals( act = message->zif_lso_log_message~get_log_id( ) exp = exp_log_id ).
    endif.

    if exp_log_seqnr is supplied.
      " Is log sequence number as expected?
      cl_abap_unit_assert=>assert_equals( act = message->zif_lso_log_message~get_log_seqnr( ) exp = exp_log_seqnr ).
    endif.

    if exp_msgty is supplied.
      " Is message type as expected?
      cl_abap_unit_assert=>assert_equals( act = message->zif_lso_log_message~get_type( ) exp = exp_msgty ).
    endif.

    if exp_msgid is supplied.
      " Is message id as expected?
      cl_abap_unit_assert=>assert_equals( act = message->zif_lso_log_message~get_class( ) exp = exp_msgid ).
    endif.

    if exp_msgno is supplied.
      " Is message number as expected?
      cl_abap_unit_assert=>assert_equals( act = message->zif_lso_log_message~get_number( ) exp = exp_msgno ).
    endif.

    if exp_has_trace is supplied.
      " Has message got a trace object?
      cl_abap_unit_assert=>assert_equals( act = message->zif_lso_log_message~has_trace( ) exp = exp_has_trace ).
    endif.

    if exp_abap_program is supplied.
      " Has message got an ABAP stack program as expected?
      cl_abap_unit_assert=>assert_char_cp( act = message->zif_lso_log_message~get_abap_stack( )-abap_program exp = exp_abap_program ).
    endif.

    if exp_abap_include is supplied.
      " Has message got an ABAP stack include as expected?
      cl_abap_unit_assert=>assert_char_cp( act = message->zif_lso_log_message~get_abap_stack( )-abap_include exp = exp_abap_include ).
    endif.

    if exp_abap_event is supplied.
      " Has message got an ABAP stack event as expected?
      cl_abap_unit_assert=>assert_equals( act = message->zif_lso_log_message~get_abap_stack( )-abap_event exp = exp_abap_event ).
    endif.
  endmethod.


  method assert_trace.
    " Is log trace correctly bound?
    cl_abap_unit_assert=>assert_bound( trace ).

    if exp_http_status is supplied.
      " Is http status as expected?
      cl_abap_unit_assert=>assert_equals( act = trace->get_http_status( ) exp = exp_http_status ).
    endif.

    if exp_request_method is supplied.
      " Is request method as expected?
      cl_abap_unit_assert=>assert_equals( act = trace->get_request_method( ) exp = exp_request_method ).
    endif.

    if exp_has_payload is supplied.
      " Has trace got request/response payload?
      cl_abap_unit_assert=>assert_equals( act = trace->has_payload( ) exp = exp_has_payload ).
    endif.

    if exp_has_request_payload is supplied.
      " Has trace got request payload?
      cl_abap_unit_assert=>assert_equals( act = trace->has_request_payload( ) exp = exp_has_request_payload ).
    endif.

    if exp_has_response_payload is supplied.
      " Has trace got response payload?
      cl_abap_unit_assert=>assert_equals( act = trace->has_response_payload( ) exp = exp_has_response_payload ).
    endif.

    if exp_req_payload_cs_pattern is supplied.
      " Does request payload contain expected text pattern?
      cl_abap_unit_assert=>assert_char_cp( act = trace->get_request_payload( )->get_payload( ) exp = |*{ exp_req_payload_cs_pattern }*| ).
    endif.

    if exp_resp_payload_cs_pattern is supplied.
      " Does response payload contain expected text pattern?
      cl_abap_unit_assert=>assert_char_cp( act = trace->get_response_payload( )->get_payload( ) exp = |*{ exp_resp_payload_cs_pattern }*| ).
    endif.

    if exp_has_headers is supplied.
      " Has trace got request/response headers?
      cl_abap_unit_assert=>assert_equals( act = trace->has_headers( ) exp = exp_has_headers ).
    endif.

    if exp_has_request_headers is supplied.
      " Has trace got request headers?
      cl_abap_unit_assert=>assert_equals( act = trace->has_request_headers( ) exp = exp_has_request_headers ).
    endif.

    if exp_has_response_headers is supplied.
      " Has trace got response headers?
      cl_abap_unit_assert=>assert_equals( act = trace->has_response_headers( ) exp = exp_has_response_headers ).
    endif.

    if exp_request_header is supplied.
      " Has trace got particular request header?
      cl_abap_unit_assert=>assert_table_contains( line = exp_request_header table = trace->get_request_headers( )->get_headers( ) ).
    endif.

    if exp_response_header is supplied.
      " Has trace got particular response header?
      cl_abap_unit_assert=>assert_table_contains( line = exp_response_header table = trace->get_response_headers( )->get_headers( ) ).
    endif.
  endmethod.


  method get_request_headers.
    rt_headers = value tihttpnvp( ( name = 'Content-Type' value = 'application/json' )
                                  ( name = 'Accept'       value = 'cookies-from-strangers' )
                                  ( name = 'Status'       value = 'married-with-children' )
                                  ( name = 'Age'          value = 'old-enough-to-be-your-dad' )
                                  ( name = 'Client-Date'  value = 'a-complete-waste-of-time' )
                                  ( name = 'Connection'   value = 'Keep-Alive-Until-Summer' )
                                  ( name = 'X-Robots-Tag' value = 'Cylon Caprica Six' ) ).
  endmethod.


  method get_response_headers.
    rt_headers = value tihttpnvp( ( name = 'Access-Control-Allow-Origin' value = '*' )
                                  ( name = 'Connection'                  value = 'Keep-Alive' )
                                  ( name = 'Content-Encoding'            value = 'gzip' )
                                  ( name = 'Content-Type'                value = 'text/html; charset=utf-8' )
                                  ( name = 'Date'                        value = 'Mon, 18 Jul 2016 16:06:00 GMT' )
                                  ( name = 'Etag'                        value = '"c561c68d0ba92bbeb8b0f612a9199f722e3a621a"' )
                                  ( name = 'Keep-Alive'                  value = 'timeout=5, max=997' )
                                  ( name = 'Last-Modified'               value = 'Mon, 18 Jul 2016 02:36:04 GMT' )
                                  ( name = 'Server'                      value = 'Apache' )
                                  ( name = 'Set-cookie'                  value = 'mykey=myvalue; expires=Mon, 17-Jul-2017 16:06:00 GMT; Max-Age=31449600; Path=/; secure' )
                                  ( name = 'Transfer-Encoding'           value = 'chunked' )
                                  ( name = 'Vary'                        value = 'Cookie, Accept-Encoding' )
                                  ( name = 'X-Backend-Server'            value = 'developer2.webapp.scl3.mozilla.com' )
                                  ( name = 'X-Cache-Info'                value = 'not cacheable; meta data too large' )
                                  ( name = 'X-kuma-revision'             value = '1085259' )
                                  ( name = 'x-frame-options'             value = 'DENY' ) ).
  endmethod.


  method get_saml_payload.

    payload =
       |<saml:Issuer>ILDCLNT001</saml:Issuer>| &&
       |  <ds:Signature xmlns:ds="http://www.w3.org/2000/09/xmldsig#">| &&
       |   <ds:SignedInfo>| &&
       |    <ds:CanonicalizationMethod Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#"/>| &&
       |    <ds:SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"/>| &&
       |    <ds:Reference URI="#A-901b0ead-956c-1eda-a3e1-5275720df665">| &&
       |     <ds:Transforms>| &&
       |      <ds:Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"/>| &&
       |      <ds:Transform Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#"/>| &&
       |     </ds:Transforms>| &&
       |     <ds:DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"/>| &&
       |     <ds:DigestValue>1vLto+S06LBS/Z+eYj4Y+8XLl8o=</ds:DigestValue>| &&
       |    </ds:Reference>| &&
       |   </ds:SignedInfo>| &&
       |   <ds:SignatureValue>MNe1K08jPj4PyjLOU+jTrCeNM5isFhhBS78X2QbZSnVYZZ6ShZcBdSsaagAySlcxY8m7CpsqZvFd| &&
       | F72kYlm/6fIQBbCewoGBHRFj4lWqdegdskwOnF4YpDXldH+9BQzh8+JkJSZUKld3J6cFPAXxc3ma| &&
       | aU7+7Pk2Ft+IkWZyxj11N5Rll36i1AU+hFAc8C9nSoDqVmfvRzYwj4ZgNiVstQmKNCG6ReVQnQc9| &&
       | RyBgUNOpHh+bfHrtCFAxR4N7wjhmIn4L69itFJr+F28oMuqf2KTZAVGUtYlYpF+UUb/Ksp68pQo2| &&
       | sVEUXPrDOWWE7T8tPg9IXQkc8JaIgaa1wWFnXA==</ds:SignatureValue>| &&
       |  </ds:Signature>| &&
       |  <saml:Subject>| &&
       |   <saml:NameID Format="urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified" NameQualifier="www.successfactors.com">E_BMW_10041</saml:NameID>| &&
       |   <saml:SubjectConfirmation Method="urn:oasis:names:tc:SAML:2.0:cm:bearer">| &&
       |    <saml:SubjectConfirmationData NotOnOrAfter="2020-05-05T20:06:57Z" Recipient="https://jam12.sapjam.com/api/v1/auth/token"/>| &&
       |   </saml:SubjectConfirmation>| &&
       |  </saml:Subject>| &&
       |  <saml:Conditions NotBefore="2020-05-05T19:51:57Z" NotOnOrAfter="2020-05-05T20:06:57Z">| &&
       |   <saml:AudienceRestriction>| &&
       |    <saml:Audience>cubetree.com</saml:Audience>| &&
       |   </saml:AudienceRestriction>| &&
       |  </saml:Conditions>| &&
       |  <saml:AuthnStatement AuthnInstant="2020-05-05T19:56:57Z" SessionIndex="S-SP-901b0ead-956c-1eda-a3e1-5275720dd665" SessionNotOnOrAfter="2020-05-05T19:56:57Z">| &&
       |   <saml:AuthnContext>| &&
       |    <saml:AuthnContextClassRef>urn:oasis:names:tc:SAML:2.0:ac:classes:PreviousSession</saml:AuthnContextClassRef>| &&
       |   </saml:AuthnContext>| &&
       |  </saml:AuthnStatement>| &&
       |  <saml:AttributeStatement>| &&
       |   <saml:Attribute Name="client_id">| &&
       |    <saml:AttributeValue>GjLxOrmK2LaUGUhgBFVj</saml:AttributeValue>| &&
       |   </saml:Attribute>| &&
       |  </saml:AttributeStatement>| &&
       | </saml:Assertion>|.

  endmethod.

  method get_html_payload.
    rv_payload =
      |<!DOCTYPE html>                     | &&
      |<html>                              | &&
      |  <body>                            | &&
      |  <h1>ZCL_LSO_LOG_UNIT Heading</h1> | &&
      |  <p>ZCL_LSO_LOG_UNIT paragraph</p> | &&
      |</body>                             | &&
      |</html>                             |.
  endmethod.


  method get_json_request_payload.
    rv_payload = || &&
|[| &&
  |\{| &&
    |"__metadata": \{ "uri": "User('P380627999')" \},| &&
    |"userId": "P380627999",| &&
    |"email": "mikolajstachura01092016@example.com",| &&
    |"firstName": "Peter",| &&
    |"lastName": "Donne",| &&
    |"username": "P380627999",| &&
    |"loginMethod": "SSO",| &&
    |"manager": \{ "__metadata": \{ "uri": "User('P380633927')" \} \},| &&
    |"hr": \{ "__metadata": \{ "uri": "User('NO_HR')" \} \},| &&
    |"country": "DE",| &&
    |"location": "DE",| &&
    |"gender": "M",| &&
    |"status": "active", | &&
    |"department": "department",| &&
    |"timeZone": "Europe/London",| &&
    |"jobCode": "unknown",| &&
    |"custom01": "custom01Test1",| &&
    |"custom02": "CUSTOMER_USER_FREE",| &&
    |"division": "PubEd",| &&
    |"custom15": "Y"| &&
  |\},| &&
  |\{| &&
    |"__metadata": \{ "uri": "User('P380634796')" \},| &&
    |"userId": "P380634796",| &&
    |"email": "emilia.clarke2@example.com",| &&
    |"firstName": "e",| &&
    |"lastName": "c",| &&
    |"username": "P380634796",| &&
    |"loginMethod": "SSO",| &&
    |"manager": \{ "__metadata": \{ "uri": "User('P380633927')" \} \},| &&
    |"hr": \{ "__metadata": \{ "uri": "User('NO_HR')" \} \},| &&
    |"country": "DE",| &&
    |"location": "DE",| &&
    |"gender": "M",| &&
    |"status": "active",| &&
    |"department": "department",| &&
    |"timeZone": "Europe/London",| &&
    |"jobCode": "unknown",| &&
    |"custom01": "custom01Test2",| &&
    |"custom02": "CUSTOMER_USER_FREE",| &&
    |"division": "PubEd",| &&
    |"custom15": "Y"| &&
  |\},| &&
  |\{| &&
    |"__metadata": \{ "uri": "User('P380627455')" \},| &&
    |"userId": "P380627455",| &&
    |"email": "bayon@example.com",| &&
    |"firstName": "Andre",| &&
    |"lastName": "Bayon",| &&
    |"username": "P380627455",| &&
    |"loginMethod": "SSO",| &&
    |"manager": \{ "__metadata": \{ "uri": "User('P380633945')" \} \},| &&
    |"hr": \{ "__metadata": \{ "uri": "User('NO_HR')" \} \},| &&
    |"country": "DE",| &&
    |"location": "DE",| &&
    |"gender": "M",| &&
    |"status": "active",| &&
    |"department": "department",| &&
    |"timeZone": "Europe/London",| &&
    |"jobCode": "unknown",| &&
    |"custom01": "custom01Test3",| &&
    |"custom02": "CUSTOMER_USER_FREE",| &&
    |"division": "PubEd",| &&
    |"custom15": "Y"| &&
  |\},| &&
  |\{| &&
    |"__metadata": \{ "uri": "User('P380603401')" \},| &&
    |"userId": "P380603401",| &&
    |"email": "kruemel.monster@example.com",| &&
    |"firstName": "Krumel",| &&
    |"lastName": "Monster",| &&
    |"username": "P380603401",| &&
    |"loginMethod": "SSO",| &&
    |"manager": \{ "__metadata": \{ "uri": "User('NO_MANAGER')" \} \},| &&
    |"hr": \{ "__metadata": \{ "uri": "User('NO_HR')" \} \},| &&
    |"country": "DE",| &&
    |"location": "DE",| &&
    |"gender": "M",| &&
    |"status": "active",| &&
    |"department": "department",| &&
    |"timeZone": "Europe/London",| &&
    |"jobCode": "unknown",| &&
    |"custom01": "custom01Test4",| &&
    |"custom02": "CUSTOMER_USER_FREE",| &&
    |"division": "PubEd",| &&
    |"custom15": "Y"| &&
  |\},| &&
  |\{| &&
    |"__metadata": \{ "uri": "User('P380610739')" \},| &&
    |"userId": "P380610739",| &&
    |"email": "alintott.tangentsnowball+agltest_20160429-1@gmail.com",| &&
    |"firstName": "AglTest_20160429-1",| &&
    |"lastName": "AglTest_20160429-1",| &&
    |"username": "P380610739",| &&
    |"loginMethod": "SSO",| &&
    |"manager": \{ "__metadata": \{ "uri": "User('NO_MANAGER')" \} \},| &&
    |"hr": \{ "__metadata": \{ "uri": "User('NO_HR')" \} \},| &&
    |"country": "EG",| &&
    |"location": "EG",| &&
    |"gender": "M",| &&
    |"status": "active",| &&
    |"department": "department",| &&
    |"timeZone": "Europe/London",| &&
    |"jobCode": "unknown",| &&
    |"custom01": "CLC",| &&
    |"custom02": "CUSTOMER_USER_PAID",| &&
    |"custom03": "CLC",| &&
    |"division": "CLC",| &&
    |"custom15": "Y"| &&
  |\}| &&
|]|.
  endmethod.


  method get_json_response_payload.
    rv_payload = || &&
| \{ | &&
  |"d" : [| &&
    |\{| &&
      |"key" : "User1",| &&
      |"status" : "ERROR",| &&
      |"editStatus" : "UPSERTED",| &&
      |"message" : "unknown property loginmethod for User",| &&
      |"index" : 0,| &&
      |"httpCode" : 400,| &&
      |"inlineResults" : null| &&
    |\},| &&
    |\{| &&
      |"key" : "User2",| &&
      |"status" : "OK",| &&
      |"editStatus" : "UPSERTED",| &&
      |"message" : "",| &&
      |"index" : 1,| &&
      |"httpCode" : 200,| &&
      |"inlineResults" : null| &&
    |\},| &&
    |\{| &&
      |"key" : "User3",| &&
      |"status" : "ERROR",| &&
      |"editStatus" : "UPSERTED",| &&
      |"message" : "unknown property loginmethod for User",| &&
      |"index" : 1,| &&
      |"httpCode" : 400,| &&
      |"inlineResults" : null| &&
    |\},| &&
    |\{| &&
      |"key" : "User4",| &&
      |"status" : "OK",| &&
      |"editStatus" : "UPSERTED",| &&
      |"message" : "",| &&
      |"index" : 1,| &&
      |"httpCode" : 200,| &&
      |"inlineResults" : null| &&
    |]| &&
|\}|.
  endmethod.


  method get_json_sensitive_payload.
    rv_payload =
      |[| &&
      |\{| &&
      | "__metadata":| &&
      | \{| &&
      |  "uri":"User('ARBED_10022')"| &&
      | \},| &&
      | "userId":"ARBED_10022",| &&
      | "email":"ildlsotester+man-go2@gmail.com",| &&
      | "firstName":"Go2",| &&
      | "lastName":"Manager",| &&
      | "username":"ildlsotester+man-go2@gmail.com",| &&
      | "loginMethod":"PWD",| &&
      | "password":"{ me->c_password }",| &&
      | "manager":| &&
      | \{| &&
      |  "__metadata":| &&
      |  \{| &&
      |   "uri":"User('ARBED_10013')"| &&
      |  \}| &&
      | \},| &&
      | "hr":| &&
      | \{| &&
      |  "__metadata":| &&
      |  \{| &&
      |   "uri":"User('NO_HR')"| &&
      |  \}| &&
      | \},| &&
      | "country":"DE",| &&
      | "location":"DE",| &&
      | "gender":"M",| &&
      | "status":"active",| &&
      | "department":"department",| &&
      | "timeZone":"Europe/London",| &&
      | "jobCode":"unknown",| &&
      | "custom01":"PartnerEd",| &&
      | "custom02":"ENTERPRISE_Customer",| &&
      | "division":"PartnerEd",| &&
      | "custom15":"Y"| &&
      |\}| &&
      |]|.
  endmethod.


  method get_sensitive_payload.
    rv_payload =
      |<?xml version="1.0"?>   | &&
      |<soapenv:Envelope xmlns:urn="urn:sfobject.sfapi.successfactors.com" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">| &&
      |  <soapenv:Header/>| &&
      |  <soapenv:Body>| &&
      |    <urn:login>| &&
      |      <urn:credential>| &&
      |      <urn:companyId>learninghubT</urn:companyId>| &&
      |      <urn:username>SFAPI</urn:username>| &&
      |      <urn:password>{ me->c_password }</urn:password>| &&
      |      </urn:credential>| &&
      |    </urn:login>| &&
      |  </soapenv:Body>| &&
      |</soapenv:Envelope>|.
  endmethod.


  method get_xml_payload.
    rv_payload =
      |<?xml version="1.0" encoding="UTF-8"?>| &&
      |<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/">| &&
      | <S:Body>| &&
      |   <upsertResponse xmlns="urn:sfobject.sfapi.successfactors.com" xmlns:ns2="urn:fault.sfapi.successfactors.com">| &&
      |     <result>| &&
      |       <jobStatus>OK</jobStatus>| &&
      |       <message xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:nil="true" />| &&
      |       <objectEditResult>| &&
      |         <id>USR-350621</id>| &&
      |         <errorStatus>OK</errorStatus>| &&
      |         <editStatus>UPDATED</editStatus>| &&
      |         <index>0</index>| &&
      |         <message xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:nil="true" />| &&
      |       </objectEditResult>| &&
      |       <objectEditResult>| &&
      |         <id>USR-350625</id>| &&
      |         <errorStatus>OK</errorStatus>| &&
      |         <editStatus>CREATED</editStatus>| &&
      |         <index>1</index>| &&
      |         <message xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:nil="true" />| &&
      |       </objectEditResult>| &&
      |       <objectEditResult>| &&
      |         <id>USR-350622</id>| &&
      |         <errorStatus>OK</errorStatus>| &&
      |         <editStatus>UPDATED</editStatus>| &&
      |         <index>2</index>| &&
      |         <message xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:nil="true" />| &&
      |       </objectEditResult>| &&
      |       <objectEditResult>| &&
      |         <id>USR-350623</id>| &&
      |         <errorStatus>OK</errorStatus>| &&
      |         <editStatus>UPDATED</editStatus>| &&
      |         <index>3</index>| &&
      |         <message xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:nil="true" />| &&
      |       </objectEditResult>| &&
      |       <objectEditResult>| &&
      |         <id>USR-350624</id>| &&
      |         <errorStatus>OK</errorStatus>| &&
      |         <editStatus>UPDATED</editStatus>| &&
      |         <index>4</index>| &&
      |         <message xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:nil="true" />| &&
      |       </objectEditResult>| &&
      |       <objectEditResult>| &&
      |         <id>USR-350625</id>| &&
      |         <errorStatus>OK</errorStatus>| &&
      |         <editStatus>UPDATED</editStatus>| &&
      |         <index>4</index>| &&
      |         <message xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:nil="true" />| &&
      |       </objectEditResult>| &&
      |       <objectEditResult>| &&
      |         <id>USR-350626</id>| &&
      |         <errorStatus>OK</errorStatus>| &&
      |         <editStatus>UPDATED</editStatus>| &&
      |         <index>4</index>| &&
      |         <message xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:nil="true" />| &&
      |       </objectEditResult>| &&
      |       <objectEditResult>| &&
      |         <id>USR-350627</id>| &&
      |         <errorStatus>OK</errorStatus>| &&
      |         <editStatus>UPDATED</editStatus>| &&
      |         <index>4</index>| &&
      |         <message xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:nil="true" />| &&
      |       </objectEditResult>| &&
      |     </result>| &&
      |   </upsertResponse>| &&
      | </S:Body>| &&
      |</S:Envelope>|.
  endmethod.

endclass.
