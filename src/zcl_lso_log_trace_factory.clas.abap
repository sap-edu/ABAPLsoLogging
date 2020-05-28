class zcl_lso_log_trace_factory definition
  public
  final
  create private .

  public section.
    types:
      begin of ty_s_custom .
    types skip_request_headers type abap_bool.
    types skip_response_headers type abap_bool.
    types skip_request_payload type abap_bool.
    types skip_response_payload type abap_bool.
    types url type string.
    types method type string.
    types status type i.
    types request_payload type string.
    types response_payload type string.
    types request_headers type tihttpnvp.
    types response_headers type tihttpnvp.
    types end of ty_s_custom .

    types tt_r_id type range of zlso_log_trace-id.

    class-methods create
      importing
        id               type zlso_log_trace-id
        request_url      type string
        request_method   type string
        http_status      type i
        payloads         type zlso_tt_log_trace_payloads
        request_headers  type tihttpnvp optional
        response_headers type tihttpnvp optional
        stripped_date    type zlso_log_trace-stripped_date optional
      returning
        value(trace)     type ref to zcl_lso_log_trace .

    class-methods create_http_trace
      importing
        !http_client      type ref to if_http_client
        !trace_parameters type ty_s_custom optional
      returning
        value(trace)      type ref to zcl_lso_log_trace .

    class-methods create_rest_trace
      importing
        !rest_client      type ref to if_rest_client
        !trace_parameters type ty_s_custom optional
      returning
        value(trace)      type ref to zcl_lso_log_trace .

    class-methods create_trace
      importing
        !trace_parameters type ty_s_custom
      returning
        value(trace)      type ref to zcl_lso_log_trace .

    class-methods create_map_by_ids
      importing ids            type tt_r_id
      returning value(objects) type ref to zcl_lso_object_map.

    class-methods create_from_data
      importing
        object       type data
      returning
        value(trace) type ref to zcl_lso_log_trace.

  protected section.
  private section.
endclass.


class zcl_lso_log_trace_factory implementation.

  method create.
    data(request_payload) = value string_table( ).
    data(response_payload) = value string_table( ).

    loop at payloads assigning field-symbol(<payload>).
      case <payload>-type.
        when zif_lso_log_payload=>c_type-request.
          insert conv #( <payload>-payload ) into table request_payload.

        when zif_lso_log_payload=>c_type-response.
          insert conv #( <payload>-payload ) into table response_payload.

        when others.
          continue.
      endcase.
    endloop.

    trace = new zcl_lso_log_trace( id               = id
                                   request_url      = request_url
                                   request_method   = request_method
                                   http_status      = http_status
                                   request_payload  = concat_lines_of( table = request_payload  sep = '' )
                                   response_payload = concat_lines_of( table = response_payload sep = '' )
                                   request_headers  = request_headers
                                   response_headers = response_headers ).

    trace->set_stripped_date( stripped_date ).
  endmethod.


  method create_http_trace.
    "copy parameters structure
    data(trace_param) = trace_parameters.

    "Setup trace data
    if trace_param-request_headers is initial and trace_param-skip_request_headers eq abap_false.
      "Request headers were requested from client:
      http_client->request->get_header_fields( changing fields = trace_param-request_headers ).
    endif.

    if trace_param-response_headers is initial and trace_param-skip_response_headers eq abap_false.
      "Response headers were requested from client:
      http_client->response->get_header_fields( changing fields = trace_param-response_headers ).
    endif.

    if trace_param-request_payload is initial and trace_param-skip_request_payload eq abap_false.
      "Request payload was requested from client:
      trace_param-request_payload = http_client->request->get_cdata( ).
    endif.

    if trace_param-response_payload is initial and trace_param-skip_response_payload eq abap_false.
      "Response payload was requested from client:
      trace_param-response_payload = http_client->response->get_cdata( ).
    endif.

    if trace_param-status is initial.
      "Get HTTP Status from client:
      http_client->response->get_status( importing code = trace_param-status ).
    endif.

    if trace_param-method is initial.
      "Get HTTP method from client:
      trace_param-method = http_client->request->get_method( ).
    endif.

    if trace_param-url is initial.
      "Get URL from client:
      trace_param-url = http_client->create_abs_url( ).
    endif.

    "Create trace object
    trace = zcl_lso_log_trace_factory=>create_trace( trace_param ).
  endmethod.


  method create_rest_trace.
    "Create entities:
    data(request_entity) = rest_client->create_request_entity( ).
    data(response_entity) = rest_client->get_response_entity( ).

    "copy parameters structure
    data(trace_param) = trace_parameters.

    "Setup trace data
    if trace_param-request_headers is initial and trace_param-skip_request_headers eq abap_false.
      "Request headers were requested from client:
      trace_param-request_headers = request_entity->get_header_fields( ).
    endif.

    if trace_param-response_headers is initial and trace_param-skip_response_headers eq abap_false.
      "Response headers were requested from client:
      trace_param-response_headers = response_entity->get_header_fields( ).
    endif.

    if trace_param-request_payload is initial and trace_param-skip_request_payload eq abap_false.
      "Request payload was requested from client:
      trace_param-request_payload = request_entity->get_string_data( ).
    endif.

    if trace_param-response_payload is initial and trace_param-skip_response_payload eq abap_false.
      "Response payload was requested from client:
      trace_param-response_payload = response_entity->get_string_data( ).
    endif.

    if trace_param-status is initial.
      "Get HTTP Status from client:
      trace_param-status = rest_client->get_status( ).
    endif.

    if trace_param-method is initial.
      "Get HTTP method from client:
      trace_param-method = request_entity->get_header_field( '~request_method' ).
    endif.

    if trace_param-url is initial.
      "Get URL from client:
      trace_param-url = cast cl_rest_http_client( rest_client )->get_http_client( )->create_abs_url( ).
    endif.

    "Create trace object
    trace = zcl_lso_log_trace_factory=>create_trace( trace_param ).
  endmethod.


  method create_trace.
    "do initial setup based on control parameters
    data(trace_param) = trace_parameters.

    if trace_param-skip_request_headers eq abap_true.
      clear trace_param-request_headers.
    endif.

    if trace_param-skip_response_headers eq abap_true.
      clear trace_param-response_headers.
    endif.

    if trace_param-skip_request_payload eq abap_true.
      clear trace_param-request_payload.
    endif.

    if trace_param-skip_response_payload eq abap_true.
      clear trace_param-response_payload.
    endif.

    "Instantiate Trace object.
    trace = new zcl_lso_log_trace( request_url       = trace_param-url
                                   request_method    = trace_param-method
                                   http_status       = trace_param-status
                                   request_payload   = trace_param-request_payload
                                   response_payload  = trace_param-response_payload
                                   request_headers   = trace_param-request_headers
                                   response_headers  = trace_param-response_headers ).
  endmethod.


  method create_map_by_ids.
    types tt_traces type sorted table of zlso_log_trace with non-unique key primary_key components id.
    types tt_payloads type sorted table of zlso_log_payload with non-unique key primary_key components trace_id type seqnr.
    types tt_headers type sorted table of zlso_log_headers with non-unique key primary_key components trace_id type name.

    field-symbols <table> type standard table.

    data(traces) = value tt_traces( ).
    data(payloads) = value tt_payloads( ).
    data(headers) = value tt_headers( ).

    if ids[] is not initial.
      " Split ids range table into smaller chunks to have it safely used in OpenSQL update statement.
      loop at new zcl_lso_log_range( )->split_table_for_opensql_in( ids ) into data(ids_split).
        assign ids_split->* to <table>.

        " Select traces from DB
        select id, http_status, request_url, request_method, stripped_date
          from zlso_log_trace
          appending corresponding fields of table @traces
          where id in @<table>
            order by primary key.

        " Select payloads from DB.
        select trace_id, type, seqnr, payload
          from zlso_log_payload
          appending corresponding fields of table @payloads
         where trace_id in @<table>
          order by primary key.

        " Select headers from DB.
        select trace_id, type, name, value
          from zlso_log_headers
          appending corresponding fields of table @headers
         where trace_id in @<table>
          order by primary key.
      endloop.

      " Make tables unique...
      delete adjacent duplicates from traces.
      delete adjacent duplicates from payloads.
      delete adjacent duplicates from headers.
    endif.

    objects = new #( ).

    loop at traces assigning field-symbol(<trace>).
      if not objects->contains_key( <trace>-id ).
        " Get payload for given trace id.
        data(trace_payload) = value zlso_tt_log_trace_payloads( for wa in payloads using key primary_key where ( trace_id = <trace>-id )
                                                                                                               ( corresponding #( wa ) ) ).

        " Get requests headers for given trace id.
        data(request_headers) = value tihttpnvp( for wah in headers using key primary_key where ( trace_id = <trace>-id and type = zcl_lso_log_headers=>c_type-request )
                                                                                                ( name = wah-name value = wah-value ) ).

        " Get response headers for given trace id.
        data(response_headers) = value tihttpnvp( for wah in headers using key primary_key where ( trace_id = <trace>-id and type = zcl_lso_log_headers=>c_type-response )
                                                                                                 ( name = wah-name value = wah-value ) ).

        " Add trace object to map.
        objects->put( key   = <trace>-id
                      value = zcl_lso_log_trace_factory=>create( id               = <trace>-id
                                                                 request_url      = conv #( <trace>-request_url )
                                                                 request_method   = conv #( <trace>-request_method )
                                                                 http_status      = conv #( <trace>-http_status )
                                                                 payloads         = trace_payload
                                                                 request_headers  = request_headers
                                                                 response_headers = response_headers
                                                                 stripped_date    = <trace>-stripped_date ) ).
      endif.

    endloop.

  endmethod.

  method create_from_data.

     data(json) = /ui2/cl_json=>serialize( data        = object
                                           compress    = abap_true
                                           pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

     trace = new zcl_lso_log_trace( http_status      = 0
                                    request_url      = |data|
                                    request_method   = |none|
                                    request_payload  = json
                                    response_payload = space ).

  endmethod.

endclass.
