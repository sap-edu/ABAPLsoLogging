class ltc_lso_log_trace definition deferred.
class zcl_lso_log_trace definition local friends ltc_lso_log_trace.

class ltc_lso_log_trace definition for testing
  duration short
  inheriting from zcl_lso_log_unit
  risk level harmless
  final.

  private section.
    constants begin of c_trace.
    constants id type zlso_log_trace-id value 'LTC_LOG_TRACE'.
    constants end of c_trace.

    class-data id_counter type i.

    data cut type ref to zcl_lso_log_trace.  "class under test

    class-methods class_setup.
    class-methods class_teardown.

    methods setup.
    methods teardown.

    methods create_id for testing.
    methods instantiate_without_id for testing.
    methods get_id for testing.
    methods get_http_status for testing.
    methods get_request_method for testing.
    methods get_request_url for testing.
    methods get_stripped_date for testing.
    methods has_payload for testing.
    methods has_request_payload for testing.
    methods has_response_payload for testing.
    methods has_request_headers for testing.
    methods has_response_headers for testing.
    methods is_stripped for testing.
    methods save_collection for testing raising zcx_lso_log.
    methods conv_table_to_json for testing.

    methods get_next_id
      returning value(id) type zlso_log_trace-id.

    methods assert_id
      importing id type zlso_log_trace-id.
endclass.


class ltc_lso_log_trace implementation.

  method class_setup.
    ltc_lso_log_trace=>id_counter = 0.
  endmethod.


  method class_teardown.
  endmethod.


  method setup.
    me->cut = me->create_trace( id               = c_trace-id
                                request_payload  = '{ "request":"payload" }'
                                response_payload = '{ "response":"payload" }' ).
  endmethod.


  method teardown.
    " Saved messages will be deleted with super->teardown( ) method.
  endmethod.


  method create_id.
    me->assert_id( me->cut->create_id( ) ).
  endmethod.


  method instantiate_without_id.
    data(trace) = new zcl_lso_log_trace( request_url      = conv #( c_trace_data-request_url )
                                         request_method   = conv #( c_trace_data-request_method )
                                         http_status      = conv #( c_trace_data-http_status )
                                         request_payload  = 'Id will be generated automatically!'
                                         response_payload = 'Is it ok?' ).

    me->assert_id( trace->get_id( ) ).
  endmethod.

  method get_id.
    cl_abap_unit_assert=>assert_equals( act = me->cut->get_id( ) exp = c_trace-id ).
  endmethod.


  method get_http_status.
    cl_abap_unit_assert=>assert_equals( act = me->cut->get_http_status( ) exp = c_trace_data-http_status ).
  endmethod.


  method get_request_method.
    cl_abap_unit_assert=>assert_equals( act = me->cut->get_request_method( ) exp = c_trace_data-request_method ).
  endmethod.


  method get_request_url.
    cl_abap_unit_assert=>assert_equals( act = me->cut->get_request_url( ) exp = c_trace_data-request_url ).
  endmethod.


  method get_stripped_date.
    me->cut->set_stripped_date( sy-datum ).

    cl_abap_unit_assert=>assert_equals( act = me->cut->zif_lso_log_trace~get_stripped_date( ) exp = sy-datum ).
  endmethod.


  method has_payload.
    cl_abap_unit_assert=>assert_true( me->cut->has_payload( ) ).
  endmethod.


  method has_request_headers.
    cl_abap_unit_assert=>assert_true( me->cut->has_request_headers( ) ).
  endmethod.


  method has_request_payload.
    cl_abap_unit_assert=>assert_true( me->cut->has_request_payload( ) ).
  endmethod.


  method has_response_headers.
    cl_abap_unit_assert=>assert_true( me->cut->has_response_headers( ) ).
  endmethod.


  method has_response_payload.
    cl_abap_unit_assert=>assert_true( me->cut->has_response_payload( ) ).
  endmethod.


  method is_stripped.
    me->cut->set_stripped_date( sy-datum ).

    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_trace~is_stripped( ) ).
  endmethod.


  method save_collection.
    data(traces) = new cl_object_collection( ).

    traces->add( me->cut ).
    traces->add( me->create_trace( id = me->get_next_id( ) ) ).
    traces->add( new zcl_lso_log_trace( request_url      = conv #( c_trace_data-request_url )
                                        request_method   = conv #( c_trace_data-request_method )
                                        http_status      = conv #( c_trace_data-http_status )
                                        request_payload  = 'Id will be generated automatically!'
                                        response_payload = 'Is it ok?' ) ).

    " Add traces object to be tear down then after test.
    me->add_traces_to_teardown( traces ).

    data(result) = zcl_lso_log_trace=>save_collection( traces ).

    " Were traces saved properly?
    cl_abap_unit_assert=>assert_true( result ).

    data(iterator) = traces->get_iterator( ).

    " Check if all trace objects are marked as 'saved'.
    while iterator->has_next( ) eq abap_true.
      data(trace) = cast zcl_lso_log_trace( iterator->get_next( ) ).

      cl_abap_unit_assert=>assert_true( trace->is_saved( ) ).
    endwhile.
  endmethod.


  method conv_table_to_json.

    types: begin of ts_banana,
             color type string,
             size  type string,
             taste type string,
           end of ts_banana.
    types: tt_bananas type standard table of ts_banana with default key.

    data(bananas) = value tt_bananas( ( color = |yellow| size = |big| taste = |yummy| )
                                      ( color = |brown|  size = |big| taste = |urghh| ) ).

    data(json_str) = zcl_lso_log_trace=>conv_table_to_json( input_tab = bananas ).
    cl_abap_unit_assert=>assert_not_initial( act = json_str ).

    data(banana) = value ts_banana( color = |yellow| size = |big| taste = |yummy| ).

    json_str = zcl_lso_log_trace=>conv_table_to_json( input_tab = value #( base bananas ( banana ) ) ).
    cl_abap_unit_assert=>assert_not_initial( act = json_str ).

    types: begin of ts_banana_container,
             box1 type tt_bananas,
             box2 type tt_bananas,
           end of ts_banana_container.
    types: tt_banana_containers type standard table of ts_banana_container with default key.
    data(banana_containers) = value tt_banana_containers( ).

    data(banana_container) = value ts_banana_container( box1 = bananas
                                                        box2 = bananas ).

    json_str = zcl_lso_log_trace=>conv_table_to_json( input_tab = value #( base banana_containers ( banana_container ) ) ).
    cl_abap_unit_assert=>assert_not_initial( act = json_str ).

  endmethod.


  method get_next_id.
    ltc_lso_log_trace=>id_counter = ltc_lso_log_trace=>id_counter + 1.

    " Generate next id not to use the range number for testing.
    id = |{ c_trace-id }_{ ltc_lso_log_trace=>id_counter }|.
  endmethod.


  method assert_id.
    " Is generated id matches the pattern: ILDNNNNNNNNNN?
    cl_abap_unit_assert=>assert_text_matches( pattern = |^{ sy-sysid }\\d+$| text = id ).
  endmethod.

endclass.
