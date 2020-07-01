class ltc_lso_log_trace definition deferred.
class zcl_lso_log_trace definition local friends ltc_lso_log_trace.

class ltc_lso_log_trace definition for testing
  duration short
  inheriting from zcu_lso_log
  risk level harmless
  final.

  private section.
    constants begin of c_trace.
    constants id type zlso_log_trace-id value 'LTC_LOG_TRACE'.
    constants end of c_trace.

    class-data id_counter type i.

    data cut type ref to zcl_lso_log_trace.

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
    methods get_object for testing.
    methods has_payload for testing.
    methods has_request_payload for testing.
    methods has_response_payload for testing.
    methods has_request_headers for testing.
    methods has_response_headers for testing.
    methods is_stripped for testing.
    methods save_collection for testing raising cx_static_check.

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

    cl_abap_unit_assert=>assert_bound( me->cut ).
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
    me->cut->set_stripped_date( cl_abap_context_info=>get_system_date( ) ).

    cl_abap_unit_assert=>assert_equals( act = me->cut->zif_lso_log_trace~get_stripped_date( )
                                        exp = cl_abap_context_info=>get_system_date( ) ).
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
    me->cut->set_stripped_date( cl_abap_context_info=>get_system_date( ) ).

    cl_abap_unit_assert=>assert_true( me->cut->zif_lso_log_trace~is_stripped( ) ).
  endmethod.


  method save_collection.
    data(traces) = value zlso_tt_log_traces( ( me->cut->get_object( ) )
                                             ( me->create_trace( me->get_next_id( ) )->get_object( ) )
                                             ( new zcl_lso_log_trace( request_url      = conv #( c_trace_data-request_url )
                                                                      request_method   = conv #( c_trace_data-request_method )
                                                                      http_status      = conv #( c_trace_data-http_status )
                                                                      request_payload  = 'Id will be generated automatically!'
                                                                      response_payload = 'Is it ok?' )->get_object( ) ) ).

    " Add traces object to be tear down then after test.
    me->add_traces_to_teardown( traces ).

    data(result) = zcl_lso_log_trace=>save_collection( traces ).

    " Were traces saved properly?
    cl_abap_unit_assert=>assert_true( result ).

    loop at traces using key object_key reference into data(object).
      data(trace) = cast zcl_lso_log_trace( object->instance ).

      " Check if all trace objects are marked as 'saved'.
      cl_abap_unit_assert=>assert_true( trace->is_saved( ) ).
    endloop.
  endmethod.


  method get_next_id.
    ltc_lso_log_trace=>id_counter = ltc_lso_log_trace=>id_counter + 1.

    " Generate next id not to use the range number for testing.
    id = |{ c_trace-id }_{ ltc_lso_log_trace=>id_counter }|.
  endmethod.


  method assert_id.
    " Is generated id matches the pattern: NNNNNNNNNN?
    cl_abap_unit_assert=>assert_text_matches( pattern = |^\\d+$| text = id ).
  endmethod.


  method get_object.
    cl_abap_unit_assert=>assert_equals( act = me->cut->get_object( ) exp = value zlso_s_log_trace( id       = me->cut->id
                                                                                                   instance = me->cut ) ).
  endmethod.

endclass.
