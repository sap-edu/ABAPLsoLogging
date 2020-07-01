class lcl_lso_log_headers_unit definition deferred.
class zcl_lso_log_headers definition local friends lcl_lso_log_headers_unit.

class lcl_lso_log_headers_unit definition for testing duration short
     inheriting from zcu_lso_log risk level harmless.

  private section.
    constants c_unit_trace_id type zlso_log_headers-trace_id value 'LCL_LOG_HEADERS_UNIT'.

    data cut type ref to zcl_lso_log_headers.

    methods setup.
    methods teardown.
    methods save_collection for testing raising cx_static_check.
    methods get_object for testing.
endclass.       "lcl_lso_log_headers_Unit


class lcl_lso_log_headers_unit implementation.

  method setup.
    me->cut = new #( trace_id = c_unit_trace_id
                     type     = zcl_lso_log_headers=>c_type-request
                     headers  = me->get_request_headers( ) ).
  endmethod.


  method teardown.
  endmethod.


  method save_collection.
    data(headers) = value zlso_tt_log_headers( ( trace_id = c_unit_trace_id
                                                 type     = zcl_lso_log_headers=>c_type-request
                                                 instance = zcl_lso_log_headers=>create_request( trace_id = c_unit_trace_id
                                                                                                 headers  = me->get_request_headers( ) ) )
                                               ( trace_id = c_unit_trace_id
                                                 type     = zcl_lso_log_headers=>c_type-response
                                                 instance = zcl_lso_log_headers=>create_response( trace_id = c_unit_trace_id
                                                                                                  headers  = me->get_request_headers( ) ) ) ).

    data(result) = zcl_lso_log_headers=>save_collection( headers ).

    cl_abap_unit_assert=>assert_true( result ).

    loop at headers using key object_key reference into data(object).
      " Check if all headers objects are marked as 'saved'.
      cl_abap_unit_assert=>assert_true( object->instance->is_saved( ) ).
    endloop.
  endmethod.


  method get_object.
    cl_abap_unit_assert=>assert_equals( act = me->cut->get_object( )
                                        exp = value zlso_s_log_header( trace_id = me->cut->trace_id
                                                                       type     = me->cut->type
                                                                       instance = me->cut ) ).
  endmethod.

endclass.
