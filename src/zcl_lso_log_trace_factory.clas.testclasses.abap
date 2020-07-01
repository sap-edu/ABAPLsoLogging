class ltc_lso_log_trace_factory definition for testing
  duration short
  risk level harmless.

  private section.
  ##TODO
*    class-data client type ref to if_http_client.
*    class-data rest_client type ref to cl_rest_http_client.

    class-methods class_setup.
    class-methods class_teardown.

    methods create_http_trace for testing.
    methods create_rest_trace for testing.
    methods create_from_data for testing raising cx_static_check.
endclass.


class ltc_lso_log_trace_factory implementation.

  method class_setup.
  ##TODO
*    try.
*        cl_http_client=>create_by_url(
*          exporting
*            url                = 'http://www.sap.com/trace_unit_test'
*            proxy_host         = 'proxy'
*            proxy_service      = '8080'
*          importing
*            client             = client
*          exceptions
*            argument_not_found = 1
*            plugin_not_active  = 2
*            internal_error     = 3 ).
*
*        if sy-subrc ne 0.
*          cl_abap_unit_assert=>fail( msg = 'Error building client' ).
*        endif.
*
*        rest_client = new cl_rest_http_client( client ).
*      catch cx_root into data(lo_cx).
*        cl_abap_unit_assert=>fail( msg = lo_cx->get_longtext( ) ).
*    endtry.
  endmethod.


  method class_teardown.
  endmethod.


  method create_http_trace.
  ##TODO
*    try.
*        client->refresh_request( ).
*        client->refresh_response( ).
*        client->refresh_cookie( ).
*        client->send( ).
*        client->receive( ).
*
*        data(trace) = zcl_lso_log_trace_factory=>create_http_trace( client ).
*
*        cl_abap_unit_assert=>assert_bound( trace ).
*        cl_abap_unit_assert=>assert_not_initial( trace->get_response_headers( )->get_headers( ) ).
*        cl_abap_unit_assert=>assert_not_initial( trace->get_response_payload( )->get_payload( ) ).
*
*        "Check if replacing parameters works as expected:
*        data(trace_params) = value zcl_lso_log_trace_factory=>ty_s_custom( skip_response_headers = abap_true
*                                                                           skip_response_payload = abap_true
*                                                                           method = 'TEST' ).
*
*        trace = zcl_lso_log_trace_factory=>create_http_trace( http_client      = client
*                                                              trace_parameters = trace_params ).
*
*        cl_abap_unit_assert=>assert_bound( trace ).
*        cl_abap_unit_assert=>assert_initial( trace->get_response_headers( ) ).
*        cl_abap_unit_assert=>assert_initial( trace->get_response_payload( ) ).
*        cl_abap_unit_assert=>assert_equals( act = trace->get_request_method( )
*                                            exp = 'TEST' ).
*      catch cx_root into data(root).
*        cl_abap_unit_assert=>fail( msg = root->get_longtext( ) ).
*    endtry.
  endmethod.


  method create_rest_trace.
##TODO
*    try.
*        client->refresh_request( ).
*        client->refresh_response( ).
*        client->refresh_cookie( ).
*        "Send request
*        rest_client->if_rest_resource~get( ).
*
*        data(trace) = zcl_lso_log_trace_factory=>create_rest_trace( rest_client ).
*
*        cl_abap_unit_assert=>assert_bound( trace ).
*        cl_abap_unit_assert=>assert_not_initial( trace->get_response_headers( )->get_headers( ) ).
*        cl_abap_unit_assert=>assert_not_initial( trace->get_response_payload( )->get_payload( ) ).
*
*        "Check if replacing parameters works as expected:
*        data(trace_params) = value zcl_lso_log_trace_factory=>ty_s_custom( skip_request_headers = abap_true
*                                                                           skip_request_payload = abap_true
*                                                                           method = 'TEST' ).
*
*        trace = zcl_lso_log_trace_factory=>create_rest_trace( rest_client      = rest_client
*                                                              trace_parameters = trace_params ).
*
*        cl_abap_unit_assert=>assert_bound( trace ).
*        cl_abap_unit_assert=>assert_initial( trace->get_request_headers( ) ).
*        cl_abap_unit_assert=>assert_initial( trace->get_request_payload( ) ).
*        cl_abap_unit_assert=>assert_equals( act = trace->get_request_method( )
*                                            exp = 'TEST' ).
*      catch cx_root into data(root).
*        cl_abap_unit_assert=>fail( msg = root->get_longtext( ) ).
*    endtry.
  endmethod.


  method create_from_data.
  ##TODO
*    types: begin of ts_fan,
*             name  type string,
*             since type datum,
*           end of ts_fan.
*
*    types: tt_fans type standard table of ts_fan with default key.
*
*    types: begin of ts_fruit,
*             fruit type string,
*             fans  type tt_fans,
*           end of ts_fruit.
*
*    types: tt_fruits type standard table of ts_fruit with default key.
*
*    data(fruits) = value tt_fruits( ( fruit = |Banana|
*                                      fans  = value #( ( name  = |Monkey|
*                                                         since = cl_abap_context_info=>get_system_date( ) )
*                                                       ( name  = |Joe|
*                                                         since = cl_abap_context_info=>get_system_date( ) ) ) ) ).
*    try.
*        " Test simple table
*        data(table_trace)   = zcl_lso_log_trace_factory=>create_from_data( fruits ).
*        data(table_payload) = table_trace->get_request_payload( )->get_payload( ).
*
*        loop at fruits assigning field-symbol(<fruit>).
*          cl_abap_unit_assert=>assert_true( cond #( when table_payload cs <fruit>-fans[ 1 ]-name then abap_true else abap_false ) ).
*        endloop.
*
*      catch cx_root into data(x).
*        cl_abap_unit_assert=>assert_equals( exp = |Pink Fluffy Unicorns|
*                                            act = x->get_longtext( ) ).
*    endtry.
  endmethod.

endclass.
