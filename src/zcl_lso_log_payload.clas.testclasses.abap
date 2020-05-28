class ltc_lso_log_payload definition deferred.
class zcl_lso_log_payload definition local friends ltc_lso_log_payload.

class ltc_lso_log_payload definition for testing
  duration short
     inheriting from zcl_lso_log_unit
     risk level harmless.

  private section.
    constants c_trace_id type zlso_log_payload-trace_id value 'LTC_LOG_PAYLOAD'.

    data cut type ref to zcl_lso_log_payload.  "class under test

    class-methods class_setup.
    class-methods class_teardown.

    methods setup.
    methods teardown.

    methods is_html for testing.
    methods is_json for testing.
    methods is_xml for testing.
    methods parse_payload for testing.
    methods get_payload for testing.
    methods get_payload_lines for testing.
    methods get_formatted_payload for testing.
    methods save_collection for testing raising zcx_lso_log.
    methods is_saml for testing raising cx_static_check.
endclass.       "lcl_Lso_Log_Payload_Unit


class ltc_lso_log_payload implementation.

  method class_setup.
  endmethod.


  method class_teardown.
  endmethod.


  method setup.
    me->cut = new #( trace_id = c_trace_id
                     type     = zif_lso_log_payload=>c_type-request
                     payload  = me->get_json_request_payload( ) ).
  endmethod.


  method teardown.
  endmethod.


  method save_collection.
    data(payloads) = new cl_object_collection( ).

    payloads->add( zcl_lso_log_payload=>create_request( trace_id = c_trace_id
                                                        payload  = me->get_json_request_payload( ) ) ).

    payloads->add( zcl_lso_log_payload=>create_response( trace_id = c_trace_id
                                                         payload  = me->get_xml_payload( ) ) ).

    " Add payloads to the buffer for further tear down.
    me->add_payloads_to_teardown( payloads ).

    data(result) = zcl_lso_log_payload=>save_collection( payloads ).

    " Were payloads save properly?
    cl_abap_unit_assert=>assert_true( result ).

    data(iterator) = payloads->get_iterator( ).

    " Check if all payload objects are marked as 'saved'.
    while iterator->has_next( ).
      data(lo_payload) = cast zcl_lso_log_payload( iterator->get_next( ) ).

      cl_abap_unit_assert=>assert_true( act = lo_payload->is_saved( ) ).
    endwhile.
  endmethod.

  method is_saml.

    data(payload) = zcl_lso_log_payload=>create_request( trace_id = c_trace_id
                                                       payload  = me->get_saml_payload( ) ).

    cl_abap_unit_assert=>assert_true( payload->is_saml( ) ).

  endmethod.


  method is_html.
    data(payload) = zcl_lso_log_payload=>create_request( trace_id = c_trace_id
                                                         payload  = me->get_html_payload( ) ).

    cl_abap_unit_assert=>assert_true( payload->is_html( ) ).
  endmethod.


  method is_json.
    cl_abap_unit_assert=>assert_true( me->cut->is_json( ) ).
  endmethod.


  method is_xml.
    data(payload) = zcl_lso_log_payload=>create_request( trace_id = c_trace_id
                                                         payload  = me->get_xml_payload( ) ).

    cl_abap_unit_assert=>assert_true( payload->is_xml( ) ).
  endmethod.


  method parse_payload.
    data(lo_payload) = zcl_lso_log_payload=>create_request( trace_id = c_trace_id
                                                            payload  = me->get_sensitive_payload( ) ).

    " Check if test payload contains the test-password
    cl_abap_unit_assert=>assert_true( act = boolc( find( val = me->get_sensitive_payload( )  sub = me->c_password  ) > -1 ) ).

    " Test parsing sensitive Payload
    data(lv_cleaned_payload) = lo_payload->parse_payload( me->get_sensitive_payload( ) ).

    " Check if the parsing removed the password
    cl_abap_unit_assert=>assert_true( act = boolc( find( val = lv_cleaned_payload  sub = me->c_password ) = -1  ) ).

    lo_payload = zcl_lso_log_payload=>create_request( trace_id = c_trace_id
                                                      payload  = me->get_json_sensitive_payload( ) ).

    " Check if test payload contains the test-password
    cl_abap_unit_assert=>assert_true( act = boolc( find( val = me->get_json_sensitive_payload( )  sub = me->c_password  ) > -1 ) ).

    " Test parsing sensitive Payload
    lv_cleaned_payload = lo_payload->parse_payload( me->get_json_sensitive_payload( ) ).

    " Check if the parsing removed the password
    cl_abap_unit_assert=>assert_true( act = boolc( find( val = lv_cleaned_payload  sub = me->c_password ) = -1  ) ).
  endmethod.


  method get_formatted_payload.
    " JSON
    data(payload) = zcl_lso_log_payload=>create_request( trace_id = c_trace_id
                                                         payload  = me->get_json_request_payload( ) ).

    data(formatted_payload) = payload->zif_lso_log_payload~get_formatted_payload( ).

    cl_abap_unit_assert=>assert_not_initial( formatted_payload ).

    " XML
    payload = zcl_lso_log_payload=>create_request( trace_id = c_trace_id
                                                   payload  = me->get_xml_payload( ) ).

    formatted_payload = payload->zif_lso_log_payload~get_formatted_payload( ).

    cl_abap_unit_assert=>assert_not_initial( formatted_payload ).

    " HTML
    payload = zcl_lso_log_payload=>create_request( trace_id = c_trace_id
                                                   payload  = me->get_html_payload( ) ).

    formatted_payload = payload->zif_lso_log_payload~get_formatted_payload( ).

    cl_abap_unit_assert=>assert_not_initial( formatted_payload ).
  endmethod.


  method get_payload.
    cl_abap_unit_assert=>assert_equals( act = me->cut->zif_lso_log_payload~get_payload( )
                                        exp = me->get_json_request_payload( ) ).
  endmethod.


  method get_payload_lines.
    data(payload_lines) = me->cut->zif_lso_log_payload~get_payload_lines( ).

    cl_abap_unit_assert=>assert_not_initial( payload_lines ).
    cl_abap_unit_assert=>assert_equals( act = lines( payload_lines ) exp = 3 ).
  endmethod.

endclass.
