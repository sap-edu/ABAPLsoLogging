class ltc_lso_log_payload definition deferred.
class zcl_lso_log_payload definition local friends ltc_lso_log_payload.

class ltc_lso_log_payload definition for testing duration short
     inheriting from zcu_lso_log
     risk level harmless.

  private section.
    constants c_trace_id type zlso_log_payload-trace_id value 'LTC_LOG_PAYLOAD'.

    data cut type ref to zcl_lso_log_payload.  "class under test

    methods setup.
    methods teardown.

    methods is_html for testing.
    methods is_json for testing.
    methods is_xml for testing.
    methods parse_payload for testing.
    methods get_payload for testing.
    methods get_payload_lines for testing.
    methods get_formatted_payload for testing.
    methods get_object for testing.
    methods is_saml for testing raising cx_static_check.
    methods save_collection for testing raising cx_static_check.
endclass.


class ltc_lso_log_payload implementation.

  method setup.
    me->cut = new #( trace_id = c_trace_id
                     type     = zif_lso_log_payload=>c_type-request
                     payload  = me->get_json_request_payload( ) ).
  endmethod.


  method teardown.
  endmethod.


  method save_collection.
    data(payloads) = value zlso_tt_log_payloads( ( trace_id = c_trace_id
                                                   type     = zif_lso_log_payload=>c_type-request
                                                   instance = zcl_lso_log_payload=>create_request( trace_id = c_trace_id
                                                                                                   payload  = me->get_json_request_payload( ) ) )
                                                 ( trace_id = c_trace_id
                                                   type     = zif_lso_log_payload=>c_type-response
                                                   instance = zcl_lso_log_payload=>create_response( trace_id = c_trace_id
                                                                                                    payload  = me->get_xml_payload( ) ) ) ).
    " Add payload's to the buffer for further tear down.
    me->add_payloads_to_teardown( payloads ).

    data(result) = zcl_lso_log_payload=>save_collection( payloads ).

    " Were payload's save properly?
    cl_abap_unit_assert=>assert_true( result ).

    loop at payloads using key object_key reference into data(payload_object).
      " Check if all payload objects are marked as 'saved'.
      data(payload) = cast zcl_lso_log_payload( payload_object->instance ).

      cl_abap_unit_assert=>assert_true( payload->is_saved( ) ).
    endloop.
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
    data(payload) = zcl_lso_log_payload=>create_request( trace_id = c_trace_id
                                                         payload  = me->get_sensitive_payload( ) ).

    " Check if test payload contains the test-password
    cl_abap_unit_assert=>assert_true( act = boolc( find( val = me->get_sensitive_payload( )  sub = me->c_password  ) > -1 ) ).

    " Test parsing sensitive Payload
    data(lv_cleaned_payload) = payload->parse_payload( me->get_sensitive_payload( ) ).

    " Check if the parsing removed the password
    cl_abap_unit_assert=>assert_true( act = boolc( find( val = lv_cleaned_payload  sub = me->c_password ) = -1  ) ).

    payload = zcl_lso_log_payload=>create_request( trace_id = c_trace_id
                                                      payload  = me->get_json_sensitive_payload( ) ).

    " Check if test payload contains the test-password
    cl_abap_unit_assert=>assert_true( act = boolc( find( val = me->get_json_sensitive_payload( )  sub = me->c_password  ) > -1 ) ).

    " Test parsing sensitive Payload
    lv_cleaned_payload = payload->parse_payload( me->get_json_sensitive_payload( ) ).

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


  method get_object.
    data(exp) = value zlso_s_log_payload( trace_id = me->cut->trace_id
                                          type     = me->cut->type
                                          instance = me->cut ).

    cl_abap_unit_assert=>assert_equals( act = me->cut->get_object( ) exp = exp ).
  endmethod.

endclass.
