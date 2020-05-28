class lcl_lso_log_headers_unit definition deferred.
class zcl_lso_log_headers definition local friends lcl_lso_log_headers_unit.

class lcl_lso_log_headers_unit definition for testing
  duration short
     inheriting from zcl_lso_log_unit  risk level harmless
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lcl_lso_log_headers_Unit
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>Zcl_lso_log_headers
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  private section.
    constants c_unit_trace_id type zlso_log_headers-trace_id value 'LCL_LOG_HEADERS_UNIT'.

    data f_cut type ref to zcl_lso_log_headers.  "class under test

    class-methods class_setup.
    class-methods class_teardown.
    methods setup.
    methods teardown.
    methods save_collection for testing.
endclass.       "lcl_lso_log_headers_Unit


class lcl_lso_log_headers_unit implementation.

  method class_setup.
    delete from zlso_log_headers
      where trace_id eq @c_unit_trace_id.

    commit work.
  endmethod.


  method class_teardown.
  endmethod.


  method setup.
    create object f_cut
      exporting
        trace_id = c_unit_trace_id
        type     = zcl_lso_log_headers=>c_type-request
        headers  = me->get_request_headers( ).
  endmethod.


  method teardown.
  endmethod.


  method save_collection.
    data(lv_result) = abap_true.

    data(lo_headers) = new cl_object_collection( ).

    lo_headers->add(
      zcl_lso_log_headers=>create_request(
        trace_id = c_unit_trace_id
        headers  = me->get_request_headers( ) ) ).

    lo_headers->add(
      zcl_lso_log_headers=>create_response(
        trace_id = c_unit_trace_id
        headers  = me->get_request_headers( ) ) ).

    try.
        lv_result = zcl_lso_log_headers=>save_collection( lo_headers ).
      catch zcx_lso_log .
        lv_result = abap_false.
    endtry.

    cl_abap_unit_assert=>assert_true( act = lv_result ).

    data(lo_iterator) = lo_headers->get_iterator( ).

*   Check if all headers objects are marked as 'saved'.
    while lo_iterator->has_next( ) eq abap_true.
      data(lo_header) = cast zcl_lso_log_headers( lo_iterator->get_next( ) ).

      cl_abap_unit_assert=>assert_true( act = lo_header->is_saved( ) ).
    endwhile.
  endmethod.

endclass.
