*"* use this source file for your ABAP unit test classes
class ltc_lso_trace_display definition deferred.
class zcl_lso_trace_display definition local friends ltc_lso_trace_display.

class ltc_lso_trace_display definition final for testing
  duration short
  risk level harmless.

  private section.

    data: cut type ref to zcl_lso_trace_display.

    class-methods: class_setup.
    class-methods: class_teardown.

    methods:  setup.
    methods:  teardown.
    methods:  check_     for testing raising cx_static_check.

endclass.


class ltc_lso_trace_display implementation.

  method class_setup.
  endmethod.

  method setup.
    cut = new zcl_lso_trace_display( ).
  endmethod.

  method check_.

    try.










      catch cx_root into data(lx_root).
        cl_abap_unit_assert=>fail( lx_root->get_longtext( ) ).
    endtry.

  endmethod.

  method teardown.
  endmethod.

  method class_teardown.
  endmethod.

endclass.
