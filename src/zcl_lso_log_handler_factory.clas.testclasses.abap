*"* use this source file for your ABAP unit test classes
class lcl_loggable definition for testing
  create public.

  public section.
    interfaces zif_lso_loggable_object.

    methods constructor
      importing key type string.

  private section.
    data key type string.
endclass.

class lcl_loggable implementation.

  method constructor.
    me->key = key.
  endmethod.


  method zif_lso_loggable_object~get_key.
    rv_key = me->key.
  endmethod.

endclass.


class ltc_lso_log_handler_factory definition final for testing
  duration short
  risk level harmless.

  private section.
    constants c_key1 type string value 'Key1'.
    constants c_key2 type string value 'Key2'.
    constants c_key3 type string value 'Key3'.

    data cut type ref to zif_lso_log_handler_factory.

    class-methods class_setup.
    class-methods class_teardown.

    methods setup.
    methods teardown.

    methods get_dummy_key for testing.
    methods create_map_by_keys for testing.
    methods create_map_by_loggable for testing raising cx_sy_move_cast_error.
    methods create for testing raising cx_static_check.
endclass.


class ltc_lso_log_handler_factory implementation.

  method class_setup.
  endmethod.


  method class_teardown.
*   Rollback is executed automatically...
  endmethod.


  method setup.
    me->cut = zcl_lso_log_handler_factory=>instance( ).
  endmethod.


  method teardown.
  endmethod.


  method get_dummy_key.
    cl_abap_unit_assert=>assert_not_initial( me->cut->get_dummy_key( ) ).
  endmethod.


  method create_map_by_keys.
    data(keys) = value string_table( ( c_key1 ) ( c_key2 ) ( c_key3 ) ).

    data(map) = me->cut->create_map_by_keys( keys ).

    " Map created successfully?
    cl_abap_unit_assert=>assert_bound( map ).

    " Are all log handlers for keys created (including dummy one)?
    cl_abap_unit_assert=>assert_equals( act = map->size( ) exp = 4 ).

    " All log handlers with keys created as expected?
    cl_abap_unit_assert=>assert_bound( cast zcl_lso_log_handler( map->if_object_map~get( c_key1 ) ) ).
    cl_abap_unit_assert=>assert_bound( cast zcl_lso_log_handler( map->if_object_map~get( c_key2 ) ) ).
    cl_abap_unit_assert=>assert_bound( cast zcl_lso_log_handler( map->if_object_map~get( c_key3 ) ) ).

    " Dummy log handler created as well?
    cl_abap_unit_assert=>assert_bound( cast zcl_lso_log_handler( map->if_object_map~get( me->cut->get_dummy_key( ) ) ) ).
  endmethod.


  method create_map_by_loggable.
    data(loggable1) = new lcl_loggable( c_key1 ).
    data(loggable2) = new lcl_loggable( c_key2 ).

    data(collection) = new cl_object_collection( ).
    collection->add( loggable1 ).
    collection->add( loggable2 ).

    data(map) = me->cut->create_map_by_loggable( collection ).

    " Map created successfully?
    cl_abap_unit_assert=>assert_bound( map ).

    " Are all log handlers for keys created (including dummy one)?
    cl_abap_unit_assert=>assert_equals( act = map->size( ) exp = 3 ).

    " All log handlers with keys created as expected?
    cl_abap_unit_assert=>assert_bound( cast zcl_lso_log_handler( map->if_object_map~get( c_key1 ) ) ).
    cl_abap_unit_assert=>assert_bound( cast zcl_lso_log_handler( map->if_object_map~get( c_key2 ) ) ).

    " Dummy log handler created as well?
    cl_abap_unit_assert=>assert_bound( cast zcl_lso_log_handler( map->if_object_map~get( me->cut->get_dummy_key( ) ) ) ).

  endmethod.

  method create.

    " Given we create a log handler from exception
    data(log_handler_1) = cut->create( new zcx_lso_log( ) ).

    " Then It's not empty
    cl_abap_unit_assert=>assert_equals( act = lines( log_handler_1->get_symsgs( ) )
                                        exp = 1
                                        msg = |Creation did not add exception| ).


    " Given we create a log handler from a message
    data(log_handler_2) = cut->create( new zcl_lso_log_message( msgid = zcx_lso_log=>zcx_lso_log-msgid
                                                                msgno = zcx_lso_log=>zcx_lso_log-msgno
                                                                msgty = zif_lso_log_message=>c_type-error ) ).

    " Then It's not empty
    cl_abap_unit_assert=>assert_equals( act = lines( log_handler_2->get_symsgs( ) )
                                        exp = 1
                                        msg = |Creation did not add zcl_lso_log_message| ).


    " Given we create a log handler from a message
    data(log_handler_3) = cut->create( value symsg( msgid = zcx_lso_log=>zcx_lso_log-msgid
                                                    msgno = zcx_lso_log=>zcx_lso_log-msgno
                                                    msgty = zif_lso_log_message=>c_type-error ) ).

    " Then It's not empty
    cl_abap_unit_assert=>assert_equals( act = lines( log_handler_3->get_symsgs( ) )
                                        exp = 1
                                        msg = |Creation did not add System Message structure| ).



  endmethod.

endclass.
