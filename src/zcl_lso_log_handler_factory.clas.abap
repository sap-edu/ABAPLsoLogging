class zcl_lso_log_handler_factory definition
  public
  final
  create private .

  public section.
    interfaces zif_lso_log_handler_factory.

    class-methods instance
      returning value(factory) type ref to zif_lso_log_handler_factory.

  protected section.
  private section.
    class-data self type ref to zcl_lso_log_handler_factory.

    data dummy_key type sysuuid_c22 .

    methods create_dummy_key
      returning value(dummy_key) type sysuuid_c22.

    methods is_exception
      importing
        object              type any
      returning
        value(is_exception) type abap_bool.

    methods is_message
      importing
        object            type any
      returning
        value(is_message) type abap_bool.

    methods is_symsg
      importing
        object          type any
      returning
        value(is_symsg) type abap_bool.


endclass.


class zcl_lso_log_handler_factory implementation.

  method instance.
    if zcl_lso_log_handler_factory=>self is not bound.
      zcl_lso_log_handler_factory=>self = new zcl_lso_log_handler_factory( ).
    endif.

    factory = zcl_lso_log_handler_factory=>self.
  endmethod.


  method zif_lso_log_handler_factory~create_map_by_loggable.
    " !IMPORTANT - objects passed in collection need to implement ZIF_LSO_LOGGABLE_OBJECT interface!
    " Otherwise exception is raised!
    " The ZIF_LSO_LOGGABLE_OBJECT interface forces objects to implement special GET_KEY method.
    " This key is then a key for and objects map.
    data(keys) = value string_table( ).

    data(iterator) = collection->get_iterator( ).

    while iterator->has_next( ).
      insert cast zif_lso_loggable_object( iterator->get_next( ) )->get_key( ) into table keys.
    endwhile.

    map = me->zif_lso_log_handler_factory~create_map_by_keys( keys ).
  endmethod.


  method zif_lso_log_handler_factory~create_map_by_keys.
    map = new zcl_lso_object_map( ).

    " Add dummy key relevant for some key less objects.
    map->put( key   = me->zif_lso_log_handler_factory~get_dummy_key( )
              value = new zcl_lso_log_handler( ) ).

    loop at keys assigning field-symbol(<key>).
      " Instantiate log handler for each key and add it to the objects map.
      map->put( key   = <key>
                value = new zcl_lso_log_handler( ) ).
    endloop.
  endmethod.


  method zif_lso_log_handler_factory~get_dummy_key.
    if me->dummy_key is initial.
      me->dummy_key = me->create_dummy_key( ).
    endif.

    dummy_key = me->dummy_key.
  endmethod.


  method create_dummy_key.
    try.
        dummy_key = to_upper( cl_system_uuid=>if_system_uuid_static~create_uuid_c22( ) ).
      catch cx_uuid_error.
        wait up to 1 seconds.
        retry.
    endtry.
  endmethod.


  method zif_lso_log_handler_factory~create.

    log_handler = new zcl_lso_log_handler( ).

    case abap_true.
      when is_exception( object ). log_handler->exception( object ).
      when is_message( object ).   log_handler->add_message( object ).
      when is_symsg( object ).     log_handler->add_symsg( object ).
    endcase.

  endmethod.

  method is_symsg.

    try.
        is_symsg = abap_false.

        if cl_abap_typedescr=>describe_by_data( object )->get_relative_name( ) = |SYMSG|.
          is_symsg = abap_true.
        endif.

      catch cx_root.
        "Not a message class type
    endtry.

  endmethod.

  method is_message.

    try.
        is_message = abap_false.

        data(class_name) = cl_abap_classdescr=>get_class_name( object ).
        if class_name = |\\CLASS=ZCL_LSO_LOG_MESSAGE|.
          is_message = abap_true.
        endif.

      catch cx_root.
        "Not a message class type
    endtry.

  endmethod.


  method is_exception.

    try.

        is_exception = abap_false.

        data: objectdescr type ref to cl_abap_objectdescr,
              interfaces  type abap_intfdescr_tab.

        objectdescr ?= cl_abap_typedescr=>describe_by_object_ref( object ).
        interfaces = objectdescr->interfaces.

        if line_exists( interfaces[ name = |IF_T100_MESSAGE| ] ).
          is_exception = abap_true.
        endif.

      catch cx_root.
        " Not an exception type
    endtry.

  endmethod.

endclass.
