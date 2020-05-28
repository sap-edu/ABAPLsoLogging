class zcl_lso_object_map definition
  public
  create public .

*"* public components of class ZCL_LSO_OBJECT_MAP
*"* do not include other source files here!!!
  public section.

    interfaces if_object_map .

    aliases contains_key     for if_object_map~contains_key .
    aliases contains_value   for if_object_map~contains_value .
    aliases get              for if_object_map~get .
    aliases get_by_position  for if_object_map~get_by_position .
    aliases get_keys         for if_object_map~get_keys .
    aliases get_position     for if_object_map~get_position .
    aliases get_values       for if_object_map~get_values .
    aliases get_values_iterator for if_object_map~get_values_iterator .
    aliases get_values_table    for if_object_map~get_values_table .
    aliases is_empty            for if_object_map~is_empty .
    aliases size                for if_object_map~size .

    types:
      begin of ty_map_element,
        key   type        string,
        value type ref to object,
      end of ty_map_element .
    types:
* Change table type to make it work faster!
*    types:
*      ty_map type standard table of ty_map_element .
      ty_map type hashed table of ty_map_element
          with unique key primary_key components key .
    types:
      ty_sorted_map type sorted table of ty_map_element with unique key key .

    data map type ty_map read-only .

    methods constructor
      importing
        !map type ty_map optional .
    methods put
      importing
        !key   type any
        !value type ref to object .
    methods remove
      importing
        !key type any .
    methods clear .
    methods change_key
      importing
        value(old_key) type any
        value(new_key) type any .
    methods put_all
      importing
        !io_map type ref to zcl_lso_object_map .
  protected section.
*"* protected components of class ZCL_LSO_OBJECT_MAP
*"* do not include other source files here!!!

    methods get_key_of_object
      importing
        !value     type ref to object
      returning
        value(key) type string .
  private section.
*"* private components of class ZCL_LSO_OBJECT_MAP
*"* do not include other source files here!!!
endclass.

class zcl_lso_object_map implementation.

  method change_key.

    data internal_key type string.

    field-symbols: <obj> type ty_map_element.

    internal_key = old_key.
    translate internal_key to upper case.
    read table me->map with key key = internal_key assigning <obj>.
    if sy-subrc = 0.
*   check if there's already an entry with the new key, if yes, don't change the old key
      internal_key = new_key.
      translate internal_key to upper case.
      read table me->map with key key = internal_key transporting no fields.
      if sy-subrc <> 0.
        <obj>-key = internal_key.
      endif.
    endif.

  endmethod.

  method clear.
    clear map.
  endmethod.

  method constructor.
    me->map = map.
  endmethod.

  method get_key_of_object .
    data map_elem type ty_map_element.
    loop at map into map_elem.
      if map_elem-value = value.
        key = map_elem-key.
        return.
      endif.
    endloop.
  endmethod.

  method if_object_map~contains_key.

    data internal_key type string.
    internal_key = key.

*  translate internal_key to upper case.
*
*  read table map with key key = internal_key transporting no fields.
*
*  if sy-subrc = 0.
*    return = 'X'.
*  else.
*    return = ' '.
*  endif.

    return = boolc( line_exists( me->map[ key primary_key components key = to_upper( internal_key ) ] ) ).

  endmethod.

  method if_object_map~contains_value .

    field-symbols <map_elem> type ty_map_element.

    loop at map assigning <map_elem>.
      if <map_elem>-value = value.
        return = abap_true.
        return.
      endif.
    endloop.

    return = abap_false.

  endmethod.

  method if_object_map~get .

    data internal_key type string.
    internal_key = key.

*  data map_elem type ty_map_element.
*  translate internal_key to upper case.
*  read table map with key key = internal_key into map_elem.
*  if sy-subrc = 0.
*    value = map_elem-value.
*  endif.

    assign me->map[ key primary_key components key = to_upper( internal_key ) ]
      to field-symbol(<ls_map_elem>).

    if sy-subrc = 0.
      value = <ls_map_elem>-value.
    endif.

  endmethod.

  method if_object_map~get_by_position .
    return.
  endmethod.

  method if_object_map~get_keys .
    data map_elem type ty_map_element.
    loop at map into map_elem.
      append map_elem-key to keys.
    endloop.
  endmethod.

  method if_object_map~get_position .
    data map_elem type ty_map_element.
    loop at map into map_elem.
      if map_elem-value = value.
        position = sy-tabix.
        return.
      endif.
    endloop.
  endmethod.

  method if_object_map~get_values .
    data object_collection type ref to cl_object_collection.
    create object object_collection.
    data map_elem type ty_map_element.
    loop at map into map_elem.
      object_collection->add( map_elem-value ).
    endloop.
    values = object_collection.
  endmethod.

  method if_object_map~get_values_iterator .
    data collection type ref to if_object_collection.
    collection = get_values( ).
    value_iterator = collection->get_iterator( ).
  endmethod.

  method if_object_map~get_values_table .
    data map_elem type ty_map_element.
    loop at map into map_elem.
      append map_elem-value to values.
    endloop.
  endmethod.

  method if_object_map~is_empty .

    is_empty = boolc( me->size( ) is initial ).

  endmethod.

  method if_object_map~size .

    size = lines( me->map ).

  endmethod.

  method put .

    data internal_key type string.

    internal_key = to_upper( key ).

*  data map_elem type ty_map_element.
*  translate internal_key to upper case.
*
*  read table map with key key = key transporting no fields.
*  if sy-subrc <> 0.
*    map_elem-key = internal_key.
*    map_elem-value = value.
*
*    insert map_elem into table map.
*  endif.

    if not line_exists( me->map[ key primary_key components key = internal_key ] ).
      insert value ty_map_element(
          key = internal_key
          value = value )
        into table me->map.
    endif.

  endmethod.


  method put_all.

    loop at io_map->map assigning field-symbol(<ls_map>).
      me->put(
        key   = <ls_map>-key
        value = <ls_map>-value ).
    endloop.

  endmethod.


  method remove .
    data internal_key type string.
    internal_key = key.
    translate internal_key to upper case.
    delete map where key = internal_key.
  endmethod.
endclass.
