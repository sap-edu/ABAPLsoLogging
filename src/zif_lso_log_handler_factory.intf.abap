interface zif_lso_log_handler_factory
  public .

  "! <p class="shorttext synchronized" lang="en">Get log handler map dummy key</p>
  "!
  "! @parameter dummy_key | <p class="shorttext synchronized" lang="en"></p>
  methods get_dummy_key
    returning value(dummy_key) type string .

  "! <p class="shorttext synchronized" lang="en">Create log handlers map by keys</p>
  "!
  "! @parameter keys | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter map | <p class="shorttext synchronized" lang="en"></p>
  methods create_map_by_keys
    importing keys       type string_table optional
    returning value(map) type ref to zcl_lso_object_map .

  "! <p class="shorttext synchronized" lang="en">Create log handler map of ZIF_LSO_LOGGABLE_OBJECT objects</p>
  "!
  "! @parameter collection | <p class="shorttext synchronized" lang="en">Collection of ZIF_LSO_LOGGABLE_OBJECT objects</p>
  "! @parameter map | <p class="shorttext synchronized" lang="en"></p>
  "! @raising cx_sy_move_cast_error | <p class="shorttext synchronized" lang="en"></p>
  methods create_map_by_loggable
    importing collection type ref to if_object_collection
    returning value(map) type ref to zcl_lso_object_map
    raising   cx_sy_move_cast_error .

  methods create
    importing
      object type any
    returning
      value(log_handler) type ref to zcl_lso_log_handler.

endinterface.
