class zcl_lso_log_cust_factory definition
  public
  final
  create private .

  public section.
    interfaces zif_lso_log_cust_factory.

    aliases get for zif_lso_log_cust_factory~get.

    "! <p class="shorttext synchronized" lang="en">Get customizing factory instance (singleton)</p>
    "!
    "! @parameter self | <p class="shorttext synchronized" lang="en"></p>
    class-methods instance
      returning value(self) type ref to zif_lso_log_cust_factory.

    "! <p class="shorttext synchronized" lang="en">Get 'delete payloads after NNN days' customizing object</p>
    "!
    "! @parameter read_db | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter custom | <p class="shorttext synchronized" lang="en"></p>
    methods get_delete_payloads_after
      importing read_db       type abap_bool default abap_false
      returning value(custom) type ref to zif_lso_log_cust
      raising   zcx_lso_log.

    "! <p class="shorttext synchronized" lang="en">Get 'delete traces after NNN days' customizing object</p>
    "!
    "! @parameter read_db | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter custom | <p class="shorttext synchronized" lang="en"></p>
    methods get_delete_traces_after
      importing read_db       type abap_bool default abap_false
      returning value(custom) type ref to zif_lso_log_cust
      raising   zcx_lso_log.

    "! <p class="shorttext synchronized" lang="en">Get 'delete messages after NNN days' customizing object</p>
    "!
    "! @parameter read_db | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter custom | <p class="shorttext synchronized" lang="en"></p>
    methods get_delete_messages_after
      importing read_db       type abap_bool default abap_false
      returning value(custom) type ref to zif_lso_log_cust
      raising   zcx_lso_log.

    "! <p class="shorttext synchronized" lang="en">Get 'delete logs after NNN days' customizing object</p>
    "!
    "! @parameter read_db | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter custom | <p class="shorttext synchronized" lang="en"></p>
    methods get_delete_logs_after
      importing read_db       type abap_bool default abap_false
      returning value(custom) type ref to zif_lso_log_cust
      raising   zcx_lso_log.

  protected section.
  private section.
    class-data self type ref to zif_lso_log_cust_factory.

    data customs type zif_lso_log_cust_factory=>tt_custom.

    methods get_default
      importing
        id          type zlso_d_log_custom_id
      returning
        value(cust) type zlso_log_cust
      raising
        zcx_lso_log.

endclass.

class zcl_lso_log_cust_factory implementation.

  method instance.
    if zcl_lso_log_cust_factory=>self is not bound.
      zcl_lso_log_cust_factory=>self = new zcl_lso_log_cust_factory( ).
    endif.

    self = zcl_lso_log_cust_factory=>self.
  endmethod.


  method get_delete_logs_after.
    custom = me->zif_lso_log_cust_factory~get( id      = zif_lso_log_cust_factory=>id-delete_log_after
                                               read_db = read_db ).
  endmethod.


  method get_delete_messages_after.
    custom = me->zif_lso_log_cust_factory~get( id      = zif_lso_log_cust_factory=>id-delete_message_after
                                               read_db = read_db ).
  endmethod.


  method get_delete_payloads_after.
    custom = me->zif_lso_log_cust_factory~get( id      = zif_lso_log_cust_factory=>id-delete_payload_after
                                               read_db = read_db ).
  endmethod.


  method get_delete_traces_after.
    custom = me->zif_lso_log_cust_factory~get( id      = zif_lso_log_cust_factory=>id-delete_trace_after
                                               read_db = read_db ).
  endmethod.

  method get_default.

    cust-mandt = sy-mandt.
    cust-id    = id.
    cust-sysid = sy-sysid.


    case id.
      when zif_lso_log_cust_factory=>id-delete_payload_after.
        cust-value       = |400|.    " Delete log trace payload after specific number of days
        cust-explanation = text-c01.

      when  zif_lso_log_cust_factory=>id-delete_trace_after.
        cust-value       = |500|.
        cust-explanation = text-c02. " Delete log trace after specific number of days

      when zif_lso_log_cust_factory=>id-delete_message_after.
        cust-value       = |800|.
        cust-explanation = text-c03. " Delete log message after specific number of days

      when zif_lso_log_cust_factory=>id-delete_log_after.
        cust-value       = |1500|.
        cust-explanation = text-c04. " Delete log after specific number of days

      when others.

        " Customizing value not found, raise an exception!
        raise exception type zcx_lso_log
          exporting
            textid   = zcx_lso_log=>customizing_value_not_found
            mv_msgv1 = conv #( id ).
    endcase.

  endmethod.

  method zif_lso_log_cust_factory~get.

    data cust type zlso_log_cust.

    if read_db eq abap_true or not line_exists( me->customs[ id = id ] ).
      " Select customizing value from DB.
      select single mandt, id, sysid, value
        from zlso_log_cust
        into @cust
       where id    eq @id
         and sysid eq @sy-sysid.

      if sy-subrc <> 0.
        " Get Default values
        cust = get_default( id ).
        insert into zlso_log_cust values cust.
        if sy-subrc <> 0.
          " Customizing value not found, raise an exception!
          raise exception type zcx_lso_log
            exporting
              textid   = zcx_lso_log=>customizing_value_not_found
              mv_msgv1 = conv #( id ).
        endif.

      endif.

      " Delete potential customizing object from the buffer.
      delete table me->customs with table key primary_key components id = cust-id.

      " Add customizing object to the buffer.
      insert value #( id       = cust-id
                      instance = new zcl_lso_log_cust( id    = cust-id
                                                       value = cust-value ) )
        into table me->customs.
    endif.

    custom = me->customs[ id = id ]-instance.

  endmethod.

endclass.
