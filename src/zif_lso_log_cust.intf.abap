interface zif_lso_log_cust
  public .

  "! <p class="shorttext synchronized" lang="en">Get customizing id</p>
  "!
  "! @parameter id | <p class="shorttext synchronized" lang="en"></p>
  methods get_id
    returning value(id) type zlso_log_cust-id.

  "! <p class="shorttext synchronized" lang="en">Get customizing value</p>
  "!
  "! @parameter value | <p class="shorttext synchronized" lang="en"></p>
  methods get_value
    returning value(value) type zlso_log_cust-value.

  "! <p class="shorttext synchronized" lang="en">Set customizing value</p>
  "!
  "! @parameter value | <p class="shorttext synchronized" lang="en"></p>
  methods set_value
    importing value type zlso_log_cust-value.

  "! <p class="shorttext synchronized" lang="en">Save customizing value</p>
  "!
  "! @raising zcx_lso_log | <p class="shorttext synchronized" lang="en"></p>
  methods save
    returning value(result) type abap_bool.
endinterface.
