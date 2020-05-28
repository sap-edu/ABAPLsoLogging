interface zif_lso_comparable
  public .

  "! <p class="shorttext synchronized" lang="en">Is object equal?</p>
  "!
  "! @parameter object | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter rv_is_equal | <p class="shorttext synchronized" lang="en">true/false</p>
  methods equals
    importing
      io_object          type ref to object
    returning
      value(rv_is_equal) type abap_bool .

endinterface.
