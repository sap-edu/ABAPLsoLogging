class zcl_lso_log_security definition
  public
  final
  create public .

  public section.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter dialog | <p class="shorttext synchronized" lang="en">Show popups when users are not authorized</p>
    methods constructor
      importing
        dialog type abap_bool default abap_false.

    methods is_technical_support
      returning
        value(is_technical_support) type abap_bool.

  protected section.

  private section.

    data: dialog type abap_bool.

    methods show_unauthorized_popup
      importing
        missing_role type string.

endclass.



class zcl_lso_log_security implementation.

  method constructor.

    me->dialog = dialog.

  endmethod.

  method is_technical_support.

    "Check if the user is Technical Support
    authority-check object 'S_DEVELOP' id 'ACTVT'    field '03'
                                       id 'P_GROUP'  field '*'
                                       id 'DEVCLASS' field '*'
                                       id 'OBJTYPE'  field '*'
                                       id 'OBJNAME'  field '*'.
    if sy-subrc = 0.
      is_technical_support = abap_true.
    else.
      is_technical_support = abap_false.
      if me->dialog = abap_true.
        show_unauthorized_popup( missing_role = |0000_BC_TECHNICAL_SUPPORT| ).
      endif.
    endif.

  endmethod.

  method show_unauthorized_popup.

    data(title) = |{ text-p01 }|. " Missing Authorization
    data(txt1)  = |{ text-p02 }|. " You're missing the authorization to use this tool.
    data(txt2)  = |{ text-p03 } { missing_role }.|. " Request Role XXX.

    call function 'POPUP_TO_INFORM'
      exporting
        titel = title
        txt1  = txt1
        txt2  = txt2.

  endmethod.
endclass.
