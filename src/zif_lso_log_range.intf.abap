interface zif_lso_log_range
  public .

  types tt_data type standard table of ref to data with empty key.

  " SAP note 635318 - Open SQL: Size restrictions for commands
  " IN with a list in the WHERE clause, must not contain more than 1000 elements.
  " The length of the statement in the database must not exceed 28672 characters (with Unicode, 14336 characters).
  constants c_max_opensql_in type i value 500.

  "! Range - option
  constants begin of c_option.
  constants equal type ddoption value 'EQ'.
  constants not_equal type ddoption value 'NE'.
  constants contains_pattern type ddoption value 'CP'.
  constants not_contains_pattern type ddoption value 'NP'.
  constants between type ddoption value 'BT'.
  constants end of c_option .

  "! Range - sign
  constants begin of c_sign.
  constants include type ddsign value 'I'.
  constants exclude type ddsign value 'E'.
  constants end of c_sign .

  "! Range - time
  constants begin of c_time.
  constants min type tims value '000000'.
  constants max type tims value '235959'.
  constants end of c_time .

  "! Range - date
  constants begin of c_date.
  constants min type dats value '19000101'.
  constants max type dats value '99991231'.
  constants end of c_date .

  constants begin of c_timestampl.    "YYYYMMDDhhmmss.mmmuuun
  constants min type timestampl value '00010101000000.0000000'.
  constants max type timestampl value '99991231235959.0000000'.
  constants end of c_timestampl.


endinterface.
