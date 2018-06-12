*"* use this source file for your ABAP unit test classes

CLASS ltcl_log DEFINITION DEFERRED.
CLASS ltcl_unix_time DEFINITION DEFERRED.
CLASS zcl_fdlog DEFINITION LOCAL FRIENDS ltcl_log ltcl_unix_time.

CLASS ltcl_log DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
  PROTECTED SECTION.
    DATA: ao_cut  TYPE REF TO zcl_fdlog.
  PRIVATE SECTION.
    METHODS:
      setup,
      happy_path FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_log IMPLEMENTATION.

  METHOD setup.

    ao_cut = NEW #( ).

  ENDMETHOD.

  METHOD happy_path.

    DATA lt_fdlog TYPE ao_cut->zif_fdlog~tt_fdlog.

    DATA(ls_fdlog1) = ao_cut->create_fdlog( ).
    ls_fdlog1-message = 'This is the 1st message'.
    APPEND ls_fdlog1 TO lt_fdlog.

    WAIT UP TO 1 SECONDS.

    DATA(ls_fdlog2) = ao_cut->create_fdlog( ).
    ls_fdlog2-message = 'This is the 2nd message'.
    APPEND ls_fdlog2 TO lt_fdlog.

    ao_cut->send( lt_fdlog ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_unix_time DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
  PROTECTED SECTION.
    DATA: ao_cut  TYPE REF TO zcl_fdlog.
  PRIVATE SECTION.
    METHODS:
      setup,
      happy_path FOR TESTING RAISING cx_static_check,
      current_time FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_unix_time IMPLEMENTATION.

  METHOD setup.

    ao_cut = NEW #( ).

  ENDMETHOD.

  METHOD happy_path.

    DATA: lv_timestamp TYPE tzonref-tstamps,
          lv_date      TYPE sy-datum,
          lv_time      TYPE sy-uzeit.

    CONVERT DATE '20180118' TIME '120000' INTO TIME STAMP lv_timestamp TIME ZONE ao_cut->c_utc.
    CONVERT TIME STAMP lv_timestamp TIME ZONE sy-zonlo INTO DATE lv_date TIME lv_time.

    cl_abap_unit_assert=>assert_equals( exp = 1516276800 act = ao_cut->unix_time( iv_date = lv_date iv_time = lv_time ) ).

    CONVERT DATE '20180118' TIME '120000' INTO TIME STAMP lv_timestamp TIME ZONE ao_cut->c_utc.
    CONVERT TIME STAMP lv_timestamp TIME ZONE 'UTC+7' INTO DATE lv_date TIME lv_time.

    cl_abap_unit_assert=>assert_equals( exp = 1516276800 act = ao_cut->unix_time( iv_date = lv_date iv_time = lv_time iv_zone = 'UTC+7' ) ).

  ENDMETHOD.

  METHOD current_time.
    DATA: lv_timestamp TYPE tzonref-tstamps.

    DATA(lv_date) = sy-datum.
    DATA(lv_time) = sy-uzeit.

    DATA(lv_time1) = ao_cut->unix_time( iv_date = lv_date iv_time = lv_time ).

    cl_abap_unit_assert=>assert_true( xsdbool( ao_cut->unix_time( ) >= lv_time1 ) ).
  ENDMETHOD.

ENDCLASS.
