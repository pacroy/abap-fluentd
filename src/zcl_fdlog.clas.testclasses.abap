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

*    DATA(ls_data) = zcl_hello_controller=>hello( 'ABAPUnit' ).
*
*    ao_cut->log( ls_data ).

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
      happy_path FOR TESTING RAISING cx_static_check.
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

  ENDMETHOD.

ENDCLASS.
