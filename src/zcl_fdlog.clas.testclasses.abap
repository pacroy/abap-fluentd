*"* use this source file for your ABAP unit test classes

CLASS ltcl_log DEFINITION DEFERRED.
CLASS ltcl_unix_time DEFINITION DEFERRED.
CLASS zcl_fdlog DEFINITION LOCAL FRIENDS ltcl_log ltcl_unix_time.

CLASS ltcl_log DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: ao_cut  TYPE REF TO zcl_fdlog.
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

*    ao_cut->send( lt_fdlog ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_unix_time DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: ao_cut  TYPE REF TO zcl_fdlog,
          ao_abap TYPE REF TO zif_fdlog_abap.
    METHODS:
      setup,
      happy_path FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_unix_time IMPLEMENTATION.

  METHOD setup.

    ao_cut = NEW #( ).
    ao_abap = CAST zif_fdlog_abap( cl_abap_testdouble=>create( 'ZIF_FDLOG_ABAP') ) ##NO_TEXT.
    zcl_fdlog_inject=>inject_abap( ao_abap ).

  ENDMETHOD.

  METHOD happy_path.
    cl_abap_testdouble=>configure_call( ao_abap )->returning( '20180118120000' ).
    ao_abap->get_utc_timestamp( ).

    cl_abap_unit_assert=>assert_equals( exp = 1516276800 act = ao_cut->current_unix_time( ) ).
  ENDMETHOD.

ENDCLASS.
