*"* use this source file for your ABAP unit test classes

CLASS ltcl_send DEFINITION DEFERRED.
CLASS ltcl_unix_time DEFINITION DEFERRED.
CLASS ltcl_write_log DEFINITION DEFERRED.
CLASS zcl_fdlog DEFINITION LOCAL FRIENDS ltcl_send ltcl_unix_time ltcl_write_log.

CLASS ltcl_send DEFINITION FINAL FOR TESTING
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


CLASS ltcl_send IMPLEMENTATION.

  METHOD setup.

    ao_cut = NEW #( ).

  ENDMETHOD.

  METHOD happy_path.


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

CLASS ltcl_write_log DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: ao_cut  TYPE REF TO zcl_fdlog.

    CLASS-METHODS: class_teardown.
    METHODS:
      setup RAISING cx_static_check,
      happy_path FOR TESTING RAISING cx_static_check,
      multiple_types FOR TESTING RAISING cx_static_check,
      write_log FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_write_log IMPLEMENTATION.

  METHOD setup.

    ao_cut = NEW #( 'ABAPUNIT' ).
    ao_cut->read_and_clear( ).

  ENDMETHOD.

  METHOD happy_path.

    ao_cut->i( 'This is a test' ).
    DATA(lt_fdlog) = ao_cut->read_and_clear( ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_fdlog ) ).

    DATA(ls_fdlog1) = lt_fdlog[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = sy-sysid act = ls_fdlog1-system ).
    cl_abap_unit_assert=>assert_equals( exp = sy-mandt act = ls_fdlog1-client ).
    cl_abap_unit_assert=>assert_equals( exp = sy-host act = ls_fdlog1-host ).
    cl_abap_unit_assert=>assert_equals( exp = 'I' act = ls_fdlog1-msgtype ).
    cl_abap_unit_assert=>assert_equals( exp = sy-cprog act = ls_fdlog1-program ).
    cl_abap_unit_assert=>assert_equals( exp = sy-uname act = ls_fdlog1-user ).
    cl_abap_unit_assert=>assert_equals( exp = 'This is a test' act = ls_fdlog1-message ).

    lt_fdlog = ao_cut->read( ).
    cl_abap_unit_assert=>assert_initial( lt_fdlog ).
  ENDMETHOD.

  METHOD multiple_types.
    ao_cut->i( 'I message' ).
    ao_cut->s( 'S message' ).
    ao_cut->w( 'W message' ).
    ao_cut->e( 'E message' ).
    ao_cut->a( 'A message' ).
    ao_cut->x( 'X message' ).

    DATA(lt_fdlog) = ao_cut->read_and_clear( ).
    cl_abap_unit_assert=>assert_equals( exp = 6 act = lines( lt_fdlog ) ).

    lt_fdlog[ 1 ]-msgtype = 'I'.
    lt_fdlog[ 2 ]-msgtype = 'S'.
    lt_fdlog[ 3 ]-msgtype = 'W'.
    lt_fdlog[ 4 ]-msgtype = 'E'.
    lt_fdlog[ 5 ]-msgtype = 'A'.
    lt_fdlog[ 6 ]-msgtype = 'X'.
  ENDMETHOD.

  METHOD write_log.
    MESSAGE ID 'SY' TYPE 'S' NUMBER 499
      WITH 'This is' 'a test'.

    ao_cut->write( ).
    ao_cut->write( VALUE #( msgid = '00' msgno = '001' msgty = 'I' msgv1 = 'Test again' msgv2 = 'and again' ) ).

    DATA(lt_fdlog) = ao_cut->read_and_clear( ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_fdlog ) ).

    DATA(ls_fdlog1) = lt_fdlog[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = 'SY' act = ls_fdlog1-msgid ).
    cl_abap_unit_assert=>assert_equals( exp = '499' act = ls_fdlog1-msgno ).
    cl_abap_unit_assert=>assert_equals( exp = 'S' act = ls_fdlog1-msgtype ).
    cl_abap_unit_assert=>assert_equals( exp = 'This is a test' act = ls_fdlog1-message ).

    DATA(ls_fdlog2) = lt_fdlog[ 2 ].
    cl_abap_unit_assert=>assert_equals( exp = '00' act = ls_fdlog2-msgid ).
    cl_abap_unit_assert=>assert_equals( exp = '001' act = ls_fdlog2-msgno ).
    cl_abap_unit_assert=>assert_equals( exp = 'I' act = ls_fdlog2-msgtype ).
    cl_abap_unit_assert=>assert_equals( exp = 'Test againand again' act = ls_fdlog2-message ).
  ENDMETHOD.

  METHOD class_teardown.
    zcl_fdlog_shr_area=>free_instance( 'ABAPUNIT'  ).
  ENDMETHOD.

ENDCLASS.
