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
    DATA: ao_rest  TYPE REF TO if_rest_client.
    DATA: ao_entity  TYPE REF TO if_rest_entity.
    METHODS:
      setup RAISING cx_static_check,
      happy_path FOR TESTING RAISING cx_static_check,
      no_data_to_send FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_send IMPLEMENTATION.

  METHOD setup.

    ao_cut = NEW #( iv_inst_name = 'ABAPUNIT' iv_upd_task = abap_false ).
    ao_cut->read_and_clear( ).

    ao_rest = CAST if_rest_client( cl_abap_testdouble=>create( 'IF_REST_CLIENT' ) ) ##NO_TEXT.
    zcl_fdlog_inject=>inject_rest( ao_rest ).
    ao_entity = CAST if_rest_entity( cl_abap_testdouble=>create( 'IF_REST_ENTITY' ) ) ##NO_TEXT.

  ENDMETHOD.


  METHOD happy_path.
    cl_abap_testdouble=>configure_call( ao_rest )->returning( ao_entity ).
    ao_rest->create_request_entity( ).
    cl_abap_testdouble=>configure_call( ao_rest )->and_expect( )->is_called_once( ).
    ao_rest->post( ao_entity ).
    cl_abap_testdouble=>configure_call( ao_entity )->ignore_all_parameters( )->and_expect( )->is_called_once( ).
    ao_entity->set_string_data( `` ).
    cl_abap_testdouble=>configure_call( ao_entity )->and_expect( )->is_called_once( ).
    ao_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
    cl_abap_testdouble=>configure_call( ao_entity )->and_expect( )->is_called_once( ).
    ao_entity->set_header_field( iv_name = '~request_uri' iv_value = |/{ sy-sysid }.{ sy-mandt }.ABAPUNIT| ).

    ao_cut->i( 'Test message' ).
    ao_cut->send( ).

    cl_abap_testdouble=>verify_expectations( ao_rest ).
    cl_abap_testdouble=>verify_expectations( ao_entity ).
  ENDMETHOD.

  METHOD no_data_to_send.
    cl_abap_testdouble=>configure_call( ao_rest )->returning( ao_entity ).
    ao_rest->create_request_entity( ).
    cl_abap_testdouble=>configure_call( ao_rest )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    ao_rest->post( ao_entity ).
    cl_abap_testdouble=>configure_call( ao_entity )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    ao_entity->set_string_data( `` ).

    ao_cut->send( ).

    cl_abap_testdouble=>verify_expectations( ao_rest ).
    cl_abap_testdouble=>verify_expectations( ao_entity ).
  ENDMETHOD.

ENDCLASS.

CLASS ltdb_fdlog_abap DEFINITION FOR TESTING CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES lif_fdlog_abap.
    DATA av_timestamp TYPE tzonref-tstamps.
    DATA av_guid TYPE guid_16.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ltdb_fdlog_abap IMPLEMENTATION.

  METHOD lif_fdlog_abap~get_utc_timestamp.
    rv_timestamp = av_timestamp.
  ENDMETHOD.

  METHOD lif_fdlog_abap~get_guid.
    rv_guid = av_guid.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_unix_time DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: ao_cut  TYPE REF TO zcl_fdlog,
          ao_abap TYPE REF TO ltdb_fdlog_abap.
    METHODS:
      setup,
      happy_path FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_unix_time IMPLEMENTATION.

  METHOD setup.

    ao_cut = NEW #( iv_upd_task = abap_false ).
    ao_abap = NEW ltdb_fdlog_abap( ).
    lcl_fdlog_inject=>inject_abap( ao_abap ).

  ENDMETHOD.

  METHOD happy_path.
    ao_abap->av_timestamp = '20180118120000'.

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
    CLASS-DATA: ao_abap  TYPE REF TO ltdb_fdlog_abap.

    CLASS-METHODS: class_setup, class_teardown.
    METHODS:
      setup RAISING cx_static_check,
      happy_path FOR TESTING RAISING cx_static_check,
      multiple_types FOR TESTING RAISING cx_static_check,
      write_log FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_write_log IMPLEMENTATION.

  METHOD class_setup.
    ao_abap = NEW ltdb_fdlog_abap( ).
    ao_abap->av_guid = '1234567890ABCDEF1234567890ABCDEF'.
    lcl_fdlog_inject=>inject_abap( ao_abap ).
  ENDMETHOD.

  METHOD setup.

    ao_cut = NEW #( iv_inst_name = 'ABAPUNIT' iv_upd_task = abap_false ).
    ao_cut->read_and_clear( ).

  ENDMETHOD.

  METHOD happy_path.

    ao_cut->i( 'This is a test' ).
    DATA(lt_fdlog) = ao_cut->read_and_clear( ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_fdlog ) ).

    DATA(ls_fdlog1) = lt_fdlog[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = sy-host act = ls_fdlog1-host ).
    cl_abap_unit_assert=>assert_equals( exp = 'I' act = ls_fdlog1-msgtype ).
    cl_abap_unit_assert=>assert_equals( exp = sy-cprog act = ls_fdlog1-program ).
    cl_abap_unit_assert=>assert_equals( exp = sy-uname act = ls_fdlog1-user ).
    cl_abap_unit_assert=>assert_equals( exp = 'This is a test' act = ls_fdlog1-message ).
    cl_abap_unit_assert=>assert_equals( exp = ao_abap->av_guid act = ls_fdlog1-corrid ).

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

    cl_abap_unit_assert=>assert_equals( exp = lt_fdlog[ 6 ]-corrid act = lt_fdlog[ 1 ]-corrid ).
  ENDMETHOD.

  METHOD write_log.
    MESSAGE ID 'SY' TYPE 'S' NUMBER 499
      WITH 'This is' 'a test'.

    ao_cut->log( ).
    ao_cut->log( VALUE #( msgid = '00' msgno = '001' msgty = 'I' msgv1 = 'Test again' msgv2 = 'and again' ) ).

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
