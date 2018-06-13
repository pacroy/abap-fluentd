CLASS zcl_fdlog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_fdlog.
    ALIASES i FOR zif_fdlog~i.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_utc TYPE string VALUE 'UTC' ##NO_TEXT.

    METHODS:
      send
        IMPORTING is_data TYPE any
        RAISING   zcx_fdlog,
      current_unix_time
        RETURNING VALUE(rv_timestamp) TYPE zif_fdlog=>tv_unixtime,
      create_fdlog
        RETURNING VALUE(rs_fdlog) TYPE zif_fdlog~ts_fdlog.
ENDCLASS.



CLASS zcl_fdlog IMPLEMENTATION.

  METHOD send.
    DATA(lo_rest) = zcl_fdlog_factory=>rest( zcl_fdlog_factory=>http( ) ).

    DATA(lo_request) = lo_rest->if_rest_client~create_request_entity( ).
    DATA(lv_data) = /ui2/cl_json=>serialize( is_data ).
    REPLACE ALL OCCURRENCES OF '"TIME"' IN lv_data WITH '"time"'.
    lo_request->set_string_data( lv_data ).
    lo_request->set_content_type( if_rest_media_type=>gc_appl_json ).

    lo_rest->if_rest_client~post( lo_request ).

    DATA(lv_status) = lo_rest->if_rest_client~get_status( ).
    DATA(lv_response) = lo_rest->if_rest_client~get_response_entity( )->get_string_data( ).
  ENDMETHOD.

  METHOD current_unix_time.
    DATA: lv_date_utc TYPE sy-datum,
          lv_time_utc TYPE sy-uzeit.

    DATA(lv_timestamp) = zcl_fdlog_factory=>abap( )->get_utc_timestamp( ).

    CONVERT TIME STAMP lv_timestamp TIME ZONE c_utc INTO DATE lv_date_utc TIME lv_time_utc.

    cl_pco_utility=>convert_abap_timestamp_to_java(
      EXPORTING
        iv_date = lv_date_utc
        iv_time = lv_time_utc
      IMPORTING
        ev_timestamp = DATA(lv_unixtime) ).
    rv_timestamp = lv_unixtime / 1000.
  ENDMETHOD.

  METHOD zif_fdlog~i.

  ENDMETHOD.

  METHOD create_fdlog.
    rs_fdlog-system = sy-sysid.
    rs_fdlog-client = sy-mandt.
    rs_fdlog-user = sy-uname.
    rs_fdlog-host = sy-host.
    rs_fdlog-program = sy-cprog.
    rs_fdlog-time = current_unix_time( ).
  ENDMETHOD.

ENDCLASS.
