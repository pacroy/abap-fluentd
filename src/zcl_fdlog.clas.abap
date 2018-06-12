CLASS zcl_fdlog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.


  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_utc TYPE string VALUE 'UTC' ##NO_TEXT.

    METHODS:
      log
        IMPORTING is_data TYPE any,
      unix_time
        IMPORTING iv_date             TYPE sy-datum OPTIONAL
                  iv_time             TYPE sy-uzeit OPTIONAL
                  iv_zone             TYPE sy-zonlo DEFAULT sy-zonlo
        RETURNING VALUE(rv_timestamp) TYPE int8.
ENDCLASS.



CLASS zcl_fdlog IMPLEMENTATION.

  METHOD log.
    TRY.
        DATA(lo_rest) = zcl_fdlog_factory=>rest( zcl_fdlog_factory=>http( ) ).

        DATA(lo_request) = lo_rest->if_rest_client~create_request_entity( ).
        DATA(lv_data) = /ui2/cl_json=>serialize( is_data ).
        lo_request->set_string_data( lv_data ).
        lo_request->set_content_type( if_rest_media_type=>gc_appl_json ).

        lo_rest->if_rest_client~post( lo_request ).

        DATA(lv_status) = lo_rest->if_rest_client~get_status( ).
        DATA(lv_response) = lo_rest->if_rest_client~get_response_entity( )->get_string_data( ).
      CATCH cx_root INTO DATA(cx).
        DATA(lv_msg) = cx->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD unix_time.
    DATA: lv_timestamp TYPE tzonref-tstamps,
          lv_date      TYPE sy-datum,
          lv_time      TYPE sy-uzeit,
          lv_date_utc  TYPE sy-datum,
          lv_time_utc  TYPE sy-uzeit.

    IF ( iv_date IS NOT INITIAL ).
      lv_date = iv_date.
      lv_time = iv_time.
    ELSE.
      lv_date = sy-datum.
      lv_time = sy-uzeit.
    ENDIF.

    CONVERT DATE lv_date TIME lv_time INTO TIME STAMP lv_timestamp TIME ZONE iv_zone.
    CONVERT TIME STAMP lv_timestamp TIME ZONE c_utc INTO DATE lv_date_utc TIME lv_time_utc.

    cl_pco_utility=>convert_abap_timestamp_to_java(
      EXPORTING
        iv_date = lv_date_utc
        iv_time = lv_time_utc
      IMPORTING
        ev_timestamp = DATA(lv_unixtime) ).
    rv_timestamp = lv_unixtime.
    rv_timestamp = rv_timestamp / 1000.
  ENDMETHOD.

ENDCLASS.
