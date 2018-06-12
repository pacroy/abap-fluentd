CLASS zcl_fdlog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.


  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      log
        IMPORTING is_data TYPE any.
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

ENDCLASS.
