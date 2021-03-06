CLASS zcl_fdlog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_fdlog.

    METHODS:
      constructor
        IMPORTING
          iv_inst_name TYPE shm_inst_name DEFAULT cl_shm_area=>default_instance
          iv_upd_task  TYPE abap_bool DEFAULT abap_true
          iv_rfc_dest  TYPE rfcdest DEFAULT 'FLUENTD' ##NO_TEXT..

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_utc TYPE string VALUE 'UTC' ##NO_TEXT.

    DATA: av_guid TYPE guid_16.

    DATA: av_inst_name TYPE shm_inst_name.

    METHODS:
      current_unix_time
        RETURNING VALUE(rv_timestamp) TYPE zif_fdlog=>tv_unixtime,
      create_fdlog
        RETURNING VALUE(rs_fdlog) TYPE zif_fdlog~ts_fdlog,
      attach_for_update
        RAISING
          cx_shm_attach_error,
      attach_for_read
        RAISING
          cx_shm_attach_error,
      get_root
        RETURNING VALUE(ro_root) TYPE REF TO zcl_fdlog_shr_root,
      detach
        RAISING cx_shm_detach_error,
      append
        IMPORTING
          it_fdlog TYPE zif_fdlog=>tt_fdlog
        RAISING
          cx_shm_attach_error,
      read
        RETURNING VALUE(rt_fdlog) TYPE zif_fdlog=>tt_fdlog
        RAISING
                  cx_shm_attach_error,
      read_and_clear
        RETURNING VALUE(rt_fdlog) TYPE zif_fdlog=>tt_fdlog
        RAISING
                  cx_shm_attach_error,
      add_message
        IMPORTING iv_message TYPE string
                  iv_msgtype TYPE sy-msgty.
ENDCLASS.



CLASS zcl_fdlog IMPLEMENTATION.


  METHOD add_message.
    TRY.
        DATA(ls_fdlog) = create_fdlog( ).
        ls_fdlog-message = iv_message.
        ls_fdlog-msgtype = iv_msgtype.
        append( VALUE #( ( ls_fdlog ) ) ).
      CATCH cx_root INTO DATA(x).
    ENDTRY.
  ENDMETHOD.


  METHOD append.
    attach_for_update( ).
    DATA(lo_shr_root) = get_root( ).

    APPEND LINES OF it_fdlog TO lo_shr_root->data.

    detach( ).
  ENDMETHOD.


  METHOD attach_for_read.
    TRY.
        lcl_fdlog_factory=>shr_area( )->attach_for_read( inst_name = av_inst_name ).
      CATCH cx_shm_no_active_version.
        WAIT UP TO 1 SECONDS.
        TRY.
            lcl_fdlog_factory=>shr_area( )->attach_for_read( inst_name = av_inst_name ).
          CATCH  cx_shm_attach_error INTO DATA(lx_attach_error).
            RAISE EXCEPTION lx_attach_error.
        ENDTRY.
    ENDTRY.
  ENDMETHOD.


  METHOD attach_for_update.
    TRY.
        lcl_fdlog_factory=>shr_area( )->attach_for_update( inst_name = av_inst_name ).
      CATCH cx_shm_no_active_version.
        WAIT UP TO 1 SECONDS.
        TRY.
            lcl_fdlog_factory=>shr_area( )->attach_for_update( inst_name = av_inst_name ).
          CATCH  cx_shm_attach_error INTO DATA(lx_attach_error).
            RAISE EXCEPTION lx_attach_error.
        ENDTRY.
    ENDTRY.
  ENDMETHOD.


  METHOD constructor.
    zcl_fdlog_factory=>av_rfc_dest = iv_rfc_dest.
    av_guid = lcl_fdlog_factory=>abap( )->get_guid( ).
    av_inst_name = iv_inst_name.

    IF ( iv_upd_task = abap_true ).
      CALL FUNCTION 'Z_FDLOG_SEND' IN UPDATE TASK
        EXPORTING
          im_inst_name = av_inst_name.
    ENDIF.
  ENDMETHOD.


  METHOD create_fdlog.
    rs_fdlog-user = sy-uname.
    rs_fdlog-host = sy-host.
    rs_fdlog-program = sy-cprog.
    rs_fdlog-time = current_unix_time( ).
    rs_fdlog-corrid = av_guid.
  ENDMETHOD.


  METHOD current_unix_time.
    DATA: lv_date_utc TYPE sy-datum,
          lv_time_utc TYPE sy-uzeit.

    DATA(lv_timestamp) = lcl_fdlog_factory=>abap( )->get_utc_timestamp( ).

    CONVERT TIME STAMP lv_timestamp TIME ZONE c_utc INTO DATE lv_date_utc TIME lv_time_utc.

    cl_pco_utility=>convert_abap_timestamp_to_java(
      EXPORTING
        iv_date = lv_date_utc
        iv_time = lv_time_utc
      IMPORTING
        ev_timestamp = DATA(lv_unixtime) ).
    rv_timestamp = lv_unixtime / 1000.
  ENDMETHOD.


  METHOD detach.
    lcl_fdlog_factory=>shr_area( )->detach( ).
  ENDMETHOD.


  METHOD get_root.
    ro_root = CAST zcl_fdlog_shr_root( lcl_fdlog_factory=>shr_area( )->get_root( ) ).
  ENDMETHOD.


  METHOD read.
    attach_for_read( ).
    rt_fdlog = get_root( )->data.
    detach( ).
  ENDMETHOD.


  METHOD read_and_clear.
    attach_for_update( ).
    DATA(lo_root) = get_root( ).
    rt_fdlog = lo_root->data.
    CLEAR lo_root->data.
    detach( ).
  ENDMETHOD.


  METHOD zif_fdlog~a.
    add_message( iv_message = iv_message iv_msgtype = 'A' ).
  ENDMETHOD.


  METHOD zif_fdlog~e.
    add_message( iv_message = iv_message iv_msgtype = 'E' ).
  ENDMETHOD.


  METHOD zif_fdlog~i.
    add_message( iv_message = iv_message iv_msgtype = 'I' ).
  ENDMETHOD.


  METHOD zif_fdlog~log.
    TRY.
        DATA ls_symsg TYPE symsg.

        IF ( is_symsg IS NOT INITIAL ).
          ls_symsg = is_symsg.
        ELSE.
          ls_symsg = CORRESPONDING #( sy ).
        ENDIF.

        MESSAGE ID ls_symsg-msgid TYPE ls_symsg-msgty NUMBER ls_symsg-msgno
          WITH ls_symsg-msgv1 ls_symsg-msgv2 ls_symsg-msgv3 ls_symsg-msgv4
          INTO DATA(lv_message).

        DATA(ls_fdlog) = create_fdlog( ).
        IF ( ls_symsg-msgid IS NOT INITIAL ).
          ls_fdlog-message = |{ lv_message }, Msg.Id:({ ls_symsg-msgid }){ ls_symsg-msgno }|.
        ELSE.
          ls_fdlog-message =  lv_message.
        ENDIF.
        ls_fdlog-msgtype = ls_symsg-msgty.
        append( VALUE #( ( ls_fdlog ) ) ).
      CATCH cx_root INTO DATA(x).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_fdlog~s.
    add_message( iv_message = iv_message iv_msgtype = 'S' ).
  ENDMETHOD.


  METHOD zif_fdlog~send.
    TRY.
        DATA(lt_fdlog) = read_and_clear( ).
      CATCH cx_shm_attach_error INTO DATA(lx_attach_error).
        RAISE EXCEPTION TYPE zcx_fdlog
          EXPORTING
            textid   = zcx_fdlog=>cx_attach_error
            previous = lx_attach_error.
    ENDTRY.
    IF ( lt_fdlog IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_fdlog
        EXPORTING
          textid = zcx_fdlog=>cx_no_data.
    ENDIF.

    DATA(lo_rest) = zcl_fdlog_factory=>rest( zcl_fdlog_factory=>http( ) ).

    DATA(lo_request) = lo_rest->create_request_entity( ).
    DATA(lv_data) = /ui2/cl_json=>serialize( lt_fdlog ).
    REPLACE ALL OCCURRENCES OF '"TIME"' IN lv_data WITH '"time"'.
    lo_request->set_string_data( lv_data ).
    lo_request->set_content_type( if_rest_media_type=>gc_appl_json ).
    lo_request->set_header_field( iv_name = '~request_uri' iv_value = |/{ sy-sysid }.{ sy-mandt }.{ av_inst_name }| ) ##NO_TEXT.

    TRY.
        lo_rest->post( lo_request ).
      CATCH cx_rest_client_exception INTO DATA(lx_rest).
        TRY.
            append( lt_fdlog ).
          CATCH cx_shm_attach_error.
            "Do nothing
        ENDTRY.
        RAISE EXCEPTION TYPE zcx_fdlog
          EXPORTING
            textid   = zcx_fdlog=>cx_http_failed
            previous = lx_rest.
    ENDTRY.

    DATA(lv_status) = lo_rest->get_status( ).
    DATA(lv_response) = lo_rest->get_response_entity( )->get_string_data( ).
    IF ( NOT lv_status BETWEEN 200 AND 299 ).
      TRY.
          append( lt_fdlog ).
        CATCH cx_shm_attach_error.
          "Do nothing
      ENDTRY.
      RAISE EXCEPTION TYPE zcx_fdlog
        EXPORTING
          textid = zcx_fdlog=>cx_http_failed.
    ENDIF.
    rv_count = lines( lt_fdlog ).
  ENDMETHOD.


  METHOD zif_fdlog~w.
    add_message( iv_message = iv_message iv_msgtype = 'W' ).
  ENDMETHOD.


  METHOD zif_fdlog~x.
    add_message( iv_message = iv_message iv_msgtype = 'X' ).
  ENDMETHOD.

ENDCLASS.
