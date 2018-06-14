CLASS zcl_fdlog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_fdlog.
    ALIASES i FOR zif_fdlog~i.
    ALIASES s FOR zif_fdlog~s.
    ALIASES w FOR zif_fdlog~w.
    ALIASES e FOR zif_fdlog~e.
    ALIASES a FOR zif_fdlog~a.
    ALIASES x FOR zif_fdlog~x.
    ALIASES log FOR zif_fdlog~log.
    ALIASES send FOR zif_fdlog~send.

    CLASS-METHODS:
      class_constructor.

    METHODS:
      constructor
        IMPORTING
          iv_inst_name TYPE shm_inst_name DEFAULT cl_shm_area=>default_instance
          iv_upd_task  TYPE abap_bool DEFAULT abap_true.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_utc TYPE string VALUE 'UTC' ##NO_TEXT.

    CLASS-DATA: av_guid TYPE guid_16.

    DATA: av_inst_name TYPE shm_inst_name.

    METHODS:
      current_unix_time
        RETURNING VALUE(rv_timestamp) TYPE zif_fdlog=>tv_unixtime,
      create_fdlog
        RETURNING VALUE(rs_fdlog) TYPE zif_fdlog~ts_fdlog,
      attach_for_update
        RETURNING VALUE(r_result) TYPE REF TO zcl_fdlog_shr_area
        RAISING
                  cx_shm_attach_error,
      attach_for_read
        RETURNING VALUE(r_result) TYPE REF TO zcl_fdlog_shr_area
        RAISING
                  cx_shm_attach_error,
      detach
        IMPORTING
          i_shr_area TYPE REF TO zcl_fdlog_shr_area
        RAISING
          cx_shm_already_detached
          cx_shm_completion_error
          cx_shm_secondary_commit
          cx_shm_wrong_handle,
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

  METHOD send.
    TRY.
        DATA(lt_fdlog) = read_and_clear( ).
        IF ( lt_fdlog IS INITIAL ). RETURN. ENDIF.

        DATA(lo_rest) = zcl_fdlog_factory=>rest( zcl_fdlog_factory=>http( ) ).

        DATA(lo_request) = lo_rest->create_request_entity( ).
        DATA(lv_data) = /ui2/cl_json=>serialize( lt_fdlog ).
        REPLACE ALL OCCURRENCES OF '"TIME"' IN lv_data WITH '"time"'.
        lo_request->set_string_data( lv_data ).
        lo_request->set_content_type( if_rest_media_type=>gc_appl_json ).
        lo_request->set_header_field( iv_name = '~request_uri' iv_value = |/{ sy-sysid }.{ sy-mandt }.{ av_inst_name }| ) ##NO_TEXT.

        lo_rest->post( lo_request ).

        DATA(lv_status) = lo_rest->get_status( ).
        DATA(lv_response) = lo_rest->get_response_entity( )->get_string_data( ).
      CATCH cx_root INTO DATA(x).
    ENDTRY.
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

  METHOD create_fdlog.
    rs_fdlog-user = sy-uname.
    rs_fdlog-host = sy-host.
    rs_fdlog-program = sy-cprog.
    rs_fdlog-time = current_unix_time( ).
    rs_fdlog-corrid = av_guid.
  ENDMETHOD.

  METHOD attach_for_read.
    TRY.
        r_result = zcl_fdlog_shr_area=>attach_for_read( inst_name = av_inst_name ).
      CATCH cx_shm_no_active_version.
        WAIT UP TO 1 SECONDS.
        r_result = zcl_fdlog_shr_area=>attach_for_read( inst_name = av_inst_name ).
    ENDTRY.
  ENDMETHOD.

  METHOD attach_for_update.
    TRY.
        r_result = zcl_fdlog_shr_area=>attach_for_update( inst_name = av_inst_name ).
      CATCH cx_shm_no_active_version.
        WAIT UP TO 1 SECONDS.
        r_result = zcl_fdlog_shr_area=>attach_for_update( inst_name = av_inst_name ).
    ENDTRY.
  ENDMETHOD.

  METHOD detach.
    i_shr_area->detach_commit( ).
  ENDMETHOD.

  METHOD append.
    DATA(lo_shr_area) = attach_for_update( ).
    DATA(lo_shr_root) = CAST zcl_fdlog_shr_root( lo_shr_area->get_root( ) ).

    APPEND LINES OF it_fdlog TO lo_shr_root->data.

    detach( lo_shr_area ).
  ENDMETHOD.

  METHOD read.
    DATA(lo_shr_area) = attach_for_read( ).
    rt_fdlog = lo_shr_area->root->data.
    lo_shr_area->detach( ).
  ENDMETHOD.

  METHOD constructor.
    av_inst_name = iv_inst_name.

    IF ( iv_upd_task = abap_true ).
      SET UPDATE TASK LOCAL.
      CALL FUNCTION 'Z_FDLOG_SEND' IN UPDATE TASK
        EXPORTING
          im_inst_name = av_inst_name.
    ENDIF.
  ENDMETHOD.

  METHOD read_and_clear.
    DATA(lo_shr_area) = attach_for_update( ).
    DATA(lo_shr_root) = CAST zcl_fdlog_shr_root( lo_shr_area->get_root( ) ).

    rt_fdlog = lo_shr_root->data.
    CLEAR lo_shr_root->data.

    detach( lo_shr_area ).
  ENDMETHOD.

  METHOD zif_fdlog~i.
    add_message( iv_message = iv_message iv_msgtype = 'I' ).
  ENDMETHOD.

  METHOD zif_fdlog~s.
    add_message( iv_message = iv_message iv_msgtype = 'S' ).
  ENDMETHOD.

  METHOD zif_fdlog~w.
    add_message( iv_message = iv_message iv_msgtype = 'W' ).
  ENDMETHOD.

  METHOD zif_fdlog~e.
    add_message( iv_message = iv_message iv_msgtype = 'E' ).
  ENDMETHOD.

  METHOD zif_fdlog~a.
    add_message( iv_message = iv_message iv_msgtype = 'A' ).
  ENDMETHOD.

  METHOD zif_fdlog~x.
    add_message( iv_message = iv_message iv_msgtype = 'X' ).
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
        if ( ls_symsg-msgid is not INITIAL ).
          ls_fdlog-message = |{ lv_message }, Msg.Id:({ ls_symsg-msgid }){ ls_symsg-msgno }|.
        else.
          ls_fdlog-message =  lv_message.
        endif.
        ls_fdlog-msgtype = ls_symsg-msgty.
        append( VALUE #( ( ls_fdlog ) ) ).
      CATCH cx_root INTO DATA(x).
    ENDTRY.
  ENDMETHOD.

  METHOD add_message.
    TRY.
        DATA(ls_fdlog) = create_fdlog( ).
        ls_fdlog-message = iv_message.
        ls_fdlog-msgtype = iv_msgtype.
        append( VALUE #( ( ls_fdlog ) ) ).
      CATCH cx_root INTO DATA(x).
    ENDTRY.
  ENDMETHOD.

  METHOD class_constructor.
    av_guid = lcl_fdlog_factory=>abap( )->get_guid( ).
  ENDMETHOD.

ENDCLASS.
