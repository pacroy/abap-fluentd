# Fluentd client in ABAP

Clone or import using [abapGit](https://github.com/larshp/abapGit)

## Sample Usage 

```abap
CLASS zcl_zhello_dpc_ext IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ao_developer = NEW zcl_hello_developer_persist( ).
    ao_fdlog = NEW zcl_fdlog( iv_inst_name = 'ZHELLO_ODATA' ).
  ENDMETHOD.

  METHOD developerset_get_entityset.

    ao_fdlog->i( |GET { iv_entity_set_name }| ).

    et_entityset = ao_developer->select_developer_set( ).

    COMMIT WORK.
  ENDMETHOD.

  METHOD developerset_get_entity.
    ao_fdlog->i( |GET { iv_entity_name }| ).
    ao_fdlog->i( |Key: { /ui2/cl_json=>serialize( it_key_tab ) } | ).

    DATA(lo_message) = mo_context->get_message_container( ).

    er_entity = ao_developer->select_developer( CONV #( it_key_tab[ name = 'ID' ]-value ) ).
    IF ( er_entity IS INITIAL ).
      lo_message->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Developer not found' ).

      ao_fdlog->e( |Developer not found| ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid            = /iwbep/cx_mgw_busi_exception=>resource_not_found
          message_container = lo_message.
    ENDIF.

    COMMIT WORK.
  ENDMETHOD.

ENDCLASS.
```

**DO NOT FORGET TO COMMIT WORK**

![](/img/kibana_abap_fluentd.jpg)