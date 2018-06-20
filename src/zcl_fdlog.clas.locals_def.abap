INTERFACE lif_fdlog_abap.
  METHODS:
    get_utc_timestamp
      RETURNING VALUE(rv_timestamp) TYPE tzonref-tstamps,
    get_guid
      RETURNING VALUE(rv_guid) TYPE guid_16.
ENDINTERFACE.

INTERFACE lif_fdlog_shr_area.
  METHODS:
    attach_for_read
      IMPORTING inst_name     TYPE shm_inst_name DEFAULT cl_shm_area=>default_instance
                  PREFERRED PARAMETER inst_name
      RETURNING VALUE(handle) TYPE REF TO zcl_fdlog_shr_area
      RAISING
                cx_shm_attach_error,
    attach_for_update
      IMPORTING inst_name     TYPE shm_inst_name DEFAULT cl_shm_area=>default_instance
                attach_mode   TYPE shm_attach_mode DEFAULT cl_shm_area=>attach_mode_default
                wait_time     TYPE i DEFAULT 0
                  PREFERRED PARAMETER inst_name
      RETURNING VALUE(handle) TYPE REF TO zcl_fdlog_shr_area
      RAISING
                cx_shm_attach_error ,
    get_root
      RETURNING VALUE(root) TYPE REF TO object
      RAISING   cx_shm_already_detached ,
    detach
      RAISING cx_shm_detach_error .
ENDINTERFACE.
