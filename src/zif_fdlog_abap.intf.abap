INTERFACE zif_fdlog_abap
  PUBLIC .
  METHODS:
    get_utc_timestamp
      RETURNING VALUE(rv_timestamp) TYPE tzonref-tstamps.
ENDINTERFACE.
