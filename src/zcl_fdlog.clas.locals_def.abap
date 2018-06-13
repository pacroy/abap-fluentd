INTERFACE lif_fdlog_abap.
  METHODS:
    get_utc_timestamp
      RETURNING VALUE(rv_timestamp) TYPE tzonref-tstamps.
ENDINTERFACE.
