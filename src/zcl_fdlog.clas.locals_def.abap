INTERFACE lif_fdlog_abap.
  METHODS:
    get_utc_timestamp
      RETURNING VALUE(rv_timestamp) TYPE tzonref-tstamps,
    get_guid
      RETURNING VALUE(rv_guid) TYPE guid_16.
ENDINTERFACE.
