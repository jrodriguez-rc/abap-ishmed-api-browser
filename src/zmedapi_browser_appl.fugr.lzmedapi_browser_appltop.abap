FUNCTION-POOL zmedapi_browser_appl.     "MESSAGE-ID ..

* INCLUDE LZISHMED_API_BROWSER_APPLD...      " Local class definition

CONSTANTS
  gc_program_id TYPE syrepid VALUE 'SAPLZMEDAPI_BROWSER_APPL'.

DATA:
  gv_ucomm_0100            TYPE syucomm,
  g_repid_sc_api_list_0100 TYPE syrepid VALUE 'SAPLN1SC',
  g_dynnr_sc_api_list_0100 TYPE sydynnr VALUE '0001'.
