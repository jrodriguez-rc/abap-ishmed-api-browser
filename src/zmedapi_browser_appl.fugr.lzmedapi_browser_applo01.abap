*----------------------------------------------------------------------*
***INCLUDE LZISHMED_API_BROWSER_APPLO01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  before_pbo_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE before_pbo_0100 OUTPUT.

  cl_ish_gui_dynpro_connector=>before_pbo( sy-repid ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  pbo_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  cl_ish_gui_dynpro_connector=>pbo( sy-repid ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  before_pbo_sc_demo_report_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE before_pbo_sc_demo_report_0100 OUTPUT.

  cl_ish_gui_dynpro_connector=>before_call_subscreen( EXPORTING i_repid           = sy-repid
                                                                i_subscreen_name  = 'SC_DEMO_REPORT'
                                                      IMPORTING e_subscreen_repid = g_repid_sc_demo_report_0100
                                                                e_subscreen_dynnr = g_dynnr_sc_demo_report_0100 ).

ENDMODULE.                 " before_pbo_sc_api_list_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  before_pbo_sc_document_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE before_pbo_sc_document_0100 OUTPUT.

  cl_ish_gui_dynpro_connector=>before_call_subscreen( EXPORTING i_repid           = sy-repid
                                                                i_subscreen_name  = 'SC_DOCUMENT'
                                                      IMPORTING e_subscreen_repid = g_repid_sc_document_0100
                                                                e_subscreen_dynnr = g_dynnr_sc_document_0100 ).

ENDMODULE.                 " before_pbo_sc_api_list_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  after_pbo_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE after_pbo_0100 OUTPUT.

  cl_ish_gui_dynpro_connector=>after_pbo( sy-repid ).

  CLEAR gv_ucomm_0100.

ENDMODULE.
