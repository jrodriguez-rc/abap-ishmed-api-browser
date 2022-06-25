*----------------------------------------------------------------------*
***INCLUDE LZISHMED_API_BROWSER_APPLO01.
*----------------------------------------------------------------------*



*&---------------------------------------------------------------------*
*&      Module  before_pbo_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE before_pbo_0100 OUTPUT.

  cl_ish_gui_dynpro_connector=>before_pbo( i_repid = gc_program_id ).

ENDMODULE.                 " before_pbo_0100  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  pbo_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  cl_ish_gui_dynpro_connector=>pbo( i_repid = gc_program_id ).

ENDMODULE.                 " pbo_0100  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  before_pbo_sc_api_list_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE before_pbo_sc_api_list_0100 OUTPUT.

  CALL METHOD cl_ish_gui_dynpro_connector=>before_call_subscreen
    EXPORTING
      i_repid           = gc_program_id
      i_subscreen_name  = 'SC_API_LIST'
    IMPORTING
      e_subscreen_repid = g_repid_sc_api_list_0100
      e_subscreen_dynnr = g_dynnr_sc_api_list_0100.

ENDMODULE.                 " before_pbo_sc_api_list_0100  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  after_pbo_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE after_pbo_0100 OUTPUT.

  cl_ish_gui_dynpro_connector=>after_pbo( i_repid = gc_program_id ).

  CLEAR: gv_ucomm_0100.

ENDMODULE.                 " after_pbo_0100  OUTPUT
