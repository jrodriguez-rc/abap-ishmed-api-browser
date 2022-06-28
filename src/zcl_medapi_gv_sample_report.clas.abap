CLASS zcl_medapi_gv_sample_report DEFINITION
  PUBLIC
  INHERITING FROM cl_ish_gui_grid_view
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS gc_def_ctrname TYPE n1gui_element_name VALUE `CTR_SAMPLE_REPORT`.
    CONSTANTS gc_def_sdy_ctrname TYPE n1gui_element_name VALUE `SDY_SAMPLE_REPORT_CTR`.
    CONSTANTS gc_def_sdy_viewname TYPE n1gui_element_name VALUE `SDY_SAMPLE_REPORT_VIEWNAME`.
    CONSTANTS gc_def_viewname TYPE n1gui_element_name VALUE `SAMPLE_REPORT_VIEWNAME`.

    CLASS-METHODS create_and_init_by_contview
      IMPORTING
        ii_api             TYPE REF TO if_ishmed_api
        iv_element_name    TYPE n1gui_element_name DEFAULT gc_def_viewname
        ii_cb_destroyable  TYPE REF TO if_ish_cb_destroyable OPTIONAL
        io_layout          TYPE REF TO cl_ish_gui_grid_layout OPTIONAL
        iv_processing_mode TYPE ish_vcode DEFAULT co_vcode_display
        iv_ctrname         TYPE n1gui_element_name DEFAULT gc_def_ctrname
        ii_parent_view     TYPE REF TO if_ish_gui_container_view
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_medapi_gv_sample_report
      RAISING
        cx_ish_static_handler.

    CLASS-METHODS create_and_init_by_dynpview
      IMPORTING
        ii_api             TYPE REF TO if_ishmed_api
        iv_element_name    TYPE n1gui_element_name DEFAULT gc_def_viewname
        ii_cb_destroyable  TYPE REF TO if_ish_cb_destroyable OPTIONAL
        io_layout          TYPE REF TO cl_ish_gui_grid_layout OPTIONAL
        iv_processing_mode TYPE ish_vcode DEFAULT co_vcode_display
        iv_ctrname         TYPE n1gui_element_name DEFAULT gc_def_ctrname
        ii_parent_view     TYPE REF TO if_ish_gui_dynpro_view
        iv_sdy_ctrname     TYPE n1gui_element_name DEFAULT gc_def_sdy_ctrname
        iv_sdy_viewname    TYPE n1gui_element_name DEFAULT gc_def_sdy_viewname
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_medapi_gv_sample_report
      RAISING
        cx_ish_static_handler.

  PROTECTED SECTION.
    METHODS initialize
      IMPORTING
        ii_api             TYPE REF TO if_ishmed_api
        ii_controller      TYPE REF TO if_ish_gui_controller
        ii_parent_view     TYPE REF TO if_ish_gui_container_view
        io_layout          TYPE REF TO cl_ish_gui_grid_layout
        iv_processing_mode TYPE ish_vcode DEFAULT co_vcode_display
      RAISING
        cx_ish_static_handler.

    METHODS get_api
      RETURNING
        VALUE(ri_model) TYPE REF TO if_ishmed_api.

    METHODS generate_model
      RETURNING
        VALUE(ri_model) TYPE REF TO if_ish_gui_table_model
      RAISING
        cx_ish_static_handler.

    METHODS get_icon_document
      RETURNING
        VALUE(rv_result) TYPE n1icon_document
      RAISING
        zcx_medapi.

    METHODS get_icon_source_code
      RETURNING
        VALUE(rv_result) TYPE zmedapi_source_code_icon
      RAISING
        zcx_medapi.

    METHODS get_icon_execute
      RETURNING
        VALUE(rv_result) TYPE zmedapi_execute_icon
      RAISING
        zcx_medapi.

    METHODS get_report_selected
      RETURNING
        VALUE(rv_result) TYPE progname
      RAISING
        cx_ish_static_handler.

    METHODS function_open_documentation
      RAISING
        cx_ish_static_handler.

    METHODS function_execute
      RAISING
        cx_ish_static_handler.

    METHODS function_open_source_code
      RAISING
        cx_ish_static_handler.

    METHODS _build_fcat REDEFINITION.
    METHODS _build_layout REDEFINITION.
    METHODS _get_main_model REDEFINITION.
    METHODS _own_cmd REDEFINITION.

  PRIVATE SECTION.
    DATA:
      mi_model TYPE REF TO if_ish_gui_table_model,
      gi_api   TYPE REF TO if_ishmed_api.

ENDCLASS.



CLASS zcl_medapi_gv_sample_report IMPLEMENTATION.


  METHOD create_and_init_by_contview.

    DATA(lo_controller) = cl_ish_gc_simple=>create( i_element_name = iv_ctrname ir_cb_destroyable = ii_cb_destroyable ).

    ro_instance = NEW #( i_element_name = iv_element_name ir_cb_destroyable = ii_cb_destroyable ).

    lo_controller->initialize( ir_parent_controller = COND #( WHEN ii_parent_view IS BOUND
                                                                  THEN ii_parent_view->get_controller( ) )
                               ir_view              = ro_instance
                               i_vcode              = iv_processing_mode ).

    ro_instance->initialize( ii_api             = ii_api
                             ii_controller      = lo_controller
                             ii_parent_view     = ii_parent_view
                             io_layout          = io_layout
                             iv_processing_mode = iv_processing_mode ).

  ENDMETHOD.


  METHOD create_and_init_by_dynpview.

    DATA(lr_sdy_custcont_ctr) =
        cl_ish_gc_sdy_custcont=>create_and_initialize(
            i_element_name          = iv_sdy_ctrname
            ir_cb_destroyable       = ii_cb_destroyable
            ir_parent_controller    = COND #( WHEN ii_parent_view IS BOUND THEN ii_parent_view->get_controller( ) )
            i_vcode                 = iv_processing_mode
            i_viewname_sdy_custcont = iv_sdy_viewname ).

    ro_instance = create_and_init_by_contview( ii_api             = ii_api
                                               iv_element_name    = iv_element_name
                                               ii_cb_destroyable  = ii_cb_destroyable
                                               io_layout          = io_layout
                                               iv_processing_mode = iv_processing_mode
                                               iv_ctrname         = iv_ctrname
                                               ii_parent_view     = lr_sdy_custcont_ctr->get_custcont_view( ) ).

  ENDMETHOD.


  METHOD initialize.

    IF is_initialized( ) OR is_in_initialization_mode( ).
      RAISE EXCEPTION TYPE zcx_medapi
        EXPORTING
          is_textid = zcx_medapi=>msg_method_error
          iv_text1  = `1`
          iv_text2  = `INITIALIZE`
          iv_text3  = `ZCL_MEDAPI_GV_SAMPLE_REPORT`.
    ENDIF.

    g_initialization_mode = abap_true.

    gi_api = ii_api.

    TRY.
        mi_model = generate_model( ).
        _init_grid_view( ir_controller  = ii_controller
                         ir_parent_view = ii_parent_view
                         ir_layout      = io_layout
                         i_vcode        = iv_processing_mode
                         ir_outtab      = NEW zmedapi_t_sample_report( ) ).
      CLEANUP.
        g_initialization_mode = abap_false.
    ENDTRY.

    g_initialization_mode = abap_false.
    g_initialized         = abap_true.

  ENDMETHOD.


  METHOD get_api.

    ri_model = gi_api.

  ENDMETHOD.


  METHOD generate_model.

    DATA:
      lt_text    TYPE textpool_t,
      lt_reports TYPE zmedapi_t_sample_report.

    get_api( )->if_ishmed_api_documentation~get_program( IMPORTING et_value = DATA(lt_programs) ).

    DATA(lv_icon_document) = get_icon_document( ).
    DATA(lv_icon_execute) = get_icon_execute( ).
    DATA(lv_icon_source_code) = get_icon_source_code( ).

    LOOP AT lt_programs INTO DATA(lv_program).
      CLEAR lt_text.

      READ TEXTPOOL lv_program INTO lt_text LANGUAGE sy-langu.

      INSERT
        VALUE #( program_name     = lv_program
                 description      = COND #( WHEN line_exists( lt_text[ id = `R` ] ) THEN lt_text[ id = `R` ]-entry )
                 icon_document    = lv_icon_document
                 icon_execute     = lv_icon_execute
                 icon_source_code = lv_icon_source_code )
        INTO TABLE lt_reports.

    ENDLOOP.

    ri_model = cl_ish_gm_table_simple=>create_by_table( lt_reports ).

  ENDMETHOD.


  METHOD get_icon_document.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = `ICON_DOCUMENT`
        info                  = 'Open documentation for sample report'(002)
      IMPORTING
        result                = rv_result
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_medapi
        EXPORTING
          is_textid = zcx_medapi=>get_system_textid( )
          iv_text1  = CONV #( sy-msgv1 )
          iv_text2  = CONV #( sy-msgv2 )
          iv_text3  = CONV #( sy-msgv3 )
          iv_text4  = CONV #( sy-msgv4 ).
    ENDIF.

  ENDMETHOD.


  METHOD get_icon_execute.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = `ICON_EXECUTE_OBJECT`
        info                  = 'Execute sample report'(001)
      IMPORTING
        result                = rv_result
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_medapi
        EXPORTING
          is_textid = zcx_medapi=>get_system_textid( )
          iv_text1  = CONV #( sy-msgv1 )
          iv_text2  = CONV #( sy-msgv2 )
          iv_text3  = CONV #( sy-msgv3 )
          iv_text4  = CONV #( sy-msgv4 ).
    ENDIF.

  ENDMETHOD.


  METHOD get_icon_source_code.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = `ICON_ABAP_LOCAL`
        info                  = 'Open sample report source code'(003)
      IMPORTING
        result                = rv_result
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_medapi
        EXPORTING
          is_textid = zcx_medapi=>get_system_textid( )
          iv_text1  = CONV #( sy-msgv1 )
          iv_text2  = CONV #( sy-msgv2 )
          iv_text3  = CONV #( sy-msgv3 )
          iv_text4  = CONV #( sy-msgv4 ).
    ENDIF.

  ENDMETHOD.


  METHOD _build_fcat.

    rt_fcat = super->_build_fcat( ).

    LOOP AT rt_fcat ASSIGNING FIELD-SYMBOL(<ls_field>).

      CASE <ls_field>-fieldname.
        WHEN `ICON_DOCUMENT` OR `ICON_EXECUTE` OR `ICON_SOURCE_CODE`.
          <ls_field>-hotspot   = abap_true.
          <ls_field>-icon      = abap_true.
          <ls_field>-outputlen = 4.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD _build_layout.

    rs_layo = super->_build_layout( ).

    rs_layo-grid_title = get_api( )->if_ishmed_api_documentation~get_description( ).
    rs_layo-no_rowmark = abap_true.

  ENDMETHOD.


  METHOD _get_main_model.

    rr_main_model = mi_model.

  ENDMETHOD.


  METHOD _own_cmd.

    CASE ir_grid_event->get_fcode( ).
      WHEN cl_ish_gui_grid_event=>co_fcode_hotspot_click.

        CASE ir_grid_event->get_col_fieldname( ).
          WHEN `ICON_DOCUMENT`.
            function_open_documentation( ).

          WHEN `ICON_EXECUTE`.
            function_execute( ).

          WHEN `ICON_SOURCE_CODE`.
            function_open_source_code( ).

          WHEN OTHERS.
            r_cmdresult = super->_own_cmd( ir_grid_event = ir_grid_event ir_orig_request = ir_orig_request ).
            RETURN.

        ENDCASE.

      WHEN OTHERS.
        r_cmdresult = super->_own_cmd( ir_grid_event = ir_grid_event ir_orig_request = ir_orig_request ).
        RETURN.

    ENDCASE.

    r_cmdresult = co_cmdresult_processed.

  ENDMETHOD.


  METHOD get_report_selected.

    TRY.
        DATA(li_model) = CAST if_ish_gui_structure_model( get_selected_model( ) ).
      CATCH cx_sy_move_cast_error.
        CLEAR li_model.
    ENDTRY.

    IF li_model IS NOT BOUND.
      RETURN.
    ENDIF.

    li_model->get_field_content( EXPORTING i_fieldname = `PROGRAM_NAME`
                                 CHANGING  c_content   = rv_result ).

  ENDMETHOD.


  METHOD function_open_documentation.

    DATA(lv_report_name) = get_report_selected( ).
    IF lv_report_name IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'DOCS'
        object_name   = lv_report_name
        object_type   = 'PROG'
        in_new_window = abap_true.

  ENDMETHOD.


  METHOD function_execute.

    DATA(lv_report_name) = get_report_selected( ).
    IF lv_report_name IS INITIAL.
      RETURN.
    ENDIF.

*    SUBMIT lv_report AND RETURN.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = `TEST`
        object_name   = lv_report_name
        object_type   = 'PROG'
        in_new_window = abap_true.

  ENDMETHOD.


  METHOD function_open_source_code.

    DATA(lv_report_name) = get_report_selected( ).
    IF lv_report_name IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = lv_report_name
        object_type   = 'PROG'
        in_new_window = abap_true.

  ENDMETHOD.


ENDCLASS.
