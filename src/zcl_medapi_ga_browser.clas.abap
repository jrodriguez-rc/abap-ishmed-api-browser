CLASS zcl_medapi_ga_browser DEFINITION
  PUBLIC
  INHERITING FROM cl_ish_gui_application
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      gc_dynpro_number      TYPE dynnr VALUE `0100`,
      gc_program_id         TYPE repid VALUE `SAPLZMEDAPI_BROWSER_APPL`,
      gc_dockcont_extension TYPE n1gui_dockcont_extension VALUE 400.

    CONSTANTS:
      BEGIN OF gc_controller,
        tree          TYPE n1gui_element_name VALUE `CTR_TREE_API`,
        demo_report   TYPE n1gui_element_name VALUE `CTR_DEMO_REPORT`,
        documentation TYPE n1gui_element_name VALUE `CTR_DOCUMENT`,
      END OF gc_controller.

    CONSTANTS:
      BEGIN OF gc_viewname,
        tree          TYPE n1gui_element_name VALUE `SC_VIEW_TREE_API`,
        demo_report   TYPE n1gui_element_name VALUE `SC_DEMO_REPORT`,
        documentation TYPE n1gui_element_name VALUE `SC_DOCUMENT`,
      END OF gc_viewname.

    CLASS-METHODS run_trx.

    CLASS-METHODS execute
      IMPORTING
        io_layout TYPE REF TO cl_ish_gui_appl_layout OPTIONAL
      RAISING
        cx_ish_static_handler.

  PROTECTED SECTION.
    METHODS create_model
      RAISING
        cx_ish_static_handler.

    METHODS get_documentation_view
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_medapi_gv_html_document.

    METHODS get_model
      RETURNING
        VALUE(ri_result) TYPE REF TO if_ish_gui_table_model.

    METHODS get_tree_controller
      RETURNING
        VALUE(rr_result) TYPE REF TO if_ish_gui_controller.

    METHODS load_tree_view
      IMPORTING
        ir_main_controller TYPE REF TO if_ish_gui_main_controller
      RETURNING
        VALUE(ro_result)   TYPE REF TO zcl_medapi_gv_tree
      RAISING
        cx_ish_static_handler.

    METHODS load_documentation_view
      IMPORTING
        ir_main_controller TYPE REF TO if_ish_gui_main_controller
      RAISING
        cx_ish_static_handler.

    METHODS _create_main_controller REDEFINITION.
    METHODS _get_okcodereq_by_controlev REDEFINITION.
    METHODS _init_appl REDEFINITION.
    METHODS _run REDEFINITION.

  PRIVATE SECTION.
    DATA:
      mo_model TYPE REF TO zcl_medapi_tree_model.

    METHODS on_selected_api
        FOR EVENT selected_api OF zcl_medapi_gv_tree
      IMPORTING
        ei_api
        sender.

ENDCLASS.



CLASS zcl_medapi_ga_browser IMPLEMENTATION.


  METHOD run_trx.

    DATA:
      lr_errorhandler TYPE REF TO cl_ishmed_errorhandling.

    TRY.
        execute( ).
      CATCH cx_ish_static_handler INTO DATA(lx_exception).
        cl_ish_utl_base=>collect_messages_by_exception( EXPORTING i_exceptions    = lx_exception
                                                        CHANGING  cr_errorhandler = lr_errorhandler ).
        lr_errorhandler->display_messages( ).
    ENDTRY.

  ENDMETHOD.


  METHOD execute.

    DATA(lr_application) = NEW zcl_medapi_ga_browser( ).

    lr_application->_init_appl( ir_layout = io_layout ).

    lr_application->run( ).

  ENDMETHOD.


  METHOD _create_main_controller.

    rr_main_controller = get_main_controller( ).
    IF rr_main_controller IS BOUND.
      RETURN.
    ENDIF.

    TRY.

        DATA(lo_main_controller) =
            cl_ish_gc_main_simple=>create(
                i_element_name = if_ish_gui_main_controller=>co_def_main_controller_name ).

        DATA(lo_main_view) =
            cl_ish_gv_mdy_simple=>create(
                i_element_name = if_ish_gui_mdy_view=>co_def_main_view_name ).

        DATA(lo_titlebar) =
            cl_ish_gui_mdy_titlebar=>create(
                i_element_name = cl_ish_gui_mdy_titlebar=>co_def_titlebar_name
                i_title        = 'TITLE_SIMPLE'
                i_repid        = gc_program_id ).

        DATA(lo_pfstatus) =
            cl_ish_gui_mdy_pfstatus=>create(
                i_element_name = cl_ish_gui_mdy_pfstatus=>co_def_pfstatus_name
                i_pfkey        = 'STATUS_SIMPLE'
                i_repid        = gc_program_id ).

        lo_main_controller->initialize( ir_application = me ir_view = lo_main_view ir_model = get_model( ) ).

        lo_main_view->initialize( ir_controller = lo_main_controller
                                  i_repid       = gc_program_id
                                  i_dynnr       = gc_dynpro_number
                                  ir_titlebar   = lo_titlebar
                                  ir_pfstatus   = lo_pfstatus
                                  i_vcode       = g_vcode ).

        load_tree_view( lo_main_controller ).

        load_documentation_view( lo_main_controller ).

      CLEANUP.

        IF lo_main_controller IS BOUND.
          lo_main_controller->destroy( ).
        ENDIF.
        IF lo_main_view IS BOUND.
          lo_main_view->destroy( ).
        ENDIF.

    ENDTRY.

    rr_main_controller = lo_main_controller.

  ENDMETHOD.


  METHOD _get_okcodereq_by_controlev.

    rr_okcode_request = super->_get_okcodereq_by_controlev( ir_control_event ).

    IF rr_okcode_request IS BOUND OR ir_control_event IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lv_create_okcode_request) =
        xsdbool( ir_control_event->get_fcode( ) = cl_ish_gui_tree_event=>co_fcode_item_double_click
              OR ir_control_event->get_fcode( ) = cl_ish_gui_tree_event=>co_fcode_node_double_click ).

    IF lv_create_okcode_request = abap_true.
      rr_okcode_request = cl_ish_gui_okcode_request=>create_by_control_event( ir_sender        = me
                                                                              ir_processor     = me
                                                                              ir_control_event = ir_control_event ).
    ENDIF.

  ENDMETHOD.


  METHOD _init_appl.

    IF is_initialized( ) OR is_in_initialization_mode( ).
      cl_ish_utl_exception=>raise_static( i_typ = 'E'
                                          i_kla = 'N1BASE'
                                          i_num = '030'
                                          i_mv1 = '1'
                                          i_mv2 = '_INIT_APPL'
                                          i_mv3 = 'ZCL_MEDAPI_GA_BROWSER' ).
    ENDIF.

    g_initialization_mode = abap_true.

    TRY.

        create_model( ).

        DATA(lo_layout) =
          COND #( WHEN ir_layout IS BOUND THEN ir_layout
                                          ELSE NEW cl_ish_gui_appl_layout( i_use_msg_viewer = abap_true ) ).

        super->_init_appl( i_vcode = i_vcode ir_layout = lo_layout ).

      CLEANUP.

        g_initialization_mode = abap_false.

    ENDTRY.

    g_initialization_mode = abap_false.
    g_initialized         = abap_true.

  ENDMETHOD.


  METHOD _run.

    CALL FUNCTION 'ZMEDAPI_BROWSER_APPL_MDY'.

  ENDMETHOD.


  METHOD create_model.

    IF mo_model IS BOUND.
      RETURN.
    ENDIF.

    mo_model = NEW #( ).

  ENDMETHOD.


  METHOD get_model.

    ri_result = mo_model.

  ENDMETHOD.


  METHOD load_tree_view.

    IF ir_main_controller IS NOT BOUND OR get_tree_controller( ) IS BOUND.
      RETURN.
    ENDIF.

    DATA(li_main_view) = ir_main_controller->get_mdy_view( ).

    DATA(lo_layout) = cl_ish_gui_dockcont_layout=>load_or_create( ir_application = me
                                                                  i_element_name = gc_viewname-tree
                                                                  i_extension    = gc_dockcont_extension ).

    DATA(lo_controller) = cl_ish_gc_simple=>create( i_element_name = gc_controller-tree ).

    DATA(lo_tree_containter_view) = cl_ish_gui_dockcont_view=>create( i_element_name = gc_viewname-tree ).

    lo_controller->initialize( ir_parent_controller = ir_main_controller
                               ir_model             = NEW zcl_medapi_tree_model( )
                               ir_view              = lo_tree_containter_view
                               i_vcode              = g_vcode ).

    lo_tree_containter_view->initialize( ir_controller  = lo_controller
                                         ir_parent_view = li_main_view
                                         ir_layout      = lo_layout
                                         i_vcode        = g_vcode ).

    DATA(lo_view) = zcl_medapi_gv_tree=>create_and_init_by_contview( ii_model           = get_model( )
                                                                     iv_processing_mode = g_vcode
                                                                     ii_parent_view     = lo_tree_containter_view ).

    SET HANDLER on_selected_api FOR lo_view ACTIVATION abap_true.

    ro_result = lo_view.

  ENDMETHOD.


  METHOD get_tree_controller.

    DATA(lr_main_controller) = get_main_controller( ).
    IF lr_main_controller IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        rr_result = CAST #( lr_main_controller->get_child_controller_by_name( gc_controller-tree ) ).
      CATCH cx_sy_move_cast_error.
        CLEAR rr_result.
    ENDTRY.

  ENDMETHOD.


  METHOD load_documentation_view.

    zcl_medapi_gv_html_document=>create_and_init_by_dynpview(
        iv_element_name    = gc_viewname-documentation
        iv_ctrname         = gc_controller-documentation
        ii_parent_view     = ir_main_controller->get_mdy_view( )
        iv_sdy_ctrname     = gc_controller-documentation
        iv_sdy_viewname    = gc_viewname-documentation ).

  ENDMETHOD.


  METHOD get_documentation_view.

    DATA(lo_documentation_main_view) = get_main_view( )->get_child_view_by_name( gc_viewname-documentation ).

    DATA(lr_custom_view) = lo_documentation_main_view->get_child_view_by_name( cl_ish_gv_sdy_custcont=>co_def_viewname_custcont ).

    ro_result = CAST #( lr_custom_view->get_child_view_by_name( gc_viewname-documentation ) ).

  ENDMETHOD.


  METHOD on_selected_api.

    IF ei_api IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lo_view) = get_documentation_view( ).

    ei_api->if_ishmed_api_documentation~get( IMPORTING e_id     = DATA(lv_object_class)
                                                       e_object = DATA(lv_object_object) ).

    TRY.
        lo_view->set_document( iv_document_object = lv_object_object iv_document_class = lv_object_class ).
      CATCH cx_ish_static_handler INTO DATA(lx_handler).
        _collect_exception( lx_handler ).
    ENDTRY.

  ENDMETHOD.


ENDCLASS.
