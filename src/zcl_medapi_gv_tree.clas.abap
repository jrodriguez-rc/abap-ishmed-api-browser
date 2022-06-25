CLASS zcl_medapi_gv_tree DEFINITION
  PUBLIC
  INHERITING FROM cl_ish_gui_tree_view
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS co_def_planhierarchy_ctrname TYPE n1gui_element_name VALUE 'PLANHIERARCHY_CTR'. "#EC NOTEXT
    CONSTANTS co_def_planhierarchy_viewname TYPE n1gui_element_name VALUE 'PLANHIERARCHY_VIEW'. "#EC NOTEXT

    CLASS-METHODS create_and_init_by_contview
      IMPORTING
        iv_element_name    TYPE n1gui_element_name DEFAULT co_def_planhierarchy_viewname
        ii_cb_destroyable  TYPE REF TO if_ish_cb_destroyable OPTIONAL
        ii_model           TYPE REF TO if_ish_gui_model
        io_layout          TYPE REF TO cl_ish_gui_tree_layout OPTIONAL
        iv_processing_mode TYPE ish_vcode DEFAULT co_vcode_display
        iv_ctrname         TYPE n1gui_element_name DEFAULT co_def_planhierarchy_ctrname
        ii_parent_view     TYPE REF TO if_ish_gui_container_view
      RETURNING
        VALUE(ro_result)   TYPE REF TO zcl_medapi_gv_tree
      RAISING
        cx_ish_static_handler.

  PROTECTED SECTION.
    METHODS initialize
      IMPORTING
        ii_controller      TYPE REF TO if_ish_gui_controller
        ii_parent_view     TYPE REF TO if_ish_gui_container_view
        ii_layout          TYPE REF TO cl_ish_gui_tree_layout OPTIONAL
        iv_processing_mode TYPE ish_vcode DEFAULT co_vcode_display
      RAISING
        cx_ish_static_handler.

    METHODS _load_layout REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_medapi_gv_tree IMPLEMENTATION.


  METHOD create_and_init_by_contview.

    IF ii_parent_view IS NOT BOUND.
      cl_ish_utl_exception=>raise_static( i_typ = 'E'
                                          i_kla = 'N1BASE'
                                          i_num = '030'
                                          i_mv1 = '1'
                                          i_mv2 = 'CREATE_AND_INIT_BY_CONTVIEW'
                                          i_mv3 = 'ZCL_MEDAPI_GV_TREE' ).
    ENDIF.

    DATA(lr_ctr) = cl_ish_gc_simple=>create( i_element_name = iv_ctrname ir_cb_destroyable = ii_cb_destroyable ).

    ro_result = NEW #( i_element_name = iv_element_name ir_cb_destroyable = ii_cb_destroyable ).

    lr_ctr->initialize( ir_parent_controller = COND #( WHEN ii_parent_view IS BOUND THEN ii_parent_view->get_controller( ) )
                        ir_model             = ii_model
                        ir_view              = ro_result
                        i_vcode              = iv_processing_mode ).

    ro_result->initialize( ii_controller      = lr_ctr
                           ii_parent_view     = ii_parent_view
                           ii_layout          = io_layout
                           iv_processing_mode = iv_processing_mode ).

  ENDMETHOD.


  METHOD initialize.

    DATA:
      l_system_client_edit  TYPE cccoractiv.

    IF is_initialized( ) OR is_in_initialization_mode( ).
      cl_ish_utl_exception=>raise_static( i_typ = 'E'
                                          i_kla = 'N1BASE'
                                          i_num = '030'
                                          i_mv1 = '1'
                                          i_mv2 = 'INITIALIZE'
                                          i_mv3 = 'ZCL_MEDAPI_GV_TREE' ).
    ENDIF.

    g_initialization_mode = abap_true.

    TRY.
        _init_tree_view( ir_controller  = ii_controller
                         ir_parent_view = ii_parent_view
                         ir_layout      = ii_layout
                         i_vcode        = iv_processing_mode
                         ir_outtab      = NEW zmedapi_t_list( ) ).
      CLEANUP.
        g_initialization_mode = abap_false.
    ENDTRY.

    g_initialization_mode = abap_false.
    g_initialized         = abap_true.

  ENDMETHOD.


  METHOD _load_layout.

    rr_layout = super->_load_layout( ir_controller  = ir_controller
                                     ir_parent_view = ir_parent_view
                                     i_layout_name  = i_layout_name
                                     i_username     = i_username ).
    IF rr_layout IS BOUND.
      RETURN.
    ENDIF.

    rr_layout = NEW cl_ish_gui_tree_layout( i_element_name         = get_element_name( )
                                            i_layout_name          = i_layout_name
                                            i_startup_expand_level = 2 ).

  ENDMETHOD.


ENDCLASS.
