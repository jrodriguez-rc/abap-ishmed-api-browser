CLASS zcl_medapi_gv_html_document DEFINITION
  PUBLIC
  INHERITING FROM cl_ish_gui_control_view
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS create_and_init_by_contview
      IMPORTING
        iv_element_name    TYPE n1gui_element_name
        ii_cb_destroyable  TYPE REF TO if_ish_cb_destroyable OPTIONAL
        iv_document_object TYPE doku_obj OPTIONAL
        iv_document_class  TYPE doku_id OPTIONAL
        io_layout          TYPE REF TO cl_ish_gui_control_layout OPTIONAL
        iv_processing_mode TYPE ish_vcode DEFAULT if_ish_gui_view=>co_vcode_display
        iv_ctrname         TYPE n1gui_element_name
        ii_parent_view     TYPE REF TO if_ish_gui_container_view
      RETURNING
        VALUE(ro_result)   TYPE REF TO zcl_medapi_gv_html_document
      RAISING
        cx_ish_static_handler.

    CLASS-METHODS create_and_init_by_dynpview
      IMPORTING
        iv_element_name    TYPE n1gui_element_name
        ii_cb_destroyable  TYPE REF TO if_ish_cb_destroyable OPTIONAL
        iv_document_object TYPE doku_obj OPTIONAL
        iv_document_class  TYPE doku_id OPTIONAL
        io_layout          TYPE REF TO cl_ish_gui_control_layout OPTIONAL
        iv_processing_mode TYPE ish_vcode DEFAULT if_ish_gui_view=>co_vcode_display
        iv_ctrname         TYPE n1gui_element_name
        ii_parent_view     TYPE REF TO if_ish_gui_dynpro_view
        iv_sdy_ctrname     TYPE n1gui_element_name
        iv_sdy_viewname    TYPE n1gui_element_name
      RETURNING
        VALUE(ro_result)   TYPE REF TO zcl_medapi_gv_html_document
      RAISING
        cx_ish_static_handler.

    METHODS set_document
      IMPORTING
        iv_document_object TYPE doku_obj
        iv_document_class  TYPE doku_id DEFAULT `TX`
      RAISING
        cx_ish_static_handler.

  PROTECTED SECTION.
    METHODS initialize
      IMPORTING
        iv_document_object TYPE doku_obj OPTIONAL
        iv_document_class  TYPE doku_id OPTIONAL
        ii_controller      TYPE REF TO if_ish_gui_controller
        ii_parent_view     TYPE REF TO if_ish_gui_container_view
        io_layout          TYPE REF TO cl_ish_gui_control_layout OPTIONAL
        iv_processing_mode TYPE ish_vcode DEFAULT if_ish_gui_view=>co_vcode_display
      RAISING
        cx_ish_static_handler.

    METHODS load_document
      IMPORTING
        iv_document_object TYPE doku_obj DEFAULT ``
        iv_document_class  TYPE doku_id DEFAULT `TX`
      RAISING
        cx_ish_static_handler.

    METHODS _create_control REDEFINITION.
    METHODS _first_display REDEFINITION.
    METHODS _refresh_display REDEFINITION.

  PRIVATE SECTION.
    DATA:
      gv_document_object TYPE doku_obj,
      gv_document_class  TYPE doku_id,
      go_html_viewer     TYPE REF TO cl_epss_html_viewer.

ENDCLASS.



CLASS zcl_medapi_gv_html_document IMPLEMENTATION.


  METHOD create_and_init_by_contview.

    DATA(lo_controller) = cl_ish_gc_simple=>create( i_element_name = iv_ctrname ir_cb_destroyable = ii_cb_destroyable ).

    ro_result = NEW #( i_element_name = iv_element_name ir_cb_destroyable = ii_cb_destroyable ).

    lo_controller->initialize(
        ir_parent_controller = COND #( WHEN ii_parent_view IS BOUND THEN ii_parent_view->get_controller( ) )
        ir_view              = ro_result
        i_vcode              = iv_processing_mode ).

    ro_result->initialize( ii_controller      = lo_controller
                           io_layout          = io_layout
                           iv_document_object = iv_document_object
                           iv_document_class  = iv_document_class
                           iv_processing_mode = iv_processing_mode
                           ii_parent_view     = ii_parent_view ).

  ENDMETHOD.


  METHOD create_and_init_by_dynpview.

    DATA(lr_sdy_custcont_ctr) =
        cl_ish_gc_sdy_custcont=>create_and_initialize(
            i_element_name          = iv_sdy_ctrname
            ir_cb_destroyable       = ii_cb_destroyable
            ir_parent_controller    = COND #( WHEN ii_parent_view IS BOUND THEN ii_parent_view->get_controller( ) )
            i_vcode                 = iv_processing_mode
            i_viewname_sdy_custcont = iv_sdy_viewname ).

    ro_result = create_and_init_by_contview( iv_element_name      = iv_element_name
                                             iv_document_object = iv_document_object
                                             iv_document_class  = iv_document_class
                                             ii_cb_destroyable  = ii_cb_destroyable
                                             io_layout          = io_layout
                                             iv_processing_mode = iv_processing_mode
                                             iv_ctrname         = iv_ctrname
                                             ii_parent_view     = lr_sdy_custcont_ctr->get_custcont_view( ) ).

  ENDMETHOD.


  METHOD set_document.

    load_document( iv_document_object = iv_document_object iv_document_class = iv_document_class ).

    gv_document_object = iv_document_object.
    gv_document_class  = iv_document_class.

  ENDMETHOD.


  METHOD load_document.

    DATA:
      lv_url  TYPE c LENGTH 5000,
      lt_html TYPE htmltable.

    TRY.
        DATA(lo_html_viewer) = CAST cl_gui_html_viewer( _create_control_on_demand( ) ).
      CATCH cx_sy_move_cast_error.
        CLEAR lo_html_viewer.
    ENDTRY.
    IF lo_html_viewer IS NOT BOUND.
      cl_ish_utl_exception=>raise_static( i_typ = 'E'
                                          i_kla = 'N1BASE'
                                          i_num = '030'
                                          i_mv1 = '1'
                                          i_mv2 = 'DISPLAY_DOCUMENT'
                                          i_mv3 = 'ZCL_MEDAPI_GV_HTML_DOCUMENT' ).
    ENDIF.

    lo_html_viewer->stop( EXCEPTIONS cntl_error = 1
                                     OTHERS     = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_medapi
        MESSAGE ID sy-msgid
        TYPE sy-msgty
        NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF iv_document_class IS NOT INITIAL AND iv_document_object IS NOT INITIAL.
      DATA(lv_document_object) = iv_document_object.
      DATA(lv_document_class)  = iv_document_class.
    ELSE.
      lv_document_object = gv_document_object.
      lv_document_class  = gv_document_class.
    ENDIF.

    CALL FUNCTION 'DOC_OBJECT_GET_HTML'
      EXPORTING
        docuid               = CONV char4( lv_document_class )
        docuobject           = lv_document_object
      TABLES
        html_page            = lt_html
      EXCEPTIONS
        docuobject_not_found = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_medapi
        MESSAGE ID sy-msgid
        TYPE sy-msgty
        NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_html_viewer->load_data( IMPORTING  assigned_url         = lv_url
                               CHANGING   data_table           = lt_html
                               EXCEPTIONS dp_invalid_parameter = 1
                                          dp_error_general     = 2
                                          cntl_error           = 3
                                          OTHERS               = 4 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_medapi
        MESSAGE ID sy-msgid
        TYPE sy-msgty
        NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    lo_html_viewer->show_data( EXPORTING  url                    = lv_url
                               EXCEPTIONS cntl_error             = 1
                                          cnht_error_not_allowed = 2
                                          cnht_error_parameter   = 3
                                          dp_error_general       = 4
                                          OTHERS                 = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_medapi
        MESSAGE ID sy-msgid
        TYPE sy-msgty
        NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD initialize.

    IF is_initialized( ) = abap_true OR
       is_in_initialization_mode( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_medapi
        MESSAGE ID 'N1BASE'
        TYPE 'E'
        NUMBER '030'
        WITH '1' 'INITIALIZE' 'ZCL_MEDAPI_GV_HTML_DOCUMENT'.
    ENDIF.

    IF ii_controller IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_medapi
        MESSAGE ID 'N1BASE'
        TYPE 'E'
        NUMBER '030'
        WITH '2' 'INITIALIZE' 'ZCL_MEDAPI_GV_HTML_DOCUMENT'.
    ENDIF.

    IF iv_document_class IS INITIAL OR iv_document_object IS INITIAL.
      gv_document_object = `ZMEDAPI_BROWSER`.
      gv_document_class  = `TX`.
    ELSE.
      gv_document_object = iv_document_object.
      gv_document_class  = iv_document_class.
    ENDIF.

    g_initialization_mode = abap_true.

    TRY.
        _init_control_view( ir_controller  = ii_controller
                            ir_parent_view = ii_parent_view
                            ir_layout      = io_layout
                            i_vcode        = iv_processing_mode ).
      CLEANUP.
        g_initialization_mode = abap_false.
    ENDTRY.

    g_initialization_mode = abap_false.
    g_initialized         = abap_true.

  ENDMETHOD.


  METHOD _create_control.

    TRY.
        DATA(li_parent_container_view) = CAST if_ish_gui_container_view( get_parent_view( ) ).
        DATA(lo_parent_container) = li_parent_container_view->get_container_for_child_view( me ).
      CATCH cx_sy_move_cast_error.
        CLEAR lo_parent_container.
    ENDTRY.

    IF lo_parent_container IS NOT BOUND.
      cl_ish_utl_exception=>raise_static( i_typ = 'E'
                                          i_kla = 'N1BASE'
                                          i_num = '030'
                                          i_mv1 = '1'
                                          i_mv2 = '_CREATE_CONTROL'
                                          i_mv3 = 'ZCL_MEDAPI_GV_HTML_DOCUMENT' ).
    ENDIF.

    go_html_viewer = NEW #( ).

    go_html_viewer->start_application( lo_parent_container ).

    rr_control = go_html_viewer->epss_html_viewer.

  ENDMETHOD.


  METHOD _first_display.

    load_document( ).

  ENDMETHOD.


  METHOD _refresh_display.

    load_document( ).

*    cl_gui_cfw=>update_view( ).

  ENDMETHOD.


ENDCLASS.
