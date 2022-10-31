CLASS zcl_app_scanner_delimiter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS set_prev_delimiter
      IMPORTING
        it_source         TYPE sourcetable
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext
        ir_token_ext      TYPE REF TO zapp_s_stokesx_ext
      RAISING
        zcx_app_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS set_delimiter_from_source
      IMPORTING
        it_source         TYPE sourcetable
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext
        ir_token_ext      TYPE REF TO zapp_s_stokesx_ext
      RAISING
        zcx_app_exception.

    METHODS get_delimiter_from_source
      IMPORTING
        it_source           TYPE sourcetable
        ir_prev_token_ext   TYPE REF TO zapp_s_stokesx_ext
        ir_token_ext        TYPE REF TO zapp_s_stokesx_ext
      RETURNING
        VALUE(rt_delimiter) TYPE zapp_t_delimiter
      RAISING
        zcx_app_exception.
    METHODS get_multi_line_delimiter
      IMPORTING
        ir_source           TYPE REF TO string
        iv_offset           TYPE i
        ir_token_ext        TYPE REF TO zapp_s_stokesx_ext
        ir_prev_token_ext   TYPE REF TO zapp_s_stokesx_ext
        it_source           TYPE sourcetable
      RETURNING
        VALUE(rt_delimiter) TYPE zapp_t_delimiter.
    METHODS get_single_line_delimiter
      IMPORTING
        ir_source           TYPE REF TO string
        ir_token_ext        TYPE REF TO zapp_s_stokesx_ext
        iv_offset           TYPE i
      RETURNING
        VALUE(rv_delimiter) TYPE string.
    METHODS get_delimiter_until_end_of_src
      IMPORTING
        ir_source           TYPE REF TO string
        iv_offset           TYPE i
      RETURNING
        VALUE(rv_delimiter) TYPE zapp_d_delimiter.

ENDCLASS.



CLASS zcl_app_scanner_delimiter IMPLEMENTATION.


  METHOD get_delimiter_from_source.
    DATA lr_source TYPE REF TO string.
    DATA lv_offset TYPE i.
    DATA lv_delimiter TYPE string.
    READ TABLE it_source REFERENCE INTO lr_source
    INDEX ir_prev_token_ext->row.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_app_exception
        MESSAGE ID 'ZAPP_MC_PRETTY_PRINT'
        TYPE 'E'
        NUMBER '004'
        WITH
        ir_prev_token_ext->str
        ir_prev_token_ext->row.
    ENDIF.

    lv_offset = ir_prev_token_ext->col +  ir_prev_token_ext->len.
    IF ir_token_ext IS INITIAL.

      lv_delimiter = get_delimiter_until_end_of_src(
        ir_source = lr_source
        iv_offset = lv_offset ).
      INSERT lv_delimiter INTO TABLE rt_delimiter.
      RETURN.
    ENDIF.

    IF ir_token_ext->row = ir_prev_token_ext->row.
      lv_delimiter = get_single_line_delimiter(
        ir_source    = lr_source
        ir_token_ext = ir_token_ext
        iv_offset    = lv_offset ).
      INSERT lv_delimiter INTO TABLE rt_delimiter.
      RETURN.
    ENDIF.

    rt_delimiter = get_multi_line_delimiter(
      ir_source         = lr_source
      iv_offset         = lv_offset
      ir_token_ext      = ir_token_ext
      ir_prev_token_ext = ir_prev_token_ext
      it_source         = it_source ).

  ENDMETHOD.


  METHOD get_delimiter_until_end_of_src.

    rv_delimiter = ir_source->*+iv_offset.
  ENDMETHOD.


  METHOD get_multi_line_delimiter.

    DATA lr_source_to TYPE REF TO string.
    DATA lv_length TYPE i.
    DATA lv_delimiter TYPE string.
    DATA lr_delimiter TYPE REF TO string.

    lv_delimiter = get_delimiter_until_end_of_src(
      EXPORTING
        ir_source = ir_source
        iv_offset = iv_offset ).
    INSERT lv_delimiter INTO TABLE rt_delimiter.
    LOOP AT it_source REFERENCE INTO lr_source_to
           FROM ir_prev_token_ext->row + 1.
      IF sy-tabix > ir_token_ext->row.
        EXIT.
      ENDIF.
      INSERT INITIAL LINE INTO TABLE rt_delimiter REFERENCE INTO lr_delimiter.
      IF sy-tabix <> ir_token_ext->row.
        lr_delimiter->* = lr_source_to->*.
      ELSE.
        lv_length = ir_token_ext->col.
        IF lv_length >= 1.
          lr_delimiter->* = lr_source_to->*(lv_length).
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_single_line_delimiter.

    DATA lv_length TYPE i.

    lv_length = ir_token_ext->col - iv_offset.

    rv_delimiter = ir_source->*+iv_offset(lv_length).
  ENDMETHOD.


  METHOD set_delimiter_from_source.
    DATA lt_delimiter TYPE zapp_t_delimiter.
    DATA lr_delimiter TYPE REF TO string.
    DATA lv_delimiter TYPE string.
    DATA lv_offset TYPE i.

    lt_delimiter = get_delimiter_from_source(
      it_source         = it_source
      ir_prev_token_ext = ir_prev_token_ext
      ir_token_ext      = ir_token_ext ).

    ir_prev_token_ext->delimiter_org = lt_delimiter.

    ir_prev_token_ext->delimiter = lt_delimiter.

  ENDMETHOD.


  METHOD set_prev_delimiter.

    IF ir_prev_token_ext IS INITIAL.
      RETURN.
    ENDIF.

    set_delimiter_from_source(
      EXPORTING
        it_source         = it_source
        ir_prev_token_ext = ir_prev_token_ext
        ir_token_ext      = ir_token_ext
    ).

  ENDMETHOD.
ENDCLASS.
