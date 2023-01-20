"! <p class="shorttext synchronized" lang="en">Pretty Printer Scanner</p>
CLASS zcl_app_scanner DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Scan Source Code</p>
    "!
    "! @parameter IT_SOURCE | <p class="shorttext synchronized" lang="en">ABAP4 Sourcetext as table of string</p>
    "! @parameter ET_TOKEN_EXT | <p class="shorttext synchronized" lang="en">Code Inspector: Tokens extended</p>
    "! @parameter ET_STATEMENT | <p class="shorttext synchronized" lang="en">Code Inspector: Anweisungen</p>
    "! @parameter ET_STRUCTURE | <p class="shorttext synchronized" lang="en">Code Inspector: SSTRUC</p>
    METHODS scan
      IMPORTING it_source    TYPE sourcetable
      EXPORTING et_token_ext TYPE zapp_t_stokesx_ext_st
                et_statement TYPE sstmnt_tab
                et_structure TYPE sstruc_tab
      RAISING   zcx_app_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS enhance_token_ext
      IMPORTING
        it_source    TYPE sourcetable
        it_statement TYPE sstmnt_tab
      CHANGING
        ct_token_ext TYPE zapp_t_stokesx_ext_st
      RAISING
        zcx_app_exception.

    METHODS set_delimiter
      IMPORTING
        it_source    TYPE sourcetable
      CHANGING
        ct_token_ext TYPE zapp_t_stokesx_ext_st
      RAISING
        zcx_app_exception.

    METHODS set_original_token
      IMPORTING
        it_source    TYPE sourcetable
        ir_token_ext TYPE REF TO zapp_s_stokesx_ext.

    METHODS set_token_end_of_sql_statement
      IMPORTING
        ir_token_ext TYPE REF TO zapp_s_stokesx_ext.

    METHODS split_closing_bracket_token
      CHANGING
        ct_token_ext TYPE zapp_t_stokesx_ext_st.

    METHODS correct_token_with_length0
      CHANGING
        ct_token_ext TYPE zapp_t_stokesx_ext_st
      RAISING
        zcx_app_exception.

    METHODS correct_token_with_length0_ml
      IMPORTING
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext
      RAISING
        zcx_app_exception.

    METHODS correct_token_with_length0_sl
      IMPORTING
        ir_token_ext      TYPE REF TO zapp_s_stokesx_ext
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext
      RAISING
        zcx_app_exception.

    METHODS correct_overlapping_token
      IMPORTING
        ir_token_ext      TYPE REF TO zapp_s_stokesx_ext
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext
      RAISING
        zcx_app_exception.

    METHODS correct_sql_comment_tokens
      CHANGING
        ct_token_ext TYPE zapp_t_stokesx_ext_st
      RAISING
        zcx_app_exception.

    METHODS correct_sql_comment_token
      IMPORTING
        ir_token_ext        TYPE REF TO zapp_s_stokesx_ext
        ir_prev_token_ext   TYPE REF TO zapp_s_stokesx_ext
      RETURNING
        VALUE(rv_corrected) TYPE abap_bool
      RAISING
        zcx_app_exception.

    METHODS correct_token_with_double_dot
      IMPORTING
        it_source    TYPE sourcetable
      CHANGING
        ct_token_ext TYPE zapp_t_stokesx_ext_st
      RAISING
        zcx_app_exception.

    METHODS split_opening_bracket_token
      CHANGING
        ct_token_ext TYPE zapp_t_stokesx_ext_st.

    METHODS scan_keywords_amdp
      CHANGING
        ct_token_ext TYPE zapp_t_stokesx_ext_st
      RAISING
        zcx_app_exception.
ENDCLASS.



CLASS zcl_app_scanner IMPLEMENTATION.


  METHOD correct_token_with_length0.
    DATA lr_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lr_prev_token_ext TYPE REF TO zapp_s_stokesx_ext.
    " This happens for example if there are several closing brackets in sequence
    " or if there is ').' or only a ')' in one row
    IF ct_token_ext IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT ct_token_ext REFERENCE INTO lr_token_ext
    USING KEY row_col.
      IF lr_prev_token_ext IS NOT INITIAL.

        IF lr_prev_token_ext->len = 0.

          IF lr_token_ext->row <> lr_prev_token_ext->row.
            correct_token_with_length0_ml( lr_prev_token_ext ).
          ELSE.
            correct_token_with_length0_sl(
              EXPORTING
                ir_token_ext      = lr_token_ext
                ir_prev_token_ext = lr_prev_token_ext ).

          ENDIF.
        ENDIF.
      ENDIF.
      lr_prev_token_ext = lr_token_ext.
    ENDLOOP.

    IF lr_token_ext->len = 0.
      correct_token_with_length0_ml( lr_prev_token_ext ).
    ENDIF.

  ENDMETHOD.


  METHOD correct_token_with_length0_ml.
    " In this case there is normally STR filled with the first part of the delimiter

    DATA lv_length_str TYPE i.
    DATA lv_length_delimiter TYPE i.
    DATA lr_delimiter TYPE REF TO string.
    READ TABLE ir_prev_token_ext->delimiter
    REFERENCE INTO lr_delimiter INDEX 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_app_exception
        MESSAGE ID 'ZAPP_MC_PRETTY_PRINT'
        TYPE 'E'
        NUMBER '012'
        WITH ir_prev_token_ext->str_org
        ir_prev_token_ext->row
        ir_prev_token_ext->col.
    ENDIF.

    lv_length_str = strlen( ir_prev_token_ext->str ).

    "This happens normally only at ')'
    IF lv_length_str <> 1.
      RAISE EXCEPTION TYPE zcx_app_exception
        MESSAGE ID 'ZAPP_MC_PRETTY_PRINT'
        TYPE 'E'
        NUMBER '012'
        WITH ir_prev_token_ext->str_org
        ir_prev_token_ext->row
        ir_prev_token_ext->col.
    ENDIF.

    lv_length_delimiter = strlen( lr_delimiter->* ).

    IF lv_length_str > lv_length_delimiter.
      RAISE EXCEPTION TYPE zcx_app_exception
        MESSAGE ID 'ZAPP_MC_PRETTY_PRINT'
        TYPE 'E'
        NUMBER '012'
        WITH ir_prev_token_ext->str_org
        ir_prev_token_ext->row
        ir_prev_token_ext->col.
    ENDIF.

    IF ir_prev_token_ext->str <> lr_delimiter->*(lv_length_str).
      RAISE EXCEPTION TYPE zcx_app_exception
        MESSAGE ID 'ZAPP_MC_PRETTY_PRINT'
        TYPE 'E'
        NUMBER '012'
        WITH ir_prev_token_ext->str_org
        ir_prev_token_ext->row
        ir_prev_token_ext->col.
    ENDIF.

    ir_prev_token_ext->len = lv_length_str.

    lr_delimiter->* = lr_delimiter->*+lv_length_str.
    ir_prev_token_ext->delimiter_org = ir_prev_token_ext->delimiter.
    IF ir_prev_token_ext->str_org IS INITIAL.
      ir_prev_token_ext->str_org = ir_prev_token_ext->str.
    ENDIF.
  ENDMETHOD.


  METHOD correct_token_with_length0_sl.

    DATA lv_delimiter TYPE string.
    DATA lv_length TYPE i.

    lv_length = ir_token_ext->col - ir_prev_token_ext->col - strlen( ir_prev_token_ext->str ).

    IF lv_length < 0.
      RAISE EXCEPTION TYPE zcx_app_exception
        MESSAGE ID 'ZAPP_MC_PRETTY_PRINT'
        TYPE 'E'
        NUMBER '012'
        WITH ir_prev_token_ext->str_org
        ir_prev_token_ext->row
        ir_prev_token_ext->col.
    ENDIF.

    ir_prev_token_ext->len = strlen( ir_prev_token_ext->str ).
    CLEAR lv_delimiter.
    DO lv_length TIMES.
      CONCATENATE lv_delimiter space INTO lv_delimiter RESPECTING BLANKS.
    ENDDO.
    CLEAR ir_prev_token_ext->delimiter.
    INSERT lv_delimiter INTO TABLE ir_prev_token_ext->delimiter.
    ir_prev_token_ext->delimiter_org = ir_prev_token_ext->delimiter.
    IF ir_prev_token_ext->str_org IS INITIAL.
      ir_prev_token_ext->str_org = ir_prev_token_ext->str.
    ENDIF.
  ENDMETHOD.


  METHOD enhance_token_ext.
    DATA lr_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lr_prev_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lr_scanner_comment TYPE REF TO zcl_app_scanner_comment.
    DATA lr_scanner_sqlscript TYPE REF TO zcl_app_scanner_sqlscript.

    CREATE OBJECT lr_scanner_sqlscript.
    CREATE OBJECT lr_scanner_comment.

    LOOP AT ct_token_ext
      REFERENCE INTO lr_token_ext
      USING KEY row_col.

      set_original_token(
        EXPORTING
          it_source    = it_source
          ir_token_ext = lr_token_ext ).
    ENDLOOP.

    LOOP AT ct_token_ext
      REFERENCE INTO lr_token_ext
      USING KEY row_col.

      IF lr_prev_token_ext IS NOT INITIAL.

        correct_overlapping_token(
          EXPORTING
            ir_token_ext      = lr_token_ext
            ir_prev_token_ext = lr_prev_token_ext
        ).

      ENDIF.
      lr_prev_token_ext = lr_token_ext.

    ENDLOOP.
    CLEAR lr_prev_token_ext.
    correct_sql_comment_tokens(
      CHANGING
        ct_token_ext = ct_token_ext
    ).


    LOOP AT ct_token_ext
      REFERENCE INTO lr_token_ext
      USING KEY row_col.
      IF lr_prev_token_ext IS NOT INITIAL.
        lr_scanner_sqlscript->set_sqlscript(
          EXPORTING
            ir_prev_token_ext = lr_prev_token_ext
            ir_token_ext      = lr_token_ext
            it_statement      = it_statement
            it_token_ext      = ct_token_ext ).

        lr_scanner_comment->set_comment(
          EXPORTING
            ir_token_ext      = lr_token_ext
            ir_prev_token_ext = lr_prev_token_ext ).

        set_token_end_of_sql_statement( ir_token_ext = lr_token_ext ).
      ENDIF.
      lr_prev_token_ext = lr_token_ext.

    ENDLOOP.

    correct_token_with_double_dot(
      EXPORTING
        it_source    = it_source
      CHANGING
        ct_token_ext = ct_token_ext ).

    set_delimiter(
      EXPORTING
        it_source    = it_source
      CHANGING
        ct_token_ext = ct_token_ext
    ).


    correct_token_with_length0( CHANGING ct_token_ext = ct_token_ext ).

    split_closing_bracket_token( CHANGING ct_token_ext = ct_token_ext ).

    split_opening_bracket_token( CHANGING ct_token_ext = ct_token_ext ).

    scan_keywords_amdp( CHANGING ct_token_ext = ct_token_ext ).

  ENDMETHOD.


  METHOD scan.
    " Scan the source code similar to the method lcl_scanner->scan in FM PRETTY_PRINTER
    DATA lt_token TYPE stokesx_tab.
    DATA lr_token TYPE REF TO stokesx.
    DATA ls_token_ext TYPE zapp_s_stokesx_ext.
    CLEAR et_structure.
    CLEAR et_token_ext.
    CLEAR et_statement.

    SCAN ABAP-SOURCE it_source
      TOKENS      INTO lt_token
      STATEMENTS  INTO et_statement
      STRUCTURES  INTO et_structure
      PRESERVING IDENTIFIER ESCAPING
      WITH ANALYSIS
      WITH COMMENTS
      WITH PRAGMAS '*'
      WITHOUT TRMAC.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_app_exception
        MESSAGE ID 'ZAPP_MC_PRETTY_PRINT'
        TYPE 'E'
        NUMBER '001'.

    ENDIF.
    "Get Row Number of the original table.
    "This is required because the SAP Standard doesn't sort this table by the occurend of the source code
    "E.g comments starting with * before the AMDP select statement the contains this somewhere this comment
    LOOP AT lt_token REFERENCE INTO lr_token.
      CLEAR ls_token_ext.
      MOVE-CORRESPONDING lr_token->* TO ls_token_ext.
      ls_token_ext-org_tab_row = sy-tabix.
      ls_token_ext-str_up = ls_token_ext-str.
      ls_token_ext-len = strlen( ls_token_ext-str ).
      TRANSLATE ls_token_ext-str_up TO UPPER CASE.
      INSERT ls_token_ext INTO TABLE et_token_ext.
    ENDLOOP.

    enhance_token_ext(
      EXPORTING
        it_source    = it_source
        it_statement = et_statement
      CHANGING
        ct_token_ext = et_token_ext ).

  ENDMETHOD.


  METHOD set_delimiter.
    DATA lt_token_ext_unsorted TYPE STANDARD TABLE OF zapp_s_stokesx_ext.
    DATA lr_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lr_prev_token_ext TYPE REF TO zapp_s_stokesx_ext.

    DATA lr_scanner_delimiter TYPE REF TO zcl_app_scanner_delimiter.

    "If you have methods defined with ':' then you can get double entries for the row and col
    " we have to remove them
    lt_token_ext_unsorted = ct_token_ext.
    SORT lt_token_ext_unsorted BY row col org_tab_row.
    DELETE ADJACENT DUPLICATES FROM lt_token_ext_unsorted COMPARING row col.
    ct_token_ext = lt_token_ext_unsorted.

    CREATE OBJECT lr_scanner_delimiter.

    LOOP AT ct_token_ext
      REFERENCE INTO lr_token_ext
      USING KEY row_col.

      lr_scanner_delimiter->set_prev_delimiter(
        EXPORTING
          it_source         = it_source
          ir_prev_token_ext = lr_prev_token_ext
          ir_token_ext      = lr_token_ext ).

      lr_prev_token_ext = lr_token_ext.

    ENDLOOP.

    CLEAR lr_token_ext.
    lr_scanner_delimiter->set_prev_delimiter(
      EXPORTING
        it_source         = it_source
        ir_prev_token_ext = lr_prev_token_ext
        ir_token_ext      = lr_token_ext ).

  ENDMETHOD.


  METHOD set_original_token.
    DATA lr_source TYPE REF TO string.
    READ TABLE it_source
     REFERENCE INTO lr_source
     INDEX ir_token_ext->row.
    IF sy-subrc = 0.
      ir_token_ext->str_org = lr_source->*+ir_token_ext->col(ir_token_ext->len).
      ir_token_ext->str_up =  ir_token_ext->str_org.
      TRANSLATE ir_token_ext->str_up TO UPPER CASE.
    ENDIF.


  ENDMETHOD.


  METHOD set_token_end_of_sql_statement.
    DATA lv_len TYPE i.
    DATA lv_offset TYPE i.
    IF  ir_token_ext->sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript
    AND ir_token_ext->comment = zcl_app_scanner_comment=>cos_comment-none.
      lv_len = strlen( ir_token_ext->str ).
      IF lv_len > 1.
        lv_offset = lv_len - 1.
        IF ir_token_ext->str+lv_offset(1) = ';'.
          ir_token_ext->len = lv_offset.
          ir_token_ext->str = ir_token_ext->str(lv_offset).
          ir_token_ext->str_up = ir_token_ext->str_up(lv_offset).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD split_closing_bracket_token.
    DATA lr_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lv_prev_org_tab_row TYPE zapp_d_org_tab_row.
    DATA lv_org_tab_row TYPE zapp_d_org_tab_row.
    DATA ls_token_ext TYPE zapp_s_stokesx_ext.
    DATA lv_found TYPE abap_bool.
    DATA lv_delimiter TYPE string.

    DO.
      lv_found = abap_false.
      LOOP AT ct_token_ext REFERENCE INTO lr_token_ext.
        lv_org_tab_row = lr_token_ext->org_tab_row.
        IF NOT lr_token_ext->str_up IS INITIAL.
          IF lr_token_ext->str_up(1) = ')' AND strlen( lr_token_ext->str_up ) > 1
          AND zcl_app_utilities=>is_sqlscript_token( lr_token_ext->sqlscript ) = abap_true
          AND zcl_app_utilities=>is_comment( lr_token_ext->comment ) = abap_false.
            lv_found = abap_true.
            CLEAR ls_token_ext.
            ls_token_ext-str = lr_token_ext->str(1).
            ls_token_ext-str_up = lr_token_ext->str_up(1).
            ls_token_ext-str_org = lr_token_ext->str_org(1).
            ls_token_ext-org_tab_row = lv_prev_org_tab_row + '0.1'  ##LITERAL.
            ls_token_ext-row = lr_token_ext->row.
            ls_token_ext-col = lr_token_ext->col.
            ls_token_ext-len = 1.
            ls_token_ext-type = lr_token_ext->type.
            ls_token_ext-sqlscript = lr_token_ext->sqlscript.
            lv_delimiter = ` `.
            INSERT lv_delimiter INTO TABLE ls_token_ext-delimiter.
            ls_token_ext-delimiter_org = ls_token_ext-delimiter.


            lr_token_ext->col = lr_token_ext->col + 1.
            lr_token_ext->len = lr_token_ext->len - 1.
            lr_token_ext->str = lr_token_ext->str+1.
            lr_token_ext->str_up = lr_token_ext->str_up+1.
            lr_token_ext->str_org = lr_token_ext->str_org+1.

            INSERT ls_token_ext INTO TABLE ct_token_ext.

          ENDIF.
        ENDIF.
        lv_prev_org_tab_row = lv_org_tab_row.
      ENDLOOP.
      IF lv_found = abap_false.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD split_opening_bracket_token.
    DATA lr_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lr_prev_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lv_org_tab_row TYPE zapp_d_org_tab_row.            "#EC NEEDED
    DATA ls_token_ext TYPE zapp_s_stokesx_ext.
    DATA lv_found TYPE abap_bool.
    DATA lv_length TYPE i.
    DATA lv_offset TYPE i.

    DO.
      lv_found = abap_false.
      LOOP AT ct_token_ext REFERENCE INTO lr_token_ext.
        lv_org_tab_row = lr_token_ext->org_tab_row.

        IF lr_prev_token_ext IS INITIAL
        OR strlen( lr_prev_token_ext->str_up ) < 2
        OR xsdbool( lr_prev_token_ext->str_up CA '(' ) = abap_false
        OR zcl_app_utilities=>is_sqlscript_token( lr_prev_token_ext->sqlscript ) = abap_false
          OR zcl_app_utilities=>is_comment( lr_prev_token_ext->comment ) = abap_true.

          lr_prev_token_ext = lr_token_ext.
          CONTINUE.
        ENDIF.

        lv_length = strlen( lr_prev_token_ext->str_up ).

        lv_offset = lv_length - 1.
        IF lr_prev_token_ext->str_up+lv_offset(1) <> '('.
          lr_prev_token_ext = lr_token_ext.
          CONTINUE.
        ENDIF.

        lv_found = abap_true.
        CLEAR ls_token_ext.
        ls_token_ext-str = lr_prev_token_ext->str+lv_offset(1).
        ls_token_ext-str_up = lr_prev_token_ext->str_up+lv_offset(1).
        ls_token_ext-str_org = lr_prev_token_ext->str_up+lv_offset(1). "It is not sure how the original look so take str_up
        ls_token_ext-org_tab_row = lr_prev_token_ext->org_tab_row - '0.01'  ##LITERAL.
        ls_token_ext-row = lr_prev_token_ext->row.
        ls_token_ext-col = lr_prev_token_ext->col + lr_prev_token_ext->len - 1.
        ls_token_ext-len = 1.
        ls_token_ext-type = lr_prev_token_ext->type.
        ls_token_ext-sqlscript = lr_prev_token_ext->sqlscript.
        ls_token_ext-delimiter = lr_prev_token_ext->delimiter.
        ls_token_ext-delimiter_org = lr_prev_token_ext->delimiter_org.
        INSERT ls_token_ext INTO TABLE ct_token_ext.


        lr_prev_token_ext->str = lr_prev_token_ext->str(lv_offset).
        lr_prev_token_ext->str_up = lr_prev_token_ext->str_up(lv_offset).
        lr_prev_token_ext->str_org = lr_prev_token_ext->str_up(lv_offset).
        lr_prev_token_ext->len = lr_prev_token_ext->len - 1.
        CLEAR lr_prev_token_ext->delimiter.
        CLEAR lr_prev_token_ext->delimiter_org.

        lr_prev_token_ext = lr_token_ext.

      ENDLOOP.
      IF lv_found = abap_false.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD scan_keywords_amdp.

    DATA lr_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lr_keyword_scanner_amdp TYPE REF TO zif_app_keyword_scanner.

    CREATE OBJECT lr_keyword_scanner_amdp TYPE ('ZCL_APP_KEYWORD_SCANNER_AMDP').

    LOOP AT ct_token_ext REFERENCE INTO lr_token_ext.
      lr_keyword_scanner_amdp->scan_keyword( ir_token_ext = lr_token_ext ).
    ENDLOOP.
  ENDMETHOD.

  METHOD correct_token_with_double_dot.
    "Correct Token with double dots
    "Add the double dot from the delimiter of the previous token
    "to start of the of the current one
    DATA lr_source TYPE REF TO string.
    DATA lv_offset TYPE i.
    DATA lv_prev_offset_end TYPE i.
    DATA lr_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lr_prev_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lt_token_ext_unsorted TYPE STANDARD TABLE OF zapp_s_stokesx_ext.

    lt_token_ext_unsorted = ct_token_ext.
    SORT lt_token_ext_unsorted BY row col org_tab_row.

    LOOP AT lt_token_ext_unsorted REFERENCE INTO lr_token_ext.

      DO 1 TIMES.
        IF lr_prev_token_ext IS INITIAL
        OR zcl_app_utilities=>is_sqlscript_token( lr_token_ext->sqlscript ) = abap_false
        OR zcl_app_utilities=>is_comment( lr_token_ext->comment ) = abap_true.
          EXIT.
        ENDIF.

        lv_offset = lr_token_ext->col - 1.
        IF lv_offset < 0.
          EXIT.
        ENDIF.

        lv_prev_offset_end = lr_prev_token_ext->col + lr_prev_token_ext->len.
        IF lv_prev_offset_end >= lv_offset.
          EXIT.
        ENDIF.

        READ TABLE it_source REFERENCE INTO lr_source INDEX lr_token_ext->row.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        IF lr_source->*+lv_offset(1) = ':'.
          CONCATENATE lr_source->*+lv_offset(1) lr_token_ext->str INTO lr_token_ext->str.
          CONCATENATE lr_source->*+lv_offset(1) lr_token_ext->str_org INTO lr_token_ext->str_org.
          CONCATENATE lr_source->*+lv_offset(1) lr_token_ext->str_up INTO lr_token_ext->str_up.
          lr_token_ext->col = lv_offset.
          lr_token_ext->len = lr_token_ext->len + 1.
        ENDIF.
      ENDDO.
      lr_prev_token_ext = lr_token_ext.
    ENDLOOP.
    ct_token_ext = lt_token_ext_unsorted.
  ENDMETHOD.

  METHOD correct_overlapping_token.
    DATA lv_offset TYPE i.
    IF ir_token_ext->row = ir_prev_token_ext->row.
      lv_offset = ir_prev_token_ext->col + ir_prev_token_ext->len.

      IF ir_token_ext->col < lv_offset.
        ir_prev_token_ext->len = ir_token_ext->col - ir_prev_token_ext->col.
        ir_prev_token_ext->str_org = ir_prev_token_ext->str_org(ir_prev_token_ext->len).
        ir_prev_token_ext->str_up = ir_prev_token_ext->str_org.
        TRANSLATE ir_prev_token_ext->str_up TO UPPER CASE.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD correct_sql_comment_token.
    DATA lv_offset TYPE i.
    DATA lv_rest_token TYPE zapp_d_token.
    DATA lv_len_rest TYPE i.
    DATA lv_org_prev_token TYPE zapp_d_token.
    DATA lv_delimiter TYPE i.
    DATA lv_org_token TYPE zapp_d_token.

    IF ir_token_ext->row = ir_prev_token_ext->row.

      FIND ';--' IN ir_prev_token_ext->str MATCH OFFSET lv_offset.
      IF sy-subrc = 0.
        " '--' must be moved to the next line else
        " the where from example
        " select * from  sflight;-- where
        " will be moved to the next line and will not be handled as sql comment

        lv_delimiter = ir_token_ext->col - ir_prev_token_ext->col - ir_prev_token_ext->len.

        lv_org_prev_token = ir_prev_token_ext->str.
        lv_offset = lv_offset + 1.
        lv_rest_token = ir_prev_token_ext->str+lv_offset.
        lv_len_rest = strlen( lv_rest_token ).

        ir_prev_token_ext->str = ir_prev_token_ext->str+0(lv_offset).
        ir_prev_token_ext->len =  ir_prev_token_ext->len - lv_len_rest.
        ir_prev_token_ext->str_org =  ir_prev_token_ext->str.
        ir_prev_token_ext->str_up = ir_prev_token_ext->str_org.
        TRANSLATE ir_prev_token_ext->str_up TO UPPER CASE.

        lv_org_token = ir_token_ext->str.

        ir_token_ext->str = lv_rest_token.
        "Add delimiter spaces
        DO lv_delimiter TIMES.
          CONCATENATE ir_token_ext->str ` ` INTO ir_token_ext->str RESPECTING BLANKS.
        ENDDO.

        CONCATENATE ir_token_ext->str lv_org_token INTO ir_token_ext->str RESPECTING BLANKS.
        ir_token_ext->col = ir_token_ext->col - lv_len_rest - lv_delimiter.

        IF ir_token_ext->col <= 0.
          RAISE EXCEPTION TYPE zcx_app_exception
            MESSAGE ID 'ZAPP_MC_PRETTY_PRINT'
            TYPE 'E'
            NUMBER '019'
            WITH lv_org_prev_token
            ir_prev_token_ext->row
            ir_prev_token_ext->col.
        ENDIF.
        ir_token_ext->len =  ir_token_ext->len + lv_len_rest + lv_delimiter.
        ir_token_ext->str_org =  ir_token_ext->str.
        ir_token_ext->str_up = ir_token_ext->str_org.
        TRANSLATE ir_token_ext->str_up TO UPPER CASE.
        rv_corrected = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD correct_sql_comment_tokens.

    DATA lt_token_ext_unsorted TYPE STANDARD TABLE OF zapp_s_stokesx_ext.
    DATA lr_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lr_prev_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lv_corrected TYPE abap_bool.

    lt_token_ext_unsorted = ct_token_ext.
    SORT lt_token_ext_unsorted BY row col org_tab_row.

    LOOP AT lt_token_ext_unsorted
      REFERENCE INTO lr_token_ext.

      IF lr_prev_token_ext IS NOT INITIAL.
        IF     correct_sql_comment_token(
                  EXPORTING
                    ir_token_ext      = lr_token_ext
                    ir_prev_token_ext = lr_prev_token_ext
                ) = abap_true.

          lv_corrected = abap_true.

        ENDIF.
      ENDIF.
      lr_prev_token_ext = lr_token_ext.

    ENDLOOP.
    IF lv_corrected = abap_true.
      CLEAR ct_token_ext.
      INSERT LINES OF lt_token_ext_unsorted INTO TABLE ct_token_ext.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
