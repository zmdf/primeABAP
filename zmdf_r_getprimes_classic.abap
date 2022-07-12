*&---------------------------------------------------------------------*
*& Report  zmdf_r_getprimes_oldstyle
*&---------------------------------------------------------------------*
*& using classical ABAP (procedural, old-style statements & notation...)
*&---------------------------------------------------------------------*
REPORT zmdf_r_getprimes_oldstyle.

PARAMETERS:
  p_limit TYPE i DEFAULT 1000000,
  p_print AS CHECKBOX.

CONSTANTS:
  yes_prime TYPE i VALUE 1,
  not_prime TYPE i VALUE 0,
  min_int4  TYPE i VALUE -2147483648,
  max_int4  TYPE i VALUE  2147483647.

TYPES:
  BEGIN OF ts_exp_result,
    number TYPE i,
    count  TYPE i,
  END OF ts_exp_result,
  tt_exp_result TYPE SORTED TABLE OF ts_exp_result WITH UNIQUE KEY number,

  BEGIN OF ts_state,
    max_number TYPE i,
    prm_count  TYPE i,
    sieve_size TYPE i,
    sieve_bits TYPE xstring,
  END OF ts_state.

DATA gt_exp_result TYPE tt_exp_result. "global
*---------------------------------------------------------------------*

START-OF-SELECTION.
  PERFORM main.

INITIALIZATION.
  PERFORM set_expectations.

*---------------------------------------------------------------------*
FORM main.
  DATA:
    ls_state    TYPE ts_state,
    lv_limit    TYPE i,
    lv_sta_time TYPE i,
    lv_end_time TYPE i,
    lv_passes   TYPE i,
    lv_is_valid TYPE c,
    lv_duration TYPE p LENGTH 5 DECIMALS 6,
    lv_temp1    TYPE c LENGTH 11,
    lv_temp2    TYPE c LENGTH 11,
    lv_output   TYPE c LENGTH 120.

  IF p_limit GT 0.
    lv_limit = p_limit.
  ELSEIF p_limit EQ 0.
    lv_limit = 1000000 ##number_ok.
  ELSEIF p_limit EQ min_int4.
    lv_limit = max_int4.
  ELSE.
    lv_limit = cl_abap_math=>max_int4.
  ENDIF.

  GET RUN TIME FIELD lv_sta_time.
  WHILE lv_duration LT 5.
    ADD 1 TO lv_passes.
    CLEAR ls_state.
    ls_state-max_number = lv_limit.
    PERFORM execute_sieve CHANGING ls_state.
    GET RUN TIME FIELD lv_end_time.
    lv_duration = ( lv_end_time - lv_sta_time ) / 1000000.
  ENDWHILE.

  IF p_print EQ abap_true.
    PERFORM print CHANGING ls_state.
  ENDIF.

  PERFORM validate CHANGING ls_state lv_is_valid.

  IF lv_is_valid EQ abap_true.
    SET COUNTRY 'US'. "to use dot as separator
    WRITE lv_passes TO lv_temp1 NO-GROUPING.
    WRITE lv_duration TO lv_temp2 NO-GROUPING.
    CONDENSE lv_temp1.
    CONDENSE lv_temp2.
    CONCATENATE 'zmdf-classicABAP;' lv_temp1 ';' lv_temp2 ';1;algorithm=base,faithful=yes,bits=1' INTO lv_output ##no_text.
    WRITE lv_output.
  ELSE.
    WRITE 'Invalid results'(t01).
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
FORM execute_sieve CHANGING cs_state TYPE ts_state.

  DATA:
    lv_boundary    TYPE i,
    lv_potential_p TYPE i,
    lv_current_p   TYPE i,
    lv_bit_index   TYPE i,
    lv_bit_value   TYPE i.

  "prepare ...
  lv_potential_p = 3.
  lv_boundary    = sqrt( cs_state-max_number ).
  cs_state-sieve_size = cs_state-max_number / 2.
  cs_state-sieve_bits = bit-set( -1 * cs_state-sieve_size ).

  "execute algorithm
  WHILE lv_potential_p LE lv_boundary.

    "locate next prime using the sieve
    lv_bit_index = lv_potential_p / 2.
    WHILE lv_bit_index LE cs_state-sieve_size.
      GET BIT lv_bit_index OF cs_state-sieve_bits INTO lv_bit_value.
      IF lv_bit_value EQ yes_prime.
        EXIT.
      ENDIF.
      ADD 1 TO lv_bit_index.
    ENDWHILE.
    lv_current_p = 2 * lv_bit_index - 1.

    IF lv_current_p GT lv_boundary.
      EXIT. "job done
    ENDIF.

    "eliminate odd multiples of this prime, starting with his square
    lv_bit_index = ipow( base = lv_current_p exp = 2 ) / 2.
    WHILE lv_bit_index LE cs_state-sieve_size.
      SET BIT lv_bit_index OF cs_state-sieve_bits TO not_prime.
      ADD lv_current_p TO lv_bit_index.
    ENDWHILE.

    "restart with the next odd number
    lv_potential_p = lv_current_p + 2.

  ENDWHILE.

ENDFORM.

*---------------------------------------------------------------------*
FORM validate CHANGING cs_state TYPE ts_state
                       ev_valid TYPE c.

  FIELD-SYMBOLS:
    <exp_result> TYPE ts_exp_result.

  IF cs_state-prm_count IS INITIAL.
    PERFORM count_primes CHANGING cs_state.
  ENDIF.

  READ TABLE gt_exp_result ASSIGNING <exp_result>
    WITH TABLE KEY number = cs_state-max_number.
  IF sy-subrc EQ 0.
    IF cs_state-prm_count EQ <exp_result>-count.
      ev_valid = abap_true.
    ENDIF.
  ELSE.
    "for inputs other than benchmark values, assume okay
    ev_valid = abap_true.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
FORM count_primes CHANGING cs_state TYPE ts_state.

  DATA:
    lv_idx TYPE i VALUE 1,
    lv_siz TYPE i,
    lv_bit TYPE i.

  IF cs_state-prm_count IS INITIAL.
    lv_siz  = cs_state-max_number / 2.
    WHILE lv_idx LE lv_siz.
      GET BIT lv_idx OF cs_state-sieve_bits INTO lv_bit.
      IF lv_bit EQ yes_prime.
        ADD 1 TO cs_state-prm_count.
      ENDIF.
      ADD 1 TO lv_idx.
    ENDWHILE.
  ENDIF.

ENDFORM.

FORM print CHANGING cs_state TYPE ts_state.

  DATA:
    lv_bit TYPE i,
    lv_prm TYPE i,
    lv_len TYPE i,
    lv_idx TYPE i VALUE 2,
    lv_out TYPE c LENGTH 11.

  IF cs_state-prm_count IS INITIAL.
    PERFORM count_primes CHANGING cs_state.
  ENDIF.

  WRITE: cs_state-prm_count NO-GROUPING, 'primes found until', cs_state-max_number NO-GROUPING LEFT-JUSTIFIED.
  WRITE: / '2'. "first prime is 2, write separately, as loop below starts at second bit index
  WHILE lv_idx LE cs_state-sieve_size.
    GET BIT lv_idx OF cs_state-sieve_bits INTO lv_bit.
      IF lv_bit EQ yes_prime.
        lv_prm = 2 * lv_idx - 1.
        lv_len = ceil( log10( lv_prm ) ).
        WRITE lv_prm TO lv_out LEFT-JUSTIFIED NO-GROUPING.
        WRITE lv_out(lv_len).
      ENDIF.
      ADD 1 to lv_idx.
  ENDWHILE.
  WRITE /.

ENDFORM.

*---------------------------------------------------------------------*
FORM set_expectations.
  DATA ls_exp_result TYPE ts_exp_result.
  ls_exp_result-number = 10.
  ls_exp_result-count  = 4.
  INSERT ls_exp_result INTO TABLE gt_exp_result.
  ls_exp_result-number = 100.
  ls_exp_result-count  = 25.
  INSERT ls_exp_result INTO TABLE gt_exp_result.
  ls_exp_result-number = 1000.
  ls_exp_result-count  = 168.
  INSERT ls_exp_result INTO TABLE gt_exp_result.
  ls_exp_result-number = 10000.
  ls_exp_result-count  = 1229.
  INSERT ls_exp_result INTO TABLE gt_exp_result.
  ls_exp_result-number = 100000.
  ls_exp_result-count  = 9592.
  INSERT ls_exp_result INTO TABLE gt_exp_result.
  ls_exp_result-number = 1000000.
  ls_exp_result-count  = 78498.
  INSERT ls_exp_result INTO TABLE gt_exp_result.
  ls_exp_result-number = 10000000.
  ls_exp_result-count  = 664579.
  INSERT ls_exp_result INTO TABLE gt_exp_result.
  ls_exp_result-number = 100000000.
  ls_exp_result-count  = 5761455.
  INSERT ls_exp_result INTO TABLE gt_exp_result.
  ls_exp_result-number = 1000000000.
  ls_exp_result-count  = 50847534.
  INSERT ls_exp_result INTO TABLE gt_exp_result.
ENDFORM.
