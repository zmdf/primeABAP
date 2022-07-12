*&---------------------------------------------------------------------*
*& Report zmdf_r_getprimes
*&---------------------------------------------------------------------*
*& using modern ABAP
*&---------------------------------------------------------------------*
REPORT zmdf_r_getprimes.

PARAMETERS:
  p_limit TYPE i DEFAULT 1000000,
  p_print AS CHECKBOX.

CONSTANTS:
  yesPrime TYPE i VALUE 1,
  notPrime TYPE i VALUE 0.

TYPES:
  BEGIN OF ts_valResult,
    limit TYPE i,
    count TYPE i,
  END OF ts_valResult,
  tt_valResult TYPE SORTED TABLE OF ts_valResult WITH UNIQUE KEY limit.

*---------------------------------------------------------------------*
CLASS lcl_sieve DEFINITION FINAL.
  PUBLIC SECTION.
    DATA:
      prmeCount TYPE i READ-ONLY,
      maxNumber TYPE i READ-ONLY,
      sieveSize TYPE i READ-ONLY,
      sieveBits TYPE xstring READ-ONLY.
    METHODS:
      constructor IMPORTING !maxNum TYPE i OPTIONAL,
      execute,
      countPrimes,
      validate RETURNING VALUE(isValid) TYPE abap_boolean.
  PRIVATE SECTION.
    DATA:
      validResults TYPE tt_valResult.
ENDCLASS.

CLASS lcl_test DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      run,
      print IMPORTING !sieve TYPE REF TO lcl_sieve.
ENDCLASS.
*---------------------------------------------------------------------*


START-OF-SELECTION.
  lcl_test=>run( ).


*--------------------------------------------------------------------*
CLASS lcl_sieve IMPLEMENTATION.
*--------------------------------------------------------------------*
METHOD constructor.
  TRY.
        maxNumber = COND #( WHEN maxNum GT 1 THEN maxNum
                            WHEN maxNum GE 0 THEN 1000000
                            ELSE abs( maxNum ) ).
      CATCH cx_sy_arithmetic_overflow. "abs doesn't like min_int...
        maxNumber = cl_abap_math=>max_int4.
  ENDTRY.
  validResults = VALUE #(
      ( limit = 10 count = 4 )
      ( limit = 100 count = 25 )
      ( limit = 1000 count = 168 )
      ( limit = 10000 count = 1229 )
      ( limit = 100000 count = 9592 )
      ( limit = 1000000 count = 78498 )
      ( limit = 10000000 count = 664579 )
      ( limit = 100000000 count = 5761455 )
      ( limit = 1000000000 count = 50847534 )
    ) ##number_ok.
ENDMETHOD.

METHOD execute.

  "prepare...
  DATA(potentialPrime) = 3.
  DATA(stop) = CONV i( sqrt( maxNumber ) ).
  sieveSize  = maxNumber / 2.
  sieveBits  = bit-set( -1 * sieveSize ).

  "execute algorithm
  WHILE potentialPrime LE stop.

    "locate next prime
    DATA(bitIdx) = potentialPrime / 2.
    WHILE bitIdx LE sieveSize.
      GET BIT bitIdx OF sieveBits INTO DATA(isPrime).
      IF isPrime EQ yesPrime.
        EXIT.
      ENDIF.
      bitIdx += 1.
    ENDWHILE.
    DATA(currPrime) = 2 * bitIdx - 1.

    IF currPrime GT stop.
      EXIT. "job done, zug-zug
    ENDIF.

    "eliminate odd multiples of this prime, starting with his square
    bitIdx = ipow( base = currPrime exp = 2 ) / 2.
    WHILE bitIdx LE sieveSize.
      SET BIT bitIdx OF sieveBits TO notPrime.
      bitIdx += currPrime.
    ENDWHILE.

    "restart with the next odd number
    potentialPrime = currPrime + 2.

  ENDWHILE.

ENDMETHOD.

METHOD countPrimes.
  IF prmeCount IS INITIAL.
    DATA(idx) = 1.
    WHILE idx LE sieveSize.
      GET BIT idx OF sieveBits INTO DATA(isPrime).
      IF isPrime EQ yesPrime.
        prmeCount += 1.
      ENDIF.
      idx += 1.
    ENDWHILE.
  ENDIF.
ENDMETHOD.

METHOD validate.
  IF prmeCount IS INITIAL.
    countPrimes( ).
  ENDIF.
  TRY.
      IF prmeCount EQ validResults[ limit = maxNumber ]-count.
        isValid = abap_true.
      ENDIF.
    CATCH cx_sy_itab_line_not_found.
      "for inputs other than benchmark values, assume okay
      isValid = abap_true.
  ENDTRY.
ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
CLASS lcl_test IMPLEMENTATION.
*---------------------------------------------------------------------*
METHOD run.

  DATA:
    passes   TYPE i,
    duration TYPE p LENGTH 5 DECIMALS 6.

  GET RUN TIME FIELD DATA(startTime).
  WHILE duration LT 5.
    passes += 1.
    DATA(sieve) = NEW lcl_sieve( p_limit ).
    sieve->execute( ).
    GET RUN TIME FIELD DATA(endTime).
    duration = ( endTime - startTime ) / 1000000.
  ENDWHILE.

  IF p_print EQ abap_true.
    print( sieve ).
  ENDIF.

  IF sieve->validate( ) EQ abap_true.
    WRITE |zmdf;{ passes NUMBER = RAW };{ duration NUMBER = RAW };1;algorithm=base,faithful=yes,bits=1| ##no_text.
  ELSE.
    WRITE 'Invalid results'(t01).
  ENDIF.

ENDMETHOD.

METHOD print.

  DATA out TYPE c LENGTH 11.
  DATA(idx) = 1.

  sieve->countPrimes( ).
  WRITE |{ sieve->prmeCount } primes found until { sieve->maxNumber }: |.

  WHILE idx LE sieve->sieveSize.
    GET BIT idx OF sieve->sieveBits INTO DATA(isPrime).
      IF isPrime EQ yesPrime.
        DATA(prime) = COND #( WHEN idx EQ 1 THEN 2 ELSE 2 * idx - 1 ).
        DATA(len) = ceil( log10( prime ) ).
        WRITE prime TO out LEFT-JUSTIFIED NO-GROUPING.
        WRITE out(len).
      ENDIF.
      idx += 1.
  ENDWHILE.
  WRITE /.

ENDMETHOD.
ENDCLASS.
