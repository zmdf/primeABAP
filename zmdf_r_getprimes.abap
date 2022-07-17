*&---------------------------------------------------------------------*
*& Report zmdf_r_getprimes
*&---------------------------------------------------------------------*
*& using modern ABAP
*&---------------------------------------------------------------------*
REPORT zmdf_r_getprimes.

CONSTANTS:
  yesPrime TYPE i VALUE 1,
  notPrime TYPE i VALUE 0,
  aMillion TYPE i VALUE 1000000.

PARAMETERS:
  p_limit TYPE i DEFAULT aMillion,
  p_print AS checkbox.

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
  "user input can't be trusted; only values between 2 and maxInt make sense
  maxNumber = COND #( WHEN maxNum GT  1 THEN maxNum
                      WHEN maxNum GE -1 THEN aMillion
                      WHEN maxNum EQ cl_abap_math=>min_int4 THEN cl_abap_math=>max_int4
                      ELSE abs( maxNum ) ).
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
  DATA(prime) = 3.
  DATA(stop)  = CONV i( sqrt( maxNumber ) ).
  sieveSize   = maxNumber / 2.
  sieveBits   = bit-set( -1 * sieveSize ).

  "execute algorithm
  WHILE prime LE stop.

    "eliminate odd multiples of this prime, starting with his square
    DATA(bitIdx) = ipow( base = prime exp = 2 ) / 2.
    WHILE bitIdx LE sieveSize.
      SET BIT bitIdx OF sieveBits TO notPrime.
      bitIdx += prime.
    ENDWHILE.

    "locate next prime, starting with next odd number
    bitIdx = 1 + prime / 2.
    WHILE bitIdx LE sieveSize.
      GET BIT bitIdx OF sieveBits INTO DATA(isPrime).
      IF isPrime EQ yesPrime.
        EXIT.
      ENDIF.
      bitIdx += 1.
    ENDWHILE.
    prime = 2 * bitIdx - 1.

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

  DATA(staTime)  = utclong_current( ).
  DATA(endTime)  = utclong_current( ).
  DATA(duration) = utclong_diff( high = endTime low = staTime ).
  DATA(passes)   = 0.

  WHILE duration LT 5.
    DATA(sieve) = NEW lcl_sieve( p_limit ).
    sieve->execute( ).
    passes  += 1.
    endTime  = utclong_current( ).
    duration = utclong_diff(  high = endTime low = staTime ).
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
