# primesABAP
ABAP sample implementations of 'base' sieve algorithm for determining prime numbers up to a certain limit.

## Details
There are two implementations:
- one using ABAP OO, `zmdf_r_getprimes.abap`, with modern statements and expressions, as well as new notations and naming conventions
- one using classic, procedural ABAP, `zmdf_r_getprimes.abap`, with alternative statements more faithful to ABAP origins as well as notations established (and still widely used) in SAP business applications 

Both implementations follows closely Dave Plummer's specifications from the  ["drag-race"](https://github.com/PlummersSoftwareLLC/Primes/blob/drag-race/CONTRIBUTING.md) - especially [the rules](https://github.com/PlummersSoftwareLLC/Primes/blob/drag-race/CONTRIBUTING.md#rules).

The chosen algorithm is "base" (used by @davepl in the YouTube video that spawned the drag-race).

## Output
- for modern ABAP:
```
zmdf;74;5.013153;1;algorithm=base,faithful=yes,bits=1
```
- for classic ABAP:
```
zmdf-classicABAP;84;5.055994;1;algorithm=base,faithful=yes,bits=1                                                       
```
It's noteworthy that the "old" legacy code runs faster. The measured blocks differ minimally: in the "modern" version, an ABAP object is instantiated and the algorithm is executed using a method call (`sieve->execute( ).`), whereas in the "classic" version a static procedure ("form-routine" in SAP jargon) is called (`PERFORM execute_sieve CHANGING ls_state.`). The code in both method and form-routine is identical (up to different coding styles, that should not impact the runtime...)

## Screenshots
Sample initial screen in SAP GUI for primes up to 100 and full listing
![Sample initial screen for up to 100 and full listing](/screenshots/SAPGUI-InitialScreen-100.png)

Sample output in SAP GUI for primes up to 100
![Sample output for up to 100](/screenshots/SAPGUI-Results-100.png)

Sample initial screen in SAP GUI for primes up to 1 Mio
![Sample initial screen for up to 1 Mio](/screenshots/SAPGUI-InitialScreen-1Mio.png)

Sample output in SAP GUI for primes up to 1 Mio (modern code)
![Sample output for up to 1 Mio](/screenshots/SAPGUI-Results-1Mio.png)

Sample output in SAP GUI for primes up to 1 Mio (classical ABAP code)
![Sample output for up to 1 Mio](/screenshots/SAPGUI-Results-1Mio-classical.png)
