# Class TOUR – Solution to a traveling salesperson problem

Class to store the solution of a TSP. Objects of this class are returned
by TSP solvers in this package. Essentially, an object of class `TOUR`
is a permutation vector containing the order of cities to visit.

## Usage

``` r
TOUR(x, method = NA, tsp = NULL)

as.TOUR(object)

# S3 method for class 'numeric'
as.TOUR(object)

# S3 method for class 'integer'
as.TOUR(object)

# S3 method for class 'TOUR'
print(x, ...)
```

## Arguments

- x:

  an integer permutation vector or, for the methods an object of class
  TOUR.

- method:

  character string; method used to create the tour.

- tsp:

  `TSP` object the tour applies to. If available then the tour will
  include the tour length. Also the labels of the cities will be
  available in the tour (otherwise the labels of `x` are used).

- object:

  data (an integer vector) which can be coerced to `TOUR`.

- ...:

  further arguments are passed on.

## Details

Since an object of class `TOUR` is an integer vector, it can be
subsetted as an ordinary vector or coerced to an integer vector using
[`as.integer()`](https://rdrr.io/r/base/integer.html). It also contains
the names of the objects as labels. Additionally, `TOUR` has the
following attributes: `"method"`, `"tour_length"`.

For most functions, e.g.,
[`tour_length()`](http://michael.hahsler.net/TSP/reference/tour_length.md)
or [`image.TSP()`](http://michael.hahsler.net/TSP/reference/TSP.md), the
`TSP/ATSP` object used to find the tour is still needed, since the tour
does not contain the distance information.

## See also

Other TOUR:
[`cut_tour()`](http://michael.hahsler.net/TSP/reference/cut_tour.md),
[`solve_TSP()`](http://michael.hahsler.net/TSP/reference/solve_TSP.md),
[`tour_length()`](http://michael.hahsler.net/TSP/reference/tour_length.md)

## Author

Michael Hahsler

## Examples

``` r
TOUR(1:10)
#> object of class ‘TOUR’ 
#> result of method ‘NA’ for 10 cities
#> tour length: unknown

## calculate a tour
data("USCA50")
tour <- solve_TSP(USCA50)
tour
#> object of class ‘TOUR’ 
#> result of method ‘arbitrary_insertion+two_opt’ for 50 cities
#> tour length: 14952 

## get tour length directly from tour
tour_length(tour)
#> [1] 14952

## get permutation vector
as.integer(tour)
#>  [1] 12 30 28 20 23 16  1  7  4 17 26 48 33 44 45 25  8  5 36 27 31 49 32 35 11
#> [26] 47  2  9 21 22 37 42 41 24 29  3 38 43 19 15 46 34 40 39 50 13  6 18 10 14

## show labels
labels(tour)
#>  [1] "Atlanta, GA"       "Birmingham, AL"    "Biloxi, MS"       
#>  [4] "Baton Rouge, LA"   "Beaumont, TX"      "Austin, TX"       
#>  [7] "Abilene, TX"       "Amarillo, TX"      "Albuquerque, NM"  
#> [10] "Bakersfield, CA"   "Berkeley, CA"      "Carson City, NV"  
#> [13] "Boise, ID"         "Butte, MT"         "Calgary, AB"      
#> [16] "Bellingham, WA"    "Anchorage, AK"     "Alert, NT"        
#> [19] "Brandon, MB"       "Billings, MT"      "Bismarck, ND"     
#> [22] "Cedar Rapids, IA"  "Bloomington, IL"   "Bowling Green, KY"
#> [25] "Ashland, KY"       "Canton, OH"        "Akron, OH"        
#> [28] "Ann Arbor, MI"     "Battle Creek, MI"  "Bay City, MI"     
#> [31] "Brantford, ON"     "Burlington, ONT"   "Buffalo, NY"      
#> [34] "Belleville, ON"    "Binghamtom, NY"    "Albany, NY"       
#> [37] "Brattleboro, VT"   "Burlington, VT"    "Bangor, ME"       
#> [40] "Augusta, ME"       "Cambridge, MA"     "Boston, MA"       
#> [43] "Brockton, MA"      "Bridgeport, CT"    "Central Islip, NY"
#> [46] "Atlantic City, NJ" "Allentown, PA"     "Baltimore, MD"    
#> [49] "Asheville, NC"     "Augusta, GA"      
```
