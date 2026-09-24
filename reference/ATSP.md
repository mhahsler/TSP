# Class ATSP – Asymmetric traveling salesperson problem

Constructor to create an instance of the asymmetric traveling
salesperson problem (ATSP) and some auxiliary methods.

## Usage

``` r
ATSP(x, labels = NULL, method = NULL)

as.ATSP(x)

# S3 method for class 'matrix'
as.ATSP(x)

# S3 method for class 'dist'
as.ATSP(x)

# S3 method for class 'ATSP'
print(x, ...)

# S3 method for class 'ATSP'
n_of_cities(x)

# S3 method for class 'ATSP'
labels(object, ...)

# S3 method for class 'ATSP'
image(x, order, col = gray.colors(64), ...)

# S3 method for class 'ATSP'
as.matrix(x, ...)
```

## Arguments

- x, object:

  an object (a square matrix) to be converted into an `ATSP` or, for the
  methods, an object of class `ATSP`.

- labels:

  optional city labels. If not given, labels are taken from `x`.

- method:

  optional name of the distance metric.

- ...:

  further arguments are passed on.

- order:

  order of cities as an integer vector or an object of class `TOUR`.

- col:

  color scheme for image.

## Value

- `ATSP()` returns `x` as an object of class `ATSP`.

- [`n_of_cities()`](http://michael.hahsler.net/TSP/reference/TSP.md)
  returns the number of cities in `x`.

- [`labels()`](https://rdrr.io/r/base/labels.html) returns a vector with
  the names of the cities in `x`.

## Details

Objects of class `ATSP` are internally represented by a matrix (use
[`as.matrix()`](https://rdrr.io/r/base/matrix.html) to get just the
matrix).

ATSPs can be transformed into (larger) symmetric TSPs using
[`reformulate_ATSP_as_TSP()`](http://michael.hahsler.net/TSP/reference/reformulate_ATSP_as_TSP.md).

## See also

Other TSP:
[`Concorde`](http://michael.hahsler.net/TSP/reference/Concorde.md),
[`ETSP()`](http://michael.hahsler.net/TSP/reference/ETSP.md),
[`TSP()`](http://michael.hahsler.net/TSP/reference/TSP.md),
[`TSPLIB`](http://michael.hahsler.net/TSP/reference/TSPLIB.md),
[`insert_dummy()`](http://michael.hahsler.net/TSP/reference/insert_dummy.md),
[`reformulate_ATSP_as_TSP()`](http://michael.hahsler.net/TSP/reference/reformulate_ATSP_as_TSP.md),
[`solve_TSP()`](http://michael.hahsler.net/TSP/reference/solve_TSP.md)

## Author

Michael Hahsler

## Examples

``` r
data <- matrix(runif(10^2), ncol = 10, dimnames = list(1:10, 1:10))

atsp <- ATSP(data)
atsp
#> object of class ‘ATSP’  (asymmetric TSP) 
#> 10 cities (distance ‘unknown’) 

## use some methods
n_of_cities(atsp)
#> [1] 10
labels(atsp)
#>  [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10"

## calculate a tour
tour <- solve_TSP(atsp, method = "nn")
tour
#> object of class ‘TOUR’ 
#> result of method ‘nn’ for 10 cities
#> tour length: 2.551933 

tour_length(tour)
#> [1] 2.551933

image(atsp, tour)
```
