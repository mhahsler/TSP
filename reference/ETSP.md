# Class ETSP – Euclidean traveling salesperson problem

Constructor to create an instance of a Euclidean traveling salesperson
problem (TSP) represented by city coordinates and some auxiliary
methods.

## Usage

``` r
ETSP(x, labels = NULL)

as.ETSP(x)

# S3 method for class 'matrix'
as.ETSP(x)

# S3 method for class 'data.frame'
as.ETSP(x)

# S3 method for class 'ETSP'
as.TSP(x)

# S3 method for class 'ETSP'
as.matrix(x, ...)

# S3 method for class 'ETSP'
print(x, ...)

# S3 method for class 'ETSP'
n_of_cities(x)

# S3 method for class 'ETSP'
labels(object, ...)

# S3 method for class 'ETSP'
image(x, order, col = gray.colors(64), ...)

# S3 method for class 'ETSP'
plot(x, y = NULL, tour = NULL, tour_lty = 2, tour_col = 2, labels = TRUE, ...)
```

## Arguments

- x, object:

  an object (data.frame or matrix) to be converted into a `ETSP` or, for
  the methods, an object of class `ETSP`.

- labels:

  logical; plot city labels.

- ...:

  further arguments are passed on.

- order:

  order of cities for the image as an integer vector or an object of
  class [TOUR](http://michael.hahsler.net/TSP/reference/TOUR.md).

- col:

  color scheme for image.

- tour, y:

  a tour to be visualized.

- tour_lty, tour_col:

  line type and color for tour.

## Value

- `ETSP()` returns `x` as an object of class `ETSP`.

- [`n_of_cities()`](http://michael.hahsler.net/TSP/reference/TSP.md)
  returns the number of cities in `x`.

- [`labels()`](https://rdrr.io/r/base/labels.html) returns a vector with
  the names of the cities in `x`.

## Details

Objects of class `ETSP` are internally represented as `matrix` objects
(use [`as.matrix()`](https://rdrr.io/r/base/matrix.html) to get the
`matrix` object).

## See also

Other TSP: [`ATSP()`](http://michael.hahsler.net/TSP/reference/ATSP.md),
[`Concorde`](http://michael.hahsler.net/TSP/reference/Concorde.md),
[`TSP()`](http://michael.hahsler.net/TSP/reference/TSP.md),
[`TSPLIB`](http://michael.hahsler.net/TSP/reference/TSPLIB.md),
[`insert_dummy()`](http://michael.hahsler.net/TSP/reference/insert_dummy.md),
[`reformulate_ATSP_as_TSP()`](http://michael.hahsler.net/TSP/reference/reformulate_ATSP_as_TSP.md),
[`solve_TSP()`](http://michael.hahsler.net/TSP/reference/solve_TSP.md)

## Author

Michael Hahsler

## Examples

``` r
## create a random ETSP
n <- 20
x <- data.frame(x = runif(n), y = runif(n), row.names = LETTERS[1:n])
etsp <- ETSP(x)
etsp
#> object of class ‘ETSP’ 
#> 20 cities (Euclidean TSP)

## use some methods
n_of_cities(etsp)
#> [1] 20
labels(etsp)
#>  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
#> [20] "T"

## plot ETSP and solution
tour <- solve_TSP(etsp)
tour
#> object of class ‘TOUR’ 
#> result of method ‘arbitrary_insertion+two_opt’ for 20 cities
#> tour length: 3.898495 

plot(etsp, tour, tour_col = "red")


# plot with custom labels
plot(etsp, tour, tour_col = "red", labels = FALSE)
text(etsp, paste("City", rownames(etsp)), pos = 1)
```
