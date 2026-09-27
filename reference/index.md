# Package index

## Defining Traveling Salesperson Problems

Create symmetric, asymmetric, and Euclidean traveling salesperson
problems.

- [`TSP()`](http://michael.hahsler.net/TSP/reference/TSP.md)
  [`as.TSP()`](http://michael.hahsler.net/TSP/reference/TSP.md)
  [`as.dist(`*`<TSP>`*`)`](http://michael.hahsler.net/TSP/reference/TSP.md)
  [`print(`*`<TSP>`*`)`](http://michael.hahsler.net/TSP/reference/TSP.md)
  [`n_of_cities()`](http://michael.hahsler.net/TSP/reference/TSP.md)
  [`labels(`*`<TSP>`*`)`](http://michael.hahsler.net/TSP/reference/TSP.md)
  [`image(`*`<TSP>`*`)`](http://michael.hahsler.net/TSP/reference/TSP.md)
  : Class TSP – Symmetric traveling salesperson problem
- [`ETSP()`](http://michael.hahsler.net/TSP/reference/ETSP.md)
  [`as.ETSP()`](http://michael.hahsler.net/TSP/reference/ETSP.md)
  [`as.TSP(`*`<ETSP>`*`)`](http://michael.hahsler.net/TSP/reference/ETSP.md)
  [`as.matrix(`*`<ETSP>`*`)`](http://michael.hahsler.net/TSP/reference/ETSP.md)
  [`print(`*`<ETSP>`*`)`](http://michael.hahsler.net/TSP/reference/ETSP.md)
  [`n_of_cities(`*`<ETSP>`*`)`](http://michael.hahsler.net/TSP/reference/ETSP.md)
  [`labels(`*`<ETSP>`*`)`](http://michael.hahsler.net/TSP/reference/ETSP.md)
  [`image(`*`<ETSP>`*`)`](http://michael.hahsler.net/TSP/reference/ETSP.md)
  [`plot(`*`<ETSP>`*`)`](http://michael.hahsler.net/TSP/reference/ETSP.md)
  : Class ETSP – Euclidean traveling salesperson problem
- [`ATSP()`](http://michael.hahsler.net/TSP/reference/ATSP.md)
  [`as.ATSP()`](http://michael.hahsler.net/TSP/reference/ATSP.md)
  [`print(`*`<ATSP>`*`)`](http://michael.hahsler.net/TSP/reference/ATSP.md)
  [`n_of_cities(`*`<ATSP>`*`)`](http://michael.hahsler.net/TSP/reference/ATSP.md)
  [`labels(`*`<ATSP>`*`)`](http://michael.hahsler.net/TSP/reference/ATSP.md)
  [`image(`*`<ATSP>`*`)`](http://michael.hahsler.net/TSP/reference/ATSP.md)
  [`as.matrix(`*`<ATSP>`*`)`](http://michael.hahsler.net/TSP/reference/ATSP.md)
  : Class ATSP – Asymmetric traveling salesperson problem
- [`insert_dummy()`](http://michael.hahsler.net/TSP/reference/insert_dummy.md)
  : Insert dummy cities into a distance matrix
- [`reformulate_ATSP_as_TSP()`](http://michael.hahsler.net/TSP/reference/reformulate_ATSP_as_TSP.md)
  [`filter_ATSP_as_TSP_dummies()`](http://michael.hahsler.net/TSP/reference/reformulate_ATSP_as_TSP.md)
  : Reformulate an ATSP as a symmetric TSP

## Solvers

Solve TSPs by creating a TOUR

- [`solve_TSP()`](http://michael.hahsler.net/TSP/reference/solve_TSP.md)
  : TSP solver interface
- [`concorde_path()`](http://michael.hahsler.net/TSP/reference/Concorde.md)
  [`concorde_help()`](http://michael.hahsler.net/TSP/reference/Concorde.md)
  [`linkern_help()`](http://michael.hahsler.net/TSP/reference/Concorde.md)
  : Using the Concorde TSP Solver

## Tours

Store TSP solutions as permutations, calculate their lengths, and cut
cycles to form paths.

- [`TOUR()`](http://michael.hahsler.net/TSP/reference/TOUR.md)
  [`as.TOUR()`](http://michael.hahsler.net/TSP/reference/TOUR.md)
  [`print(`*`<TOUR>`*`)`](http://michael.hahsler.net/TSP/reference/TOUR.md)
  : Class TOUR – Solution to a traveling salesperson problem
- [`tour_length()`](http://michael.hahsler.net/TSP/reference/tour_length.md)
  : Calculate the length of a tour
- [`cut_tour()`](http://michael.hahsler.net/TSP/reference/cut_tour.md) :
  Cut a tour to form a path

## Data

Data sets and import/export funcitons.

- [`read_TSPLIB()`](http://michael.hahsler.net/TSP/reference/TSPLIB.md)
  [`write_TSPLIB()`](http://michael.hahsler.net/TSP/reference/TSPLIB.md)
  : Read and write TSPLIB files
- [`USCA`](http://michael.hahsler.net/TSP/reference/USCA.md)
  [`USCA312`](http://michael.hahsler.net/TSP/reference/USCA.md)
  [`USCA312_GPS`](http://michael.hahsler.net/TSP/reference/USCA.md)
  [`USCA50`](http://michael.hahsler.net/TSP/reference/USCA.md) :
  USCA312/USCA50 – 312/50 cities in the US and Canada
