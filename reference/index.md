# Package index

## Defining Traveling Salesperson Problems

Create symmetric, asymmetric, and Euclidean traveling salesperson
problems.

- [`TSP()`](https://michael.hahsler.net/TSP/reference/TSP.md)
  [`as.TSP()`](https://michael.hahsler.net/TSP/reference/TSP.md)
  [`as.dist(`*`<TSP>`*`)`](https://michael.hahsler.net/TSP/reference/TSP.md)
  [`print(`*`<TSP>`*`)`](https://michael.hahsler.net/TSP/reference/TSP.md)
  [`n_of_cities()`](https://michael.hahsler.net/TSP/reference/TSP.md)
  [`labels(`*`<TSP>`*`)`](https://michael.hahsler.net/TSP/reference/TSP.md)
  [`image(`*`<TSP>`*`)`](https://michael.hahsler.net/TSP/reference/TSP.md)
  : Class TSP – Symmetric traveling salesperson problem
- [`ETSP()`](https://michael.hahsler.net/TSP/reference/ETSP.md)
  [`as.ETSP()`](https://michael.hahsler.net/TSP/reference/ETSP.md)
  [`as.TSP(`*`<ETSP>`*`)`](https://michael.hahsler.net/TSP/reference/ETSP.md)
  [`as.matrix(`*`<ETSP>`*`)`](https://michael.hahsler.net/TSP/reference/ETSP.md)
  [`print(`*`<ETSP>`*`)`](https://michael.hahsler.net/TSP/reference/ETSP.md)
  [`n_of_cities(`*`<ETSP>`*`)`](https://michael.hahsler.net/TSP/reference/ETSP.md)
  [`labels(`*`<ETSP>`*`)`](https://michael.hahsler.net/TSP/reference/ETSP.md)
  [`image(`*`<ETSP>`*`)`](https://michael.hahsler.net/TSP/reference/ETSP.md)
  [`plot(`*`<ETSP>`*`)`](https://michael.hahsler.net/TSP/reference/ETSP.md)
  : Class ETSP – Euclidean traveling salesperson problem
- [`ATSP()`](https://michael.hahsler.net/TSP/reference/ATSP.md)
  [`as.ATSP()`](https://michael.hahsler.net/TSP/reference/ATSP.md)
  [`print(`*`<ATSP>`*`)`](https://michael.hahsler.net/TSP/reference/ATSP.md)
  [`n_of_cities(`*`<ATSP>`*`)`](https://michael.hahsler.net/TSP/reference/ATSP.md)
  [`labels(`*`<ATSP>`*`)`](https://michael.hahsler.net/TSP/reference/ATSP.md)
  [`image(`*`<ATSP>`*`)`](https://michael.hahsler.net/TSP/reference/ATSP.md)
  [`as.matrix(`*`<ATSP>`*`)`](https://michael.hahsler.net/TSP/reference/ATSP.md)
  : Class ATSP – Asymmetric traveling salesperson problem
- [`insert_dummy()`](https://michael.hahsler.net/TSP/reference/insert_dummy.md)
  : Insert dummy cities into a distance matrix
- [`reformulate_ATSP_as_TSP()`](https://michael.hahsler.net/TSP/reference/reformulate_ATSP_as_TSP.md)
  [`filter_ATSP_as_TSP_dummies()`](https://michael.hahsler.net/TSP/reference/reformulate_ATSP_as_TSP.md)
  : Reformulate an ATSP as a symmetric TSP

## Solvers

Solve TSPs by creating a TOUR

- [`solve_TSP()`](https://michael.hahsler.net/TSP/reference/solve_TSP.md)
  : TSP solver interface
- [`concorde_path()`](https://michael.hahsler.net/TSP/reference/Concorde.md)
  [`concorde_help()`](https://michael.hahsler.net/TSP/reference/Concorde.md)
  [`linkern_help()`](https://michael.hahsler.net/TSP/reference/Concorde.md)
  : Using the Concorde TSP Solver

## Tours

Store TSP solutions as permutations, calculate their lengths, and cut
cycles to form paths.

- [`TOUR()`](https://michael.hahsler.net/TSP/reference/TOUR.md)
  [`as.TOUR()`](https://michael.hahsler.net/TSP/reference/TOUR.md)
  [`print(`*`<TOUR>`*`)`](https://michael.hahsler.net/TSP/reference/TOUR.md)
  : Class TOUR – Solution to a traveling salesperson problem
- [`tour_length()`](https://michael.hahsler.net/TSP/reference/tour_length.md)
  : Calculate the length of a tour
- [`cut_tour()`](https://michael.hahsler.net/TSP/reference/cut_tour.md)
  : Cut a tour to form a path

## Data

Data sets and import/export funcitons.

- [`read_TSPLIB()`](https://michael.hahsler.net/TSP/reference/TSPLIB.md)
  [`write_TSPLIB()`](https://michael.hahsler.net/TSP/reference/TSPLIB.md)
  : Read and write TSPLIB files
- [`USCA`](https://michael.hahsler.net/TSP/reference/USCA.md)
  [`USCA312`](https://michael.hahsler.net/TSP/reference/USCA.md)
  [`USCA312_GPS`](https://michael.hahsler.net/TSP/reference/USCA.md)
  [`USCA50`](https://michael.hahsler.net/TSP/reference/USCA.md) :
  USCA312/USCA50 – 312/50 cities in the US and Canada
