# Cut a tour to form a path

Cuts a tour at a specified city to form a path.

## Usage

``` r
cut_tour(x, cut, exclude_cut = TRUE)

# S3 method for class 'TOUR'
cut_tour(x, cut, exclude_cut = TRUE)
```

## Arguments

- x:

  an object of class
  [TOUR](http://michael.hahsler.net/TSP/reference/TOUR.md).

- cut:

  the index or label of the city/cities to cut the tour.

- exclude_cut:

  exclude the city where we cut? If `FALSE`, the city at the cut is
  included in the path as the first city.

## Value

Returns a named vector with city ids forming the path. If multiple cuts
are used then a list with paths is returned.

## See also

Other TOUR:
[`TOUR()`](http://michael.hahsler.net/TSP/reference/TOUR.md),
[`solve_TSP()`](http://michael.hahsler.net/TSP/reference/solve_TSP.md),
[`tour_length()`](http://michael.hahsler.net/TSP/reference/tour_length.md)

## Author

Michael Hahsler

## Examples

``` r
data("USCA50")

## find a path starting at Austin, TX
tour <- solve_TSP(USCA50)
path <- cut_tour(tour, cut = "Austin, TX", exclude_cut = FALSE)
path
#>        Austin, TX      Beaumont, TX   Baton Rouge, LA        Biloxi, MS 
#>                16                23                20                28 
#>    Birmingham, AL Bowling Green, KY       Atlanta, GA       Augusta, GA 
#>                30                35                12                14 
#>     Asheville, NC       Ashland, KY        Canton, OH         Akron, OH 
#>                10                11                47                 2 
#>    Binghamtom, NY     Allentown, PA     Baltimore, MD Atlantic City, NJ 
#>                29                 6                18                13 
#> Central Islip, NY    Bridgeport, CT      Brockton, MA        Boston, MA 
#>                50                39                40                34 
#>     Cambridge, MA       Augusta, ME        Bangor, ME    Burlington, VT 
#>                46                15                19                43 
#>   Brattleboro, VT        Albany, NY    Belleville, ON       Buffalo, NY 
#>                38                 3                24                41 
#>   Burlington, ONT     Brantford, ON      Bay City, MI     Ann Arbor, MI 
#>                42                37                22                 9 
#>  Battle Creek, MI   Bloomington, IL  Cedar Rapids, IA      Bismarck, ND 
#>                21                32                49                31 
#>       Brandon, MB      Billings, MT         Butte, MT       Calgary, AB 
#>                36                27                44                45 
#>         Alert, NT     Anchorage, AK    Bellingham, WA         Boise, ID 
#>                 5                 8                25                33 
#>   Carson City, NV      Berkeley, CA   Bakersfield, CA   Albuquerque, NM 
#>                48                26                17                 4 
#>      Amarillo, TX       Abilene, TX 
#>                 7                 1 

## cut the tours at two cities
tour <- solve_TSP(USCA50)
path <- cut_tour(tour, cut = c("Austin, TX", "Cambridge, MA"), exclude_cut = FALSE)
path
#> [[1]]
#>       Austin, TX      Abilene, TX     Amarillo, TX  Albuquerque, NM 
#>               16                1                7                4 
#>  Bakersfield, CA     Berkeley, CA  Carson City, NV        Boise, ID 
#>               17               26               48               33 
#>        Butte, MT     Billings, MT     Bismarck, ND      Brandon, MB 
#>               44               27               31               36 
#>      Calgary, AB   Bellingham, WA    Anchorage, AK        Alert, NT 
#>               45               25                8                5 
#> Cedar Rapids, IA  Bloomington, IL Battle Creek, MI     Bay City, MI 
#>               49               32               21               22 
#>    Ann Arbor, MI        Akron, OH       Canton, OH    Brantford, ON 
#>                9                2               47               37 
#>  Burlington, ONT      Buffalo, NY   Belleville, ON   Binghamtom, NY 
#>               42               41               24               29 
#>       Albany, NY  Brattleboro, VT   Burlington, VT       Bangor, ME 
#>                3               38               43               19 
#>      Augusta, ME 
#>               15 
#> 
#> [[2]]
#>     Cambridge, MA        Boston, MA      Brockton, MA    Bridgeport, CT 
#>                46                34                40                39 
#> Central Islip, NY Atlantic City, NJ     Allentown, PA     Baltimore, MD 
#>                50                13                 6                18 
#>       Ashland, KY     Asheville, NC       Augusta, GA       Atlanta, GA 
#>                11                10                14                12 
#> Bowling Green, KY    Birmingham, AL        Biloxi, MS   Baton Rouge, LA 
#>                35                30                28                20 
#>      Beaumont, TX 
#>                23 
#> 

## cut a tour at the largest gap using a dummy city
tsp <- insert_dummy(USCA50, label = "cut")
tour <- solve_TSP(tsp)

## cut tour into path at the dummy city
path <- cut_tour(tour, "cut")
path
#>         Alert, NT     Anchorage, AK    Bellingham, WA       Calgary, AB 
#>                 5                 8                25                45 
#>       Brandon, MB      Bismarck, ND      Billings, MT         Butte, MT 
#>                36                31                27                44 
#>         Boise, ID   Carson City, NV      Berkeley, CA   Bakersfield, CA 
#>                33                48                26                17 
#>   Albuquerque, NM      Amarillo, TX       Abilene, TX        Austin, TX 
#>                 4                 7                 1                16 
#>      Beaumont, TX   Baton Rouge, LA        Biloxi, MS    Birmingham, AL 
#>                23                20                28                30 
#>       Atlanta, GA       Augusta, GA     Asheville, NC     Baltimore, MD 
#>                12                14                10                18 
#>     Allentown, PA Atlantic City, NJ Central Islip, NY    Bridgeport, CT 
#>                 6                13                50                39 
#>        Albany, NY   Brattleboro, VT      Brockton, MA        Boston, MA 
#>                 3                38                40                34 
#>     Cambridge, MA       Augusta, ME        Bangor, ME    Burlington, VT 
#>                46                15                19                43 
#>    Binghamtom, NY    Belleville, ON       Buffalo, NY   Burlington, ONT 
#>                29                24                41                42 
#>     Brantford, ON      Bay City, MI  Battle Creek, MI     Ann Arbor, MI 
#>                37                22                21                 9 
#>         Akron, OH        Canton, OH       Ashland, KY Bowling Green, KY 
#>                 2                47                11                35 
#>   Bloomington, IL  Cedar Rapids, IA 
#>                32                49 
```
