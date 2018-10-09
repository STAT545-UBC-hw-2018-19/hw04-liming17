hw04\_gapminder
================
Liming Liu
2018-10-6

# Homework 04: Tidy data and joins

``` r
library(gapminder)
library(knitr)
library(kableExtra)
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ───────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

### Data Reshaping Prompts (and relationship to aggregation)

Problem: You have data in one “shape” but you wish it were in another.
Usually this is because the alternative shape is superior for presenting
a table, making a figure, or doing aggregation and statistical analysis.

Solution: Reshape your data. For simple reshaping, gather() and spread()
from tidyr will suffice. Do the thing that is possible / easier now that
your data has a new
shape.

#### Activity 2 : Make a tibble with one row per year and columns for life expectancy for two or more countries. Take advantage of this new data shape to scatterplot life expectancy for one country against that of another.

First take a brief look at the data, let’s choose the country with the
highest lifeExp in each continent.

``` r
gapminder
```

    ## # A tibble: 1,704 x 6
    ##    country     continent  year lifeExp      pop gdpPercap
    ##    <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
    ##  1 Afghanistan Asia       1952    28.8  8425333      779.
    ##  2 Afghanistan Asia       1957    30.3  9240934      821.
    ##  3 Afghanistan Asia       1962    32.0 10267083      853.
    ##  4 Afghanistan Asia       1967    34.0 11537966      836.
    ##  5 Afghanistan Asia       1972    36.1 13079460      740.
    ##  6 Afghanistan Asia       1977    38.4 14880372      786.
    ##  7 Afghanistan Asia       1982    39.9 12881816      978.
    ##  8 Afghanistan Asia       1987    40.8 13867957      852.
    ##  9 Afghanistan Asia       1992    41.7 16317921      649.
    ## 10 Afghanistan Asia       1997    41.8 22227415      635.
    ## # ... with 1,694 more rows

Africa:

``` r
Af <- gapminder %>% 
  filter(continent == "Africa") %>% 
  group_by(country) %>% 
  summarise(lifeExp_mean = mean(lifeExp)) %>% 
  arrange(desc(lifeExp_mean))
knitr::kable(Af) %>% 
  kable_styling(bootstrap_options = "bordered",latex_options = "basic",full_width = F)
```

<table class="table table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

country

</th>

<th style="text-align:right;">

lifeExp\_mean

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Reunion

</td>

<td style="text-align:right;">

66.64425

</td>

</tr>

<tr>

<td style="text-align:left;">

Mauritius

</td>

<td style="text-align:right;">

64.95325

</td>

</tr>

<tr>

<td style="text-align:left;">

Tunisia

</td>

<td style="text-align:right;">

60.72100

</td>

</tr>

<tr>

<td style="text-align:left;">

Libya

</td>

<td style="text-align:right;">

59.30417

</td>

</tr>

<tr>

<td style="text-align:left;">

Algeria

</td>

<td style="text-align:right;">

59.03017

</td>

</tr>

<tr>

<td style="text-align:left;">

Sao Tome and Principe

</td>

<td style="text-align:right;">

57.89633

</td>

</tr>

<tr>

<td style="text-align:left;">

Morocco

</td>

<td style="text-align:right;">

57.60883

</td>

</tr>

<tr>

<td style="text-align:left;">

Egypt

</td>

<td style="text-align:right;">

56.24300

</td>

</tr>

<tr>

<td style="text-align:left;">

Botswana

</td>

<td style="text-align:right;">

54.59750

</td>

</tr>

<tr>

<td style="text-align:left;">

South Africa

</td>

<td style="text-align:right;">

53.99317

</td>

</tr>

<tr>

<td style="text-align:left;">

Namibia

</td>

<td style="text-align:right;">

53.49133

</td>

</tr>

<tr>

<td style="text-align:left;">

Kenya

</td>

<td style="text-align:right;">

52.68100

</td>

</tr>

<tr>

<td style="text-align:left;">

Zimbabwe

</td>

<td style="text-align:right;">

52.66317

</td>

</tr>

<tr>

<td style="text-align:left;">

Congo, Rep.

</td>

<td style="text-align:right;">

52.50192

</td>

</tr>

<tr>

<td style="text-align:left;">

Comoros

</td>

<td style="text-align:right;">

52.38175

</td>

</tr>

<tr>

<td style="text-align:left;">

Ghana

</td>

<td style="text-align:right;">

52.34067

</td>

</tr>

<tr>

<td style="text-align:left;">

Mauritania

</td>

<td style="text-align:right;">

52.30208

</td>

</tr>

<tr>

<td style="text-align:left;">

Togo

</td>

<td style="text-align:right;">

51.49875

</td>

</tr>

<tr>

<td style="text-align:left;">

Gabon

</td>

<td style="text-align:right;">

51.22050

</td>

</tr>

<tr>

<td style="text-align:left;">

Senegal

</td>

<td style="text-align:right;">

50.62592

</td>

</tr>

<tr>

<td style="text-align:left;">

Lesotho

</td>

<td style="text-align:right;">

50.00708

</td>

</tr>

<tr>

<td style="text-align:left;">

Swaziland

</td>

<td style="text-align:right;">

49.00242

</td>

</tr>

<tr>

<td style="text-align:left;">

Benin

</td>

<td style="text-align:right;">

48.77992

</td>

</tr>

<tr>

<td style="text-align:left;">

Cote d’Ivoire

</td>

<td style="text-align:right;">

48.43617

</td>

</tr>

<tr>

<td style="text-align:left;">

Sudan

</td>

<td style="text-align:right;">

48.40050

</td>

</tr>

<tr>

<td style="text-align:left;">

Cameroon

</td>

<td style="text-align:right;">

48.12850

</td>

</tr>

<tr>

<td style="text-align:left;">

Tanzania

</td>

<td style="text-align:right;">

47.91233

</td>

</tr>

<tr>

<td style="text-align:left;">

Madagascar

</td>

<td style="text-align:right;">

47.77058

</td>

</tr>

<tr>

<td style="text-align:left;">

Uganda

</td>

<td style="text-align:right;">

47.61883

</td>

</tr>

<tr>

<td style="text-align:left;">

Chad

</td>

<td style="text-align:right;">

46.77358

</td>

</tr>

<tr>

<td style="text-align:left;">

Djibouti

</td>

<td style="text-align:right;">

46.38075

</td>

</tr>

<tr>

<td style="text-align:left;">

Eritrea

</td>

<td style="text-align:right;">

45.99925

</td>

</tr>

<tr>

<td style="text-align:left;">

Zambia

</td>

<td style="text-align:right;">

45.99633

</td>

</tr>

<tr>

<td style="text-align:left;">

Burundi

</td>

<td style="text-align:right;">

44.81733

</td>

</tr>

<tr>

<td style="text-align:left;">

Burkina Faso

</td>

<td style="text-align:right;">

44.69400

</td>

</tr>

<tr>

<td style="text-align:left;">

Niger

</td>

<td style="text-align:right;">

44.55867

</td>

</tr>

<tr>

<td style="text-align:left;">

Congo, Dem. Rep.

</td>

<td style="text-align:right;">

44.54375

</td>

</tr>

<tr>

<td style="text-align:left;">

Ethiopia

</td>

<td style="text-align:right;">

44.47575

</td>

</tr>

<tr>

<td style="text-align:left;">

Gambia

</td>

<td style="text-align:right;">

44.40058

</td>

</tr>

<tr>

<td style="text-align:left;">

Central African Republic

</td>

<td style="text-align:right;">

43.86692

</td>

</tr>

<tr>

<td style="text-align:left;">

Nigeria

</td>

<td style="text-align:right;">

43.58133

</td>

</tr>

<tr>

<td style="text-align:left;">

Mali

</td>

<td style="text-align:right;">

43.41350

</td>

</tr>

<tr>

<td style="text-align:left;">

Malawi

</td>

<td style="text-align:right;">

43.35158

</td>

</tr>

<tr>

<td style="text-align:left;">

Guinea

</td>

<td style="text-align:right;">

43.23983

</td>

</tr>

<tr>

<td style="text-align:left;">

Equatorial Guinea

</td>

<td style="text-align:right;">

42.96000

</td>

</tr>

<tr>

<td style="text-align:left;">

Liberia

</td>

<td style="text-align:right;">

42.47625

</td>

</tr>

<tr>

<td style="text-align:left;">

Rwanda

</td>

<td style="text-align:right;">

41.48158

</td>

</tr>

<tr>

<td style="text-align:left;">

Somalia

</td>

<td style="text-align:right;">

40.98867

</td>

</tr>

<tr>

<td style="text-align:left;">

Mozambique

</td>

<td style="text-align:right;">

40.37950

</td>

</tr>

<tr>

<td style="text-align:left;">

Guinea-Bissau

</td>

<td style="text-align:right;">

39.21025

</td>

</tr>

<tr>

<td style="text-align:left;">

Angola

</td>

<td style="text-align:right;">

37.88350

</td>

</tr>

<tr>

<td style="text-align:left;">

Sierra Leone

</td>

<td style="text-align:right;">

36.76917

</td>

</tr>

</tbody>

</table>

We can see that the country with the highest lifeExp in Africa is
Reunion, so let’s choose that country. Use the similar method to choose
the countries in other continent, show the country with highest lifeExp
directly:

Asia:

``` r
(As <- gapminder %>% 
  filter(continent == "Asia") %>% 
  group_by(country) %>% 
  summarise(lifeExp_mean = mean(lifeExp)) %>% 
  filter(lifeExp_mean == max(lifeExp_mean)))
```

    ## # A tibble: 1 x 2
    ##   country lifeExp_mean
    ##   <fct>          <dbl>
    ## 1 Japan           74.8

Americas:

``` r
(Am <- gapminder %>% 
  filter(continent == "Americas") %>% 
  group_by(country) %>% 
  summarise(lifeExp_mean = mean(lifeExp)) %>% 
  filter(lifeExp_mean == max(lifeExp_mean)))
```

    ## # A tibble: 1 x 2
    ##   country lifeExp_mean
    ##   <fct>          <dbl>
    ## 1 Canada          74.9

Europe:

``` r
(Eu <- gapminder %>% 
  filter(continent == "Europe") %>% 
  group_by(country) %>% 
  summarise(lifeExp_mean = mean(lifeExp)) %>% 
  filter(lifeExp_mean == max(lifeExp_mean)))
```

    ## # A tibble: 1 x 2
    ##   country lifeExp_mean
    ##   <fct>          <dbl>
    ## 1 Iceland         76.5

Oceania:

``` r
(Oc <- gapminder %>% 
  filter(continent == "Oceania") %>% 
  group_by(country) %>% 
  summarise(lifeExp_mean = mean(lifeExp)) %>% 
  filter(lifeExp_mean == max(lifeExp_mean)))
```

    ## # A tibble: 1 x 2
    ##   country   lifeExp_mean
    ##   <fct>            <dbl>
    ## 1 Australia         74.7

So the chosen countries are : Reunion, Japan, Canada, Iceland,
Australia. Then let’s make the tibble. First, filter the data we want to
use:

``` r
(lifeExp_data <- gapminder %>% 
  filter(country == "Reunion" | country == "Japan" | country == "Canada" | country == "Iceland" | country == "Australia") %>% 
   select(-pop, -gdpPercap, -continent))
```

    ## # A tibble: 60 x 3
    ##    country    year lifeExp
    ##    <fct>     <int>   <dbl>
    ##  1 Australia  1952    69.1
    ##  2 Australia  1957    70.3
    ##  3 Australia  1962    70.9
    ##  4 Australia  1967    71.1
    ##  5 Australia  1972    71.9
    ##  6 Australia  1977    73.5
    ##  7 Australia  1982    74.7
    ##  8 Australia  1987    76.3
    ##  9 Australia  1992    77.6
    ## 10 Australia  1997    78.8
    ## # ... with 50 more rows

Then, use spread() to reshape the data. We need to put each country in a
column, and show the lifeExp, so the key is “country” column, and value
is “lifeExp” column

``` r
lifeExp_year <- lifeExp_data %>% 
  spread(key = "country", value = "lifeExp")
knitr::kable(lifeExp_year) %>% 
  kable_styling(bootstrap_options = "bordered",latex_options = "basic",full_width = F)
```

<table class="table table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:right;">

year

</th>

<th style="text-align:right;">

Australia

</th>

<th style="text-align:right;">

Canada

</th>

<th style="text-align:right;">

Iceland

</th>

<th style="text-align:right;">

Japan

</th>

<th style="text-align:right;">

Reunion

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

1952

</td>

<td style="text-align:right;">

69.120

</td>

<td style="text-align:right;">

68.750

</td>

<td style="text-align:right;">

72.490

</td>

<td style="text-align:right;">

63.030

</td>

<td style="text-align:right;">

52.724

</td>

</tr>

<tr>

<td style="text-align:right;">

1957

</td>

<td style="text-align:right;">

70.330

</td>

<td style="text-align:right;">

69.960

</td>

<td style="text-align:right;">

73.470

</td>

<td style="text-align:right;">

65.500

</td>

<td style="text-align:right;">

55.090

</td>

</tr>

<tr>

<td style="text-align:right;">

1962

</td>

<td style="text-align:right;">

70.930

</td>

<td style="text-align:right;">

71.300

</td>

<td style="text-align:right;">

73.680

</td>

<td style="text-align:right;">

68.730

</td>

<td style="text-align:right;">

57.666

</td>

</tr>

<tr>

<td style="text-align:right;">

1967

</td>

<td style="text-align:right;">

71.100

</td>

<td style="text-align:right;">

72.130

</td>

<td style="text-align:right;">

73.730

</td>

<td style="text-align:right;">

71.430

</td>

<td style="text-align:right;">

60.542

</td>

</tr>

<tr>

<td style="text-align:right;">

1972

</td>

<td style="text-align:right;">

71.930

</td>

<td style="text-align:right;">

72.880

</td>

<td style="text-align:right;">

74.460

</td>

<td style="text-align:right;">

73.420

</td>

<td style="text-align:right;">

64.274

</td>

</tr>

<tr>

<td style="text-align:right;">

1977

</td>

<td style="text-align:right;">

73.490

</td>

<td style="text-align:right;">

74.210

</td>

<td style="text-align:right;">

76.110

</td>

<td style="text-align:right;">

75.380

</td>

<td style="text-align:right;">

67.064

</td>

</tr>

<tr>

<td style="text-align:right;">

1982

</td>

<td style="text-align:right;">

74.740

</td>

<td style="text-align:right;">

75.760

</td>

<td style="text-align:right;">

76.990

</td>

<td style="text-align:right;">

77.110

</td>

<td style="text-align:right;">

69.885

</td>

</tr>

<tr>

<td style="text-align:right;">

1987

</td>

<td style="text-align:right;">

76.320

</td>

<td style="text-align:right;">

76.860

</td>

<td style="text-align:right;">

77.230

</td>

<td style="text-align:right;">

78.670

</td>

<td style="text-align:right;">

71.913

</td>

</tr>

<tr>

<td style="text-align:right;">

1992

</td>

<td style="text-align:right;">

77.560

</td>

<td style="text-align:right;">

77.950

</td>

<td style="text-align:right;">

78.770

</td>

<td style="text-align:right;">

79.360

</td>

<td style="text-align:right;">

73.615

</td>

</tr>

<tr>

<td style="text-align:right;">

1997

</td>

<td style="text-align:right;">

78.830

</td>

<td style="text-align:right;">

78.610

</td>

<td style="text-align:right;">

78.950

</td>

<td style="text-align:right;">

80.690

</td>

<td style="text-align:right;">

74.772

</td>

</tr>

<tr>

<td style="text-align:right;">

2002

</td>

<td style="text-align:right;">

80.370

</td>

<td style="text-align:right;">

79.770

</td>

<td style="text-align:right;">

80.500

</td>

<td style="text-align:right;">

82.000

</td>

<td style="text-align:right;">

75.744

</td>

</tr>

<tr>

<td style="text-align:right;">

2007

</td>

<td style="text-align:right;">

81.235

</td>

<td style="text-align:right;">

80.653

</td>

<td style="text-align:right;">

81.757

</td>

<td style="text-align:right;">

82.603

</td>

<td style="text-align:right;">

76.442

</td>

</tr>

</tbody>

</table>

Then, making a tibble with the data by using as\_tibble().

``` r
(lifeExp_tibble <- as_tibble(lifeExp_year))
```

    ## # A tibble: 12 x 6
    ##     year Australia Canada Iceland Japan Reunion
    ##    <int>     <dbl>  <dbl>   <dbl> <dbl>   <dbl>
    ##  1  1952      69.1   68.8    72.5  63.0    52.7
    ##  2  1957      70.3   70.0    73.5  65.5    55.1
    ##  3  1962      70.9   71.3    73.7  68.7    57.7
    ##  4  1967      71.1   72.1    73.7  71.4    60.5
    ##  5  1972      71.9   72.9    74.5  73.4    64.3
    ##  6  1977      73.5   74.2    76.1  75.4    67.1
    ##  7  1982      74.7   75.8    77.0  77.1    69.9
    ##  8  1987      76.3   76.9    77.2  78.7    71.9
    ##  9  1992      77.6   78.0    78.8  79.4    73.6
    ## 10  1997      78.8   78.6    79.0  80.7    74.8
    ## 11  2002      80.4   79.8    80.5  82      75.7
    ## 12  2007      81.2   80.7    81.8  82.6    76.4

Now, let’s scatterplot life expectancy for one country against that of
another. x-axes is year, y-axes is lifeExp, each country is in different
colour:

``` r
data2 <- lifeExp_tibble %>% 
  melt(id = "year")
data2 %>% 
  ggplot(aes(year, value, color = variable)) + geom_point() + ylab("lifeExp") + ggtitle("scatter plot for lifeExp in each country between 1952 ~ 2007") + geom_smooth(se=FALSE)
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](hw04_Liming_Liu_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

We can see that all the countries have an increasing lifeExp, and both
Japan and Reunion have a very sharp growth between 1952 to 1980. And
Japan goes to top in 1987. However, Reunion always has the lowest
lifeExp during the
years.

### Join Prompts (join, merge, look up)

#### Activity 1: Create a second data frame, complementary to Gapminder. Join this with (part of) Gapminder using a dplyr join function and make some observations about the process and result. Explore the different types of joins.

Let’s build one row per continent, a continent variable and one or more
variables with extra info, so add the largest country, the area of the
largest country, the capital of the largest country, and the time zone
of the capital. Call the new data frame new\_data, and in order to
observe result, let’s drop Oceania in
new\_data

``` r
new_data <- tibble(continent = c("Africa", "Americas", "Asia", "Europe"), largestCountry = c("Algeria", "Canada", "China", "Russia"), areaOfCountrySqkm = c("2,381,741", "9,984,670", "9,596,961", "17,098,242"), capital = c("Algiers", "Ottawa", "Beijing", "Moscow"), timeZone = c("GMT+1", "GMT-4", "GMT+8", "GMT+3"))
knitr::kable(new_data) %>% 
  kable_styling(bootstrap_options = "bordered",latex_options = "basic",full_width = F)
```

<table class="table table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

continent

</th>

<th style="text-align:left;">

largestCountry

</th>

<th style="text-align:left;">

areaOfCountrySqkm

</th>

<th style="text-align:left;">

capital

</th>

<th style="text-align:left;">

timeZone

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Africa

</td>

<td style="text-align:left;">

Algeria

</td>

<td style="text-align:left;">

2,381,741

</td>

<td style="text-align:left;">

Algiers

</td>

<td style="text-align:left;">

GMT+1

</td>

</tr>

<tr>

<td style="text-align:left;">

Americas

</td>

<td style="text-align:left;">

Canada

</td>

<td style="text-align:left;">

9,984,670

</td>

<td style="text-align:left;">

Ottawa

</td>

<td style="text-align:left;">

GMT-4

</td>

</tr>

<tr>

<td style="text-align:left;">

Asia

</td>

<td style="text-align:left;">

China

</td>

<td style="text-align:left;">

9,596,961

</td>

<td style="text-align:left;">

Beijing

</td>

<td style="text-align:left;">

GMT+8

</td>

</tr>

<tr>

<td style="text-align:left;">

Europe

</td>

<td style="text-align:left;">

Russia

</td>

<td style="text-align:left;">

17,098,242

</td>

<td style="text-align:left;">

Moscow

</td>

<td style="text-align:left;">

GMT+3

</td>

</tr>

</tbody>

</table>

First, let’s try left join new\_data into
    gapminder

``` r
(leftJoin <- left_join(gapminder, new_data, by = "continent"))
```

    ## Warning: Column `continent` joining factor and character vector, coercing
    ## into character vector

    ## # A tibble: 1,704 x 10
    ##    country continent  year lifeExp    pop gdpPercap largestCountry
    ##    <fct>   <chr>     <int>   <dbl>  <int>     <dbl> <chr>         
    ##  1 Afghan… Asia       1952    28.8 8.43e6      779. China         
    ##  2 Afghan… Asia       1957    30.3 9.24e6      821. China         
    ##  3 Afghan… Asia       1962    32.0 1.03e7      853. China         
    ##  4 Afghan… Asia       1967    34.0 1.15e7      836. China         
    ##  5 Afghan… Asia       1972    36.1 1.31e7      740. China         
    ##  6 Afghan… Asia       1977    38.4 1.49e7      786. China         
    ##  7 Afghan… Asia       1982    39.9 1.29e7      978. China         
    ##  8 Afghan… Asia       1987    40.8 1.39e7      852. China         
    ##  9 Afghan… Asia       1992    41.7 1.63e7      649. China         
    ## 10 Afghan… Asia       1997    41.8 2.22e7      635. China         
    ## # ... with 1,694 more rows, and 3 more variables: areaOfCountrySqkm <chr>,
    ## #   capital <chr>, timeZone <chr>

let’s see what happend to Oceania in leftJoin:

``` r
leftJoin %>% 
  filter(continent == "Oceania")
```

    ## # A tibble: 24 x 10
    ##    country continent  year lifeExp    pop gdpPercap largestCountry
    ##    <fct>   <chr>     <int>   <dbl>  <int>     <dbl> <chr>         
    ##  1 Austra… Oceania    1952    69.1 8.69e6    10040. <NA>          
    ##  2 Austra… Oceania    1957    70.3 9.71e6    10950. <NA>          
    ##  3 Austra… Oceania    1962    70.9 1.08e7    12217. <NA>          
    ##  4 Austra… Oceania    1967    71.1 1.19e7    14526. <NA>          
    ##  5 Austra… Oceania    1972    71.9 1.32e7    16789. <NA>          
    ##  6 Austra… Oceania    1977    73.5 1.41e7    18334. <NA>          
    ##  7 Austra… Oceania    1982    74.7 1.52e7    19477. <NA>          
    ##  8 Austra… Oceania    1987    76.3 1.63e7    21889. <NA>          
    ##  9 Austra… Oceania    1992    77.6 1.75e7    23425. <NA>          
    ## 10 Austra… Oceania    1997    78.8 1.86e7    26998. <NA>          
    ## # ... with 14 more rows, and 3 more variables: areaOfCountrySqkm <chr>,
    ## #   capital <chr>, timeZone <chr>

We can see that, by left join the gapminder adds the columns in
new\_data, and two data frames are matching by continent. The total
number of rows in gapminder doesn’t change, but for the Oceania, because
there are no matching rows in new\_data, so the data in new added
columns is NA.

Then let’s try right join new\_data into
    gapminder

``` r
(rightJoin <- right_join(gapminder, new_data, by = "continent"))
```

    ## Warning: Column `continent` joining factor and character vector, coercing
    ## into character vector

    ## # A tibble: 1,680 x 10
    ##    country continent  year lifeExp    pop gdpPercap largestCountry
    ##    <fct>   <chr>     <int>   <dbl>  <int>     <dbl> <chr>         
    ##  1 Algeria Africa     1952    43.1 9.28e6     2449. Algeria       
    ##  2 Algeria Africa     1957    45.7 1.03e7     3014. Algeria       
    ##  3 Algeria Africa     1962    48.3 1.10e7     2551. Algeria       
    ##  4 Algeria Africa     1967    51.4 1.28e7     3247. Algeria       
    ##  5 Algeria Africa     1972    54.5 1.48e7     4183. Algeria       
    ##  6 Algeria Africa     1977    58.0 1.72e7     4910. Algeria       
    ##  7 Algeria Africa     1982    61.4 2.00e7     5745. Algeria       
    ##  8 Algeria Africa     1987    65.8 2.33e7     5681. Algeria       
    ##  9 Algeria Africa     1992    67.7 2.63e7     5023. Algeria       
    ## 10 Algeria Africa     1997    69.2 2.91e7     4797. Algeria       
    ## # ... with 1,670 more rows, and 3 more variables: areaOfCountrySqkm <chr>,
    ## #   capital <chr>, timeZone <chr>

Let’s see if we can find Oceania in rightJoin data frame:

``` r
rightJoin %>% 
  filter(continent == "Oceania")
```

    ## # A tibble: 0 x 10
    ## # ... with 10 variables: country <fct>, continent <chr>, year <int>,
    ## #   lifeExp <dbl>, pop <int>, gdpPercap <dbl>, largestCountry <chr>,
    ## #   areaOfCountrySqkm <chr>, capital <chr>, timeZone <chr>

So the answer is no.

We can see that, the total rows of new\_data become less than
gapminder’s, and the order of continent is still the same as the order
of continent in new\_data, because there are more rows in gapminder for
a matching continent, so the oringinal new\_data rows became larger. And
because the Oceania data in gapminder can’t find a matching row in
new\_data, so it doesn’t exist in the rightJoin data frame.

Next, let’s try inner
    join:

``` r
(innerJoin <- inner_join(gapminder, new_data, by = "continent"))
```

    ## Warning: Column `continent` joining factor and character vector, coercing
    ## into character vector

    ## # A tibble: 1,680 x 10
    ##    country continent  year lifeExp    pop gdpPercap largestCountry
    ##    <fct>   <chr>     <int>   <dbl>  <int>     <dbl> <chr>         
    ##  1 Afghan… Asia       1952    28.8 8.43e6      779. China         
    ##  2 Afghan… Asia       1957    30.3 9.24e6      821. China         
    ##  3 Afghan… Asia       1962    32.0 1.03e7      853. China         
    ##  4 Afghan… Asia       1967    34.0 1.15e7      836. China         
    ##  5 Afghan… Asia       1972    36.1 1.31e7      740. China         
    ##  6 Afghan… Asia       1977    38.4 1.49e7      786. China         
    ##  7 Afghan… Asia       1982    39.9 1.29e7      978. China         
    ##  8 Afghan… Asia       1987    40.8 1.39e7      852. China         
    ##  9 Afghan… Asia       1992    41.7 1.63e7      649. China         
    ## 10 Afghan… Asia       1997    41.8 2.22e7      635. China         
    ## # ... with 1,670 more rows, and 3 more variables: areaOfCountrySqkm <chr>,
    ## #   capital <chr>, timeZone <chr>

Let’s see if we can find Oceania:

``` r
innerJoin %>% 
  filter(continent == "Oceania")
```

    ## # A tibble: 0 x 10
    ## # ... with 10 variables: country <fct>, continent <chr>, year <int>,
    ## #   lifeExp <dbl>, pop <int>, gdpPercap <dbl>, largestCountry <chr>,
    ## #   areaOfCountrySqkm <chr>, capital <chr>, timeZone <chr>

So the answer is no.

We can see by inner\_join, two dataframe are combined together by
matching continent. Since ther are no Oceania in new\_data, so the total
rows of the innerJoin data frame gets smaller than gapminder.

Next, try
    full\_join:

``` r
(fullJoin <- full_join(gapminder, new_data, by = "continent"))
```

    ## Warning: Column `continent` joining factor and character vector, coercing
    ## into character vector

    ## # A tibble: 1,704 x 10
    ##    country continent  year lifeExp    pop gdpPercap largestCountry
    ##    <fct>   <chr>     <int>   <dbl>  <int>     <dbl> <chr>         
    ##  1 Afghan… Asia       1952    28.8 8.43e6      779. China         
    ##  2 Afghan… Asia       1957    30.3 9.24e6      821. China         
    ##  3 Afghan… Asia       1962    32.0 1.03e7      853. China         
    ##  4 Afghan… Asia       1967    34.0 1.15e7      836. China         
    ##  5 Afghan… Asia       1972    36.1 1.31e7      740. China         
    ##  6 Afghan… Asia       1977    38.4 1.49e7      786. China         
    ##  7 Afghan… Asia       1982    39.9 1.29e7      978. China         
    ##  8 Afghan… Asia       1987    40.8 1.39e7      852. China         
    ##  9 Afghan… Asia       1992    41.7 1.63e7      649. China         
    ## 10 Afghan… Asia       1997    41.8 2.22e7      635. China         
    ## # ... with 1,694 more rows, and 3 more variables: areaOfCountrySqkm <chr>,
    ## #   capital <chr>, timeZone <chr>

Let’s see what happened to Oceania rows:

``` r
fullJoin %>% 
  filter(continent == "Oceania")
```

    ## # A tibble: 24 x 10
    ##    country continent  year lifeExp    pop gdpPercap largestCountry
    ##    <fct>   <chr>     <int>   <dbl>  <int>     <dbl> <chr>         
    ##  1 Austra… Oceania    1952    69.1 8.69e6    10040. <NA>          
    ##  2 Austra… Oceania    1957    70.3 9.71e6    10950. <NA>          
    ##  3 Austra… Oceania    1962    70.9 1.08e7    12217. <NA>          
    ##  4 Austra… Oceania    1967    71.1 1.19e7    14526. <NA>          
    ##  5 Austra… Oceania    1972    71.9 1.32e7    16789. <NA>          
    ##  6 Austra… Oceania    1977    73.5 1.41e7    18334. <NA>          
    ##  7 Austra… Oceania    1982    74.7 1.52e7    19477. <NA>          
    ##  8 Austra… Oceania    1987    76.3 1.63e7    21889. <NA>          
    ##  9 Austra… Oceania    1992    77.6 1.75e7    23425. <NA>          
    ## 10 Austra… Oceania    1997    78.8 1.86e7    26998. <NA>          
    ## # ... with 14 more rows, and 3 more variables: areaOfCountrySqkm <chr>,
    ## #   capital <chr>, timeZone <chr>

We can see that, by full\_join, all rows in gapminder and new\_data are
combined together, so the rows of fullJoin data frame is the same as the
gapminder.

Now, try
    semi\_join:

``` r
(semiJoin <- semi_join(gapminder, new_data, by = "continent")) 
```

    ## Warning: Column `continent` joining factor and character vector, coercing
    ## into character vector

    ## # A tibble: 1,680 x 6
    ##    country     continent  year lifeExp      pop gdpPercap
    ##    <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
    ##  1 Afghanistan Asia       1952    28.8  8425333      779.
    ##  2 Afghanistan Asia       1957    30.3  9240934      821.
    ##  3 Afghanistan Asia       1962    32.0 10267083      853.
    ##  4 Afghanistan Asia       1967    34.0 11537966      836.
    ##  5 Afghanistan Asia       1972    36.1 13079460      740.
    ##  6 Afghanistan Asia       1977    38.4 14880372      786.
    ##  7 Afghanistan Asia       1982    39.9 12881816      978.
    ##  8 Afghanistan Asia       1987    40.8 13867957      852.
    ##  9 Afghanistan Asia       1992    41.7 16317921      649.
    ## 10 Afghanistan Asia       1997    41.8 22227415      635.
    ## # ... with 1,670 more rows

To see whether can find Oceania:

``` r
semiJoin %>% 
  filter(continent == "Oceania")
```

    ## # A tibble: 0 x 6
    ## # ... with 6 variables: country <fct>, continent <fct>, year <int>,
    ## #   lifeExp <dbl>, pop <int>, gdpPercap <dbl>

We can see semi\_join listed all the rows in gapminder that have a match
in new\_data, that is all the rows except Oceania.

Let’s try a reverse
    order:

``` r
(semiJoin2 <- semi_join(new_data, gapminder, by = "continent")) 
```

    ## Warning: Column `continent` joining character vector and factor, coercing
    ## into character vector

    ## # A tibble: 4 x 5
    ##   continent largestCountry areaOfCountrySqkm capital timeZone
    ##   <chr>     <chr>          <chr>             <chr>   <chr>   
    ## 1 Africa    Algeria        2,381,741         Algiers GMT+1   
    ## 2 Americas  Canada         9,984,670         Ottawa  GMT-4   
    ## 3 Asia      China          9,596,961         Beijing GMT+8   
    ## 4 Europe    Russia         17,098,242        Moscow  GMT+3

We can see all the rows have matching rows in gapminder, so the number
of rows didn’t change.

Now try
    anti\_join:

``` r
(ant <- anti_join(gapminder, new_data, by = "continent")) 
```

    ## Warning: Column `continent` joining factor and character vector, coercing
    ## into character vector

    ## # A tibble: 24 x 6
    ##    country   continent  year lifeExp      pop gdpPercap
    ##    <fct>     <fct>     <int>   <dbl>    <int>     <dbl>
    ##  1 Australia Oceania    1952    69.1  8691212    10040.
    ##  2 Australia Oceania    1957    70.3  9712569    10950.
    ##  3 Australia Oceania    1962    70.9 10794968    12217.
    ##  4 Australia Oceania    1967    71.1 11872264    14526.
    ##  5 Australia Oceania    1972    71.9 13177000    16789.
    ##  6 Australia Oceania    1977    73.5 14074100    18334.
    ##  7 Australia Oceania    1982    74.7 15184200    19477.
    ##  8 Australia Oceania    1987    76.3 16257249    21889.
    ##  9 Australia Oceania    1992    77.6 17481977    23425.
    ## 10 Australia Oceania    1997    78.8 18565243    26998.
    ## # ... with 14 more rows

So we can see because Oceania rows in gapminder donn’t have matching
rows in new\_data, so anti\_join listed these rows.
