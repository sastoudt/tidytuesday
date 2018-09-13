Tidy Tuesday Week 23
================
Sara Stoudt
9/12/2018

[Data](https://github.com/rfordatascience/tidytuesday/tree/master/data/2018-09-04)

[Article](https://fastfoodnutrition.org/)

[Data Source](https://www.franchiseopportunities.com/blog/general-franchise-information/fast-food-calorie-comparison-charts)

``` r
require(ggplot2)
require(dplyr)
require(gridExtra)
require(tidyr)

setwd("~/Desktop/tidytuesday/data/2018-09-04")

ff = read.csv("fastfood_calories.csv", stringsAsFactors = F)

ff = ff[,-c(1, ncol(ff))] ## remove salad (all the same) and X
```

I wanted a simple way to group items across restaurants, so I'm going to follow [this](https://stackoverflow.com/questions/21511801/text-clustering-with-levenshtein-distances) example and use hierarchical clustering via the [Levenshtein Distance](https://en.wikipedia.org/wiki/Levenshtein_distance) (typically used for string distances).

``` r
d  <- adist(ff$item) 
rownames(d) <- ff$Item
hc <- hclust(as.dist(d))
df <- data.frame(ff,cut = cutree(hc, k = 10)) ## 10 is a totally arbitrary choice
```

Let's see what groups we end up with.

``` r
unlist(lapply(split(df,df$cut), nrow))
```

    ##   1   2   3   4   5   6   7   8   9  10 
    ##  57 368  39   5  15  12   4   4   2   9

``` r
lapply(split(df[,c("cut","item","restaurant")], df$cut), head, 15)
```

    ## $`1`
    ##     cut                                 item  restaurant
    ## 1     1     Artisan Grilled Chicken Sandwich   Mcdonalds
    ## 58    1    Chargrilled Chicken Club Sandwich Chick Fil-A
    ## 59    1         Chargrilled Chicken Sandwich Chick Fil-A
    ## 73    1      4 Piece Grilled Chicken Nuggets Chick Fil-A
    ## 74    1      6 Piece Grilled Chicken Nuggets Chick Fil-A
    ## 75    1      8 piece Grilled Chicken Nuggets Chick Fil-A
    ## 76    1     12 Piece Grilled Chicken Nuggets Chick Fil-A
    ## 77    1   Spicy Grilled Chicken Sub Sandwich Chick Fil-A
    ## 78    1 Regular Grilled Chicken Sub Sandwich Chick Fil-A
    ## 79    1        Smokehouse BBQ Bacon Sandwich Chick Fil-A
    ## 82    1        Chargrilled Chicken Cool Wrap Chick Fil-A
    ## 112   1 3 Piece Crispy Chicken Tender Dinner       Sonic
    ## 113   1 5 Piece Crispy Chicken Tender Dinner       Sonic
    ## 117   1          Small Jumbo Popcorn Chicken       Sonic
    ## 118   1          Large Jumbo Popcorn Chicken       Sonic
    ## 
    ## $`2`
    ##    cut                                item restaurant
    ## 2    2      Single Bacon Smokehouse Burger  Mcdonalds
    ## 3    2      Double Bacon Smokehouse Burger  Mcdonalds
    ## 6    2                             Big Mac  Mcdonalds
    ## 7    2                        Cheeseburger  Mcdonalds
    ## 8    2            Classic Chicken Sandwich  Mcdonalds
    ## 9    2                 Double Cheeseburger  Mcdonalds
    ## 10   2 Double Quarter Pounder® with Cheese  Mcdonalds
    ## 11   2                       Filet-O-Fish®  Mcdonalds
    ## 12   2         Garlic White Cheddar Burger  Mcdonalds
    ## 15   2                           Hamburger  Mcdonalds
    ## 16   2                        Lobster Roll  Mcdonalds
    ## 17   2     Maple Bacon Dijon 1/4 lb Burger  Mcdonalds
    ## 20   2                           McChicken  Mcdonalds
    ## 21   2                            McDouble  Mcdonalds
    ## 22   2                               McRib  Mcdonalds
    ## 
    ## $`3`
    ##    cut                                              item restaurant
    ## 4    3         Grilled Bacon Smokehouse Chicken Sandwich  Mcdonalds
    ## 5    3          Crispy Bacon Smokehouse Chicken Sandwich  Mcdonalds
    ## 13   3     Grilled Garlic White Cheddar Chicken Sandwich  Mcdonalds
    ## 14   3      Crispy Garlic White Cheddar Chicken Sandwich  Mcdonalds
    ## 18   3        Grilled Maple Bacon Dijon Chicken Sandwich  Mcdonalds
    ## 19   3         Crispy Maple Bacon Dijon Chicken Sandwich  Mcdonalds
    ## 24   3           Grilled Pico Guacamole Chicken Sandwich  Mcdonalds
    ## 25   3            Crispy Pico Guacamole Chicken Sandwich  Mcdonalds
    ## 26   3 Premium Buttermilk Crispy Chicken Deluxe Sandwich  Mcdonalds
    ## 27   3            Premium Crispy Chicken Deluxe Sandwich  Mcdonalds
    ## 30   3       Grilled Signature Sriracha Chicken Sandwich  Mcdonalds
    ## 31   3        Crispy Signature Sriracha Chicken Sandwich  Mcdonalds
    ## 33   3          Grilled Sweet BBQ Bacon Chicken Sandwich  Mcdonalds
    ## 34   3           Crispy Sweet BBQ Bacon Chicken Sandwich  Mcdonalds
    ## 35   3         3 piece Buttermilk Crispy Chicken Tenders  Mcdonalds
    ## 
    ## $`4`
    ##     cut                                             item  restaurant
    ## 46    4  4 piece Sweet N' Spicy Honey BBQ Glazed Tenders   Mcdonalds
    ## 47    4  6 piece Sweet N' Spicy Honey BBQ Glazed Tenders   Mcdonalds
    ## 48    4 10 piece Sweet N' Spicy Honey BBQ Glazed Tenders   Mcdonalds
    ## 270   4    4 Piece Chicken Strip Basket w/ Country Gravy Dairy Queen
    ## 271   4    6 Piece Chicken Strip Basket w/ Country Gravy Dairy Queen
    ## 
    ## $`5`
    ##     cut                                                item  restaurant
    ## 49    5                     Premium Asian Salad w/o Chicken   Mcdonalds
    ## 50    5              Premium Asian Salad w/ Grilled Chicken   Mcdonalds
    ## 51    5               Premium Asian Salad w/ Crispy Chicken   Mcdonalds
    ## 52    5               Premium Bacon Ranch Salad w/o Chicken   Mcdonalds
    ## 53    5        Premium Bacon Ranch Salad w/ Grilled Chicken   Mcdonalds
    ## 54    5         Premium Bacon Ranch Salad w/ Crispy Chicken   Mcdonalds
    ## 55    5                 Premium Southwest Salad w/o Chicken   Mcdonalds
    ## 56    5          Premium Southwest Salad w/ Grilled Chicken   Mcdonalds
    ## 57    5           Premium Southwest Salad w/ Crispy Chicken   Mcdonalds
    ## 225   5                Chicken BLT Salad w/ Grilled Chicken Burger King
    ## 226   5                 Chicken BLT Salad w/ Crispy Chicken Burger King
    ## 227   5             Chicken Caesar Salad w/ Grilled Chicken Burger King
    ## 228   5              Chicken Caesar Salad w/ Crispy Chicken Burger King
    ## 229   5 Chicken, Apple & Cranberry Salad w/ Grilled Chicken Burger King
    ## 230   5  Chicken, Apple & Cranberry Salad w/ Crispy Chicken Burger King
    ## 
    ## $`6`
    ##     cut                                           item restaurant
    ## 92    6              Sonic Bacon Cheeseburger (w/mayo)      Sonic
    ## 99    6 Super Sonic Bacon Double Cheeseburger (w/mayo)      Sonic
    ## 100   6     Super Sonic Double Cheeseburger W/ Mustard      Sonic
    ## 101   6     Super Sonic Double Cheeseburger W/ Ketchup      Sonic
    ## 102   6        Super Sonic Double Cheeseburger W/ Mayo      Sonic
    ## 103   6       Super Sonic Jalapeno Double Cheeseburger      Sonic
    ## 435   6       Cool Ranch® Doritos® Double Decker® Taco  Taco Bell
    ## 442   6                Spicy Sweet Double Stacked Taco  Taco Bell
    ## 443   6        Cool Ranch Habanero Double Stacked Taco  Taco Bell
    ## 444   6               Nacho Crunch Double Stacked Taco  Taco Bell
    ## 445   6             Fiery Doritos® Double Decker® Taco  Taco Bell
    ## 449   6      Nacho Cheese Doritos® Double Decker® Taco  Taco Bell
    ## 
    ## $`7`
    ##     cut                                                            item
    ## 223   7 Bacon Cheddar Ranch Chicken Salad w/ grilled Chicken & Dressing
    ## 224   7  Bacon Cheddar Ranch Chicken Salad w/ crispy Chicken & Dressing
    ## 231   7    Garden Grilled Chicken Salad w/ Grilled Chicken, no dressing
    ## 232   7     Garden Grilled Chicken Salad w/ Crispy Chicken, no dressing
    ##      restaurant
    ## 223 Burger King
    ## 224 Burger King
    ## 231 Burger King
    ## 232 Burger King
    ## 
    ## $`8`
    ##     cut                                                       item
    ## 233   8                            Side Caesar Salad with dressing
    ## 234   8               Side Garden Salad and Avocado Ranch Dressing
    ## 374   8                Buffalo Chicken Salad (with Ranch dressing)
    ## 377   8 Chicken & Bacon Ranch Melt Salad (includes Ranch dressing)
    ##      restaurant
    ## 233 Burger King
    ## 234 Burger King
    ## 374      Subway
    ## 377      Subway
    ## 
    ## $`9`
    ##     cut                                           item restaurant
    ## 362   9       6" Turkey Italiano Melt (with Provolone)     Subway
    ## 363   9 Footlong Turkey Italiano Melt (with Provolone)     Subway
    ## 
    ## $`10`
    ##     cut                                          item restaurant
    ## 436  10               Cool Ranch® Doritos® Locos Taco  Taco Bell
    ## 437  10       Cool Ranch® Doritos® Locos Taco Supreme  Taco Bell
    ## 446  10                     Fiery Doritos® Locos Taco  Taco Bell
    ## 447  10             Fiery Doritos® Locos Taco Supreme  Taco Bell
    ## 450  10             Nacho Cheese Doritos® Locos Tacos  Taco Bell
    ## 451  10     Nacho Cheese Doritos® Locos Tacos Supreme  Taco Bell
    ## 470  10   Doritos® Cheesy Gordita Crunch - Cool Ranch  Taco Bell
    ## 471  10        Doritos® Cheesy Gordita Crunch - Fiery  Taco Bell
    ## 472  10 Doritos® Cheesy Gordita Crunch - Nacho Cheese  Taco Bell

``` r
## should be purrr-ing
```

First things first, the Doritos + Tacobell pairing is in a league of its own.

![](https://media.giphy.com/media/xGR4QD7ncz9QY/giphy.gif)

Just from looking at the top 15:

Group 1: chicken

Group 2: sandwiches (no chicken) \[but chicken items are in here, just not in head\]

Group 3: chicken sandwiches

Group 4: chicken

Group 5: chicken salads

Group 6: doubles

Group 7: chicken salads with dressing

Group 8: salad with dressing

Group 9: Subway

Group 10: Doritos + Tacobell

Where are the other Subway items? Where are the Arby's items hiding?

``` r
lapply(split(df, df$restaurant), 
       function(x){x %>% group_by(cut) %>% summarise(count=n()) %>% mutate(prop = count/sum(count))})
```

    ## $Arbys
    ## # A tibble: 3 x 3
    ##     cut count   prop
    ##   <int> <int>  <dbl>
    ## 1     1    16 0.291 
    ## 2     2    37 0.673 
    ## 3     3     2 0.0364
    ## 
    ## $`Burger King`
    ## # A tibble: 6 x 3
    ##     cut count   prop
    ##   <int> <int>  <dbl>
    ## 1     1     2 0.0286
    ## 2     2    51 0.729 
    ## 3     3     5 0.0714
    ## 4     5     6 0.0857
    ## 5     7     4 0.0571
    ## 6     8     2 0.0286
    ## 
    ## $`Chick Fil-A`
    ## # A tibble: 2 x 3
    ##     cut count  prop
    ##   <int> <int> <dbl>
    ## 1     1    10 0.370
    ## 2     2    17 0.630
    ## 
    ## $`Dairy Queen`
    ## # A tibble: 3 x 3
    ##     cut count   prop
    ##   <int> <int>  <dbl>
    ## 1     1     1 0.0238
    ## 2     2    39 0.929 
    ## 3     4     2 0.0476
    ## 
    ## $Mcdonalds
    ## # A tibble: 5 x 3
    ##     cut count   prop
    ##   <int> <int>  <dbl>
    ## 1     1     1 0.0175
    ## 2     2    24 0.421 
    ## 3     3    20 0.351 
    ## 4     4     3 0.0526
    ## 5     5     9 0.158 
    ## 
    ## $Sonic
    ## # A tibble: 4 x 3
    ##     cut count  prop
    ##   <int> <int> <dbl>
    ## 1     1     6 0.113
    ## 2     2    29 0.547
    ## 3     3    12 0.226
    ## 4     6     6 0.113
    ## 
    ## $Subway
    ## # A tibble: 4 x 3
    ##     cut count   prop
    ##   <int> <int>  <dbl>
    ## 1     1    14 0.146 
    ## 2     2    78 0.812 
    ## 3     8     2 0.0208
    ## 4     9     2 0.0208
    ## 
    ## $`Taco Bell`
    ## # A tibble: 4 x 3
    ##     cut count   prop
    ##   <int> <int>  <dbl>
    ## 1     1     7 0.0609
    ## 2     2    93 0.809 
    ## 3     6     6 0.0522
    ## 4    10     9 0.0783

Most of the Subway and Arby's items are in the biggest group. Increasing the number of clusters would eventually break this group up further.

What makes some restaraunts more spread out across groups than others? Since the distance metric counts additions, deletions, and substitutions, there is some dependence on the length of the item name.

``` r
ff$len = nchar(ff$item)
ggplot(ff,aes(len, col = restaurant, group = restaurant)) + geom_density()
```

![](fastfoodClustering_files/figure-markdown_github/unnamed-chunk-5-1.png)

Hm... nothing is jumping out at me here.

Are there differences in nutrient levels across clusters? I'm going to color the clusters that I preliminarily marked as "salad" groups to see if they are healthier. Remember, that these groups have all salads, but there are salads in other groups.

``` r
toP = df %>% group_by(cut) %>% summarise_if(is.numeric,mean,na.rm=T) %>% gather(nutrient,value,-cut)

toP$isSalad = as.factor(ifelse(toP$cut %in% c(5,7,8), 1, 0))

ggplot(toP, aes(as.factor(cut), value, col = isSalad)) + geom_col() + facet_wrap(~nutrient,scales="free_y") + xlab("cluster") + ylab("mean value")
```

![](fastfoodClustering_files/figure-markdown_github/unnamed-chunk-6-1.png)

The purely salad groups still have fairly high cholesterol and saturated fat levels. They also have high values of vitamin A and C though, so it's not all bad.

Why are chicken items so spread out among groups? Again, is this due to a difference in the length of item description? We can also see the distribution of distances between an item and all other items by whether or not the item has "chicken" in it to see if that gives us any clues.

``` r
dim(d) ## distance between each item and every other item
```

    ## [1] 515 515

``` r
dim(ff)
```

    ## [1] 515  17

``` r
ff$med = apply(d,1,median)
ff$max = apply(d,1,max)
ff$sd = apply(d,1,sd)

ff$isChicken = as.factor(ifelse(grepl("Chicken", ff$item), 1 , 0))
ff$len = nchar(ff$item)

g1 = ggplot(ff, aes(med, col = isChicken, group = isChicken)) + geom_density() + xlab("median string distance between other items")
g2 = ggplot(ff, aes(max, col = isChicken, group = isChicken)) + geom_density() + xlab("max string distance between other items")
g3 = ggplot(ff, aes(sd, col = isChicken, group = isChicken)) + geom_density() + xlab("sd string distance between other items")
g4 = ggplot(ff, aes(len, col = isChicken, group = isChicken)) + geom_density() + xlab("length of item name")

grid.arrange(g1, g2, g3, g4, ncol = 2)
```

![](fastfoodClustering_files/figure-markdown_github/unnamed-chunk-7-1.png)

Chicken items do seem to be further in string distance from other items than non-chicken items. They also have slightly longer item name lengths. However, non-chicken items actually have higher maximum distances from other items and a wider range of variability in distances.

**Disclaimer:** This is not real text analysis; I'm just using some fancy-ish tools to explore. If anyone has ideas about how to make this more rigorous, I'd be curious to learn more.
