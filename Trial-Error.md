Trial & Error
================
columbaspexit
4/11/2020

## Two things going on here.

1.  (trying to) Learn R Markdown
2.  Document attempts (failed or otherwise) at making TotePacking more
    user-friendly.

## How does TotePacking work now?

It’s a recursive function that takes in two vectors, item & tote
dimensions, and optionally: the current count of items that fit and
which iteration the function is on (not actually needed, it’s a holdover
from writing the function and just kind of neat to know). It returns the
number of copies that fit and the count of iterations called so far.

Here’s a sample of data that works with TotePacking:

``` r
items <- read.csv("data/sample_items.csv")
head(items)
```

    ##   item_id item_length item_width item_height tote_length tote_width tote_height
    ## 1 9320337       7.394      5.303       0.791          24         12          10
    ## 2 2272980       6.263      4.459       0.805          24         12          10
    ## 3 3505830      10.904      8.469       0.651          24         12          10
    ## 4 7729016       9.958      7.148       1.242          24         12          10
    ## 5 4442272       8.962      5.900       2.120          24         12          10
    ## 6 9797646       7.692      4.929       2.091          24         12          10

The main appeal of a function is being able to calculate how many copies
fit, not for just one item but for MANY items. But running the function
for a lot of items is a little ridiculous right now…

``` r
for (i in 1:nrow(items)){
  tempy <- TotePacking(as.vector(c(items$item_length[i], 
                                   items$item_width[i], 
                                   items$item_height[i])), 
                       as.vector(c(items$tote_length[i], 
                                   items$tote_width[i], 
                                   items$tote_height[i])))
  items$CvnQty[i] <- tempy[1]
  items$Itrs[i] <- tempy[2]
}
```

## How could TotePacking work better?

The current goal to is get to something like this…

``` r
items_fits <- TotePacking(items$item_dims,items$tote_dims)
```

Or maybe even something like this, where items is a custom data type…

``` r
items_fit <- TotePacking(items)
```

TotePacking should also be a proper *package* and not just an RStudio
project. \>.\>

TBC.\!
