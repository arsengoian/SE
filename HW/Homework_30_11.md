R Notebook
================

We’ll need some libraries…

``` r
pacman::p_load(haven, purrr, knitr)

# I gave up, let's just use tidyverse for everything else
library(tidyverse)
```

### Question 1

#### ia

Let us import the dataset and discover the necessary columns

``` r
DISCRIM <- read_stata("http://fmwww.bc.edu/ec-p/data/wooldridge/discrim.dta")

Variables <- c(
  'pentree',
  'prpblck',
  'income',
  'lincome',
  'prppov',
  'lhseval'
)

# Text descriptions
Description <- unlist(Variables %>% map(function(row) attributes(DISCRIM[[row]])$label))

# Descriptive stats
fMapData <- function(F) {
  return (
        Variables
          %>% map(function(row) F(Filter(function(x) !is.na(x), DISCRIM[[row]])))
          %>% unlist
          %>% format(scientific = FALSE)
  )
}

Mean <- fMapData(mean)
StandardDeviation <- fMapData(sd)

kable(data.frame(
  Description, Variables, Mean, StandardDeviation
))
```

| Description                                | Variables | Mean           | StandardDeviation |
|:-------------------------------------------|:----------|:---------------|:------------------|
| price entree (burger or chicken), 1st wave | pentree   | 1.32218591     | 0.64308450        |
| proportion black, zipcode                  | prpblck   | 0.11348640     | 0.18241647        |
| median family income, zipcode              | income    | 47053.78484108 | 13179.28606894    |
| log(income)                                | lincome   | 10.71993718    | 0.28447937        |
| proportion in poverty, zipcode             | prppov    | 0.07129732     | 0.06743866        |
| log(hseval)                                | lhseval   | 11.82853320    | 0.38934922        |

Thus, we can conclude that these variables have quite large deviations

#### ib

``` r
my_data <- as_tibble(DISCRIM)
variables <- (my_data
  %>% select(pentree, prpblck, income, lincome, prppov, lhseval)
  %>% filter(!is.na(pentree) & !is.na(prpblck))) # Apparently two of these is enough

  cor(variables)
```

    ##            pentree    prpblck     income    lincome     prppov    lhseval
    ## pentree  1.0000000  0.1618867 -0.1141603 -0.1104026  0.1110576 -0.1133929
    ## prpblck  0.1618867  1.0000000 -0.4376962 -0.5027743  0.6831473 -0.3513266
    ## income  -0.1141603 -0.4376962  1.0000000  0.9707630 -0.7237746  0.7980433
    ## lincome -0.1104026 -0.5027743  0.9707630  1.0000000 -0.8392938  0.7981214
    ## prppov   0.1110576  0.6831473 -0.7237746 -0.8392938  1.0000000 -0.5569763
    ## lhseval -0.1133929 -0.3513266  0.7980433  0.7981214 -0.5569763  1.0000000

#### ii

(…is being updated)
