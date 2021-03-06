---
title: "3 Seas 37 - Fishes"
author: "Kosmas Hench"
date: "2019-02-13"
documentclass: book
bibliography: [bibliography.bib]
biblio-style: apalike
link-citations: yes
---

# Intro

Here are some scripts to help with the data analysis:

- how to create a [map including piecharts](pie-map-template.html)
- how to do an [NMDS plot and run a PERMANOVA](nmdspermanova-template.html)

<center>![](intro.png)</center>

To run these, please make sure you have the following **R** packages installed:


```r
# Fundamental packages
install.packages("tidyverse")
install.packages("vegan")
# ggplot extensions
install.packages("ggalt")
install.packages("scatterpie")
install.packages("cowplot")
# Mapping packages
install.packages("ggmap")
install.packages("sf")
install.packages("maptools")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
```
------

Following the conventions of the [*tidyverse*](https://www.tidyverse.org/), some aspects that you might be used from base **R** might look a little different:

First of all, the *tidyverse* comes with a few functions that replace common standard functions to achieve a more consistent behavior than base **R**. This means that here we will use for example `read_delim()` (*tidyverse*) instead of `read.table()` (base **R**).

The second thing that migh need some getting used to is the use of *the pipe* (`%>%`):

The pipe is an elegant way of connecting severeal functions that are executed one after the other. If we for example have a silly function that simply addes 1 to an input value, there are several ways to get from 1 to 4:



```r
add_1 <- function(x){x + 1}

a <- 1
b <- add_1(a)
c <- add_1(b)
d <- add_1(c)
d
```

```
## [1] 4
```


```r
add_1(add_1(add_1(1)))
```

```
## [1] 4
```


```r
1 %>%
  add_1() %>%
  add_1() %>%
  add_1()
```

```
## [1] 4
```

Of these three versions I usually find the pipe to be most clear and easily understandable way (especially if you try to make sense of your code when you come back a little while later).

Therefore you will see many pipes thoughout the scripts.

------

This function might help you to turn missing values into zeros:


```r
na_to_0 <- function(df){
  df[is.na(df)] <- 0
  df
}
```

------

Further **really** useful help on **R** is given by Hadley Wickham in his online book ["R for Data Science"](https://r4ds.had.co.nz/introduction.html).

------

Further helpful information on **ggplot2** can be found at:

- the [R Graphics cook book](http://www.cookbook-r.com/Graphs/)
- a [tutorial by the Harvad University](http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html#introduction)
- this [page by Zev Ross](http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/)

Also, this platform of [**ggplot** extensions](http://www.ggplot2-exts.org/) is definitively worth a look.


