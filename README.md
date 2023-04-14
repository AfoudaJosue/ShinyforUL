
# ShinyforUL: R Shiny web app as a package for applying Unsupervised Learning methods.

<!-- badges: start -->
<!-- badges: end -->

ShinyforUL is an R package that provides a GUI (Graphical User Interface) to easily perform Data Mining using unsupervised learning techniques like Clustering with K-Means and dimensionnality reduction using Principal Component Analysis (PCA), and -Distributed Stochastic Neighbor Embedding (t-SNE).

## Installation

You can install the development version of ShinyforUL from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AfoudaJosue/ShinyforUL")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ShinyforUL)
## basic example code
shinyUL(sampleData)
```

*sampleData* is a iris dataframe without the *Species* column. To use the ***shinyUL()*** function, pre-process your data as you see fit. Note that the ***shinyUL()*** function automatically removes missing data from the dataframe and keeps only its numeric variables.
