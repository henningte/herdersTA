
<!-- README.md is generated from README.Rmd. Please edit that file -->
herdersTA
=========

herdersTA provides functions in order to extract movement characteristics from high-temporal resolution GPS trajectories of nomadic households. Key functions are the extraction of visited locations using density-based clustering and flexible time thresholding, the identification of individual visits, gap filling, temporal aggregation and the extraction of raster and polygon data at locations. Moreover, functions to plot the time course of visited locations with additional information while ensuring anonymisation are provided.

herdersTA makes use of the [trajectories](https://github.com/edzer/trajectories) package for general GPS trajectory handling and the [dbscan](https://github.com/mhahsler/dbscan) package for density based clustering with which locations are identified.

### How to install

You can install herdersTA from GitHub using R via:

``` r
remotes::install_github(repo = "henningte/herdersTA")
```

### How to use

You can load herdersTA in R with:

``` r
library(herdersTA) 
```

### How to cite

Please cite this R package as:

> Henning Teickner and Christian Knoth (2020). *herdersTA: Extracting Movement Characteristics from GPS Trajectories of Nomadic Households'*. Accessed 18 Jan 2020. Online at <https://github.com/henningte/herdersTA>.

### Licenses

**Text and figures :** [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) attribution requested in reuse (Note that currently no sample data is provided).

### Contributions

We welcome contributions from everyone. Before you get started, please see our [contributor guidelines](CONTRIBUTING.md). Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

### Sources

This packages was developed in R (R version 3.5.3 (2019-03-11)) (R Core Team 2017) using functions from devtools (Wickham, Hester, and Chang 2019), usethis (Wickham and Bryan 2019), rrtools (Marwick 2019) and roxygen2 (Wickham et al. 2019).

### References

Marwick, Ben. 2019. “rrtools: Creates a Reproducible Research Compendium.” <https://github.com/benmarwick/rrtools>.

R Core Team. 2017. “R: A Language and Environment for Statistical Computing.” Vienna, Austria: R Foundation for Statistical Computing. <https://www.R-project.org/>.

Wickham, Hadley, and Jennifer Bryan. 2019. “usethis: Automate Package and Project Setup.” <https://CRAN.R-project.org/package=usethis>.

Wickham, Hadley, Peter Danenberg, Gábor Csárdi, and Manuel Eugster. 2019. “roxygen2: In-Line Documentation for R.” <https://CRAN.R-project.org/package=roxygen2>.

Wickham, Hadley, Jim Hester, and Winston Chang. 2019. “devtools: Tools to Make Developing R Packages Easier.” <https://CRAN.R-project.org/package=devtools>.
