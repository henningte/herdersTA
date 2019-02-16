# herdersTA

`herdersTA` is an R package that provides functions in order to analyse trajectories data of individuals. It is specifically designed in order to analyse movements of nomadic households. Functions mainly focus on temporally adjusting tracks from different households, filling gaps, identifying locations and visits, classifying visits according to their durations (short-term visits and campsite visits) and computing several summary indicators for the movement.
Since the funcitons are developed for a specific data set with certain special features, not all functions may be generally applicable.

The package relies especially on the [trajectories](https://cran.r-project.org/web/packages/trajectories/index.html) package in order to organise and handle trajectoy data.

# Installation

`herdersTA` can be installed from within R by typing
```
# install.packages("devtools")
devtools::install_github("henningte/herdersTA")
```

and loaded by typing
```
library(herdersTA)
```

# Publications
