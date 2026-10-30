# Tools for data.table objects in the REMIND context

R package **rmndt**, version **0.8.0**

   [![R build status](https://github.com/ahagen-pik/rmndt/workflows/check/badge.svg)](https://github.com/ahagen-pik/rmndt/actions) [![codecov](https://codecov.io/gh/ahagen-pik/rmndt/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ahagen-pik/rmndt) [![r-universe](https://pik-piam.r-universe.dev/badges/rmndt)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

Helper functions for REMIND-related tasks with data.table objects, e.g., interpolation and (dis-)aggregation.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r
install.packages("rmndt")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Alois Dirnaichner <dirnaichner@pik-potsdam.de>.

## Citation

To cite package **rmndt** in publications use:

Dirnaichner A, Hagen A (2026). "rmndt: Tools for data.table objects in the REMIND context." Version: 0.8.0.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {rmndt: Tools for data.table objects in the REMIND context},
  author = {Alois Dirnaichner and Alex K. Hagen},
  date = {2026-10-30},
  year = {2026},
  note = {Version: 0.8.0},
}
```
