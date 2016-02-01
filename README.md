[![Build status](https://ci.appveyor.com/api/projects/status/xnbqfyt9oaicigl4?svg=true)](https://ci.appveyor.com/project/omarbenites/traittools)

[![Build Status](https://travis-ci.org/omarbenites/traittools.svg)](https://travis-ci.org/omarbenites/traittools)
# traittools
R-tools for trait manipulation in fieldbooks using Trait Dictionaries


### Installation

To install this package you need package devtools:
```{r eval=F}
install.packages("devtools")
```
Then type:
```{r eval=F}
devtools::install_github("omarbenites/openxlsx")
devtools::install_github("omarbenites/shinyFiles")
devtools::install_github("omarbenites/sbformula")
devtools::install_github("omarbenites/traittools")

```

### Usage
To load the package type in your computer:

```{r eval=F}
install.packages(traittools)
library(traittools)
```

### Complementary packages and tools

```{r eval=F}
install.packages("data.table")
install.packages("shinydashboard")
```

### Note: If you are a Windows user, please install Rtools

Run the trait app using traittoolsApp() or traittoolsApp2() on the console

####Currently, this module has 4 types of trials for potato and 1 for sweetpotato. The modules which are considered:


1. Yield
2. Late Blight
  * In Crop Management consider "dd/mm/yyyy" date format. Example 12/05/2015, 05/05/2015    
3. Drought Tolerance
4. Bulking-Maturity
