Reproducibility package for the article:

**Using simulations to explore sampling distributions: an antidote to hasty and extravagant inferences**
Rousselet G.A.
*in preparation*

[[OSF repository]()] [[GitHub repository](https://github.com/GRousselet/sampdist)] [[PsyArXiv Preprint]()]

The repository contains all of the [R](https://www.r-project.org/) code  used in the article. The code is best seen by running the RMarkdown notebooks, within [RStudio](https://www.rstudio.com/). 

The code is released under the [MIT license](https://opensource.org/licenses/MIT). Copyright 2019, Guillaume A. Rousselet.

The figures are released under the [CC-BY 4.0 license](https://creativecommons.org/licenses/by/4.0/legalcode). Copyright 2019, Guillaume A. Rousselet.

# Content

|folder|description|
|-----|-----|
|`code`|R `.Rmd` files to run simulations and create figures|
|`notebooks`|pdf versions of the code, with embedded figures|
|`data`|simulation results needed to run the code|
|`figures`|all the figures used in the article, in pdf format|
|`functions`|extra R functions defined in text files|

# Notebooks

The notebooks contains code to reproduce the figures and analyses presented in the article. They also contain extra resources, figures and analyses.

|Notebook|Description|Figures|
|-----|-----|-----|
|[lognormal](/docs/lognormal.md)|introduction to sampling distributions|Figure 1 (figure_lognormal.pdf)|
|`corr_sim`|correlation simulations|Figures 2 (figure_rand_corr.pdf), 3 (figure_samp_dist.pdf), 4A, 5 (figure_samp_dist_rho.pdf)|
|`corr_power`|estimation precision and power analyses|Figure 4B (figure_precision.pdf)|
|`pc`|percent correct data|Figure 6 (figure_pc.pdf)|

# R packages needed
If you want to run the code in RStudio, you will need to install a few packages. 

To reproduce the figures only, you can install the required packages by typing this in the console:

`install.packages(c("ggplot2", "tibble", "dplyr", "Rfast"))` 

Or you can navigate in the GUI to Tools > Install Packages...

To install `rogme` and `facetscales`, first you might need to install `devtools`:

`install.packages("devtools")`

then:

`devtools::install_github("GRousselet/rogme")`

`devtools::install_github("zeehio/facetscales")`

To reproduce the summary figures, you also need `cowplot` to combine panels and `Cairo` to export pdf versions of figures with math notations:

`install.packages("cowplot", "Cairo")` 

Finally, if you decide to run the simulations, you will need `beepr` to get a little auditory reward:

`install.packages("beepr")` 

# Additional R functions
A few extra R functions are defined in `.txt` files, which are listed in the **Dependencies** section of the RMarkdown notebooks. Each notebook will install the appropriate functions for you; otherwise, in the console you can type `source(file.choose())` and select the relevant `.txt` file. In particular, the `akerd` file contains a function from Rand Wilcox, implementing an adaptive kernel density estimator. [functions.txt](https://github.com/GRousselet/articles/blob/master/bootstrap/functions/functions.txt) and [theme_gar.txt](https://github.com/GRousselet/articles/blob/master/bootstrap/functions/theme_gar.txt) contain custom code to set some ggplot2 parameters and to compute a few things. Other custom functions are defined in the notebooks.

# Resources for power analyses
- [Quick-R: Power Analysis](https://www.statmethods.net/stats/power.html)
- [Getting started with the pwr package](https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html)
- [Sample Size Calculators](http://www.sample-size.net/correlation-sample-size/)
- [SIMR: an R package for power analysis of generalized linear mixed models by simulation](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12504)
- [A Practical Primer To Power Analysis for Simple Experimental Designs](https://www.rips-irsp.com/articles/10.5334/irsp.181/)

# Resources to compare correlation coefficients
- [cocor comparing correlations](http://comparingcorrelations.org)
- [How to compare dependent correlations](https://garstats.wordpress.com/2017/03/01/comp2dcorr/)
- [Comparing correlations: independent and dependent (overlapping or non-overlapping)](https://seriousstats.wordpress.com/2012/02/05/comparing-correlations/)

# Video explaining SEM
[Standard error of the mean](https://www.youtube.com/watch?v=J1twbrHel3o)

# Extra resources
- [Abandoning standardised effect sizes and opening up other roads to power](http://janhove.github.io/design/2017/07/14/OtherRoadsToPower)

- [When correlations go bad](https://thepsychologist.bps.org.uk/volume-23/edition-2/methods-when-correlations-go-bad)

- [The nature of correlation perception in scatterplots](https://link.springer.com/article/10.3758/s13423-016-1174-7)

- Game: [guess the correlation](http://guessthecorrelation.com/)
