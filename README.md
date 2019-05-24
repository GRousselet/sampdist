Reproducibility package for the article:

**Using simulations to explore sampling distributions: an antidote to hasty and extravagant inferences**
Rousselet G.A.
*in preparation*

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
|`lognormal`|introduction to sampling distributions|Figure 1 (figure_lognormal.pdf)|
|`corr_sim`|correlation simulations|Figures 2 (figure_rand_corr.pdf), 3 (figure_samp_dist.pdf), 4A, 5 (figure_samp_dist_rho.pdf)|
|`corr_power`|estimation precision and power analyses|Figure 4B (figure_precision.pdf)|
|`pc`|percent correct data|Figure 6 (figure_pc.pdf)|

The extra notebook `corr_look_like` illustrates different data patterns matching the same correlation coefficient, and produces the extra figure `figure_scatterplots.pdf`.

# R packages needed
- `ggplot2`
- `tibble`
- `dplyr`
- `cowplot`
- `akerd` function from Rand Wilcox, implementing an adaptive kernel density estimator
- `Cairo` to export pdf versions of figures with math notations

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

# Extra resources
- [Abandoning standardised effect sizes and opening up other roads to power](http://janhove.github.io/design/2017/07/14/OtherRoadsToPower)

- [When correlations go bad](https://thepsychologist.bps.org.uk/volume-23/edition-2/methods-when-correlations-go-bad)

- [The nature of correlation perception in scatterplots](https://link.springer.com/article/10.3758/s13423-016-1174-7)

- Game: [guess the correlation](http://guessthecorrelation.com/)
