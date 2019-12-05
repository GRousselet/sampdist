Reproducibility package for the article:

**Using simulations to explore sampling distributions: an antidote to hasty and extravagant inferences**
Rousselet G.A.

# Content

|folder|description|location|
|-----|-----|-----|
|`code`|R `.Rmd` files to run simulations and create figures|OSF + GitHub|
|`notebooks`|pdf versions of the code, with embedded figures|OSF|
|`data`|simulation results needed to run the code|OSF + GitHub|
|`figures`|all the figures used in the article, in pdf format|OSF|
|`functions`|extra R functions defined in text files|OSF + GitHub|

# Notebooks

The notebooks contain code to reproduce the figures and analyses presented in the article. They also contain extra resources, figures and analyses.

|Notebook|Description|Figures|
|-----|-----|-----|
|`lognormal`|introduction to sampling distributions|Figure 1 (figure_lognormal.pdf)|
|`corr_sim`|correlation simulations|Figures 2 (figure_rand_corr.pdf), 3 (figure_samp_dist.pdf), 4A, 5 (figure_samp_dist_rho.pdf)|
|`corr_power`|estimation precision and power analyses|Figure 4B (figure_precision.pdf)|
|`pc`|percent correct data|Figure 6 (figure_pc.pdf)|
|`onsets`|ERP onset data|Figure 7 (figure_onsets.pdf)|
|`flp`|Reaction time data|Figure 8 (figure_flp.pdf)|

# R packages needed
- `ggplot2`
- `tibble`
- `dplyr`
- `cowplot`
- `akerd` function from Rand Wilcox, implementing an adaptive kernel density estimator
- `Cairo` to export pdf versions of figures with math notations
- `rogme` to get 2 datasets
- `Rfast` 
- `beepr` for little auditory rewards
- `facetscales`

# Resources for power analyses
- [Quick-R: Power Analysis](https://www.statmethods.net/stats/power.html)
- [Getting started with the pwr package](https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html)
- [Sample Size Calculators](http://www.sample-size.net/correlation-sample-size/)
- [SIMR: an R package for power analysis of generalized linear mixed models by simulation](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12504)
- [A Practical Primer To Power Analysis for Simple Experimental Designs](https://www.rips-irsp.com/articles/10.5334/irsp.181/)

# Extra resources
- [Abandoning standardised effect sizes and opening up other roads to power](http://janhove.github.io/design/2017/07/14/OtherRoadsToPower)

- [When correlations go bad](https://thepsychologist.bps.org.uk/volume-23/edition-2/methods-when-correlations-go-bad)

- [The nature of correlation perception in scatterplots](https://link.springer.com/article/10.3758/s13423-016-1174-7)

- Game: [guess the correlation](http://guessthecorrelation.com/)
