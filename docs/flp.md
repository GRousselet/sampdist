Sampling distributions for reaction time data
================
Guillaume A. Rousselet
2025-09-23

- [Dependencies](#dependencies)
- [Illustrate random sample of
  participants](#illustrate-random-sample-of-participants)
  - [Create data frame](#create-data-frame)
  - [Make figure](#make-figure)
- [Summary results for all
  participants](#summary-results-for-all-participants)
  - [Compute trimmed means](#compute-trimmed-means)
  - [Illustrate results](#illustrate-results)
- [Hierarchical sampling: effect](#hierarchical-sampling-effect)
  - [Illustrate results](#illustrate-results-1)
- [Hierarchical sampling: no effect](#hierarchical-sampling-no-effect)
  - [Illustrate results](#illustrate-results-2)
  - [Summary figure](#summary-figure)

# Dependencies

``` r
library(ggplot2)
source("./functions/theme_gar.txt") # define ggplot theme
source("./functions/trimpval.txt") # get one-sample t-test on trimmed means
source("./functions/functions.txt")
library(tibble)
library(cowplot)
library(rogme) # this package is not on CRAN, follow these steps to install it:
# install.packages("devtools")
# devtools::install_github("GRousselet/rogme")
library(beepr)
```

``` r
sessionInfo()
```

    ## R version 4.5.1 (2025-06-13)
    ## Platform: x86_64-apple-darwin20
    ## Running under: macOS Sequoia 15.6.1
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.5-x86_64/Resources/lib/libRblas.0.dylib 
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.5-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## time zone: Europe/London
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] beepr_2.0     rogme_0.2.1   cowplot_1.2.0 tibble_3.3.0  ggplot2_4.0.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] vctrs_0.6.5        cli_3.6.5          knitr_1.50         rlang_1.1.6       
    ##  [5] xfun_0.53          generics_0.1.4     S7_0.2.0           glue_1.8.0        
    ##  [9] htmltools_0.5.8.1  scales_1.4.0       rmarkdown_2.29     grid_4.5.1        
    ## [13] evaluate_1.0.5     fastmap_1.2.0      yaml_2.3.10        lifecycle_1.0.4   
    ## [17] compiler_4.5.1     dplyr_1.1.4        RColorBrewer_1.1-3 pkgconfig_2.0.3   
    ## [21] rstudioapi_0.17.1  farver_2.1.2       digest_0.6.37      R6_2.6.1          
    ## [25] tidyselect_1.2.1   pillar_1.11.1      magrittr_2.0.4     withr_3.0.2       
    ## [29] tools_4.5.1        gtable_0.3.6       audio_0.1-11

Before we conduct an experiment, we decide on a number of trials per
condition, and a number of participants, and then we hope that whatever
we measure comes close to the population values we want to estimate.
Using a large dataset, we can perform data-driven simulations to
illustrate how close we can get to the truth.

We use data from the [French Lexicon Project](https://osf.io/f8kc4/).
The `.RData` dataset was created by applying the script
`getflprtdata.Rmd` available on
[GitHub](https://github.com/GRousselet/rogme/tree/master/data-raw). For
convenience, the data frame containing the FLP reaction time data is
stored in the `rogme` R package, available on
[GitHub](https://github.com/GRousselet/rogme).

``` r
# get data - tibble = `flp`
flp <- rogme::flp
# columns =
#1 = participant
#2 = rt
#3 = acc = accuracy 0/1
#4 = condition = word/non-word

# Determine population trimmed mean of trimmed mean differences
tmp <- tapply(flp$rt, list(flp$participant, flp$condition), mean, trim = 0.2)
pop.tm.diff <- mean(tmp[,2]-tmp[,1], trim = 0.2)
pop.np <- length(unique(flp$participant))
```

N = 959 participants.

# Illustrate random sample of participants

## Create data frame

``` r
set.seed(3)
# participants' indices
p.list <- as.numeric(as.character(unique(flp$participant)))
np <- 20
rsamp <- sample(p.list, np, replace = FALSE)
df <- flp[flp$participant %in% rsamp, ]
df$participant <- as.character(df$participant)
df$participant <- factor(df$participant, levels=unique(df$participant))
#levels(df$condition)
levels(df$condition) <- c("Word", "Non-Word")
```

## Make figure

``` r
p <- ggplot(df, aes(x = rt)) + theme_gar +
  geom_line(stat = "density", aes(colour=participant), linewidth = 0.5) + 
  scale_color_viridis_d() + 
  scale_x_continuous(breaks=seq(0,2000,500), minor_breaks = seq(0, 2000, 250)) +
  coord_cartesian(xlim = c(0, 2000)) +
  theme(legend.position = "none") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(x = "Reaction times (ms)", y = "Density") +
  facet_grid(. ~ condition)
p
```

![](flp_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
p.20 <- p
```

The distributions are positively skewed, as expected for RT data, and
participants tend to be slower in the non-word condition compared to the
word condition. Usually, a single number is used to summarise each
individual RT distribution. From 1000 values to 1, that’s some serious
data compression! In psychology, the mean is often used, but here we use
the 20% trimmed mean, which gives a better indication of the location of
the typical observation and protects against the influence of outliers.

# Summary results for all participants

Here is the distribution across participants of trimmed mean RT for the
word and non-word conditions and their differences.

## Compute trimmed means

``` r
# get data: mean RT for every participant
tmres <- tapply(flp$rt, list(flp$participant, flp$condition), mean, trim = 0.2)
summary(tmres)
```

    ##       word           non-word     
    ##  Min.   : 455.1   Min.   : 497.9  
    ##  1st Qu.: 631.2   1st Qu.: 695.9  
    ##  Median : 695.4   Median : 777.1  
    ##  Mean   : 711.4   Mean   : 802.4  
    ##  3rd Qu.: 780.4   3rd Qu.: 887.7  
    ##  Max.   :1066.8   Max.   :1318.8

## Illustrate results

All distributions are skewed.

``` r
# create data frame
df <- tibble(x = c(as.vector(tmres),tmres[,2]-tmres[,1]),
             Condition = rep(c("Word", "Non-Word", "Difference"), each = pop.np)
)
df$Condition <- as.character(df$Condition)
df$Condition <- factor(df$Condition, levels=unique(df$Condition))

facet_bounds <- read.table(header=TRUE,
text=                           
"Condition xmin xmax breaks
Word 0 1600 250
Non-Word 0 1600 250
Difference -100 500 100",
stringsAsFactors=FALSE)

ff <- with(facet_bounds,
           data.frame(x=c(xmin,xmax),
                      Condition=c(Condition,Condition)))

ff$Condition <- keeporder(ff$Condition)

# make plot
p <- ggplot(df, aes(x = x)) + theme_gar + 
  geom_line(stat = "density", linewidth = 1.5) + 
  labs(x = "20% trimmed mean reaction times (ms)", y = "Density") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  facet_grid(. ~ Condition, scales = "free") + 
  geom_blank(data=ff)
p
```

![](flp_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
p.alltm <- p
```

96.2% of participants had a positive difference, meaning that they
tended to be faster in the Word than the Non-Word condition.

# Hierarchical sampling: effect

We sample participants and trials with replacement. For each condition,
participant and trial sample size, we compute the 20% trimmed mean.
Then, for each condition and participant sample size we compute the
group 20% trimmed mean and the *p* value based on a t-test for trimmed
means.

``` r
set.seed(22222)
p.list <- as.numeric(as.character(unique(flp$participant)))
nsim <- 5000
np.seq <- c(10, 20, 50) # number of participants
maxNP <- max(np.seq)
nt.seq <- seq(50, 250, 50) # number of trials
maxNT <- max(nt.seq)

# intermediate results with single trials
diff.all <- matrix(NA, nrow = maxNP, ncol = length(nt.seq))

# group results
sim.res <- array(NA, dim = c(nsim, length(np.seq), length(nt.seq))) 
sim.res.pval <- sim.res

for(iter in 1:nsim){
  if(iter %% 100 == 0){
    print(paste("Simulation ",iter," / ",nsim,"..."))
    beep(2)
  }
  # Sample participants with replacement
  bootid <- sample(p.list, maxNP, replace = TRUE)
  # Sample trials with replacement 
  for(P in 1:maxNP){ # for each bootstrap participant
    boot.w <- sample(flp$rt[flp$participant %in% bootid[P] & flp$condition == "word"],
                     maxNT, replace = TRUE)
    boot.nw <- sample(flp$rt[flp$participant %in% bootid[P] & flp$condition == "non-word"],
                      maxNT, replace = TRUE)
    
    # compute individual differences between trimmed means
    for(T in 1:length(nt.seq)){ # for each sample size
      # compute trimmed means
      diff.all[P,T] <- mean(boot.nw[1:nt.seq[T]], trim = 0.2) - mean(boot.w[1:nt.seq[T]], trim = 0.2)
    }
  }
  
  # compute group trimmed means and p values 
  for(NP in 1:length(np.seq)){
    for(NT in 1:length(nt.seq)){
      sim.res[iter, NP, NT] <- mean(diff.all[1:np.seq[NP],NT], trim = 0.2)
      sim.res.pval[iter, NP, NT] <- trimpval(diff.all[1:np.seq[NP],NT], tr = 0.2)
    }
  }
}

save(
  nsim,
  np.seq,
  nt.seq,
  sim.res,
  sim.res.pval,
  file = "./data/flp_sim_res.RData"  
)

beep(8)
```

## Illustrate results

### Panel 1

Original sampling distributions.

``` r
load(file = "./data/flp_sim_res.RData")

alpha <- 0.05 # arbitrary cut-off

v.sim.res <- as.vector(sim.res)

df <- tibble(x = v.sim.res,
             np = factor(rep(rep(np.seq, each = nsim), length(nt.seq))),
             nt = factor(rep(nt.seq, each = nsim * length(np.seq)))
)

levels(df$np) <- c("10 participants", "20 participants", "50 participants")

p <- ggplot(df, aes(x = x, colour = nt)) + theme_gar +
  geom_vline(xintercept = pop.tm.diff) +
  geom_line(stat = "density", linewidth = 0.75) +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(colour = guide_legend(override.aes = list(linewidth = 3),
                               title = "Number of trials")) +
  scale_colour_viridis_d(option = "plasma", 
                         begin = 0, end = 0.7) + 
  facet_grid(cols = vars(np), scales = "fixed") +
  labs(x = "Group 20% trimmed mean difference", y = "Density")
p.diff1 <- p
p
```

![](flp_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Panel 2

Plot sampling distributions conditional on statistical significance.

``` r
alpha <- 0.05 # arbitrary cut-off

v.sim.res <- as.vector(sim.res)
v.sim.res.pval <- as.vector(sim.res.pval)
v.sim.res[v.sim.res.pval>alpha] <- NA

df <- tibble(x = v.sim.res,
             np = factor(rep(rep(np.seq, each = nsim), length(nt.seq))),
             nt = factor(rep(nt.seq, each = nsim * length(np.seq)))
)

p <- ggplot(df, aes(x = x, colour = nt)) + theme_gar +
  geom_vline(xintercept = pop.tm.diff) +
  geom_line(stat = "density", linewidth = 0.75) +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(colour = guide_legend(override.aes = list(linewidth = 3),
                               title = "Number of trials")) +
  scale_colour_viridis_d(option = "plasma", 
                         begin = 0, end = 0.7) + 
  facet_grid(cols = vars(np), scales = "fixed") +
  labs(x = "Group 20% trimmed mean difference", y = "Density")
p
```

![](flp_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

The vertical line in each panel marks the population 20% trimmed mean.
Here, conditioning on $p\leq0.05$ has very little effect on the sampling
distributions because the population effect size is so large.

# Hierarchical sampling: no effect

In this situation, trials are sampled with replacement from the two
conditions merged together, so that on average, there is no effect.

``` r
set.seed(22222)
p.list <- as.numeric(as.character(unique(flp$participant)))
nsim <- 5000
np.seq <- c(10, 20, 50) # number of participants
maxNP <- max(np.seq)
nt.seq <- seq(50, 250, 50) # number of trials
maxNT <- max(nt.seq)

# intermediate results with single trials
diff.all <- matrix(NA, nrow = maxNP, ncol = length(nt.seq))

# group results
sim.res <- array(NA, dim = c(nsim, length(np.seq), length(nt.seq))) 
sim.res.pval <- sim.res

for(iter in 1:nsim){
  if(iter %% 100 == 0){
    print(paste("Simulation ",iter," / ",nsim,"..."))
    beep(2)
  }
  # Sample participants with replacement
  bootid <- sample(p.list, maxNP, replace = TRUE)
  # Sample trials with replacement 
  for(P in 1:maxNP){ # for each bootstrap participant
    all.trials <- flp$rt[flp$participant %in% bootid[P] & 
                           (flp$condition == "word" | flp$condition == "non-word")]
    boot.w <- sample(all.trials, maxNT, replace = TRUE)
    boot.nw <- sample(all.trials, maxNT, replace = TRUE)
    
    # compute individual differences between trimmed means
    for(T in 1:length(nt.seq)){ # for each sample size
      # compute trimmed means
      diff.all[P,T] <- mean(boot.nw[1:nt.seq[T]], trim = 0.2) - mean(boot.w[1:nt.seq[T]], trim = 0.2)
    }
  }
  
  # compute group trimmed means and p values 
  for(NP in 1:length(np.seq)){
    for(NT in 1:length(nt.seq)){
      sim.res[iter, NP, NT] <- mean(diff.all[1:np.seq[NP],NT], trim = 0.2)
      sim.res.pval[iter, NP, NT] <- trimpval(diff.all[1:np.seq[NP],NT], tr = 0.2)
    }
  }
}

save(
  nsim,
  np.seq,
  nt.seq,
  sim.res,
  sim.res.pval,
  file = "./data/flp_sim_res_noeffect.RData"  
)

beep(8)
```

## Illustrate results

### Panel 1

``` r
load(file = "./data/flp_sim_res_noeffect.RData")

alpha <- 0.05 # arbitrary cut-off

v.sim.res <- as.vector(sim.res)
v.sim.res.pval <- as.vector(sim.res.pval)

df <- tibble(x = v.sim.res,
             np = factor(rep(rep(np.seq, each = nsim), length(nt.seq))),
             nt = factor(rep(nt.seq, each = nsim * length(np.seq)))
)

p <- ggplot(df, aes(x = x, colour = nt)) + theme_gar +
  geom_vline(xintercept = 0) + # long-run effect size
  geom_line(stat = "density", linewidth = 0.75) +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(colour = guide_legend(override.aes = list(linewidth = 3),
                               title = "Number of trials")) +
  scale_colour_viridis_d(option = "plasma", 
                         begin = 0, end = 0.7) + 
  facet_grid(cols = vars(np), scales = "fixed") +
  labs(x = "Group 20% trimmed mean difference", y = "Density")
p
```

![](flp_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### Panel 2

Same as panel 1, now only plotting differences if test was significant.

``` r
alpha <- 0.05 # arbitrary cut-off

v.sim.res <- as.vector(sim.res)
v.sim.res.pval <- as.vector(sim.res.pval)
v.sim.res[v.sim.res.pval>alpha] <- NA

df <- tibble(x = v.sim.res,
             np = factor(rep(rep(np.seq, each = nsim), length(nt.seq))),
             nt = factor(rep(nt.seq, each = nsim * length(np.seq)))
)

p <- ggplot(df, aes(x = x, colour = nt)) + theme_gar +
  geom_vline(xintercept = 0) +
  geom_line(stat = "density", linewidth = 0.75) +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(colour = guide_legend(override.aes = list(linewidth = 3),
                               title = "Number of trials")) +
  scale_colour_viridis_d(option = "plasma", 
                         begin = 0, end = 0.7) + 
  facet_grid(cols = vars(np), scales = "fixed") +
  labs(x = "Group 20% trimmed mean difference", y = "Density")
p
```

![](flp_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Results conditioned on $p\leq0.05$ are symmetric around zero. With 10
participants and 50 trials per condition, the median of the absolute
group differences is 24.1 ms, and among the experiments reporting
significant results, 10% report differences at least as large as 36.7
ms. With 50 participants and 200 trials per condition, the median of the
absolute group differences is 6.8 ms, and among the experiments
reporting significant results, 10% report differences at least as large
as 8.9 ms.

### Merged panels using facetting

``` r
load(file = "./data/flp_sim_res_noeffect.RData")

alpha <- 0.05 # arbitrary cut-off

v.sim.res.raw <- as.vector(sim.res)
v.sim.res.cond <- as.vector(sim.res)
v.sim.res.pval <- as.vector(sim.res.pval)
v.sim.res.cond[v.sim.res.pval>alpha] <- NA

df <- tibble(x = c(v.sim.res.raw, v.sim.res.cond),
             np = factor(rep(rep(rep(np.seq, each = nsim), length(nt.seq)),2)),
             nt = factor(rep(rep(nt.seq, each = nsim * length(np.seq)),2)),
             cond = factor(rep(c("Raw data","Cond. data"), 
                               each = nsim * length(np.seq) * length(nt.seq)))
)

levels(df$np) <- c("10 participants", "20 participants", "50 participants")
df$cond <- keeporder(df$cond)

p <- ggplot(df, aes(x = x, colour = nt)) + theme_gar +
  geom_vline(xintercept = 0) + # long-run effect size
  geom_line(stat = "density", linewidth = 0.75) +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(colour = guide_legend(override.aes = list(linewidth = 3),
                               title = "Number of trials")) +
  scale_colour_viridis_d(option = "plasma", 
                         begin = 0, end = 0.7) + 
  facet_grid(rows = vars(cond), cols = vars(np), scales = "fixed") +
  labs(x = "Group 20% trimmed mean difference", y = "Density")
p.diff.noeffect <- p
p
```

## Summary figure

``` r
cowplot::plot_grid(p.20, 
                   p.alltm, 
                   p.diff1, 
                   p.diff.noeffect + theme(legend.position = "none"),
                   labels = c("A", "B", "C", "D"),
                   ncol = 1,
                   nrow = 4,
                   rel_heights = c(1, 0.75, 0.75, 1), 
                   label_size = 20,
                   align = 'v',
                   axis = 'l',
                   hjust = -0.5,
                   scale = 1)

# save figure
ggsave(filename=('./figures/fig_flp.tiff'),width=13,height=20,dpi=300)
```
