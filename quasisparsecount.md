# Git pages for Datta & Dunson (2016)

The following pages host the R and Stan codes for the paper: 

[**Bayesian Inference on Quasi-sparse Count Data**, Datta, J and Dunson, D. (2016), *Biometrika (to appear)*.](http://arxiv.org/abs/1510.04320)

These pages describe sampling from the Gauss hypergeometric posterior distribution as well as the simulation experiments in the paper. 
The main pages are as follows:

1. [R codes for reproducing the simulation experiments using three different candidates](http://dattahub.github.io/GHsim)
   *  Gauss hypergeometric prior,
   *  Kiefer–Wolfowitz nonparametric maximum likelihood estimator and 
   *  Robbins’ naive estimator: 
2.  [Slice sampling strategy and corresponding R codes
       for sampling from the posterior distribution under the Gauss hypergeometric prior](http://dattahub.github.io/GHSlice)
3.  [Stan codes for easy implementation of our method](http://dattahub.github.io/GHstancodes)

### Abstract of the paper: 

There is a growing interest in analyzing high-dimensional count data, which often exhibit quasi-sparsity corresponding to an over-abundance of zeros and small non-zero counts. Existing
approaches for analyzing multivariate count data via Poisson or negative binomial log-linear hierarchical models with zero-inflation cannot flexibly adapt to quasi-sparse settings. We develop
a new class of continuous local-global shrinkage priors tailored for quasi-sparse counts. Theoretical propertzes are assessed, including flexible posterior concentration and stronger control on
false discoveries in multiple testing. Simulation studies illustrate excellent small-sample properties relative to competitors. We apply the method to detect rare mutational hotspots in exome
sequencing data and to identify US cities most impacted by terrorism.
