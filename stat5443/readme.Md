---
title: "Stat 5443: Computational Statistics"
subtitle: "Syllabus and R demos"
author: "Jyotishka Datta"
date: "Fall Semester, 2017-18"
output: html_document
---
# Syllabus 

### Foundations: 
1.  Computational Complexity 
2.  Introduction to R / basic programming. 
3.  Introduction to Algorithms and basic concepts (e.g. sorting)
4.  Divide-and-Conquer Strategy. 
5.  Linear Systems (Gaussian Elimination, Cholesky Decomposition etc.)

### Sampling:
1.  Accept-Reject Scheme 
2.  Rejection sampling and Envelope.
3.  Advanced Sampling Techniques: 
    *  Monte Carlo Integration
    *  Importance Sampling, SIR, Adaptive Rejection Sampling. 
4. Markov Chain Monte Carlo. 
    *  Basic Theory. 
    *  Metropolis & Metropolis-Hastings. 
    *  Gibbs sampler. 
5.  Sampling strategies for Bayesian Inference. 

### Statistical Learning: 
1.  Unsupervised and Supervised Learning. 
2.  Examples. 
3.  Bias-Variance Decomposition. 
4.  Accuracy vs. Model Interpretability. 

### Unsupervised 
1.  K-nearest neighbour 
2.  PCA. 

### Supervised 
1. Modern Regression: (large $p$, small $n$: **wide** data)
   *  Basics - Geometry of Regression. 
   *  Subset selection: forward selection/backward selection/Best subset - AIC. 
   *  Shrinkage and Selection: Ridge, LASSO, Elastic Net. 
   *  __Cross-validation__ (general)
   *  Bayesian Lasso and Horseshoe. 
   *  Principal Components Regression. 

2. Classification: 
   *   Logistic Regression (with $\ell_1$ penalty). 
   *   LDA and QDA. 
   *   Comparison. 
3. Decision Tree 
4. Bagging, Boosting. 
5. Bootstrap (general)

### Random Forest. 
### Support Vector Machines. 
### Multidimensional Scaling. 


# In-class examples

All the R examples shown in class for Stat 5443 (Computational Statistics) are given below as a list. 
Some of the codes are my own and the rest are either derived or taken from the R codes are taken from various resources such as matrix examples in R tutorial by Prof. Giovanni Petris, MCMC examples by Prof. David Draper and the [R codes](http://www-bcf.usc.edu/~gareth/ISL/code.html) accompanying the [ISLR book](http://www-bcf.usc.edu/~gareth/ISL/index.html). 



The book website also has a link to the MOOC for Statistical Learning by Prof.s Hastie and Tibshriani which has a large interesection with the second half of the class : starting from modern regression to random forest. It's a great lecture series. 

1. [Matrix Example in R](http://dattahub.github.io/stat5443/matrix_R.html)
2. [Random Variable Generation](http://dattahub.github.io/stat5443/rvgen.html)
3. [Lab Example: Quantile Transform](http://dattahub.github.io/stat5443/lab_ex.html)
4. [Importance Sampling](http://dattahub.github.io/stat5443/importance_sampling_demo.html)
5. [PCA](http://dattahub.github.io/stat5443/demoPCA.html)
6. [Modern Regression 1](http://dattahub.github.io/stat5443/regression_demo_1.html)
7. [Modern Regression 2](http://dattahub.github.io/stat5443/regression_demo_2.html)
8. [Lab on GLMNET](http://dattahub.github.io/stat5443/lab_glmnet.html)
9. [Lab on GLMNET & Solution](http://dattahub.github.io/stat5443/lab_glmnet_soln.html)
10. [Modern Regression 3](http://dattahub.github.io/stat5443/regression_demo_3.html)
11. [Logistic Demo](http://dattahub.github.io/stat5443/logistic_demo.html)
12. [Final Project Data](http://dattahub.github.io/stat5443/final_projectintro.html)
13. [Decision Tree, Random Forest](http://dattahub.github.io/stat5443/tree_demo.html)
14.  [Multidimensional Scaling](http://dattahub.github.io/stat5443/mds_demo.html)
15.  [Support Vector Machine](http://dattahub.github.io/stat5443/svm_islr.html)



