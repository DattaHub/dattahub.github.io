## Data Analytics R Tutorials 

### Details 

**Course Title**: DAAS 5525 (Data Analytics)
**Semester/term**: Summer 2022 <br>
**Meeting time**: : Thursday: 6-9 pm (Online via zoom) <br>
**Canvas web-site**: [Canvas](https://canvas.vt.edu/courses/144945) <br>
**Office hours**: Friday 9-10 am or by appointment (online: zoom link on canvas).  <br>
**Instructor**: Dr. Jyotishka Datta (Email: jyotishka@vt.edu, Office: Hutcheson 409C <br>

Git repo for this course: https://github.com/DattaHub/DattaHub.github.io/tree/master/data-analytics

**This is just the R repository for some of the demos I will show in class. The lecture notes are available on Canvas. I will also upload the PDF prints of these demos on Canvas.**

## R demos/tutorials 

(*You'll find below links to some R tutorials for this course, but these will be updated during the course, adjusting to the needs and interest of the current cohort. These are generated using R-markdown and you can find the source files by going to the github repository. In addition, I will post base R codes for different topics covered.*)

#### Introduction to R 

(*You can skip to 4 directly if you know about vectors and data-frames.*)

1. What is R: https://dattahub.github.io/data-analytics/000_why_R.html 
2. Vectors and data types: https://dattahub.github.io/data-analytics/02-vectors-and-datatypes.html
3. Data frames: https://dattahub.github.io/data-analytics/03-data-frames.html
4. Logic in R and functions: https://dattahub.github.io/data-analytics/00_logic_in_R.html#1 

#### R-tools for data wrangling and visulization 

1. Packages and R-markdown: https://dattahub.github.io/data-analytics/04-packages-and-rmarkdown.html 
2. Using dplyr: https://dattahub.github.io/data-analytics/05-dplyr.html
3. Data Viz Using ggplot2:
https://dattahub.github.io/data-analytics/06-data-visualization-slide.html (slides, preferred)
https://dattahub.github.io/data-analytics/06-data-visualization.html (notes)
4. Basic Statistics (we might skip this): https://dattahub.github.io/data-analytics/07-statistics.html 
5. Efficiency considerations: https://dattahub.github.io/data-analytics/08-efficient_R.html 
6. Matrix in R: https://dattahub.github.io/data-analytics/09-matrix_R.html 

### Univariate and multiple regression basics 

1. Simple univariate regression [Canvas folder link, needs login](https://canvas.vt.edu/courses/144945/files/folder/Lecture%205%3A%20Simple%20linear%20regression)
2. Multiple regression basics [Canvas folder link, needs login](https://canvas.vt.edu/courses/144945/files/folder/Lecture%206%3A%20Multiple%20Linear%20Regression)
3. (Related topic) Bias and variance trade-off in ML [Canvas folder link, needs login](https://canvas.vt.edu/courses/144945/files/folder/Lecture%207-9%3A%20Modern%20regression%201)

#### Modern Regression 

1. Modern Regression 1: http://dattahub.github.io/data-analytics/regression_demo_1.html (adjusted $R^2$, AIC, BIC, stepwise procedures and Ridge)
2. Modern Regression 2: http://dattahub.github.io/data-analytics/regression_demo_2.1.html
3. Modern Regression 3: http://dattahub.github.io/stat5443/regression_demo_3.1.html
4. Lab on Shrinkage Methods: Ridge, Lasso etc.
http://dattahub.github.io/data-analytics/lab_glmnet.html
5. PCA, PCR demo: https://dattahub.github.io/data-analytics/demoPCA.html#1

#### Resampling methods 

1. Cross-validation: https://dattahub.github.io/data-analytics/crossvalidation.html 
2. Bootstrap : https://dattahub.github.io/data-analytics/bootstrap_demo.html


#### Classification & KDE

0. Challenger case study: https://dattahub.github.io/data-analytics/challenger_casestudy_ninja.html 
1. Kernel Density: https://dattahub.github.io/data-analytics/kde_demo.html 
2. Logistic demo: https://dattahub.github.io/data-analytics/logistic_demo.html (from last week)
3. Deviance explained: https://dattahub.github.io/data-analytics/deviance_explained.html 
4. Multiclass Logistic Regression : https://dattahub.github.io/data-analytics/logistic_other.html 
5. LDA, QDA & KNN: https://dattahub.github.io/data-analytics/classification_demo.html#27 (# denotes page) 
6. Credit card default example: https://dattahub.github.io/data-analytics/credit_card_default.html
7. Sparse Classification: https://dattahub.github.io/data-analytics/sparse_classification.html 



#### Trees, random forests etc.

1. Trees, Bagging, Boosting: http://dattahub.github.io/data-analytics/tree_demo_2023.html

#### Multile Testing

1. http://dattahub.github.io/data-analytics/multiple_testing.html

#### Neural net

1. Stochastic Gradient Descent: http://dattahub.github.io/data-analytics/sgd_demo.html
2. Introduction to Artificial Neural Net: http://dattahub.github.io/data-analytics/neural_net_demo.html

### Email: 

Please use "Data Analytics + [Your query]" on the subject line for me to easily find and respond to your email. Example: if you have a query about Q.1 on homework 3, your subject line should be something like: "Data Analytics (Query about HW1, Q1)". 

### Covid-19 classroom expectations:

Students are required to follow the university's guidelines for public health and uphold the wellness commitment. Please visit https://ready.vt.edu/public-health-guidelines.html#wellness for more information. 

**Patience and Communication:** As we navigate coming back to in-person modality for a technical class that has a strong focus in creative, group, problem solving, we will discover that some course choices will work and not work for supporting your needs to learn. I will be as responsive as possible to such discoveries.  In no way do I want your experience as a student to suffer from constraints imposed by Covid-19 and the university.  Please be patient and communicate your needs as they arise. 

### Course Outlines

(Items in _italic fonts_ are optional, depending on interest, preparation etc.)

**Module 1: Foundations:**

1. Computational Complexity
2. Introduction to algorithms and basic concepts (Divide-and-Conquer Strategy)
4. Recap: Maximum likelihood estimation and least squares.
4. _Linear Systems (Gaussian Elimination, Cholesky Decomposition etc.)_
5. Introduction to R / basic programming.

**Module 2:** Resampling Methods, Cross-Validation, Bootstrap

**Module 3: Statistical Learning:**

1. **Background / Introduction**

1. Unsupervised and Supervised Learning.
2. Bias-Variance Decomposition.
3. Accuracy vs. Model Interpretability.

1. **Unsupervised Learning:**

1. Principal components analysis and other clustering methods.

1. **Supervised Learning**

1. Modern Regression: (large p, small n:  **wide**  data)

- Basics - Geometry of Regression.
- Subset selection: forward selection/backward selection/Best subset - AIC.
- Shrinkage and Selection: Ridge, LASSO, Elastic Net.
- _Bayesian Lasso and Horseshoe._
- Principal Components Regression.

1. Classification:

- Logistic Regression (with ???1 penalty).
- LDA and QDA.
- Comparison.

1. Decision Tree, Bagging, Boosting and Random Forest.
2. _Support Vector Machines._

**Module 4: Advanced topics:**

  1. Beyond Linearity, Splines, GAMs
  2. Artificial Neural Network.

**Note:** This is only a _ **tentative list of topics** _ - the instructor may add topics or change the length of time spent on any particular topic to accommodate this particular class of students.


**Homework Policies:**

1. Unless otherwise announced, all homework assignments will be posted on Wednesday or Friday afternoon and will be due in exactly a week later on the following week.

2. You need to present complete solution for the homework problem, and not just the final answer, to get full marks for a problem.

3. The lowest homework grade will be dropped.

**Group discussions are encouraged to further understand difficult topics. You may consult with other students about homework problems, provided that you indicate such information (whom you consulted with, which problem, to which extent) on your solution sheet. However, you must refrain from getting direct answers from others. Any violation will result in zero credit for the assignment.**

**Study Plan:**

**Textbook:**

This is a graduate level course and we will mostly follow the ISLR book for many of the topics:

An Introduction to Statistical Learning by James, Witten, Hastie &amp; Tibshirani.

You can get the pdf file for the first edition from this website:

[https://www.statlearning.com/](https://www.statlearning.com/)

For the remaining topics, I draw upon a number of resources and books to teach this course. Some of them can be found on the homepage and are listed below:

1. Algorithms, Dasgupta et al.:  [http://algorithmics.lsi.upc.edu/docs/](https://nam03.safelinks.protection.outlook.com/?url=http%3A%2F%2Falgorithmics.lsi.upc.edu%2Fdocs%2FDasgupta-Papadimitriou-Vazirani.pdf&amp;data=02%7C01%7Cjd033%40uark.edu%7C7f92fc9ea6ee40081d3708d7b87cf9f8%7C79c742c4e61c4fa5be89a3cb566a80d1%7C0%7C0%7C637180719216291158&amp;sdata=M9T2VYakqwVFIdqakLHjUbzJyFVzDfPnw%2BPBcs%2BCqG0%3D&amp;reserved=0)Dasgupta-Papadimitriou-Vazirani.pdf
2. Elements of Computational Statistics: Gentle. (E-copy at:[https://link-springer-com.ezproxy.lib.vt.edu/book/10.1007%2Fb97337](https://link-springer-com.ezproxy.lib.vt.edu/book/10.1007%2Fb97337) )
3. The Elements of Statistical Learning (2nd edition), Hastie, Tibshirani and Friedman (2009). Springer-Verlag. .[https://web.stanford.edu/~hastie/ElemStatLearn/](https://web.stanford.edu/~hastie/ElemStatLearn/)

However, I will regularly post handouts with material covered in the class. I will post lecture notes for almost all topics except standard ones for which I will refer you to chapters in a text book or to other standard resources.

**Software:** I will also teach the use of R. For all methods introduced in class, you will learn how to implement those methods using R. You need to install R in your laptop (or the computing resource you use). It is available (free) from [http://cran.r-project.org/](http://cran.r-project.org/).

The software R comes with its own GUI (graphic user interface), where you can write your code and see the results. There is also a very nice interface called R-Studio (more compact, and easy to use), the basic version of R-studio is free and can be downloaded from [https://www.rstudio.com/](https://www.rstudio.com/).

There are many books for teaching the basics of R, also lots of resources on the internet.

1. ATS UCLA R Page: [https://stats.idre.ucla.edu/r/](https://stats.idre.ucla.edu/r/) (This is a great resource by itself, and points to a lot of other useful resources too!)

2. Quick R: [http://www.statmethods.net/](http://www.statmethods.net/)

3. R for data-science: [http://r4ds.had.co.nz/index.html](http://r4ds.had.co.nz/index.html)

4. R bloggers: [https://www.r-bloggers.com/](https://www.r-bloggers.com/)

**Changes to the Syllabus:**

The instructor reserves the right to make changes to the syllabus during the course. Any necessary changes will be announced in class and posted on the course website.



