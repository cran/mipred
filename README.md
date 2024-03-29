
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mipred

[![Build
Status](https://travis-ci.org/BARTJAMertens/mipred.svg?branch=master)](https://travis-ci.org/BARTJAMertens/mipred)

<!-- Check https://www.r-pkg.org/services   for badges -->

<!-- OR:    https://cran.r-project.org/web/packages/badgecreatr/vignettes/all_badges.html -->

<!-- [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-last-release/mipred)](https://CRAN.R-project.org/package=mipred) -->

<!-- [![](https://cranlogs.r-pkg.org/badges/grand-total/mipred)](https://CRAN.R-project.org/package=mipred) -->

<!-- [![Download counter](http://cranlogs.r-pkg.org/badges/mipred)](https://cran.r-project.org/package=mipred) -->

<!-- [![Rdoc](http://www.rdocumentation.org/badges/version/mipred)](http://www.rdocumentation.org/packages/mipred) -->

The goal of mipred is to calibrate a prediction rule using generalized
linear models or Cox regression modeling, using multiple imputation to
account for missing values in the predictors as described by Mertens,
Banzato and de Wreede (2018) (<https://arxiv.org/abs/1810.05099>).
Imputations are generated using the R package ‘mice’ without using
outcomes on observations for which the prediction is generated. Two
options are provided to generate predictions. The first is
prediction-averaging of predictions calibrated from single models fitted
on single imputed datasets within a set of multiple imputations. The
second is application of the Rubin’s rules pooled model. For both
implementations, unobserved values in the predictor data of new
observations for which the predictions are derived are automatically
imputed. The package contains two basic workhorse functions, the first
of which is mipred() which generates predictions of outcome on new
observations (when outcomes will by definition usually not be available
at the time of calibration of the prediction rule). The second is the
function mipred.cv() which generates cross-validated predictions with
the methodology on existing data for which outcomes have already been
observed. This allows users to assess predictive potential of the
prediction models which are calibrated. The present version of the
package is preliminary (development) and has only been thoroughly
checked for application on binary-outcome logistic regression for now.
The vignette which is included documents application of the functions
for binary outcome data. Although we did not check extensively, the
package should also work for continuous and counting outcomes. We are
working to expand the functionality to censored survival outcomes.

## Installation

You can install the released version of mipred from
[CRAN](https://CRAN.R-project.org).  
<!-- with:  -->

Alternatively, you can install the current version into R from GitHub
using devtools:

``` r
library(devtools)
devtools::install.github("BartJAMertens/mipred")
```

For installation from Github, you may need to install and load the
devtools package first before using the above command. See the book “R
packages” (online version) by Hadley Wickham, chapter “Git and Github”.

## Main functions

There are currently two key functions

mipred() \# prediction calibration with multiple imputation for missing
predictors  
mipred.cv() \# cross-validation for prediction calibration with multiple
imputation for missing predictors

The first function calibrates predictions for new observations and
accounts for missing values in the predictor data (of either the
calibration or new validation sample) through multiple imputation. The
second function implements cross-validation of the same approach.

## Example

Let `dataset` be a data.frame consisting of a vector of binary outcomes
`outcome` and two predictors `x1` and `x2`. The outcome must be fully
observed. Likewise, let `newdataset` be a data.frame with new
observations for which the same predictors `x1` and `x2` are observed
and for which we want to predict outcome, using a model fitted to the
old data in `dataset`. Either or both of these predictors may contain
missing values in the calibration data, but this is also allowed in the
new data for which we want to generate predictions.

We can generate predictions using the
    command

    preds <- mipred(outcome ~ x1 + x2, family=binomial, data=dataset, newdata=newdataset, nimp=100)

This will use the logistic regression model and 100 imputations.

If we wanted to generate cross-validated predictions within the set
`dataset`, then we can generate these with the same model
    using

    preds.cv <- mipred.cv(outcome ~ x1 + x2, family=binomial, data=dataset,  nimp=100,  folds=10)

This will generate cross-validated predictions from the same model and
100 imputations for each predicted observation, using 10-folds.

Please refer to the example included with the package. The package also
includes a vignette which documents use for binary outcome
data.

<!-- This is a basic example which shows you how to solve a common problem: -->

<!-- ```{r example} -->

<!-- ## basic example code -->

<!-- ``` -->

<!-- What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so: -->

<!-- ```{r cars} -->

<!-- summary(cars) -->

<!-- ``` -->

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. -->

<!-- You can also embed plots, for example: -->

<!-- ```{r pressure, echo = FALSE} -->

<!-- plot(pressure) -->

<!-- ``` -->

<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub! -->
