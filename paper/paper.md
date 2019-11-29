---
# Example from https://joss.readthedocs.io/en/latest/submitting.html
title: 'perccalc: An R package for estimating percentiles from categorical variables'
tags:
  - R
  - categorical data analysis
  - achievement gaps
authors:
  - name: Jorge Cimentada
    orcid: 0000-0001-5594-1156
    affiliation: 1
affiliations:
 - name: Laboratory of Digital and Computational Demography, Max Planck Institute of Demographic Research (MPIDR)
   index: 1
date: 2 October 2019
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
---

# Summary

Social science research makes extensive use of categorical variables. This
means that most variables in model definitions are a combination of categorical
and ordered categorical variables, which sometimes are proxies of continuous
variables such as income or years of education. The seriousness of this
phenomena can be best exemplified by the surge and usage of techniques tailored
specifically for this type of analysis in social science research [@agresti2010;
@agresti2011].

In particular, in educational research, where there is a maturing literature on
calculating inequality gaps, categorical data are essential for estimating
inequality. For example, the income of a person is often asked in income
brackets rather than the exact amount of money; researchers would prefer the
exact amount but to avoid non-response accumulation and privacy concerns, income
brackets are a partial solution. This solution gives the income information of
respondents but at the same time in a limited fashion, since we cannot
estimate traditional statistics such as the differences of percentiles from the
income brackets. One example of this is calculating the gap in cognitive
abilities between the top (e.g 90th percentiles) and bottom (e.g 10th
percentiles) groups in the income distribution.

`perccalc` is a direct implementation of the theoretical work of @reardon2011
where it is possible to estimate the difference between two percentiles from an
ordered categorical variable. More concretely, by specifying an ordered
categorical variable and a continuous variable, this method can estimate
differences in the continuous variable between percentiles of the ordered
categorical variable. This provides a meaningful strategy for contrasting ordered
categorical variables that usually have alternative continuous measures to the
percentiles of the continuous measures. Moreover, it opens an avenue for
calculating percentile distributions and percentile differences for ordered
categorical variables which do not necessarily have an alternative continuous
measure, such as job occupation classifications. A relevant example is the
classification found in @erikson1979.

Recently, this method has been growing in usage in education research
[@reardon2011; @reardon_portilla2016; @bassok2016; @chmielewski2016]. However, the
technique is not limited to this field alone and can be used essentially in any
context where percentiles of ordered categorical variables are of
interest. For example, biomedical research frequently involves
demographic characteristics such as education categories as
factors for looking at differences between groups.

The field of computational categorical data analysis has a long history in R,
with packages addressing small-area estimation for categorical variables
[@boonstr], missing data imputation [@vanbuuren] and standard generalized models
for ordinal data [@ordinal]. The `qualvar` package [@gombin] is one attempt to
focus not on the modelling of categorical variable but rather on the properties
of such variables to calculate variation in categorical variables.  Yet despite
the popularity of categorical-based methods, there is still not an official
software package that reliably implements and tests Reardon's method in the
**R** programming language [@rsoftware]; nor in any other programming language,
that I am aware of.

Currently, `perccalc` implements:

* Calculating differences in a continuous variable relative to the percentiles
  of an ordered categorical variable
* Calculating values for a continuous variable relative to the percentiles of an
  ordered categorical variable (values of a continuous variable for the 1th,
  10th, 20th, ..., 100th percentile of the ordered categorical variable)
* Weight-adjusted estimations for all percentile calculations
* Provides uncertainty estimates for all calculations which allows the user to
  produce uncertainty intervals or propagate further calculations with these
  uncertainty coefficients

`perccalc` offers flexibility and reliability for estimating any number of
percentile differences for ordered categorical variables as well as the
distribution of percentiles values for an ordered categorical
variable. Moreover, it provides the standard errors for the estimation which can
be used to construct uncertainty intervals. This full-featured implementation
offers a reliable software to use in serious peer-review research. Researchers
can trust this implementation as an accurate representation given that it has
been built by testing it to decimal accuracy to the theoretical model of
@reardon2011; these tests are continually checked on a weekly basis making the
package particularly reliable.

The major features (including examples addressing real world problems) of
`perccalc` are shown in a series of vignettes in the package's website
(https://cimentadaj.github.io/perccalc/), where there is a direct implementation
that matches @reardon2011's initial implementation. Additionally, the package
is hosted on its own open source repository on Github
(https://github.com/cimentadaj/perccalc/) and on the official CRAN repository
(https://cran.r-project.org/web/packages/perccalc/index.html)

# Acknowledgements

I acknowledge contributions from Sean Reardon in the writing of this software
both in the form of recommendations and of the theoretical underpinnings of the
implementation.

# References
