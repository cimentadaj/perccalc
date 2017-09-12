## ---- message = FALSE, warning = FALSE-----------------------------------
# install.packages(c("devtools", "matrixStats", "tidyverse"))
# devtools::install_github("pbiecek/PISA2012lite")

library(matrixStats)
library(tidyverse)
library(haven)
library(PISA2012lite)

## ---- message = FALSE, warning = FALSE-----------------------------------
ger_student <- student2012 %>%
  filter(CNT == "Germany") %>%
  select(CNT, STIDSTD, matches("^PV*.MATH$")) %>%
  transmute(CNT, STIDSTD,
            avg_score = rowMeans(student2012[student2012$CNT == "Germany", paste0("PV", 1:5, "MATH")]))

ger_parent <-
  parent2012 %>%
  filter(CNT == "Germany") %>%
  select(CNT, STIDSTD, PA07Q01)

ger_weights <-
  student2012weights %>%
  filter(CNT == "Germany") %>%
  select(CNT, STIDSTD, W_FSTUWT)

dataset_ready <-
  ger_student %>%
  left_join(ger_parent, by = c("CNT", "STIDSTD")) %>%
  left_join(ger_weights, by = c("CNT", "STIDSTD")) %>%
  as_tibble() %>%
  rename(income = PA07Q01,
         score = avg_score,
         wt = W_FSTUWT) %>%
  select(-CNT, -STIDSTD)

## ---- echo = FALSE-------------------------------------------------------
dataset_ready %>%
  arrange(income) %>%
  head(10)

## ---- message = FALSE, warning = FALSE-----------------------------------
devtools::install_github("cimentadaj/perccalc")
library(perccalc)

## ---- error = TRUE-------------------------------------------------------
perc_calculator(dataset_ready, income, score, percentiles = c(90, 10))

## ------------------------------------------------------------------------
dataset_ready <-
  dataset_ready %>%
  mutate(income = factor(income, ordered = TRUE))

## ------------------------------------------------------------------------
perccalc::perc_calculator(dataset_ready, income, score, percentiles = c(90, 10))

## ------------------------------------------------------------------------
perccalc::perc_calculator(dataset_ready, income, score, percentiles = c(50, 10))

## ------------------------------------------------------------------------
perccalc::perc_calculator(dataset_ready, income, score, weights = wt, percentiles = c(90, 10))

## ---- eval = FALSE-------------------------------------------------------
#  # Saving the dataset to a path
#  dataset_ready %>%
#    write_dta(path = "/Users/cimentadaj/Downloads/pisa_income.dta", version = 13)

## ---- eval = F-----------------------------------------------------------
#  *--------
#  use "/Users/cimentadaj/Downloads/pisa_income.dta", clear
#  
#  tab income, gen(inc)
#  *--------
#  
#  /*-----------------------
#  	Making a data set that has
#  	one observation per income category
#  	and has mean and se(mean) in each category
#  	and percent of population in the category
#  ------------------------*/
#  
#  tempname memhold
#  tempfile results
#  postfile `memhold' income mean se_mean per using `results'
#  
#  forv i = 1/6 {
#  	qui sum inc`i' [aw=wt]
#  	loc per`i' = r(mean)	
#  								
#  	qui sum score if inc`i'==1
#  							
#  	if `r(N)'>0 {
#  		qui regress score if inc`i'==1 [aw=wt]
#  		post `memhold' (`i') (_b[_cons]) (_se[_cons]) (`per`i'')
#  							
#  	}				
#  }
#  postclose `memhold'	
#  
#  /*-----------------------
#  	Making income categories
#  	into percentiles
#  ------------------------*/
#  
#  
#  	use `results', clear
#  
#  	sort income
#  	gen cathi = sum(per)
#  	gen catlo = cathi[_n-1]
#  	replace catlo = 0 if income==1
#  	gen catmid = (catlo+cathi)/2
#  	
#  	/*-----------------------
#  		Calculate income
#  		achievement gaps
#  	------------------------*/
#  
#  	sort income
#  	
#  	g x1 = catmid
#  	g x2 = catmid^2 + ((cathi-catlo)^2)/12
#  	g x3 = catmid^3 + ((cathi-catlo)^2)/4
#  
#  	g cimnhi = mean + 1.96*se_mean
#  	g cimnlo = mean - 1.96*se_mean
#  
#  	reg mean x1 x2 x3 [aw=1/se_mean^2]
#  
#  	twoway (rcap cimnhi cimnlo catmid) (scatter mean catmid) ///
#  		(function y = _b[_cons] + _b[x1]*x + _b[x2]*x^2 + _b[x3]*x^3, ran(0 1))	
#  	
#  	loc hi_p = 90
#  	loc lo_p = 10
#  
#  	loc d1 = [`hi_p' - `lo_p']/100
#  	loc d2 = [(`hi_p')^2 - (`lo_p')^2]/(100^2)
#  	loc d3 = [(`hi_p')^3 - (`lo_p')^3]/(100^3)
#  
#  	lincom `d1'*x1 + `d2'*x2 + `d3'*x3
#  	loc diff`hi_p'`lo_p' = r(estimate)
#  	loc se`hi_p'`lo_p' = r(se)
#  	
#  	di "`hi_p'-`lo_p' gap:     `diff`hi_p'`lo_p''"
#  	di "se(`hi_p'-`lo_p' gap): `se`hi_p'`lo_p''"

## ------------------------------------------------------------------------
perc_calculator(dataset_ready, income, score, weights = wt, percentiles = c(90, 10))

