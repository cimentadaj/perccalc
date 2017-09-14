## ---- message = FALSE, warning = FALSE-----------------------------------
library(tidyverse)
library(haven)

## ---- message = FALSE, warning = FALSE-----------------------------------
temp_file <- tempfile(fileext = ".zip")
download.file("http://gss.norc.org/Documents/stata/2016_stata.zip", temp_file)
unzip(temp_file, exdir = tempdir())

data_link <- list.files(tempdir(), full.names = TRUE, pattern = "*.DTA")

gss <-
  read_dta(data_link) %>%
  select(rincome, prestg10, wtss) %>%
  rename(
    income = rincome,
    prestige = prestg10,
    wt = wtss
  )

unlink(c(temp_file, data_link)) # deleting both files.

gss

## ---- eval = F-----------------------------------------------------------
#  devtools::install_github("cimentadaj/perccalc")
#  library(perccalc)

## ---- echo = F, message = F, warning = F---------------------------------
library(perccalc)

## ---- error = TRUE-------------------------------------------------------
perc_diff(gss, income, prestige, percentiles = c(90, 10))

## ------------------------------------------------------------------------
gss <-
  gss %>%
  mutate(income = factor(income,
                        ordered = TRUE))

## ------------------------------------------------------------------------
perc_diff(gss, income, prestige, percentiles = c(90, 10))

## ------------------------------------------------------------------------
perc_diff(gss, income, prestige, percentiles = c(50, 10))

## ------------------------------------------------------------------------
perc_diff(gss, income, prestige, weights = wt)

## ---- eval = FALSE-------------------------------------------------------
#  # Saving the dataset to a path
#  gss %>%
#    write_dta(path = "/Users/cimentadaj/Downloads/gss_income.dta", version = 13)

## ---- eval = F-----------------------------------------------------------
#  *--------
#  use "/Users/cimentadaj/Downloads/gss_income.dta", clear
#  
#  drop if missing(income)
#  drop if missing(prestige)
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
#  forv i = 1/12 {
#  	sum inc`i' [aw=wt]
#  	loc per`i' = r(mean)
#  									
#  	qui sum prestige if inc`i'==1
#  							
#  	if `r(N)'>0 {
#  		qui regress prestige if inc`i'==1 [aw=wt]
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
perc_diff(gss, income, prestige, weights = wt)

## ------------------------------------------------------------------------
perc_calculator(gss, income, prestige) %>%
  head()

## ---- fig.align = 'center', fig.width = 6, fig.height = 5----------------
gss %>%
  perc_calculator(income, prestige, wt) %>%
  mutate(ci_low = estimate - 1.96 * std.error,
         ci_hi = estimate + 1.96 * std.error) %>%
  ggplot(aes(percentile, estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_hi))

## ------------------------------------------------------------------------
perc_calculator(gss, income, prestige, wt) %>%
  filter(percentile %in% c(90, 10)) %>%
  summarize(diff = diff(estimate),
            se_diff = diff(std.error))

## ------------------------------------------------------------------------
perc_diff(gss, income, prestige, weights = wt, percentiles = c(90, 10))

