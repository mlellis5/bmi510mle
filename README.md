The objective of the BMI-510 final project is to familiarize myself with creating documented, reusable code for projects and colleagues. 

I created a a github repository containing a small R package with functions described below.

**Installation**
```{r}
install.packages("bmi510mle")
```

**R functions included in package:**

  1. logLikBernoulli = function(data)

     A function that takes a vector like data = c(1,0,0,0,1,1,1,) and calculates the parameter p that maximizes the log-likelihood. Uses a grid-based search with p in steps of 0.001.
```{r}
logLikBernoulli = function(data)
```

  2. survCurv = function(status,time)

      A function that takes a numerical vector status and anumerical vector time, and calculates and plots a survival curve S(t).
```{r}
survCurv = function(status,time)
```

  3. unscale = function(x)

       A function that takes a vector that has been put through scale and reverses the centering/scaling, if any.
```{r}
unscale = function(x)
```

  4. pcApprox = function(x, npc)

       A function that returns an approximation to the data x based on npc number of PCs that has been rescaled and centered to match the original data.
```{r}
pcApprox = function(x, npc)
```

  5. standardizeNames = function(data)

         A function that serves as a wrapper around dplyr::rename_with and janitor::make_clean_names that converts the variables in a tibble data to "small_camel" case. 
```{r}
standardizeNames = function(data)
```

  6. minimumN = function(x1,x2)

      A function that serves as a wrapper around pwr::pwr.t2n.test that takes either one (x1)or two (x2) samples of preliminary data and returns the minimum sample size needed for a t-test of the null hypotheses that either mu_x1 == 0 or mu_x1 == mu_x2 with 80% power at alpha=0.05.
```{r}
minimumN = function(x1,x2)
```

  7. downloadRedcapReport = function(redcapTokenName,redcapUrl,redcapReportId)

      A function that: (1) uses Sys.getenv() to read an API token called redcapTokenName from the users’ .REnviron file; (2) queries redcapUrl to return the Redcap Report specified by redcapReportId; (3) returns the contents as a tibble.
```{r}
downloadRedcapReport = function(redcapTokenName,redcapUrl,redcapReportId)
```
