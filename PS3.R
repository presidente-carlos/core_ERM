---
title: 'Core ERM Problem Set #3'
author: "2937141"
date: "Due at Noon on Thursday, May 19th"
output: pdf_document
fontsize: 12pt
---

## Problem \#1 - The Supervisor Problem

In this problem you will use simulation explore a simple and slightly silly example of an *optimal stopping* problem. Such problems crop up throughout economics, probability and statistics, and operations research. For an analytical approach to the problem outlined below see *Fifty Challenging Problems in Probability with Solutions* by Frederick Mosteller. For an approachable overview of optimal stopping problems, see Brian Christian and Tom Griffiths' recent book *Algorithms to Live By*.

Soon you will have to choose your MPhil thesis supervisor. If you could meet and talk with all possible supervisors, you'd be able to rank them from best fit to worst fit, depending on your research interests. (For simplicity assume that there are no ties.) Somewhere in the department is your *most preferred* supervisor: you just have to find them! 

If you meet with a potential supervisor, you will immediately learn the quality of fit with your research interests. You can think of this as a "supervisor score" that is revealed during the meeting. At the end of the meeting your potential supervisor will make you a take-it-or-leave-it offer. If you accept, then congratulations: you've found your supervisor! If you decline, then your jilted potential supervisor will be so heartbroken that they will be emotionally incapable of speaking to you again. This limits your ability to shop around for a supervisor. Once you've turned someone down, you can't go back and change your mind. Once you've accepted an offer, you can't renege.

Being a methodical but extremely busy young economist, you decide on the following two-phase strategy. In phase one you randomly choose an "initial sample" of `k` from the full list of `n` potential supervisors. You then meet with each of these `k`. No matter how well the meetings go, you *decline* these first `k` offers of supervision. At the end of this process you are still without a supervisor but you've collected `k` "supervisor scores." Call the maximum score among the initial sample `max_score`. In phase two, you sequentially meet with the remaining `n - k` supervisors in random order until you either encounter one whose score is higher than `max_score` or run out of options. In the latter case, you're stuck with whichever supervisor you met with last.

With the appropriate choice of `k`, this two-phase strategy can be shown to maximize the probability that you will end up with your most preferred supervisor. So what value of `k` should you choose? If you choose a very large value, there's a good chance that your most preferred supervisor will be among the initial `k`, so you'll turn down their offer. If you choose a very low value, then `max_score` will likely be too low and you'll end up "settling" for a less-than-ideal supervisor. It seems clear that the best choice of `k` should depend on `n`, but how? And if you make the best possible choice, when is the probability that you'll end up with your most preferred supervisor? Let's find out! 

1. Write a function called `supervisor_sim()` that takes two input arguments: `n` is the number of potential supervisors, and `k` is the size of your phase one "initial sample." Your function should simulate one "draw" of the two-phase procedure described above:
    - Randomly permute the list of supervisors. 
    - Identify the best supervisor from the initial sample of `k`. 
    - Cycle through the remaining `n - k` supervisors until you hit your stopping criterion or run out of candidates. 
    - Return the *rank* of the supervisor you end up with, i.e. `1` for your most preferred, `2` for your second most preferred, and so on.
    
```{r}
supervisor_sim<-function(n, k){
  
 ## Create a vector of professors with an associate ranking

 supervisors<-1:n
  
 ## Draw k candidates
 first_stage<-sample(1:n, k, replace = FALSE)
 first_stage_sup<-supervisors[supervisors %in% first_stage]

 ## Select best supervisor in first stage
 first_best<-first_stage_sup[first_stage_sup==min(first_stage_sup)]

 ## Identify the n-k supervisors
 second_stage_sup<-supervisors[!supervisors %in% first_stage_sup]

## Loop
  for (i in 1:length(second_stage_sup)){
    
    ## Randomly sample a new potential supervisor from the remaining n-k
    second_best<-sample(second_stage_sup, 1)
    ## Drop this supervisor from the n-k list
    second_stage_sup<-second_stage_sup[second_stage_sup!=second_best]

    ## Break loop if supervisor is better than first_best
    if (second_best<first_best){
      break
    }
    
    ## Break loop if lenght of vector of n-k supervisors =1
    ## sample does weird things when vector is of length one so we don't
    ## want the loop to extract the last element of the vector
    if (length(second_stage_sup)==1){
      second_best<-second_stage_sup
      break
    }
  }

second_best

}

```

  
2. Write a function called `get_prob_preferred()` that combines `supervisor_sim()` with `replicate()` to approximate the probability of getting your preferred supervisor for a given choice of `k` and `n` based on 10,000 simulation replications. Test this function with `n = 50` and `k = 5`. You should get a result within `0.235` plus or minus `0.01`. If you don't, then double-check your code for both this part and the preceding one.

```{r}
get_prob_preferred<-function(n, k, n_rep=10000){
  store<-replicate(n_rep, supervisor_sim(n,k))
  mean(store==1)
}

## Check code
set.seed(1234)
get_prob_preferred(n=50,k=5) ## Result 0.2433 (<0.245)
```



3. I'll let you in on a secret: `k = 10` is *sub-optimal* for `n = 50` but the optimal value lies between `5` and `35`. Combine `get_prob_best()` with `Map()` to approximate the optimal choice of `k` and the corresponding probability of getting your most preferred supervisor when you choose `k` optimally.

```{r}
## Generate band of results to loop over using Map
k_band<-5:35
sup_100<-Map(get_prob_preferred, n=50, k=k_band)

## Formatting
sup_100_row<-do.call(rbind, sup_100)
results<-data.frame(k_band=k_band, prob_of_best = sup_100_row)

## Best: k=18
results[results$prob_of_best==max(results$prob_of_best),]
```


4. **Optional Bonus Part**: Repeat the preceding for larger choices of `n`. How do the results change? Can you guess the optimal choice of `k` as a fraction of `n`?

```{r}
library(tidyverse)

## I suggest not running this piece (estimated runnning code 50mins-1hr)

## Expand grid to loop over using Map
# k_band<-5:35
# n_band<-40:70

# bands<-expand.grid(n=n_band, k=k_band)

# sup_n<-Map(get_prob_preferred, n=bands$n, k=bands$k)

## Formatting
# sup_n_row<-do.call(rbind, sup_n)
# results_optional<-data.frame(n_band=bands$n, k_band=bands$k, 
#                              prob_of_best = sup_n_row)

## Computing average best k to n ratio
# summary<-results_optional %>% group_by(n_band) %>%
#                               summarize(best_prob = max(prob_of_best))
# results_filtered<-results_optional[results_optional$prob_of_best %in%
#                                      summary$best_prob,]
# results_filtered$share<-results_filtered$k_band/results_filtered$n_band
# mean(results_filtered$share)

## On average, the optimal k lies around 37% of the sample size n (37.19%)

```


**Hints / Pep Talk:** This is a *hard* problem, but once you crack the first part, the rest of the problem should be fairly straightforward. As usual when it comes to simulations, there are various ways to set this up, but one point may be helpful to consider. The ideas of "supervisor scores" and `max_score` in the write-up above are helpful for describing the stopping rule, but you do not necessarily need to generate and work with supervisor scores in your simulation code. It turns out to be enough to work with the *ranks* of the randomly-ordered supervisors.

## Problem \#2 - Robust Standard Errors

As we saw in Lesson \#4, the command `lm_robust()` from the `estimatr` package provides various "flavors" of heteroskedasticity-robust standard errors: HC0, HC1, HC2, and HC3. In this problem you will use simulation to compare the performance of these alternatives in the simplest possible example: a regression model with a single dummy variable $D_i \in \{0, 1\}$ and normal errors. Suppose that:
$$
Y_i = \alpha + \beta D_i + U_i, \quad U_i|D_i \sim \text{indep. N}\big(0, (1 - D_i) \sigma^2 + D_i \big)
$$
for $i = 1, ..., n$. Notice that this design allows for heteroskedasticity. If $D_i = 1$ then $U_i$ is standard normal, but if $D_i = 0$ then $U_i \sim \text{N}(0, \sigma^2)$. By changing the value of $\sigma^2$ we can change the extent of heteroskedasticity in the simulation design.

Our goal is to determine which method provides the most accurate approximation to the standard error of $\widehat{\beta}$, the ordinary least squares estimator of $\beta$. Because this example is so simple, there's no need to use `lm_robust()` or even `lm()` to calculate $\widehat{\beta}$. It simply equals the difference of sample means for observations with $D_i = 1$ compared to those with $D_i = 0$, i.e.
$$
\widehat{\beta} = \bar{Y}_1 - \bar{Y}_0, \quad 
\bar{Y_1} \equiv \frac{1}{n_1}\sum_{\{i \colon D_i = 1\}}  Y_i, \quad
\bar{Y_0} \equiv \frac{1}{n_0}\sum_{\{i \colon D_i = 0\}}  Y_i
$$
where $n$ is the total sample size, $n_1$ is the number of observations for which $D_i = 1$ and $n_0$ is the number of observations for which $D_i = 0$:
$$
n_1 = \sum_{i=1}^n D_i, \quad n_0 = n - n_1.
$$
Again, because this example is so simple, we don't need to use `lm_robust()` to calculate the standard errors for us. Let $\text{RSS}_0$ denote the residual sum of squares for the observations with $D_i=0$ and $\text{RSS}_1$ denote the total sum of squares for the observations with $D_i=1$, in other words
$$
\text{RSS}_0 \equiv \sum_{\{i \colon D_i = 0\}}^n (Y_i - \bar{Y}_0)^2,\quad
\text{RSS}_1 \equiv \sum_{\{i \colon D_i = 1\}}^n (Y_i - \bar{Y}_1)^2
$$
Using this notation, it can be shown that the *usual*, i.e. non-robust, standard error for $\widehat{\beta}$ is given by
$$
\text{SE}_{\text{usual}} = \sqrt{\left(\frac{n}{n_0 n_1}\right) \left( \frac{\text{RSS}_0 + \text{RSS}_1}{n - 2}\right)}.
$$
while the various flavors of "robust" standard errors are given by
$$
\begin{aligned}
\text{SE}_0 &= \sqrt{\frac{\text{RSS}_0}{n_0^2} + \frac{\text{RSS}_1}{n_1^2}}\\
\text{SE}_1 &= \sqrt{\frac{n}{n-2}\left(\frac{\text{RSS}_0}{n_0^2} + \frac{\text{RSS}_1}{n_1^2}\right)}\\
\text{SE}_2 &= \sqrt{\frac{\text{RSS}_0}{n_0(n_0 - 1)} + \frac{\text{RSS}_1}{n_1(n_1 - 1)}}\\
\text{SE}_3 &= \sqrt{\frac{\text{RSS}_0}{(n_0 - 1)^2} + \frac{\text{RSS}_1}{(n_1 - 1)^2}}
\end{aligned}
$$
where $\text{SE}_0$ corresponds to HC0, $\text{SE}_1$ to HC1, and so on. **To keep things simple, set $n  = 30$ and $\alpha = \beta = 0$ in all of your simulations below.** The choice of $\alpha$ and $\beta$ doesn't matter for the results. The choice of $n$ does matter. I've chosen $n = 30$ so that you can check your results against those from Table 8.1.1 of *Mostly Harmless Econometrics* if you wish. The parameters that will vary in this exercise are $n_1$ and $\sigma$.

1. Write code to calculate each of the standard error estimators from above. Then simulate one draw of the simulation design with $n_1 = 3$ and $\sigma = 0.5$ and use it to check your work by comparing against the standard errors provided by `lm()` and `lm_robust()`. They should match *exactly*.

```{r}

## Number of observations
n<-30

## Function to generate simulated data frames 
## (useful for next part of the exercise)

sim_data<-function(n_1, sigma){

  ## Generate n_0
  n_0<-n-n_1
  
  ## Generate D
  D<-c(rep(1,n_1), rep(0,n_0))
  
  ## Generate Y (based on D)
  Y<- rnorm(n, mean = 0, sd = (D + (1-D)*sigma))
  
  simulation<<-data.frame(D=D, Y=Y) ## To save it in global environment

}

## Function to generate standard errors
errors<-function(n_1, sigma){
  
  ## Generate n_0
  n_0<-n-n_1
  
  ## Generate D
  D<-c(rep(1,n_1), rep(0,n_0))
  
  ## Generate Y (based on D)
  Y<- rnorm(n, mean = 0, sd = (D + (1-D)*sigma))
  
  simulation<-data.frame(D=D, Y=Y)

  ## Split data frame for operational convenience later on
  Y0<- simulation %>% filter(D==0)
  Y1<- simulation %>% filter(D==1)
  
  ## Compute mean
  avg_Y0<-mean(simulation[simulation$D==0,]$Y)
  avg_Y1<-mean(simulation[simulation$D==1,]$Y)
  
  ## Identifying coefficients (important for next exercise)
  empirical_beta<-rep(avg_Y1-avg_Y0,5)
  
  ## Identify RSS
  RSS0<-sum((Y0$Y-avg_Y0)^2)
  RSS1<-sum((Y1$Y-avg_Y1)^2)

  ## Compute errors
  conventional<-sqrt((n/(n_0*n_1))*((RSS0+RSS1)/(n-2)))
  hc0<-sqrt(RSS0/(n_0^2) + RSS1/(n_1^2))
  hc1<-sqrt((n/(n-2))*(RSS0/(n_0^2) + RSS1/(n_1^2)))
  hc2<-sqrt(RSS0/(n_0*(n_0-1)) + RSS1/(n_1*(n_1-1)))
  hc3<-sqrt(RSS0/((n_0-1)^2) + RSS1/((n_1-1)^2))
  
  ## Store and display results
  errors<-c(conventional=conventional, hc0=hc0,hc1=hc1,hc2=hc2,hc3=hc3)
  
  results<-data.frame(empirical_beta = empirical_beta, errors = errors)
  results
}

## Example
set.seed(12345)
errors(3,0.5)

## Check my answers (They match perfectly!)
 library(estimatr)
 set.seed(12345)
 sim_data(3,0.5)
 
 ## Convetional (0.2952)
 summary(lm(Y~D, data = simulation))$coefficients[2,2] ## 0.2952
 
 for (i in c("HC0", "HC1", "HC2", "HC3")){
   model<- lm_robust(Y~D, data = simulation, se_type = i)
   print(paste(i, ":", summary(model)$coefficients[2,2]))
 }
 
  ## HC0 (0.2276)
  ## HC1 (0.2355)
  ## HC2 (0.2716)
  ## HC3 (0.3264)


```


2. Carry out a simulation study with $n_1 = 3$ and $\sigma = 0.5$ to answer the following questions:
    (i) What is the mean of the sampling distribution of $\widehat{\beta}$?
    (ii) What is the *true* standard error of $\widehat{\beta}$? (In other words, what is the standard deviation of the sampling distribution of $\widehat{\beta}$?)
    (iii) Like $\widehat{\beta}$, each of the five standard errors from above has *its own* sampling distribution. What are the means and standard deviations of these distributions?
    (iv) Given a point estimate $\widehat{\beta}$ and standard error estimate $\widehat{\text{SE}}$ we can form a t-ratio $|\widehat{\beta}|/\widehat{\text{SE}}$ to test the null hypothesis $H_0\colon \beta =0$ against the two-sided alternative. In our simulation design $\beta = 0$ so a 5\% test should reject the null 5\% of the time. If you base your test on a standard normal critical value, what is the *actual* rejection rate of a *nominal* 5\% test for each of the five standard error estimators?
    
  (i) -0.06 which is completely coherent with the true value of $\beta = 0$. I have used 500 iterations to reach this value (see code below).
  (ii) [I don't fully understand the concept of "true" in this context]. Still, the standard deviation of the sampling distribution (under 500 iterations) is 0.59. This result is quite consistent with our parameters. Note that 27 out of 30 observations have a "true" standard deviation of 0.5. While 3 out 30 observations have a "true" standard deviation of 1. Hence, we would expect a $0.5*27/30+1*3/30=0.55$. 0.59 is reasonably close to this "true" value.
  (iii) See table below for full results.
  (iv)  See table below for full results.
  
```{r}

## To get access to melt() function for presentation purposes
#install.packages("reshape2")
library(reshape2)

set.seed(1234)
## Generate simulation function
sim_errors<-function(n_rep=500, n_1, sigma){
  
  ## Replicate
  store<-replicate(n_rep, errors(n_1, sigma))

  ## Beta coefficients
  coeff<-melt(store[1,])
  
  ## Mean of \beta
  print(mean(coeff$value))

  ## Standard deviation
  coeff_summary<-coeff %>% group_by(L1) %>% summarize(beta_dist = mean(value))
  coeff_summary<-as.data.frame(coeff_summary)
  print(sd(coeff_summary$beta_dist))
 
  ## Plot distribution of \beta
  coeff_plot<-coeff_summary %>% ggplot() +
              geom_histogram(aes(x=beta_dist))
  print(coeff_plot)

  ## Standard errors
  errors<-melt(store[2,])
  errors$type<-c("conventional", "hc0", "hc1", "hc2", "hc3")
  errors_summary<-errors %>% group_by(type) %>%
                  summarize(avg_error = mean(value), sd_error = sd(value))
  print(errors_summary)

  ## Plot errors distribution by type
  errors_plot<- errors %>% ggplot() +
                geom_histogram(aes(x=value)) +
                facet_grid(~type)
  print(errors_plot)

  ## T-ratios
  testing<-data.frame(coeff_value = coeff$value,
                      error_value = errors$value, 
                      t_ratio=coeff$value/errors$value)
  testing$type<-c("conventional", "hc0", "hc1", "hc2", "hc3")

  ## TRUE = rejection
  testing$rejection<-ifelse(testing$t_ratio>1.96 | testing$t_ratio<(-1.96),T,F)
  testing_summary<-testing %>% group_by(type) %>%
    summarise(avg_rejection = mean(rejection))
  
  print(testing_summary)
 
}

set.seed(12345)
sim_errors(n_1=3, sigma=0.5)
```
    

3. Repeat the preceding for $n_1 = 3$ and $\sigma = 0.85$.

```{r}
sim_errors(n_1=3, sigma=0.85)
```


4. Repeat the preceding for $n_1 = 3$ and $\sigma = 1$.

```{r}
sim_errors(n_1=3, sigma=1)
```

5. Briefly discuss your results from the preceding three parts.

I would say that there are two key messages based on our distribution. All our standard errors tend to overreject the null hypothesis. This is probably driven by the particular form of our heteroskedasticity and the small sample size in each of the repetitions. Moreover, we observe that the closer we get to homoskedasticity the closer our conventional rate gets to predicted rejection ratio of 5%.
