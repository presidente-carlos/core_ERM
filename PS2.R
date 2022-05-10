---
  title: 'Core ERM Problem Set #2'
author: "2937141"
date: "Due at Noon on Thursday, May 12th"
output: pdf_document
fontsize: 12pt
---
  
  
  ## Problem \# 1 - College Football Rankings and Market Efficiency
  
  This question is based on the paper "College Football Rankings and Market Efficiency" by Ray Fair and John F.\ Oster (*Journal of Sports Economics*, Vol. 8 No. 1, February 2007, pp. 3-18) and the related discussion in Chapter 10 of *Predicting Presidential Elections and Other Things* by Ray Fair. The data used in this exercise are courtesy of Professor Fair. You can download a copy from the data directory of my website as follows:
  
  ```{r, message = FALSE}
#tinytex::install_tinytex() ## To be able to knit my document

rm(list=ls())
library(tidyverse)
football <- read_csv('https://ditraglia.com/data/fair_football.csv')
#head(football)
```
Each row of the tibble `football` contains information on a single division I-A American college football game. All of these games were played in 1998, 1999, 2000, or 2001. We have ten weeks of data for each year, beginning in week 6 of the college football season.

### Outcome Variable: `SPREAD`
Our goal is to predict `SPREAD`, the *point spread* in a given football game.
This variable is constructed as follows.
For each game, one of the two teams is *arbitrarily* designated "Team A" and the other "Team B."
The point spread is defined as A's final score minus B's final score.
For example, in the first row of `football` the value of `SPREAD` is 34.
This means that team A scored 34 more points than team B.
Again, the designations of A and B are *completely arbitrary*, so `SPREAD` can be positive or negative.
The value of `-2` for `SPREAD` in row 6 indicates that the team designated A in that game scored two points *fewer* than team designated B.

### Predictor Variables

#### Home Field Indicator: `H`
The predictor `H` is a categorical variable that equals `1` if team A was the home team, `-1` if team B was the home team, and `0` if neither was the home team as in, e.g. the Rose Bowl.


#### Computer Ranking Systems: `(MAT, SAG, BIL, COL, MAS, DUN)`

Our next set of predictors is constructed from the following computer ranking systems:
  
  1. Matthews/Scripps Howard (MAT)
2. Jeff Sagarin's *USA Today* (SAG)
3. Richard Billingsley (BIL)
4. *Atlanta Journal-Constitution* Colley Matrix (COL)
5. Kenneth Massey (MAS)
6. Dunkel (DUN)

Fair and Oster (2007) describe these as follows:

> Each week during a college football season, there are many rankings of the Division I-A teams. Some rankings are based on the votes of sports writers, and some are based on computer algorithms ...  The algorithms are generally fairly complicated, and there is no easy way to summarize their main differences. 

The predictors `MAT, SAG, BIL, COL, MAS` and `DUN` are constructed as the *difference* of rankings for team A minus team B in the week when the corresponding game is scheduled to occur.
Suppose, for example, that in a week when Stanford is schedule to play UCLA, Richard Billingsley has Stanford \#10 and UCLA \#22.
The *difference* of ranks is 12.
So if Stanford is team A, `BIL` will equal `12` and if Stanford is team B, `BIL` will equal `-12`.
To be clear, each of these predictors will be *positive* when the team designated A is *more highly ranked*.

#### Win-Loss Record: `REC`

Continuing their discussion of computer ranking systems, Fair and Oster (2007) write:

> Each system more or less starts with a team's win-loss record and makes adjustments from there. An interesting system to use as a basis of comparison is one in which only win-loss records are used ... denoted `REC`.

The predictor `REC` is constructed differently from `MAT, SAG, BIL, COL, MAS` and `DUN`.
This predictor equals the difference in *percentage of games won* for team A minus team B.
For example, returning to the Stanford versus UCLA example, suppose that Stanford has won 80\% of its games thus far while UCLA has won 50\%.
Then `REC` will equal `30` if Stanford is team A and `-30` if Stanford is team B.

#### Las Vegas Point Spread: `LV`
Our final predictor is `LV`: the Las Vegas line point spread.
[ESPN](http://www.espn.com/college-football/lines) defines a point spread as follows:
  
  > Also known as the line or spread, it [a point spread] is a number chosen by Las Vegas and overseas oddsmakers that will encourage an equal number of people to wager on the underdog as on the favorite. If fans believe that Team A is two touchdowns better than Team B, they may bet them as 14-point favorites. In a point spread, the negative value (-14) indicates the favorite and the positive value (+14) indicates the underdog. Betting a -14 favorite means the team must win by at least 15 points to cover the point spread. The +14 underdog team can lose by 13 points and still cover the spread. 

For example, the value of 24 for `LV` row 1 of `football` indicates that fans believe team A is 24 points better than team B.
The fact that a point spread is an *equilibrium value* chosen to balance the quantity of bets for and against a given team has some important economic implications that we will explore below.

### Exercises
1. After loading the data from my website, calculate the *home field advantage*. How often does the home team win? How many more points, on average, does the home team score?
  
  ```{r}
## There exist two approaches to the current question
## Approach number one will simply be based on counting the number of victories
# of the home team, conditioned on the home team being team A
## This could be implemented as follows

football %>% filter(H==1)%>% summarize(perc_win = mean(SPREAD>0)*100)

## However, note that even when Team A is playing away, we can learn if the home
## team won that match as well. We simply need to redefine the spread of the game

football %>% filter(H!=0)%>% mutate(SPREAD=SPREAD*H) %>%
  summarise(perc_win =mean(SPREAD>0)*100)

##We can implement both approaches to identify the average point difference
## of the home team

football %>% filter(H==1)%>% summarize(avg_points = mean(SPREAD))
football %>% filter(H!=0)%>% mutate(SPREAD=SPREAD*H) %>%
  summarise(avg_points = mean(SPREAD))


```

2. Run a linear regression *without an intercept* that uses `H` to predict `SPREAD`. Interpret the coefficient estimates, carry out appropriate inference, and summarize the model fit. Why *doesn't* it make sense to include an intercept in this regression, or indeed in *any* regression predicting `SPREAD`?

By recoding our variables as factors we gain some interpretability. In particular, we recover the average score for each of the three categories (`Away`, `Neutral`, `Home`). More concretely, we observe that when teams play home they score 15 points more on average that the away team. This effect is very statistically significant. On the other hand, playing away has a small (non-significant) effect on spread (meaning, on average, teams playing away have a small negative spread). Finally, teams playing in a neutral field have a small (non-significant) positive spread.

This result is surprising in the following sense. A priori, considering that team allocation (A vs B) was "random", we would expect (i) some balance in the number of away games vs home games and (ii) we would expect a symmetric behavior in the spread between home and away games. However, while playing home teams score +14 points, playing away teams score just -1 points on average. Number of home vs away homes is also very different across `H`. Further investigation will be required in this matter.

Both the finite sample (F-stat) and the asymptotic (Chi-squared) tests suggest that the home variable is a statistically significant predictor of spread. The adjusted R^2 is of 0.1312, which is small but has a considerable size if we take into account that we are just including one variable in our regression.

Finally regarding the intercept, note that given the random allocation of teams A and B, without including any covariate, when regressing `SPREAD` on a constant (i.e. equivalent to taking the mean), we expect an average zero spread. This is, by construction, we expect our "true model" to go through `SPREAD` = 0. As a result, it makes little sense to incorporate an intercept which may violate this consideration. 

```{r}
library(car) ##Used to conduct finite and asymptotic testing on our hypothesis

## Note that `H` is not a binary variable (as neutral games are also a possibility)
## We can then opt for excluding `H`=0 matches or recoding `H` as a categorical
## variable. We opt for the latter

football_new<-football %>%
            mutate(H=factor(as.character(H))) 
levels(football_new$H)<-c("Away", "Neutral", "Home") #Order (-1, 0, 1)

reg_home<-football_new %>% lm(SPREAD~H-1,.)
summary(reg_home)

# Finite Sample (F-statistic based)
linearHypothesis(reg_home, c("HAway=0", "HNeutral=0", "HHome=0"))

# Asymptotic coverage (Chi-squared based)
linearHypothesis(reg_home, test = 'Chisq',
                 c("HAway=0", "HNeutral=0", "HHome=0"))

## Goodness of fit
summary(reg_home)$r.squared
summary(reg_home)$adj.r.squared

## Extra comments (unbalancedness of away vs home)
football %>% 
  group_by(H) %>%
  summarise(no_rows = length(H))

## In the rest of the exercises though, given that we won't be interested anymore in 
## the interpretability of the `H`coefficient, for simplicity, we will simply include `H` as
## a numeric regressor


```


3. The R package `GGally` has a handy function called `ggpairs()` that can be used to make a very attractive and informative data visualization called a *pairs plot*. Install `GGally` and through a combination of reading the help file for `ggpairs()` and searching the internet, find out how to make such a plot and what information it contains. Then use `ggpairs()` to produce a pairs plot of the columns `MAT, SAG, BIL, COL, MAS, DUN,` and `REC`. Briefly discuss your results.

Discuss here

```{r}
#install.packages("GGally")
library(GGally)

football %>% ggpairs(columns = 3:9)

```

4. Run a regression *without an intercept* using `H`, `REC` and the six computer ranking systems (`MAT, SAG, BIL, COL, MAS,` and `DUN`) to predict `SPREAD`. Do all of the ranking systems add additional predictive information beyond that contained in `H` and the other ranking systems? Carry out appropriate statistical inference to make this determination. If, based on your results, some predictors appear to be redundant, re-estimate your regression dropping them. Based on your results, is it possible to make better predictions of college football games than the *best* of the seven computer systems?
  
  I do not fully understand what is meant by "all the ranking systems add additional predictive information" (as even if some of the regressors might not be significant towards predicting `SPREAD` they may reduce the variance of the other coefficients, what makes inference more efficient).

However, from a strict significance point of view, we observe how some of the computer ranking systems are not significant under standard confidence levels (95%). In particular only REC, SAG, BIL and DUN are significant at this confidence level. Moreover note that the computer-predictors as a whole have some predicting power.

It is not clear from the context how "prediction" is defined. In particular, we have not established a training and a testing data set, thus standard in-sample SSR minimization could suffer from overfitting. Other "prediction statistics" could also be taken into account like absolute residuals among others. Here we just use SSR for simplicity. (Note: We consider `REC`to also be a "computer predictor, what is not very clear from the exercise neither.)

```{r}
## Base regression
reg_all<-football %>% lm(SPREAD ~ H + REC + MAT + SAG + BIL + COL + MAS + DUN -1, .)
summary(reg_all)

## Extract significant variables
sign_vars<- summary(reg_all)$coefficients[ ,4] < 0.05
sign_vars

## F-test
linearHypothesis(reg_all, c("REC=0", "MAT=0", "SAG=0", "BIL=0", "COL=0",
                            "MAS=0", "DUN=0" ))

## Only-significant covariates regression

reg_few<-football %>% lm(SPREAD ~ H + REC + SAG + BIL + DUN -1, .)
summary(reg_few)

## Identifying the best predictor

## We create a data frame to store our results
computer_pred<-c("REC", 'MAT', "SAG", "BIL", "COL", "MAS", "DUN")
SSR<-data.frame(predictor=computer_pred, ssr = NA)

## Initiate vector to store results
j<-1

## Our strategy is based on running individual regressions on each of the predictors
## and compute the SSR for each of them
for (i in 3:9){
  SSR[j,]$ssr<-sum(residuals(lm(football$SPREAD ~ football[[i]]))^2)
  j<-j+1
}

##"Best" predictor
SSR[SSR$ssr==min(SSR$ssr),]$predictor ## `MAT` is the "best" predictor

## Can we do better by combining them?
SSR[SSR$ssr==min(SSR$ssr),]$ssr
sum(residuals(reg_all)^2) ## Yes, we can do better by combining them
## (at least in terms of in-sample SSR)
sum(residuals(reg_few)^2)


```


5. Run a regression *without an intercept* that predicts `SPREAD` using `LV`, `H` and whichever of the seven ranking systems you found to contain independent information in part 4 above. Does `H` or any of the ranking systems contain additional predictive information beyond that contained in `LV`? Carry out appropriate statistical inference to make this determination.

```{r}
reg_lv<-lm(SPREAD ~ LV + H + REC + SAG + BIL + DUN -1, data = football)
summary(reg_lv)

linearHypothesis(reg_lv, c("H=0", "REC=0", "SAG=0", "BIL=0", "DUN=0"))
## We cannot reject the null, this is, all the coefficients could be zero
## when accounting for LV, this suggests that none of the additional predictors
## contain additional information beyond the one contained in `LV`
```


6. What do your findings from part 5 above have to do with the concept of market efficiency?
   If betting markets are efficient, what should be the slope in a regression that uses `LV` *alone* to predict `SPREAD`? Can you statistically reject these values for the regression coefficients? How accurately does `LV` alone predict `SPREAD`?
   
The above result not only suggests the existence of market efficiency but also perfect information in the market. This is, considering that the Vegas Line (either the market creators or the players themselves) perfectly identify the expected (average) spread, winning the bid becomes a "quasi-random process". Considering that there is no more information available to the players, if players were to bid on one less (more) spread point, under expected utility considerations, players would be better off my bidding on one side of the `LV` destroying the market (as no maximizing expected utility agent with the same information would like to bid against). In this context, market existence is a symptom of market efficiency.     
   
```{r}
## We expect then a one-to-one relation between LV and SPREAD if the LV 
## market were to be efficient
reg_lv_only <- lm(SPREAD ~ LV -1, data = football)
summary(reg_lv_only)

## We cannot reject the null, this is, we cannot reject that the LV coefficient = 1
## under standard confidence levels (and asymptotic considerations)
linearHypothesis(reg_lv_only, "LV=1")
```

   
7. Produce a single nicely-formatted table summarizing the results of all the regressions that you ran in this question.

```{r}
library(modelsummary)
regressions<-list("Home only" = reg_home, 
                  "All comp pred" = reg_all, 
                  "Sign comp pred" = reg_few,
                  "LV and others" = reg_lv,
                  "LV only" = reg_lv_only)
modelsummary(regressions, gof_omit = 'Log.Lik|R2 Adj.|AIC|BIC')
```


## Question \#2 - Sieving Prime Numbers

A *prime number* is any counting number greater than one that cannot be written as a product of two smaller counting numbers. For example: 2 is prime but 4 is not because $4 = 2\times 2$. Any number that is not prime is called *composite*. There are many computational procedures for finding prime numbers. One of the oldest and simplest is called the *Sieve of Eratosthenes*. Here's how this method finds all prime numbers less than or equal to `n`:

(a) We already know that `1` isn't a prime number, so start off with a vector of integers from `2` to `n`. 
(b) Set `p` equal to `2`, the smallest prime number.
(c) Mark all the *multiples* of `p`, namely `2 * p, 3 * p, ...`, as `NOT PRIME`.
(d) Find the smallest number that is both *greater* than `p` and *hasn't* been marked `NOT PRIME`. If there is no such number, go to the next step. If there is such a number, this is your new `p`. Use it to repeat step (c). 
(e) Congratulations: you're finished! All the numbers in your list that *aren't* market `NOT PRIME` are prime.

Here's a fully-worked example. Suppose we want to find all the prime numbers between `2` and `13`. Start with the full list.

```
2 3 4 5 6 7 8 9 10 11 12 13
```
Now *remove* all the multiples of two: 
```
2 3 _ 5 6 7 _ 9 _ 11 _ 13
```
The smallest number that is bigger than `2` but isn't crossed out is `3`, so now we cross out all multiples of `3`:
```
2 3 _ 5 _ 7 _ _ _ 11 _ 13
```
The smallest number that is bigger than `3` but isn't crossed out is `5`. Now we'd cross out any multiples of `5`, but none are left. The smallest number that is greater than `5` but hasn't been crossed out is `7`. Again, there aren't any multiples of `7`. The same goes for `11` and same goes for `13`. Once we reach `13` there are no longer any values larger than `p` in our list that haven't been crossed out. So we're done!

Sieving primes by hand isn't much fun, so in this problem you'll write R code to automate the task:

### Exercises
1. Write R code to implement the Sieve of Eratosthenes.
2. Using your code from part 1, print out all the prime numbers between 1 and 100. You should obtain the following result:
```
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
```

```{r}
rm(list=ls())
sieve<-function(primes){
  
  ## Note for User: `primes` must be a numeric vector
  
  ## OPTIONAL ##
  ## Not explicitly asked by the problem, but it generalizes
  ## the applicability of our code
  
  ## Completing the vector
  complete_vector<-seq(1:max(primes))
  complete_vector<-complete_vector[!complete_vector %in% primes]
  primes<-append(primes, complete_vector)
  
  ## Manually extract 1 from primes. Note that 1 will always be in primes given
  ## our completion mechanism
  primes<-primes[primes!=1]
  
  ## Define initial p
  p<-2
  
  ## Initiate p_list (to store all used p)
  p_list<-c()
  
  while(p!=Inf){
    p_list<-append(p_list,p)
    
    ##Remove multiples of p
    primes<-primes[primes%%p!=0]
    
    ## We reappend p to the list given that p is a multiple of p
    primes<-append(primes,p) 
    
    ##Identify p
    p<-suppressWarnings(min(primes[!primes %in% p_list]))
    
    ## When the minimum is not identified in an object, this is equaled to `Inf`
    ## and a warning message is thrown. We use this fact to create a while loop.
    ## Finally, we prevent R from throwing a Warning message
  }
  
  ## Remove the extra elements which we added for completion and print
  primes[!primes %in% complete_vector]
  
}

sieve(1:100)

## Some further comments on my program
## By "completing" the vector, my program is not only able to compute 
## the prime numbers from complete series starting in 1,
## but to any series, even if this sequence is "incomplete"
## My program is also able to cope with duplicates (if any)

sieve(26:145)
sieve(c(2,2,5,7,45,68,97,33,243,54))

```


## Problem \#3 - Class Size and Test Scores

The file `final5.dta` from the [Angrist data archive](https://economics.mit.edu/faculty/angrist/data1/data/anglavy99) contains data from the article "Using Maimonides Rule to estimate the Effect of Class Size on Student Achievement" by Angrist & Lavy, published in the *Quarterly Journal of Economics*, May 1999. This article uses the fact that Israeli class sizes are capped at 40 to estimate the effects of class size on test scores. We have not yet studied the regression discontinuity methods used in the paper, so in this problem we'll examine the dataset using linear regression. In class we've exclusively discussed linear regression as tool for *prediction*, but in this problem we'll begin to wade into the murky waters of causal inference based on observational data. The file `final5.dta` contains data for 5th grade classes:

| Name | Description |
| ---- | ----------- |
| `c_size` | September grade enrollment at the school |
| `classize` | class size: number of students in class in the spring |
| `tipuach` | percent of students in the school from disadvantaged backgrounds |
| `avgverb` | average composite reading score in the class |
| `avgmath` | average composite math score in the class |
| `mathsize` | number of students who took the math test |
| `verbsize` | number of students who took the reading test |

### Notes and Hints
In part 1 below you will load and clean the data. While I list the data cleaning steps as separate items, you can do all of them in a single `dplyr` pipeline. This will be much easier to read. I suggest combining `mutate()` with the base R function `replace()` to handle the erroneous observations. Several of the remaining parts will ask you to run various linear regressions. To make marking easier, please display all of your regression results in a *single table* with appropriately labeled columns rather than in a series of separate tables. You can create such a table using `modelsummary()`. Please use robust standard errors throughout.

### Exercises
1. Load and clean the dataset: 
      (a) Download the file `final5.dta` from the Angrist Data Archive and store a copy in your project directory. 
      (b) Try to read `final5.dta` into a tibble called `final5` using the function `read_dta()` from the package `haven`. **You will get an error message!** This file was created with an old version of STATA that does funny things with text encoding. To fix the error, consult the section "Character encoding" in the R help file for `read_dta()` and follow the instructions give there.
      (c) Rename the column `c_size` to `enroll`, and `tipuach` to `pdis`. 
      (d) Restrict `final5` so that it contains only observations for schools with 5th grade enrollment of at least 5 students, and classrooms with fewer than 45 students. 
      (e) Select only the columns we will use later in the analysis: `classize`, `enroll`, `pdis`, `verbsize`, `mathsize`, `avgverb`, and `avgmath`.
      (f) There was a data entry error for one value of `avgmath`: `181.246` should be `81.246` since the test score is out of `100`. Correct this.
      (g) There was a data entry error for one value of `avgread`: `187.606` should be `87.606` since the test score is out of `100`. Correct this.
      (h) There is a classroom with `mathsize` equal to zero, i.e. no students in this class took the math test, which has a *non-missing* value for `avgmath`. This is an error: since no one in this class took the test, there is no average math score for this class.
      (i) Replace all values of `avgmath` for classes with `mathsize` equal to zero with `NA`.
      
```{r}
rm(list = ls())
library(haven)

## I have tried to directly download the dataset from the website using the following line of code
## final5<-
## read_dta("https://economics.mit.edu/faculty/angrist/data1/data/anglavy99/final5.dta")
## However, there seems to be some certification issue with the server of this page,
## thus I had to manually download the dataset and load it from within
## the project files

final5<- read_dta("dta/final5.dta", encoding= "latin1") ##Because pre-Stata14 dta
 
final5<- final5 %>% rename("enroll"=c_size, "pdis" = tipuach) %>%
         filter(grade==5, enroll>=5, classize<45) %>%
         select(classize, enroll, pdis, verbsize,
         mathsize, avgverb, avgmath) %>%
         mutate(avgmath = 
                ifelse(round(avgmath,3) == 181.246 , 81.246, avgmath)) %>%
         mutate(avgverb =
                ifelse(round(avgverb,3) == 187.606 , 87.606, avgverb)) %>%
         mutate(avgmath =
                ifelse(mathsize==0, NA, avgmath))

## Some notes on my code:
## a) The actual name of the variable avgread is avgverb
## b) I have used the function ifelse, rather than replace, which I found
## a bit complicated to implement within the pipe
## c) There are many decimal places within the variables avgmath and avgread.
## In fact, my computer had lots of problems to identify identities
## in this context. To overcome this problem I have rounded the relevant
## variables to identify the identities (without changing the actual
## value of vars)

```
      

2. Create a table of descriptive statistics:
      (a) Download the Angrist & Lavy paper and consult Table I on page 539.
      (b) Use `datasummary_skim()` to make a "fast and dirty" table of summary statistics for `final5`.
      (c) Consult <https://vincentarelbundock.github.io/modelsummary/> to learn more about the various customization options available in the `modelsummary` package. Then try to construct a table that approximates panel A of Table I as well as you can. It doesn't have to be perfect, but the idea is to get closer than the output of `datasummary_skim()` from above. (If you want to be a real perfectionist about this combining `modelsummary` with the `gt` package allows a tremendous amount of customization.)
      
```{r}
library(modelsummary)
datasummary_skim(final5)


##Creating custom variables for display
P10<-function(x){
  round(quantile(x,0.10, na.rm=T, names=F),0)
}
P90<-function(x){
  round(quantile(x,0.90, na.rm=T, names=F),0)
}

## Data summary just works with data.frames
final5<-as.data.frame(final5)

datasummary( (`Class size`=classize) + (`Enrollment`=enroll) +
            (`Percentage disadvantaged` = pdis) + (`Reading size`=verbsize) +
            (`Math Size`=mathsize)+ (`Average Verbal` = avgverb) +
            (`Average Math`=avgmath) ~ 
            (Mean + (`S.D.`=SD))*Arguments(fmt = "%.1f") +
            (`0.10`= P10) + (`0.25`=P25) + (`0.50`=P50) +
               (`0.75`=P75) + (`0.90`=P90),
            data = final5, fmt = "%.0f",
            title = 'TABLE I UNWEIGHTED DESCRIPTIVE STATISTICS',
            align = 'lccccccc',
            notes = "A. Full sample 5th grade (2019 classes, 1002 schools,
                                               tested in 1991")


```
      

3. Regress achievement on class size:
      (a) Carry out a regression predicting average verbal scores from class size. 
      (b) Repeat part (a) but predict average math test scores.
      (c) Discuss your results from (a) and (b). If smaller classes improve student achievement, what sign should the coefficient estimates from your regression have? What kind of relationship do you find? Is it large enough to be of practical importance? Statistically significant?
      
A priori we would expect a negative effect of class size on student performance (based on the underlying mechanism that smaller classes should allow for better education quality and student involvement). However, we observe a significant (small) positive effect of class size on grades.

As in whether this effect is "important" it feels to me as a subjective question. It could be argued that a student in a 20 kids class is expected to score more than six points than a student in a 40 people class (see table below). It is for the policy designer to judge whether this effect is of practical importance or not.
      
```{r}
library(estimatr)
reg_verbal<- final5 %>% lm_robust( avgverb ~ classize , .)
reg_math<- final5 %>% lm_robust( avgmath ~ classize , .)


```
      

4. Control for school size:
      (a) A possible explanation for your findings in question 3 is that larger schools have larger classes *and* better students. Repeat question 3 but add `enrollment`, which measures the size of the 5th grade at the school, to your regressions. How do the results change? Combine the results for all four regressions into a single table to make it easier to compare them.
      
When accounting for school enrollment, we observe a substantial decrease of the impact on class size on students' performance. However, these coefficients remain positive in the two extra especifications.      
      
```{r}
reg_verbal_enroll<- final5 %>% lm_robust( avgverb ~ classize + enroll , .)
reg_math_enroll<- final5 %>% lm_robust( avgmath ~ classize + enroll, .)

regressions <- list('Avg Verbal' = reg_verbal,
                    'Avg Math' = reg_math,
                    'Avg Verbal' = reg_verbal_enroll,
                    'Avg Math' = reg_math_enroll)

modelsummary(regressions, gof_omit = 'Log.Lik|R2 Adj.|AIC|BIC')

```
      

5. Use the base R function `cor()` to construct the correlation matrix of math test scores, class size, and enrollment. If you need help to understand how this function works, consult the help file and/or search the internet. Use this matrix and your regression results to explain why and how the coefficient on class size changes when you control for enrollment.

Although this analysis is very superficial, we can observe that indeed exists a positive correlation between school enrollment and class size. Moreover, the impact of enrollment and class size on math scores seems to be very similar. These intuitions may suggest that part of the correlation that we observe between class and math scores could be driven by a correlation between school enrollment and class size. Our regression results in the previous part, suggest that this intuition could be correct.

```{r}
## I have added a extra piece in the code, given that cor()
## cannot work with missing values
final5%>% select(avgmath, classize, enroll) %>% filter(!is.na(avgmath)) %>% cor() 

```


6. Control for percent disadvantaged:
      (a) Repeat question 4 but add the percent of students who came from disadvantaged backgrounds `pdis` in place of enrollment. How does this affect the results?
      (b) Calculate the correlation matrix for math test scores, class size, and `pdis`. Using this information along with the correlation matrix from question 5 above and your regression results, why does controlling for `pdis` have a larger effect on the estimated coefficient for class size than controlling for `enroll`? 

After controlling for `pdis` our coefficients for class size finally become non-significant (and the one for verbal actually becomes negative), what gets us closer to the expected result.  Controlling for pdis may have a larger impact on class size, because pdis may be correcting for many unobservable covariates that enrollment could not really correct for. For instance, we may expect lower health outcomes of low income students. As a result, controlling for pdis might work as a good predictor of income level of students. This factor, which could be of ultimate importance when predicting student performance might not suitably be detected when simply correcting for enrollment.

Our intuitions might be correct based on the information contained in the correlation matrix, as pdis seems to have stronger correlation with student performance than enrollment did.

```{r}
reg_verbal_pdis<- final5 %>% lm_robust( avgverb ~ classize + pdis , .)
reg_math_pdis<- final5 %>% lm_robust( avgmath ~ classize + pdis, .)

regressions <- list('Avg Verbal' = reg_verbal,
                    'Avg Math' = reg_math,
                    'Avg Verbal' = reg_verbal_pdis,
                    'Avg Math' = reg_math_pdis)

modelsummary(regressions, gof_omit = 'Log.Lik|R2 Adj.|AIC|BIC')

final5%>% select(avgmath, classize, pdis) %>% filter(!is.na(avgmath)) %>% cor() 
```


7. Regress math and verbal test scores on class size controlling for *both* `pdis` and `enroll`. Discuss your results in light of questions 4-6 above. All told, do your results for this dataset suggest that smaller classes are good, bad, or neutral?

When combining both additional regressors, our results change very little. In particular, enrollment seems to have a non-significant predictor power once class size and pdis are factored in. As a result, our coefficients for class size change very little when we introduce enrollment compared to the `pdis` only regression.

Overall, and although this analysis is very limited, it seems that there is no effect of class size on students' maths and verbal test scores.

```{r}
reg_verbal_enroll_pdis<- final5 %>% lm_robust( avgverb ~ classize + enroll + pdis , .)
reg_math_enroll_pdis<- final5 %>% lm_robust( avgmath ~ classize + enroll + pdis, .)

regressions <- list('Avg Verbal' = reg_verbal,
                    'Avg Math' = reg_math,
                    'Avg Verbal' = reg_verbal_enroll,
                    'Avg Math' = reg_math_enroll,
                    'Avg Verbal' = reg_verbal_pdis,                    
                    'Avg Math' = reg_math_pdis,
                    'Avg Math' = reg_verbal_enroll_pdis,
                    'Avg Verbal' = reg_math_enroll_pdis)

library(kableExtra) ## To fit the table in the page
modelsummary(regressions, gof_omit = 'Log.Lik|R2 Adj.|AIC|BIC') %>%
  kable_styling(latex_options="scale_down")

## We cannot reject the null hypothesis
linearHypothesis(reg_verbal_enroll_pdis, "classize=0")
linearHypothesis(reg_math_enroll_pdis, "classize=0")

```

