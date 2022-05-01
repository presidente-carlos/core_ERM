---
title: 'Core ERM Problem Set #1'
author: "1477940"
date: "Due at Noon on Thursday, May 5th"
output: pdf_document
fontsize: 12pt
---


## Instructions (delete before submitting)

This problem set is due at *noon on Thursday, May 5th*. You should submit a *single pdf file* to Inspera, knitted from the underlying `.Rmd` file. If you want feedback, you should additionally submit the same file to canvas. See `course-information.md` for details. Add your bod card number in `author` field above and your solutions to the document below. Please **do not write your name on this assignment.** 

There are three problems on this problem set. You goal should be to complete all of them, getting help as needed at the TA office hours on Monday and Tuesday. It's fine to collaborate with your classmates on problem sets, provided that you don't copy their solutions. The material you submit should be work that you carried out yourself, even if you needed some help to make it across the finish line. If Thursday morning rolls around and you haven't finished everything: don't panic; submit what you have. You do not necessarily have to complete every part of every problem to pass the assignment. 


## Problem \# 1 - Making Change
Do you remember physical money? In olden times I used to collect small change in a jar and periodically sort through it so I could do my laundry. I always seemed to have too many of the coins I *didn't* need, and to few of the ones I *did* need! This problem is about making change; it's also about making *functions*. To answer it you'll need two pieces of outside information. First: US coinage consists of pennies (\$0.01), nickels (\$0.05), dimes (\$0.10), quarters (\$0.25), and dollar coins (\$1.00). Second: UK coinage consists of pence, 2p coins, 5p coins, 10p coins, 20p coins, 50p coins, 1 pound coins, and 2 pound coins. 

You'll also need to use the R operators `%/%` and `%%`. These represent *integer division* and *modulo*. For example `16` divided by `3` equals `5` with a remainder of `1` so `16 %/% 3` returns `5` while `16 %% 3` returns `1`. Before beginning this exercise, experiment with `%/%` and `%%` to make sure you understand how they work. If necessary, search the internet to find out more about them or read the R help file "Arithmetic Operators." 

For technical reasons that I won't delve into here, the simplest reliable way of representing monetary values in a computer is by using *whole numbers* rather than decimals. Rather than representing fourteen pounds and thirty-two pence as `14.32`, for example, we'd store this value as `1432`. In short, we store the number of *pence* (or cents) as an *integer*. Please follow this convention throughout. 

1. The following code block outlines a simple algorithm for making change. The customer is owed a certain number of `cents`, an integer between `1` and `99`, and your task is to calculate how many `quarters`, `dimes`, `nickels`, and `pennies` to remove from the cash register. The rule is to always give as many *high denomination* coins as you can before moving on to lower denomination coins. If the customer is owed \$0.75, for example, you remove three quarters rather than seven dimes and a nickel. Fill in the gaps in the code, run it, and display the resulting vector `change`. Check the result by hand to make sure your logic is correct.

```{r}
rm(list=ls())
cents <- 73
quarters <- cents %/% 25
cents <- cents %% 25

dimes <- cents %/% 10
cents <- cents %% 10  

nickels <- cents %/% 5
cents <- cents %% 5 

change <- c('quarters' = quarters,
            'dimes' = dimes,
            'nickels' = nickels,
            'pennies' = cents)

change
```

2. You've written some handy code! But now suppose you wanted to reuse it to make change for a different amount, say \$0.37 or \$0.19. Copying-and-pasting your existing code is tedious and error prone. Instead of doing this, write a *function* called `make_change_US()`. It should take a single input argument `cents`, a integer between `1` and `99`, and return a vector called `change` listing the number of quarters, dimes, nickels, and pennies formatted as above. Run your function to make change for \$0.37 and \$0.19 and check the results against your hand calculations.

```{r}

##Create function
make_change_US<-function(cents){
  
  quarters <- cents %/% 25
  cents <- cents %% 25
  
  dimes <- cents %/% 10
  cents <- cents %% 10  
  
  nickels <- cents %/% 5
  cents <- cents %% 5 
  
  change <- c('quarters' = quarters,
              'dimes' = dimes,
              'nickels' = nickels,
              'pennies' = cents)

change

}

## Check function for particular values
make_change_US(37)
make_change_US(19)
```


3. The function `make_change_US()` has some limitations. Suppose we wanted to make change for *UK* currency rather than US currency. This would require us to write a completely new function despite the underlying logic of the problem being identical. There's a better way forward. Write a new function called `make_change()` that it takes *two* input arguments. The first argument is `cents`. As before, this is a integer between `1` and `99`. The second argument is new: `currency` is a named vector that stores the denominations between `1` and `99` cents along with their labels. For example we would set `currency` equal to `c('quarter' = 25, 'dime' = 10, 'nickel' = 5, 'penny' = 1)` for US currency and `c('50p' = 50, '20p' = 20, '10p' = 10, '5p' = 5, '2p' = 2, '1p' = 1)` for UK currency. As above, `make_change()` should return a named vector called `change` indicating how many of each coin to give as change, but now this vector should take its names from `currency`. The logic is the same as above, but the implementation is a bit trickier. While there are various ways to solve this, the simplest is probably to use a `for` loop. Test `make_change()` by making change for 78 cents/pence in US/UK currency. Compare your results to `make_change_US(78)` to double-check.

```{r}

##Create function

make_change<-function(cents, currency){
  
  ##Create an empty vector to store results
  change<-c()
  
  ##Compute change using for loop
  for (i in 1:length(currency)){
    change[i]<-cents%/%currency[i]
    cents<-cents%%currency[i]
    
  }
  
  ##Reassign names into our empty vector for pretty output
  names(change)<-names(currency)
  change
  
}

make_change(78, c('quarter' = 25, 'dime' = 10, 'nickel' = 5, 'penny' = 1))
make_change(78, c('50p' = 50, '20p' = 20, '10p' = 10, '5p' = 5, '2p' = 2, '1p' = 1))

```


## Problem \# 2 - Racial Bias in the Labor Market
In this question you'll partially replicate a well-known paper on racial bias in the labor market: ["Are Emily and Greg More Employable Than Lakisha and Jamal? A Field Experiment on Labor Market Discrimination"](https://www.aeaweb.org/articles?id=10.1257/0002828042002561) by Marianne Bertrand and Sendhil Mullainathan. The paper, which I'll refer to as BM for short, appears in Volume 94, Issue \#4 of the *American Economic Review*. Before reading further, please download a copy of the paper. 

For convenience, I've posted a copy of the dataset from this paper on my website at `https://ditraglia.com/data/lakisha_aer.dta`. Each row of the dataset corresponds to a single fictitious job applicant. As its `.dta` extension indicates, this is a Stata file. To open it in R we'll use the function `read_dta()` from `haven`. After installing `haven`, you can run the following two lines of code to read the data into a tibble called `bm`:

```{r}

##Install packages
#install.packages("haven")

##Read dta
library(haven)
bm <- read_dta('https://ditraglia.com/data/lakisha_aer.dta')
```

1. Read the introduction and conclusion of BM. Then write a short paragraph answering the following: 
   (a) What research question do BM try to answer?
   
   They study the existence of racism in job adds call-back rates as a proxy of labor market discrimination agains the African-American community in the United States. In addition, they explore the heterogeneity of racial discrimination across resume quality, neighborhood, sector and others.
   
   (b) What data and methodology do they use to address the question?
   
   They create fictitious CVs which are randomly assigned a traditionally "African American" or "White" sounding names (as well as a randomly assigned neighborhood) and use them to reply to job adds. Finally, they analyse the call-back rates of these fictitious applicants.
   
   Data includes one entry per "applicant". Each row includes a detailed list of fictitious personal variables of the applicant as well as information on the advertised position and associated company. Data also includes the output (call-back or not) of the application process.
   
   (c) What do the authors consider to be their key findings?
   
   They discover that applicants with African American names are 50% less likely to receive a call-back (after controlling for resume quality). Moreover, while a good curriculum seems to substantially improve the call-back ratio for a white applicant, it does very little for a black one. In addition, applicants coming from more wealthy neighborhoods also experience an increase in call-back rates uniformly across race. They do not observe differences in discrimination across occupations nor even across "Equal Opportunity Employer" status of the contractor.
   
2. Now that you have a rough idea of what the paper is about, it's time to examine the dataset `bm`. Carry out the following steps:
   (a) Display the tibble `bm`. How many rows and columns does it have? 
   
   4870 rows and 65 columns (Code does not display the whole tibble as suggested for presentational purposes)
  
```{r}
dim(bm)
#bm
```
   
   (b) Display only the columns `sex`, `race` and `firstname` of `bm`. What information do these columns contain? How are `sex` and `race` encoded? 
   
   Column `sex` includes binary information (f/m) on the sex of the applicant, `race` includes binary information of the presumed racial identity of the applicant based on its name (black vs white) and `firstname` is a string variable containing the assigned first name of the applicant.
   
   Both, `sex`and `race` variables are stored as character variables (non-factor).
   
```{r}
#Displaying overall structure
str(bm[c("sex", "race", "firstname")])

#Unique values in sex and race
unique(bm$sex)
unique(bm$race)

#Checking type and class of the variables. (Note that this information is also contained within the str() display)
typeof(bm$sex)
typeof(bm$race)
class(bm$sex)
class(bm$race)

##Alternative code using dplyr syntax
library(dplyr)
bm %>% select(sex, race, firstname) %>% str()

```
   
   (c) Add two new columns to `bm`: `female` should take the value `TRUE` if `sex` is female, and `black` should take value `TRUE` if `race` is black.
   
```{r}
bm$female<-(bm$sex=="f")
bm$black<-(bm$race=="b")
```
   
   
3. Read parts A-D of section II in BM. Then write a short paragraph answering the following:
   (a) How did the experimenters create their bank of resumes for the experiment?
   
   They used as starting point real CVs from real job applicants. Then, they modified them using different techniques (i) to ensure the privacy of the original applicant, (ii) to clearly distinguish between high and low qualified resumes and (iii) to "approximately" fit a particular job description. Eventually, random names and addresses are assigned to each CV. Strongly racialized names were selected via census and interviews. Minor final adjustments including the assignment of an email were conducted to maximize the plausibility of the fictitious profile.
   
   (b) The experimenters classified the resumes into two groups. What were they and how did they make the classification?
   
   Resumes are classified into high and low quality resumes. This distinction is based on "labor market experience, career profile, existence of gaps in employment, and skills listed". This preliminary classification is reinforced by general extra elements and skills, and job-description specific extra elements. Minor final amends like the email assignment described above are added for plausability concerns.
   
   (c) How did the experimenters generate identities for their fictitious job applicants?
   
   As described in (a) names were build using census information. Researchers identify those names whose share of a particular race (black or white) was particularly salient. This preliminary selection was confirmed via questionnaires. Finally, assignment of names to curricula is done through a (quasi)random process.
   
4. Randomized controlled trials are all about *balance*: when the treatment is randomly assigned, the characteristics of the treatment and control groups will be the same on average. To answer the following parts you'll need a few additional pieces of information. First, the variable `computerskills` takes on the value `1` if a given resume says that the applicant has computer skills. Second, the variables `education` and `yearsexp` indicate level of education and years experience, while `ofjobs` indicates the number of previous jobs listed on the resume.
    (a) Is sex balanced across race? Use `dplyr` to answer this question. Hint: what happens if you apply the function `sum` to a vector of `TRUE` and `FALSE` values?
    Data seems very gender-balanced across race (77% of black applicants are female vs 76% of white applicants are female)
    
```{r}
bm %>% select(female, race) %>% group_by(race) %>% summarize(share_fem=mean(female)*100)
```
    (b) Are computer skills balanced across race?  Hint: the summary statistic you'll want to use is the *proportion* of individuals in each group with computer skills. If you have a vector of ones and zeros, there is a very easy way to compute this.
    One more time, data seems fairly balanced (just a 3% difference across races). We could also obtain the desired result by summing `computerskills' and then dividing by nrow().
```{r}
 bm %>% select(computerskills, race) %>% group_by(race) %>% summarize(share=mean(computerskills))
 
#Alternative piece of code using the sum command
 sum_CS<-bm %>% select(computerskills, race) %>% group_by(race) %>% summarize(n = n(), sum=sum(computerskills), share = sum/n)
 sum_CS

```
    (c) Are `education` and `ofjobs` balanced across race?
    One more time, both variables seem fairly balanced across race. See the ouput of the code for a detailed evaluation of the education variable.

```{r}
#First note that the following piece of code produces an unsatisfactory solution
   bm %>% select(education, race) %>% group_by(race) %>% summarize(share=mean(education))
#Because, education is in fact a categorical variable, we might want to know the share of people in each of the five categories across each race. To do so: 
str(bm$education)

##We first identify the number of people in each education level by race
sum_educ<-bm %>% select(education, race) %>% group_by(race) %>% count(education)

##We also count the number of people by race
sum_race<-bm %>% count(race)

##Reassign those values to the main data set `sum_educ`
for (i in c("b","w")){
sum_educ$race_n<- sum_race[sum_race$race==i,]$n
}

##Compute the share and rearrange for presentational purposes
sum_educ$share<-sum_educ$n/sum_educ$race_n
sum_educ %>% arrange(education, race)

##Means are however useful given that ofjobs is a truly numeric variable
 bm %>% select(ofjobs, race) %>% group_by(race) %>% summarize(share=mean(ofjobs))
```
    (d) Compute the mean and standard deviation of `yearsexp` by race. Comment on your findings.
    Once again data is fairly balanced across races. It is surprising though the very high standard deviation of the years of experience. This fact could be due to the artificial big gap generated by the researchers when creating high vs low quality resumes.
    
```{r}
bm %>% select(yearsexp, race) %>% group_by(race) %>% summarize(avg_yearsexp = mean(yearsexp), sd_yearsexp = sd(yearsexp))
```
    (e) Why do we care if `sex`, `education`, `ofjobs`, `computerskills`, and `yearsexp` are balanced across race?
    There are two main reasons behind our concerns. First, we are being told that name assignment (and consequently race of the applicant) were assigned as if random after the creation of the resumes. LLN suggests that if this assignment was truly random, we should observe close-to balanced data across all different variable partitions (as soon as LLN holds in that particular partition of data). The fact that we observe this balance strenghthens our belief in the researchers' assignment process.
    Moreover, to obtain an ATE interpretation this balance is also crucial. If data was not properly balanced then lower call-back rates of black candidates might be due to a lower education, poorer set of skills or other sources of discrimination (as gender). The contribution of race would remain unidentified in this context.
    [Note however, that even if our RCT assumptions would not be valid anymore if data was not properly balanced we could still restore on an alternative set of assumptions such as CIA.]
    (f) Is `computerskills` balanced across `sex`? What about `education`? What's going on here? Is it a problem? Hint: re-read section II C of the paper.
    (Disclaimer: For a more rigorous analysis of the education variable we would need to restore on the previously displayed "per-level" approach. Here, for simplicity we just rely on mean comparisons).
    As you can see, there exists a big gap between computer skills and average education of women and men. This gap might be driven by the non-random assignment of genders into profiles. In particular, women were oversampled in administrative and clerical positions. Authors argue that this is done such that they could get higher call-back rates. This claim is probably based on some prior which suggests that women receive higher call-back rates in these types of occupations. Based on this difference, we may understand that the average candidate applying to a clerical/administrative job presents higher computer skills and slightly lower education than the average applicant in their sample. Because women are overrepresented in these jobs, they would present a lower aggregate level of education and a higher command of computer skills than the average male applicant.
    
```{r}
bm %>% select(sex, computerskills, education) %>% group_by(sex) %>% summarize(avg_CS = mean(computerskills), avg_educ = mean(education))
```
    
    
5. The outcome of interest in `bm` is `call` which takes on the value `1` if the corresponding resume elicts an email or telephone callback for an interview. Check your answers to the following against Table 1 of the paper:
    (a) Calculate the average callback rate for all resumes in `bm`. 

```{r}
bm %>% select(call) %>% summarize(avg_call = mean(call)*100)
```
    (b) Calculate the average callback rates separately for resumes with "white-sounding" and "black-sounding" names. What do your results suggest?
    
```{r}
bm %>% select(call, race) %>% group_by(race) %>% summarize(avg_call = mean(call)*100)
```
    (c) Repeat part 2, but calculate the average rates for each combination of race and sex. What do your results suggest?
    
```{r}
bm %>% select(call, race, sex) %>% group_by(race, sex) %>% summarize(avg_call = mean(call)*100)
```
    First we should mention that we replicate almost perfectly the descriptive statistics shown in Table 1 of the original paper. From part (a) main takeaway is that call-back rate is relatively low overall. An average candidate will get less than a 10% call-back rate when applying for a job (even if his/her skills somehow match those required by the position).
    From part (b) main takeaway is that EVEN after random assignment candidates with black sounding names are up to 3 percentage points less likely (i.e. around a 30% less) than white candidates to receive a call-back.
    From part (c) there are several messages we could extract. First, women seem to receive more call-backs than males. This is not necessarily driven by gender discrimination but by the unequal call-back shares of different occupations. Because women were overrepresented in admin/clerical positions (with potentially a higher call-back ratio than the average position) these differences could be occupation driven. Further analysis would be needed to make a final claim.
    Besides this fact, it is confirmed that black candidates receive lower call-back rates uniformly across gender.
    
6. Read the help files for the `dplyr` function `pull()` and the base R function `t.test()`. Then test the null hypothesis that there is no difference in callback rates across black and white-sounding names against the two-sided alternative. Comment on your results.

 The difference across means is significant at 99% confidence level. This is, we reject the null (call-back shares being the same across races) at a 99% confidence level.

```{r}
t.test(call~race, data = bm, alternative= "two.sided")

## I don't understand the reason for using the function pull here. See a more "inefficient" way to run the same code using pull

white_call <- bm %>% filter(race=="w") %>% pull(call)
black_call <- bm %>% filter(race=="b") %>% pull(call)

t.test(white_call, black_call, alternative= "two.sided")

```



## Problem \# 3 - Two Truths and a Lie

In this problem, you'll make some simple plots of the data we collected in class during our "two truths and a lie" experiment. I've stored the raw data in a `.csv` file called `two-truths-and-a-lie-2022.csv` and posted it in the data directory of my website <https://ditraglia.com/data/two-truths-and-a-lie-2022.csv>. If you need a refresher on the details of our experiment, you can read a detailed explanation [here](http://www.stat.columbia.edu/~gelman/research/published/truths_paper.pdf).

1. Load the `tidyverse` package, giving you access to `dplyr` along with `ggplot2` and `readr`. Then used `read_csv()`to read the raw data from our "two truths" experiment from my website into a tibble called `two_truths`. Display this tibble so you can see what information it contains.

```{r}
library(tidyverse)
two_truths<-read.csv("https://ditraglia.com/data/two-truths-and-a-lie-2022.csv", stringsAsFactors = F)
head(two_truths)
```


2. By the standards of data that you'll encounter in real-world research, `two_truths` is already pretty clean! But it still needs a bit of work. Read the help files for the functions `rename()` and `if_else()` from the `dplyr` package. Use them, along with other `dplyr` functions you learned in class to do a bit of cleanup on `two_truths`. When you're done, you should have a tibble with only two columns. The first, called `certainty`, contains the 0-10 certainty scores. The second, called `guessed_right`, should be a dummy variable that equals `1` if the participants guessed right and `0` if they guessed wrong.

```{r}

two_truths$Outcome<-if_else(two_truths$Outcome == "Guessed Right", 1, 0)
two_truths<- two_truths %>% select(-Timestamp) %>% rename("certainty" = Certainty.in.your.guess, "guessed_right" = Outcome)
head(two_truths)

```

3. Plot a histogram of `certainty`. What binwidth makes sense here?
Certainty can only take 10 different values (1:10). As a result, a binwidth of 1 seems the right choice in this context.

```{r}
library(ggplot2)
two_truths %>% ggplot() +
  geom_histogram(aes(x = certainty), binwidth = 1)
```

4. Calculate the fraction of correct guesses for each certainty score. Display your results and then depict them using a simple scatter plot.
```{r}
two_truths %>% group_by(certainty) %>% summarize(avg_guess = mean(guessed_right)) %>%
  ggplot() +
  geom_point(aes(x=certainty, y=avg_guess))
```

5. From the histogram you made in step 3, you probably noticed that some values of `certainty` are very rare or even non-existent in this dataset. Repeat the preceding part with the following changes:
    (i) Exclude any values of `certainty` for which there are 3 or fewer observations. 
    (ii) Calculate the *standard error* of the proportion of correct guesses in addition to the proportion itself. 
    (iii) Compute the `lower` and `upper` endpoints of the associated 95\% confidence interval for a proportion. It's fine to use the "textbook" confidence interval. If you're not sure what I mean, see [this blog post](https://www.econometrics.blog/post/don-t-use-the-textbook-ci-for-a-proportion/).
    (iv) Read the help file for the `ggplot2` function `geom_errorbar()`, and use it to add "error bars" based on your calculations from (iii) to the plot you made in part 4 above.

```{r}

## Apologies for the long code, hopefully it is legible enough (I tried to over use the dplyr syntax)
## Note that in practice it makes little sense to have CI for proportions out of the [0,1] interval

certainty_levels <- two_truths %>% group_by(certainty) %>% count() %>% filter(n>3) %>% pull(certainty)
two_truths %>% group_by(certainty) %>% filter(certainty %in% certainty_levels) %>% summarize(n = n(), proportion = mean(guessed_right), se = sqrt(proportion*(1-proportion)/n), CI_low = proportion-1.96*se, CI_high = proportion + 1.96*se) %>% ggplot() + 
  geom_point(aes(x=certainty, y=proportion)) +
  geom_errorbar(aes(x=certainty, y=proportion,  ymin=CI_low, ymax=CI_high), width=.2,
                 position=position_dodge(.9)) 

```

