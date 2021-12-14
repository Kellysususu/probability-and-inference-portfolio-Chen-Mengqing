The `.rmd` version of this file is available here:
[(link)](https://biostatdata.app.vumc.org/tgs/practice-exam.rmd)

# 1. Simulation

The Monte Hall problem is a classic game show. Contestants on the show
where shown three doors. Behind one randomly selected door was a
sportscar; behind the other doors were goats.

At the start of the game, contestants would select a door, say door A.
Then, the host would open either door B or C to reveal a goat. At that
point in the game, the host would ask the contestant if she would like
to change her door selection. Once a contestant decided to stay or
change, the host would open the choosen door to reveal the game prize,
either a goat or a car.

Consider two strategies:

1.  Always stay with the first door selected.
2.  Always switch to the unopened door.

**C.** Assuming that prizes are equally likely to be assigned to each
door, calculate the probability of winning a sports car with strategy 1.

**B.** The function `game` below plays a single game of Monte Hall. The
function returns a vector of length two, the first element is the prize
under strategy 1 and the second element is the prize under strategy 2.

Use the `game` function to estimate the probability of winning with each
strategy.

``` r
require(magrittr)
require(dplyr)


game <- function(){
  prize <- sample(c("goat","goat","car"), 3)
  guess <- sample(1:3,1)
  game <- data.frame(door = 1:3, prize = prize, stringsAsFactors = FALSE) %>% 
    mutate(first_guess = case_when(
      door == guess ~ 1
      , TRUE ~ 0
    )) %>% 
    mutate(potential_reveal = case_when(
        first_guess == 1 ~ 0
      , prize == "car" ~ 0
      , TRUE ~ 1
    )) %>% 
    mutate(reveal = 1*(rank(potential_reveal, ties.method = "random") == 3)) %>% 
    mutate(switch = case_when(
      first_guess == 1 ~ 0
      , reveal == 1 ~ 0
      , TRUE ~ 1
    ))
  c(game$prize[game$first_guess == 1], game$prize[game$switch == 1])
}
```

``` r
sample(1:3,1)
```

    ## [1] 3

**A**. Communicate the precision of your simulated probability by
calculating a 95% confidence interval.

# 2. Probability

Consider a test for a rare genetic condition. Let T+ denote a test
result that indicates the condition is present, while T- denotes
absence. Let D+ and D- denote the true status of the disease.

**C**. Fill-in the probability table using the following information:

-   P(T+\|D+) = .8, and
-   P(T-\|D-) = .9, and
-   P(D+) = 0.001

|     |  D+   | D-  |     |
|:---:|:-----:|:---:|:---:|
| T+  |       |     |     |
| T-  |       |     |     |
|     | 0.001 |     |  1  |

**B**. Calculate the positive predictive value of the test, P(D+\|T+).

**A** If researchers could improve the test so that P(T+\|D+) =
P(T-\|D-) = p, what would p need to be so that the positive predictive
value of the test is 0.5?

# 3. Discrete Distributions

Suppose the rate of car accidents on I-440 in Nashville is, on average,
5 per day. Assume that car accidents follow a Poisson distribution, and
that accident counts are independent from day to day.

**C**. What is the 90th percentile for the number of car accidents?

**B**. Generate a plot of the density function for the number of
accidents on I-440 in Nashville during a 30 day period. (Simulated or
analytic approaches are acceptable.)

**A**. What is the probability of observing 180 or more accidents in a
30 day period?

# 4. Continuous Distributions

**C.** Suppose diastolic blood pressure (DBP) follows a normal
distribution with mean 80 mmHg and SD 15 mmHg. What is the probability
that a randomly sampled personâ€™s DBP lies between 90 and 110 mmHg?

**B.** Suppose a human femur was discovered that is 40 cm long. Also
suppose that using the NHANES data, researchers believe the distribution
of femor bones, by sex, are distributed as follows:

-   Female adult femor  ∼ *N*(36, 3.3)
-   Male adult femor  ∼ *N*(40, 3.4)

![](leg-length-distributions.svg)

Under the assumption that male and females are equally likely, what is
the probability that the discovered femor was from a male?

**A.** Suppose the risk of surgical site infection within 30 days of
hernia repair is *p*. Suppose the value of *p* is a random variable
described by the beta(10, 100) distribution. What is the probability
that 2 or more patients in a cohort of 10 experience an infection within
30 days?

# 5. Expectation and Variance

Suppose a random variable X is distributed as a mixture of beta
distributions. The probability, density, and random generation functions
are below.

``` r
pf5 <- function(x){
  pbeta(x,5,25)*.6 + pbeta(x,10,5)*.4
}

df5 <- function(x){
  dbeta(x,5,25)*.6 + dbeta(x,10,5)*.4
}

rf5 <- function(N){
  G <- rbinom(N,1,.6)
  rbeta(N,5,25)*G + rbeta(N,10,5)*(1-G)
}
```

**C.** What is *E*\[*X*\]?

**B.** Suppose we create a new random variable
$$Y = \\frac{1}{1+e^{-X}}$$

What is *E*\[*Y*\]?

**A.** What is *E*\[*X* + *Y*\]

# 6. Transformations & Sampling Distributions

The mean of the cauchy distribution does not exist. Data that follow
this distribution should be summarized with the median instead of the
mean. See `?dcauchy` for the help documentation for the cauchy
distribution function in R.

**C.** Generate an histogram or density curve for the sampling
distribution of the median when the sample size is 301 and data are
cauchy with location = 0 and scale = 1.

**B.** What is the connection between order statistics and the median?

**A.** Below is the CDF function for the kth order statistic when the
underlying distribution is cauchy with location = 0 and scale = 1.
Create a plot of the ECDF of the simulated sampling distribution
generated in **C** and overlay the CDF using the function below.

``` r
Fk <- function(x,k,n){
  pbinom(k-1, n, pcauchy(x), lower.tail = FALSE)
}
```

# 7. Estimation of CDF and PDF from data

The following code will load the low birth weight data from the MASS
package. The description of the variables in the dataset can be found in
the birthwt documentation with the command `?MASS::birthwt`.

**C.** Estimate the distribution of birth weights of infants whose
mothers did not smoke during pregnancy using the MLE method with a
normal distribution. Create a plot of the estimated density function.

``` r
bwt <- MASS::birthwt
```

**B.** Estimate the same distribution as in part **C** using the method
of moment method with the gamma distribution. Create a plot of the
estimated density function.

**A.** Estimate the same distribution as in part **C** using the kernel
density method with a gaussian kernel. Create a plot of the estimated
density function.

# 8. Sample from an estimated distribution

**C.** Generate a 95% confidence interval for the mean birthweight of
infants whose mothers did not smoke during pregnancy using the
bootstrap.

**B.** Generate a 95% confidence interval for the mean birthweight of
infants whose mothers did not smoke during pregnancy using the Central
Limit Theorem shortcut.

**A.** Let *μ*<sub>*s*</sub> be the mean birthweight of infants whose
mothers smoked during pregnancy. Let *μ*<sub>*n**s*</sub> be the mean
for the non-smoking group. Use simulation to calculate the 95%
confidence interval for *μ*<sub>*s*</sub>/*μ*<sub>*n**s*</sub>.

# 9. Inference

A colleague comes to you looking for statistcal advice about a study she
is planning. She wants to show that the probability of an allergic
reaction to a new immunization shot is less than 0.1. Suppose the usual
immunization shot has an allergic reaction risk of 0.2. Suppose there
are two valid scientific hypotheses:

-   *H*<sub>*o*</sub>: P(allergic reaction) = .2
-   *H*<sub>*a*</sub>: P(allergic reaction) = .1

The current plan is to enroll 100 subjects and to reject
*H*<sub>*o*</sub> if the upper bound of the one-sided 95% confidence
interval for the proportion of allergic reactions is less than 0.2.

Your collegue has calculated that if 13 patients or fewer experience an
allergic reaction, the confidence interval will fall below 0.2, and the
study will demonstrate a conclusive difference from the historical
reaction rate.

**C.** What is the Type I error of your colleague’s analysis plan?

**B.** What is the power of your colleague’s analysis plan?

**A.** T/F. If 89 in a cohort of 1000 laparoscopic repairs and 97 in a
cohort of 1000 robotic repairs experience an infection, the p-value for
the standard hypothesis of no difference is 0.54. Because p-value \>
0.05, we can conclude that rates of infection are the same in the two
groups.

# 10. Joint Distributions

**C.** Fill in the blank. The sample correlation is a measure of
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ association.

**B.** Explain why predictions from a conditional distribution generally
have smaller prediction error than predictions from the marginal
distribution.

**A.** Use the CLT shortcut to calculate the 95% confidence interval for
the correlation of arm circumferance and arm length using the NHANES
dataset. Is the sample correlation a reasonable measure of association
for this data?
