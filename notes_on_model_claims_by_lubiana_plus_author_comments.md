
# Model claims

I will write long reports for some claims, but not for all. 

Notably, I will make it clear whenever I find something strange. 
The lack of a report means that it is *probably okay*.

The snippets here are (except when noted) derived from a revised and cleaned version of the model.
The logic, however, was not changed in the refactoring process, as far as the established unit tests
could cover.

## 01

### Claim 
* Each scientist is characterized by two parameters representing their characteristic methods: the sample size of their conducted research studies, s, and their probability of abandoning a research question when another scientist publishes a result on that question, a.

### Report 
- The code has both abstractions implemented in the `play_complexcomp` function. 

- The user can either enter a vector with sample sizes for each scientist, or generate random sample sizes based on constraints. When simulation starts, each scientist has one fixed sample size for all experiments during their life. 

- There is a parameter for the "abandon probability" which is implemented as: 
 `any(runif(number_of_publications_of_scooped_question, 0, 1) < scientist_df$abandon_prob[ids_for_scooped_scientists[i]])` 
The code rolls the dice for abandonment for every publication about their ongoing question. 


## 02
### Claim 

* Scientists transmit their methods to trainees, so the distributions of these parameters can evolve across generations

### Report 

- Even though trainer-trainee relations are not explictly modeled, the code 
weighs sample size and abandon probabilities by the fitness:

```
fitness <- outcome_list[[1]]$payoff

normalized_fitness <- fitness / sum(fitness)


sample_size <- get_sample_sizes_based_on_fitness(sample_size,
                                                 rounded_popsize,
                                                 normalized_fitness)

sample_size <- mutate_sample_sizes(sample_size,
                                   rounded_popsize)


abandon_probabilities <- get_abandon_probabilities_based_on_fitness(abandon_probabilities,
                                                                    rounded_popsize,
                                                                    normalized_fitness)

abandon_probabilities <- mutate_abandon_probabilities(abandon_probabilities,
                                                      rounded_popsize)
```

## 03

### Claim 
* Each population is initialized by sampling n integer values of s from a uniform distribution [2, 1000] and n real-numbered values of a from a uniform distribution [0, 1].

### Report 

- This is represented by the following snippet (composed from original code) :

```
min_sample_size <- 2
max_sample_size <- 1000
min_aban <- 0
max_aban <- 1

ss <- round(runif(rounded_popsize, min_sample_size, max_sample_size))
aban <- runif(rounded_popsize, min_aban, max_aban)

```

## 04
### Claim

* On any given question, a scientist’s statistical power, pwr, can take on any real-numbered value in [0.05, 1].

### Report 

- This is a feature of the function "pwr.t.test". See its documentation [here](https://www.rdocumentation.org/packages/pwr/versions/1.3-0/topics/pwr.t.test).  

## 05
### Claim
* pwr is a function of three parameters: sample size, s, the false-positive rate, α, and the size of the effect being studied, e. pwr is calculated using a two-sample t-test, implemented with the pwr.t.test() function in the ‘pwr’ package in R3

* This effectively assumes that all research is of the form where scientists collect s independent data points from each of two populations and test for a difference between the two.

* Following convention, the level of statistical significance required for a positive result, α, remains fixed at 0.05

* We assume that there are an infinite number of research questions, each of which is characterized by an effect size e, where e represents a standardized mean difference between two populations

* we assume that the e value of each question is drawn from an exponential distribution, with a rate parameter of 5 and rounded to the nearest 0.1


### Report
- The function pwr is implemented as described:

```
get_t_test_powers <-
  function(ss_of_testers,
           questions_e_size,
           questions_they_are_working_on) {
    as.numeric(
      pwr.t.test(
        n = ss_of_testers,
        d = questions_e_size[questions_they_are_working_on],
        sig.level = 0.05,
        type = "two.sample"
      )[4]$power
    )
  }
```

- True effect size is randomly sampled from exponential distribution 
based on parameter (which seems to be always set to 5 in the analysis):

```
get_questions_effect_size <- function(number_of_questions, exp_shape) {
  round(rexp(number_of_questions, exp_shape), 1)
}
```


## 06
### Claim
* Each research question has a unique id (e.g. 1, 2, 3, ….) and a maximum of m scientists can work on any given question

### Report

- In the dataframe for the models, each scientist get a numeric id: 

```
get_ids_for_scientists <- function(sample_sizes) {
  1:length(sample_sizes)
}
```

- In the simulation, there is a parameter for controlling the maximum number of scientists per question. It is used twice.
    - When moving scientists that published:
    
```
get_next_question <-
  function(questions_n_on_q,
           largest_q_avail,
           max_scientists_per_q,
           question_ids,
           max_previously_published_questions) {
    dum1 <- questions_n_on_q[1:largest_q_avail] < max_scientists_per_q
    dum2 <-
      question_ids[1:largest_q_avail] > max_previously_published_questions
    dum <- dum1 & dum2
    next_q <- match(TRUE, dum)
    return(next_q)
  }
```
    - And when moving scientists that, after scooping, abandoned their previous questions:
```  
check_if_questions_are_not_full <- function(scientists_per_question, largest_question_for_scooped_scientists, max_scientists_per_q) {
    scientists_per_question[1:largest_question_for_scooped_scientists] < max_scientists_per_q
    }
```  
    

## 07
### Claim
* A scientist begins their career on the smallest-numbered open research question (i.e., the smallest numbered question occupied by fewer than m other scientists)

### Report 

- That is represented in the code. This is the snipped that deals with it: 
```
assign_scientists_to_questions <-
  function(scientist_df,
           questions_q_id,
           max_scientists_per_q,
           ids_for_scientists) {
    scientist_df$question[ids_for_scientists] <-
      rep(questions_q_id, each = max_scientists_per_q)[ids_for_scientists]
    return(scientist_df)
  }
```
  - The rep function generates "n" repetitions for each question id (where "n" is the max_scientists_per_q parameter). The scientists then get assigned in order to the questions.
  
  
## 08

### Claim
* We do this to avoid unrealistic outcomes (e.g., all scientists working on a single question; all scientists working on different questions) 

### Report
- Even though the code does not enforce this constraint (all scientists could work on the same question) the combination of parameters for that doesn't seem to have been used.

## 09 

### Claim
* Each scientist’s career lasts T = 15,000 timesteps

* In one specific case of low startup costs (c = 10, see below), career length was reduced to 5000 time-steps for computational efficiency, without affecting the simulation results

### Report
- These both claims are not enforced by the code, but 15000 is the parameters set in the analysis. __I could not find a mention of "5000" in the Main_CompetitionSimulation_Code_OSF.R (Version: 4) code__.

- __2 snippets below are from the original, pre-refactoring code__

```
#total population size
num_players <- 120
```

```
#              LIFE   MAX_PER_Q,   G,    Rep,   SCS,  SCS2,   E_RATE, DECAY,     B_NEG)
run_complexsim(15000, max_on_q,   500,   50,   start_c,   1,   c(5),  decay_2nd, ben_neg)
```

### Authors' comment
*As indicated in the manuscript, we ran the simulation for 15,000 timesteps, except in the one case where startup costs = 10, where we set career length to 5000 timesteps. This can be seen in the raw data on the OSF repository. Running the simulation for this combination of parameters requires the user to change 15000 to 5000 in the code, manually.*

## 10 

* Once their career has started, a scientist collects data until they reach their desired sample size as dictated by their respective s value. The number of time steps required to do this. cs represents the sample cost: the number of time steps needed to acquire one data point (fixed at 1). c represents the startup cost: the number of time steps needed to set up a study.

- That is precisely how the time costs are set:

```
get_time_cost_for_each_question <- function(scientist_df, sample_cost, startup_cost) {
  sample_sizes <- scientist_df$ss
  time_cost_for_each_question <- sample_sizes * sample_cost + startup_cost
  return(time_cost_for_each_question)
}
```
 
The times are updated every cycle:

```
time_cost_for_each_question <- time_cost_for_each_question - time_to_next_event
```

## 11

### Claim
* We assume that c is independent of s.
### Report
- They are independent parameters in all functions

## 12

### Claim
* For questions with a true effect (e > 0), a scientist obtains a statistically-significant result with probability pwr.
### Report
- That is present in the code in the following snippets.

Get a power value usint the "pwr.t.test" function:

```
get_t_test_powers <-
  function(ss_of_testers,
           questions_e_size,
           questions_they_are_working_on) {
    as.numeric(
      pwr.t.test(
        n = ss_of_testers,
        d = questions_e_size[questions_they_are_working_on],
        sig.level = 0.05,
        type = "two.sample"
      )[4]$power
    )
  }
```
Then, a random number is sampled from an uniform distribution. If the number is less than the power, it returs "TRUE", effectively mimicking the statistical power.

```
check_results_scientists_got <- function(num_testers, powers) {
  runif(num_testers, 0, 1) < powers
  
```

## 13

### Claim
* For questions with no true effect (e = 0), a scientist obtains a statistically-significant result with probability α.

### Report
- In the function used for power, if the effect is 0, the reported "power" is 0.05. Even though power is not defined in that case, the code behaves as described. 
Example run:

```
> as.numeric(
+     pwr.t.test(
+         n = 3,
+         d = 0,
+         sig.level = 0.05,
+         type = "two.sample"
+     )[4]$power)
[1] 0.05
```

## 14
### Claim
* Once a scientist publishes a result, two parameters determine the scientist’s payoff: the novelty of the result, v, and whether the result is positive (i.e., significant) or negative (i.e., non-significant)

### Report
- Both characteristics are modelled by the code (see reports below).

## 15
### Claim
* The novelty of a result is calculated as: ni = (1/(1+ number_of_prior_results)^d where d (the decay) determines the severity of the cost of being scooped

### Report
- That is exacly what is modelled: 
```
calculate_novelty <- function(num_prior, decay) {
  (1 / (1 + num_prior))^decay
}

```

## 16
### Claim
* For negative results, scientists receive payoff vbn, where 0 ≤ bn ≤ 1

### Report
- That is exacly what is modelled: 
```
calculate_payoff <-
  function(results_scientists_got,
           i,
           novelty_of_result,
           b_neg) {
    if (results_scientists_got[i]) {
      payoff <- novelty_of_result
    } else {
      payoff <- novelty_of_result * b_neg
    }
  }
```

## 17
### Claim
* After publishing, the scientist moves to the next open research question (i.e., one with fewer than m other scientists working on it)

### Report
- That seems to be true, however it is hard to get the details of the code. In the code below it seems like the first bit (dum1) get "TRUE" for questions which are not full, and "TRUE" for question ids that have not been published. 
- All move to the same question.

```
get_next_question <-
  function(questions_n_on_q,
           largest_q_avail,
           max_scientists_per_q,
           question_ids,
           max_previously_published_questions) {
    dum1 <- questions_n_on_q[1:largest_q_avail] < max_scientists_per_q
    dum2 <-
      question_ids[1:largest_q_avail] > max_previously_published_questions
    dum <- dum1 & dum2
    next_q <- match(TRUE, dum)
    return(next_q)
  }

```
### Authors' comment 
*This is correct. dum 1 is true for all questions that have less than max_scientists_per_q on them. These could be questions currently being investigated, questions that have not been investigated, or questions that have been investigated but no longer have any scientists on them. dum 2 is true only if a question is larger than the largest question that has previously been published on. The combination of dum 1 and dum 2 ensures that scientists don't move backwards, and also don't move to questions that are already "full" with the maximum number of scientists. *

## 18
### Claim 
* All other scientists working on the question corresponding to the newly-published result abandon that question with a probability determined by their individual a value.

### Report
This wording is slightly **Innacurate.** The way it is written gives the impression that publishing at the same time is not possible. In the code is possible that multiple scientists publish at exactly the same time and that actually happens, as scientists with same sample size publish at the same time.
It could be reworded to "to the newly-published result (or results)" or similar

The moving is done by the following loop:

```

for (i in 1:number_of_testers) {
        next_question <- get_next_question(
          scientists_per_question,
          largest_question_id_available,
          max_scientists_per_q,
          ids_for_questions,
          max_previously_published_question
        )
        scientists_per_question <- update_scientists_per_question(scientists_per_question,
          question_to_abandon = questions_they_are_working_on,
          question_to_go_to = next_question
        )
        scientist_df$question[testers_ids[i]] <- next_question
      }
```

### Authors' comment 
*This point is correct. In the manuscript, the accurate description would be to write that "all other scientists who are on the newly published question (and who are not themselves publishing that round) abandon with probability a."*


## 19
### Claim 
* In order to prevent scientists from being persistently “stuck” on the same questions as the scientist who just scooped them, we assume that scientists who abandon move to a different question than the one assigned to their scooper

### Report
- That is exaclty what the code does. 

```
make_checks_and_get_first_question_available <- function(scientists_per_question, largest_question_for_scooped_scientists, max_scientists_per_q, ids_for_questions, max_of_scoopers_questions) {
  checker_for_questions_not_full <- check_if_questions_are_not_full(scientists_per_question, largest_question_for_scooped_scientists, max_scientists_per_q)

  checker_for_questions_not_taken_by_scoopers <- check_which_questions_have_not_been_taken_by_scoopers(ids_for_questions, largest_question_for_scooped_scientists, max_of_scoopers_questions)

  checker_for_questions_available <- checker_for_questions_not_full & checker_for_questions_not_taken_by_scoopers

  first_question_available <- get_first_question_available(checker_for_questions_available)
}
```

## 20
### Claim 
* This process repeats until scientists reach the end of their careers, at which point all scientists retire.

### Report
- The process occurs indeed until the "lifespan" is reached:

```
   while (current_time_period < lifespan) {
      print(current_time_period)

      # Set up time-related parameters ----

      time_to_next_event <- get_time_to_next_event(time_cost_for_each_question, lifespan, current_time_period)
      print(time_to_next_event)
      time_cost_for_each_question <- time_cost_for_each_question - time_to_next_event
      current_time_period <- current_time_period + time_to_next_event
  
      # And the code continues (...)
  }
```

## 21
### Claim 
* Upon retiring, each scientist’s “fitness” is calculated as proportional to the total number of points that they acquired during their career.
* A new (non-overlapping) generation of scientists is then created, with their s and a values sampled from members of the previous generation, weighted by fitness.

### Report
- This seems to be represented in the code: 

```
fitness <- outcome_list[[1]]$payoff
normalized_fitness <- fitness / sum(fitness)
                  
                  
sample_size <- select_sample_sizes_that_reproduce(sample_size,
                                                  rounded_popsize,
                                                  normalized_fitness)
```

## 22
### Claim 
* We assume that inheritance is noisy: once a parent is selected to “reproduce,” the sample size, s, of its “offspring” scientist is drawn from a normal distribution with a mean corresponding to the parent’s value and a standard deviation of 2
* Offspring s values are rounded to the nearest integer and truncated to remain in [2, 1000].
* Values of s < 2 are set to 2 because two-sample t-tests require at least 2 samples per group.

### Report
I could not see _scientists_ being selected to reproduce. The new generation parameters are biased by the success (fitness) of the previous generation, but *abandon probabilities* and *sample sizes* are sampled independently.

The math is exactly as described:

```
select_sample_sizes_that_reproduce <- function(sample_size, rounded_popsize, normalized_fitness) {
  sample(sample_size,
         size = rounded_popsize,
         replace = TRUE,
         prob = normalized_fitness
  )
}

reproduce_sample_sizes <- function(sample_size, rounded_popsize) {
  sample_size <- round(sample_size + rnorm(rounded_popsize, 0, 2))
  sample_size <- pmin(pmax(sample_size, 2), 1000)
}

```

### Authors' comment 
*This is a non-issue. Strategies being transmitted probabilistically, weighted by fitness, is the way that "reproduction" happens in the model. *


## 23
### Claim 
* Similarly, the abandonment probabilities of offspring scientists, a, are drawn from a normal distribution with a mean corresponding to their parent’s value and a standard deviation of 0.01, truncated to remain in [0, 1].

### Report
As mentioned previously,  *abandon probabilities* and *sample sizes* are sampled independently.

```
The math is exactly as described:

select_abandon_probabilities_that_reproduce <- function(abandon_probabilities,
                                                        rounded_popsize, 
                                                        normalized_fitness) {
  sample(abandon_probabilities,
         size = rounded_popsize,
         replace = TRUE,
         prob = normalized_fitness)
}

reproduce_abandon_probabilities <- function(abandon_probabilities, rounded_popsize) {
  abandon_probabilities + rnorm(rounded_popsize, 0, 0.01)
  abandon_probabilities <- pmin(pmax(abandon_probabilities, 0), 1)
}

```

## 24
### Claim 
* To ensure convergence to equilibrium sample sizes (see Supplementary Section 7), the evolutionary process proceeds for 500 generations, at which point the simulation stops.

### Report

I have not looked at the convergence analysis, but the main code is run for 500 generations. Original code:

```
#              LIFE   MAX_PER_Q,   G,    Rep,   SCS,  SCS2,   E_RATE, DECAY,     B_NEG)
run_complexsim(15000, max_on_q,   500,   50,   start_c,   1,   c(5),  decay_2nd, ben_neg)

```

## 25

### Claim
Parameter Definition

Value/Range
n - population size = 120
s - Scientist’s target sample size - Uniform [2, 1000]
a - Scientist’s probability of abandoning a research question when scooped - Uniform [0, 1]

α - False-positive rate - 0.05
e - Effect size - Exponential Rate parameter characterizing distribution of effect sizes  - 5
T -  Scientists’ career length - 15000 if c > 10,  5000 if c = 10

cs - Sample cost: number of time steps to acquire one data point - 1
c - Startup cost: number of time steps to set up a study - 10, 100, 200, 400
m - Maximum number of scientists per research question - 1, 2, 4, 8
d - Decay parameter determining the penalty for being scooped - 0, 0.15, 0.40, 1, 10
bn - Payoff from publishing negative results, relative to positive results - 0, 0.25, 0.5, 0.75, 1


### Report

For that check, I`ve revised only the primary code for adherence. 

- Main_CompetitionSimulation_Code_OSF.R (Version: 4):

  - Those are the exact parameters used. I could not find the code for the "5000" lifespan when c = 10, though.
  
The parameters used for the secondary analysis are not taken into account by the table.

### Authors' comment 
*As noted earlier, lifespan issue simply requires the user to change the lifespan parameter from 15000 to 5000. The parameters for the secondary analyses can be seen in the secondary analysis files on the OSF repository.*

