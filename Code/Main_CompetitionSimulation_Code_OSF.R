library(pwr)
library(plyr)
library(dplyr)
library(ggplot2)
library(fastmatch)

# Sessioninfo
# R version 3.4.4 (2018-03-15)
 
num_scientists <- 120
min_sample_size <- 2
max_sample_size <- 1000


min_aban <- 0
max_aban <- 1


###Full parameters for simulation###
ben_neg <- c(0, 0.25, 0.5, 0.75, 1) #benefit to publishing negative result
max_on_q <- c(1, 2, 4, 8) #level of competition (i.e. number of researchers on a given question)
decay_2nd <- c(0, 0.15, 0.40, 1, 10) #how quickly payoffs for non-novel publications decays (larger numbers = bigger cost to being scooped)
start_c <- c(10, 100, 200, 400) #startup cost

###Run###
run_complexsim(LIFE = 15000,
               MAX_PER_Q = max_on_q,
               G = 500,   
               R = 50,
               SCS = start_c,
               SCS2 = 1,
               EXP_RATE = c(5),
               SCOOPED_DECAY = decay_2nd,
               B_NEG = ben_neg)




