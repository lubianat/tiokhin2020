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

###Evolutionary Simulation###

run_complexsim <- function(LIFE, MAX_PER_Q, G, R, SCS, SCS2, EXP_RATE, SCOOPED_DECAY, B_NEG) {
  
  lifespan <- LIFE #life of 1 generation of scientists
  popsize <- num_scientists # sets population size, from global parameter "num_scientists"
  max_per_q <- MAX_PER_Q #sets number of competitors
  gens <- G # number of generations
  repeats <- R # number of simulations for every unique combo of effect and startup cost
  startup_costs <- SCS # the startup costs we want to use
  sample_costs <- SCS2 # the sample costs we want to use
  exp_rate <- EXP_RATE #shape of the exponential distribution
  decay <- SCOOPED_DECAY
  b_neg <- B_NEG # payoff to being second
  
  eq.samplecost <- vector()
  eq.startupcost <- vector()
  eq.exprate <- vector()
  eq.decay <- vector()
  eq.samplesize <- vector()
  eq.totalfitness <- vector()
  eq.maxperq <- vector()
  eq.b_neg <- vector()
  eq.abandonprob <- vector()
  eq.life <- vector()
  res_list_all <- list()
  
  single_region <- length(max_per_q) == 1 && length(repeats) == 1 && length(startup_costs) == 1 && length(sample_costs) == 1 &&
                   length(exp_rate) == 1 && length(decay) == 1 && length(b_neg) == 1
  
  mean_ss <- rep(0, gens)
  lower_ss <- rep(0, gens)
  upper_ss <- rep(0, gens)
  mean_aban <- rep(0, gens)
  lower_aban <- rep(0, gens)
  upper_aban <- rep(0, gens)
  
  #tracker
  zz <- 1
  
    for(rate in exp_rate) {
      for (sample_cost in sample_costs) {
        for(startup_cost in startup_costs) {
           for(comp_n in max_per_q) {
             for(dcy in decay) {
               for(bn in b_neg) {
                  
                  #list to store all results across repeats
                  results_list <- vector("list", repeats)
                  
                  for(rep in 1:repeats) {
                    
                    # initialize the population, for each repeat 
                    rounded_popsize <- round(popsize / comp_n) * comp_n
                    ss <- round(runif(rounded_popsize, min_sample_size, max_sample_size))
                    aban <- runif(rounded_popsize, min_aban, max_aban)
                    
                    # start looping
                    for (gen in 1:gens) {

                      #play scientists against each other
                      outcome_list <- play_complexcomp(1, lifespan, ss, comp_n, startup_cost, sample_cost, rate, dcy, 
                                                       bn, aban)
                      fitness <- outcome_list[[1]]
                      
                      # calculate fitness and manage reproduction
                      fitness2 <- fitness/sum(fitness)
                      ss <- sample(ss, size = rounded_popsize, replace=TRUE, prob=fitness2)
                      ss <- round(ss + rnorm(rounded_popsize, 0, 2)) #change sd to change size of mutations
                      ss <- pmin(pmax(ss, 2), 1000)
                      
                      aban <- sample(aban, size=rounded_popsize, replace=TRUE, prob=fitness2)
                      aban <- aban + rnorm(rounded_popsize, 0, 0.01) #change sd to change size of mutations
                      aban <- pmin(pmax(aban, 0), 1)
                      
                      #save state of the population sample sizes and abandonment probabilities
                      if (single_region) {
                        mean_ss[gen] <- mean_ss[gen] + mean(ss)
                        dum <- quantile(ss, c(0.025, 0.975))
                        lower_ss[gen] <- lower_ss[gen] + dum[[1]]
                        upper_ss[gen] <- upper_ss[gen] + dum[[2]]
                        
                        mean_aban[gen] <- mean_aban[gen] + mean(aban)
                        dum <- quantile(aban, c(0.025, 0.975))
                        lower_aban[gen] <- lower_aban[gen] + dum[[1]]
                        upper_aban[gen] <- upper_aban[gen] + dum[[2]]
                      }
                      
                    } # end of generation
                    
                    eq.samplecost <- c(eq.samplecost, sample_cost)
                    eq.startupcost <- c(eq.startupcost, startup_cost)
                    eq.exprate <- c(eq.exprate, rate)
                    eq.decay <- c(eq.decay, dcy)
                    eq.samplesize <- c(eq.samplesize, mean(ss))
                    eq.totalfitness <- c(eq.totalfitness, sum(fitness))
                    eq.maxperq <- c(eq.maxperq, comp_n)
                    eq.b_neg <- c(eq.b_neg, bn)
                    eq.abandonprob <- c(eq.abandonprob, mean(aban))
                    eq.life <- c(eq.life, lifespan)
                    results_list[[rep]] <- outcome_list[[2]]
                    
                  } # end of repeat
                  
                  #combine results and label df w/ accurate parameters
                  eq.result.df <- bind_rows(results_list)
                  eq.result.df$eq.samplecost <- sample_cost
                  eq.result.df$eq.startupcost <- startup_cost
                  eq.result.df$eq.exprate <- rate
                  eq.result.df$eq.decay <- dcy
                  eq.result.df$eq.maxperq <- comp_n
                  eq.result.df$eq.b_neg <- bn
                  eq.result.df$eq.life <- lifespan
                  
                  #save eq.results.df after all repeats for one unique combination of paramaters 
                  res_list_all[[length(res_list_all)+1]] <- eq.result.df
                } 
               
      results <- data.frame(eq.samplesize, eq.abandonprob, eq.totalfitness,
                            eq.samplecost, eq.startupcost, eq.exprate, eq.decay, eq.maxperq, 
                            eq.b_neg, eq.life) 
      
      #save objects; 
      if(single_region == FALSE){
        save(results, file = "eq_data_sample.RData")
        save(res_list_all, file = "res_list_sample.RData")
        
        #update tracker
        print(paste("parameter combo #", zz))
        zz <- zz + 1
        
      } else{
          return(list(mean_ss, lower_ss, upper_ss, 
                      mean_aban, lower_aban, upper_aban))
        }

               } } } } } 
}

####################
###Run simulation for various parameter combinations###
####################

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




