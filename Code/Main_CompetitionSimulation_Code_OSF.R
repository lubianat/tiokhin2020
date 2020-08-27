# #instal latest version of packages
# install.packages("pwr")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("fastmatch")

#load libraries
library(pwr)
library(plyr)
library(dplyr)
library(ggplot2)
library(fastmatch)

#Sessioninfo
# R version 3.4.4 (2018-03-15)
# 
# other attached packages:
#   [1] fastmatch_1.1-0 ggplot2_3.1.0   dplyr_0.7.8     plyr_1.8.4      pwr_1.2-2  

##################
###set these parameters before simulations###
##################

#total population size
num_players <- 120

#Scientists' initial sample sizes. Change to check sensitivity to initial conditions. 
#Note that this does not change the minimum and maximum allowable sample sizes, 2 and 1000, respectively. 
min_sample_size <- 2
max_sample_size <- 1000

#Scientists' initial abandonment probabilities. Change to check sensitivity to initial conditions. 
#Note that this does not change the minimum and maximum allowable abandonment probabilities, 0 and 1, respectively. 
min_aban <- 0
max_aban <- 1

##################
###function to play scientists against one another###
##################

play_complexcomp <- function(evolution, lifespan, ss, max_players_per_q, startup_cost, sample_cost, exp_shape, decay,
                             b_neg, abandon_prob){
  
  if(evolution == 1) {
    samplesizes <- ss
  } else {
    samplesizes <- round(runif(num_players, min_sample_size, max_sample_size)) # sample sizes
  }
  
  #set sci ids
  sci_ids <- 1:length(samplesizes)
  
  #set number of questions
  num_questions <- round((lifespan / (startup_cost + 2)) * num_players) + 1000
  
  #set payoffs to 0 at beginning of each run
  payoff_v <- rep(0.0000001, length = length(samplesizes)) 
  
  #data frames
  scientist_df <- data.frame(sci_id = sci_ids,
                             ss = samplesizes, 
                             question = 0,
                             abandon_prob = abandon_prob, 
                             payoff = payoff_v)
  
  results_m <- as.matrix(data.frame(q_id = rep(0, num_questions * max_players_per_q),
                           sci_id = 0,
                           ss = 0,
                           result = 0))
  
  #vectors to store information about questions
  questions.q_id <- seq_len(num_questions) 
  questions.n_on_q <- rep(0, num_questions)
  questions.esize <- round(rexp(num_questions, exp_shape), 1)

  t <- 1 #starting time period
  tt.all <- scientist_df$ss*sample_cost + startup_cost #baseline
  t.all <- scientist_df$ss*sample_cost + startup_cost #tracker
  
  #assign scientists to questions and update questions_df with scientists
  scientist_df$question[sci_ids] <- rep(questions.q_id, each = max_players_per_q)[sci_ids]
  sci_per_q <- questions.n_on_q
  tab <- table(scientist_df$question)
  sci_per_q[as.integer(names(tab))] <- as.vector(tab)
  questions.n_on_q <- sci_per_q
  
  #will store unique q_ids for published results
  prev_pub_q <- vector()
  
  #results tracker
  results_tracker_old <- 0
  
  ###while loop###
  while (t < lifespan) {
    
    time_to_next_event <- min(c(t.all, lifespan-t))
    t.all <- t.all - time_to_next_event
    t <- t + time_to_next_event
    
    #exit loop when necessary#
    if(t >= lifespan){break}
    
    samplers <- c(scientist_df$sci_id[t.all == 0]) #id of scientists that can sample
    num_samplers <- length(samplers)
    ques <- c(scientist_df$question[samplers]) #question they are working on
    ss_of_samplers <- scientist_df$ss[samplers]
    
    powers <- as.numeric(pwr.t.test(n=ss_of_samplers, d = questions.esize[ques],
                                      sig.level=0.05, type="two.sample")[4]$power)
    res <- runif(num_samplers, 0, 1) < powers #checks what result those players got
    
    for(i in 1:num_samplers){
      
      # find all prior published results (true or false) on the corresponding question, 
      num_prior <- length(prev_pub_q[prev_pub_q == ques[i]])

      # how novel is the new result
      novelty_of_res <- (1 / (1 + num_prior)) ^ decay
      
      #calculate payoff
      if(res[i]){
        payoff <- novelty_of_res
      } else{
        payoff <- novelty_of_res * b_neg
      }
      #add payoff to data frame for that sampler
      scientist_df$payoff[samplers[i]] <- scientist_df$payoff[samplers[i]] + payoff
    } 
    
    #update vector for previously published questions
    prev_pub_q <- c(prev_pub_q, ques)
    
    #updated results tracker
    results_tracker_new <- results_tracker_old + num_samplers
    
    #update results_m 
    results_m[(results_tracker_old+1):results_tracker_new,] <- c(ques, samplers, ss_of_samplers, res)
    
    #subset of new question space to search
    largest_q_avail <- max(scientist_df$question) + num_samplers
    
    #largest question in previously pub questions
    max_prev_pub <- max(prev_pub_q)
    
    #move scientists who published to subsequent questions
    for(i in 1:num_samplers){
      dum1 <- questions.n_on_q[1:largest_q_avail] < max_players_per_q
      dum2 <- questions.q_id[1:largest_q_avail] > max_prev_pub
      dum <- dum1 & dum2
      next_q <- match(TRUE, dum)
      questions.n_on_q[ques[i]] <- questions.n_on_q[ques[i]] - 1 #subtract 1 from current 1
      questions.n_on_q[next_q] <- questions.n_on_q[next_q] + 1 # add 1 to next q
      scientist_df$question[samplers[i]] <- next_q #move scientist to new question
    }
    
    #reset the time until sampling for the subset of players who sampled
    t.all[samplers] <- tt.all[samplers]
    
    #update positions of scientists who are working on questions where there just was published result
    pos_potent_mover <- which(scientist_df$question %fin% ques & !scientist_df$sci_id %fin% samplers)
    num_potent_movers <- length(pos_potent_mover)
    
    # limit search for new questions to the smaller subset of all q's that could potentially be moved to, for movers
    largest_q_avail_movers <- largest_q_avail + num_potent_movers
    
    if (num_potent_movers > 0) {
 
      #for each scientist, check the number of new results for their question. They then move to next question 
      #probabalistically. The larger the number of new results, the higher the probability that they move. 
      
      for (i in 1:num_potent_movers) {
        
        #questions that the scooped scientist is working on
        question_of_scoopee <- scientist_df$question[pos_potent_mover[i]]
        #id of the scooper
        scooper_id <- samplers[ques == question_of_scoopee]
        #take max of scooper current questions
        ineligible_max <- max(scientist_df$question[scientist_df$sci_id %fin% scooper_id])
        
        #number of new results that have been published on that scientist's question
        n_new <- length(scooper_id)
        
        if(any(runif(n_new, 0, 1) < scientist_df$abandon_prob[pos_potent_mover[i]])){

          dum1 <- questions.n_on_q[1:largest_q_avail_movers] < max_players_per_q
          dum2 <- questions.q_id[1:largest_q_avail_movers] > ineligible_max
          dum <- dum1 & dum2
          next_q <- match(TRUE, dum)
          questions.n_on_q[question_of_scoopee] <- questions.n_on_q[question_of_scoopee] - 1 #subtract 1 from current 1
          questions.n_on_q[next_q] <- questions.n_on_q[next_q] + 1 # add 1 to next q
          scientist_df$question[pos_potent_mover[i]] <- next_q #move scientist to new question
          #reset the time until sampling for the subset of players who moved
          t.all[pos_potent_mover[i]] <- tt.all[pos_potent_mover[i]]
        }
      }
    } 
    
    #update results tracker
    results_tracker_old <- results_tracker_new
    
  }
  results_df <- as.data.frame(results_m[1:results_tracker_old,])
  results_df$esize <- questions.esize[match(results_df$q_id, questions.q_id)]
  return(list(scientist_df$payoff, results_df))
} 

###################
###Evolutionary Simulation###
###################

run_complexsim <- function(LIFE, MAX_PER_Q, G, R, SCS, SCS2, EXP_RATE, SCOOPED_DECAY, B_NEG) {
  
  lifespan <- LIFE #life of 1 generation of scientists
  popsize <- num_players # sets population size, from global parameter "num_players"
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




