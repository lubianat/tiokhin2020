library(pwr)
library(plyr)
library(dplyr)
library(ggplot2)
library(viridisLite)
library(viridis)
library(RColorBrewer)
library(profvis)
library(fastmatch)

###### Function to play scientists against one another, including measure of number of scoops #####

play_complexcomp <- function(evolution, lifespan, ss, max_players_per_q, startup_cost, sample_cost, exp_shape, decay,
                             b_neg, abandon_prob){
  
  if(evolution == 1) {
    samplesizes <- ss
  } else {
    samplesizes <- round(runif(num_players, min_sample_size, max_sample_size)) # sample sizes
  }
  
  sci_ids <- 1:length(samplesizes)
  
  num_questions <- round((lifespan / (startup_cost + 2)) * num_players) + 1000
  
  #set payoffs to 0 at beginning of each run
  payoff_v <- rep(0.0000001, length = length(samplesizes)) 
  
  scientist_df <- data.frame(sci_id = sci_ids,
                             ss = samplesizes, 
                             question = 0,
                             abandon_prob = abandon_prob, 
                             payoff = payoff_v, 
                             scooped = 0, 
                             num_players = num_players)
  
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
  
  results_tracker_old <- 0
  
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
      
      ### Adjust the below payoff functions depending on which one you are interested in ###
      
      # calculate payoff
      if(res[i]){
        payoff <- novelty_of_res
      } else{
        payoff <- novelty_of_res * b_neg
      }
      ### End selection of payoff functions ###
      
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
    
    results_tracker_old <- results_tracker_new
    
  }
  results_df <- as.data.frame(results_m[1:results_tracker_old,])
  results_df$esize <- questions.esize[match(results_df$q_id, questions.q_id)]
  return(list(scientist_df))
} 

##### Run the simulation #####

#set simulation parameters
popsize <- c(120, 960)
comps <- c(2, 8)
dcy <- c(0.15, 10)
min_sample_size <- 3
max_sample_size <- 12

for(c in comps){
for(d in dcy){
for(players in popsize){
  
  dum_list <- vector(mode = "list", length = 200)
  tracker <- 1
  
  num_players <- players
  aban_rep_dum <- players / 4
  A_PROB <- c(rep(0, aban_rep_dum), rep(0.33, aban_rep_dum), rep(0.66, aban_rep_dum), rep(1, aban_rep_dum))
  
  if(players == 120){
    for(rep in 1:25){
      #                        evol, life,    ss, max_players_per_q, SUC, SAMP_C,  exp_shape, decay, b_neg, abandon
      RR <- play_complexcomp(0,    15000,    NA,       c,         100,       1,     5,        d,     1,   A_PROB)
      dum_list[tracker] <- RR
      tracker <- tracker + 1
    }
    
  } else if(players == 960){
    for(rep in 1:10){
      #                        evol, life,    ss, max_players_per_q, SUC, SAMP_C,  exp_shape, decay, b_neg, abandon
      RR <- play_complexcomp(0,    15000,    NA,       c,         100,       1,     5,        d,     1,   A_PROB)
      dum_list[tracker] <- RR
      tracker <- tracker + 1
    }
  }
  
  df <- bind_rows(dum_list)

agg.dummy <- group_by(df, abandon_prob) %>% summarise(payoff_mean = mean(payoff))
df$mean_payoff <- NA

for(i in unique(df$abandon_prob)){
  df$mean_payoff[df$abandon_prob == i] <- agg.dummy$payoff_mean[agg.dummy$abandon_prob == i]
}

df$num_players <- as.factor(df$num_players)
levels(df$num_players) <- c(paste("Popsize:", df$num_players))

aa <- ggplot(data = df,
             aes(x = as.factor(abandon_prob), y = payoff, 
                 color = as.factor(abandon_prob), 
                 group = as.factor(abandon_prob))) +
  geom_jitter(width = 0.3) + geom_violin() + 
  stat_summary(fun.y = "mean", geom = "point", 
               shape = 20, size = 5, color = "black" ) +
  facet_wrap( ~ num_players, ncol = 3) +
  theme_bw(base_size=14) +
  ylab("Payoff") + 
  xlab("Abandonment Probability") + 
  scale_colour_brewer(name="Abandonment \nProbability", palette = "PuRd") + 
  ggtitle(paste('SS: 3 to 12   Normal Payoffs   SUC: 100   Bneg: 1  Comp:', c, " Decay:", d)) +
  theme(plot.title = element_text(hjust = 0.5))
plot(aa)

}}}



