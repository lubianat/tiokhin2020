#' @import pwr
#' @import plyr
#' @import dplyr
#' @import ggplot2
#' @import viridisLite
#' @import viridis
#' @import RColorBrewer
#' @import profvis
#' @import fastmatch
#' @importFrom stats rexp runif
NULL

#' play_complexcomp
#'
#' Function to play scientists against one another,
#' including measure of number of scoops
#'
#' @param evolution evolution
#' @param lifespan lifespan
#' @param ss ss
#' @param max_players_per_q max_players_per_q
#' @param startup_cost startup_cost
#' @param sample_cost sample_cost
#' @param exp_shape exp_shape
#' @param decay decay
#' @param b_neg b_neg
#' @param abandon_prob abandon_prob
#' @param num_players num_players
#' @param min_sample_size min_sample_size
#' @param max_sample_size max_sample_size
#' @import pwr
#' @return
#' @export
#'
play_complexcomp <-
  function(evolution,
           lifespan,
           ss,
           max_players_per_q,
           startup_cost,
           sample_cost,
           exp_shape,
           decay,
           b_neg,
           abandon_prob,
           num_players,
           min_sample_size,
           max_sample_size) {

    sample_sizes <- get_sample_sizes(evolution, ss, num_players, min_sample_size, max_sample_size)
    ids_for_scientists <- 1:length(sample_sizes)
    initial_payoffs <- rep(0.0000001, length = length(sample_sizes))

    scientist_df <- get_scientists_data_frame(ids_for_scientists, sample_sizes, abandon_prob, initial_payoffs, num_players)

    number_of_questions <- get_number_of_questions(lifespan, startup_cost, num_players)
    
    results_m <- get_results_matrix(number_of_questions, max_players_per_q)

    question_ids <- seq_len(number_of_questions)
    
    scientist_df <-
      assign_scientists_to_questions(scientist_df, question_ids, max_players_per_q, ids_for_scientists)
    

    questions_e_size <- round(rexp(number_of_questions, exp_shape), 1)

    time_period <- 1
    baseline_time <- scientist_df$ss * sample_cost + startup_cost
    tracker_time <- scientist_df$ss * sample_cost + startup_cost

    questions_n_on_q <- get_questions_n_on_q(number_of_questions, scientist_df)
    
    #will store unique q_ids for published results
    prev_pub_q <- vector()

    results_tracker_old <- 0

    while (time_period < lifespan) {
      time_to_next_event <- min(c(tracker_time, lifespan - time_period))
      tracker_time <- tracker_time - time_to_next_event
      time_period <- time_period + time_to_next_event

      if (time_period >= lifespan) {
        break
      }

      sampler_ids <- get_sampler_ids(scientist_df, tracker_time) 

      num_samplers <- length(sampler_ids)
      
      questions_they_are_working_on <- get_questions_they_are_working_on(scientist_df, sampler_ids)

      ss_of_samplers <- scientist_df$ss[sampler_ids]

      powers <- get_t_test_powers(ss_of_samplers, questions_e_size, questions_they_are_working_on)
     
      res <-
        runif(num_samplers, 0, 1) < powers #checks what result those players got

      for (i in 1:num_samplers) {
        # find all prior published results (true or false) on the corresponding question,
        num_prior <- length(prev_pub_q[prev_pub_q == questions_they_are_working_on[i]])

        # how novel is the new result
        novelty_of_res <- (1 / (1 + num_prior)) ^ decay

        ### Adjust the below payoff functions depending on which one you are interested in ###

        # calculate payoff
        if (res[i]) {
          payoff <- novelty_of_res
        } else{
          payoff <- novelty_of_res * b_neg
        }
        ### End selection of payoff functions ###

        #add payoff to data frame for that sampler
        scientist_df$payoff[sampler_ids[i]] <-
          scientist_df$payoff[sampler_ids[i]] + payoff
      }

      #update vector for previously published questions
      prev_pub_q <- c(prev_pub_q, questions_they_are_working_on)

      #updated results tracker
      results_tracker_new <- results_tracker_old + num_samplers

      #update results_m
      results_m[(results_tracker_old + 1):results_tracker_new, ] <-
        c(questions_they_are_working_on, sampler_ids, ss_of_samplers, res)

      #subset of new question space to search
      largest_q_avail <- max(scientist_df$question) + num_samplers

      #largest question in previously pub questions
      max_prev_pub <- max(prev_pub_q)

      #move scientists who published to subsequent questions
      for (i in 1:num_samplers) {
        dum1 <- questions_n_on_q[1:largest_q_avail] < max_players_per_q
        dum2 <- question_ids[1:largest_q_avail] > max_prev_pub
        dum <- dum1 & dum2
        next_q <- match(TRUE, dum)
        questions_n_on_q[questions_they_are_working_on[i]] <-
          questions_n_on_q[questions_they_are_working_on[i]] - 1 #subtract 1 from current 1
        questions_n_on_q[next_q] <-
          questions_n_on_q[next_q] + 1 # add 1 to next q
        scientist_df$question[sampler_ids[i]] <-
          next_q #move scientist to new question
      }

      #reset the time until sampling for the subset of players who sampled
      tracker_time[sampler_ids] <- baseline_time[sampler_ids]

      #update positions of scientists who are working on questions where there just was published result
      pos_potent_mover <-
        which(scientist_df$question %fin% questions_they_are_working_on &
                !scientist_df$sci_id %fin% sampler_ids)
      num_potent_movers <- length(pos_potent_mover)

      # limit search for new questions to the smaller subset of all q's that could potentially be moved to, for movers
      largest_q_avail_movers <- largest_q_avail + num_potent_movers

      if (num_potent_movers > 0) {
        #for each scientist, check the number of new results for their question. They then move to next question
        #probabalistically. The larger the number of new results, the higher the probability that they move.

        for (i in 1:num_potent_movers) {
          #questions that the scooped scientist is working on
          question_of_scoopee <-
            scientist_df$question[pos_potent_mover[i]]
          #id of the scooper
          scooper_id <- sampler_ids[questions_they_are_working_on == question_of_scoopee]
          #take max of scooper current questions
          ineligible_max <-
            max(scientist_df$question[scientist_df$sci_id %fin% scooper_id])

          #number of new results that have been published on that scientist's question
          n_new <- length(scooper_id)

          if (any(runif(n_new, 0, 1) < scientist_df$abandon_prob[pos_potent_mover[i]])) {
            dum1 <-
              questions_n_on_q[1:largest_q_avail_movers] < max_players_per_q
            dum2 <-
              question_ids[1:largest_q_avail_movers] > ineligible_max
            dum <- dum1 & dum2
            next_q <- match(TRUE, dum)
            questions_n_on_q[question_of_scoopee] <-
              questions_n_on_q[question_of_scoopee] - 1 #subtract 1 from current 1
            questions_n_on_q[next_q] <-
              questions_n_on_q[next_q] + 1 # add 1 to next q
            scientist_df$question[pos_potent_mover[i]] <-
              next_q #move scientist to new question
            #reset the time until sampling for the subset of players who moved
            tracker_time[pos_potent_mover[i]] <- baseline_time[pos_potent_mover[i]]
          }
        }
      }

      results_tracker_old <- results_tracker_new

    }
    results_df <- as.data.frame(results_m[1:results_tracker_old, ])
    results_df$esize <-
      questions_e_size[match(results_df$q_id, question_ids)]
    return(list(scientist_df))
  }


get_sample_sizes <- function(evolution, ss, num_players, min_sample_size, max_sample_size) {
  if (evolution == 1) {
    samplesizes <- ss
  } else {
    samplesizes <-
      round(runif(num_players, min_sample_size, max_sample_size))
  }
  return(samplesizes)
}

get_number_of_questions <- function(lifespan, startup_cost, num_players) {
  round((lifespan / (startup_cost + 2)) * num_players) + 1000
}

assign_scientists_to_questions <- function(scientist_df, questions_q_id, max_players_per_q, ids_for_scientists) {
  scientist_df$question[ids_for_scientists] <- rep(questions_q_id, each = max_players_per_q)[ids_for_scientists]
  return(scientist_df)
}

get_scientists_data_frame <- function(ids_for_scientists, sample_sizes, abandon_prob, initial_payoffs, num_players) {
  data.frame(
    sci_id = ids_for_scientists,
    ss = sample_sizes,
    question = 0,
    abandon_prob = abandon_prob,
    payoff = initial_payoffs,
    scooped = 0,
    num_players = num_players
  )
}

get_results_matrix <- function(number_of_questions, max_players_per_q) {
  as.matrix(data.frame(
    q_id = rep(0, number_of_questions * max_players_per_q),
    sci_id = 0,
    ss = 0,
    result = 0
  ))
}

get_questions_n_on_q <- function(number_of_questions, scientist_df) {
  questions_n_on_q <- rep(0, number_of_questions)
  sci_per_q <- questions_n_on_q
  table_of_questions <- table(scientist_df$question)
  sci_per_q[as.integer(names(table_of_questions))] <- as.vector(table_of_questions)
  questions_n_on_q <- sci_per_q
}

get_sampler_ids <- function(scientist_df, tracker_time) {
  c(scientist_df$sci_id[tracker_time == 0]) 
}

get_questions_they_are_working_on <- function(scientist_df, sampler_ids) {
  c(scientist_df$question[sampler_ids])
}

get_t_test_powers <- function(ss_of_samplers, questions_e_size, questions_they_are_working_on) {
  as.numeric(
    pwr.t.test(
      n = ss_of_samplers,
      d = questions_e_size[questions_they_are_working_on],
      sig.level = 0.05,
      type = "two.sample"
    )[4]$power
  )
}