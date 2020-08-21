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

    if (evolution == 1) {
      samplesizes <- ss
    } else {
      samplesizes <-
        round(runif(num_players, min_sample_size, max_sample_size))
    }

    ids_for_scientists <- 1:length(samplesizes)

    number_of_questions <-
      round((lifespan / (startup_cost + 2)) * num_players) + 1000

    initial_payoffs <- rep(0.0000001, length = length(samplesizes))

    scientist_df <- data.frame(
      sci_id = ids_for_scientists,
      ss = samplesizes,
      question = 0,
      abandon_prob = abandon_prob,
      payoff = initial_payoffs,
      scooped = 0,
      num_players = num_players
    )

    results_m <-
      as.matrix(data.frame(
        q_id = rep(0, number_of_questions * max_players_per_q),
        sci_id = 0,
        ss = 0,
        result = 0
      ))

    questions_q_id <- seq_len(number_of_questions)
    questions_n_on_q <- rep(0, number_of_questions)
    questions_e_size <- round(rexp(number_of_questions, exp_shape), 1)

    time_period <- 1
    baseline_time <- scientist_df$ss * sample_cost + startup_cost
    tracker_time <- scientist_df$ss * sample_cost + startup_cost

    #assign scientists to questions and update questions_df with scientists
    scientist_df$question[ids_for_scientists] <-
      rep(questions_q_id, each = max_players_per_q)[ids_for_scientists]

    sci_per_q <- questions_n_on_q
    tab <- table(scientist_df$question)
    sci_per_q[as.integer(names(tab))] <- as.vector(tab)
    questions_n_on_q <- sci_per_q

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

      samplers <-
        c(scientist_df$sci_id[tracker_time == 0]) #id of scientists that can sample
      num_samplers <- length(samplers)
      ques <-
        c(scientist_df$question[samplers]) #question they are working on
      ss_of_samplers <- scientist_df$ss[samplers]

      powers <-
        as.numeric(
          pwr.t.test(
            n = ss_of_samplers,
            d = questions_e_size[ques],
            sig.level = 0.05,
            type = "two.sample"
          )[4]$power
        )
      res <-
        runif(num_samplers, 0, 1) < powers #checks what result those players got

      for (i in 1:num_samplers) {
        # find all prior published results (true or false) on the corresponding question,
        num_prior <- length(prev_pub_q[prev_pub_q == ques[i]])

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
        scientist_df$payoff[samplers[i]] <-
          scientist_df$payoff[samplers[i]] + payoff
      }

      #update vector for previously published questions
      prev_pub_q <- c(prev_pub_q, ques)

      #updated results tracker
      results_tracker_new <- results_tracker_old + num_samplers

      #update results_m
      results_m[(results_tracker_old + 1):results_tracker_new, ] <-
        c(ques, samplers, ss_of_samplers, res)

      #subset of new question space to search
      largest_q_avail <- max(scientist_df$question) + num_samplers

      #largest question in previously pub questions
      max_prev_pub <- max(prev_pub_q)

      #move scientists who published to subsequent questions
      for (i in 1:num_samplers) {
        dum1 <- questions_n_on_q[1:largest_q_avail] < max_players_per_q
        dum2 <- questions_q_id[1:largest_q_avail] > max_prev_pub
        dum <- dum1 & dum2
        next_q <- match(TRUE, dum)
        questions_n_on_q[ques[i]] <-
          questions_n_on_q[ques[i]] - 1 #subtract 1 from current 1
        questions_n_on_q[next_q] <-
          questions_n_on_q[next_q] + 1 # add 1 to next q
        scientist_df$question[samplers[i]] <-
          next_q #move scientist to new question
      }

      #reset the time until sampling for the subset of players who sampled
      tracker_time[samplers] <- baseline_time[samplers]

      #update positions of scientists who are working on questions where there just was published result
      pos_potent_mover <-
        which(scientist_df$question %fin% ques &
                !scientist_df$sci_id %fin% samplers)
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
          scooper_id <- samplers[ques == question_of_scoopee]
          #take max of scooper current questions
          ineligible_max <-
            max(scientist_df$question[scientist_df$sci_id %fin% scooper_id])

          #number of new results that have been published on that scientist's question
          n_new <- length(scooper_id)

          if (any(runif(n_new, 0, 1) < scientist_df$abandon_prob[pos_potent_mover[i]])) {
            dum1 <-
              questions_n_on_q[1:largest_q_avail_movers] < max_players_per_q
            dum2 <-
              questions_q_id[1:largest_q_avail_movers] > ineligible_max
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
      questions_e_size[match(results_df$q_id, questions_q_id)]
    return(list(scientist_df))
  }
