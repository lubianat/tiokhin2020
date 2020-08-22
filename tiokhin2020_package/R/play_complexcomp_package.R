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
#' @param max_scientists_per_q max_scientists_per_q
#' @param startup_cost startup_cost
#' @param sample_cost sample_cost
#' @param exp_shape exp_shape
#' @param decay decay
#' @param b_neg b_neg
#' @param abandon_prob abandon_prob
#' @param num_scientists num_scientists
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
           max_scientists_per_q,
           startup_cost,
           sample_cost,
           exp_shape,
           decay,
           b_neg,
           abandon_prob,
           num_scientists,
           min_sample_size,
           max_sample_size) {
   
    sample_sizes <- get_sample_sizes(evolution,
                                     ss, 
                                     num_scientists,
                                     min_sample_size, 
                                     max_sample_size)
    
    ids_for_scientists <- get_ids_for_scientists(sample_sizes) 
    initial_payoffs <- get_initial_payoffs(sample_sizes)

    scientist_df <- get_scientists_data_frame(ids_for_scientists,
                                              sample_sizes,
                                              abandon_prob,
                                              initial_payoffs,
                                              num_scientists)
   
    number_of_questions <- get_number_of_questions(lifespan, 
                                                    startup_cost, 
                                                    num_scientists)

    ids_for_questions <- get_question_ids(number_of_questions)

    
    scientist_df <-
      assign_scientists_to_questions(scientist_df,
                                     ids_for_questions,
                                     max_scientists_per_q,
                                     ids_for_scientists)
    
    questions_e_size <- get_questions_e_size(number_of_questions,
                                             exp_shape)

    results_matrix <- get_results_matrix(number_of_questions,
                                         max_scientists_per_q)
    
    current_time_period <- 1
    time_cost_for_each_question <- sample_sizes * sample_cost + startup_cost
    time_cost_for_each_question_at_baseline <- time_cost_for_each_question
    
    previously_published_questions <- vector()
    results_tracker_old <- 0
    
    while (current_time_period < lifespan) {
      time_to_next_event <- min(c(time_cost_for_each_question, lifespan - current_time_period))
      time_cost_for_each_question <- time_cost_for_each_question - time_to_next_event
      current_time_period <- current_time_period + time_to_next_event
      
      if (current_time_period >= lifespan) {
        break
      }
      
      testers_ids <- get_tester_ids(scientist_df, time_cost_for_each_question)
      number_of_testers <- length(testers_ids)
      
     questions_they_are_working_on <-
        get_questions_they_are_working_on(scientist_df, testers_ids)
     
      sample_size_of_testers <- scientist_df$ss[testers_ids]
      
      powers <-
        get_t_test_powers(sample_size_of_testers,
                          questions_e_size,
                          questions_they_are_working_on)
      
      results_scientists_got <-
        check_results_scientists_got(number_of_testers, powers)
      
      for (i in 1:number_of_testers) {
        num_prior <-
          count_all_prior_published_results_for_questions(previously_published_questions,
                                                          questions_they_are_working_on,
                                                          i)
        novelty_of_result <- calculate_novelty(num_prior, decay)
        payoff <-
          calculate_payoff(results_scientists_got, i, novelty_of_result, b_neg)
        
        scientist_df <-
          add_payoff_to_that_tester(scientist_df, testers_ids, i, payoff)
      }
      
      previously_published_questions <-
        c(previously_published_questions,
          questions_they_are_working_on)
      
      results_tracker_new <-
        results_tracker_old + number_of_testers
      
      
      index_to_update <-
        (results_tracker_old + 1):results_tracker_new
      set_of_results <-
        c(
          questions_they_are_working_on,
          testers_ids,
          sample_size_of_testers,
          results_scientists_got
        )
      results_matrix[index_to_update,] <- set_of_results
      
      #subset of new question space to search
      largest_question_available <-
        max(scientist_df$question) + number_of_testers
      
      max_previously_published_questions <-
        max(previously_published_questions)
      
      #move scientists who published to subsequent questions
      questions_n_on_q <-
        get_questions_n_on_q(number_of_questions, scientist_df)
      
      for (i in 1:number_of_testers) {
        next_question <- get_next_question(
          questions_n_on_q,
          largest_question_available,
          max_scientists_per_q,
          ids_for_questions,
          max_previously_published_questions
        )
        
        questions_n_on_q[questions_they_are_working_on[i]] <-
          questions_n_on_q[questions_they_are_working_on[i]] - 1
        
        questions_n_on_q[next_question] <-
          questions_n_on_q[next_question] + 1
        
        scientist_df$question[testers_ids[i]] <-
          next_question #move scientist to new question
      }
      
      #reset the time until sampling for the subset of scientists who sampled
      time_cost_for_each_question[testers_ids] <- time_cost_for_each_question_at_baseline[testers_ids]
      
      #update positions of scientists who are working on questions where there just was published result
      pos_potent_mover <- get_potent_mover(scientist_df, questions_they_are_working_on, testers_ids)
      num_potent_movers <- length(pos_potent_mover)
      
      # limit search for new questions to the smaller subset of all q's that could potentially be moved to, for movers
      largest_q_avail_movers <-
        largest_question_available + num_potent_movers
      
      if (num_potent_movers > 0) {
        for (i in 1:num_potent_movers) {
          #questions that the scooped scientist is working on
          question_of_scoopee <-
            scientist_df$question[pos_potent_mover[i]]
          #id of the scooper
          scooper_id <-
            testers_ids[questions_they_are_working_on == question_of_scoopee]
          #take max of scooper current questions
          ineligible_max <-
            max(scientist_df$question[scientist_df$sci_id %fin% scooper_id])
          
          #number of new results that have been published on that scientist's question
          n_new <- length(scooper_id)
          
          if (any(runif(n_new, 0, 1) < scientist_df$abandon_prob[pos_potent_mover[i]])) {
            dum1 <-
              questions_n_on_q[1:largest_q_avail_movers] < max_scientists_per_q
            dum2 <-
              ids_for_questions[1:largest_q_avail_movers] > ineligible_max
            dum <- dum1 & dum2
            next_question <- match(TRUE, dum)
            questions_n_on_q[question_of_scoopee] <-
              questions_n_on_q[question_of_scoopee] - 1 #subtract 1 from current 1
            questions_n_on_q[next_question] <-
              questions_n_on_q[next_question] + 1 # add 1 to next q
            scientist_df$question[pos_potent_mover[i]] <-
              next_question #move scientist to new question
            #reset the time until sampling for the subset of scientists who moved
            time_cost_for_each_question[pos_potent_mover[i]] <-
              time_cost_for_each_question_at_baseline[pos_potent_mover[i]]
          }
        }
      }
      
      results_tracker_old <- results_tracker_new
      
    }
    
    results_df <-
      as.data.frame(results_matrix[1:results_tracker_old,])
    results_df$esize <-
      questions_e_size[match(results_df$q_id, ids_for_questions)]
    return(list(scientist_df))
  }


get_sample_sizes <-
  function(evolution,
           ss,
           num_scientists,
           min_sample_size,
           max_sample_size) {
    if (evolution == 1) {
      samplesizes <- ss
    } else {
      samplesizes <-
        round(runif(num_scientists, min_sample_size, max_sample_size))
    }
    return(samplesizes)
  }

get_ids_for_scientists <- function(sample_sizes) {
  1:length(sample_sizes)
}

get_initial_payoffs <- function(sample_sizes) {
  rep(0.0000001, length = length(sample_sizes))
}

get_number_of_questions <-
  function(lifespan, startup_cost, num_scientists) {
    round((lifespan / (startup_cost + 2)) * num_scientists) + 1000
  }

get_question_ids <- function(number_of_questions) {
  seq_len(number_of_questions)
}

assign_scientists_to_questions <-
  function(scientist_df,
           questions_q_id,
           max_scientists_per_q,
           ids_for_scientists) {
    scientist_df$question[ids_for_scientists] <-
      rep(questions_q_id, each = max_scientists_per_q)[ids_for_scientists]
    return(scientist_df)
  }

get_scientists_data_frame <-
  function(ids_for_scientists,
           sample_sizes,
           abandon_prob,
           initial_payoffs,
           num_scientists) {
    data.frame(
      sci_id = ids_for_scientists,
      ss = sample_sizes,
      question = 0,
      abandon_prob = abandon_prob,
      payoff = initial_payoffs,
      scooped = 0,
      num_scientists = num_scientists
    )
  }

get_questions_e_size <- function(number_of_questions, exp_shape) {
  round(rexp(number_of_questions, exp_shape), 1)
}

get_results_matrix <-
  function(number_of_questions, max_scientists_per_q) {
    as.matrix(data.frame(
      q_id = rep(0, number_of_questions * max_scientists_per_q),
      sci_id = 0,
      ss = 0,
      result = 0
    ))
  }

get_questions_n_on_q <-
  function(number_of_questions, scientist_df) {
    questions_n_on_q <- rep(0, number_of_questions)
    sci_per_q <- questions_n_on_q
    table_of_questions <- table(scientist_df$question)
    sci_per_q[as.integer(names(table_of_questions))] <-
      as.vector(table_of_questions)
    questions_n_on_q <- sci_per_q
  }

get_tester_ids <- function(scientist_df, tracker_time) {
  c(scientist_df$sci_id[tracker_time == 0])
}

get_questions_they_are_working_on <-
  function(scientist_df, sampler_ids) {
    c(scientist_df$question[sampler_ids])
  }

check_results_scientists_got <- function(num_testers, powers) {
  runif(num_testers, 0, 1) < powers
}

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

calculate_payoff <-
  function(results_scientists_got,
           i,
           novelty_of_result,
           b_neg) {
    if (results_scientists_got[i]) {
      payoff <- novelty_of_result
    } else{
      payoff <- novelty_of_result * b_neg
    }
  }

count_all_prior_published_results_for_questions <-
  function(prev_pub_q,
           questions_they_are_working_on,
           i) {
    length(prev_pub_q[prev_pub_q == questions_they_are_working_on[i]])
  }

calculate_novelty <- function(num_prior, decay) {
  (1 / (1 + num_prior)) ^ decay
}

add_payoff_to_that_tester <-
  function(scientist_df, sampler_ids, i, payoff) {
    scientist_df$payoff[sampler_ids[i]] <-
      scientist_df$payoff[sampler_ids[i]] + payoff
    return(scientist_df)
  }

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


get_potent_mover <- function(scientist_df, questions_they_are_working_on, sampler_ids) {
  which(
    scientist_df$question %fin% questions_they_are_working_on &
      !scientist_df$sci_id %fin% sampler_ids
  )
}