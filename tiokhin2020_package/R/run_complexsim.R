#' run_complexsim
#'
#' Function to run complex simulation
#'
#'
#' @param lifespan life of 1 generation of scientists
#' @param max_scientists_per_q number of competitors
#' @param generations number of generations
#' @param simulation_repeats number of simulations for every unique combo of effect and startup cost
#' @param startup_costs  startup_costs
#' @param sample_costs sample_costs
#' @param exp_rate exp_rate
#' @param scooped_decay the decay for payoff for a question that has been answered before
#' @param b_neg payoff for negative result
#' @param num_scientists num_scientists
#' @param min_sample_size min_sample_size
#' @param max_sample_size max_sample_size
#' @param min_aban min_aban
#' @param max_aban max_aban
#'
#' @return
run_complexsim <- function(lifespan,
                           max_scientists_per_q,
                           generations,
                           simulation_repeats,
                           startup_costs,
                           sample_costs,
                           exp_rate,
                           scooped_decay,
                           b_neg,
                           num_scientists,
                           min_sample_size,
                           max_sample_size,
                           min_aban = 0,
                           max_aban = 1) {
  max_scientists_per_q_list <- max_scientists_per_q

  scooped_decay <- scooped_decay
  b_neg <- b_neg

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

  single_region <- length(max_scientists_per_q_list) == 1 && length(simulation_repeats) == 1 && length(startup_costs) == 1 && length(sample_costs) == 1 &&
    length(exp_rate) == 1 && length(scooped_decay) == 1 && length(b_neg) == 1

  mean_ss <- rep(0, generations)
  lower_ss <- rep(0, generations)
  upper_ss <- rep(0, generations)
  mean_aban <- rep(0, generations)
  lower_aban <- rep(0, generations)
  upper_aban <- rep(0, generations)

  # tracker
  zz <- 1

  for (rate in exp_rate) {
    for (sample_cost in sample_costs) {
      for (startup_cost in startup_costs) {
        for (max_scientists_per_q in max_scientists_per_q_list) {
          for (dcy in scooped_decay) {
            for (bn in b_neg) {

              # list to store all results across repeats
              results_list <- vector("list", simulation_repeats)

              for (rep in 1:simulation_repeats) {

                # initialize the population, for each repeat
                rounded_popsize <- round(num_scientists / max_scientists_per_q) * max_scientists_per_q
                sample_size <- round(runif(
                  rounded_popsize,
                  min_sample_size,
                  max_sample_size
                ))
                abandon_probabilities <- runif(rounded_popsize, min_aban, max_aban)

                # start looping
                for (gen in 1:generations) {

                  # play scientists against each other
                  print(lifespan)
                  outcome_list <- play_complexcomp(
                    evolution = TRUE,
                    lifespan = lifespan,
                    num_scientists = num_scientists,
                    startup_cost = startup_cost,
                    max_scientists_per_q = max_scientists_per_q,
                    sample_cost = sample_cost,
                    exp_shape = exp_rate,
                    decay = dcy,
                    b_neg = bn,
                    abandon_prob = abandon_probabilities,
                    min_sample_size = min_sample_size,
                    max_sample_size = max_sample_size,
                    ss = sample_size
                  )

                  fitness <- outcome_list[[1]]$payoff

                  normalized_fitness <- fitness / sum(fitness)
                  
                  
                  sample_size <- select_sample_sizes_that_reproduce(sample_size,
                                                                   rounded_popsize,
                                                                   normalized_fitness)

                  sample_size <- mutate_sample_sizes(sample_size,
                                                     rounded_popsize)

  
                  abandon_probabilities <- get_abandon_probabilities_based_on_fitness(abandon_probabilities,
                                                                                      rounded_popsize,
                                                                                      normalized_fitness)

                  abandon_probabilities <- mutate_abandon_probabilities(abandon_probabilities,
                                                                        rounded_popsize)

                  # save state of the population sample sizes and abandonment probabilities
                  if (single_region) {
                    mean_ss[gen] <- mean_ss[gen] + mean(sample_size)
                    dum <- quantile(sample_size, c(0.025, 0.975))
                    lower_ss[gen] <- lower_ss[gen] + dum[[1]]
                    upper_ss[gen] <- upper_ss[gen] + dum[[2]]

                    mean_aban[gen] <- mean_aban[gen] + mean(abandon_probabilities)
                    dum <- quantile(abandon_probabilities, c(0.025, 0.975))
                    lower_aban[gen] <- lower_aban[gen] + dum[[1]]
                    upper_aban[gen] <- upper_aban[gen] + dum[[2]]
                  }
                } # end of generation

                eq.samplecost <- c(eq.samplecost, sample_cost)
                eq.startupcost <- c(eq.startupcost, startup_cost)
                eq.exprate <- c(eq.exprate, rate)
                eq.decay <- c(eq.decay, dcy)
                eq.samplesize <- c(eq.samplesize, mean(sample_size))
                eq.totalfitness <- c(eq.totalfitness, sum(fitness))
                eq.maxperq <- c(eq.maxperq, max_scientists_per_q)
                eq.b_neg <- c(eq.b_neg, bn)
                eq.abandonprob <- c(eq.abandonprob, mean(abandon_probabilities))
                eq.life <- c(eq.life, lifespan)
                results_list[[rep]] <- outcome_list[[2]]
              } # end of repeat

              # combine results and label df w/ accurate parameters
              eq.result.df <- bind_rows(results_list)
              eq.result.df$eq.samplecost <- sample_cost
              eq.result.df$eq.startupcost <- startup_cost
              eq.result.df$eq.exprate <- rate
              eq.result.df$eq.decay <- dcy
              eq.result.df$eq.maxperq <- max_scientists_per_q
              eq.result.df$eq.b_neg <- bn
              eq.result.df$eq.life <- lifespan

              # save eq.results.df after all repeats for one unique combination of paramaters
              res_list_all[[length(res_list_all) + 1]] <- eq.result.df
            }

            results <- data.frame(
              eq.samplesize, eq.abandonprob, eq.totalfitness,
              eq.samplecost, eq.startupcost, eq.exprate, eq.decay, eq.maxperq,
              eq.b_neg, eq.life
            )

            # save objects;
            if (single_region == FALSE) {
              save(results, file = "eq_data_sample.RData")
              save(res_list_all, file = "res_list_sample.RData")

              # update tracker
              print(paste("parameter combo #", zz))
              zz <- zz + 1
            } else {
              return(list(
                mean_ss, lower_ss, upper_ss,
                mean_aban, lower_aban, upper_aban
              ))
            }
          }
        }
      }
    }
  }
}

mutate_sample_sizes <- function(sample_size, rounded_popsize) {
  sample_size <- round(sample_size + rnorm(rounded_popsize, 0, 2))
  sample_size <- pmin(pmax(sample_size, 2), 1000)
}

select_sample_sizes_that_reproduce <- function(sample_size, rounded_popsize, normalized_fitness) {
  sample(sample_size,
         size = rounded_popsize,
         replace = TRUE,
         prob = normalized_fitness
  )
}

mutate_abandon_probabilities <- function(abandon_probabilities, rounded_popsize) {
  abandon_probabilities + rnorm(rounded_popsize, 0, 0.01)
  abandon_probabilities <- pmin(pmax(abandon_probabilities, 0), 1)
}

get_abandon_probabilities_based_on_fitness <- function(abandon_probabilities, rounded_popsize, normalized_fitness) {
  sample(abandon_probabilities,
         size = rounded_popsize,
         replace = TRUE,
         prob = normalized_fitness)
}