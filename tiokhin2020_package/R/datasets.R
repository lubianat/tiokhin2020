#' Example simulation result
#'
#' play_complex_comp was run with set.seed(3)
#' Result was saved in data using
#' set.seed(3)
#' base_sim <-play_complexcomp(evolution = 0,
#'                             lifespan = 15000,
#'                             ss = NA,
#'                             max_scientists_per_q = 120,
#'                             startup_cost =   100,
#'                             sample_cost =    1,
#'                             exp_shape =      5,
#'                             decay =         10,
#'                             b_neg =      1,
#'                             abandon_prob =    0.5,
#'                             num_scientists = 120,
#'                             min_sample_size = 3,
#'                             max_sample_size = 12)
#' save(base_sim, file = "data/base_sim.rda")
#'
#' @name base_sim
#' @docType data
#' @format A list containing 1 dataframe
#' @keywords datasets
#' @examples
#' data(base_sim)
#' base_sim
"base_sim"
