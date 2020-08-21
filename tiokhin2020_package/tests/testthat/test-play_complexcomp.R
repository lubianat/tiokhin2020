library(tiokhin2020)
context("integration_test")


data("base_sim")

test_that("play_complexcomp works", {
  set.seed(3)
  test_sim <-play_complexcomp(evolution = 0,
                                                lifespan = 15000,
                                                ss = NA, 
                                                max_players_per_q = 120,
                                                startup_cost =   100,
                                                sample_cost =    1,
                                                exp_shape =      5,
                                                decay =         10,
                                                b_neg =      1,
                                                abandon_prob =    0.5,
                                                num_players = 120,
                                                min_sample_size = 3,
                                                max_sample_size = 12)
  expect_equal(test_sim, base_sim)
})
