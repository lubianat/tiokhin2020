test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

library(tiokhin2020)

play_complexcomp(evolution = 0,
                 lifespan = 10,
                 ss = NA, 
                 max_players_per_q = 120,
                 startup_cost =   100,
                 sample_cost =    1,
                 exp_shape =      5,
                 decay =         10,
                 b_neg =      1,
                 abandon_prob =    0.5)
