context("integration_test")


data("base_sim")

test_that("play_complexcomp works", {
  set.seed(3)
  evolution <- FALSE
  lifespan <- 15000
  ss <- NA
  max_scientists_per_q <- 120
  startup_cost <- 100
  sample_cost <- 1
  exp_shape <- 5
  decay <- 10
  b_neg <- 1
  abandon_prob <- 0.5
  num_scientists <- 120
  min_sample_size <- 3
  max_sample_size <- 12

  test_sim <- play_complexcomp(
    evolution = evolution,
    lifespan = lifespan,
    ss = ss,
    max_scientists_per_q = max_scientists_per_q,
    startup_cost = startup_cost,
    sample_cost = sample_cost,
    exp_shape = exp_shape,
    decay = decay,
    b_neg = b_neg,
    abandon_prob = abandon_prob,
    num_scientists = num_scientists,
    min_sample_size = min_sample_size,
    max_sample_size = max_sample_size
  )
  expect_equal(test_sim, base_sim)
})

context("get sample sizes")
test_that("get_sample_sizes works", {
  sample_sizes_one_evolution <- get_sample_sizes(
    evolution = 1,
    ss = 3,
    num_scientists = 120,
    min_sample_size = 3,
    max_sample_size = 12
  )
  expect_equal(sample_sizes_one_evolution, 3)

  set.seed(3)
  sample_sizes_zero_evolution <- get_sample_sizes(
    evolution = 0,
    ss = 3,
    num_scientists = 120,
    min_sample_size = 3,
    max_sample_size = 12
  )

  expect_equal(sample_sizes_zero_evolution[1:5], c(5, 10, 6, 6, 8))
})
