% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_complexsim.R
\name{run_complexsim}
\alias{run_complexsim}
\title{run_complexsim}
\usage{
run_complexsim(
  lifespan,
  max_scientists_per_q,
  generations,
  simulation_repeats,
  startup_costs,
  sample_costs,
  exp_rate,
  scooped_decay,
  payoff_to_being_second,
  num_scientists,
  min_sample_size,
  max_sample_size
)
}
\arguments{
\item{lifespan}{life of 1 generation of scientists}

\item{max_scientists_per_q}{number of competitors}

\item{generations}{number of generations}

\item{simulation_repeats}{number of simulations for every unique combo of effect and startup cost}

\item{startup_costs}{startup_costs}

\item{sample_costs}{sample_costs}

\item{exp_rate}{exp_rate}

\item{scooped_decay}{the decay for payoff for a question that has been answered before}

\item{payoff_to_being_second}{payoff_to_being_second}

\item{num_scientists}{num_scientists}

\item{min_sample_size}{min_sample_size}

\item{max_sample_size}{max_sample_size}
}
\value{

}
\description{
Function to run complex simulation
}
