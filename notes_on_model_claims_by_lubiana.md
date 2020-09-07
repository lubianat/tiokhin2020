
# Model clains

* Each scientist is characterized by two parameters representing their characteristic methods: the sample size of their conducted research studies, s, and their probability of abandoning a research question when another scientist publishes a result on that question, a.


* Scientists transmit their methods to trainees, so the distributions of these parameters can evolve across generations

* Each population is initialized by sampling n integer values of s from a uniform distribution [2, 1000] and n real-numbered values of a from a uniform distribution [0, 1].

* On any given question, a scientist’s statistical power, pwr, can take on any real-numbered value in [0.05, 1].

* pwr is a function of three parameters: sample size, s, the false-positive rate, α, and the size of the effect being studied, e. pwr is calculated using a two-sample t-test, implemented with the pwr.t.test() function in the ‘pwr’ package in R3

* This effectively assumes that all research is of the form where scientists collect s independent data points from each of two populations and test for a difference between the two.

* Following convention, the level of statistical significance required for a positive result, α, remains fixed at 0.05

* We assume that there are an infinite number of research questions, each of which is characterized by an effect size e, where e represents a standardized mean difference between two populations

* we assume that the e value of each question is drawn from an exponential distribution, with a rate parameter (??) of 5 and rounded to the nearest 0.1

* Each research question has a unique id (e.g. 1, 2, 3, ….) and a maximum of m scientists can work on any given question

* A scientist begins their career on the smallest-numbered open research question (i.e., the smallest numbered question occupied by fewer than m other scientists)

* We do this to avoid unrealistic outcomes (e.g., all scientists working on a single question; all scientists working on different questions) 

**This is not true! All scientistists may start on the same question, and that actually happens in (at least) one of the simulations.** 


* Each scientist’s career lasts T = 15,000 timesteps

* In one specific case of low startup costs (c = 10, see below), career length was reduced to 5000 time-steps for computational efficiency, without affecting the simulation results

* Once their career has started, a scientist collects data until they reach their desired sample size as dictated by their respective s value. The number of time steps required to do this. cs represents the sample cost: the number of time steps needed to acquire one data point (fixed at 1). c represents the startup cost: the number of time steps needed to set up a study.

* We assume that c is independent of s.

* For questions with a true effect (e > 0), a scientist obtains a statistically-significant result with probability pwr.

* For questions with no true effect (e = 0), a scientist obtains a statistically-significant result with probability α.

* We assume that the results of all completed studies are published, but that there may be bias against negative results (see below).

* Once a scientist publishes a result, two parameters determine the scientist’s payoff: the novelty of the result, v, and whether the result is positive (i.e., significant) or negative (i.e., non-significant)

* The novelty of a result is calculated as: ni = (1/(1+ number_of_prior_results)^d where d (the decay) determines the severity of the cost of being scooped

* For negative results, scientists receive payoff vbn, where 0 ≤ bn ≤ 1

* After publishing, the scientist moves to the next open research question (i.e., one with fewer than m other scientists working on it)

* All other scientists working on the question corresponding to the newly-published result abandon that question with a probability determined by their individual a value.

**Innacurate.**
In the code is possible that multiple scientists publish at exactly the same time. The way it is written gives the impression that publishing at the same time is not possible. 

As of note, it could be changed in the code by adding an element of randomness: it could take a bit more or a bit less depending on a "luck" parameter. 

* In order to prevent scientists from being persistently “stuck” on the same questions as the scientist who just scooped them, we assume that scientists who abandon move to a different question than the one assigned to their scooper

* This process repeats until scientists reach the end of their careers, at which point all scientists retire.

* Upon retiring, each scientist’s “fitness” is calculated as proportional to the total number of points that they acquired during their career.

* A new (non-overlapping) generation of scientists is then created, with their s and a values sampled from members of the previous generation, weighted by fitness.

* We assume that inheritance is noisy: once a parent is selected to “reproduce,” the sample size, s, of its “offspring” scientist is drawn from a normal distribution with a mean corresponding to the parent’s value and a standard deviation of 2

* Offspring s values are rounded to the nearest integer and truncated to remain in [2, 1000].

* Values of s < 2 are set to 2 because two-sample t-tests require at least 2 samples per group.

* Similarly, the abandonment probabilities of offspring scientists, a, are drawn from a normal distribution with a mean corresponding to their parent’s value and a standard deviation of 0.01, truncated to remain in [0, 1

* To ensure convergence to equilibrium sample sizes (see Supplementary Section 7), the evolutionary process proceeds for 500 generations, at which point the simulation stops.



Parameter Definition

Value/Range
n - population size = 120
s - Scientist’s target sample size - Uniform [2, 1000]
a - Scientist’s probability of abandoning a research question when scooped - Uniform [0, 1]

α - False-positive rate - 0.05
e - Effect size - Exponential Rate parameter characterizing distribution of effect sizes  - 5
T -  Scientists’ career length - 15000 if c > 10,  5000 if c = 10

cs - Sample cost: number of time steps to acquire one data point - 1
c - Startup cost: number of time steps to set up a study - 10, 100, 200, 400
m - Maximum number of scientists per research question - 1, 2, 4, 8
d - Decay parameter determining the penalty for being scooped - 0, 0.15, 0.40, 1, 10
bn - Payoff from publishing negative results, relative to positive results - 0, 0.25, 0.5, 0.75, 1
