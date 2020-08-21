
# Log of the code review by Tiago

Goals:

* Check if scripts  "Experimental_Manip..." and "RawPop..." match the verbal descriptions
* Find any bugs (if they exist)

Extra goals:
 
* Improve code reproducibility
* Optimize memory usage of "RawPop..."

# Step 0: 

I will start by cleaning and understanding what the code does.
This might look like wasting time, but the only way to properly 
check the code is by making sure it is clean. 


Every bit is saved and commited to GitHub, so previous versions 
can be restored if needed. 

I can feel free to delete anything, as I can always come back to 
an older version on GitHub. 


In the process, I'll share my notes.

## "Experimental_Manip..."
### Notes

#### Some comments are clutter that can be removed for clarity. For example: 
  ` #load libraries
    #set sci ids
    #set number of questions
    #data frames
    #results tracker
    ###while loop###`

I have removed at first the ones that could be removed right away. 

#### For some comments, it is better to change the variable names before removing. For example:
    num_questions ---> number_of_questions
    sci_ids ---> ids_for_scientists
    payoff_v ---> initial_payoffs
    tt.all ---> ? (baybe baseline_for_something?)
    t.all <- scientist_df$ss*sample_cost + startup_cost #tracker


#### Commented out code is bad

There is a lot of commented out code. This kind of code cannot be sistematically tested. 
I will *REMOVE* commented out code for now. It is stored in the version control system. 
If you really want to have it, we can deploy it as options in some function. 


#### Mixed notations make code harder to read

Even though computers do not care, humans prefer it variables and functions follow some 
standard. For example separate_like_this, or SeparateLikeThis or even separate.like.this . 
Mixing is confusing, specially whent using dots, as they are sometimes using for **method calls**. 
For example:
* scientist_df 
* questions.q_id 
* tt.all

The same applies for all uppercase, like `A_PROB`

I will make separate_like_this and lowercase the standard. 

#### Loop runs and whoever is running the code is left lost

It is nice when running code that might take long to add progress bars. 
One option is the [progress R pakage](https://github.com/r-lib/progress).

#### Big functions are hard to test

There is a clean code paradigm that each function should do 1 thing and only
1 thing. The `play_complexcomp` function does way too many things. 
It will have to be broken into little pieces to actually know if the code really
does what you say it does on the article. 


#### Files that do multiple things are confusing

The  "Experimental_Manip..." code does two things: present a function (`play_complexcomp`)
and runs a simulation. 

These are 2 conceptually different things. The end user might be interested in the 
implementation details, or just how to run the code. 
I  split the simulation into two files, `play_complexcomp.R` and 
`run_simulation_for_play_complexcomp.R` but I am making it into a package.

#### Have complex code built into a small R package

Ideally, I like the idea of small, personal R packages. That does not take THAT much 
effort and makes things way clearer and reproducible. 
That is the easiest and best (often the same) way to do it properly. 
One of the things that I will do in the package is to "extract out"
smaller parts of `play_complexcomp`

This will make it way more reproducible, testable and reusable, I hope.

#### Avoid global variables! 

There are places in the code that play_complexcomp uses a variable that is not
explicitly passed into the function! (See problem 2 below)

This is a problem, because scopes can be mixed, leading to very tricky bugs. 
It might not be actually causing any problems, but it is dangerous coding. 

It also makes the package not work properly for when evolution = 1. 


#### Always use parameter names in large function calls

The call to `play_complexcomp` does not make it explict which parameters are
which. Take makes it also dangerous coding. It is way too easy to miss the right
position and change one parameter for another. 
I will change that once I finish the R package.


#### Variables are defined far from where they are used

Some variables are defined in the beginning, but only used much later. This makes it 
very hard to track what is doing what. 


#### Never ever publish dummy variable names

Variables like `dum1` and `dum2` are severely frowned upon. They make
reading the code way more complicated. 

We should always try to come up with meaningful names.

#### I cannot understand what question_n_on_q is 

I have no idea what the variable `question_n_on_q`. I need to understand
all the variables to be sure that the code does what it is supposed to 
do. 

The same goes for `questions_e_size`


# Step 1: 

## Add tests for the functions

I will add tests for everything, so we know that the code does what
you expect it does. That is the industry standard: if it is not tested,
you can't know if it works the way it should. 

Tests are added to the package with the help of another package called 
testthat.

### Add integration test
The first thing is to add a test that runs the whole script so I can modify
and re-test and be sure that the results are the same. 

I've made a simple test for only one of the cases of interest. I will make 
more to be sure that (almost) all the code is covered. 

### Add coverage testing
Using the R package [covr](https://github.com/r-lib/covr), I set up the
repository to check how much of the code is being run by tests. 

So far, the testes are covering 99.22% of the code, but eventually they
will cover everything. 

The tests are not still testing what we want, but they can already see the
whole code, which is already good

### Extract and test

The part now is to extract the individual bits of the `play_complexcomp` function and test them individually, one by one. 




### Actual problems

#### 1 

`#set payoffs to 0 at beginning of each run
 payoff_v <- rep(0.0000001, length = length(samplesizes)) `
 
 The comment does not match the code. Should it be 0 or  0.0000001?
 I have removed the comment and     payoff_v ---> initial_payoffs
  
#### 2

`num_players <- players
 (...) 
 RR <-play_complexcomp(0,    15000,    NA,       c,         100,       1,     5,        d,     1,   a_prob)`
 
 This is the error code when the play_complexcomp is encapsulated into a package: 
 
 ` Error in runif(num_players, min_sample_size, max_sample_size) : 
  object 'num_players' not found `
  
This is an error that I introduced, but only because the `num_players` variable were 
not deployed as argument of `play_complexcomp` 

#### 3 

Some functions have "randomness" built in. This is interesting, but for sheer computational 
reproducibility, it is problematic. It is important to use "set.seed" in analysis code to 
ensure that the results are reproducible. 

By the stochastic nature of these functions, the original article is, **stricto sensu**, 
irreparably irreproducible. 


