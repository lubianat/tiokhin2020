
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

