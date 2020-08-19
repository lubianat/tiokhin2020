
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



### Actual problems

#### 1 

`#set payoffs to 0 at beginning of each run
 payoff_v <- rep(0.0000001, length = length(samplesizes)) `
 
 The comment does not match the code. Should it be 0 or  0.0000001?
 I have removed the comment and     payoff_v ---> initial_payoffs
  

