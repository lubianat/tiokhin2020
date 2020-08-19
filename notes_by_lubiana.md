
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

### "Experimental_Manip..."

* Some comments are clutter that can be removed for clarity. For example: 
    #load libraries
    #set sci ids
    #set number of questions
    #data frames
    #results tracker
    ###while loop###

I have removed at first the ones that could be removed right away. 

* For some comments, it is better to change the variable names before removing. For example:
    num_questions ---> number_of_questions
    tt.all ---> ? (baybe baseline_for_something?)
    t.all <- scientist_df$ss*sample_cost + startup_cost #tracker


* Commented out code is bad

There is a lot of commented out code. This kind of code cannot be sistematically tested. 
I will *REMOVE* commented out code for now. It is stored in the version control system. 
If you really want to have it, we can deploy it as options in some function. 


