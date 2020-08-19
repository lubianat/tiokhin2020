#load libraries
library(pwr)
library(plyr)
library(dplyr)
library(ggplot2)
library(viridisLite)
library(viridis)
library(RColorBrewer)
library(profvis)
library(fastmatch)

#load as much of the raw data as possible
#do this manually each of the disaggregated raw data files

#for example:
#load("C:/Users......originaldata_indvfiles/all_results_in_each_sim/res_list_400sc_8comp.RData")

a <- bind_rows(res_list_all)
b <- bind_rows(res_list_all)
c <- bind_rows(res_list_all)
d <- bind_rows(res_list_all)

#store raw data in new object
d.sci <- bind_rows(a, b, c, d) 

#unique values of each parameter
startc <- c(unique(d.sci$eq.startupcost))
exprate <- c(unique(d.sci$eq.exprate))
decay <- c(unique(d.sci$eq.decay))
compnum <- c(unique(d.sci$eq.maxperq))
bneg <- c(unique(d.sci$eq.b_neg))
life <- c(unique(d.sci$eq.life))

#create dataframe to store science-level outcomes for each parameter combination
#number of combinations
combs <- length(bneg) * length(exprate) * length(compnum) * length(decay) * 50
sci.outcomes.df <- d.sci %>% select(eq.startupcost, eq.exprate, eq.decay, eq.maxperq, eq.b_neg, eq.life)
sci.outcomes.df <- sci.outcomes.df[1:combs,]
sci.outcomes.df[,] <- NA
sci.outcomes.df$ppv <- NA
sci.outcomes.df$prop_pub_true <- NA
sci.outcomes.df$prop_pub_false <- NA
sci.outcomes.df$diff_true_false_pub <- NA
sci.outcomes.df$prop_more_true <- NA
sci.outcomes.df$prop_equal_ormore_true <- NA
sci.outcomes.df$prop_equal_true_false <- NA
sci.outcomes.df$productivity_prop <- NA
sci.outcomes.df$num_pubs <- NA
sci.outcomes.df$n_sims <- NA

#mean effect size
mean_effect <- mean(round(rexp(1e6, 5), 1))

#empty list to store df_belief data frames
df_belief_list <- vector("list", combs)
df_belief_abs_list <- vector("list", combs)

#tracker
a <- 0

for(start in startc){
  for(exp in exprate){
    for(dec in decay){
      for(comp in compnum){
        for(bn in bneg){

#unique data frame for each unique combination of parameters
df <- d.sci[d.sci$eq.startupcost == start & d.sci$eq.exprate == exp & d.sci$eq.decay == dec &
              d.sci$eq.maxperq == comp & d.sci$eq.b_neg == bn,]
#num rows
df_nrow <- nrow(df)
#difference between the q_ids of bordering rows
q_id_lag_diff1 <- diff(df$q_id, lag = 1)
#make new column in df to keep track of lagged differences. check num rows. 
df$q_id_lag_diff1 <- 0
#add lagged differences
df$q_id_lag_diff1[2:(df_nrow)] <- q_id_lag_diff1

#whenever column df$q_id_lag_diff1 has a very small value, this is the beginning of a new sim. 
df$newsim <- df$q_id_lag_diff1 < -(sd(q_id_lag_diff1) * 8)
#get position of where new simulations begin
pos_newsim <- which(df$newsim == TRUE)
#add 0 to beginning of this vector
pos_newsim <- c(0, pos_newsim)
#lag this vector
pos_newsim <- diff(pos_newsim)

#num sims
n_sims <- sum(df$newsim) + 1 #check to make sure that the data was divided into 50 parts, 1 for each run, for each unique parameter combination
df$n_sims <- n_sims

for(i in 1:n_sims){
  
  #tracker
  a <- a + 1
  
  if(i < n_sims){
    
    #subset data frame into single simulation. 
    df_subset <- df[1:(pos_newsim[i] - 1),]
    df_subset$true <- NA
  } else {
    
    #the final run
    df_subset <- df
    df_subset$true <- NA
  }
  
  
  ### calculating ppv by dividing all positive, true results by all positive results ###
  ppv <- nrow(df_subset[df_subset$result==1 & df_subset$esize > 0.0,]) / nrow(df_subset[df_subset$result==1,])
  
  ### sum of true results divided by sum of all results ###
  true_results <- nrow(df_subset[df_subset$result == 1 & df_subset$esize > 0.0,]) + nrow(df_subset[df_subset$result == 0 & df_subset$esize == 0.0,])
  false_results <- nrow(df_subset[df_subset$result == 1 & df_subset$esize == 0.0,]) + nrow(df_subset[df_subset$result == 0 & df_subset$esize > 0.0,])
  #proportions
  prop_pub_true <- true_results / (true_results + false_results) # proportion of published results that are true
  prop_pub_false <- 1 - prop_pub_true  # proportion of published results that are false
  
  ### difference between total number of true and false results ## 
  diff_true_false_pub <- true_results - false_results
  
  ### productivity ###
  num_pubs <- true_results + false_results
  #total time put towards something useful = n_pubs*startup_cost + sum(published sample sizes),
  #divided by total time available across all 50 simulations (i.e., lifespan * num scientists * 50)
  productivity_prop <- (num_pubs * start + sum(df_subset$ss)) / (life * 120)
  
  ### mean change in log odds belief ##
  
  #calculate powers for all results
  df_subset$power <-  as.numeric(pwr.t.test(n = df_subset$ss, d = mean_effect, 
                                     sig.level=0.05, type="two.sample")[4]$power)
  #calculate change in log-odds belief
  df_subset$d_logodds_belief <- NA
  df_subset$d_logodds_belief[df_subset$result == 1] <- log(df_subset$power[df_subset$result == 1] / 0.05)
  df_subset$d_logodds_belief[df_subset$result == 0] <- log((1 - df_subset$power[df_subset$result == 0]) / 0.95)
  
  #calculate absolute change in log-odds belief
  df_subset$d_logodds_belief_abs <- NA
  df_subset$d_logodds_belief_abs[df_subset$result == 1] <- abs(log(df_subset$power[df_subset$result == 1] / 0.05))
  df_subset$d_logodds_belief_abs[df_subset$result == 0] <- abs(log((1 - df_subset$power[df_subset$result == 0]) / 0.95))
  
  #mean change in log odds and abs log odds belief across entire simulation, for each effect size
  df_belief <- aggregate(d_logodds_belief ~ esize, FUN = mean, data = df_subset)
  df_belief_abs <- aggregate(d_logodds_belief_abs ~ esize, FUN = mean, data = df_subset)
  
  #add relevant column information
  df_belief <- bind_cols(df_belief, df_subset[1:nrow(df_belief), 7:11])
  df_belief_abs <- bind_cols(df_belief_abs, df_subset[1:nrow(df_belief_abs), 7:11])
  
  ### calculate number of questions where there are more true than false results ###
  df_subset$true[df_subset$esize > 0 & df_subset$result == 1] <- 1
  df_subset$true[df_subset$esize == 0 & df_subset$result == 0] <- 1
  df_subset$true[df_subset$esize > 0 & df_subset$result == 0] <- 0
  df_subset$true[df_subset$esize == 0 & df_subset$result == 1] <- 0
  
  zz <- aggregate(true ~ q_id, FUN = sum, data = df_subset)
  zz$total <- as.vector(table(df_subset$q_id))
  zz$false <- zz$total - zz$true
  zz$more_true <- (zz$true - zz$false) > 0
  zz$equal_ormore_true <- (zz$true - zz$false) >= 0
  zz$equal_true_false <- (zz$true - zz$false) == 0
  prop_more_true <- sum(zz$more_true) / length(zz$more_true)
  prop_equal_ormore_true <- sum(zz$equal_ormore_true) / length(zz$equal_ormore_true)
  prop_equal_true_false <- sum(zz$equal_true_false) / length(zz$equal_true_false)
  
  #update full data frame to remove this single run
  if(i < n_sims){
  df <- df[-(1:pos_newsim[i]), ]
  }
  
  #store all results
  sci.outcomes.df[a,] <- c(start, exp, dec, comp, bn, life,
                           ppv, prop_pub_true, prop_pub_false, diff_true_false_pub, 
                           prop_more_true, prop_equal_ormore_true, prop_equal_true_false, 
                           productivity_prop, num_pubs, n_sims)
  
  df_belief_list[[a]] <- df_belief
  df_belief_abs_list[[a]] <- df_belief_abs
  
}
        }
      }
    }
  }
}

raw_indvruns_200 <- sci.outcomes.df
df_belief_list_200 <- df_belief_list
df_belief_abs_list_200 <- df_belief_abs_list

save(raw_indvruns_200, file = "raw_indvruns_200.RData")
save(df_belief_list_200, file = "df_belief_list_200.RData")
save(df_belief_abs_list_200, file = "df_belief_abs_list_200.RData")

#check unique number of simulations column
unique(raw_indvruns_200$n_sims)

### Aggregate beliefs within each sim ## 
dum <- bind_rows(df_belief_list_100, df_belief_list_200, df_belief_list_400)

### mean change in log odds belief across entire simulation, for each effect size ###
df_belief <- aggregate(d_logodds_belief ~ esize + eq.startupcost + eq.decay + eq.maxperq + 
                         eq.b_neg, FUN = mean, data = dum)

df_belief_SE <- aggregate(d_logodds_belief ~ esize + eq.startupcost + eq.decay + eq.maxperq + 
            eq.b_neg, FUN = function(x) sd(x) / sqrt(length(x)), data = dum)

df_belief$SE <- df_belief_SE$d_logodds_belief
save(df_belief, file = "aggregated_logodds_belief_se.RData")

###Repeat for Absolute Change in Log Odds Belief ### 
dum <- bind_rows(df_belief_abs_list_100, df_belief_abs_list_200, df_belief_abs_list_400)

### mean change in log odds belief across entire simulation, for each effect size ###
df_belief_abs <- aggregate(d_logodds_belief_abs ~ esize + eq.startupcost + eq.decay + eq.maxperq + 
                         eq.b_neg, FUN = mean, data = dum)

df_belief_abs_SE <- aggregate(d_logodds_belief_abs ~ esize + eq.startupcost + eq.decay + eq.maxperq + 
                            eq.b_neg, FUN = function(x) sd(x) / sqrt(length(x)), data = dum)

df_belief_abs$SE <- df_belief_abs_SE$d_logodds_belief_abs
save(df_belief_abs, file = "aggregated_logodds_belief_absoluteval_se.RData")

#final aggregated file used in paper and SI is "rawdata_agg_suc.RData"


