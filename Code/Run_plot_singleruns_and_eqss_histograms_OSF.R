source("Main_CompetitionSimulation_Code_OSF.R")

#change initial conditions if necessary
min_sample_size <- 2 
max_sample_size <- 1000
gens <- 500

###############
### run single runs to visualize evolutionary trajectory ###
##############

dum_list = list() #list to store individual runs

#set single combination of parameters 
start_c <- c(400) #startup cost 
max_on_q <- c(4) #level of competition (VARY THIS IN 1, 2, 4, 8)
decay_2nd <- c(10) #how quickly payoffs for non-novel publications decays (larger numbers = bigger cost to being scooped)
ben_neg <- c(0) #benefit to publishing negative result

for(i in 1:12){
  #                    LIFE   MAX_PER_Q,   G,    Rep,   SCS,  SCS2,   E_RATE, DECAY,     B_NEG) 
  df <- run_complexsim(15000, max_on_q,   gens,   1,   start_c,   1,   c(5),  decay_2nd, ben_neg)
  
  data <- data.frame(mean_ss = df[[1]], lower_ss = df[[2]], upper_ss = df[[3]], 
                     mean_aban = df[[4]], lower_aban = df[[5]], upper_aban = df[[6]], 
                     generation = 1:gens, run = i, start_c = start_c, 
                     max_on_q = max_on_q, decay = decay_2nd, ben_neg = ben_neg)
  
  dum_list[[i]] <- data
}

#combine data from all runs into single df
data <- bind_rows(dum_list)

#plot
ggplot(data = data, aes(x = generation, y = mean_ss)) +
  geom_line(data = data, aes(x = generation, y = mean_ss), lwd = 0.8) +
  geom_ribbon(data = data, aes(ymin = lower_ss, ymax = upper_ss, alpha = .2), show.legend = F) +
  ylim(-1, 1000) + theme_bw(base_size = 14) +
  ylab("Mean Sample Size") + 
  xlab("Generation") + 
  facet_wrap( ~ run, ncol = 3) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) + ggtitle("Startup Cost: 400, Competitors: 4, Decay: 10, Ben.Neg: 0") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") 


## plot histograms of sample size distribution at equilibrium 
## requires modification of the run_complex_sim function to save equilibrium sample-size distributions by
## also returning the equilibrium sample sizes by modifying what the function returns as follows:

# return(list(mean_ss, lower_ss, upper_ss, mean_aban, lower_aban, upper_aban, ss))

list_dum <- list()

for(b in c(0, 1)){
  for(comp in c(2)){
    for(d in c(0, 10)){
      for(i in 1:10){
        
        df <- data.frame(run = NA, 
                         ss = rep(NA, 120), 
                         startupcost = NA, 
                         bneg = NA, 
                         comp = NA, 
                         decay = NA)
        
        ###Full parameters for simulation###
        ben_neg <- b #benefit to publishing negative result
        max_on_q <- comp #level of competition (i.e. number of researchers on a given question)
        decay_2nd <- d #how quickly payoffs for non-novel publications decays (larger numbers = bigger cost to being scooped)
        start_c <- c(400) #startup cost (adjust as necessary)
        
        #                 LIFE   MAX_PER_Q,    G,    Rep,   SCS,  SCS2,   E_RATE, DECAY,     B_NEG)
        a <- run_complexsim(15000, max_on_q,   500,   1,   start_c,   1,   c(5),  decay_2nd, ben_neg)
        df$run <- i
        df$ss <- a[[7]]
        df$startupcost <- 400
        df$bneg <- b
        df$comp <- comp
        df$decay <- d
        
        list_dum[[length(list_dum) + 1]] <- df
        
      }
    } }}

data_hist <- bind_rows(list_dum)

for(s in unique(data_hist$startupcost)){
  for(b in unique(data_hist$bneg)){
    for(d in unique(data_hist$decay)){
      for(c in unique(data_hist$comp)){
        
        
        aa <- ggplot(data = data_hist[data_hist$startupcost == s &
                                        data_hist$bneg == b &
                                        data_hist$decay == d &
                                        data_hist$comp == c,], aes(ss)) +
          geom_bar(stat = "bin", fill="#636363", colour="black", alpha = 0.8) +
          theme_bw(base_size = 14) +
          ylab("Frequency") +
          xlab("Equilibrium sample size") +
          ylim(0, 100) +
          facet_wrap( ~ run, ncol = 3) +
          theme(
            strip.background = element_blank(),
            strip.text.x = element_blank()
          ) + ggtitle(paste('Startup Cost:', s,  ' Comps:', c,  ' Decay:', d,  ' Ben.Neg:', b)) +
          theme(plot.title = element_text(hjust = 0.5)) +
          theme(legend.position = "none")
        plot(aa)
      }}}}


