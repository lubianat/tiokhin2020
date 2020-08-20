library(tiokhin2020)

##### Run the simulation #####

#set simulation parameters
popsize <- c(120, 960)
comps <- c(2, 8)
dcy <- c(0.15, 10)
min_sample_size <- 3
max_sample_size <- 12

library("progress")

for (c in comps) {
  print(paste("Running comp =", c, "; Out of "))
  cat(comps, "\n")
  
  for (d in dcy) {
    print(paste("Running dcy =", d, "; Out of "))
    cat(dcy, "\n")
    
    for (players in popsize) {
      print(paste("Running players =", players, "; Out of "))
      cat(popsize, "\n")
      
      dum_list <- vector(mode = "list", length = 200)
      tracker <- 1
      
      num_players <- players
      aban_rep_dum <- players / 4
      a_prob <-
        c(
          rep(0, aban_rep_dum),
          rep(0.33, aban_rep_dum),
          rep(0.66, aban_rep_dum),
          rep(1, aban_rep_dum)
        )
      
      if (players == 120) {
        number_of_loops <- 25
        pb_for_120 <- progress_bar$new(
          format = "  [:bar] :percent eta: :eta",
          total = number_of_loops)
        
        for (rep in 1:number_of_loops) {
          pb_for_120$tick()
          RR <-
            play_complexcomp(0,    15000,    NA,       c,         100,       1,     5,        d,     1,   a_prob)
          dum_list[tracker] <- RR
          tracker <- tracker + 1
        }
        
      } else if (players == 960) {
        number_of_loops <- 10
        pb_for_960 <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                                       total = number_of_loops)
        for (rep in 1:10) {
          pb_for_960$tick()
          RR <-
            play_complexcomp(0,    15000,    NA,       c,         100,       1,     5,        d,     1,   a_prob)
          dum_list[tracker] <- RR
          tracker <- tracker + 1
        }
      }
      
      df <- bind_rows(dum_list)
      
      agg.dummy <-
        group_by(df, abandon_prob) %>% summarise(payoff_mean = mean(payoff))
      df$mean_payoff <- NA
      
      for (i in unique(df$abandon_prob)) {
        df$mean_payoff[df$abandon_prob == i] <-
          agg.dummy$payoff_mean[agg.dummy$abandon_prob == i]
      }
      
      df$num_players <- as.factor(df$num_players)
      levels(df$num_players) <- c(paste("Popsize:", df$num_players))
      
      aa <- ggplot(data = df,
                   aes(
                     x = as.factor(abandon_prob),
                     y = payoff,
                     color = as.factor(abandon_prob),
                     group = as.factor(abandon_prob)
                   )) +
        geom_jitter(width = 0.3) + geom_violin() +
        stat_summary(
          fun.y = "mean",
          geom = "point",
          shape = 20,
          size = 5,
          color = "black"
        ) +
        facet_wrap(~ num_players, ncol = 3) +
        theme_bw(base_size = 14) +
        ylab("Payoff") +
        xlab("Abandonment Probability") +
        scale_colour_brewer(name = "Abandonment \nProbability", palette = "PuRd") +
        ggtitle(paste(
          'SS: 3 to 12   Normal Payoffs   SUC: 100   Bneg: 1  Comp:',
          c,
          " Decay:",
          d
        )) +
        theme(plot.title = element_text(hjust = 0.5))
      plot(aa)
      
    }
  }
}
