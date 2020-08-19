library(dplyr)
library(ggplot2)
library(RColorBrewer)

num_prior <- seq(0, 10, 0.01)
decay <- c(0, 0.15, 0.4, 1, 10)

df <- data.frame(num_prior = num_prior,
                 decay = NA, 
                 payoff = NA)

dum_list <- list()
for(i in 1:length(decay)){
  
  novelty_of_res <- (1 / (1 + num_prior)) ^ decay[i]
  df$decay <- decay[i]
  df$payoff <- novelty_of_res
  dum_list[[i]] <- df
}

df_fin <- bind_rows(dum_list)

  ggplot(data = df_fin, aes(x = as.factor(num_prior), y = payoff, 
                   color = as.factor(decay), 
                   group = as.factor(decay))) +
    geom_line(size = 1) + 
    theme_bw(base_size=14) +
    ylab("Payoff for publishing positive result") + 
    xlab("Number of published results on a research question") + 
    scale_x_discrete(breaks = c(0:10), labels = c(0:10)) +
    scale_colour_manual(name = "Decay", values = c("#fd8d3c", "#f16913", "#d94801", 
                                                   "#a63603", "#7f2704")) +
    theme(plot.title = element_text(hjust = 0.5))
