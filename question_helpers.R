library(dplyr)
library(tidyr)

combine_stats <- function(pokemon_combat_summary) {
  pokemon_combat_summary <-
    pokemon_combat_summary %>%
    gather(stat, value, hp:speed)
}

combine_types <- function(pokemon_combat_summary) {
  pokemon_combat_summary <-
    pokemon_combat_summary %>%
    mutate(type.2=factor(type.2, levels=levels(type.1))) %>%
    gather(which_type, type, type.1:type.2) %>%
    filter(type!="")
}

summarize_stats <- function(data) {
  combine_stats(data) %>%
    group_by(stat) %>%
    summarize(min=min(value),
              q10=quantile(value,probs=0.10),
              q25=quantile(value,probs=0.25),
              median=median(value),
              mean=mean(value),
              sd=sd(value),
              q75=quantile(value,probs=0.75),
              q90=quantile(value,probs=0.90),
              max=max(value))
}
