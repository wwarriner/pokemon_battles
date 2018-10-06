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
