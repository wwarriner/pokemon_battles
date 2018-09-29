library(dplyr)
library(tidyr)
library(ggplot2)
source("helpers.R")

combats <- tbl_df(read.csv("combats.csv", header=TRUE)) %>%
  rename_all(tolower)
c1 <-
  combats %>%
  group_by(first_pokemon) %>%
  summarize(appears=n()) %>%
  rename(id=first_pokemon)
c2 <-
  combats %>%
  group_by(second_pokemon) %>%
  summarize(appears=n()) %>%
  rename(id=second_pokemon)
c3 <-
  combats %>%
  group_by(winner) %>%
  summarize(wins=n()) %>%
  rename(id=winner)
combat_summary <-
  inner_join(c1,c2,by="id") %>%
  inner_join(c3,by="id") %>%
  mutate(appears=appears.x+appears.y,
         #win_rate=wins/appears,
         #loss_rate=1-win_rate,
         appears.x=NULL,
         appears.y=NULL)

pokemon_raw <- tbl_df(read.csv("pokemon.csv", header=TRUE)) %>%
  rename(id=X.) %>%
  rename_all(tolower)
pkmn <-
  inner_join(pokemon_raw,combat_summary,by="id")

rm("c1","c2","c3",combat_summary)
colors <- pkmn_colors()

firsts <-
  combats %>%
  select(first_pokemon) %>%
  rename(id=first_pokemon) %>%
  inner_join(pokemon_raw,combats,by="id") %>%
  select(type.1:speed) %>%
  rename_all(.funs=funs(paste0("first_", .))) %>%
  bind_cols(as_tibble(seq(1,nrow(combats)))) %>%
  rename(row=value)
seconds <-
  combats %>%
  select(second_pokemon) %>%
  rename(id=second_pokemon) %>%
  inner_join(pokemon_raw,combats,by="id") %>%
  select(type.1:speed) %>%
  rename_all(.funs=funs(paste0("second_", .))) %>%
  bind_cols(as_tibble(seq(1,nrow(combats)))) %>%
  rename(row=value)
who_won <-
  combats %>%
  mutate(first_won=(first_pokemon==winner)) %>%
  select(first_won)
firsts_who_won <-
  bind_cols(firsts,who_won) %>%
  filter(first_won) %>%
  select(-first_won)
firsts_who_lost <-
  bind_cols(firsts,who_won) %>%
  filter(!first_won) %>%
  select(-first_won)
seconds_who_won <-
  bind_cols(seconds,who_won) %>%
  filter(!first_won) %>%
  select(-first_won)
seconds_who_lost <-
  bind_cols(seconds,who_won) %>%
  filter(first_won) %>%
  select(-first_won)
first_won_combat_features <-
  inner_join(firsts_who_won,seconds_who_lost,by="row") %>%
  mutate(left_side_won=TRUE)
first_lost_combat_features<-
  inner_join(firsts_who_lost,seconds_who_won,by="row") %>%
  mutate(left_side_won=FALSE)
combat_features <-
  bind_rows(first_won_combat_features,
            first_lost_combat_features) %>%
  arrange(row) %>%
  select(-row) %>%
  mutate(hp_diff=first_hp-second_hp) %>%
  select(-first_hp,-second_hp) %>%
  mutate(attack_diff=first_attack-second_attack) %>%
  select(-first_attack,-second_attack) %>%
  mutate(defense_diff=first_defense-second_defense) %>%
  select(-first_defense,-second_defense) %>%
  mutate(spatk_diff=first_sp..atk-second_sp..atk) %>%
  select(-first_sp..atk,-second_sp..atk) %>%
  mutate(spdef_diff=first_sp..def-second_sp..def) %>%
  select(-first_sp..def,-second_sp..def) %>%
  mutate(speed_diff=first_speed-second_speed) %>%
  select(-first_speed,-second_speed)

