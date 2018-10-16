library(dplyr)
library(tidyr)

from_data <- function(file_name) {
  file.path("data",file_name)
}

import_combat_data <- function() {
  combat_data <- tbl_df(read.csv(from_data("combats.csv"), header=TRUE)) %>%
    rename_all(tolower)
}

import_pokemon_data <- function() {
  tbl_df(read.csv(from_data("pokemon.csv"), header=TRUE)) %>%
    rename(id=X.) %>%
    rename_all(tolower)
}

import_color_data <- function() {
  color_data <- tbl_df(read.csv(from_data("colors.csv"),
                          header=FALSE,
                          stringsAsFactors=FALSE)) %>%
    rename(type=V1, color=V2)
  lut <- sapply(color_data$color, function(x)paste0("#",tolower(x)))
  names(lut) <- color_data$type
  lut
}

summarize_combat_data <- function(combat_data) {
  first_appears <-
    combat_data %>%
    group_by(first_pokemon) %>%
    summarize(appears=n()) %>%
    rename(id=first_pokemon)
  second_appears <-
    combat_data %>%
    group_by(second_pokemon) %>%
    summarize(appears=n()) %>%
    rename(id=second_pokemon)
  wins <-
    combat_data %>%
    group_by(winner) %>%
    summarize(wins=n()) %>%
    rename(id=winner)
  combat_summary <-
    first_appears %>%
    inner_join(second_appears,by="id") %>%
    inner_join(wins,by="id") %>%
    mutate(appears=appears.x+appears.y) %>%
    select(-appears.x,
           -appears.y)
}

combine_pokemon_and_combat_data <- function(pokemon_data, combat_data) {
  first_pokemon_data <-
    combat_data %>%
    select(first_pokemon) %>%
    rename(id=first_pokemon) %>%
    inner_join(pokemon_data,combats,by="id") %>%
    select(type.1:speed) %>%
    rename_all(.funs=funs(paste0("first_", .))) %>%
    bind_cols(as_tibble(seq(1,nrow(combat_data)))) %>%
    rename(row=value)
  second_pokemon_data <-
    combat_data %>%
    select(second_pokemon) %>%
    rename(id=second_pokemon) %>%
    inner_join(pokemon_data,combats,by="id") %>%
    select(type.1:speed) %>%
    rename_all(.funs=funs(paste0("second_", .))) %>%
    bind_cols(as_tibble(seq(1,nrow(combat_data)))) %>%
    rename(row=value)
  who_won <-
    combat_data %>%
    mutate(first_won=(first_pokemon==winner)) %>%
    select(first_won)
  firsts_who_won <-
    bind_cols(first_pokemon_data,who_won) %>%
    filter(first_won) %>%
    select(-first_won)
  firsts_who_lost <-
    bind_cols(first_pokemon_data,who_won) %>%
    filter(!first_won) %>%
    select(-first_won)
  seconds_who_won <-
    bind_cols(second_pokemon_data,who_won) %>%
    filter(!first_won) %>%
    select(-first_won)
  seconds_who_lost <-
    bind_cols(second_pokemon_data,who_won) %>%
    filter(first_won) %>%
    select(-first_won)
  first_won_combat_features <-
    inner_join(firsts_who_won,seconds_who_lost,by="row") %>%
    mutate(left_side_won=TRUE)
  first_lost_combat_features<-
    inner_join(firsts_who_lost,seconds_who_won,by="row") %>%
    mutate(left_side_won=FALSE)
  raw_combat_features <-
    bind_rows(first_won_combat_features,
              first_lost_combat_features) %>%
    arrange(row) %>%
    select(-row)
}

extract_combat_features <- function(combined_data) {
  combat_features <-
    combined_data %>%
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
    select(-first_speed,-second_speed) %>%
    rename(left_first_type=first_type.1) %>%
    rename(left_second_type=second_type.1) %>%
    rename(right_first_type=first_type.2) %>%
    rename(right_second_type=second_type.2) %>%
    mutate(left_first_type=as.numeric(left_first_type)) %>%
    mutate(left_second_type=as.numeric(left_second_type)) %>%
    mutate(right_first_type=as.numeric(right_first_type)) %>%
    mutate(right_second_type=as.numeric(right_second_type)) %>%
    mutate(left_side_won=as.numeric(left_side_won))
}

extract_battle_emulation_features <- function(combined_data) {
  battle_emulation_features <-
    combined_data %>%
    mutate(first_attack_advantage=first_attack/second_defense) %>%
    mutate(first_spatk_advantage=first_sp..atk/second_sp..def) %>%
    mutate(first_advantage=pmax(first_attack_advantage, first_spatk_advantage)) %>%
    mutate(second_attack_advantage=second_attack/first_defense) %>%
    mutate(second_spatk_advantage=second_sp..atk/first_sp..def) %>%
    mutate(second_advantage=pmax(second_attack_advantage, second_spatk_advantage)) %>%
    mutate(advantage_diff=first_advantage-second_advantage) %>%
    mutate(advantage_diff=scale(advantage_diff)) %>%
    mutate(speed_diff=first_speed-second_speed) %>%
    mutate(speed_diff=scale(speed_diff)) %>%
    mutate(attack_diff=first_attack-second_attack) %>%
    mutate(attack_diff=scale(attack_diff))
}
