source("import_helpers.R")

combat_data <- import_combat_data()
combat_summary <- summarize_combat_data(combat_data)
combat_left_side_wins_table <- table(combat_data[,1:2])

pokemon_data <- import_pokemon_data()
pokemon_combat_summary <- inner_join(pokemon_data,combat_summary,by="id")

color_data <- import_color_data()

combined_data <- combine_pokemon_and_combat_data(pokemon_data, combat_data)

combat_features <- extract_combat_features(combined_data)
write.csv(combat_features, file="combat_features.csv")

battle_emulation_features <- extract_battle_emulation_features(combined_data)
