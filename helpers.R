pkmn_colors <- function() {
  pkmn <- tbl_df(read.csv("colors.csv",
                          header=FALSE,
                          stringsAsFactors=FALSE)) %>%
    rename(type=V1, color=V2)
  lut <- sapply(pkmn$color, function(x)paste0("#",tolower(x)))
  names(lut) <- pkmn$type
  lut
}

combine_stats <- function(pkmn) {
  pkmn <-
    pkmn %>%
    gather(stat, value, hp:speed)
}

combine_types <- function(pkmn) {
  pkmn <-
    pkmn %>%
    mutate(type.2=factor(type.2, levels=levels(type.1))) %>%
    gather(which_type, type, type.1:type.2) %>%
    filter(type!="")
}

write_combat_features <- function(combat_features) {
  write.csv(combat_features, file="combat_features.csv")
}
