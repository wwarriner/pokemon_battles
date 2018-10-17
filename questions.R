source("import_helpers.R")
library(ggplot2)

# How many unique pokemon ids are there?
q1.1 <- function(pokemon_data) {
  id_unique_count <- length(unique(pokemon_data$id))
  print(sprintf("There are %d unique pokemon ids in the dataset.", id_unique_count))
  id_unique_count
}

# How are they distributed by first type?
q1.2 <- function(pokemon_data, color_data) {
  type_counts <-
    pokemon_data %>%
    group_by(type.1) %>%
    summarize(count=n())
  print(type_counts)
  write.csv(type_counts, file="q1.2.csv")
  g <- ggplot(type_counts, aes(x=type.1, y=count)) +
    geom_col(aes(fill=type.1)) +
    scale_color_manual(values=color_data, aesthetics="fill") +
    theme(axis.text.x = element_text(angle=90, hjust=1))
  print(g)
  ggsave("q1.2.png")
}

# How are they distributed by second type?
q1.3 <- function(pokemon_data, color_data) {
  type_counts <-
    pokemon_data %>%
    group_by(type.2) %>%
    summarize(count=n())
  print(type_counts)
  write.csv(type_counts, file="q1.3.csv")
  color_data <- append(color_data, "#444444")
  g <- ggplot(type_counts, aes(x=type.2, y=count)) +
    geom_col(aes(fill=type.2)) +
    scale_color_manual(values=color_data, aesthetics="fill") +
    theme(axis.text.x = element_text(angle=90, hjust=1))
  print(g)
  ggsave("q1.3.png")
}

# What percent of pokemon have two types?
q1.4 <- function(pokemon_data) {
  two_types_count <- sum(pokemon_data$type.2!="")
  ratio <- two_types_count/q1.1(pokemon_data)
  percent <- ratio*100
  print(sprintf("%.2f%% of pokemon have two types.", percent))
  ratio
}

# How frequently are types paired?
q1.5 <- function(pokemon_data) {
  two_types <-
    pokemon_data %>%
    group_by(type.1, type.2) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    complete(type.1, type.2, fill=list(count=0))
  g <- ggplot(two_types, aes(x=type.1, y=type.2)) +
    geom_tile(aes(fill=count)) +
    geom_text(aes(label=count)) +
    scale_fill_gradientn(colors=c("white", "lightskyblue", "springgreen4"),
                         values=scales::rescale(c(0,0.01,1.0))) +
    theme(axis.text.x=element_text(angle=90))
  print(g)
  ggsave("q1.5.png")
}

# Box plot of stats overall
q1.6 <- function(pokemon_data) {
  box_plot_data <-
    pokemon_data %>%
    combine_stats()
  g <- ggplot(box_plot_data, aes(x=stat, y=value, fill=stat)) +
    geom_boxplot()
  print(g)
  ggsave("q1.6.png")
}

# Summary data of stats
q1.7 <- function(pokemon_data) {
  summary_data <-
    pokemon_data %>%
    summarize_stats() %>%
    mutate_at(.vars=vars(mean, sd), .funs=function(x){sprintf("%.2f",round(x,2))})
  print(summary_data)
  write.csv(summary_data, file="q1.7.csv")
}

# What percent of pokemon are legendary?
q1.8 <- function(pokemon_data) {
  legendary_data <-
    pokemon_data %>%
    group_by(legendary) %>%
    summarize(count=n())
  legendary_count <- sum(legendary_data=="True")
  ratio <- legendary_count/q1.1(pokemon_data)
  percent <- ratio*100
  print(sprintf("%.2f%% of pokemon have two types.", percent))
  print(ratio)
  g <- ggplot(legendary_data, aes(x=legendary, y=count)) +
    geom_col()
  print(g)
  ggsave("q1.8.png")
}

# What do stats look like broken out by legendary status?
q1.9 <- function(pokemon_data) {
  box_plot_data <-
    pokemon_data %>%
    group_by(legendary) %>%
    combine_stats()
  g <- ggplot(box_plot_data, aes(x=stat, y=value, fill=legendary)) +
    geom_boxplot()
  print(g)
  ggsave("q1.9.png")
}

# What does the left-side-wins space look like?
q2 <- function(combat_left_side_wins_table) {
  par(pty="s")
  png("q2.png")
  image(combat_left_side_wins_table,
        useRaster=TRUE,
        xaxt="none",
        yaxt="none",
        xlab="Left Side ID",
        ylab="Right Side ID")
  x <- ncol(combat_left_side_wins_table)
  at <- seq(0.0, 1.0, 0.125)
  labels <- as.character(seq(0, x, round(x/8)))
  axis(1, at=at, labels=labels)
  axis(2, at=at, labels=labels)
  dev.off()
}

# What fraction of coverage of the space is there in the dataset?
q3 <- function(pokemon_data, combat_data) {
  all_uids <- q1(pokemon_data)
  population_count <- all_uids*all_uids
  sample_count <- nrow(combat_data)
  coverage_ratio <- sample_count/population_count
  coverage_percent <- coverage_ratio*100
  print(sprintf("The coverage ratio of the population space is %.2f%%\n", coverage_percent))
  coverage_ratio
}

# how many repeat battles are there?
q4 <- function(pokemon_data,combat_data,combat_left_side_wins_table) {
  symmetric_count <- sum(colSums(pmin(combat_left_side_wins_table,t(combat_left_side_wins_table))))
  symmetric_percent <- symmetric_count/nrow(combat_data)*100
  print(sprintf("There are %d repeat battles, which is %.2f%% of all battles.\n", symmetric_count, symmetric_percent))
  symmetric_count
}

# Which type is most likely to win?
# legendary-fighting, just ahead of legendary-grass and legendary-normal
# of non-legendary, it is flying, then dark, then electric
q5 <- function(pokemon_combat_summary, color_data) {
  wins_appears_data <-
    pokemon_combat_summary %>%
    combine_types() %>%
    mutate(type=factor(type)) %>%
    group_by(legendary, type) %>%
    summarize(wins=sum(wins), appears=sum(appears))
  labels <- c("False"="Nonlegendary",
              "True"="Legendary")
  g <-
    ggplot(wins_appears_data,
           aes(x=type,
               y=wins/appears)) +
    facet_grid(cols=vars(legendary),
               labeller=as_labeller(labels)) +
    geom_col(aes(fill=type)) +
    scale_fill_manual(values=color_data) +
    scale_y_continuous(label=scales::percent) +
    coord_cartesian(ylim=c(0, 1)) +
    theme(axis.text.x = element_text(angle=60, hjust=1))
  print(g)
  ggsave("q5.png")
}

# which stat plays the greatest role in winning?
# speed, apparently
q6 <- function(pokemon_combat_summary, color_data) {
  d <-
    pokemon_combat_summary %>%
    combine_stats()
  g <-
    ggplot(d,
           aes(x=value,
               y=wins/appears)) +
    facet_grid(cols=vars(stat)) +
    geom_point(aes(color=type.1)) +
    geom_smooth(method='lm', formula=y~x) +
    scale_color_manual(values=color_data) +
    scale_y_continuous(label=scales::percent) +
    coord_cartesian(ylim=c(0, 1))
  print(g)
  ggsave("q6.png")
}

# is speed stat subject to Simpson paradox?
q7 <- function(pokemon_combat_summary, color_data) {
  d <-
    pokemon_combat_summary %>%
    combine_types()
  g <-
    ggplot(d,
           aes(x=speed,
               y=wins/appears)) +
    facet_grid(cols=vars(type)) +
    geom_point(aes(color=type)) +
    geom_smooth(method='lm', formula=y~x) +
    scale_color_manual(values=color_data) +
    scale_y_continuous(label=scales::percent) +
    coord_cartesian(ylim=c(0, 1)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  print(g)
  ggsave("q7.png")
}

# what is the distribution of wins by stat difference?
q8 <- function(combat_features) {
  d <-
    combat_features %>%
    select(left_side_won,hp_diff:speed_diff) %>%
    gather(key=stat,value=diff,hp_diff:speed_diff) %>%
    mutate(diff=as.numeric(diff)) %>%
    mutate(left_side_won=as.logical(left_side_won))
  g <-
    ggplot(d,
           aes(x=diff,
               group=left_side_won,
               fill=left_side_won)) +
    facet_grid(cols=vars(stat)) +
    geom_density(alpha=0.5)
  print(g)
  ggsave("q8.png")
}

# can we model battles using domain knowledge?
# not better than simple diffs of vars apparently
q9 <- function(battle_emulation_features) {
  d <- battle_emulation_features %>%
    select(left_side_won,speed_diff,advantage_diff) %>%
    gather(key=stat,value=diff,speed_diff,advantage_diff)
  g <-
    ggplot(d,
           aes(x=diff,
               group=left_side_won,
               fill=left_side_won)) +
    facet_grid(cols=vars(stat),
               scales="free") +
    xlim(-4,4) +
    geom_density(alpha=0.5)
  print(g)
  ggsave("q9.png")
}
