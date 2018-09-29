# how many repeat battles are there?
q0 <- function(combats) {
  coverage <- table(combats[,1:2])
  symmetric_count <- sum(colSums(pmin(coverage,t(coverage))))
}

# Which type is most likely to win?
# legendary-fighting, just ahead of legendary-grass and legendary-normal
# of non-legendary, it is flying, then dark, then electric
q1 <- function(pkmn, colors) {
  labels <- c("False"="Nonlegendary",
              "True"="Legendary")
  d <-
    pkmn %>%
    combine_types() %>%
    mutate(type=factor(type)) %>%
    group_by(legendary, type) %>%
    summarize(wins=sum(wins), appears=sum(appears))
  g <-
    ggplot(d,
           aes(x=type,
               y=wins/appears)) +
    facet_grid(cols=vars(legendary),
               labeller=as_labeller(labels)) +
    geom_col(aes(fill=type)) +
    scale_fill_manual(values=colors) +
    scale_y_continuous(label=scales::percent) +
    coord_cartesian(ylim=c(0, 1)) +
    theme(axis.text.x = element_text(angle=60, hjust=1))
  print(g)
  ggsave("q1.png")
}

# which stat plays the greatest role in winning?
# speed, apparently
q2 <- function(pkmn, colors) {
  d <-
    pkmn %>%
    combine_stats()
  g <-
    ggplot(d,
           aes(x=value,
               y=wins/appears)) +
    facet_grid(cols=vars(stat)) +
    geom_point(aes(color=type.1)) +
    geom_smooth(method='lm', formula=y~x) +
    scale_color_manual(values=colors) +
    scale_y_continuous(label=scales::percent) +
    coord_cartesian(ylim=c(0, 1))
  print(g)
  ggsave("q2.png")
}

# is speed stat subject to Simpson paradox?
q3 <- function(pkmn, colors) {
  d <-
    pkmn %>%
    combine_types()
  g <-
    ggplot(d,
           aes(x=speed,
               y=wins/appears)) +
    facet_grid(cols=vars(type)) +
    geom_point(aes(color=type)) +
    geom_smooth(method='lm', formula=y~x) +
    scale_color_manual(values=colors) +
    scale_y_continuous(label=scales::percent) +
    coord_cartesian(ylim=c(0, 1))
  print(g)
  ggsave("q3.png")
}
