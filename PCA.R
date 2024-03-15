
install.packages("mlbench");

library(mlbench)

data("BreastCancer")

BreastCancer <- tibble::as_tibble(BreastCancer)

BreastCancer

?BreastCancer

data("BreastCancer")

summary(BreastCancer)

BreastCancer <- na.omit(BreastCancer)

predictors <- BreastCancer %>% select(Cl.thickness:Mitoses)

library(dplyr)

predictors

print("In my point of view all must be disqualified")

predictors <- predictors %>% mutate_all(~as.numeric(.))
predictors

PCA <- princomp(predictors, scores = TRUE)
BreastCancer$Id <- PCA$scores[,1]
BreastCancer$Cl.thickness <- PCA$scores[,2]

ggplot2::ggplot(x = BreastCancer, aes(BreastCancer$Id, BreastCancer$Cl.thickness)) +
  geom_point(aes(col = class))

ggplot(BreastCancer,aes(PC1, PC2)) +
  geom_point(aes(col = Class))

small_data <- data.frame(
                X = c(1, 1, -1, 1),
                Y = c(1, -1, 1, 1),
                Z = c(1, 1, -1, -1))


small_data

library(gutenbergr)
library(tidytext)

physics <- gutenberg_download(c(37729, 14725, 13476, 30155),
                              meta_fields = "author", mirror = "http://mirrors.xmission.com/gutenberg/")

physics_words <- physics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE) %>%
  ungroup()


physics_words

physics_words <- physics_words %>%
  bind_tf_idf(word, author, n)

mystopwords <- tibble(word = c("eq", "co", "rc", "ac", "eq", "ak", "fig", "file", "cg", "cb","cm"))

physics_words <-anti_join(physics_words, mystopwords, by = 'word')

plot_physics <- physics_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels=rev(unique(word)))) %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))


library(dplyr)


library(ggplot2)


ggplot2::ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf_idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()
  














