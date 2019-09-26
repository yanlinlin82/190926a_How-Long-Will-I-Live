library(tidyverse)

a <- read_tsv("data/life-time.txt")

gender <- "男"
age <- 37

b <- tibble(age_lower = a$`年龄下限`,
            age_upper = a$`年龄上限`,
            total = a[[paste0("总人口数_", gender)]],
            dead = a[[paste0("死亡人数_", gender)]])

d <- tibble(age = age:100) %>%
  mutate(death_ratio = sapply(age, function(n) {
    b %>% filter(n >= age_lower & n <= ifelse(is.na(age_upper), Inf, age_upper)) %>% with(dead / total)
  }))

d <- d %>%
  mutate(death_prob = ifelse(row_number() == 1, death_ratio, NA),
         survival_prob = ifelse(row_number() == 1, 1 - death_ratio, NA))

for (i in 2:nrow(d)) {
  d$death_prob[[i]] <- d$survival_prob[[i - 1]] * d$death_ratio[[i]]
  d$survival_prob[[i]] <- d$survival_prob[[i - 1]] * (1 - d$death_ratio[[i]])
}

g <- d %>%
  ggplot(aes(x = age, y = survival_prob * 100)) +
  geom_segment(aes(xend = age, yend = 0, color = (age %% 5 == 0))) +
  geom_point(size = 3, color = "red") +
  scale_color_manual(values = c("darkgray", "black")) +
  scale_x_continuous(breaks = seq(35,100,by=5), minor_breaks=FALSE) +
  scale_y_continuous(breaks = seq(0,100,by=10)) +
  guides(color = FALSE) +
  labs(x = "Age", y = "Survival Probability (%)") +
  ggtitle("How Long Will I Live?")
g %>% ggsave(filename = "plot.png", width = 12, height = 7)
