library(tidyverse)

a <- read_tsv("data/life-time.txt")

gender <- "男"
age <- 37

dl <- NULL

for (gender in c("男", "女")) {
  for (age_level in seq(0, 95, by = 5)) {
    message("process: ", gender, ", ", age_level)
    b <- tibble(age_lower = a$`年龄下限`,
                age_upper = a$`年龄上限`,
                total = a[[paste0("总人口数_", gender)]],
                dead = a[[paste0("死亡人数_", gender)]])
    
    d <- tibble(age = age_level:100) %>%
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
    d <- d %>% mutate(age_level = age_level, gender = gender)

    if (is.null(dl)) {
      dl <- d
    } else {
      dl <- rbind(dl, d)
    }
  }
}

for (i in 0:4) {
  g <- dl %>%
    arrange(age) %>%
    filter(floor(age_level / 20) == i) %>%
    ggplot(aes(x = age, y = survival_prob * 100, color = gender, group = gender)) +
    geom_path() +
    facet_wrap(~ paste0("当前年龄 = ", age_level)) +
    scale_x_continuous(breaks = seq(0,100,by=10), limits = c(0,100)) +
    scale_y_continuous(breaks = seq(0,100,by=20), limits = c(0,100)) +
    labs(x = "年龄", y = "存活概率 (%)", color = "性别")
  g %>% ggsave(filename = paste0("plot.", (i + 1), ".png"), width = 8, height = 6)
}
