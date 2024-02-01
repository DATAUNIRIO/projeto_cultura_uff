library(ggplot2)
library(tibble)
library(forcats)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

set.seed(1)

dframe <- tibble(id = 1:20, 
                 res1 = sample(0:5, 20, replace = TRUE), 
                 res2 = sample(0:4, 20, replace = TRUE), 
                 diff = res1-res2,
                 cat = sample(c("a", "b", "c", "d"), 20, replace = TRUE))

dframe <- mutate(dframe, 
                 id = as.factor(id),
                 id = fct_reorder(id, diff),
                 res1 = -res1)

dframe <- gather(dframe, result_name, value, res1, res2)

breaks <- -6:6
names(breaks) <- abs(breaks)

expand_data <- data.frame(result_name = c("res1", "res2"),
                          value = c(-5, 5))

ggplot(dframe) +
  geom_col(aes(x = id, y = value, fill = cat)) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_blank(aes(y = value * 1.05), expand_data) +
  coord_flip() +
  facet_grid(~ result_name, scales = "free_x") +
  scale_y_continuous(breaks = breaks, labels = names(breaks), 
                     expand = c(0, 0)) +
  theme(panel.spacing.x = unit(0, "pt"), 
        strip.background = element_rect(colour = "black"))