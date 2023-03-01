library(tidyverse)
library(psych)
data(bfi)
bfi_tbl <- bfi %>%
  rowwise() %>%
  mutate(A = mean(c(A1, A2, A3, A4, A5), na.rm=T)) %>%
  ungroup()
bfi_fig1 <- bfi_tbl %>%
  ggplot(aes(x = A)) +
  geom_histogram() +
  scale_x_continuous("Agreeableness")
bfi_fig2 <- bfi_tbl %>%
  ggplot(aes(x = A, group=age, color=age, fill=age)) +
  geom_histogram() +
  scale_x_continuous("Agreeableness") +
  scale_y_continuous("Number of Cases")
bfi_fig3 <- bfi_tbl %>%
  ggplot(aes(x = A, group=age, color=age, fill=age)) +
  geom_dotplot() +
  geom_histogram(fill="#39B961") +
  scale_x_continuous("Agreeableness") +
  scale_y_continuous("Number of Cases")
bfi_fig4 <- bfi_tbl %>%
  ggplot(aes(x = A1, y = A2)) +
  geom_jitter()
bfi_fig5 <- bfi_tbl %>%
  ggplot(aes(x = A1, y = A2)) +
  geom_jitter() +
  facet_grid(gender ~ .) +
  theme_bw()
bfi_fig5

library(GGally)
bfi_tbl %>% select(A1, A5, C2, gender) %>%
  mutate_all(as.factor) %>% ggpairs
