library(tidyverse)

mtcars %>%
  select(cyl, hp, disp) %>% 
  mutate(pwr = hp / disp) %>%
  group_by(cyl) %>%
  summarize(power_avg = mean(pwr))

select(mtcars, cyl, hp, disp) %>%
  mutate(pwr = hp / disp) %>%
  group_by(cyl) %>%
  summarize(power_avg = mean(pwr))

data(starwars)

starwars %>%
  group_by(species) %>%
  # ungroup() %>%
  summarize(avg_height = mean(height, na.rm = T), 
            avg_mass = mean(mass, na.rm = T),
            group_n = n()) %>%
  # arrange(desc(group_n)) %>%
  slice_max(group_n, n=5)
  # View

library(psych)

my_numeric_func <- function(x) {
  as.numeric(x)
}

data(bfi)
bfi_tbl <- bfi %>%
  mutate(A1 = as.numeric(A1)) %>%
  mutate(across(A1:A5, function(x) as.numeric(x))) %>%
  mutate(across(C1:C5, ~ as.numeric(.))) %>%
  mutate(across(E1:E5, as.numeric)) %>%
  mutate(across(N1:N5, my_numeric_func),
         across(A1:O5, ~ {
           as.numeric(.)
           as.character(.)
         }))

# new_tbl <- bfi_tbl %>%
  
bfi %>% 
  rowwise() %>%
  mutate(A = mean(c(A1, A2, A3, A4, A5), na.rm=T)) %>%
  select(A) %>%
  ungroup
  View

out <- separate(my_tbl, col=“qs”, into=c(“q1”,”q2”, sep=“-”))

uhoh <- function(x) { return(q) }

