# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(conflicted)
library(GGally)


# Data Import and Cleaning
week7_tbl <- read_csv("../data/week3.csv") %>%
  mutate(timeStart = ymd_hms(timeStart),
    condition = factor(condition, levels=c("A","B","C"), labels=c("Block A","Block B","Control")),
    gender = factor(gender, levels=c("M","F"), labels=c("Male","Female")),
    timeSpent = timeEnd - timeStart) %>%
  dplyr::filter(q6 == 1) %>%
  select(-q6)





# Visualization
week7_tbl %>%
  select(q1:q10) %>%
  ggpairs
(ggplot(week7_tbl,
        aes(x=timeStart, y=q1)) +
    geom_point() +
    xlab("Date of Experiment") +
    ylab("Q1 Score")) %>%
  ggsave(filename="../fig/fig1.png", units="px", width=1920, height=1080)
(ggplot(week7_tbl,
        aes(x=q1, y=q2, color=gender)) +
    geom_jitter() +
    scale_color_discrete("Participant Gender")) %>%
  ggsave(filename="../fig/fig2.png", units="px", width=1920, height=1080)
(ggplot(week7_tbl,
        aes(x=q1, y=q2)) +
    geom_jitter() +
    xlab("Score on Q1") +
    ylab("Score on Q2") +
    facet_wrap(. ~ gender)) %>%
  ggsave(filename="../fig/fig3.png", units="px", width=1920, height=1080)
(ggplot(week7_tbl,
        aes(x=gender, y=timeSpent)) +
    geom_boxplot() +
    xlab("Gender") +
    ylab("Time Elapsed (mins)")) %>%
  ggsave(filename="../fig/fig4.png", units="px", width=1920, height=1080)
(ggplot(week7_tbl,
       aes(x=q5, y=q7, color=condition)) +
  geom_jitter(width=.1) +
  geom_smooth(method="lm", se=F) +
  scale_x_continuous("Score on Q5") +
  scale_y_continuous("Score on Q7") +
  scale_color_discrete("Experimental Condition") +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill="#e0e0e0")
  )) %>%
ggsave(filename="../fig/fig5.png", units="px", width=1920, height=1080)
