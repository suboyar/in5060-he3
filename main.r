library(ggplot2)
library(readxl)
library(janitor)
library(dplyr)
library(stringr)

quest <- read_excel("LatencyPerception/questionnaire_data-561422-2025-11-11-1622.xlsx")
exp <- read.csv("LatencyPerception/participant_experiment.csv")

quest_long <- quest %>%
    select(participant = 3,
           gender = 4,
           age = 5,
           hand = 6,
           experience = 7,
           answer_time_ms = 48,
           8:47) %>%              # Response columns
    pivot_longer(
        cols = 7:46,
        values_to = "response"
    ) %>%
    group_by(participant) %>%
    mutate(
        trial_order = rep(1:10, each = 4),
        question_type = rep(c("delay_perception", "difficulty", "control", "embodiment"), 10)) %>%
    ungroup() %>%
    select(participant, gender, age, hand, experience, answer_time_ms, trial_order, question_type, response)

data <- quest_long %>%
    left_join(exp, by = c("participant", "trial_order")) %>%
    filter(!is.na(task_type)) %>%
    filter(latency != "") %>%
    mutate(
        response = as.numeric(response),
        latency_num = as.numeric(str_remove(latency, "ms")),
        latency_fct = factor(latency_num, levels = c(0, 50, 100, 150, 200, 250)))

data %>%
    ggplot(aes(x = latency_fct, y = response, fill = task_type)) +
    geom_boxplot(coef=NULL) +
    facet_wrap(~question_type) +
    labs(
        x = "Latency (ms)",
        y = "Response (1-5)",
        fill = "Task Type"
    ) +
    theme_minimal()
