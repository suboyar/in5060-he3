library(ggplot2)
library(readxl)
library(janitor)
library(dplyr)
library(stringr)
library(knitr)
library(kableExtra)
library(gt)
library(patchwork)
library(ggbeeswarm)
library(afex)
library(emmeans)
library(lmerTest)
library(lme4)
library(tidyr)

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


## Data Exclusions
data <- data %>% filter(
                     ## Missing data entries
                     participant != 2,
                     participant != 20,
                     ## Hardware malfunction
                     participant != 7,
                     participant != 8,
                     participant != 10
                 )


boxplot_trend_latency <- function(question = NULL) {
    plot_data <- data
    if (!is.null(question)) {
        plot_data <- data %>% filter(question_type == question)
    }

    # Calculate means and SE for error bars
    means <- plot_data %>%
        group_by(latency_fct, task_type, question_type) %>%
        summarise(
            mean_response = mean(response, na.rm=TRUE),
            se = sd(response, na.rm=TRUE) / sqrt(n()),
            .groups='drop'
        )

    p <- plot_data %>%
        ggplot(aes(x = latency_fct)) +
                                        # Boxplot for distribution
        geom_boxplot(aes(y = response, fill = task_type),
                     alpha = 0.3, coef = NULL,
                     staplewidth = 0.5,
                     position = position_dodge(width = 0.8)) +
                                        # Mean line for trend
        geom_line(data = means,
                  aes(y = mean_response, color = task_type, group = task_type),
                  linewidth = 1.2, position = position_dodge(width = 0.8)) +
                                        # Mean points
        geom_point(data = means,
                   aes(y = mean_response, color = task_type),
                   size = 3, shape = 18, position = position_dodge(width = 0.8)) +
                                        # Optional: error bars
        geom_errorbar(data = means,
                      aes(y = mean_response, ymin = mean_response-se, ymax = mean_response+se,
                          color = task_type),
                      width = 0.2, position = position_dodge(width = 0.8)) +
        facet_wrap(~question_type,
                   labeller = labeller(question_type = c(
                                           "delay_perception" = "Delay Perception",
                                           "difficulty" = "Task Difficulty",
                                           "control" = "Sense of Control",
                                           "embodiment" = "Embodiment"
                                       ))) +
        scale_fill_brewer(palette = "Set2") +
        scale_color_brewer(palette = "Set2") +
        labs(
            x = "Latency (ms)",
            y = "Response (1-5)",
            fill = "Task Type",
            color = "Task Type",
            caption = expression("Boxplots show distribution; lines show mean" %+-% "SE")
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom",
              strip.text = element_text(face = "bold", size = 12),
              plot.caption = element_text(hjust = 0.5, face = "italic"))

    return(p)
}

ggsave("faceted.png", boxplot_trend_latency())
ggsave("delay_perception.png", boxplot_trend_latency("delay_perception"))
ggsave("difficulty.png", boxplot_trend_latency("difficulty"))
ggsave("control.png", boxplot_trend_latency("control"))
ggsave("embodiment.png", boxplot_trend_latency("embodiment"))

