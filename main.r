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
        latency_fct = factor(latency_num, levels = c(0, 50, 100, 150, 200)))


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
                                        # Error bars
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

save_table <- function(table){
    gtsave(table, paste(substitute(table), ".png", sep=""))
}

significant_analysis <- function() {
    anova_results <- list()
    for (q in unique(data$question_type)) {
        cat("\n========", q, "(afex) ========\n")
        d <- data %>% filter(question_type == q)

        model <-aov_ez(id = "participant",
                       dv = "response",
                       data = d,
                       within = c("latency_fct", "task_type"))

        s <- summary(model)
        ## print(s)

        anova_table <- s$univariate.tests
        result_df = data.frame(
            Effect = rownames(anova_table)[-1],
            SSA = anova_table[-1, "Sum Sq"], #
            SSE = anova_table[-1, "Error SS"], # noise not explained by the factor.
            df_num = anova_table[-1, "num Df"],
            df_den = anova_table[-1, "den Df"],
            F_value = anova_table[-1, "F value"],
            p_value = anova_table[-1, "Pr(>F)"] # hos significant this group is
        )
        result_df$eta_sq_partial <- result_df$SSA / (result_df$SSA + result_df$SSE)

        attr(result_df, "model") <- model
        anova_results[[q]] <- result_df

    }

    return(anova_results)
}

create_anova_table <- function(question_name, anova_results) {
    df <- anova_results[[question_name]]

    # Clean up effect names
    df$Effect <- c("Latency", "Task Type", "Latency × Task")

    # Format p-values
    df$p_value <- ifelse(df$p_value < .001, "< .001",
                         sprintf("%.2f", df$p_value))

    # Create gt table
    gt(df) %>%
        tab_header(
            title = paste("ANOVA Results:",
                         str_to_title(str_replace(question_name, "_", " ")))
        ) %>%
        cols_label(
            Effect = "Source",
            SSA = "SS Effect", # TODO: maybe use SSA instead
            SSE = "SS Error",  # TODO: maybe use SSE instead
            df_num = "df₁",
            df_den = "df₂",
            F_value = "F",
            p_value = "P", # TODO: make capitalized and italic
            eta_sq_partial = "η²p"
        ) %>%
        fmt_number(columns = c(SSA, SSE, F_value), decimals = 2) %>%
        fmt_number(columns = eta_sq_partial, decimals = 3) %>%
        tab_style(
            style = cell_text(weight = "bold"),
            locations = cells_body(
                columns = p_value,
                rows = p_value == "< .001"
            )
        )

}

anova_results <- significant_analysis()
anova_delay <- create_anova_table("delay_perception", anova_results)
anova_diff <- create_anova_table("difficulty", anova_results)
anova_control <- create_anova_table("control", anova_results)
anova_embodiment <- create_anova_table("embodiment", anova_results)

posthoc <- function(anova_results, alpha=0.05) {
    posthoc_results = list()
    for (q in unique(data$question_type)) {
        cat("\n========", q, "========\n")
        df <- anova_results[[q]]
        model <- attr(df, "model")

        significant_rows <- which(df$p_value < alpha)

        if (length(significant_rows) == 0) {
            cat("No significant effects found\n")
            posthoc_results[[q]] <- NULL
            next
        }

        posthoc_results[[q]] <- list()

        for (i in significant_rows) {
            effect_name <- df$Effect[i]
            cat("Post-hoc for:", effect_name, "\n")

            if (effect_name == "latency_fct") {
                ## Main effect of latency
                emm <- emmeans(model, ~ latency_fct)
                ph <- pairs(emm, adjust = "tukey")
            } else if (effect_name == "task_type") {
                ## Main effect of task type
                emm <- emmeans(model, ~ task_type)
                ph <- pairs(emm, adjust = "tukey")
            } else if (effect_name == "latency_fct:task_type") {
                ## Interaction effect - compare all combinations
                emm <- emmeans(model, ~ latency_fct * task_type)
                ph <- pairs(emm, adjust = "tukey")
            }
            posthoc_results[[q]][[effect_name]] <- ph
        }
    }

    return(posthoc_results)
}

create_posthoc_table <- function(question_name, posthoc_results) {
    if (is.null(posthoc_results[[question_name]])) {
        return(NULL)
    }

    # Extract post-hoc results for this question
    ph_list <- posthoc_results[[question_name]]

    # Combine all effects into one data frame
    all_comparisons <- list()

    for (effect_name in names(ph_list)) {
        ph_data <- as.data.frame(ph_list[[effect_name]])
        ph_data$Effect <- effect_name
        all_comparisons[[effect_name]] <- ph_data
    }

    df <- do.call(rbind, all_comparisons)
    rownames(df) <- NULL

    # Clean up effect names
    df$Effect <- gsub("latency_fct", "Latency", df$Effect)
    df$Effect <- gsub("task_type", "Task Type", df$Effect)
    df$Effect <- gsub(":", " × ", df$Effect)

    # Format p-values
    df$p.value <- ifelse(df$p.value < .001, "< .001",
                         ifelse(df$p.value < .05, sprintf("%.3f", df$p.value),
                                sprintf("%.2f", df$p.value)))

    # Create gt table
    gt(df) %>%
        tab_header(
            title = paste("Post-hoc Comparisons:",
                         str_to_title(str_replace(question_name, "_", " ")))
        ) %>%
        cols_label(
            Effect = "Effect",
            contrast = "Comparison",
            estimate = "Difference",
            SE = "SE",
            df = "df",
            t.ratio = "t",
            p.value = "p"
        ) %>%
        fmt_number(columns = c(estimate, SE, t.ratio), decimals = 3) %>%
        fmt_number(columns = df, decimals = 0) %>%
        tab_style(
            style = cell_text(weight = "bold"),
            locations = cells_body(
                columns = p.value,
                rows = p.value == "< .001"
            )
        ) %>%
        tab_style(
            style = cell_fill(color = "#f0f0f0"),
            locations = cells_body(
                rows = p.value == "< .001" |
                       (p.value != "< .001" & as.numeric(p.value) < 0.05)
            )
        )
}

posthoc_results <- posthoc(anova_results)
posthoc_delay <- create_posthoc_table("delay_perception", posthoc_results)
posthoc_diff <- create_posthoc_table("difficulty", posthoc_results)
posthoc_control <- create_posthoc_table("control", posthoc_results)
posthoc_embodiment <- create_posthoc_table("embodiment", posthoc_results)

generate_pngs <- function() {
    ggsave("faceted.png", boxplot_trend_latency())
    ggsave("delay_perception.png", boxplot_trend_latency("delay_perception"))
    ggsave("difficulty.png", boxplot_trend_latency("difficulty"))
    ggsave("control.png", boxplot_trend_latency("control"))
    ggsave("embodiment.png", boxplot_trend_latency("embodiment"))

    save_table(anova_delay)
    save_table(anova_diff)
    save_table(anova_control)
    save_table(anova_embodiment)

    save_table(posthoc_delay)
    save_table(posthoc_diff)
    save_table(posthoc_control)
    save_table(posthoc_embodiment)
}

