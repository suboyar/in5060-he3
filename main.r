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

quest <- read_excel("LatencyPerception/questionnaire_data-561422-2025-11-21-1620.xlsx")
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
    drop_na(task_type, repetition, condition, latency) %>%
    mutate(
        response = as.numeric(response),
        latency_num = as.numeric(str_remove(latency, "ms")),
        latency_fct = factor(latency_num, levels = c(0, 50, 100, 150, 200)),
        question_type = factor(question_type,
                               levels = c("delay_perception", "difficulty", "control", "embodiment")))

create_summary <- function() {
    data %>%
        summarise(
            N = n(),
            `Age (Mean ± SD)` = sprintf("%.1f ± %.1f",
                                        mean(`How old are you?`, na.rm = TRUE),
                                        sd(`How old are you?`, na.rm = TRUE)),
            `Male` = sum(`What is your gender` == "Male"),
            `Female` = sum(`What is your gender` == "Female"),
            `Other` = sum(`What is your gender` == "Other"),
            `Right-handed` = sum(str_detect(`What is your dominant hand?`, "Right")),
            `Left-handed` = sum(str_detect(`What is your dominant hand?`, "Left")),
            `Novice (Experience 1-2)` = sum(`How experienced are you with robotic systems?` %in% c("1", "2")),
            `Intermediate (Experience 3-4)` = sum(`How experienced are you with robotic systems?` %in% c("3", "4")),
            `Expert (Experience 5)` = sum(`How experienced are you with robotic systems?` %in% c("5")),
            ) %>%
        t() %>%
        as.data.frame()
}

demographics_summary <- function() {

    calculate_demographics <- function(data) {
        tibble(
            Characteristic = c(
                "N",
                "Age (Mean ± SD)",
                "Male",
                "Female",
                "Other",
                "Right-handed",
                "Left-handed",
                "Novice (Experience 1-2)",
                "Intermediate (Experience 3-4)",
                "Expert (Experience 5)"
            ),
            Value = c(
                nrow(data),
                sprintf("%.1f ± %.1f", mean(data[["How old are you?"]], na.rm = TRUE), sd(data[["How old are you?"]], na.rm = TRUE)),
                sum(data[["What is your gender"]] == "Male", na.rm = TRUE),
                sum(data[["What is your gender"]] == "Female", na.rm = TRUE),
                sum(data[["What is your gender"]] == "Other", na.rm = TRUE),
                sum(str_detect(data[["What is your dominant hand?"]], "Right"), na.rm = TRUE),
                sum(str_detect(data[["What is your dominant hand?"]], "Left"), na.rm = TRUE),
                sum(data[["How experienced are you with robotic systems?"]] %in% c(1, 2), na.rm = TRUE),
                sum(data[["How experienced are you with robotic systems?"]] %in% c(3, 4), na.rm = TRUE),
                sum(data[["How experienced are you with robotic systems?"]] == 5, na.rm = TRUE)
            )
        )
    }

    all_participants <- quest %>%
        distinct(participant = `Participant number`, .keep_all = TRUE) %>%
        calculate_demographics()

    valid_participants <- data %>%
        distinct(participant) %>%
        pull(participant)

    valid_only <- quest %>%
        filter(`Participant number` %in% valid_participants) %>%
        distinct(participant = `Participant number`, .keep_all = TRUE) %>%
        calculate_demographics()

    demo_summary <- all_participants %>%
        rename(`All Participants` = Value) %>%
        left_join(
            valid_only %>% rename(`Valid Participants` = Value),
            by = "Characteristic"
        )

    return(demo_summary)
}

demographics_summary_table <- function(paper = "#eaeaea", ink = "#032c3c", accent="#e1dcd8") {

    demo_summary <- demographics_summary()

    demo_summary_table <- demo_summary %>%
        gt() %>%
        tab_header(
            title = "Participant Demographics",
            subtitle = "Comparison of all participants and those with valid data"
        ) %>%
        tab_options(
            table.background.color = paper,
            table.font.color = ink,
            table.border.left.style = "solid",
            table.border.left.width = px(2),
            table.border.right.style = "solid",
            table.border.right.width = px(2),
            ) %>%
        tab_style(
            style = cell_text(color = ink),
            locations = list(
                cells_title(),
                cells_column_labels(),
                cells_body()
            )
        ) %>%
        tab_style(
            style = cell_text(weight = "bold"),
            locations = cells_body(columns = Characteristic)
        ) %>%
        tab_style(
            style = cell_fill(color = accent),
            locations = cells_column_labels()
        ) %>%
        cols_align(
            align = "left",
            columns = Characteristic
        ) %>%
        cols_align(
            align = "center",
            columns = c(`All Participants`, `Valid Participants`)
        )

    return(demo_summary_table)
}

boxplot_explanation_plot <- function(paper = "#eaeaea", ink = "#032c3c") {

    sample_df <- data.frame(parameter = "test", values = 1:100)

    ggplot2_boxplot <- function(x){

        quartiles <- as.numeric(quantile(x,
                                         probs = c(0.25, 0.5, 0.75)))

        names(quartiles) <- c("25th percentile",
                              "Median",
                              "75th percentile")

        IQR <- diff(quartiles[c(1,3)])

        upper_whisker <- max(x)
        lower_whisker <- min(x)

        return(list("quartiles" = quartiles,
                    "25th percentile" = as.numeric(quartiles[1]),
                    "median" = as.numeric(quartiles[2]),
                    "75th percentile" = as.numeric(quartiles[3]),
                    "IQR" = IQR,
                    "upper_whisker" = upper_whisker,
                    "lower_whisker" = lower_whisker))
    }

    ggplot_output <- ggplot2_boxplot(sample_df$values)

    set1_color <- tail(RColorBrewer::brewer.pal(4, "Set1"), n=1)

    fontsize <- 5.0
    fontfamily <- ""
    update_geom_defaults("text", list(size = fontsize, family = fontfamily))
    update_geom_defaults("label", list(size = fontsize, family = fontfamily))

    x_bracket_line <- 1.55
    x_bracket_tick <- 1.35
    x_iqr_text <- x_bracket_line + 0.17
    x_whisker_text <- 1.30
    x_quartile_label <- 1.30

    explain_plot <- ggplot() +
        geom_boxplot(data = sample_df,
                     aes(x = parameter, y = values),
                     fill = set1_color,
                     width = 0.20,
                     alpha=0.3,
                     staplewidth = 0.5,
                     coef = NULL) +
        theme_void(base_size = 14, paper = paper, ink = ink) +
        geom_segment(aes(x = x_bracket_line, xend = x_bracket_line ,
                         y = ggplot_output[["25th percentile"]],
                         yend = ggplot_output[["75th percentile"]]),
                     color = ink) +
        geom_segment(aes(x = x_bracket_tick, xend = x_bracket_line,
                         y = ggplot_output[["25th percentile"]],
                         yend = ggplot_output[["25th percentile"]])) +
        geom_segment(aes(x = x_bracket_tick, xend = x_bracket_line,
                         y = ggplot_output[["75th percentile"]],
                         yend = ggplot_output[["75th percentile"]])) +
        geom_text(aes(x = x_iqr_text, y = ggplot_output[["median"]]),
                  label = "Interquartile", fontface = "bold",
                  vjust = 0.4) +
        geom_text(aes(x = c(x_whisker_text, x_whisker_text),
                      y = c(ggplot_output[["upper_whisker"]],
                            ggplot_output[["lower_whisker"]]),
                      label = c("Maximum",
                                "Minimum"))) +
        geom_label(aes(x = x_quartile_label, y = ggplot_output[["quartiles"]],
                       label = names(ggplot_output[["quartiles"]])),
                   linewidth = 0) +
        ylab("") + xlab("") +
        coord_cartesian(clip = "off") +
        theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            plot.margin = margin(t = 5, r = 100, b = 5, l = 5, unit = "pt"),
        )
    return(explain_plot)
}

boxplot_trend_latency <- function(question = NULL, paper = "#eaeaea", ink = "#032c3c", alpha=100) {
    plot_data <- data
    if (!is.null(question)) {
        plot_data <- data %>% filter(question_type == question)
    }

    ## Calculate means and SE for error bars
    means <- plot_data %>%
        group_by(latency_fct, task_type, question_type) %>%
        summarise(
            mean_response = mean(response, na.rm=TRUE),
            se = sd(response, na.rm=TRUE) / sqrt(n()),
            .groups='drop'
        )

    p <- plot_data %>%
        ggplot(aes(x = latency_fct)) +
        ## Boxplot for distribution
        geom_boxplot(aes(y = response, fill = task_type),
                     alpha = 0.3, coef = NULL,
                     staplewidth = 0.5,
                     position = position_dodge(width = 0.8)) +
        ## Mean line for trend
        geom_line(data = means,
                  aes(y = mean_response, color = task_type, group = task_type),
                  linewidth = 1.2, position = position_dodge(width = 0.8)) +
        ## Mean points
        geom_point(data = means,
                   aes(y = mean_response, color = task_type),
                   size = 3, shape = 18, position = position_dodge(width = 0.8)) +
        ## Error bars
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
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1") +
        labs(
            x = "Latency (ms)",
            y = "Response (1-5)",
            fill = "Task Type",
            color = "Task Type",
            caption = expression("Boxplots show distribution; lines show mean" %+-% "SE")
        ) +
        theme_linedraw(base_size = 14, paper = alpha(paper, alpha), ink = ink) +
        theme(legend.position = "bottom",
              strip.text = element_text(face = "bold", size = 12),
              plot.caption = element_text(hjust = 0.5, face = "italic"),
              panel.grid.minor = element_blank())

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
                ## Interaction effect - compare task type at each latency level
                emm <- emmeans(model, ~ task_type | latency_fct)
                ph <- pairs(emm, adjust = "bonferroni")
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

