#-----------------------------------------------------------------------------
# Purpose:  Function to produce time-profiling graph
# Author:   Feiyang Niu
# Date:     May 18, 2016
#-----------------------------------------------------------------------------


# load required r scripts
use_package('dplyr')
install_('lazyeval')
source('r_scripts/common_statistics.R')
source('r_scripts/strings.R')
source('r_scripts/mean_se_plot.R')
source('r_scripts/median_iqr_plot.R')
source('r_scripts/spaghetti_plot.R')


# Define defaults
ss_threshold <- 0L
study_col <- 'STUDYID'
subj_col <- 'USUBJID'
param_col <- 'PARAM'
aval_col <- 'AVAL'
base_col <- 'BASE'
chg_col <- 'CHG'
pchg_col <- 'PCHG'
prchg_col <- 'PRCHG'
xvar_col <- 'XTICKER'
xlabel_col <- 'XTICKERL'
time_graph_list <- c('Boxplot', 'Mean + SE plot', 'Mean + SD plot',
                     'Median + IQR plot', 'Spaghetti plot')
# study_col <- 'STUDYID'
# subj_col <- 'USUBJID'
# param_col <- 'PARAM'
# aval_col <- 'AVAL'
# base_col <- 'BASE'
# time_unit_col <- 'AVISIT'
# time_num_col <- 'AVISITN'
# bmk_unit_col <- 'UNIT'
# time_y_list <- c('Raw', '% baseline')


# function to produce time-profiling graph
output_time_profiling <- function(file_name, data, subset_, study, biomarker,
                                  y_variable, graph_type = time_graph_list,
                                  group = '', group_levels = '',
                                  to_log_y = TRUE, add_sample_size = TRUE,
                                  add_points = TRUE, add_subjid = FALSE,
                                  reference_line = NULL,
                                  xmin = NULL, xmax = NULL,
                                  ymin = NULL, ymax = NULL,
                                  line_cols = NULL, line_types = NULL,
                                  xtick = xvar_col,
                                  xlab = '', ylab = '', main = '',
                                  sample_size_threshold = ss_threshold,
                                  fnote_size = 0.9, fnote_text = '',
                                  height = 8, width = 16,
                                  formats = c('pdf'), include_rtf = FALSE) {
    if(is.null(data) || nrow(data) == 0)
        stop('There are no data to plot!')
    if(!is_blank(subset_)) {
        data <- with(data, data[eval(parse(text = subset_)), ])
    }
    if(!is_blank(study)) {
        data <- data[data[[study_col]] %in% study, , drop = FALSE]
        study_msg <- paste0('Study: ', study, '\n')
    }
    if(is_blank(biomarker))
        stop('Biomarker can not be empty')
    cond_x <- between(data[[xvar_col]],
                      ifelse(is_blank(xmin), -Inf, xmin),
                      ifelse(is_blank(xmax), Inf, xmax))
    data <- data[data[[param_col]] %in% biomarker & cond_x, , drop = FALSE]
    if(to_log_y) {
        value <- data[[y_variable]]
        value[value == 0 & !is.na(value)] <- min_na(value[value != 0]) / 2
        data[[y_variable]] <- value
    }
    expr <- list(lazyeval::interp(~mean_na(var), var = as.name(xvar_col)),
                 lazyeval::interp(~sum(!is.na(var)), var = as.name(y_variable)))
    dots <- setNames(expr, c(xvar_col, 'N'))
    sample_size <- data %>%
        group_by_(xlabel_col) %>%
        summarise_(.dots = dots) %>%
        filter(N >= sample_size_threshold)
    data <- data[data[[xvar_col]] <= max_na(sample_size[[xvar_col]]),
                 , drop = FALSE]
    reference_line <- as.numeric(reference_line)
    add_legend <- ifelse(is_blank(group), FALSE, TRUE)
    if(include_rtf && (!('png' %in% formats)))
        formats <- c(formats, 'png')
    for(format in formats) {
        format <- tolower(format)
        file_ <- paste(file_name, format, sep = '.')
        if(format == 'pdf')
            pdf(file_, height = height, width = width)
        else if(format == 'png')
            png(file_, height = height, width = width, units = 'in', res = 600)
        else if(format == 'jpg')
            jpeg(file_, height = height, width = width, units = 'in', res = 600)
        if(graph_type == 'Spaghetti plot') {
            spaghetti_plot(data, subj_col, y_variable, xvar_col,
                           group, group_levs = group_levels, to_log = to_log_y,
                           bmk_name = biomarker, xlab = xlab,
                           ylab = ylab, main = main,
                           ylim = c(ifelse(is_blank(ymin), -Inf, ymin),
                                    ifelse(is_blank(ymax), Inf, ymax)),
                           reference_line = reference_line, x_tick = xtick,
                           fnote_size = fnote_size, fnote_text = fnote_text,
                           add_legend = add_legend, add_subjid = add_subjid,
                           add_points = add_points, include_n = add_sample_size,
                           line_cols = line_cols, line_types = line_types,
                           output_plot = TRUE)
        } else if(graph_type == 'Mean + SE plot') {
            mean_se_plot(data, y_variable, xvar_col, group,
                         group_levs = group_levels,
                         to_log = to_log_y, bmk_name = biomarker, method = 'SE',
                         xlab = xlab, ylab = ylab, main = main,
                         ylim = c(ifelse(is_blank(ymin), -Inf, ymin),
                                  ifelse(is_blank(ymax), Inf, ymax)),
                         reference_line = reference_line, x_tick = xtick,
                         fnote_size = fnote_size, fnote_text = fnote_text,
                         add_legend = add_legend, include_n = add_sample_size,
                         line_cols = line_cols, line_types = line_types,
                         output_plot = TRUE)
        } else if(graph_type == 'Mean + SD plot') {
            mean_se_plot(data, y_variable, xvar_col, group,
                         group_levs = group_levels,
                         to_log = to_log_y, bmk_name = biomarker, method = 'SD',
                         xlab = xlab, ylab = ylab, main = main,
                         ylim = c(ifelse(is_blank(ymin), -Inf, ymin),
                                  ifelse(is_blank(ymax), Inf, ymax)),
                         reference_line = reference_line, x_tick = xtick,
                         fnote_size = fnote_size, fnote_text = fnote_text,
                         add_legend = add_legend, include_n = add_sample_size,
                         line_cols = line_cols, line_types = line_types,
                         output_plot = TRUE)
        } else if(graph_type == 'Median + IQR plot') {
            median_iqr_plot(data, y_variable, xvar_col, group,
                            group_levs = group_levels,
                            to_log = to_log_y, bmk_name = biomarker,
                            xlab = xlab, ylab = ylab, main = main,
                            ylim = c(ifelse(is_blank(ymin), -Inf, ymin),
                                     ifelse(is_blank(ymax), Inf, ymax)),
                            reference_line = reference_line, x_tick = xtick,
                            fnote_size = fnote_size, fnote_text = fnote_text,
                            add_points = add_points, add_legend = add_legend,
                            include_n = add_sample_size,
                            line_cols = line_cols, line_types = line_types,
                            output_plot = TRUE)
        } else if(graph_type == 'Boxplot') {
            boxplot_time(data, y_variable, xvar_col, group,
                         group_levs = group_levels,
                         to_log = to_log_y, bmk_name = biomarker,
                         xlab = xlab, ylab = ylab, main = main,
                         ylim = c(ifelse(is_blank(ymin), -Inf, ymin),
                                  ifelse(is_blank(ymax), Inf, ymax)),
                         reference_line = reference_line, x_tick = xtick,
                         fnote_size = fnote_size, fnote_text = fnote_text,
                         add_points = add_points, add_legend = add_legend,
                         include_n = add_sample_size,
                         line_cols = line_cols, line_types = line_types,
                         output_plot = TRUE)
        }
        dev.off()
    }
    if(include_rtf) {
        rtf_name <- paste(file_name, 'rtf', sep = '.')
        png_name <- paste(file_name, 'png', sep = '.')
        rtf_file <- RTF(rtf_name)
        addPng(rtf_file, png_name, width = 6.5, height = 4)
        done(rtf_file)
    }
}




























