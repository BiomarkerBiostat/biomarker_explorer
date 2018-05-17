#-----------------------------------------------------------------------------
# Purpose:  Function to produce association graph
# Author:   Feiyang Niu
# Date:     May 18, 2016
#-----------------------------------------------------------------------------


# load required r scripts
use_package('dplyr')
install_('lazyeval')
source('r_scripts/common_statistics.R')
source('r_scripts/plot_utils.R')
source('r_scripts/mean_se_plot.R')
source('r_scripts/median_iqr_plot.R')
source('r_scripts/spaghetti_plot.R')
source('r_scripts/contingency_table.R')


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


# ss_threshold <- 15L
# study_col <- 'STUDYID'
# subj_col <- 'SUBJID'
# param_col <- 'PARAM'
# aval_col <- 'AVAL'
# base_col <- 'BASE'
# time_unit_col <- 'AVISIT'
# time_num_col <- 'AVISITN'
# bmk_unit_col <- 'UNIT'

association_types <- c('Continuous vs Continuous',
                       'Continuous vs Categorical',
                       'Categorical vs Categorical')


# function to produce association graph
output_association <- function(file_name, data, subset_, bmkunit, study,
                               variable_x, visit_x, biomarker_x, log_x,
                               variable_y, visit_y, biomarker_y, log_y,
                               association_type = association_types,
                               group_levels = NULL, smoothing = F,
                               fnote_size = 0.8, fnote_text = '', height = 6,
                               width = 8, formats = c('pdf'),
                               table_format_convert = FALSE,
                               include_rtf = TRUE,
                               group_level_color_prefix = TRUE, alpha = 0.6) {
    if(is.null(data) || nrow(data) == 0)
        stop('There are no data to plot!')
    if(!is_blank(subset_)) {
        data <- with(data, data[eval(parse(text = subset_)), ])
    }
    if(!is_blank(study)) {
        data <- data[data[[study_col]] %in% study, , drop = FALSE]
        study_msg <- paste0('Study: ', study)
    } else
        study_msg <- ''
    association_type <- match.arg(association_type)
    if(include_rtf && (!('png' %in% formats)))
        formats <- c(formats, 'png')
    cond_x <- !(is_blank(variable_x) || is_blank(visit_x))
    cond_y <- !(is_blank(variable_y) || is_blank(visit_y))
    if(!cond_x && !cond_y)
        stop('Please provide enough column entries!')
    if(cond_x && !is_blank(variable_y) && is_blank(visit_y))
        visit_y <- visit_x
    if(cond_y && !is_blank(variable_x) && is_blank(visit_x))
        visit_x <- visit_y
    cond_x <- !(is_blank(variable_x) || is_blank(visit_x))
    cond_y <- !(is_blank(variable_y) || is_blank(visit_y))
    if(cond_x) {
        data_x <- data[data[[xlabel_col]] %in% visit_x, ]
        if(!is_blank(biomarker_x))
            data_x <- data_x[data_x[[param_col]] %in% biomarker_x, ]
        else {
            data_x <- data_x %>%
                group_by_(study_col, subj_col, xvar_col) %>%
                filter(row_number() == 1)
        }
        to_keep_x <- c(study_col, subj_col, variable_x)
        if(length(visit_x) > 1)
            to_keep_x <- c(to_keep_x, xlabel_col)
        data_x <- data_x[, to_keep_x]
    }
    if(cond_y) {
        data_y <- data[data[[xlabel_col]] %in% visit_y, ]
        if(!is_blank(biomarker_y))
            data_y <- data_y[data_y[[param_col]] %in% biomarker_y, ]
        else {
            data_y <- data_y %>%
                group_by_(study_col, subj_col, xvar_col) %>%
                filter(row_number() == 1)
        }
        to_keep_y <- c(study_col, subj_col, variable_y)
        if(length(visit_y) > 1)
            to_keep_y <- c(to_keep_y, xlabel_col)
        data_y <- data_y[, to_keep_y]
    }
    name_x <- ifelse(cond_x, ifelse(is_blank(biomarker_x), variable_x,
                                    paste(variable_x, '-', biomarker_x)), '')
    name_y <- ifelse(cond_y, ifelse(is_blank(biomarker_y), variable_y,
                                    paste(variable_y, '-', biomarker_y)), '')
    unit_x <- ifelse(cond_x && variable_x == aval_col && !is_blank(biomarker_x),
                     bmkunit[[biomarker_x]], '')
    unit_y <- ifelse(cond_y && variable_y == aval_col && !is_blank(biomarker_y),
                     bmkunit[[biomarker_y]], '')
    if((cond_x && is_blank(variable_y)) || (cond_y && is_blank(variable_x))) {
        if(cond_x && is_blank(variable_y))
            data_ass <- data_x
        else
            data_ass <- data_y
        if(cond_y && association_type != 'Continuous vs Continuous' ||
           cond_x && association_type == 'Categorical vs Categorical') {
            file_html <- paste(file_name, 'html', sep = '.')
            file.create(file_html)
            ctg_table <- html_contingency_table(data_ass, variable_x, variable_y,
                                                name_x = name_x, name_y = name_y,
                                                visit_x = visit_x,
                                                visit_y = visit_y,
                                                caption = study_msg,
                                                fnote_text = fnote_text)
            htmltbl <- ctg_table$html
            htmlpage <- html_page(htmltbl)
            cat(htmlpage, file = file_html)
            if(table_format_convert) {
                file_pdf <- paste(file_name, 'pdf', sep = '.')
                convert_command <- paste0(c(
                    'wkhtmltopdf',
                    '-q -O Landscape --minimum-font-size 40',
                    file_html, file_pdf
                ), collapse = ' ')
                system(convert_command)
            }
            table_total <- ctg_table$table
        } else {
            var_col <- ifelse(cond_x, variable_x, variable_y)
            var_name <- ifelse(cond_x, name_x, name_y)
            var_unit <- ifelse(cond_x, unit_x, unit_y)
            visit <- ternary(cond_x, visit_x, visit_y)
            to_log <- ifelse(cond_x, log_x, log_y)
            for(format in formats) {
                file <- paste(file_name, format, sep = '.')
                if(format == 'pdf')
                    pdf(file, height = height, width = width)
                else if(format == 'png')
                    png(file, height = height, width = width, units = 'in', res = 600)
                else if(format == 'jpg')
                    jpeg(file, height = height, width = width, units = 'in', res = 600)
                histogram_plot(data, var_col, var_name = var_name,
                               var_unit = var_unit, to_log = to_log,
                               visit = visit, main = study_msg,
                               add_density_curve = TRUE,
                               fnote_size = fnote_size, fnote_text = fnote_text)
                dev.off()
            }
        }
    } else {
        if(length(visit_x) > 1 || length(visit_y) > 1) {
            if(!all_equal(sort(visit_x), sort(visit_y)))
                return()
            data_ass <- merge(data_x, data_y,
                              by = c(study_col, subj_col, xlabel_col),
                              all = TRUE)
            visit_x <- ''
            visit_y <- ''
        } else {
            data_ass <- merge(data_x, data_y, by = c(study_col, subj_col),
                              all = TRUE)
        }
        if(cond_x && cond_y && variable_x == variable_y) {
            variable_x <- paste0(variable_x, '.x')
            variable_y <- paste0(variable_y, '.y')
        }
        if(association_type == 'Continuous vs Continuous') {
            for(format in formats) {
                file <- paste(file_name, format, sep = '.')
                if(format == 'pdf')
                    pdf(file, height = height, width = width)
                else if(format == 'png')
                    png(file, height = height, width = width, units = 'in', res = 600)
                else if(format == 'jpg')
                    jpeg(file, height = height, width = width, units = 'in', res = 600)
                scatter_plot(data_ass, variable_x, variable_y,
                             visit_x, visit_y, name_x, name_y,
                             to_log_x = log_x, to_log_y = log_y,
                             x_unit = unit_x, y_unit = unit_y, main = study_msg,
                             add_smooth = smoothing,
                             fnote_size = fnote_size, fnote_text = fnote_text)
                dev.off()
            }
        } else if(association_type == 'Continuous vs Categorical') {
            for(format in formats) {
                file <- paste(file_name, format, sep = '.')
                if(format == 'pdf')
                    pdf(file, height = height, width = width)
                else if(format == 'png')
                    png(file, height = height, width = width, units = 'in', res = 600)
                else if(format == 'jpg')
                    jpeg(file, height = height, width = width, units = 'in', res = 600)
                if(group_level_color_prefix && !is_blank(group_levels)) {
                    group_cols <- add_alpha(gg_color_hue(
                        length(unique(data_ass[[variable_x]]))
                    ), alpha = alpha)
                    group_cols <- group_cols[match(
                        group_levels, sort(unique(data_ass[[variable_x]]))
                    )]
                } else group_cols <- NULL
                boxplot_bmk(data_ass, variable_y, variable_x, visit_y,
                            var_name = name_y, group_lev_name = group_levels,
                            group_cols = group_cols,
                            to_log = log_y, var_unit = unit_y, main = study_msg,
                            fnote_size = fnote_size, fnote_text = fnote_text)
                dev.off()
            }
        } else {
            file_html <- paste(file_name, 'html', sep = '.')
            file.create(file_html)
            ctg_table <- html_contingency_table(data_ass, variable_x, variable_y,
                                                name_x = name_x, name_y = name_y,
                                                visit_x = visit_x,
                                                visit_y = visit_y,
                                                caption = study_msg,
                                                fnote_text = fnote_text)
            htmltbl <- ctg_table$html
            htmlpage <- html_page(htmltbl)
            cat(htmlpage, file = file_html)
            if(table_format_convert) {
                file_pdf <- paste(file_name, 'pdf', sep = '.')
                convert_command <- paste0(
                    'wkhtmltopdf -q ',
                    '--page-height ', height, 'inch ',
                    '--page-width ', width, 'inch ',
                    file_html, ' ', file_pdf
                )
                system(convert_command)
            }
            table_total <- ctg_table$table
        }
    }
    if(include_rtf) {
        png_name <- paste(file_name, 'png', sep = '.')
        if(file.exists(png_name)) {
            rtf_name <- paste(file_name, 'rtf', sep = '.')
            rtf_file <- RTF(rtf_name)
            addPng(rtf_file, png_name, width = 4.5, height = 4)
            done(rtf_file)
        }
        if(exists('table_total')) {
            rtf_name <- paste(file_name, 'rtf', sep = '.')
            rtf_file <- RTF(rtf_name)
            addTable(rtf_file, table_total, font.size = 11, row.names = TRUE,
                     NA.string = '-')
            done(rtf_file)
        }
    }
}

































