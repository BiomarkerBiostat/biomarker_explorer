#-----------------------------------------------------------------------------
# Purpose:  Function to draw scatter plot between two biomarkers
# Author:   Feiyang Niu
# Date:     April 19, 2016
#-----------------------------------------------------------------------------


# Use ggplot2 default color palette
source('r_scripts/plot_utils.R')
source('r_scripts/common_statistics.R')
source('r_scripts/stats_utils.R')


# Draw scatter plot between two biomarkers
scatter_plot <- function(data, var_x, var_y, subj_col = NULL,
                         visit_x = 'Baseline', visit_y = 'Baseline',
                         name_x = var_x, name_y = var_y,
                         to_log_x = TRUE, to_log_y = TRUE,
                         x_unit = '', y_unit = '', xlab = '', ylab = '',
                         main = NULL, xlim = NULL, ylim = NULL,
                         add_line = NULL, line_col = 'darkred',
                         add_subjid = FALSE, subjid_size = 0.6, point_colors = NULL,
                         test_method = NULL, fnote_size = 0.9, fnote_text = '') {
    non_missing <- (!is.na(data[[var_x]])) & (!is.na(data[[var_y]]))
    data <- data[non_missing, ]
    if(nrow(data) == 0)
        stop('There are 0 rows to plot!')
    bmk_x <- data[[var_x]]
    bmk_y <- data[[var_y]]
    fnote_text <- trimws(fnote_text)
    log_opt <- ''
    if(is.logical(to_log_x) && to_log_x) {
        bmk_x[bmk_x == 0] <- min(bmk_x[bmk_x != 0]) / 2
        log_opt <- paste0(log_opt, 'x')
    }
    if(is.logical(to_log_y) && to_log_y) {
        bmk_y[bmk_y == 0] <- min(bmk_y[bmk_y] != 0) / 2
        log_opt <- paste0(log_opt, 'y')
    }
    add_subjid <- is_true(add_subjid)
    xlim <- ternary(is.null(xlim), range_na(bmk_x),
                    intersection(xlim, range_na(bmk_x)))
    ylim <- ternary(is.null(ylim), range_na(bmk_y),
                    intersection(ylim, range_na(bmk_y)))
    if(is_true(to_log_x)) {
        test_x <- log(bmk_x)
        if(add_subjid) {
            xlim <- exp(extendrange(r = log(xlim), f = 0.075))
        }
    } else {
        test_x <- bmk_x
        if(add_subjid) xlim <- extendrange(r = xlim, f = 0.075)
    }
    if(is_true(to_log_y)) test_y <- log(bmk_y) else test_y <- bmk_y
    if(!is_blank(test_method)) {
        test_method <- match.arg(tolower(test_method),
                                 c('spearman', 'pearson'))
        test_result <- cor.test(test_x, test_y, method = test_method)
        cor_value <- test_result$estimate
        p_value <- test_result$p.val
        if(test_method == 'pearson') conf_int <- CIr(cor_value, length(test_x))
        else conf_int <- c(CIrho(cor_value, length(test_x)))[-1]
        p_value <- ifelse(p_value < 0.001, '< 0.001', specify_decimal(p_value, 3))
        cor_value <- specify_decimal(cor_value, 3)
        conf_int <- specify_decimal(conf_int, 3)
        test_msg <- paste(
            list('spearman' = 'Spearman', 'pearson' = 'Pearson')[test_method],
            'correlation: ', cor_value, '; 95% CI: (',
            paste0(conf_int, collapse = ', '), '); P value: ', p_value
        )
        fnote_text <- ternary(is_blank(fnote_text), test_msg, c(test_msg, fnote_text))
    }
    main <- trimws(main)
    if(is_blank(xlab))
        xlab <- trimws(paste(ifelse(is_blank(visit_x), '', visit_x), name_x,
                             add_parenthesis(x_unit)))
    if(is_blank(ylab))
        ylab <- trimws(paste(ifelse(is_blank(visit_y), '', visit_y), name_y,
                             add_parenthesis(y_unit)))
    add_fnote <- FALSE
    if(is_blank(fnote_text)) {
        par_opt <- par(mar = c(4.2, 3.2, 2.5, 0.5), 
                       cex.lab = 1, cex.main = 1.1, pty = 's')
    }
    else {
        add_fnote <- TRUE
        fnote_lines <- length(fnote_text)
        par_opt <- par(mar = c(4.2, 3.2, 2.5, 0.5),
                       oma = c(fnote_lines + 0.5, 0, 0, 0),
                       cex.lab = 1, cex.main = 1.1, pty = 's')
    }
    if(is.null(point_colors))
        point_colors <- add_alpha(1, 0.65)
    plot(bmk_x, bmk_y, log = log_opt, xlab = xlab, ylab = ylab, main = main,
         xlim = xlim, ylim = ylim, pch = 20, cex = 1.5, col = point_colors)
    if(add_subjid && !is.null(subj_col)) {
        text(bmk_x, bmk_y, data[[subj_col]], cex = subjid_size, pos = 1,
             offset = .2, col = point_colors)
    }
    if(!is_blank(add_line)) {
        add_line <- match.arg(
            tolower(add_line),
            choices = c('linear regression line', 'loess curve', 'identity line'
        ))
        srt_x <- order(bmk_x)
        if(is_blank(line_col)) line_col <- 'darkred'
        fml <- formula(paste(
            ifelse(is_true(to_log_y), 'log(bmk_y)', 'bmk_y'),
            ifelse(is_true(to_log_x), 'log(bmk_x)', 'bmk_x'), sep = '~'
        ))
        if(add_line == 'linear regression line') {
            fit_lm <- lm(fml)
            pred_lm <- predict(fit_lm, interval = 'confidence')
            if(is_true(to_log_y)) pred_lm <- exp(pred_lm)
            lines(bmk_x[srt_x], pred_lm[srt_x, 1], col = line_col, lwd = 2)
            lines(bmk_x[srt_x], pred_lm[srt_x, 2], col = line_col, lwd = 2, lty = 2)
            lines(bmk_x[srt_x], pred_lm[srt_x, 3], col = line_col, lwd = 2, lty = 2)
        } else if(add_line == 'loess curve') {
            fit_loess <- loess(fml)
            pred_loess <- predict(fit_loess, se = FALSE)
            if(is_true(to_log_y)) pred_loess <- exp(pred_loess)
            lines(bmk_x[srt_x], pred_loess[srt_x], col = line_col, lwd = 2)
        } else if(add_line == 'identity line') {
            lines(bmk_x[srt_x], bmk_x[srt_x], col = line_col, lwd = 2)
        }
    }
    if(add_fnote) {
        mtext(fnote_text, side = 1, line = 0.3 + (0:(fnote_lines - 1)),
              adj = 0, cex = fnote_size, outer = TRUE)
    }
    par <- par(par_opt)
}



























