#-----------------------------------------------------------------------------
# Purpose:  Function to draw boxplot for biomarker
# Author:   Feiyang Niu
# Date:     April 18, 2016
#-----------------------------------------------------------------------------


# Use ggplot2 default color palette
source('r_scripts/plot_utils.R')
source('r_scripts/common_statistics.R')


# Draw boxplot for biomarker
boxplot_bmk <- function(data, var_col, group_col = NULL, subj_col = NULL,
                        visit = 'Baseline', var_name = var_col,
                        group_lev_name = NULL, group_cols = NULL, to_log = TRUE,
                        var_unit = '', alpha = 0.6, xlab = '', ylab = '',
                        main = group_col, xlim = NULL, ylim = NULL,
                        test_method = NULL, add_points = TRUE,
                        add_subjid = FALSE, subjid_size = 0.6, fnote_size = 0.8,
                        fnote_text = '', seed = 0) {
    set.seed(0)
    data <- data[!is.na(data[[var_col]]), , drop = F]
    if(is_blank(group_col)) {
        group <- factor(as.character(rep.int(1, nrow(data))))
    } else{
        data <- data[!is.na(data[[group_col]]), , drop = FALSE]
        if(!is_blank(group_lev_name))
            group <- factor(data[[group_col]], levels = group_lev_name)
        else
            group <- factor(data[[group_col]])
    }
    if(nrow(data) == 0)
        stop('There are 0 rows to plot!')
    group_levs <- levels(group)
    n_levs <- length(group_levs)
    x_coord <- rep_jitter(as.numeric(group),
                          factor = ifelse(n_levs == 1, 4, 1), seed = seed)
    bmk <- data[[var_col]]
    if(to_log) {
        zero_idx <- bmk == 0
        if(any(zero_idx, na.rm = TRUE))
            bmk[which(zero_idx)] <- min_na(bmk[!zero_idx]) / 2
    }
    xlim <- ternary(is.null(xlim), NULL, intersection(xlim, range_na(x_coord)))
    ylim <- ternary(is.null(ylim), range_na(bmk),
                    intersection(ylim, range_na(bmk)))
    point_colors <- factor(group)
    if(is.null(group_cols))
        levels(point_colors) <- add_alpha(gg_color_hue(n_levs), alpha = alpha)
    else
        levels(point_colors) <- group_cols
    point_colors <- as.character(point_colors)
    log_opt <- ifelse(to_log, 'y', '')
    sizes <- unname(sapply(split(bmk, group), n_nna))
    if(is_blank(ylab)) {
        ylab <- trimws(paste(ifelse(is_blank(visit), '', visit), var_name,
                             add_parenthesis(var_unit)))
    }
    if(is_blank(xlab) && !is_blank(group_col))
        xlab <- group_col
    fnote_text <- trimws(fnote_text)
    add_fnote <- FALSE
    y_tick_label <- axis_ticks(ylim, to_log = to_log)
    y_tick_label <- sprintf(
        paste0('%.', max(sapply(y_tick_label, decimalplaces)), 'f'), y_tick_label
    )
    x_lab_lines <- if(is_blank(xlab)) 0 else length(strsplit(xlab, '\n')[[1]])
    y_lab_lines <- if(is_blank(ylab)) 0 else length(strsplit(ylab, '\n')[[1]])
    main_lines <- if(is_blank(main)) 0 else length(strsplit(main, '\n')[[1]])
    main_fsize <- 1.2
    x_tick_lines <- ifelse(
        is_blank(group_col), 0.3,
        0.5 + gceiling(max_na(strlines(group_levs)) / sqrt(2), 0.5)
    )
    if(!is_blank(group_col)) {
        if(!is_blank(test_method)) {
            test_method <- match.arg(
                tolower(test_method),
                c('wilcoxon', 'kruskal', 't', 'anova', 'parametric', 'non-parametric')
            )
            fml <- formula(paste0(
                ifelse(is_true(to_log), 'log(bmk)', 'bmk'), '~group'
            ))
            if(test_method == 'wilcoxon' ||
               (test_method == 'non-parametric' && n_levs == 2)) {
                test_method <- 'Wilcoxon rank sum test'
                test_result <- tryCatch(suppressWarnings(wilcox.test(fml)),
                                        error = function(e) {NULL})
            } else if(test_method == 'kruskal' ||
                      (test_method == 'non-parametric' && n_levs > 2)) {
                test_method <- 'Kruskal-Wallis test'
                test_result <- tryCatch(suppressWarnings(kruskal.test(fml)),
                                        error = function(e) {NULL})
            } else if(test_method == 't' ||
                      (test_method == 'parametric' && n_levs == 2)) {
                test_method <- 'T test'
                test_result <- tryCatch(suppressWarnings(t.test(fml)),
                                        error = function(e) {NULL})
            } else if(test_method == 'anova' ||
                      (test_method == 'parametric' && n_levs > 2)) {
                test_method <- 'ANOVA'
                test_result <- tryCatch(suppressWarnings(aov(fml)),
                                        error = function(e) {NULL})
            }
            if(!is.null(test_result)) {
                if('p.value' %in% names(test_result))
                    p_value <- test_result$p.value
                else
                    p_value <- summary(test_result)[[1]][["Pr(>F)"]][[1]]
                p_value <- ifelse(p_value < 0.001, '< 0.001',
                                  specify_decimal(p_value, 3))
                test_msg <- paste0(test_method, ', p = ', p_value)
                fnote_text <- ternary(is_blank(fnote_text), test_msg,
                                      c(test_msg, fnote_text))
            }
        }
    }
    plot_mar <- c(
        x_tick_lines + 0.2 + x_lab_lines + 1.3,
        1.4 + gceiling(max_na(strlines(y_tick_label)), 0.5) + y_lab_lines,
        0.7 + main_lines * main_fsize, 0.5
    )
    if(is_blank(fnote_text)) {
        par_opt <- par(mar = plot_mar, cex.lab = 1)
    }
    else {
        add_fnote <- TRUE
        fnote_lines <- length(fnote_text)
        par_opt <- par(mar = plot_mar, oma = c(fnote_lines + 0.5, 0, 0, 0),
                       cex.lab = 1)
    }
    add_points <- is_true(add_points)
    add_subjid <- is_true(add_subjid)
    boxplot(bmk ~ group, outline = !add_points, xaxt = 'n', xlab = '', ylab = '',
            xlim = xlim, ylim = ylim, lwd = 1, log = log_opt)
    if(add_points) {
        points(x_coord, bmk, col = point_colors, pch = 1, cex = 1)
        if(add_subjid && !is.null(subj_col)) {
            text(x_coord, bmk, data[[subj_col]], cex = subjid_size, pos = 1,
                 offset = .2, col = point_colors)
        }
    }
    if(!is.null(main)) mtext(main, side = 3, cex = main_fsize, line = 0.5)
    if(!is_blank(group_col)) {
        y_pos <- ifelse(
            isTRUE(to_log),
            10^(par('usr')[3] - 0.02*(par('usr')[4] - par('usr')[3])),
            par('usr')[3] - 0.02*(par('usr')[4] - par('usr')[3])
        )
        text(seq_len(n_levs), y_pos,
             labels = group_levs, srt = -45, adj = 0, xpd = TRUE)
    }
    mtext(xlab, side = 1, cex = 1, line = plot_mar[1] - 2.3)
    mtext(ylab, side = 2, cex = 1, line = plot_mar[2] - y_lab_lines - 0.2)
    mtext('N = ', side = 1, at = par('usr')[1], line = plot_mar[1] - 1.1,
          cex = 0.9)
    mtext(sizes, side = 1, at = seq_len(n_levs), line = plot_mar[1] - 1.1,
          cex = 0.9)
    if(add_fnote) {
        mtext(fnote_text, side = 1, line = 0.3 + (0:(fnote_lines - 1)),
              adj = 0, cex = fnote_size, outer = TRUE)
    }
    par(par_opt)
}



























