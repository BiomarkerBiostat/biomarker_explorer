#-----------------------------------------------------------------------------
# Purpose:  Function to draw Spaghetti plot
# Author:   Feiyang Niu
# Date:     April 7, 2016
#-----------------------------------------------------------------------------


# Load libraries
use_package('dplyr')
install_('lazyeval')
install_('stringr')
source('r_scripts/common_statistics.R')


# Draw Spaghetti plot
spaghetti_plot <- function(data, subj_col, bmk_col, visit_col, group_col = '',
                           group_levs = NULL, to_log = FALSE, bmk_name = '',
                           xlab = '', ylab = '', main = 'Spaghetti plot',
                           xlim = NULL, ylim = NULL, reference_line = NULL,
                           x_tick = visit_col, highlight_data = NULL,
                           highlight_col = NULL, fnote_size = 0.8,
                           fnote_text = '', add_legend = FALSE,
                           add_subjid = FALSE, add_points = FALSE,
                           line_cols = NULL, line_types = NULL,
                           include_n = TRUE, save_plot = FALSE,
                           output_plot = FALSE) {
    
    set.seed(0)
    
    #------------------------------
    # argument match
    #------------------------------
    to_log <- isTRUE(to_log)
    add_legend <- isTRUE(add_legend)
    add_subjid <- isTRUE(add_subjid)
    add_points <- isTRUE(add_points)
    include_n <- isTRUE(include_n)
    save_plot <- isTRUE(save_plot)
    xlab <- trimws(xlab)
    ylab <- trimws(ylab)
    main <- trimws(main)
    fnote_text <- trimws(fnote_text)
    
    #------------------------------
    # data manipulation
    #------------------------------
    data <- filter(data, !is.na(data[[bmk_col]]), !is.na(data[[visit_col]]))
    if(is_blank(group_col)) {
        data$group_ <- factor(as.character(1))
        if(is_blank(line_cols)) line_cols <- 1
        if(is_blank(line_types)) line_types <- 1
    } else {
        data <- data %>% 
            filter(!is.na(data[[group_col]])) %>%
            rename_(group_ = as.name(group_col)) %>%
            mutate(group_ = factor(group_))
        if(!is_blank(group_levs))
            data$group_ <- factor(data$group_, levels = group_levs)
        if(is_blank(line_cols)) line_cols <- seq_len(nlevels(data$group_))
        if(is_blank(line_types)) line_types <- seq_len(nlevels(data$group_))
    }
    if(!is.null(xlim))
        data <- filter(data, between(data[[visit_col]], xlim[1], xlim[2]))
    if(nrow(data) == 0) stop('There are 0 rows to plot!')
    if(length(levels(data$group_)) > time_ngroups_threshold) return()
    expr <- list(lazyeval::interp(~mean(var), var = as.name(visit_col)),
                 lazyeval::interp(~n_nna(var), var = as.name(bmk_col)))
    x_tick <- ifelse(x_tick == visit_col, '', x_tick)
    if(!is_blank(x_tick)) {
        expr <- append_alist(
            lazyeval::interp(~unique(na.omit(var)), var = as.name(x_tick)), expr
        )
        dots <- setNames(expr, c('visit', 'N', 'x_tick'))
    } else dots <- setNames(expr, c('visit', 'N'))
    sample_size <- data %>%
        arrange_('group_', visit_col) %>%
        group_by_('group_', visit_col) %>%
        summarise_(.dots = dots)
    groups <- levels(data$group_)
    ngroups <- length(groups)
    
    time_visit <- sample_size
    if(ngroups > 1) {
        if(is_blank(x_tick)) {
            time_visit <- data.frame(visit = sort(unique_na(sample_size$visit)))
        } else {
            time_visit <- sample_size %>% select(visit, x_tick) %>%
                group_by(visit) %>% summarise(x_tick = unique_na(x_tick)[1])
        }
    }
    
    #------------------------------
    # plotting range
    #------------------------------
    xlim <- range_na(data[[visit_col]])
    if(add_subjid) {
        xlim[2] <- diff(xlim)*(1+0.015*max(strlines(data[[subj_col]]))) + xlim[1]
    }
    ylim <- ternary(is.null(ylim), range_na(data[[bmk_col]]),
                    intersection(ylim, range_na(data[[bmk_col]])))
    
    #------------------------------
    # defaults and constants
    #------------------------------
    x_tick_len_ratio <- 1
    if(!is_blank(x_tick)) {
        x_tick_angle <- -45
        x_tick_len_ratio <- abs(sin(x_tick_angle * pi / 180))
        if(output_plot) x_tick_len_ratio <- 1.2
    }
    ss_font <- ifelse(include_n, 0.9, 0)
    ss_space <- ifelse(is_blank(x_tick), 0.5, 0.8)
    fnote_space <- 0.8
    legend_row_lines <- 25
    legend_nrow <- ceiling(max(strlines(groups)) * ngroups / legend_row_lines)
    legend_ncol <- ceiling(ngroups / legend_nrow)
    legend_space <- ifelse(add_legend, 1.5 * legend_nrow, 0)
    main_fsize <- 1
    line_width <- 1.5
    point_shape <- 19
    log_opt <- ifelse(to_log, 'y', '')
    add_fnote <- FALSE
    
    #------------------------------
    # calculate plot margins
    #------------------------------
    x_tick_label <- ternary(is_blank(x_tick), time_visit$visit, time_visit$x_tick)
    y_tick_label <- axis_ticks(ylim, to_log = to_log)
    y_tick_label <- sprintf(
        paste0('%.', max(sapply(y_tick_label, decimalplaces)), 'f'), y_tick_label
    )
    x_lab_lines <- if(is_blank(xlab)) 0 else length(strsplit(xlab, '\n')[[1]])
    y_lab_lines <- if(is_blank(ylab)) 0 else length(strsplit(ylab, '\n')[[1]])
    main_lines <- if(is_blank(main)) 0 else length(strsplit(main, '\n')[[1]])
    plot_mar <- c(
        ifelse(is_blank(x_tick), 1, 0.2) +
            max_na(strlines(x_tick_label)) * x_tick_len_ratio + ss_space +
            x_lab_lines + 0.2 + ss_font * ngroups + 0.2 + legend_space,
        1 + max_na(strlines(y_tick_label)) + 0.5 + y_lab_lines + 0.2,
        0.5 + main_lines * main_fsize + 0.2,
        1.5
    )
    if(is_blank(fnote_text)) par_opt <- par(mar = plot_mar)
    else {
        add_fnote <- TRUE
        fnote_lines <- ifelse(is_blank(fnote_text), 0,
                              length(strsplit(fnote_text, '\n')[[1]]))
        par_opt <- par(
            mar = plot_mar,
            oma = c(fnote_space + fnote_lines * fnote_size + 0.2, 0, 0, 0)
        )
    }
    
    #------------------------------
    # plot Mean + SE plot
    #------------------------------
    if(save_plot) {
        win.metafile()
        dev.control('enable')
    }
    plot(NA, NA, xlim = xlim, ylim = ylim, log = log_opt,
         xlab = '', ylab = '', main = '', axes = FALSE, type = 'n')
    plot_range <- par('usr')
    highlight_subj <- NULL
    if(!is.null(highlight_data) && !is.null(dim(highlight_data)) &&
       nrow(highlight_data) >= 1) {
        highlight_subj <- unique(highlight_data[[subj_col]])
    }
    for(idx in seq_along(groups)) {
        sub_data <- filter(data, group_ == groups[idx])
        if(nrow(sub_data) == 0) next
        subj <- as.character(sub_data[[subj_col]])
        visit <- sub_data[[visit_col]]
        bmk <- sub_data[[bmk_col]]
        ids <- unique(subj)
        ssize <- filter(sample_size, group_ == groups[idx])
        for(iter in seq_along(ids)) {
            pos <- which(subj == ids[iter])
            wk_order <- order(visit[pos])
            is_highlight <- ids[iter] %in% highlight_subj
            lwd <- ifelse(is_highlight, 5, 1)
            col <- ifelse(is_highlight && !is.null(highlight_col),
                          highlight_col, line_cols[idx])
            lines(visit[pos][wk_order], bmk[pos][wk_order], col = col,
                  lwd = lwd, lty = line_types[idx])
            if(add_points) {
                points(visit[pos][wk_order], bmk[pos][wk_order],
                       col = line_cols[idx], pch = 20)
            }
            if(add_subjid) {
                cex_text <- 0.7
                text(last(visit[pos][wk_order]), last(bmk[pos][wk_order]),
                     ids[iter], cex = cex_text, col = line_cols[idx],
                     pos = 4, offset = 0.1)
            }
            # if(add_subjid) {
            #     cex_text <- ifelse(is_highlight, 1.2, 0.7)
            #     text(last(visit[pos][wk_order]), last(bmk[pos][wk_order]),
            #          ids[iter], cex = cex_text, col = idx, pos = 4, offset = 0.1)
            # } else if(is_highlight) {
            #     text(last(visit[pos][wk_order]), last(bmk[pos][wk_order]),
            #          ids[iter], cex = 1.2, col = idx, pos = 4, offset = 0.1)
            # }
        }
        if(include_n) {
            mtext(
                ssize$N, side = 1, at = ssize$visit, font = 3,
                col = line_cols[idx], cex = ss_font,
                line = plot_mar[1] - (ss_font * ngroups + 0.2 + legend_space) +
                    (idx - 1) * ss_font
            )
        }
    }
    if(is_blank(x_tick)) axis(1)
    else {
        text_y <- plot_range[3] - 0.05*(plot_range[4]-plot_range[3])
        if(to_log) text_y <- 10^text_y
        text(time_visit$visit, text_y, labels = x_tick_label,
             srt = x_tick_angle, adj = 0, xpd = TRUE)
    }
    axis(2, las = 2)
    if(include_n) {
        mtext('N = ', side = 1, at = plot_range[1],
              line = plot_mar[1] - (ss_font * ngroups + 0.2 + legend_space))
    }
    if(!is_blank(xlab)) {
        mtext(
            xlab, side = 1, cex = 1,
            line = plot_mar[1] - (ss_font * ngroups + 0.2 + legend_space) -0.2-1
        )
    }
    if(!is_blank(ylab))
        mtext(ylab, side = 2, cex = 1, line = plot_mar[2] - y_lab_lines - 0.2)
    if(!is_blank(main))
        mtext(main, side = 3, cex = main_fsize, line = 0.5)
    box()
    if(!is_blank(reference_line) && is.numeric(reference_line)) {
        for(line_ in reference_line)
            abline(h = line_, col = 'black', lty = 2)
    }
    if(add_fnote) {
        mtext(fnote_text, side = 1, adj = 0, cex = fnote_size, outer = TRUE,
              line = fnote_space + (fnote_lines - 1) * fnote_size)
    }
    if(add_legend) {
        legend(grconvertX(0.52, 'nfc'), grconvertY(0, 'nfc'),
               xjust = 0.5, yjust = 0.2, groups, xpd = TRUE, bty = 'n',
               col = line_cols, lty = line_types, lwd = 2, pch = 19,
               ncol = legend_ncol)
    }
    if(save_plot) {
        the_plot <- recordPlot()
        dev.off()
        return(the_plot)
    }
    par(par_opt)
}
