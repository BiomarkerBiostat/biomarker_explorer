#-----------------------------------------------------------------------------
# Purpose:  Function to draw histogram with density curve
# Author:   Feiyang Niu
# Date:     May 12, 2016
#-----------------------------------------------------------------------------


# load necessary scripts
source('r_scripts/strings.R')
source('r_scripts/common_statistics.R')


# plot histogram with density curve
histogram_plot <- function(data, var_col, var_name = var_col, var_unit = '',
                           to_log = TRUE, visit = 'Baseline',
                           bar_col = 'grey', curve_col = 'blue', xlab = '',
                           main = '', xlim = NULL, add_density_curve = TRUE,
                           fnote_size = 0.9, fnote_text = '') {
    data <- data[!is.na(data[[var_col]]), ]
    if(nrow(data) == 0)
        stop('There are 0 rows to plot!')
    var_y <- data[[var_col]]
    if(to_log && any(var_y <= 0))
        stop('Biomarker contains values <= 0! No log transformation possible!')
    num_obs <- length(var_y)
    if(is_blank(main)) {
        main <- ifelse(length(visit) > 1 || visit == '',
                       '', paste(visit, var_name))
    }
    if(is_blank(xlab)) xlab <- paste(var_name, add_parenthesis(var_unit))
    axes_opt <- ifelse(to_log, FALSE, TRUE)
    if(to_log) {
        var_y <- log10(var_y)
        legd_txt <- paste(c('N', 'Mean','SD','Median','Range'),
                          c(length(var_y),
                            smart_round(10^(mean(var_y))),
                            smart_round(10^(sd(var_y))),
                            smart_round(10^(median(var_y))),
                            paste0('(', smart_round(10^(min(var_y))), " - ",
                                   smart_round(10^(max(var_y))), ')')),
                          sep = ': ')
    } else {
        sd_y <- sd(var_y)
        summary_y <- summary(var_y)
        legd_txt <- paste(c('N', 'Mean','SD','Median','Range'),
                          c(length(var_y),
                            smart_round(summary_y[['Mean']]),
                            smart_round(sd_y),
                            smart_round(summary_y[['Median']]),
                            paste0('(', smart_round(summary_y[['Min.']]), " - ",
                                   smart_round(summary_y[['Max.']]), ')')),
                          sep = ': ')
    }
    hist_y <- hist(var_y, plot = FALSE)
    multiplier <- hist_y$counts / hist_y$density
    density_y <- density(var_y, na.rm = TRUE)
    density_y$y <- density_y$y * multiplier[1]
    xlim <- ternary(is.null(xlim), range_na(hist_y$breaks),
                    intersection(xlim, range_na(hist_y$breaks)))
    ylim <- c(0, max(c(density_y$y, hist_y$counts)))
    fnote_text <- stringr::str_trim(fnote_text)
    add_fnote <- FALSE
    if(!is_blank(fnote_text)) {
        add_fnote <- TRUE
        fnote_lines <- length(fnote_text)
        fnote_text <- paste0('FN ', seq_len(fnote_lines), ': ', fnote_text)
        par_opt <- par(oma = c(fnote_lines + 0.3, 0.5, 0, 0))
    }
    hist(var_y, col = bar_col, xlab = xlab, xlim = xlim, ylim = ylim,
         main = main, axes = axes_opt)
    lines(density_y, col = curve_col, lwd = 2)
    if(to_log) {
        brks <- hist_y$breaks
        axis(side = 1, at = brks, labels = round(10^(brks), 1), lty = NULL)
        axis(2)
    }
    legend('topright',legd_txt, bty = 'n', text.font = 3)
    box()
    if(add_fnote) {
        mtext(fnote_text, side = 1, line = 0:(fnote_lines - 1), adj = 0,
              cex = fnote_size, outer = TRUE)
        par <- par(par_opt)
    }
}



























