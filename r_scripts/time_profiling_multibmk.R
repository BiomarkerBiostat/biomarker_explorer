#-----------------------------------------------------------------------------
# Purpose:  Function to put time-profiling plots for multiple biomarkers in
#           one graph window
# Author:   Feiyang Niu
# Date:     September 12, 2016
#-----------------------------------------------------------------------------


time_profiling_multibmk <- function(data_list, type, ...) {
    type <- match.arg(type, c('Spaghetti plot', 'Mean + SE plot',
                              'Mean + SD plot', 'Boxplot', 'Median + IQR plot'))
    num_plots <- length(data_list)
    if(num_plots > 1) {
        on.exit(par(par('usr')))
        par(mfrow = c(ceiling(num_plots / 2), 2))
    }
    for(i in 1:num_plots) {
        data_iter <- data_list[[i]]
        if(type == 'Spaghetti plot') {
            spaghetti_plot(data_iter, ...)
        } else if(type %in% c('Mean + SE plot', 'Mean + SD plot')) {
            mean_se_plot(data_iter, ...)
        } else if(type == 'Boxplot') {
            boxplot_time(data_iter, ...)
        } else if(type == 'Median + IQR plot') {
            median_iqr_plot(data_iter, ...)
        }
    }
}


























