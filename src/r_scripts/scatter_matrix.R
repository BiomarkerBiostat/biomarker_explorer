#-----------------------------------------------------------------------------
# Purpose:  Function to draw scatter matrix plot
# Author:   Feiyang Niu
# Date:     August 8, 2016
#-----------------------------------------------------------------------------


# load packaegs and necessary R script files
source('r_scripts/plot_utils.R')


scatter_matrix <- function(data_, labels_ = NULL, cex.labels = 1.5,
                           xlim = NULL, ylim = NULL, cor.method = 'spearman',
                           main = NULL) {
    if(is.null(dim(data_)) || ncol(data_) <= 1) return()
    if(is.null(labels_)) {
        sample_sizes <- colSums(!is.na(data_))
        labels_ <- paste0(colnames(data_), '\n', '(N=', sample_sizes, ')')
    }
    if(is.null(ylim)) ylim <- range_na(data_)
    if(is.null(xlim)) xlim <- ylim
    pairs(data_, labels = labels_, cex.labels = cex.labels,
          panel = panel.points.abline(pch = 3, abline = TRUE, a = 0, b = 1,
                                      col = 'blue', lwd = 2, cex = 0.6,
                                      log="xy", xlim = xlim, ylim = ylim),
          diag.panel = panel.density(rug = TRUE, col = 3, log = "x"),
          lower.panel=panel.cor(cor.method = cor.method, cex.cor = 2.5), 
          main = main, xlim = xlim, ylim = ylim)
}
