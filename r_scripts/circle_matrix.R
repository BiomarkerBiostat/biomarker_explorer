#-----------------------------------------------------------------------------
# Purpose:  Function to draw bubble matrix plot
# Author:   Feiyang Niu
# Date:     August 9, 2016
#-----------------------------------------------------------------------------


# load packaegs and necessary R script files
install_('corrplot')


# corrplot_mixed for combining number and circle
corrplot_mixed <- partial(corrplot::corrplot.mixed, order = "hclust",
                          hclust.method = "ward")















