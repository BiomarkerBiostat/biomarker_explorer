#-----------------------------------------------------------------------------
# Purpose:  Function to automatically install and load packages if not yet
#           installed or loaded
# Author:   Feiyang Niu
# Date:     April 7, 2016
#-----------------------------------------------------------------------------


# Automatically install if not installed
install_ <- function(package) {
    package <- as.character(substitute(package))
    if (!is.element(package, installed.packages()[,1]))
        install.packages(package, dep = TRUE)
}


# Automatically install and load packages if not installed or loaded
use_package <- function(package) {
    package <- as.character(substitute(package))
    if (!is.element(package, installed.packages()[,1]))
        install.packages(package, dep = TRUE)
    suppressMessages(library(package, character.only = TRUE))
}
