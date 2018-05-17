#-----------------------------------------------------------------------------
# Purpose:  Utility functions to deal with format in R
# Author:   Feiyang Niu
# Date:     April 28, 2016
#-----------------------------------------------------------------------------


# smart round
smart_round <- function(a_scalar) {
    if(!(is.character(a_scalar) | is.numeric(a_scalar) | is.complex(a_scalar)))
        return(a_scalar)
    else if(suppressWarnings(is.na(as.complex(a_scalar))))
        return(a_scalar)
    else {
        if(is.character(a_scalar)) {
            if(suppressWarnings(is.na(as.numeric(a_scalar))))
                a_scalar <- as.complex(a_scalar)
            else
                a_scalar <- as.numeric(a_scalar)
        }
        if(is.integer(a_scalar))
            return(a_scalar)
        else if(is.complex(a_scalar))
            return(smart_round(Re(a_scalar)) + 1i * smart_round(Im(a_scalar)))
        else if(abs(a_scalar) >= 10)
            return(round(a_scalar, 1))
        else if(abs(a_scalar) >= 1)
            return(round(a_scalar, 2))
        else
            return(signif(a_scalar, 2))
    }
}


# smart print
smart_print <- function(obj) {
    obj_char <- sapply(obj, as.character)
    obj_print <- sapply(obj_char, smart_round)
    return(obj_print)
}






































