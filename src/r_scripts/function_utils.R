#-----------------------------------------------------------------------------
# Purpose:  Utility functions used in writing functions
# Author:   Feiyang Niu
# Date:     March 20, 2017
#-----------------------------------------------------------------------------


# test whether an object contains nothing
is_blank <- function(obj, empty_str_triggers = TRUE, false_triggers = FALSE) {
    if(is.function(obj)) return(FALSE)
    return(
        is.null(obj) ||
            length(obj) == 0 ||
            all(is.na(obj)) ||
            (empty_str_triggers && all(obj == '')) ||
            (false_triggers && all(!obj))
    )
}


# check if the dataframe contains the column
column_in_dataframe <- function(column, df, error_message = NULL, call_ = F) {
    if(!all(column %in% names(df))) {
        df_name <- deparse(substitute(df))
        column_name <- deparse(substitute(column))
        if(is.null(error_message)) {
            error_message <- paste0(
                df_name, " doesn't have a column named '", column_name, "'"
            )
        }
        stop(error_message, call. = call_)
    }
}


# check if arg is in a table of candidate values
arg_in_choices <- function(arg, choices, error_message = NULL, call_ = F) {
    if(!all(arg %in% choices)) {
        arg_name <- deparse(substitute(arg))
        if(is.null(error_message)) {
            error_message <- paste0(
                arg_name, ' must be one of the following:\n',
                paste(choices, collapse = '\n')
            )
        }
        stop(error_message, call. = call_)
    }
}


# check if var is of specified class
check_var_class <- function(var, class_method,
                            class_name, error_message = NULL) {
    class_method_name <- deparse(substitute(class_method))
    if(!is.function(class_method)) {
        stop(paste0(class_method_name, ' must be a callable function'))
    }
    if(!class_method(var)) {
        var_name <- deparse(substitute(var))
        if(is.null(error_message)) {
            error_message <- paste0(
                var_name, " must be of class '", class_name, "'"
            )
        }
        stop(error_message, call. = call_)
    }
}






























