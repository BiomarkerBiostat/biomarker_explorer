#-----------------------------------------------------------------------------
# Purpose:  Utility functions to deal with string in R
# Author:   Feiyang Niu
# Date:     April 14, 2016
#-----------------------------------------------------------------------------


# R version of `endswith`; returns TRUE if the string ends with the suffix
endswith <- function(string, suffixes) {
    pattern <- paste0(suffixes, '$', collapse = '|')
    return(grepl(pattern, string))
}


# R version of `startswith`; returns TRUE if the string starts with the prefixes
startswith <- function(string, prefixes) {
    pattern <- paste0('^', prefixes, collapse = '|')
    return(grepl(pattern, string))
}


# Return `word` if string contains a whole `word`, empty string otherwise
matched_word <- function(string, word) {
    pattern <- paste0('^.*(\\b', '[0-9]*', word, '[0-9]*', '\\b).*$')
    return(ifelse(grepl(pattern, string), word, NA))
}


# replace current extension with target
extension_replace <- function(file_name, target) {
    pos <- regexpr('\\.([[:alnum:]]+)$', file_name)
    has_ext <- pos != -1L
    extensions <- substring(file_name[has_ext], pos[has_ext] + 1)
    target_name <- mapply(gsub, pattern = extensions, x = file_name[has_ext],
                          MoreArgs = list(replacement = target))
    res <- file_name
    res[has_ext] <- target_name
    return(res)
}


# Extract time unit from a vector
time_unit <- function(vec) {
    vec <- tolower(paste(as.character(unique(vec)), collapse = ' '))
    units <- c('seconds', 'second', 'sec', 's',
               'minutes', 'minute', 'min', 'm',
               'hours', 'hour', 'hr', 'h',
               'days', 'day', 'd',
               'weeks', 'week', 'wk',
               'months', 'month', 'mo',
               'years', 'year', 'yr', 'y')
    names(units) <- c(rep('Second', 4),
                      rep('Minute', 4),
                      rep('Hour', 4),
                      rep('Day', 3),
                      rep('Week', 3),
                      rep('Month', 3),
                      rep('Year', 4))
    units_order <- c('Second', 'Minute', 'Hour', 'Day', 'Week', 'Month', 'Year')
    match_res <- sapply(units, matched_word, string = vec)
    if(sum(!is.na(match_res)) == 0) {
        return(NULL)
    }
    contain_units <- match_res[!is.na(match_res)]
    contain_units_fac <- factor(unique(names(contain_units)),
                                levels = units_order)
    return(as.character(sort(contain_units_fac)))
}


# add paranthesis to surround a string; return empty if the string is empty
add_parenthesis <- function(astring) {
    if(is_blank(astring)) return(character(0L))
    return(paste0('(', as.character(astring), ')'))
}


# obtain number of decimal places
decimalplaces <- function(x) {
    if((x %% 1) != 0) {
        parts <- strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)
        return(nchar(parts[[1]][[2]]))
    } else {
        return(0)
    }
}


# prefix some string to a string
str_prefix <- function(string, prefix, sep = ' ', collapse = NULL) {
    paste(prefix, string, sep = sep, collapse = collapse)
}


# a function that computes the width of the given strings in lines
strlines <- function(a_string) {
    strwidth(a_string, units = 'inches') / 0.2
}


# test if a string is empty
is_empty_string <- function(string = NULL, remove_ws = TRUE) {
    res <- missing(string) || is.null(string) || length(string) == 0 || is.na(string)
    if(remove_ws) string <- trimws(string)
    res <- res || nchar(string) == 0
    return(res)
}


# smart paste
smart_paste <- function(..., sep = ' ') {
    all_inputs <- list(...)
    filtered_inputs <- all_inputs[!sapply(all_inputs, is_empty_string)]
    return(paste(filtered_inputs, sep = sep, collapse = ''))
}


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


# smart decimal places
smart_decimal_places <- function(vec) {
    vec <- abs(na.omit(vec))
    max_decimals <- max(sapply(vec, decimalplaces))
    if(max_decimals <= 1) return(max_decimals)
    min_vec <- min(vec)
    if(min_vec >= 10) return(min(1, max_decimals))
    else if(min_vec >= 0.99) return(min(2, max_decimals))
    else {
        decimals <- min(max(0, leading_decimal_zeros(min_vec)) + 2,
                        decimalplaces(min_vec))
        return(decimals)
    }
}


# Count leading zeros between the decimal point and first nonzero digit
leading_decimal_zeros <- function(a_num) {
    num_zeros <- attr(regexpr('(?<=\\.)0+', a_num, perl = TRUE), 'match.length')
    return(num_zeros)
}


# smart print
smart_print <- function(obj) {
    obj_char <- sapply(obj, as.character)
    obj_print <- sapply(obj_char, smart_round)
    return(obj_print)
}


# returns the file paths without extensions
file_path_sans_ext <- function (x, compression = FALSE) {
    if (compression) 
        x <- sub("[.](gz|bz2|xz)$", "", x)
    sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}


# evaluate string as expression
eval_string <- function(string, envir = parent.frame()) {
    if(!is.environment(envir))
        envir <- list2env(envir, parent = parent.frame())
    return(eval(parse(text = string), envir = envir))
}



# r equivalent of `string.format` in Python
install_('stringi')
r_format <- function(string, dictionary, .open = '{', .close = '}') {
    if(is.null(names(dictionary))) stop('`dictionary` must be named')
    pattern <- paste0('\\', .open, '.*?\\', .close)
    matchings <- unlist(regmatches(string, gregexpr(pattern, string, perl = T)))
    origins <- matchings
    replacements <- c()
    if(length(matchings) > 0) {
        for(matching in matchings) {
            content <- substring(matching, 2, nchar(matching) - 1)
            if(nchar(content) > 0) {
                to <- tryCatch(
                    as.character(eval_string(content, dictionary)),
                    error = function(e) {numeric(0)}
                )
                if(!identical(to, numeric(0))) {
                    replacements <- c(replacements, to)
                } else origins <- setdiff(origins, matching)
            } else origins <- setdiff(origins, matching)
        }
        if(length(origins) > 0)
            string <- stringi::stri_replace_all_fixed(string,
                                                      origins,
                                                      replacements,
                                                      vectorize_all = FALSE)
    }
    return(string)
}


# evaluate expressions enclosed by braces in specified columns for one row
# of a data frame
eval_df_row <- function(df, row_idx = NULL, .open = '{', .close = '}') {
    if(!is.data.frame(df) || nrow(df) == 0) stop('`df` must be valid dataframe')
    if(nrow(df) > 1 && is.null(row_idx)) {
        row_idx <- 1
        df <- df[row_idx, , drop = FALSE]
    }
    pattern <- paste0('\\', .open, '.*?\\', .close)
    columns_to_eval <- base::grep(pattern, unlist(df))
    if(length(columns_to_eval) > 0) {
        for(column_to_eval in columns_to_eval) {
            df[, column_to_eval] <- r_format(unlist(df[, column_to_eval]),
                                             df[, -columns_to_eval],
                                             .open, .close)
        }
    }
    return(df)
}

































