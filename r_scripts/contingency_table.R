#-----------------------------------------------------------------------------
# Purpose:  Function to produce contingency table in HTML
# Author:   Feiyang Niu
# Date:     May 19, 2016
#-----------------------------------------------------------------------------


# load required r packages/scripts
use_package('shiny')
install_('htmlTable')
source('r_scripts/common_statistics.R')


# define global variables
table_padding <- paste(c('padding-top: 2em', 'padding-right: 1em',
                         'padding-bottom: 2em', 'padding-left: 1em'),
                       sep = '', collapse = ';')


# function to produce contingency table in HTML
html_contingency_table <- function(data, variable_x, variable_y = NULL,
                                   name_x = variable_x, name_y = variable_y,
                                   visit_x = '', visit_y = '', percentage = NULL,
                                   caption = '', fnote_text = '',
                                   test_method = NULL) {
    cond_x <- is_blank(variable_x)
    cond_y <- is_blank(variable_y)
    fnote_text <- trimws(fnote_text)
    if(cond_x && cond_y) {
        stop('Please provide column name(s)!')
    } else if(!cond_x && !cond_y) {
        table_ <- table(data[, c(variable_x, variable_y)])
        if(!is_blank(test_method)) {
            test_method <- match.arg(
                tolower(test_method), c('chi-square', 'fisher')
            )
            if(test_method == 'chi-square') {
                test <- tryCatch(suppressWarnings(chisq.test(table_)),
                                 error = function(e) {NULL})
                test_name <- 'Chi-square test'
            } else if(test_method == 'fisher') {
                test <- tryCatch(suppressWarnings(fisher.test(table_)),
                                 error = function(e) {NULL})
                test_name <- 'Fisher exact test'
            }
            if(!is.null(test) && 'p.value' %in% names(test)) {
                p_value <- test$p.value
                test_msg <- paste0(test_name, ', p = ', specify_decimal(p_value, 3))
            }
        }
        caption <- ifelse(is_blank(caption), 'Contingency Table', caption)
        tbl_total <- addmargins(table_, c(1, 2), FUN = list(Total = sum), quiet = T)
        if(!is_blank(percentage))
            tbl_total <- table_add_percentage(tbl_total, type = tolower(percentage))
        fnotes <- paste0('&dagger; ', name_x, ' (', visit_x, ') vs ',
                         name_y, ' (', visit_y, ')')
        if(exists('test_msg')) fnotes <- c(fnotes, test_msg)
        if(!is_blank(fnote_text)) {
            fnotes <- paste(paste0(fnote_text, collapse = '\n'), fnotes, sep = '\n')
        }
        html <- invisible(
            htmlTable::htmlTable(
                tbl_total,
                rgroup = c(paste0(name_x, '&dagger;'), ''),
                n.rgroup = c(nrow(table_), 1),
                cgroup = c(paste0(name_y, '&dagger;'), ''),
                n.cgroup = c(ncol(table_), 1),
                caption = caption,
                css.cell = table_padding,
                tfoot = fnotes
            )
        )
        
        # html <- gsub('<td', '<td nowrap = "nowrap"; ', html)
        # html <- gsub("<caption style='", "<caption style='text-align: center; ", html)
        
        return(list(html = html, table = tbl_total))
    } else {
        if(!cond_x) {
            variable <- variable_x
            vname <- name_x
            visit <- visit_x
        } else {
            variable <- variable_y
            vname <- name_y
            visit <- visit_y
        }
        table_ <- table(data[, variable, drop = FALSE])
        tbl_total <- addmargins(table_, 1, FUN = list(Total = sum), quiet = T)
        tbl_total <- as.data.frame(t(as.matrix(tbl_total)),
                                   row.names = '', stringsAsFactors = F)
        if(!is_blank(percentage))
            tbl_total <- table_add_percentage(tbl_total, type = tolower(percentage))
        fnotes <- paste0('&dagger; ', vname, ' (', visit, ')')
        if(!is_blank(fnote_text)) {
            fnotes <- paste(paste0(fnote_text, collapse = '\n'), fnotes, sep = '\n')
        }
        html <- invisible(
            htmlTable::htmlTable(
                tbl_total,
                cgroup = c(paste0(vname, '&dagger;'), ''),
                n.cgroup = c(length(table_), 1),
                caption = caption,
                css.cell = table_padding,
                tfoot = fnotes
            )
        )
        
        # html <- gsub('<td', '<td nowrap = "nowrap"; ', html)
        # html <- gsub("<caption style='", "<caption style='text-align: center; ", html)
        
        return(list(html = html, table = tbl_total))
    }
}


# Add percentage to table
table_add_percentage <- function(table_, type = c('row', 'column'), digits = 1) {
    type <- match.arg(type)
    dim_table <- dim(table_)
    if(type == 'row') {
        row_total <- c(table_[, dim_table[2]])
        left <- table_[, 1:(dim_table[2] - 1), drop = F]
        percentages <- specify_decimal(sweep(
            left, MARGIN = 1, STATS = row_total, FUN = '/'
        ) * 100, digits)
        result <- cbind(matrix(paste0(left, ' (', percentages, '%)'),
                               nrow = dim_table[1]),
                        row_total)
    } else if(type == 'column') {
        col_total <- c(table_[dim_table[1], ])
        upper <- table_[1:(dim_table[1] - 1), , drop = F]
        percentages <- specify_decimal(sweep(
            upper, MARGIN = 2, STATS = col_total, FUN = '/'
        ) * 100, digits)
        result <- rbind(matrix(paste0(upper, ' (', percentages, '%)'),
                               ncol = dim_table[2]),
                        col_total)
    }
    result <- matrix(result, nrow = nrow(table_), dimnames = dimnames(table_))
    class(result) <- 'table'
    return(result)
}






























