#-----------------------------------------------------------------------------
# Purpose:  Define utility functions for dealing with tables in Shiny
# Author:   Feiyang Niu
# Date:     July 26, 2016
#-----------------------------------------------------------------------------


# load packaegs and necessary R script files
source('r_scripts/common_statistics.R')


# define summary table
summary_table <- function(data_, rowlabel = '',
                          caption = '', footnote = '',
                          func_list = c('Min' = min, 'Median' = median,
                                        'Mean' = mean, 'Max' = max),
                          func_names = NULL) {
    if(is.null(func_names)) func_names <- names(func_list)
    summary_table <- fapply(func_list, data_, transpose = TRUE)
    html <- invisible(
        htmlTable::htmlTable(
            summary_table, rowlabel = rowlabel,
            caption = caption, tfoot = footnote,
            css.cell = paste(c('padding-top: 0.5em', 'padding-right: 2em',
                               'padding-bottom: 0.5em', 'padding-left: 2em'),
                             sep = '', collapse = ';')
        )
    )
    return(html)
}


# define one-dim summary table by row
summary_table_row <- function(data_, row_var, row_names = NULL,
                             rowlabel = '', caption = '', footnote = '',
                             func_list = c('Min' = min, 'Median' = median,
                                           'Mean' = mean, 'Max' = max),
                             func_names = NULL) {
    all_columns <- names(data_)
    stopifnot(row_var %in% all_columns)
    split_result <- split_2d(data_, row_var)
    if(is.null(row_names)) row_names <- names(split_result)
    if(is.null(func_names)) func_names <- names(func_list)
    summary_table <- matrix(fapply(
        func_list, split_result, transpose = TRUE
    ), ncol = 1)
    n_rgroup <- rep(length(func_list), length(row_names))
    rnames <- rep(func_names, length(row_names))
    html <- invisible(
        htmlTable::htmlTable(
            summary_table, header = 'Value', rnames = rnames,
            rgroup = row_names, n.rgroup = n_rgroup, rowlabel = rowlabel,
            caption = caption, tfoot = footnote,
            css.cell = paste(c('padding-top: 0.5em', 'padding-right: 2em',
                               'padding-bottom: 0.5em', 'padding-left: 2em'),
                             sep = '', collapse = ';')
        )
    )
    return(html)
}


# define one-dim summary table by column
summary_table_col <- function(data_, col_var, col_names = NULL,
                              rowlabel = '',
                              caption = '', footnote = '',
                              func_list = c('Min' = min, 'Median' = median,
                                            'Mean' = mean, 'Max' = max),
                              func_names = NULL) {
    all_columns <- names(data_)
    #stopifnot(col_var %in% all_columns)
    split_result <- split_2d(data_, col_var)
    if(is.null(col_names)) col_names <- names(split_result)
    if(is.null(func_names)) func_names <- names(func_list)
    summary_table <- fapply(func_list, split_result, transpose = TRUE)
    html <- invisible(
        htmlTable::htmlTable(
            summary_table, header = col_names, rowlabel = rowlabel,
            cgroup = col_var, n.cgroup = length(col_names),
            caption = caption, tfoot = footnote,
            css.cell = paste(c('padding-top: 0.5em', 'padding-right: 2em',
                               'padding-bottom: 0.5em', 'padding-left: 2em'),
                             sep = '', collapse = ';')
        )
    )
    return(html)
}


# define two-dim summary table
summary_table_2d <- function(data_, row_var, col_var,
                             row_names = NULL, col_names = NULL,
                             rowlabel = '', caption = '', footnote = '',
                             func_list = c('Min' = min, 'Median' = median,
                                           'Mean' = mean, 'Max' = max),
                             func_names = NULL) {
    all_columns <- names(data_)
    stopifnot(all(c(row_var, col_var) %in% all_columns))
    split_result <- split_2d(data_, row_var, col_var, include_total = TRUE)
    if(is.null(row_names)) row_names <- rownames(split_result)
    if(is.null(col_names)) col_names <- colnames(split_result)
    if(is.null(func_names)) func_names <- names(func_list)
    if(is.null(dim(split_result))) {
        split_result <- array(
            split_result,
            dim = c(length(unique(data_[[row_var]])),
                    length(unique(data_[[col_var]]))),
            dimnames = list(row_names, col_names)
        )
    }
    summary_table <- apply(
        split_result, 2, fapply, func_list = func_list, transpose = TRUE
    )
    n_rgroup <- rep(length(func_list), length(row_names))
    rnames <- rep(func_names, length(row_names))
    html <- invisible(
        htmlTable::htmlTable(
            summary_table, header = col_names, rnames = rnames,
            rgroup = row_names, n.rgroup = n_rgroup, rowlabel = rowlabel,
            cgroup = col_var, n.cgroup = length(col_names),
            caption = caption, tfoot = footnote,
            css.cell = paste(c('padding-top: 0.5em', 'padding-right: 2em',
                               'padding-bottom: 0.5em', 'padding-left: 2em'),
                             sep = '', collapse = ';')
        )
    )
    return(html)
}


# function that calculate summary matrix
summary_matrix <- function(data_, row_var, col_var = NULL, val_var = NULL,
                           col_totals = NULL, name_totals = NULL,
                           func_list = c('Min' = min, 'Median' = median,
                                         'Mean' = mean, 'Max' = max)
                           
                           , pval
                           , pval_loc
                           ) {
    all_columns <- names(data_)
    #stopifnot(row_var %in% all_columns )
    if(!is.factor(data_[[row_var]]))
        data_[[row_var]] <- factor(data_[[row_var]])
    vars <- row_var
    if(!is_blank(col_var)) {
        vars <- c(vars, col_var)
        stopifnot(col_var %in% all_columns)
        if(!is.factor(data_[[col_var]]))
            data_[[col_var]] <- factor(data_[[col_var]])
    }
    if(!is_blank(val_var)) {
        vars <- c(vars, val_var)
        stopifnot(val_var %in% all_columns)
        data_ <- data_[, vars, drop = FALSE]
    }

    if(is_blank(col_var)) {
        if(is_blank(val_var)) val_var <- base::setdiff(all_columns, row_var)
        data_split <- split(data_[[val_var]], data_[[row_var]])
        summary_table <- matrix(fapply(
            func_list, data_split, transpose = TRUE
        ), ncol = 1)
        
        ###### ADDED by WANGSHU
        if(!is_blank(pval)) {
          digits = 3
          summary_table <- rbind(summary_table, round(pval, digits))
        }
        ###### END ADDED by WANGSHU
        
    } else {
        if(is_blank(val_var))
            val_var <- base::setdiff(all_columns, c(row_var, col_var))
        temp <- split(data_[, c(val_var, row_var)], data_[[col_var]])
        data_list <- lapply(lapply(temp, `[`, val_var), as.matrix)
        factor_list <- lapply(temp, `[`, row_var)
        data_split <- mapply(split, data_list, factor_list)
        if(!is_blank(col_totals)) {
            if(is_blank(name_totals))
                name_totals <- paste('Total', seq_along(col_totals))
            start_idx <- 1
            data_split_all <- NULL
            for(idx in seq_along(col_totals)) {
                col_total <- col_totals[idx]
                to_append_idx <- start_idx:col_total
                start_idx <- col_total + 1
                col_to_add <- array(
                    lapply(apply(data_split[, 1:col_total, drop = F],
                                 1, cbind), unlist),
                    dim = c(nrow(data_split), 1),
                    dimnames = list(rownames(data_split), name_totals[idx])
                )
                data_split_all <- cbind(data_split_all,
                                        data_split[, to_append_idx, drop = F],
                                        col_to_add)
            }
            if(max(col_totals) < ncol(data_split)) {
                data_split_all <- cbind(
                    data_split_all,
                    data_split[, start_idx:ncol(data_split), drop = F]
                )
            }
            data_split <- data_split_all
        }
        if(is.null(dim(data_split))) {
            data_split <- array(
                data_split,
                dim = c(length(unique(data_[[row_var]])),
                        length(unique(data_[[col_var]])))
            )
        }
        summary_table <- apply(
            data_split, 2, fapply, func_list = func_list, transpose = TRUE
        )
        
        ### ADDED by WANGSHU
        if(!is_blank(pval)) {
          digits = 3
          pval = round(pval, digits)
          n_funcs <- length(func_list)
          
          if(pval_loc == "EACH") {
            temp <- matrix(0, (n_funcs + 1) * length(unique(data_[[row_var]])), length(unique(data_[[col_var]])))
            i = 1
            j = 1
            while (i <= (n_funcs + 1) * length(unique(data_[[row_var]]))) {
              temp[i:(i + n_funcs - 1), ] <- summary_table[j:(j + n_funcs - 1), ]
              temp[i + n_funcs, ] <- c(pval[as.integer(j/n_funcs + 1)], rep(NA, length(unique(data_[[col_var]])) - 1))
              i = i + n_funcs + 1
              j = j + n_funcs
            }
            summary_table <- temp
          } else {
            summary_table <- rbind(summary_table, pval)
          }
        }
        ### END ADDED by WANGSHU
    }

    return(summary_table)
}


# function that outputs rtf, csv or html summary table
summary_table_all <- function(data_, row_var, row_names = '',
                              col_var = NULL, col_names = '', val_var = NULL,
                              col_totals = NULL, name_totals = NULL,
                              n_in_header = TRUE, subj_col = NULL,
                              baseline_name = NULL,
                              add_cfb = FALSE, cfb_var = NULL,
                              func_list = c('Min' = min, 'Median' = median,
                                            'Mean' = mean, 'Max' = max),
                              func_names = names(func_list),
                              
                              pval,
                              pval_loc,
                              
                              caption = '', footnote = '', mdgrp = '',
                              rowlabel = '', visit_header_space = 4,name_N='Y',
                              format = 'html') {
    n_in_header <- isTRUE(n_in_header)
    add_cfb <- isTRUE(add_cfb)
    format <- match.arg(format, choices = c('rtf', 'html', 'csv'))
    if(is.null(func_names)) stop('Please provide func_names')

    all_columns <- names(data_)
    #stopifnot(row_var %in% all_columns)
    if(!is.factor(data_[[row_var]]))
        data_[[row_var]] <- factor(data_[[row_var]])
    row_nlevels <- nlevels(data_[[row_var]])
    row_levels <- levels(data_[[row_var]])
    if(is_blank(row_names)) row_names <- row_levels
    if(is_blank(baseline_name)) baseline_name <- row_levels[1]
    n_funcs <- length(func_list)
    if(!is_blank(col_var)) {
        stopifnot(col_var %in% all_columns)
        if(!is.factor(data_[[col_var]]))
            data_[[col_var]] <- factor(data_[[col_var]])
        if(is_blank(col_names)) col_names <- levels(data_[[col_var]])
        
        col_nlevels <- nlevels(data_[[col_var]])
        if(!is_blank(col_totals)) {
            if(is_blank(name_totals))
                name_totals <- paste('Total', seq_along(col_totals))
            col_names <- vector_insert(col_names, name_totals, after = col_totals)
        }
        if(n_in_header) {
            if(is_blank(subj_col))
                stop('Please provide subj_col')
            subj_split <- split(data_[[subj_col]], data_[[col_var]])
            subj_split <- lapply(subj_split, unique)
            nsubj <- unlist(lapply(subj_split, length))
            if(!is_blank(col_totals)) {
                n_totals <- unlist(lapply(
                    col_totals,
                    function(n) {length(unique(unlist(subj_split[seq_len(n)])))}
                ))
                nsubj <- vector_insert(nsubj, n_totals, after = col_totals)
            }
            ## Summary table Column name
            if(name_N == 'N'){
              col_names <- paste0(col_names, '~')
            }else{
              col_names <- paste0(col_names, '~', '(N=', nsubj, ')')
            
            }
            

        }
    }

    summary_tbl <- summary_matrix(
        data_, row_var, col_var = col_var, val_var = val_var,
        col_totals = col_totals, name_totals = name_totals,
        func_list = func_list
        
        ############# Added by WANGSHU: add pvalues to the last line of table
        , pval
        , pval_loc
        ############# END added by WANGSHU
    )
    if(!is_blank(col_var)) colnames(summary_tbl) <- col_names
    
    # add change from baseline block
    if(add_cfb && row_nlevels > 1) {
        if(is_blank(cfb_var) || !cfb_var %in% all_columns)
            stop('Please provide a valid column name for cfb_var')
        data_cfb <- data_[!(data_[[row_var]] %in% baseline_name), , drop = F]
        data_cfb[[row_var]] <- factor(
            data_cfb[[row_var]],
            levels = row_levels[row_levels != baseline_name]
        )
        summary_tbl_cfb <- summary_matrix(
            data_cfb, row_var, col_var = col_var, val_var = cfb_var,
            col_totals = col_totals, name_totals = name_totals,
            func_list = func_list
        )
        
        # combine summary_tbl and summary_tbl_cfb
        summary_tbl_combined <- rbind(summary_tbl, summary_tbl_cfb)
        idx_val <- c(
            seq_len(n_funcs),
            unlist(lapply(seq(n_funcs + 1,
                              nrow(summary_tbl_combined),
                              by = 2 * n_funcs),
                          `+`, 0:(n_funcs - 1)))
        )
        idx_cfb <- unlist(lapply(
            seq(2 * n_funcs + 1, nrow(summary_tbl_combined), by = 2 * n_funcs),
            `+`, 0:(n_funcs - 1)))
        summary_tbl_combined[idx_val, ] <- summary_tbl
        summary_tbl_combined[idx_cfb, ] <- summary_tbl_cfb
        summary_tbl <- summary_tbl_combined
    }
    
    if(format == 'html') {
        header <- ternary(is_blank(col_var), 'Value', col_names)
        if(add_cfb && row_nlevels > 1) {
            idx_bs <- row_levels == baseline_name
            rnames <- rep(func_names, 2 * row_nlevels - sum(idx_bs))
            rgroup<- c(
                row_names[idx_bs],
                c(rbind(row_names[!idx_bs],
                        paste('Change from baseline,', row_names[!idx_bs])))
            )
            n_rgroup <- rep(n_funcs, 2 * row_nlevels - sum(idx_bs))
        } else {
            rnames <- rep(func_names, row_nlevels)
            rgroup <- row_names
            n_rgroup <- rep(n_funcs, row_nlevels)
        }
        
        #### ADDED by WANGSHU
        if(!is_blank(pval)) {
          if (pval_loc == "LAST") {
            rgroup <- c(rgroup, "Statistical Test")
            rnames <- c(rnames, "Pvalue")
            n_rgroup <- c(n_rgroup, 1)
          }
          
          if(pval_loc == "EACH") {
            rnames <- rep(c(func_names, "Pvalue"), row_nlevels)
            n_rgroup <- rep(n_funcs + 1, row_nlevels)
          }
        }
        ### END ADDED by WANGSHU
        
        html <- invisible(
            htmlTable::htmlTable(
                summary_tbl, header = header, rnames = rnames,
                rgroup = rgroup, n.rgroup = n_rgroup, rowlabel = rowlabel,
                caption = caption, tfoot = footnote, 
                css.cell = paste(c('padding-top: 0.5em', 'padding-right: 2em',
                                   'padding-bottom: 0.5em', 'padding-left: 2em'),
                                 sep = '', collapse = ';')
            )
        )
        return(html)
    } else if(format == 'rtf') {
        if(add_cfb && row_nlevels > 1) {
            summary_tbl_rtf <- do.call(
                rbind,
                c(list(summary_tbl), as.list(rep('', 2 * row_nlevels - 1)))
            )
            idx_na <- seq(1, nrow(summary_tbl_rtf), by = n_funcs + 1)
            idx_grp <- c(1)
            idx_rtf <- sort(setdiff(seq_len(nrow(summary_tbl_rtf)), idx_na))
            idx_na <- idx_na + 1
            idx_rtf <- idx_rtf + 1
            summary_tbl_rtf <- rbind(c(" "), summary_tbl_rtf)
            summary_tbl_rtf[idx_rtf, ] <- summary_tbl
            summary_tbl_rtf[idx_na, ] <- ''
            summary_tbl_rtf[idx_grp, ] <- ''
            add_col <- rep('', nrow(summary_tbl_rtf))
            visits <- c(
                baseline_name,
                rep(row_levels[-match(baseline_name, row_levels)], each = 2)
            )
            idx_cfb <- seq(3, length(visits), by = 2)
            visits[idx_cfb] <- paste('Change from baseline,', visits[idx_cfb])
            add_col[idx_na] <- visits
            add_col[idx_rtf] <- paste(rep(' ', visit_header_space), func_names)
            add_col[idx_grp] <- mdgrp 
        } else {
            summary_tbl_rtf <- do.call(
                rbind,
                c(list(summary_tbl), as.list(rep('', row_nlevels)))
            )
            idx_na <- seq(1, nrow(summary_tbl_rtf), by = n_funcs + 1)
            idx_rtf <- sort(setdiff(seq_len(nrow(summary_tbl_rtf)), idx_na)) 
            idx_na <- idx_na + 1
            idx_rtf <- idx_rtf + 1
            #idx_grp <- idx_na
            idx_grp <- c(1)
            summary_tbl_rtf <- rbind(c(" "), summary_tbl_rtf)
            summary_tbl_rtf[idx_rtf, ] <- summary_tbl
            summary_tbl_rtf[idx_na, ] <- ''
            summary_tbl_rtf[idx_grp, ] <- ''
            add_col <- rep('', nrow(summary_tbl_rtf))
            add_col[idx_na] <- row_levels
            add_col[idx_rtf] <- paste(rep(' ', visit_header_space), func_names)
            add_col[idx_grp] <- mdgrp
        }
        summary_tbl_rtf <- cbind(
            ' ' = add_col,
            summary_tbl_rtf
        )
        dimnames(summary_tbl_rtf) <- list(
            seq_len(nrow(summary_tbl_rtf)),
            c(' ', ternary(is_blank(col_var), 'Value', col_names))
        )
        return(summary_tbl_rtf)
    } else if(format == 'csv') {
        summary_tbl_csv <- data.frame(
            Statistics = func_names,
            Value = c(summary_tbl),
            stringsAsFactors = TRUE, check.names = TRUE
        )
        visits <- rep(row_levels, each = n_funcs)
        
        if(is_blank(col_var)) {
            if(add_cfb && row_nlevels > 1) {
                visits_cfb <- rep(row_levels[-match(baseline_name, row_levels)],
                                  each = n_funcs)
                summary_tbl_csv$Visits <- c(visits, visits_cfb)
                summary_tbl_csv$YVar <- c(
                    rep('AVAL', length(visits)), rep('CHG', length(visits_cfb))
                )
            } else {
                summary_tbl_csv$Visits <- visits
            }
        } else {
            group_levels <- rep(col_names, each = length(visits))
            visits_col <- rep(visits, length(col_names))
            if(add_cfb && row_nlevels > 1) {
                visits_cfb <- rep(row_levels[-match(baseline_name, row_levels)],
                                  each = n_funcs)
                visits_cfb_col <- rep(visits_cfb, length(col_names))
                group_levels_cfb <- rep(col_names, each = length(visits_cfb))
                summary_tbl_csv$Visits <- c(visits_col, visits_cfb_col)
                summary_tbl_csv$Group <- c(group_levels, group_levels_cfb)
                summary_tbl_csv$YVar <- c(
                    rep('AVAL', length(visits_col)),
                    rep('CHG', length(visits_cfb_col))
                )
                summary_tbl_csv$visit_num <- as.numeric(summary_tbl_csv$Visits)
                summary_tbl_csv_AVAL_t <- summary_tbl_csv[summary_tbl_csv$YVar == 'AVAL',] %>% spread(Group, Value)
                summary_tbl_csv_CHG_t <- summary_tbl_csv[summary_tbl_csv$YVar == 'CHG',] %>% spread(Group, Value)
                summary_tbl_csv <- rbind(summary_tbl_csv_AVAL_t,summary_tbl_csv_CHG_t)
                summary_tbl_csv <- summary_tbl_csv[with(summary_tbl_csv,order(YVar, visit_num, Statistics)),]
                summary_tbl_csv <- subset(summary_tbl_csv, select = -visit_num )

            } else {
                summary_tbl_csv$Visits <- visits_col
                summary_tbl_csv$Group <- group_levels
                
                # Transpose the csv output data
                  summary_tbl_csv_t <- summary_tbl_csv %>% spread(Group, Value)
                  summary_tbl_csv_t$visit_num <- as.numeric(summary_tbl_csv_t$Visits)
                  summary_tbl_csv <- summary_tbl_csv_t[with(summary_tbl_csv_t,order(visit_num, Statistics)),]
                  summary_tbl_csv <- subset(summary_tbl_csv, select = -visit_num )
                  #summary_tbl_csv$grp1 <- grp_c
                  
                
                #summary_tbl_csv <- summary_tbl_csv_t
                #summary_tbl_csv$Visit_Stat <- paste0(summary_tbl_csv$Visit,summary_tbl_csv$Statistics)
                #summary_tbl_csv_x <- summary_tbl_csv_t[1:10, ]
                ##@summary_tbl_csv <- summary_tbl_csv_t[order(summary_tbl_csv_t[,2], summary_tbl_csv_t[,1]),]
                #summary_tbl_csv <- summary_tbl_csv_t[order(visit_num, Statistics),]

                
            }
            
        }
        return(summary_tbl_csv)
    }
}


# rtf table wrapper
rtf_table_wrapper <- function(file, tbl, width = 11, height = 8.5,
                              fontsize = 8, omi = NULL, cell1 = NULL,
                              cell2 = 1, nheader = NULL, 
                              nline.table = 42, nline.body = NULL,
                              block_break = FALSE, nline_block = NULL, 
                              caption = '', footnote = '',
                              content_width = 2, firstcol_width = 2,
                              addSpaceHeader = 0, addSpaceFoot = 0) {
    block_break <- isTRUE(block_break)
    caption <- trimws(caption)
    footnote <- trimws(footnote)
    num_wd <- min(max(nchar(tbl[, -1])) * fontsize * 0.01, content_width)
    firstcol_wd <- min(max(nchar(tbl[, 1])) * fontsize * 0.01, firstcol_width)
    width_content <- firstcol_wd + num_wd * (ncol(tbl) - 1)
    if(!is.null(omi)) omi <- omi
    else {
        left_omi <- (width - width_content) / 2
        right_omi <- left_omi
        omi = c(1, left_omi, 1, right_omi)
    }
    rtf <- rtf::RTF(file, width = width, height = height, omi = omi,
                    font.size = fontsize)
    if(is.null(nline.body)) {
        caption_lines <- length(unlist(strsplit(caption, '\n')))
        footnote_lines <- length(unlist(strsplit(footnote, '\n')))
        header_lines <- 1
        if(!is.null(nheader)) {
            if(is.null(cell2)) header_lines <- cell2
            header_lines <- header_lines + nheader - 1
        }
        nline.body <- nline.table - caption_lines - footnote_lines - header_lines
    }
    if(nrow(tbl) < nline.body) nline.body <- nrow(tbl)
    else {
        if(block_break) {
            stopifnot(!is.null(nline_block))
            nline.body <- nline_block * (nline.body %/% nline_block)
        }
    }
    rtf.table.out(
      rtf, tb = tbl,
      cell1 = cell1, cell2 = cell2, nheader = nheader,
      colFormat = c('L', rep('C', ncol(tbl) - 1)), 
      cw = c(firstcol_wd, rep(num_wd, ncol(tbl) - 1)),
      width = width, height = height, omi = omi,
      varName = NULL, var.ul = NULL,
      titles = caption, prd.status = '', footns = footnote,
      nline.body = nline.body, addSpaceHeader = addSpaceHeader,
      addSpaceFoot = addSpaceFoot
    )
    done(rtf)

}


# survival table
survival_table <- function(data, event_var, time_var, group_var,
                           group_levels = NULL, digits = 2,
                           event_val = 1, non_event_val = 0,
                           time_unit = '', caption = '', footnote = '',
                           output = 'html') {
    all_cols <- names(data)
    if(!all(c(event_var, time_var, group_var) %in% all_cols))
        stop('`event_var`, `time_var` and `group_var` all must be present in data')
    output <- match.arg(output, c('html', 'rtf', 'csv'))
    group_ <- data[[group_var]]
    if(!is.factor(group_)) group_ <- factor(group_)
    if(is.null(group_levels)) group_levels <- levels(group_)
    data_by_group <- split(data, f = group_)
    event_by_group <- split(data[[event_var]], f = group_)
    match_event <- partial(n_match_pctg_str, to_match = event_val,
                           digits = digits)
    match_non_event <- partial(n_match_pctg_str, to_match = non_event_val,
                               digits = digits)
    event_row <- unlist(lapply(event_by_group, match_event))
    non_event_row <- unlist(lapply(event_by_group, match_non_event))
    
    formula_ <- as.formula(paste(
        'survival::Surv(', time_var, ',', event_var, ') ~', group_var
    ))
    fit_summary <- summary(survival::survfit(formula_, data = data,
                                             conf.type = 'plain'))
    med_tte <- specify_decimal(fit_summary$table[, 'median'], digits)
    med_ci_tte <- paste0(
        '(', specify_decimal(fit_summary$table[, '0.95LCL'], digits), ', ',
        specify_decimal(fit_summary$table[, '0.95UCL'], digits), ')'
    )

    formula_each <- as.formula(paste(
        'survival::Surv(', time_var, ',', event_var, ') ~ 1'
    ))
    fit_list <- lapply(data_by_group, survival::survfit, formula = formula_each)
    q1_tte <- unlist(lapply(fit_list, function(fit) {which(fit$surv <= 0.75)[1]}))
    q3_tte <- unlist(lapply(fit_list, function(fit) {which(fit$surv <= 0.25)[1]}))
    q1_q3_tte <- paste0(
        specify_decimal(q1_tte, digits), ', ', specify_decimal(q3_tte, digits)
    )
    min_tte <- unlist(lapply(fit_list, function(fit) {min_na(fit$time)}))
    max_tte <- unlist(lapply(fit_list, function(fit) {max_na(fit$time)}))
    range_tte <- paste0(
        specify_decimal(min_tte, digits), ', ', specify_decimal(max_tte, digits)
    )
    
    formula_logrank <- as.formula(paste(
        'survival::Surv(', time_var, ',', event_var, ') ~ factor(', group_var, ')'
    ))
    logrank <- survival::survdiff(formula_logrank, data = data)
    pval <- specify_decimal(1 - pchisq(logrank$chisq, length(logrank$n) - 1),
                            digits + 1)
    cox <- summary(survival::coxph(formula_logrank, data = data))
    hdratio <- specify_decimal(cox$coefficients[, 'exp(coef)'], digits)
    hdratio_ci <- paste0(
        '(', specify_decimal(cox$conf.int[, 'lower .95'], digits), ', ',
        specify_decimal(cox$conf.int[, 'upper .95'], digits), ')'
    )
    col_names <- paste(
        levels(group_),
        paste('N =', unlist(lapply(event_by_group, length))), sep = '\n'
    )
    tte_header <- trimws(paste('Time to event', add_parenthesis(time_unit)))
    if(output == 'csv') {
        
    } else if(output == 'html') {
        table_out <- rbind(
            event_row, non_event_row, '', med_tte, med_ci_tte,
            q1_q3_tte, range_tte, c('', pval), '', c('', hdratio),
            c('', hdratio_ci)
        )
        header <- col_names
        rnames <- c('Patients with event', 'Patients without event', '',
                    'Median', '95% CI Median', '25% and 75%-ile',
                    'Range (inc. cens.)', 'p-value (Log-Rank Test)', '',
                    'Hazard Ratio', '95% CI')
        rgroup <- c('', tte_header, '')
        n_rgroup <- c(3, 5, 3)
        html <- invisible(
            htmlTable::htmlTable(
                table_out, header = header, rnames = rnames,
                rgroup = rgroup, n.rgroup = n_rgroup, rowlabel = '',
                caption = caption, tfoot = footnote,
                css.cell = paste(c('padding-top: 0.5em', 'padding-right: 2em',
                                   'padding-bottom: 0.5em', 'padding-left: 2em'),
                                 sep = '', collapse = ';')
            )
        )
        return(html)
    } else if(output == 'rtf') {
        table_out <- rbind(
            event_row, non_event_row, '', med_tte, med_ci_tte,
            q1_q3_tte, range_tte, c('', pval), '', '', c('', hdratio),
            c('', hdratio_ci)
        )
        table_out <- cbind(
            c('Patients with event', 'Patients without event', '', tte_header,
              paste0('    ',
                     c('Median', '95% CI Median', '25% and 75%-ile',
                       'Range (inc. cens.)', 'p-value (Log-Rank Test)')), '',
              'Hazard Ratio', '95% CI'), table_out
        )
        dimnames(table_out) <- list(seq_len(nrow(table_out)), c(' ', col_names))
        return(table_out)
    }
}





























