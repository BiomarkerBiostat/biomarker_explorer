#-----------------------------------------------------------------------------
# Purpose:  Draw dotplot with ggplot2
# Author:   Feiyang Niu
# Date:     March 6, 2017
#-----------------------------------------------------------------------------


# load required r scripts
use_package('ggplot2')
use_package('dplyr')
source('r_scripts/function_utils.R')
source('r_scripts/plot_utils.R')
# n_nna()
source('r_scripts/common_statistics.R')


#' Draw box plot
#' 
#' Function that generates box plot with ggplot2 package.
#' 
#' @param data Data frame: default dataset to use for plot. If not already a
#' data.frame, will be converted to one by fortify.
#' @param x Character: name of a data column mapped to x-axis
#' @param y Character: name of a data column mapped to y-axis
#' @param label Character: name of a data column used to label the points.
#'                  Default is set to `NULL`
#' @param facet_r Character: name of a data column 
gg_boxplot <- function(data, x, y, label = NULL,
                       facet_r = NULL, facet_c = NULL,
                       facet_r_levels = NULL, facet_c_levels = NULL,
                       color_var = NULL, x_lab = x, y_lab = y, title = '',
                       x_limit = NULL, y_limit = NULL, y_log = FALSE,
                       add_points = TRUE, point_shape = 19,
                       add_legend = TRUE, legend_pos = 'bottom',
                       reference_hline = NULL, reference_vline = NULL,
                       text_content = NULL, text_pos = 'topleft',
                       xtick_angle = 0, xtick_align = 'center',
                       x_point_var = 'x_point', randseed = 12345,
                       add_sample_size = TRUE, add_label = FALSE,
                       grids = 'on', return_data = FALSE) {
    
    #-----------------------------
    # argument match & error catch
    #-----------------------------
    if(!is.data.frame(data)) {
        tryCatch(
            data <- as.data.frame(data),
            error = function(err) {stop(
                'data must be a data frame or data frame convertable'
            )}
        )
    }
    has_x_var <- !is_blank(x)
    if(has_x_var) column_in_dataframe(x, data)
    column_in_dataframe(y, data)
    if(!is_blank(label)) column_in_dataframe(label, data)
    if(!is_blank(facet_r)) column_in_dataframe(facet_r, data)
    if(!is_blank(facet_c)) column_in_dataframe(facet_c, data)
    if(!is_blank(color_var)) column_in_dataframe(color_var, data)
    if(!is_blank(x_limit)) check_var_class(x_limit, is.numeric, 'numeric')
    if(!is_blank(y_limit)) check_var_class(y_limit, is.numeric, 'numeric')
    text_content <- trimws(text_content)
    y_log <- isTRUE(y_log)
    add_points <- isTRUE(add_points)
    add_legend <- isTRUE(add_legend)
    add_sample_size <- isTRUE(add_sample_size)
    add_label <- isTRUE(add_label)
    return_data <- isTRUE(return_data)
    if(add_legend) arg_in_choices(legend_pos, c('left', 'right', 'bottom', 'up'))
    if(!is_blank(reference_hline))
        check_var_class(reference_hline, is.numeric, 'numeric')
    if(!is_blank(reference_vline))
        check_var_class(reference_vline, is.numeric, 'numeric')
    if(is.character(text_pos))
        arg_in_choices(text_pos, c('topleft', 'topright',
                                   'bottomleft', 'bottomright'))
    if(is.character(xtick_align))
        arg_in_choices(xtick_align, c('left', 'center', 'right', 'up', 'bottom'))
    arg_in_choices(grids, c('on', 'x', 'y', 'off'))
    
    
    #-----------------------------
    # define constants and vars
    #-----------------------------
    alignment_dict <- list(
        'left' = 0, 'center' = 0.5, 'right' = 1,
        'up' = 1, 'bottom' = 0
    )
    if(is.character(xtick_align)) xtick_align <- alignment_dict[[xtick_align]]
    # create x as constant 1L if not specified
    if(!has_x_var) {
        x <- 'x_var'
        data[[x]] <- factor(1)
    }
    # convert x to numeric if not
    if(!is.numeric(data[[x]])) {
        data[[x]] <- factor(data[[x]])
        x_tick_labels <- levels(data[[x]])
        data[[x]] <- as.numeric(data[[x]])
        x_axis_breaks <- sort(unique_na(data[[x]]))
        # default expand on discrete x-axis
        x_expand <- c(0, 0.3)
    } else {
        x_axis_breaks <- NULL
        x_tick_labels <- NULL
        # default expand on continuous x-axis
        x_expand <- c(0.05, 0)
    }
    group_list <- NULL
    if(!is_blank(facet_r)) group_list <- c(group_list, facet_r)
    if(!is_blank(facet_c)) group_list <- c(group_list, facet_c)
    if(!is_blank(color_var)) group_list <- c(group_list, color_var)
    group_list <- unique(c(group_list, x))
    group_cols <- data[, group_list, drop = FALSE]
    data <- mutate(data, x_var_factor = do.call(interaction, as.list(group_cols)))
    dodge_boxplot_factor <- 0.75
    resolution_x <- ggplot2::resolution(
        sort(unique_na(data[[x]])), zero = FALSE
    )
    dodge_width <- resolution_x * dodge_boxplot_factor
    outlier_shape <- ifelse(add_points, NA, point_shape)
    if(is_blank(x_point_var)) x_point_var <- 'x_point'
    data[[x_point_var]] <- data[[x]]
    ngroups <- 1
    if(!is_blank(color_var)) {
        data[[color_var]] <- factor(data[[color_var]])
        ngroups <- nlevels(data[[color_var]])
    }
    if(ngroups > 1) {
        shift <- (as.integer(data[[color_var]]) - median(1:ngroups)) / ngroups
        data[[x_point_var]] <- data[[x_point_var]] + shift * dodge_width
    }
    if(add_points) {
        jitter_factor <- 0.2
        data[[x_point_var]] <- gg_jitter(
            data[[x_point_var]], dodge_width * jitter_factor, randseed
        )
    }
    
    
    #---------------------------
    # data manipulation
    #---------------------------
    if(!is_blank(facet_r)) {
        facet_r_levels <- unlist(facet_r_levels)
        if(is.null(facet_r_levels))
            facet_r_levels <- sort(unique(data[[facet_r]]))
        data[[facet_r]] <- factor(data[[facet_r]], levels = facet_r_levels)
        data <- data[!is.na(data[[facet_r]]), , drop = F]
        if(!is.null(names(facet_r_levels))) {
            levels(data[[facet_r]]) <- names(facet_r_levels)
        }
    }
    if(!is_blank(facet_c)) {
        facet_c_levels <- unlist(facet_c_levels)
        if(is.null(facet_c_levels))
            facet_c_levels <- sort(unique(data[[facet_c]]))
        data[[facet_c]] <- factor(data[[facet_c]], levels = facet_c_levels)
        data <- data[!is.na(data[[facet_c]]), , drop = F]
        if(!is.null(names(facet_c_levels))) {
            levels(data[[facet_c]]) <- names(facet_c_levels)
        }
    }
    
    
    #-----------------------------
    # make the plot
    #-----------------------------
    plot_ <- gg_wrapper(
        data = data,
        aes_string(x = paste0('`', x, '`'), y = paste0('`', y, '`')),
        facet_r = facet_r, facet_c = facet_c,
        facet_scale = 'free', facet_space = 'fixed',
        is_x_continuous = TRUE, is_y_continuous = TRUE,
        x_lab = x_lab, y_lab = y_lab, title = title,
        x_limit = x_limit, y_limit = y_limit,
        x_log = FALSE, y_log = y_log,
        x_axis_breaks = x_axis_breaks, x_tick_labels = x_tick_labels,
        xtick_angle = xtick_angle, x_hjust = xtick_align,
        add_legend = add_legend, legend_pos = legend_pos, color_var = color_var,
        reference_hline = reference_hline, reference_vline = reference_vline,
        grids = grids, x_expand = x_expand
    )
    
    plot_ <- plot_ +
        geom_boxplot(aes(group = x_var_factor), outlier.shape = outlier_shape,
                     position = position_dodge(dodge_width), alpha = 0.5)
    
    # add points
    if(add_points) {
        point_size <- 1
        point_alpha <- 0.5
        plot_ <- plot_ +
            geom_point(aes_string(x = x_point_var), shape = point_shape,
                       size = point_size, alpha = point_alpha)
        
        # add labels to the points
        if(add_label && !is_blank(label)) {
            plot_ <- plot_ +
                geom_text(aes_string(x = x_point_var, label = label),
                          vjust = 'inward', hjust = 'inward')
        }
    }
    
    
    if(!is_blank(text_content) || add_sample_size) {
        # extract y-axis range from the ggplot object
        plot_range <- ggplot_build(plot_)$layout$panel_ranges[[1]]
        plot_range_x <- plot_range$x.range
        plot_range_y <- plot_range$y.range
        min_x <- plot_range_x[1]; max_x <- plot_range_x[2]
        min_y <- plot_range_y[1]; max_y <- plot_range_y[2]
    }
    
    # add text
    if(!is_blank(text_content)) {
        if(is.character(text_pos)) {
            text_content <- trimws(unlist(strsplit(text_content, '\n')))
            num_lines <- length(text_content)
            text_content <- paste(text_content, '', collapse = '\n')
            if(text_pos == 'topleft') {
                text_pos <- c(-Inf, Inf)
                text_align <- c(0, 1)
            }
            else if(text_pos == 'topright') {
                text_pos <- c(Inf, Inf)
                text_align <- c(1, 1)
            }
            else if(text_pos == 'bottomleft') {
                text_pos <- c(-Inf, -Inf)
                vjust <- ifelse(num_lines == 1, -0.5,
                                ifelse(num_lines == 2, -0.2, -0.1))
                text_align <- c(0, vjust)
            }
            else {
                text_pos <- c(Inf, -Inf)
                vjust <- ifelse(num_lines == 1, -0.5,
                                ifelse(num_lines == 2, -0.2, -0.1))
                text_align <- c(1, vjust)
            }
        } else {
            text_pos <- pmin(text_pos, c(max_x, max_y))
            text_pos <- pmax(text_pos, c(min_x, min_y))
            text_align <- c(0, 1)
        }
        if(y_log) text_pos[2] <- 10^text_pos[2]
        plot_ <- plot_ + annotate(
            'text', x = text_pos[1], y = text_pos[2], label = text_content,
            hjust = text_align[1], vjust = text_align[2]
        )
    }
    
    # add sample size
    if(add_sample_size) {
        nrows <- 1
        if(!is_blank(facet_r)) nrows <- nlevels(factor(data[[facet_r]]))
        
        # for sample size annotation
        ss_factor <- 0.04
        fnote_size_ss <- 3 / (1 + 0.5 * (nrows - 1))
        center_aligned <- 0.5
        slightly_right_aligned <- 0.7
        
        dots_group <- lapply(group_list, as.symbol)
        dots_summarise <- setNames(list(
            lazyeval::interp(~n_nna(var), var = as.name(y)),
            lazyeval::interp(
                ~min_y-ss_factor*(max_y-min_y)*(as.integer(unique_na(var))-1),
                var = if(is_blank(color_var)) 1 else as.name(color_var)
            ),
            lazyeval::interp(~unique_na(var), var = as.name(x))
        ), c('n', 'y', 'x'))
        
        data_ss <- data %>%
            group_by_(.dots = dots_group) %>%
            summarise_(.dots = dots_summarise)
        if(y_log) data_ss$y <- 10^data_ss$y
        x_1 <- sort(unique_na(data_ss$x))[1]
        data_ss_1 <- filter(data_ss, x == x_1)
        plot_ <- plot_ +
            geom_text(data = data_ss_1, show.legend = FALSE,
                      aes(label = paste0('N=', data_ss_1$n), x = x, y = y),
                      size = fnote_size_ss, hjust = slightly_right_aligned)
        if(length(unique_na(data_ss$x)) > 1) {
            data_ss_rest <- filter(data_ss, x != x_1)
            plot_ <- plot_ +
                geom_text(data = data_ss_rest, show.legend = FALSE,
                          aes(label = data_ss_rest$n, x = x, y = y),
                          size = fnote_size_ss, hjust = center_aligned)
        }
    }
    
    if(!has_x_var) {
        plot_ <- gg_remove(plot_, elements_to_remove = 'xaxis')
    }
    
    if(return_data) {
        return(list(plot = plot_, data = data))
    } else return(plot_)
    
}






















