#-----------------------------------------------------------------------------
# Purpose:  Draw scatter plot with ggplot2
# Author:   Feiyang Niu
# Date:     May 11, 2017
#-----------------------------------------------------------------------------


# load required r scripts
use_package('ggplot2')
# use_package('ggrepel')
source('r_scripts/function_utils.R')
source('r_scripts/plot_utils.R')


gg_scatter <- function(data, x, y, label = NULL,
                       facet_r = NULL, facet_c = NULL,
                       facet_r_levels = NULL, facet_c_levels = NULL,
                       color_var = NULL, all_colors = NULL,
                       shape_var = NULL, all_shapes = NULL,
                       add_label = FALSE, repel_label = FALSE,
                       label_xlim = c(-Inf, Inf), label_ylim = c(-Inf, Inf),
                       label_xloc = 'middle', label_yloc = 'middle',
                       x_lab = x, y_lab = y, title = '',
                       x_limit = NULL, y_limit = NULL,
                       x_log = FALSE, y_log = FALSE,
                       add_legend = TRUE, legend_pos = 'bottom',
                       reference_hline = NULL, reference_vline = NULL,
                       bw_theme = TRUE, grids = 'on') {
    
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
    column_in_dataframe(x, data)
    column_in_dataframe(y, data)
    if(!is_blank(label)) column_in_dataframe(label, data)
    if(!is_blank(facet_r)) column_in_dataframe(facet_r, data)
    if(!is_blank(facet_c)) column_in_dataframe(facet_c, data)
    if(!is_blank(color_var)) column_in_dataframe(color_var, data)
    if(!is_blank(shape_var)) column_in_dataframe(shape_var, data)
    add_label <- isTRUE(add_label)
    repel_label <- isTRUE(repel_label)
    if(add_label && !is_blank(label_xlim))
        check_var_class(label_xlim, is.numeric, 'numeric')
    if(add_label && !is_blank(label_ylim))
        check_var_class(label_ylim, is.numeric, 'numeric')
    if(add_label) {
        if(is_blank(label_xloc)) label_xloc <- 'middle'
        else arg_in_choices(label_xloc, c('middle', 'sides'))
        if(is_blank(label_yloc)) label_yloc <- 'middle'
        else arg_in_choices(label_yloc, c('middle', 'sides'))
    }
    if(!is.null(x_lab)) check_var_class(x_lab, is.character, 'character')
    if(!is.null(y_lab)) check_var_class(y_lab, is.character, 'character')
    if(!is.null(title)) check_var_class(title, is.character, 'character')
    if(!is_blank(x_limit)) check_var_class(x_limit, is.numeric, 'numeric')
    if(!is_blank(y_limit)) check_var_class(y_limit, is.numeric, 'numeric')
    x_log <- isTRUE(x_log)
    y_log <- isTRUE(y_log)
    add_legend <- isTRUE(add_legend)
    if(add_legend) arg_in_choices(legend_pos, c('left', 'right', 'bottom', 'up'))
    if(!is_blank(reference_hline))
        check_var_class(reference_hline, is.numeric, 'numeric')
    if(!is_blank(reference_vline))
        check_var_class(reference_vline, is.numeric, 'numeric')
    bw_theme <- isTRUE(bw_theme)
    arg_in_choices(grids, c('on', 'x', 'y', 'off'))
    
    
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
    
    if(!is_blank(shape_var)) {
        num_shapes <- length(unique(na.omit(data[[shape_var]])))
        all_shapes <- c(0:25, 32:255)[seq_len(num_shapes)]
    } else all_shapes <- NULL
    
    
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
        x_log = x_log, y_log = y_log,
        add_legend = add_legend, legend_pos = legend_pos,
        color_var = color_var, all_colors = all_colors,
        shape_var = shape_var, all_shapes = all_shapes,
        reference_hline = reference_hline, reference_vline = reference_vline,
        bw_theme = bw_theme, grids = grids
    )
    
    # add points
    plot_ <- plot_ + geom_point()
    
    # add labels
    data_label <- data
    if(!is_blank(label_xlim)) {
        if(label_xloc == 'middle') {
            cond_x <- data_label[[x]] >= label_xlim[1] &
                data_label[[x]] <= label_xlim[2]
        } else {
            cond_x <- data_label[[x]] <= label_xlim[1] |
                data_label[[x]] >= label_xlim[2]
        }
        data_label <- data_label[cond_x, , drop = FALSE]
    }
    if(!is_blank(label_xlim)) {
        if(label_yloc == 'middle') {
            cond_y <- data_label[[y]] >= label_ylim[1] &
                data_label[[y]] <= label_ylim[2]
        } else {
            cond_y <- data_label[[y]] <= label_ylim[1] |
                data_label[[y]] >= label_ylim[2]
        }
        data_label <- data_label[cond_y, , drop = FALSE]
    }
    if(add_label && !is_blank(label)) {
        if(repel_label) {
            plot_ <- plot_ +
                ggrepel::geom_text_repel(
                    data = data_label,
                    aes_string(label = paste0('`', label, '`')),
                    show.legend = FALSE
                )
        } else {
            plot_ <- plot_ +
                geom_text(data = data_label,
                          aes_string(label = paste0('`', label, '`')),
                          hjust = 'inward', vjust = 0.5, show.legend = FALSE)
        }
    }
    
    return(plot_)
}


























