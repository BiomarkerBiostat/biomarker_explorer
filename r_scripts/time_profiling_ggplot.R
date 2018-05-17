# load required r scripts
use_package('ggplot2')
use_package('dplyr')
use_package('directlabels')
install_('lazyeval')
source('r_scripts/common_statistics.R')
source('r_scripts/plot_utils.R')


# plot time-profiling graph with ggplot
time_profiling_ggplot <- function(data, x, y, subject = NULL,
                                  group = NULL, group_levels = NULL,
                                  facet_r = NULL, facet_c = NULL,
                                  facet_r_levels = NULL, facet_c_levels = NULL,
                                  geoms = c('point', 'line', 'sumline', 'boxplot'),
                                  avg_method = 'mean', var_method = 'se',
                                  y_log = FALSE, sample_size = TRUE,
                                  sample_size_font_size = 3,
                                  xlab = x, ylab = y, group_lab = group,
                                  title = '', x_limit = NULL, y_limit = NULL,
                                  x_tick_label = x, all_xticks = FALSE,
                                  x_tick_angle = 0, y_tick_angle = 0,
                                  reference_line = NULL, subject_show = FALSE,
                                  add_legend = TRUE, legend_pos = 'bottom',
                                  all_colors = NULL, all_linetypes = NULL,
                                  highlighted_subj = NULL,
                                  randseed = 0, return_data = FALSE) {
    
    start_time <- proc.time()
    set.seed(randseed)
    
    #---------------------------
    # error-catch
    #---------------------------
    if(!is.data.frame(data)) stop('data must be a data frame')
    all_columns <- names(data)
    if(!all(x %in% all_columns, y %in% all_columns))
        stop('xvar & y must both be columns of data')
    if(!is_blank(group)) {
        if(!(group %in% all_columns))
            stop('group must be a column of data')
    }
    if(!is_blank(subject)) {
        if(!(subject %in% all_columns))
            stop('`subject` must be a column of data')
    }
    if(!is_blank(facet_r)) {
        if(!(facet_r %in% all_columns)) stop('facet_r must be a column of data')
    }
    if(!is_blank(facet_c)) {
        if(!(facet_c %in% all_columns)) stop('facet_c must be a column of data')
    }
    x_var_is_numeric <- TRUE
    tryCatch(
        data[[x]] <- as.numeric(data[[x]]),
        warning = function(w) {
            stop('x column must be numeric or numeric-convertable')
        }
    )
    if(!is.null(highlighted_subj)) {
        if(!is.data.frame(highlighted_subj) && is.character(highlighted_subj))
            stop('highlighted_subj must be either a data frame or subject id(s)')
    }
    
    #---------------------------
    # argument match
    #---------------------------
    y_log <- isTRUE(y_log)
    sample_size <- isTRUE(sample_size)
    add_legend <- isTRUE(add_legend)
    return_data <- isTRUE(return_data)
    all_xticks <- isTRUE(all_xticks)
    subject_show <- isTRUE(subject_show)
    if(!is.null(avg_method))
        avg_method <- match.arg(tolower(avg_method),choices = c('mean', 'median'))
    if(!is.null(var_method))
        var_method <- match.arg(tolower(var_method), choices = c('sd', 'se'))
    geoms <- tolower(geoms)
    legend_pos <- match.arg(tolower(legend_pos),
                            c('left', 'right', 'bottom', 'top'))
    
    #---------------------------
    # define constants (those could be moved to the function arguments)
    #---------------------------
    point_shape <- 19
    point_size <- 1
    point_alpha <- 0.5
    dodge_boxplot_factor <- 0.75
    dodge_line_factor <- 0.4
    jitter_factor <- 0.25
    errorbar_factor <-0.6
    x_point_var <- 'x_point'
    
    #---------------------------
    # data manipulation
    #---------------------------
    data <- data[!is.na(data[[x]]) & !is.na(data[[y]]), ]
    if(!is.null(x_limit)) {
        data <- data[data[[x]] >= x_limit[1] & data[[x]] <= x_limit[2], ]
    }
    unique_x <- sort(unique_na(data[[x]]))
    group_list <- NULL
    nrows <- 1
    if(!is_blank(facet_r)) {
        facet_r_levels <- unlist(facet_r_levels)
        if(is.null(facet_r_levels))
            facet_r_levels <- sort(unique(data[[facet_r]]))
        data[[facet_r]] <- factor(data[[facet_r]], levels = facet_r_levels)
        data <- data[!is.na(data[[facet_r]]), , drop = F]
        if(!is.null(names(facet_r_levels))) {
            levels(data[[facet_r]]) <- names(facet_r_levels)
        }
        group_list <- c(group_list, facet_r)
        nrows <- nlevels(data[[facet_r]])
    }
    ncols <- 1
    if(!is_blank(facet_c)) {
        facet_c_levels <- unlist(facet_c_levels)
        if(is.null(facet_c_levels))
            facet_c_levels <- sort(unique(data[[facet_c]]))
        data[[facet_c]] <- factor(data[[facet_c]], levels = facet_c_levels)
        data <- data[!is.na(data[[facet_c]]), , drop = F]
        if(!is.null(names(facet_c_levels))) {
            levels(data[[facet_c]]) <- names(facet_c_levels)
        }
        group_list <- c(group_list, facet_c)
        ncols <- nlevels(data[[facet_c]])
    }
    ngroups <- 1
    if(!is_blank(group)) {
        group_levels <- unlist(group_levels)
        if(is.null(group_levels)) group_levels <- sort(unique(data[[group]]))
        data[[group]] <- factor(data[[group]], levels = group_levels)
        data <- data[!is.na(data[[group]]), , drop = F]
        if(!is.null(names(group_levels))) {
            levels(data[[group]]) <- names(group_levels)
        }
        group_list <- c(group_list, group)
        ngroups <- nlevels(data[[group]])
    }
    group_list <- c(group_list, x)
    
    # create jitter for geom_point manually if present
    resolution_x <- ggplot2::resolution(unique_x, zero = FALSE)
    dodge_line <- min(diff(range(unique_x))/50, resolution_x*dodge_line_factor)
    dodge_boxplot <- resolution_x * dodge_boxplot_factor
    dodge_geom <- ifelse('boxplot' %in% geoms, dodge_boxplot, dodge_line)
    dodge_ <- position_dodge(dodge_geom)
    if('point' %in% geoms) {
        if(ngroups > 1) {
            shift <- (as.integer(data[[group]]) - median(1:ngroups))/ngroups
            data[[x_point_var]] <- data[[x]] + shift * dodge_geom
        } else data[[x_point_var]] <- data[[x]]
        data[[x_point_var]] <- gg_jitter(
            data[[x_point_var]], dodge_geom * jitter_factor, randseed
        )
    }

    # define expand in ggplot2 axis
    if(sample_size) x_expand <- c(0.01, 0) else x_expand <- NULL
    
    # define 'line_size' colso that lines of highlighted_subj are highlighted
    line_alpha_col <- NULL
    if(!is.null(highlighted_subj)) {
        if(is.data.frame(highlighted_subj) && !is.null(subject))
            highlighted_subj <- unique(highlighted_subj[[subject]])
        if(is.character(highlighted_subj)) {
            highlighted_idx <- data[[subject]] %in% highlighted_subj
            if(any(highlighted_idx)) {
                highlighted_alpha <- 1
                unhighlighted_alpha <- 0.1
                point_alpha <- unhighlighted_alpha
                line_alpha_col <- 'line_alpha'
                data[['line_alpha']] <- unhighlighted_alpha
                data[['line_alpha']][highlighted_idx] <- highlighted_alpha
            }
        }
    }

    
    #---------------------------
    # make the plot
    #---------------------------
    plot_ <- gg_wrapper(
        data, aes_string(x = paste0('`', x, '`'),
                         y = paste0('`', y, '`')),
        facet_r = facet_r, facet_c = facet_c,
        y_log = y_log, x_lab = xlab, y_lab = ylab, title = title,
        x_limit = NULL, y_limit = y_limit,
        x_tick_angle = x_tick_angle, y_tick_angle = y_tick_angle,
        facet_scale = 'free', facet_space = 'fixed',
        add_legend = add_legend, legend_pos = legend_pos,
        color_var = group, all_colors = all_colors, color_lab = group_lab,
        linetype_var = if(is_blank(all_linetypes)) NULL else group,
        all_linetypes = all_linetypes, linetype_lab = group_lab,
        reference_hline = reference_line, x_expand = x_expand
    )
    
    if('boxplot' %in% geoms) {
        group_cols <- data[, group_list, drop = FALSE]
        data_boxplot <- data %>%
            mutate(x_var_factor = do.call(interaction, as.list(group_cols)))
        outlier_shape <- ifelse('point' %in% geoms, NA, point_shape)
        plot_ <- plot_ +
            geom_boxplot(data = data_boxplot, position = dodge_,
                         aes_string(x = x, group = 'x_var_factor'),
                         outlier.shape = outlier_shape, alpha = 0.5)
    }
    if('line' %in% geoms) {
        if(is.null(subject)) stop('please provide `subject`')
        plot_ <- plot_ + geom_line(aes_string(group = subject,
                                              alpha = line_alpha_col),
                                   show.legend = FALSE)
        if(subject_show) {
            subject_label_size <- 0.8
            plot_ <- plot_ + geom_dl(aes_string(label = subject),
                                     method = list(dl.combine('last.points'),
                                                   cex = subject_label_size))
        }
    }
    if('sumline' %in% geoms) {
        if(avg_method == 'median') {
            fun_y_ <- stats::median
            fun_data_ <- median_iqr
        } else if(avg_method == 'mean') {
            fun_y_ <- mean_na
            if(var_method == 'se') fun_data_ <- mean_se
            else if(var_method == 'sd') fun_data_ <- mean_sd
        }
        plot_ <- plot_ +
            stat_summary(fun.y = fun_y_, geom = 'line', position = dodge_) +
            stat_summary(fun.y = fun_y_, geom = 'point', position = dodge_) +
            stat_summary(fun.data = fun_data_, geom = 'errorbar',
                         width = dodge_geom * errorbar_factor,
                         position = dodge_, linetype = 'solid')
    }
    if('point' %in% geoms) {
        plot_ <- plot_ + geom_point(aes(x = x_point), shape = point_shape,
                                    size = point_size, alpha = point_alpha)
    }
    
    #---------------------------
    # add sample size
    #---------------------------
    if(sample_size) {
        
        # for sample size annotation
        ss_factor <- 0.04 * sample_size_font_size / 3
        fnote_size_ss <- sample_size_font_size / (1 + 0.5 * (nrows - 1))
        center_aligned <- 0.5
        slightly_right_aligned <- 0.7
        
        # calculate x-/y-axis range
        if(any(c('point', 'line', 'boxplot') %in% geoms)) {
            fun_yrange <- range_na
        } else if('sumline' %in% geoms) {
            fun_yrange <- function(x) {
                res <- fun_data_(x)
                if(is.na(res$ymin) || is.na(res$ymax)) return(c(res$y, res$y))
                return(c(res$ymin, res$ymax))
            }
        }
        group_list_ss <- c()
        if(!is_blank(facet_r)) group_list_ss <- c(group_list_ss, facet_r)
        if(!is_blank(facet_c)) group_list_ss <- c(group_list_ss, facet_c)
        yrange <- data %>%
            group_by_(.dots = lapply(group_list, as.symbol)) %>% 
            do(res = expand_interval(
                if(y_log) log(fun_yrange(.[[y]]), base = 10)
                else fun_yrange(.[[y]])
            )) %>%
            mutate(ymin = unlist(res)[1], ymax = unlist(res)[2]) %>%
            ungroup() %>%
            group_by_(.dots = lapply(group_list_ss, as.symbol)) %>%
            summarise(ymin = min(ymin), ymax = max(ymax))
        
        dots_ss <- setNames(list(
            lazyeval::interp(~n_nna(var), var = as.name(y)),
            lazyeval::interp(~unique_na(var), var = as.name(x))
        ), c('n', 'x'))
        data_ss <- data %>%
            group_by_(.dots = lapply(group_list, as.symbol)) %>%
            summarise_(.dots = dots_ss)
        
        dots_y_pos <- list(y = lazyeval::interp(
            ~min_y-ss_factor*(max_y-min_y)*(as.integer(unique_na(var_g))-1),
            min_y = as.name('ymin'), max_y = as.name('ymax'),
            var_g = if(is_blank(group)) 1 else as.name(group)
        ))
        if(length(group_list_ss) == 0) {
            data_ss$ymin <- yrange$ymin
            data_ss$ymax <- yrange$ymax
        } else {
            data_ss <- left_join(data_ss, yrange)
        }
        data_ss <- data_ss %>% mutate_(.dots = dots_y_pos)
        
        # # extract y-axis range from the ggplot object
        # plot_range_x <- ggplot_build(plot_)$layout$panel_ranges[[1]]$x.range
        # plot_range_y <- ggplot_build(plot_)$layout$panel_ranges[[1]]$y.range
        # min_x <- plot_range_x[1]; max_x <- plot_range_x[2]
        # min_y <- plot_range_y[1]; max_y <- plot_range_y[2]
        # print(paste('end of getting range:', (proc.time() - start_time)[3], 'seconds'))
        # dots_group <- lapply(group_list, as.symbol)
        # dots_summarise <- setNames(list(
        #     lazyeval::interp(~n_nna(var), var = as.name(y)),
        #     lazyeval::interp(
        #         ~min_y-ss_factor*(max_y-min_y)*(as.integer(unique_na(var))-1),
        #         var = if(is_blank(group)) 1 else as.name(group)
        #     ),
        #     lazyeval::interp(~unique_na(var), var = as.name(x))
        # ), c('n', 'y', 'x'))
        # 
        # data_ss <- data %>%
        #     group_by_(.dots = dots_group) %>%
        #     summarise_(.dots = dots_summarise)
        
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

    if(!is_blank(x_tick_label) && (all_xticks || x_tick_label != x)) {
        x_ticks <- sort(unique_na(data[[x]]))
        x_tick_labels <- unique_na(data[[x_tick_label]][order(data[[x]])])
        plot_ <- plot_ +
            scale_x_continuous(breaks = x_ticks, labels = x_tick_labels)
    }
    
    if(return_data) {
        return(list(plot = plot_, data = data))
    } else return(plot_)
    
}






























