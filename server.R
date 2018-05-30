# server.R

shinyServer(function(input, output, session) {
    
    
    #-----------------------------------------------
    # Import data page
    #-----------------------------------------------
    
    # define some variables at the app launch
    file_import <- reactiveValues(status = FALSE, message = '')
    raw_data <- reactiveValues(value = NULL)
    tnf <- reactiveValues(
        time_current = NULL, ass_current = NULL,
        dir = NULL, name = NULL, time_df = NULL, ass_df = NULL,
        time_count = 0, ass_count = 0, ass_count_graph = 0,
        ass_count_table = 0, ass_count_type = rep(0, 5)
    )
    
    # initialization
    observe({
        if(!is.null(input$file)) {
            results_ <- tryCatch(
                shiny_readin_file(input$file, 1, required_cols), error = c
            )
            if('message' %in% names(results_)) {
                file_import$status <- FALSE
                file_import$message <- results_$message
            } else {
                file_import$status <- TRUE
                file_import$message <- 'The data file is successfully imported!'
                raw_data$value <- results_
            }
            # if(file_import$status) {
            #     if(is.null(tnf$dir)) tnf$dir <- tempdir()
            #     if(is.null(tnf$name))
            #         tnf$name <- paste0(
            #             'TNF_', format(Sys.Date(),format = '%Y%m%d'), '.xls'
            #         )
            #     tnf_create(tnf$name, tnf$dir, out_tnf_names)
            # }
        }
    }, priority = 10)

    # print data importation status
    output$file_import_status <- renderUI({
        req(input$file)
        HTML(gsub('\n', '<br/>', file_import$message))
    })
    
    # order the raw data by SUBJID & Visits
    order_data <- reactive({
        req(file_import$status)
        data <- raw_data$value
        data <- arrange_(data, study_col, subj_col, xvar_col, param_col)
        return(data)
    })
    
    # subset data
    subset_data <- reactive({
        data <- order_data()
        to_filter <- rep(TRUE, nrow(data))
        subset_expr <- ''
        if(file_subset$done) {
            for(i in seq_len(file_subset_num_cond)) {
                var_name <- input[[paste0('file_subset_var_', i)]]
                val_name <- input[[paste0('file_subset_val_', i)]]
                if(!is_blank(var_name) && !is_blank(val_name)) {
                    if(is.numeric(data[[var_name]]) ||
                       is.date(data[[var_name]])) {
                        to_filter <- to_filter & between(
                            data[[var_name]], val_name[1], val_name[2]
                        )
                        subset_expr <- paste(
                            subset_expr, '&', '(',
                            paste(paste(var_name, '>=', val_name[1]),
                                  paste(var_name, '<=', val_name[2]),
                                  sep = '&'), ')'
                        )
                    } else {
                        to_filter <- to_filter & (data[[var_name]] %in% val_name)
                        subset_expr <- paste(
                            subset_expr, '&', '(',
                            paste(var_name, '%in%', deparse(val_name)), ')'
                        )
                    }
                }
            }
        }
        data <- data[to_filter, ]
        return(list(data = data, subset_expr = subset_expr))
    })
    
    # action button to open data subset modal window
    output$file_subset_button <- renderUI({
        req(input$file)
        req(file_import$status)
        actionButton('file_subset_button', 'Data subset')
    })
    
    # print out top-10 rows button widget
    output$file_topn_button <- renderUI({
        req(input$file)
        req(file_import$status)
        actionButton('file_topn_button', 'View top 10 rows', icon = icon('table'))
    })
    
    # select input for choosing column on data subset modal window
    file_subset <- reactiveValues(number_rows = 1, done = FALSE)
    #   -- Remark: These two lines take the value of file_subset$number_rows 
    #              and make it available in the client as
    #              output.file_subset_number_rows.
    output$file_subset_number_rows <- reactive(file_subset$number_rows)
    outputOptions(output, 'file_subset_number_rows', suspendWhenHidden = FALSE)
    output$file_subset_var <- renderUI({tagList(
        lapply(seq_len(file_subset_num_cond), function(i) {
            var_name <- paste0('file_subset_var_', i)
            conditionalPanel(
                condition = paste0('output.file_subset_number_rows', '>=', i),
                output[[var_name]] <- renderUI({
                    choices <- c('Choose'='', names(raw_data$value))
                    selectInput(var_name, paste('Variable', i), choices)
                })
            )
        })
    )})

    # input widget for choosing values on data subset modal window
    output$file_subset_val <- renderUI({tagList(
        lapply(seq_len(file_subset_num_cond), function(i) {
            data <- raw_data$value
            
            var_name <- input[[paste0('file_subset_var_', i)]]
            val_name <- paste0('file_subset_val_', i)
            show_condition <- paste0('input.file_subset_var_', i, ' && ',
                                     'output.file_subset_number_rows', '>=', i)
            if(!is.null(var_name) && is.numeric(data[[var_name]])) {
                val_min <- min_na(data[[var_name]])
                val_max <- max_na(data[[var_name]])
                stillSelected <- isolate(
                    ternary(length(input[[val_name]]) == 2, input[[val_name]],
                            ternary(length(input[[val_name]]) == 1,
                                    c(input[[val_name]], val_max),
                                    c(val_min, val_max)))
                )
                conditionalPanel(
                    condition = show_condition,
                    sliderInput(val_name, var_name, val_min, val_max,
                                stillSelected)
                )
            } else if(!is.null(var_name) && is.date(data[[var_name]])) {
                start <- min_na(data[[var_name]])
                end <- max_na(data[[var_name]])
                conditionalPanel(
                    condition = show_condition,
                    dateRangeInput(val_name, var_name, start, end)
                )
            } else if(!is.null(var_name)) {
                if(is_blank(var_name))
                    choices <- character(0)
                else
                    choices <- c('Choose'='', unique(data[[var_name]]))
                stillSelected <- isolate(
                    input[[val_name]][input[[val_name]] %in% choices]
                )
                conditionalPanel(
                    condition = show_condition,
                    selectizeInput(
                        val_name, var_name, choices, stillSelected, multiple = T,
                        options = list(plugins = list('drag_drop','remove_button'))
                    )
                )
            }
        })
    )})
    
    # action button to add more data filtering rows
    output$file_subset_add <- renderUI({
        actionButton('file_subset_add', 'Add more', icon = icon('plus'))
    })
    observeEvent(input$file_subset_add, {
        file_subset$number_rows <- file_subset$number_rows + 1
    })
    
    # action button to close the data subset modal window
    output$file_subset_done <- renderUI({
        actionButton('file_subset_done', 'Done', icon = icon('check'))
    })
    observeEvent(input$file_subset_done, {
        file_subset$done <- TRUE
        toggleModal(session, 'file_subset_bsmodal', toggle = 'close')
    })
    
    # action button to clear the data filtering
    output$file_subset_clear <- renderUI({
        actionButton('file_subset_clear', 'Reset', icon = icon('undo'))
    })
    observeEvent(input$file_subset_clear, {
        for(i in seq_len(file_subset_num_cond)) {
            choices <- c('Choose'='', names(raw_data$value))
            updateSelectInput(
                session, paste0('file_subset_var_', i), paste('Variable', i),
                choices = choices, selected = NULL
            )
        }
        file_subset$done <- FALSE
        file_subset$number_rows <- 1
    })
    
    # print out top-1o rows only when the 'print' button is pressed
    output$file_topn_table <- DT::renderDataTable({
        req(file_import$status)
        req(input$file_topn_button)
        if(input$file_topn_button == 0)
            return()
        DT::datatable(raw_data$value, options = list(autoWidth = TRUE))
    })
    
    # extract time unit from data
    t_unit <- reactive({
        req(file_import$status)
        data <- req(raw_data$value)
        result <- time_unit(data[[xlabel_col]])
        if(is.null(result) || length(result) > 1) return('')
        return(result)
    })
    
    # test if biomarker measurement unit exists
    bmk_unit <- reactive({
        req(file_import$status)
        data <- req(raw_data$value)
        if(!(bmk_unit_col %in% names(data)))
            data[[bmk_unit_col]] <- ''
        group_ <- split(data[[bmk_unit_col]], data[[param_col]])
        return(lapply(lapply(group_, unique), as.character))
    })
    
    # extract a list of biomarker names
    bmk_names <- reactive({
        req(file_import$status)
        data <- req(raw_data$value)
        return(unique(data[[param_col]]))
    })
    
    # obtain a list of two types of names:
    #   1. continuous variable names
    #   2. discrete variable names
    col_continuity <- reactive({
        req(file_import$status)
        data <- raw_data$value
        names_var <- names(data)
        is_cont <- unlist(lapply(data[, names_var, drop = F], continuity_test))
        names_cont <- c(names_var[is_cont])
        names_disc <- c(names_var[!is_cont])
        bmk_list <- split(data[[aval_col]], data[[param_col]])
        is_cont_bmk <- unlist(lapply(bmk_list, continuity_test))
        if(any(is_cont_bmk))
            names_cont <- c(names_cont, aval_col)
        if(any(!is_cont_bmk))
            names_disc <- c(names_disc, aval_col)
        return(list(continuous = sort(names_cont), discrete = sort(names_disc)))
    })
    
    # reset all the input value when new data file is uploaded
    observe({
        input$file
        shinyjs::reset('file_format')
        shinyjs::reset('time')
        shinyjs::reset('ass_overall')
        shinyjs::reset('ass_x')
        shinyjs::reset('ass_y')
        shinyjs::reset('ass_add_smooth')
        file_subset$done <- FALSE
        file_subset$number_rows <- 1
        tnf$dir = NULL
        tnf$name = NULL
        tnf$time_df = NULL
        tnf$ass_df = NULL
        tnf$time_count = 0
        tnf$ass_count = 0; tnf$ass_count_graph = 0; tnf$ass_count_table = 0
        tnf$ass_count_type = rep(0, 5)
    })
    
    
    #-----------------------------------------------
    # Time profiling page
    #-----------------------------------------------
    
    # a drop-down select list for choosing study
    output$time_study <- renderUI({
        req(file_import$status)
        data <- subset_data()$data
        choices <- c('Choose' = '', unique(data[[study_col]]))
        selected <- isolate(
            ifelse(is.null(input$time_study), '', input$time_study)
        )
        selectInput('time_study', 'Study', choices, selected = selected)
    })
    observe({
        data <- subset_data()$data
        if(!is_blank(input$time_cohort)) {
            data <- data[data[[cohort_col]] == input$time_cohort, , drop = F]
        }
        if(!is_blank(input$time_bmk)) {
            data <- data[data[[param_col]] %in% input$time_bmk, , drop = F]
        }
        if(!is_blank(input$time_group) && !is_blank(time_group_levs$value)) {
            cond_g <- data[[input$time_group]] %in% time_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$time_facet_r) &&
           !is_blank(time_facet_rlevs$value)) {
            cond_r <- data[[input$time_facet_r]] %in% time_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        if(!is_blank(input$time_facet_c) &&
           !is_blank(time_facet_clevs$value)) {
            cond_c <- data[[input$time_facet_c]] %in% time_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- c('Choose' = '', unique(data[[study_col]]))
        selected <- isolate(
            ifelse(is.null(input$time_study), '', input$time_study)
        )
        updateSelectInput(session, 'time_study', 'Study', choices, selected)
    })
    
    # a drop-down select list for choosing cohort
    output$time_cohort <- renderUI({
        req(file_import$status)
        data <- subset_data()$data
        req(cohort_col %in% names(data))
        choices <- c('Choose' = '', unique(data[[cohort_col]]))
        selected <- isolate(
            ifelse(is.null(input$time_cohort), '', input$time_cohort)
        )
        selectInput('time_cohort', 'Cohort', choices, selected = selected)
    })
    observe({
        data <- subset_data()$data
        if(!is_blank(input$time_study)) {
            data <- data[data[[study_col]] == input$time_study, , drop = F]
        }
        if(!is_blank(input$time_bmk)) {
            data <- data[data[[param_col]] %in% input$time_bmk, , drop = F]
        }
        if(!is_blank(input$time_group) && !is_blank(time_group_levs$value)) {
            cond_g <- data[[input$time_group]] %in% time_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$time_facet_r) &&
           !is_blank(time_facet_rlevs$value)) {
            cond_r <- data[[input$time_facet_r]] %in% time_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        if(!is_blank(input$time_facet_c) &&
           !is_blank(time_facet_clevs$value)) {
            cond_c <- data[[input$time_facet_c]] %in% time_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- c('Choose' = '', unique(data[[cohort_col]]))
        selected <- isolate(
            ifelse(is.null(input$time_cohort), '', input$time_cohort)
        )
        updateSelectInput(session, 'time_cohort', 'Cohort', choices, selected)
    })
    
    # a drop-down select list for choosing biomarker
    output$time_bmk <- renderUI({
        req(file_import$status)
        data <- subset_data()$data
        
        if(!is_blank(input$time_study))
          data <- data[data[[study_col]] == input$time_study, , drop = F]
        if(!is_blank(input$time_cohort))
          data <- data[data[[cohort_col]] == input$time_cohort, , drop = F]
        if(!is_blank(input$time_group) && !is_blank(time_group_levs$value)) {
          cond_g <- data[[input$time_group]] %in% time_group_levs$value
          data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$time_facet_r) &&
           !is_blank(time_facet_rlevs$value)) {
          cond_r <- data[[input$time_facet_r]] %in% time_facet_rlevs$value
          data <- data[cond_r, , drop = F]
        }
        if(!is_blank(input$time_facet_c) &&
           !is_blank(time_facet_clevs$value)) {
          cond_c <- data[[input$time_facet_c]] %in% time_facet_clevs$value
          data <- data[cond_c, , drop = F]
        }
        
        choices <- c('Choose' = '' , unique(data[[param_col]]))
        selected <- isolate(ternary(
          is.null(input$time_bmk), '', input$time_bmk
        ))    
        #selectInput('time_bmk', 'Biomarker', choices, selected = selected)
        selectizeInput(
          'time_bmk', 'Biomarker', choices, selected = selected,
          multiple = T, options = list(plugins = list('drag_drop','remove_button'))
        )
    })
 
    
    # a drop-down select list for choosing y variable type
    output$time_y <- renderUI({
      req(file_import$status)
      selectInput('time_y', 'Y Variable', c('Choose'='', time_y_list))
    })
    
    # a drop-down select list for choosing time profiling graph type
    # a drop-down select list for choosing time profiling graph type
    output$time_graph_type <- renderUI({
      req(file_import$status)
      choices <- ternary(length(input$time_bmk) > 1 || 'All' %in% input$time_bmk,
                         time_graph_list, time_output_list)
      selectInput('time_graph_type', 'Output Type', c('Choose'='', choices))
    })
    
    # a drop-down select list for choosing group variable
    output$time_group <- renderUI({
        req(file_import$status)
        data <- subset_data()$data
        names_disc <- c(names(data)[!unlist(lapply(data, continuity_test))])
        choices <- c('Choose' = '', names_disc)
        selected <- isolate(
            ifelse(is.null(input$time_group), '', input$time_group)
        )
        selectInput('time_group', 'Group', choices, selected = selected)
    })
    observe({
        data <- subset_data()$data
        to_remove <- NULL
        if(!is_blank(input$time_study)) {
            data <- data[data[[study_col]] == input$time_study, , drop = F]
            to_remove <- c(to_remove, study_col)
        }
        if(!is_blank(input$time_cohort)) {
            data <- data[data[[cohort_col]] == input$time_cohort, , drop = F]
            to_remove <- c(to_remove, cohort_col)
        }
        if(!is_blank(input$time_bmk)) {
            #*data <- data[data[[param_col]] == input$time_bmk, , drop = F]
            data <- data[data[[param_col]] %in% input$time_bmk, , drop = F]
            #*to_remove <- c(to_remove, param_col)
        }
        if(!is_blank(input$time_facet_r) && !is_blank(time_facet_rlevs$value)) {
            cond_r <- data[[input$time_facet_r]] %in% time_facet_rlevs$value
            data <- data[cond_r, , drop = F]
            to_remove <- c(to_remove, input$time_facet_r)
        }
        if(!is_blank(input$time_facet_c) && !is_blank(time_facet_clevs$value)) {
            cond_c <- data[[input$time_facet_c]] %in% time_facet_clevs$value
            data <- data[cond_c, , drop = F]
            to_remove <- c(to_remove, input$time_facet_c)
        }
        names_disc <- c(names(data)[!unlist(lapply(data, continuity_test))])
        names_disc <- setdiff(names_disc, to_remove)
        choices <- c('Choose' = '', names_disc)
        selected <- isolate(
            ifelse(is.null(input$time_group), '', input$time_group)
        )
        updateSelectInput(session, 'time_group', 'Group', choices, selected)
    })
    
    # group variable levels
    output$time_group_levs <- renderUI({
        req(input$time_group)
        data <- subset_data()$data
        choices <- sort(unique(data[[input$time_group]]))
        selected <- isolate(ternary(
            !is_blank(input$time_group_levs) &&
                all(input$time_group_levs %in% choices),
            input$time_group_levs, choices
        ))
        selectizeInput(
            'time_group_levs', input$time_group, choices,
            selected = selected, multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(input$time_group)
        data <- subset_data()$data
        if(!is_blank(input$time_study)) {
            data <- data[data[[study_col]] == input$time_study, , drop = F]
        }
        if(!is_blank(input$time_cohort)) {
            data <- data[data[[cohort_col]] == input$time_cohort, , drop = F]
        }
        if(!is_blank(input$time_bmk)) {
            #*data <- data[data[[param_col]] == input$time_bmk, , drop = F]
          data <- data[data[[param_col]] %in% input$time_bmk, , drop = F]
        }
        if(!is_blank(input$time_facet_r) && !is_blank(time_facet_rlevs$value)) {
            cond_r <- data[[input$time_facet_r]] %in% time_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        if(!is_blank(input$time_facet_c) && !is_blank(time_facet_clevs$value)) {
            cond_c <- data[[input$time_facet_c]] %in% time_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- sort(unique(data[[input$time_group]]))
        selected <- isolate(ternary(
            !is_blank(input$time_group_levs) &&
                all(input$time_group_levs %in% choices),
            input$time_group_levs, choices
        ))
        updateSelectizeInput(
            session, 'time_group_levs', input$time_group, choices, selected,
            options = list(plugins = list('drag_drop','remove_button'))
        )
        time_group_levs$value <- selected
    })
    
    time_group_levs <- reactiveValues(value = NULL)
    observe({
        if(is_blank(input$time_group)) {
            time_group_levs$value <- NULL
        } else {
            data <- subset_data()$data
            choices <- sort(unique(data[[input$time_group]]))
            time_group_levs$value <- choices
        }
    })
    observe({
        time_group_levs$value <- input$time_group_levs
    })
    
    # a drop-down select list for choosing facet row variable
    output$time_facet_r <- renderUI({
        req(file_import$status)
        data <- subset_data()$data
        names_disc <- c(names(data)[!unlist(lapply(data, continuity_test))])
        choices <- c('Choose' = '', names_disc)
        selected <- isolate(
            ifelse(is.null(input$time_facet_r), '', input$time_facet_r)
        )
        selectInput('time_facet_r', 'Facet row', choices, selected = selected)
    })
    observe({
        data <- subset_data()$data
        to_remove <- NULL
        if(!is_blank(input$time_study)) {
            data <- data[data[[study_col]] == input$time_study, , drop = F]
            to_remove <- c(to_remove, study_col)
        }
        if(!is_blank(input$time_cohort)) {
            data <- data[data[[cohort_col]] == input$time_cohort, , drop = F]
            to_remove <- c(to_remove, cohort_col)
        }
        if(!is_blank(input$time_bmk)) {
            #*data <- data[data[[param_col]] == input$time_bmk, , drop = F]
          data <- data[data[[param_col]] %in% input$time_bmk, , drop = F]
          #*to_remove <- c(to_remove, param_col)
        }
        if(!is_blank(input$time_group) && !is_blank(time_group_levs$value)) {
            cond_g <- data[[input$time_group]] %in% time_group_levs$value
            data <- data[cond_g, , drop = F]
            to_remove <- c(to_remove, input$time_group)
        }
        if(!is_blank(input$time_facet_c) && !is_blank(time_facet_clevs$value)) {
            cond_c <- data[[input$time_facet_c]] %in% time_facet_clevs$value
            data <- data[cond_c, , drop = F]
            to_remove <- c(to_remove, input$time_facet_c)
        }
        names_disc <- c(names(data)[!unlist(lapply(data, continuity_test))])
        names_disc <- setdiff(names_disc, to_remove)
        choices <- c('Choose' = '', names_disc)
        selected <- isolate(
            ifelse(is.null(input$time_facet_r), '', input$time_facet_r)
        )
        updateSelectInput(
            session, 'time_facet_r', 'Facet row', choices, selected
        )
    })
    
    # facet row variable levels
    output$time_facet_rlevs <- renderUI({
        req(input$time_facet_r)
        data <- subset_data()$data
        choices <- sort(unique(data[[input$time_facet_r]]))
        selected <- isolate(ternary(
            !is_blank(input$time_facet_rlevs) &&
                all(input$time_facet_rlevs %in% choices),
            input$time_facet_rlevs, choices
        ))
        selectizeInput(
            'time_facet_rlevs', input$time_facet_r, choices,
            selected = selected, multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(input$time_facet_r)
        data <- subset_data()$data
        if(!is_blank(input$time_study)) {
            data <- data[data[[study_col]] == input$time_study, , drop = F]
        }
        if(!is_blank(input$time_cohort)) {
            data <- data[data[[cohort_col]] == input$time_cohort, , drop = F]
        }
        if(!is_blank(input$time_bmk)) {
            #*data <- data[data[[param_col]] == input$time_bmk, , drop = F]
          data <- data[data[[param_col]] %in% input$time_bmk, , drop = F]
        }
        if(!is_blank(input$time_group) && !is_blank(time_group_levs$value)) {
            cond_g <- data[[input$time_group]] %in% time_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$time_facet_c) && !is_blank(time_facet_clevs$value)) {
            cond_c <- data[[input$time_facet_c]] %in% time_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- sort(unique(data[[input$time_facet_r]]))
        selected <- isolate(ternary(
            !is_blank(input$time_facet_rlevs) &&
                all(input$time_facet_rlevs %in% choices),
            input$time_facet_rlevs, choices
        ))
        updateSelectizeInput(
            session, 'time_facet_rlevs', input$time_facet_r, choices, selected,
            options = list(plugins = list('drag_drop','remove_button'))
        )
        time_facet_rlevs$value <- selected
    })
    
    time_facet_rlevs <- reactiveValues(value = NULL)
    observe({
        if(is_blank(input$time_facet_r)) {
            time_facet_rlevs$value <- NULL
        } else {
            data <- subset_data()$data
            choices <- sort(unique(data[[input$time_facet_r]]))
            time_facet_rlevs$value <- choices
        }
    })
    observe({
        time_facet_rlevs$value <- input$time_facet_rlevs
    })
    
    # a drop-down select list for choosing facet row variable
    output$time_facet_c <- renderUI({
        req(file_import$status)
        data <- subset_data()$data
        names_disc <- c(names(data)[!unlist(lapply(data, continuity_test))])
        choices <- c('Choose' = '', names_disc)
        selected <- isolate(
            ifelse(is.null(input$time_facet_c), '', input$time_facet_c)
        )
        selectInput('time_facet_c', 'Facet column', choices, selected)
    })
    observe({
        data <- subset_data()$data
        to_remove <- NULL
        if(!is_blank(input$time_study)) {
            data <- data[data[[study_col]] == input$time_study, , drop = F]
            to_remove <- c(to_remove, study_col)
        }
        if(!is_blank(input$time_cohort)) {
            data <- data[data[[cohort_col]] == input$time_cohort, , drop = F]
            to_remove <- c(to_remove, cohort_col)
        }
        if(!is_blank(input$time_bmk)) {
            #*data <- data[data[[param_col]] == input$time_bmk, , drop = F]
            data <- data[data[[param_col]] %in% input$time_bmk, , drop = F]
            #*to_remove <- c(to_remove, param_col)
        }
        if(!is_blank(input$time_group) && !is_blank(time_group_levs$value)) {
            cond_g <- data[[input$time_group]] %in% time_group_levs$value
            data <- data[cond_g, , drop = F]
            to_remove <- c(to_remove, input$time_group)
        }
        if(!is_blank(input$time_facet_r) && !is_blank(time_facet_rlevs$value)) {
            cond_r <- data[[input$time_facet_r]] %in% time_facet_rlevs$value
            data <- data[cond_r, , drop = F]
            to_remove <- c(to_remove, input$time_facet_r)
        }
        names_disc <- c(names(data)[!unlist(lapply(data, continuity_test))])
        names_disc <- setdiff(names_disc, to_remove)
        choices <- c('Choose' = '', names_disc)
        selected <- isolate(
            ifelse(is.null(input$time_facet_c), '', input$time_facet_c)
        )
        updateSelectInput(
            session, 'time_facet_c', 'Facet column', choices, selected
        )
    })
    
    # facet row variable levels
    output$time_facet_clevs <- renderUI({
        req(input$time_facet_c)
        data <- subset_data()$data
        choices <- sort(unique(data[[input$time_facet_c]]))
        selected <- isolate(ternary(
            !is_blank(input$time_facet_clevs) &&
                all(input$time_facet_clevs %in% choices),
            input$time_facet_clevs, choices
        ))
        selectizeInput(
            'time_facet_clevs', input$time_facet_c, choices,
            selected = selected, multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(input$time_facet_c)
        data <- subset_data()$data
        if(!is_blank(input$time_study)) {
            data <- data[data[[study_col]] == input$time_study, , drop = F]
        }
        if(!is_blank(input$time_cohort)) {
            data <- data[data[[cohort_col]] == input$time_cohort, , drop = F]
        }
        if(!is_blank(input$time_bmk)) {
            #*data <- data[data[[param_col]] == input$time_bmk, , drop = F]
          data <- data[data[[param_col]] %in% input$time_bmk, , drop = F]
        }
        if(!is_blank(input$time_group) && !is_blank(time_group_levs$value)) {
            cond_g <- data[[input$time_group]] %in% time_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$time_facet_r) && !is_blank(time_facet_rlevs$value)) {
            cond_r <- data[[input$time_facet_r]] %in% time_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        choices <- sort(unique(data[[input$time_facet_c]]))
        selected <- isolate(ternary(
            !is_blank(input$time_facet_clevs) &&
                all(input$time_facet_clevs %in% choices),
            input$time_facet_clevs, choices
        ))
        updateSelectizeInput(
            session, 'time_facet_clevs', input$time_facet_c, choices, selected,
            options = list(plugins = list('drag_drop','remove_button'))
        )
        time_facet_clevs$value <- selected
    })
    
    time_facet_clevs <- reactiveValues(value = NULL)
    observe({
        if(is_blank(input$time_facet_c)) {
            time_facet_clevs$value <- NULL
        } else {
            data <- subset_data()$data
            choices <- sort(unique(data[[input$time_facet_c]]))
            time_facet_clevs$value <- choices
        }
    })
    observe({
        time_facet_clevs$value <- input$time_facet_clevs
    })
    
    
    #---------------------------------------------
    # UI widegts for table refinement
    #---------------------------------------------
    
    # text area input for specifying table title
    output$time_table_title <- renderUI({
        req(input$time_bmk, input$time_y)
        req(req(input$time_graph_type) == 'Summary table')
        value <- 'Summary statistics for {Biomarker} ({YVariable})'
        #if(!is_blank(input$time_group)) value <- paste(value, 'by {Group}')
        ## Titles for 3D group selection;
        if(!is_blank(input$time_group) && !is_blank(input$time_facet_r) && !is_blank(input$time_facet_c))
          value <- paste(value, 'by', input$time_group, ', ',input$time_facet_r, ', ',input$time_facet_c)
        ## Titles for 2D group selection;        
        else if(!is_blank(input$time_group) && !is_blank(input$time_facet_r))
          value <- paste(value, 'by', input$time_group, ', ',input$time_facet_r)
        else if(!is_blank(input$time_group) && !is_blank(input$time_facet_c))
          value <- paste(value, 'by', input$time_group, ', ',input$time_facet_c)
        else if(!is_blank(input$time_facet_r) && !is_blank(input$time_facet_c))
          value <- paste(value, 'by', input$time_facet_r, ', ',input$time_facet_c)        
        ## Titles for 1D group selection;
        else if(!is_blank(input$time_group))
          value <- paste(value, 'by', input$time_group)
        else if(!is_blank(input$time_facet_r))
          value <- paste(value, 'by', input$time_facet_r)
        else if(!is_blank(input$time_facet_c))
          value <- paste(value, 'by', input$time_facet_c)   
        textareaInput('time_table_title', 'Table title', value = value)
    })
    time_table_title <- reactiveValues(value = NULL)
    observe({
        req(input$time_bmk, input$time_y)
        req(req(input$time_graph_type) == 'Summary table')
        value <- 'Summary statistics for {Biomarker} ({YVariable})'
        #if(!is_blank(input$time_group)) value <- paste(value, 'by {Group}')
        ## Titles for 3D group selection;
        if(!is_blank(input$time_group) && !is_blank(input$time_facet_r) && !is_blank(input$time_facet_c))
          value <- paste(value, 'by', input$time_group, ', ',input$time_facet_r, ', ',input$time_facet_c)
        ## Titles for 2D group selection;        
        else if(!is_blank(input$time_group) && !is_blank(input$time_facet_r))
          value <- paste(value, 'by', input$time_group, ', ',input$time_facet_r)
        else if(!is_blank(input$time_group) && !is_blank(input$time_facet_c))
          value <- paste(value, 'by', input$time_group, ', ',input$time_facet_c)
        else if(!is_blank(input$time_facet_r) && !is_blank(input$time_facet_c))
          value <- paste(value, 'by', input$time_facet_r, ', ',input$time_facet_c)        
        ## Titles for 1D group selection;
        else if(!is_blank(input$time_group))
          value <- paste(value, 'by', input$time_group)
        else if(!is_blank(input$time_facet_r))
          value <- paste(value, 'by', input$time_facet_r)
        else if(!is_blank(input$time_facet_c))
          value <- paste(value, 'by', input$time_facet_c)    
        time_table_title$value <- value
    })
    observe({
        time_table_title$value <- input$time_table_title
    })
    
    # text area input for specifying table footnote
    output$time_table_footnote <- renderUI({
        req(input$time_bmk, input$time_y)
        req(req(input$time_graph_type) == 'Summary table')
        textareaInput('time_table_footnote', 'Table footnote', value = '')
    })
    time_table_footnote <- reactiveValues(value = '')
    observe({
        time_table_footnote$value <- input$time_table_footnote
    })
    
    # a numericInput for specifying decimal places
    output$time_decimal <- renderUI({
        req(input$time_bmk, input$time_y)
        req(req(input$time_graph_type) == 'Summary table')
        stillSelected <- isolate(
            ifelse(is.null(input$time_decimal), 1, input$time_decimal)
        )
        numericInput('time_decimal', 'Decimal places', value = stillSelected)
    })
    time_decimal <- reactiveValues(value = 1)
    observe({
        time_decimal$value <- input$time_decimal
    })
    
    # a textInput for specifying top n column totals to add in the table
    output$time_col_totals_n <- renderUI({
        req(input$time_bmk, input$time_y, input$time_group)
        req(req(input$time_graph_type) == 'Summary table')
        textInput('time_col_totals_n', 'Column totals', value = '')
    })
    time_col_totals_n <- reactiveValues(value = NULL)
    observe({
        input$time_col_totals_n
        time_col_totals_n$value <- as.numeric(trimws(unlist(strsplit(as.character(
            input$time_col_totals_n
        ), ','))))
    })
    
    # a textInput for specifying top n column total names
    output$time_col_totals_name <- renderUI({
        req(input$time_bmk, input$time_y, input$time_group)
        req(req(input$time_graph_type) == 'Summary table')
        textareaInput('time_col_totals_name', 'Column total headers', value = '')
    })
    time_col_totals_name <- reactiveValues(value = NULL)
    observe({
      input$time_col_totals_name
        time_col_totals_name$value <- trimws(unlist(strsplit(as.character(
            input$time_col_totals_name
        ), '\n')))
    })
    
    # a checkbox for whether to add change from baseline block
    output$time_add_cfb <- renderUI({
        req(input$time_bmk, input$time_y)
        req(req(input$time_graph_type) == 'Summary table')
        checkboxInput('time_add_cfb', 'Add change from baseline', value = FALSE)
    })
    time_add_cfb <- reactiveValues(value = FALSE)
    observe({
      input$time_add_cfb
        time_add_cfb$value <- input$time_add_cfb
    })
    
    
    #---------------------------------------------
    # UI widgets for plot refinement
    #---------------------------------------------
    
    # a selectInput for choosing x tick
    output$time_xticker <- renderUI(({
        req(input$time_bmk, input$time_y, input$time_graph_type)
        data <- time_data_input()
        all_columns <- names(data)
        nuniques <- unlist(lapply(lapply(data, unique), length))
        xtick_idx <- nuniques == length(unique(data[[xvar_col]]))
        choices <- union(c(xvar_col, xlabel_col), all_columns[xtick_idx])
        if(!is_blank(input$time_xticker) && input$time_xticker %in% choices)
            selected <- input$time_xticker
        else selected <- xvar_col
        selectInput('time_xticker', 'X tick', choices, selected)
    }))
    
    # a text input for modifying plot title
    output$time_plot_title <- renderUI({
        req(input$time_bmk, input$time_y)
        req(req(input$time_graph_type) != 'Summary table')
        value <- trimws(paste0(
            '{Biomarker}: ', input$time_graph_type,
            ifelse(is_blank(input$time_group), '', paste0(' by {Group}')),
            ifelse(input$time_to_log, ', semi-log', '')
        ))
        if(!is_blank(input$time_study))
            value <- paste('Study: {Study}', value, sep = ' ')
        if(!is_blank(input$time_cohort))
            value <- paste(value, '{Cohort} cohort', sep = ' ')
        #value <- trimws(value)
        value <- "Plot Title"
        textareaInput('time_plot_title', 'Plot title', value = value)
    })
    time_plot_title <- reactiveValues(value = '')
    observe({
        req(input$time_bmk, input$time_y)
        req(req(input$time_graph_type) != 'Summary table')
        value <- trimws(paste0(
            '{Biomarker}: ', input$time_graph_type,
            ifelse(is_blank(input$time_group), '', paste0(' by {Group}')),
            ifelse(input$time_to_log, ', semi-log', '')
        ))
        if(!is_blank(input$time_study))
            value <- paste('Study: {Study}', value, sep = '\n')
        if(!is_blank(input$time_cohort))
            value <- paste(value, '{Cohort} cohort', sep = '\n')
        #value <- trimws(value)
        value <- "Plot Title"
        time_plot_title$value <- value
    })
    observe({
        time_plot_title$value <- trimws(input$time_plot_title)
    })
    
    # a text input for modify x-axis label
    output$time_plot_xlab <- renderUI({
        req(input$time_bmk, input$time_y)
        req(req(input$time_graph_type) != 'Summary table')
        value <- 'Time on treatment'
        textInput('time_plot_xlab', 'X-axis label', value)
    })
    time_plot_xlab <- reactiveValues(value = 'Time on treatment')
    observe({
        time_plot_xlab$value <- trimws(input$time_plot_xlab)
    })
    
    # a text input for modify y-axis label
    output$time_plot_ylab <- renderUI({
        req(input$time_bmk, input$time_y)
        req(req(input$time_graph_type) != 'Summary table')
        value <- '{Biomarker} ({YVariable})'
        textInput('time_plot_ylab', 'Y-axis label', value)
    })
    time_plot_ylab <- reactiveValues(value = NULL)
    observe({
        req(input$time_bmk, input$time_y)
        req(req(input$time_graph_type) != 'Summary table')
        value <- '{Biomarker} ({YVariable})'
        time_plot_ylab$value <- value
    })
    observe({
        time_plot_ylab$value <- trimws(input$time_plot_ylab)
    })
    
    # a text input for modifying plot footnote
    output$time_plot_footnote <- renderUI({
        req(input$time_bmk, input$time_y)
        req(req(input$time_graph_type) != 'Summary table')
        value <- ''
        textareaInput('time_plot_footnote', 'Plot footnote', value = value)
    })
    time_plot_footnote <- reactiveValues(value = '')
    observe({
        time_plot_footnote$value <- trimws(input$time_plot_footnote)
    })
    
    # a numeric input for adding a reference line
    output$time_reference_line <- renderUI({
        req(input$time_bmk, input$time_y)
        req(req(input$time_graph_type) != 'Summary table')
        textInput('time_reference_line', 'Add a reference line', value = '')
    })
    
    # a sliderbar for adjusting x-axis tick label orientation angle
    output$time_plot_xtick_angle <- renderUI({
        req(input$time_bmk, input$time_y)
        req(req(input$time_graph_type) != 'Summary table')
        sliderInput(
            'time_plot_xtick_angle', 'X tick angle', 0, 360, 0, step = 15
        )
    })
    
    # a log-scale check box
    output$time_to_log <- renderUI({
        req(input$time_bmk, input$time_y, input$time_graph_type)
        value <- isolate(
            ifelse(is.null(input$time_to_log), FALSE, input$time_to_log)
        )
        checkboxInput('time_to_log', label = 'Log of Y', value = value)
    })
    time_to_log <- reactiveValues(value = FALSE)
    observe({
        time_to_log$value <- input$time_to_log
    })
    
    # a checkbox to toggle sample size denotation
    output$time_toggle_sample_size <- renderUI({
        req(input$time_bmk, input$time_y)
        req(req(input$time_graph_type) != 'Summary table')
        value <- isolate(
            ifelse(is.null(input$time_toggle_sample_size),
                   TRUE, input$time_toggle_sample_size)
        )
        checkboxInput('time_toggle_sample_size', 'Add sample size', value = value)
    })
    time_toggle_sample_size <- reactiveValues(value = TRUE)
    observe({
        time_toggle_sample_size$value <- input$time_toggle_sample_size
    })
    
    # a checkbox to toggle points
    output$time_toggle_points <- renderUI({
        req(input$time_bmk, input$time_y)
        types <- c('Boxplot', 'Mean + SE plot', 'Mean + SD plot',
                   'Median + IQR plot', 'Spaghetti plot')
        req(req(input$time_graph_type) %in% types)
        value <- isolate(
            ifelse(is.null(input$time_toggle_points),
                   FALSE, input$time_toggle_points)
        )
        checkboxInput('time_toggle_points', 'Add points', value = value)
    })
    time_toggle_points <- reactiveValues(value = FALSE)
    observe({
        time_toggle_points$value <- input$time_toggle_points
    })
    
    # a checkbox to toggle subject ID
    output$time_toggle_subjid <- renderUI({
        req(input$time_bmk, input$time_y)
        req(req(input$time_graph_type) %in% c('Spaghetti plot'))
        value <- isolate(
            ifelse(is.null(input$time_toggle_subjid),
                   FALSE, input$time_toggle_subjid)
        )
        checkboxInput('time_toggle_subjid', 'Show subject ID', value = value)
    })
    time_toggle_subjid <- reactiveValues(value = FALSE)
    observe({
        time_toggle_subjid$value <- input$time_toggle_subjid
    })
    
    # an action button to open a modal window to specify line attributes
    output$time_graph_attributes <- renderUI({
        req(input$time_bmk, input$time_y)
        output_types <- c('Boxplot', 'Mean + SE plot', 'Mean + SD plot',
                          'Median + IQR plot', 'Spaghetti plot')
        req(req(input$time_graph_type) %in% output_types)
        actionButton('time_graph_attributes', 'Graph attributes')
    })
    output$time_line_color_ui <- renderUI({
        if(is_blank(input$time_group) || length(time_group_levs$value) <= 1) {
            colourpicker::colourInput(
                'time_line_color', 'Color', value = col2hex(gg_color_hue(1))
            )
        } else {
            col_hex <- col2hex(gg_color_hue(length(time_group_levs$value)))
            tagList(lapply(
                seq_along(time_group_levs$value),
                function(idx) {
                    colourpicker::colourInput(
                        paste0('time_line_color', idx),
                        paste0('Color for "', time_group_levs$value[idx], '"'),
                        value = col_hex[idx], showColour = 'both',
                        palette = 'square'
                    )
                }
            ))
        }
    })
    time_line_color <- reactiveValues(value = col2hex(gg_color_hue(1)))
    observe({
        if(is_blank(input$time_group) || length(time_group_levs$value) <= 1) {
            if(!is.null(input$time_line_color)) {
                time_line_color$value <- input$time_line_color
            } else time_line_color$value <- col2hex(gg_color_hue(1))
        } else {
            color_input <- paste0(
                'time_line_color', seq_along(time_group_levs$value)
            )
            color_values <- c()
            for(ele in color_input) {
                color_values <- c(color_values, input[[ele]])
            }
            if(!is.null(color_values)) {
                time_line_color$value <- color_values
            } else {
                time_line_color$value <- col2hex(gg_color_hue(
                    length(time_group_levs$value)
                ))
            }
        }
    })
    output$time_line_type_ui <- renderUI({
        output_types <- c('Mean + SE plot', 'Mean + SD plot',
                          'Median + IQR plot', 'Spaghetti plot')
        req(req(input$time_graph_type) %in% output_types)
        group_levels <- time_group_levs$value
        choices <- c('solid', 'dashed', 'dotted', 'dotdash','longdash','twodash')
        if(is.null(group_levels)) {
            selectInput('time_line_type', 'Line type', choices)
        } else {
            tagList(lapply(
                seq_along(group_levels),
                function(idx) {
                    selectInput(
                        paste0('time_line_type', idx),
                        paste0('Line type for "', time_group_levs$value[idx], '"'),
                        choices
                    )
                }
            ))
        }
    })
    time_line_type <- reactiveValues(value = 'solid')
    observe({
        group_select <- time_group_levs$value
        if(!is.null(group_select)) {
            type_input <- paste0(
                'time_line_type', seq_along(group_select)
            )
            type_values <- c()
            for(ele in type_input) {
                type_values <- c(type_values, input[[ele]])
            }
            if(!is.null(type_values)) {
                time_line_type$value <- type_values
            } else {
                time_line_type$value <- rep('solid', length(group_select))
            }
        } else {
            if(!is.null(input$time_line_type)) {
                time_line_type$value <- input$time_line_type
            }
        }
    })
    
    # color convert to hex code
    observeEvent(input$hex_modal_button, {
        hex_value <- tryCatch(
            col2hex(input$hex_modal_color),
            error = function(e) 'Invalid'
        )
        message <- ifelse(hex_value == 'Invalid', 'Invalid color name!',
                          hex_value)
        output$hex_modal_hex <- renderText({message})
    })
    
    #---------------------------------------------
    # Setting plot x-/y-axis limits
    #---------------------------------------------
    
    # x-/y-axis limit of the plotting data
    data_limits <- reactiveValues(x = NULL, y = NULL)
    observe({
        req(input$time_bmk, input$time_y, input$time_graph_type)
        data <- time_data_input()
        xlimits <- range_na(data[[xvar_col]])
        xlimits <- round(xlimits, digits = max(sapply(xlimits, decimalplaces)))
        data_limits$x <- xlimits
        data_limits$y <- range_na(data[[input$time_y]])
    })
    
    # an action button to clear brush selection
    output$time_unbrush_button <- renderUI({
        req((!is.null(time_brush_range$xlim)) ||
                (!is.null(time_brush_range$ylim)))
        actionButton('time_unbrush_button', 'Reset graph', icon = icon('undo'))
    })
    
    # a time slider bar that automatically adjusts to the data of selection
    output$time_xrange <- renderUI({
        req(input$time_bmk, input$time_y, input$time_graph_type)
        xlimits <- data_limits$x
        xlimits <- round(xlimits, digits = max(sapply(xlimits, decimalplaces)))
        selected <- isolate(ternary(
            is.null(time_brush_range$xlim),
            xlimits, intersection(xlimits, time_brush_range$xlim)
        ))
        sliderInput('time_xrange', 'X range', min = xlimits[1],
                    max = xlimits[2], value = selected)
    })
    time_xrange <- reactiveValues(value = NULL)
    observe({
        req(input$time_bmk, input$time_y, input$time_graph_type)
        xlimits <- data_limits$x
        xlimits <- round(xlimits, digits = max(sapply(xlimits, decimalplaces)))
        selected <- isolate(ternary(
            is.null(time_brush_range$xlim),
            xlimits, intersection(xlimits, time_brush_range$xlim)
        ))
        updateSliderInput(session, 'time_xrange', 'X range', value = selected,
                          min = xlimits[1], max = xlimits[2])
        time_xrange$value <- selected
    })
    observe({
        time_xrange$value <- input$time_xrange
    })
    
    # time plot brush
    time_brush_range <- reactiveValues(xlim = NULL, ylim = NULL)
    
    # dynamically detect brush location
    observe({
        brush <- input$time_plot_brush
        if(!is.null(brush)) {
            time_brush_range$xlim <- c(brush$xmin, brush$xmax)
            time_brush_range$ylim <- c(brush$ymin, brush$ymax)
        }
    })
    
    # dynamically update brush selection
    observe({
        if(all(time_xrange$value == data_limits$x)) {
            time_brush_range$xlim <- NULL
        } else {
            time_brush_range$xlim <- time_xrange$value
        }
    })
    
    # dynamically deteck double click and set the time-profiling
    # graph back to its original scale when brushed
    observeEvent(input$time_plot_dblclick, {
        time_brush_range$xlim <- NULL
        time_brush_range$ylim <- NULL
    })
    
    # clear brush selection when the 'clear' button is pressed
    observeEvent(req(input$time_unbrush_button), {
        time_brush_range$xlim <- NULL
        time_brush_range$ylim <- NULL
    })
    
    # an action button to add the current graph setting to TNF
    output$time_add_to_tnf <- renderUI({
        req(input$time_bmk, input$time_y, input$time_graph_type)
        actionButton('time_add_to_tnf', 'Add to output specs',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$time_add_to_tnf, {
        tnf$time_count <- tnf$time_count + 1
        
        file_key <- paste0('g-time', tnf$time_count)
        tfl_type <- ifelse(input$time_graph_type == 'Summary table', 'Table',
                           'Figure')
        group_levs <- paste(time_group_levs$value, collapse = '\n')
        facet_r_levs <- paste(time_facet_rlevs$value, collapse = '\n')
        facet_c_levs <- paste(time_facet_clevs$value, collapse = '\n')
        Xmin <- input$time_xrange[1]
        Xmax <- input$time_xrange[2]
        table_col_totals_n <- paste(time_col_totals_n$value, collapse = '\n')
        table_col_totals_name <- paste(time_col_totals_name$value, collapse = '\n')
        reflines <- trimws(gsub(',', '\n', gsub(' ', '', input$time_reference_line)))
        line_cols <- paste(time_line_color$value, collapse = '\n')
        line_types <- paste(time_line_type$value, collapse = '\n')
        
        row_list <- setNames(list(
            file_key, tfl_type, tnf$time_count, input$time_graph_type,
            input$time_study, input$time_cohort, input$time_bmk, input$time_y,
            input$time_to_log, input$time_group,
            group_levs, input$time_facet_r, facet_r_levs, input$time_facet_c,
            facet_c_levs, Xmin, Xmax, NULL, NULL,
            input$time_download_plot_format, input$time_download_plot_height,
            input$time_download_plot_width, input$time_download_plot_resolution,
            input$time_xticker, time_table_title$value, time_table_footnote$value,
            time_decimal$value, time_col_totals_n$value, time_col_totals_name$value,
            time_add_cfb$value, time_plot_title$value, time_plot_xlab$value,
            time_plot_ylab$value, time_plot_footnote$value,
            input$time_plot_xtick_angle, reflines, time_toggle_sample_size$value,
            time_toggle_points$value, time_toggle_subjid$value,
            line_cols, line_types, subset_data()$subset_expr
        ), out_time_cols)
        
        row_df <- data.frame(
            lapply(row_list, function(x) replace(x, is_length_zero(x), '')),
            check.names = FALSE, stringsAsFactors = FALSE
        )
        tnf$time_df <- rbind(tnf$time_df, row_df)
    })
    
    observe({
        file_key <- paste0('g-time', tnf$time_count)
        tfl_type <- ifelse(input$time_graph_type == 'Summary table', 'Table',
                           'Figure')
        group_levs <- paste(time_group_levs$value, collapse = '\n')
        facet_r_levs <- paste(time_facet_rlevs$value, collapse = '\n')
        facet_c_levs <- paste(time_facet_clevs$value, collapse = '\n')
        Xmin <- input$time_xrange[1]
        Xmax <- input$time_xrange[2]
        table_col_totals_n <- paste(time_col_totals_n$value, collapse = '\n')
        table_col_totals_name <- paste(time_col_totals_name$value, collapse = '\n')
        reflines <- trimws(gsub(',', '\n', gsub(' ', '', input$time_reference_line)))
        line_cols <- paste(time_line_color$value, collapse = '\n')
        line_types <- paste(time_line_type$value, collapse = '\n')
        
        row_list <- setNames(list(
            file_key, tfl_type, tnf$time_count, input$time_study,
            input$time_cohort, input$time_bmk, input$time_y,
            input$time_to_log, input$time_graph_type, input$time_group,
            group_levs, input$time_facet_r, facet_r_levs, input$time_facet_c,
            facet_c_levs, Xmin, Xmax, NULL, NULL,
            input$time_download_plot_format, input$time_download_plot_height,
            input$time_download_plot_width, input$time_download_plot_resolution,
            input$time_xticker, time_table_title$value, time_table_footnote$value,
            time_decimal$value, time_col_totals_n$value, time_col_totals_name$value,
            time_add_cfb$value, time_plot_title$value, time_plot_xlab$value,
            time_plot_ylab$value, time_plot_footnote$value,
            input$time_plot_xtick_angle, reflines, time_toggle_sample_size$value,
            time_toggle_points$value, time_toggle_subjid$value,
            line_cols, line_types, subset_data()$subset_expr
        ), out_time_cols)
        
        row_df <- data.frame(
            lapply(row_list, function(x) replace(x, is_length_zero(x), '')),
            check.names = FALSE, stringsAsFactors = FALSE
        )
        
        tnf$time_current <- row_df
    })

    
    #---------------------------------------------
    # Plot/Table output
    #---------------------------------------------
    
    # modify the data according to the biomarker & y variable selection
    time_data_input <- reactive({
        #req(input$time_bmk, input$time_y, input$time_graph_type)
        data <- req(subset_data()$data)
        #data <- arrange_(data, subj_col, xvar_col)
        
        if(!is_blank(input$time_study)) {
            data <- data[data[[study_col]] == input$time_study, , drop = F]
        }
        if(!is_blank(input$time_cohort)) {
            data <- data[data[[cohort_col]] == input$time_cohort, , drop = F]
        }
        if(!is_blank(input$time_bmk)) {
            #*data <- data[data[[param_col]] == input$time_bmk, , drop = F]
          data <- data[data[[param_col]] %in% input$time_bmk, , drop = F]
        }

        if(!is.null(time_group_levs$value)) {
          data <- data[
            data[[input$time_group]] %in% time_group_levs$value, , drop = F
            ]
          data[, input$time_group] <- factor(
            data[, input$time_group], levels = time_group_levs$value
          )
        }
        if(!is.null(time_facet_rlevs$value)) {
          data <- data[
            data[[input$time_facet_r]] %in% time_facet_rlevs$value,
            , drop = F
            ]
          data[, input$time_facet_r] <- factor(
            data[, input$time_facet_r],
            levels = time_facet_rlevs$value
          )
        }
        if(!is.null(time_facet_clevs$value)) {
          data <- data[
            data[[input$time_facet_c]] %in% time_facet_clevs$value,
            , drop = F
            ]
          data[, input$time_facet_c] <- factor(
            data[, input$time_facet_c],
            levels = time_facet_clevs$value
          )
        }

        data[[xlabel_col]] <- factor(
            data[[xlabel_col]],
            levels = unique(data[[xlabel_col]][order(data[[xvar_col]])])
        )
        

        return(data)
    })
    
    
    # modify the data according to the biomarker & y variable selection
    time_m_data_input <- reactive({
      #req(input$time_bmk, input$time_y, input$time_graph_type)
      data <- req(subset_data()$data)
      #data <- arrange_(data, subj_col, xvar_col)
      
      if(!is_blank(input$time_study)) {
        data <- data[data[[study_col]] == input$time_study, , drop = F]
      }
      if(!is_blank(input$time_cohort)) {
        data <- data[data[[cohort_col]] == input$time_cohort, , drop = F]
      }
      if(!is_blank(input$time_download_multibmk)) {
        #*data <- data[data[[param_col]] == input$time_bmk, , drop = F]
        data <- data[data[[param_col]] %in% input$time_download_multibmk, , drop = F]
      }
      
      if(!is.null(time_group_levs$value)) {
        data <- data[
          data[[input$time_group]] %in% time_group_levs$value, , drop = F
          ]
        data[, input$time_group] <- factor(
          data[, input$time_group], levels = time_group_levs$value
        )
      }
      if(!is.null(time_facet_rlevs$value)) {
        data <- data[
          data[[input$time_facet_r]] %in% time_facet_rlevs$value,
          , drop = F
          ]
        data[, input$time_facet_r] <- factor(
          data[, input$time_facet_r],
          levels = time_facet_rlevs$value
        )
      }
      if(!is.null(time_facet_clevs$value)) {
        data <- data[
          data[[input$time_facet_c]] %in% time_facet_clevs$value,
          , drop = F
          ]
        data[, input$time_facet_c] <- factor(
          data[, input$time_facet_c],
          levels = time_facet_clevs$value
        )
      }
      
      data[[xlabel_col]] <- factor(
        data[[xlabel_col]],
        levels = unique(data[[xlabel_col]][order(data[[xvar_col]])])
      )
      
      
      return(data)
    })    
    
    
    # produce the time profiling summary table
    time_table <- reactive({
        req(time_table_show$value)
        req(!is.null(input$time_xticker),
            !is.null(time_decimal$value),
            !is.null(time_table_title$value),
            !is.null(time_table_footnote$value),
            !is.null(time_add_cfb$value))
        if(!is_blank(input$time_group)) {
            req(!is.null(time_col_totals_n$value),
                !is.null(time_col_totals_name$value))
        }
        data <- time_data_input()
        xlim <- time_brush_range$xlim
        if(!is.null(xlim)) {
            cond <- data[[xvar_col]] >= xlim[1] & data[[xvar_col]] <= xlim[2]
            data <- data[cond, ,  drop = FALSE]
        }
        data[, input$time_xticker] <- factor(
            data[, input$time_xticker],
            levels = unique(data[[input$time_xticker]][order(data[[xvar_col]])])
        )
        dgt <- time_decimal$value
        summary_func <- c(
            'N' = n_nna,
            'Mean (SD)' = partial(mean_sd_str, digits = dgt),
            '%CV' = partial(coeff_var_str, digits = dgt),
            'Median' = partial(median_str, digits = dgt),
            'Q1, Q3' = partial(q1_q3_str, digits = dgt),
            'Min, Max' = partial(min_max_str, digits = dgt)
        )
        if(isTRUE(input$time_to_log)) {
            summary_func <- c(
                summary_func,
                'Geom Mean (%CV)' = partial(geo_mean_cv_str, digits = dgt),
                'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
            )
        }
        
        caption <- r_format(time_table_title$value, tnf$time_current)
        footnote <- r_format(time_table_footnote$value, tnf$time_current)
        name_totals <- r_format(time_col_totals_name$value, tnf$time_current)
        
        # Summary table without grouping;
        if(input$time_graph_type == 'Summary table' && is_blank(input$time_group) && is_blank(input$time_facet_r) && is_blank(input$time_facet_c)){
          summary_tbl <- summary_table_all(
            data, row_var = input$time_xticker,
            col_var = input$time_group, val_var = input$time_y,
            col_totals = time_col_totals_n$value,
            name_totals = time_col_totals_name$value,
            n_in_header = TRUE, subj_col = subj_col,
            baseline_name = NULL,
            add_cfb = time_add_cfb$value, cfb_var = chg_col,
            func_list = summary_func,
            caption = time_table_title$value,
            footnote = time_table_footnote$value,
            rowlabel = '', format = 'html'
          )
        }
        # Summary table with grouping by input$time_group;
        else if(input$time_graph_type == 'Summary table' && !is_blank(input$time_group) && is_blank(input$time_facet_r) && is_blank(input$time_facet_c)){
          summary_tbl <- summary_table_all(
            data, row_var = input$time_xticker,
            col_var = input$time_group, val_var = input$time_y,
            col_totals = time_col_totals_n$value,
            name_totals = time_col_totals_name$value,
            n_in_header = TRUE, subj_col = subj_col,
            baseline_name = NULL,
            add_cfb = time_add_cfb$value, cfb_var = chg_col,
            func_list = summary_func,
            caption = time_table_title$value,
            footnote = time_table_footnote$value,
            rowlabel = '', format = 'html'
          )
        }
        # Summary table with grouping by input$time_row;
        else if(input$time_graph_type == 'Summary table' && is_blank(input$time_group) && !is_blank(input$time_facet_r) && is_blank(input$time_facet_c)){
          summary_tbl <- summary_table_all(
            data, row_var = input$time_xticker,
            col_var = input$time_facet_r, val_var = input$time_y,
            col_totals = time_col_totals_n$value,
            name_totals = time_col_totals_name$value,
            n_in_header = TRUE, subj_col = subj_col,
            baseline_name = NULL,
            add_cfb = time_add_cfb$value, cfb_var = chg_col,
            func_list = summary_func,
            caption = time_table_title$value,
            footnote = time_table_footnote$value,
            rowlabel = '', format = 'html'
          )
        }
        # Summary table with grouping by input$time_column;
        else if(input$time_graph_type == 'Summary table' && is_blank(input$time_group) && is_blank(input$time_facet_r) && !is_blank(input$time_facet_c)){
          summary_tbl <- summary_table_all(
            data, row_var = input$time_xticker,
            col_var = input$time_facet_c, val_var = input$time_y,
            col_totals = time_col_totals_n$value,
            name_totals = time_col_totals_name$value,
            n_in_header = TRUE, subj_col = subj_col,
            baseline_name = NULL,
            add_cfb = time_add_cfb$value, cfb_var = chg_col,
            func_list = summary_func,
            caption = time_table_title$value,
            footnote = time_table_footnote$value,
            rowlabel = '', format = 'html'
          )
        }
        
        # Summary table with grouping by input$time_group and input$time_facet_r;
        else if(input$time_graph_type == 'Summary table' && !is_blank(input$time_group) && !is_blank(input$time_facet_r) && is_blank(input$time_facet_c)){
          # subset data into different dataframe by input$time_facet_r
          
          unique_fr <- c(unique(time_facet_rlevs$value))
          summary_tbl=NULL
          d = NULL
          for (i in unique_fr) {
            #tmp_data <- data[[input$time_facet_c]] == unique_fc[i]
            #lab_tmp = paste(c(i,time_table_title$value))
            tmp_data <- data[
              data[[input$time_facet_r]] == i,
              , drop = F
              ]  
            d <- summary_table_all(
              tmp_data, row_var = input$time_xticker,
              col_var = input$time_group, val_var = input$time_y,
              col_totals = time_col_totals_n$value,
              name_totals = time_col_totals_name$value,
              n_in_header = TRUE, subj_col = subj_col,
              baseline_name = NULL,
              add_cfb = time_add_cfb$value, cfb_var = chg_col,
              func_list = summary_func,
              caption = time_table_title$value,
              footnote = time_table_footnote$value,
              rowlabel = '', format = 'html'
            )
            summary_tbl = rbind(summary_tbl,i,d)
            #summary_tbl = rbind(summary_tbl,d)
          }
        }
        
        # Summary table with grouping by input$time_group and input$time_facet_c;
        else if(input$time_graph_type == 'Summary table' && !is_blank(input$time_group) && is_blank(input$time_facet_r) && !is_blank(input$time_facet_c)){
          # subset data into different dataframe by input$time_facet_r
          #facet_row_levls <- unique_na(data[[input$time_facet_r]])
          unique_fc <- c(unique(time_facet_clevs$value))
          #facet_col_dim = unique_fc
          summary_tbl=NULL
          d = NULL
          for (i in unique_fc) {
            #tmp_data <- data[[input$time_facet_c]] == unique_fc[i]
            
            tmp_data <- data[
              data[[input$time_facet_c]] == i,
              , drop = F
              ]  
            d <- summary_table_all(
              tmp_data, row_var = input$time_xticker,
              col_var = input$time_group, val_var = input$time_y,
              col_totals = time_col_totals_n$value,
              name_totals = time_col_totals_name$value,
              n_in_header = TRUE, subj_col = subj_col,
              baseline_name = NULL,
              add_cfb = time_add_cfb$value, cfb_var = chg_col,
              func_list = summary_func,
              caption = time_table_title$value,
              footnote = time_table_footnote$value,
              rowlabel = '', format = 'html'
            )
            summary_tbl = rbind(summary_tbl,i,d)
            #summary_tbl = rbind(summary_tbl,d)
          }
        }
        
        # Summary table with grouping by input$time_facet_c and input$time_facet_r;
        else if(input$time_graph_type == 'Summary table' && is_blank(input$time_group) && !is_blank(input$time_facet_r) && !is_blank(input$time_facet_c)){
          # subset data into different dataframe by input$time_facet_r
          unique_fr <- c(unique(time_facet_rlevs$value))
          summary_tbl=NULL
          d = NULL
          for (i in unique_fr) {
            #tmp_data <- data[[input$time_facet_c]] == unique_fc[i]
            tmp_data <- data[
              data[[input$time_facet_r]] == i,
              , drop = F
              ]  
            d <- summary_table_all(
              tmp_data, row_var = input$time_xticker,
              col_var = input$time_facet_c, val_var = input$time_y,
              col_totals = time_col_totals_n$value,
              name_totals = time_col_totals_name$value,
              n_in_header = TRUE, subj_col = subj_col,
              baseline_name = NULL,
              add_cfb = time_add_cfb$value, cfb_var = chg_col,
              func_list = summary_func,
              caption = time_table_title$value,
              footnote = time_table_footnote$value,
              rowlabel = '', format = 'html'
            )
            summary_tbl = rbind(summary_tbl,i,d)
            #summary_tbl = rbind(summary_tbl,d)
          }
        }
        # Summary table with grouping by input$time_group and input$time_facet_c and input$time_facet_r;
        else if(input$time_graph_type == 'Summary table' && !is_blank(input$time_group) && !is_blank(input$time_facet_r) && !is_blank(input$time_facet_c)){
          # subset data into different dataframe by input$time_facet_r
          unique_fr <- c(unique(time_facet_rlevs$value))
          unique_fc <- c(unique(time_facet_clevs$value))
          summary_tbl=NULL
          d = NULL
          for (i in unique_fr) {
            
            summary_tbl_c = NULL
            tmp_data_r <- data[
              data[[input$time_facet_r]] == i,
              , drop = F
              ] 
            for (j in unique_fc) {
              d_c = NULL
              tmp_data_c <- tmp_data_r[
                tmp_data_r[[input$time_facet_c]] == j,
                , drop = F
                ]                
              d_c <- summary_table_all(
                tmp_data_c, row_var = input$time_xticker,
                col_var = input$time_group, val_var = input$time_y,
                col_totals = time_col_totals_n$value,
                name_totals = time_col_totals_name$value,
                n_in_header = TRUE, subj_col = subj_col,
                baseline_name = NULL,
                add_cfb = time_add_cfb$value, cfb_var = chg_col,
                func_list = summary_func,
                caption = time_table_title$value,
                footnote = time_table_footnote$value,
                rowlabel = '', format = 'html'
              )
              ij <- c(",")
              summary_tbl_c = rbind(summary_tbl_c,i,ij,j, d_c)
            }
            
            summary_tbl = rbind(summary_tbl,summary_tbl_c)
          }
        }
        return(summary_tbl)
    })
    time_table_show <- reactiveValues(value = FALSE)
    observe({
        if(any(is_blank(input$time_bmk), is_blank(input$time_y),
               is_blank(input$time_graph_type)))
            time_table_show$value <- FALSE
        else {
            if(any(input$time_graph_type != 'Summary table',
                   is.null(input$time_xticker),
                   is.null(time_decimal$value),
                   is.null(time_table_title$value),
                   is.null(time_table_footnote$value)))
                time_table_show$value <- FALSE
            else time_table_show$value <- TRUE
        }
    })
    output$time_table <- renderUI({
        req(time_table_show$value)
        HTML(time_table())
    })

    #####################################################################
    # produce the time profiling summary table with the graph (GH)
    #####################################################################
    time_summary <- reactive({
      req(time_plot_show$value)
      req(!is.null(input$time_xticker))
      if(!is_blank(input$time_group)) {
        req(!is.null(time_col_totals_n$value),
            !is.null(time_col_totals_name$value))
      }
      data <- time_data_input()
      xlim <- time_brush_range$xlim
      if(!is.null(xlim)) {
        cond <- data[[xvar_col]] >= xlim[1] & data[[xvar_col]] <= xlim[2]
        data <- data[cond, ,  drop = FALSE]
      }
      data[, input$time_xticker] <- factor(
        data[, input$time_xticker],
        levels = unique(data[[input$time_xticker]][order(data[[xvar_col]])])
      )
      dgt <- 2
      summary_func <- c(
        'N' = n_nna,
        'Mean (SD)' = partial(mean_sd_str, digits = dgt),
        '%CV' = partial(coeff_var_str, digits = dgt),
        'Median' = partial(median_str, digits = dgt),
        'Q1, Q3' = partial(q1_q3_str, digits = dgt),
        'Min, Max' = partial(min_max_str, digits = dgt)
      )
      if(isTRUE(input$time_to_log)) {
        summary_func <- c(
          summary_func,
          'Geom Mean (%CV)' = partial(geo_mean_cv_str, digits = dgt),
          'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
        )
      }
      
      # Summary table without grouping;
      if(input$time_graph_type != 'Summary table' && is_blank(input$time_group) && is_blank(input$time_facet_r) && is_blank(input$time_facet_c)){
        summary_tbl <- summary_table_all(
          data, row_var = input$time_xticker,
          col_var = input$time_group, val_var = input$time_y,
          col_totals = time_col_totals_n$value,
          name_totals = time_col_totals_name$value,
          n_in_header = TRUE, subj_col = subj_col,
          baseline_name = NULL,
          add_cfb = time_add_cfb$value, cfb_var = chg_col,
          func_list = summary_func,
          caption = '',
          footnote = '',
          rowlabel = '', format = 'html'
        )
      }
      # Summary table with grouping by input$time_group;
      else if(input$time_graph_type != 'Summary table' && !is_blank(input$time_group) && is_blank(input$time_facet_r) && is_blank(input$time_facet_c)){
        summary_tbl <- summary_table_all(
          data, row_var = input$time_xticker,
          col_var = input$time_group, val_var = input$time_y,
          col_totals = time_col_totals_n$value,
          name_totals = time_col_totals_name$value,
          n_in_header = TRUE, subj_col = subj_col,
          baseline_name = NULL,
          add_cfb = time_add_cfb$value, cfb_var = chg_col,
          func_list = summary_func,
          caption = '',
          footnote = '',
          rowlabel = '', format = 'html'
        )
      }
      # Summary table with grouping by input$time_row;
      else if(input$time_graph_type != 'Summary table' && is_blank(input$time_group) && !is_blank(input$time_facet_r) && is_blank(input$time_facet_c)){
        summary_tbl <- summary_table_all(
          data, row_var = input$time_xticker,
          col_var = input$time_facet_r, val_var = input$time_y,
          col_totals = time_col_totals_n$value,
          name_totals = time_col_totals_name$value,
          n_in_header = TRUE, subj_col = subj_col,
          baseline_name = NULL,
          add_cfb = time_add_cfb$value, cfb_var = chg_col,
          func_list = summary_func,
          caption = '',
          footnote = '',
          rowlabel = '', format = 'html'
        )
      }
      # Summary table with grouping by input$time_column;
      else if(input$time_graph_type != 'Summary table' && is_blank(input$time_group) && is_blank(input$time_facet_r) && !is_blank(input$time_facet_c)){
        summary_tbl <- summary_table_all(
          data, row_var = input$time_xticker,
          col_var = input$time_facet_c, val_var = input$time_y,
          col_totals = time_col_totals_n$value,
          name_totals = time_col_totals_name$value,
          n_in_header = TRUE, subj_col = subj_col,
          baseline_name = NULL,
          add_cfb = time_add_cfb$value, cfb_var = chg_col,
          func_list = summary_func,
          caption = '',
          footnote = '',
          rowlabel = '', format = 'html'
        )
      }
      
      # Summary table with grouping by input$time_group and input$time_facet_r;
      else if(input$time_graph_type != 'Summary table' && !is_blank(input$time_group) && !is_blank(input$time_facet_r) && is_blank(input$time_facet_c)){
        # subset data into different dataframe by input$time_facet_r
        unique_fr <- c(unique(time_facet_rlevs$value))
        summary_tbl=NULL
        d = NULL
        for (i in unique_fr) {
          #tmp_data <- data[[input$time_facet_c]] == unique_fc[i]
          tmp_data <- data[
            data[[input$time_facet_r]] == i,
            , drop = F
            ]  
          d <- summary_table_all(
            tmp_data, row_var = input$time_xticker,
            col_var = input$time_group, val_var = input$time_y,
            col_totals = time_col_totals_n$value,
            name_totals = time_col_totals_name$value,
            n_in_header = TRUE, subj_col = subj_col,
            baseline_name = NULL,
            add_cfb = time_add_cfb$value, cfb_var = chg_col,
            func_list = summary_func,
            caption = '',
            footnote = '',
            rowlabel = '', format = 'html'
          )
          summary_tbl = rbind(summary_tbl,i,d)
        }
      }
      
      # Summary table with grouping by input$time_group and input$time_facet_c;
      else if(input$time_graph_type != 'Summary table' && !is_blank(input$time_group) && is_blank(input$time_facet_r) && !is_blank(input$time_facet_c)){
        # subset data into different dataframe by input$time_facet_r
        #facet_row_levls <- unique_na(data[[input$time_facet_r]])
        unique_fc <- c(unique(time_facet_clevs$value))
        #facet_col_dim = unique_fc
        summary_tbl=NULL
        d = NULL
        for (i in unique_fc) {
          #tmp_data <- data[[input$time_facet_c]] == unique_fc[i]
          tmp_data <- data[
            data[[input$time_facet_c]] == i,
            , drop = F
            ]  
          d <- summary_table_all(
            tmp_data, row_var = input$time_xticker,
            col_var = input$time_group, val_var = input$time_y,
            col_totals = time_col_totals_n$value,
            name_totals = time_col_totals_name$value,
            n_in_header = TRUE, subj_col = subj_col,
            baseline_name = NULL,
            add_cfb = time_add_cfb$value, cfb_var = chg_col,
            func_list = summary_func,
            caption = '',
            footnote = '',
            rowlabel = '', format = 'html'
          )
          summary_tbl = rbind(summary_tbl,i,d)
        }
      }
      
      # Summary table with grouping by input$time_facet_c and input$time_facet_r;
      else if(input$time_graph_type != 'Summary table' && is_blank(input$time_group) && !is_blank(input$time_facet_r) && !is_blank(input$time_facet_c)){
        # subset data into different dataframe by input$time_facet_r
        unique_fr <- c(unique(time_facet_rlevs$value))
        summary_tbl=NULL
        d = NULL
        for (i in unique_fr) {
          #tmp_data <- data[[input$time_facet_c]] == unique_fc[i]
          tmp_data <- data[
            data[[input$time_facet_r]] == i,
            , drop = F
            ]  
          d <- summary_table_all(
            tmp_data, row_var = input$time_xticker,
            col_var = input$time_facet_c, val_var = input$time_y,
            col_totals = time_col_totals_n$value,
            name_totals = time_col_totals_name$value,
            n_in_header = TRUE, subj_col = subj_col,
            baseline_name = NULL,
            add_cfb = time_add_cfb$value, cfb_var = chg_col,
            func_list = summary_func,
            caption = '',
            footnote = '',
            rowlabel = '', format = 'html'
          )
          summary_tbl = rbind(summary_tbl,i,d)
        }
      }
      # Summary table with grouping by input$time_group and input$time_facet_c and input$time_facet_r;
      else if(input$time_graph_type != 'Summary table' && !is_blank(input$time_group) && !is_blank(input$time_facet_r) && !is_blank(input$time_facet_c)){
        # subset data into different dataframe by input$time_facet_r
        unique_fr <- c(unique(time_facet_rlevs$value))
        unique_fc <- c(unique(time_facet_clevs$value))
        summary_tbl=NULL
        d = NULL
        for (i in unique_fr) {
          
          summary_tbl_c = NULL
          
          tmp_data_r <- data[
            data[[input$time_facet_r]] == i,
            , drop = F
            ] 
          for (j in unique_fc) {
            d_c = NULL
            tmp_data_c <- tmp_data_r[
              tmp_data_r[[input$time_facet_c]] == j,
              , drop = F
              ]                
            d_c <- summary_table_all(
              tmp_data_c, row_var = input$time_xticker,
              col_var = input$time_group, val_var = input$time_y,
              col_totals = time_col_totals_n$value,
              name_totals = time_col_totals_name$value,
              n_in_header = TRUE, subj_col = subj_col,
              baseline_name = NULL,
              add_cfb = time_add_cfb$value, cfb_var = chg_col,
              func_list = summary_func,
              caption = '',
              footnote = '',
              rowlabel = '', format = 'html'
            )
            ij <- c(",")
            summary_tbl_c = rbind(summary_tbl_c,i,ij,j, d_c)
          }
          
          summary_tbl = rbind(summary_tbl,summary_tbl_c)
        }
      }
      
      return(summary_tbl)
    })
    
    
    output$time_summary <- renderUI({
      req(time_plot_show$value)
      HTML(time_summary())
    })    
    
    
    
    # a reactive value linked to the status of time-profiling plot
    time_plot_show <- reactiveValues(value = FALSE)
    observe({
        if(any(is_blank(input$time_bmk), is_blank(input$time_y),
               is_blank(input$time_graph_type),
               is.null(time_toggle_sample_size$value))) {
            time_plot_show$value <- FALSE
        } else {
            if(input$time_graph_type == 'Summary table')
                time_plot_show$value <- FALSE
            else {
                if((input$time_graph_type %in%
                    c('Boxplot', 'Mean + SE plot',
                      'Mean + SD plot', 'Median + IQR plot') &&
                    is.null(time_toggle_points$value)) ||
                   (input$time_graph_type %in% c('Spaghetti plot') &&
                    (is.null(time_toggle_subjid$value) ||
                     is.null(time_toggle_points$value)))) {
                    time_plot_show$value <- FALSE
                } else {
                    time_plot_show$value <- TRUE
                }
            }
        }
    })

    output$time_plot_ui <- renderUI({
        req(time_plot_show$value)
      if(length(input$time_bmk) == 1) {
        tags$div(
          style = 'position:relative',
          tabsetPanel(
            tabPanel("Graph",  
                     plotOutput(
                       'time_plot',
                       brush = brushOpts(id = 'time_plot_brush',
                                         resetOnNew = TRUE,
                                         delay = 1000),
                       dblclick = 'time_plot_dblclick',
                       click = 'time_plot_click',
                       hover = hoverOpts('time_plot_hover',
                                         delay = 100,
                                         delayType = 'debounce')
                     ),
                     uiOutput('time_hover_info')
            ),
            tabPanel("Summary Table", 
                     htmlOutput('time_summary')
                     #plotOutput('time_summary')
                     
            ) 
          ) 
        )
        
      } else {
        height <- 400 * 1.4^(ceiling(length(input$time_bmk) / 2) - 1)
        tags$div(
          style = 'position:relative',
          tabsetPanel(
            tabPanel("Graph", 
                     plotOutput('time_plot', height = paste0(height, 'px')),
                     uiOutput('time_hover_info')
            ), 
            tabPanel("Summary Table",
                     htmlOutput('time_summary')
                     #htmlOutput('time_summary')
                     #plotOutput('time_summary')
            ) 
          )
        )
      }
    })
    
    time_plot_obj <- reactive({
        req(isTRUE(time_plot_show$value))
        data <- time_data_input()
        ngroup_levs <- length(time_group_levs$value)
        n_colors <- length(time_line_color$value)
        n_linetypes <- length(time_line_type$value)
        is_linegraph <- input$time_graph_type %in%
            setdiff(time_graph_list, 'Boxplot')
        req(!(!is_blank(input$time_group) &&
                  (ngroup_levs != n_colors ||
                       (is_linegraph && ngroup_levs != n_linetypes))))
        start_time <- proc.time()

        
        reference_line <- as.numeric(trimws(unlist(strsplit(as.character(
            input$time_reference_line
        ), ','))))

        geoms <- c()
        avg_method <- NULL
        var_method <- NULL
        if(input$time_graph_type == 'Boxplot') geoms <- 'boxplot'
        else if(input$time_graph_type == 'Spaghetti plot')
            geoms <- 'line'
        else {
            geoms <- 'sumline'
            if(input$time_graph_type == 'Median + IQR plot')
                avg_method <- 'median'
            else {
                avg_method <- 'mean'
                if(input$time_graph_type == 'Mean + SE plot')
                    var_method <- 'se'
                else var_method <- 'sd'
            }
        }
        if(isTRUE(time_toggle_points$value)) geoms <- c(geoms, 'point')
        if(is_blank(time_facet_rlevs$value)) facet_r_levels <- NULL
        else {
            facet_r_levels <- setNames(
                time_facet_rlevs$value,
                paste0(input$time_facet_r, ': ', time_facet_rlevs$value)
            )
        }
        if(is_blank(time_facet_clevs$value)) facet_c_levels <- NULL
        else {
            facet_c_levels <- setNames(
                time_facet_clevs$value,
                paste0(input$time_facet_c, ': ', time_facet_clevs$value)
            )
        }
        
        # replace content inside `{}`
        #x_label <- r_format(time_plot_xlab$value, tnf$time_current)
        #y_label <- r_format(time_plot_ylab$value, tnf$time_current)
        #plot_title <- r_format(time_plot_title$value, tnf$time_current)
        x_label <-time_plot_xlab$value
        if (time_plot_ylab$value != '{Biomarker} ({YVariable})'){
        y_label <- time_plot_ylab$value
        }
        else if (time_plot_ylab$value == '{Biomarker} ({YVariable})'){
          y_label <- input$time_y
        } 
        plot_title_tmp <- time_plot_title$value 

        if (plot_title_tmp == 'Plot Title' && length(input$time_bmk) == 1){
          plot_title <- input$time_bmk
        }
        else if (plot_title_tmp != 'Plot Title' && length(input$time_bmk) == 1){
          plot_title <- paste(time_plot_title$value, '\n', input$time_bmk)
        }
        else if (length(input$time_bmk) != 1){
          plot_title <- paste(time_plot_title$value)
        }
        
          obj <- suppressWarnings(time_profiling_ggplot(
            data, x = xvar_col, y = input$time_y,
            subject = subj_col, group = input$time_group,
            group_levels = time_group_levs$value,
            facet_r = input$time_facet_r, facet_c = input$time_facet_c,
            facet_r_levels = facet_r_levels, facet_c_levels = facet_c_levels,
            x_tick_label = input$time_xticker, all_xticks = TRUE,
            geoms = geoms, avg_method = avg_method,
            var_method = var_method, y_log = input$time_to_log,
            sample_size = time_toggle_sample_size$value,
            sample_size_font_size = 3,
            xlab = x_label, ylab = y_label, title = plot_title,
            x_limit = time_brush_range$xlim, y_limit = time_brush_range$ylim,
            x_tick_angle = input$time_plot_xtick_angle,
            y_tick_angle = 0, reference_line = reference_line,
            subject_show = time_toggle_subjid$value,
            add_legend = TRUE, legend_pos = 'bottom',
            all_colors = time_line_color$value,
            all_linetypes = time_line_type$value,
            highlighted_subj = highlight_line$value,
            randseed = 0, return_data = TRUE
          ))
          
        
        return(obj)
        
    }) 
    
    
    time_plot_m_obj <- reactive({
      req(isTRUE(time_plot_show$value))
      

      ngroup_levs <- length(time_group_levs$value)
      n_colors <- length(time_line_color$value)
      n_linetypes <- length(time_line_type$value)
      is_linegraph <- input$time_graph_type %in%
        setdiff(time_graph_list, 'Boxplot')
      req(!(!is_blank(input$time_group) &&
              (ngroup_levs != n_colors ||
                 (is_linegraph && ngroup_levs != n_linetypes))))
      start_time <- proc.time()
      
      
      reference_line <- as.numeric(trimws(unlist(strsplit(as.character(
        input$time_reference_line
      ), ','))))
      
      geoms <- c()
      avg_method <- NULL
      var_method <- NULL
      if(input$time_graph_type == 'Boxplot') geoms <- 'boxplot'
      else if(input$time_graph_type == 'Spaghetti plot')
        geoms <- 'line'
      else {
        geoms <- 'sumline'
        if(input$time_graph_type == 'Median + IQR plot')
          avg_method <- 'median'
        else {
          avg_method <- 'mean'
          if(input$time_graph_type == 'Mean + SE plot')
            var_method <- 'se'
          else var_method <- 'sd'
        }
      }
      if(isTRUE(time_toggle_points$value)) geoms <- c(geoms, 'point')
      if(is_blank(time_facet_rlevs$value)) facet_r_levels <- NULL
      else {
        facet_r_levels <- setNames(
          time_facet_rlevs$value,
          paste0(input$time_facet_r, ': ', time_facet_rlevs$value)
        )
      }
      if(is_blank(time_facet_clevs$value)) facet_c_levels <- NULL
      else {
        facet_c_levels <- setNames(
          time_facet_clevs$value,
          paste0(input$time_facet_c, ': ', time_facet_clevs$value)
        )
      }

      data <- time_m_data_input()
      ## object to save all the data frame for each biomarker;
      multi_bm_obj <- list()
      #data <- data[data[[param_col]] == 'IL-10']
      #data <- mutate(data, param_col=xvar_col)
      unique_bmk <- c(unique(input$time_download_multibmk))
      
      # replace content inside `{}`
      #x_label <- r_format(time_plot_xlab$value, tnf$time_current)
      #y_label <- r_format(time_plot_ylab$value, tnf$time_current)
      #plot_title <- r_format(time_plot_title$value, tnf$time_current)
      
      x_label <-time_plot_xlab$value

      for (i in unique_bmk){
      multi_bm1 <- data[data[[param_col]] == i, , drop = F]
      
      if (time_plot_ylab$value != '{Biomarker} ({YVariable})'){
        y_label <- time_plot_ylab$value
      }
      else if (time_plot_ylab$value == '{Biomarker} ({YVariable})'){
        y_label <- paste(i , '  ', input$time_y)
      } 
      
      plot_title_tmp <- time_plot_title$value
      if (plot_title_tmp != 'Plot Title'){
        plot_title <- paste(time_plot_title$value, '\n', i)
      }
      else if (plot_title_tmp == 'Plot Title'){
        plot_title <- i
      }
      
      
      multi_bm_obj[[i]] <- suppressWarnings(time_profiling_ggplot(
        multi_bm1, x = xvar_col, y = input$time_y,
        subject = subj_col, group = input$time_group,
        group_levels = time_group_levs$value,
        facet_r = input$time_facet_r, facet_c = input$time_facet_c,
        facet_r_levels = facet_r_levels, facet_c_levels = facet_c_levels,
        x_tick_label = input$time_xticker, all_xticks = TRUE,
        geoms = geoms, avg_method = avg_method,
        var_method = var_method, y_log = input$time_to_log,
        sample_size = time_toggle_sample_size$value,
        sample_size_font_size = 3,
        xlab = x_label, ylab = y_label, title = plot_title,
        x_limit =  , y_limit =  ,
        x_tick_angle = input$time_plot_xtick_angle,
        y_tick_angle = 0, reference_line = reference_line,
        subject_show = time_toggle_subjid$value,
        add_legend = TRUE, legend_pos = 'bottom',
        all_colors = time_line_color$value,
        all_linetypes = time_line_type$value,
        highlighted_subj = highlight_line$value,
        randseed = 0, return_data = TRUE
      ))
      }
      
      
      #return(obj)
      return(multi_bm_obj)
    })  
    
    output$time_plot <- renderPlot({
        req(time_plot_obj())
        time_plot_obj()$plot
    })
    
    # footnote for plot
    output$time_plot_footnote_out <- renderUI({
        req(time_plot_obj())
        footnote <- r_format(time_plot_footnote$value, tnf$time_current)
        value <- trimws(paste(footnote, default_footnote_lines(), sep = '\n'))
        HTML(paste(strsplit(value, '\n')[[1]], collapse = '<br/>'))
    })
    
    # print out tooltip
    #   1. tooltip for Median + IQR & Boxplot graphs
    #   2. tooltip for Subject ID of highlighted line in Spaghetti plot    
    output$time_hover_info <- renderUI({
        req(time_plot_show$value, time_plot_obj())
        types <- c('Boxplot', 'Mean + SE plot', 'Mean + SD plot',
                   'Median + IQR plot', 'Spaghetti plot')
        if(input$time_graph_type %in% types && time_toggle_points$value) {
            data <- time_plot_obj()$data
            tooltip(input$time_plot_hover, data, 'x_point', input$time_y,
                    c(subj_col, xlabel_col, input$time_y),
                    c(subj_col, 'Visit', input$time_y))
        } else if(input$time_graph_type %in% 'Spaghetti plot' &&
                  !is.null(highlight_line$value)) {
            highlight_data <- highlight_line$value %>%
                group_by_(subj_col) %>%
                filter(row_number() == 1)
            mouse <- isolate(input$time_plot_hover)
            tooltip(mouse, highlight_data,
                    vars_print = c(subj_col), vars_name = c(subj_col),
                    search = FALSE)
        }
    })
    
    # capture highlight line(s) in Spaghetti plot when mouse hovering
    highlight_line <- reactiveValues(value = NULL)
    observeEvent(input$time_plot_hover, {
        req(req(input$time_graph_type) == 'Spaghetti plot', input$time_y)
        data <- req(time_data_input())
        df <- near_lines(data, input$time_plot_hover, subjvar = subj_col,
                         xvar = xvar_col, yvar = input$time_y, maxlines = 1)
        if(!is.null(df) && !is.null(dim(df)) && nrow(df) >= 1) {
            highlight_line$value <- df
        } else {
            highlight_line$value <- NULL
        }
    })
    observeEvent(input$time_plot_click, {
        highlight_line$value <- NULL
    })

    
    #---------------------------------------------
    # Plot/Table download
    #---------------------------------------------
    
    # time-profiling graph download button widget
    output$time_download_plot_action <- renderUI({
        req(time_plot_show$value)
        actionButton('time_download_plot_action', 'Download Plot')
    })
    # download page format select input widget
    output$time_download_plot_format <- renderUI({
      if(length(input$time_download_multibmk) == 1) {
        selectInput(
            'time_download_plot_format', 'Graph format',
            choices = c('Choose' = '', time_download_plot_formats)
        )
      }
      else if (length(input$time_download_multibmk) != 1) {
        time_download_plot_formats = c('pdf')
        selectInput(
          'time_download_plot_format', 'Graph format',
          choices = c('Choose' = '', time_download_plot_formats)
        )
      }
    })
    # download page height numeric input widget
    output$time_download_plot_height <- renderUI({
        numericInput(
            'time_download_plot_height', 'Height (inches)',
            value = time_plot_height_default
        )
    })
    # download page width numeric input widget
    output$time_download_plot_width <- renderUI({
        numericInput(
            'time_download_plot_width', 'Width (inches)',
            value = time_plot_width_default
        )
    })
    # download page with resolution widget
    output$time_download_plot_resolution <- renderUI({
        sliderInput(
            'time_download_plot_resolution', 'Resolution', 100, 800,
            value = 600, step = 100
        )
    })
    # download option for selection of multi biomarkers 
    output$time_download_multibmk <- renderUI({
      req(file_import$status)
      if (length(input$time_bmk) > 1) req(FALSE)

      data <- subset_data()$data
      
      if(!is_blank(input$time_study))
        data <- data[data[[study_col]] == input$time_study, , drop = F]
      if(!is_blank(input$time_cohort))
        data <- data[data[[cohort_col]] == input$time_cohort, , drop = F]
      if(!is_blank(input$time_group) && !is_blank(time_group_levs$value)) {
        cond_g <- data[[input$time_group]] %in% time_group_levs$value
        data <- data[cond_g, , drop = F]
      }
      if(!is_blank(input$time_facet_r) &&
         !is_blank(time_facet_rlevs$value)) {
        cond_r <- data[[input$time_facet_r]] %in% time_facet_rlevs$value
        data <- data[cond_r, , drop = F]
      }
      if(!is_blank(input$time_facet_c) &&
         !is_blank(time_facet_clevs$value)) {
        cond_c <- data[[input$time_facet_c]] %in% time_facet_clevs$value
        data <- data[cond_c, , drop = F]
      }              
      choices <- c('Choose' = '' , unique(data[[param_col]]))
      selected <- isolate(ternary(
        is.null(input$time_bmk), '', input$time_bmk
      ))    
      
      selectizeInput(
        'time_download_multibmk', 'Multi Biomarkers', choices, 
        multiple = T, options = list(plugins = list('drag_drop','remove_button'))
      )
    })
    
    # download button widget on download page
    output$time_download_plot_button <- renderUI({
        shinyjs::disabled(downloadButton('time_download_plot', 'Download'))
    })
    observe({
        if(all(!is_blank(input$time_download_plot_format),
               !is_blank(input$time_download_plot_height),
               !is_blank(input$time_download_plot_width),
               !is_blank(input$time_download_plot_resolution))) {
            shinyjs::enable('time_download_plot')
        }
    })
    
    # time-profiling table download button widget
    output$time_download_table_action <- renderUI({
        req(time_table_show$value)
        actionButton('time_download_table_action', 'Download Table')
    })
    output$time_download_table_button <- renderUI({
        req(!is_blank(input$time_download_table_format))
        downloadButton('time_download_table', strong('Download table'))
    })
    
    # time-profiling plot download handler
    output$time_download_plot <- downloadHandler(
        filename = function() {
            if(input$time_graph_type == 'Spaghetti plot') {
                graph_type <- 'spaghetti_plot'
            } else if(input$time_graph_type == 'Mean + SE plot') {
                graph_type <- 'mean_se'
            } else if(input$time_graph_type == 'Mean + SD plot') {
                graph_type <- 'mean_sd'
            } else if(input$time_graph_type == 'Boxplot') {
                graph_type <- 'boxplot'
            } else {
                graph_type <- 'median_iqr'
            }
            fname <- paste('time', graph_type, input$time_bmk,
                           format(Sys.Date(), format = '%Y%m%d'), sep = '_')
            fname <- paste(fname, input$time_download_plot_format, sep = '.')
            return(fname)
        },
        content = function(file) {
            height <- input$time_download_plot_height
            width <- input$time_download_plot_width
            dpi <- input$time_download_plot_resolution
            #####################################
            # Single/ Multiple biomarker plot output
            #####################################
            if(is_blank(input$time_download_multibmk)){
            plot_ <- time_plot_obj()$plot
            footnote <- r_format(time_plot_footnote$value, tnf$time_current)
            footnote <- trimws(paste(footnote,default_footnote_lines(), sep = '\n'))
            plot_ <- add_footnote(plot_, footnote)
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = dpi)
            }
            else if(!is_blank(input$time_download_multibmk)){
            
            pdf(file = file, width = width, height = height)
            footnote <- r_format(time_plot_footnote$value, tnf$time_current)
            footnote <- trimws(paste(footnote,default_footnote_lines(), sep = '\n'))
            #plot_ <- add_footnote(plot_, footnote)
            for (i in 1:length(c(unique(input$time_download_multibmk)))){
            temp <- time_plot_m_obj()
            plot_ <- temp[[i]]$plot
            print( plot_ )
            }
            dev.off()
            }

        }
    )
    
    # time-profiling table download handler
    output$time_download_table <- downloadHandler(
      filename = function() {
        file_name <- paste0(
          'time_table_', format(Sys.Date(),format = '%Y%m%d'),
          '.', input$time_download_table_format
        )
        return(file_name)
      },
      content = function(file) {
        data <- time_data_input()
        xlim <- time_brush_range$xlim
        if(!is.null(xlim)) {
          cond <- data[[xvar_col]] >= xlim[1] & data[[xvar_col]] <= xlim[2]
          data <- data[cond, ,  drop = FALSE]
        }
        data[, input$time_xticker] <- factor(
          data[, input$time_xticker],
          levels = unique(data[[input$time_xticker]][order(data[[xvar_col]])])
        )
        dgt <- time_decimal$value
        if(input$time_download_table_format == 'csv') {
          summary_func <- c(
            'N' = n_nna,
            'Mean' = partial(mean_na_str, digits = dgt),
            'SD' = partial(sd_na_str, digits = dgt + 1),
            '%CV' = partial(coeff_var_str, digits = dgt),
            'Median' = partial(median_str, digits = dgt),
            'Q1' = partial(q1_na_str, digits = dgt),
            'Q3' = partial(q3_na_str, digits = dgt),
            'Min' = partial(min_na_str, digits = dgt),
            'Max' = partial(max_na_str, digits = dgt)
          )
          if(isTRUE(input$time_to_log)) {
            summary_func <- c(
              summary_func,
              'Geom Mean' = partial(geo_mean_str, digits = dgt),
              'Geom %CV' = partial(geo_cv_str, digits = dgt),
              'Mean of LN' = partial(mean_ln_str, digits = dgt),
              'SD of LN' = partial(sd_ln_str, digits = dgt + 1)
            )
          }
          # Summary table without grouping;
          if(input$time_graph_type == 'Summary table' && is_blank(input$time_group) && is_blank(input$time_facet_r) && is_blank(input$time_facet_c)){
            summary_tbl <- summary_table_all(
              data, row_var = input$time_xticker,
              col_var = input$time_group, val_var = input$time_y,
              col_totals = time_col_totals_n$value,
              name_totals = time_col_totals_name$value,
              n_in_header = TRUE, subj_col = subj_col,
              baseline_name = NULL,
              add_cfb = time_add_cfb$value, cfb_var = chg_col,
              func_list = summary_func,
              caption = time_table_title$value,
              footnote = time_table_footnote$value,
              rowlabel = '', format = 'csv'
            )
            
            write.csv(summary_tbl, file = file, row.names = F, na = '')
          }
          # Summary table with grouping by input$time_group;
          else if(input$time_graph_type == 'Summary table' && !is_blank(input$time_group) && is_blank(input$time_facet_r) && is_blank(input$time_facet_c)){
            summary_tbl <- summary_table_all(
              data, row_var = input$time_xticker,
              col_var = input$time_group, val_var = input$time_y,
              col_totals = time_col_totals_n$value,
              name_totals = time_col_totals_name$value,
              n_in_header = TRUE, subj_col = subj_col,
              baseline_name = NULL,
              add_cfb = time_add_cfb$value, cfb_var = chg_col,
              func_list = summary_func,
              caption = time_table_title$value,
              footnote = time_table_footnote$value,
              rowlabel = '', format = 'csv'
            )
            
            write.csv(summary_tbl, file = file, row.names = F, na = '')
          }
          # Summary table with grouping by input$time_row;
          else if(input$time_graph_type == 'Summary table' && is_blank(input$time_group) && !is_blank(input$time_facet_r) && is_blank(input$time_facet_c)){
            summary_tbl <- summary_table_all(
              data, row_var = input$time_xticker,
              col_var = input$time_facet_r, val_var = input$time_y,
              col_totals = time_col_totals_n$value,
              name_totals = time_col_totals_name$value,
              n_in_header = TRUE, subj_col = subj_col,
              baseline_name = NULL,
              add_cfb = time_add_cfb$value, cfb_var = chg_col,
              func_list = summary_func,
              caption = time_table_title$value,
              footnote = time_table_footnote$value,
              rowlabel = '', format = 'csv'
            )
            write.csv(summary_tbl, file = file, row.names = F, na = '')
          }
          # Summary table with grouping by input$time_column;
          else if(input$time_graph_type == 'Summary table' && is_blank(input$time_group) && is_blank(input$time_facet_r) && !is_blank(input$time_facet_c)){
            summary_tbl <- summary_table_all(
              data, row_var = input$time_xticker,
              col_var = input$time_facet_c, val_var = input$time_y,
              col_totals = time_col_totals_n$value,
              name_totals = time_col_totals_name$value,
              n_in_header = TRUE, subj_col = subj_col,
              baseline_name = NULL,
              add_cfb = time_add_cfb$value, cfb_var = chg_col,
              func_list = summary_func,
              caption = time_table_title$value,
              footnote = time_table_footnote$value,
              rowlabel = '', format = 'csv'
            )
            write.csv(summary_tbl, file = file, row.names = F, na = '')
          }
          
          # Summary table with grouping by input$time_group and input$time_facet_r;
          else if(input$time_graph_type == 'Summary table' && !is_blank(input$time_group) && !is_blank(input$time_facet_r) && is_blank(input$time_facet_c)){
            # subset data into different dataframe by input$time_facet_r
            
            unique_fr <- c(unique(time_facet_rlevs$value))
            summary_tbl=NULL
            d = NULL
            for (i in unique_fr) {
              #tmp_data <- data[[input$time_facet_c]] == unique_fc[i]
              #lab_tmp = paste(c(i,time_table_title$value))
              tmp_data <- data[
                data[[input$time_facet_r]] == i,
                , drop = F
                ]  
              d <- summary_table_all(
                tmp_data, row_var = input$time_xticker,
                col_var = input$time_group, val_var = input$time_y,
                col_totals = time_col_totals_n$value,
                name_totals = time_col_totals_name$value,
                n_in_header = TRUE, subj_col = subj_col,
                baseline_name = NULL,
                add_cfb = time_add_cfb$value, cfb_var = chg_col,
                func_list = summary_func,
                caption = time_table_title$value, 
                footnote = time_table_footnote$value,
                rowlabel = '', format = 'csv'
              )
              #change column names
              d$subgroup <- i
              colname_tmp <- data.frame(matrix(c(colnames(d)), nrow=1))
              colnames(d) <- c(paste0('VAR', 1:ncol(d)))
              colnames(colname_tmp) <- c(paste0('VAR', 1:ncol(d)))
              summary_tbl = rbind(summary_tbl,colname_tmp,d)
            }
            summary_tbl <- unname(summary_tbl)
            write.csv(summary_tbl, file = file, row.names = F, na = '')
          }
          
          # Summary table with grouping by input$time_group and input$time_facet_c;
          else if(input$time_graph_type == 'Summary table' && !is_blank(input$time_group) && is_blank(input$time_facet_r) && !is_blank(input$time_facet_c)){
            # subset data into different dataframe by input$time_facet_r
            #facet_row_levls <- unique_na(data[[input$time_facet_r]])
            unique_fc <- c(unique(time_facet_clevs$value))
            #facet_col_dim = unique_fc
            summary_tbl=NULL
            d = NULL
            for (i in unique_fc) {
              #tmp_data <- data[[input$time_facet_c]] == unique_fc[i]
              
              tmp_data <- data[
                data[[input$time_facet_c]] == i,
                , drop = F
                ]  
              d <- summary_table_all(
                tmp_data, row_var = input$time_xticker,
                col_var = input$time_group, val_var = input$time_y,
                col_totals = time_col_totals_n$value,
                name_totals = time_col_totals_name$value,
                n_in_header = TRUE, subj_col = subj_col,
                baseline_name = NULL,
                add_cfb = time_add_cfb$value, cfb_var = chg_col,
                func_list = summary_func,
                caption = time_table_title$value,
                footnote = time_table_footnote$value,
                rowlabel = '', format = 'csv'
              )
              #change column names
              d$subgroup <- i
              colname_tmp <- data.frame(matrix(c(colnames(d)), nrow=1))
              colnames(d) <- c(paste0('VAR', 1:ncol(d)))
              colnames(colname_tmp) <- c(paste0('VAR', 1:ncol(d)))
              summary_tbl = rbind(summary_tbl,colname_tmp,d)
              
              #summary_tbl = rbind(summary_tbl,i,d)
              #summary_tbl = rbind(summary_tbl,d)
            }
            summary_tbl <- unname(summary_tbl)
            write.csv(summary_tbl, file = file, row.names = F, na = '')
          }
          
          # Summary table with grouping by input$time_facet_c and input$time_facet_r;
          else if(input$time_graph_type == 'Summary table' && is_blank(input$time_group) && !is_blank(input$time_facet_r) && !is_blank(input$time_facet_c)){
            # subset data into different dataframe by input$time_facet_r
            unique_fr <- c(unique(time_facet_rlevs$value))
            summary_tbl=NULL
            d = NULL
            for (i in unique_fr) {
              #tmp_data <- data[[input$time_facet_c]] == unique_fc[i]
              tmp_data <- data[
                data[[input$time_facet_r]] == i,
                , drop = F
                ]  
              d <- summary_table_all(
                tmp_data, row_var = input$time_xticker,
                col_var = input$time_facet_c, val_var = input$time_y,
                col_totals = time_col_totals_n$value,
                name_totals = time_col_totals_name$value,
                n_in_header = TRUE, subj_col = subj_col,
                baseline_name = NULL,
                add_cfb = time_add_cfb$value, cfb_var = chg_col,
                func_list = summary_func,
                caption = time_table_title$value,
                footnote = time_table_footnote$value,
                rowlabel = '', format = 'csv'
              )
              #change column names
              d$subgroup <- i
              colname_tmp <- data.frame(matrix(c(colnames(d)), nrow=1))
              colnames(d) <- c(paste0('VAR', 1:ncol(d)))
              colnames(colname_tmp) <- c(paste0('VAR', 1:ncol(d)))
              summary_tbl = rbind(summary_tbl,colname_tmp,d)
              #summary_tbl = rbind(summary_tbl,i,d)
            }
            summary_tbl <- unname(summary_tbl)
            write.csv(summary_tbl, file = file, row.names = F, na = '')
          }
          # Summary table with grouping by input$time_group and input$time_facet_c and input$time_facet_r;
          else if(input$time_graph_type == 'Summary table' && !is_blank(input$time_group) && !is_blank(input$time_facet_r) && !is_blank(input$time_facet_c)){
            # subset data into different dataframe by input$time_facet_r
            unique_fr <- c(unique(time_facet_rlevs$value))
            unique_fc <- c(unique(time_facet_clevs$value))
            summary_tbl=NULL
            d = NULL
            for (i in unique_fr) {
              
              summary_tbl_c = NULL
              tmp_data_r <- data[
                data[[input$time_facet_r]] == i,
                , drop = F
                ] 
              for (j in unique_fc) {
                d_c = NULL
                tmp_data_c <- tmp_data_r[
                  tmp_data_r[[input$time_facet_c]] == j,
                  , drop = F
                  ]                
                d_c <- summary_table_all(
                  tmp_data_c, row_var = input$time_xticker,
                  col_var = input$time_group, val_var = input$time_y,
                  col_totals = time_col_totals_n$value,
                  name_totals = time_col_totals_name$value,
                  n_in_header = TRUE, subj_col = subj_col,
                  baseline_name = NULL,
                  add_cfb = time_add_cfb$value, cfb_var = chg_col,
                  func_list = summary_func,
                  caption = time_table_title$value,
                  footnote = time_table_footnote$value,
                  rowlabel = '', format = 'csv'
                )
                ij <- c("~")
                i_j <- paste0(i,'~',j)
                #change column names
                d_c$subgroup <- i_j
                colname_tmp <- data.frame(matrix(c(colnames(d_c)), nrow=1))
                colnames(d_c) <- c(paste0('VAR', 1:ncol(d_c)))
                colnames(colname_tmp) <- c(paste0('VAR', 1:ncol(d_c)))
                summary_tbl_c = rbind(summary_tbl_c,colname_tmp,d_c)
                
                #summary_tbl_c = rbind(summary_tbl_c,i,ij,j, d_c)
              }
              
              #colname_tmp <- data.frame(matrix(c(colnames(summary_tbl_c)), nrow=1))
              #colnames(summary_tbl_c) <- c(paste0('VAR', 1:ncol(summary_tbl_c)))
              #colnames(colname_tmp) <- c(paste0('VAR', 1:ncol(summary_tbl_c)))
              #summary_tbl = rbind(summary_tbl,colname_tmp,summary_tbl_c)
              
              
              
              summary_tbl = rbind(summary_tbl,summary_tbl_c)
            }
            summary_tbl <- unname(summary_tbl)
            write.csv(summary_tbl, file = file, row.names = F, na = '')
          }
          
          
        } else if(input$time_download_table_format == 'rtf') {
          summary_func <- c(
            'N' = n_nna,
            'Mean (SD)' = partial(mean_sd_str, digits = dgt),
            '%CV' = partial(coeff_var_str, digits = dgt),
            'Median' = partial(median_str, digits = dgt),
            'Q1, Q3' = partial(q1_q3_str, digits = dgt),
            'Min, Max' = partial(min_max_str, digits = dgt)
          )
          if(isTRUE(input$time_to_log)) {
            summary_func <- c(
              summary_func,
              'Geom Mean (%CV)' = partial(geo_mean_cv_str, digits = dgt),
              'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
            )
          }
          # Summary table without grouping;
          if(input$time_graph_type == 'Summary table' && is_blank(input$time_group) && is_blank(input$time_facet_r) && is_blank(input$time_facet_c)){
            summary_tbl <- summary_table_all(
              data, row_var = input$time_xticker,
              col_var = input$time_group, val_var = input$time_y,
              col_totals = time_col_totals_n$value,
              name_totals = time_col_totals_name$value,
              n_in_header = TRUE, subj_col = subj_col,
              baseline_name = NULL,
              add_cfb = time_add_cfb$value, cfb_var = chg_col,
              func_list = summary_func,
              caption = time_table_title$value,
              footnote = time_table_footnote$value,
              rowlabel = '', format = 'rtf'
            )
            rtf_table_wrapper(
              file, summary_tbl, block_break = TRUE,
              nline_block = length(summary_func) + 1,
              caption = time_table_title$value,
              footnote = time_table_footnote$value
            )
          }
          # Summary table with grouping by input$time_group;
          else if(input$time_graph_type == 'Summary table' && !is_blank(input$time_group) && is_blank(input$time_facet_r) && is_blank(input$time_facet_c)){
            summary_tbl <- summary_table_all(
              data, row_var = input$time_xticker,
              col_var = input$time_group, val_var = input$time_y,
              col_totals = time_col_totals_n$value,
              name_totals = time_col_totals_name$value,
              n_in_header = TRUE, subj_col = subj_col,
              baseline_name = NULL,
              add_cfb = time_add_cfb$value, cfb_var = chg_col,
              func_list = summary_func,
              caption = time_table_title$value,
              footnote = time_table_footnote$value,
              rowlabel = '', format = 'rtf'
            )
            rtf_table_wrapper(
              file, summary_tbl, block_break = TRUE,
              nline_block = length(summary_func) + 1,
              caption = time_table_title$value,
              footnote = time_table_footnote$value
            )
          }
          # Summary table with grouping by input$time_row;
          else if(input$time_graph_type == 'Summary table' && is_blank(input$time_group) && !is_blank(input$time_facet_r) && is_blank(input$time_facet_c)){
            summary_tbl <- summary_table_all(
              data, row_var = input$time_xticker,
              col_var = input$time_facet_r, val_var = input$time_y,
              col_totals = time_col_totals_n$value,
              name_totals = time_col_totals_name$value,
              n_in_header = TRUE, subj_col = subj_col,
              baseline_name = NULL,
              add_cfb = time_add_cfb$value, cfb_var = chg_col,
              func_list = summary_func,
              caption = time_table_title$value,
              footnote = time_table_footnote$value,
              rowlabel = '', format = 'rtf'
            )
            rtf_table_wrapper(
              file, summary_tbl, block_break = TRUE,
              nline_block = length(summary_func) + 1,
              caption = time_table_title$value,
              footnote = time_table_footnote$value
            )
          }
          # Summary table with grouping by input$time_column;
          else if(input$time_graph_type == 'Summary table' && is_blank(input$time_group) && is_blank(input$time_facet_r) && !is_blank(input$time_facet_c)){
            summary_tbl <- summary_table_all(
              data, row_var = input$time_xticker,
              col_var = input$time_facet_c, val_var = input$time_y,
              col_totals = time_col_totals_n$value,
              name_totals = time_col_totals_name$value,
              n_in_header = TRUE, subj_col = subj_col,
              baseline_name = NULL,
              add_cfb = time_add_cfb$value, cfb_var = chg_col,
              func_list = summary_func,
              caption = time_table_title$value,
              footnote = time_table_footnote$value,
              rowlabel = '', format = 'rtf'
            )
            rtf_table_wrapper(
              file, summary_tbl, block_break = TRUE,
              nline_block = length(summary_func) + 1,
              caption = time_table_title$value,
              footnote = time_table_footnote$value
            )
          }
          
          # Summary table with grouping by input$time_group and input$time_facet_r;
          else if(input$time_graph_type == 'Summary table' && !is_blank(input$time_group) && !is_blank(input$time_facet_r) && is_blank(input$time_facet_c)){
            # subset data into different dataframe by input$time_facet_r
            
            unique_fr <- c(unique(time_facet_rlevs$value))
            summary_tbl=NULL
            d = NULL
            for (i in unique_fr) {
              #tmp_data <- data[[input$time_facet_c]] == unique_fc[i]
              #lab_tmp = paste(c(i,time_table_title$value))
              tmp_data <- data[
                data[[input$time_facet_r]] == i,
                , drop = F
                ]  
              i_m <-paste0('\n', i,'\n')
              d <- summary_table_all(
                tmp_data, row_var = input$time_xticker,
                col_var = input$time_group, val_var = input$time_y,
                col_totals = time_col_totals_n$value,
                name_totals = time_col_totals_name$value,
                n_in_header = TRUE, subj_col = subj_col,
                baseline_name = NULL,
                add_cfb = time_add_cfb$value, cfb_var = chg_col,
                func_list = summary_func,
                caption = time_table_title$value, mdgrp=i_m,
                footnote = time_table_footnote$value,name_N='N',
                rowlabel = '', format = 'rtf'
              )
              #summary_tbl = rbind(d)
              # To change the row names of the dataframe d
              d_nams <- row.names(d)
              d_nams <- paste0(i,d_nams)
              row.names(d) <- d_nams
              summary_tbl = rbind(summary_tbl, d)
              
            }
            rtf_table_wrapper(
              file, summary_tbl, block_break = TRUE,
              nline_block = length(summary_func) + 1,
              caption = time_table_title$value,
              footnote = time_table_footnote$value
            )                  
          }
          
          # Summary table with grouping by input$time_group and input$time_facet_c;
          else if(input$time_graph_type == 'Summary table' && !is_blank(input$time_group) && is_blank(input$time_facet_r) && !is_blank(input$time_facet_c)){
            # subset data into different dataframe by input$time_facet_r
            #facet_row_levls <- unique_na(data[[input$time_facet_r]])
            unique_fc <- c(unique(time_facet_clevs$value))
            #facet_col_dim = unique_fc
            summary_tbl=NULL
            d = NULL
            for (i in unique_fc) {
              #tmp_data <- data[[input$time_facet_c]] == unique_fc[i]
              
              tmp_data <- data[
                data[[input$time_facet_c]] == i,
                , drop = F
                ]  
              i_m <-paste0('\n', i,'\n')
              d <- summary_table_all(
                tmp_data, row_var = input$time_xticker,
                col_var = input$time_group, val_var = input$time_y,
                col_totals = time_col_totals_n$value,
                name_totals = time_col_totals_name$value,
                n_in_header = TRUE, subj_col = subj_col,
                baseline_name = NULL,
                add_cfb = time_add_cfb$value, cfb_var = chg_col,
                func_list = summary_func,
                caption = time_table_title$value, mdgrp=i_m,
                footnote = time_table_footnote$value,name_N='N',
                rowlabel = '', format = 'rtf'
              )
              d_nams <- row.names(d)
              d_nams <- paste0(i,d_nams)
              row.names(d) <- d_nams
              summary_tbl = rbind(summary_tbl, d)
            }
            rtf_table_wrapper(
              file, summary_tbl, block_break = TRUE,
              nline_block = length(summary_func) + 1,
              caption = time_table_title$value,
              footnote = time_table_footnote$value
            )
          }
          
          # Summary table with grouping by input$time_facet_c and input$time_facet_r;
          else if(input$time_graph_type == 'Summary table' && is_blank(input$time_group) && !is_blank(input$time_facet_r) && !is_blank(input$time_facet_c)){
            # subset data into different dataframe by input$time_facet_r
            unique_fr <- c(unique(time_facet_rlevs$value))
            summary_tbl=NULL
            d = NULL
            for (i in unique_fr) {
              #tmp_data <- data[[input$time_facet_c]] == unique_fc[i]
              tmp_data <- data[
                data[[input$time_facet_r]] == i,
                , drop = F
                ]  
              i_m <-paste0('\n', i,'\n')
              d <- summary_table_all(
                tmp_data, row_var = input$time_xticker,
                col_var = input$time_facet_c, val_var = input$time_y,
                col_totals = time_col_totals_n$value,
                name_totals = time_col_totals_name$value,
                n_in_header = TRUE, subj_col = subj_col,
                baseline_name = NULL,
                add_cfb = time_add_cfb$value, cfb_var = chg_col,
                func_list = summary_func,
                caption = time_table_title$value, mdgrp=i_m,
                footnote = time_table_footnote$value,name_N='N',
                rowlabel = '', format = 'rtf'
              )
              d_nams <- row.names(d)
              d_nams <- paste0(i,d_nams)
              row.names(d) <- d_nams
              summary_tbl = rbind(summary_tbl, d)
            }
            rtf_table_wrapper(
              file, summary_tbl, block_break = TRUE,
              nline_block = length(summary_func) + 1,
              caption = time_table_title$value,
              footnote = time_table_footnote$value
            )
          }
          # Summary table with grouping by input$time_group and input$time_facet_c and input$time_facet_r;
          else if(input$time_graph_type == 'Summary table' && !is_blank(input$time_group) && !is_blank(input$time_facet_r) && !is_blank(input$time_facet_c)){
            # subset data into different dataframe by input$time_facet_r
            unique_fr <- c(unique(time_facet_rlevs$value))
            unique_fc <- c(unique(time_facet_clevs$value))
            summary_tbl=NULL
            d = NULL
            for (i in unique_fr) {
              
              summary_tbl_c = NULL
              tmp_data_r <- data[
                data[[input$time_facet_r]] == i,
                , drop = F
                ] 
              for (j in unique_fc) {
                d_c = NULL
                tmp_data_c <- tmp_data_r[
                  tmp_data_r[[input$time_facet_c]] == j,
                  , drop = F
                  ]     
                ij <- paste0('\n',i,",",j,'\n')
                d_c <- summary_table_all(
                  tmp_data_c, row_var = input$time_xticker,
                  col_var = input$time_group, val_var = input$time_y,
                  col_totals = time_col_totals_n$value,
                  name_totals = time_col_totals_name$value,
                  n_in_header = TRUE, subj_col = subj_col,
                  baseline_name = NULL,
                  add_cfb = time_add_cfb$value, cfb_var = chg_col,
                  func_list = summary_func,
                  caption = time_table_title$value, mdgrp=ij,
                  footnote = time_table_footnote$value,name_N='N',
                  rowlabel = '', format = 'rtf'
                )
                d_nams <- row.names(d_c)
                d_nams <- paste0(j,d_nams)
                row.names(d_c) <- d_nams
                summary_tbl_c = rbind(summary_tbl_c,d_c)
              }
              d_nams <- row.names(summary_tbl_c)
              d_nams <- paste0(i,d_nams)
              row.names(summary_tbl_c) <- d_nams
              summary_tbl = rbind(summary_tbl,summary_tbl_c)
            }
            rtf_table_wrapper(
              file, summary_tbl, block_break = TRUE,
              nline_block = length(summary_func) + 1,
              caption = time_table_title$value,
              footnote = time_table_footnote$value
            )
          }
          
        }
      }
    )
    

    #-----------------------------------------------
    # Association graph page
    #-----------------------------------------------
    
    # a reactive data set based on the specification of study and cohort
    ass_data_input <- reactiveValues(value = NULL)
    observe({
        req(file_import$status, input$ass_type)
        data <- subset_data()$data
        if(!is_blank(input$ass_study)) {
            data <- data[data[[study_col]] == input$ass_study, , drop = F]
        }
        if(!is_blank(input$ass_cohort)) {
            data <- data[data[[cohort_col]] == input$ass_cohort, , drop = F]
        }
        ass_data_input$value <- data
    })

    # --- UI widgets shared by all the association plots/table ---
    # radio buttons for choosing association type
    output$ass_type <- renderUI({
        req(file_import$status)
        selectInput('ass_type', tags$h3('Association type'),
                     choices = c('Choose' = '', ass_type_list))
    })
    # a drop-down select list for choosing study
    output$ass_study <- renderUI({
        req(input$ass_type)
        data <- subset_data()$data
        selectInput('ass_study', 'Study', c('Choose'='', unique(data[[study_col]])))
    })
    # a drop-down select list for choosing cohort
    output$ass_cohort <- renderUI({
        req(input$ass_type)
        data <- subset_data()$data
        req(cohort_col %in% names(data))
        selectInput('ass_cohort', 'Cohort',
                    c('Choose'='', unique(data[[cohort_col]])), selected = '')
    })
    
    #-------------------------------------------------
    #   Histogram
    #-------------------------------------------------
    
    # --- UI widgets for producing histogram ---
    output$ass_download_plot_format <- renderUI({
      if(is_blank(input$ass_download_plot_multibmk) || (!is_blank(input$ass_download_plot_multibmk) && length(input$ass_hist_bmk) > 1)) {
        selectInput(
          'ass_download_plot_format', 'Graph format',
          choices = c('Choose' = '', ass_download_plot_formats)
        )
      }
      else if (!is_blank(input$ass_download_plot_multibmk)) {
        ass_download_plot_formats = c('pdf')
        selectInput(
          'ass_download_plot_format', 'Graph format',
          choices = c('Choose' = '', ass_download_plot_formats)
        )
      }
    })
    
    # a selectInput for choosing variable
    output$ass_hist_var <- renderUI({
        req(req(input$ass_type) == 'Histogram')
        choices <- c('Choose' = '', ass_y_list)
        selected <- isolate(ternary(
            is.null(input$ass_hist_var), '', input$ass_hist_var
        ))
        selectInput('ass_hist_var', 'Variable', choices, selected)
    })
    
    # a selectInput for choosing biomarker
    output$ass_hist_bmk <- renderUI({
        req(req(input$ass_type) == 'Histogram')
        data <- ass_data_input$value
        choices <- c('Choose' = '', unique(data[[param_col]]))
        selected <- isolate(ternary(
            is.null(input$ass_hist_bmk), '', input$ass_hist_bmk
        ))
        #selectInput('ass_hist_bmk', 'Biomarker', choices, selected)
        selectizeInput(
         'ass_hist_bmk', 'Biomarker', choices, selected, multiple = TRUE,
         options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Histogram')
        data <- ass_data_input$value
        if(!is_blank(input$ass_hist_visits)) {
            cond_visits <- data[[xlabel_col]] %in% input$ass_hist_visits
            data <- data[cond_visits, , drop = F]
        }
        if(!is_blank(input$ass_hist_group) &&
           !is_blank(ass_hist_group_levs$value)) {
            cond_g <- data[[input$ass_hist_group]] %in%ass_hist_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$ass_hist_facet_r) &&
           !is_blank(ass_hist_facet_rlevs$value)) {
            cond_r <- data[[input$ass_hist_facet_r]] %in%
                ass_hist_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        if(!is_blank(input$ass_hist_facet_c) &&
           !is_blank(ass_hist_facet_clevs$value)) {
            cond_c <- data[[input$ass_hist_facet_c]] %in%
                ass_hist_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- c('Choose' = '', unique(data[[param_col]]))
        selected <- isolate(ternary(
            is.null(input$ass_hist_bmk), '', input$ass_hist_bmk
        ))
        #updateSelectInput(session, 'ass_hist_bmk_x', 'Biomarker',
        #                  choices, selected)
        updateSelectizeInput(
          session, 'ass_hist_bmk_x', 'Biomarker', choices, selected,
          options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    
    # a selectInput for choosing visit time
    output$ass_hist_visits <- renderUI({
        req(req(input$ass_type) == 'Histogram')
        data <- ass_data_input$value
        choices <- c('Choose' = '', unique(data[[xlabel_col]]))
        selected <- isolate(ternary(
            is.null(input$ass_hist_visits), '', input$ass_hist_visits
        ))
        selectizeInput(
            'ass_hist_visits', 'Visit(s)', choices, selected, multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Histogram')
        data <- ass_data_input$value
        if(!is_blank(input$ass_hist_bmk_x)) {
            cond_param <- data[[param_col]] %in% input$ass_hist_bmk_x
            data <- data[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_hist_group) &&
           !is_blank(ass_hist_group_levs$value)) {
            cond_g <- data[[input$ass_hist_group]] %in%ass_hist_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$ass_hist_facet_r) &&
           !is_blank(ass_hist_facet_rlevs$value)) {
            cond_r <- data[[input$ass_hist_facet_r]] %in%
                ass_hist_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        if(!is_blank(input$ass_hist_facet_c) &&
           !is_blank(ass_hist_facet_clevs$value)) {
            cond_c <- data[[input$ass_hist_facet_c]] %in%
                ass_hist_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- c('Choose' = '', unique(data[[xlabel_col]]))
        
        selected <- isolate(ternary(
            !is_blank(input$ass_hist_visits) &&
                all(input$ass_hist_visits %in% choices),
            input$ass_hist_visits, ''
        ))
        updateSelectizeInput(
            session, 'ass_hist_visits', 'Visit(s)', choices, selected,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    
    # select group variable
    output$ass_hist_group <- renderUI({
        req(req(input$ass_type) == 'Histogram')
        data <- ass_data_input$value
        # get categorical column names
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        selected <- isolate(ternary(
            is.null(input$ass_hist_group), '', input$ass_hist_group
        ))
        selectInput('ass_hist_group', 'Group', choices, selected)
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Histogram')
        data <- ass_data_input$value
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        if(!is_blank(input$ass_hist_facet_r))
            choices <- setdiff(choices, input$ass_hist_facet_r)
        if(!is_blank(input$ass_hist_facet_c))
            choices <- setdiff(choices, input$ass_hist_facet_c)
        selected <- isolate(ternary(
            is.null(input$ass_hist_group), '', input$ass_hist_group
        ))
        updateSelectInput(
            session, 'ass_hist_group', 'Group', choices, selected
        )
    })
    
    # select group variable levels
    output$ass_hist_group_levs <- renderUI({
        req(req(input$ass_type) == 'Histogram', input$ass_hist_group)
        data <- ass_data_input$value
        choices <- sort(unique(data[[input$ass_hist_group]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_hist_group_levs) &&
                all(input$ass_hist_group_levs %in% choices),
            input$ass_hist_group_levs, choices
        ))
        selectizeInput(
            'ass_hist_group_levs', input$ass_hist_group, choices,
            selected = selected, multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Histogram',
            input$ass_hist_group)
        data <- ass_data_input$value
        if(!is_blank(input$ass_hist_bmk)) {
            cond_param <- data[[param_col]] %in% input$ass_hist_bmk
            data <- data[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_hist_visits)) {
            cond_visits <- data[[xlabel_col]] %in% input$ass_hist_visits
            data <- data[cond_visits, , drop = F]
        }
        if(!is_blank(input$ass_hist_facet_r) &&
           !is_blank(ass_hist_facet_rlevs$value)) {
            cond_r <- data[[input$ass_hist_facet_r]] %in%
                ass_hist_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        if(!is_blank(input$ass_hist_facet_c) &&
           !is_blank(ass_hist_facet_clevs$value)) {
            cond_c <- data[[input$ass_hist_facet_c]] %in%
                ass_hist_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- sort(unique(data[[input$ass_hist_group]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_hist_group_levs) &&
                all(input$ass_hist_group_levs %in% choices),
            input$ass_hist_group_levs, choices
        ))
        updateSelectizeInput(
            session, 'ass_hist_group_levs', input$ass_hist_group,
            choices, selected,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    ass_hist_group_levs <- reactiveValues(value = NULL)
    observe({
        req(req(input$ass_type) == 'Histogram')
        if(is_blank(input$ass_hist_group)) {
            ass_hist_group_levs$value <- NULL
        } else {
            data <- ass_data_input$value
            ass_hist_group_levs$value <- sort(unique(
                data[[input$ass_hist_group]]
            ))
        }
    })
    observe({ ass_hist_group_levs$value <- input$ass_hist_group_levs })
    
    # select facet row variable
    output$ass_hist_facet_r <- renderUI({
        req(req(input$ass_type) == 'Histogram')
        data <- ass_data_input$value
        # get categorical column names
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        selected <- isolate(ternary(
            is.null(input$ass_hist_facet_r), '', input$ass_hist_facet_r
        ))
        selectInput('ass_hist_facet_r', 'Facet row', choices, selected)
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Histogram')
        data <- ass_data_input$value
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        if(!is_blank(input$ass_hist_group))
            choices <- setdiff(choices, input$ass_hist_group)
        if(!is_blank(input$ass_hist_facet_c))
            choices <- setdiff(choices, input$ass_hist_facet_c)
        selected <- isolate(ternary(
            is.null(input$ass_hist_facet_r), '', input$ass_hist_facet_r
        ))
        updateSelectInput(
            session, 'ass_hist_facet_r', 'Facet row', choices, selected
        )
    })
    
    # select facet row variable levels
    output$ass_hist_facet_rlevs <- renderUI({
        req(req(input$ass_type) == 'Histogram', input$ass_hist_facet_r)
        data <- ass_data_input$value
        choices <- sort(unique(data[[input$ass_hist_facet_r]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_hist_facet_rlevs) &&
                all(input$ass_hist_facet_rlevs %in% choices),
            input$ass_hist_facet_rlevs, choices
        ))
        selectizeInput(
            'ass_hist_facet_rlevs', input$ass_hist_facet_r, choices,
            selected = selected, multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Histogram',
            input$ass_hist_facet_r)
        data <- ass_data_input$value
        if(!is_blank(input$ass_hist_bmk)) {
            cond_param <- data[[param_col]] %in% input$ass_hist_bmk
            data <- data[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_hist_visits)) {
            cond_visits <- data[[xlabel_col]] %in% input$ass_hist_visits
            data <- data[cond_visits, , drop = F]
        }
        if(!is_blank(input$ass_hist_group) &&
           !is_blank(ass_hist_group_levs$value)) {
            cond_g <- data[[input$ass_hist_group]] %in%ass_hist_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$ass_hist_facet_c) &&
           !is_blank(ass_hist_facet_clevs$value)) {
            cond_c <- data[[input$ass_hist_facet_c]] %in%
                ass_hist_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- sort(unique(data[[input$ass_hist_facet_r]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_hist_facet_rlevs) &&
                all(input$ass_hist_facet_rlevs %in% choices),
            input$ass_hist_facet_rlevs, choices
        ))
        updateSelectizeInput(
            session, 'ass_hist_facet_rlevs', input$ass_hist_facet_r,
            choices, selected,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    ass_hist_facet_rlevs <- reactiveValues(value = NULL)
    observe({
        req(req(input$ass_type) == 'Histogram')
        if(is_blank(input$ass_hist_facet_r)) {
            ass_hist_facet_rlevs$value <- NULL
        } else {
            data <- ass_data_input$value
            ass_hist_facet_rlevs$value <- sort(unique(
                data[[input$ass_hist_facet_r]]
            ))
        }
    })
    observe({ ass_hist_facet_rlevs$value <- input$ass_hist_facet_rlevs })
    
    # select facet column variable
    output$ass_hist_facet_c <- renderUI({
        req(req(input$ass_type) == 'Histogram')
        data <- ass_data_input$value
        # get categorical column names
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        selected <- isolate(ternary(
            is.null(input$ass_hist_facet_c), '', input$ass_hist_facet_c
        ))
        selectInput('ass_hist_facet_c', 'Facet column', choices, selected)
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Histogram')
        data <- ass_data_input$value
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        if(!is_blank(input$ass_hist_group))
            choices <- setdiff(choices, input$ass_hist_group)
        if(!is_blank(input$ass_hist_facet_r))
            choices <- setdiff(choices, input$ass_hist_facet_r)
        selected <- isolate(ternary(
            is.null(input$ass_hist_facet_c), '', input$ass_hist_facet_c
        ))
        updateSelectInput(
            session, 'ass_hist_facet_c', 'Facet column', choices, selected
        )
    })
    
    # select facet column variable levels
    output$ass_hist_facet_clevs <- renderUI({
        req(req(input$ass_type) == 'Histogram', input$ass_hist_facet_c)
        data <- ass_data_input$value
        choices <- sort(unique(data[[input$ass_hist_facet_c]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_hist_facet_clevs) &&
                all(input$ass_hist_facet_clevs %in% choices),
            input$ass_hist_facet_clevs, choices
        ))
        selectizeInput(
            'ass_hist_facet_clevs', input$ass_hist_facet_c, choices,
            selected = selected, multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Histogram',
            input$ass_hist_facet_c)
        data <- ass_data_input$value
        if(!is_blank(input$ass_hist_bmk)) {
            cond_param <- data[[param_col]] %in% input$ass_hist_bmk
            data <- data[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_hist_visits)) {
            cond_visits <- data[[xlabel_col]] %in% input$ass_hist_visits
            data <- data[cond_visits, , drop = F]
        }
        if(!is_blank(input$ass_hist_group) &&
           !is_blank(ass_hist_group_levs$value)) {
            cond_g <- data[[input$ass_hist_group]] %in%
                ass_hist_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$ass_hist_facet_r) &&
           !is_blank(ass_hist_facet_rlevs$value)) {
            cond_r <- data[[input$ass_hist_facet_r]] %in%
                ass_hist_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        choices <- sort(unique(data[[input$ass_hist_facet_c]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_hist_facet_clevs) &&
                all(input$ass_hist_facet_clevs %in% choices),
            input$ass_hist_facet_clevs, choices
        ))
        updateSelectizeInput(
            session, 'ass_hist_facet_clevs', input$ass_hist_facet_c,
            choices, selected,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    ass_hist_facet_clevs <- reactiveValues(value = NULL)
    observe({
        req(req(input$ass_type) == 'Histogram')
        if(is_blank(input$ass_hist_facet_c)) {
            ass_hist_facet_clevs$value <- NULL
        } else {
            data <- ass_data_input$value
            ass_hist_facet_clevs$value <- sort(unique(
                data[[input$ass_hist_facet_c]]
            ))
        }
    })
    observe({ ass_hist_facet_clevs$value <- input$ass_hist_facet_clevs })
    
    # download option for selection of multi biomarkers 
    output$ass_download_plot_multibmk <- renderUI({
      req(req(input$ass_type) == 'Histogram' || req(input$ass_type) == 'Box plot')

      data <- ass_data_input$value
      choices <- c('Choose' = '', unique(data[[param_col]]))
      if (input$ass_type == 'Histogram'){
        if (length(input$ass_hist_bmk) > 1) req(FALSE)
        selected <- isolate(ternary(
          is.null(input$ass_hist_bmk), '', input$ass_hist_bmk
        ))
        #selectInput('ass_hist_bmk', 'Biomarker', choices, selected)
        selectizeInput(
          'ass_download_plot_multibmk', 'Multi Biomarker', choices, multiple = TRUE,
          options = list(plugins = list('drag_drop','remove_button'))
        )
      }
      else if (input$ass_type == 'Box plot'){
        if (length(input$ass_box_bmk) > 1) req(FALSE)
        selected <- isolate(ternary(
          is.null(input$ass_box_bmk), '', input$ass_box_bmk
        ))
        #selectInput('ass_hist_bmk', 'Biomarker', choices, selected)
        selectizeInput(
          'ass_download_plot_multibmk', 'Multi Biomarker', choices, multiple = TRUE,
          options = list(plugins = list('drag_drop','remove_button'))
        )
      }
      
    })
    
    
    # --- UI widgets for refining histogram ---
    
    # choose histogram, density curve, or both to plot
    output$ass_hist_geom <- renderUI({
        req(ass_hist_show$value)
        choices <- c('Histogram', 'Density curve', 'Histogram + density curve')
        selected <- isolate(ternary(
            is.null(input$ass_hist_geom), 'Histogram', input$ass_hist_geom
        ))
        radioButtons('ass_hist_geom', 'Plot type', choices)
    })
    
    # choose log scale
    output$ass_hist_log_x <- renderUI({
        req(ass_hist_show$value)
        value <- isolate(ternary(
            is.null(input$ass_hist_log_x), FALSE, input$ass_hist_log_x
        ))
        checkboxInput('ass_hist_log_x', 'Log scale', value)
    })
    
    # x-axis label
    output$ass_hist_xlab <- renderUI({
        req(ass_hist_show$value)
        value <- paste('{BiomarkerY} at {VisitsY}')
        textInput('ass_hist_xlab', 'X-axis label', value)
    })
    ass_hist_xlab <- reactiveValues(value = NULL)
    observe({
        if(isTRUE(ass_hist_show$value)) {
            value <- value <- paste('{BiomarkerY} at {VisitsY}')
            ass_hist_xlab$value <- value
        } else ass_hist_xlab$value <- NULL
    })
    observe({ ass_hist_xlab$value <- input$ass_hist_xlab })
    
    # y-axis label
    output$ass_hist_ylab <- renderUI({
        req(ass_hist_show$value)
        textInput('ass_hist_ylab', 'Y-axis label', 'Frequency')
    })
    ass_hist_ylab <- reactiveValues(value = 'Frequency')
    observe({ ass_hist_ylab$value <- input$ass_hist_ylab })
    
    # plot title
    output$ass_hist_title <- renderUI({
        req(ass_hist_show$value)
        value <- ifelse(!is_blank(input$ass_study), 'Study: {Study}', '')
        if(!is_blank(input$ass_cohort))
            value <- paste(value, paste('{Cohort} cohort'), sep = '\n')
        value <- trimws(value)
        textAreaInput('ass_hist_title', 'Plot title', value)
    })
    ass_hist_title <- reactiveValues(value = NULL)
    observe({
        if(isTRUE(ass_hist_show$value)) {
            value <- ifelse(!is_blank(input$ass_study), 'Study: {Study}', '')
            if(!is_blank(input$ass_cohort))
                value <- paste(value, paste('{Cohort} cohort'), sep = '\n')
            value <- trimws(value)
            ass_hist_title$value <- value  
        } else ass_hist_title$value <- NULL
    })
    observe({ ass_hist_title$value <- input$ass_hist_title })
    
    # add footnote
    output$ass_hist_footnote_in <- renderUI({
        req(ass_hist_show$value)
        textAreaInput('ass_hist_footnote_in', 'Plot footnote', '')
    })
    ass_hist_footnote_in <- reactiveValues(value = '')
    observe({
        value <- ''
        if(!is_blank(input$ass_hist_footnote_in)) {
            value <- trimws(paste(
                value, input$ass_hist_footnote_in, sep = '\n'
            ))
        }
        ass_hist_footnote_in$value <- value
    })
    observe({ ass_hist_footnote_in$value <- input$ass_hist_footnote_in })
    
    # --- Histogram plot output ---
    
    # histogram shown status
    ass_hist_show <- reactiveValues(value = FALSE)
    observe({
        data <- ass_data_input$value
        if(!is_blank(data) && nrow(data) > 0 &&
           !is_blank(input$ass_type) &&
           input$ass_type == 'Histogram' &&
           !is_blank(input$ass_hist_var) &&
           !is_blank(input$ass_hist_bmk) &&
           !is_blank(input$ass_hist_visits)) {
            ass_hist_show$value = TRUE
        } else ass_hist_show$value = FALSE
    })
    
    ass_hist_data <- reactive({
        req(ass_hist_show$value)
        data <- ass_data_input$value
        if(!is_blank(input$ass_hist_bmk) && is_blank(input$ass_download_plot_multibmk)) {
            cond_param <- data[[param_col]] %in% input$ass_hist_bmk
            data <- data[cond_param, , drop = F]
        }
        else if(!is_blank(input$ass_download_plot_multibmk) && length(input$ass_hist_bmk) == 1) {
          cond_param <- data[[param_col]] %in% input$ass_download_plot_multibmk
          data <- data[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_hist_visits)) {
            cond_visits <- data[[xlabel_col]] %in% input$ass_hist_visits
            data <- data[cond_visits, , drop = F]
        }
        if(!is_blank(input$ass_hist_group) &&
           !is_blank(ass_hist_group_levs$value)) {
            cond_g <- data[[input$ass_hist_group]] %in%ass_hist_group_levs$value
            data <- data[cond_g, , drop = F]
            data[[input$ass_hist_group]] <- factor(
                data[[input$ass_hist_group]],
                levels = ass_hist_group_levs$value
            )
        }
        if(!is_blank(input$ass_hist_facet_r) &&
           !is_blank(ass_hist_facet_rlevs$value)) {
            cond_r <- data[[input$ass_hist_facet_r]] %in%
                ass_hist_facet_rlevs$value
            data <- data[cond_r, , drop = F]
            data[[input$ass_hist_facet_r]] <- factor(
                data[[input$ass_hist_facet_r]],
                levels = ass_hist_facet_rlevs$value
            )
        }
        if(!is_blank(input$ass_hist_facet_c) &&
           !is_blank(ass_hist_facet_clevs$value)) {
            cond_c <- data[[input$ass_hist_facet_c]] %in%
                ass_hist_facet_clevs$value
            data <- data[cond_c, , drop = F]
            data[[input$ass_hist_facet_c]] <- factor(
                data[[input$ass_hist_facet_c]],
                levels = ass_hist_facet_clevs$value
            )
        }
        return(data)
    })
    
    ass_hist_plot <- reactive({
        req(ass_hist_data(),
            !is_blank(input$ass_hist_geom),
            !is.null(input$ass_hist_log_x))
        data <- ass_hist_data()
        if(length(ass_hist_group_levs$value) <= 1)
            all_colors = gg_color_hue(1)
        else all_colors = gg_color_hue(length(ass_hist_group_levs$value))
        if(!is_blank(input$ass_hist_facet_r)) {
            data[[input$ass_hist_facet_r]] <- paste0(
                input$ass_hist_facet_r, ': ', data[[input$ass_hist_facet_r]]
            )
        }
        if(!is_blank(input$ass_hist_facet_c)) {
            data[[input$ass_hist_facet_c]] <- paste0(
                input$ass_hist_facet_c, ': ', data[[input$ass_hist_facet_c]]
            )
        }
        
        #x_lab <- r_format(ass_hist_xlab$value, tnf$ass_current)
        #y_lab <- r_format(ass_hist_ylab$value, tnf$ass_current)
        #plot_title <- r_format(ass_hist_title$value, tnf$ass_current)
        
        x_lab <- ass_hist_xlab$value
        y_lab <- ass_hist_ylab$value
        
        plot_title <- ass_hist_title$value

        plot_ <- gg_wrapper(
            data = data,
            aes_string(x = paste0('`', input$ass_hist_var, '`')),
            facet_r = input$ass_hist_facet_r, facet_c = input$ass_hist_facet_c,
            x_lab = x_lab, y_lab = y_lab, title = plot_title,
            x_log = isTRUE(input$ass_hist_log_x),
            color_var = input$ass_hist_group, all_colors = all_colors,
            fill_var = input$ass_hist_group, all_fills = all_colors,
            bw_theme = TRUE, grids = 'on'
        )
        if(input$ass_hist_geom == 'Histogram') {
            plot_ <- plot_ + geom_histogram(aes(y = ..density.., colour = NULL),
                                            alpha = .5, position = 'identity')
        } else if(input$ass_hist_geom == 'Density curve') {
            plot_ <- plot_ + geom_density(aes(fill = NULL))
        } else {
            plot_ <- plot_ +
                geom_histogram(aes(y = ..density.., colour = NULL),
                               alpha = .5, position = 'identity') +
                
                geom_density(alpha = .5, aes(fill = NULL))
        }
        return(plot_)
    })
    
    ass_hist_m_plot <- reactive({
      req(ass_hist_data(),
          !is_blank(input$ass_hist_geom),
          !is.null(input$ass_hist_log_x))
      data <- ass_hist_data()
      
      ## object to save all the data frame for each biomarker;
      multi_bm_obj <- list()
      unique_bmk <- c(unique(input$ass_download_plot_multibmk))
      
      for (i in unique_bmk){
        multi_bm1 <- data[data[[param_col]] == i, , drop = F]
      
          if(length(ass_hist_group_levs$value) <= 1)
            all_colors = gg_color_hue(1)
          else all_colors = gg_color_hue(length(ass_hist_group_levs$value))
          if(!is_blank(input$ass_hist_facet_r)) {
            multi_bm1[[input$ass_hist_facet_r]] <- paste0(
              input$ass_hist_facet_r, ': ',multi_bm1[[input$ass_hist_facet_r]]
            )
          }
          if(!is_blank(input$ass_hist_facet_c)) {
            multi_bm1[[input$ass_hist_facet_c]] <- paste0(
              input$ass_hist_facet_c, ': ', multi_bm1[[input$ass_hist_facet_c]]
            )
          }
          
          #x_lab <- r_format(ass_hist_xlab$value, tnf$ass_current)
          #y_lab <- r_format(ass_hist_ylab$value, tnf$ass_current)
          #plot_title <- r_format(ass_hist_title$value, tnf$ass_current)
          
          x_lab <- ass_hist_xlab$value
          y_lab <- ass_hist_ylab$value
          
          plot_title_tmp <- ass_hist_title$value
          if (plot_title_tmp != 'Study: {Study}'){
            plot_title <- paste(ass_hist_title$value, '\n', i)
          }
          else if (plot_title_tmp == 'Study: {Study}'){
            plot_title <- i
          }
          
          
          
          plot_ <- gg_wrapper(
            data = multi_bm1,
            aes_string(x = paste0('`', input$ass_hist_var, '`')),
            facet_r = input$ass_hist_facet_r, facet_c = input$ass_hist_facet_c,
            x_lab = x_lab, y_lab = y_lab, title = plot_title,
            x_log = isTRUE(input$ass_hist_log_x),
            color_var = input$ass_hist_group, all_colors = all_colors,
            fill_var = input$ass_hist_group, all_fills = all_colors,
            bw_theme = TRUE, grids = 'on'
          )
          if(input$ass_hist_geom == 'Histogram') {
            plot_ <- plot_ + geom_histogram(aes(y = ..density.., colour = NULL),
                                            alpha = .5, position = 'identity')
          } else if(input$ass_hist_geom == 'Density curve') {
            plot_ <- plot_ + geom_density(aes(fill = NULL))
          } else {
            plot_ <- plot_ +
              geom_histogram(aes(y = ..density.., colour = NULL),
                             alpha = .5, position = 'identity') +
              
              geom_density(alpha = .5, aes(fill = NULL))
          }
          multi_bm_obj[[i]] <- plot_
      } 
      return(multi_bm_obj)
    })    
    
    
    #####################################################################
    # produce the Association Histogram summary table with the graph (GH)
    #####################################################################
    ass_hist_summary <- reactive({
      req(ass_hist_data(),
          !is_blank(input$ass_hist_geom),
          !is.null(input$ass_hist_log_x))
      data <- ass_hist_data()
      
      #data <- ass_hist_data$value
      
      dgt <- 2
      summary_func <- c(
        'N' = n_nna,
        'Mean (SD)' = partial(mean_sd_str, digits = dgt),
        '%CV' = partial(coeff_var_str, digits = dgt),
        'Median' = partial(median_str, digits = dgt),
        'Q1, Q3' = partial(q1_q3_str, digits = dgt),
        'Min, Max' = partial(min_max_str, digits = dgt)
      )
      if(isTRUE(input$time_to_log)) {
        summary_func <- c(
          summary_func,
          'Geom Mean (%CV)' = partial(geo_mean_cv_str, digits = dgt),
          'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
        )
      }
      
      data <- mutate(data, hist_sum_all = seq(0,0,nrow(data)-1))
      # Summary table with grouping;
      if(input$ass_type == 'Histogram' && is_blank(input$ass_hist_group) && is_blank(input$ass_hist_facet_r) && is_blank(input$ass_hist_facet_c)){
        summary_tbl <- summary_table_all(
          data, row_var = 'hist_sum_all',row_names = ' ',
          col_var = , val_var = input$ass_hist_var,
          col_totals = ,
          name_totals = ,
          n_in_header = FALSE, subj_col = subj_col,
          baseline_name = NULL,
          add_cfb = , cfb_var = ,
          func_list = summary_func,
          caption = '',
          footnote = '',
          rowlabel = '', format = 'html'
        )
      }
      # Summary table with grouping by input$ass_hist_group;
      else if(input$ass_type == 'Histogram' && !is_blank(input$ass_hist_group) && is_blank(input$ass_hist_facet_r) && is_blank(input$ass_hist_facet_c)){
        summary_tbl <- summary_table_all(
          data, row_var = 'hist_sum_all',row_names = ' ',
          col_var = input$ass_hist_group, val_var = input$ass_hist_var,
          col_totals = ,
          name_totals = ,
          n_in_header = FALSE, subj_col = subj_col,
          baseline_name = NULL,
          add_cfb = , cfb_var =,
          func_list = summary_func,
          caption = '',
          footnote = '',
          rowlabel = '', format = 'html'
        )
      }
      # Summary table with grouping by input$ass_row;
      else if(input$ass_type == 'Histogram' && is_blank(input$ass_hist_group) && !is_blank(input$ass_hist_facet_r) && is_blank(input$ass_hist_facet_c)){
        summary_tbl <- summary_table_all(
          data, row_var = 'hist_sum_all',row_names = ' ',
          col_var = input$ass_hist_facet_r, val_var = input$ass_hist_var,
          col_totals = ,
          name_totals = ,
          n_in_header = FALSE, subj_col = subj_col,
          baseline_name = NULL,
          add_cfb = , cfb_var =,
          func_list = summary_func,
          caption = '',
          footnote = '',
          rowlabel = '', format = 'html'
        )
      }
      # Summary table with grouping by input$ass_column;
      else if(input$ass_type == 'Histogram' && is_blank(input$ass_hist_group) && is_blank(input$ass_hist_facet_r) && !is_blank(input$ass_hist_facet_c)){
        summary_tbl <- summary_table_all(
          data, row_var = 'hist_sum_all',row_names = ' ',
          col_var = input$ass_hist_facet_c, val_var = input$ass_hist_var,
          col_totals = ,
          name_totals = ,
          n_in_header = FALSE, subj_col = subj_col,
          baseline_name = NULL,
          add_cfb = , cfb_var =,
          func_list = summary_func,
          caption = '',
          footnote = '',
          rowlabel = '', format = 'html'
        )
      }
      
      # Summary table with grouping by input$ass_hist_group and input$ass_hist_facet_r;
      else if(input$ass_type == 'Histogram' && !is_blank(input$ass_hist_group) && !is_blank(input$ass_hist_facet_r) && is_blank(input$ass_hist_facet_c)){
        # subset data into different dataframe by input$ass_hist_facet_r
        unique_fr <- c(unique(ass_hist_facet_rlevs$value))
        summary_tbl=NULL
        d = NULL
        for (i in unique_fr) {
          #tmp_data <- data[[input$ass_hist_facet_c]] == unique_fc[i]
          tmp_data <- data[
            data[[input$ass_hist_facet_r]] == i,
            , drop = F
            ]  
          d <- summary_table_all(
            tmp_data, row_var = 'hist_sum_all',row_names = ' ',
            col_var = input$ass_hist_group, val_var = input$ass_hist_var,
            col_totals = ,
            name_totals = ,
            n_in_header = FALSE, subj_col = subj_col,
            baseline_name = NULL,
            add_cfb = , cfb_var =,
            func_list = summary_func,
            caption = '',
            footnote = '',
            rowlabel = '', format = 'html'
          )
          summary_tbl = rbind(summary_tbl,i,d)
        }
      }
      
      # Summary table with grouping by input$ass_hist_group and input$ass_hist_facet_c;
      else if(input$ass_type == 'Histogram' && !is_blank(input$ass_hist_group) && is_blank(input$ass_hist_facet_r) && !is_blank(input$ass_hist_facet_c)){
        # subset data into different dataframe by input$ass_hist_facet_r
        #facet_row_levls <- unique_na(data[[input$ass_hist_facet_r]])
        unique_fc <- c(unique(ass_hist_facet_clevs$value))
        #facet_col_dim = unique_fc
        summary_tbl=NULL
        d = NULL
        for (i in unique_fc) {
          #tmp_data <- data[[input$ass_hist_facet_c]] == unique_fc[i]
          tmp_data <- data[
            data[[input$ass_hist_facet_c]] == i,
            , drop = F
            ]  
          d <- summary_table_all(
            tmp_data, row_var = 'hist_sum_all',row_names = ' ',
            col_var = input$ass_hist_group, val_var = input$ass_hist_var,
            col_totals = ,
            name_totals = ,
            n_in_header = FALSE, subj_col = subj_col,
            baseline_name = NULL,
            add_cfb = , cfb_var =,
            func_list = summary_func,
            caption = '',
            footnote = '',
            rowlabel = '', format = 'html'
          )
          summary_tbl = rbind(summary_tbl,i,d)
        }
      }
      
      # Summary table with grouping by input$ass_hist_facet_c and input$ass_hist_facet_r;
      else if(input$ass_type == 'Histogram' && is_blank(input$ass_hist_group) && !is_blank(input$ass_hist_facet_r) && !is_blank(input$ass_hist_facet_c)){
        # subset data into different dataframe by input$ass_hist_facet_r
        unique_fr <- c(unique(ass_hist_facet_rlevs$value))
        summary_tbl=NULL
        d = NULL
        for (i in unique_fr) {
          #tmp_data <- data[[input$ass_hist_facet_c]] == unique_fc[i]
          tmp_data <- data[
            data[[input$ass_hist_facet_r]] == i,
            , drop = F
            ]  
          d <- summary_table_all(
            tmp_data, row_var = 'hist_sum_all',row_names = ' ',
            col_var = input$ass_hist_facet_c, val_var = input$ass_hist_var,
            col_totals = ,
            name_totals = ,
            n_in_header = FALSE, subj_col = subj_col,
            baseline_name = NULL,
            add_cfb = , cfb_var =,
            func_list = summary_func,
            caption = '',
            footnote = '',
            rowlabel = '', format = 'html'
          )
          summary_tbl = rbind(summary_tbl,i,d)
        }
      }
      # Summary table with grouping by input$ass_hist_group and input$ass_hist_facet_c and input$ass_hist_facet_r;
      else if(input$ass_type == 'Histogram' && !is_blank(input$ass_hist_group) && !is_blank(input$ass_hist_facet_r) && !is_blank(input$ass_hist_facet_c)){
        # subset data into different dataframe by input$ass_hist_facet_r
        unique_fr <- c(unique(ass_hist_facet_rlevs$value))
        unique_fc <- c(unique(ass_hist_facet_clevs$value))
        summary_tbl=NULL
        d = NULL
        for (i in unique_fr) {
          
          summary_tbl_c = NULL
          
          tmp_data_r <- data[
            data[[input$ass_hist_facet_r]] == i,
            , drop = F
            ] 
          for (j in unique_fc) {
            d_c = NULL
            tmp_data_c <- tmp_data_r[
              tmp_data_r[[input$ass_hist_facet_c]] == j,
              , drop = F
              ]    
            d_c <- summary_table_all(
              tmp_data_c, row_var = 'hist_sum_all',row_names = ' ',
              col_var = input$ass_hist_group, val_var = input$ass_hist_var,
              col_totals = ,
              name_totals = ,
              n_in_header = FALSE, subj_col = subj_col,
              baseline_name = NULL,
              add_cfb = , cfb_var =,
              func_list = summary_func,
              caption = '',
              footnote = '',
              rowlabel = '', format = 'html'
            )
            ij <- c(",")
            summary_tbl_c = rbind(summary_tbl_c,i,ij,j, d_c)
          }
          
          summary_tbl = rbind(summary_tbl,summary_tbl_c)
        }
      }
      return(summary_tbl)
    })
    
    
    output$ass_hist_summary <- renderUI({
      req(ass_hist_show$value)
      HTML(ass_hist_summary())
    })        
    
    # scatter plot output
    output$ass_hist_output <- renderUI({
        req(ass_hist_show$value)
        tags$div(
            style = 'position:relative',
            tabsetPanel(
              tabPanel("Graph",
                plotOutput('ass_hist_plot'),
                uiOutput('ass_hist_footnote_out')
              ),
              tabPanel("Association Histogram Summary Table",
                       htmlOutput('ass_hist_summary')
                       #plotOutput('time_summary')
                       
              ) 
            )
        )
    })
    
    # draw scatter plot
    output$ass_hist_plot <- renderPlot({
        req(ass_hist_plot())
        ass_hist_plot()
    })
    
    # -- histogram footnote
    output$ass_hist_footnote_out <- renderUI({
        req(!is.null(ass_hist_footnote_in$value))
        value <- trimws(paste(
            unlist(strsplit(ass_hist_footnote_in$value, '\n')),
            collapse = '<br/>'
        ))
        HTML(value)
    })
    
    
    # # a selectInput for choosing variable 
    # output$ass_hist_var <- renderUI({
    #     req(input$ass_type)
    #     if(input$ass_type != 'Histogram') return()
    #     data <- ass_data_input$value
    #     choices <- col_continuity()$continuous
    #     selectInput('ass_hist_var', 'Variable', c('Choose' = '', choices))
    # })
    # # a selectInput for choosing visit time
    # output$ass_hist_time <- renderUI({
    #     req(input$ass_type)
    #     if(input$ass_type != 'Histogram') return()
    #     data <- ass_data_input$value
    #     selectizeInput(
    #         'ass_hist_time', 'Visit(s)',
    #         c('Choose'='', unique(data[[xlabel_col]])), multiple = TRUE,
    #         options = list(plugins = list('drag_drop','remove_button'))
    #     )
    # })
    # # a checkbox for deciding whether the variable is biomarker dependent
    # output$ass_hist_bmk_depend <- renderUI({
    #     req(input$ass_type)
    #     if(input$ass_type != 'Histogram') return()
    #     req(input$ass_hist_var)
    #     checkboxInput(
    #         'ass_hist_bmk_depend', label = 'Is it biomarker-dependent?', value = F
    #     )
    # })
    # # a inputSelect for choosing biomarker
    # output$ass_hist_bmk <- renderUI({
    #     req(input$ass_type)
    #     if(input$ass_type != 'Histogram') return()
    #     req(input$ass_hist_bmk_depend)
    #     names_bmk <- unique(ass_data_input$value[[param_col]])
    #     selectInput('ass_hist_bmk', 'Biomarker', c('Choose'='', names_bmk))
    # })
    # # a checkbox for specifying log-scale
    # output$ass_hist_to_log <- renderUI({
    #     req(input$ass_type)
    #     if(input$ass_type != 'Histogram') return()
    #     checkboxInput(
    #         'ass_hist_to_log', label = 'Log scale', value = F
    #     )
    # })
    # # --- Output for histogram ---
    # ass_hist_data <- reactive({
    #     req(ass_hist_show$value)
    #     data <- ass_data_input$value
    #     data <- data[data[[xlabel_col]] %in% input$ass_hist_time, , drop = F]
    #     if(is_blank(input$ass_hist_bmk)) {
    #         if(is_blank(input$ass_cohort)) {
    #             data <- group_by_(data, study_col, subj_col, xvar_col)
    #         } else {
    #             data <- group_by_(data, study_col, cohort_col, subj_col, xvar_col)
    #         }
    #         data <- filter(data, row_number() == 1)
    #     } else {
    #         data <- data[data[[param_col]] == input$ass_hist_bmk, , drop = F]
    #     }
    #     return(data)
    # })
    # ass_hist_attr <- reactive({
    #     data <- ass_hist_data()
    #     var_unit <- ''
    #     if(!is_blank(input$ass_hist_bmk)) {
    #         var_unit <- bmk_value_name_dict[[input$ass_hist_var]]
    #         has_unit <- input$ass_hist_var %in% c('AVAL', 'CHG')
    #         if(has_unit && bmk_unit_col %in% names(data)) {
    #             var_unit <- smart_paste(
    #                 var_unit, unique(data[[bmk_unit_col]]), sep = ', '
    #             )
    #         }
    #     }
    #     var_name <- ifelse(
    #         !is_blank(input$ass_hist_bmk), input$ass_hist_bmk, input$ass_hist_var
    #     )
    #     main <- ifelse(
    #         !is_blank(input$ass_study), paste0('Study: ', input$ass_study), ''
    #     )
    #     if(length(input$ass_hist_time) == 1)
    #         main <- paste(main, paste(input$ass_hist_time, var_name), sep = '\n')
    #     if(!is_blank(input$ass_cohort))
    #         main <- paste(main, paste(input$ass_cohort, 'cohort'), sep = '\n')
    #     main <- trimws(main)
    #     return(list(
    #         var_unit = var_unit, var_name = var_name, main = main
    #     ))
    # })
    # # A reactive value linked to the status of histogram
    # ass_hist_show <- reactiveValues(value = FALSE)
    # observe({
    #     if(any(is_blank(input$ass_hist_var), is_blank(input$ass_hist_time))) {
    #         ass_hist_show$value <- FALSE
    #     } else {
    #         if(isTRUE(input$ass_hist_bmk_depend) &&
    #            is_blank(input$ass_hist_bmk)) {
    #             ass_hist_show$value <- FALSE
    #         } else ass_hist_show$value <- TRUE
    #     }
    # })
    # observe({
    #     input$ass_type
    #     ass_hist_show$value <- FALSE
    # })
    # # Draw histogram
    # output$ass_hist_output <- renderUI({
    #     req(ass_hist_show$value)
    #     plotOutput('ass_histogram')
    # })
    # output$ass_histogram <- renderPlot({
    #     data <- ass_hist_data()
    #     plot_attr <- ass_hist_attr()
    #     histogram_plot(
    #         data, input$ass_hist_var, var_unit = plot_attr$var_unit,
    #         to_log = input$ass_hist_to_log, visit = input$ass_hist_time,
    #         var_name = plot_attr$var_name, main = plot_attr$main,
    #         add_density_curve = TRUE
    #     )
    # })
    
    
    #-------------------------------------------------
    #   Scatter plot
    #-------------------------------------------------
    
    # --- UI widgets for producing scatter plot ---
    
    # a selectInput for choosing x variable
    output$ass_scatter_x <- renderUI({
        req(req(input$ass_type) == 'Scatter plot')
        choices <- c('Choose' = '', ass_y_list)
        selected <- isolate(ternary(
            is.null(input$ass_scatter_x), '', input$ass_scatter_x
        ))
        selectInput('ass_scatter_x', 'X', choices, selected)
    })
    
    # a selectInput for choosing biomarker for X
    output$ass_scatter_bmk_x <- renderUI({
        req(req(input$ass_type) == 'Scatter plot')
        data <- ass_data_input$value
        choices <- c('Choose' = '', unique(data[[param_col]]))
        selected <- isolate(ternary(
            is.null(input$ass_scatter_bmk_x), '', input$ass_scatter_bmk_x
        ))
        selectInput('ass_scatter_bmk_x', 'Biomarker X', choices, selected)
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Scatter plot')
        data <- ass_data_input$value
        if(!is_blank(input$ass_scatter_visits_x)) {
            cond_visits <- data[[xlabel_col]] %in% input$ass_scatter_visits_x
            data <- data[cond_visits, , drop = F]
        }
        if(!is_blank(input$ass_scatter_group) &&
           !is_blank(ass_scatter_group_levs$value)) {
            cond_g <- data[[input$ass_scatter_group]] %in%
                ass_scatter_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$ass_scatter_facet_r) &&
           !is_blank(ass_scatter_facet_rlevs$value)) {
            cond_r <- data[[input$ass_scatter_facet_r]] %in%
                ass_scatter_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        if(!is_blank(input$ass_scatter_facet_c) &&
           !is_blank(ass_scatter_facet_clevs$value)) {
            cond_c <- data[[input$ass_scatter_facet_c]] %in%
                ass_scatter_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- c('Choose' = '', unique(data[[param_col]]))
        selected <- isolate(ternary(
            is.null(input$ass_scatter_bmk_x), '', input$ass_scatter_bmk_x
        ))
        updateSelectInput(session, 'ass_scatter_bmk_x', 'Biomarker X',
                          choices, selected)
    })
    
    # a selectInput for choosing visit time for X
    output$ass_scatter_visits_x <- renderUI({
        req(req(input$ass_type) == 'Scatter plot')
        data <- ass_data_input$value
        choices <- c('Choose' = '', unique(data[[xlabel_col]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_scatter_visits_x) &&
                all(input$ass_scatter_visits_x %in% choices),
            input$ass_scatter_visits_x, ''
        ))
        selectizeInput(
            'ass_scatter_visits_x', 'Visit(s) X', choices, selected,
            multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Scatter plot')
        data <- ass_data_input$value
        if(!is_blank(input$ass_scatter_bmk_x)) {
            cond_param <- data[[param_col]] %in% input$ass_scatter_bmk_x
            data <- data[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_scatter_group) &&
           !is_blank(ass_scatter_group_levs$value)) {
            cond_g <- data[[input$ass_scatter_group]] %in%
                ass_scatter_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$ass_scatter_facet_r) &&
           !is_blank(ass_scatter_facet_rlevs$value)) {
            cond_r <- data[[input$ass_scatter_facet_r]] %in%
                ass_scatter_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        if(!is_blank(input$ass_scatter_facet_c) &&
           !is_blank(ass_scatter_facet_clevs$value)) {
            cond_c <- data[[input$ass_scatter_facet_c]] %in%
                ass_scatter_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- c('Choose' = '', unique(data[[xlabel_col]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_scatter_visits_x) &&
                all(input$ass_scatter_visits_x %in% choices),
            input$ass_scatter_visits_x, ''
        ))
        updateSelectizeInput(
            session, 'ass_scatter_visits_x', 'Visit(s) X', choices, selected,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    
    # a selectInput for choosing Y variable
    output$ass_scatter_y <- renderUI({
        req(req(input$ass_type) == 'Scatter plot')
        choices <- c('Choose' = '', ass_y_list)
        selected <- isolate(ternary(
            is.null(input$ass_scatter_y), '', input$ass_scatter_y
        ))
        selectInput('ass_scatter_y', 'Y', choices, selected)
    })
    
    # a selectInput for choosing biomarker for X
    output$ass_scatter_bmk_y <- renderUI({
        req(req(input$ass_type) == 'Scatter plot')
        data <- ass_data_input$value
        choices <- c('Choose' = '', unique(data[[param_col]]))
        selected <- isolate(ternary(
            is.null(input$ass_scatter_bmk_y), '', input$ass_scatter_bmk_y
        ))
        selectInput('ass_scatter_bmk_y', 'Biomarker Y', choices, selected)
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Scatter plot')
        data <- ass_data_input$value
        if(!is_blank(input$ass_scatter_visits_y)) {
            cond_visits <- data[[xlabel_col]] %in% input$ass_scatter_visits_y
            data <- data[cond_visits, , drop = F]
        }
        if(!is_blank(input$ass_scatter_group) &&
           !is_blank(ass_scatter_group_levs$value)) {
            cond_g <- data[[input$ass_scatter_group]] %in%
                ass_scatter_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$ass_scatter_facet_r) &&
           !is_blank(ass_scatter_facet_rlevs$value)) {
            cond_r <- data[[input$ass_scatter_facet_r]] %in%
                ass_scatter_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        if(!is_blank(input$ass_scatter_facet_c) &&
           !is_blank(ass_scatter_facet_clevs$value)) {
            cond_c <- data[[input$ass_scatter_facet_c]] %in%
                ass_scatter_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- c('Choose' = '', unique(data[[param_col]]))
        selected <- isolate(ternary(
            is.null(input$ass_scatter_bmk_y), '', input$ass_scatter_bmk_y
        ))
        updateSelectInput(session, 'ass_scatter_bmk_y', 'Biomarker Y',
                          choices, selected)
    })
    
    # a selectInput for choosing visit time for X
    output$ass_scatter_visits_y <- renderUI({
        req(req(input$ass_type) == 'Scatter plot')
        data <- ass_data_input$value
        choices <- c('Choose' = '', unique(data[[xlabel_col]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_scatter_visits_y) &&
                all(input$ass_scatter_visits_y %in% choices),
            input$ass_scatter_visits_y, ''
        ))
        selectizeInput(
            'ass_scatter_visits_y', 'Visit(s) Y', choices, selected,
            multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Scatter plot')
        data <- ass_data_input$value
        if(!is_blank(input$ass_scatter_bmk_y)) {
            cond_param <- data[[param_col]] %in% input$ass_scatter_bmk_y
            data <- data[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_scatter_group) &&
           !is_blank(ass_scatter_group_levs$value)) {
            cond_g <- data[[input$ass_scatter_group]] %in%
                ass_scatter_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$ass_scatter_facet_r) &&
           !is_blank(ass_scatter_facet_rlevs$value)) {
            cond_r <- data[[input$ass_scatter_facet_r]] %in%
                ass_scatter_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        if(!is_blank(input$ass_scatter_facet_c) &&
           !is_blank(ass_scatter_facet_clevs$value)) {
            cond_c <- data[[input$ass_scatter_facet_c]] %in%
                ass_scatter_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- c('Choose' = '', unique(data[[xlabel_col]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_scatter_visits_y) &&
                all(input$ass_scatter_visits_y %in% choices),
            input$ass_scatter_visits_y, ''
        ))
        updateSelectizeInput(
            session, 'ass_scatter_visits_y', 'Visit(s) Y', choices, selected,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    
    # select group variable
    output$ass_scatter_group <- renderUI({
        req(req(input$ass_type) == 'Scatter plot')
        data <- ass_data_input$value
        # get categorical column names
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        selected <- isolate(ternary(
            is.null(input$ass_scatter_group), '', input$ass_scatter_group
        ))
        selectInput('ass_scatter_group', 'Group', choices, selected)
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Scatter plot')
        data <- ass_data_input$value
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        if(!is_blank(input$ass_scatter_facet_r))
            choices <- setdiff(choices, input$ass_scatter_facet_r)
        if(!is_blank(input$ass_scatter_facet_c))
            choices <- setdiff(choices, input$ass_scatter_facet_c)
        selected <- isolate(ternary(
            is.null(input$ass_scatter_group), '', input$ass_scatter_group
        ))
        updateSelectInput(
            session, 'ass_scatter_group', 'Group', choices, selected
        )
    })
    
    # select group variable levels
    output$ass_scatter_group_levs <- renderUI({
        req(req(input$ass_type) == 'Scatter plot', input$ass_scatter_group)
        data <- ass_data_input$value
        choices <- sort(unique(data[[input$ass_scatter_group]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_scatter_group_levs) &&
                all(input$ass_scatter_group_levs %in% choices),
            input$ass_scatter_group_levs, choices
        ))
        selectizeInput(
            'ass_scatter_group_levs', input$ass_scatter_group, choices,
            selected = selected, multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Scatter plot',
            input$ass_scatter_group)
        data <- ass_data_input$value
        if(!is_blank(input$ass_scatter_facet_r) &&
           !is_blank(ass_scatter_facet_rlevs$value)) {
            cond_r <- data[[input$ass_scatter_facet_r]] %in%
                ass_scatter_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        if(!is_blank(input$ass_scatter_facet_c) &&
           !is_blank(ass_scatter_facet_clevs$value)) {
            cond_c <- data[[input$ass_scatter_facet_c]] %in%
                ass_scatter_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        data_x <- data
        if(!is_blank(input$ass_scatter_bmk_x)) {
            cond_param <- data_x[[param_col]] %in% input$ass_scatter_bmk_x
            data_x <- data_x[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_scatter_visits_x)) {
            cond_visits <- data_x[[xlabel_col]] %in% input$ass_scatter_visits_x
            data_x <- data_x[cond_visits, , drop = F]
        }
        data_y <- data
        if(!is_blank(input$ass_scatter_bmk_y)) {
            cond_param <- data_y[[param_col]] %in% input$ass_scatter_bmk_y
            data_y <- data_y[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_scatter_visits_y)) {
            cond_visits <- data_y[[xlabel_col]] %in% input$ass_scatter_visits_y
            data_y <- data_y[cond_visits, , drop = F]
        }
        choices <- base::intersect(
            sort(unique(data_x[[input$ass_scatter_group]])),
            sort(unique(data_y[[input$ass_scatter_group]]))
        )
        selected <- isolate(ternary(
            !is_blank(input$ass_scatter_group_levs) &&
                all(input$ass_scatter_group_levs %in% choices),
            input$ass_scatter_group_levs, choices
        ))
        updateSelectizeInput(
            session, 'ass_scatter_group_levs', input$ass_scatter_group,
            choices, selected,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    ass_scatter_group_levs <- reactiveValues(value = NULL)
    observe({
        req(req(input$ass_type) == 'Scatter plot')
        if(is_blank(input$ass_scatter_group)) {
            ass_scatter_group_levs$value <- NULL
        } else {
            data <- ass_data_input$value
            ass_scatter_group_levs$value <- sort(unique(
                data[[input$ass_scatter_group]]
            ))
        }
    })
    observe({ ass_scatter_group_levs$value <- input$ass_scatter_group_levs })
    
    # select facet row variable
    output$ass_scatter_facet_r <- renderUI({
        req(req(input$ass_type) == 'Scatter plot')
        data <- ass_data_input$value
        # get categorical column names
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        selected <- isolate(ternary(
            is.null(input$ass_scatter_facet_r), '', input$ass_scatter_facet_r
        ))
        selectInput('ass_scatter_facet_r', 'Facet row', choices, selected)
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Scatter plot')
        data <- ass_data_input$value
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        if(!is_blank(input$ass_scatter_group))
            choices <- setdiff(choices, input$ass_scatter_group)
        if(!is_blank(input$ass_scatter_facet_c))
            choices <- setdiff(choices, input$ass_scatter_facet_c)
        selected <- isolate(ternary(
            is.null(input$ass_scatter_facet_r), '', input$ass_scatter_facet_r
        ))
        updateSelectInput(
            session, 'ass_scatter_facet_r', 'Facet row', choices, selected
        )
    })
    
    # select facet row variable levels
    output$ass_scatter_facet_rlevs <- renderUI({
        req(req(input$ass_type) == 'Scatter plot', input$ass_scatter_facet_r)
        data <- ass_data_input$value
        choices <- sort(unique(data[[input$ass_scatter_facet_r]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_scatter_facet_rlevs) &&
                all(input$ass_scatter_facet_rlevs %in% choices),
            input$ass_scatter_facet_rlevs, choices
        ))
        selectizeInput(
            'ass_scatter_facet_rlevs', input$ass_scatter_facet_r, choices,
            selected = selected, multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Scatter plot',
            input$ass_scatter_facet_r)
        data <- ass_data_input$value
        if(!is_blank(input$ass_scatter_group) &&
           !is_blank(ass_scatter_group_levs$value)) {
            cond_g <- data[[input$ass_scatter_group]] %in%
                ass_scatter_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$ass_scatter_facet_c) &&
           !is_blank(ass_scatter_facet_clevs$value)) {
            cond_c <- data[[input$ass_scatter_facet_c]] %in%
                ass_scatter_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        data_x <- data
        if(!is_blank(input$ass_scatter_bmk_x)) {
            cond_param <- data_x[[param_col]] %in% input$ass_scatter_bmk_x
            data_x <- data_x[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_scatter_visits_x)) {
            cond_visits <- data_x[[xlabel_col]] %in% input$ass_scatter_visits_x
            data_x <- data_x[cond_visits, , drop = F]
        }
        data_y <- data
        if(!is_blank(input$ass_scatter_bmk_y)) {
            cond_param <- data_y[[param_col]] %in% input$ass_scatter_bmk_y
            data_y <- data_y[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_scatter_visits_y)) {
            cond_visits <- data_y[[xlabel_col]] %in% input$ass_scatter_visits_y
            data_y <- data_y[cond_visits, , drop = F]
        }
        choices <- base::intersect(
            sort(unique(data_x[[input$ass_scatter_facet_r]])),
            sort(unique(data_y[[input$ass_scatter_facet_r]]))
        )
        selected <- isolate(ternary(
            !is_blank(input$ass_scatter_facet_rlevs) &&
                all(input$ass_scatter_facet_rlevs %in% choices),
            input$ass_scatter_facet_rlevs, choices
        ))
        updateSelectizeInput(
            session, 'ass_scatter_facet_rlevs', input$ass_scatter_facet_r,
            choices, selected,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    ass_scatter_facet_rlevs <- reactiveValues(value = NULL)
    observe({
        req(req(input$ass_type) == 'Scatter plot')
        if(is_blank(input$ass_scatter_facet_r)) {
            ass_scatter_facet_rlevs$value <- NULL
        } else {
            data <- ass_data_input$value
            ass_scatter_facet_rlevs$value <- sort(unique(
                data[[input$ass_scatter_facet_r]]
            ))
        }
    })
    observe({ ass_scatter_facet_rlevs$value <- input$ass_scatter_facet_rlevs })
    
    # select facet column variable
    output$ass_scatter_facet_c <- renderUI({
        req(req(input$ass_type) == 'Scatter plot')
        data <- ass_data_input$value
        # get categorical column names
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        selected <- isolate(ternary(
            is.null(input$ass_scatter_facet_c), '', input$ass_scatter_facet_c
        ))
        selectInput('ass_scatter_facet_c', 'Facet column', choices, selected)
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Scatter plot')
        data <- ass_data_input$value
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        if(!is_blank(input$ass_scatter_group))
            choices <- setdiff(choices, input$ass_scatter_group)
        if(!is_blank(input$ass_scatter_facet_r))
            choices <- setdiff(choices, input$ass_scatter_facet_r)
        selected <- isolate(ternary(
            is.null(input$ass_scatter_facet_c), '', input$ass_scatter_facet_c
        ))
        updateSelectInput(
            session, 'ass_scatter_facet_c', 'Facet column', choices, selected
        )
    })
    
    # select facet column variable levels
    output$ass_scatter_facet_clevs <- renderUI({
        req(req(input$ass_type) == 'Scatter plot', input$ass_scatter_facet_c)
        data <- ass_data_input$value
        choices <- sort(unique(data[[input$ass_scatter_facet_c]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_scatter_facet_clevs) &&
                all(input$ass_scatter_facet_clevs %in% choices),
            input$ass_scatter_facet_clevs, choices
        ))
        selectizeInput(
            'ass_scatter_facet_clevs', input$ass_scatter_facet_c, choices,
            selected = selected, multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Scatter plot',
            input$ass_scatter_facet_c)
        data <- ass_data_input$value
        if(!is_blank(input$ass_scatter_group) &&
           !is_blank(ass_scatter_group_levs$value)) {
            cond_g <- data[[input$ass_scatter_group]] %in%
                ass_scatter_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$ass_scatter_facet_r) &&
           !is_blank(ass_scatter_facet_rlevs$value)) {
            cond_r <- data[[input$ass_scatter_facet_r]] %in%
                ass_scatter_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        data_x <- data
        if(!is_blank(input$ass_scatter_bmk_x)) {
            cond_param <- data_x[[param_col]] %in% input$ass_scatter_bmk_x
            data_x <- data_x[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_scatter_visits_x)) {
            cond_visits <- data_x[[xlabel_col]] %in% input$ass_scatter_visits_x
            data_x <- data_x[cond_visits, , drop = F]
        }
        data_y <- data
        if(!is_blank(input$ass_scatter_bmk_y)) {
            cond_param <- data_y[[param_col]] %in% input$ass_scatter_bmk_y
            data_y <- data_y[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_scatter_visits_y)) {
            cond_visits <- data_y[[xlabel_col]] %in% input$ass_scatter_visits_y
            data_y <- data_y[cond_visits, , drop = F]
        }
        choices <- base::intersect(
            sort(unique(data_x[[input$ass_scatter_facet_c]])),
            sort(unique(data_y[[input$ass_scatter_facet_c]]))
        )
        selected <- isolate(ternary(
            !is_blank(input$ass_scatter_facet_clevs) &&
                all(input$ass_scatter_facet_clevs %in% choices),
            input$ass_scatter_facet_clevs, choices
        ))
        updateSelectizeInput(
            session, 'ass_scatter_facet_clevs', input$ass_scatter_facet_c,
            choices, selected,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    ass_scatter_facet_clevs <- reactiveValues(value = NULL)
    observe({
        req(req(input$ass_type) == 'Scatter plot')
        if(is_blank(input$ass_scatter_facet_c)) {
            ass_scatter_facet_clevs$value <- NULL
        } else {
            data <- ass_data_input$value
            ass_scatter_facet_clevs$value <- sort(unique(
                data[[input$ass_scatter_facet_c]]
            ))
        }
    })
    observe({ ass_scatter_facet_clevs$value <- input$ass_scatter_facet_clevs })
    
    
    # --- UI widgets for refining scatter plot ---
    
    # x-axis label
    output$ass_scatter_xlab <- renderUI({
        req(ass_scatter_show$value)
        textInput('ass_scatter_xlab', 'X-axis label', '{BiomarkerX} at {VisitsX}')
    })
    ass_scatter_xlab <- reactiveValues(value = NULL)
    observe({
        if(isTRUE(ass_scatter_show$value)) {
            ass_scatter_xlab$value <- '{BiomarkerX} at {VisitsX}'
        } else ass_scatter_xlab$value <- NULL
    })
    observe({ ass_scatter_xlab$value <- input$ass_scatter_xlab })
    
    # y-axis label
    output$ass_scatter_ylab <- renderUI({
        req(ass_scatter_show$value)
        textInput('ass_scatter_ylab', 'Y-axis label', '{BiomarkerY} at {VisitsY}')
    })
    ass_scatter_ylab <- reactiveValues(value = NULL)
    observe({
        if(isTRUE(ass_scatter_show$value)) {
            ass_scatter_ylab$value <- '{BiomarkerY} at {VisitsY}'
        } else ass_scatter_ylab$value <- NULL
    })
    observe({ ass_scatter_ylab$value <- input$ass_scatter_ylab })
    
    # plot title
    output$ass_scatter_title <- renderUI({
        req(ass_scatter_show$value)
        value <- ifelse(!is_blank(input$ass_study), 'Study: {Study}', '')
        if(!is_blank(input$ass_cohort))
            value <- paste(value, '{Cohort} cohort', sep = '\n')
        value <- trimws(value)
        textAreaInput('ass_scatter_title', 'Plot title', value)
    })
    ass_scatter_title <- reactiveValues(value = NULL)
    observe({
        if(isTRUE(ass_scatter_show$value)) {
            value <- ifelse(!is_blank(input$ass_study), 'Study: {Study}', '')
            if(!is_blank(input$ass_cohort))
                value <- paste(value, '{Cohort} cohort', sep = '\n')
            value <- trimws(value)
            ass_scatter_title$value <- value  
        } else ass_scatter_title$value <- NULL
    })
    observe({ ass_scatter_title$value <- input$ass_scatter_title })
    
    # add footnote
    output$ass_scatter_footnote_in <- renderUI({
        req(ass_scatter_show$value)
        textAreaInput('ass_scatter_footnote_in', 'Plot footnote', '')
    })
    ass_scatter_footnote_in <- reactiveValues(value = '')
    observe({
        value <- ''
        if(!is_blank(input$ass_scatter_test)) {
            value <- paste('P value is calculated from', input$ass_scatter_test,
                           'correlation test')
        }
        if(!is_blank(input$ass_scatter_footnote_in)) {
            value <- trimws(paste(
                value, input$ass_scatter_footnote_in, sep = '\n'
            ))
        }
        ass_scatter_footnote_in$value <- value
    })
    observe({ ass_scatter_footnote_in$value <- input$ass_scatter_footnote_in })
    
    # log of X
    output$ass_scatter_log_x <- renderUI({
        req(ass_scatter_show$value)
        value <- isolate(ternary(
            is.null(input$ass_scatter_log_x), FALSE, input$ass_scatter_log_x
        ))
        checkboxInput('ass_scatter_log_x', 'Log of X', value = value)
    })
    
    # log of X
    output$ass_scatter_log_y <- renderUI({
        req(ass_scatter_show$value)
        value <- isolate(ternary(
            is.null(input$ass_scatter_log_y), FALSE, input$ass_scatter_log_y
        ))
        checkboxInput('ass_scatter_log_y', 'Log of Y', value = value)
    })
    
    # add a fitted line
    output$ass_scatter_add_line <- renderUI({
        req(ass_scatter_show$value)
        choices <- c('Choose' = '',
                     c('Loess line', 'Linear regression line', 'Identify line'))
        selected <- isolate(ternary(
            is.null(input$ass_scatter_add_line), '',
            input$ass_scatter_add_line
        ))
        selectInput('ass_scatter_add_line', 'Add a fitted line', choices,
                    selected)
    })
    
    # add confidence band
    output$ass_scatter_add_ci <- renderUI({
        req(ass_scatter_show$value,
            req(input$ass_scatter_add_line) != 'Identify line')
        value <- isolate(ternary(
            is.null(input$ass_scatter_add_ci), FALSE, input$ass_scatter_add_ci
        ))
        checkboxInput('ass_scatter_add_ci', 'Include confidence band', value)
    })
    
    # add correlation test
    output$ass_scatter_test <- renderUI({
        req(ass_scatter_show$value)
        choices <- c('Choose' = '', c('Spearman', 'Pearson'))
        selected <- isolate(ternary(
            is.null(input$ass_scatter_test), '',
            input$ass_scatter_test
        ))
        selectInput('ass_scatter_test', 'Correlation test', choices, selected)
    })
    
    # toggle subject IDs
    output$ass_scatter_toggle_subjid <- renderUI({
        req(ass_scatter_show$value)
        value <- isolate(ternary(
            is.null(input$ass_scatter_toggle_subjid), FALSE,
            input$ass_scatter_toggle_subjid
        ))
        checkboxInput('ass_scatter_toggle_subjid', 'Label with subject IDs',
                      value)
    })
    
    # --- Scatter plot output ---
    
    # scatter shown status
    ass_scatter_show <- reactiveValues(value = FALSE)
    observe({
        data <- ass_data_input$value
        if(!is_blank(input$ass_type) &&
           input$ass_type == 'Scatter plot' &&
           !is_blank(data) && nrow(data) > 0 &&
           !is_blank(input$ass_scatter_x) &&
           !is_blank(input$ass_scatter_bmk_x) &&
           !is_blank(input$ass_scatter_visits_x) &&
           !is_blank(input$ass_scatter_y) &&
           !is_blank(input$ass_scatter_bmk_y) &&
           !is_blank(input$ass_scatter_visits_y) &&
           length(input$ass_scatter_visits_x) ==
           length(input$ass_scatter_visits_y)) {
            ass_scatter_show$value = TRUE
        } else ass_scatter_show$value = FALSE
    })
    
    ass_scatter_plot_data <- reactive({
        req(ass_scatter_show$value)
        data <- ass_data_input$value
        group_var <- c()
        if(!is_blank(input$ass_scatter_group) &&
           !is_blank(ass_scatter_group_levs$value)) {
            cond_g <- data[[input$ass_scatter_group]] %in%
                ass_scatter_group_levs$value
            data <- data[cond_g, , drop = F]
            data[[input$ass_scatter_group]] <- factor(
                data[[input$ass_scatter_group]],
                levels = ass_scatter_group_levs$value
            )
            group_var <- c(input$ass_scatter_group, group_var)
        }
        if(!is_blank(input$ass_scatter_facet_r) &&
           !is_blank(ass_scatter_facet_rlevs$value)) {
            cond_r <- data[[input$ass_scatter_facet_r]] %in%
                ass_scatter_facet_rlevs$value
            data <- data[cond_r, , drop = F]
            data[[input$ass_scatter_facet_r]] <- factor(
                data[[input$ass_scatter_facet_r]],
                levels = ass_scatter_facet_rlevs$value
            )
            group_var <- c(input$ass_scatter_facet_r, group_var)
        }
        if(!is_blank(input$ass_scatter_facet_c) &&
           !is_blank(ass_scatter_facet_clevs$value)) {
            cond_c <- data[[input$ass_scatter_facet_c]] %in%
                ass_scatter_facet_clevs$value
            data <- data[cond_c, , drop = F]
            data[[input$ass_scatter_facet_c]] <- factor(
                data[[input$ass_scatter_facet_c]],
                levels = ass_scatter_facet_clevs$value
            )
            group_var <- c(input$ass_scatter_facet_c, group_var)
        }
        group_var <- c(group_var, subj_col)
        data_x <- data
        if(!is_blank(input$ass_scatter_bmk_x)) {
            cond_param <- data_x[[param_col]] %in% input$ass_scatter_bmk_x
            data_x <- data_x[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_scatter_visits_x)) {
            cond_visits <- data_x[[xlabel_col]] %in% input$ass_scatter_visits_x
            data_x <- data_x[cond_visits, , drop = F]
        }
        data_y <- data
        if(!is_blank(input$ass_scatter_bmk_y)) {
            cond_param <- data_y[[param_col]] %in% input$ass_scatter_bmk_y
            data_y <- data_y[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_scatter_visits_y)) {
            cond_visits <- data_y[[xlabel_col]] %in% input$ass_scatter_visits_y
            data_y <- data_y[cond_visits, , drop = F]
        }
        data_x <- data_x[, c(group_var, input$ass_scatter_x), drop = F]
        data_y <- data_y[, c(group_var, input$ass_scatter_y), drop = F]
        data_scatter <- dplyr::full_join(data_x, data_y, by = group_var)
        if(input$ass_scatter_x == input$ass_scatter_y) {
            x_var <- paste0(input$ass_scatter_x, '.x')
            y_var <- paste0(input$ass_scatter_y, '.y')
        } else {
            x_var <- input$ass_scatter_x
            y_var <- input$ass_scatter_y
        }
        data_scatter <- rename_(
            data_scatter, .dots = setNames(c(x_var, y_var), c('x_var', 'y_var'))
        )
        return(data_scatter)
    })
    
    ass_scatter_test_result <- reactive({
        req(ass_scatter_plot_data(), input$ass_scatter_test)
        data <- ass_scatter_plot_data()
        group_list <- c()
        if(!is_blank(input$ass_scatter_group) &&
           !is_blank(ass_scatter_group_levs$value)) {
            group_list <- c(input$ass_scatter_group, group_list)
        }
        if(!is_blank(input$ass_scatter_facet_r) &&
           !is_blank(ass_scatter_facet_rlevs$value)) {
            group_list <- c(input$ass_scatter_facet_r, group_list)
        }
        if(!is_blank(input$ass_scatter_facet_c) &&
           !is_blank(ass_scatter_facet_clevs$value)) {
            group_list <- c(input$ass_scatter_facet_c, group_list)
        }
        dots_group <- lapply(group_list, as.symbol)
        if(!is_blank(dots_group)) {
            data <- data %>%
                arrange_(.dots = dots_group) %>%
                group_by_(.dots = dots_group)
        }
        if(is_blank(dots_group)) {
            p_value_expr <- lazyeval::interp(
                ~var[[1]]$p.value, var = as.name('test')
            )
        } else {
            p_value_expr <- lazyeval::interp(
                ~var$p.value,var = as.name('test')
            )
        }
        dots_summarise <- setNames(c(
            as.list(group_list), list(p_value_expr, ~(Inf), ~(Inf))
        ), c(group_list, 'p_value', 'x', 'y'))
        test_method <- tolower(input$ass_scatter_test)
        test_result <- data %>%
            do(test = tryCatch(
                cor.test(.[['x_var']], .[['y_var']], method = test_method),
                error = function(e) {NULL}
            )) %>%
            filter(!is.null(test)) %>%
            summarise_(.dots = dots_summarise)
        if(!is_blank(input$ass_scatter_group) &&
           !is_blank(ass_scatter_group_levs$value)) {
            group_list <- setdiff(group_list, input$ass_scatter_group)
            dots_group <- lapply(group_list, as.symbol)
        }
        if(!is_blank(dots_group)) {
            test_result <- test_result %>%
                arrange_(.dots = dots_group) %>%
                group_by_(.dots = dots_group)
        }
        test_result <- test_result %>%
            mutate(row_idx = row_number())
        test_result$p_value_text <- paste(
            sapply(
                lapply(test_result$row_idx - 1, rep.int, x = '\n'),
                paste0, 'p value: '
            ),
            round(test_result$p_value, 3), sep = ''
        )
        return(test_result)
    })
    
    ass_scatter_plot <- reactive({
        req(ass_scatter_plot_data(),
            !is.null(ass_scatter_xlab$value),
            !is.null(ass_scatter_ylab$value),
            !is.null(ass_scatter_title$value),
            !is.null(input$ass_scatter_add_line),
            !is.null(input$ass_scatter_toggle_subjid))
        data <- ass_scatter_plot_data()
        if(length(ass_scatter_group_levs$value) <= 1)
            all_colors = gg_color_hue(1)
        else all_colors = gg_color_hue(length(ass_scatter_group_levs$value))
        if(is_blank(ass_scatter_facet_rlevs$value)) facet_r_levels <- NULL
        else {
            facet_r_levels <- setNames(
                ass_scatter_facet_rlevs$value,
                paste0(input$ass_scatter_facet_r, ': ',
                       ass_scatter_facet_rlevs$value)
            )
        }
        if(is_blank(ass_scatter_facet_clevs$value)) facet_c_levels <- NULL
        else {
            facet_c_levels <- setNames(
                ass_scatter_facet_clevs$value,
                paste0(input$ass_scatter_facet_c, ': ',
                       ass_scatter_facet_clevs$value)
            )
        }
        
        x_lab <- r_format(ass_scatter_ylab$value, tnf$ass_current)
        y_lab <- r_format(ass_scatter_xlab$value, tnf$ass_current)
        plot_title <- r_format(ass_scatter_title$value, tnf$ass_current)
        
        plot_ <- gg_scatter(
            data, x = 'x_var', y = 'y_var', label = subj_col,
            facet_r = input$ass_scatter_facet_r,
            facet_c = input$ass_scatter_facet_c,
            facet_r_levels = facet_r_levels, facet_c_levels = facet_c_levels,
            color_var = input$ass_scatter_group, all_colors = all_colors,
            shape_var = NULL, add_label = input$ass_scatter_toggle_subjid,
            repel_label = TRUE, label_xlim = c(-Inf, Inf),
            label_ylim = c(-Inf, Inf), label_xloc = 'middle',
            label_yloc = 'middle',
            x_lab = x_lab, y_lab = y_lab, title = plot_title,
            x_limit = ass_scatter_brush_range$xlim,
            y_limit = ass_scatter_brush_range$ylim, 
            x_log = input$ass_scatter_log_x, y_log = input$ass_scatter_log_y,
            add_legend = TRUE, legend_pos = 'bottom',
            reference_hline = NULL, reference_vline = NULL,
            bw_theme = TRUE, grids = 'on'
        )
        if(!is_blank(input$ass_scatter_add_line)) {
            if(input$ass_scatter_add_line != 'Identify line') {
                if(input$ass_scatter_add_line == 'Loess line') method <- 'loess'
                else if(input$ass_scatter_add_line == 'Linear regression line')
                    method <- 'lm'
                se <- isTRUE(input$ass_scatter_add_ci)
                plot_ <- plot_ + geom_smooth(method = method, se = se)
            } else {
                plot_ <- plot_ + geom_abline(intercept = 0, slope = 1)
            }
        }
        if(!is_blank(input$ass_scatter_test)) {
            test_res <- ass_scatter_test_result()
            plot_ <- plot_ + geom_text(data = test_res,
                                       aes(x, y, label = p_value_text),
                                       show.legend = FALSE,
                                       hjust = 1.2, vjust = 1.5)
        }
        return(plot_)
    })
    
    # scatter plot output
    output$ass_scatter_output <- renderUI({
        req(ass_scatter_show$value)
        tags$div(
            style = 'position:relative',
            tabsetPanel(
              tabPanel("Graph",
                plotOutput(
                    'ass_scatter_plot',
                    dblclick = 'ass_scatter_dblclick',
                    hover = hoverOpts('ass_scatter_hover', delay = 100,
                                      delayType = 'debounce'),
                    brush = brushOpts(id = 'ass_scatter_brush', resetOnNew = T,
                                      delay = 1000)
                ),
                uiOutput('ass_scatter_hover'),
                uiOutput('ass_scatter_footnote_out')
              ),
              tabPanel("Association Summary Table"
                       # htmlOutput('time_summary')
                       #plotOutput('time_summary')
                       
              ) 
            ) 
        )
    })
    
    # draw scatter plot
    output$ass_scatter_plot <- renderPlot({
        req(ass_scatter_plot())
        ass_scatter_plot()
    })
    
    # -- scatter plot hover tooltip
    output$ass_scatter_hover <- renderUI({
        req(ass_scatter_show$value, ass_scatter_plot())
        data <- ass_scatter_plot_data()
        tooltip(input$ass_scatter_hover, data, 'x_var', 'y_var',
                vars_name = c(ass_scatter_xlab$value,
                              ass_scatter_ylab$value))
    })
    
    # -- scatter plot brush
    # Link brush range to a reactive value
    ass_scatter_brush_range <- reactiveValues(xlim = NULL, ylim = NULL)
    observe({
        brush <- input$ass_scatter_brush
        if(!is.null(brush)) {
            ass_scatter_brush_range$xlim <- c(brush$xmin, brush$xmax)
            ass_scatter_brush_range$ylim <- c(brush$ymin, brush$ymax)
        }
    })
    # double click to reset the graph
    observeEvent(input$ass_scatter_dblclick, {
        if((!is.null(ass_scatter_brush_range$xlim)) ||
           !is.null(ass_scatter_brush_range$ylim))
        {
            ass_scatter_brush_range$xlim <- NULL
            ass_scatter_brush_range$ylim <- NULL
        }
    })
    
    # -- scatter plot footnote
    output$ass_scatter_footnote_out <- renderUI({
        req(!is.null(ass_scatter_footnote_in$value))
        value <- trimws(paste(
            unlist(strsplit(ass_scatter_footnote_in$value, '\n')),
            collapse = '<br/>'
        ))
        HTML(value)
    })
    
    # # --- UI widgets for scatter plot ---
    # # a selectInput for choosing x variable
    # output$ass_scatter_x <- renderUI({
    #     req(input$ass_type)
    #     if(input$ass_type != 'Scatter plot') return()
    #     data <- ass_data_input$value
    #     choices <- col_continuity()$continuous
    #     selectInput('ass_scatter_x', 'X', c('Choose' = '', choices))
    # })
    # # a selectInput for choosing visit time for X
    # output$ass_scatter_time_x <- renderUI({
    #     req(input$ass_type)
    #     if(input$ass_type != 'Scatter plot') return()
    #     data <- ass_data_input$value
    #     selectizeInput(
    #         'ass_scatter_time_x', 'Visit(s)',
    #         c('Choose'='', unique(data[[xlabel_col]])), multiple = TRUE,
    #         options = list(plugins = list('drag_drop','remove_button'))
    #     )
    # })
    # # a checkbox for deciding whether X is biomarker dependent
    # output$ass_scatter_bmk_depend_x <- renderUI({
    #     req(input$ass_type)
    #     if(input$ass_type != 'Scatter plot') return()
    #     req(input$ass_scatter_x)
    #     checkboxInput(
    #         'ass_scatter_bmk_depend_x', label = 'Is it biomarker-dependent?', value = F
    #     )
    # })
    # # a selectInput for choosing biomarker for X
    # output$ass_scatter_bmk_x <- renderUI({
    #     req(input$ass_type)
    #     if(input$ass_type != 'Scatter plot') return()
    #     req(input$ass_scatter_bmk_depend_x)
    #     names_bmk <- unique(ass_data_input$value[[param_col]])
    #     selectInput('ass_scatter_bmk_x', 'Biomarker X', c('Choose'='', names_bmk))
    # })
    # # a checkbox for specifying log-scale of X
    # output$ass_scatter_to_log_x <- renderUI({
    #     req(input$ass_type)
    #     if(input$ass_type != 'Scatter plot') return()
    #     checkboxInput(
    #         'ass_scatter_to_log_x', label = 'Log of X', value = F
    #     )
    # })
    # 
    # # a selectInput for choosing Y variable
    # output$ass_scatter_y <- renderUI({
    #     req(input$ass_type)
    #     if(input$ass_type != 'Scatter plot') return()
    #     data <- ass_data_input$value
    #     choices <- col_continuity()$continuous
    #     selectInput('ass_scatter_y', 'Y', c('Choose' = '', choices))
    # })
    # # a selectInput for choosing visit time for Y
    # output$ass_scatter_time_y <- renderUI({
    #     req(input$ass_type)
    #     if(input$ass_type != 'Scatter plot') return()
    #     data <- ass_data_input$value
    #     selectizeInput(
    #         'ass_scatter_time_y', 'Visit(s)',
    #         c('Choose'='', unique(data[[xlabel_col]])), multiple = TRUE,
    #         options = list(plugins = list('drag_drop','remove_button'))
    #     )
    # })
    # # a checkbox for deciding whether Y is biomarker dependent
    # output$ass_scatter_bmk_depend_y <- renderUI({
    #     req(input$ass_type)
    #     if(input$ass_type != 'Scatter plot') return()
    #     req(input$ass_scatter_y)
    #     checkboxInput(
    #         'ass_scatter_bmk_depend_y', label = 'Is it biomarker-dependent?', value = F
    #     )
    # })
    # # a selectInput for choosing biomarker for Y
    # output$ass_scatter_bmk_y <- renderUI({
    #     req(input$ass_type)
    #     if(input$ass_type != 'Scatter plot') return()
    #     req(input$ass_scatter_bmk_depend_y)
    #     names_bmk <- unique(ass_data_input$value[[param_col]])
    #     selectInput('ass_scatter_bmk_y', 'Biomarker Y', c('Choose'='', names_bmk))
    # })
    # # a checkbox for specifying log-scale of Y
    # output$ass_scatter_to_log_y <- renderUI({
    #     req(input$ass_type)
    #     if(input$ass_type != 'Scatter plot') return()
    #     checkboxInput(
    #         'ass_scatter_to_log_y', label = 'Log of Y', value = F
    #     )
    # })
    # # a selectInput for choosing smoothing method
    # output$ass_scatter_add_line <- renderUI({
    #     req(input$ass_type)
    #     if(input$ass_type != 'Scatter plot') return()
    #     selectInput(
    #         'ass_scatter_add_line', 'Add a line',
    #         choices = c('Choose' = '', ass_add_line_types)
    #     )
    # })
    # # a selectInput for choosing correlation type
    # output$ass_scatter_cortype <- renderUI({
    #     req(input$ass_type)
    #     if(input$ass_type != 'Scatter plot') return()
    #     selectInput(
    #         'ass_scatter_cortype', 'Correlation type',
    #         choices = c('Choose' = '', ass_scatter_correlation_types)
    #     )
    # })
    # 
    # # --- UI widgets for refining scatter plot ---
    # # a checkbox to toggle subject ID
    # output$ass_scatter_toggle_subjid <- renderUI({
    #     req(ass_scatter_show$value)
    #     checkboxInput('ass_scatter_toggle_subjid', 'Show subject ID', value = FALSE)
    # })
    # 
    # # --- Output for scatter plot ---
    # ass_scatter_data <- reactive({
    #     req(ass_scatter_show$value)
    #     data <- ass_data_input$value
    #     data_x <- data[data[[xlabel_col]] %in% input$ass_scatter_time_x, , drop = F]
    #     data_y <- data[data[[xlabel_col]] %in% input$ass_scatter_time_y, , drop = F]
    #     if(is_blank(input$ass_scatter_bmk_x)) {
    #         if(is_blank(input$ass_cohort))
    #             data_x <- group_by_(data_x, study_col, subj_col, xvar_col)
    #         else
    #             data_x <- group_by_(data_x, study_col, cohort_col, subj_col, xvar_col)
    #         data_x <- filter(data_x, row_number() == 1)
    #     } else {
    #         data_x <- data_x[data_x[[param_col]] == input$ass_scatter_bmk_x, , drop = F]
    #     }
    #     if(is_blank(input$ass_scatter_bmk_y)) {
    #         if(is_blank(input$ass_cohort))
    #             data_y <- group_by_(data_y, study_col, subj_col, xvar_col)
    #         else
    #             data_y <- group_by_(data_y, study_col, cohort_col, subj_col, xvar_col)
    #         data_y <- filter(data_y, row_number() == 1)
    #     } else {
    #         data_y <- data_y[data_y[[param_col]] == input$ass_scatter_bmk_y, , drop = F]
    #     }
    #     by_vars <- c(study_col)
    #     if(!is_blank(input$ass_cohort)) by_vars <- c(by_vars, cohort_col)
    #     by_vars <- c(by_vars, subj_col)
    #     if(identical(input$ass_scatter_time_x, input$ass_scatter_time_y))
    #         by_vars <- c(by_vars, xlabel_col)
    #     data_x <- data_x[, c(by_vars, input$ass_scatter_x)]
    #     data_y <- data_y[, c(by_vars, input$ass_scatter_y)]
    #     data <- merge(data_x, data_y, by = by_vars, all = TRUE)
    #     return(data)
    # })
    # ass_scatter_attr <- reactive({
    #     var_x <- input$ass_scatter_x
    #     var_y <- input$ass_scatter_y
    #     if(identical(var_x, var_y)) {
    #         var_x <- paste(var_x, 'x', sep = '.')
    #         var_y <- paste(var_y, 'y', sep = '.')
    #     }
    #     visit_x <- ifelse(length(input$ass_scatter_time_x) > 1,
    #                       '', input$ass_scatter_time_x)
    #     visit_y <- ifelse(length(input$ass_scatter_time_y) > 1,
    #                       '', input$ass_scatter_time_y)
    #     unit_x <- ''
    #     unit_y <- ''
    #     name_x <- input$ass_scatter_x
    #     name_y <- input$ass_scatter_y
    #     if(!is_blank(input$ass_scatter_bmk_x)) {
    #         name_x <- input$ass_scatter_bmk_x
    #         unit_x <- bmk_value_name_dict[[input$ass_scatter_x]]
    #         if(input$ass_scatter_x %in% c('AVAL', 'CHG'))
    #             unit_x <- smart_paste(unit_x, bmk_unit()[[name_x]], sep = ', ')
    #     }
    #     if(!is_blank(input$ass_scatter_bmk_y)) {
    #         name_y <- input$ass_scatter_bmk_y
    #         unit_y <- bmk_value_name_dict[[input$ass_scatter_y]]
    #         if(input$ass_scatter_y %in% c('AVAL', 'CHG'))
    #             unit_y <- smart_paste(unit_y, bmk_unit()[[name_y]], sep = ', ')
    #     }
    #     main <- ifelse(
    #         !is_blank(input$ass_study), paste0('Study: ', input$ass_study), ''
    #     )
    #     if(!is_blank(input$ass_cohort))
    #         main <- paste(main, paste(input$ass_cohort, 'cohort'), sep = '\n')
    #     main <- trimws(main)
    #     return(list(
    #         var_x = var_x, name_x = name_x, unit_x = unit_x, visit_x = visit_x,
    #         var_y = var_y, name_y = name_y, unit_y = unit_y, visit_y = visit_y,
    #         main = main
    #     ))
    # })
    # output$ass_scatter_output <- renderUI({
    #     req(ass_scatter_show$value)
    #     tags$div(
    #         style = 'position:relative',
    #         plotOutput(
    #             'ass_scatter_plot',
    #             dblclick = 'ass_scatter_dblclick',
    #             hover = hoverOpts('ass_scatter_hover', delay = 100, delayType = 'debounce'),
    #             brush = brushOpts(id = 'ass_scatter_brush', resetOnNew = T, delay = 1000)
    #         ),
    #         uiOutput('ass_scatter_hover')
    #     )
    # })
    # # A reactive value linked to the status of association scatter plot
    # ass_scatter_show <- reactiveValues(value = FALSE)
    # observe({
    #     if(any(is_blank(input$ass_scatter_x), is_blank(input$ass_scatter_time_x),
    #            is_blank(input$ass_scatter_y), is_blank(input$ass_scatter_time_y))) {
    #         ass_scatter_show$value <- FALSE
    #     } else {
    #         if((length(input$ass_scatter_time_x) > 1 ||
    #             length(input$ass_scatter_time_y) > 1) &&
    #            !identical(input$ass_scatter_time_x, input$ass_scatter_time_y)) {
    #             ass_scatter_show$value <- FALSE
    #         } else if((isTRUE(input$ass_scatter_bmk_depend_x) &&
    #                    is_blank(input$ass_scatter_bmk_x)) ||
    #                   (isTRUE(input$ass_scatter_bmk_depend_y) &&
    #                    is_blank(input$ass_scatter_bmk_y))) {
    #             ass_scatter_show$value <- FALSE
    #         } else ass_scatter_show$value <- TRUE
    #     }
    # })
    # observe({
    #     input$ass_type
    #     ass_scatter_show$value <- FALSE
    # })
    # # Draw association scatter plot
    # output$ass_scatter_plot <- renderPlot({
    #     data <- ass_scatter_data()
    #     plot_attr <- ass_scatter_attr()
    #     scatter_plot(
    #         data, plot_attr$var_x, plot_attr$var_y, subj_col = subj_col,
    #         plot_attr$visit_x, plot_attr$visit_y,
    #         plot_attr$name_x, plot_attr$name_y,
    #         to_log_x = input$ass_scatter_to_log_x,
    #         to_log_y = input$ass_scatter_to_log_y,
    #         x_unit = plot_attr$unit_x, y_unit = plot_attr$unit_y,
    #         main = plot_attr$main,
    #         xlim = ass_scatter_brush_range$xlim,
    #         ylim = ass_scatter_brush_range$ylim,
    #         add_line = input$ass_scatter_add_line,
    #         add_subjid = input$ass_scatter_toggle_subjid,
    #         test_method = input$ass_scatter_cortype
    #     )
    # })
    # 
    # # -- Scatter brush
    # # Link brush range to a reactive value
    # ass_scatter_brush_range <- reactiveValues(xlim = NULL, ylim = NULL)
    # observe({
    #     brush <- input$ass_scatter_brush
    #     if(!is.null(brush)) {
    #         ass_scatter_brush_range$xlim <- c(brush$xmin, brush$xmax)
    #         ass_scatter_brush_range$ylim <- c(brush$ymin, brush$ymax)
    #     }
    # })
    # # dynamically deteck double click and set the time-profiling
    # # graph back to its original scale when brushed
    # observeEvent(input$ass_scatter_dblclick, {
    #     if((!is.null(ass_scatter_brush_range$xlim)) || !is.null(ass_scatter_brush_range$ylim))
    #     {
    #         ass_scatter_brush_range$xlim <- NULL
    #         ass_scatter_brush_range$ylim <- NULL
    #     }
    # })
    # 
    # # clear brush selection when the 'clear' button is pressed
    # observeEvent(input$ass_scatter_unbrush_button, {
    #     ass_scatter_brush_range$xlim <- NULL
    #     ass_scatter_brush_range$ylim <- NULL
    # })
    # 
    # # clear brush selection when any of the time-profiling input changes
    # observe({
    #     input$ass_scatter_x
    #     input$ass_scatter_y
    #     input$ass_scatter_bmk_depend_x
    #     input$ass_scatter_bmk_depend_y
    #     input$ass_scatter_bmk_x
    #     input$ass_scatter_bmk_y
    #     input$ass_scatter_time_x
    #     input$ass_scatter_time_y
    #     ass_scatter_brush_range$xlim <- NULL
    #     ass_scatter_brush_range$ylim <- NULL
    # })
    # 
    # # -- Scatter hover tooltip
    # output$ass_scatter_hover <- renderUI({
    #     req(ass_scatter_show$value)
    #     data <- ass_scatter_data()
    #     plot_attr <- ass_scatter_attr()
    #     tooltip(input$ass_scatter_hover, data, plot_attr$var_x, plot_attr$var_y,
    #             c(subj_col))
    # })
    
    
    #-------------------------------------------------
    #   Boxplot
    #-------------------------------------------------
    
    # --- UI widgets for producing box plot ---
    
    # select y variable, continuous
    output$ass_box_y <- renderUI({
        req(req(input$ass_type) == 'Box plot')
        choices <- c('Choose' = '', ass_y_list)
        selectInput('ass_box_y', 'Y (Continuous)', choices)
    })

    # select biomarker
    output$ass_box_bmk <- renderUI({
        req(req(input$ass_type) == 'Box plot')
        data <- ass_data_input$value
        choices <- c('Choose' = '', unique(data[[param_col]]))
        selected <- isolate(
            ternary(is.null(input$ass_box_bmk), '', input$ass_box_bmk)
        )
        #selectInput('ass_box_bmk', 'Biomarker', choices, selected)
        selectizeInput(
         'ass_box_bmk', 'Biomarker', choices, selected, multiple = TRUE,
         options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Box plot')
        data <- ass_data_input$value
        if(!is_blank(input$ass_box_visits)) {
            cond_visits <- data[[xlabel_col]] %in% input$ass_box_visits
            data <- data[cond_visits, , drop = F]
        }
        if(!is_blank(input$ass_box_x) && !is_blank(ass_box_xlevs$value)) {
            cond_x <- data[[input$ass_box_x]] %in% ass_box_xlevs$value
            data <- data[cond_x, , drop = F]
        }
        if(!is_blank(input$ass_box_group) &&
           !is_blank(ass_box_group_levs$value)) {
            cond_g <- data[[input$ass_box_group]] %in% ass_box_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$ass_box_facet_r) &&
           !is_blank(ass_box_facet_rlevs$value)) {
            cond_r <- data[[input$ass_box_facet_r]]%in%ass_box_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        if(!is_blank(input$ass_box_facet_c) &&
           !is_blank(ass_box_facet_clevs$value)) {
            cond_c <- data[[input$ass_box_facet_c]]%in%ass_box_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- c('Choose' = '', unique(data[[param_col]]))
        selected <- isolate(
            ternary(is.null(input$ass_box_bmk), '', input$ass_box_bmk)
        )
        #updateSelectInput(session, 'ass_box_bmk', 'Biomarker', choices, selected)
        updateSelectizeInput(
          session, 'ass_box_bmk', 'Biomarker', choices, selected,
          options = list(plugins = list('drag_drop','remove_button'))
        )
    })

    # select visit(s)
    output$ass_box_visits <- renderUI({
        req(req(input$ass_type) == 'Box plot')
        data <- ass_data_input$value
        choices <- c('Choose' = '', unique(data[[xlabel_col]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_box_visits) &&
                all(input$ass_box_visits %in% choices),
            input$ass_box_visits, ''
        ))
        selectizeInput(
            'ass_box_visits', 'Visit(s)', choices, selected, multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Box plot')
        data <- ass_data_input$value
        if(!is_blank(input$ass_box_bmk)) {
            cond_param <- data[[param_col]] %in% input$ass_box_bmk
            data <- data[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_box_x) && !is_blank(ass_box_xlevs$value)) {
            cond_x <- data[[input$ass_box_x]] %in% ass_box_xlevs$value
            data <- data[cond_x, , drop = F]
        }
        if(!is_blank(input$ass_box_group) &&
           !is_blank(ass_box_group_levs$value)) {
            cond_g <- data[[input$ass_box_group]] %in% ass_box_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$ass_box_facet_r) &&
           !is_blank(ass_box_facet_rlevs$value)) {
            cond_r <- data[[input$ass_box_facet_r]]%in%ass_box_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        if(!is_blank(input$ass_box_facet_c) &&
           !is_blank(ass_box_facet_clevs$value)) {
            cond_c <- data[[input$ass_box_facet_c]]%in%ass_box_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- c('Choose' = '', unique(data[[xlabel_col]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_box_visits) &&
                all(input$ass_box_visits %in% choices),
            input$ass_box_visits, ''
        ))
        updateSelectizeInput(
            session, 'ass_box_visits', 'Visit(s)', choices, selected,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })

    # select x variable, categorical
    output$ass_box_x <- renderUI({
        req(req(input$ass_type) == 'Box plot')
        data <- ass_data_input$value
        # get categorical column names
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        selected <- isolate(
            ternary(is.null(input$ass_box_x), '', input$ass_box_x)
        )
        selectInput('ass_box_x', 'X (categorical)', choices, selected)
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Box plot')
        data <- ass_data_input$value
        if(!is_blank(input$ass_box_bmk)) {
            cond_param <- data[[param_col]] %in% input$ass_box_bmk
            data <- data[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_box_visits)) {
            cond_visits <- data[[xlabel_col]] %in% input$ass_box_visits
            data <- data[cond_visits, , drop = F]
        }
        if(!is_blank(input$ass_box_group) &&
           !is_blank(ass_box_group_levs$value)) {
            cond_g <- data[[input$ass_box_group]] %in% ass_box_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$ass_box_facet_r) &&
           !is_blank(ass_box_facet_rlevs$value)) {
            cond_r <- data[[input$ass_box_facet_r]]%in%ass_box_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        if(!is_blank(input$ass_box_facet_c) &&
           !is_blank(ass_box_facet_clevs$value)) {
            cond_c <- data[[input$ass_box_facet_c]]%in%ass_box_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        if(!is_blank(input$ass_box_group))
            choices <- setdiff(choices, input$ass_box_group)
        if(!is_blank(input$ass_box_facet_r))
            choices <- setdiff(choices, input$ass_box_facet_r)
        if(!is_blank(input$ass_box_facet_c))
            choices <- setdiff(choices, input$ass_box_facet_c)
        selected <- isolate(
            ternary(is.null(input$ass_box_x), '', input$ass_box_x)
        )
        updateSelectInput(
            session, 'ass_box_x', 'X (categorical)', choices, selected
        )
    })

    # select x variable levels
    output$ass_box_xlevs <- renderUI({
        req(req(input$ass_type) == 'Box plot', input$ass_box_x)
        data <- ass_data_input$value
        choices <- sort(unique(data[[input$ass_box_x]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_box_xlevs) &&
                all(input$ass_box_xlevs %in% choices),
            input$ass_box_xlevs, choices
        ))
        selectizeInput(
            'ass_box_xlevs', input$ass_box_x, choices,
            selected = selected, multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Box plot',
            input$ass_box_x)
        data <- ass_data_input$value
        if(!is_blank(input$ass_box_bmk)) {
            cond_param <- data[[param_col]] %in% input$ass_box_bmk
            data <- data[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_box_visits)) {
            cond_visits <- data[[xlabel_col]] %in% input$ass_box_visits
            data <- data[cond_visits, , drop = F]
        }
        if(!is_blank(input$ass_box_group) &&
           !is_blank(ass_box_group_levs$value)) {
            cond_g <- data[[input$ass_box_group]] %in% ass_box_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$ass_box_facet_r) &&
           !is_blank(ass_box_facet_rlevs$value)) {
            cond_r <- data[[input$ass_box_facet_r]]%in%ass_box_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        if(!is_blank(input$ass_box_facet_c) &&
           !is_blank(ass_box_facet_clevs$value)) {
            cond_c <- data[[input$ass_box_facet_c]]%in%ass_box_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- sort(unique(data[[input$ass_box_x]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_box_xlevs) &&
                all(input$ass_box_xlevs %in% choices),
            input$ass_box_xlevs, choices
        ))
        updateSelectizeInput(
            session, 'ass_box_xlevs', input$ass_box_x, choices, selected,
            options = list(plugins = list('drag_drop','remove_button'))
        )
        ass_box_xlevs$value <- selected
    })
    ass_box_xlevs <- reactiveValues(value = NULL)
    observe({
        req(req(input$ass_type) == 'Box plot')
        if(is_blank(input$ass_box_x)) {
            ass_box_xlevs$value <- NULL
        } else {
            data <- ass_data_input$value
            ass_box_xlevs$value <- sort(unique(data[[input$ass_box_x]]))
        }
    })
    observe({ ass_box_xlevs$value <- input$ass_box_xlevs })

    # select group variable, categorical
    output$ass_box_group <- renderUI({
        req(req(input$ass_type) == 'Box plot')
        data <- ass_data_input$value
        # get categorical column names
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        selected <- isolate(
            ternary(is.null(input$ass_box_group), '', input$ass_box_group)
        )
        selectInput('ass_box_group', 'Group', choices, selected)
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Box plot')
        data <- ass_data_input$value
        if(!is_blank(input$ass_box_bmk)) {
            cond_param <- data[[param_col]] %in% input$ass_box_bmk
            data <- data[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_box_visits)) {
            cond_visits <- data[[xlabel_col]] %in% input$ass_box_visits
            data <- data[cond_visits, , drop = F]
        }
        if(!is_blank(input$ass_box_x) &&
           !is_blank(ass_box_xlevs$value)) {
            cond_x <- data[[input$ass_box_x]] %in% ass_box_xlevs$value
            data <- data[cond_x, , drop = F]
        }
        if(!is_blank(input$ass_box_facet_r) &&
           !is_blank(ass_box_facet_rlevs$value)) {
            cond_r <- data[[input$ass_box_facet_r]]%in%ass_box_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        if(!is_blank(input$ass_box_facet_c) &&
           !is_blank(ass_box_facet_clevs$value)) {
            cond_c <- data[[input$ass_box_facet_c]]%in%ass_box_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        if(!is_blank(input$ass_box_x))
            choices <- setdiff(choices, input$ass_box_x)
        if(!is_blank(input$ass_box_facet_r))
            choices <- setdiff(choices, input$ass_box_facet_r)
        if(!is_blank(input$ass_box_facet_c))
            choices <- setdiff(choices, input$ass_box_facet_c)
        selected <- isolate(
            ternary(is.null(input$ass_box_group), '', input$ass_box_group)
        )
        updateSelectInput(
            session, 'ass_box_group', 'Group', choices, selected
        )
    })

    # select group variable levels
    output$ass_box_group_levs <- renderUI({
        req(req(input$ass_type) == 'Box plot', input$ass_box_group)
        data <- ass_data_input$value
        choices <- sort(unique(data[[input$ass_box_group]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_box_group_levs) &&
                all(input$ass_box_group_levs %in% choices),
            input$ass_box_group_levs, choices
        ))
        selectizeInput(
            'ass_box_group_levs', input$ass_box_group, choices,
            selected = selected, multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Box plot',
            input$ass_box_group)
        data <- ass_data_input$value
        if(!is_blank(input$ass_box_bmk)) {
            cond_param <- data[[param_col]] %in% input$ass_box_bmk
            data <- data[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_box_visits)) {
            cond_visits <- data[[xlabel_col]] %in% input$ass_box_visits
            data <- data[cond_visits, , drop = F]
        }
        if(!is_blank(input$ass_box_x) && !is_blank(ass_box_xlevs$value)) {
            cond_x <- data[[input$ass_box_x]] %in% ass_box_xlevs$value
            data <- data[cond_x, , drop = F]
        }
        if(!is_blank(input$ass_box_facet_r) &&
           !is_blank(ass_box_facet_rlevs$value)) {
            cond_r <- data[[input$ass_box_facet_r]]%in%ass_box_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        if(!is_blank(input$ass_box_facet_c) &&
           !is_blank(ass_box_facet_clevs$value)) {
            cond_c <- data[[input$ass_box_facet_c]]%in%ass_box_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- sort(unique(data[[input$ass_box_group]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_box_group_levs) &&
                all(input$ass_box_group_levs %in% choices),
            input$ass_box_group_levs, choices
        ))
        updateSelectizeInput(
            session, 'ass_box_group_levs', input$ass_box_group,choices,selected,
            options = list(plugins = list('drag_drop','remove_button'))
        )
        ass_box_group_levs$value <- selected
    })
    
    ass_box_group_levs <- reactiveValues(value = NULL)
    observe({
        req(req(input$ass_type) == 'Box plot')
        if(is_blank(input$ass_box_group)) {
            ass_box_group_levs$value <- NULL
        } else {
            data <- ass_data_input$value
            ass_box_group_levs$value <- sort(unique(data[[input$ass_box_group]]))
        }
    })
    observe({ ass_box_group_levs$value <- input$ass_box_group_levs })

    # select facet row variable, categorical
    output$ass_box_facet_r <- renderUI({
        req(req(input$ass_type) == 'Box plot')
        data <- ass_data_input$value
        # get categorical column names
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        selected <- isolate(
            if(is.null(input$ass_box_facet_r)) '' else input$ass_box_facet_r
        )
        selectInput('ass_box_facet_r', 'Facet row', choices, selected)
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Box plot')
        data <- ass_data_input$value
        if(!is_blank(input$ass_box_bmk)) {
            cond_param <- data[[param_col]] %in% input$ass_box_bmk
            data <- data[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_box_visits)) {
            cond_visits <- data[[xlabel_col]] %in% input$ass_box_visits
            data <- data[cond_visits, , drop = F]
        }
        if(!is_blank(input$ass_box_x) && !is_blank(ass_box_xlevs$value)) {
            cond_x <- data[[input$ass_box_x]] %in% ass_box_xlevs$value
            data <- data[cond_x, , drop = F]
        }
        if(!is_blank(input$ass_box_group) &&
           !is_blank(ass_box_group_levs$value)) {
            cond_g <- data[[input$ass_box_group]] %in% ass_box_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$ass_box_facet_c) &&
           !is_blank(ass_box_facet_clevs$value)) {
            cond_c <- data[[input$ass_box_facet_c]]%in%ass_box_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        if(!is_blank(input$ass_box_x))
            choices <- setdiff(choices, input$ass_box_x)
        if(!is_blank(input$ass_box_group))
            choices <- setdiff(choices, input$ass_box_group)
        if(!is_blank(input$ass_box_facet_c))
            choices <- setdiff(choices, input$ass_box_facet_c)
        selected <- isolate(
            if(is.null(input$ass_box_facet_r)) '' else input$ass_box_facet_r
        )
        updateSelectInput(
            session, 'ass_box_facet_r', 'Facet row', choices, selected
        )
    })

    # select facet row variable levels
    output$ass_box_facet_rlevs <- renderUI({
        req(req(input$ass_type) == 'Box plot', input$ass_box_facet_r)
        data <- ass_data_input$value
        choices <- sort(unique(data[[input$ass_box_facet_r]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_box_facet_rlevs) &&
                all(input$ass_box_facet_rlevs %in% choices),
            input$ass_box_facet_rlevs, choices
        ))
        selectizeInput(
            'ass_box_facet_rlevs', input$ass_box_facet_r, choices,
            selected = selected, multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Box plot',
            input$ass_box_facet_r)
        data <- ass_data_input$value
        if(!is_blank(input$ass_box_bmk)) {
            cond_param <- data[[param_col]] %in% input$ass_box_bmk
            data <- data[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_box_visits)) {
            cond_visits <- data[[xlabel_col]] %in% input$ass_box_visits
            data <- data[cond_visits, , drop = F]
        }
        if(!is_blank(input$ass_box_x) && !is_blank(ass_box_xlevs$value)) {
            cond_x <- data[[input$ass_box_x]] %in% ass_box_xlevs$value
            data <- data[cond_x, , drop = F]
        }
        if(!is_blank(input$ass_box_group) &&
           !is_blank(ass_box_group_levs$value)) {
            cond_g <- data[[input$ass_box_group]] %in% ass_box_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$ass_box_facet_c) &&
           !is_blank(ass_box_facet_clevs$value)) {
            cond_c <- data[[input$ass_box_facet_c]]%in%ass_box_facet_clevs$value
            data <- data[cond_c, , drop = F]
        }
        choices <- sort(unique(data[[input$ass_box_facet_r]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_box_facet_rlevs) &&
                all(input$ass_box_facet_rlevs %in% choices),
            input$ass_box_facet_rlevs, choices
        ))
        updateSelectizeInput(
            session,'ass_box_facet_rlevs',input$ass_box_facet_r,choices,selected,
            options = list(plugins = list('drag_drop','remove_button'))
        )
        ass_box_facet_rlevs$value <- selected
    })
    
    ass_box_facet_rlevs <- reactiveValues(value = NULL)
    observe({
        req(req(input$ass_type) == 'Box plot')
        if(is_blank(input$ass_box_facet_r)) {
            ass_box_facet_rlevs$value <- NULL
        } else {
            data <- ass_data_input$value
            ass_box_facet_rlevs$value <- sort(unique(data[[input$ass_box_facet_r]]))
        }
    })
    observe({ ass_box_facet_rlevs$value <- input$ass_box_facet_rlevs })

    # select facet column variable, categorical
    output$ass_box_facet_c <- renderUI({
        req(req(input$ass_type) == 'Box plot')
        data <- ass_data_input$value
        # get categorical column names
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        selected <- isolate(
            if(is.null(input$ass_box_facet_c)) '' else input$ass_box_facet_c
        )
        selectInput('ass_box_facet_c', 'Facet column', choices, selected)
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Box plot')
        data <- ass_data_input$value
        if(!is_blank(input$ass_box_bmk)) {
            cond_param <- data[[param_col]] %in% input$ass_box_bmk
            data <- data[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_box_visits)) {
            cond_visits <- data[[xlabel_col]] %in% input$ass_box_visits
            data <- data[cond_visits, , drop = F]
        }
        if(!is_blank(input$ass_box_x) && !is_blank(ass_box_xlevs$value)) {
            cond_x <- data[[input$ass_box_x]] %in% ass_box_xlevs$value
            data <- data[cond_x, , drop = F]
        }
        if(!is_blank(input$ass_box_group) &&
           !is_blank(ass_box_group_levs$value)) {
            cond_g <- data[[input$ass_box_group]] %in% ass_box_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$ass_box_facet_r) &&
           !is_blank(ass_box_facet_rlevs$value)) {
            cond_r <- data[[input$ass_box_facet_r]]%in%ass_box_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        choices <- c('Choose' = '', names(data)[!sapply(data, is.numeric)])
        if(!is_blank(input$ass_box_x))
            choices <- setdiff(choices, input$ass_box_x)
        if(!is_blank(input$ass_box_group))
            choices <- setdiff(choices, input$ass_box_group)
        if(!is_blank(input$ass_box_facet_r))
            choices <- setdiff(choices, input$ass_box_facet_r)
        selected <- isolate(
            if(is.null(input$ass_box_facet_c)) '' else input$ass_box_facet_c
        )
        updateSelectInput(
            session, 'ass_box_facet_c', 'Facet column', choices, selected
        )
    })

    # select facet column variable levels
    output$ass_box_facet_clevs <- renderUI({
        req(req(input$ass_type) == 'Box plot', input$ass_box_facet_c)
        data <- ass_data_input$value
        choices <- sort(unique(data[[input$ass_box_facet_c]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_box_facet_clevs) &&
                all(input$ass_box_facet_clevs %in% choices),
            input$ass_box_facet_clevs, choices
        ))
        selectizeInput(
            'ass_box_facet_clevs', input$ass_box_facet_c, choices,
            selected = selected, multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        req(ass_data_input$value, req(input$ass_type) == 'Box plot',
            input$ass_box_facet_c)
        data <- ass_data_input$value
        if(!is_blank(input$ass_box_bmk)) {
            cond_param <- data[[param_col]] %in% input$ass_box_bmk
            data <- data[cond_param, , drop = F]
        }
        if(!is_blank(input$ass_box_visits)) {
            cond_visits <- data[[xlabel_col]] %in% input$ass_box_visits
            data <- data[cond_visits, , drop = F]
        }
        if(!is_blank(input$ass_box_x) && !is_blank(ass_box_xlevs$value)) {
            cond_x <- data[[input$ass_box_x]] %in% ass_box_xlevs$value
            data <- data[cond_x, , drop = F]
        }
        if(!is_blank(input$ass_box_group) &&
           !is_blank(ass_box_group_levs$value)) {
            cond_g <- data[[input$ass_box_group]] %in% ass_box_group_levs$value
            data <- data[cond_g, , drop = F]
        }
        if(!is_blank(input$ass_box_facet_r) &&
           !is_blank(ass_box_facet_rlevs$value)) {
            cond_r <- data[[input$ass_box_facet_r]]%in%ass_box_facet_rlevs$value
            data <- data[cond_r, , drop = F]
        }
        choices <- sort(unique(data[[input$ass_box_facet_c]]))
        selected <- isolate(ternary(
            !is_blank(input$ass_box_facet_clevs) &&
                all(input$ass_box_facet_clevs %in% choices),
            input$ass_box_facet_clevs, choices
        ))
        updateSelectizeInput(
            session,'ass_box_facet_clevs',input$ass_box_facet_c,choices,selected,
            options = list(plugins = list('drag_drop','remove_button'))
        )
        ass_box_facet_clevs$value <- selected
    })
    
    ass_box_facet_clevs <- reactiveValues(value = NULL)
    observe({
        req(req(input$ass_type) == 'Box plot')
        if(is_blank(input$ass_box_facet_c)) {
            ass_box_facet_clevs$value <- NULL
        } else  {
            data <- ass_data_input$value
            ass_box_facet_clevs$value <- sort(unique(data[[input$ass_box_facet_c]]))
        }
    })
    observe({ ass_box_facet_clevs$value <- input$ass_box_facet_clevs })
    
    # --- UI widgets for refining box plot ---
    
    # x-axis label
    output$ass_box_xlab <- renderUI({
        req(ass_box_show$value)
        textInput('ass_box_xlab', 'X-axis label', '{VariableX}')
    })
    ass_box_xlab <- reactiveValues(value = NULL)
    observe({
        req(ass_box_show$value)
        ass_box_xlab$value <- '{VariableX}'
    })
    observe({
        input$ass_box_xlab
        ass_box_xlab$value <- input$ass_box_xlab
    })
    
    # y-axis label
    output$ass_box_ylab <- renderUI({
        req(ass_box_show$value)
        textInput('ass_box_ylab', 'Y-axis label', '{BiomarkerY} at {VisitsY}')
    })
    ass_box_ylab <- reactiveValues(value = NULL)
    observe({
        req(ass_box_show$value)
        ass_box_ylab$value <- '{BiomarkerY} at {VisitsY}'
    })
    observe({
        input$ass_box_ylab
        ass_box_ylab$value <- input$ass_box_ylab
    })
    
    # plot title
    output$ass_box_title <- renderUI({
        req(ass_box_show$value)
        value <- ifelse(!is_blank(input$ass_study), 'Study: {Study}', '')
        if(!is_blank(input$ass_cohort))
            value <- paste(value, '{Cohort} cohort', sep = '\n')
        value <- trimws(value)
        textAreaInput('ass_box_title', 'Plot title', value)
    })
    ass_box_title <- reactiveValues(value = NULL)
    observe({
        req(ass_box_show$value)
        value <- ifelse(!is_blank(input$ass_study), 'Study: {Study}', '')
        if(!is_blank(input$ass_cohort))
            value <- paste(value, '{Cohort} cohort', sep = '\n')
        value <- trimws(value)
        ass_box_title$value <- value
    })
    observe({
        input$ass_box_title
        ass_box_title$value <- input$ass_box_title
    })
    
    # add footnote
    output$ass_box_footnote_in <- renderUI({
        req(ass_box_show$value)
        textAreaInput('ass_box_footnote_in', 'Plot footnote', '')
    })
    ass_box_footnote_in <- reactiveValues(value = '')
    observe({
        value <- NULL
        if(!is_blank(input$ass_box_test)) {
            value <- paste('P value is calculated from', input$ass_box_test)
        }
        if(!is_blank(input$ass_box_footnote_in)) {
            value <- trimws(paste(value, input$ass_box_footnote_in, sep = '\n'))
        }
        ass_box_footnote_in$value <- value
    })
    
    # checkBox for log of y variable
    output$ass_box_log_y <- renderUI({
        req(ass_box_show$value)
        checkboxInput('ass_box_log_y', 'Log Y', value = FALSE)
    })
    ass_box_log_y <- reactiveValues(value = FALSE)
    observe({
        input$ass_box_log_y
        ass_box_log_y$value <- input$ass_box_log_y
    })
    
    # checkBox for adding sample size annotations
    output$ass_box_add_sample_size <- renderUI({
        req(ass_box_show$value)
        checkboxInput('ass_box_add_sample_size', 'Sample size', value = TRUE)
    })
    ass_box_add_sample_size <- reactiveValues(value = TRUE)
    observe({
        input$ass_box_add_sample_size
        ass_box_add_sample_size$value <- input$ass_box_add_sample_size
    })
    
    # checkBox for adding points
    output$ass_box_add_points <- renderUI({
        req(ass_box_show$value)
        checkboxInput('ass_box_add_points', 'Points', value = TRUE)
    })
    ass_box_add_points <- reactiveValues(value = TRUE)
    observe({
        input$ass_box_add_points
        ass_box_add_points$value <- input$ass_box_add_points
    })
    
    # checkBox for showing subject IDs
    output$ass_box_show_subjid <- renderUI({
        req(ass_box_show$value)
        checkboxInput('ass_box_show_subjid', 'Subject IDs', value = FALSE)
    })
    ass_box_show_subjid <- reactiveValues(value = FALSE)
    observe({
        input$ass_box_show_subjid
        ass_box_show_subjid$value <- input$ass_box_show_subjid
    })
    
    # add reference line(s)
    output$ass_box_refline <- renderUI({
        req(ass_box_show$value)
        textInput('ass_box_refline', 'Add a reference line', value = '')
    })
    ass_box_refline <- reactiveValues(value = NULL)
    observe({
        if(!is_blank(input$ass_box_refline)) {
            value <- as.numeric(stringr::str_trim(unlist(strsplit(as.character(
                input$ass_box_refline
            ), ','))))
            ass_box_refline$value <- value
        } else ass_box_refline$value <- NULL
    })
    
    # add equality test methods
    output$ass_box_test <- renderUI({
        has_x <- !is_blank(input$ass_box_x) && !is_blank(ass_box_xlevs$value)
        has_group <- !is_blank(input$ass_box_group) &&
            !is_blank(ass_box_group_levs$value)
        req(ass_box_show$value, has_x || has_group)
        if(has_group) ncategories <- length(ass_box_group_levs$value)
        else ncategories <- length(ass_box_xlevs$value)
        req(ncategories >= 2)
        if(ncategories == 2) {
            choices <- c('Choose' = '', ass_box_tests[['wilcox']],
                         ass_box_tests[['t']])
        } else {
            choices <- c('Choose' = '',ass_box_tests[['kruskal']],
                         ass_box_tests[['anova']], ass_box_tests[['jonckheere']])
        }
        selected <- isolate(
            if(is.null(input$ass_box_test)) NULL else input$ass_box_test
        )
        selectInput('ass_box_test', 'Test method', choices, selected)
    })
    
    # --- Output for Box plot ---
    
    # data for boxplot
    ass_box_data <- reactiveValues(value = NULL)
    observe({
        data <- ass_data_input$value
        if(isTRUE(ass_box_show$value)) {
            if(!is_blank(input$ass_box_bmk) && is_blank(input$ass_download_plot_multibmk)) {
                data <- data[data[[param_col]] %in% input$ass_box_bmk, , drop = F]
            }
            else if(!is_blank(input$ass_download_plot_multibmk)) {
              data <- data[data[[param_col]] %in% input$ass_download_plot_multibmk, , drop = F]
            }
            if(!is_blank(input$ass_box_visits)) {
                data <- data[data[[xlabel_col]] %in% input$ass_box_visits, , drop = F]
            }
            if(!is_blank(input$ass_box_x)) {
                req(ass_box_xlevs$value)
                cond_x <- data[[input$ass_box_x]] %in% ass_box_xlevs$value
                data <- data[cond_x, , drop = F]
                data[[input$ass_box_x]] <- factor(
                    data[[input$ass_box_x]], levels = ass_box_xlevs$value
                )
            }
            if(!is_blank(input$ass_box_group)) {
                req(ass_box_group_levs$value)
                cond_g <- data[[input$ass_box_group]] %in% ass_box_group_levs$value
                data <- data[cond_g, , drop = F]
                data[[input$ass_box_group]] <- factor(
                    data[[input$ass_box_group]], levels = ass_box_group_levs$value
                )
            }
            if(!is_blank(input$ass_box_facet_r)) {
                req(ass_box_facet_rlevs$value)
                cond_r <- data[[input$ass_box_facet_r]] %in% ass_box_facet_rlevs$value
                data <- data[cond_r, , drop = F]
                data[[input$ass_box_facet_r]] <- factor(
                    data[[input$ass_box_facet_r]],
                    levels = ass_box_facet_rlevs$value
                )
            }
            if(!is_blank(input$ass_box_facet_c)) {
                req(ass_box_facet_clevs$value)
                cond_c <- data[[input$ass_box_facet_c]] %in% ass_box_facet_clevs$value
                data <- data[cond_c, , drop = F]
                data[[input$ass_box_facet_c]] <- factor(
                    data[[input$ass_box_facet_c]],
                    levels = ass_box_facet_clevs$value
                )
            }
            ass_box_data$value <- data
        } else {
            ass_box_data$value <- NULL
        }
    })
    
    # boxplot shown status
    ass_box_show <- reactiveValues(value = FALSE)
    observe({
        data <- ass_data_input$value
        if(all(
            !is_blank(data), nrow(data) > 0, !is_blank(input$ass_type),
            input$ass_type == 'Box plot', !is_blank(input$ass_box_y),
            !is_blank(input$ass_box_bmk), !is_blank(input$ass_box_visits),
            !is.null(input$ass_box_x), !is.null(input$ass_box_facet_r),
            !is.null(input$ass_box_facet_c), !is.null(input$ass_box_group)
        )) {
            ass_box_show$value = TRUE
        } else ass_box_show$value = FALSE
    })
    
    # equality test result
    ass_box_test <- reactiveValues(value = NULL)
    observe({
        data <- ass_box_data$value
        # plot_ <- ass_box_plot$value
        has_x <- !is_blank(input$ass_box_x) && !is_blank(ass_box_xlevs$value)
        has_group <- !is_blank(input$ass_box_group) &&
            !is_blank(ass_box_group_levs$value)
        if(has_group) ncategories <- length(ass_box_group_levs$value)
        else ncategories <- length(ass_box_xlevs$value)
        if(all(isTRUE(ass_box_show$value), !is_blank(input$ass_box_test),
               has_x || has_group, ncategories >= 2,
               !is.null(data), nrow(data) > 0)) {
            group_list <- NULL
            if(!is_blank(input$ass_box_facet_r)) {
                group_list <- c(group_list, input$ass_box_facet_r)
                data[[input$ass_box_facet_r]] <- paste0(
                    input$ass_box_facet_r, ': ', data[[input$ass_box_facet_r]]
                )
            }
            if(!is_blank(input$ass_box_facet_c)) {
                group_list <- c(group_list, input$ass_box_facet_c)
                data[[input$ass_box_facet_c]] <- paste0(
                    input$ass_box_facet_c, ': ', data[[input$ass_box_facet_c]]
                )
            }
            has_x <- !is_blank(input$ass_box_x)
            has_group <- !is_blank(input$ass_box_group)
            has_both <- has_x && has_group
            if(has_both) group_list <- c(group_list, input$ass_box_x)
            dots_group <- lapply(group_list, as.symbol)
            group_var <- ifelse(has_group, input$ass_box_group, input$ass_box_x)
            formula_ <- formula(paste(input$ass_box_y, '~', group_var))
            
            test_method <- paste(
                names(ass_box_tests)[ass_box_tests == input$ass_box_test],
                'test', sep = '.'
            )
            
            #=========Added By WANGSHU
            if(test_method == 'jonckheere.test') {
              data[[group_var]] <- ordered(data[[group_var]])
              test_method <- 'JonckheereTerpstraTest'
            }
            
            test_func <- match.fun(test_method)
            if(is_blank(dots_group)) {
                p_value_expr <- lazyeval::interp(
                    ~var[[1]]$p.value, var = as.name('test')
                )
            } else {
                p_value_expr <- lazyeval::interp(
                    ~var$p.value,var = as.name('test')
                )
            }
            if(has_both)
                x_expr <- lazyeval::interp(~as.integer(var),
                                           var = as.name(input$ass_box_x))
            else x_expr <- ~(-Inf)
            dots_summarise <- setNames(c(
                as.list(group_list), list(p_value_expr, x_expr, ~(Inf))
            ), c(group_list, 'p_value', 'x', 'y'))
            if(!is_blank(dots_group)) {
                data <- data %>%
                    arrange_(.dots = dots_group) %>%
                    group_by_(.dots = dots_group)
            }

            test_result <- data %>%
              do(test = tryCatch(
                suppressWarnings(test_func(formula_, data = .)),
                error = function(e) {NULL}
              )) %>%
              filter(!is.null(test)) %>%
              summarise_(.dots = dots_summarise) %>%
              mutate(p_value_text = paste('p value =', round(p_value, 3)))

            ass_box_test$value <- test_result
        } else {
            ass_box_test$value <- NULL
        }
    })
    
    
    ## 
    # equality test result for multi boxplot output
    ass_box_m_test <- reactive({
 
      data <- ass_box_data$value
      ## object to save all the test data frame for each biomarker;
      multi_bm_test <- list()
      unique_bmk <- c(unique(input$ass_download_plot_multibmk))
      for (i in unique_bmk){
        multi_bm1 <- data[data[[param_col]] == i, , drop = F]
        has_x <- !is_blank(input$ass_box_x) && !is_blank(ass_box_xlevs$value)
        has_group <- !is_blank(input$ass_box_group) &&
          !is_blank(ass_box_group_levs$value)
        if(has_group) ncategories <- length(ass_box_group_levs$value)
        else ncategories <- length(ass_box_xlevs$value)
        if(all(isTRUE(ass_box_show$value), !is_blank(input$ass_box_test),
               has_x || has_group, ncategories >= 2,
               !is.null(multi_bm1), nrow(multi_bm1) > 0)) {
          group_list <- NULL
          if(!is_blank(input$ass_box_facet_r)) {
            group_list <- c(group_list, input$ass_box_facet_r)
            multi_bm1[[input$ass_box_facet_r]] <- paste0(
              input$ass_box_facet_r, ': ', multi_bm1[[input$ass_box_facet_r]]
            )
          }
          if(!is_blank(input$ass_box_facet_c)) {
            group_list <- c(group_list, input$ass_box_facet_c)
            multi_bm1[[input$ass_box_facet_c]] <- paste0(
              input$ass_box_facet_c, ': ', multi_bm1[[input$ass_box_facet_c]]
            )
          }
          has_x <- !is_blank(input$ass_box_x)
          has_group <- !is_blank(input$ass_box_group)
          has_both <- has_x && has_group
          if(has_both) group_list <- c(group_list, input$ass_box_x)
          dots_group <- lapply(group_list, as.symbol)
          group_var <- ifelse(has_group, input$ass_box_group, input$ass_box_x)
          formula_ <- formula(paste(input$ass_box_y, '~', group_var))
          test_method <- paste(
            names(ass_box_tests)[ass_box_tests == input$ass_box_test],
            'test', sep = '.'
          )
          test_func <- match.fun(test_method)
          if(is_blank(dots_group)) {
            p_value_expr <- lazyeval::interp(
              ~var[[1]]$p.value, var = as.name('test')
            )
          } else {
            p_value_expr <- lazyeval::interp(
              ~var$p.value,var = as.name('test')
            )
          }
          if(has_both)
            x_expr <- lazyeval::interp(~as.integer(var),
                                       var = as.name(input$ass_box_x))
          else x_expr <- ~(-Inf)
          dots_summarise <- setNames(c(
            as.list(group_list), list(p_value_expr, x_expr, ~(Inf))
          ), c(group_list, 'p_value', 'x', 'y'))
          if(!is_blank(dots_group)) {
            multi_bm1 <- multi_bm1 %>%
              arrange_(.dots = dots_group) %>%
              group_by_(.dots = dots_group)
          }
          test_result <- multi_bm1 %>%
            do(test = tryCatch(
              suppressWarnings(test_func(formula_, data = .)),
              error = function(e) {NULL}
            )) %>%
            filter(!is.null(test)) %>%
            summarise_(.dots = dots_summarise) %>%
            mutate(p_value_text = paste('p value =', round(p_value, 3)))
          #ass_box_test$value <- test_result
          multi_bm_test[[i]] <- test_result
        } else {
          #ass_box_test$value <- NULL
          multi_bm_test[[i]] <- NULL
        }
        
      }
      return(multi_bm_test)
      
    })
    
    # ggplot object for boxplot
    ass_box_plot <- reactiveValues(value = NULL)
    ass_box_plot_data <- reactiveValues(value = NULL)
    observe({
        data <- ass_box_data$value
        if(all(!is_blank(data), nrow(data) > 0)) {
            if(is_blank(ass_box_facet_rlevs$value)) facet_r_levels <- NULL
            else {
                facet_r_levels <- setNames(
                    ass_box_facet_rlevs$value,
                    paste0(input$ass_box_facet_r, ': ',
                           ass_box_facet_rlevs$value)
                )
            }
            if(is_blank(ass_box_facet_clevs$value)) facet_c_levels <- NULL
            else {
                facet_c_levels <- setNames(
                    ass_box_facet_clevs$value,
                    paste0(input$ass_box_facet_c, ': ',
                           ass_box_facet_clevs$value)
                )
            }
            
            #x_lab <- r_format(ass_box_xlab$value, tnf$ass_current)
            #y_lab <- r_format(ass_box_ylab$value, tnf$ass_current)
            #plot_title <- r_format(ass_box_title$value, tnf$ass_current)
            
            x_lab <- ass_box_xlab$value
            y_lab <- ass_box_ylab$value
            plot_title <- ass_box_title$value
            
            plot_obj <- gg_boxplot(
                data, input$ass_box_x, input$ass_box_y, subj_col,
                facet_r = input$ass_box_facet_r,
                facet_c = input$ass_box_facet_c,
                facet_r_levels = facet_r_levels, facet_c_levels = facet_c_levels,
                color_var = input$ass_box_group,
                x_lab = x_lab, y_lab = y_lab, title = plot_title,
                x_limit = ass_box_brush_range$xlim,
                y_limit = ass_box_brush_range$ylim,
                y_log = ass_box_log_y$value,
                add_points = ass_box_add_points$value, point_shape = 19,
                add_legend = TRUE, legend_pos = 'bottom',
                reference_hline = ass_box_refline$value,
                # text_content = NULL, text_pos = 'topleft',
                # xtick_angle = 0, xtick_align = 'center',
                add_sample_size = ass_box_add_sample_size$value,
                grids = 'on', add_label = ass_box_show_subjid$value,
                return_data = TRUE
            )
            plot_ <- plot_obj$plot
            if(!is.null(ass_box_test$value)) {
                if(!is_blank(input$ass_box_x) && !is_blank(input$ass_box_group))
                    hjust = 0.5
                else hjust = -0.2
                plot_ <- plot_ +
                    geom_text(data = ass_box_test$value,
                              aes(x, y, label = p_value_text),
                              inherit.aes = FALSE,
                              hjust = hjust, vjust = 1.5)
            }
            ass_box_plot$value <- plot_
            ass_box_plot_data$value <- plot_obj$data
        } else {
            ass_box_plot$value <- NULL
            ass_box_plot_data$value <- NULL
        }
    })
    
    # ggplot boxplot object for multi biomarker output  
    #ass_box_m_plot_data <- reactiveValues(value = NULL)
    ass_box_m_plot <- reactive({
      
      req(!is_blank(input$ass_download_plot_multibmk))

      #Calculate test p values
      tmp <- ass_box_m_test()
      ## object to save all the data frame for each biomarker;
      multi_bm_obj <- list()
      unique_bmk <- c(unique(input$ass_download_plot_multibmk))
      data <- ass_box_data$value
      
      
      for (i in unique_bmk){
        multi_bm1 <- data[data[[param_col]] == i, , drop = F]
        
        if(all(!is_blank(multi_bm1), nrow(multi_bm1) > 0)) {
          if(is_blank(ass_box_facet_rlevs$value)) facet_r_levels <- NULL
          else {
            facet_r_levels <- setNames(
              ass_box_facet_rlevs$value,
              paste0(input$ass_box_facet_r, ': ',
                     ass_box_facet_rlevs$value)
            )
          }
          if(is_blank(ass_box_facet_clevs$value)) facet_c_levels <- NULL
          else {
            facet_c_levels <- setNames(
              ass_box_facet_clevs$value,
              paste0(input$ass_box_facet_c, ': ',
                     ass_box_facet_clevs$value)
            )
          }
          
          #x_lab <- r_format(ass_box_xlab$value, tnf$ass_current)
          #y_lab <- r_format(ass_box_ylab$value, tnf$ass_current)
          #plot_title <- r_format(ass_box_title$value, tnf$ass_current)
          
          x_lab <- ass_box_xlab$value
          y_lab <- ass_box_ylab$value
          if (ass_box_title$value == 'Study: {Study}') {
            plot_title <- i
            
          }
          else if (ass_box_title$value != 'Study: {Study}') {
            plot_title <- paste0(ass_box_title$value, i , sep = '\n')
          }
          
          
          plot_obj <- gg_boxplot(
            multi_bm1, input$ass_box_x, input$ass_box_y, subj_col,
            facet_r = input$ass_box_facet_r,
            facet_c = input$ass_box_facet_c,
            facet_r_levels = facet_r_levels, facet_c_levels = facet_c_levels,
            color_var = input$ass_box_group,
            x_lab = x_lab, y_lab = y_lab, title = plot_title,
            x_limit = ass_box_brush_range$xlim,
            y_limit = ass_box_brush_range$ylim,
            y_log = ass_box_log_y$value,
            add_points = ass_box_add_points$value, point_shape = 19,
            add_legend = TRUE, legend_pos = 'bottom',
            reference_hline = ass_box_refline$value,
            # text_content = NULL, text_pos = 'topleft',
            # xtick_angle = 0, xtick_align = 'center',
            add_sample_size = ass_box_add_sample_size$value,
            grids = 'on', add_label = ass_box_show_subjid$value,
            return_data = TRUE
          )
          plot_ <- plot_obj$plot
          if(!is.null(tmp[[i]])) {
            if(!is_blank(input$ass_box_x) && !is_blank(input$ass_box_group))
              hjust = 0.5
            else hjust = -0.2
            plot_ <- plot_ +
              geom_text(data = tmp[[i]],
                        aes(x, y, label = p_value_text),
                        inherit.aes = FALSE,
                        hjust = hjust, vjust = 1.5)
          }
          #ass_box_plot$value <- plot_
          multi_bm_obj[[i]] <- plot_
          #ass_box_plot_data$value <- plot_obj$data
        } else {
          #ass_box_plot$value <- NULL
          multi_bm_obj[[i]] <- NULL
          #ass_box_plot_data$value <- NULL
        }
      }

      return(multi_bm_obj)
    })
    
    
    
    
    
    
    output$ass_box_output <- renderUI({
        req(ass_box_data$value)
        tagList(
          tabsetPanel(
            tabPanel("Graph",
              tags$div(
                  style = 'position:relative',
                  plotOutput(
                      'ass_box_plot',
                      dblclick = 'ass_box_dblclick',
                      hover = hoverOpts('ass_box_hover', delay = 100,
                                        delayType = 'debounce'),
                      brush = brushOpts(id = 'ass_box_brush', resetOnNew = T,
                                        delay = 1000)
                  ),
                  uiOutput('ass_box_hover')
              ),
              uiOutput('ass_box_footnote_out'),
              tags$br(),
              uiOutput('ass_box_table')
            ),
            tabPanel("Association Summary Table",
                     htmlOutput('ass_box_summary')
                     #plotOutput('time_summary')
                     
            ) 
        )
      ) 
    })
    
    # draw boxplot
    output$ass_box_plot <- renderPlot({
        req(ass_box_plot$value)
        plot_ <- ass_box_plot$value
        plot_
    })
    
    # footnote for boxplot
    output$ass_box_footnote_out <- renderUI({
        req(!is.null(ass_box_footnote_in$value))
        value <- trimws(paste(
            strsplit(ass_box_footnote_in$value, '\n')[[1]], collapse = '<br/>'
        ))
        HTML(value)
    })
    
    # -- Boxplot hover tooltip
    output$ass_box_hover <- renderUI({
        req(ass_box_show$value, ass_box_plot_data$value)
        data <- ass_box_plot_data$value
        data <- data[!is.na(data[[input$ass_box_y]]), , drop = F]
        tooltip(input$ass_box_hover, data, 'x_point', input$ass_box_y, c(subj_col))
    })
    
    # -- Boxplot brush
    # Link brush range to a reactive value
    ass_box_brush_range <- reactiveValues(xlim = NULL, ylim = NULL)
    observe({
        brush <- input$ass_box_brush
        if(!is.null(brush)) {
            ass_box_brush_range$xlim <- c(brush$xmin, brush$xmax)
            ass_box_brush_range$ylim <- c(brush$ymin, brush$ymax)
        }
    })
    # double click to reset the graph
    observeEvent(input$ass_box_dblclick, {
        if((!is.null(ass_box_brush_range$xlim)) ||
           !is.null(ass_box_brush_range$ylim))
        {
            ass_box_brush_range$xlim <- NULL
            ass_box_brush_range$ylim <- NULL
        }
    })
    
    # boxplot associated table
    boxplot_table <- reactive({
        req(ass_box_show$value)
        
    })

    
    #####################################################################
    # produce the Association boxplot summary table with the graph (GH)
    #####################################################################
    ass_box_summary <- reactive({
      req(ass_box_show$value)
      req(!is.null(input$ass_box_x))
      
      data <- ass_box_data$value
      
      dgt <- 2
      summary_func <- c(
        'N' = n_nna,
        'Mean (SD)' = partial(mean_sd_str, digits = dgt),
        '%CV' = partial(coeff_var_str, digits = dgt),
        'Median' = partial(median_str, digits = dgt),
        'Q1, Q3' = partial(q1_q3_str, digits = dgt),
        'Min, Max' = partial(min_max_str, digits = dgt)
      )
      if(isTRUE(input$time_to_log)) {
        summary_func <- c(
          summary_func,
          'Geom Mean (%CV)' = partial(geo_mean_cv_str, digits = dgt),
          'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
        )
      }
      data <- mutate(data, hist_sum_all = seq(0,0,nrow(data)-1))
      # Summary table without grouping;
      if(input$ass_type == 'Box plot' && is_blank(input$ass_box_x) && is_blank(input$ass_box_group) && is_blank(input$ass_box_facet_r) && is_blank(input$ass_box_facet_c)){
        summary_tbl <- summary_table_all(
          data, row_var = 'hist_sum_all',row_names = ' ',
          col_var = , val_var = input$ass_box_y,
          col_totals = ,
          name_totals = ,
          n_in_header = FALSE, subj_col = subj_col,
          baseline_name = NULL,
          add_cfb = , cfb_var = ,
          func_list = summary_func,
          caption = '',
          footnote = '',
          rowlabel = '', format = 'html'
        )
      }
      # Summary table with grouping in x axis;
      if(input$ass_type == 'Box plot' && !is_blank(input$ass_box_x) && is_blank(input$ass_box_group) && is_blank(input$ass_box_facet_r) && is_blank(input$ass_box_facet_c)){
        summary_tbl <- summary_table_all(
          data, row_var = input$ass_box_x,
          col_var = , val_var = input$ass_box_y,
          col_totals = ,
          name_totals = ,
          n_in_header = FALSE, subj_col = subj_col,
          baseline_name = NULL,
          add_cfb = , cfb_var = ,
          func_list = summary_func,
          caption = '',
          footnote = '',
          rowlabel = '', format = 'html'
        )
      }
      # Summary table with grouping by input$ass_box_group;
      else if(input$ass_type == 'Box plot' && !is_blank(input$ass_box_x) && !is_blank(input$ass_box_group) && is_blank(input$ass_box_facet_r) && is_blank(input$ass_box_facet_c)){
        summary_tbl <- summary_table_all(
          data, row_var = input$ass_box_x,
          col_var = input$ass_box_group, val_var = input$ass_box_y,
          col_totals = ,
          name_totals = ,
          n_in_header = FALSE, subj_col = subj_col,
          baseline_name = NULL,
          add_cfb = , cfb_var =,
          func_list = summary_func,
          caption = '',
          footnote = '',
          rowlabel = '', format = 'html'
        )
      }
      # Summary table with grouping by input$ass_row;
      else if(input$ass_type == 'Box plot' && !is_blank(input$ass_box_x) && is_blank(input$ass_box_group) && !is_blank(input$ass_box_facet_r) && is_blank(input$ass_box_facet_c)){
        summary_tbl <- summary_table_all(
          data, row_var = input$ass_box_x,
          col_var = input$ass_box_facet_r, val_var = input$ass_box_y,
          col_totals = ,
          name_totals = ,
          n_in_header = FALSE, subj_col = subj_col,
          baseline_name = NULL,
          add_cfb = , cfb_var =,
          func_list = summary_func,
          caption = '',
          footnote = '',
          rowlabel = '', format = 'html'
        )
      }
      # Summary table with grouping by input$ass_column;
      else if(input$ass_type == 'Box plot' && !is_blank(input$ass_box_x) && is_blank(input$ass_box_group) && is_blank(input$ass_box_facet_r) && !is_blank(input$ass_box_facet_c)){
        summary_tbl <- summary_table_all(
          data, row_var = input$ass_box_x,
          col_var = input$ass_box_facet_c, val_var = input$ass_box_y,
          col_totals = ,
          name_totals = ,
          n_in_header = FALSE, subj_col = subj_col,
          baseline_name = NULL,
          add_cfb = , cfb_var =,
          func_list = summary_func,
          caption = '',
          footnote = '',
          rowlabel = '', format = 'html'
        )
      }
      
      # Summary table with grouping by input$ass_box_group and input$ass_box_facet_r;
      else if(input$ass_type == 'Box plot' && !is_blank(input$ass_box_x) && !is_blank(input$ass_box_group) && !is_blank(input$ass_box_facet_r) && is_blank(input$ass_box_facet_c)){
        # subset data into different dataframe by input$ass_box_facet_r
        unique_fr <- c(unique(ass_box_facet_rlevs$value))
        summary_tbl=NULL
        d = NULL
        for (i in unique_fr) {
          #tmp_data <- data[[input$ass_box_facet_c]] == unique_fc[i]
          tmp_data <- data[
            data[[input$ass_box_facet_r]] == i,
            , drop = F
            ]  
          d <- summary_table_all(
            tmp_data, row_var = input$ass_box_x,
            col_var = input$ass_box_group, val_var = input$ass_box_y,
            col_totals = ,
            name_totals = ,
            n_in_header = FALSE, subj_col = subj_col,
            baseline_name = NULL,
            add_cfb = , cfb_var =,
            func_list = summary_func,
            caption = '',
            footnote = '',
            rowlabel = '', format = 'html'
          )
          summary_tbl = rbind(summary_tbl,i,d)
        }
      }
      
      # Summary table with grouping by input$ass_box_group and input$ass_box_facet_c;
      else if(input$ass_type == 'Box plot' && !is_blank(input$ass_box_x) && !is_blank(input$ass_box_group) && is_blank(input$ass_box_facet_r) && !is_blank(input$ass_box_facet_c)){
        # subset data into different dataframe by input$ass_box_facet_r
        #facet_row_levls <- unique_na(data[[input$ass_box_facet_r]])
        unique_fc <- c(unique(ass_box_facet_clevs$value))
        #facet_col_dim = unique_fc
        summary_tbl=NULL
        d = NULL
        for (i in unique_fc) {
          #tmp_data <- data[[input$ass_box_facet_c]] == unique_fc[i]
          tmp_data <- data[
            data[[input$ass_box_facet_c]] == i,
            , drop = F
            ]  
          d <- summary_table_all(
            tmp_data, row_var = input$ass_box_x,
            col_var = input$ass_box_group, val_var = input$ass_box_y,
            col_totals = ,
            name_totals = ,
            n_in_header = FALSE, subj_col = subj_col,
            baseline_name = NULL,
            add_cfb = , cfb_var =,
            func_list = summary_func,
            caption = '',
            footnote = '',
            rowlabel = '', format = 'html'
          )
          summary_tbl = rbind(summary_tbl,i,d)
        }
      }
      
      # Summary table with grouping by input$ass_box_facet_c and input$ass_box_facet_r;
      else if(input$ass_type == 'Box plot' && !is_blank(input$ass_box_x) && is_blank(input$ass_box_group) && !is_blank(input$ass_box_facet_r) && !is_blank(input$ass_box_facet_c)){
        # subset data into different dataframe by input$ass_box_facet_r
        unique_fr <- c(unique(ass_box_facet_rlevs$value))
        summary_tbl=NULL
        d = NULL
        for (i in unique_fr) {
          #tmp_data <- data[[input$ass_box_facet_c]] == unique_fc[i]
          tmp_data <- data[
            data[[input$ass_box_facet_r]] == i,
            , drop = F
            ]  
          d <- summary_table_all(
            tmp_data, row_var = input$ass_box_x,
            col_var = input$ass_box_facet_c, val_var = input$ass_box_y,
            col_totals = ,
            name_totals = ,
            n_in_header = FALSE, subj_col = subj_col,
            baseline_name = NULL,
            add_cfb = , cfb_var =,
            func_list = summary_func,
            caption = '',
            footnote = '',
            rowlabel = '', format = 'html'
          )
          summary_tbl = rbind(summary_tbl,i,d)
        }
      }
      # Summary table with grouping by input$ass_box_group and input$ass_box_facet_c and input$ass_box_facet_r;
      else if(input$ass_type == 'Box plot' && !is_blank(input$ass_box_x) && !is_blank(input$ass_box_group) && !is_blank(input$ass_box_facet_r) && !is_blank(input$ass_box_facet_c)){
        # subset data into different dataframe by input$ass_box_facet_r
        unique_fr <- c(unique(ass_box_facet_rlevs$value))
        unique_fc <- c(unique(ass_box_facet_clevs$value))
        summary_tbl=NULL
        d = NULL
        for (i in unique_fr) {
          
          summary_tbl_c = NULL
          
          tmp_data_r <- data[
            data[[input$ass_box_facet_r]] == i,
            , drop = F
            ] 
          for (j in unique_fc) {
            d_c = NULL
            tmp_data_c <- tmp_data_r[
              tmp_data_r[[input$ass_box_facet_c]] == j,
              , drop = F
              ]                
            d_c <- summary_table_all(
              tmp_data_c, row_var = input$ass_box_x,
              col_var = input$ass_box_group, val_var = input$ass_box_y,
              col_totals = ,
              name_totals = ,
              n_in_header = FALSE, subj_col = subj_col,
              baseline_name = NULL,
              add_cfb = , cfb_var =,
              func_list = summary_func,
              caption = '',
              footnote = '',
              rowlabel = '', format = 'html'
            )
            ij <- c(",")
            summary_tbl_c = rbind(summary_tbl_c,i,ij,j, d_c)
          }
          
          summary_tbl = rbind(summary_tbl,summary_tbl_c)
        }
      }
      return(summary_tbl)
    })
    
    
    output$ass_box_summary <- renderUI({
      req(ass_box_show$value)
      HTML(ass_box_summary())
    })    
    
    # --- UI widgets for contingency table ---
    # a selectInput for choosing x variable
    output$ass_table_x <- renderUI({
        req(input$ass_type)
        if(input$ass_type != 'Contingency table') return()
        data <- ass_data_input$value
        choices <- col_continuity()$discrete
        selectInput('ass_table_x', 'Categorical X', c('Choose' = '', choices))
    })
    # a selectInput for choosing visit time for X
    output$ass_table_time_x <- renderUI({
        req(input$ass_type)
        if(input$ass_type != 'Contingency table') return()
        data <- ass_data_input$value
        selectizeInput(
            'ass_table_time_x', 'Visit(s)',
            c('Choose'='', unique(data[[xlabel_col]])), multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    # a checkbox for deciding whether X is biomarker dependent
    output$ass_table_bmk_depend_x <- renderUI({
        req(input$ass_type)
        if(input$ass_type != 'Contingency table') return()
        req(input$ass_table_x)
        checkboxInput(
            'ass_table_bmk_depend_x', label = 'Is it biomarker-dependent?', value = F
        )
    })
    # a selectInput for choosing biomarker for X
    output$ass_table_bmk_x <- renderUI({
        req(input$ass_type)
        if(input$ass_type != 'Contingency table') return()
        req(input$ass_table_bmk_depend_x)
        names_bmk <- unique(ass_data_input$value[[param_col]])
        selectInput('ass_table_bmk_x', 'Biomarker X', c('Choose'='', names_bmk))
    })

    # a selectInput for choosing Y variable
    output$ass_table_y <- renderUI({
        req(input$ass_type)
        if(input$ass_type != 'Contingency table') return()
        data <- ass_data_input$value
        choices <- col_continuity()$discrete
        selectInput('ass_table_y', 'Categorical Y', c('Choose' = '', choices))
    })
    # a selectInput for choosing visit time for Y
    output$ass_table_time_y <- renderUI({
        req(input$ass_type)
        if(input$ass_type != 'Contingency table') return()
        data <- ass_data_input$value
        selectizeInput(
            'ass_table_time_y', 'Visit(s)',
            c('Choose'='', unique(data[[xlabel_col]])), multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    # a checkbox for deciding whether Y is biomarker dependent
    output$ass_table_bmk_depend_y <- renderUI({
        req(input$ass_type)
        if(input$ass_type != 'Contingency table') return()
        req(input$ass_table_y)
        checkboxInput(
            'ass_table_bmk_depend_y', label = 'Is it biomarker-dependent?', value = F
        )
    })
    # a selectInput for choosing biomarker for Y
    output$ass_table_bmk_y <- renderUI({
        req(input$ass_type)
        if(input$ass_type != 'Contingency table') return()
        req(input$ass_table_bmk_depend_y)
        names_bmk <- unique(ass_data_input$value[[param_col]])
        selectInput('ass_table_bmk_y', 'Biomarker Y', c('Choose'='', names_bmk))
    })
    # a radiobutton for specifying percentage
    output$ass_table_percentage <- renderUI({
        req(input$ass_type)
        if(input$ass_type != 'Contingency table') return()
        radioButtons('ass_table_percentage', 'Add %', ass_table_percentage_types,
                     inline = TRUE)
    })
    # an selectInput for choosing statistical test method
    output$ass_table_test <- renderUI({
        req(input$ass_type)
        if(input$ass_type != 'Contingency table') return()
        selectInput(
            'ass_table_test', 'Statistical test',
            c('Choose' = '', ass_table_statistical_test)
        )
    })
    
    # --- Output for contingency table ---
    ass_table_data <- reactive({
        req(ass_table_show$value)
        data <- ass_data_input$value
        data_x <- data[data[[xlabel_col]] %in% input$ass_table_time_x, , drop = F]
        data_y <- data[data[[xlabel_col]] %in% input$ass_table_time_y, , drop = F]
        if(is_blank(input$ass_table_bmk_x)) {
            if(is_blank(input$ass_cohort))
                data_x <- group_by_(data_x, study_col, subj_col, xvar_col)
            else
                data_x <- group_by_(data_x, study_col, cohort_col, subj_col, xvar_col)
            data_x <- filter(data_x, row_number() == 1)
        } else {
            data_x <- data_x[data_x[[param_col]] == input$ass_table_bmk_x, , drop = F]
        }
        if(is_blank(input$ass_table_bmk_y)) {
            if(is_blank(input$ass_cohort))
                data_y <- group_by_(data_y, study_col, subj_col, xvar_col)
            else
                data_y <- group_by_(data_y, study_col, cohort_col, subj_col, xvar_col)
            data_y <- filter(data_y, row_number() == 1)
        } else {
            data_y <- data_y[data_y[[param_col]] == input$ass_table_bmk_y, , drop = F]
        }
        by_vars <- c(study_col)
        if(!is_blank(input$ass_cohort)) by_vars <- c(by_vars, cohort_col)
        by_vars <- c(by_vars, subj_col)
        if(identical(input$ass_table_time_x, input$ass_table_time_y))
            by_vars <- c(by_vars, xlabel_col)
        data_x <- data_x[, c(by_vars, input$ass_table_x)]
        data_y <- data_y[, c(by_vars, input$ass_table_y)]
        data <- merge(data_x, data_y, by = by_vars, all = TRUE)
        return(data)
    })
    ass_table_attr <- reactive({
        var_x <- input$ass_table_x
        var_y <- input$ass_table_y
        if(identical(var_x, var_y)) {
            var_x <- paste(var_x, 'x', sep = '.')
            var_y <- paste(var_y, 'y', sep = '.')
        }
        visit_x <- ifelse(length(input$ass_table_time_x) > 1,
                          '', input$ass_table_time_x)
        visit_y <- ifelse(length(input$ass_table_time_y) > 1,
                          '', input$ass_table_time_y)
        name_x <- input$ass_table_x
        name_y <- input$ass_table_y
        if(!is_blank(input$ass_table_bmk_x)) name_x <- input$ass_table_bmk_x
        if(!is_blank(input$ass_table_bmk_y)) name_y <- input$ass_table_bmk_y
        main <- ifelse(
            !is_blank(input$ass_study), paste0('Study: ', input$ass_study), ''
        )
        if(!is_blank(input$ass_cohort))
            main <- paste(main, paste(input$ass_cohort, 'cohort'), sep = '\n')
        main <- trimws(main)
        return(list(
            var_x = var_x, name_x = name_x, visit_x = visit_x,
            var_y = var_y, name_y = name_y, visit_y = visit_y,
            main = main
        ))
    })
    # A reactive value linked to the status of contingency table
    ass_table_show <- reactiveValues(value = FALSE)
    observe({
        if(any(is_blank(input$ass_table_x), is_blank(input$ass_table_time_x),
               is_blank(input$ass_table_y), is_blank(input$ass_table_time_y))) {
            ass_table_show$value <- FALSE
        } else {
            if((length(input$ass_table_time_x) > 1 ||
                length(input$ass_table_time_y) > 1) &&
               !identical(input$ass_table_time_x, input$ass_table_time_y)) {
                ass_table_show$value <- FALSE
            } else if((isTRUE(input$ass_table_bmk_depend_x) &&
                       is_blank(input$ass_table_bmk_x)) ||
                      (isTRUE(input$ass_table_bmk_depend_y) &&
                       is_blank(input$ass_table_bmk_y))) {
                ass_table_show$value <- FALSE
            } else ass_table_show$value <- TRUE
        }
    })
    observe({
        input$ass_type
        ass_table_show$value <- FALSE
    })
    # output contingency table
    output$ass_table_output <- renderUI({
        req(ass_table_show$value)
        data <- ass_table_data()
        table_attr <- ass_table_attr()
        HTML(html_contingency_table(
            data, table_attr$var_x, table_attr$var_y, table_attr$name_x,
            table_attr$name_y, table_attr$visit_x, table_attr$visit_y,
            percentage = input$ass_table_percentage, caption = table_attr$main,
            test_method = input$ass_table_test
        )$html)
    })
    
    # --- UI widgets for correlation matrix plot ---
    
    # a radioButtons for choosing biomarker correlation type
    output$ass_cormat_cortype <- renderUI({
        req(input$ass_type)
        if(input$ass_type != 'Correlation matrix') return()
        radioButtons(
            'ass_cormat_cortype', 'Correlation type',
            choices = c('intra-biomarker', 'inter-biomarker')
        )
    })
    # a radioButton for choosing matrix plot type
    output$ass_cormat_plottype <- renderUI({
        req(input$ass_type)
        if(input$ass_type != 'Correlation matrix') return()
        radioButtons(
            'ass_cormat_plottype', 'Plot type',
            choices = c('Scatter matrix', 'circle matrix')
        )
    })
    # an selectizeInput for choosing biomarker(s)
    output$ass_cormat_bmk <- renderUI({
        req(input$ass_type)
        if(input$ass_type != 'Correlation matrix') return()
        req(input$ass_cormat_cortype)
        if(input$ass_cormat_cortype == 'intra-biomarker') {
            label_ <- 'Biomarker(s)'
        } else if(input$ass_cormat_cortype == 'inter-biomarker') {
            label_ <- 'Biomarkers'
        }
        selectizeInput(
            'ass_cormat_bmk', label_,
            c('Choose' = '', bmk_names()), multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    # an selectInput for choosing y
    output$ass_cormat_y <- renderUI({
        req(input$ass_type)
        if(input$ass_type != 'Correlation matrix') return()
        selectInput(
            'ass_cormat_y', 'Value',
            c('Choose' = '', aval_col, base_col, chg_col, pchg_col, prchg_col)
        )
    })
    # an selectInput for choosing visit
    output$ass_cormat_time <- renderUI({
        req(input$ass_type)
        if(input$ass_type != 'Correlation matrix') return()
        req(input$ass_cormat_cortype)
        data <- subset_data()$data
        if(input$ass_cormat_cortype == 'intra-biomarker') {
            label_ <- 'Visits'
        } else if(input$ass_cormat_cortype == 'inter-biomarker') {
            label_ <- 'Visit(s)'
        }
        selectizeInput(
            'ass_cormat_time', label_,
            c('Choose' = '', unique(data[[xlabel_col]])), multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    
    # --- Output for correlation matrix plot ---
    
    # prepare data for correlation matrix
    ass_cormat_data <- reactive({
        req(ass_cormat_show$value)
        data <- subset_data()$data
        if(!is_blank(input$ass_study)) {
            data <- data[data[[study_col]] == input$ass_study, ]
        }
        if(!is_blank(input$ass_cohort)) {
            data <- data[data[[cohort_col]] == input$ass_cohort, ]
        }
        data <- data[data[[param_col]] %in% input$ass_cormat_bmk, ]
        data <- data[data[[xlabel_col]] %in% input$ass_cormat_time, ]
        if(input$ass_cormat_cortype == 'intra-biomarker') {
            formula_ <- paste(param_col, '+', subj_col, '~', xlabel_col)
        } else if(input$ass_cormat_cortype == 'inter-biomarker') {
            formula_ <- paste(xlabel_col, '+', subj_col, '~', param_col)
        }
        data <- reshape2::dcast(
            data, formula_, value.var = input$ass_cormat_y,
            fun.aggregate = mean_na
        )
    })
    # A reactive value linked to the status of correlation matrix plot
    ass_cormat_show <- reactiveValues(value = FALSE)
    observe({
        if(any(is_blank(input$ass_cormat_cortype), is_blank(input$ass_cormat_bmk),
               is_blank(input$ass_cormat_y), is_blank(input$ass_cormat_time))) {
            ass_cormat_show$value <- FALSE
        } else {
            ass_cormat_show$value <- TRUE
        }
    })
    observe({
        input$ass_type
        ass_cormat_show$value <- FALSE
    })
    # Insert the right number of plot output objects into the web page
    n_ass_cormat_plot <- reactiveValues(value = NULL)
    observe({
        req(ass_cormat_show$value)
        data <- ass_cormat_data()
        if(input$ass_cormat_cortype == 'intra-biomarker')
            n_ass_cormat_plot$value <- length(input$ass_cormat_bmk)
        else if(input$ass_cormat_cortype == 'inter-biomarker')
            n_ass_cormat_plot$value <- length(input$ass_cormat_time)
    })
    output$ass_cormat_output <- renderUI({
        req(ass_cormat_show$value, n_ass_cormat_plot$value)
        data <- ass_cormat_data()
        plot_list <- lapply(1:n_ass_cormat_plot$value, function(i) {
            plotname <- paste('ass_cormat_plot', i, sep = '_')
            plotOutput(plotname)
        })
    })
    observe({
        req(ass_cormat_show$value, n_ass_cormat_plot$value)
        data <- ass_cormat_data()
        if(input$ass_cormat_cortype == 'intra-biomarker') {
            var_widget <- input$ass_cormat_bmk
            var_colname <- param_col
        } else if(input$ass_cormat_cortype == 'inter-biomarker') {
            var_widget <- input$ass_cormat_time
            var_colname <- xlabel_col
        }
        value_cols <- !(colnames(data) %in% c(subj_col, var_colname))
        for(i in 1:n_ass_cormat_plot$value) {
            local({
                plotname <- paste('ass_cormat_plot', i, sep = '_')
                var_ith <- var_widget[i]
                output[[plotname]] <- renderPlot({
                    data_ith <- data[data[[var_colname]] == var_ith,
                                     value_cols, drop = FALSE]
                    if(!(is.null(dim(data_ith)) || ncol(data_ith) <= 1)) {
                        if(input$ass_cormat_plottype == 'Scatter matrix')
                            scatter_matrix(data_ith, main = var_ith)
                        else if(input$ass_cormat_plottype == 'circle matrix') {
                            cormat <- cor(data_ith, use = "pairwise.complete.obs",
                                          method = "spearman")
                            suppressMessages(corrplot::corrplot.mixed(
                                cormat, order = "hclust",
                                hclust.method = "ward", title = var_ith
                            ))
                        }
                    }
                })
            })
        }
    })
    
    
    # --- Association plot/table download  ---
    
    # -- Association plot download
    # download button to specify the attributes of the plot to download
    output$ass_download_plot_action <- renderUI({
        req(any(ass_hist_show$value, ass_scatter_show$value,
                ass_box_show$value, ass_cormat_show$value))
        actionButton('ass_download_plot_action', 'Download plot')
    })
    # download page height numeric input widget for association graph
    output$ass_download_plot_height <- renderUI({
        numericInput(
            'ass_download_plot_height', 'Height (inches)', value = 6
        )
    })
    # download page width numeric input widget for association graph
    output$ass_download_plot_width <- renderUI({
        numericInput(
            'ass_download_plot_width', 'Width (inches)', value = 7
        )
    })
    # download button widget on download page for association histogram
    output$ass_download_hist_button <- renderUI({
        req(!is_blank(input$ass_download_plot_format),
            !is_blank(input$ass_download_plot_height),
            !is_blank(input$ass_download_plot_width),
            ass_hist_show$value)
        downloadButton('ass_download_hist', strong('Download Plot'))
    })

    output$ass_download_hist <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'asso_histogram_', format(Sys.Date(),format = '%Y%m%d'),
                '.', input$ass_download_plot_format
            )
            return(file_name)
        },
        content = function(file) {
            height = input$ass_download_plot_height
            width = input$ass_download_plot_width
            dev <- plot_dev(tolower(input$ass_download_plot_format), file,
                            dpi = 600)
            
            
            #####################################
            # Single/ Multiple biomarker plot output
            #####################################
            if(is_blank(input$ass_download_plot_multibmk) || (!is_blank(input$ass_download_plot_multibmk) && length(input$ass_hist_bmk) > 1)){
             
            dev(file = file, width = width, height = height)
              plot_ <- add_footnote(ass_hist_plot(),
                                    trimws(ass_hist_footnote_in$value))
              grid.draw(plot_)
              dev.off()
            } 
            
            else if(!is_blank(input$ass_download_plot_multibmk)){
              pdf(file = file, width = width, height = height)
              for (i in 1:length(c(unique(input$ass_download_plot_multibmk)))){
                temp <- ass_hist_m_plot()
                plot_ <- temp[[i]]
                print(plot_)
              }
              dev.off()
            }  
              
              
        }
    )
    # download button widget on download page for association scatter plot
    output$ass_download_scatter_button <- renderUI({
        req(!is_blank(input$ass_download_plot_format),
            !is_blank(input$ass_download_plot_height),
            !is_blank(input$ass_download_plot_width),
            ass_scatter_show$value)
        downloadButton('ass_download_scatter', strong('Download Plot'))
    })
    output$ass_download_scatter <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'asso_scatter_plot_', format(Sys.Date(),format = '%Y%m%d'),
                '.', input$ass_download_plot_format
            )
            return(file_name)
        },
        content = function(file) {
            height = input$ass_download_plot_height
            width = input$ass_download_plot_width
            dev <- plot_dev(tolower(input$ass_download_plot_format), file,
                            dpi = 600)
            dev(file = file, width = width, height = height)
            plot_ <- add_footnote(ass_scatter_plot(),
                                  trimws(ass_scatter_footnote_in$value))
            grid.draw(plot_)
            dev.off()
        }
    )
    # download button widget on download page for association box plot
    output$ass_download_box_button <- renderUI({
        req(!is_blank(input$ass_download_plot_format),
            !is_blank(input$ass_download_plot_height),
            !is_blank(input$ass_download_plot_width),
            ass_box_show$value)
        downloadButton('ass_download_box', strong('Download Plot'))
    })
    output$ass_download_box <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'asso_boxplot_', format(Sys.Date(),format = '%Y%m%d'),
                '.', input$ass_download_plot_format
            )
            return(file_name)
        },
        content = function(file) {
            height = input$ass_download_plot_height
            width = input$ass_download_plot_width
            dev <- plot_dev(tolower(input$ass_download_plot_format), file,
                            dpi = 600)
            #####################################
            # Single/ Multiple biomarker plot output
            #####################################
            if(is_blank(input$ass_download_plot_multibmk) || (!is_blank(input$ass_download_plot_multibmk) && length(input$ass_box_bmk) > 1)){
              
              dev(file = file, width = width, height = height)
              
              
              plot_ <- add_footnote(ass_box_plot$value,
                                    trimws(ass_box_footnote_in$value))
              
              grid.draw(plot_)
              dev.off()
            }
            else if(!is_blank(input$ass_download_plot_multibmk)){
              pdf(file = file, width = width, height = height)
              for (i in 1:length(c(unique(input$ass_download_plot_multibmk)))){
                temp <- ass_box_m_plot()
                plot_ <- temp[[i]]
                print(plot_)
              }
              dev.off()
            }  
            
        }
    )
    # download button widget on download page for correlation matrix plot
    output$ass_download_cormat_button <- renderUI({
        req(!is_blank(input$ass_download_plot_format),
            !is_blank(input$ass_download_plot_height),
            !is_blank(input$ass_download_plot_width),
            ass_cormat_show$value)
        downloadButton('ass_download_cormat', strong('Download Plot'))
    })
    output$ass_download_cormat <- downloadHandler(
        filename = function() {
            if(n_ass_cormat_plot$value <= 1) {
                file_name <- paste0(
                    'asso_cormat_', format(Sys.Date(),format = '%Y%m%d'),
                    '.', input$ass_download_plot_format
                )
            } else {
                file_name <- 'asso_cormat.zip'
            }
            return(file_name)
        },
        content = function(file) {
            height = input$ass_download_plot_height
            width = input$ass_download_plot_width
            data <- ass_cormat_data()
            if(input$ass_cormat_cortype == 'intra-biomarker') {
                var_widget <- input$ass_cormat_bmk
                var_colname <- param_col
            } else if(input$ass_cormat_cortype == 'inter-biomarker') {
                var_widget <- input$ass_cormat_time
                var_colname <- xlabel_col
            }
            value_cols <- !(colnames(data) %in% c(subj_col, var_colname))
            if(n_ass_cormat_plot$value > 1) fnames <- c()
            for(i in 1:n_ass_cormat_plot$value) {
                var_ith <- var_widget[i]
                if(n_ass_cormat_plot$value <= 1) fname <- file
                else {
                    fname <- paste0(
                        'asso_cormat_', var_ith, '_',
                        format(Sys.Date(),format = '%Y%m%d'),
                        '.', input$ass_download_plot_format
                    )
                    fnames <- c(fnames, fname)
                }
                if(input$ass_download_plot_format == 'pdf')
                    pdf(fname, height = height, width = width)
                else if(input$ass_download_plot_format == 'png')
                    png(fname, height = height, width = width, units = 'in', res = 600)
                else if(input$ass_download_plot_format == 'jpg')
                    jpeg(fname, height = height, width = width, units = 'in', res = 600)
                else if(input$ass_download_plot_format == 'ps')
                    postscript(fname, height = height, width = width)
                data_ith <- data[data[[var_colname]] == var_ith,
                                 value_cols, drop = FALSE]
                if(!(is.null(dim(data_ith)) || ncol(data_ith) <= 1)) {
                    if(input$ass_cormat_plottype == 'Scatter matrix')
                        scatter_matrix(data_ith, main = var_ith)
                    else if(input$ass_cormat_plottype == 'circle matrix') {
                        cormat <- cor(
                            data_ith, use = "pairwise.complete.obs",
                            method = "spearman"
                        )
                        suppressMessages(corrplot::corrplot.mixed(
                            cormat, order = "hclust",
                            hclust.method = "ward", title = var_ith
                        ))
                    }
                }
                dev.off()
            }
            if(n_ass_cormat_plot$value > 1) {
                system2('zip', args = shQuote(c('-r9X', file, fnames)), stdout = F)
            }
        }
    )
    
    # -- Association table download
    # download button to specify the attributes of the table to download
    output$ass_download_table_action <- renderUI({
        req(ass_table_show$value)
        actionButton('ass_download_table_action', 'Download table')
    })
    # download page height numeric input widget for association table
    output$ass_download_table_height <- renderUI({
        numericInput(
            'ass_download_table_height', 'Height (inches)',
            value = ass_table_height_default
        )
    })
    # download page width numeric input widget for association table
    output$ass_download_table_width <- renderUI({
        numericInput(
            'ass_download_table_width', 'Width (inches)',
            value = ass_table_width_default
        )
    })
    # download button widget on download page for association table
    output$ass_download_table_button <- renderUI({
        req(!is_blank(input$ass_download_table_format),
            !is_blank(input$ass_download_table_height),
            !is_blank(input$ass_download_table_width))
        downloadButton('ass_download_table', strong('Download Table'))
    })
    output$ass_download_table <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'asso_table_', format(Sys.Date(),format = '%Y%m%d'),
                '.', input$ass_download_table_format
            )
            return(file_name)
        },
        content = function(file) {
            data <- ass_table_data()
            table_attr <- ass_table_attr()
            htmltbl <- html_contingency_table(
                data, table_attr$var_x, table_attr$var_y, table_attr$name_x,
                table_attr$name_y, table_attr$visit_x, table_attr$visit_y,
                percentage = input$ass_table_percentage, caption = table_attr$main,
                test_method = input$ass_table_test
            )$html
            htmlpage <- html_page(htmltbl)
            if(input$ass_download_table_format == 'html') {
                cat(htmlpage, file = file)
            } else if(input$ass_download_table_format == 'pdf') {
                file_html <- tempfile('table', fileext = '.html')
                cat(htmlpage, file = file_html)
                convert_command <- paste0(
                    'wkhtmltopdf -q ',
                    '--page-height ', input$ass_download_table_height, 'inch ',
                    '--page-width ', input$ass_download_table_width, 'inch ',
                    file_html, ' ', file
                )
                system(convert_command)
            }
        }
    )
    
    # output$ass_add_to_tnf <- renderUI({
    #     req(any(ass_hist_show$value, ass_scatter_show$value,
    #             ass_box_show$value, ass_table_show$value, ass_cormat_show$value))
    #     actionButton('ass_add_to_tnf', 'Add to output specs', icon = icon('cloud-upload'))
    # })
    # observeEvent(input$ass_add_to_tnf, {
    #     tnf$ass_count <- tnf$ass_count + 1
    #     
    #     file_key <- paste0('g-ass', tnf$time_count)
    #     tfl_type <- ifelse(input$ass_type == 'Contingency table', 'Table',
    #                        'Figure')
    #     biomarker_x <- NULL
    #     variable_x <- NULL
    #     levels_x <- NULL
    #     visits_x <- NULL
    #     log_x <- NULL
    #     biomarker_y <- NULL
    #     variable_y <- NULL
    #     levels_y <- NULL
    #     visits_y <- NULL
    #     log_y <- NULL
    #     group <- NULL
    #     group_levels <- NULL
    #     facet_row <- NULL
    #     facet_row_levels <- NULL
    #     facet_column <- NULL
    #     facet_column_levels <- NULL
    #     cormat_type <- NULL
    #     cormat_plot <- NULL
    #     stat_test <- NULL
    #     plot_title <- NULL
    #     plot_xlab <- NULL
    #     plot_ylab <- NULL
    #     plot_footnote <- NULL
    #     include_subjid <- NULL
    #     add_lines <- NULL
    #     
    #     if(input$ass_type == 'Histogram') {
    #         biomarker_y <- input$ass_hist_bmk
    #         variable_y <- input$ass_hist_var
    #         visits_y <- paste(input$ass_hist_visits, collapse = '\n')
    #         log_y <- input$ass_hist_log_x
    #         group <- input$ass_hist_group
    #         group_levels <- ass_hist_group_levs$value
    #         facet_row <- input$ass_hist_facet_r
    #         facet_row_levels <- ass_hist_facet_rlevs$value
    #         facet_column <- input$ass_hist_facet_c
    #         facet_column_levels <- ass_hist_facet_clevs$value
    #         plot_title <- ass_hist_title$value
    #         plot_xlab <- ass_hist_xlab$value
    #         plot_ylab <- ass_hist_ylab$value
    #         plot_footnote <- trimws(ass_hist_footnote_in$value)
    #     } else if(input$ass_type == 'Scatter plot') {
    #         biomarker_x <- input$ass_scatter_bmk_x
    #         variable_x <- input$ass_scatter_x
    #         visits_x <- paste(input$ass_scatter_visits_x, collapse = '\n')
    #         log_x <- input$ass_scatter_log_x
    #         biomarker_y <- input$ass_scatter_bmk_y
    #         variable_y <- input$ass_scatter_y
    #         visits_y <- paste(input$ass_scatter_visits_y, collapse = '\n')
    #         log_y <- input$ass_scatter_log_y
    #         group <- input$ass_scatter_group
    #         group_levels <- ass_scatter_group_levs$value
    #         facet_row <- input$ass_scatter_facet_r
    #         facet_row_levels <- ass_scatter_facet_rlevs$value
    #         facet_column <- input$ass_scatter_facet_c
    #         facet_column_levels <- ass_scatter_facet_clevs$value
    #         stat_test <- input$ass_scatter_test
    #         plot_title <- ass_scatter_title$value
    #         plot_xlab <- ass_scatter_xlab$value
    #         plot_ylab <- ass_scatter_ylab$value
    #         plot_footnote <- trimws(ass_scatter_footnote_in$value)
    #         include_subjid <- input$ass_scatter_toggle_subjid
    #         add_lines <- input$ass_scatter_add_line
    #     } else if(input$ass_type == 'Box plot') {
    #         variable_x <- input$ass_box_x
    #         visits_x <- paste(input$ass_box_visits, collapse = '\n')
    #         levels_x <- paste(ass_box_xlevs$value, collapse = '\n')
    #         biomarker_y <- input$ass_box_bmk
    #         variable_y <- input$ass_box_y
    #         log_y <- input$ass_box_log_y
    #         group <- input$ass_box_group
    #         group_levels <- ass_box_group_levs$value
    #         facet_row <- input$ass_box_facet_r
    #         facet_row_levels <- ass_box_facet_rlevs$value
    #         facet_column <- input$ass_box_facet_c
    #         facet_column_levels <- ass_box_facet_clevs$value
    #         stat_test <- input$ass_box_test
    #         plot_title <- ass_box_title$value
    #         plot_xlab <- ass_box_xlab$value
    #         plot_ylab <- ass_box_ylab$value
    #         plot_footnote <- trimws(ass_box_footnote_in$value)
    #         include_subjid <- input$ass_box_show_subjid
    #         add_lines <- paste(ass_box_refline$value, collapse = '\n')
    #     } else if(input$ass_type == 'Correlation matrix') {
    #         
    #     } else if(input$ass_type == 'Contingency table') {
    #         
    #     }
    #     
    #     row_list <- setNames(list(
    #         file_key, tfl_type, tnf$ass_count, input$ass_type,
    #         input$ass_study, input$ass_cohort, biomarker_x, variable_x,
    #         levels_x, visits_x, log_x, biomarker_y, variable_y,
    #         levels_y, visits_y, log_y, group, group_levels, facet_row,
    #         facet_row_levels, facet_column, facet_column_levels,
    #         input$ass_cormat_cortype, input$ass_cormat_plottype,
    #         stat_test, input$ass_table_percentage, NULL, NULL,
    #         input$ass_download_plot_format, input$ass_download_plot_height,
    #         input$ass_download_plot_width, NULL, plot_title,
    #         plot_xlab, plot_ylab, plot_footnote, input$ass_hist_geom,
    #         ass_box_add_sample_size$value, include_subjid,
    #         input$ass_scatter_add_ci, add_lines, subset_data()$subset_expr
    #     ), out_ass_cols)
    #     
    #     row_df <- data.frame(
    #         lapply(row_list, function(x) replace(x, is_length_zero(x), '')),
    #         check.names = FALSE, stringsAsFactors = FALSE
    #     )
    #     tnf$ass_df <- rbind(tnf$ass_df, row_df)
    # })

    observe({
        req(input$ass_type)
        file_key <- paste0('g-ass', tnf$time_count)
        tfl_type <- ifelse(input$ass_type == 'Contingency table', 'Table',
                           'Figure')
        biomarker_x <- NULL
        variable_x <- NULL
        levels_x <- NULL
        visits_x <- NULL
        log_x <- NULL
        biomarker_y <- NULL
        variable_y <- NULL
        levels_y <- NULL
        visits_y <- NULL
        log_y <- NULL
        group <- NULL
        group_levels <- NULL
        facet_row <- NULL
        facet_row_levels <- NULL
        facet_column <- NULL
        facet_column_levels <- NULL
        cormat_type <- NULL
        cormat_plot <- NULL
        stat_test <- NULL
        plot_title <- NULL
        plot_xlab <- NULL
        plot_ylab <- NULL
        plot_footnote <- NULL
        include_subjid <- NULL
        add_lines <- NULL

        if(input$ass_type == 'Histogram') {
            biomarker_y <- input$ass_hist_bmk
            variable_y <- input$ass_hist_var
            visits_y <- paste(input$ass_hist_visits, collapse = ', ')
            log_y <- input$ass_hist_log_x
            group <- input$ass_hist_group
            group_levels <- paste(ass_hist_group_levs$value, collapse = '\n')
            facet_row <- input$ass_hist_facet_r
            facet_row_levels <- paste(ass_hist_facet_rlevs$value, collapse = '\n')
            facet_column <- input$ass_hist_facet_c
            facet_column_levels <- paste(ass_hist_facet_clevs$value, collapse = '\n')
            plot_title <- ass_hist_title$value
            plot_xlab <- ass_hist_xlab$value
            plot_ylab <- ass_hist_ylab$value
            plot_footnote <- trimws(ass_hist_footnote_in$value)
        } else if(input$ass_type == 'Scatter plot') {
            biomarker_x <- input$ass_scatter_bmk_x
            variable_x <- input$ass_scatter_x
            visits_x <- paste(input$ass_scatter_visits_x, collapse = ', ')
            log_x <- input$ass_scatter_log_x
            biomarker_y <- input$ass_scatter_bmk_y
            variable_y <- input$ass_scatter_y
            visits_y <- paste(input$ass_scatter_visits_y, collapse = ', ')
            log_y <- input$ass_scatter_log_y
            group <- input$ass_scatter_group
            group_levels <- paste(ass_scatter_group_levs$value, collapse = '\n')
            facet_row <- input$ass_scatter_facet_r
            facet_row_levels <- paste(ass_scatter_facet_rlevs$value, collapse = '\n')
            facet_column <- input$ass_scatter_facet_c
            facet_column_levels <- paste(ass_scatter_facet_clevs$value, collapse = '\n')
            stat_test <- input$ass_scatter_test
            plot_title <- ass_scatter_title$value
            plot_xlab <- ass_scatter_xlab$value
            plot_ylab <- ass_scatter_ylab$value
            plot_footnote <- trimws(ass_scatter_footnote_in$value)
            include_subjid <- input$ass_scatter_toggle_subjid
            add_lines <- input$ass_scatter_add_line
        } else if(input$ass_type == 'Box plot') {
            variable_x <- input$ass_box_x
            levels_x <- paste(ass_box_xlevs$value, collapse = ', ')
            biomarker_y <- input$ass_box_bmk
            variable_y <- input$ass_box_y
            visits_y <- paste(input$ass_box_visits, collapse = ', ')
            log_y <- input$ass_box_log_y
            group <- input$ass_box_group
            group_levels <- paste(ass_box_group_levs$value, collapse = '\n')
            facet_row <- input$ass_box_facet_r
            facet_row_levels <- paste(ass_box_facet_rlevs$value, collapse = '\n')
            facet_column <- input$ass_box_facet_c
            facet_column_levels <- paste(ass_box_facet_clevs$value, collapse = '\n')
            stat_test <- input$ass_box_test
            plot_title <- ass_box_title$value
            plot_xlab <- ass_box_xlab$value
            plot_ylab <- ass_box_ylab$value
            plot_footnote <- trimws(ass_box_footnote_in$value)
            include_subjid <- input$ass_box_show_subjid
            add_lines <- paste(ass_box_refline$value, collapse = '\n')
        } else if(input$ass_type == 'Correlation matrix') {

        } else if(input$ass_type == 'Contingency table') {

        }

        row_list <- setNames(list(
            file_key, tfl_type, tnf$ass_count, input$ass_type,
            input$ass_study, input$ass_cohort, biomarker_x, variable_x,
            levels_x, visits_x, log_x, biomarker_y, variable_y,
            levels_y, visits_y, log_y, group, group_levels, facet_row,
            facet_row_levels, facet_column, facet_column_levels,
            input$ass_cormat_cortype, input$ass_cormat_plottype,
            stat_test, input$ass_table_percentage, NULL, NULL,
            input$ass_download_plot_format, input$ass_download_plot_height,
            input$ass_download_plot_width, NULL, plot_title,
            plot_xlab, plot_ylab, plot_footnote, input$ass_hist_geom,
            ass_box_add_sample_size$value, include_subjid,
            ass_box_add_points$value, input$ass_scatter_add_ci, add_lines,
            subset_data()$subset_expr
        ), out_ass_cols)

        row_df <- data.frame(
            lapply(row_list, function(x) replace(x, is_length_zero(x), '')),
            check.names = FALSE, stringsAsFactors = FALSE
        )

        tnf$ass_current <- row_df
    })
    
    # observeEvent(input$ass_add_to_tnf, {
    #     if(ass_hist_show$value) {
    #         tnf$ass_count <- tnf$ass_count + 1
    #         row_idx <- tnf$ass_count
    #         type_idx <- which(input$ass_type == ass_type_list)
    #         tnf$ass_count_type[type_idx] <- tnf$ass_count_type[type_idx] + 1
    #         tnf$ass_count_graph <- tnf$ass_count_graph + 1
    #         title_key_ <- paste0('g-asso', tnf$ass_count_graph)
    #         tfln_ <- paste0('2.', type_idx, '.', tnf$ass_count_type[type_idx])
    #         title_ <- 'Figure'
    #         ass_type <- 'Histogram'
    #         
    #         var_x <- input$ass_hist_var
    #         visit_x <- paste(input$ass_hist_time, collapse = ',')
    #         bmk_x <- input$ass_hist_bmk
    #         log_x <- input$ass_hist_to_log
    #         
    #         var_y <- visit_y <- bmk_y <- log_y <- NULL
    #         
    #         corr_type <- stat_test <- ptg_type <- NULL
    #         group_levels <- add_line <- add_points <- add_subjid <- NULL
    #         cormat_plottype <- NULL
    #     } else if(ass_scatter_show$value) {
    #         tnf$ass_count <- tnf$ass_count + 1
    #         row_idx <- tnf$ass_count
    #         type_idx <- which(input$ass_type == ass_type_list)
    #         tnf$ass_count_type[type_idx] <- tnf$ass_count_type[type_idx] + 1
    #         tnf$ass_count_graph <- tnf$ass_count_graph + 1
    #         title_key_ <- paste0('g-asso', tnf$ass_count_graph)
    #         tfln_ <- paste0('2.', type_idx, '.', tnf$ass_count_type[type_idx])
    #         title_ <- 'Figure'
    #         ass_type <- 'Scatter plot'
    #         
    #         var_x <- input$ass_scatter_x
    #         visit_x <- paste(input$ass_scatter_time_x, collapse = ',')
    #         bmk_x <- input$ass_scatter_bmk_x
    #         log_x <- input$ass_scatter_to_log_x
    #         
    #         var_y <- input$ass_scatter_y
    #         visit_y <- paste(input$ass_scatter_time_y, collapse = ',')
    #         bmk_y <- input$ass_scatter_bmk_y
    #         log_y <- input$ass_scatter_to_log_y
    #         
    #         corr_type <- input$ass_scatter_cortype
    #         add_line <- input$ass_scatter_add_line
    #         add_subjid <- input$ass_scatter_toggle_subjid
    #         stat_test <- ptg_type <- group_levels <- add_points <- NULL
    #         cormat_plottype <- NULL
    #         
    #     } else if(ass_box_show$value) {
    #         tnf$ass_count <- tnf$ass_count + 1
    #         row_idx <- tnf$ass_count
    #         type_idx <- which(input$ass_type == ass_type_list)
    #         tnf$ass_count_type[type_idx] <- tnf$ass_count_type[type_idx] + 1
    #         tnf$ass_count_graph <- tnf$ass_count_graph + 1
    #         title_key_ <- paste0('g-asso', tnf$ass_count_graph)
    #         tfln_ <- paste0('2.', type_idx, '.', tnf$ass_count_type[type_idx])
    #         title_ <- 'Figure'
    #         ass_type <- 'Box plot'
    #         
    #         var_x <- input$ass_box_x
    #         visit_x <- paste(input$ass_box_time_x, collapse = ',')
    #         bmk_x <- input$ass_box_bmk_x
    #         log_x <- NULL
    #         
    #         var_y <- input$ass_box_y
    #         visit_y <- paste(input$ass_box_time_y, collapse = ',')
    #         bmk_y <- input$ass_box_bmk_y
    #         log_y <- input$ass_box_to_log_y
    #         
    #         stat_test <- input$ass_box_test
    #         group_levels <- paste(input$ass_box_group_levels, collapse = ',')
    #         add_points <- input$ass_box_toggle_points
    #         add_subjid <- input$ass_box_toggle_subjid
    #         corr_type <- ptg_type <- add_line <- NULL
    #         cormat_plottype <- NULL
    #     } else if(ass_cormat_show$value) {
    #         if(input$ass_cormat_cortype == 'intra-biomarker') {
    #             n_graphs <- length(input$ass_cormat_bmk)
    #             visit_x <- paste(input$ass_cormat_time, collapse = ',')
    #             bmk_x <- input$ass_cormat_bmk
    #         } else if(input$ass_cormat_cortype == 'inter-biomarker') {
    #             n_graphs <- length(input$ass_cormat_time)
    #             visit_x <- input$ass_cormat_time
    #             bmk_x <- paste(input$ass_cormat_bmk, collapse = ',')
    #         }
    #         
    #         title_key_ <- paste0('g-asso', tnf$ass_count_graph + seq_len(n_graphs))
    #         type_idx <- which(input$ass_type == ass_type_list)
    #         tfln_ <- paste0('2.', type_idx, '.',
    #                         tnf$ass_count_type[type_idx] + seq_len(n_graphs))
    #         title_ <- 'Figure'
    #         ass_type <- 'Correlation matrix'
    #         
    #         tnf$ass_count_graph <- tnf$ass_count_graph + n_graphs
    #         row_idx <- tnf$ass_count + seq_len(n_graphs)
    #         tnf$ass_count <- tnf$ass_count + n_graphs
    #         tnf$ass_count_type[type_idx] <- tnf$ass_count_type[type_idx] + n_graphs
    #         
    #         var_x <- input$ass_cormat_y
    #         log_x <- NULL
    #         cormat_plottype <- input$ass_cormat_plottype
    #         
    #         var_y <- visit_y <- bmk_y <- log_y <- NULL
    #         
    #         corr_type <- input$ass_cormat_cortype
    #         stat_test <- ptg_type <- NULL
    #         group_levels <- add_line <- add_points <- add_subjid <- NULL
    #         
    #     } else if(ass_table_show$value) {
    #         tnf$ass_count <- tnf$ass_count + 1
    #         row_idx <- tnf$ass_count
    #         type_idx <- which(input$ass_type == ass_type_list)
    #         tnf$ass_count_type[type_idx] <- tnf$ass_count_type[type_idx] + 1
    #         tnf$ass_count_table <- tnf$ass_count_table + 1
    #         title_key_ <- paste0('t-asso', tnf$ass_count_table)
    #         tfln_ <- paste0('2.', type_idx, '.', tnf$ass_count_type[type_idx])
    #         title_ <- 'Table'
    #         ass_type <- 'Contingency table'
    #         
    #         var_x <- input$ass_table_x
    #         visit_x <- paste(input$ass_table_time_x, collapse = ',')
    #         bmk_x <- input$ass_table_bmk_x
    #         log_x <- NULL
    #         
    #         var_y <- input$ass_table_y
    #         visit_y <- paste(input$ass_table_time_y, collapse = ',')
    #         bmk_y <- input$ass_table_bmk_y
    #         log_y <- NULL
    #         
    #         stat_test <- input$ass_table_test
    #         ptg_type <- input$ass_table_percentage
    #         corr_type <-group_levels <-add_line <-add_points <-add_subjid <- NULL
    #         cormat_plottype <- NULL
    #     }
    #     
    #     height <- ifelse(
    #         is.null(input$ass_download_plot_height), ass_plot_height_default,
    #         input$ass_download_plot_height
    #     )
    #     width <- ifelse(
    #         is.null(input$ass_download_plot_width), ass_plot_width_default,
    #         input$ass_download_plot_width
    #     )
    #     subset_ <- gsub('\t', '&', stringr::str_trim(subset_data()$subset_expr))
    #     
    #     row_list <- list(
    #         title_key_, title_, tfln_, ass_type, cormat_plottype,
    #         input$ass_study, input$ass_cohort,
    #         var_x, visit_x, bmk_x, log_x, var_y, visit_y, bmk_y, log_y,
    #         height, width, group_levels, corr_type, stat_test, ptg_type,
    #         add_line, add_points, add_subjid, subset_
    #     )
    #     row_df <- data.frame(
    #         lapply(row_list, function(x) replace(x, is.null(x), NA))
    #     )
    #     tnf$ass_df <- update_df(tnf$ass_df, setNames(row_df, out_ass_cols))
    #     tnf_update(
    #         tnf$name, tnf$dir, row_df, sheet_names$ass, row_idx + 1
    #     )
    # })
    
    
    #-----------------------------------------------
    # Survival analysis page
    #-----------------------------------------------
    
    # whether data has required columns to perform survival analysis
    surv_has_pfs <- reactiveValues(value = FALSE)
    surv_has_os <- reactiveValues(value = FALSE)
    observe({
        if(isTRUE(file_import$status)) {
            data <- subset_data()$data
            if(nrow(data) > 0) {
                all_columns <- names(data)
                if(all(c(pfs_time_col, pfs_event_col) %in% all_columns)) {
                    surv_has_pfs$value <- TRUE
                } else surv_has_pfs$value <- FALSE
                if(all(c(os_time_col, os_event_col) %in% all_columns)) {
                    surv_has_os$value <- TRUE
                } else surv_has_os$value <- FALSE
            } else {
                surv_has_pfs$value <- FALSE
                surv_has_os$value <- FALSE
            }
        } else {
            surv_has_pfs$value <- FALSE
            surv_has_os$value <- FALSE
        }
    })
    
    #-------------------------------
    #   Survival analysis UI widgets
    #-------------------------------
    
    # selectInput to choose study
    output$surv_study <- renderUI({
        req(any(surv_has_pfs$value, surv_has_os$value))
        data <- subset_data()$data
        choices <- c('Choose' = '', unique(data[[study_col]]))
        selectInput('surv_study', 'Study', choices)
    })
    
    # selectInput to choose cohort
    output$surv_cohort <- renderUI({
        req(any(surv_has_pfs$value, surv_has_os$value))
        data <- subset_data()$data
        req(cohort_col %in% names(data))
        choices <- c('Choose' = '', unique(data[[cohort_col]]))
        selectInput('surv_cohort', 'Cohort', choices)
    })
    
    # selectInput to choose survival time
    output$surv_time <- renderUI({
        req(any(surv_has_pfs$value, surv_has_os$value))
        choices <- c('Choose' = '')
        if(surv_has_pfs$value) choices <- c(choices, pfs_time_col)
        if(surv_has_os$value) choices <- c(choices, os_time_col)
        selectInput('surv_time', 'Survival time', choices)
    })
    
    # selectInput to choose biomarker name
    output$surv_bmk_name <- renderUI({
        req(any(surv_has_pfs$value, surv_has_os$value))
        data <- subset_data()$data
        choices <- c('Choose' = '', sort(unique(data[[param_col]])))
        selectInput('surv_bmk_name', 'Biomarker name', choices)
    })
    
    # selectInput to choose biomarker value
    output$surv_bmk_value <- renderUI({
        req(any(surv_has_pfs$value, surv_has_os$value))
        choices <- c('Choose' = '', surv_bmk_value_list)
        selectInput('surv_bmk_value', 'Biomarker value', choices)
    })
    
    # selectInput to choose biomarker visit
    output$surv_bmk_visit <- renderUI({
        req(any(surv_has_pfs$value, surv_has_os$value))
        data <- subset_data()$data
        
        if(!is_blank(input$surv_study))
            data <- data[data[[study_col]] %in% input$surv_study, , drop = F]
        if(!is_blank(input$surv_cohort))
            data <- data[data[[cohort_col]] %in% input$surv_cohort, , drop = F]
        if(!is_blank(input$bmk_name))
            data <- data[data[[param_col]] %in% input$bmk_name, , drop = F]
        if(!is_blank(input$surv_bmk_value))
            data <- data[!is.na(data[[input$surv_bmk_value]]), , drop = F]
        
        choices <- c('Choose' = '',
                     unique(data[[xlabel_col]][order(data[[xvar_col]])]))
        selectInput('surv_bmk_visit', 'Biomarker visit', choices)
    })
    
    # sliderbar to choose a cutoff point to dichotomize the biomarker
    output$surv_bmk_cutoff <- renderUI({
        req(any(surv_has_pfs$value, surv_has_os$value))
        sliderInput('surv_bmk_cutoff', 'Biomarker cutoff (quantile)',
                    min = 0, max = 100, value = 50, post = '%')
    })
    
    # a download action button for downloading KM-curve
    output$surv_download_plot_action <- renderUI({
        req(surv_show$plot)
        actionButton('surv_download_plot_action', 'Download plot')
    })
    
    # download page height numeric input widget
    output$surv_download_plot_height <- renderUI({
        numericInput(
            'surv_download_plot_height', 'Height (inches)',
            value = surv_plot_height_default
        )
    })
    # download page width numeric input widget
    output$surv_download_plot_width <- renderUI({
        numericInput(
            'surv_download_plot_width', 'Width (inches)',
            value = surv_plot_width_default
        )
    })
    # download button widget on download page
    output$surv_download_plot_button <- renderUI({
        req(!is_blank(input$surv_download_plot_format),
            !is_blank(input$surv_download_plot_action),
            !is_blank(input$surv_download_plot_width))
        downloadButton('surv_download_plot', strong('Download plot'))
    })
    
    # a download action button for downloading table
    output$surv_download_table_action <- renderUI({
        req(surv_show$table)
        actionButton('surv_download_table_action', 'Download table')
    })
    # download button widget on download page
    output$surv_download_table_button <- renderUI({
        req(!is_blank(input$surv_download_table_format))
        downloadButton('surv_download_table', strong('Download table'))
    })
    
    # survival output status
    surv_show <- reactiveValues(plot = FALSE, table = FALSE)
    observe({
        if(any(surv_has_pfs$value, surv_has_os$value)) {
            if(all(!is_blank(input$surv_time),
                   !is_blank(input$surv_bmk_name),
                   !is_blank(input$surv_bmk_value),
                   !is_blank(input$surv_bmk_visit),
                   !is_blank(input$surv_bmk_cutoff))) {
                surv_show$plot <- TRUE
                surv_show$table <- TRUE
            } else {
                surv_show$plot <- FALSE
                surv_show$table <- FALSE
            }
        } else {
            surv_show$plot <- FALSE
            surv_show$table <- FALSE
        }
    })
    
    
    #-------------------------------
    #   Survival analysis output
    #-------------------------------
    
    surv_data <- reactive({
        req(surv_show$plot)
        data <- subset_data()$data
        if(!is_blank(input$surv_study))
            data <- data[data[[study_col]] %in% input$surv_study, , drop = F]
        if(!is_blank(input$surv_cohort))
            data <- data[data[[cohort_col]] %in% input$surv_cohort, , drop = F]
        data <- data[data[[param_col]] %in% input$surv_bmk_name, , drop = F]
        data <- data[data[[xlabel_col]] %in% input$surv_bmk_visit, , drop = F]
        
        data[[input$surv_time]] <- as.numeric(data[[input$surv_time]])
        data[[input$surv_bmk_value]] <- as.numeric(data[[input$surv_bmk_value]])
        
        bmk_value <- data[[input$surv_bmk_value]]
        cutoff_prob <- as.numeric(sub('%', '', input$surv_bmk_cutoff)) / 100
        bmk_cutoff <- quantile(bmk_value, probs = cutoff_prob, na.rm = T)
        data$bmkgroup <- ifelse(
            bmk_value < bmk_cutoff,
            paste0(input$surv_bmk_name, ' < ', bmk_cutoff, ' (',
                   input$surv_bmk_cutoff, '%-ile)'),
            paste0(input$surv_bmk_name, ' >= ', bmk_cutoff, ' (',
                   input$surv_bmk_cutoff, '%-ile)')
        )
        data[['bmkgroup']] <- factor(
            data[['bmkgroup']], levels = sort(unique(data[['bmkgroup']]))
        )
        return(data)
    })
    
    # survival analysis - message
    output$surv_message <- renderUI({
        req(!any(surv_has_pfs$value, surv_has_os$value))
        tags$div(
            tags$p(paste(
                'At least one of the following two pairs must be',
                'present in the data in order to perform survival analysis:'
            )),
            tags$ul(
                tags$li(
                    tags$strong(pfs_time_col),
                    ' (Progression free survival time)',
                    ' and ', tags$strong(pfs_event_col),
                    ' (Progression free survival event)'
                ),
                tags$li(
                    tags$strong(os_time_col), '( Overall survival time)', ' and ',
                    tags$strong(os_event_col), ' (Overall free survival event)'
                )
            )
        )
    })
    
    # survival analysis - KM curve
    output$surv_kmcurve <- renderPlot({
        surv_data <- surv_data()
        req(surv_show$plot, surv_data)
        req(!is.null(surv_plot_xlab$value),
            !is.null(surv_plot_ylab$value),
            !is.null(surv_plot_main$value))
        if(input$surv_time == pfs_time_col) {
            event_var <- pfs_event_col
        }
        if(input$surv_time == os_time_col) {
            event_var <- os_event_col
        }
        km_plot(survdata = surv_data, tte = input$surv_time, cens = event_var,
                strata = 'bmkgroup', plot.nrisk = TRUE, nrisk.interval = 1,
                cex.nrisk = 1, col = 1:2, plot.medsurv = FALSE, lty = 1,
                lwd = c(2, 1, 1), plot.grid = TRUE, grids=c(.5),
                plot.legend = TRUE, plot.CI = TRUE, ylim = c(0, 1.15),
                xlab = surv_plot_xlab$value, ylab = surv_plot_ylab$value,
                main = surv_plot_main$value, sub = '',
                plot.pdf = FALSE)
    })
    
    # survival analysis - table
    output$surv_table <- renderUI({
        surv_data <- surv_data()
        req(surv_show$plot, surv_data)
        req(!is.null(surv_table_title$value),
            !is.null(surv_table_footnote$value),
            !is.null(surv_table_decimal$value))
        if(input$surv_time == pfs_time_col) {
            event_var <- pfs_event_col
        }
        if(input$surv_time == os_time_col) {
            event_var <- os_event_col
        }
        HTML(survival_table(
            surv_data, event_var, input$surv_time, 'bmkgroup',
            group_levels = NULL, digits = surv_table_decimal$value,
            time_unit = '', caption = surv_table_title$value,
            footnote = surv_table_footnote$value,
            output = 'html'
        ))
    })
    
    
    #-------------------------------
    #   Survival analysis refine output
    #-------------------------------
    
    # KM-curve xlab
    output$surv_plot_xlab <- renderUI({
        surv_data <- surv_data()
        req(surv_show$plot, surv_data)
        if(input$surv_time == pfs_time_col) {
            value <- 'PFS time'
        }
        if(input$surv_time == os_time_col) {
            value <- 'OS time'
        }
        textareaInput('surv_plot_xlab', 'X-axis label', value)
    })
    surv_plot_xlab <- reactiveValues(value = NULL)
    observe({
        surv_data <- surv_data()
        req(surv_show$plot, surv_data)
        if(input$surv_time == pfs_time_col) {
            value <- 'PFS time'
        }
        if(input$surv_time == os_time_col) {
            value <- 'OS time'
        }
        surv_plot_xlab$value <- value
    })
    observe({
        input$surv_plot_xlab
        surv_plot_xlab$value <- input$surv_plot_xlab
    })
    
    # KM curve ylab
    output$surv_plot_ylab <- renderUI({
        surv_data <- surv_data()
        req(surv_show$plot, surv_data)
        textareaInput('surv_plot_ylab', 'Y-axis label', 'Survival Probability')
    })
    surv_plot_ylab <- reactiveValues(value = 'Survival Probability')
    observe({
        input$surv_plot_ylab
        surv_plot_ylab$value <- input$surv_plot_ylab
    })
    
    # KM curve title
    output$surv_plot_main <- renderUI({
        surv_data <- surv_data()
        req(surv_show$plot, surv_data)
        main <- ''
        if(!is_blank(input$surv_study))
            main <- paste(main, paste('Study:', input$surv_study), sep ='\n')
        if(!is_blank(input$surv_cohort))
            main <- paste(main, paste(input$surv_cohort, 'Cohort'), sep = '\n')
        main <- trimws(paste(main, paste('KM curve by', input$surv_bmk_name, 'at',
                                         input$surv_bmk_visit), sep = '\n'))
        textareaInput('surv_plot_main', 'Plot title', main)
    })
    surv_plot_main <- reactiveValues(value = NULL)
    observe({
        surv_data <- surv_data()
        req(surv_show$plot, surv_data)
        main <- ''
        if(!is_blank(input$surv_study))
            main <- paste(main, paste('Study:', input$surv_study), sep ='\n')
        if(!is_blank(input$surv_cohort))
            main <- paste(main, paste(input$surv_cohort, 'Cohort'), sep = '\n')
        main <- trimws(paste(main, paste('KM curve by', input$surv_bmk_name, 'at',
                                         input$surv_bmk_visit), sep = '\n'))
        surv_plot_main$value <- main
    })
    observe({
        input$surv_plot_main
        surv_plot_main$value <- input$surv_plot_main
    })
    
    # table title
    output$surv_table_title <- renderUI({
        req(surv_show$table)
        textareaInput('surv_table_title', 'Table title', '')
    })
    surv_table_title <- reactiveValues(value = '')
    observe({
        input$surv_table_title
        surv_table_title$value <- input$surv_table_title
    })
    
    # table footnote
    output$surv_table_footnote <- renderUI({
        req(surv_show$table)
        textareaInput('surv_table_footnote', 'Table footnote', '')
    })
    surv_table_footnote <- reactiveValues(value = '')
    observe({
        input$surv_table_footnote
        surv_table_footnote$value <- input$surv_table_footnote
    })
    
    # table decimal places
    output$surv_table_decimal <- renderUI({
        req(surv_show$table)
        numericInput('surv_table_decimal', 'Decimal places', value = 2)
    })
    surv_table_decimal <- reactiveValues(value = 2)
    observe({
        input$surv_table_decimal
        surv_table_decimal$value <- input$surv_table_decimal
    })
    
    
    
    #-------------------------------
    #   Survival analysis download
    #-------------------------------
    
    # download KM-curve
    output$surv_download_plot <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'km_curve_', format(Sys.Date(),format = '%Y%m%d'),
                '.', input$surv_download_plot_format
            )
            return(file_name)
        },
        content = function(file) {
            height <- input$surv_download_plot_height
            width <- input$surv_download_plot_width
            if(input$surv_download_plot_format == 'pdf')
                pdf(file, height = height, width = width)
            else if(input$surv_download_plot_format == 'png')
                png(file, height = height, width = width, units = 'in', res = 600)
            else if(input$surv_download_plot_format == 'jpg')
                jpeg(file, height = height, width = width, units = 'in', res = 600)
            else if(input$surv_download_plot_format == 'ps')
                postscript(file, height = height, width = width)
            surv_data <- surv_data()
            if(input$surv_time == pfs_time_col) {
                event_var <- pfs_event_col
            }
            if(input$surv_time == os_time_col) {
                event_var <- os_event_col
            }
            km_plot(survdata = surv_data, tte = input$surv_time, cens = event_var,
                    strata = 'bmkgroup', plot.nrisk = TRUE, nrisk.interval = 1,
                    cex.nrisk = 1, col = 1:2, plot.medsurv = FALSE, lty = 1,
                    lwd = c(2, 1, 1), plot.grid = TRUE, grids=c(.5),
                    plot.legend = TRUE, plot.CI = TRUE, ylim = c(0, 1.15),
                    xlab = surv_plot_xlab$value, ylab = surv_plot_ylab$value,
                    main = surv_plot_main$value, sub = '',
                    plot.pdf = FALSE)
            dev.off()
        }
    )
    
    # download table
    output$surv_download_table <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'survival_table_', format(Sys.Date(),format = '%Y%m%d'),
                '.', input$surv_download_table_format
            )
            return(file_name)
        },
        content = function(file) {
            surv_data <- surv_data()
            req(surv_show$plot, surv_data)
            if(input$surv_time == pfs_time_col) {
                event_var <- pfs_event_col
            }
            if(input$surv_time == os_time_col) {
                event_var <- os_event_col
            }
            surv_table <- survival_table(
                surv_data, event_var, input$surv_time, 'bmkgroup',
                group_levels = NULL, digits = surv_table_decimal$value,
                time_unit = '', caption = surv_table_title$value,
                footnote = surv_table_footnote$value,
                output = 'rtf'
            )
            rtf_table_wrapper(file, surv_table, caption = surv_table_title$value,
                              footnote = surv_table_footnote$value)
        }
    )
    
    
    #-----------------------------------------------
    # Output results page
    #-----------------------------------------------
    
    # file input for TNF file upload
    output$out_file <- renderUI({
        req(file_import$status)
        fileInput('out_file', tags$p('Choose file to import',
                                     tags$a('(Need input data template?)',
                                            target = '_blank',
                                            href = out_template_file)),
                  accept = out_accepted_file_format)
    })
    
    # group checkbox for output graph formats
    output$out_graph_format <- renderUI({
        req(file_import$status)
        checkboxGroupInput(
            'out_graph_format', label = 'Output graph format',
            choices = c('pdf', 'png'), selected = c('pdf', 'png'), inline = T
        )
    })
    # group checkbox for output table formats
    output$out_table_format <- renderUI({
        req(file_import$status)
        checkboxGroupInput(
            'out_table_format', label = 'Output table format',
            choices = c('csv', 'rtf'), selected = c('csv', 'rtf'), inline = T
        )
    })
    
    # read in the output data uploaded by the user
    observe({
        req(file_import$status)
        if(!is.null(input$out_file)) {
            file_input <- fix_uploaded_files_names(input$out_file)
            file_name <- tolower(file_input$name)
            if(endswith(file_name, c('.xls', '.xlsx'))) {
                tnf_time <- readxl::read_excel(file_input$datapath,
                                               sheet = 'Time Profiling')
                tnf_ass <- readxl::read_excel(file_input$datapath,
                                              sheet = 'Association')
                tnf$time_df <- tnf_time
                tnf$ass_df <- tnf_ass
                tnf$time_count <- nrow(tnf_time)
                tnf$ass_count <- nrow(tnf_ass)
            }
        }
    })
    
    # # fixe the uploaded file name issue
    # output_file <- reactive({
    #     req(file_import$status)
    #     req(input$out_file)
    #     return(fix_uploaded_files_names(input$out_file))
    # })
    # 
    # # read in the output data uploaded by the user
    # output_data <- reactive({
    #     req(input$out_graph_format)
    #     out_file <- output_file()
    #     file_name <- out_file$name
    #     if(endswith(tolower(file_name), c('xls', 'xlsx'))) {
    #         wb <- XLConnect::loadWorkbook(out_file$datapath)
    #         XLConnect::setMissingValue(wb, value = c('NA', ''))
    #         out_time_data <- XLConnect::readWorksheet(wb, sheet_names$time)
    #         out_ass_data <- XLConnect::readWorksheet(wb, sheet_names$ass)
    #         out_fn_data <- XLConnect::readWorksheet(wb, sheet_names$fn)
    #         out_fn_data[[out_fn_num_col]] <- as.character(
    #             out_fn_data[[out_fn_num_col]]
    #         )
    #         return(list(
    #             time = out_time_data, ass = out_ass_data, fn = out_fn_data
    #         ))
    #     }
    # })
    
    # download handler for TNF file
    output$out_download_tnf <- downloadHandler(
        filename = function() {
            paste0('output_specs_', format(Sys.Date(),format = '%Y%m%d'),
                   '.xlsx')
        },
        content = function(file) {
            tmpdir <- tempdir()
            owd <- setwd(tmpdir)
            on.exit(setwd(owd))
            tnf_file <- 'temp.xlsx'
            if(file.exists(tnf_file)) file.remove(tnf_file)
            wb <- XLConnect::loadWorkbook(tnf_file, create = TRUE)
            name_list <- list(
                'Time Profiling' = tnf$time_df,
                'Association' = tnf$ass_df
            )
            for(sheet in names(name_list)) {
                XLConnect::createSheet(wb, name = sheet)
                if(!is.null(name_list[[sheet]]) && nrow(name_list[[sheet]]) > 0) {
                    XLConnect::writeWorksheet(wb, name_list[[sheet]], sheet)
                }
            }
            XLConnect::saveWorkbook(wb)
            file.copy(tnf_file, file)
        }
    )
    
    # action button for processing TNF file
    output$out_process_tnf_button <- renderUI({
        req(file_import$status)
        shinyBS::bsButton(
            'out_process_tnf_button', 'Process output specs', disabled = TRUE
        )
    })
    observe({
        if((!is.null(tnf$time_df) && nrow(tnf$time_df) > 0) ||
           (!is.null(tnf$ass_df) && nrow(tnf$ass_df) > 0)) {
            shinyBS::updateButton(
                session, 'out_process_tnf_button', disabled = FALSE
            )
        } else {
            shinyBS::updateButton(
                session, 'out_process_tnf_button', disabled = TRUE
            )
        }
    })
    
    observeEvent(input$out_process_tnf_button, {
        tmpdir <- tempdir()
        out_download_tfl$dir <- tmpdir
        owd <- setwd(tmpdir)
        on.exit(setwd(owd))
        in_data <- subset_data()$data
        tnf_time <- tnf$time_df
        tnf_ass <- tnf$ass_df
        
        #------------------------------------------------
        # process TNF rows
        #
        #   + 'all' entries in 'Biomarker' column
        #   + 'index', 'Index', and 'INDEX' column
        #   + evaluate expression inside `{}`
        #------------------------------------------------
        n_bmks <- length(unique(in_data[[param_col]]))
        all_bmks <- gtools::mixedsort(unique(in_data[[param_col]]))
        n_tfl <- 0
        if(!is.null(tnf_time) && nrow(tnf_time) > 0) {
            index_all <- which(tolower(tnf_time[['Biomarker']]) == 'all')
            n_all <- length(index_all)
            repeats <- rep.int(n_bmks, n_all)
            index <- rep.int(1, nrow(tnf_time))
            if(n_all > 0) {
                tnf_time <- repeat_rows_df(tnf_time, index_all, repeats)
                all_from <- index_all + seq.int(0, by = n_bmks - 1,
                                                length.out = n_all)
                index_all_new <- unlist(lapply(
                    all_from, seq.int, length.out = n_bmks
                ))
                tnf_time[index_all_new, 'Biomarker'] <- rep(all_bmks, n_all)
                index[index_all] <- n_bmks
                index <- unlist(lapply(index, seq.int, from = 1))
            }
            
            tnf_time$index <- index
            tnf_time$Index <- seq_len(nrow(tnf_time))
            tnf_time$INDEX <- seq_len(nrow(tnf_time))
            n_tfl <- n_tfl + nrow(tnf_time)
            
            for(idx in seq_len(nrow(tnf_time))) {
                tnf_time[idx, ] <- eval_df_row(tnf_time[idx, ])
            }
        }
        if(!is.null(tnf_ass) && nrow(tnf_ass) > 0) {
            index <- rep.int(1, nrow(tnf_ass))
            tnf_time$index <- index
            tnf_time$Index <- seq_len(nrow(tnf_ass))
            tnf_time$INDEX <- seq.int(n_tfl + 1, to = n_tfl + nrow(tnf_ass))
            n_tfl <- n_tfl + nrow(tnf_ass)
        }
        
        
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = 'Processing output specs', value = 0)
        #------------------------------------------------
        # Generate graph/table for each row
        #------------------------------------------------
        sep = '\\\\n|\\n|\\r\\n'
        if(!is.null(tnf_time) && nrow(tnf_time) > 0) {
            for(idx in seq_len(nrow(tnf_time))) {
                irow <- tnf_time[idx, ]
                
                study <- as.character(irow[['Study']])
                cohort <- as.character(irow[['Cohort']])
                biomarker <- as.character(irow[['Biomarker']])
                group_levels <- unlist(strsplit(
                    as.character(irow[['GroupLevels']]), sep
                ))
                facet_r_levels <- unlist(strsplit(
                    as.character(irow[['FacetRowLevels']]), sep
                ))
                facet_c_levels <- unlist(strsplit(
                    as.character(irow[['FacetColumnLevels']]), sep
                ))
                col_total_n <- as.numeric(unlist(strsplit(
                    as.character(irow[['TableColumnTotals']]), sep
                )))
                col_total_name <- unlist(strsplit(
                    as.character(irow[['TableColumnTotalNames']]), sep
                ))
                reflines <- as.numeric(unlist(strsplit(
                    as.character(irow[['ReferenceLine']]), sep
                )))
                all_colors <- unlist(strsplit(as.character(irow[['Colors']]), sep))
                all_linetypes <- unlist(strsplit(as.character(irow[['LineTypes']]), sep))
                
                y_log <- irow[['LogY']] %in% string_yes
                chg_from_bs <- irow[['TableChangeFromBaseline']] %in% string_yes
                include_ss <- irow[['IncludeSampleSize']] %in% string_yes
                include_points <- irow[['IncludePoints']] %in% string_yes
                include_subjid <- irow[['IncludeSubjectID']] %in% string_yes
                
                x_min <- as.numeric(irow[['Xmin']])
                x_max <- as.numeric(irow[['Xmax']])
                y_min <- as.numeric(irow[['Ymin']])
                y_max <- as.numeric(irow[['Ymax']])
                if(!is_blank(x_min) || !is_blank(x_max))
                    x_limit <- c(x_min, x_max)
                else x_limit <- NULL
                if(!is_blank(y_min) || !is_blank(y_max))
                    y_limit <- c(y_min, y_max)
                else y_limit <- NULL
                plot_height <- as.numeric(irow[['PlotHeight']])
                plot_width <- as.numeric(irow[['PlotWidth']])
                plot_res <- as.numeric(irow[['PlotResolution']])
                table_decimal <- as.numeric(irow[['TableDecimal']])
                x_tick_angle <- as.numeric(irow[['XTickAngle']])
                if(is_blank(plot_height)) plot_height <- time_plot_height_default
                if(is_blank(plot_width)) plot_width <- time_plot_width_default
                if(is_blank(plot_res)) plot_res <- 600
                if(is_blank(table_decimal)) table_decimal <- 1
                
                subset_expr <- as.character(irow[['SubsetExpression']])
                
                # data filtering
                data_i <- in_data
                if(!is_blank(subset_expr)) {
                    data_i <- with(
                        data_i, data_i[eval(parse(text = subset_expr)), ]
                    )
                }
                if(!is_blank(study))
                    data_i <- data_i[data_i[[study_col]] %in% study, , drop = F]
                if(!is_blank(cohort))
                    data_i <- data_i[data_i[[cohort_col]] %in% cohort, , drop = F]
                data_i <- data_i[data_i[[param_col]] %in% biomarker, , drop = F]
                
                avg_method <- NULL
                var_method <- NULL
                output_type <- irow[['OutputType']]
                if(output_type == 'Boxplot') geoms <- 'boxplot'
                else if(output_type == 'Spaghetti plot') geoms <- 'line'
                else {
                    geoms <- 'sumline'
                    if(output_type == 'Median + IQR plot') avg_method <- 'median'
                    else {
                        avg_method <- 'mean'
                        if(output_type == 'Mean + SE plot') var_method <- 'se'
                        else var_method <- 'sd'
                    }
                }
                if(isTRUE(include_points)) geoms <- c(geoms, 'point')
                
                if(tolower(irow[['TFLT']]) == 'figure') {
                    plot_ <- suppressWarnings(time_profiling_ggplot(
                        data_i, x = xvar_col, y = irow[['YVariable']],
                        subject = subj_col, group = irow[['Group']],
                        group_levels = group_levels,
                        facet_r = irow[['FacetRow']],
                        facet_c = irow[['FacetColumn']],
                        facet_r_levels = facet_r_levels,
                        facet_c_levels = facet_c_levels, geoms = geoms,
                        avg_method = avg_method, var_method = var_method,
                        y_log = y_log, sample_size = include_ss,
                        sample_size_font_size = 3,
                        xlab = irow[['PlotXLabel']], ylab = irow[['PlotYLabel']],
                        title = irow[['PlotTitle']], x_limit = x_limit,
                        y_limit = y_limit,
                        x_tick_label = irow[['XTickVar']], all_xticks = TRUE,
                        x_tick_angle = x_tick_angle, reference_line = reflines,
                        subject_show = include_subjid,
                        all_colors = all_colors, all_linetypes = all_linetypes
                    ))
                    plot_format <- ifelse(is_blank(irow[['PlotFormat']]),
                                          'pdf', tolower(irow[['PlotFormat']]))
                    filename <- paste(irow[['FileKey']], plot_format, sep = '.')
                    suppressWarnings(ggsave(
                        filename, plot_, width = plot_width,
                        height = plot_height, dpi = plot_res
                    ))
                    out_download_tfl$time <- c(out_download_tfl$time, filename)
                }
                progress$inc(1 / n_tfl, detail = filename)
            }
        }
        if(!is.null(tnf_ass) && nrow(tnf_ass) > 0) {
            for(idx in seq_len(nrow(tnf_ass))) {
                irow <- tnf_time[idx, ]
                
                study <- as.character(irow[['Study']])
                cohort <- as.character(irow[['Cohort']])
                output_type <- tolower(as.character(irow[['OutputType']]))
                biomarker_x <- as.character(irow[['BiomarkerX']])
                biomarker_y <- as.character(irow[['BiomarkerY']])
                variable_x <- as.character(irow[['VariableX']])
                variable_y <- as.character(irow[['VariableY']])
                levels_x <- unlist(strsplit(
                    as.character(irow[['LevelsX']]), sep
                ))
                levels_y <- unlist(strsplit(
                    as.character(irow[['LevelsY']]), sep
                ))
                visits_x <- unlist(strsplit(
                    as.character(irow[['VisitsX']]), sep
                ))
                visits_y <- unlist(strsplit(
                    as.character(irow[['VisitsY']]), sep
                ))
                group <- as.character(irow[['Group']])
                group_levels <- unlist(strsplit(
                    as.character(irow[['GroupLevels']]), sep
                ))
                facet_r <- as.character(irow[['FacetRow']])
                facet_r_levels <- unlist(strsplit(
                    as.character(irow[['FacetRowLevels']]), sep
                ))
                facet_c <- as.character(irow[['FacetColumn']])
                facet_c_levels <- unlist(strsplit(
                    as.character(irow[['FacetColumnLevels']]), sep
                ))
                
                subset_expr <- as.character(irow[['SubsetExpression']])
                group_var <- c()
                
                # data filtering
                data_i <- in_data
                if(!is_blank(subset_expr)) {
                    data_i <- with(
                        data_i, data_i[eval(parse(text = subset_expr)), ]
                    )
                }
                if(!is_blank(study)) {
                    data_i <- data_i[data_i[[study_col]] == study, , drop = F]
                }
                if(!is_blank(cohort)) {
                    data_i <- data_i[data_i[[cohort_col]] == cohort, , drop = F]
                }
                if(length(group_levels) <= 1) all_colors = gg_color_hue(1)
                else all_colors = gg_color_hue(length(group_levels))
                if(!is_blank(group)) group_var <- c(group, group_var)
                if(!is_blank(facet_r)) {
                    data_i[[facet_r]] <- paste0(facet_r, ': ', data_i[[facet_r]])
                    
                }
                if(!is_blank(facet_c)) {
                    data_i[[facet_c]] <- paste0(facet_c, ': ', data_i[[facet_c]])
                    group_var <- c(facet_c, group_var)
                }
                
                if(output_type == 'histogram') {
                    
                } else if(output_type == 'scatter plot') {
                    group_var <- c(group_var, subj_col)
                    data_x <- data_i
                    if(!is_blank(biomarker_x)) {
                        cond_param <- data_x[[param_col]] %in% biomarker_x
                        data_x <- data_x[cond_param, , drop = F]
                    }
                    if(!is_blank(visits_x)) {
                        cond_visits <- data_x[[xlabel_col]] %in% visits_x
                        data_x <- data_x[cond_visits, , drop = F]
                    }
                    data_y <- data_i
                    if(!is_blank(biomarker_y)) {
                        cond_param <- data_y[[param_col]] %in% biomarker_y
                        data_y <- data_y[cond_param, , drop = F]
                    }
                    if(!is_blank(visits_y)) {
                        cond_visits <- data_y[[xlabel_col]] %in% visits_y
                        data_y <- data_y[cond_visits, , drop = F]
                    }
                    data_x <- data_x[, c(group_var, variable_x), drop = F]
                    data_y <- data_y[, c(group_var, variable_y), drop = F]
                    data_scatter <- dplyr::full_join(
                        data_x, data_y, by = group_var
                    )
                    if(variable_x == variable_y) {
                        x_var <- paste0(variable_x, '.x')
                        y_var <- paste0(variable_y, '.y')
                    } else {
                        x_var <- variable_x
                        y_var <- variable_y
                    }
                    data_scatter <- rename_(
                        data_scatter,
                        .dots = setNames(c(x_var, y_var), c('x_var', 'y_var'))
                    )
                    
                    add_label <- as.character(
                        irow[['IncludeSubjectID']]
                    ) %in% string_yes
                    log_x <- as.character(irow[['LogX']]) %in% string_yes
                    log_y <- as.character(irow[['LogY']]) %in% string_yes
                    x_lab <- as.character(irow[['PlotXLabel']])
                    y_lab <- as.character(irow[['PlotYLabel']])
                    plot_title <- as.character(irow[['PlotFootnote']])
                    plot_ <- gg_scatter(
                        data_scatter, x = 'x_var', y = 'y_var', label = subj_col,
                        facet_r = facet_r, facet_c = facet_c,
                        facet_r_levels = facet_r_levels,
                        facet_c_levels = facet_c_levels,
                        color_var = group, all_colors = all_colors,
                        shape_var = NULL, add_label = add_label,
                        repel_label = TRUE, x_lab = x_lab, y_lab = y_lab,
                        title = plot_title, x_log = log_x, y_log = log_y,
                        reference_hline = NULL, reference_vline = NULL,
                        bw_theme = TRUE, grids = 'on'
                    )
                    add_line <- tolower(as.character(irow[['AddLines']]))
                    add_ci <- as.character(
                        irow[['IncludeConfidenceInterval']]
                    ) %in% string_yes
                    if(!is_blank(add_line)) {
                        if(add_line != 'identify line') {
                            if(add_line == 'Loess line') method <- 'loess'
                            else if(add_line == 'Linear regression line')
                                method <- 'lm'
                            se <- isTRUE(add_ci)
                            plot_ <- plot_ + geom_smooth(method = method, se = se)
                        } else {
                            plot_ <- plot_ + geom_abline(intercept = 0, slope = 1)
                        }
                    }
                    # if(!is_blank(input$ass_scatter_test)) {
                    #     test_res <- ass_scatter_test_result()
                    #     plot_ <- plot_ + geom_text(data = test_res,
                    #                                aes(x, y, label = p_value_text),
                    #                                show.legend = FALSE,
                    #                                hjust = 1.2, vjust = 1.5)
                    # }
                    plot_format <- ifelse(is_blank(irow[['PlotFormat']]),
                                          'pdf', tolower(irow[['PlotFormat']]))
                    filename <- paste(irow[['FileKey']], plot_format, sep = '.')
                    suppressWarnings(ggsave(
                        filename, plot_, width = plot_width,
                        height = plot_height, dpi = plot_res
                    ))
                    out_download_tfl$time <- c(out_download_tfl$time, filename)
                    progress$inc(1 / n_tfl, detail = filename)
                } else if(output_type == 'box plot') {
                    
                } else if(output_type == 'correlation matrix') {
                    
                } else if(output_type == 'contingency table') {
                    
                }
            }
        }
        out_download_ready$value <- TRUE
    })
    
    
    
    # observeEvent(input$out_process_tnf_button, {
    #     
    #     tmpdir <- tempdir()
    #     out_download_tfl$dir <- tmpdir
    #     owd <- setwd(tmpdir)
    #     on.exit(setwd(owd))
    #     in_data <- subset_data()$data
    #     out_data <- output_data()
    #     out_time <- out_data$time
    #     out_ass <- out_data$ass
    #     out_fn <- out_data$fn
    #     nrow_time <- nrow(out_time)
    #     nrow_ass <- nrow(out_ass)
    #     nrow_total <- nrow_time + nrow_ass
    #     tfls_time <- c()
    #     tfls_ass <- c()
    #     include_pdf <- 'pdf' %in% input$out_graph_format
    #     include_rtf <- 'rtf' %in% input$out_graph_format
    #     names_time <- names(out_time)
    #     names_ass <- names(out_ass)
    #     fn_map <- setNames(out_fn[[out_fn_text_col]], out_fn[[out_fn_num_col]])
    #     
    #     progress <- shiny::Progress$new()
    #     on.exit(progress$close())
    #     progress$set(message = 'Processing output specs', value = 0)
    #     for (idx in seq_len(nrow(out_time))) {
    #         irow <- out_time[idx, , drop = FALSE]
    #         data_idx <- in_data
    #         path <- irow[[out_time_title_col]]
    #         tfl_type <- irow[[out_time_tflt_col]]
    #         study <- irow[[out_time_study_col]]
    #         cohort <- irow[[out_time_cohort_col]]
    #         bmk <- irow[[out_time_bmk_col]]
    #         y_variable <- irow[[out_time_y_col]]
    #         xticker <- irow[[out_time_xtick]]
    #         graph_type <- irow[[out_time_graph_type_col]]
    #         group <- irow[[out_time_group_col]]
    #         group_levels <- unlist(strsplit(gsub(
    #             '\\\\n', '\n', as.character(irow[[out_time_group_levels_col]])
    #         ), '\n'))
    #         to_log_y <- ifelse(
    #             as.character(irow[[out_time_log_y_col]]) %in% string_yes,
    #             TRUE, FALSE
    #         )
    #         line_cols <- unlist(strsplit(gsub(
    #             '\\\\n', '\n', as.character(irow[[out_time_colors]])
    #         ), '\n'))
    #         line_types <- unlist(strsplit(gsub(
    #             '\\\\n', '\n', as.character(irow[[out_time_linetypes]])
    #         ), '\n'))
    #         subset_ <- irow[[out_time_subset]]
    #         
    #         if(!is_blank(cohort)) {
    #             data_idx <- data_idx[data_idx[[cohort_col]] %in% cohort, ,
    #                                  drop = FALSE]
    #         }
    #         
    #         if(graph_type == 'Summary table') {
    #             for(format_ in input$out_table_format) {
    #                 tfls_time <- c(tfls_time, paste(path, format_, sep = '.'))
    #             }
    #             decimal <- as.numeric(irow[[out_time_decimal]])
    #             add_total_n <- as.numeric(unlist(strsplit(
    #                 as.character(irow[[out_time_col_totals_n]]), ','
    #             )))
    #             add_total_name <- unlist(strsplit(gsub(
    #                 '\\\\n', '\n', as.character(irow[[out_time_col_totals_name]])
    #             ), '\n'))
    #             add_cfb <- ifelse(
    #                 as.character(irow[[out_time_add_cfb]]) %in% string_yes,
    #                 TRUE, FALSE
    #             )
    #             dgt <- as.numeric(irow[[out_time_decimal]])
    #             main <- trimws(gsub('\\\\n', '\n', irow[['Main']]))
    #             if(is_blank(main)) {
    #                 main <- paste('Summary statistics for', bmk,
    #                                add_parenthesis(y_variable))
    #                 if(!is_blank(group)) main <- paste(main, 'by', group)
    #             }
    #             main <- paste0(irow[[out_time_tflt_col]], ' ',
    #                            irow[[out_time_tfln_col]], ': ', main)
    #             idx_fn <- startswith(names_time, 'FN') &
    #                 endswith(names_time, '[1-9][0-9]*')
    #             if(any(idx_fn)) {
    #                 fnn <- unlist(irow[, sort(names_time[idx_fn])], use.names = F)
    #                 footnote <- paste(fn_map[fnn], collapse = '\n')
    #             } else {
    #                 fnote_text <- gsub('\\\\n', '\n', ifelse(
    #                     irow[[out_time_footnote]] == time_plot_footnote_default, '',
    #                     irow[[out_time_footnote]]
    #                 ))
    #                 if(!is_blank(fnote_text)) footnote <- fnote_text
    #                 else footnote <- ''
    #             }
    #             footnote <- trimws(paste(footnote, default_footnote_lines(), sep = '\n'))
    #             
    #             if(!is_blank(subset_)) {
    #                 data_tbl <- with(
    #                     data_idx, data_idx[eval(parse(text = subset_)), ]
    #                 )
    #             }
    #             if(!('All' %in% bmk))
    #                 data_tbl <- data_tbl[data_tbl[[param_col]] %in% bmk, ,
    #                                      drop = FALSE]
    #             if(!is_blank(study))
    #                 data_tbl <- data_tbl[data_tbl[[study_col]] %in% study, ,
    #                                      drop = FALSE]
    #             if(!is_blank(group)) {
    #                 if(!is_blank(group_levels)) {
    #                     data_tbl <- data_tbl[data_tbl[[group]] %in% group_levels, ,
    #                                          drop = FALSE]
    #                     data_tbl[[group]] <- factor(
    #                         data_tbl[[group]], levels = group_levels
    #                     )
    #                 } else {
    #                     data_tbl[[group]] <- factor(data_tbl[[group]])
    #                 }
    #             }
    #             
    #             data_tbl[, xticker] <- factor(
    #                 data_tbl[, xticker],
    #                 levels = unique(data_tbl[[xticker]][order(data_tbl[[xvar_col]])])
    #             )
    #             
    #             summary_func <- c(
    #                 'N' = n_nna,
    #                 'Mean (SD)' = partial(mean_sd_str, digits = dgt),
    #                 '%CV' = partial(coeff_var_str, digits = dgt),
    #                 'Median' = partial(median_str, digits = dgt),
    #                 'Q1, Q3' = partial(q1_q3_str, digits = dgt),
    #                 'Min, Max' = partial(min_max_str, digits = dgt)
    #             )
    #             if(isTRUE(to_log_y)) {
    #                 summary_func <- c(
    #                     summary_func,
    #                     'Geom Mean (%CV)' = partial(geo_mean_cv_str, digits = dgt),
    #                     'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
    #                 )
    #             }
    #             
    #             progress$inc(1 / nrow_total, detail = path)
    #             if('rtf' %in% input$out_table_format) {
    #                 file <- paste(path, 'rtf', sep = '.')
    #                 summary_tbl <- summary_table_all(
    #                     data_tbl, row_var = xticker,
    #                     col_var = group, val_var = y_variable,
    #                     col_totals = add_total_n, name_totals = add_total_name,
    #                     n_in_header = TRUE, subj_col = subj_col,
    #                     baseline_name = NULL, add_cfb = add_cfb, cfb_var = chg_col,
    #                     func_list = summary_func, caption = main,
    #                     footnote = footnote, rowlabel = '', format = 'rtf'
    #                 )
    #                 
    #                 rtf_table_wrapper(
    #                     file, summary_tbl, block_break = TRUE,
    #                     nline_block = length(summary_func) + 1,
    #                     caption = main, footnote = footnote
    #                 )
    #             }
    #         } else {
    #             add_points <- ifelse(
    #                 as.character(irow[[out_time_add_points]]) %in% string_yes,
    #                 TRUE, FALSE
    #             )
    #             add_subjid <- ifelse(
    #                 as.character(irow[[out_time_add_subjid]]) %in% string_yes,
    #                 TRUE, FALSE
    #             )
    #             tunit <- t_unit()
    #             add_sample_size <- isTRUE(irow[[out_time_add_sample_size]])
    #             add_legend <- ifelse(is_blank(group), FALSE, TRUE)
    #             reference_line <- NULL
    #             if(!is_blank(irow[[out_time_reference_line]])) {
    #                 reference_line <- as.numeric(stringr::str_trim(unlist(strsplit(
    #                     as.character(irow[[out_time_reference_line]]) , ','))))
    #             }
    #             
    #             #-----------------------------------
    #             #   Temporary fix for
    #             #   1. Xlab; 2. Ylab; 3. Main
    #             #-----------------------------------
    #             xlab <- trimws(gsub('\\\\n', '\n', irow[['Xlab']]))
    #             ylab <- trimws(gsub('\\\\n', '\n', irow[['Ylab']]))
    #             main <- trimws(gsub('\\\\n', '\n', irow[['Main']]))
    #             if(is_blank(xlab)) {
    #                 if(is.null(xticker) || xticker == xvar_col)
    #                     xlab <- ifelse(tunit == '', 'Time on treatment',
    #                                    paste0(tunit, 's on treatment'))
    #                 else
    #                     xlab <- ''
    #             }
    #             if(is_blank(ylab)) {
    #                 ylab <- paste(
    #                     bmk, add_parenthesis(bmk_value_name_dict[[y_variable]])
    #                 )
    #             }
    #             if(is_blank(main)) {
    #                 main <- paste0(ifelse(is_blank(study), '',
    #                                       paste0('Study: ', study, '\n')),
    #                                bmk, ': ')
    #                 main <- paste0(main, graph_type,
    #                                ifelse(is_blank(group), '',
    #                                       paste0(' by ', group)),
    #                                ifelse(to_log_y, ', semi-log', ''))
    #                 if(!is_blank(cohort))
    #                     main <- paste0(main, '\n', cohort, ' cohort')
    #             }
    #             main <- paste0(irow[[out_time_tflt_col]], ' ',
    #                            irow[[out_time_tfln_col]], ': ', main)
    #             #-----------------------------------
    #             #   Temporary fix for footnote
    #             #-----------------------------------
    #             idx_fn <- startswith(names_time, 'FN') &
    #                 endswith(names_time, '[1-9][0-9]*')
    #             if(any(idx_fn)) {
    #                 fnn <- unlist(irow[, sort(names_time[idx_fn])], use.names = F)
    #                 fnn <- na.omit(fnn)
    #                 if(length(fnn) == 0) {
    #                     footnote <- ''
    #                 } else {
    #                     footnote <- paste(fn_map[fnn], collapse = '\n')
    #                 }
    #             } else {
    #                 fnote_text <- gsub('\\\\n', '\n', ifelse(
    #                     irow[[out_time_footnote]] == time_plot_footnote_default, '',
    #                     irow[[out_time_footnote]]
    #                 ))
    #                 if(!is_blank(fnote_text)) footnote <- fnote_text
    #                 else footnote <- ''
    #             }
    #             footnote <- trimws(paste(footnote, default_footnote_lines(), sep = '\n'))
    #             
    #             for(format_ in input$out_graph_format) {
    #                 tfls_time <- c(tfls_time, paste(path, format_, sep = '.'))
    #             }
    #             include_rtf <- FALSE
    #             formats <- input$out_graph_format
    #             progress$inc(1 / nrow_total, detail = path)
    #             output_time_profiling(path, data_idx, subset_,
    #                                   study, bmk, y_variable = y_variable,
    #                                   graph_type = graph_type, group = group,
    #                                   group_levels = group_levels,
    #                                   to_log_y = to_log_y,
    #                                   add_sample_size = add_sample_size,
    #                                   add_points = add_points,
    #                                   add_subjid = add_subjid,
    #                                   reference_line = reference_line,
    #                                   xmin = irow[[out_time_xmin]],
    #                                   xmax = irow[[out_time_xmax]],
    #                                   ymin = irow[[out_time_ymin]],
    #                                   ymax = irow[[out_time_ymax]],
    #                                   line_cols = line_cols,
    #                                   line_types = line_types,
    #                                   xtick = xticker, xlab = xlab, ylab = ylab,
    #                                   main = main, fnote_text = footnote,
    #                                   height = irow[[out_time_height]],
    #                                   width = irow[[out_time_width]],
    #                                   formats = formats,
    #                                   include_rtf = include_rtf)
    #         }
    #         
    #         
    #     }
    #     out_download_tfl$time <- tfls_time
    #     for(idx in seq_len(nrow(out_ass))) {
    #         irow <- out_ass[idx, , drop = FALSE]
    #         data_idx <- in_data
    #         
    #         file_name <- irow[[out_ass_title_col]]
    #         ass_type <- irow[[out_ass_type]]
    #         cormat_type <- irow[[out_ass_cormat_type]]
    #         study <- irow[[out_ass_study_col]]
    #         cohort <- irow[[out_ass_cohort_col]]
    #         var_x <- irow[[out_ass_var_x_col]]
    #         visit_x <- irow[[out_ass_visit_x_col]]
    #         bmk_x <- irow[[out_ass_bmk_x_col]]
    #         log_x <- irow[[out_ass_log_x_col]]
    #         var_y <- irow[[out_ass_var_y_col]]
    #         visit_y <- irow[[out_ass_visit_y_col]]
    #         bmk_y <- irow[[out_ass_bmk_y_col]]
    #         log_y <- irow[[out_ass_log_y_col]]
    #         height <- irow[[out_ass_height]]
    #         width <- irow[[out_ass_width]]
    #         group_levels <- unlist(strsplit(
    #             as.character(irow[[out_ass_group_levels]]), ','
    #         ))
    #         cortype <- irow[[out_ass_cortype]]
    #         stat_test <- irow[[out_ass_stat_test]]
    #         ptg_type <- irow[[out_ass_ptg_type]]
    #         add_line <- irow[[out_ass_add_line]]
    #         add_point <- irow[[out_ass_add_point]]
    #         add_subjid <- irow[[out_ass_add_subjid]]
    #         subset_str <- irow[[out_ass_subset]]
    #         
    #         if(!is_blank(subset_str)) {
    #             data_idx <- with(
    #                 data_idx, data_idx[eval(parse(text = subset_str)), ]
    #             )
    #         }
    #         if(!is_blank(study)) {
    #             data_idx <- data_idx[data_idx[[study_col]] %in% study, ,
    #                                  drop = FALSE]
    #         }
    #         if(!is_blank(cohort)) {
    #             data_idx <- data_idx[data_idx[[cohort_col]] %in% cohort, ,
    #                                  drop = FALSE]
    #         }
    #         log_x <- ifelse(as.character(log_x) %in% string_yes, T, F)
    #         log_y <- ifelse(as.character(log_y) %in% string_yes, T, F)
    #         add_point <- ifelse(as.character(add_point) %in% string_yes, T, F)
    #         add_subjid <- ifelse(as.character(add_subjid) %in% string_yes, T, F)
    #         
    #         progress$inc(1 / nrow_total, detail = file_name)
    #         
    #         if(ass_type == 'Contingency table') {
    #             
    #         } else {
    #             if(ass_type == 'Histogram') {
    #                 data_idx <- data_idx[data_idx[[xlabel_col]] %in% visit_x, ,
    #                                      drop = F]
    #                 if(is_blank(bmk_x)) {
    #                     if(is_blank(cohort)) {
    #                         data_idx <- group_by_(
    #                             data_idx, study_col, subj_col, xvar_col
    #                         )
    #                     } else {
    #                         data_idx <- group_by_(
    #                             data_idx, study_col, cohort_col,
    #                             subj_col, xvar_col
    #                         )
    #                     }
    #                     data_idx <- filter(data_idx, row_number() == 1)
    #                 } else {
    #                     data_idx <- data_idx[
    #                         data_idx[[param_col]] == bmk_x, , drop = F
    #                         ]
    #                 }
    #                 var_unit <- ''
    #                 if(!is_blank(bmk_x)) {
    #                     var_unit <- bmk_value_name_dict[[var_x]]
    #                     has_unit <- var_x %in% c('AVAL', 'CHG')
    #                     if(has_unit && bmk_unit_col %in% names(data_idx)) {
    #                         var_unit <- smart_paste(
    #                             var_unit, unique(data_idx[[bmk_unit_col]]),
    #                             sep = ', '
    #                         )
    #                     }
    #                 }
    #                 var_name <- ifelse(!is_blank(bmk_x), bmk_x, var_x)
    #                 main <- ifelse(
    #                     !is_blank(study), paste0('Study: ', study), ''
    #                 )
    #                 if(length(visit_x) == 1)
    #                     main <- paste(main, paste(visit_x, var_name), sep = '\n')
    #                 if(!is_blank(cohort))
    #                     main <- paste(main, paste(cohort, 'cohort'), sep = '\n')
    #                 main <- trimws(main)
    #                 for(format_ in input$out_graph_format) {
    #                     if(format_ == 'pdf') {
    #                         file_ <- paste0(file_name, '.pdf')
    #                         pdf(file = file_, height = height, width = width)
    #                     } else if(format_ == 'png') {
    #                         file_ <- paste0(file_name, '.png')
    #                         png(file = file_, height = height, width = width,
    #                             units = 'in', res = 600)
    #                     }
    #                     histogram_plot(
    #                         data_idx, var_x, var_unit = var_unit,
    #                         to_log = log_x, visit = visit_x,
    #                         var_name = var_name, main = main,
    #                         add_density_curve = TRUE
    #                     )
    #                     tfls_ass <- c(tfls_ass, file_)
    #                     dev.off()
    #                 }
    #             } else if(ass_type == 'Scatter plot') {
    #                 data_x <- data_idx[
    #                     data_idx[[xlabel_col]] %in% visit_x, , drop = F
    #                     ]
    #                 data_y <- data_idx[
    #                     data_idx[[xlabel_col]] %in% visit_y, , drop = F
    #                     ]
    #                 if(is_blank(bmk_x)) {
    #                     if(is_blank(cohort))
    #                         data_x <- group_by_(
    #                             data_x, study_col, subj_col, xvar_col
    #                         )
    #                     else
    #                         data_x <- group_by_(
    #                             data_x, study_col, cohort_col, subj_col,
    #                             xvar_col
    #                         )
    #                     data_x <- filter(data_x, row_number() == 1)
    #                 } else {
    #                     data_x <- data_x[
    #                         data_x[[param_col]] == bmk_x, , drop = F
    #                         ]
    #                 }
    #                 if(is_blank(bmk_y)) {
    #                     if(is_blank(cohort))
    #                         data_y <- group_by_(
    #                             data_y, study_col, subj_col, xvar_col
    #                         )
    #                     else
    #                         data_y <- group_by_(
    #                             data_y, study_col, cohort_col, subj_col,
    #                             xvar_col
    #                         )
    #                     data_y <- filter(data_y, row_number() == 1)
    #                 } else {
    #                     data_y <- data_y[
    #                         data_y[[param_col]] == bmk_y, , drop = F
    #                         ]
    #                 }
    #                 by_vars <- c(study_col)
    #                 if(!is_blank(cohort)) by_vars <- c(by_vars, cohort_col)
    #                 by_vars <- c(by_vars, subj_col)
    #                 if(identical(visit_x, visit_y))
    #                     by_vars <- c(by_vars, xlabel_col)
    #                 data_x <- data_x[, c(by_vars, var_x)]
    #                 data_y <- data_y[, c(by_vars, var_y)]
    #                 data_merge <- merge(data_x, data_y, by = by_vars, all = T)
    #                 if(identical(var_x, var_y)) {
    #                     var_x_merge <- paste(var_x, 'x', sep = '.')
    #                     var_y_merge <- paste(var_y, 'y', sep = '.')
    #                 } else {
    #                     var_x_merge <- var_x
    #                     var_y_merge <- var_y
    #                 }
    #                 visit_x <- ifelse(length(visit_x) > 1, '', visit_x)
    #                 visit_y <- ifelse(length(visit_y) > 1, '', visit_y)
    #                 unit_x <- ''
    #                 unit_y <- ''
    #                 name_x <- var_x
    #                 name_y <- var_y
    #                 if(!is_blank(bmk_x)) {
    #                     name_x <- bmk_x
    #                     unit_x <- bmk_value_name_dict[[var_x]]
    #                     if(var_x %in% c('AVAL', 'CHG'))
    #                         unit_x <- smart_paste(
    #                             unit_x, bmk_unit()[[name_x]], sep = ', '
    #                         )
    #                 }
    #                 if(!is_blank(bmk_y)) {
    #                     name_y <- bmk_y
    #                     unit_y <- bmk_value_name_dict[[var_y]]
    #                     if(var_y %in% c('AVAL', 'CHG'))
    #                         unit_y <- smart_paste(
    #                             unit_y, bmk_unit()[[name_y]], sep = ', '
    #                         )
    #                 }
    #                 main <- ifelse(
    #                     !is_blank(study), paste0('Study: ', study), ''
    #                 )
    #                 if(!is_blank(cohort))
    #                     main <- paste(
    #                         main, paste(cohort, 'cohort'), sep = '\n'
    #                     )
    #                 main <- trimws(main)
    #                 for(format_ in input$out_graph_format) {
    #                     if(format_ == 'pdf') {
    #                         file_ <- paste0(file_name, '.pdf')
    #                         pdf(file = file_, height = height, width = width)
    #                     } else if(format_ == 'png') {
    #                         file_ <- paste0(file_name, '.png')
    #                         png(file = file_, height = height, width = width,
    #                             units = 'in', res = 600)
    #                     }
    #                     scatter_plot(
    #                         data_merge, var_x_merge, var_y_merge,
    #                         subj_col = subj_col, visit_x, visit_y,
    #                         name_x, name_y, to_log_x = log_x, to_log_y = log_y,
    #                         x_unit = unit_x, y_unit = unit_y, main = main,
    #                         add_line = add_line, add_subjid = add_subjid,
    #                         test_method = cortype
    #                     )
    #                     tfls_ass <- c(tfls_ass, file_)
    #                     dev.off()
    #                 }
    #             } else if(ass_type == 'Box plot') {
    #                 data_x <- data_idx[
    #                     data_idx[[xlabel_col]] %in% visit_x, , drop = F
    #                     ]
    #                 data_y <- data_idx[
    #                     data_idx[[xlabel_col]] %in% visit_y, , drop = F
    #                     ]
    #                 if(is_blank(bmk_x)) {
    #                     if(is_blank(cohort))
    #                         data_x <- group_by_(
    #                             data_x, study_col, subj_col, xvar_col
    #                         )
    #                     else
    #                         data_x <- group_by_(
    #                             data_x, study_col, cohort_col, subj_col,
    #                             xvar_col
    #                         )
    #                     data_x <- filter(data_x, row_number() == 1)
    #                 } else {
    #                     data_x <- data_x[
    #                         data_x[[param_col]] == bmk_x, , drop = F
    #                         ]
    #                 }
    #                 if(is_blank(bmk_y)) {
    #                     if(is_blank(cohort))
    #                         data_y <- group_by_(
    #                             data_y, study_col, subj_col, xvar_col
    #                         )
    #                     else
    #                         data_y <- group_by_(
    #                             data_y, study_col, cohort_col, subj_col,
    #                             xvar_col
    #                         )
    #                     data_y <- filter(data_y, row_number() == 1)
    #                 } else {
    #                     data_y <- data_y[
    #                         data_y[[param_col]] == bmk_y, , drop = F
    #                         ]
    #                 }
    #                 by_vars <- c(study_col)
    #                 if(!is_blank(cohort)) by_vars <- c(by_vars, cohort_col)
    #                 by_vars <- c(by_vars, subj_col)
    #                 if(identical(visit_x, visit_y))
    #                     by_vars <- c(by_vars, xlabel_col)
    #                 data_x <- data_x[, c(by_vars, var_x)]
    #                 data_y <- data_y[, c(by_vars, var_y)]
    #                 data_merge <- merge(data_x, data_y, by = by_vars, all = T)
    #                 data_merge <- data_merge[
    #                     data_merge[[var_x]] %in% group_levels, , drop = F
    #                     ]
    #                 
    #                 visit_y <- ifelse(length(visit_y) > 1, '', visit_y)
    #                 unit_y <- ''
    #                 name_y <- var_y
    #                 if(!is_blank(bmk_y)) {
    #                     name_y <- bmk_y
    #                     unit_y <- bmk_value_name_dict[[var_y]]
    #                     if(var_y %in% c('AVAL', 'CHG'))
    #                         unit_y <- smart_paste(
    #                             unit_y, bmk_unit()[[name_y]], sep = ', '
    #                         )
    #                 }
    #                 main <- ifelse(
    #                     !is_blank(study), paste0('Study: ', study), ''
    #                 )
    #                 if(!is_blank(cohort))
    #                     main <- paste(
    #                         main, paste(cohort, 'cohort'), sep = '\n'
    #                     )
    #                 main <- trimws(main)
    #                 for(format_ in input$out_graph_format) {
    #                     if(format_ == 'pdf') {
    #                         file_ <- paste0(file_name, '.pdf')
    #                         pdf(file = file_, height = height, width = width)
    #                     } else if(format_ == 'png') {
    #                         file_ <- paste0(file_name, '.png')
    #                         png(file = file_, height = height, width = width,
    #                             units = 'in', res = 600)
    #                     }
    #                     boxplot_bmk(
    #                         data_merge, var_y, var_x, subj_col, visit_y,
    #                         var_name = name_y, group_lev_name = group_levels,
    #                         to_log = log_y, var_unit = unit_y, main = main,
    #                         test_method = stat_test, add_points = add_point,
    #                         add_subjid = add_subjid
    #                     )
    #                     tfls_ass <- c(tfls_ass, file_)
    #                     dev.off()
    #                 }
    #             } else if(ass_type == 'Correlation matrix') {
    #                 data_cormat <- data_idx
    #                 if(!is_blank(study)) {
    #                     data_cormat <- data_cormat[
    #                         data_cormat[[study_col]] == study, , drop = F
    #                         ]
    #                 }
    #                 if(!is_blank(cohort)) {
    #                     data_cormat <- data_cormat[
    #                         data_cormat[[cohort_col]] == cohort, , drop = F
    #                         ]
    #                 }
    #                 bmk_x <- unlist(strsplit(bmk_x, ','))
    #                 visit_x <- unlist(strsplit(visit_x, ','))
    #                 data_cormat <- data_cormat[
    #                     data_cormat[[param_col]] %in% bmk_x, 
    #                     ]
    #                 data_cormat <- data_cormat[
    #                     data_cormat[[xlabel_col]] %in% visit_x, 
    #                     ]
    #                 if(cortype == 'intra-biomarker') {
    #                     formula_ <- paste(
    #                         param_col, '+', subj_col, '~', xlabel_col
    #                     )
    #                     var_widget <- bmk_x
    #                     var_colname <- param_col
    #                 } else if(cortype == 'inter-biomarker') {
    #                     formula_ <- paste(
    #                         xlabel_col, '+', subj_col, '~', param_col
    #                     )
    #                     var_widget <- visit_x
    #                     var_colname <- xlabel_col
    #                 }
    #                 data_wide <- reshape2::dcast(
    #                     data_cormat, formula_, value.var = var_x
    #                 )
    #                 
    #                 value_cols <- !(colnames(data_wide) %in%
    #                                     c(subj_col, var_colname))
    #                 data_plot <- data_wide[, value_cols, drop = FALSE]
    #                 
    #                 for(format_ in input$out_graph_format) {
    #                     if(format_ == 'pdf') {
    #                         file_ <- paste0(file_name, '.pdf')
    #                         pdf(file = file_, height = height, width = width)
    #                     } else if(format_ == 'png') {
    #                         file_ <- paste0(file_name, '.png')
    #                         png(file = file_, height = height, width = width,
    #                             units = 'in', res = 600)
    #                     }
    #                     if(cormat_type == 'Scatter matrix') {
    #                         scatter_matrix(data_plot, main = var_widget)
    #                     } else if(cormat_type == 'circle matrix') {
    #                         cormat <- cor(
    #                             data_plot, use = 'pairwise.complete.obs',
    #                             method = 'spearman'
    #                         )
    #                         suppressMessages(corrplot::corrplot.mixed(
    #                             cormat, order = 'hclust',
    #                             hclust.method = 'ward', title = var_widget
    #                         ))
    #                     }
    #                     tfls_ass <- c(tfls_ass, file_)
    #                     dev.off()
    #                 }
    #             } else if(ass_type == 'Contingency table') {
    #                 
    #             }
    #         }
    #     }
    #     out_download_tfl$ass <- tfls_ass
    #     
    #     out_download_ready$value <- TRUE
    # })
    
    # output results button
    out_download_ready <- reactiveValues(value = FALSE)
    out_download_tfl <- reactiveValues(dir = NULL, time = c(), ass = c())
    
    # batch download button through TNF
    output$out_download_button <- renderUI({
        req(file_import$status)
        shinyjs::disabled(downloadButton('out_download', 'Output results'))
    })
    observe({
        if(isTRUE(out_download_ready$value) &&
           !is_blank(c(out_download_tfl$time, out_download_tfl$ass))) {
            shinyjs::enable('out_download')
        } else {
            shinyjs::disable('out_download')
        }
    })
    observe({
        input$out_file
        out_download_ready$value <- FALSE
        out_download_tfl$time <- c()
        out_download_tfl$ass <- c()
        out_download_tfl$dir <- NULL
    })
    
    # output results button handler
    output$out_download <- downloadHandler(
        filename = function() {
            paste('output', 'zip', sep = '.')
        },
        content = function(file) {
            owd <- setwd(out_download_tfl$dir)
            on.exit(setwd(owd))
            system2(
                'zip',
                args = shQuote(c('-r9X',
                                 file,
                                 out_download_tfl$time,
                                 out_download_tfl$ass)),
                stdout = FALSE
            )
        },
        contentType = 'application/zip'
    )
    
    # a tabset UI for TNF file
    output$out_tnf_tabpanel <- renderUI({
        tabs <- list(id = 'out_tnf_tabs')
        if(!is.null(tnf$time_df)) {
            tabs <- append_alist(tabPanel(
                sheet_names$time, DT::dataTableOutput('out_tnf_time_tab')
            ), tabs)
        }
        if(!is.null(tnf$ass_df)) {
            tabs <- append_alist(tabPanel(
                sheet_names$ass, DT::dataTableOutput('out_tnf_ass_tab')
            ), tabs)
        }
        do.call(tabsetPanel, tabs)
    })
    
    # data table output for TNF file
    output$out_tnf_time_tab <- DT::renderDataTable({
        DT::datatable(tnf$time_df, options = list(dom = 't'))
    })
    output$out_tnf_ass_tab <- DT::renderDataTable({
        DT::datatable(tnf$ass_df, options = list(dom = 't'))
    })
    
})




















