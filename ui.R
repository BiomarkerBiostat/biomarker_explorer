# ui.R

shinyUI(navbarPage(
    
    title = 'Biomarker Explorer',
    
    #-----------------------------------------------
    # Import data page
    #-----------------------------------------------
    tabPanel(
        title = 'Import data',
        sidebarLayout(
            sidebarPanel(
                fileInput('file', tags$p('Choose file to import',
                                         tags$a('(Need input data template?)',
                                                target = '_blank',
                                                href = template_file)),
                          accept = accepted_file_format),
                tags$hr(),
                tags$div(
                    id = 'file_format',
                    checkboxInput('file_header', 'Header', TRUE),
                    radioButtons('file_sep', 'Separator',
                                 choices = separator_choices,
                                 selected = separator_default),
                    radioButtons('file_quote', 'Quote',
                                 choices = quote_choices,
                                 selected = quote_default)
                ),
                tags$hr(),
                message
            ),
            mainPanel(
                fluidRow(
                    column(2, uiOutput('file_subset_button')),
                    bsTooltip('file_subset_button', paste(
                        'Click to open a window to subset the read-in data'
                    )),
                    column(3, uiOutput('file_topn_button'), offset = 7)
                ),
                shinyBS::bsModal(
                    'file_subset_bsmodal', 'Data subsetting', 'file_subset_button',
                    size = 'large',
                    fluidRow(
                        column(5, uiOutput('file_subset_var')),
                        column(5, offset = 2, uiOutput('file_subset_val'))
                    ),
                    fluidRow(
                        column(2, uiOutput('file_subset_add')),
                        column(2, offset = 3, uiOutput('file_subset_clear')),
                        column(2, offset = 3, uiOutput('file_subset_done'))
                    )
                ),
                tags$hr(),
                uiOutput('file_import_status'),
                tags$br(),
                DT::dataTableOutput('file_topn_table')
            )
        )
    ),
    
    shinyjs::useShinyjs(),


    #-----------------------------------------------
    # Time profiling page
    #-----------------------------------------------
    tabPanel(
        title = 'Time profiling',
        sidebarPanel(
            tags$div(
                id = 'time',
                uiOutput('time_study'),
                uiOutput('time_cohort'),
                uiOutput('time_bmk'),
                uiOutput('time_y'),
                uiOutput('time_graph_type'),
                fluidRow(
                    column(width = 6, uiOutput('time_group')),
                    column(width = 6, uiOutput('time_group_levs'))
                ),
                fluidRow(
                    column(width = 6, uiOutput('time_facet_r')),
                    column(width = 6, uiOutput('time_facet_rlevs'))
                ),
                fluidRow(
                    column(width = 6, uiOutput('time_facet_c')),
                    column(width = 6, uiOutput('time_facet_clevs'))
                ),
                uiOutput('time_xrange')
            ),
            width = 4
        ),
        mainPanel(
            fluidRow(
                column(
                    width = 3,
                    uiOutput('time_download_plot_action'),
                    shinyBS::bsTooltip('time_download_plot_action', paste(
                        'Click to open a window to specify the attributes of the',
                        'graph to be downloaded'
                    )),
                    uiOutput('time_download_table_action'),
                    shinyBS::bsTooltip('time_download_table_action', paste(
                        'Click to open a window to specify the attributes of the',
                        'table to be downloaded'
                    ))
                ),
                column(width = 9)
            ),
            shinyBS::bsModal(
                'time_plot_bsmodal', 'Download page', 'time_download_plot_action',
                size = 'large',
                fluidRow(
                    column(width = 2, uiOutput('time_download_plot_format')),
                    column(width = 2, uiOutput('time_download_plot_height')),
                    column(width = 2, uiOutput('time_download_plot_width')),
                    column(width = 3, uiOutput('time_download_plot_resolution')),
                    column(width = 2, uiOutput('time_download_multibmk')),
                    column(width = 2, uiOutput('time_download_plot_button'))
                ),
                tags$style(type='text/css', "#time_download_plot_button {margin-top: 25px;}")
            ),
            shinyBS::bsModal(
                'time_table_bsmodal', 'Download page', 'time_download_table_action',
                size = 'large',
                fluidRow(
                    column(
                        width = 3,
                        selectInput(
                            'time_download_table_format', 'Table format',
                            choices = c('Choose' = '', time_download_table_formats)
                        )
                    ),
                    column(width = 3, uiOutput('time_download_table_height')),
                    column(width = 3, uiOutput('time_download_table_width')),
                    column(width = 3, uiOutput('time_download_table_button'))
                ),
                tags$style(type='text/css', "#time_download_table_button {vertical-align: bottom;}")
            ),
            tags$hr(),
            uiOutput('time_plot_ui'),
            uiOutput('time_plot_footnote_out'),
            uiOutput('time_table'),
            fluidRow(
                column(3, uiOutput('time_unbrush_button')),
                column(9)
            ),
            width = 6
        ),
        column(
            width = 2,
            uiOutput('time_add_to_tnf'),
            tags$br(),
            uiOutput('time_xticker'),
            
            uiOutput('time_table_title'),
            uiOutput('time_table_footnote'),
            uiOutput('time_decimal'),
            uiOutput('time_col_totals_n'),
            uiOutput('time_col_totals_name'),
            uiOutput('time_add_cfb'),
            
            uiOutput('time_plot_title'),
            uiOutput('time_plot_xlab'),
            uiOutput('time_plot_ylab'),
            uiOutput('time_plot_footnote'),
            uiOutput('time_plot_xtick_angle'),
            uiOutput('time_reference_line'),
            
            uiOutput('time_to_log'),
            uiOutput('time_toggle_sample_size'),
            uiOutput('time_toggle_points'),
            uiOutput('time_toggle_subjid'),
            
            uiOutput('time_graph_attributes'),
            
            shinyBS::bsTooltip('time_add_to_tnf', paste(
                'Click to add the current time-profiling',
                'graph parameter specification to a output specs file.',
                'The output specs file can be downloaded in "Output results" page.'
            )),
            shinyBS::bsTooltip('time_col_totals_n', paste(
                'Add first n column totals to the summary table.',
                'For example, input `3` if the user wants to add the',
                'total of the first 3 group categories. Multiple inputs are',
                'permitted, e.g. 2, 3'
            )),
            shinyBS::bsTooltip('time_col_totals_name', paste(
                'Specify headers for the column totals.', 'One row per each.'
            )),
            shinyBS::bsTooltip(
                'time_reference_line',
                paste0('Enter reference line position.',
                       'Multiple lines separated by comma, e.g. 1, 2')
            ),
            
            shinyBS::bsModal(
                'time_graph_attr_modal', 'Graph attributes',
                'time_graph_attributes', size = 'large',
                fluidRow(
                    column(5, uiOutput('time_line_color_ui')),
                    column(5, offset = 2, uiOutput('time_line_type_ui'))
                ),
                tags$br(),
                tags$hr(),
                tags$h4('Convert color names to hex RGB strings'),
                fluidRow(
                    column(
                        width = 5,
                        textInput('hex_modal_color', 'Input color name',
                                  value = '')
                    ),
                    column(width = 2),
                    column(width = 5, textOutput('hex_modal_hex')),
                    tags$style(type = 'text/css',
                               '#hex_modal_hex {vertical-align: bottom;}')
                ),
                actionButton('hex_modal_button', 'Convert')
            )
        )
    ),
    
    #-----------------------------------------------
    # Association graph page
    #-----------------------------------------------
    tabPanel(
        title = 'Association',
        fluidRow(
            column(width = 4,
                wellPanel(
                    tags$div(
                        id = 'ass_overall',
                        uiOutput('ass_type'),
                        uiOutput('ass_study'),
                        uiOutput('ass_cohort')
                    ),
                    tags$div(
                        id = 'ass_hist',
                        fluidRow(
                            column(width = 6, uiOutput('ass_hist_bmk')),
                            column(width = 6, uiOutput('ass_hist_var'))
                        ),
                        uiOutput('ass_hist_visits'),
                        fluidRow(
                            column(width = 6, uiOutput('ass_hist_group')),
                            column(width = 6, uiOutput('ass_hist_group_levs'))
                        ),
                        fluidRow(
                            column(width = 6, uiOutput('ass_hist_facet_r')),
                            column(width = 6, uiOutput('ass_hist_facet_rlevs'))
                        ),
                        fluidRow(
                            column(width = 6, uiOutput('ass_hist_facet_c')),
                            column(width = 6, uiOutput('ass_hist_facet_clevs'))
                        )
                    ),
                    tags$div(
                        id = 'ass_scatter',
                        fluidRow(
                            column(
                                width = 6,
                                uiOutput('ass_scatter_bmk_x'),
                                uiOutput('ass_scatter_x'),
                                uiOutput('ass_scatter_visits_x')
                            ),
                            column(
                                width = 6,
                                uiOutput('ass_scatter_bmk_y'),
                                uiOutput('ass_scatter_y'),
                                uiOutput('ass_scatter_visits_y')
                            )
                        ),
                        fluidRow(
                            column(width = 6, uiOutput('ass_scatter_group')),
                            column(width = 6, uiOutput('ass_scatter_group_levs'))
                        ),
                        fluidRow(
                            column(width = 6, uiOutput('ass_scatter_facet_r')),
                            column(width = 6, uiOutput('ass_scatter_facet_rlevs'))
                        ),
                        fluidRow(
                            column(width = 6, uiOutput('ass_scatter_facet_c')),
                            column(width = 6, uiOutput('ass_scatter_facet_clevs'))
                        )
                    ),
                    tags$div(
                        id = 'ass_box',
                        fluidRow(
                            column(width = 6, uiOutput('ass_box_y')),
                            column(width = 6, uiOutput('ass_box_bmk'))
                        ),
                        uiOutput('ass_box_visits'),
                        fluidRow(
                            column(width = 6, uiOutput('ass_box_x')),
                            column(width = 6, uiOutput('ass_box_xlevs'))
                        ),
                        fluidRow(
                            column(width = 6, uiOutput('ass_box_group')),
                            column(width = 6, uiOutput('ass_box_group_levs'))
                        ),
                        fluidRow(
                            column(width = 6, uiOutput('ass_box_facet_r')),
                            column(width = 6, uiOutput('ass_box_facet_rlevs'))
                        ),
                        fluidRow(
                            column(width = 6, uiOutput('ass_box_facet_c')),
                            column(width = 6, uiOutput('ass_box_facet_clevs'))
                        )
                    ),
                    tags$div(
                        id = 'ass_table',
                        fluidRow(
                            column(
                                width = 6,
                                uiOutput('ass_table_x'),
                                uiOutput('ass_table_bmk_depend_x'),
                                uiOutput('ass_table_bmk_x'),
                                uiOutput('ass_table_time_x')
                            ),
                            column(
                                width = 6,
                                uiOutput('ass_table_y'),
                                uiOutput('ass_table_bmk_depend_y'),
                                uiOutput('ass_table_bmk_y'),
                                uiOutput('ass_table_time_y')
                            )
                        ),
                        uiOutput('ass_table_percentage'),
                        uiOutput('ass_table_test')
                    ),
                    tags$div(
                        id = 'ass_cormat',
                        fluidRow(
                            column(width = 6, uiOutput('ass_cormat_cortype')),
                            column(width = 6, uiOutput('ass_cormat_plottype'))
                        ),
                        uiOutput('ass_cormat_bmk'),
                        uiOutput('ass_cormat_y'),
                        uiOutput('ass_cormat_time')
                    )
                )
            ),
            column(
                width = 6,

                fluidRow(
                    column(
                        width = 3,
                        uiOutput('ass_download_plot_action'),
                        bsTooltip('ass_download_plot_action', paste(
                            'Click to open a window to specify the attributes',
                            'of the graph to be download'
                        )),
                        uiOutput('ass_download_table_action'),
                        bsTooltip('ass_download_table_action', paste(
                            'Click to open a window to specify the attributes',
                            'of the table to be download'
                        ))
                    ),
                    column(width = 9)
                ),
                
                bsModal(
                    'ass_plot_bsmodal', 'Download page', 'ass_download_plot_action',
                    size = 'large',
                    fluidRow(
                        column(
                            width = 3,
                            selectInput(
                                'ass_download_plot_format', 'Plot format',
                                choices = c('Choose' = '', ass_download_plot_formats)
                            )
                        ),
                        column(width = 3, uiOutput('ass_download_plot_height')),
                        column(width = 3, uiOutput('ass_download_plot_width')),
                        column(width = 3, uiOutput('ass_download_plot_multibmk')),
                        column(
                            width = 3,
                            uiOutput('ass_download_hist_button'),
                            uiOutput('ass_download_scatter_button'),
                            uiOutput('ass_download_box_button'),
                            uiOutput('ass_download_cormat_button')
                        )
                    ),
                    tags$style(type='text/css', "#ass_download_hist_button {margin-top: 25px;}"),
                    tags$style(type='text/css', "#ass_download_scatter_button {margin-top: 25px;}"),
                    tags$style(type='text/css', "#ass_download_box_button {margin-top: 25px;}"),
                    tags$style(type='text/css', "#ass_download_cormat_button {margin-top: 25px;}")
                ),
                bsModal(
                    'ass_table_bsmodal', 'Download page', 'ass_download_table_action',
                    size = 'large',
                    fluidRow(
                        column(
                            width = 3,
                            selectInput(
                                'ass_download_table_format', 'Table format',
                                choices = c('Choose' = '', ass_download_table_formats),
                                selected = 'pdf'
                            )
                        ),
                        column(width = 3, uiOutput('ass_download_table_height')),
                        column(width = 3, uiOutput('ass_download_table_width')),
                        column(width = 3, uiOutput('ass_download_table_button'))
                    ),
                    tags$style(type='text/css', "#ass_download_table_button {margin-top: 25px;}")
                ),
                
                tags$hr(),
                uiOutput('ass_plot_block'),
                uiOutput('ass_hist_output'),
                uiOutput('ass_scatter_output'),
                uiOutput('ass_box_output'),
                uiOutput('ass_table_output'),
                uiOutput('ass_cormat_output')
            ),
            column(
                width = 2,
                uiOutput('ass_add_to_tnf'),
                bsTooltip('ass_add_to_tnf', paste(
                    'Click to add the current association',
                    'graph/table parameter specification to a output specs file.',
                    'The output specs file can be downloaded in "Output results" page.'
                )),
                
                uiOutput('ass_hist_title'),
                uiOutput('ass_hist_xlab'),
                uiOutput('ass_hist_ylab'),
                uiOutput('ass_hist_log_x'),
                uiOutput('ass_hist_geom'),
                uiOutput('ass_hist_footnote_in'),
                
                uiOutput('ass_scatter_title'),
                uiOutput('ass_scatter_xlab'),
                uiOutput('ass_scatter_ylab'),
                fluidRow(
                    column(width = 6, uiOutput('ass_scatter_log_x')),
                    column(width = 6, uiOutput('ass_scatter_log_y'))
                ),
                uiOutput('ass_scatter_add_line'),
                uiOutput('ass_scatter_add_ci'),
                uiOutput('ass_scatter_toggle_subjid'),
                uiOutput('ass_scatter_test'),
                uiOutput('ass_scatter_footnote_in'),
                
                uiOutput('ass_box_title'),
                uiOutput('ass_box_xlab'),
                uiOutput('ass_box_ylab'),
                uiOutput('ass_box_footnote_in'),
                fluidRow(
                    column(width = 5, uiOutput('ass_box_log_y')),
                    column(width = 7, uiOutput('ass_box_add_sample_size'))
                ),
                fluidRow(
                    column(width = 5, uiOutput('ass_box_add_points')),
                    column(width = 7, uiOutput('ass_box_show_subjid'))
                ),
                uiOutput('ass_box_refline'),
                uiOutput('ass_box_test'),
                shinyBS::bsTooltip(
                    'ass_box_refline',
                    paste0('Enter reference line position.',
                           'Multiple lines separated by comma, e.g. 1, 2')
                )
            )
        )
    ),
    
    
    #-----------------------------------------------
    # Survival analysis page
    #-----------------------------------------------
    tabPanel(
        title = 'Survival',
        fluidRow(
            column(
                width = 3,
                wellPanel(
                    tags$div(
                        id = 'surv',
                        uiOutput('surv_study'),
                        uiOutput('surv_cohort'),
                        uiOutput('surv_time'),
                        uiOutput('surv_bmk_name'),
                        uiOutput('surv_bmk_value'),
                        uiOutput('surv_bmk_visit'),
                        uiOutput('surv_bmk_cutoff')
                    )
                )
            ),
            column(
                width = 7,
                fluidRow(
                    column(
                        width = 3,
                        uiOutput('surv_download_plot_action'),
                        bsTooltip('surv_download_plot_action', paste(
                            'Click to open a window to specify the attributes',
                            'of the graph to be downloaded'
                        ))
                    ),
                    bsModal(
                        'surv_plot_bsmodal', 'Download page', 'surv_download_plot_action',
                        size = 'large',
                        fluidRow(
                            column(
                                width = 3,
                                selectInput(
                                    'surv_download_plot_format', 'Plot format',
                                    choices = c('Choose' = '', surv_download_plot_formats)
                                )
                            ),
                            column(width = 3, uiOutput('surv_download_plot_height')),
                            column(width = 3, uiOutput('surv_download_plot_width')),
                            column(
                                width = 3,
                                uiOutput('surv_download_plot_button')
                            )
                        ),
                        tags$style(type='text/css', "#surv_download_plot_button {margin-top: 25px;}")
                    ),
                    column(width = 6),
                    column(
                        width = 3,
                        uiOutput('surv_download_table_action'),
                        bsTooltip('surv_download_table_action', paste(
                            'Click to open a window to specify the attributes',
                            'of the table to be downloaded'
                        ))
                    ),
                    bsModal(
                        'surv_table_bsmodal', 'Download page', 'surv_download_table_action',
                        size = 'large',
                        fluidRow(
                            column(
                                width = 3,
                                selectInput(
                                    'surv_download_table_format', 'Table format',
                                    choices = c('Choose' = '', surv_download_table_formats)
                                )
                            ),
                            column(width = 6),
                            column(width = 3, uiOutput('surv_download_table_button'))
                        ),
                        tags$style(type='text/css', "#surv_download_table_button {margin-top: 25px;}")
                    )
                ),
                uiOutput('surv_message'),
                plotOutput('surv_kmcurve'),
                uiOutput('surv_table')
            ),
            column(
                width = 2,
                uiOutput('surv_plot_xlab'),
                uiOutput('surv_plot_ylab'),
                uiOutput('surv_plot_main'),
                tags$hr(),
                uiOutput('surv_table_title'),
                uiOutput('surv_table_footnote'),
                uiOutput('surv_table_decimal')
            )
        )
    ),
    


    #-----------------------------------------------
    # Output results page
    #-----------------------------------------------
    tabPanel(
        title = 'Output results',
        sidebarLayout(
            sidebarPanel(
                tags$h3('Upload output specs file'),
                uiOutput('out_file'),
                fluidRow(
                    column(width = 6, uiOutput('out_graph_format')),
                    column(width = 6, uiOutput('out_table_format'))
                ),
                bsTooltip('out_graph_format',
                          'Please select at least one format!'),
                conditionalPanel(
                    'input.time_add_to_tnf > 0 || input.ass_add_to_tnf > 0',
                    tags$hr(),
                    tags$h3('Download output specs file'),
                    downloadButton('out_download_tnf', 'Download output specs'),
                    tags$br(), tags$br(),
                    actionButton('out_tnf_preview', 'Preview output specs', icon = icon('table')),
                    bsTooltip('out_tnf_preview', paste(
                        'Click to show the output specs file with all saved graph/table',
                        'specifications.'
                    ))
                )
            ),
            mainPanel(
                fluidRow(
                    column(
                        width = 4,
                        uiOutput('out_process_tnf_button')
                    ),
                    column(width = 4),
                    column(
                        width = 4,
                        uiOutput('out_download_button')
                    )
                ),
                conditionalPanel(
                    condition = 'input.out_tnf_preview > 0',
                    tags$hr(),
                    uiOutput('out_tnf_tabpanel')
                )
            )
        )
    ),

    #-----------------------------------------------
    # Survey page
    #-----------------------------------------------
    navbarMenu('Documents',
        tabPanel('User manual',
            fluidRow(
                column(3),
                column(6,
                       tags$p(style = 'font-size:175%;',
                           tags$br(), tags$br(), tags$br(), tags$br(), tags$br(),
                           'Please refer to ',
                           tags$a(tags$b('user manual'), target = '_blank',
                                  href = user_manual),
                           ' for more details on how to use the app. Thank you!'
                       )
                ),
                column(3)
            )
        ),
        tabPanel('User experience survey',
            fluidRow(
                column(3),
                column(6,
                       tags$p(style = 'font-size:175%;',
                           tags$br(), tags$br(), tags$br(), tags$br(), tags$br(),
                           'Please take a few minutes to complete ',
                           tags$a(tags$b('the survey'), target = '_blank',
                                  href = user_survey),
                           ' to help us improve. Thank you!'
                       )
                ),
                column(3)
            )
        )
    )

))
