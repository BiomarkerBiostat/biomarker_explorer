#------------------------------#
# Biomarker Explorer shiny app #
#------------------------------#

# global.R

#-----------------------------------------------
# Set up Shiny environment
#-----------------------------------------------

# Set the heap size = 4GB to read possibly large xlsx data
options(java.parameters = '-Xmx4000m')

# By default, the file size limit is 5MB. Here we'll raise limit to 500MB
options(shiny.maxRequestSize = 500 * 1024^2)


#-----------------------------------------------
# Load necessary libraries and files
# Purpose of each necessary library
#   - shiny:    Web Application Framework for R
#   - shinyjs:  Perform common JavaScript operations in Shiny applications 
#               without having to know any JavaScript. Examples include: 
#               hiding an element, disabling an input, resetting an input back 
#               to its original value.
#   - shinyBS:  Twitter Bootstrap Components for Shiny which adds additional 
#               functionality and interactivity to your Shiny applications, 
#               such as a pop-up modal windows triggered by hitting an button
#   - DT:       The R package DT provides an R interface to the JavaScript
#               library DataTables.
#   - rtf:      A set of R functions to output Rich Text Format (RTF) files 
#               with high resolution tables and graphics that may be edited 
#               with a standard word processor such as Microsoft Word.
#   - dplyr:    A Grammar of Data Manipulation with operations that can be 
#               easily chained together
#   - lazyeval: An alternative approach to non-standard evaluation using
#               formulas.
#   - stringr:  Simple, Consistent Wrappers for Common String Operations
#   - reshape2: A package which makes it easy to transform data between wide 
#               and long formats
#   - sas7bdat: SAS Database Reader (experimental)
#   - XLConnect:Provides comprehensive functionality to read, write and format 
#               Excel data
#   - htmlTable:Tables with state-of-the-art layout elements such as row 
#               spanners, column spanners, table spanners, zebra striping, and 
#               more for Markdown/HTML
#   - gridExtra:Provides a number of user-level functions to work with "grid" 
#               graphics, notably to arrange multiple grid-based plots on a 
#               page, and draw tables.

source('r_scripts/use_package.R')
use_package('shiny')
use_package('shinyjs')
use_package('shinyBS')
use_package('ggplot2')
use_package('grid')
use_package('gridExtra')
use_package('scales')
use_package('colourpicker')
use_package('DT')
use_package('rtf')
use_package('dplyr')
use_package('lazyeval')
use_package('tidyr')
use_package('tidyselect')
install_('reshape2')
install_('stringr')
install_('haven')
install_('readxl')
install_('XLConnect')
install_('htmlTable')
install_('gridExtra')
install_('corrplot')
install_('survival')
install_('ggrepel')
install_('qpcR')
install_('gtools')
# install_('psychometric')
source('r_scripts/common_statistics.R')
source('r_scripts/strings.R')
source('r_scripts/function_utils.R')
source('r_scripts/plot_utils.R')
source('r_scripts/table_utils.R')
source('r_scripts/format_utils.R')
source('r_scripts/tnf_utils.R')
source('r_scripts/stats_utils.R')
source('r_scripts/geom_utils.R')
source('r_scripts/shiny_related.R')
source('r_scripts/time_profiling_ggplot.R')
source('r_scripts/mean_se_plot.R')
source('r_scripts/median_iqr_plot.R')
source('r_scripts/spaghetti_plot.R')
source('r_scripts/boxplot_time.R')
source('r_scripts/time_profiling_multibmk.R')
source('r_scripts/boxplot_biomarker.R')
source('r_scripts/scatter_plot.R')
source('r_scripts/scatter_matrix.R')
source('r_scripts/circle_matrix.R')
source('r_scripts/histogram_plot.R')
source('r_scripts/gg_scatter.R')
source('r_scripts/gg_boxplot.R')
source('r_scripts/contingency_table.R')
source('r_scripts/output_time_profiling.R')
source('r_scripts/kmplot.R')
source('r_scripts/output_association.R')
source('r_scripts/RTF2/RTF_functions.R')
source('r_scripts/RTF2/RTF_misc.R')


#-----------------------------------------------
# Define defaults
#-----------------------------------------------

continuity_threshold <- 5L
string_yes <- c('Yes', 'Y', 'yes', 'y', 'TRUE', 'T', 'True', 'true', 't')

study_col <- 'STUDYID'
cohort_col <- 'COHORT'
subj_col <- 'USUBJID'
param_col <- 'PARAM'
aval_col <- 'AVAL'
base_col <- 'BASE'
chg_col <- 'CHG'
pchg_col <- 'PCHG'
prchg_col <- 'PRCHG'
xvar_col <- 'XTICKER'
xlabel_col <- 'XTICKERL'
bmk_unit_col <- 'UNIT'
pfs_time_col <- 'PFSTIME'
pfs_event_col <- 'PFSEVENT'
os_time_col <- 'OSTIME'
os_event_col <- 'OSEVENT'

required_cols <- c(
    study_col, subj_col, param_col, aval_col, base_col,
    chg_col, pchg_col, prchg_col, xvar_col, xlabel_col
)

bmk_value_name_dict <- list(
    'AVAL' = '', 'BASE' = 'Baseline', 'CHG' = 'Change from Baseline',
    'PCHG' = '% Baseline',
    'PRCHG' = 'Proportion of Baseline'
)

# footnote_append <- paste(
#     '\nThe app has been validated by',
#     paste0('Source: ...\\path\\to\\folder\\Biomarker Explorer, ',
#            format(Sys.time(), "%d%b%Y:%H:%M")),
#     sep = '\n'
# )

default_footnote_lines <- function() {
    paste(
        paste('Date:', format(Sys.time(), format = '%d%b%Y %H:%M')),
        'Produced by Biomarker Explorer Version 1',
        sep = '\n'
    )
}


# study_col <- 'STUDYID'
# subj_col <- 'SUBJID'
# bmk_name_col <- 'PARAM'
# bmk_val_col <- 'AVAL'
# bmk_base_col <- 'BASE'
# time_unit_col <- 'AVISIT'
# time_num_col <- 'AVISITN'
# bmk_unit_col <- 'UNIT'
# 
# to_exclude <- c(subj_col, bmk_name_col, bmk_val_col, bmk_base_col,
#                 time_unit_col, time_num_col)


#-----------------------------------------------
# Import data
#-----------------------------------------------

# accepted file format list
accepted_file_format <- c(
    'text/csv',
    'text/comma-separated-values',
    'text/tab-separated-values',
    'text/plain',
    '.csv',
    '.tsv',
    '.RData',
    '.sas7bdat',
    '.xls',
    '.xlsx'
)
template_file <- 'input_data_template.xlsx'
user_manual <- 'Rshiny_Biomarker_Explorer_User_Manual_V0.4.pdf'
user_survey <- 'Survey_Biomarker_Explorer.docx'

# separator
separator_choices <- c(comma = ',', semicolon = ';', tab = '\t')
separator_default <- ','

# quote
quote_choices <- c(None = '', 'Double Quote' = '"', 'Single Quote' = "'")
quote_default <- '"'

# note message
message <- tags$div(
    tags$p('Please note that the following file types are permitted:'),
    tags$ul(
        tags$li('RData'),
        tags$li('txt'),
        tags$li('csv / tsv'),
        tags$li('xlsx / xls'),
        tags$li('sas7bdat')
    )
)

# number of rows of 'Variable' and 'Value' input to perform data subsetting
file_subset_num_cond <- 10L


#-----------------------------------------------
# Time profiling
#-----------------------------------------------
time_y_list <- c(aval_col, base_col, chg_col, pchg_col, prchg_col)
time_output_list <- c('Boxplot', 'Mean + SE plot', 'Mean + SD plot',
                     'Median + IQR plot', 'Spaghetti plot', 'Summary table')
time_graph_list <- c('Boxplot', 'Mean + SE plot', 'Mean + SD plot',
                     'Median + IQR plot', 'Spaghetti plot')
time_ngroups_threshold <- 20L
time_download_plot_formats <- c('pdf', 'png', 'jpg', 'ps')
time_download_table_formats <- c('csv', 'rtf')

time_plot_height_default <- 8
time_plot_width_default <- 12

time_xlab_default <- 'Enter X-axis label...'
time_ylab_default <- 'Enter Y-axis label...'
time_plot_main_default <- 'Enter plot title...'
time_plot_footnote_default <- ''

time_jitter_factor <- 0.5


#-----------------------------------------------
# Association
#-----------------------------------------------

study_col <- 'STUDYID'
ass_type_list <- c(
    'Histogram', 'Scatter plot', 'Box plot', 'Correlation matrix',
    'Contingency table'
)
ass_y_list <- c(aval_col, base_col, chg_col, pchg_col, prchg_col)
ass_add_line_types <- c('Linear regression line', 'Loess curve', 'Identity line')
ass_scatter_correlation_types <- c('Spearman', 'Pearson')
ass_box_tests <- c(wilcox = 'Wilcox rank sum test',
                   t = 'Two sample t test',
                   kruskal = 'Kruskal Wallis test',
                   anova = 'ANOVA')
ass_box_statistical_test <- c('Parametric', 'Non-parametric')
ass_table_statistical_test <- c('Chi-square', 'Fisher')
ass_table_percentage_types <- c('None' = '', 'Row', 'Column')

table_padding <- paste(c('padding-top: 10px', 'padding-right: 15px',
                         'padding-bottom: 5px', 'padding-left: 15px'),
                       sep = '', collapse = ';')
table_style <- paste('margin-bottom: 5px',
                     'font-size: 130%',
                     spe = ';')

color_alpha <- 0.6
group_level_color_prefix <- TRUE

ass_download_plot_formats <- c('pdf', 'png', 'jpg', 'ps')
ass_download_table_formats <- c('pdf', 'html')

ass_plot_height_default <- 4
ass_plot_width_default <- 6

ass_table_height_default <- 4
ass_table_width_default <- 7



#-----------------------------------------------
# Survival analysis page
#-----------------------------------------------

surv_bmk_value_list <- time_y_list
surv_download_plot_formats <- c('pdf', 'png', 'jpg', 'ps')
surv_download_table_formats <- c('csv', 'rtf')

surv_plot_height_default <- 8
surv_plot_width_default <- 12

#-----------------------------------------------
# Output results
#-----------------------------------------------

# accepted file format list
out_accepted_file_format <- c('.xls', '.xlsx')
out_template_file <- 'output_data_template.xls'

# sheet names
sheet_names <- list(time = 'Time Profiling', ass = 'Association', fn = 'fn')

# column names in the output data sheet for time-profiling graph
out_time_cols <- c(
    'FileKey', 'TFLT', 'TFLN', 'OutputType', 'Study', 'Cohort', 'Biomarker',
    'YVariable', 'LogY', 'Group', 'GroupLevels', 'FacetRow', 'FacetRowLevels',
    'FacetColumn', 'FacetColumnLevels', 'Xmin', 'Xmax', 'Ymin', 'Ymax',
    'PlotFormat', 'PlotHeight', 'PlotWidth', 'PlotResolution', 'XTickVar',
    'TableTitle', 'TableFootnote', 'TableDecimal', 'TableColumnTotals',
    'TableColumnTotalNames', 'TableChangeFromBaseline', 'PlotTitle', 
    'PlotXLabel', 'PlotYLabel', 'PlotFootnote', 'XTickAngle', 'ReferenceLine',
    'IncludeSampleSize', 'IncludePoints', 'IncludeSubjectID', 'Colors',
    'LineTypes', 'SubsetExpression'
)

# column names in the output data sheet for association graph/table
out_ass_cols <- c(
    'FileKey', 'TFLT', 'TFLN', 'OutputType', 'Study', 'Cohort', 'BiomarkerX',
    'VariableX', 'LevelsX', 'VisitsX', 'LogX', 'BiomarkerY', 'VariableY',
    'LevelsY', 'VisitsY', 'LogY', 'Group', 'GroupLevels', 'FacetRow',
    'FacetRowLevels', 'FacetColumn', 'FacetColumnLevels', 'CorrelationMatrixType',
    'CorrelationMatrixPlot', 'StatisticalTest', 'TablePercentage', 'TableTitle',
    'TableFootnote', 'PlotFormat', 'PlotHeight', 'PlotWidth', 'PlotResolution',
    'PlotTitle', 'PlotXLabel', 'PlotYLabel', 'PlotFootnote', 'HistogramGeom',
    'IncludeSampleSize', 'IncludeSubjectID', 'IncludePoints',
    'IncludeConfidenceInterval', 'AddLines', 'SubsetExpression'
)

# column names in the output data sheet for footnote
fn_cols_start <- 'FN'
out_fn_num_col <- 'FNN'
out_fn_text_col <- 'FNText'
out_fn_cols <- c(out_fn_num_col, out_fn_text_col)

out_table_convert <- TRUE

out_tnf_names <- list(
    'Time Profiling' = out_time_cols,
    'Association' = out_ass_cols,
    'fn' = out_fn_cols
)


    































