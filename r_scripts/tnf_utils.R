#-----------------------------------------------------------------------------
# Purpose:  Function to create/update the TNF file
# Author:   Feiyang Niu
# Date:     July 8, 2016
#-----------------------------------------------------------------------------


# load necessary scripts and packages
install_('XLConnect')


tnf_create <- function(tnf_name, tnf_dir, name_list) {
    if(is.null(tnf_dir)) tnf_dir <- '.'
    tnf_path <- paste(tnf_dir, tnf_name, sep = '\\')
    if(file.exists(tnf_path)) file.remove(tnf_path)
    # Load workbook; create if not existing
    wb <- XLConnect::loadWorkbook(tnf_path, create = TRUE)
    # write to the workbook
    for(sheet in names(name_list)) {
        XLConnect::createSheet(wb, name = sheet)
        XLConnect::writeWorksheet(wb, rbind(name_list[[sheet]]), sheet, header = F)
    }
    # Save workbook
    XLConnect::saveWorkbook(wb)
}


tnf_update <- function(tnf_name, tnf_dir, content_df, sheet_name, start_row) {
    if(is.null(tnf_dir)) tnf_dir <- '.'
    tnf_path <- paste(tnf_dir, tnf_name, sep = '\\')
    # Load workbook; create if not existing
    wb <- XLConnect::loadWorkbook(tnf_path, create = FALSE)
    # write to the workbook
    XLConnect::writeWorksheet(wb, content_df, sheet = sheet_name,
                              startRow = start_row, header = FALSE)
    # Save workbook
    XLConnect::saveWorkbook(wb)
}





























