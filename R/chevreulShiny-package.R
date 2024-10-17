#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom alabaster.base saveObject
#' @importFrom alabaster.base readObject
#' @importFrom chevreuldata human_gene_transcript_sce
#' @importFrom clustree clustree
#' @importFrom ComplexHeatmap draw
#' @importFrom ComplexHeatmap Heatmap
#' @importFrom ComplexHeatmap HeatmapAnnotation
#' @importFrom DataEditR dataEditServer
#' @importFrom DataEditR dataEditUI
#' @importFrom DataEditR dataFilterServer
#' @importFrom DataEditR dataFilterUI
#' @importFrom DataEditR dataOutputServer
#' @importFrom DataEditR dataOutputUI
#' @importFrom DataEditR dataSelectServer
#' @importFrom DataEditR dataSelectUI
#' @importFrom DBI dbAppendTable
#' @importFrom DBI dbConnect
#' @importFrom DBI dbDisconnect
#' @importFrom DBI dbReadTable
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom DT datatable
#' @importFrom DT DTOutput
#' @importFrom DT formatStyle
#' @importFrom DT renderDT
#' @importFrom EnhancedVolcano EnhancedVolcano
#' @importFrom fs dir_ls
#' @importFrom fs path
#' @importFrom fs path_dir
#' @importFrom fs path_file
#' @importFrom future plan
#' @importFrom ggplotify as.ggplot
#' @importFrom ggpubr theme_pubr
#' @importFrom glue glue
#' @importFrom grDevices dev.off 
#' @importFrom grDevices pdf
#' @importFrom methods is
#' @importFrom plotly event_data
#' @importFrom plotly ggplotly
#' @importFrom plotly partial_bundle
#' @importFrom plotly plotlyOutput
#' @importFrom plotly renderPlotly
#' @importFrom plotly style
#' @importFrom plotly toWebGL
#' @importFrom purrr flatten_chr
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom rappdirs user_cache_dir
#' @importFrom readr read_csv
#' @importFrom RSQLite SQLite
#' @importFrom S4Vectors metadata
#' @importFrom scran findMarkers
#' @importFrom shiny incProgress
#' @importFrom shiny selectInput
#' @importFrom shiny withProgress
#' @importFrom shinydashboard box
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboard dashboardHeader
#' @importFrom shinydashboard dashboardSidebar
#' @importFrom shinydashboard menuItem
#' @importFrom shinydashboard sidebarMenu
#' @importFrom shinydashboard tabItem
#' @importFrom shinydashboard tabItems
#' @importFrom shinyFiles getVolumes
#' @importFrom shinyFiles parseFilePaths
#' @importFrom shinyFiles parseSavePath
#' @importFrom shinyFiles shinyFileChoose
#' @importFrom shinyFiles shinyFileSave
#' @importFrom shinyFiles shinyFilesButton
#' @importFrom shinyFiles shinySaveButton
#' @importFrom shinyhelper helper
#' @importFrom shinyhelper observe_helpers
#' @importFrom shinyjs alert
#' @importFrom shinyjs hidden
#' @importFrom shinyjs html
#' @importFrom shinyjs runcodeServer
#' @importFrom shinyjs runcodeUI
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets prettyRadioButtons
#' @importFrom stats runif
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_subset
#' @importFrom stringr str_to_title
#' @importFrom tibble column_to_rownames
#' @importFrom tibble deframe
#' @importFrom tibble enframe
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr unite
#' @importFrom utils capture.output
#' @importFrom utils data
#' @importFrom utils head
#' @importFrom utils packageVersion
#' @importFrom utils sessionInfo
#' @importFrom waiter spin_loaders
#' @importFrom waiter transparent
#' @importFrom waiter use_waiter
#' @importFrom waiter Waiter
## usethis namespace: end
#' @import ggplot2
#' @import SingleCellExperiment
#' @import SummarizedExperiment
#' @import shiny
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import chevreulProcess
#' @import chevreulPlot
NULL

