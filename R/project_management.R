#' Create a Table of single Cell Projects
#'
#' Uses a list of projects to create a matrix of single cell projects
#'
#' @param proj_list List of projects
#'
#' @return a tibble of single cell projects
create_proj_matrix <- function(proj_list) {
    proj_list <- unlist(proj_list)

    sp_cols <- c("date", "user", "note", "species")
    
    proj_matrix <-
        proj_list |> 
        tibble::enframe("project_name", "project_path") |> 
        tidyr::separate(project_name, sp_cols, sep = "-", remove = FALSE) |> 
        mutate(date = path_file(date)) |>
        dplyr::select(everything(), project_name) |> 
        identity()

    primary_projects <-
        proj_matrix |>
        filter(!grepl("integrated_projects", project_path)) |>
        filter(str_count(project_name, "_") == 1) |>
        identity()

    integrated_projects <-
        proj_matrix |>
        anti_join(primary_projects) |>
        identity()

    proj_matrices <- list(primary_projects = primary_projects, integrated_projects = integrated_projects)

    return(proj_matrices)
}

#' Subset by new colData
#'
#' Subset the object using new colData
#'
#' @param colData_path Path to new colData
#' @param object A object
#'
#' @return a SingleCellExperiment object
#'
subset_by_colData <- function(colData_path, object) {
    upload_colData <- read_csv(colData_path, col_names = "sample_id") |>
        filter(!is.na(sample_id) & !sample_id == "sample_id") |>
        mutate(name = sample_id) |>
        column_to_rownames("sample_id") |>
        identity()

    upload_cells <- rownames(upload_colData)

    object <- object[, colnames(object) %in% upload_cells]

    colData(object) <- merge(colData(object), upload_colData, by = "row.names")

    return(object)
}


#' Read in Gene and Transcript SingleCellExperiment Objects
#'
#' @param proj_dir path to project directory
#' @param prefix default "unfiltered"
#'
#' @return a SingleCellExperiment object
load_alabaster_path <- function(proj_dir = getwd(), prefix = "unfiltered") {
    alabaster_regex <- paste0(paste0(".*/", prefix, "_alabaster"))

    alabaster_path <- path(proj_dir, "output", "singlecellexperiment") |>
        dir_ls(regexp = alabaster_regex)

    if (!length(alabaster_path) == 0) {
        return(alabaster_path)
    }

    stop(alabaster_path, " does not exist in current working directory ", getwd(), ".",
        call. = FALSE
    )
}

#' Load SingleCellExperiment Files from a single project path
#'
#' @param proj_dir project directory
#' @param ... extra args passed to load_alabaster_path
#'
#' @return a SingleCellExperiment object
load_alabaster_from_proj <- function(proj_dir, ...) {
    alabaster_file <- load_alabaster_path(proj_dir, ...)
    alabaster_file <- readObject(alabaster_file)
}
