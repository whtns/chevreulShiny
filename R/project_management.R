#' Create a Table of single Cell Projects
#'
#' Uses a list of projects to create a matrix of single cell projects
#'
#' @param proj_list List of projects
#'
#' @return a tibble of single cell projects
create_proj_matrix <- function(proj_list) {
    proj_list <- unlist(proj_list)

    proj_tbl <- tibble(project_path = proj_list, project_name = path_file(proj_list))

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


#' Subset by new metadata
#'
#' Subset the object using new metadata
#'
#' @param meta_path Path to new metadata
#' @param object A object
#'
#' @return a SingleCellExperiment object
#'
subset_by_meta <- function(meta_path, object) {
    upload_meta <- read_csv(meta_path, col_names = "sample_id") |>
        filter(!is.na(sample_id) & !sample_id == "sample_id") |>
        mutate(name = sample_id) |>
        column_to_rownames("sample_id") |>
        identity()

    upload_cells <- rownames(upload_meta)

    object <- object[, colnames(object) %in% upload_cells]

    colData(object) <- merge(colData(object), upload_meta, by = "row.names")

    return(object)
}




#' Load SingleCellExperiment Files from a single project path
#'
#' @param proj_dir project directory
#' @param ... extra args passed to load_object_path
#'
#' @return a SingleCellExperiment object
load_object_from_proj <- function(proj_dir, ...) {
    object_file <- load_object_path(proj_dir, ...)
    object_file <- readRDS(object_file)
}
