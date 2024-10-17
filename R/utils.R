#' Unite metadata
#'
#' @param object A SingleCellExperiment object
#' @param group_bys A feature or variable to combine
#'
#' @return a SingleCellExperiment object with Idents formed from concatenation of group_bys
#' @export
#'
#' @examples
#' 
#' 
#' data(small_example_dataset)
#' unite_metadata(small_example_dataset, "Mutation_Status")
#'
unite_metadata <-
    function(object, group_bys) {
        newcolname <- paste(group_bys, collapse = "_by_")
        newdata <- colData(object)[group_bys] |>
            as.data.frame() |>
            unite(!!newcolname, any_of(group_bys)) |>
            deframe()
        
        return(object)
    }

#' Get Transcripts in object
#'
#' Get transcript ids in objects for one or more gene of interest
#'
#' @param object A SingleCellExperiment object
#' @param gene Gene of interest
#' @param organism Organism
#'
#' @return transcripts constituting a 
#' gene of interest in a SingleCellExperiment object
get_transcripts_from_sce <- function(object, gene, organism = "human") {
    transcripts <- genes_to_transcripts(gene, organism)

    transcripts <- transcripts[transcripts %in% get_features(object, 
                                                             "transcript")]
}


#' Create a database of chevreul projects
#'
#' Create a database containing chevreul projects
#'
#' @param cache_location Path to cache "~/.cache/chevreul"
#' @param sqlite_db Database to be created
#' @param verbose print messages
#'
#' @return a sqlite database with SingleCellExperiment objects
create_project_db <- function(cache_location = "~/.cache/chevreul", 
                              sqlite_db = "single-cell-projects.db", 
                              verbose = TRUE) {
    if (!dir.exists(cache_location)) {
        dir.create(cache_location)
    }
    con <- dbConnect(SQLite(), path(cache_location, sqlite_db))
    projects_tbl <- tibble(project_name = character(), 
                           project_path = character(), 
                           project_slug = character(), 
                           project_type = character(), )
    message(glue(
        "building table of chevreul projects at {path(cache_location, 
        sqlite_db)}"))
    tryCatch({
        dbWriteTable(con, "projects_tbl", projects_tbl)
    }, warning = function(w) {
        message(sprintf("Warning in %s: %s", deparse(w[["call"]]), 
                        w[["message"]]))
    }, error = function(e) {
        message("projects db already exists!")
    }, finally = {
    })
    dbDisconnect(con)
}

#' Update a database of chevreul projects
#'
#' Add new/update existing projects to the database by recursing fully
#'
#' @param projects_dir The project directory to be updated
#' @param cache_location Path to cache "~/.cache/chevreul"
#' @param sqlite_db sqlite db
#' @param verbose print messages
#'
#' @return a sqlite database with SingleCellExperiment objects
update_project_db <- function(
        projects_dir = NULL,
        cache_location = "~/.cache/chevreul",
        sqlite_db = "single-cell-projects.db",
        verbose = TRUE) {
    if (!dir.exists(cache_location)) {
        dir.create(cache_location)
    }

    con <- dbConnect(SQLite(), path(cache_location, sqlite_db))
    
    projects_tbl <-
        dir_ls(projects_dir, glob = "*.here", recurse = TRUE, 
               fail = FALSE, all = TRUE) |>
               path_dir() 
        
        names(projects_tbl) <- path_file(projects_tbl)
        
        projects_tbl <- 
        projects_tbl |>
        enframe("project_name", "project_path") |>
        mutate(project_slug = str_remove(project_name, "_proj$")) |>
        mutate(project_type = path_file(path_dir(project_path))) |>
        identity()

    current_projects_tbl <-
        dbReadTable(con, "projects_tbl") |>
        filter(file.exists(project_path)) |>
        filter(!project_path %in% projects_tbl$project_path) |>
        bind_rows(projects_tbl) |>
        distinct(project_path, .keep_all = TRUE)

    dbWriteTable(con, "projects_tbl", projects_tbl, overwrite = TRUE)

    dbDisconnect(con)
}

#' Update a database of chevreul projects
#'
#' Append projects to database
#'
#' @param new_project_path new project path
#' @param cache_location Path to cache "~/.cache/chevreul"
#' @param sqlite_db sqlite db
#' @param verbose print messages
#' @return a sqlite database with SingleCellExperiment objects

append_to_project_db <- function(
        new_project_path,
        cache_location = "~/.cache/chevreul",
        sqlite_db = "single-cell-projects.db",
        verbose = TRUE) {
    if (!dir.exists(cache_location)) {
        dir.create(cache_location)
    }

    con <- dbConnect(SQLite(), path(cache_location, sqlite_db))

    names(new_project_path) <- path_file(new_project_path)

    projects_tbl <-
        new_project_path |>
        enframe("project_name", "project_path") |>
        mutate(project_slug = str_remove(project_name, "_proj$")) |>
        mutate(project_type = path_file(path_dir(project_path))) |>
        identity()

    current_projects_tbl <-
        dbReadTable(con, "projects_tbl") |>
        filter(file.exists(project_path)) |>
        filter(!project_path %in% projects_tbl$project_path) |>
        bind_rows(projects_tbl) |>
        distinct(project_path, .keep_all = TRUE)

    dbWriteTable(con, "projects_tbl", current_projects_tbl, overwrite = TRUE)

    dbDisconnect(con)
}

#' Read a database of chevreul projects
#'
#' Reads database of chevreul projects to a data frame
#'
#' @param cache_location Path to cache "~/.cache/chevreul"
#' @param sqlite_db sqlite db
#' @param verbose print messages
#'
#' @return a tibble with SingleCellExperiment objects
#

read_project_db <- function(
        cache_location = "~/.cache/chevreul",
        sqlite_db = "single-cell-projects.db",
        verbose = TRUE) {
    if (!dir.exists(cache_location)) {
        dir.create(cache_location)
    }

    con <- dbConnect(SQLite(), path(cache_location, sqlite_db))

    current_projects_tbl <-
        dbReadTable(con, "projects_tbl")

    dbDisconnect(con)

    return(current_projects_tbl)
}

#' Make Bigwig Database
#'
#'
#' @param new_project Project directory
#' @param cache_location Path to cache "~/.cache/chevreul"
#' @param sqlite_db sqlite db containing bw files
#'
#' @return a sqlite database of bigwig files for cells 
#' in a SingleCellExperiment object
make_bigwig_db <- function(new_project = NULL, 
                           cache_location = "~/.cache/chevreul/", 
                           sqlite_db = "bw-files.db") {
    new_bigwigfiles <- dir_ls(new_project, glob = "*.bw", recurse = TRUE)

    names(new_bigwigfiles) <- path_file(new_bigwigfiles)
        
    new_bigwigfiles <- 
    new_bigwigfiles |>
        enframe("name", "bigWig") |>
        mutate(sample_id = 
                   str_remove(name, "_Aligned.sortedByCoord.out.*bw$")) |>
        filter(!str_detect(name, "integrated")) |>
        distinct(sample_id, .keep_all = TRUE) |>
        identity()

    con <- dbConnect(SQLite(), dbname = path(cache_location, sqlite_db))

    all_bigwigfiles <-
        dbReadTable(con, "bigwigfiles") |>
        bind_rows(new_bigwigfiles)

    dbWriteTable(con, "bigwigfiles", all_bigwigfiles, overwrite = TRUE)

    return(all_bigwigfiles)
}

#' Retrieve Metadata from Batch
#'
#' @param batch batch
#' @param projects_dir path to project dir
#' @param db_path path to .db file
#'
#' @return a tibble with cell level metadata from a SingleCellExperiment object
metadata_from_batch <- function(
        batch, projects_dir = "/dataVolume/storage/single_cell_projects",
        db_path = "single-cell-projects.db") {
    mydb <- dbConnect(SQLite(), path(projects_dir, db_path))

    projects_tbl <- dbReadTable(mydb, "projects_tbl") |>
        filter(!project_type %in% c("integrated_projects", "resources"))

    dbDisconnect(mydb)

    metadata <-
        projects_tbl |>
        filter(project_slug == batch) |>
        pull(project_path) |>
        path("data") |>
        dir_ls(glob = "*.csv") |>
        identity()
}


#' Save object to <project>/output/sce/<feature>_sce.rds
#'
#' @param object a SingleCellExperiment object
#' @param prefix a prefix for saving
#' @param proj_dir path to a project directory
#'
#' @return a path to an rds file containing a SingleCellExperiment object
#'
#'
#'
save_sce <- function(object, prefix = "unfiltered", proj_dir = getwd()) {
    sce_dir <- path(proj_dir, "output", "singlecellexperiment")

    dir.create(sce_dir)

    sce_path <- path(sce_dir, paste0(prefix, "_sce.rds"))

    message(glue("saving to {sce_path}"))
    saveRDS(object, sce_path)

    return(object)
}

#' Collate list of variables to be plotted
#'
#' @param object a SingleCellExperiment object
#'
#' @return plot_types a list of category_vars or continuous_vars
#' @export
#' @examples
#' 
#' data(small_example_dataset)
#' list_plot_types(small_example_dataset)
list_plot_types <- function(object) {
    meta_types <- tibble(
        vars = colnames(colData(object)),
        var_type = map_chr(colData(object), class),
        num_levels = unlist(map(colData(object), ~ length(unique(.x))))
    )
    
    meta_types <- meta_types |>
        # filter(!grepl("_snn_res", vars)) |>
        mutate(meta_type = case_when(
            var_type %in% c("integer", "numeric") ~ "continuous",
            var_type %in% c("character", "factor", "logical") ~ "category"
        )) |>
        mutate(meta_type = ifelse(meta_type == "continuous" & num_levels < 30, "category", meta_type)) |>
        filter(num_levels > 1) |>
        identity()
    
    continuous_vars <- meta_types |>
        filter(meta_type == "continuous") |>
        pull(vars)
    
    continuous_vars <- c("feature", continuous_vars)
    
    names(continuous_vars) <- 
        str_to_title(str_replace_all(continuous_vars, "[[:punct:]]", " "))
    
    category_vars <- meta_types |>
        filter(meta_type == "category") |>
        pull(vars)
    
    names(category_vars) <-  
        str_to_title(str_replace_all(
            category_vars, 
            "[^[:alnum:][:space:]\\.]", " "))
    
    plot_types <- list(category_vars = category_vars, continuous_vars = continuous_vars)
    
    
    
    return(plot_types)
}

#' Clean Vector of Chevreul Names
#'
#' Cleans names of objects provided in a vector form
#'
#' @param myvec A vector of object names
#'
#' @return a clean vector of object names
#' @export
#' @examples
#' 
#' data(small_example_dataset)
#' make_chevreul_clean_names(colnames(
#' get_colData(small_example_dataset)))
make_chevreul_clean_names <- function(myvec) {
    names(myvec) <- 
        myvec |> 
        str_replace_all("[^[:alnum:][:space:]\\.]", " ") |> 
        str_to_title()
    
    return(myvec)
}