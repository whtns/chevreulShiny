
#' Load Bigwigs
#'
#' Load a tibble of bigwig file paths by cell id
#'
#' @param object A object
#' @param bigwig_db Sqlite database of bigwig files
#'
#' @return a vector of bigwigs file paths
load_bigwigs <- function(object, bigwig_db = "~/.cache/chevreul/bw-files.db") {
    con <- dbConnect(SQLite(), dbname = bigwig_db)

    bigwigfiles <- dbReadTable(con, "bigwigfiles") |>
        filter(sample_id %in% colnames(object)) |>
        identity()

    missing_bigwigs <- colnames(object)[!(colnames(object) %in% 
                                              bigwigfiles$sample_id)] |>
        paste(collapse = ", ")

    warning(paste0("Sample coverage files ", missing_bigwigs, 
                   "(.bw) do not match samples in object (check file names)"))

    dbDisconnect(con)

    bigwigfiles <-
        bigwigfiles |>
        filter(sample_id %in% colnames(object))

    return(bigwigfiles)
}
