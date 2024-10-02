#' Regress SingleCellExperiment Object by Given Set of Genes
#'
#' @param object A object
#'
#' @return a SingleCellExperiment object with features regressed
regress_cell_cycle <- function(object) {
    message("regressing objects by cell cycle")

    if (dim(object)[2] < 100) {
        ctrl <- dim(object)[2] / 10
    }
    if (!"Phase" %in% colnames(colData(object))) {
        object <- annotate_cell_cycle(object)
    }
    if (!any(str_detect(c(mainExpName(object), altExpNames(object)), pattern = ".*regress.*"))) {
        dec.nocycle <- modelGeneVar(object, block = colData(object)[["Phase"]])
        reg.nocycle <- regressBatches(object, batch = colData(object)[["Phase"]])

        reg.nocycle <- runPCA(reg.nocycle,
            exprs_values = "corrected",
            subset_row = getTopHVGs(dec.nocycle, prop = 0.1)
        )

        original_experiment <- mainExpName(object)

        altExp(object, glue("{original_experiment}_regressed")) <- reg.nocycle
        # browser()
        object <- swapAltExp(object, as.character(glue("{original_experiment}_regressed")))

        reductions <- reducedDimNames(object)
        resolutions <- str_extract(colnames(colData(object))[grepl(glue("{original_experiment}_snn_res."), colnames(colData(object)))], "[0-9].*$")
        object <- runTSNE(x = object, dimred = "PCA", n_dimred = seq(30))
        object <- runUMAP(x = object, dimred = "PCA", n_dimred = seq(30))
        object <- object_cluster(object, resolution = resolutions)
    }
    return(object)
}





#' Annotate Cell Cycle
#'
#' Annotate Cell Cycle for Gene and Transcript SingleCellExperiment Objects
#'
#' @param object A SingleCellExperiment object
#'
#' @return a SingleCellExperiment object
annotate_cell_cycle <- function(object) {
    
    data_env <- new.env(parent = emptyenv())
    data("cc.genes.cyclone", envir = data_env, package = "chevreul")
    cc.genes.cyclone <- data_env[["cc.genes.cyclone"]]
    
    assignments <- cyclone(object, cc.genes.cyclone, 
                           gene.names = rownames(object))
    colData(object)[colnames(assignments$scores)] <- assignments$scores
    colData(object)["Phase"] <- assignments$phases
    return(object)
}


#' Run Louvain Clustering at Multiple Resolutions
#'
#' @param object A SingleCellExperiment objects
#' @param resolution Clustering resolution
#' @param custom_clust custom cluster
#' @param reduction Set dimensional reduction object
#' @param algorithm 1
#' @param ... extra args passed to single cell packages
#'
#' @return a SingleCellExperiment object with louvain clusters
object_cluster <- function(object = object, resolution = 0.6, 
                           custom_clust = NULL, reduction = "PCA", 
                           algorithm = 1, ...) {
    message(glue("[{format(Sys.time(), '%H:%M:%S')}] Clustering Cells..."))
    if (length(resolution) > 1) {
        for (i in resolution) {
            message(glue("clustering at {i} resolution"))
            cluster_labels <- 
                clusterCells(object,
                             use.dimred = reduction,
                             BLUSPARAM = NNGraphParam(cluster.fun = "louvain", 
                                                      cluster.args = 
                                                          list(resolution = i))
                )
            colData(object)[[glue("gene_snn_res.{i}")]] <- cluster_labels
        }
    } else if (length(resolution) == 1) {
        message(glue("clustering at {resolution} resolution"))
        cluster_labels <- clusterCells(object,
                                       use.dimred = reduction,
                                       BLUSPARAM = NNGraphParam(
                                           cluster.fun = "louvain", 
                                           cluster.args = 
                                               list(resolution = resolution))
        )
        
        
        colData(object)[[glue("gene_snn_res.{resolution}")]] <- cluster_labels
    }
    
    return(object)
}