
#' Prep Slider Values
#'
#' @param default_val Provide a default value
#'
#' @noRd
prep_slider_values <- function(default_val) {
    min <- round(default_val * 0.25, digits = 1)
    max <- round(default_val * 2.0, digits = 1)
    step <- 10^((ceiling(log10(default_val))) - 1)
    value <- default_val
    return(list(min = min, max = max, value = value, step = step))
}


#' Create a shiny app for a project on disk
#'
#' @param preset_project A preloaded project to start the app with
#' @param appTitle A title of the App
#' @param organism_type human or mouse
#' @param futureMb amount of Mb allocated to future package
#' @param db_name sqlite database with list of saved
#' SingleCellExperiment objects
#' @return a shiny app
chevreulApp <-
  function(preset_project,
           appTitle = "chevreul",
           organism_type = "human",
           futureMb = 13000,
           db_name = "single-cell-projects.db") {

    db_path <- file.path(user_cache_dir(appname="chevreul"), db_name)

    message(packageVersion("chevreul"))
    plan(strategy = "multicore", workers = 6)
    future_size <- futureMb * 1024^2
    options(future.globals.maxSize = future_size)
    options(shiny.maxRequestSize = 40 * 1024^2)
    options(DT.options = list(
        pageLength = 2000, paging = FALSE,
        info = TRUE, searching = TRUE, autoWidth = FALSE, ordering = TRUE,
        scrollX = TRUE, language = list(search = "Filter:")
    ))
    header <- dashboardHeader(title = appTitle)
    sidebar <- dashboardSidebar(
        uiOutput("projInput"),
        actionButton("loadProject", "Load Selected Project") |>
            default_helper(type = "markdown", content = "overview"),
        textOutput("appTitle"),
        bookmarkButton(),
        prettyRadioButtons("organism_type",
            inline = TRUE,
            "Organism", choices = c("human", "mouse"), selected = organism_type
        ),
        shinyFilesButton("objectUpload", "Load a SingleCellExperiment Dataset",
            "Please select a .rds file",
            multiple = FALSE
        ),
        shinySaveButton("saveSCE", "Save Current Dataset",
            "Save file as...",
            filetype = list(rds = "rds")
        ),
        verbatimTextOutput("savefile"),
        sidebarMenu(
            menuItem("Integrate Projects",
                tabName = "integrateProjects", icon = icon("object-group")
            ), menuItem("Reformat Metadata",
                tabName = "reformatMetadataDR", icon = icon("columns")
            ), menuItem("Plot Data",
                tabName = "comparePlots", icon = icon("chart-bar"),
                selected = TRUE
            ), menuItem("Heatmap/Violin Plots",
                tabName = "violinPlots", icon = icon("sort")
            ), menuItem("Coverage Plots",
                tabName = "coveragePlots", icon = icon("mountain")
            ), menuItem("Differential Expression",
                tabName = "diffex", icon = icon("magnet")
                # ), menuItem("Pathway Enrichment Analysis",
                #   tabName = "pathwayEnrichment", icon = icon("sitemap")
            ), menuItem("Find Markers",
                tabName = "findMarkers", icon = icon("bullhorn")
            ), menuItem("Subset SingleCellExperiment Input",
                tabName = "subsetSingleCellExperiment", icon = icon("filter")
            ), menuItem("All Transcripts",
                tabName = "allTranscripts", icon = icon("sliders-h")
            ), menuItem("RNA Velocity",
                tabName = "plotVelocity", icon = icon("tachometer-alt")
            ), menuItem("Regress Features",
                tabName = "regressFeatures", icon = icon("eraser")
            ), menuItem("Technical Information",
                tabName = "techInfo", icon = icon("cogs")
            )
        ),
        actionButton("changeEmbedAction",
            label = "Change Embedding Parameters"
        ),
        changeEmbedParamsui("changeembed"),
        width = 250
    )
    body <- dashboardBody(
        use_waiter(),
        tabItems(
            tabItem(
                tabName = "comparePlots",
                h2("Compare Plots") |>
                    default_helper(type = "markdown", 
                                   content = "comparePlots"),
                plotDimRedui("plotdimred1"),
                plotDimRedui("plotdimred2"),
                plotReadCountui("plotreadcount1"),
                plotReadCountui("plotreadcount2"),
                chevreulBox(
                    title = "Selected Cells",
                    tableSelectedui("tableselected"),
                    width = 6
                ),
                plotClustree_UI("clustreePlot")
            ),
            tabItem(
                tabName = "violinPlots",
                fluidRow(
                    plotHeatmapui("heatMap")
                ),
                fluidRow(
                    plotViolinui("violinPlot")
                )
            ),
            tabItem(
                tabName = "coveragePlots",
                fluidRow(
                    plotCoverage_UI("coverageplots")
                )
            ),
            tabItem(
                tabName = "integrateProjects",
                fluidRow(
                    integrateProjui("integrateproj"),
                    width = 12
                )
            ),
            tabItem(
                tabName = "reformatMetadataDR",
                fluidRow(
                    reformatMetadataDRui("reformatMetadataDR")
                )
            ),
            tabItem(
                tabName = "subsetSingleCellExperiment",
                h2("Subset SingleCellExperiment Input") |>
                    default_helper(type = "markdown",
                                   content = "subsetSingleCellExperiment"),
                plotDimRedui("subset"),
                chevreulBox(
                    title = "Subset Settings",
                    checkboxInput("legacySettingsSubset", 
                                  "Use Legacy Settings",
                                  value = FALSE),
                    actionButton("subsetAction",
                                 "subset object by selected cells"),
                    actionButton("subsetCsv", "subset object by uploaded csv"),
                    fileInput("uploadCsv",
                        "Upload .csv file with cells to include",
                        accept = c(".csv")
                    ),
                    useShinyjs(),
                    # runcodeUI(code = "alert('Hello!')"),
                    textOutput("subsetMessages"),
                    width = 6
                ),
                chevreulBox(
                    title = "Selected Cells", tableSelectedui("subset"),
                    width = 6
                )
            ), tabItem(
                tabName = "findMarkers",
                h2("Find Markers"),
                chevreulMarkersui("findmarkers"),
                plotDimRedui("markerScatter")
            ), tabItem(
                tabName = "allTranscripts",
                h2("All Transcripts"),
                plotDimRedui("alltranscripts2"),
                allTranscriptsui("alltranscripts1")
            ),
            tabItem(
                tabName = "diffex",
                h2("Differential Expression") |>
                    default_helper(type = "markdown", content = "diffex"),
                plotDimRedui("diffex"),
                diffexui("diffex")
            ),
            # tabItem(
            #   tabName = "pathwayEnrichment",
            #   h2("Pathway Enrichment"),
            #   fluidRow(
            #     pathwayEnrichmentui("pathwayEnrichment")
            #   )
            # ),
            tabItem(
                tabName = "regressFeatures",
                fluidRow(
                    chevreulBox(
                        title = "Regress Features",
                        actionButton(
                            "regressAction",
                            "Regress SingleCellExperiment Objects By Genes"
                        ),
                        width = 12
                    ) |>
                        default_helper(type = "markdown",
                                       content = "regressFeatures")
                )
            ), tabItem(
                tabName = "techInfo",
                h2("Technical Information"),
                h3(paste0("chevreul version: ", packageVersion("chevreul"))),
                techInfoui("techInfo")
            )
        )
    )

    ui <- function(request) {
        ui <- dashboardPage(
            header = header, sidebar = sidebar,
            body = body
        )
    }
    server <- function(input, output, session) {
        # runcodeServer()
        # observe({
        #   list_of_inputs <- reactiveValuesToList(input)
        #   list_of_inputs <<- reactiveValuesToList(input)
        #   print(list_of_inputs)
        #
        #   retained_inputs <- c("setProject")
        #
        #   list_of_inputs[!list_of_inputs %in% retained_inputs]
        # })
        # setBookmarkExclude(names(list_of_inputs))

        onBookmark(function(state) {
            state$values$uploadSCEPath <- uploadSCEPath()
        })

        onRestore(function(state) {
            uploadSCEPath(state$values$uploadSCEPath[[1]])
        })

        w <- Waiter$new()

        observe_helpers(
            help_dir = system.file("helpers", package = "chevreulShiny"))
        options(warn = -1)

        con <- reactive({
            dbConnect(
                SQLite(),
                db_path
            )
        })

        projList <- reactivePoll(4000, session, checkFunc = function() {
            if (file.exists(db_path)) {
                dbReadTable(con(), "projects_tbl") |>
                    deframe()
            } else {
                ""
            }
        }, valueFunc = function() {
            dbReadTable(con(), "projects_tbl") |>
                deframe()
        })

        output$projInput <- renderUI({
            selectizeInput("setProject", "Select Project to Load",
                choices = projList(), selected = preset_project,
                multiple = FALSE
            )
        })

        proj_matrices <- reactive({
            create_proj_matrix(projList())
        })

        object <- reactiveVal()
        proj_dir <- reactiveVal()
        uploadSCEPath <- reactiveVal()

        if (!is.null(preset_project)) {
            proj_dir(preset_project)
        }
        organism_type <- reactive({
            input$organism_type
        })
        plot_types <- reactive({
            list_plot_types(object())
        })
        observeEvent(input$loadProject, {
            proj_dir(input$setProject)
        })
        output$appTitle <- renderText({
            req(proj_dir())
            paste0("Loaded Project: ", path_file(proj_dir()))
        })
        dataset_volumes <- reactive({
            message(proj_dir())
            dataset_volumes <- c(
                Home = path(
                    proj_dir(),
                    "output", "singlecellexperiment"
                ), "R Installation" = R.home(),
                getVolumes()()
            )
        })
        observe({
            req(dataset_volumes())
            shinyFileChoose(input, "objectUpload",
                roots = dataset_volumes(), session = session
            )
        })
        observeEvent(input$objectUpload, {
            req(dataset_volumes())

            file <- parseFilePaths(
                dataset_volumes(),
                input$objectUpload
            )
            message(file)
            uploadSCEPath(file$datapath)
        })

        observe({
            req(uploadSCEPath())
            message("uploaded")
            withProgress(
                message = paste0("Uploading Data"),
                value = 0,
                {
                    incProgress(2 / 10)
                    message(uploadSCEPath())
                    updated_sce <- readRDS(uploadSCEPath())
                    object(updated_sce)
                    incProgress(6 / 10)

                    organism <- case_when(
                        str_detect(uploadSCEPath(), "Hs") ~ "human",
                        str_detect(uploadSCEPath(), "Mm") ~ "mouse"
                    )

                    message(uploadSCEPath())
                    message(names(object))
                    incProgress(8 / 10)
                }
            )
        })


        featureType <- reactive({
            "gene"
        })

        observe({
            req(dataset_volumes())
            shinyFileSave(input, "saveSCE",
                # roots = dataset_volumes(),
                roots = c(Home = path(
                    proj_dir(),
                    "output", "singlecellexperiment"
                )),
                session = session, restrictions = system.file(package = "base")
            )
        })
        subSingleCellExperimentPath <- eventReactive(input$saveSCE, {
            req(object())
            savefile <- parseSavePath(
                dataset_volumes(),
                input$saveSCE
            )
            return(savefile$datapath)
        })
        observeEvent(input$saveSCE, {
            req(object())
            req(subSingleCellExperimentPath())

            withProgress(
                message = paste0("Saving Data"),
                value = 0,
                {
                    Sys.sleep(6)
                    incProgress(2 / 10)
                    saveRDS(
                        object(),
                        subSingleCellExperimentPath()
                    )
                    incProgress(10 / 10)
                }
            )
        })

        integrationResults <- integrateProj( "integrateproj",
            proj_matrices, object, proj_dir, con()
        )
        newprojList <- reactive({
            req(integrationResults())
            integration_path <- paste0(integrationResults())
            proj_dir(integration_path)

        names(integration_path) <- path_file(integration_path)

            newprojList <- c(projList(), integration_path)
        })
        observe({
            # print(newprojList())
            updateSelectizeInput(session, "setProject",
                label = "Select input label",
                choices = newprojList(),
                server = TRUE
            )
        })

        observe({
            reformatted_sce <- reformatMetadataDR(
                                             "reformatMetadataDR", object,
                                             featureType)
            object(reformatted_sce())
        })

        reductions <- reactive({
            req(object())
            reducedDimNames(object())
        })

        observe({
            req(object())

            plotDimRed( "plotdimred1", object, plot_types,
                       featureType,
                organism_type = organism_type, reductions
            )
            plotDimRed( "plotdimred2", object, plot_types,
                       featureType,
                organism_type = organism_type, reductions
            )
            plotDimRed( "diffex", object, plot_types, featureType,
                organism_type = organism_type, reductions
            )
            plotDimRed( "subset", object, plot_types, featureType,
                organism_type = organism_type, reductions
            )
            plotDimRed( "markerScatter", object, plot_types,
                       featureType,
                organism_type = organism_type, reductions
            )
        })

        plotReadCount( "plotreadcount1", object, plot_types)
        plotReadCount( "plotreadcount2", object, plot_types)
        plotViolin( "violinPlot", object, featureType,
            organism_type
        )
        plotHeatmap( "heatMap", object, featureType,
            organism_type
        )

        plotCoverage( "coverageplots", object, plot_types, proj_dir,
            organism_type
        )
        plotClustree( "clustreePlot", object)
        tableSelected( "tableselected", object)
        diffex_selected_cells <- tableSelected( "diffex",
            object
        )

        pathwayEnrichment( "pathwayEnrichment", object, featureType)

        cell_subset <- tableSelected( "subset",
            object
        )
        observeEvent(input$subsetAction, {
            req(cell_subset())
            withCallingHandlers(
                {
                    html("subsetMessages", "")
                    message("Beginning")

                    subset_sce <-
                      object()[, colnames(object()) %in% cell_subset()]
                    object(subset_sce)
                    if (length(unique(object()[["batch"]])) > 1) {
                        message("reintegrating gene expression")
                        reintegrated_sce <- reintegrate_sce(object(),
                            resolution = seq(0.2, 1, by = 0.2),
                            legacy_settings = input$legacySettingsSubset,
                            organism = metadata(object())$experiment$organism
                        )
                        object(reintegrated_sce)
                    } else {
                        subset_sce <- sce_process(
                          object(),
                          resolution = seq(0.2, 1, by = 0.2),
                          legacy_settings = input$legacySettingsSubset)
                        object(subset_sce)
                    }
                    message("Complete!")
                },
                message = function(m) {
                    html(id = "subsetMessages", html = paste0(
                        "Subsetting SingleCellExperiment Object: ",
                        m$message
                    ), add = FALSE)
                }
            )
        })
        observeEvent(input$subsetCsv, {
            req(input$subsetCsv)
            req(input$uploadCsv)
            withCallingHandlers(
                {
                    html("subsetMessages", "")
                    message("Beginning")
                    subset_sce <- subset_by_meta(
                        input$uploadCsv$datapath,
                        object()
                    )
                    object(subset_sce)
                    if (length(unique(object()[["batch"]])) > 1) {
                        message("reintegrating gene expression")
                        reintegrated_sce <- reintegrate_sce(object(),
                            resolution = seq(0.2, 1, by = 0.2),
                            legacy_settings = input$legacySettingsSubset,
                            organism = metadata(object())$experiment$organism
                        )
                        object(reintegrated_sce)
                    } else {
                        subset_sce <- sce_process(object(),
                            resolution = seq(0.2,
                                2,
                                by = 0.2
                            ),
                            legacy_settings = input$legacySettingsSubset
                        )
                        object(subset_sce)
                    }
                    message("Complete!")
                },
                message = function(m) {
                    html(id = "subsetMessages", html = paste0(
                        "Subsetting SingleCellExperiment Object: ",
                        m$message
                    ), add = FALSE)
                }
            )
        })
        observeEvent(input$changeEmbedAction, {
            showModal(modalDialog(
                title = "Recalculating Embedding",
                "This process may take a minute or two!"
            ))
            object <- changeEmbedParams( "changeembed",
                object
            )
            removeModal()
        })
        chevreulMarkers( "findmarkers", object, plot_types,
                   featureType)
        diffex_results <- diffex( "diffex", object, featureType,
            diffex_selected_cells
        )

        observe({
            req(featureType())
            req(object())
            allTranscripts( "alltranscripts1", object, featureType,
                organism_type
            )

            plotDimRed( "alltranscripts2", object, plot_types,
                       featureType,
                organism_type = organism_type, reductions
            )
        })

        observeEvent(input$regressAction, {
            req(object())
            showModal(modalDialog(
                title = "Regressing cycle effects",
                "This process may take a minute or two!"
            ))
            regressed_sce <- regress_cell_cycle(object())
            object(regressed_sce)
            removeModal()
        })

        observe({
            req(uploadSCEPath())
            req(object())

            proj_path <- str_replace(uploadSCEPath(), "output.*", "")

            proj_name <- path_file(proj_path)
            message(proj_name)
        })

        techInfo( "techInfo", object)

        sessionId <- as.integer(runif(1, 1, 100000))
        output$sessionId <- renderText(paste0("Session id: ", sessionId))
        session$onSessionEnded(function() {
            cat(paste0("Ended: ", sessionId))
            observe(dbConnect(con()))
        })
    }

    # onStop(function() dbDisconnect(con))
    shinyApp(ui, server, enableBookmarking = "url")
}
