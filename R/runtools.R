#' Optimise one genotype using the specified project/management(s) against one or more observations.
#'
#' @param sqconsole Path to SiriusQuality-Console.exe.
#' @param project Path to Project file (can be relative).
#' @param runitem Character string identifying the chosen RunItem.
#' @param mng Character vector including all the chosen Management(s).
#' @param var Character string identifying the chosen Variety.
#' @param parameters Character string indicating the parameters to be optimized.
#' @param obsitem Character string identifying the chosen Observations Item.
#' @param observation Character vector including all the chosen Observation(s) to optimise against.
#' @return A data frame containing all the optimised combinations with value(s) of the fitness function(s)
#' @export
sqOptimiseVar <- function(sqconsole, project, runitem, mng, var, parameters, ..., obsitem, observations) {

  # List of Observation Files with corresponding type. TODO * Should be a package constant/option? *
  obs.data <- list(PhenologyObservationFile = "sqmat",
                   HaunIndexObservationFile = "sqoln")

  # Get project path
  proj.wd <- normalizePath(dirname(project))

  # Get project file tree
  sqpro.tree <- xmlInternalTreeParse(project)

  # Get project files paths
  paths <-
    xmlToList(sqpro.tree) %>%
    .$Inputs

  #### Remove unused runitem/management(s)/variety from run file ###
  # Needed to speed up run time

  # Get the XML tree of Run File (.sqrun)
  sqrun.tree <- xmlParse(paste0(proj.wd,"/",paths$RunFileName))

  # Remove unused RunItems
  xpathApply(sqrun.tree, paste0("//RunItem[@name!='", runitem,"']"),removeNodes)

  # Remove unused Managements
  if (length(mng) > 1) {
    for (i in 1:length(mng)) {
      if (i == 1)
        xpath <- paste0("//ManagementItem[text()!='", mng[i],"'")
      else
        xpath <- paste0(xpath," and text()!='", mng[i],"'")
    }
    xpath <- paste0(xpath,"]/..")
  } else
    xpath <- paste0("//ManagementItem[text()!='", mng,"']/..")
  xpathApply(sqrun.tree, xpath,removeNodes)

  # Remove unused varieties
  xpathApply(sqrun.tree,paste0("//MultiRunItem/VarietyItem[text()!='", var,"']/.."), removeNodes)

  # Save the modified XML tree as temporary Run File (tmp.sqrun)
  saveXML(sqrun.tree, file = paste0(proj.wd,"/","tmp.sqrun"))

  ### Create a temporary .sqvar file ###
  # Retrieve the XML tree from Variety File (.sqvar)
  sqvar.tree <- xmlParse(paste0(proj.wd,"/", paths$VarietyFileName))
  xpathApply(sqvar.tree,paste0("//CropParameterItem[@name!='", var,"']"), removeNodes)
  saveXML(sqvar.tree, file = paste0(proj.wd,"/","tmp.sqvar"))

  # Edit Project File to reference the temporary Run File (tmp.sqrun)
  xpathSApply(sqpro.tree,
              "//RunFileName",
              `xmlValue<-`,
              value = "tmp.sqrun")

  # Edit Project File to reference the temporary Variety File (tmp.sqvar)
  xpathSApply(sqpro.tree,
              "//VarietyFileName",
              `xmlValue<-`,
              value = "tmp.sqvar")

  # Save edited XML node in a temporary Project File (tmp.sqpro)
  saveXML(sqpro.tree, file = paste0(proj.wd,"/","tmp.sqpro"))

  # Set a state variable for next loop
  # first_item <- FALSE
  #
  # for (observation in observations) {
  #
  #   if (observation %in% c("daysto_anth", "fln")) {
  #     type <- "sqmat" } else if (observations %in% c("eln")) {
  #       type <- "sqoln" } else {
  #         stop("Usupported observati on")
  #       }
  #
  #   obs.file <-
  #     sqgetObsFile(paste0(proj.wd,"/","tmp.sqpro"), obsitem, type)
  #
  #   obs <-
  #     getObservations(obs.file, type) %>% dplyr::filter(variety == var & manag %in% mng) %>%
  #     dplyr::select_("manag", "site", "sow", observation)
  #
  #   if (!observation %in% colnames(obs)) stop(paste0("Observations not present in file: ", observation))
  #
  #   if (!first_item) {
  #     obs.data <- obs
  #   } else {
  #     obs.data <- obs.data %>% dplyr::left_join(obs)
  #   }
  #
  #   first_item <- TRUE
  # }

  ### Loading all the observation data files ###

  # Get the path to the .sqobs Observation File
  obs.file <-
    paths$ObservationFileName %>%
    gsub('\\\\', '/', .) %>%
    tools::file_path_as_absolute()

  # Create a list of the available Observation Data Files, for the desidered Observation Item
  list <- xmlInternalTreeParse(obs.file) %>%
    xpathApply(., paste0("//ObservationItem[@name='", obsitem,"']"), xmlToList) %>%
    .[[1]]

  # Remove unused items
  list <- list[grep("*File", names(list))]
  list[list == "?"] <- NULL

  # For each Observation Data File load the data in R objects named obs.sqmat, obs.sqoln, etc.
  for (file in names(list)) {
    assign(paste0("obs.",obs.data[[file]]),
           getObservations(gsub('\\\\', '/',list[file]), type = obs.data[file]))
  }

  observed <- list()

  for (obs in observations) {

    o <-
      locateObservationData(obs) %>%
      get() %>%
      dplyr::filter(variety == var & manag %in% mng) %>%
      dplyr::arrange(sow) %>%
      dplyr::pull(obs) %>%
      as.numeric

    observed[[obs]] <- o
  }

  r <-
    mco::nsga2(fn = sqrunOpt,
               idim = length(parameters),
               odim = length(observations),
               parameters = parameters,
               observations = observations,
               obs.data = observed,
               sqconsole = sqconsole,
               proj.wd = proj.wd,
               ...)
  # res <- r$par %>% dplyr::as_data_frame()
  # res <- dplyr::bind_cols(res,dplyr::as_data_frame(r$value))
  # res <- dplyr::bind_cols(res,dplyr::as_data_frame(r$pareto.optimal))
  # colnames(res) <- c(parameters, "RMSE", "Optimal")
  #
  # res %>%
  #   dplyr::filter(Optimal == T) %>%
  #   print()
  #
  # return(res)
  return(r)
}

#' @export
sqrunOpt <- function(values, parameters, observations, obs.data, sqconsole, proj.wd) {

    # Check that number of supplied values are the same of target parameters
    if (length(values) != length(parameters)) {
      stop("Input dimensions different from supplied parameters.")
    }

    # Get output file
    sqrun.tree <- xmlInternalTreeParse(paste0(proj.wd,"/tmp.sqrun"))
    out.file <- normalizePath(gsub('\\\\', '/', paste0(xpathApply(sqrun.tree, "//Multi/OutputDirectory",xmlValue),"/",
                                                     xpathApply(sqrun.tree, "//Multi/OutputPattern",xmlValue),".sqbrs")))

    ### Edit tmp.sqvar file with new parameter(s) value(s) ###

    # Load tmp.sqvar
    sqvar.tree <- xmlParse(paste0(proj.wd,"/tmp.sqvar"))

    # Loop over parameters and change values for the selected variety
    for (i in 1:length(parameters)) {
      sqeditParam(node = sqvar.tree, parameter = parameters[i], value = values[i])
    }

    # Save edited tree in tmp.sqvar
    saveXML(sqvar.tree,
            file = paste0(proj.wd,"/tmp.sqvar"))

    # Run SiriusQuality with tmp.sqpro
    runSQ(sqconsole, paste0(proj.wd, "/tmp.sqpro"))

    # Get simulation results
    predicted <- list()
    fitness <- vector()

    for (obs in observations) {
      p <-
        getSimulation(out.file, type = "seasonal") %>%
        # dplyr::filter(variety == var & manag %in% mng) %>% - Not needed (tmp.sqrun)
        dplyr::arrange(sow) %>%
        dplyr::pull(obs) %>%
        as.numeric


      if (length(p) != length(obs.data[[obs]])) {
        stop("Not all the specified management are present in the observation file")
      }

      error <- obs.data[[obs]] - p
      rmse <- sqrt(mean(error^2))
      fitness <- append(fitness, rmse)
    }
    print(fitness)
    return(fitness)
}

#' @export
runSQ <- function(sqconsole, project) {
  system2(command = sqconsole,
          args = paste(project, "/OutputSingleRun"),
          wait = T)
}
