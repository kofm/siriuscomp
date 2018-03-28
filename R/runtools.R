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
  # Get project path
  proj.wd <- normalizePath(dirname(project))

  # Get project file tree
  sqpro.tree <- xmlInternalTreeParse(project)

  # Get project files paths
  paths <-
    xmlToList(sqpro.tree) %>%
    .$Inputs

  # Remove unused runitem/management(s)/variety from run file
  sqrun.tree <- xmlParse(paste0(proj.wd,"/",paths$RunFileName))
  # Runitem
  xpathApply(sqrun.tree, paste0("//RunItem[@name!='", runitem,"']"),removeNodes)

  # Management(s)
  sqrun.tree <- xmlParse(paste0(proj.wd,"/",paths$RunFileName))
  xpathApply(sqrun.tree, paste0("//RunItem[@name!='", runitem,"']"),removeNodes)
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
  #Variety
  xpathApply(sqrun.tree,paste0("//MultiRunItem/VarietyItem[text()!='", var,"']/.."), removeNodes)
  #Save temoprary Run file
  saveXML(sqrun.tree, file = paste0(proj.wd,"/","tmp.sqrun"))

  # Remove unused varieties from varieties parameters
  sqvar.tree <- xmlParse(paste0(proj.wd,"/", paths$VarietyFileName))
  xpathApply(sqvar.tree,paste0("//CropParameterItem[@name!='", var,"']"), removeNodes)
  saveXML(sqvar.tree, file = paste0(proj.wd,"/","tmp.sqvar"))

  # Change sqrun file with temporary file
  xpathSApply(sqpro.tree,
              "//RunFileName",
              `xmlValue<-`,
              value = "tmp.sqrun")

  # Change sqvar file with temporary file
  xpathSApply(sqpro.tree,
              "//VarietyFileName",
              `xmlValue<-`,
              value = "tmp.sqvar")

  # Save all in temoprary Project file
  saveXML(sqpro.tree, file = paste0(proj.wd,"/","tmp.sqpro"))

  first_item <- FALSE

  for (observation in observations) {

    if (observation %in% c("daysto_anth", "fln")) {
      type <- "sqmat" } else if (observations %in% c("eln")) {
        type <- "sqoln" } else {
          stop("Usupported observati on")
        }

    obs.file <-
      sqgetObsFile(paste0(proj.wd,"/","tmp.sqpro"), obsitem, type)

    obs <-
      getObservations(obs.file, type) %>% dplyr::filter(variety == var & manag %in% mng) %>%
      dplyr::select_("manag", "site", "sow", observation)

    if (!observation %in% colnames(obs)) stop(paste0("Observations not present in file: ", observation))

    if (!first_item) {
      obs.data <- obs
    } else {
      obs.data <- obs.data %>% dplyr::left_join(obs)
    }

    first_item <- TRUE
  }

  r <-
    mco::nsga2(fn = sqrunOpt,
               idim = length(parameters),
               odim = length(observations),
               parameters = parameters,
               observations = observations,
               obs.data = obs.data,
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

    # Get working directory from project file
    # proj.wd <- normalizePath(dirname(projdir))

    # Get output file
    sqrun.tree <- xmlInternalTreeParse(paste0(proj.wd,"/tmp.sqrun"))
    out.file <- normalizePath(paste0(xpathApply(sqrun.tree, "//Multi/OutputDirectory",xmlValue),"/",
                                     xpathApply(sqrun.tree, "//Multi/OutputPattern",xmlValue),".sqbrs"))

    #### Edit tmp.sqvarsu file
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
    fitness <- vector()

    for (observation in observations) {

      if (observation %in% c("daysto_anth", "fln")) sim <- getSimulation(out.file, type = "seasonal")
      else if (observations %in% c("eln")) sim <- getSimulation(dirname(out.file), type = "daily")
      else stop("Unsupported observation")


      r <-
        sim %>%
        dplyr::left_join(obs.data, by = c("manag", "sow")) %>%
        dplyr::select(dplyr::starts_with(observation)) %>%
        dplyr::mutate_all(as.numeric) %>%
        na.omit()

      error <- r[,1] - r[,2]

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
