#' @export
sqOptimiseVar <- function(sqconsole, project, runitem, manag, variety, parameters, ..., observations) {
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
  if (length(manag) > 1) {
    for (i in 1:length(manag)) {
      if (i == 1)
        xpath <- paste0("//ManagementItem[text()!='", manag[i],"'")
      else
        xpath <- paste0(xpath," and text()!='", manag[i],"'")
    }
    xpath <- paste0(xpath,"]/..")
  } else
    xpath <- paste0("//ManagementItem[text()!='", manag,"']/..")
  xpathApply(sqrun.tree, xpath,removeNodes)
  #Variety
  xpathApply(sqrun.tree,paste0("//MultiRunItem/VarietyItem[text()!='", variety,"']/.."), removeNodes)
  #Save temoprary Run file
  saveXML(sqrun.tree, file = paste0(proj.wd,"/","tmp.sqrun"))

  # Remove unused varieties from varieties parameters
  sqvar.tree <- xmlParse(paste0(proj.wd,"/", paths$VarietyFileName))
  xpathApply(sqvar.tree,paste0("//CropParameterItem[@name!='", variety,"']"), removeNodes)
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

  if (observations %in% c("daysto_anth", "fln")) type <- "sqmat"
  else if (observations %in% c("eln")) type <- "sqoln"
  else stop("Usupported observation")

  obs.file <-
    sqgetObsFile("tmp.sqpro", runitem, type)

  obs <- getObservations(obs.file, type)

  if (!observations %in% colnames(obs)) stop(paste0("Observations not present in file: ", observations))

  r <-
    mco::nsga2(fn = sqrunOpt,
               idim = length(parameters),
               odim = length(observations),
               parameters = parameters,
               observations = observations,
               obs.data = obs,
               sqconsole = sqconsole,
               proj.wd = proj.wd,
               ...)
  res <- r$par %>% as_data_frame()
  res <- bind_cols(res,as_data_frame(r$value))
  res <- bind_cols(res,as_data_frame(r$pareto.optimal))
  colnames(res) <- c(parameters, "RMSE", "Optimal")

  res %>%
    filter(Optimal == T) %>%
    print()

  return(res)
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
    if (observations %in% c("daysto_anth", "fln")) sim <- getSimulation(out.file, type = "sum")
    else if (observations %in% c("eln")) sim <- getSimulation(dirname(out.file), type = "daily")
    else stop("Unsupported observation")

    r <-
      sim %>%
      dplyr::left_join(obs.data, by = c("manag", "variety")) %>%
      dplyr::select(dplyr::starts_with(observations)) %>%
      dplyr::mutate_all(as.numeric)

    error <- r[,1] - r[,2]

    fitness <- sqrt(mean(error^2))

    print(fitness)
    return(fitness)

}

#' @export
runSQ <- function(sqconsole, project) {
  system2(command = sqconsole,
          args = paste(project, "/OutputSingleRun"),
          wait = T)
}
