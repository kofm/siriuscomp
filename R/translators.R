#' @export
sqtranslate_colnames <- function(df) {

  dic <-
    list(# grain_num = "Grain.number..grain.m..",
    anthesis = "Anthesis.date..ZC65.",
    anthesis = "ZC65_Anthesis",
    cropdm_mat = "Maturity.crop.dry.mass",
    cropdm_mat = "Crop.DM.at.maturity..kgDM.ha.",
    daysto_anth = "Anthesis.day.after.sowing..ZC65.",
    doy = "Day.of.year",
    eln = "Emerged.leaf.number..leaf.mainstem.",
    eln = "Emerged.leaf.number",
    emergence = "ZC10_Emergence",
    endfill_date = "End.of.grain.filling..ZC91.",
    fln = "Final.leaf.number",
    graindm_mat = "Grain.DM.at.maturity..kgDM.ha.",
    grainnum = "Grain.number",
    harvind = "DM.harvest.index",
    harvind = "DM.harvest.index..dimensionless.",
    manag = "Management",
    maturity = "Maturity.date..ZC92.",
    maturity = "ZC92_Maturity",
    shoots = "Maturity.shoot.number",
    site = "Site",
    soil = "Soil",
    sow = "Sowing.date",
    sow = "Sowing.date..ZC00.",
    stem = "Anthesis.stem.length",
    tsw = "Maturity.single.grain.dry.mass",
    tsw = "Single.grain.DM.at.maturity..mgDM.grain.",
    variety = "Varietal.parameters",
    variety = "Variety",
    yield = "Maturity.grain.yield")

  df %>%
    dplyr::rename_(.dots = dic[which(dic %in% colnames(df))]) %>%
    return()
}
