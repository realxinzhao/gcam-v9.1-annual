# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_en_Fert_xml
#'
#' Construct XML data structure for \code{en_Fert.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_Fert.xml}. The corresponding file in the
#' original data system was \code{batch_en_Fert_xml.R} (energy XML).
module_energy_en_Fert_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L2322.Supplysector_Fert",
      "L2322.SectorUseTrialMarket_tra",
      "L2322.FinalEnergyKeyword_Fert",
      "L2322.SubsectorLogit_Fert",
      "L2322.SubsectorShrwtFllt_Fert",
      "L2322.SubsectorInterp_Fert",
      "L2322.StubTech_Fert",
      "L2322.GlobalTechShrwt_Fert",
      "L2322.TechShrwt_TradedFert",
      "L2322.GlobalTechCoef_Fert",
      "L2322.TechCoef_TradedFert",
      "L2322.StubTechMarket_FertImports",
      "L2322.GlobalTechCost_Fert",
      "L2322.GlobalTechCapture_Fert",
      "L2322.GlobalTechSCurve_Fert",
      "L2322.GlobalTechProfitShutdown_Fert",
      "L2322.StubTechProd_FertProd",
      "L2322.StubTechCoef_Fert",
      "L2322.Production_FertExport",
      "L2322.StubTechProd_FertImport",
      "L2322.StubTechProd_FertDomCons",
      "L2322.StubTechProd_NtoAg",
      "L281.TechAccountOutput_entrade",
      "L281.GlobalTechAccountInput_entrade")

  MODULE_OUTPUTS <-
    c("en_Fert.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = MODULE_OUTPUTS))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    L281.TechAccountOutput_entrade <- L281.TechAccountOutput_entrade %>%
      filter(supplysector %in% unique(L2322.TechCoef_TradedFert$supplysector))
    L281.GlobalTechAccountInput_entrade <- L281.GlobalTechAccountInput_entrade %>%
      filter(sector.name %in% unique(L2322.StubTechMarket_FertImports$supplysector))


    # Produce outputs
    create_xml("en_Fert.xml") %>%
      add_logit_tables_xml(L2322.Supplysector_Fert, "Supplysector") %>%
      add_xml_data(L2322.SectorUseTrialMarket_tra, "SectorUseTrialMarket") %>%
      add_xml_data(L2322.FinalEnergyKeyword_Fert, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2322.SubsectorLogit_Fert, "SubsectorLogit") %>%
      add_xml_data(L2322.SubsectorShrwtFllt_Fert, "SubsectorShrwtFllt") %>%
      add_xml_data(L2322.SubsectorInterp_Fert, "SubsectorInterp") %>%
      add_xml_data(L2322.StubTech_Fert, "StubTech") %>%
      add_xml_data(L2322.TechShrwt_TradedFert, "TechShrwt") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L281.GlobalTechAccountInput_entrade, "GlobalTechAccountInput") %>%
      add_xml_data(L2322.GlobalTechCoef_Fert, "GlobalTechCoef") %>%
      add_xml_data(L2322.GlobalTechShrwt_Fert, "GlobalTechShrwt") %>%
      add_xml_data(L2322.TechCoef_TradedFert, "TechCoef") %>%
      add_xml_data(L2322.GlobalTechCost_Fert, "GlobalTechCost") %>%
      add_xml_data(L2322.GlobalTechCapture_Fert, "GlobalTechCapture") %>%
      add_xml_data(L2322.GlobalTechSCurve_Fert, "GlobalTechSCurve") %>%
      add_xml_data(L2322.GlobalTechProfitShutdown_Fert, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2322.StubTechProd_FertProd, "StubTechProd") %>%
      add_xml_data(L2322.StubTechCoef_Fert, "StubTechCoef") %>%
      add_xml_data(L2322.StubTechMarket_FertImports, "StubTechMarket") %>%
      add_xml_data(L281.TechAccountOutput_entrade, "TechAccountOutput") %>%
      add_xml_data(L2322.Production_FertExport, "Production") %>%
      add_xml_data(L2322.StubTechProd_FertImport, "StubTechProd") %>%
      add_xml_data(L2322.StubTechProd_FertDomCons, "StubTechProd") %>%
      add_xml_data(L2322.StubTechProd_NtoAg, "StubTechProd") %>%
      add_precursors(MODULE_OUTPUTS) ->
      en_Fert.xml


    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
