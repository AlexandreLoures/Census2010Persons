#' Read microdata persons 2010 Census
#'
#' @description This function will read the microdata refering to the
#' information available on the individuals of each of the households
#' available in the 2010 Census.
#'
#' @import dplyr httr magrittr projmgr readr readxl timeDate utils
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom magrittr %$%
#'
#' @param microdata A text file containing microdata from 2010 Census persons
#' survey, available on official website: \url{https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_Gerais_da_Amostra/Microdados/}
#' @param input_txt A text file, related to the microdata from 2010 Census
#' persons, containing the input script for SAS, available on the official
#' website: \url{https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_Gerais_da_Amostra/Microdados/}
#' @param vars Vector of variable names to be kept for analysis. Default is to
#' keep all variables.
#'
#' @export

read_census <- function (microdata, input_txt, vars = NULL) {
  X1 = X2 = X3 = start = end = NULL
  input <- suppressWarnings (suppressMessages ({readr::read_table (input_txt, col_names = FALSE) %>% subset (substr (X1, 1, 1) == "@") %>%
      dplyr::mutate (type = ifelse (substr (X3, 1, 1) == "$", "c", "d"), start = as.numeric (gsub ("@", "", X1)), X3 = as.integer (chartr ("$", " ", X3)),
                     end = start + X3 - 1)}))
  if (!is.null (vars)) {
    if (any (!(vars %in% input$X2))) {
      missvar <- vars [!(vars %in% input$X2)]
      message (paste ("Variables", paste (missvar , collapse = ", "), "not present in microdata.\n"))
    }
    input %<>% subset (X2 %in% c ("V0001", "V0002", "V0011", "V0300", "V0010",
                                  "V1001", "V1002", "V1003", "V1004", "V1006",
                                  "V0502", "V0504", "V0601", "V6033", "V6036",
                                  "V6037", "V6040", "V0606", "V0613", "V0614",
                                  "V0615", "V0616", "V0617", "V0618", "V0619",
                                  "V0620", "V0621", "V0622", "V6222", "V6224",
                                  "V0623", "V0624", "V0625", "V6252", "V6254",
                                  "V6256", "V0626", "V6262", "V6264", "V6266",
                                  "V0627", "V0628", "V0629", "V0630", "V0631",
                                  "V0632", "V0633", "V0634", "V0635", "V6400",
                                  "V6352", "V6354", "V6356", "V0636", "V6362",
                                  "V6364", "V6366", "V0637", "V0638", "V0639",
                                  "V0640", "V0641", "V0642", "V0643", "V0644",
                                  "V0645", "V6461", "V6471", "V0648", "V0649",
                                  "V0650", "V0651", "V6511", "V6513", "V6514",
                                  "V0652", "V6521", "V6524", "V6525", "V6526",
                                  "V6527", "V6528", "V6529", "V6530", "V6531",
                                  "V6532", "V0653", "V0654", "V0655", "V0656",
                                  "V0657", "V0658", "V0659", "V6591", "V0660",
                                  "V6602", "V6604", "V6606", "V0661", "V0662",
                                  "V0663", "V6631", "V6632", "V6633", "V0664",
                                  "V6641", "V6642", "V6643", "V0665", "V6660",
                                  "V6664", "V0667", "V0668", "V6681", "V6682",
                                  "V0669", "V6691", "V6692", "V6693", "V6800",
                                  "V0670", "V0671", "V6900", "V6910", "V6920",
                                  "V6930", "V6940", "V6121", "V0604", "V0605",
                                  "V5020", "V5060", "V5070", "V5080", "V6462",
                                  "V6472", "V5110", "V5120", "V5030", "V5040",
                                  "V5090", "V5100", "V5130", "M0502", "M0601",
                                  "M6033", "M0606", "M0613", "M0614", "M0615",
                                  "M0616", "M0617", "M0618", "M0619", "M0620",
                                  "M0621", "M0622", "M6222", "M6224", "M0623",
                                  "M0624", "M0625", "M6252", "M6254", "M6256",
                                  "M0626", "M6262", "M6264", "M6266", "M0627",
                                  "M0628", "M0629", "M0630", "M0631", "M0632",
                                  "M0633", "M0634", "M0635", "M6352", "M6354",
                                  "M6356", "M0636", "M6362", "M6364", "M6366",
                                  "M0637", "M0638", "M0639", "M0640", "M0641",
                                  "M0642", "M0643", "M0644", "M0645", "M6461",
                                  "M6471", "M0648", "M0649", "M0650", "M0651",
                                  "M6511", "M0652", "M6521", "M0653", "M0654",
                                  "M0655", "M0656", "M0657", "M0658", "M0659",
                                  "M6591", "M0660", "M6602", "M6604", "M6606",
                                  "M0661", "M0662", "M0663", "M6631", "M6632",
                                  "M6633", "M0664", "M6641", "M6642", "M6643",
                                  "M0665", "M6660", "M0667", "M0668", "M6681",
                                  "M6682", "M0669", "M6691", "M6692", "M6693",
                                  "M0670", "M0671", "M6800", "M6121", "M0604",
                                  "M0605", "M6462", "M6472", "V1005", vars))
  }
  columns <- input %$% readr::fwf_positions (start, end, X2)
  data_census <- suppressWarnings (readr::read_fwf (microdata, columns, col_types = paste0 (input$type, collapse = "")))
  return (data_census)
}
