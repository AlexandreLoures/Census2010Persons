#' Label microdata variables 2010 Census persons
#'
#' @description This function will label the microdata variables refering to
#' available information about the individuals of each of the households
#' available in the 2010 Census.
#'
#' @import dplyr httr magrittr projmgr readr readxl timeDate utils
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom magrittr %$%
#'
#' @param data_census An object of the 2010 Census persons microdata read with
#' \code{read_censo} function.
#' @param dictionary.file The dictionary file for selected survey available on
#' official website: \url{https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_Gerais_da_Amostra/Microdados/}
#'
#' @export

census_labeller <- function (data_census, dictionary.file) {
  if (sum (class (data_census) == "tbl_df") > 0 ) {
    dictionary <- suppressMessages (readxl::read_excel (dictionary.file))
    X_3 = X_6 = X_7 = NULL
    colnames (dictionary) <- paste0 ("X_", 1:dim (dictionary)[2])
    dictionary %<>% subset (!is.na (X_6))
    codcurrent <- dictionary$X_3
    for (i in 1:dim (dictionary)[1]) {
      if (is.na (dictionary$X_3[i])) {
        dictionary$X_3[i] <- codcurrent
      }
      else {
        codcurrent <- dictionary$X_3[i]
      }
    }
    notlabel <- c ("V0001", "V0002", "V0011", "V0300", "V0010",
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
                   "M0605", "M6462", "M6472", "V1005")
    vars <- names (data_census)
    varsc <- vars[sapply (data_census, class) == "character"]
    varsf <- setdiff (varsc, notlabel)
    for (i in 1:length (varsf)) {
      if (i > 0 & varsf[i] %in% (dictionary$X_3)) {
        data_census[varsf[i]] <- factor (suppressWarnings (as.numeric (unlist (data_census[varsf[i]]))),
                                        levels = suppressWarnings (as.numeric (unlist (dictionary %>% subset (X_3 == varsf[i]) %>% select (X_6)))),
                                                                   labels = unlist (dictionary %>% subset (X_3 == varsf[i]) %>% select (X_7)))
      }
    }
  }
  else {
    message ("The microdata object is not of the tibble class or sample design was already defined for microdata, so labeling categorical variables
             is not possible.")
  }
  return (data_census)
}
