read_censo <- function (microdata, input_txt, vars = NULL) {
  X1 = X2 = X3 = start = end = NULL
  input <- suppressWarnings (suppressMessages ({readr::read_table (input_txt, col_names = FALSE) %>% subset (substr (X1, 1, 1) == "@") %>%
      dplyr::mutate (type = ifelse (substr (X3, 1, 1) == "$", "c", "d"), start = as.numeric (gsub ("@", "", X1)), X3 = as.integer (chartr ("$", " ", X3)),
                     end = start + X3 - 1)}))
  if (!is.null (vars)) {
    if (any (!(vars %in% input$X2))) {
      missvar <- vars [!(vars %in% input$X2)]
      message (paste ("Variables", paste (missvar , collapse = ", "), "not present in microdata.\n"))
    }
    input %<>% subset (X2 %in% vars)
  }
  columns <- input %$% readr::fwf_positions (start, end, X2)
  data_censo <- suppressWarnings (readr::read_fwf (microdata, columns, col_types = paste0 (input$type, collapse = "")))
  data_censo <- dplyr::mutate (data_censo, ID_DOMICILIO = paste0 ())
  return (data_censo)
}