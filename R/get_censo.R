get_censo <- function (state, savedir = tempdir ()) {

  if (!dir.exists (savedir)) {
    savedir <- tempdir ()
    message (paste0 ("The directory provided does not exist, so the directory was set to '", savedir), "'.")
  }

  if (substr (savedir, nchar (savedir), nchar (savedir)) == "/" | substr (savedir, nchar (savedir), nchar (savedir)) == "\\") {
    savedir <- substr (savedir, 1, nchar (savedir) - 1)
  }

  ftpdata <- paste0 ("https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_Gerais_da_Amostra/Microdados/", state, ".zip")
  if (!projmgr::check_internet ()) {
    message ("The internet connection is unavailable.")
    return (NULL)
  }
  if (httr::http_error (httr::GET (ftpdata, httr::timeout (60)))) {
    message ("The microdata server is unavailable.")
    return (NULL)
  }
  options (timeout = max (300, getOption ("timeout")))
  datastate <- ftpdata
  dataname <- state
  if (length (dataname) == 0) {
    message ("Data unavailable for state")
    return (NULL)
  }
  else if (length (dataname) > 1) {
    message ("There is more than one file available for the requested microdata, please contact the package maintainer.")
    return (NULL)
  }
  else {
    dataname <- paste0 (dataname, ".zip")
  }
  utils::download.file (url = ftpdata, destfile = paste0 (savedir, "/", dataname), mode = "wb")
  if (suppressWarnings (class (try (utils::unzip (zipfile = paste0 (savedir, "/", dataname), exdir = savedir), silent = TRUE)) == "try-error")) {
    message ("The directory defined to save the download data is denied permission to overwrite the existing files, please clear or change this directory.")
    return (NULL)
  }
  utils::unzip (zipfile = paste0 (savedir, "/", dataname), exdir = savedir)
  docfiles <- unlist (strsplit (unlist (strsplit (unlist (strsplit (gsub ("\r\n", "\n", RCurl::getURL (ftpdata), dirlistonly = TRUE)), "\n")), "<a href=[[:punct:]]")), ".zip")
  utils::download.file (url = "https://raw.githubusercontent.com/AlexandreLoures/Censo2010Persons/main/auxiliary/input.zip", destfile = paste0 (savedir, "input.zip"), mode = "wb")
  utils::unzip (zipfile = paste0 (savedir, "/input.zip"), exdir = savedir)
  microdataname <- dir (savedir, pattern = paste0 ("^Amostra_Pessoas_", 12, ".*\\.txt$"), ignore.case = FALSE)
  microdatafile <- paste0 (savedir, "/", microdataname)
  microdatafile <- rownames (file.info (microdatafile)[order(file.info (microdatafile)$ctime),])[length (microdatafile)]
  inputname <- dir (savedir, pattern = ("^input.*\\.txt$"), ignore.case = FALSE)
  inputfile <- paste0 (savedir, "/", inputname)
  inputfile <- rownames (file.info (inputfile)[order (file.info (inputfile)$ctime),])[length (inputfile)]
  data_censo <- Censo2010Persons::read_censo (microdata = microdatafile, input_txt = inputfile, vars = vars)
}
