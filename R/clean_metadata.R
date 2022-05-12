#' Clean aru metadata
#'
#' @param type BarLT or SM4
#' @param folder_base Folder base directory
#' @param included_subfolders Defaults to 'all', which means all subfolders included.
#'        To include only some folders (at first level only),
#'        provide a vector with folder names to include.
#' @param gps_locations GPS locations in lat/lon format with SiteID
#'                       Must also include timezone (tz) and deploy date.
#'
#' @return
#' @export
clean_metadata <- function(type, folder_base,included_subfolders = 'all',gps_locations = NULL, ... ){
  list2env(list(...), envir = environment())
  if(!exists("file_ext")) file_ext <- ".wav"
  if(!exists("file_split_pattern")) file_split_pattern <- "T|\\-|\\_|\\."
  list_files <- list.files(folder_base, recursive = T, full.names = F, include.dirs = F)

  if(included_subfolders!="all") {
    list_folders[list_folders %in% included_subfolders]
     list_files <- purrr::map(glue::glue("^{included_subfolders}/"),
                                        ~list_files[grepl(.x, list_files)]) |>
       unlist()
     if(length(list_files)==0){
       abort(c("You have listed subfolders to include, but no files are returned",
               "x" = "Folders must be case-sensitive and be only at one level below folder_base",
               "i" = "Check folder structure using list.dirs(folder_base, full.names = F, recursive = F)")
       )
     }
  }
  list_waves <- list_files[grep(file_ext, list_files)]
  if(length(list_waves)==0){
    abort(c("Did not find any wav files. Default is set to '.wav'",
            "x" = "Sound file locations needed to process.",
            "i" = "Use file_ext to change file extension for sound files and check folder_base is correct."))
    }
  # if(!exists("folder_sep")) {
  #   if(sum(grepl(list_waves, pattern = "Data"))>0) {folder_sep <- "Data"
  #   } else if(sum(grepl(list_waves, pattern = "Wav"))>0) {folder_sep <- "Wav"
  #   } else abort("Tried 'Wav' and 'Data' as folder separators. Please use your own using folder_sep argument")
  # }
  # |- BarLT   ----------------
  if(grepl("bar", type, ignore.case = T) & grepl("lt", type, ignore.case = T)){

    log_files <- list_files[grep("logfile", list_files)]
    log_data <- purrr::map(glue::glue("{folder_base}/{log_files}"), read_log_barlt) |>
      purrr::transpose()

    if(!exists("site_in_filename")) site_in_filename <- FALSE

    if(!exists("deploy_start_date")) deploy_start_date <-
      list_waves |>
      stringr::str_extract("\\d{8}") |>
      lubridate::ymd() %>% { min(.) -2 } |>
      format("%d/%m/%Y") |> lubridate::dmy()
    if(!exists("check_dists")) check_dists <- TRUE
    if(!exists("site_pattern")) site_pattern <-  "[P|Q]\\d+_\\d"


    if(is_null(gps_locations)){
      gps_locations <- process_gps_barlt(base_folder = folder_base,
                      file_list= list_files,
                      deploy_start_date = deploy_start_date,
                      check_dists)
    }


  }
  # |- SM4   ----------------
  if(grepl("sm", type, ignore.case = T) & grepl("4", type, ignore.case = T)){
    if(!exists("site_pattern")) site_pattern <-  "S4A\\d{5}"
    # file.location <- "//WRIV02DTSTDNT1/RecordStor20172019/BetweenRivers_2019"

      # list.files(folder_base, "_Summary.txt", recursive = T, full.names = T)



    if(is_null(gps_locations)){
      gps_locations <- process_gps_SM(summaries)
    }
    if(!exists("site_in_filename")) site_in_filename <- TRUE
  }

    wav_names_log <- parse_datetimes(list_waves,filename_separator = file_split_pattern,
                                           site_in_filename = site_in_filename,
                                           site_pattern =site_pattern,
                                           tz_loc = unique(gps_locations$tz ))

    rec_log_ss <- prep_sunrise_sunset(gps_locations = gps_locations,
                                            wav_names_log = wav_names_log)


  if(!type %in% c("BarLT","SM4")){
    simpleError("Currently only BarLT and SM4 ARUs are supported. Will add more as needed.")
  }


}
