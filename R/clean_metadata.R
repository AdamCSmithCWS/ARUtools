#' Clean aru metadata
#'
#' @param type BarLT or SM4
#' @param folder_base Folder base directory
#' @param included_subfolders Defaults to 'all', which means all subfolders included.
#'        To include only some folders (at first level only),
#'        provide a vector with folder names to include.
#' @param gps_locations GPS locations in lat/lon format with SiteID
#'                       Must also include timezone (tz) and deploy date.
#' @param file_split_pattern Regular expression to split sound file name into pieces.
#'                      May need to adjust if non-standard naming convention is used.
#' @param site_pattern  Regular expression to extract site name from folder path.
#'
#' @return
#' @export
clean_metadata <- function(type, folder_base,included_subfolders = 'all',gps_locations = NULL,
                           file_split_pattern = "T|\\-|\\_|\\.",
                           site_pattern =  "[P|Q]\\d+_\\d",
                           ... ){
  BarLT <- (grepl("bar", type, ignore.case = T) & grepl("lt", type, ignore.case = T))
  SM <- (grepl("sm", type, ignore.case = T) & grepl("3|4", type, ignore.case = T))
  if(!any(BarLT ,SM) ){
    abort("Currently only BarLT and SM4 ARUs are supported. Will add more as needed.")
  }
  list2env(list(...), envir = environment())
  if(!exists("file_ext")) file_ext <- ".wav"
  # browser()
  list_files <- list.files(folder_base, recursive = T, full.names = F, include.dirs = F)

  if(!any(grepl("all", included_subfolders)) | length(included_subfolders)>1) {
     list_files <- purrr::map(glue::glue("{included_subfolders}/"),
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
  if(BarLT){

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


    if(is_null(gps_locations)){
      if(!exists("dist_cutoff")) dist_cutoff <- 100
      gps_locations <- process_gps_barlt(base_folder = folder_base,
                                         site_pattern = site_pattern,
                      file_list= list_files,dist_cutoff=dist_cutoff,
                      deploy_start_date = deploy_start_date,
                      check_dists)
    }


  }
  # |- SM4   ----------------
  if(SM){
    if(grepl("3", type)) warn("Default site_pattern is set for SM4. Recommend changing based on file structure")
    if(!exists("site_pattern")) site_pattern <-  "S4A\\d{5}"
    # file.location <- "//WRIV02DTSTDNT1/RecordStor20172019/BetweenRivers_2019"

      # list.files(folder_base, "_Summary.txt", recursive = T, full.names = T)



    if(is_null(gps_locations)){
      gps_locations <- process_gps_SM(folder_base = folder_base, list_files = list_files,
                                      site_pattern = site_pattern)
    }
    if(!exists("site_in_filename")) site_in_filename <- TRUE
  }
    # if(length(unique(gps_locations$tz ))>1) browser()
    wav_names_log <- parse_datetimes(list_waves,filename_separator = file_split_pattern,
                                           site_in_filename = site_in_filename,
                                           site_pattern =site_pattern,
                                           tz_loc = unique(gps_locations$tz ))

    rec_log_ss <- prep_sunrise_sunset(gps_locations = gps_locations,
                                            wav_names_log = wav_names_log)


    browser()


    return(rec_log_ss)


}
