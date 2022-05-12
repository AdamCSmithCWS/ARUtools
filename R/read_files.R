#' Parse the log file for BarLT
#'
#' Returns the serial number, firware version
#' and log file parsed as a tibble.
#'
#' @param filename Character. File location of log file
#'
#' @return
#'
read_log_barlt <- function(filename){
  # Read the full file in
  full_file <- readr::read_lines(file = filename, skip = 2)
  # Parse out serial number
  serial <- full_file[grep("Serial Number:", full_file)] |>
    stringr::str_extract_all(pattern ="((\\d{5,}\\-)+\\d{5,}|\\d{5,})") |> unique()
  firmware <- full_file[grepl("Firmware:", full_file)] |>
     stringr::str_remove(" Firmware: ") |> unique()

  if(length(serial)>1 ){
    warn(c(glue::glue("Multiple serial numbers read from file {filename}"),
            "x" = "Cannot have multiple serial numbers in single log file",
          "i"=glue::glue("Current serial numbers are {glue::glue_collapse(serial, sep ='; ')}.
                              Check original log file for errors.") ))
    tmp_action <- menu(c(glue::glue("Use {serial}"), "Abort program"),
                       title = glue::glue("Multiple serial numbers detected for {filename}. What would you like to do?"))

  if (tmp_action == (length(serial) + 1)) {
    abort("Check ARU log file and try again")
  } else{
    serial <- serial[[tmp_action]]
  }
  }

    if(length(firmware)>1 ){
      warn(c(glue::glue("Multiple firmware version read from file {filename}"),
             "x" = "Should not have multiple firmware versions in single log file",
             "i"=glue::glue("Current firmware versions are {glue::glue_collapse(firmware, sep ='; ')}.
                              Check original log file for errors.") ))
      tmp_action2 <- menu(c(glue::glue("Use {firmware}"), "Abort program"),
                         title = glue::glue("Multiple firmware versions detected for {filename}. What would you like to do?"))

      if (tmp_action2 == (length(firmware) + 1)) {
        abort("Check ARU log file and try again")
      } else{
        firmware <- firmware[[tmp_action2]]
      }
    }



  dated_logs <- full_file[grepl("^\\d\\d\\/", full_file)] |>
    stringr::str_remove("\xffffffb0") |>
    stringr::str_conv("UTF-8") |>
    tibble::tibble(row = _ ) |>
    tidyr::separate(col = row, into = c("Date", "Time", "Log"),
                    sep = c(10,20)) |>
    mutate(filename = filename,
           serial = serial,
           firmware = firmware)

  return(list(serial_number = serial,
              firmware = firmware,
              log_entries = dated_logs))



}


#' Parse SM4 logfiles
#'
#' @param filename Character string or vector of character strings of file locations.
#'                  Can be a single file or a vector of file locations.
#' @param SiteID_pattern Pattern for extracting SiteID from filename. Defaults to 'SM4A\\d{5}',
#'                         which is SM4, followed by 5 digits.
#'
#' @return Returns data frame of log files.
#'
read_summary_SM4 <- function(filename, SiteID_pattern = "SM4A\\d{5}"){
  if(length(filename)!=1 & !(length(filename)>1)) abort("Filename in not correct format. Cannot read SM logs.")
  if(length(filename)>1){

  out <- purrr::map_df(summText, ~{read.csv(.x) |>
      dplyr::mutate(SiteID = stringr::str_extract(.x, SiteID_pattern))})

  if(any(is.na(out$SiteID))) warn("Some SiteID were not parsed. Check SiteID_pattern is correct")
  }
  if(length(filename)==1){
    out <- read.csv(filename) |>
             dplyr::mutate(SiteID = stringr::str_extract(.x, SiteID_pattern))
    if(any(is.na(out$SiteID))) warn("Some SiteID were not parsed. Check SiteID_pattern is correct")

  }


  return(out)


}

#' Process GPS locations for SongMeters
#'
#' @param folder_base Base folder were summary folders locationed
#' @param list_files List of files in folder_base
#'
#' @return Returns a data frame with lat/lon for each location and date collected
process_gps_SM <- function(folder_base, list_files){

  summText <- list_files[grep("_Summary.txt", list_files)]

  summaries <- purrr::map_df(summText, ~{read.csv(glue::glue("{folder_base}/{.x}")) |>
      dplyr::mutate(SiteID = stringr::str_extract(.x, site_pattern))}) |>
    tibble::as_tibble()
  needed_names <- c("LAT", "LON", "DATE", "TIME")
  if(!all( needed_names%in% names(summaries))) abort(
    glue::glue(
      "Check your summary files. Should include {glue::glue_collapse(needed_names, sep = '; ')}"
      )
    )

  gps_locations <- summaries |>
    # HH/MM,DD/MM/YY
    mutate(date = lubridate::ymd(DATE),
           time =  lubridate::hms(TIME),
           dd_mm_yy = as.character(format(date, "%d/%m%/%Y")),
           hh_mm = as.character(TIME)) |>
    dplyr::arrange(SiteID,date,time) |>
    dplyr::distinct(SiteID, LAT, LON,
                    .keep_all = T) |>
    dplyr::select(SiteID,dd_mm_yy, hh_mm, LAT, LON) |>
    dplyr::mutate(LON = -1*LON) |>
    sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326) %>%
    dplyr::bind_cols(
      tibble::as_tibble(sf::st_coordinates(.))
    ) |> dplyr::rename(longitude_decimal_degrees=X,
                       latitude_decimal_degrees=Y) |>
    sf::st_drop_geometry()

  gps_locations$tz <- lutz::tz_lookup_coords(lat = gps_locations$latitude_decimal_degrees,
                                             lon = gps_locations$longitude_decimal_degrees,
                                             method = 'accurate')
  stopifnot("Multiple time zones detected, please run separately"=
              length(unique(gps_locations$tz))==1)
  return(gps_locations)
}


#' Process GPS log file for BarLT
#'
#' Extracts out lat/lon locations from gps log files
#'   will drop locations before a given date, but does
#'   not drop duplicate locations
#'
#' @param base_folder base_folder where files are stored
#' @param file_list  file list relative to base_folder
#' @param deploy_start_date  first date of deployment
#'
#' @return Returns a data frame with lat/lon for each location and date collected
process_gps_barlt <- function(base_folder, file_list, deploy_start_date,check_dists,...){
  crs_m <- 3161
  dist_cutoff <- 100
  list2env(list(...), environment())
  gps_log_full <- purrr::map_df(glue::glue("{base_folder}/{file_list[grepl('GPS', file_list)]}"),
    ~{readr::read_csv(.x,skip = 1, col_names = T, col_types = readr::cols()) |>
    janitor::clean_names() |> mutate(filepath = .x)}) |>
    mutate(dd_mm_yy = lubridate::dmy(dd_mm_yy)) |>
    tidyr::separate(filepath, remove=F, sep = paste0(base_folder), into = c("dropme", "Other")) |>
    tidyr::separate(Other, into =c("dropme2", "SiteID", "Filename"), sep = "/") |>
    dplyr::select(-dropme, -dropme2) |> filter(dd_mm_yy >= deploy_start_date)

  tz_loc <- lutz::tz_lookup_coords(lat = gps_log_full$latitude_decimal_degrees,
                                   lon = gps_log_full$longitude_decimal_degrees,
                                   method = 'accurate')
  gps_log_full$tz <- tz_loc

  if(dplyr::n_distinct(tz_loc)>1) warn("Multple time zones detected. This may affect sunrise/sunset accuracy")

  if(isTRUE(check_dists)) {
    site_distances <- check_gps_distances(gps_log_full, crs_m = crs_m, dist_cutoff = dist_cutoff)
    gps_log_full <- left_join(gps_log_full, site_distances, by = "SiteID")
    }



  # # rec_log <- readr::read_csv(glue::glue("{folder_base}/{list_files[grepl('Reclog', list_files)]}"),
  # #                            skip = 1, col_names = T, col_types = readr::cols()) |>
  # #   janitor::clean_names()
  #
  # # gps_loc <- gps_log[(gps_log$dd_mm_yy %in% rec_log$date_dd_mm_yyyy)|(gps_log$dd_mm_yy %in% (rec_log$date_dd_mm_yyyy+1)) ,]
  # gps_loc <- gps_log_full[nrow(gps_log_full) ,]
  # if(nrow(gps_loc)>1) warn("Multple GPS locations. Please check")
  # if(nrow(gps_loc)==0) abort("GPS location did not pull.")
  #
  # message("Full GPS log contains the following locations")
  # print(gps_log_full)
  # message("However only the last one is being used for ARU location.")
  # if (menu(c("Yes", "No"), title = "Do you wish to continue?") != 1) {
  #   abort("Check gps locations and try again")
  # }
  return(gps_log_full)


}


#' Check distances between points from GPS log
#'
#' @param gps_log gps log file generated from process_gps_barlt
#' @param crs_m  CRS for measurement of distances. Should be in meters
#' @param dist_cutoff Distance cutoff in meters. Can be set to Inf to avoid this check.
#'
#' @return Returns a data frame with maximum distances between gps points at site.
check_gps_distances <- function(gps_log, crs_m = 3161, dist_cutoff = 100){
  max_distances <-
  gps_log |>
    sf::st_as_sf(coords= c("longitude_decimal_degrees",
                       "latitude_decimal_degrees"),
             crs = 4326) |>
    sf::st_transform(crs_m) |>
    dplyr::group_by(SiteID) %>%
    dplyr::summarize(max_dist = max(sf::st_distance(geometry, geometry)),
              .groups = 'drop') |>
    sf::st_drop_geometry()

  if(any(max_distances$max_dist>units::set_units(dist_cutoff, "m")))
    abort(c("Within Site distance is greater than cuttoff",
            "x" = "Distance must be less than `dist_cutoff`",
            "i" = "Set dist_cutoff to Inf to avoid this (e.g. moving ARU)"))

  return(max_distances)

}


