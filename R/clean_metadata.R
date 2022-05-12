#' Clean aru metadata
#'
#' @param type BarLT or SM4
#' @param folder_base Folder base directory
#' @param gps_locations GPS locations in lat/lon format with SiteID
#'                       Must also include timezone (tz) and deploy date.
#'
#' @return
#' @export
clean_metadata <- function(type, folder_base,gps_locations = NULL, ... ){
  list2env(list(...), envir = environment())
  if(!exists("file_ext")) file_ext <- ".wav"
  if(!exists("file_split_pattern")) file_split_pattern <- "T|\\-|\\_|\\."
  list_files <- list.files(folder_base, recursive = T, full.names = F, include.dirs = F)
  list_waves <- list_files[grep(file_ext, list_files)]
  if(length(list_waves)==0){
    abort(c("Did not find any wav files. Default is set to '.wav'",
            "x" = "Sound file locations needed to process.",
            "i" = "Use file_ext to change file extension for sound files and check folder_base is correct."))
    return(tibble::tibble(SiteID = sitename))}
  # if(!exists("folder_sep")) {
  #   if(sum(grepl(list_waves, pattern = "Data"))>0) {folder_sep <- "Data"
  #   } else if(sum(grepl(list_waves, pattern = "Wav"))>0) {folder_sep <- "Wav"
  #   } else abort("Tried 'Wav' and 'Data' as folder separators. Please use your own using folder_sep argument")
  # }
  # |- BarLT   ----------------
  if(type == "BarLT"){

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

    wav_names_log <- parse_datetimes_BarLT(list_waves, site_pattern =site_pattern,
                                           site_in_filename = site_in_filename,
                                           tz_loc = unique(gps_locations$tz ))

    rec_log_ss <- prep_sunrise_sunset_BarLT(gps_locations = gps_locations,
                                      wav_names_log = wav_names_log)

    return(rec_log_ss)

  }
  # |- SM4   ----------------
  if(type == "SM4"){
    if(!exists("site_pattern")) site_pattern <-  "S4A\\d{5}"
    # file.location <- "//WRIV02DTSTDNT1/RecordStor20172019/BetweenRivers_2019"
    summText <- list_files[grep("_Summary.txt", list_files)]
      # list.files(folder_base, "_Summary.txt", recursive = T, full.names = T)

    summaries <- purrr::map_df(summText, ~{read.csv(glue::glue("{folder_base}/{.x}")) |>
       dplyr::mutate(SiteID = stringr::str_extract(.x, site_pattern))}) |>
      tibble::as_tibble()

    if(is_null(gps_locations)){
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
    }
    if(!exists("site_in_filename")) site_in_filename <- TRUE


    wav_names_log <- parse_datetimes_BarLT(list_waves,filename_separator = file_split_pattern,
                                           site_in_filename = site_in_filename,
                                           site_pattern =site_pattern,
                                           tz_loc = unique(gps_locations$tz ))

    rec_log_ss <- prep_sunrise_sunset_BarLT(gps_locations = gps_locations,
                                            wav_names_log = wav_names_log)



    SM_files <-
      tibble::tibble(
        filepath = list.files(folder_base, pattern = '.wav', recursive = T, full.names = T)) %>%
      tidyr::separate(., remove=F, col = filepath, sep = glue::glue("/{folder_sep}/"),
                      into = c("Folder", "WaveFilename"))  %>%
      tidyr::separate(remove=F, col = WaveFilename, sep = "T|\\-|\\_|\\.",
                      into = c("SMID",  "yyyymmdd", "hhmmss", "wav")) |>
      dplyr::mutate(
        SiteID = glue::glue("LutherMarsh2021-{SMID}"),
        ARU_serial = SMID,
        date = lubridate::ymd(yyyymmdd),
        year = lubridate::year(date),
        month = lubridate::month(date),
        day = lubridate::day(date),
        doy = lubridate::yday(date),
        hour = stringr::str_sub(hhmmss, 1L, 2L),
        min =  stringr::str_sub(hhmmss, 3L, 4L),
        sec =  stringr::str_sub(hhmmss, 5L, 6L),
        time = lubridate::hms(glue::glue("{hour}:{min}:{sec}")),
        raw_date_time=glue::glue("{year}-{month}-{day} {hour}:{min}:{sec}"),
        date_time = lubridate::ymd_hms(raw_date_time, tz = locs$tz_loc[[1]]),

        day_before = date-1,
        day_after=date+1

      )

    if(sum(is.na(SM_files$time))==nrow(SM_files)){
      rlang::abort(c("File parsing failed.",
                     "x" = "Folder structure not parsed",
                     "i" = "Check folder structure and use folder_sep to modify parsing. "))
    }
    if(any(is.na(SM_files$time))) rlang::warn(c("Some time parsing failed",
                                                "i" = "Check output"))

    d_se <- seq(min(SM_files$date)-1, max(SM_files$date)+1, by = '1 day')

    ss <- purrr::map_df(1:nrow(locs), ~{suncalc::getSunlightTimes(date =d_se , lat =  locs$Y[[.x]],
                                                  lon =  locs$X[[.x]], tz = locs$tz_loc[[.x]],
                                                  keep = c("sunrise", "sunset")) |>
       dplyr::mutate(ARU_serial = locs$ARU_serial[[.x]])}) |> tibble::as_tibble()


    rec_log_SM <- SM_files |>
      dplyr::left_join(ss, by = c("ARU_serial", "date")) |>
      dplyr::left_join(ss |>
                         dplyr::rename(sunrise_before = sunrise,
                                       sunset_before = sunset)
                       , by = c("ARU_serial","day_before" = "date")) |>

      dplyr::left_join(ss |>
                         dplyr::rename(sunrise_after = sunrise,
                                       sunset_after = sunset)
                       , by = c("ARU_serial","day_after" = "date")) |>
      dplyr::mutate(
        t2sr = lubridate::int_length(lubridate::interval(sunrise, date_time))/60,
        t2sr_before = lubridate::int_length(lubridate::interval(sunrise_before,date_time))/60,
        t2sr_after = lubridate::int_length(lubridate::interval(sunrise_after,date_time))/60,


        t2ss = lubridate::int_length(lubridate::interval(sunset, date_time))/60,
        t2ss_before = lubridate::int_length(lubridate::interval(sunset_before,date_time))/60,
        t2ss_after = lubridate::int_length(lubridate::interval(sunset_after,date_time))/60,
        doy = lubridate::yday(date)) |>
      dplyr::rowwise() |>
      dplyr::mutate(t2sr_min = c(t2sr, t2sr_before, t2sr_after)[which.min(c(abs(t2sr), abs(t2sr_before),
                                                                            abs(t2sr_after)))],
                    t2ss_min = c(t2ss, t2ss_before, t2ss_after)[which.min(c(abs(t2ss), abs(t2ss_before),
                                                                            abs(t2ss_after)))],

      ) |> dplyr::ungroup() |>
      dplyr::mutate(
        #   Sample_Group = case_when(
        #   (min_to_Sunrise> -90 & min_to_Sunrise < 5*60)~"Dawn",
        #   (min_to_Sunset > -60 & min_to_Sunset < 120)~"Evening",
        #   (min_to_Sunrise > -5*60 & min_to_Sunrise < -30)~"Night",
        #   TRUE~"Other"
        # ),
        week = lubridate::week(date_time),
        dow = lubridate::wday(date_time),
        Site_ARU_id = glue::glue("{SiteID}_{ARU_serial}"),
        t2sr_round = round(t2sr_min/5)*5,
        t2ss_round = round(t2ss_min/5)*5,
        SS_SR = ifelse(t2sr_min > -150, "SR", "SS"),
        recid = dplyr::row_number())
    return(rec_log_SM)
  }

  if(!type %in% c("BarLT","SM4")){
    simpleError("Currently only BarLT and SM4 ARUs are supported. Will add more as needed.")
  }


}
