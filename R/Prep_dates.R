#' Parse file names of wave files for dates and times.
#'
#' @param list_waves list of wav files to be parsed
#' @param site_pattern pattern  in grep to pull site id from path.
#'                  Default is "[P|Q]\\d+_\\d"
#' @param filename_separator pattern in grep to separate out WaveFileName.
#'                Default is "T|\\-|\\_|\\."
#' @param tz_loc Time zone for location. Default to "America/Toronto"
#'
#' @return Returns a data frame with filenames parsed to date & time.
parse_datetimes_BarLT <- function(list_waves,
                                  site_in_filename,
                                  site_pattern = "[P|Q]\\d+_\\d",
                                  filename_separator ="T|\\-|\\_|\\.",
                                  tz_loc = "America/Toronto"){
  ll <- length(stringr::str_split(list_waves[[1]], "/")[[1]])

  pathnames <- c(glue::glue("Folder{1:(ll-1)}"), "WaveFilename")

  WaveFileName_Strings <- if_else(site_in_filename,
                                  glue::glue_collapse(c("ARUName",
                                                        "yyyymmdd",
                                                        "hhmmss", "utm",
                                                        "SR_SS",
                                                        "wav"), sep = ";"),
                                  glue::glue_collapse(c("yyyymmdd",
                                                        "hhmmss", "utm",
                                                        "SR_SS",
                                                        "wav"), sep = ";")
  )

  wav_names_log <- tibble::tibble(filename=list_waves) %>%
    {if(ll==1){
      dplyr::mutate(., WaveFilename=filename)
    } else{

      tidyr::separate(., remove=F, col = filename, sep = "/",
                      into = pathnames) } } %>%
    tidyr::separate(remove=F, col = WaveFilename, sep = filename_separator,
                    into =stringr::str_split(WaveFileName_Strings, ";")[[1]]) |>
    dplyr::mutate(
      SiteID = stringr::str_extract(filename, site_pattern),
      # ARU_serial = serial_number,
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
      date_time = lubridate::ymd_hms(raw_date_time, tz = tz_loc),

      day_before = date-1,
      day_after=date+1

    )

  if(sum(is.na(wav_names_log$time))==length(list_waves)){
    rlang::abort(c("File parsing failed.",
                   "x" = "Folder structure not parsed",
                   "i" = "Check folder structure and use folder_sep to modify parsing. "))

  }
  if(any(is.na(wav_names_log$time))) rlang::warn(c("Some time parsing failed",
                                                   "i" = "Check output"))

  return(wav_names_log)


}


link_gps_locs_BarLT <- function(gps_locations, wav_names_log){
  stopifnot(
    "Not all SiteID from GPS log are in wav_names_log"=
      all(gps_locations$SiteID %in% wav_names_log$SiteID),
    "Not all SiteID from wave_names_log are in GPS log" =
      all(wav_names_log$SiteID %in% gps_locations$SiteID)
  )

  g <- gps_locations |> #filter(SiteID == sites[[i]]) |>
    dplyr::rowwise() |>
    dplyr::mutate(dategps = lubridate::dmy_hms(paste(dd_mm_yy, hh_mm, sep = " "), tz = tz)) |>
    dplyr::ungroup() |>
    dplyr::arrange(SiteID,dategps) |>
    dplyr::mutate(
      gr = dplyr::row_number())
  wav_with_gps <-
    wav_names_log |>
    # filter(SiteID == sites[[i]]) |>
    # slice_tail(n=2) |>
    left_join(g[,c("SiteID","dategps",
                   "latitude_decimal_degrees",
                   "longitude_decimal_degrees",
                   "tz"
    )], by = "SiteID") |>
    filter(date_time>=(dategps-lubridate::days(1))) |>
    group_by(filename) |>
    slice_min(date_time - dategps) |>
    ungroup()

  stopifnot(
    "Differing numbers of records after joining wav_names_log with gps_locations" =
      nrow(wav_with_gps)==nrow(wav_names_log),
    "Not all files in final record" =
      all(wav_names_log$filename %in% wav_with_gps$filename)
  )
  if(any(is.na(wav_with_gps$latitude_decimal_degrees))) warn("Some records did not link with a gps location")

  return(wav_with_gps)

}

#' Calculate sunrise sunset
#'
#' A wrapper around suncalc::getSunlightTimes to ensure
#'    timezones and lat/lon are correct.
#'
#' @param .data data frame from prep_sunrise_sunset
#' @param var_day variable from .data to use as date to calculate sunrise
#'
#' @return Returns a data frame with sunrise and sunset
#'
calculate_sunrise_sunset <- function(.data,var_day){
    .tz <- unique(.data$tz)
    stopifnot("Multiple time zones detected, please run separately"=
                length(.tz)==1)
    .data |>
      dplyr::transmute(SiteID, original_date = date,
                       date = {{var_day}},
                       lat = latitude_decimal_degrees,
                       lon = longitude_decimal_degrees,
                       tz = tz) %>%
      suncalc::getSunlightTimes(data = .,
                                keep = c("sunrise", "sunset"),
                                tz = .tz) |>
       dplyr::rename(
      "calc_{{var_day}}" := date,
        "sunrise_{{var_day}}":= sunrise,
        "sunset_{{var_day}}":= sunset
      ) |> dplyr::bind_cols(
        dplyr::select(.data, SiteID, date)
      )|>
    tibble::as_tibble()
    }


#' Calculate sunrise and sunset and time to sunset for BarLT recordings
#'
#'
#'
#' @param gps_locations data frame generated by process_gps_barlt or process_gps_SM
#' @param wav_names_log data frame generated by parse_datetimes
#'
#' @return returns a data frame with time to sunrise
prep_sunrise_sunset_BarLT <- function(gps_locations,wav_names_log){

  wav_with_gps <- link_gps_locs_BarLT(gps_locations = gps_locations,
                                      wav_names_log = wav_names_log)



 ss_dayof <- calculate_sunrise_sunset(.data = wav_with_gps, date)
 ss_daybefore <- calculate_sunrise_sunset(.data = wav_with_gps, day_before )
 ss_dayafter <- calculate_sunrise_sunset(.data = wav_with_gps, day_after )

 stopifnot("Time zones not equal"=all.equal(lubridate::tz(ss_dayafter$sunrise_day_after),
                     lubridate::tz(ss_daybefore$sunset_day_before),
                     lubridate::tz(ss_dayof$sunset_date),
                     lubridate::tz(wav_with_gps$date_time)
                     ),
           "Sites not row paired in Sunrise Calculation"=all.equal(ss_dayafter$SiteID,
                                            (ss_daybefore$SiteID),
                                            (ss_dayof$SiteID),
                                            (wav_with_gps$SiteID) ) ,
           "Dates not row paired in Sunrise Calculation"=all.equal(as.numeric(ss_dayafter$date),
                                                                   as.numeric(ss_daybefore$date),
                                                                   as.numeric(ss_dayof$date),
                                                                   as.numeric(wav_with_gps$date),
                                                                   tolerance = 0 ) )


 ss <- wav_with_gps |>
    bind_cols(
      ss_dayof |>
      dplyr::select(-SiteID, -date, -lat, -lon)
      ) |>
   bind_cols(
     ss_daybefore |>
       dplyr::select(-SiteID, -date,-lat, -lon)
   ) |>
   bind_cols(
     ss_dayafter |>
       dplyr::select(-SiteID, -date,-lat, -lon)
   )
  # ,
  #   calculate_sunrise_sunset(.data = wav_with_gps, day_before ),
  #   by = c("SiteID",  "date")) |>
  #   left_join(
  #     calculate_sunrise_sunset(.data = wav_with_gps, day_after )
  #   )





  ss |>
    dplyr::mutate(
      t2sr = lubridate::int_length(lubridate::interval(sunrise_date, date_time))/60,
      t2sr_before = lubridate::int_length(lubridate::interval(sunrise_day_before,date_time))/60,
      t2sr_after = lubridate::int_length(lubridate::interval(sunrise_day_after,date_time))/60,


      t2ss = lubridate::int_length(lubridate::interval(sunset_date, date_time))/60,
      t2ss_before = lubridate::int_length(lubridate::interval(sunset_day_before,date_time))/60,
      t2ss_after = lubridate::int_length(lubridate::interval(sunset_day_after,date_time))/60,
      doy = lubridate::yday(date)) |>
    dplyr::rowwise() |>
    dplyr::mutate(t2sr_min = c(t2sr, t2sr_before, t2sr_after)[which.min(c(abs(t2sr), abs(t2sr_before),
                                                                          abs(t2sr_after)))],
                  t2ss_min = c(t2ss, t2ss_before, t2ss_after)[which.min(c(abs(t2ss), abs(t2ss_before),
                                                                          abs(t2ss_after)))],

    ) |> dplyr::ungroup() |>
    dplyr::mutate(
      week = lubridate::week(date_time),
      dow = lubridate::wday(date_time))
}

