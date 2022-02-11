#' Clean aru metadata
#'
#' @param type BarLT or SM4
#' @param folder_base Folder base directory
#' @param sitename  Site name
#'
#' @return
#' @export
clean_metadata <- function(type, folder_base, sitename ){
  if(type == "BarLT"){
    ## Testing Bar-LT data
    # library(tidyverse)
    # library(lubridate)
    # library(suncalc)
    # folder_base <- "D:/!_TEMP_AUDIO_SAMPLES/ARU_RecordingSample_P7-04/"
    list_hexes <- list.dirs(folder_base, full.names = F, recursive = F)
    list_arus <- purrr::map(list_hexes, ~list.dirs(glue::glue("{folder_base}/{.x}"), full.names = F, recursive = F)) |>
        purrr::flatten_chr()
    list_files <- list.files(folder_base, recursive = T, full.names = F, include.dirs = F)
    list_waves <- list.files(folder_base, pattern = ".wav", recursive = T)
    if(length(list_waves)==0){return(tibble::tibble(SiteID = sitename))}
    serial_number <- readr::read_table(glue::glue("{folder_base}/logfile.txt"),skip = 3,col_names = F,
                                col_types = "ccc",

                                n_max = 1) |>
      dplyr::pull(3)


    gps_log <- readr::read_csv(glue::glue("{folder_base}/{list_files[grepl('GPS_log', list_files)]}"),
                               skip = 1, col_names = T, col_types = readr::cols()) |>
      janitor::clean_names()



    # rec_log <- readr::read_csv(glue::glue("{folder_base}/{list_files[grepl('Reclog', list_files)]}"),
    #                            skip = 1, col_names = T, col_types = readr::cols()) |>
    #   janitor::clean_names()

    # gps_loc <- gps_log[(gps_log$dd_mm_yy %in% rec_log$date_dd_mm_yyyy)|(gps_log$dd_mm_yy %in% (rec_log$date_dd_mm_yyyy+1)) ,]
    gps_loc <- gps_log[nrow(gps_log) ,]
    if(nrow(gps_loc)>1) warning("Multple GPS locations. Please check")
    if(nrow(gps_loc)==0) simpleError("GPS location did not pull.")

    tz_loc <- lutz::tz_lookup_coords(lat = gps_loc$latitude_decimal_degrees,
                                     lon = gps_loc$longitude_decimal_degrees,
                                     method = 'accurate')
    # browser()
    #FixNames - Currently gain is a single column tidyr::separated by
    # names_err <- names(rec_log)
    # names_fixed <- c(names_err[1:4], "gain_db_A","gain_db_B", names_err[6:15])
    # names(rec_log) <- names_fixed
    ll <- length(stringr::str_split(list_waves[[1]], "/")[[1]])
    wav_names_log <- tibble::tibble(filename=list_waves) %>%
      {if(ll==1){
      dplyr::mutate(., WaveFilename=filename)
    } else{

      tidyr::separate(., remove=F, col = filename, sep = "/",
               into = c("Folder", "WaveFilename")) } } %>%
      tidyr::separate(remove=F, col = WaveFilename, sep = "T|\\-|\\_|\\.",
               into = c("yyyymmdd", "hhmmss", "utm", "SR_SS", "wav")) |>
      dplyr::mutate(
        SiteID = sitename,
        ARU_serial = serial_number,
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
             date_time = lubridate::ymd_hms(raw_date_time, tz = tz_loc[[1]]),

             day_before = date-1,
             day_after=date+1

      )




    # rec_log_clean <- rec_log |>
    #   dplyr::mutate(date = lubridate::dmy(date_dd_mm_yyyy),
    #          time = lubridate::hms(time_hh_mm_ss),
    #          date_time = lubridate::dmy_hms(glue::glue("{date_dd_mm_yyyy} {time_hh_mm_ss}"), tz = tz_loc),
    #          free_space_gb = dplyr::case_when(
    #            grepl("GB$", free_space)~as.numeric(stringr::str_remove(free_space,"[A-Z]+")),
    #            grepl("MB$", free_space)~as.numeric(stringr::str_remove(free_space,"[A-Z]+"))*0.001,
    #            TRUE~as.numeric(free_space)*1.25e-10),
    #          p_Battery_Cap = as.numeric(stringr::str_remove(percent_full, "\\%"))/100,
    #          day_before = date-1,
    #          day_after=date+1
    #
    #   )

    # browser()

    d_se <- seq(min(wav_names_log$date)-1, max(wav_names_log$date)+1, by = '1 day')

    ss <- suncalc::getSunlightTimes(date =d_se , lat = gps_loc$latitude_decimal_degrees,
                                    lon = gps_loc$longitude_decimal_degrees, tz = tz_loc,
                                    keep = c("sunrise", "sunset"))

    # browser()

    rec_log_ss <- wav_names_log |>
      dplyr::left_join(ss, by = "date") |>
      dplyr::left_join(ss |>
                  dplyr::rename(sunrise_before = sunrise,
                         sunset_before = sunset)
                , by = c("day_before" = "date")) |>

      dplyr::left_join(ss |>
                  dplyr::rename(sunrise_after = sunrise,
                         sunset_after = sunset)
                , by = c("day_after" = "date")) |>
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
        dow = lubridate::wday(date_time))

    return(rec_log_ss)

  }
  if(type == "SM4"){
    browser()
    # file.location <- "//WRIV02DTSTDNT1/RecordStor20172019/BetweenRivers_2019"
    interp.dir <- list.dirs(folder_base,full.names = F,recursive = F)

    ARUs <-
      tibble::tibble(ARU_ID = interp.dir) |>
      tidyr::separate(ARU_ID, into = c("SiteID", "ARU_Num", "Special"),
               sep = "_",remove = F )
    summary.files <- list.files(folder_base, "_Summary.txt", recursive = T)

    colNames <- read_csv(file = paste0(folder_base, "/",summary.files[[1]]),
                         col_types = cols()) |> names()


    summary.aru.info <- tibble::tibble(fileloc = summary.files) |>
      dplyr::mutate(f = map(fileloc, ~read_csv(file = paste0(folder_base, "/",.x), skip = 1,
                                        col_types = paste(rep("c",11), collapse = ""),
                                        col_names = colNames)))



    ARU_summary_information <- unnest(summary.aru.info, cols = c(f)) |>
      filter(LAT != "LAT") |>
      janitor::clean_names() |>
      dplyr::mutate_at(vars(lat, lon,power_v, temp_c, number_files), as.numeric) |>
      tidyr::separate(fileloc, into = c("ARU_ID", "FileName", "Type"), remove =F, sep = "[/.]")

    file.names <- list.files(folder_base, ".wav", recursive = T)

    ARU_file_tibble <- tibble::tibble(file.loc = file.names) |>
      tidyr::separate(file.loc, into = c("ARU_ID", "Data", "FileID", "type"), sep = "[/.]" , remove = F) |>
      tidyr::separate(FileID, into = c("SMID", "DateID", "TimeID"), sep = "_", remove = F) |>
      dplyr::left_join(
        dplyr::select(ARU_summary_information, ARU_ID, lat, lon) |>
          distinct(), by = "ARU_ID"
      ) |>
      dplyr::mutate(Year = substr(DateID, 1,4),
             Month = substr(DateID, 5, 6),
             Day = substr(DateID, 7, 8),
             StartTime_HH = substr(TimeID, 1,2),
             StartTime_MM = substr(TimeID, 3,4),
             Second = substr(TimeID, 4,5),
             timestamp = lubridate::mdy_hm(paste(Month, Day, Year,
                                                 StartTime_HH,
                                                 StartTime_MM,
                                                 sep = "-"),
                                           tz = "America/Toronto"),
             ts_4_sur = case_when(StartTime_HH>18~timestamp + lubridate::days(1),
                                  StartTime_HH<18~timestamp ),
             ts_4_sunset = case_when(StartTime_HH<11~timestamp- lubridate::days(1),
                                     StartTime_HH>11~timestamp),
             sunrise =  sunr(-1*lon, lat, ts_4_sur,
                             dir = 'sunrise'),
             sunset = sunr(-1*lon, lat, ts_4_sunset, dir = 'sunset'),
             min_to_Sunrise = lubridate::time_length(difftime(timestamp, sunrise, units = "mins"), ## FIX THIS
                                                     unit = "minute"
             ),
             min_to_Sunset = lubridate::time_length(difftime( timestamp, sunset, units = "mins"), ## FIX THIS
                                                    unit = "minute"
             ),
             doy = lubridate::yday(timestamp) ) |>
      dplyr::select(-ts_4_sur, -ts_4_sunset) |>
      dplyr::mutate(Sample_Group = case_when(
        (min_to_Sunrise> -90 & min_to_Sunrise < 5*60)~"Dawn",
        (min_to_Sunset > -60 & min_to_Sunset < 120)~"Evening",
        (min_to_Sunrise > -5*60 & min_to_Sunrise < -30)~"Night",
        TRUE~"Other"
      ),
      week = lubridate::week(timestamp),
      dow = lubridate::wday(timestamp))


    # date = lubridate::ymd(yyyymmdd),
    # year = year(date),
    # month = month(date),
    # day = day(date),
    # doy = yday(date),
    # hour = str_sub(hhmmss, 1L, 2L),
    # min =  str_sub(hhmmss, 3L, 4L),
    # sec =  str_sub(hhmmss, 5L, 6L),
    # time = hms(glue::glue("{hour}:{min}:{sec}")),
    # raw_date_time=glue::glue("{year}-{month}-{day} {hour}:{min}:{sec}"),
    # date_time = lubridate::ymd_hms(raw_date_time, tz = tz_loc),
    #
    # day_before = date-1,
    # day_after=date+1

    # )

  }

  if(!type %in% c("BarLT","SM4")){
    simpleError("Currently only BarLT and SM4 ARUs are supported. Will add more as needed.")
  }


}
