## Testing Bar-LT data
library(tidyverse)
library(lubridate)
library(suncalc)
folder_base <- "D:/!_TEMP_AUDIO_SAMPLES/ARU_RecordingSample_P7-04/"
list_files <- list.files(folder_base)
list_waves <- list.files(folder_base, pattern = ".wav", recursive = T)
serial_number <- read_table(glue::glue("{folder_base}logfile.txt"),skip = 3,col_names = F,
                            col_types = "ccc",

                            n_max = 1) %>%
  pull(X3)
gps_log <- readr::read_csv(glue::glue("{folder_base}{list_files[grepl('GPS_log', list_files)]}"),
                           skip = 1, col_names = T) %>%
  janitor::clean_names()



rec_log <- readr::read_csv(glue::glue("{folder_base}{list_files[grepl('Reclog', list_files)]}"),
                           skip = 1, col_names = T) %>%
  janitor::clean_names()

gps_loc <- gps_log[gps_log$dd_mm_yy %in% rec_log$date_dd_mm_yyyy,]
if(nrow(gps_loc)>1) warning("Multple GPS locations. Please check")

tz_loc <- lutz::tz_lookup_coords(lat = gps_loc$latitude_decimal_degrees,
                                 lon = gps_loc$longitude_decimal_degrees,
                                 method = 'accurate')

#FixNames - Currently gain is a single column separated by
names_err <- names(rec_log)
names_fixed <- c(names_err[1:4], "gain_db_A","gain_db_B", names_err[6:15])
names(rec_log) <- names_fixed

wav_names_log <- tibble(filename=list_waves) %>%
  separate(remove=F, col = filename, sep = "/",
           into = c("Folder", "WaveFilename")) %>%
  separate(remove=F, col = WaveFilename, sep = "T|\\-|\\_|\\.",
           into = c("yyyymmdd", "hhmmss", "utm", "SR_SS", "wav")) %>%
  mutate(ARU_serial = serial_number,
    date = ymd(yyyymmdd),
         year = year(date),
         month = month(date),
         day = day(date),
         doy = yday(date),
         hour = str_sub(hhmmss, 1L, 2L),
         min =  str_sub(hhmmss, 3L, 4L),
         sec =  str_sub(hhmmss, 5L, 6L),
         time = hms(glue::glue("{hour}:{min}:{sec}")),
         raw_date_time=glue::glue("{year}-{month}-{day} {hour}:{min}:{sec}"),
         date_time = ymd_hms(raw_date_time, tz = tz_loc),

         day_before = date-1,
         day_after=date+1

  )




rec_log_clean <- rec_log %>%
  mutate(date = dmy(date_dd_mm_yyyy),
         time = hms(time_hh_mm_ss),
         date_time = dmy_hms(glue::glue("{date_dd_mm_yyyy} {time_hh_mm_ss}"), tz = tz_loc),
         free_space_gb = case_when(
           grepl("GB$", free_space)~as.numeric(str_remove(free_space,"[A-Z]+")),
           grepl("MB$", free_space)~as.numeric(str_remove(free_space,"[A-Z]+"))*0.001,
           TRUE~as.numeric(free_space)*1.25e-10),
         p_Battery_Cap = as.numeric(str_remove(percent_full, "\\%"))/100,
         day_before = date-1,
         day_after=date+1

         )


d_se <- seq(min(wav_names_log$date)-1, max(wav_names_log$date)+1, by = '1 day')

ss <- suncalc::getSunlightTimes(date =d_se , lat = gps_loc$latitude_decimal_degrees,
                                lon = gps_loc$longitude_decimal_degrees, tz = tz_loc,
                                keep = c("sunrise", "sunset"))


rec_log_ss <- wav_names_log %>%
  left_join(ss, by = "date") %>%
  left_join(ss %>%
              rename(sunrise_before = sunrise,
                     sunset_before = sunset)
              , by = c("day_before" = "date")) %>%

  left_join(ss %>%
              rename(sunrise_after = sunrise,
                     sunset_after = sunset)
            , by = c("day_after" = "date")) %>%
  mutate(
    t2sr = int_length(interval(sunrise, date_time))/60,
    t2sr_before = int_length(interval(sunrise_before,date_time))/60,
    t2sr_after = int_length(interval(sunrise_after,date_time))/60,


    t2ss = int_length(interval(sunset, date_time))/60,
    t2ss_before = int_length(interval(sunset_before,date_time))/60,
    t2ss_after = int_length(interval(sunset_after,date_time))/60,
    doy = yday(date)) %>%
  rowwise() %>%
  mutate(t2sr_min = c(t2sr, t2sr_before, t2sr_after)[which.min(c(abs(t2sr), abs(t2sr_before),
                                abs(t2sr_after)))],
         t2ss_min = c(t2ss, t2ss_before, t2ss_after)[which.min(c(abs(t2ss), abs(t2ss_before),
                                abs(t2ss_after)))],

         ) %>% ungroup %>%
  mutate(
  #   Sample_Group = case_when(
  #   (min_to_Sunrise> -90 & min_to_Sunrise < 5*60)~"Dawn",
  #   (min_to_Sunset > -60 & min_to_Sunset < 120)~"Evening",
  #   (min_to_Sunrise > -5*60 & min_to_Sunrise < -30)~"Night",
  #   TRUE~"Other"
  # ),
  week = lubridate::week(date_time),
  dow = lubridate::wday(date_time))


ggplot(rec_log_ss, aes(time_hh_mm_ss, t2sr)) + geom_point()+
  geom_point(aes(y=t2sr_before), colour='blue') +
  geom_point(aes(y=t2sr_after), colour='red') +
  geom_point(aes(y=t2sr_min), colour = 'green')


ggplot(rec_log_ss, aes(time_hh_mm_ss, t2ss)) + geom_point()+
  geom_point(aes(y=t2ss_before), colour='blue') +
  geom_point(aes(y=t2ss_after), colour='red')+
  geom_point(aes(y=t2ss_min), colour = 'green')



ggplot(rec_log_clean, aes(date_time, free_space_gb)) + geom_point()
ggplot(rec_log_clean, aes(date_time, p_Battery_Cap)) + geom_point()




ggplot(rec_log_ss %>% filter(abs(t2sr_min)<180), aes( doy,time_hh_mm_ss, colour = t2sr_min)) + geom_point() +
  scale_colour_gradient2() + theme_dark()


ggplot(rec_log_ss %>% filter(abs(t2ss_min)<180), aes( doy,time_hh_mm_ss, colour = t2ss_min)) + geom_point() +
  scale_colour_gradient2() + theme_dark()


parms_ <- list(min_range = c(-60, 300),
     doy_range = c(150, 350),
     mean_min = 30, sd_min = 30,
     mean_doy = 0, sd_doy = 10,off=0,
     log_ = FALSE, fun = "norm")

sel_pr_morning <-
  rec_log_ss %>%
  calc_sel_pr(ARU_ID_col = ARU_serial, min_col = t2sr_min, day_col = doy, parms = parms_)

hist(sel_pr_morning$psel_doy)

ggplot(sel_pr_morning, aes(doy, t2sr_min, colour = psel_std)) + geom_point() + scale_colour_viridis_c()


