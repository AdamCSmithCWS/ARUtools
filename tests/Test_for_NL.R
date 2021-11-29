## Testing ARU process for BarLTs


## 1 Create dummy structure
base_location <- "C:/Users/hoped/Documents/Local_Workspace/Projects/NapkenLake2021Retrevals"
DeplyedPSUs <- sf::st_read(here::here(base_location, "data/spatial/_DEPLOYED2021_/2021-05-12_Napken_Lake_BMS_PSUs.shp"), quiet=T)
DataTracker <- readxl::read_excel(here::here(base_location,"data/xlsx/BlueHeronDeploymentLog_2021Jul07.xlsx"),
                                  sheet = "ARU Deployment") %>%
  dplyr::filter(!is.na(Station))

PSUs <- DeplyedPSUs$PltLbl[DeplyedPSUs$Deploy21>0] %>% stringr::str_extract(., "P\\d\\d")
SSUs <- DataTracker$Plot %>% stringr::str_replace("-", "_")
create_directory_structure(hexagons = PSUs, units = SSUs, base_dir = "D:/ARU_TEST_DIRECTORIES")


folder_base <- "D:/!_TEMP_AUDIO_SAMPLES/ARU_RecordingSample_P7-04/"
list_files <- list.files(folder_base)
list_waves <- list.files(folder_base, pattern = ".wav", recursive = T) |>
  stringr::str_split(pattern = "/") |> purrr::transpose()

gps_file <- "D:/!_TEMP_AUDIO_SAMPLES/ARU_RecordingSample_P7-04/GPS_log.csv"
log_file <- glue::glue("D:/!_TEMP_AUDIO_SAMPLES/ARU_RecordingSample_P7-04/logfile.txt")
rec_lco <- "D:/!_TEMP_AUDIO_SAMPLES/Reclog.csv"

dirs <- list.dirs("D:/ARU_TEST_DIRECTORIES")
for(i in dirs){
  if(grepl("_\\d$", i)){
    file.create(glue::glue("{i}/{list_waves[[2]]}"))
    file.copy(gps_file, glue::glue("{i}/GPS_log.csv"))
    file.copy(log_file, glue::glue("{i}/logfile.txt"))
    file.copy(rec_lco, glue::glue("{i}/Reclog.csv"))
  }
}


test_dir <- "D:/ARU_TEST_DIRECTORIES/P01/P01_1"
options(readr.read_lazy = TRUE)

## Clean for selection

test <- clean_metadata("BarLT", test_dir, "P01_1")
parms_ <- list(min_range = c(-60, 300),
               doy_range = c(150, 350),
               mean_min = 30, sd_min = 30,
               mean_doy = 0, sd_doy = 10,off=0,
               log_ = FALSE, fun = "norm")

loc_dirs <-  list.dirs("D:/ARU_TEST_DIRECTORIES", recursive = T)
loc_dirs <- loc_dirs[grepl("_\\d$", loc_dirs)]



test_si <- calc_sel_pr(test, ARU_ID_col = SiteID, min_col = t2sr_min, day_col = doy, parms = parms_)
all_directories_data <- purrr::map_df(loc_dirs, ~clean_metadata(type = "BarLT", .x ,
                                                               stringr::str_extract(.x, "P\\d\\d_\\d")))

full_si <- calc_sel_pr(all_directories_data,
                       ARU_ID_col = SiteID, min_col = t2sr_min, day_col = doy, parms = parms_)


grts_selction <- fun_aru_samp(full_si[full_si$psel_std>0,], N = 10, os = 0,
                              strat_ = "SiteID",
                              seed = 65461, selprob_id = "psel_std",
                              x = 'doy',y = 't2sr')


fun_aru_samp(df = grts_selction[grts_selction$psel_std>0,],
             N = 12, os =0.2,
             seed = 5735779,
             strat_ = "ARU_ID",
             selprob_id = "psel_tod", x = 'doy', y = 't2sr'
)
