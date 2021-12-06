folder_base <- "D:/!_TEMP_AUDIO_SAMPLES/ARU_RecordingSample_P7-04/"
list_files <- list.files(folder_base)

purrr::map(list_waves[2:10],~soxSpectrogram(sox.file.path = "C:/Program Files (x86)/sox-14-4-2/sox.exe",
                                            file.path =  paste0(folder_base, "/", .x),
                                            out.path = "D:/!_TEMP_AUDIO_SAMPLES/Spect"))



windY <- "D:/!_TEMP_AUDIO_SAMPLES/ARU_RecordingSample_P7-04/20211005_NapkenLk_duskdawn/20211005T124200-0400_SR.wav"
soxSpectrogram(sox.file.path = "C:/Program Files (x86)/sox-14-4-2/sox.exe",
               file.path =  windY,
               out.path = "D:/!_TEMP_AUDIO_SAMPLES/Spect")
t1 <- getvals(filename = list_waves[[1]], fl = folder_base)
t2 <- getvals(filename = windY, fl = "")
wave_ex <- tuneR::readWave(windY, units = "samples")

library(tuneR)
psepcturm <- powspec(wave_ex@left, wave_ex@samp.rate)
play(wave_ex)
rethinking::dens(psepcturm)

w

system()

json_windy <- jsonlite::read_json("D:/TMP_R_Clones/WindNoiseDetection/test/sampletest2.json")
json_botwindy <- jsonlite::read_json("D:/TMP_R_Clones/WindNoiseDetection/test/sampletest3.json")
windless <- jsonlite::read_json("D:/TMP_R_Clones/WindNoiseDetection/test/Windless.json")
windless2 <- jsonlite::read_json("D:/TMP_R_Clones/WindNoiseDetection/test/windless2.json")
windless3 <- jsonlite::read_json("D:/TMP_R_Clones/WindNoiseDetection/test/windless3.json")
js <- jsonlite::read_json(
"D:/TMP_R_Clones/WindNoiseDetection/test/iphone2.json"
)
j <- 0
for(i in length(json_windy$`Wind free regions`)) j <- j + abs(diff(unlist(json_windy$`Wind free regions`[[i]])))
j
k <- 0
for(i in length(json_botwindy$`Wind free regions`)) k <- k + abs(diff(unlist(json_botwindy$`Wind free regions`[[i]])))
k

# purrr::transpose(json_windy$`Wind free regions`) %>%
sum_json <- function(jsonfile){
  tmp <- pluck(jsonfile, "Wind free regions") %>% transpose()
  nm <- pluck(jsonfile, "FileName")
  tibble(s = unlist(pluck(tmp, "s")),
       e = unlist(flatten(pluck(tmp, "e")))) %>%
mutate(t=e-s) %>% summarize(totalwindless = sum(t),
                              pwindless = totalwindless/max(e),
                              n=n(),
                              mean_windless = mean(t),
                            name = nm)
}

# 12 minutes
arufiles <- list.files(path = "D:/!_TEMP_AUDIO_SAMPLES/outputs", pattern = '.json', full.names = T)
loaded_files <- map_df(arufiles, ~sum_json(jsonlite::read_json(.x)))





