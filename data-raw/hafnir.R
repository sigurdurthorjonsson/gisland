library(dplyr)
library(dplyrOracle)
library(mar)
library(ggmap)
library(gisland)

mar <- src_oracle("mar")
hafnir <-
  mar:::ora_table(mar, "afli.stadur") %>%
  collect()
hafnir$heiti <- stringr::str_trim(hafnir$heiti)
loc <- geocode(hafnir$heiti)
hafnir$lon <- loc$lon
hafnir$lat <- loc$lat

i <- hafnir$heiti == "Brjánslækur"
hafnir$lat[i] <-  geo_convert(653177)
hafnir$lon[i] <- -geo_convert(231152)
i <- hafnir$heiti == "Norðurfjörður"
hafnir$lat[i] <- geo_convert(660305)
hafnir$lon[i] <- -geo_convert(213292)
hafnir %>% filter(is.na(lon))
i <- hafnir$heiti == "Árskógsströnd"
hafnir$lat[i] <- geo_convert(655676)
hafnir$lon[i] <- -geo_convert(182132)
hafnir %>% filter(is.na(lon))
i <- hafnir$heiti == "Hrísey"
hafnir$lat[i] <- geo_convert(655875)
hafnir$lon[i] <- -geo_convert(182282)
hafnir <- hafnir %>% filter(!is.na(lon))
i <- hafnir$heiti == "Rif"
hafnir$lat[i] <- geo_convert(645520)
hafnir$lon[i] <- -geo_convert(234858)
i <- hafnir$heiti == "Djúpavík"
hafnir$lat[i] <- geo_convert(655669)
hafnir$lon[i] <- -geo_convert(213329)
hafnir <-
  hafnir %>%
  filter(!heiti %in% c("Selfoss", "Skútustaðahreppur", "Höfn vantar"))
devtools::use_data(hafnir, overwrite = TRUE)
