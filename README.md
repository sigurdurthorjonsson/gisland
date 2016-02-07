## gisland

some shapes

### Installing

```
devtools::install_github("einarhjorleifsson/gisland", build_vignettes = TRUE)
library(gisland)
vignette("gisland")
```

### To do:

* skipaflokkur - redo eez, see:
```
ship3 <- 
  eez %>% 
  gDifference(skipaflokkur3)
plot(ship3, col = "grey")
```