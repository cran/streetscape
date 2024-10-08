## ----eval = FALSE-------------------------------------------------------------
#  bbox <- c(-83.751812,42.272984,-83.741255,42.279716)
#  data <- streetscape::strview_searchByGeo(bbox = bbox,
#                                           epsg = 2253,
#                                           token = "")
#  

## ----eval = FALSE-------------------------------------------------------------
#  data <- streetscape::strview_searchByGeo(x = -83.743460634278,
#                                           y = 42.277848830294,
#                                           r = 100,
#                                           epsg = 2253,
#                                           token = "")

## ----eval = FALSE-------------------------------------------------------------
#  # check supported filters
#  streetscape::available_filter()
#  # only search for 360-degree street views
#  data <- streetscape::strview_searchByGeo(bbox = bbox,
#                                           epsg = 2253,
#                                           token = "",
#                                           is_pano = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  data <- streetscape::strview_search_nnb(
#    x = -83.743460634278,
#    y = 42.277848830294,
#    epsg = 2253,
#    token = '')

## ----eval = FALSE-------------------------------------------------------------
#  bbox <- c(-83.752041,42.274896,-83.740711,42.281945)
#  data <- streetscape::strview_search_osm(
#          bbox = bbox,
#          epsg = 2253,
#          token = '',
#          size = 100)

## ----eval = FALSE-------------------------------------------------------------
#  data$gvi()

## ----eval = FALSE-------------------------------------------------------------
#  streetviewdata <- streetscape::scdataframe
#  # calculate the percentage of each segmentation
#  data$decodeDetection()
#  data$data$segmentation[[1]]
#  # extract the semantic segmentation of a street view
#  mask <- streetviewdata$get_mask(1)

## ----eval = FALSE-------------------------------------------------------------
#  map1 <- data$mapPreview('meta')
#  print(map1)
#  # assume that one has run data$gvi() and data$decodeDetection()
#  map2 <- data$mapPreview('seg')
#  print(map2)
#  map3 <- data$mapPreview('gvi')
#  print(map3)

## ----eval = FALSE-------------------------------------------------------------
#  # download street view images
#  data$download_data(path = 'path/to/download', items = 'image')
#  # download images and masks in sf format
#  data$download_data(path = 'path/to/download', items = c('image', 'mask'))

## ----eval = FALSE-------------------------------------------------------------
#  # general survey for understanding subjective perception from streetviews
#  questions <- c('1. To what extent you feel pleasant if you were in this environment',
#                '2. To what extent you feel safe if you were in this environment')
#  choices <- list(c('Unpleasant','Less pleasant', 'Pleasant', 'More pleasant'),
#                  c('Unsafe', 'Less safe','Safe','Safer'))
#  header <- "Please review the following picture(s):"
#  streetscape::strview2rate(data, header, questions, choices, file = 'folder/filename')
#  
#  # pair-wised comparison survey for ranking some specific property (such as perceived safety) of street views
#  questions <- c('which one is more beautiful?', 'which one is safer?')
#  streetscape::strview2pwc(data, k=1, header, questions, file = 'folder/filename')

