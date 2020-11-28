#' @header *********************************************************************
#' @dataset (01) BARTONOVA 2016
#' @header *********************************************************************

# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "Bartonova2016.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = 1:ncol(dataset))


# Run Analysis ----

run_analysis(dataset, name = "bartonova2016")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (02) CLEARLY 2016
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "Clearly2016.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = c(1:10, 15))
dataset <- as_ordinal(dataset, cols = c(11, 12))


# Run Analysis ----

run_analysis(dataset, name = "clearly2016")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (03) RIBERA 2001
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "Ribera2001.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = 1:10)
dataset <- as_categorical(dataset, cols = 11:ncol(dataset))


# Run Analysis ----

run_analysis(dataset, name = "ribera2001")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (04) PAKEMAN 2011
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "Pakeman2011.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = c(3:4, 23:28))
dataset <- as_ordinal(dataset, cols = 5:22)


# Run Analysis ----

run_analysis(dataset, name = "pakeman2011")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (05) GONCALVES 2014
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "Goncalves2014.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_categorical(dataset, cols = 1)
dataset <- as_numerical(dataset, cols = 2:6)


# Run Analysis ----

run_analysis(dataset, name = "goncalves2014")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (06) DIAZ 2008
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "Diaz2008.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_ordinal(dataset, cols = 1:ncol(dataset))


# Run Analysis ----

run_analysis(dataset, name = "diaz2008")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (07) NORTHSEATRAITS
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "northseatraits.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = c(4:5, 7:11, 13:14))
dataset <- as_categorical(dataset, cols = c(1:3, 6))
dataset <- as_ordinal(dataset, cols = 12)


# Run Analysis ----

run_analysis(dataset, name = "northseatraits")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (08) NZ INSECT
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "nz_insect.csv"),
  header    = TRUE
)


# Traits Preparation and Categorization ----

rownames(dataset) <- dataset$"ID_sp"
dataset <- dataset[ , -c(1:5)]

dataset <- as_ordinal(dataset, cols = 1:ncol(dataset))


# Run Analysis ----

run_analysis(dataset, name = "nzinsect")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (09) KRASNOV 2015
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "Krasnov2015.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = 1:6)


# Run Analysis ----

run_analysis(dataset, name = "krasnov2015")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (10) USDA PLANTS                                                    NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "usda_plant_traits.csv"),
  header    = TRUE
)


# Traits Preparation and Categorization ----

dataset <- dataset[ , -c(1:4)]
rownames(dataset) <- dataset[ , 1]
dataset <- dataset[ , -1]

dataset <- as_numerical(dataset, cols = 10:18)
dataset <- as_categorical(dataset, cols = 19:ncol(dataset))


# Run Analysis ----

run_analysis(dataset, name = "usda_plant_traits")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (11) PALM TRAITS                                                    NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "PalmTraits_1.0.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- dataset[ , -c(1:4)]

dataset <- as_numerical(dataset, cols = c(7:8, 11:19))
dataset <- as_categorical(dataset, cols = c(9, 20:ncol(dataset)))


# Run Analysis ----

run_analysis(dataset, name = "palmtraits")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (12) FRESHWATER FISHES                                              NOT RUN
#' @header *********************************************************************



# Read Dataset ----

load(here::here("data", "freshfishtrait.RData"))
dataset <- freshfishtrait
rm(list = "freshfishtrait")


# Remove lines with too much NA (i.e. more than 5) ----

dataset <- dataset[rowSums(is.na(dataset)) < 5, ]


# Run Analysis ----

run_analysis(dataset, name = "freshfishtrait")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (13) BIRDS                                                          NOT RUN
#' @header *********************************************************************



# Read Dataset ----

load(here::here("data", "birdstrait.RData"))
dataset <- birdstrait
rm(list = "birdstrait")


# Run Analysis ----

run_analysis(dataset, name = "birdstrait")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (14) MAMMALS                                                        NOT RUN
#' @header *********************************************************************



# Read Dataset ----

load(here::here("data", "mammalstrait.RData"))
dataset <- mammalstrait
rm(list = "mammalstrait")


# Run Analysis ----

run_analysis(dataset, name = "mammalstrait")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (15) CORAL                                                          NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "coraltraitsv3.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = c(2:5, 13))
dataset <- as_categorical(dataset, cols = c(1, 6:12))

dataset <- dataset[ , -10]
dataset <- dataset[ , -13]


# Run Analysis ----

run_analysis(dataset, name = "coral")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (16) BACTERIA                                                       NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "data_FULL_bacteria.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_categorical(dataset, cols = 1)


# Run Analysis ----

run_analysis(dataset, name = "bacteria")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (17) EALLONARDO 2013                                                NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "Eallonardo2013.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = 6:11)
dataset <- as_categorical(dataset, cols = 1:5)


# Run Analysis ----

run_analysis(dataset, name = "eallonardo2013")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (18) YATES 2014                                                     NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "Yates2014.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = 1:11)
dataset <- as_ordinal(dataset, cols = 11)


# Run Analysis ----

run_analysis(dataset, name = "yates2014")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (19) JELIAZKOV 2013                                                 NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "Jeliazkov2013.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_ordinal(dataset, cols = 1:89)


# Run Analysis ----

run_analysis(dataset, name = "jeliazkov2013")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (20) GIBB 2015                                                      NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "Gibb2015.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = 1:10)


# Run Analysis ----

run_analysis(dataset, name = "gibb2015")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (21) THERMAL VENT FAUNA                                             NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "Thermal Vent Fauna.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_ordinal(dataset, cols = c(1, 3:8, 13:16))


# Run Analysis ----

run_analysis(dataset, name = "thermal_fauna")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (22) PLANTS ALPS                                                    NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "plant_alps.csv"),
  header    = TRUE,
  row.names = 1
)


# Remove traits with too much NA (> 60 %) ----

dataset <- dataset[ , (colSums(is.na(dataset)) < nrow(dataset) * 0.6)]


# Remove line with too much NA (> 50%) ----

dataset <- dataset[(rowSums(is.na(dataset)) < ncol(dataset) * 0.5), ]


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = c(18, 28:30))
dataset <- as_ordinal(dataset, cols = c(1:3, 5, 7, 10, 13, 15, 17, 20:27, 31:33), level = FALSE)
dataset <- as_ordinal(dataset, cols = c(4, 6, 8, 9, 11:12, 14, 16, 19))


# Run Analysis ----

run_analysis(dataset, name = "plant_alps")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (23) CHONDRICHTYENS                                                 NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "Chondrichtyens_traits.csv"),
  header    = TRUE,
  row.names = 1
)


# Remove traits with too much NA ----

dataset <- dataset[ , (colSums(is.na(dataset)) < 672)]


# Remove line with too much NA ----

dataset <- dataset[(rowSums(is.na(dataset)) < 7), ]


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = c(1:7, 11:14))
dataset <- as_categorical(dataset, cols = 8:10)


# Run Analysis ----

run_analysis(dataset, name = "chondrichtyens")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (24) PHYTOPLANKTON                                                  NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "Phytoplankton.csv"),
  header    = TRUE,
  row.names = 1
)


# Remove traits with too much NA ----

dataset <- dataset[ , (colSums(is.na(dataset)) < nrow(dataset) * 0.6)]


# Remove line with too much NA ----

dataset <- dataset[(rowSums(is.na(dataset)) < ncol(dataset) * 0.5), ]


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = c(1:3, 5:7))
dataset <- as_categorical(dataset, cols = c(4, 8:15))


# Run Analysis ----

run_analysis(dataset, name = "phytoplankton")
rm(list = "dataset")
