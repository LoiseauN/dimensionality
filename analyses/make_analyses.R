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
#' @dataset (10) USDA PLANTS
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
#' @dataset (11) PALM TRAITS
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "PalmTraits_1.0.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = c(7:8, 11:19))
dataset <- as_categorical(dataset, cols = c(9, 20:ncol(dataset)))


# Run Analysis ----

run_analysis(dataset, name = "palmtraits")
rm(list = "dataset")
