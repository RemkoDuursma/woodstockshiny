
# Some custom functions first.
source("R/dataentry_functions.R")
source("R/functions_plotting.R")

# Nursery locations
locations <- read.csv("data/nursery_locations.csv")
locations$howmany <- paste(locations$nursery, locations$trees, sep=" ")
locations$popup <- paste(
  '<strong>Nursery:</strong>', capitalize(as.character(locations$nursery)), '<br>',
  '<strong>Number of trees:</strong>', locations$trees
)

# Tree-level data.
treestats <- read.csv("data/size_index_raw.csv") %>%
  dplyr::select(volume, sizeindex, leaf_type) %>%
  rename(si = sizeindex) %>%
  filter(volume >= 18)

# Batch-level data/
si_means <- read.csv("data/size_index_batch_means.csv")


# treestats_tab <- dplyr::select(treestats, volume, species, nursery, sizeindex) %>%
#   dplyr::mutate(sizeindex = round(sizeindex,1),
#                 species = capitalize(gsub("_"," ", species))) %>%
#   dplyr::arrange(species, volume)

# Old standard
standard_df <- data.frame(x=c(20,2500,2500,20), y=c(24, 1627, 2393, 37), limit=c("min","min","max","max"),
                          value="Standard", stringsAsFactors = FALSE)


# Quantile regressions
taus_plot <- c(0.05, 0.25, 0.75, 0.95)
qf_plot <- lapply(taus_plot, function(x)rq(log10(si) ~ log10(volume), data=treestats, tau=x))


# For plotting, and checking ranges
x_range_large <- c(100,3000)
y_range_large <- c(50,3000)
x_range_small <- c(18,100)
y_range_small <- c(8,200)




