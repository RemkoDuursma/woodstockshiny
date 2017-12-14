
# Some custom functions first.
source("R/dataentry_functions.R")

# Nursery locations
locations <- read.csv("data/nursery_locations.csv")
locations$howmany <- paste(locations$nursery, locations$trees, sep=" ")
locations$popup <- paste(
  '<strong>Nursery:</strong>', capitalize(as.character(locations$nursery)), '<br>',
  '<strong>Number of trees:</strong>', locations$trees
)

# Tree-level data.
treestats <- read.csv("data/tree_stats.csv") %>%
  dplyr::select(volume, sizeindex, leaf_type) %>%
  rename(si = sizeindex) %>%
  filter(volume >= 18)

treestats_small_ever <- filter(treestats, volume < 100, leaf_type=="evergreen")
treestats_small_deci <- filter(treestats, volume < 100, leaf_type=="deciduous")
treestats_large <- filter(treestats, volume >= 100)

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
taus <- seq(0.2,0.8,by=0.2)
qf_small_ever <- lapply(taus, function(x)rq(log10(si) ~ log10(volume), data=treestats_small_ever, tau=x))
qf_small_deci <- lapply(taus, function(x)rq(log10(si) ~ log10(volume), data=treestats_small_deci, tau=x))
qf_large <- lapply(taus, function(x)rq(log10(si) ~ log10(volume), data=treestats_large, tau=x))






