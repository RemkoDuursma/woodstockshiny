
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

treestats_small <- filter(treestats, volume < 100)
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
taus_all <- seq(0.05,0.95,by=0.05)
qf_small_ever_all <- lapply(taus_all, function(x)rq(log10(si) ~ log10(volume), data=treestats_small_ever, tau=x))
qf_small_deci_all <- lapply(taus_all, function(x)rq(log10(si) ~ log10(volume), data=treestats_small_deci, tau=x))
qf_small_all <- lapply(taus_all, function(x)rq(log10(si) ~ log10(volume), data=treestats_small, tau=x))
qf_large_all <- lapply(taus_all, function(x)rq(log10(si) ~ log10(volume), data=treestats_large, tau=x))


taus_plot <- c(0.1, 0.25, 0.75, 0.9)
qf_small_ever_plot <- lapply(taus_plot, function(x)rq(log10(si) ~ log10(volume), data=treestats_small_ever, tau=x))
qf_small_deci_plot <- lapply(taus_plot, function(x)rq(log10(si) ~ log10(volume), data=treestats_small_deci, tau=x))
qf_small_plot <- lapply(taus_plot, function(x)rq(log10(si) ~ log10(volume), data=treestats_small, tau=x))
qf_large_plot <- lapply(taus_plot, function(x)rq(log10(si) ~ log10(volume), data=treestats_large, tau=x))



