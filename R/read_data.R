
locations <- read.csv("data/nursery_locations.csv")
locations$howmany <- paste(locations$nursery, locations$trees, sep=" ")
locations$popup <- paste(
  '<strong>Nursery:</strong>', capitalize(as.character(locations$nursery)), '<br>',
  '<strong>Number of trees:</strong>', locations$trees
)

treestats <- read.csv("data/tree_stats.csv")

si_means <- read.csv("data/size_index_batch_means.csv")

treestats_tab <- dplyr::select(treestats, volume, species, nursery, sizeindex) %>%
  dplyr::mutate(sizeindex = round(sizeindex,1),
                species = capitalize(gsub("_"," ", species))) %>%
  dplyr::arrange(species, volume)

standard_df <- data.frame(x=c(20,2500,2500,20), y=c(24, 1627, 2393, 37), limit=c("min","min","max","max"),
                          value="Standard", stringsAsFactors = FALSE)


