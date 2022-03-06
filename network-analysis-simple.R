## Data Preparation ----

# install packages
install.packages(c("tidyverse", "igraph", "ggraph", "RSocrata", "networkD3"))

# load libraries
library(tidyverse)
library(igraph)
library(ggraph)

# load data from NYC Open Data
contributions <- RSocrata::read.socrata("https://data.cityofnewyork.us/resource/rjkp-yttg.json")

# across all races
contributions_simple <- contributions %>%
  select(recipname, name, c_code, boroughcd, amnt) %>%
  # add filters for later exploration
  filter(amnt > 0) %>%
  # set amount to numeric
  mutate(amnt = as.numeric(amnt)) %>%
  # summarize multiple donations from one individual to the same candidate
  group_by(recipname, name) %>%
  summarize(amnt = sum(amnt)) %>%
  ungroup()

# find individuals that contributed to more than one candidate
double_dippers <- contributions_simple %>%
  group_by(name) %>%
  summarize(n = n()) %>%
  filter(n > 1) %>%
  arrange(desc(n)) %>%
  ungroup()

# save contributions only from individuals that contributed to more than one candidate
network_data <- contributions_simple %>%
  filter(name %in% double_dippers$name) %>%
  rename(
    "candidate" = recipname,
    "contributor" = name,
    "amount" = amnt
  ) %>%
  # only look at large donations
  filter(amount >= 1500)

### Create Network Dataframe

# create a network of shared donations
candidate_network <- unique(network_data$candidate) %>%
  # create matrix of all possible candidate combinations
  combn(2) %>%
  # transpose matrix into long format
  t() %>%
  # make data frame
  data.frame() %>%
  # rename columns
  rename(
    "from" = X1,
    "to" = X2
  ) %>%
  # count length of intersections of candidates from a single contributor
  mutate(donations = map2_dbl(from, to, ~length(intersect(network_data$contributor[network_data$candidate == .x],
                                                          network_data$contributor[network_data$candidate == .y])))) %>%
  # remove rows with no overlapping donations
  filter(donations > 0) %>%
  # arrange in descending order of shared donations
  arrange(desc(donations))

## Network Graph

# visualize
candidate_network %>%
  networkD3::simpleNetwork()

## Network Analysis

### Calculate Centrality

# create graph object
graph_df <- candidate_network %>%
  igraph::graph_from_data_frame(directed = FALSE)

# set centralities as vertex properties
V(graph_df)$degree <- degree(graph_df)
V(graph_df)$betweenness <- betweenness(graph_df)
V(graph_df)$eigen <- eigen_centrality(graph_df)$vector

# visualize node centrality
ggraph(graph_df, layout = "fr") +
  geom_edge_link(color = "gray", alpha = 0.5,
                 show.legend = FALSE) +
  geom_node_point(aes(color = eigen)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  geom_node_text(aes(label = ifelse(eigen > 0.8, name, NA))) +
  labs(color = "Centrality") +
  theme_void()

### Detect Communities

# detect communities using Louvain algorithm
communities <- cluster_louvain(graph_df)

# assign as a vertex property
V(graph_df)$community <- membership(communities)

# number and size of communities detected
sizes(communities)

# visualize communities
ggraph(graph_df, layout = "fr") +
  geom_edge_link(color = "gray", alpha = 0.5, show.legend = FALSE) +
  geom_node_point(aes(color = as.factor(community))) +
  geom_node_text(aes(label = ifelse(community == 1, name, NA))) +
  labs(color = "Community",
       caption = "Community 1 members labelled") +
  theme_void()
