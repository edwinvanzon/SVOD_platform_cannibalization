# Download datasets
releases <- read_csv(here("data", "viewing_releases.csv"))
TMDb <- read_csv(here("data", "tmdb_data.csv"))

title_basics <- read_tsv(here("data", "title.basics.tsv")) %>%
  select(tconst, genres)

title_ratings <- read_tsv(here("data", "title.ratings.tsv")) %>%
  select(-numVotes)
