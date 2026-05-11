# Download datasets
releases <- read_csv(here("data", "viewing_releases.csv"))
TMDb <- read_csv(here("data", "tmdb_data.csv"))
viewing_14_days <- read_csv(here("data", "viewing_14days.csv")) %>% 
  select(imdb_id, viewing_14days)

title_basics <- read_tsv(here("data", "title.basics.tsv")) %>%
  select(tconst, genres)

title_ratings <- read_tsv(here("data", "title.ratings.tsv")) %>%
  select(-numVotes)