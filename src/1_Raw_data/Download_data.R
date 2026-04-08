# Download datasets
releases <- read_csv('viewing_releases.csv')
TMDb <- read_csv('tmdb_data.csv')
title_basics <- read_tsv('title.basics.tsv') %>% select('tconst', 'titleType', 'genres')
title_ratings <- read_tsv('title.ratings.tsv') %>% select(-'numVotes')