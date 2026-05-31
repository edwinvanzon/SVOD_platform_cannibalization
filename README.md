# Competing for Attention: How Release Volume Shapes Title Performance on SVOD Platforms

Master Thesis — Marketing Analytics, Tilburg University (Spring 2026)
Author: Edwin van Zon | Supervisor: B. Krijger

## Overview
This repository contains all code used for the empirical analysis in my master thesis. The study examines how release density within a weekly release window affects individual title viewership on SVOD platforms (Netflix, Hulu, Amazon), with title type (original vs. licensed) and content variety (genre diversity) as moderating variables. Data is sourced from Trakt.tv, IMDb, TMDb, and web-scraped release data.

## Repository structure
```
├── data/                        # Raw data files (excluded via .gitignore)
├── src/
│   ├── 1_Raw_data/
│   │   ├── Install_and_loading_packages.R
│   │   └── Download_data.R
│   ├── 2_data_preparation/
│   │   ├── Data_preparation.R
│   │   └── Data_exploration.R
│   ├── 3_analysis/
│   │   ├── Assumptions.R
│   │   ├── Model.R
│   │   └── RobustnessCheck.R
│   └── output/
├──TMDB.ipynb
└──.gitignore
              
```
         
## Reproducing the analysis
1. Install R dependencies by running src/1_Raw_data/Install_and_loading_packages.R
2. Place the raw data files in the data/ folder
3. Run src/2_data_preparation/Data_preparation.R to construct the analysis dataset
4. Run src/2_data_preparation/Data_exploration.R for descriptive statistics and figures
5. Run the scripts in src/3_analysis/ in order: Assumptions → Model → RobustnessCheck

## Data sources
| Source | Contents | Access |
|---|---|---|
| Trakt.tv | Viewing figures (888 US titles, Dec 2019 – Jan 2021) | Provided by supervisor |
| Web scraper | Release dates, platform, original/licensed flag | Provided by supervisor |
| IMDb (non-commercial) | Genres, ratings | [imdb.com/interfaces](https://www.imdb.com/interfaces/) |
| TMDb API | Movie vs. series classification | [themoviedb.org](https://www.themoviedb.org/documentation/api) |

The TMDb media type data was collected seperately using **TMDB.ipynb**, which requires a TMDb API key
