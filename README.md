Superhero EDA - In Progress
================

  - [Introduction](#introduction)
  - [TMDb API](#tmdb-api)
  - [Packages](#packages)
  - [The Queries Pt 1](#the-queries-pt-1)
      - [Searching for Keywords](#searching-for-keywords)
      - [Superhero Movie Ids](#superhero-movie-ids)
      - [Movie Details](#movie-details)
  - [Super Analysis](#super-analysis)
  - [The Queries Pt 2](#the-queries-pt-2)

## Introduction

It feels as though superhero movies have taken over the box office. It’s
hard to name a popular performer who hasn’t appeared in at least one
film where people can fly or move things with their minds. The goal of
this project is to dig deeper into the growth of superhero movies and
ultimately determine what percentage of popular performers have been in
such films. This analysis is extremely subjective, but it does give me a
great opportunity to utilize the `purrr` package to automate API calls.

All of the code used to conduct this analysis will be available in this
repository. More in-depth explanations of the code used will be
available in various posts on my
website.<https://mhdemoblog.netlify.app/>

## TMDb API

All of the data used in this analysis, will be queried from The Movie
Database. This site offers an API that is free to access with no rate
limiting. More information on the TMDb API can be found on their fact
page.

[TBDb API Documentation](https://www.themoviedb.org/documentation/api)

## Packages

``` r
library(tidyverse)
library(glue)
library(jsonlite)
library(janitor)
library(lubridate)
library(scales)
library(patchwork)
library(ggfittext)
```

## The Queries Pt 1

Before I can conduct my analysis I will need to query the data using the
API. So what’s the best way to start?

### Searching for Keywords

First I need to find all of the keywords that contain the word
“superhero” and save the IDS for those keywords.

``` r
keyword_search <- "superhero"

fromJSON(glue("https://api.themoviedb.org/3/search/keyword?\\
                api_key={Sys.getenv('THE_MOVIE_DB_KEY')}\\
                &query={keyword_search}")) %>%
  pluck(2) %>% 
  head()
```

    ##                 name     id
    ## 1          superhero   9715
    ## 2       superheroine  10843
    ## 3     superhero team 155030
    ## 4    superhero spoof 157677
    ## 5 death of superhero 174016
    ## 6   masked superhero 180734

``` r
super_hero_key_ids <- fromJSON(glue("https://api.themoviedb.org/3/search/keyword?\\
                api_key={Sys.getenv('THE_MOVIE_DB_KEY')}\\
                &query={keyword_search}")) %>%
  pluck(2) %>% 
  pull(2)
```

### Superhero Movie Ids

Using the keyword IDS I can search for any movies that contain those
keywords. I just need to determine how many pages of results there are,
create a function that queries the individual pages, and use the `map`
function to iterate over those pages.

``` r
# without_genres=16 - removes movies classified as animation
# with_release_type=3 - filters for theatrical releases
(page_count <- fromJSON(glue("https://api.themoviedb.org/3/discover/movie?\\
                       api_key={Sys.getenv('THE_MOVIE_DB_KEY')}\\
                       &certification_country=US&language=en-US&\\
                       without_genres=16&\\
                       with_release_type=3\\
                       &region=US\\
                       &with_keywords=\\
                       {glue_collapse(super_hero_key_ids, sep = '|')}\\
                       &primary_release_date.gte=1970-01-01")) %>%
  pluck(3))
```

    ## [1] 10

``` r
super_discover_query <- function(page_num) {
  fromJSON(glue("https://api.themoviedb.org/3/discover/movie?\\
                       api_key={Sys.getenv('THE_MOVIE_DB_KEY')}\\
                       &certification_country=US&language=en-US&\\
                       without_genres=16&\\
                       with_release_type=3\\
                       &region=US\\
                       &with_keywords=\\
                       {glue_collapse(super_hero_key_ids, sep = '|')}\\
                       &primary_release_date.gte=1970-01-01&page={page_num}")) %>%
    pluck(4)
}

super_discover_query(1) %>%
  glimpse()
```

    ## Rows: 20
    ## Columns: 14
    ## $ popularity        <dbl> 226.101, 209.943, 206.434, 205.109, 174.033, 140.46…
    ## $ vote_count        <int> 116, 15854, 3072, 19551, 14852, 8235, 10002, 9560, …
    ## $ video             <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    ## $ poster_path       <chr> "/45FNxAIooJFqjsVaCJts9YJHXS4.jpg", "/uxzzxijgPIY7s…
    ## $ id                <int> 340102, 284054, 338762, 299536, 299534, 429617, 299…
    ## $ adult             <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    ## $ backdrop_path     <chr> "/eCIvqa3QVCx6H09bdeOS8Al2Sqy.jpg", "/6ELJEzQJ3Y45H…
    ## $ original_language <chr> "en", "en", "en", "en", "en", "en", "en", "en", "en…
    ## $ original_title    <chr> "The New Mutants", "Black Panther", "Bloodshot", "A…
    ## $ genre_ids         <list> [<28, 12, 27, 878>, <28, 12, 14, 878>, <28, 878>, …
    ## $ title             <chr> "The New Mutants", "Black Panther", "Bloodshot", "A…
    ## $ vote_average      <dbl> 6.0, 7.4, 7.0, 8.3, 8.3, 7.5, 7.0, 6.2, 7.6, 7.1, 7…
    ## $ overview          <chr> "Five young mutants, just discovering their abiliti…
    ## $ release_date      <chr> "2020-08-28", "2018-02-16", "2020-03-13", "2018-04-…

The function appears to work as the first two movies visible from the
`title` column are <i>The New Mutants</i> and <i>Black Panther</i>. Next
up is the iteration.

``` r
hero_movie_id <- map_df(1:page_count,
                        ~super_discover_query(.x)) %>%
  pull(id)

hero_movie_id %>% 
  glimpse()
```

    ##  int [1:197] 340102 284054 338762 299536 299534 429617 299537 141052 284053 399579 ...

The results from the pages were combined into a single data frame. I
then used the `pull` function again to extract a vector containing the
movie ids. It looks as though there have been 197 movies released in
American theaters since Jan 1, 1970 that have been classified as
superhero films.

### Movie Details

I could use the rest of the data found in the discover API call, but I
want to know more. Have the budgets for superhero movies changed since
1970? Have they become more profitable? Additional information on these
movies’ cost and performance can be found using a slightly different API
call. Querying the data requires a similar process as collecting all of
the movie IDS.

``` r
super_detail_query <-  function(movie_id) {

  fromJSON(glue("https://api.themoviedb.org/3/movie/{movie_id}?\\
                api_key={Sys.getenv('THE_MOVIE_DB_KEY')}&language=en-US"))
}

super_detail_query(hero_movie_id[1]) %>%
  glimpse()
```

    ## List of 25
    ##  $ adult                : logi FALSE
    ##  $ backdrop_path        : chr "/eCIvqa3QVCx6H09bdeOS8Al2Sqy.jpg"
    ##  $ belongs_to_collection: NULL
    ##  $ budget               : int 67000000
    ##  $ genres               :'data.frame':   4 obs. of  2 variables:
    ##   ..$ id  : int [1:4] 28 878 27 12
    ##   ..$ name: chr [1:4] "Action" "Science Fiction" "Horror" "Adventure"
    ##  $ homepage             : chr "https://www.foxmovies.com/movies/the-new-mutants"
    ##  $ id                   : int 340102
    ##  $ imdb_id              : chr "tt4682266"
    ##  $ original_language    : chr "en"
    ##  $ original_title       : chr "The New Mutants"
    ##  $ overview             : chr "Five young mutants, just discovering their abilities while held in a secret facility against their will, fight "| __truncated__
    ##  $ popularity           : num 226
    ##  $ poster_path          : chr "/45FNxAIooJFqjsVaCJts9YJHXS4.jpg"
    ##  $ production_companies :'data.frame':   3 obs. of  4 variables:
    ##   ..$ id            : int [1:3] 7505 127928 25
    ##   ..$ logo_path     : chr [1:3] "/837VMM4wOkODc1idNxGT0KQJlej.png" "/qAh0Ofz47KVy0A3118rjHUx3usk.png" "/qZCc1lty5FzX30aOCVRBLzaVmcp.png"
    ##   ..$ name          : chr [1:3] "Marvel Entertainment" "20th Century Studios" "20th Century Fox"
    ##   ..$ origin_country: chr [1:3] "US" "US" "US"
    ##  $ production_countries :'data.frame':   1 obs. of  2 variables:
    ##   ..$ iso_3166_1: chr "US"
    ##   ..$ name      : chr "United States of America"
    ##  $ release_date         : chr "2020-08-26"
    ##  $ revenue              : int 3100000
    ##  $ runtime              : int 94
    ##  $ spoken_languages     :'data.frame':   3 obs. of  2 variables:
    ##   ..$ iso_639_1: chr [1:3] "es" "fr" "en"
    ##   ..$ name     : chr [1:3] "Español" "Français" "English"
    ##  $ status               : chr "Released"
    ##  $ tagline              : chr "It's time to face your demons"
    ##  $ title                : chr "The New Mutants"
    ##  $ video                : logi FALSE
    ##  $ vote_average         : num 5.9
    ##  $ vote_count           : int 127

Wow\! This list contains a combination of lists and data frames. I can
see some fields that I’m interested in, such as `budget` and
`vote_average`. I just need to map over the movie IDS and transform
these list elements into a single data frame.

``` r
super_movies <- map(hero_movie_id,
                        ~super_detail_query(.x)) %>%
  map_df(~as_tibble(t(.x))) %>%
  select(budget, imdb_id, original_title, title,
         release_date, revenue, runtime, status, vote_average,
         vote_count, id) %>%
  unnest(cols = everything()) %>%
  rename(movie_id = id) %>% 
  mutate(release_date = date(release_date))

# Used when working on the analysis to speed up the knit process
# saveRDS(super_movies, "Data/superMovies.RDS")
```

``` r
super_movies %>%
  glimpse()
```

    ## Rows: 197
    ## Columns: 11
    ## $ budget         <int> 200000000, 42000000, 300000000, 356000000, 67000000, 1…
    ## $ imdb_id        <chr> "tt1825683", "tt1634106", "tt4154756", "tt4154796", "t…
    ## $ original_title <chr> "Black Panther", "Bloodshot", "Avengers: Infinity War"…
    ## $ title          <chr> "Black Panther", "Bloodshot", "Avengers: Infinity War"…
    ## $ release_date   <date> 2018-02-13, 2020-03-05, 2018-04-25, 2019-04-24, 2020-…
    ## $ revenue        <dbl> 1346739107, 30234182, 2046239637, 2797800564, 3100000,…
    ## $ runtime        <int> 134, 110, 149, 181, 94, 129, 124, 122, 131, 114, 120, …
    ## $ status         <chr> "Released", "Released", "Released", "Released", "Relea…
    ## $ vote_average   <dbl> 7.4, 7.0, 8.3, 8.3, 6.0, 7.5, 7.0, 7.1, 7.6, 6.1, 6.2,…
    ## $ vote_count     <int> 15849, 3072, 19549, 14851, 113, 8232, 10000, 5597, 145…
    ## $ movie_id       <int> 284054, 338762, 299536, 299534, 340102, 429617, 299537…

This looks much easier to interpret. All of the information that I want
is in a nice data frame and ready for some exploratory data analysis.

## Super Analysis

How many superhero movies have been released per year since 1970? Have
these movies become more profitable over time? I’ll attempt to answer
these questions with some simple plots. I’ve also chosen to adjust the
financial figures for inflation.

``` r
super_movies %>%
  mutate(release_year = year(release_date)) %>%
  filter(release_year < year(Sys.Date())) %>%
  count(release_year) %>%
  ggplot(aes(release_year, n)) +
  geom_line() + geom_point() +
  scale_y_continuous(breaks = breaks_width(1)) +
  scale_x_continuous(breaks = breaks_width(5)) +
  theme(panel.grid.minor.y = element_blank()) +
  labs(x = "Release Year", title = "Superhero Movies Released by Year")
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

There has definitely been an upward trend in the number of superhero
movies released per year. More of these types of films start to pop up
around the mid 2000s. But were these newer superhero films more
successful than their older counterparts?

``` r
cpi_data <- read_csv("Data/cpi_data.csv")
curr_cpi <- cpi_data %>% 
  tail(1) %>% 
  pull(avg_cpi)
cpi_data <- cpi_data %>% 
  mutate(inf_rate = curr_cpi / avg_cpi) %>% 
  select(-avg_cpi)

super_movies_adj <- super_movies %>% 
  mutate(release_year = year(release_date)) %>%
  left_join(cpi_data, by = c("release_year" = "year")) %>% 
  mutate(across(.cols = c(budget, revenue),
                .fns = ~ inf_rate * .x,
                .names = "{col}_inf_adj"),
         profit = revenue_inf_adj - budget_inf_adj,
         roi = profit / budget_inf_adj)

super_summ <- super_movies_adj %>%
  filter(release_date < Sys.Date(), budget_inf_adj > 100000) %>%
  mutate(profit = revenue_inf_adj - budget_inf_adj,
         roi = profit / budget_inf_adj) %>%
  group_by(release_year) %>%
  summarize(across(.cols = c(budget_inf_adj, revenue_inf_adj, profit, roi),
                   .fns = median,
                   .names = "med_{col}"),
            .groups = "drop") %>%
  pivot_longer(cols = contains("med"), names_to = "metric")

super_summ %>%
  filter(metric != "med_roi", release_year < year(Sys.Date())) %>%
  ggplot(aes(release_year, value, col = metric)) +
  geom_line() + geom_point() +
  scale_y_continuous(labels = dollar, breaks = breaks_width(250000000)) +
  scale_x_continuous(breaks = breaks_width(5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Release Year", col = "Metric", title = "Median Performance by Year")
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
super_summ %>%
  filter(metric == "med_roi", release_year < year(Sys.Date())) %>%
  ggplot(aes(release_year, value)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = breaks_width(5)) +
  scale_y_continuous(labels = percent) +
  labs(x = "Release Year", y = "ROI", title = "Median ROI by Year")
```

![](README_files/figure-gfm/unnamed-chunk-11-2.png)<!-- --> There appear
to be some early superhero films that performed great at the box office
and offered investors some amazing returns. However, the highest returns
seem to be held by movies released after 2012. There also appear to be
less occurrences of annual losses for this genre after 2012. This could
be a product of studios’ improved understanding of how to produce and
market these types of movies.

Before I move on to collecting the cast information, I’d like to know
what the top performing movie of each decade was.

``` r
super_movies_adj %>% 
  filter(budget > 100000, release_date < Sys.Date()) %>% 
  mutate(decade = glue("{release_year - (release_year %% 10)}'s")) %>% 
  group_by(decade) %>% 
  slice_max(order_by = profit,
            n = 1) %>% 
  ggplot(aes(decade, profit, label = glue("{title}\n{percent(roi)} ROI"))) + geom_col() +
  geom_bar_text(reflow = TRUE) +
  scale_y_continuous(labels = dollar) +
  labs(x = "Decade", y = "Profit")
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

This plot is pretty interesting. <i>Avengers: Endgame</i> is by far the
greatest superhero success at the box office, but 1989’s <i>Batman</i>
offered investors a higher rate of return at a whopping 1,075 % ROI. The
2020s have only shown negative returns for superhero films at the time
of this analysis.

## The Queries Pt 2

The movie data that I’ve collected so far is great, but it lacks any
information on the cast for each film. I’ll have to use a slightly
different set of API calls to collect all of the cast listing for the
movies I’ve identified.

``` r
cast_query <- function(movie_id) {
  
  fromJSON(glue("https://api.themoviedb.org/3/movie/{movie_id}\\
                /credits?api_key={Sys.getenv('THE_MOVIE_DB_KEY')}"))
}

super_cc <- map(hero_movie_id, ~cast_query(.x))

super_cast <- super_cc %>% 
  map_df(~as_tibble(t(.x))) %>% 
  select(-crew) %>% 
  unnest(c(id, cast), names_repair = "unique") %>% 
  rename(movie_id = 1, performer_id = 6, performer = 7) %>% 
  select(movie_id:order)

# Used when working on the analysis to speed up the knit process
# saveRDS(super_cast, "Data/super_cast.RDS")
# rm(super_cc)
```

Now that I have the cast data I can join it together with the rest of
the movie data.

``` r
super_data <- super_cast %>% 
  left_join(super_movies_adj,
            by = "movie_id") %>% 
  relocate(movie_id, title, budget_inf_adj, revenue_inf_adj, profit, character)
```

Before I move on to the last step, I’d like to answer a couple more
questions. First up, what performer has been in the most superhero
movies? This question should be pretty easy to answer for any MCU fan.

``` r
super_data %>% 
  count(performer) %>% 
  arrange(desc(n)) %>% 
  head()
```

    ## # A tibble: 6 x 2
    ##   performer              n
    ##   <chr>              <int>
    ## 1 Stan Lee              43
    ## 2 Chris Evans           13
    ## 3 Samuel L. Jackson     13
    ## 4 Jimmy Star            12
    ## 5 Robert Downey Jr.     11
    ## 6 Scarlett Johansson    10

``` r
super_data %>% 
  filter(performer == "Stan Lee") %>% 
  slice_max(order_by = release_date, n = 1) %>% 
  select(title, release_date, performer, character)
```

    ## # A tibble: 1 x 4
    ##   title             release_date performer character
    ##   <chr>             <date>       <chr>     <chr>    
    ## 1 Avengers: Endgame 2019-04-24   Stan Lee  Driver

Stan Lee has made a cameo in nearly every Marvel film with his last
being <i>Avengers: Endgame</i>. His cameos were always a great touch in
the films. The other performers below Mr. Lee in appearances makes sense
except for one. Who is this Jimmy Star guy? If I dig a little deeper I
might be able to find out.

``` r
jimmy_credits <- pluck(fromJSON(glue("https://api.themoviedb.org/3\\
                               /person/1781358/movie_credits?\\
                               api_key={Sys.getenv('THE_MOVIE_DB_KEY')}&\\
                               language=en-US")), 1)

nrow(jimmy_credits)
```

    ## [1] 86

``` r
jimmy_credits %>% 
  arrange(desc(vote_average)) %>% 
  select(title, release_date, character, vote_average) %>% 
  head()
```

    ##                     title release_date                      character
    ## 1            Pulp Fiction   1994-09-10 Restaurant Patron (uncredited)
    ## 2                Scarface   1983-12-08       Boy at Pool (uncredited)
    ## 3                    Heat   1995-12-15 Restaurant Patron (uncredited)
    ## 4 Guardians of the Galaxy   2014-07-30          Prisoner (uncredited)
    ## 5     Catch Me If You Can   2002-12-25 Airline Passenger (uncredited)
    ## 6  Straight Outta Compton   2015-08-13      Record Mogul (uncredited)
    ##   vote_average
    ## 1          8.5
    ## 2          8.1
    ## 3          7.9
    ## 4          7.9
    ## 5          7.9
    ## 6          7.8

Mr. Star appears to be a very active extra. He’s had 12 appearances to
date in superhero films and 86 across all genres\! Above are the top
five movies that he’s appeared in by vote average.
