Superhero EDA - In Progress
================

  - [Introduction](#introduction)
  - [tl;dr](#tldr)
  - [TMDb API](#tmdb-api)
  - [Packages](#packages)
  - [The Queries Pt 1](#the-queries-pt-1)
      - [Searching for Keywords](#searching-for-keywords)
      - [Superhero Movie Ids](#superhero-movie-ids)
      - [Movie Details](#movie-details)
  - [Super Analysis](#super-analysis)
  - [The Queries Pt 2](#the-queries-pt-2)
  - [The Finale](#the-finale)

## Introduction

It feels as though superhero movies have taken over the box office. It’s
hard to name a popular actor who hasn’t appeared in at least one film
where people can fly or move things with their minds. The goal of this
project is to dig deeper into the growth of superhero movies and
ultimately determine what percentage of popular actors have been in such
films. This analysis is extremely subjective, but it does give me a
great opportunity to utilize the `purrr` package to automate API calls.

All of the code used to conduct this analysis will be available in this
repository. More in-depth explanations of the code used will be
available in various posts on my
website.<https://mhdemoblog.netlify.app/>

## tl;dr

Superhero movies have become more expensive, profitable, and abundant
over the last 15 years. Nearly 1 in every 3 active famous actors has
been in a superhero movie. Stan Lee was a very busy man.

## TMDb API

All of the data used in this analysis, will be queried from The Movie
Database. This site offers an API that is free to access with no rate
limiting. More information can be found on the TMDb API [documentation
page](https://www.themoviedb.org/documentation/api). An unofficial [TMDb
package](https://www.rdocumentation.org/packages/TMDb/versions/1.1) is
also available that provides an R ready interface to the API, but I
chose to create the queries on my own to better understand how they
work.

## Packages

``` r
xfun::pkg_attach("tidyverse", "glue", "jsonlite", "janitor",
                 "lubridate", "scales", "patchwork", "ggfittext",
                 "httr")
```

## The Queries Pt 1

Before I can conduct my analysis I will need to query the data using the
API. So what’s the best way to start?

### Searching for Keywords

First I need to find all of the keywords that contain the word
“superhero” and save the IDS for those keywords.

``` r
keyword_search <- "superhero"

GET(url = "https://api.themoviedb.org/3/search/keyword",
    query = list(api_key = Sys.getenv("THE_MOVIE_DB_KEY"),
                 query = keyword_search)) %>% 
  content(as = "text") %>% 
  fromJSON() %>% 
  pluck("results") %>% 
  head()
```

    ##                 name     id
    ## 1          superhero   9715
    ## 2     superhero team 155030
    ## 3    superhero spoof 157677
    ## 4 death of superhero 174016
    ## 5   masked superhero 180734
    ## 6     superhero kids 191219

``` r
super_hero_key_ids <- GET(url = "https://api.themoviedb.org/3/search/keyword",
    query = list(api_key = Sys.getenv("THE_MOVIE_DB_KEY"),
                 query = keyword_search)) %>% 
  content(as = "text") %>% 
  fromJSON() %>% 
  pluck("results") %>% 
  pull(id)
```

### Superhero Movie Ids

Using the keyword IDS I can search for any movies that contain those
keywords. I just need to determine how many pages of results there are,
create a function that queries the individual pages, and use the `map`
function to iterate over those pages.

``` r
# without_genres=16 - removes movies classified as animation
# with_release_type=3 - filters for theatrical releases
(page_count <- GET(url = "https://api.themoviedb.org/3/discover/movie",
    query = list(api_key = Sys.getenv("THE_MOVIE_DB_KEY"),
                 certification_country = "US",
                 language = "en-US",
                 without_genres = 16,
                 with_release_type = 3,
                 region = "US",
                 with_keywords = glue_collapse(super_hero_key_ids, sep = '|'),
                 primary_release_date.gte = "1970-01-01")) %>% 
  content(as = "text") %>% 
  fromJSON() %>% 
  pluck("total_pages"))
```

    ## [1] 10

``` r
super_discover_query <- function(page_num) {
  GET(url = "https://api.themoviedb.org/3/discover/movie",
      query = list(api_key = Sys.getenv("THE_MOVIE_DB_KEY"),
                   certification_country = "US",
                   language = "en-US",
                   without_genres = 16,
                   with_release_type = 3,
                   region = "US",
                   with_keywords = glue_collapse(super_hero_key_ids, sep = '|'),
                   primary_release_date.gte = "1970-01-01",
                   page = page_num)) %>% 
    content(as = "text") %>% 
    fromJSON() %>% 
    pluck("results")
}

super_discover_query(1) %>%
  glimpse()
```

    ## Rows: 20
    ## Columns: 14
    ## $ popularity        <dbl> 250.661, 240.432, 213.691, 206.743, 179.668, 148.38…
    ## $ vote_count        <int> 3128, 19662, 15031, 254, 8335, 15969, 5651, 5544, 9…
    ## $ video             <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    ## $ poster_path       <chr> "/8WUVHemHFH2ZIP6NWkwlHWsyrEL.jpg", "/7WsyChQLEftFi…
    ## $ id                <int> 338762, 299536, 299534, 340102, 429617, 284054, 399…
    ## $ adult             <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    ## $ backdrop_path     <chr> "/lP5eKh8WOcPysfELrUpGhHJGZEH.jpg", "/bOGkgRGdhrBYJ…
    ## $ original_language <chr> "en", "en", "en", "en", "en", "en", "en", "en", "en…
    ## $ original_title    <chr> "Bloodshot", "Avengers: Infinity War", "Avengers: E…
    ## $ genre_ids         <list> [<28, 18, 878>, <28, 12, 878>, <28, 12, 878>, <28,…
    ## $ title             <chr> "Bloodshot", "Avengers: Infinity War", "Avengers: E…
    ## $ vote_average      <dbl> 6.9, 8.3, 8.3, 5.8, 7.5, 7.4, 7.1, 7.0, 6.2, 6.7, 7…
    ## $ overview          <chr> "After he and his wife are murdered, marine Ray Gar…
    ## $ release_date      <chr> "2020-03-13", "2018-04-27", "2019-04-26", "2020-08-…

The function appears to work as the first two movies visible from the
`title` column are <i>Bloodshot</i> and <i>Avengers: Infinity War</i>.
Next up is the iteration.

``` r
hero_movie_id <- map_df(1:page_count,
                        ~super_discover_query(.x)) %>%
  pull(id)

hero_movie_id %>% 
  glimpse()
```

    ##  int [1:193] 338762 299536 299534 340102 429617 284054 399579 287947 141052 335983 ...

The results from the pages were combined into a single data frame. I
then used the `pull` function again to extract a vector containing the
movie ids. It looks as though there have been 193 movies released in
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

  GET(url = glue("https://api.themoviedb.org/3/movie/{movie_id}"),
      query = list(api_key = Sys.getenv("THE_MOVIE_DB_KEY"),
                   language = "en-US")) %>% 
    content(as = "text") %>% 
    fromJSON()
}

super_detail_query(hero_movie_id[1]) %>%
  glimpse()
```

    ## List of 25
    ##  $ adult                : logi FALSE
    ##  $ backdrop_path        : chr "/lP5eKh8WOcPysfELrUpGhHJGZEH.jpg"
    ##  $ belongs_to_collection: NULL
    ##  $ budget               : int 42000000
    ##  $ genres               :'data.frame':   3 obs. of  2 variables:
    ##   ..$ id  : int [1:3] 28 878 18
    ##   ..$ name: chr [1:3] "Action" "Science Fiction" "Drama"
    ##  $ homepage             : chr "https://www.bloodshot.movie/"
    ##  $ id                   : int 338762
    ##  $ imdb_id              : chr "tt1634106"
    ##  $ original_language    : chr "en"
    ##  $ original_title       : chr "Bloodshot"
    ##  $ overview             : chr "After he and his wife are murdered, marine Ray Garrison is resurrected by a team of scientists. Enhanced with n"| __truncated__
    ##  $ popularity           : num 251
    ##  $ poster_path          : chr "/8WUVHemHFH2ZIP6NWkwlHWsyrEL.jpg"
    ##  $ production_companies :'data.frame':   9 obs. of  4 variables:
    ##   ..$ id            : int [1:9] 34 10246 6573 333 103673 124335 5 1225 30148
    ##   ..$ logo_path     : chr [1:9] "/GagSvqWlyPdkFHMfQ3pNq6ix9P.png" "/rREvQNWAxkDfY9CDn2c5YxEMPdP.png" NA "/5xUJfzPZ8jWJUDzYtIeuPO4qPIa.png" ...
    ##   ..$ name          : chr [1:9] "Sony Pictures" "Cross Creek Pictures" "Mimran Schur Pictures" "Original Film" ...
    ##   ..$ origin_country: chr [1:9] "US" "US" "US" "US" ...
    ##  $ production_countries :'data.frame':   2 obs. of  2 variables:
    ##   ..$ iso_3166_1: chr [1:2] "CN" "US"
    ##   ..$ name      : chr [1:2] "China" "United States of America"
    ##  $ release_date         : chr "2020-03-05"
    ##  $ revenue              : int 30234182
    ##  $ runtime              : int 110
    ##  $ spoken_languages     :'data.frame':   1 obs. of  2 variables:
    ##   ..$ iso_639_1: chr "en"
    ##   ..$ name     : chr "English"
    ##  $ status               : chr "Released"
    ##  $ tagline              : chr "Being a superhero is in his blood"
    ##  $ title                : chr "Bloodshot"
    ##  $ video                : logi FALSE
    ##  $ vote_average         : num 6.9
    ##  $ vote_count           : int 3133

Wow\! This list contains a combination of lists and data frames. I can
see some fields that I’m interested in, such as `budget` and
`vote_average`. I just need to map over the movie IDS and transform
these list elements into a single data frame.

``` r
super_movies <- xfun::cache_rds({
  map(hero_movie_id,
       ~super_detail_query(.x)) %>%
    map_df(~as_tibble(t(.x))) %>%
    select(budget, imdb_id, original_title, title,
           release_date, revenue, runtime, status, vote_average,
           vote_count, id) %>%
    unnest(cols = everything()) %>%
    rename(movie_id = id) %>% 
    mutate(release_date = date(release_date))
})
```

``` r
super_movies %>%
  glimpse()
```

    ## Rows: 193
    ## Columns: 11
    ## $ budget         <int> 42000000, 300000000, 356000000, 67000000, 160000000, 2…
    ## $ imdb_id        <chr> "tt1634106", "tt4154756", "tt4154796", "tt4682266", "t…
    ## $ original_title <chr> "Bloodshot", "Avengers: Infinity War", "Avengers: Endg…
    ## $ title          <chr> "Bloodshot", "Avengers: Infinity War", "Avengers: Endg…
    ## $ release_date   <date> 2020-03-05, 2018-04-25, 2019-04-24, 2020-08-26, 2019-…
    ## $ revenue        <dbl> 30234182, 2046239637, 2797800564, 3100000, 1131927996,…
    ## $ runtime        <int> 110, 149, 181, 94, 129, 134, 122, 132, 120, 112, 124, …
    ## $ status         <chr> "Released", "Released", "Released", "Released", "Relea…
    ## $ vote_average   <dbl> 6.9, 8.3, 8.3, 5.8, 7.5, 7.4, 7.1, 7.0, 6.2, 6.7, 7.0,…
    ## $ vote_count     <int> 3133, 19665, 15035, 254, 8335, 15971, 5655, 5544, 9600…
    ## $ movie_id       <int> 338762, 299536, 299534, 340102, 429617, 284054, 399579…

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

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
super_summ %>%
  filter(metric == "med_roi", release_year < year(Sys.Date())) %>%
  ggplot(aes(release_year, value)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = breaks_width(5)) +
  scale_y_continuous(labels = percent) +
  labs(x = "Release Year", y = "ROI", title = "Median ROI by Year")
```

![](README_files/figure-gfm/unnamed-chunk-10-2.png)<!-- --> There appear
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

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

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
  GET(url = glue("https://api.themoviedb.org/3/movie/{movie_id}/credits"),
      query = list(api_key = Sys.getenv("THE_MOVIE_DB_KEY"))) %>% 
    content(as = "text") %>% 
    fromJSON()
}

super_cc <- xfun::cache_rds({
  map(hero_movie_id, ~cast_query(.x))
})

super_cast <- super_cc %>% 
  map_df(~as_tibble(t(.x))) %>% 
  select(-crew) %>% 
  unnest(c(id, cast), names_repair = "unique") %>% 
  rename(movie_id = 1, actor_id = 6, actor = 7) %>% 
  select(movie_id:order)
```

    ## New names:
    ## * id -> id...1
    ## * id -> id...6

Now that I have the cast data I can join it together with the rest of
the movie data.

``` r
super_data <- super_cast %>% 
  left_join(super_movies_adj,
            by = "movie_id") %>% 
  relocate(movie_id, title, budget_inf_adj, revenue_inf_adj, profit, character)
```

Before I move on to the last step, I’d like to answer a couple more
questions. First up, what actor has been in the most superhero movies?
This question should be pretty easy to answer for any MCU fan.

``` r
super_data %>% 
  count(actor) %>% 
  arrange(desc(n)) %>% 
  head()
```

    ## # A tibble: 6 x 2
    ##   actor                  n
    ##   <chr>              <int>
    ## 1 Stan Lee              43
    ## 2 Chris Evans           13
    ## 3 Samuel L. Jackson     13
    ## 4 Jimmy Star            12
    ## 5 Robert Downey Jr.     10
    ## 6 Scarlett Johansson    10

``` r
super_data %>% 
  filter(actor == "Stan Lee") %>% 
  slice_max(order_by = release_date, n = 1) %>% 
  select(title, release_date, actor, character)
```

    ## # A tibble: 1 x 4
    ##   title             release_date actor    character
    ##   <chr>             <date>       <chr>    <chr>    
    ## 1 Avengers: Endgame 2019-04-24   Stan Lee Driver

Stan Lee has made a cameo in nearly every Marvel film with his last
being <i>Avengers: Endgame</i>. His cameos were always a great touch in
the films. The other actors below Mr. Lee in appearances makes sense
except for one. Who is this Jimmy Star guy? If I dig a little deeper I
might be able to find out.

``` r
jimmy_credits <- GET(url = glue("https://api.themoviedb.org/3/person/1781358/movie_credits"),
    query = list(api_key = Sys.getenv("THE_MOVIE_DB_KEY"),
                 language = "en-US")) %>% 
  content(as = "text") %>% 
  fromJSON() %>% 
  pluck("cast")

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

## The Finale

The last thing that I need is collect a list of popular people as
defined by TMDb.

``` r
pop_pages <- GET(url = "https://api.themoviedb.org/3/person/popular",
    query = list(api_key = Sys.getenv("THE_MOVIE_DB_KEY"),
                 language = "en-US")) %>% 
  content(as = "text") %>% 
  fromJSON() %>% 
  pluck("total_pages")

# Function to query the data from the popular search
pop_query <- function(page_num) {
  GET(url = "https://api.themoviedb.org/3/person/popular",
      query = list(api_key = Sys.getenv("THE_MOVIE_DB_KEY"),
                   language = "en-US",
                   page = page_num)) %>% 
    content(as = "text") %>% 
    fromJSON() %>% 
    pluck("results")
}

# Maps pop_query functions over the total number of a pages
pop_actors <- xfun::cache_rds({
  map_df(.x = c(1:pop_pages),
         ~pop_query(.x))
})

pop_actors %>% 
  select(popularity, name) %>% 
  slice_sample(n = 10)
```

    ##    popularity                 name
    ## 1       5.161        Richard Coyle
    ## 2       4.028        Brett Rickaby
    ## 3       3.787      Benjamin Cattan
    ## 4       3.332           Terry Chen
    ## 5       3.642      Meadow Williams
    ## 6       9.400        Nikolaj Groth
    ## 7       3.779       Tobias Jelinek
    ## 8       6.346         Maia Brewton
    ## 9       3.245            Lana Wood
    ## 10      3.011 Pilar López de Ayala

This list is great but it contains some popular people that I don’t
want. I only want to focus current, popular, movie actors. I decided to
filter for actors who have been in a American movie since 2005 and who
are in the 75th percentile based upon the popularity metric.

``` r
top_pop_perf <- pop_actors %>%
  # Unnest the known_for data frames for each actor
  unnest(known_for, names_repair = "unique") %>% 
  rename(actor = 25, actor_id = 4,
         movie_id = 8) %>% 
  # Removes missing release date rows
  filter(release_date > 0) %>%  
  mutate(release_date = date(release_date)) %>% 
  group_by(actor) %>% 
  # Filters for the oldest known_for item for each actor
  slice_min(order_by = release_date, n = 1) %>% 
  ungroup() %>%
  filter(release_date > "2005-01-01", media_type == "movie", 
         known_for_department == "Acting", 
         original_language == "en") %>%
  # Creates a quantile value for each actor based upon their popularity
  mutate(q_rank = ntile(popularity, 4)) %>% 
  filter(q_rank == 4) %>% 
  select(actor, actor_id)

top_pop_perf %>% 
  nrow()
```

    ## [1] 792

``` r
top_pop_perf %>% 
  slice_sample(n = 10)
```

    ## # A tibble: 10 x 2
    ##    actor                 actor_id
    ##    <chr>                    <int>
    ##  1 Eliza Taylor           1213278
    ##  2 Steve Austin             77120
    ##  3 Olwen Catherine Kelly  1422264
    ##  4 Natalia Dyer           1039011
    ##  5 Cole Sprouse             56730
    ##  6 Gwyneth Paltrow          12052
    ##  7 Cillian Murphy            2037
    ##  8 Lillian Doucet-Roche   1988801
    ##  9 Ritu Arya              1742596
    ## 10 Florence Pugh          1373737

After filtering I’ve narrowed the pool down to the top 734 actors. A
sample of this pool should show more recognizable names.

I can use the setdiff function to identify the popular actors who have
never been in a superhero film.

``` r
non_supers <- top_pop_perf %>% 
  dplyr::setdiff(super_data %>% 
                   distinct(actor, actor_id))

(super_perc <- percent(1 - nrow(non_supers) / nrow(top_pop_perf)))
```

    ## [1] "30%"

``` r
non_supers %>% 
  slice_sample(n = 15)
```

    ## # A tibble: 15 x 2
    ##    actor                  actor_id
    ##    <chr>                     <int>
    ##  1 Dominique McElligott     115146
    ##  2 Tom Glynn-Carney        1765227
    ##  3 Loretta Devine            18284
    ##  4 Amanda Schull             65871
    ##  5 Ruby O. Fee              229396
    ##  6 Andy Samberg              62861
    ##  7 Olwen Catherine Kelly   1422264
    ##  8 Luke Newton             1794961
    ##  9 Paige Turco               74932
    ## 10 Marcia Gay Harden          4726
    ## 11 Tyler Hoechlin            78198
    ## 12 Philip Seymour Hoffman     1233
    ## 13 Natalia Dyer            1039011
    ## 14 Olivia d'Abo              46423
    ## 15 Maggie Grace              11825

``` r
top_pop_perf %>% 
  slice_sample(n = 4)
```

    ## # A tibble: 4 x 2
    ##   actor               actor_id
    ##   <chr>                  <int>
    ## 1 Emily Blunt             5081
    ## 2 Haley Lu Richardson  1286328
    ## 3 Alec Baldwin            7447
    ## 4 Blake Lively           59175

After a little work I have my very rough estimate of how many popular
actors have been in a superhero film, 30%. So roughly 1 out of every 3
popular actors is a pretty big proportion. Throughout this process I
noticed some exceptions that seemed to crop up. Eva Mendes is
categorized as a non-super but she starred in 2007’s <i>Ghost Rider</i>,
which isn’t designated as a superhero film. Subjective factors like this
have influenced every facet of this analysis.
