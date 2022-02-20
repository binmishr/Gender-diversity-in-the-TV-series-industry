# Gender-diversity-in-the-TV-series-industry

The details of the codeset and plots are included in the attached Microsoft Word Document (.docx) file in this repository. 
You need to view the file in "Read Mode" to see the contents properly after downloading the same.

# Gender diversity in the film industry


The year 2017 has completely turned the film industry upside down. The allegations of harassment and sexual assault against Harvey Weinstein have raised the issue of the sexism and misogyny among this industry to the eyes of the general public. In addition, it helped raise the topic of the **gender non-diversity and the under-representation of women in Hollywood**. One of the main problem posed by the weak presence of women behind the camera is that this is reflected in the fictional characters on screen: lots of movies portray women in an incomplete, stereotyped and biased way.  

This project focuses on some key **behind-the-camera roles** to measure the evolution of their gender diversity in the last decade, from 2007 until 2017. The roles I studied were the following: **directors, writers, producers, sound teams, music teams, art teams, makeup teams and costume teams**. 

All the data used for this project was gathered from the **IMDb website**. The main objects I created and which gather the information I used to proceed with the analysis are available in the **data folder**.

Data frame creation - Web scraping
----------------------------------

What I needed first to proceed was a list which gathered the names
corresponding to these roles for a certain amount of movies. For each
year between 2007 and 2017, I gathered the information of the 50 most
profitable movies of the year from the **IMDb website**.

As a first step, I built data frames which contains the titles of these
movies, their gross and their IMDb crew links - the link where we can
find the names and roles of the whole movie crew. The following code is
aiming at building the corresponding data frame for the 50 most
profitable movies of 2017.

    # IMDB TOP US GROSSING 2017: 50 MORE PROFITABLE MOVIES OF 2017 -------------

    url <- "https://www.imdb.com/search/title?release_date=2017-01-01,2017-12-31&sort=boxoffice_gross_us,desc"
    page <- read_html(url)

    # Movies details
    movie_nodes <- html_nodes(page, '.lister-item-header a') 
    movie_link <- sapply(html_attrs(movie_nodes),`[[`,'href')
    movie_link <- paste0("http://www.imdb.com", movie_link)
    movie_crewlink <- gsub("[?]", "fullcredits?", movie_link) #Full crew links
    movie_name <- html_text(movie_nodes)
    movie_year <- rep(2017, 50)
    movie_gross <- html_nodes(page, '.sort-num_votes-visible span:nth-child(5)') %>%
      html_text()

    # CREATE DATAFRAME: TOP 2017 ----------------------------------------------

    top_2017 <- data.frame(movie_name, movie_year, movie_gross, movie_crewlink, stringsAsFactors = FALSE)

Let's have a look to the `top_2017` data frame:

    ##                                movie_name movie_year movie_gross
    ## 1 Star Wars: Episode VIII - The Last Jedi       2017    $620.18M
    ## 2                    Beauty and the Beast       2017    $504.01M
    ## 3                            Wonder Woman       2017    $412.56M
    ## 4          Jumanji: Welcome to the Jungle       2017    $404.26M
    ## 5         Guardians of the Galaxy: Vol. 2       2017    $389.81M
    ## 6                   Spider-Man Homecoming       2017    $334.20M
    ##                                                   movie_crewlink
    ## 1 http://www.imdb.com/title/tt2527336/fullcredits?ref_=adv_li_tt
    ## 2 http://www.imdb.com/title/tt2771200/fullcredits?ref_=adv_li_tt
    ## 3 http://www.imdb.com/title/tt0451279/fullcredits?ref_=adv_li_tt
    ## 4 http://www.imdb.com/title/tt2283362/fullcredits?ref_=adv_li_tt
    ## 5 http://www.imdb.com/title/tt3896198/fullcredits?ref_=adv_li_tt
    ## 6 http://www.imdb.com/title/tt2250912/fullcredits?ref_=adv_li_tt

I adapted the previous code in order to build equivalent data frames for
the past 10 years. I then had 11 data frames: `top2017`, `top2016`, ...,
`top2007`, which gathered the names, years, gross and crew links of the
50 most profitable movies of each year.

I combined these 11 data frames into one data frame called `top_movies`.

List creation - Web scraping
----------------------------

After that, I had a data frame with 550 rows, and I needed to build a
list which gathered:

-   the years from 2007 to 2017

-   for each year, the names of the top 50 grossing movies corresponding

-   for each movie, the names of the people whose job was included in
    one of the categories I listed above (director, writer, ..., costume
    teams)

In order to build this list, I navigated through all the IMDb full crew
web pages stored in our `top_movies` data frame, and did some **web
scraping** again to gather the information listed above.

    movies_list <- list()

    for (r in seq_len(nrow(top_movies))) {
      
      # FOCUS ON EACH MOVIE -----------------------------------------------------------------
      movie_name <- top_movies[r, "movie_name"]
      movie_year <- as.character(top_movies[r, "movie_year"])
      page <- read_html(as.character(top_movies[r, "movie_crewlink"]))
      
      # GATHER THE CREW NAMES FOR THIS MOVIE ------------------------------------------------
      movie_allcrew <- html_nodes(page, '.name , .dataHeaderWithBorder') %>%
        html_text()
      movie_allcrew <- gsub("[\n]", "", movie_allcrew) %>%
        trimws() #Remove white spaces 
      
      # SPLIT THE CREW NAMES BY CATEGORY ----------------------------------------------------
      movie_categories <- html_nodes(page, '.dataHeaderWithBorder') %>%
        html_text()
      movie_categories <- gsub("[\n]", "", movie_categories) %>%
        trimws() #Remove white spaces
        
      ## MUSIC DEPARTMENT -------------------------------------------------------------------
      movie_music <- c()
      for (i in 1:(length(movie_allcrew)-1)){
        if (grepl("Music by", movie_allcrew[i])){
          j <- 1
          while (! grepl(movie_allcrew[i], movie_categories[j])){
            j <- j+1
          }
          k <- i+1
          while (! grepl(movie_categories[j+1], movie_allcrew[k])){
            movie_music <- c(movie_music, movie_allcrew[k])
            k <- k+1
          }
        }
      }
      for (i in 1:(length(movie_allcrew)-1)){
        if (grepl("Music Department", movie_allcrew[i])){
          j <- 1
          while (! grepl(movie_allcrew[i], movie_categories[j])){
            j <- j+1
          }
          k <- i+1
          while (! grepl(movie_categories[j+1], movie_allcrew[k])){
            movie_music <- c(movie_music, movie_allcrew[k])
            k <- k+1
          }
        }
      }
      if (length(movie_music) == 0){
        movie_music <- c("")
      }
        
      ## IDEM FOR OTHER CATEGORIES ---------------------------------------------------------
        
      ## MOVIE_INFO CONTAINS THE MOVIE CREW NAMES ORDERED BY CATEGORY ----------------------
      movie_info <- list()
      movie_info$directors <- movie_directors
      movie_info$writers <- movie_writers
      movie_info$producers <- movie_producers
      movie_info$sound <- movie_sound
      movie_info$music <- movie_music
      movie_info$art <- movie_art
      movie_info$makeup <- movie_makeup
      movie_info$costume <- movie_costume
        
      ## MOVIES_LIST GATHERS THE INFORMATION FOR EVERY YEAR AND EVERY MOVIE ----------------
      movies_list[[movie_year]][[movie_name]] <- movie_info

    }

Here are some of the names I collected:

    ## - Star Wars VIII 2017, Director:
    ## Rian Johnson

    ## - Sweeney Todd 2007, Costume team:
    ## Colleen Atwood, Natasha Bailey, Sean Barrett, Emma Brown, Charlotte Child, Charlie Copson, Steve Gell, Liberty Kelly, Colleen Kelsall, Linda Lashley, Rachel Lilley, Cavita Luchmun, Ann Maskrey, Ciara McArdle, Sarah Moore, Jacqueline Mulligan, Adam Roach, Sunny Rowley, Jessica Scott-Reed, Marcia Smith, Sophia Spink, Nancy Thompson, Suzi Turnbull, Dominic Young, Deborah Ambrosino, David Bethell, Mariana Bujoi, Mauricio Carneiro, Sacha Chandisingh, Lisa Robinson

Gender determination
--------------------

All the names I needed to measure the gender diversity of these jobs
were now gathered in the list `movies_list`. Then, I had to determine
the gender of these almost 275,000 names. This is what the R package
**GenderizeR** does: "The genderizeR package uses genderize.io API to
predict gender from first names". At the moment, the genderize.io
database contains 216286 distinct names across 79 countries and 89
languages. The data is collected from social networks from all over the
world, which ensure the diversity of origins.

However, I was aware that determining genders based on names is not an
ideal solution: some names are unisex, some people do not recognize
themselves as men or women, and some transitioning transgender people
still have their former name. But this solution was the only option I
had, and as I worked on about 275,000 names, I assumed that the error
induced by the cases listed above was not going to have a big impact on
my results.

With this in mind, I used the **GenderizeR** package and applied its
main function on the lists of names I gathered earlier in `movies_list`.
The function `genderizeAPI` checks if the names tested are included in
the genderize.io database and returns:

-   the gender associated with the first name tested

-   the counts of this first name in database

-   the probability of gender given the first name tested.

The attribute I was interested in was obviously the first one, the
**gender** associated with the first name tested.

The aim was to focus on every category of jobs, and to count the number
of males and females by category, by film and by year. In brief with the
script below, here is the information I added to each object
`movies_list$year$film`:

-   the number of male directors,

-   the number of female directors,

-   the number of male producers,

-   the number of female producers,

-   ...,

-   the number of male in costume team,

-   the number of female in costume team.

The following code shows how I determined the gender of the directors'
names for every film in our `movie_list`. The code is similar for all
the other categories.

    # for each year
    for (y in seq_along(movies_list)){ 
      
      # for each movie
      for (i in seq_along(movies_list[[y]])){
        
    # Genderize directors -----------------------------------------------------
        directors <- movies_list[[y]][[i]]$directors
        
        if (directors == ""){
          directors_gender <- list()
          directors_gender$male <- 0
          directors_gender$female <- 0
          movies_list[[y]][[i]]$directors_gender <- directors_gender
        }
        
        else{
          # Split the firstnames and the lastnames
          # Keep the firstnames
          directors <- strsplit(directors, " ")
          l <- c()
          for (j in seq_along(directors)){
          l <- c(l, directors[[j]][1])
          }
      
          directors <- l
          movie_directors_male <- 0
          movie_directors_female <- 0
      
          # Genderize every firstname and count the number of males and females 
          for (p in seq_along(directors)){
            directors_gender <- genderizeAPI(x = directors[p], apikey = "233b284134ae754d9fc56717fec4164e")
            gender <- directors_gender$response$gender
            if (length(gender)>0 && gender == "male"){
              movie_directors_male <- movie_directors_male + 1
            }
            if (length(gender)>0 && gender == "female"){
              movie_directors_female <- movie_directors_female + 1
            }
          }
      
          # Put the number of males and females in movies_list
          directors_gender <- list()
          directors_gender$male <- movie_directors_male
          directors_gender$female <- movie_directors_female
          movies_list[[y]][[i]]$directors_gender <- directors_gender
        }  
        
    # Idem for the 7 other categories -----------------------------------------------------    

      }
    }

Here are some examples of numbers of male and female I collected:

    ## - Star Wars VIII 2017 
    ##  Number of male directors: 1 
    ##  Number of female directors: 0

    ## - Sweeney Todd 2007 
    ##  Number of male in costume team: 9 
    ##  Number of female in costume team: 20

Percentages calculation
-----------------------

Once I had all the gender information listed above, the next step was to
**calculate some percentages by year**. I then went through the whole
list `movies_list` and created a data frame called `percentages` which
gathered the percentages of women in each job category for each year.

Let's have a look to the `percentages` data frame:

    ##    year women_directors women_writers women_producers women_sound
    ## 1  2017        3.571429      9.386282        23.03030    14.17497
    ## 2  2016        3.174603      9.174312        19.04762    14.02918
    ## 3  2015        6.000000     12.432432        21.19914    15.69061
    ## 4  2014        1.785714      8.041958        23.12634    14.89028
    ## 5  2013        1.886792     10.769231        22.86282    13.54005
    ## 6  2012        5.357143     10.227273        24.06542    12.33696
    ## 7  2011        3.846154      9.523810        19.73392    15.08410
    ## 8  2010        0.000000     10.526316        17.40088    16.06700
    ## 9  2009        7.407407     13.157895        21.24711    15.30185
    ## 10 2008        7.547170      9.756098        18.67612    14.70588
    ## 11 2007        3.333333      9.047619        17.42243    16.13904

    ##    year women_music women_art women_makeup women_costume
    ## 1  2017    22.46998  26.87484     68.22204      69.89796
    ## 2  2016    25.84896  25.04481     67.54386      69.44655
    ## 3  2015    20.46163  24.90697     68.83117      70.83333
    ## 4  2014    22.86967  22.31998     67.29508      67.47430
    ## 5  2013    20.46482  22.45546     63.88697      69.79495
    ## 6  2012    21.62819  20.90395     66.95402      68.83539
    ## 7  2011    18.09816  20.22792     70.09482      67.44548
    ## 8  2010    20.90137  22.38199     65.81118      68.72082
    ## 9  2009    19.15734  22.14386     61.15619      70.25948
    ## 10 2008    19.82984  21.80974     60.87768      71.20253
    ## 11 2007    19.64385  20.21891     59.23310      67.36035

Visualisation - gender diversity in 2017
----------------------------------------

I was then able to visualise these percentages. For example, here is the
code I used to visualise the **gender diversity in 2017**.

    # Formating our dataframe
    percentages_t <- data.frame(t(percentages), stringsAsFactors = FALSE)
    colnames(percentages_t) <- percentages_t[1, ]
    percentages_t <- percentages_t[-1, ]
    rownames(percentages_t) <- c("directors", "writers", "producers", "sound", "music", "art", "makeup", "costume")

    # Ploting our barplot
    percentages_2017 <- percentages_t$`2017`
    y <- as.matrix(percentages_2017)

    p <- ggplot(percentages_t, aes(x = rownames(percentages_t),
                                   y = percentages_2017, 
                                   fill = rownames(percentages_t))) + 
      geom_bar(stat = "identity") +
      coord_flip() + # Horizontal bar plot
      geom_text(aes(label=format(y, digits = 2)), hjust=-0.1, size=3.5) + # pecentages next to bars
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.y=element_blank(),
            legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5)) + # center the title
      labs(title = "Percentages of women in the film industry in 2017") +
      guides(fill = guide_legend(reverse=TRUE)) + # reverse the order of the legend
      scale_fill_manual(values = brewer.pal(8, "Spectral")) # palette used to fill the bars and legend boxs

![](analysis/plots/p_plot-1.png)

As we can see, in 2017, the behind-the-camera roles of both **directors
and writers** show the **most limited women occupation**: less than 10%
for writers and less than 4% for directors. This is really worrying
considering that these are key roles which determine the way women are
portrayed in front of the camera. Some studies have already shown that
the more these roles are diversified in terms of gender, the more gender
diversity is shown on screen.

Let's go back to our barplot. Women are also under-represented in sound
teams (14%), music teams (22.5%), producer roles (23%) and art teams
(27%). The only jobs which seem open to women are the **stereotyped
female jobs of make-up artists and costume designers**, among which
almost 70% of the roles are taken by women.

Visualisation - gender diversity evolution through the last decade
------------------------------------------------------------------

Even if those 2017 results are not exciting, I wanted to know whether
there had been an improvement through the last decade. The evolution I
managed to visualise is as follows.

    # From wide to long dataframe
    colnames(percentages) <- c("year", "directors", "writers","producers", "sound",    
                               "music", "art", "makeup", "costume")
    percentages_long <- percentages %>%
      gather(key = category, value = percentage, -year)
    percentages_long$year <- ymd(percentages_long$year, truncated = 2L) # year as date 

    # line plot
    evolution_10 <- ggplot(percentages_long, aes(x = year,
                                                 y = percentage,
                                                 group = category,
                                                 colour = category)) +
      geom_line(size = 2) +
      theme(panel.grid.minor.x = element_blank(),
            plot.title = element_text(hjust = 0.5)) + # center the title
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      scale_color_manual(values = brewer.pal(8, "Set1")) +
      labs(title = "Percentages of women in the film industry from 2007 to 2017",
           x = "",
           y = "Percentages")

![](analysis/plots/show_evolution-1.png)

The first thing I noticed is that **the representativeness gap between
the roles of make-up artists and costume designers and the other ones
has not decreased in a flagrant way since 2007**.

In addition, for the roles were women are really under-represented -
directors, writers and jobs related to sound, no improvement has been
achieved.

If we focus on directors, we do not see any trend. Figures vary
depending on the year we consider. For example **in 2010, we notice that
there is not any woman director amoung the 50 most profitable movies,
and for other years it never goes beyond 7.5%**. What is interesting for
the role of director is that the best woman's representation was reached
in 2008 and 2009, and after that, it has declined and never reached more
than 6%. Besides that, **the percentage of women directors reached in
2017 is substantially the same as the one reached in 2007**.

We then notice a **stability in sound teams and amoung writers**: women
consistently represent around 10% of writers and 15% of sound teams in
the last decade. But there is no sign of improvement.

Only a **slight improvement** of 3-5% is notable among **producers,
music and art teams**. But nothing astonishing.

Visualisation - gender diversity forecasting in 2018
----------------------------------------------------

The last step of our study was to forecast, at a basic level, these
percentages for 2018. I used the **forecast** package and its function
`forecast`, applied to the data I collected between 2007 and 2017, in
order to get this prediction.

    # Time series
    ts <- ts(percentages, start = 2007, end = 2017, frequency = 1)

    # Auto forecast directors 2018
    arma_fit_director <- auto.arima(ts[ ,2])
    arma_forecast_director <- forecast(arma_fit_director, h = 1)
    dir_2018 <- arma_forecast_director$fitted[1] # value predicted

    # Idem for writers, producers, sound, music, art, makeup and costume

    # Create a data frame for 2018 fitted values
    percentages_2018 <- data.frame(year = ymd(2018, truncated = 2L), 
                                   women_directors = dir_2018, 
                                   women_writers = writ_2018, 
                                   women_producers = prod_2018, 
                                   women_sound = sound_2018,
                                   women_music = music_2018,
                                   women_art = art_2018,
                                   women_makeup = makeup_2018,
                                   women_costume = costu_2018, 
                                   stringsAsFactors = FALSE)

    # Values from 2007 to 2017 + 2018 fitted values
    percentages_fitted_2018 <- bind_rows(percentages, percentages_2018)

    # From wide to long dataframe
    colnames(percentages_fitted_2018) <- c("year", "directors", "writers","producers", "sound",    
                                          "music", "art", "makeup", "costume")
    percentages_long_f2018 <- percentages_fitted_2018 %>%
      gather(key = category, value = percentage, -year)
    percentages_long_f2018$year <- ymd(percentages_long_f2018$year, truncated = 2L) # year as date

    # Forecast plot for 2018 
    forecast_2018 <- ggplot(percentages_long_f2018, aes(x = year,
                                                        y = percentage,
                                                        group = category,
                                                        colour = category)) +
      geom_line(size = 2)+
      theme(panel.grid.minor.x = element_blank(),
            plot.title = element_text(hjust = 0.5)) + # center the title
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      scale_color_manual(values = brewer.pal(8, "Set1")) +
      labs(title = "Percentages of women in the film industry from 2007 to 2017\n Fitted values for 2018",
           x = "",
           y = "Percentages")

![](analysis/plots/show_fcst-1.png)

The predicted values I got for 2018 are **approximately the same as the
one I calculated for 2017**. However it is a basic forecasting, and it
**does not take into consideration the upheaval** which happened in the
film industry in 2017. It will surely have an impact on the gender
diversity of this industry. But to what extent ? Has general awareness
been sufficient to truly achieve change ?

In any case, I sincerely hope that our forecasting is wrong and that a
constant improvement will be seen in the next couple of years, so that
**female characters on cinema screens would be more interesting and
complex.**


# Gender diversity in the series industry

Introduction
------------

In a previous post, I studied the gender diversity in the film industry,
by focusing on some key **behind-the-camera roles** and measuring the
evolution of their gender diversity in the last decade. The conclusion
was not great: women are under-represented, especially in the most
important roles of directors and writers, while these key roles
determine the way women are portrayed in front of the camera.

I was curious about the series industry too: **as series are a more
recent area than movies, they might be more open to women?** I decided
to have a look.

In this post, as in the film industry one, the behind-the-camera roles I
studied were the following: **directors**, **writers**, **producers**,
**sound teams**, **music teams**, **art teams**, **makeup teams** and
**costume teams**.

Data Frame Creation - Web Scraping
----------------------------------

Here again, all the data I used was gathered from the **IMDb website**:
I went through the
[`100 Most Popular TV Shows`](https://www.imdb.com/chart/tvmeter?sort=rk,asc&mode=simple&page=1)
(according to the IMDb ratings), and gathered some useful information
about these 100 series: I built a data frame which contains the titles
of these series, their years of release and their IMDb episode links -
the link where we can find all the episodes of a series.

    # IMDb 100 most popular TV shows ------------------------------

    url <- "https://www.imdb.com/chart/tvmeter?sort=us,desc&mode=simple&page=1"
    page <- read_html(url)

    serie_nodes <- html_nodes(page, '.titleColumn') %>%
      as_list()

    # Series details
    serie_name <- c()
    serie_link <- c()
    serie_year <- c()
    for (i in seq_along(serie_nodes)){
      serie_name <- c(serie_name, serie_nodes[[i]]$a[[1]])
      serie_link <- c(serie_link, attr(serie_nodes[[i]]$a, "href"))
      serie_year <- c(serie_year, serie_nodes[[i]]$span[[1]])
    }
    serie_link <- paste0("http://www.imdb.com",serie_link)
    serie_year <- gsub("[()]", "", serie_year)
    serie_episodelist <- sapply(strsplit(serie_link, split='?', fixed=TRUE),
                                function(x) (x[1])) %>%
      paste0("episodes?ref_=tt_eps_yr_mr")


    # Create dataframe ----------------------------------------------
    top_series <- data.frame(serie_name, serie_year, serie_episodelist, stringsAsFactors = FALSE)


    # serie_year was the date of 1st release but we needed the years of release for all the episodes
    # I did not manage to gather this information by doing some web scraping.
    # I added it manually as it is available on the IMDb episodes links (column serie_episodelist)

    top_series[20:30, ]

    ##                        serie_name       serie_year
    ## 20                         Legion             2017
    ## 21 A Series of Unfortunate Events       2017, 2018
    ## 22                       Timeless 2016, 2017, 2018
    ## 23                      Westworld       2016, 2018
    ## 24                      Luke Cage             2016
    ## 25                       MacGyver 2016, 2017, 2018
    ## 26                  Lethal Weapon 2016, 2017, 2018
    ## 27            Designated Survivor 2016, 2017, 2018
    ## 28                           Bull 2016, 2017, 2018
    ## 29                     This Is Us 2016, 2017, 2018
    ## 30                        Atlanta       2016, 2018
    ##                                                 serie_episodelist
    ## 20 http://www.imdb.com/title/tt5114356/episodes?ref_=tt_eps_yr_mr
    ## 21 http://www.imdb.com/title/tt4834206/episodes?ref_=tt_eps_yr_mr
    ## 22 http://www.imdb.com/title/tt5511582/episodes?ref_=tt_eps_yr_mr
    ## 23 http://www.imdb.com/title/tt0475784/episodes?ref_=tt_eps_yr_mr
    ## 24 http://www.imdb.com/title/tt3322314/episodes?ref_=tt_eps_yr_mr
    ## 25 http://www.imdb.com/title/tt1399045/episodes?ref_=tt_eps_yr_mr
    ## 26 http://www.imdb.com/title/tt5164196/episodes?ref_=tt_eps_yr_mr
    ## 27 http://www.imdb.com/title/tt5296406/episodes?ref_=tt_eps_yr_mr
    ## 28 http://www.imdb.com/title/tt5827228/episodes?ref_=tt_eps_yr_mr
    ## 29 http://www.imdb.com/title/tt5555260/episodes?ref_=tt_eps_yr_mr
    ## 30 http://www.imdb.com/title/tt4288182/episodes?ref_=tt_eps_yr_mr

The `series_year` column often contains several years. For example, for
the series called "This is us", it means that episodes have been
released in 2016, 2017 and 2018. This column will allow me to split the
episodes by year of release, and then visualise the gender diversity of
the crew for each year.

List Creation - Web Scraping
----------------------------

At this stage, I just had some global information on the 100 series. The
next step was to go through the IMDb links gathered in the column
`series_episodelist` of my `top_series` data frame, which give me access
to all the series episodes split by year of release. I did some **web
scraping** on these links and built a list which gathered:

-   the names of the 100 most popular TV shows

-   for each series, the different years of release

-   for each year, the names of the episodes which have been released

-   for each episode, the names of the people whose job was included in
    one of the categories I listed above (directors, writers, ...,
    costume teams)

<!-- -->

    ### Create series list

    series_list <- list()

    # FOCUS ON EACH SERIE -----------------------------------------------------------------
    for (r in seq_len(nrow(top_series))) { 
      
      serie_name <- top_series[r, "serie_name"]
      print(serie_name)
      
      # Years of release for each serie
      list_serieyear <- as.list(strsplit(top_series[r, "serie_year"], split = ", ")[[1]]) 
      # List of IMDb links where we find all the episodes per year of release
      link_episodelist_peryear <- list() 
      
      episodes_list_peryear <- list()
      
      # FOCUS ON EACH YEAR OF REALEASE FOR THIS SERIE -------------------------------------
      for (u in seq_along(list_serieyear)){ 

        year <- list_serieyear[[u]]
        print(year)
        
        link_episodelist_yeari <- strsplit(top_series[r, "serie_episodelist"], split='?', fixed=TRUE)[[1]][1] %>%
          paste0("?year=", year, collapse = "")
        link_episodelist_peryear[[u]] <- link_episodelist_yeari
        
        # FOCUS ON EACH EPISODE FOR THIS YEAR OF RELEASE ----------------------------------
        for (l in seq_along(link_episodelist_peryear)){ 
          
          page <- read_html(link_episodelist_peryear[[l]]) 
          episodes_nodes <- html_nodes(page, '.info') %>%
            as_list()
          
          episode_name <- c()
          episode_link <- c()
          
          for (t in seq_along(episodes_nodes)){
            episode_name <- c(episode_name, episodes_nodes[[t]]$strong$a[[1]])
            episode_link <- c(episode_link, attr(episodes_nodes[[t]]$strong$a, "href"))
          }
          
          episode_link <- paste0("http://www.imdb.com",episode_link)
          episode_link <- sapply(strsplit(episode_link, split='?', fixed=TRUE), 
                                 function(x) (x[1])) %>%
            paste0("fullcredits?ref_=tt_ql_1")
          
          episode_name <- sapply(episode_name, 
                                 function(x) (gsub(pattern = "\\#", replacement = "", x)))  %>% # some names = "Episode #1.1"
            as.character()
          
          # GATHER THE NAME OF THE EPISODE, ITS YEAR OF RELEASE AND ITS FULL CREW LINK ----
          episodes_details_peryear <- data.frame(year = year,
                                                 episode_name = episode_name,
                                                 episode_link = episode_link,
                                                 stringsAsFactors = FALSE)
        }
        
        # FOCUS ON EACH FULL CREW LINK ----------------------------------------------------
        for (e in seq_len(nrow(episodes_details_peryear))){
          
          print(episodes_details_peryear[e, "episode_link"])
          
          episode_page <- read_html(episodes_details_peryear[e, "episode_link"])
          episode_name <- episodes_details_peryear[e, "episode_name"]
          
          # GATHER ALL THE CREW NAMES FOR THIS EPISODE -------------------------------------
          episode_allcrew <- html_nodes(episode_page, '.name , .dataHeaderWithBorder') %>%
            html_text()
          episode_allcrew <- gsub("[\n]", "", episode_allcrew) %>%
            trimws() #Remove white spaces 
          
          # SPLIT ALL THE CREW NAMES BY CATEGORY -------------------------------------------
          episode_categories <- html_nodes(episode_page, '.dataHeaderWithBorder') %>%
            html_text()
          episode_categories <- gsub("[\n]", "", episode_categories) %>%
            trimws() #Remove white spaces
          
          ## MUSIC DEPT -----------------------------------------------------------------------
          episode_music <- c()
          for (i in 1:(length(episode_allcrew)-1)){
            if (grepl("Music by", episode_allcrew[i])){
              j <- 1
              while (! grepl(episode_allcrew[i], episode_categories[j])){
                j <- j+1
              }
              k <- i+1
              while (! grepl(episode_categories[j+1], episode_allcrew[k])){
                episode_music <- c(episode_music, episode_allcrew[k])
                k <- k+1
              }
            }
          }
          for (i in 1:(length(episode_allcrew)-1)){
            if (grepl("Music Department", episode_allcrew[i])){
              # Sometimes music dept is last category
              if (grepl ("Music Department", episode_categories[length(episode_categories)])){ 
                first <- i+1
                for (p in first:length(episode_allcrew)) {
                  episode_music <- c(episode_music, episode_allcrew[p])
                }
              } else {
                j <- 1
                while (! grepl(episode_allcrew[i], episode_categories[j])){
                  j <- j+1
                }
                k <- i+1
                while (! grepl(episode_categories[j+1], episode_allcrew[k])){
                  episode_music <- c(episode_music, episode_allcrew[k])
                  k <- k+1
                }
              }
            }
          }
          if (length(episode_music) == 0){
            episode_music <- c("")
          }
          
          ## IDEM FOR OTHER CATEGORIES ----------------------------------------------------------
          
          ## EPISODE_INFO CONTAINS THE EPISODE CREW NAMES ORDERED BY CATEGORY -------------------
          episode_info <- list()
          episode_info$directors <- episode_directors
          episode_info$writers <- episode_writers
          episode_info$producers <- episode_producers
          episode_info$sound <- episode_sound
          episode_info$music <- episode_music
          episode_info$art <- episode_art
          episode_info$makeup <- episode_makeup
          episode_info$costume <- episode_costume
          
          ## EPISODES_LIST_PER_YEAR GATHERS THE INFORMATION FOR EVERY EPISODE OF THE SERIE-------
          ## SPLIT BY YEAR OF RELEASE --------------------------------------------------------
          episodes_list_peryear[[year]][[episode_name]] <- episode_info
        }
        
        ## SERIES_LIST GATHERS THE INFORMATION FOR EVERY YEAR AND EVERY SERIE -------------------
        series_list[[serie_name]] <- episodes_list_peryear
      } 
    }

Let's have a look at the information gathered in `series_list`. Here are
some of the names I collected:

    ## - Black Mirror, 2011
    ##  Episode: The National Anthem 
    ##  Director: Otto Bathurst

    ## - Black Mirror, 2017
    ##  Episode: Black Museum 
    ##  Director: Colm McCarthy

    ## - Game of Thrones, 2011
    ##  Episode: Winter Is Coming 
    ##  Music team: Ramin Djawadi, Evyen Klean, David Klotz, Robin Whittaker, Michael K. Bauer, Brandon Campbell, Stephen Coleman, Janet Lopez, Julie Pearce, Joe Rubel, Bobby Tahouri

    ## - Game of Thrones, 2017
    ##  Episode: Dragonstone 
    ##  Music team: Ramin Djawadi, Omer Benyamin, Evyen Klean, David Klotz, William Marriott, Douglas Parker, Stephen Coleman

What we can see is that for the same series the crew changes depending
on the episode we consider.

Gender Determination
--------------------

Now that I had all the names gathered in the `series_list`, I needed to
determine their gender. I used the same package as in my previous post
on the film industry: **GenderizeR**, which "uses genderize.io API to
predict gender from first names". More details on this package and the
reasons why I decided to use it are available in my previous post.

With this R package, I was able to determine for each episode the number
of males and females in each category of jobs:

-   the number of male directors,

-   the number of female directors,

-   the number of male producers,

-   the number of female producers,

-   ...,

-   the number of male in costume team,

-   the number of female in costume team.

Here is the code I wrote:

    ### Genderize our lists of names

    # for each serie
    for (s in seq_along(series_list) ){  
      print(names(series_list[s])) # print serie name
      
      # for each year
      for (y in seq_along(series_list[[s]])){ 
        print(names(series_list[[s]][y])) # print serie year
        
        # for each episode
        for (i in seq_along(series_list[[s]][[y]])){ 
          print(names(series_list[[s]][[y]][i])) # print serie episode
          
          # Genderize directors -----------------------------------------------------
          directors <- series_list[[s]][[y]][[i]]$directors
          
          if (directors == ""){
            directors_gender <- list()
            directors_gender$male <- 0
            directors_gender$female <- 0
            series_list[[s]][[y]][[i]]$directors_gender <- directors_gender
          }
          
          else{
            # Split the firstnames and the lastnames
            # Keep the firstnames
            directors <- strsplit(directors, " ")
            l <- c()
            for (j in seq_along(directors)){
              l <- c(l, directors[[j]][1])
            }
            
            directors <- l
            serie_directors_male <- 0
            serie_directors_female <- 0
            
            # Genderize every firstname and count the number of males and females 
            for (p in seq_along(directors)){
              directors_gender <- genderizeAPI(x = directors[p], apikey = "233b284134ae754d9fc56717fec4164e")
              gender <- directors_gender$response$gender
              if (length(gender)>0 && gender == "male"){
                serie_directors_male <- serie_directors_male + 1
              }
              if (length(gender)>0 && gender == "female"){
                serie_directors_female <- serie_directors_female + 1
              }
            }
            
            # Put the number of males and females in series_list
            directors_gender <- list()
            directors_gender$male <- serie_directors_male
            directors_gender$female <- serie_directors_female
            series_list[[s]][[y]][[i]]$directors_gender <- directors_gender
          }  
          
          # Same code for the 7 other categories -----------------------------------
          
          } 
        }
      }
    }

Here are some examples of numbers of male and female I collected:

    ## Black Mirror, 2011
    ##  Episode: The National Anthem 
    ##  Number of male directors: 1 
    ##  Number of female directors: 0 
    ## 

    ## Black Mirror, 2017
    ##  Episode: Black Museum 
    ##  Number of male directors: 1 
    ##  Number of female directors: 0 
    ## 

    ## Game of Thrones, 2011
    ##  Episode: Winter Is Coming 
    ##  Number of male in music team: 8 
    ##  Number of female in music team: 3 
    ## 

    ## Game of Thrones, 2017
    ##  Episode: Dragonstone 
    ##  Number of male in music team: 7 
    ##  Number of female in music team: 0 
    ## 

Percentages Calculation
-----------------------

With these numbers gathered in my list, I then calculated the
percentages of women in each job category, for each year between 2007
and 2018. I gathered these figures in a data frame called `percentages`:

    ##    year directors  writers producers     sound    music      art   makeup
    ## 1  2018  22.69693 25.06514  27.87217 12.247212 23.25581 36.93275 73.10795
    ## 2  2017  20.51948 28.20016  27.28932 10.864631 25.46912 29.90641 71.41831
    ## 3  2016  17.13456 24.51189  27.93240 11.553444 25.03117 30.98003 71.74965
    ## 4  2015  16.14764 19.42845  26.43828 11.214310 22.16505 29.83354 69.50787
    ## 5  2014  18.38624 20.88644  27.59163 10.406150 22.21016 30.11341 69.97544
    ## 6  2013  14.94413 19.60432  28.15726 10.504896 23.29693 29.01968 69.01683
    ## 7  2012  15.60694 19.82235  29.66566 10.685681 21.45378 26.74160 67.47677
    ## 8  2011  13.95349 17.60722  26.73747 11.296882 17.11185 25.61805 64.81795
    ## 9  2010  15.95745 17.05882  27.38841 11.264644 16.51376 24.14815 65.33004
    ## 10 2009  16.49123 18.90496  28.79557  8.498350 21.72285 26.11128 68.15961
    ## 11 2008  17.87440 16.62088  29.05844  7.594264 18.74405 23.46251 68.39827
    ## 12 2007  21.15385 21.78771  30.12798  9.090909 19.23077 21.66124 63.03502
    ##     costume
    ## 1  77.24853
    ## 2  81.34648
    ## 3  79.35358
    ## 4  76.48649
    ## 5  76.62972
    ## 6  74.74791
    ## 7  77.35247
    ## 8  77.46315
    ## 9  77.67380
    ## 10 79.56332
    ## 11 80.53191
    ## 12 79.24720

Gender Diversity in 2017: Series Industry VS Film Industry
----------------------------------------------------------

Based on this data frame, I created some bar plots to visualise the gender diversity of each job category for each year. I have built a simple Shiny application which gives access to the bar plots for each year between 2007 and 2017.

Here is the code I wrote to create the bar plot for 2017, which compares the series industry to the film industry.

    ### Barplot 2017

    # Data manipulation -------------------------------------------------------------

    # Import our movies dataset
    percentages_movies <- read.csv("percentages_movies.csv") 
    percentages_movies <- percentages_movies[ , -1]

    # Change column names for movie and serie dataframes
    colnames(percentages_movies) <- c("year", "directors", "writers", "producers", "sound", "music", "art", "makeup", "costume")
    colnames(percentages) <- c("year", "directors", "writers", "producers", "sound", "music", "art", "makeup", "costume")

    # From wide to long dataframes
    percentages_movies_long <- percentages_movies %>%
      gather(key = category, value = percentage, -year)
    percentages_long <- percentages %>%
      gather(key = category, value = percentage, -year)

    # Add a column to these dataframes: movie or film ?
    percentages_movies_long$industry <- rep("Film industry", 88)
    percentages_long$industry <- rep("Series industry", 96)

    # Combine these 2 long dataframes
    percentages_movies_series <- bind_rows(percentages_long, percentages_movies_long)

    # Filter with year=2017
    percentages_movies_series_2017 <- percentages_movies_series %>%
      filter(year == 2017)


    # Data visualisation -------------------------------------------------------------

    percentages_movies_series_2017$percentage <- as.numeric(format(percentages_movies_series_2017$percentage, 
                                                            digits = 2))

    bar_2017 <- ggplot(percentages_movies_series_2017, aes(x = category,
                                                           y = percentage,
                                                           group = category,
                                                           fill = category)) +
      geom_bar(stat = "identity") +
      facet_wrap(~industry) +
      coord_flip() + # Horizontal bar plot
      geom_text(aes(label = percentage), hjust=-0.1, size=3) +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.y=element_blank(),
            plot.title = element_text(hjust = 0.5), # center the title
            legend.title=element_blank()) + 
      labs(title = paste("Percentages of women in 2017"),
           x = "",
           y = "Percentages") +
      guides(fill = guide_legend(reverse=TRUE)) + # reverse the order of the legend
      scale_fill_manual(values = brewer.pal(8, "Spectral")) # palette used to fill the bars and legend boxs

![](analysis/plots/barplot_2017.png)

Let's analyse this graph.

If we only focus on series figures, we see that sound teams show the
most limited women occupation, with less than 11%. It is followed by the
**role of director with 20.5%.** Then, we can see that **between 25% and
30% of the roles of writers, producers, music teams and art teams are
taken by women**.

Thus, women are still under-represented in the series industry. However,
even if series figures show little gender diversity in the above job
categories, they are better than the film industry ones, especially for
the key roles of **directors, writors and producers**, which are
respectively **5.7, 3 and 1.2 times higher for the series industry than
for the film one.**

The last thing to notice is that as in the film industry, the series
industry graph shows a representativeness gap between the above roles
and the jobs of **make-up artists and costume designers, among which
more than 70% of the roles are taken by women.**

Evolution of the Gender Diversity: Series Industry VS Film Industry
-------------------------------------------------------------------

Let's have a look to the evolution of the gender diversity in these two
industries in the last decade.

    ### Evolution plot

    # year as date
    percentages_movies_series_ymd <- percentages_movies_series %>%
      subset(year != 2018)
    percentages_movies_series_ymd$year <- ymd(percentages_movies_series_ymd$year, truncated = 2L) 

    # Data visualisation
    evolution <- ggplot(percentages_movies_series_ymd, aes(x = year,
                                                           y = percentage,
                                                           group = category,
                                                           colour = category)) +
      geom_line(size = 2) +
      facet_wrap(~industry) +
      theme(panel.grid.minor.x = element_blank(),
            plot.title = element_text(hjust = 0.5)) + # center the title
      scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
      scale_color_manual(values = brewer.pal(8, "Set1")) +
      labs(title = "Percentages of women from 2007 to 2017\n Film industry VS serie industry",
           x = "",
           y = "Percentages")

![](analysis/plots/evolution.png)

The first thing I noticed is that for both the film and series
industries, **the representativeness gap between the roles of make-up
artists and costume designers and the other ones has not decreased since
2007**.

The fact that the roles of directors, writers and producers are more
open to women in the series industry than in the film one is easy to
visualise with this graph, and we can see that it has been the case at
least since 2007 (and probably before). Besides, **since 2007 the series
industry has been more diversified in terms of gender for all the
categories I studied, except for the sound roles**.

I also noticed that **since 2010/2011, in the series industry, almost
all the categories tend to be more diversified in terms of gender**. The
only exceptions are the roles of producers (percentages are generally
decreasing a bit since 2007), sound teams (no improvement has been
achieved since 2010) and costume teams (the trend has been positive only
since 2013). Apart from that, **there is a positive trend for the series
industry, which is not the case for the film industry**.

This trend is significant for the some roles: **writers, music teams,
art teams and make-up teams percentages in the series industry have
increased by 5 to 10% in the last decade**. But if we look at the role
of **directors**, the percentage of women have also increased by 5%
since 2011, but **the percentage reached in 2017 is essentially the same
as the one reached in 2007, just as for the film industry**. Let's hope
that the trend seen since 2011 for directors will continue.

Conclusion
----------

This study has definitely shown that **the series industry is more
diversified in terms of gender than the film industry, especially for
the key roles of directors and writers**.

However even if the series percentages are better than the film ones,
**women are still under-represented in the series industry** as the same
regrettable analysis has been echoed: the only jobs which seem open to
women are the stereotyped female jobs of make-up artists and costume
designers. In all the other categories, the percentages of women in the
series industry never reach more than 30%.

But **contrary to the film industry, the series one is actually evolving
in the right direction**: since 2011, a positive trend has been
happening for, inter alia, directors and writers. This evolution is
encouraging for the future and suggests that powerful female characters,
such as Daenerys Targaryen from Game of Thrones, are coming on TV
screens.
