SnowyCities\_
================
scarufel\_
27 janvier 2020

I asked myself a fairly simple question : what are the world most snowy big cities ?

Let's break this down :

Big city = any world city with a population over 500 000 inhabitants. NOT the metro, urban area or some other census measuring: really the population within the official boundaries of a city. So New York City would be around 8 million, not 20 million.

Snowy = yearly average snowfall normals, 1981-2010, in centimeters or inches.

``` r
library(WikidataQueryServiceR)
library(dplyr)
library(rvest)
library(rlist)
library(stringr)
library(xml2)
library(BBmisc)
```

Now we first need to request a list of all of the world's big cities (over 500k in this case, remember). If you have never used the Wikidata Query Service, I strongly recommend doing this tutorial, this is how I learned how to use it : <https://www.wikidata.org/wiki/Wikidata:SPARQL_tutorial>

``` r
RequestCity500k <- "SELECT DISTINCT ?city ?cityLabel ?pop ?country ?countryLabel
WHERE
{
# The data is an instance of a CITY
  ?city wdt:P31/wdt:P279* wd:Q515;
# POPULATION is data too
        wdt:P1082 ?pop;
        wdt:P17 ?country.
# Cities with a population over 500 000
  FILTER(?pop > 500000).
# To print the name of all entities in the table
  SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
}"
  
CitiesMin500k <- query_wikidata(RequestCity500k)
```

    ## 995 rows were returned by WDQS

``` r
head(CitiesMin500k, 10) # Just checking how the data looks
```

    ##                                    city cityLabel      pop
    ## 1   http://www.wikidata.org/entity/Q172   Toronto  2731571
    ## 2  http://www.wikidata.org/entity/Q1355 Bengaluru 10535000
    ## 3  http://www.wikidata.org/entity/Q1354     Dhaka  8906039
    ## 4    http://www.wikidata.org/entity/Q64    Berlin  3613495
    ## 5  http://www.wikidata.org/entity/Q1538      Pune  5945000
    ## 6  http://www.wikidata.org/entity/Q1867    Taipei  2684567
    ## 7   http://www.wikidata.org/entity/Q612     Dubai  2502715
    ## 8   http://www.wikidata.org/entity/Q490     Milan  1351562
    ## 9   http://www.wikidata.org/entity/Q334 Singapore  5888926
    ## 10 http://www.wikidata.org/entity/Q1731   Dresden   554434
    ##                                country         countryLabel
    ## 1   http://www.wikidata.org/entity/Q16               Canada
    ## 2  http://www.wikidata.org/entity/Q668                India
    ## 3  http://www.wikidata.org/entity/Q902           Bangladesh
    ## 4  http://www.wikidata.org/entity/Q183              Germany
    ## 5  http://www.wikidata.org/entity/Q668                India
    ## 6  http://www.wikidata.org/entity/Q865               Taiwan
    ## 7  http://www.wikidata.org/entity/Q878 United Arab Emirates
    ## 8   http://www.wikidata.org/entity/Q38                Italy
    ## 9  http://www.wikidata.org/entity/Q334            Singapore
    ## 10 http://www.wikidata.org/entity/Q183              Germany

``` r
nrow(CitiesMin500k) # Just checking
```

    ## [1] 995

``` r
class(CitiesMin500k) # Just checking
```

    ## [1] "data.frame"

So what we've got here is a data.frame containing 995 rows. The city's instance URL in Wikidata is there, as well as its name, its population, its country and its country's instance URL in Wikidata.

The climate data of most major cities, including the snowfall data where there is some snow, is printed in a city's Wikipedia article in a table - not its Wikidata page. The Wikipedia articles are the ones you read when reading about something on Wikipedia, while the Wikidata page lists all the information and data, including linked data Wikidata has on this city. Example for Aba, Nigeria: <http://www.wikidata.org/entity/Q202162>

When available, the climate data table looks like this, like for Toronto :

![Toronto Climate Data](C:/Users/Boris7/Google%20Drive/SCARUFEL-COM/toronto.png)

What we are looking for here is the yearly average snowfall (circled in red). We need our script to visit every page we stored moments ago and look up for any indication of snowfall data. If there isn't any, our script must say so and move on. But we have one last thing to do before that.

A city's Wikipedia article URL can be found in its Wikidata entry page.

![The Toronto Wikidata entry page contains a link to its Wikipedia article page in English - circled in red above](C:/Users/Boris7/Google%20Drive/SCARUFEL-COM/toronto2.png)

So here the first webscraping operation begins. For every city stored, we will tell our script to search for the city's English-language Wikipedia article URL so we can visit it.

By right-clicking on the red-circled link in a browser, I can copy its CSS select information, which goes like this "span.wikibase-sitelinkview-link.wikibase-sitelinkview-link-enwiki span.wikibase-sitelinkview-page a".

Before doing this for every city, we'll just test it first on a single city.

``` r
# Read and store the page
PageWD_Toronto <- read_html("https://www.wikidata.org/wiki/Q172")

# Find the link to the Engolish-language Wikipedia article URL
LinkEN_Toronto <- html_attrs(html_nodes(PageWD_Toronto, css = "span.wikibase-sitelinkview-link.wikibase-sitelinkview-link-enwiki span.wikibase-sitelinkview-page a"))
href_Toronto <- LinkEN_Toronto[[1]][1] # Select the first item only, which is the URL link we want

# Go to the Wikipedia article page and retrieve the yearly average snowfall data - the row name goes by Average snowfall [...]"

PageToronto <- read_html(href_Toronto) # reading the page
TableToronto <- html_text(html_nodes(PageToronto, "table tr")) # store all table rows
TableTorontoSnow <- str_which(TableToronto, "Average snowfall") # Look for any row beginning with "Average snowfall"
SnowToronto <- TableToronto[[TableTorontoSnow]] 
SnowToronto_Split <- unlist(str_split_fixed(SnowToronto, "\n\n", n = 14)) # Some data cleaning, removing the extra space rendered as /n/n here
print(SnowToronto_Split) # How does it looks?
```

    ##      [,1]                           [,2]         [,3]         [,4]       
    ## [1,] "Average snowfall cm (inches)" "37.2(14.6)" "27.0(10.6)" "19.8(7.8)"
    ##      [,5]       [,6]       [,7]       [,8]       [,9]       [,10]     
    ## [1,] "5.0(2.0)" "0.0(0.0)" "0.0(0.0)" "0.0(0.0)" "0.0(0.0)" "0.0(0.0)"
    ##      [,11]      [,12]      [,13]       [,14]          
    ## [1,] "0.1(0.0)" "8.3(3.3)" "24.1(9.5)" "121.5(47.8)\n"

Looks good enough, although we're only interested in the last figure (the yearly average), we'll sort that later.

This works because we knew there's some snowfall occurring in Toronto every winter. But what we did above is of no use for a city that has no English-language article, or a city that does have one but receives no snow at all, like Dubai in the United Arab Emirates. So we need to tell our script it can happens and what it should do when this happens.

We will write a function that will look up any city and see if it can find (1) an English-language Wikipedia article and, if so, (2) retrieve any snowfall data and, if so, (3) store it. But still for only one city at the time at this stage.

``` r
SnowFinder <- function(x){
  wdURL <- str_replace(x, "entity", "wiki")
  PageWD <- read_html(wdURL)
  LinkEN <- html_attrs(html_nodes(PageWD, css = "span.wikibase-sitelinkview-link.wikibase-sitelinkview-link-enwiki span.wikibase-sitelinkview-page a"))
  
  if (length(LinkEN) < 1) { # If he can't find an English-language article, the length of this information will be smaller than 1
    Snow <- "No EN link"
    print(Snow)
  } else {
    
    href_ <- LinkEN[[1]][1]
    Page <- read_html(href_)
    Table <- html_text(html_nodes(Page, "table tr"))
    TableSnow <- str_which(Table, "Average snowfall")
    Header <- html_text(html_nodes(Page, "table th"))
    HeaderSnow <- str_which(Header, "Climate data")
    if (length(TableSnow) == 0) { # If there is an article in English but no snowfall data, like for Dubai
      Snow <- "No snow"
      print(Snow)
    } else if (length(TableSnow) > 1) {
      Snow <- Table[[TableSnow[1]]]
      SnowSplit <- unlist(str_split_fixed(Snow, "\n\n", n = 14))
      SnowSplit <- as.data.frame(SnowSplit)
      SnowSplit <- cbind(SnowSplit, wdURL, Header[[HeaderSnow[1]]]) # The first table is the one of interest
      print(SnowSplit)   
    } else {
      Snow <- Table[[TableSnow]]
      SnowSplit <- unlist(str_split_fixed(Snow, "\n\n", n = 14))
      SnowSplit <- as.data.frame(SnowSplit)
      SnowSplit <- cbind(SnowSplit, wdURL, Header[[HeaderSnow[1]]]) # Just in case there is more than one table regarding climate, for instance when there is a table showing the data for an airport's weather station and another table from another station
      print(SnowSplit)
    }
    
  }
  
}
```

OK, so let's test our function on some random city

``` r
SnowFinder("https://www.wikidata.org/entity/Q172") # Toronto - There is snow
```

    ##                             V1         V2         V3        V4       V5
    ## 1 Average snowfall cm (inches) 37.2(14.6) 27.0(10.6) 19.8(7.8) 5.0(2.0)
    ##         V6       V7       V8       V9      V10      V11      V12       V13
    ## 1 0.0(0.0) 0.0(0.0) 0.0(0.0) 0.0(0.0) 0.0(0.0) 0.1(0.0) 8.3(3.3) 24.1(9.5)
    ##             V14                              wdURL
    ## 1 121.5(47.8)\n https://www.wikidata.org/wiki/Q172
    ##                                                             Header[[HeaderSnow[1]]]
    ## 1 Climate data for Toronto (The Annex), 1981–2010 normals, extremes 1840–present[b]

``` r
SnowFinder("https://www.wikidata.org/entity/Q612") # Dubai - No snow there
```

    ## [1] "No snow"

OK ! Now let's do it on all of our cities. It will take a couple of minutes.

``` r
DataSnow <- lapply(CitiesMin500k$city, SnowFinder)
names(DataSnow) <- CitiesMin500k$cityLabel
```

Items in the list with actual data are data.frames, while everything else (no snow, no EN Wikipedia article) is a character string (vector). So, well, we'll just keep the actual data and get rid of everything else by telling R we only want to keep the data.frames.

``` r
DataSnowOnly <- list.filter(DataSnow, x ~ class(x) == "data.frame")
```

Cleaning the data now

``` r
SnowDF <- list.stack(DataSnowOnly, data.table = FALSE)
SnowDF <- cbind(names(DataSnowOnly), SnowDF)
SnowDF$V14 <- str_replace_all(SnowDF$V14, "\\n", "") # Removing the \\n, replacing them with nothing
```

Now we need to put everything in the metric system (cm), not the imperial one (inches).

``` r
SnowDFusa <- SnowDF %>% 
  filter(V1 == "Average snowfall inches (cm)")  # The row name for US cities

SnowDFout <- NeigeDF %>% 
  filter(V1 == "Average snowfall cm (inches)")  # The row name for every other city
```

Dealing with the imperial system (inches), putting the figure in centimeters first.

``` r
CleanerSnowUSA <- function(x) {
    Data <- x
    Position <- str_locate(Data, "\\(")
    Data <- str_sub(Data, start = as.integer(Position))
    Data <- Data[1]
    Data <- str_replace(Data, "[)]", "")
    Data <- str_replace(Data, "[(]", "")
    print(Data)
}

VectorUSA <- lapply(X = SnowDFusa$V14, FUN = CleanerSnowUSA)
names(VectorUSA) <- SnowDFusa$`names(DataSnowOnly)`
```

Dealing with the metric system (cm).

``` r
CleanerSnowOUT <- function(x) {
  Data <- x
  Position <- str_locate(Data, "\\(")
  Data <- str_sub(Data, end = as.integer(Position))
  Data <- Data[1]
  Data <- str_replace(Data, "[()]", "")
  print(Data)
}

VectorOUT <- lapply(X = SnowDFout$V14, FUN = CleanerSnowOUT)
names(VectorOUT) <- SnowDFout$`names(DataSnowOnly)`
```

And lastly, putting all the data together so we can compare!

``` r
MatrixUSA <- list.rbind(VectorUSA)
MatrixOUT <- list.rbind(VectorOUT)
MatrixSnow <- rbind(MatrixOUT, MatrixUSA)
names(MatrixSnow[,1]) <- "TotalCM"
DF_Snow <- as.data.frame(MatrixSnow)
DF_Snow$V1 <- as.numeric(levels(DF_Snow$V1))[DF_Snow$V1]
DF_Snow <- data.frame(City = rownames(DF_Snow), Snow_cm = DF_Snow$V1)


DF_Snow %>%
  arrange(desc(Snow_cm)) %>%
  select(City, City_cm)
```

And that's it. My 5 most snowy big cities are, in descending order, Sapporo, Quebec City, Ottawa, Niigata, Montreal.

Now, there are some caveats :

1- Overtime, because everything Wiki is subject to changes made by humans, some cities may or may not appear. For instance at some point during my analysis Birmingham, England became classified as a metropolis rather than a city in Wikidata, which had the effect of leaving out this actual city.

2- Some data were from other weather stations that the city's main airport. It is a standard to look for a city's airport weather station as it is considered the most official and reliable source of information. So for my top snowy cities here I had to manually check whether my data were from the weather stations.

3- For some reason, Russian cities, some of which do receive some snow, had no 1981-2010 weather normals data regarding snowfall. I looked elsewhere on the internet and on some Russian government webpages but it is nowhere to be found, as if the country refused to do so or something.

Nevertheless, despite all this, meteorologists at MétéoMédia have had a look at my results and they are very confident about them and were comfortable in saying these five cities were the most likely to be the most snowy cities. They do not think cities in Russia can get as much snow in a typical year than my 3 Canadian and 2 Japanese cities. I also checked the data of the 15 top cities against the relevant national meteorological agency (Environment and Climate Change Canada, the National Weather Service, the Japanese Meteorological Agency, etc.).

Now, when you look at the resulting article I wrote, it doesn't look like all this operation was required to come up with those 5 cities, I must admit: <https://www.meteomedia.com/ca/nouvelles/article/quelles-grandes-villes-du-monde-recoivent-le-plus-de-neige>
