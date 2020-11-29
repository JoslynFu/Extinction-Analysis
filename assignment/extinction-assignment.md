Extinctions Unit
================
Joslyn Fu, Kelly Yuan

## Background

  - [Section Intro Video](https://youtu.be/QsH6ytm89GI)
  - [Ceballos et al (2015)](http://doi.org/10.1126/sciadv.1400253)

Our focal task will be to reproduce the result from Ceballos and
colleagues showing the recent increase in extinction rates relative to
the background rate:

![](https://espm-157.carlboettiger.info/img/extinctions.jpg)

## Coding Part

Let’s start with downloading the data we need.

``` r
base_url <- "https://apiv3.iucnredlist.org"
endpoint <- "/api/v3/species/page/"
page_number <- 0
args <- "?token="
token <- "9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee"
query <- paste0(base_url, endpoint, page_number, args, token)
```

``` r
resp <- GET("https://apiv3.iucnredlist.org/api/v3/species/page/")
resp
```

    Response [https://apiv3.iucnredlist.org/api/v3/species/page/]
      Date: 2020-11-29 05:09
      Status: 200
      Content-Type: application/json; charset=utf-8
      Size: 92 B

``` r
txt <- content(resp,as = "parsed")
```

``` r
base_url <- "https://apiv3.iucnredlist.org"
endpoint <- "/api/v3/species/page/"
page_number <- 0:12
args <- "?token="
token <- "9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee"
queries <- paste0(base_url, endpoint, page_number, args, token)
```

``` r
resp <- map(queries, GET)
```

``` r
status <- map_int(resp, status_code)
good <- resp[status==200]
try_again <- map(queries[status != 200], GET)
good <- c(good, try_again)
```

Then, we need to rectangle the full species list:

``` r
txts <- map(good, content, as = "parsed")
```

``` r
rectangle_me <- function(x) as_tibble(compact(x))
stack_txt <- function(txt) map_dfr(txt$result, rectangle_me)
all_sci_names <- purrr::map_dfr(txts,stack_txt)
all_sci_names
```

    # A tibble: 122,933 x 12
       taxonid kingdom_name phylum_name class_name order_name family_name genus_name
         <int> <chr>        <chr>       <chr>      <chr>      <chr>       <chr>     
     1       3 ANIMALIA     MOLLUSCA    GASTROPODA STYLOMMAT… ENDODONTID… Aaadonta  
     2       4 ANIMALIA     MOLLUSCA    GASTROPODA STYLOMMAT… ENDODONTID… Aaadonta  
     3       5 ANIMALIA     MOLLUSCA    GASTROPODA STYLOMMAT… ENDODONTID… Aaadonta  
     4       6 ANIMALIA     MOLLUSCA    GASTROPODA STYLOMMAT… ENDODONTID… Aaadonta  
     5       7 ANIMALIA     MOLLUSCA    GASTROPODA STYLOMMAT… ENDODONTID… Aaadonta  
     6       8 ANIMALIA     MOLLUSCA    GASTROPODA STYLOMMAT… ENDODONTID… Aaadonta  
     7       9 ANIMALIA     CHORDATA    ACTINOPTE… CYPRINIFO… CYPRINIDAE  Aaptosyax 
     8      18 ANIMALIA     CHORDATA    MAMMALIA   RODENTIA   ABROCOMIDAE Abrocoma  
     9      20 ANIMALIA     CHORDATA    REPTILIA   SQUAMATA   ANGUIDAE    Abronia   
    10      43 ANIMALIA     ARTHROPODA  INSECTA    ODONATA    AESHNIDAE   Acanthaes…
    # … with 122,923 more rows, and 5 more variables: scientific_name <chr>,
    #   category <chr>, infra_rank <chr>, infra_name <chr>, population <chr>

We need to filter the category to get the extinct species and arrange it
by
scientific\_name.

``` r
extinct <- all_sci_names %>% filter(category == "EX") %>% arrange(scientific_name)
extinct
```

    # A tibble: 919 x 12
       taxonid kingdom_name phylum_name class_name order_name family_name genus_name
         <int> <chr>        <chr>       <chr>      <chr>      <chr>       <chr>     
     1   44072 PLANTAE      TRACHEOPHY… MAGNOLIOP… ROSALES    ROSACEAE    Acaena    
     2  195373 PLANTAE      TRACHEOPHY… MAGNOLIOP… MALPIGHIA… EUPHORBIAC… Acalypha  
     3   37854 PLANTAE      TRACHEOPHY… MAGNOLIOP… MALPIGHIA… EUPHORBIAC… Acalypha  
     4  199821 PLANTAE      TRACHEOPHY… MAGNOLIOP… MALPIGHIA… EUPHORBIAC… Acalypha  
     5      82 ANIMALIA     ARTHROPODA  INSECTA    EPHEMEROP… ACANTHAMET… Acanthame…
     6     167 ANIMALIA     MOLLUSCA    GASTROPODA STYLOMMAT… ACHATINELL… Achatinel…
     7     211 ANIMALIA     MOLLUSCA    GASTROPODA STYLOMMAT… ACHATINELL… Achatinel…
     8     170 ANIMALIA     MOLLUSCA    GASTROPODA STYLOMMAT… ACHATINELL… Achatinel…
     9     210 ANIMALIA     MOLLUSCA    GASTROPODA STYLOMMAT… ACHATINELL… Achatinel…
    10     173 ANIMALIA     MOLLUSCA    GASTROPODA STYLOMMAT… ACHATINELL… Achatinel…
    # … with 909 more rows, and 5 more variables: scientific_name <chr>,
    #   category <chr>, infra_rank <chr>, infra_name <chr>, population <chr>

Here’s a quick example to see how we can extract the approximate
extinction date from the data:

``` r
name <- extinct$scientific_name[[919]]
url <- paste0(base_url, "/api/v3/species/narrative/", name, args, token)

x <- url %>% GET() %>% content()
#Dig inside the reslut object. There's a line of text saying it was last discovered in 1990s. However, it's not the style we want. We need regular expression!

rationale <- x$result[[1]]$rationale
# How to parse this text to get the date?
rationale
```

    [1] "This species was endemic to Lord Howe Island, Australia, but it is now Extinct due to overpredation by introduced rats. It was last recorded in 1908, and not found on a survey in 1928."

To do that, we need to import the stringr library to enable the usage of
regular expression.

``` r
library(stringr)
stringr::str_extract(rationale,"\\d+")
```

    [1] "1908"

We then need this new data.

``` r
if (!file.exists("resp2.rds")) {
  resp2 <- map(url[1:20], get2)
  saveRDS(resp2, "resp2.rds")
}
resp2 <- readRDS("resp2.rds")
```

We need to generate a dataframe that has one column for scientific\_name
and one column for rationale using the method illustrated above.

``` r
status <- map_int(resp2, status_code)
all(status == 200)
```

    [1] TRUE

``` r
narrative <- map(resp2, content)
names <- map(narrative, "name")
missing <- map_lgl(names, is.null)

good_names <- names[!missing]
good_narrative <- narrative[!missing]

result <- map(good_narrative, "result")
result1 <- map(result, function(x) x[[1]])
rationale <- map(result1, "rationale")
missing_rationale <- map_lgl(rationale, is.null)

complete_narrative <- good_narrative[!missing_rationale]
complete_rationale <- rationale[!missing_rationale]
complete_names <- good_names[!missing_rationale]
narrative_df <- tibble(scientific_name = as.character(complete_names),
                       rationale = as.character(complete_rationale))
narrative_df
```

    # A tibble: 643 x 2
       scientific_name       rationale                                              
       <chr>                 <chr>                                                  
     1 Acaena exigua         "The last known individual of <em>A. exigua</em> was d…
     2 Acalypha dikuluwensis "<span style=\"font-style: italic;\">Acalypha dikuluwe…
     3 Acalypha rubrinervis  "The Stringwood (<em>Acalypha rubrinervis</em>) was la…
     4 Acalypha wilderi      "<span style=\"font-style: italic;\">Acalypha wilderi<…
     5 Achyranthes atollens… "The last known individual of <em>A. atollensis</em> w…
     6 Acipenser nudiventris "The species is known from the Black, Aral and Caspian…
     7 Acrocephalus astrola… "This species is Extinct. It was not recorded since th…
     8 Acrocephalus luscini… "This species qualifies as Extinct because of a very r…
     9 Acrocephalus musae    "This species was lost from both islands where it used…
    10 Acrocephalus nijoi    "There are no records of the species since 1995, despi…
    # … with 633 more rows

Then, we need to combine the narrative\_df and the extinct dataframe we
have earlier and add one more column “century” for future use.

``` r
mutated <- narrative_df %>% 
  left_join(extinct) %>% 
  mutate(date = stringr::str_extract(rationale, "\\d{4}"),century = stringr::str_extract(date, "\\d{2}"))
```

    Joining, by = "scientific_name"

``` r
mutated
```

    # A tibble: 663 x 15
       scientific_name rationale taxonid kingdom_name phylum_name class_name
       <chr>           <chr>       <int> <chr>        <chr>       <chr>     
     1 Acaena exigua   "The las…  4.41e4 PLANTAE      TRACHEOPHY… MAGNOLIOP…
     2 Acalypha dikul… "<span s…  1.95e5 PLANTAE      TRACHEOPHY… MAGNOLIOP…
     3 Acalypha rubri… "The Str…  3.79e4 PLANTAE      TRACHEOPHY… MAGNOLIOP…
     4 Acalypha wilde… "<span s…  2.00e5 PLANTAE      TRACHEOPHY… MAGNOLIOP…
     5 Achyranthes at… "The las…  4.41e4 PLANTAE      TRACHEOPHY… MAGNOLIOP…
     6 Acipenser nudi… "The spe…  2.51e2 ANIMALIA     CHORDATA    ACTINOPTE…
     7 Acrocephalus a… "This sp…  1.04e8 ANIMALIA     CHORDATA    AVES      
     8 Acrocephalus l… "This sp…  1.04e8 ANIMALIA     CHORDATA    AVES      
     9 Acrocephalus m… "This sp…  2.27e7 ANIMALIA     CHORDATA    AVES      
    10 Acrocephalus n… "There a…  1.04e8 ANIMALIA     CHORDATA    AVES      
    # … with 653 more rows, and 9 more variables: order_name <chr>,
    #   family_name <chr>, genus_name <chr>, category <chr>, infra_rank <chr>,
    #   infra_name <chr>, population <chr>, date <chr>, century <chr>

We would then do group\_by classname first and century next to get the
number of species that went extinct in each century.

``` r
combined <- mutated %>%
  group_by(class_name,century) %>% 
  summarise(extinct = n())
```

    `summarise()` regrouping output by 'class_name' (override with `.groups` argument)

``` r
combined
```

    # A tibble: 61 x 3
    # Groups:   class_name [19]
       class_name     century extinct
       <chr>          <chr>     <int>
     1 ACTINOPTERYGII 18            4
     2 ACTINOPTERYGII 19           43
     3 ACTINOPTERYGII 20            3
     4 ACTINOPTERYGII <NA>         36
     5 AMPHIBIA       13            1
     6 AMPHIBIA       19           11
     7 AMPHIBIA       <NA>         23
     8 ARACHNIDA      18            5
     9 ARACHNIDA      19            4
    10 AVES           15            6
    # … with 51 more rows

The final steps is to calculate the extinction rate\! We need to get the
cumulative extinction number and the total species number.

``` r
cumulative_extinction <- combined %>% 
  arrange(class_name,century) %>%
  mutate(cumulative_extinction = cumsum(extinct))
cumulative_extinction
```

    # A tibble: 61 x 4
    # Groups:   class_name [19]
       class_name     century extinct cumulative_extinction
       <chr>          <chr>     <int>                 <int>
     1 ACTINOPTERYGII 18            4                     4
     2 ACTINOPTERYGII 19           43                    47
     3 ACTINOPTERYGII 20            3                    50
     4 ACTINOPTERYGII <NA>         36                    86
     5 AMPHIBIA       13            1                     1
     6 AMPHIBIA       19           11                    12
     7 AMPHIBIA       <NA>         23                    35
     8 ARACHNIDA      18            5                     5
     9 ARACHNIDA      19            4                     9
    10 AVES           15            6                     6
    # … with 51 more rows

To get the total species number, we need a new method called “count.”

``` r
count <- all_sci_names %>% 
  count(class_name)
count
```

    # A tibble: 62 x 2
       class_name           n
       <chr>            <int>
     1 ACTINOPTERYGII   19812
     2 AGARICOMYCETES     280
     3 AMPHIBIA          6893
     4 ANDREAEOPSIDA        2
     5 ANTHOCEROTOPSIDA     2
     6 ANTHOZOA           868
     7 ARACHNIDA          344
     8 ARTHONIOMYCETES      1
     9 AVES             11147
    10 BIVALVIA           823
    # … with 52 more rows

We need to join the two tables to appraoch our final result.

``` r
final_result1 <- cumulative_extinction %>%
  inner_join(count)
```

    Joining, by = "class_name"

``` r
final_result1
```

    # A tibble: 61 x 5
    # Groups:   class_name [19]
       class_name     century extinct cumulative_extinction     n
       <chr>          <chr>     <int>                 <int> <int>
     1 ACTINOPTERYGII 18            4                     4 19812
     2 ACTINOPTERYGII 19           43                    47 19812
     3 ACTINOPTERYGII 20            3                    50 19812
     4 ACTINOPTERYGII <NA>         36                    86 19812
     5 AMPHIBIA       13            1                     1  6893
     6 AMPHIBIA       19           11                    12  6893
     7 AMPHIBIA       <NA>         23                    35  6893
     8 ARACHNIDA      18            5                     5   344
     9 ARACHNIDA      19            4                     9   344
    10 AVES           15            6                     6 11147
    # … with 51 more rows

Here is the final result table\! To make our final graph more readable,
we decide to choose classes that are more representative.

``` r
final_result2 <- final_result1 %>% 
  mutate(cumulative_extinction_rate = cumulative_extinction/n) %>%
  filter(century %in% 15:21) %>%
  filter(class_name == 'ACTINOPTERYGII'|class_name == 'AVES'|class_name =='INSECTA'|class_name =='MAMMALIA'|class_name =='REPTILIA')
final_result2
```

    # A tibble: 22 x 6
    # Groups:   class_name [5]
       class_name    century extinct cumulative_extinct…     n cumulative_extinctio…
       <chr>         <chr>     <int>               <int> <int>                 <dbl>
     1 ACTINOPTERYG… 18            4                   4 19812              0.000202
     2 ACTINOPTERYG… 19           43                  47 19812              0.00237 
     3 ACTINOPTERYG… 20            3                  50 19812              0.00252 
     4 AVES          15            6                   6 11147              0.000538
     5 AVES          16           16                  22 11147              0.00197 
     6 AVES          17           22                  44 11147              0.00395 
     7 AVES          18           47                  91 11147              0.00816 
     8 AVES          19           53                 144 11147              0.0129  
     9 AVES          20            2                 146 11147              0.0131  
    10 INSECTA       18            6                   6  9885              0.000607
    # … with 12 more rows

The last step is to generate the graph.

``` r
final_result2 %>%
  ggplot(aes(x = century,y = cumulative_extinction_rate,group=class_name,color=class_name)) + 
  geom_line()
```

![](extinction-assignment_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

## Analysis Part

*Are we experiencing the sixth great extinction?*

What is the current pace of extinction? Is it accelerating? How does it
compare to background extinction rates?

According to the graph, “AVES” represents birds, “ACTINOPTERYGII”
represents fish, “INSECTA” represents insects, “MAMMALIA” represents
mammals, and “REPTILIA” represents reptiles. The cumulative extinction
rate started accelerating significantly from the 17th century for birds
and mammals. Due to lack of data, we can only observe the extinction
rate for fish and insects from 18 to 20 century, but the cumulative
extinction is apparently increasing. For reptiles, the cumulative
extinction rate is gradually increasing as well.

Compared to the graph by Ceballos et al., our cumulative rate of
extinction smoothed from 19 century to 20 century. However, the graph by
Ceballos et al. shows that the rate is increasing in this period of
time. This could be due to missing data in our sample and
missing/different categories of animals.

From our observation, we are on the way towards the sixth great
extinction because the cumulative rate of extinction is accelerating at
an unprecedented pace. While palaeontologists characterize mass
extinctions as times when the Earth loses more than three-quarters of
its species in a geologically short interval, we think we could
potentially lead to the sixth great extinction without proper species
management. Barnosky confirmed in his paper that “current extinction
rates are higher than would be expected from the fossil record,
highlighting the need for effective conservation measures.”
