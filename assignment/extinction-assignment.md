Extinctions Unit
================
Your name, partner name

## Extinctions Module

*Are we experiencing the sixth great extinction?*

What is the current pace of extinction? Is it accelerating? How does it
compare to background extinction rates?

## Background

  - [Section Intro Video](https://youtu.be/QsH6ytm89GI)
  - [Ceballos et al (2015)](http://doi.org/10.1126/sciadv.1400253)

Our focal task will be to reproduce the result from Ceballos and
colleagues showing the recent increase in extinction rates relative to
the background rate:

![](https://espm-157.carlboettiger.info/img/extinctions.jpg)

## Computational Topics

  - Accessing data from a RESTful API
  - Error handling
  - JSON data format
  - Regular expressions
  - Working with missing values

## Additional references:

  - <http://www.hhmi.org/biointeractive/biodiversity-age-humans> (Video)
  - [Barnosky et al. (2011)](http://doi.org/10.1038/nature09678)
  - [Pimm et al (2014)](http://doi.org/10.1126/science.1246752)
  - [Sandom et al (2014)](http://dx.doi.org/10.1098/rspb.2013.3254)

<!-- end list -->

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
      Date: 2020-11-18 19:07
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

``` r
#Rectangle the full species list:#
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

``` r
#Search for extinction date#
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

``` r
library(stringr)
stringr::str_extract(rationale,"\\d+")
```

    [1] "1908"

``` r
pb <- progress_bar$new(
  format = "  RedList query [:bar] :percent eta: :eta",
  total = 100, clear = FALSE, width= 60)
get2 <- function(url){
  pb$tick()
  resp <- GET(url)
  if(status_code(resp) >= 500){
    Sys.sleep(0.1)
    resp <- GET(url)
  }
}
```

``` r
if (!file.exists("resp2.rds")) {
  resp2 <- map(url[1:20], get2)
  saveRDS(resp2, "resp2.rds")
}
resp2 <- readRDS("resp2.rds")
```

``` r
name <-extinct$scientific_name[1:919]
url <- paste0(base_url, "/api/v3/species/narrative/", name, args, token)
#resp2 <- map(url, GET)
```

``` r
status <- map_int(resp2, status_code)
all(status == 200)

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

narrative_df %>% 
  left_join(extinct) %>% 
  mutate(date = stringr::str_extract(rationale, "\\d{4}"),
         century = stringr::str_extract(date, "\\d{2}"))
```
