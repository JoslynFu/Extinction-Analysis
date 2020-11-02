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
      Date: 2020-11-02 19:24
      Status: 200
      Content-Type: application/json; charset=utf-8
      Size: 92 B

``` r
txt <- content(resp,as = "parsed")
```

``` r
all_sci_names <- 
  purrr::map_dfr(txt$result, 
                 function(x) 
                   data.frame(sci_name = x$scientific_name,
                   kingdom = x$kingdom_name)
                 )

all_sci_names
```

    # A tibble: 0 x 0
