# Generate experiment start page (HTML)

Generate experiment start page (HTML)

## Usage

``` r
genstartpage(
  survey_json,
  text_randomize,
  text_starttime = NULL,
  weights_randomize = NULL,
  timeZone = "survey_json timeZone",
  file = NULL
)
```

## Arguments

- survey_json:

  Name of JSON survey file exported by gformexperiment custom add-on.

- text_randomize:

  Raw full text(s) of the question(s) that are used to randomize
  experimental conditions.

- text_starttime:

  Raw full text of the question that is used to record starting time.

- weights_randomize:

  Numeric vector or a list of numeric vectors indicating randomization
  weights. List length must be equal to the number of randomization
  questions. Length of each list element must be equal to the number of
  experimental conditions.

- timeZone:

  Timezone to record starting time. The default is to set the same time
  zone as the one set in survey_json.

- file:

  Name and path of the output html file to be saved.
