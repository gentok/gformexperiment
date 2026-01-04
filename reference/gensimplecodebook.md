# Generate (and export) simplified codebook

Generate (and export) simplified codebook

## Usage

``` r
gensimplecodebook(
  survey_json,
  DKtext = "わからない",
  DKcode = 888,
  NAtext = "答えたくない",
  NAcode = 999,
  OtherOptiontext = "その他",
  OtherOptioncode = 666,
  includeTimeStamp = TRUE,
  includeEmail = FALSE,
  text_randomize = NULL,
  text_starttime = NULL,
  show_duration = TRUE,
  textout = NULL,
  csvout = NULL
)
```

## Arguments

- survey_json:

  Name of JSON survey file exported by gformexperiment custom add-on.

- DKtext:

  Raw full text(s) of the response option(s) that indicate "don't know"
  answers.

- DKcode:

  Response code for "don't know" answers.

- NAtext:

  Raw full text(s) of the response option(s) that indicate refused
  answers.

- NAcode:

  Response code for refused answers.

- OtherOptiontext:

  Texts in the codebook to indicate "other" answers.

- OtherOptioncode:

  Response code for "other" answers.

- includeTimeStamp:

  Boolean to indicate whether to include system-generated timestamp
  variable in the codebook or not.

- includeEmail:

  Boolean to indicate whether to include system-generated email variable
  in the codebook or not.

- text_randomize:

  Raw full text(s) of the question(s) that are used to randomize
  experimental conditions.

- text_starttime:

  Raw full text of the question that is used to record starting time.

- show_duration:

  Boolean to indicate whether to include duration variable in the
  codebook or not (if `text_starttime` is available).

- textout:

  Name and path of the output html file to be saved.

- csvout:

  Name and path of the output csv file to be saved.
