# Generate full-information codebook dataset

Generate full-information codebook dataset

## Usage

``` r
gencodebook(
  survey_json,
  DKtext = "わからない",
  DKcode = 888,
  NAtext = "答えたくない",
  NAcode = 999,
  text_randomize = NULL,
  text_starttime = NULL
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

- text_randomize:

  Raw full text(s) of the question(s) that are used to randomize
  experimental conditions.

- text_starttime:

  Raw full text of the question that is used to record starting time.
