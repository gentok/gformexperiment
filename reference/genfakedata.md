# Generate fake data with random responses using JSON file information

Generate fake data with random responses using JSON file information

## Usage

``` r
genfakedata(
  survey_json,
  sample_size = 1000,
  rate_missing = 0.01,
  custom_weights = NULL,
  DKtext = "わからない",
  DKcode = 888,
  NAtext = "答えたくない",
  NAcode = 999,
  OtherOptiontext = "その他",
  OtherOptioncode = 666,
  text_randomize = NULL,
  text_starttime = NULL
)
```

## Arguments

- survey_json:

  Name of JSON survey file exported by gformexperiment custom add-on.

- sample_size:

  Sample size for fake data.

- rate_missing:

  The rate of missing data to occur for any variable that is not
  required.

- custom_weights:

  Named list object that indicates baseline weights for generating fake
  answers for designated questions. List element names must correspond
  to `name` and list element lengths must match with the length of
  `codes (+1 if there is 'other' option).`

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

- text_randomize:

  Raw full text(s) of the question(s) that are used to randomize
  experimental conditions.

- text_starttime:

  Raw full text of the question that is used to record starting time.
