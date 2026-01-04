# Import Google Form survey CSV Data using JSON file information

Import Google Form survey CSV Data using JSON file information

## Usage

``` r
read_gform(
  responses_data,
  survey_json,
  DKtext = "わからない",
  DKcode = 888,
  NAtext = "答えたくない",
  NAcode = 999,
  OtherOptiontext = "その他",
  OtherOptioncode = 666,
  text_randomize = NULL,
  text_starttime = NULL,
  includeTimeStamp = TRUE,
  show_duration = TRUE,
  includeEmail = FALSE,
  survey_csv = NULL,
  ...
)
```

## Arguments

- responses_data:

  Name of either one of (1) JSON responses file exported by
  gformexperiment custom add-on or (2) CSV survey response file exported
  using Google Sheets output.

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

- text_randomize:

  Raw full text(s) of the question(s) that are used to randomize
  experimental conditions.

- text_starttime:

  Raw full text of the question that is used to record starting time.

- includeTimeStamp:

  Boolean to indicate whether to include system-generated timestamp
  variable in the codebook or not.

- show_duration:

  Boolean to indicate whether to include duration variable in the
  codebook or not (if `text_starttime` is available).

- includeEmail:

  Boolean to indicate whether to include system-generated email variable
  in the codebook or not.

- survey_csv:

  (deprecated) import responses data in CSV file (incorporated by
  `responses_data`).

- ...:

  Optional argumens passed to `read.csv()` function.
