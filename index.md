# `gformexperiment` Package

Developed by Gento Kato (Last Updated: 2026/01/05)

## Description

Generate HTML file to randomize prefilled answers in Google Forms,
generate codebook from JSON output, import and recode Google Forms
responses data file. The package website is published
[HERE](https://gentok.github.io/gformexperiment/).

## Installation

`remotes::install_github(“gentok/gformexperiment”)`

## Main Functions

- `genstartpage` Generate experiment start page (HTML).
- `gensimplecodebook` Generate (and export) simplified codebook.
- `read_gform` Import Google Form survey data using JSON survey
  information.
- `bindquestions` Combine multiple variables.

## Supplemental Functions

- `gencodebook` Generate full-information codebook dataset.

## Updates Log

- 2026/01/05 Version 0.0.2.002 Adding attach_gotoPage argument to
  gencodebook() and bug fixes in read_gform()
- 2026/01/05 Version 0.0.2.001 Expanded contents exported by
  gensimplecodebook() and gencodebook()
- 2025/01/22 Version 0.0.1.005 Bug fixes in read_gform()
- 2025/01/22 Version 0.0.1.004.1 Adjusted small typo in
  gensimplecodebook()
- 2025/01/21 Version 0.0.1.004 Bug fixes in read_gform()
- 2025/01/06 Version 0.0.1.003 Bug fixes in gencodebook()
- 2024/11/25 Version 0.0.1.002 Updates to genstartpage and read_gform()
- 2024/11/20 Version 0.0.1.001 released
