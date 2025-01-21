# <code>gformexperiment</code> Package
Developed by Gento Kato (Last Updated: 2025/01/21) 

## Description

Generate HTML file to randomize prefilled answers in Google Forms, generate codebook from JSON output, import and recode Google Forms responses data file. The package website is published [HERE](https://gentok.github.io/gformexperiment/).

## Installation

<code>remotes::install_github("gentok/gformexperiment")</code>

## Main Functions

* <code>genstartpage</code> Generate experiment start page (HTML).
* <code>gensimplecodebook</code> Generate (and export) simplified codebook.
* <code>read_gform</code> Import Google Form survey data using JSON survey information.
* <code>bindquestions</code> Combine multiple variables.

## Supplemental Functions

* <code>gencodebook</code> Generate full-information codebook dataset.

## Updates Log

* 2025/01/21 Version 0.0.1.004 Bug fixes in read_gform()
* 2025/01/06 Version 0.0.1.003 Bug fixes in gencodebook()
* 2024/11/25 Version 0.0.1.002 Updates to genstartpage and read_gform()
* 2024/11/20 Version 0.0.1.001 released
