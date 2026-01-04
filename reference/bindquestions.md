# Combine multiple variables

Combine multiple variables

## Usage

``` r
bindquestions(
  data,
  vnames,
  keep_var_label = FALSE,
  keep_val_labels = TRUE,
  reversed_loc = NULL,
  val_lim = NULL
)
```

## Arguments

- data:

  data.frame object

- vnames:

  character vector of variable names to combine.

- keep_var_label:

  Boolean to indicate whether to keep variable label (of the first
  variable in `vnames`) or not.

- keep_val_labels:

  Boolean to indicate whether to keep value labels (of the first
  variable in `vnames`) or not.

- reversed_loc:

  Integer vector to indicate the location of variables to be reversed in
  `vnames`.

- val_lim:

  Integer vector of length 2 that indicates the limits of values (only
  used when `reversed_loc` is not NULL.)
