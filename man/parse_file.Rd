\name{parse_file}
\alias{parse_file}

\title{
   Multi-hypothesis parsing of CSV files.
}
\description{
  hypoparsr takes a different approach to CSV parsing by creating different parsing hypotheses for a given file and ranking them based on data quality features. \code{parse_file} creates and returns   the ranked parsing results.
}
\usage{
parse_file(file, pruning_level = 0.1, quality_weights =
                 c(warnings = -1, edits = -1, moves = -1, confidence = 1,
                 total_cells = 1, typed_cells = 1, empty_header = -1,
                 empty_cells = -1, non_latin_chars = -1, row_col_ratio =
                 1)) 
}
\arguments{
  \item{file}{Path to a CSV file.}
  \item{pruning_level}{Numeric value between 0-1 which defined the lower threshold for confidence values of parsing hypotheses. The higher the value, the less hypotheses are created and the correct hypothesis might be omitted.}
  \item{quality_weights}{A named list of numeric quality feature weights which influence the hypothesis ranking. Positive weights improve the ranking of results with the respective characteristic and negative weights penalize the same.}
 
 }
\value{
  A \code{hypoparsr_result}, which contains all created hypotheses and their ranking. Call \code{as.data.frame()} on this object to retrieve the highest ranked parsing result.
}
\examples{
# generate a CSV
csv <- tempfile()
write.csv(iris, csv, row.names=FALSE)

# call hypoparsr
res <- hypoparsr::parse_file(csv)

# get result data frames
best_guess <- as.data.frame(res)
second_best_guess <- as.data.frame(res, rank=2)
}
