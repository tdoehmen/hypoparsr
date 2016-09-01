\name{parse_file}
\alias{parse_file}

\title{
   TODO
}
\description{
  TODO
}
\usage{
parse_file(file, configuration) 
}
\arguments{
  \item{file}{TODO}
  \item{configuration}{TODO}

  }
\value{
  TODO
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
