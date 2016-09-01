library(testthat)

test_that("it parses a perfect csv", {
	csv <- tempfile()
	write.csv(iris, csv, row.names=F)
	res <- as.data.frame(parse_file(csv))
	res$Species <- as.factor(res$Species)
	expect_equal(iris, res, check.attributes = FALSE)
})

