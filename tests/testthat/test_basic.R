library(testthat)

test_that("various error cases work", {
	res <- parse_file("fuuu")
	expect_true(!is.null(res$error))
	expect_warning({df <- as.data.frame(res)})
	expect_equal(data.frame(), df)
})

test_that("it parses a perfect csv", {
	csv <- tempfile()
	write.csv(iris, csv, row.names=F)
	pres <- parse_file(csv)
	res <- as.data.frame(pres)
	res$Species <- as.factor(res$Species)
	expect_equal(iris, res, check.attributes = FALSE)
	expect_warning({as.data.frame(pres, rank=100)})
})

