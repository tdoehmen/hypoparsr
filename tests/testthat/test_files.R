library(testthat)

test_that("it parses our ugly collection", {
	skip("This takes way too long")
	dists <- parallel::mclapply(dir("tests/data/original", full.names=TRUE, recursive=TRUE), function(f) {
		print(f)
		clean_table = feather::read_feather(paste0("../data/cleaned/",basename(f),".feather"))

		res <- hypoparsr::parse_file(f)
		print(res)

		parsed_table = as.data.frame(res)
		clean_string=paste(as.character(unlist(clean_table)),collapse=" ")
		if(any(colnames(clean_table)!="")){
			clean_string = paste(paste(colnames(clean_table),collapse=" "),clean_string)
		}

	    parsed_string = paste(as.character(unlist(parsed_table)),collapse=" ")
	    if(any(colnames(parsed_table)!="")){
	        parsed_string = paste(paste(colnames(parsed_table),collapse=" "),parsed_string)
	    }

	   dist = RecordLinkage::levenshteinDist(parsed_string,clean_string)
	   print(dist)
	   dist
	})
	expect_true(median(as.integer(unlist(dists)), na.rm=T) <= 10)
})

