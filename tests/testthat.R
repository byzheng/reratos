library(testthat)
library(reratos)
library(httptest2)

# Get the test data
a <- capture_requests({
    result <- get_resources(limit = 2, skip = 0, sort = "@date:asc")
})

test_check("reratos")
