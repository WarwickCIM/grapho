library(testthat)
library(grapho)

# prevent message output for grapho
Sys.setenv(GRAPHO_TEST_ENVIRONMENT = TRUE)

test_check("grapho")
