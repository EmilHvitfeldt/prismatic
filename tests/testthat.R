library(testthat)
library(prismatic)

if (requireNamespace("xml2")) {
  test_check("prismatic", reporter = MultiReporter$new(reporters = list(JunitReporter$new(file = "test-results.xml"), CheckReporter$new())))
} else {
  test_check("prismatic")
}
