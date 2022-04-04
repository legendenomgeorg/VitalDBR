source("../VitalDBR/R/VitalDBR.R", chdir = TRUE)
library(testthat)

# Run with: testthat::test_dir(“tests”)
# her skal vi skrive nogle unit tests
closeAllConnections()
# Test load_VDB()
test_that("load_vdb", {
  expect_equal(load_VDB("https://api.vitaldb.net/d3b01ea0d7080f0d")[100,1], 781.876)
})

# Test fix_hz_index()
test_that("fix_hz_index()", {
  expect_equal(fix_hz_index(2,0.002), 0.004)
})

# Test check_hz()
Time <- c(0, 0.04, NA, 4)
second_column <- c(NA, NA, NA, 5)
df1 <- data.frame(Time, second_column)
second_column <- c(5)
df1_result <- data.frame(second_column)
rownames(df1_result) <- 0.04

first_column <- c(0, 1)
second_column <- c(0.04, 3)
df2 <-- data.frame(first_column, second_column)

test_that("check_hz", {
  expect_equal(check_hz(df1)[1,1], df1_result[1,1])
  expect_equal(check_hz(df1)[1,2], df1_result[1,2])
  expect_equal(check_hz(df2), df2)
})

# Test load_trk()
test_that("load_trk",{
  expect_equal(load_trk("d3b01ea0d7080f0d")[47,2], 1.7)
})

# Test load_case()
load_case <- load_case("Primus/AWP", 1)
test_that("load_case",{
  expect_equal(load_case[509372,1], 18.964)
})



