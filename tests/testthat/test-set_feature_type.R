test_that("Feature type set", {
    new_sce <- set_feature_type(small_example_dataset, "gene")
    expect_equal(mainExpName(new_sce), "gene")
})
