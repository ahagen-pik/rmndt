keys <- c("year", "region", "se", "fe", "te")

test_that("Aggregation and dis-aggregation without weights on data with multiple dimensions", {
    ## to iso countries, without weights (value reproduced)
    FEiso <- disaggregate_dt(REMIND_FinalEnergy, REMIND_RegionMap, datacols=c("se", "fe", "te"))

    ## every country gets the value of its region
    check <- merge(FEiso, REMIND_RegionMap[, c("iso", "region")], by="iso")
    check <- merge(check, REMIND_FinalEnergy, by=keys)
    expect_equal(check$value.x, check$value.y)

    ## re-aggregate and sum: the sum has to be equal to the value times the number of countries in the region
    FEsum <- aggregate_dt(FEiso, REMIND_RegionMap, datacols=c("se", "fe", "te"))
    nCountries <- REMIND_RegionMap[, .(n=.N), by="region"]
    check <- merge(merge(FEsum, REMIND_FinalEnergy, by=keys, all=TRUE), nCountries, by="region")
    expect_equal(check$value.x, check$value.y * check$n)
})

test_that("Disaggregation with GDP weights on data with multiple dimensions", {
    ## to iso countries, distribute by GDP
    FEiso <- disaggregate_dt(REMIND_FinalEnergy, REMIND_RegionMap,
                             datacols=c("se", "fe", "te"),
                             weights=REMIND_GDP)

    ## summing has to reproduce the regional data
    FEsum <- aggregate_dt(FEiso, REMIND_RegionMap, datacols=c("se", "fe", "te"))
    check <- merge(FEsum, REMIND_FinalEnergy, by=keys, all=TRUE)
    expect_equal(check$value.x, check$value.y)
})

test_that("Aggregation with GDP weights on data with multiple dimensions", {
    ## to iso countries, distribute by GDP
    FEiso <- disaggregate_dt(REMIND_FinalEnergy, REMIND_RegionMap,
                      datacols=c("se", "fe", "te"),
                      weights=REMIND_GDP)

    ## weighted average
    FEavg <- aggregate_dt(FEiso, REMIND_RegionMap,
                      datacols=c("se", "fe", "te"),
                      weights=REMIND_GDP)

    ## compare with the weighted average calculated explicitly
    expected <- merge(FEiso, REMIND_GDP[, c("iso", "year", "weight")], by=c("iso", "year"))
    expected <- merge(expected, REMIND_RegionMap[, c("iso", "region")], by="iso")
    expected <- expected[, .(value=sum(value * weight) / sum(weight)), by=keys]
    check <- merge(FEavg, expected, by=keys, all=TRUE)
    expect_equal(check$value.x, check$value.y)
})

test_that("Weighted disaggregation: column order, missing and duplicated weights", {
    mapping <- data.table(region=c("R", "R"), iso=c("A", "B"))
    data <- data.table(region="R", sector="s", period=c(2020, 2030), value=10)
    weights <- data.table(iso=c("A", "B", "A", "B"), period=c(2020, 2020, 2030, 2030), weight=c(1, 3, 1, 1))

    ## columns shared with the weights come first, as in a join weights[data]
    out <- disaggregate_dt(data, mapping, datacols="sector", weights=weights)
    expect_identical(names(out), c("iso", "period", "sector", "value"))
    expect_equal(out[period == 2020, value], c(2.5, 7.5))

    ## a missing weight gives NA for its group and a warning
    expect_warning(out <- disaggregate_dt(data, mapping, datacols="sector",
                                          weights=weights[!(iso == "B" & period == 2030)]),
                   "NAs are found")
    expect_true(all(is.na(out[period == 2030, value])))

    ## duplicated weights multiply the data rows
    out <- disaggregate_dt(data, mapping, datacols="sector",
                           weights=rbind(weights, weights[iso == "B" & period == 2020]))
    expect_equal(out[period == 2020, value], c(10 / 7, 30 / 7, 30 / 7))
})
