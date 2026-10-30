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

test_that("Time-invariant weights are applied separately for each year", {
    mapping <- data.table(region=c("R", "R"), iso=c("A", "B"))
    weights <- data.table(iso=c("A", "B"), weight=c(1, 3))

    ## weighted average per year
    data <- data.table(iso=c("A", "B", "A", "B"), year=c(2020, 2020, 2030, 2030), value=c(1, 3, 1, 3))
    out <- aggregate_dt(data, mapping, datacols=NULL, weights=data.table(iso=c("A", "B"), weight=1))
    expect_equal(out$value, c(2, 2))

    ## disaggregation per year, re-aggregation reproduces the regional values
    data <- data.table(region="R", year=c(2020, 2030), value=10)
    out <- disaggregate_dt(data, mapping, datacols=NULL, weights=weights)
    expect_equal(out[year == 2020, value], c(2.5, 7.5))
    expect_equal(out[year == 2030, value], c(2.5, 7.5))
    expect_equal(aggregate_dt(out, mapping)$value, c(10, 10))

    ## GDP of a single year as weights for all years
    gdp <- REMIND_GDP[year == 2020, c("iso", "weight")]
    FEiso <- disaggregate_dt(REMIND_FinalEnergy, REMIND_RegionMap, weights=gdp)
    FEsum <- aggregate_dt(FEiso, REMIND_RegionMap)
    check <- merge(FEsum, REMIND_FinalEnergy, by=keys, all=TRUE)
    expect_equal(check$value.x, check$value.y)
})

test_that("Aggregation keeps additional index columns", {
    mapping <- data.table(region=c("R", "R"), iso=c("A", "B"))
    data <- data.table(iso=c("A", "B", "A", "B"), year=2020, extra=c("x", "x", "y", "y"), value=1:4)
    out <- aggregate_dt(data, mapping)
    expect_identical(names(out), c("region", "year", "extra", "value"))
    expect_equal(nrow(out), 2)
    expect_equal(out[extra == "x", value], 3)
    expect_equal(out[extra == "y", value], 7)
})
