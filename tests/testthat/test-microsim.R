
test_that("test-microsim works with valid inputs", {
    library(dembase)
    for (seed in 1:5) {
        set.seed(seed)
        initial_popn <- array(rpois(n = 8, lambda = 10),
                              dim = c(4, 2),
                              dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                              sex = c("Female", "Male")))
        fertility_rates <- array(runif(80, max = c(0, 0, 2, 0)),
                                 dim = c(4, 2, 2, 5),
                                 dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                                 triangle = c("Lower", "Upper"),
                                                 sex = c("Female", "Male"),
                                                 time = c("2001-2005", "2006-2010",
                                                          "2011-2015", "2016-2020",
                                                          "2021-2025")))
        mortality_rates <- array(runif(80, max = c(0.05, 0.1, 0.1, 0.3)),
                                 dim = c(4, 2, 2, 5),
                                 dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                                 triangle = c("Lower", "Upper"),
                                                 sex = c("Female", "Male"),
                                                 time = c("2001-2005", "2006-2010",
                                                          "2011-2015", "2016-2020",
                                                          "2021-2025")))
        ans_obtained <- microsim(initial_popn = initial_popn,
                                 fertility_rates = fertility_rates,
                                 mortality_rates = mortality_rates)
        population <- ans_obtained$population
        dimnames(population) <- c(dimnames(initial_popn), list(time = seq(2000, 2025, 5)))
        population <- Counts(population)
        births <- ans_obtained$births[3, , , , drop = FALSE]
        dimnames(births) <- dimnames(fertility_rates[3, , , , drop = FALSE])
        births <- Counts(births)
        deaths <- ans_obtained$deaths
        dimnames(deaths) <- dimnames(mortality_rates)
        deaths <- Counts(deaths)
        acc <- Movements(population = population,
                         births = births,
                         exits = list(deaths = deaths))
        expect_true(all(isConsistent(acc)))
    }
})


    
