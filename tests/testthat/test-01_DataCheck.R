test_that("check data.frame", {
    # RESHAPE FOR TEST
    ls <- list(time = 1:3, Nsurv = 10:8, replicate = "A", conc = 2.0)
    df <- data.frame(time = 1:3, Nsurv = 10:8, replicate = "A", conc = 2.0)
    # TEST
    testthat::expect_error(binaryData(ls))
    testthat::expect_error(!(binaryData(df)))
})

test_that("colnames", {
    # RESHAPE FOR TEST
    df_shorter <- Cd2[, c("time", "conc", "replicate")]
    df_longer <- Cd2 ; df_longer$ADD = "add"
    df_missing <- df_shorter ; df_missing$ADD = "add"
    # TEST
    testthat::expect_error(binaryData(df_shorter))
    testthat::expect_no_warning(binaryData(df_longer))
    testthat::expect_error(binaryData(df_missing))
})

test_that("time is (1) numeric, (2) unique, (3) minimal value is 0.s", {
    # RESHAPE FOR TEST
    df_no_numeric <- Cd2 ; df_no_numeric$time = as.character(df_no_numeric$time)
    df_no_unique <- Cd2 ; df_no_unique$time[1:3] = df_no_unique$time[3]
    df_no_min0 <- Cd2 ; df_no_min0[df_no_min0$time == 0, ]$time[1] = 1
    # TEST
    expect_error(binaryData(df_no_numeric))
    expect_error(binaryData(df_no_unique))
    expect_error(binaryData(df_no_min0))
})

test_that("concentrations are numeric", {
    # RESHAPE FOR TEST
    df_no_numeric <- Cd2 ; df_no_numeric$conc = as.character(df_no_numeric$conc)
    # TEST
    expect_error(binaryData(df_no_numeric))
})

test_that("Nsurv contains integer and positive >=0", {
    # RESHAPE FOR TEST
    df_no_intNsurv <- Cd2 ; df_no_intNsurv$Nsurv[10] = df_no_intNsurv$Nsurv[10] + 0.2
    df_no_posNsurv <- Cd2 ; df_no_posNsurv$Nsurv[10] = -10
    # TEST
    expect_error(binaryData(df_no_intNsurv))
    expect_error(binaryData(df_no_posNsurv))
})

test_that("Nsurv never decreases with time and > 0 at 0", {
    # RESHAPE FOR TEST
    df_Nurv0_at_time0 <- Cd2
    df_Nurv0_at_time0[df_Nurv0_at_time0$time == 0, ]$Nsurv[1] = 0
    df_Nurv_increase <- Cd2
    df_Nurv_increase$Nsurv[2] = df_Nurv_increase$Nsurv[1] + 2
    # TEST
    expect_error(binaryData(df_Nurv0_at_time0))
    expect_error(binaryData(df_Nurv_increase))
})

test_that("each (replicate, time) pair is unique", {
    # RESHAPE FOR TEST
    df_no_unik <- Cd2
    df_no_unik <- rbind(df_no_unik, df_no_unik[nrow(df_no_unik),])
    # TEST
    expect_error(binaryData(df_no_unik))
})

############################# REPRODUCTION DATA ONLY

test_that("Nrepro is missing", {
    # RESHAPE FOR TEST
    df_Nrepro_missing <- Cd2
    df_Nrepro_missing$Nrepro = NULL
    # TEST
    expect_no_warning(binaryData(df_Nrepro_missing))
    expect_error(countData(df_Nrepro_missing))
})

test_that("'Nrepro' must contain only positive (>=0) integer values.", {
    # RESHAPE FOR TEST
    df_no_intNrepro <- Cd2 ; df_no_intNrepro$Nrepro[10] = df_no_intNrepro$Nrepro[10] + 0.2
    df_no_posNrepro <- Cd2 ; df_no_posNrepro$Nrepro[10] = -10
    # TEST
    expect_error(countData(df_no_intNrepro))
    expect_error(countData(df_no_posNrepro))
})

test_that("each time T for which Nsurv = 0, Nrepro = 0 at time T+1.", {
    # RESHAPE FOR TEST
    df_Nsurv0 <- Cd2[Cd2$replicate == "19", ]
    df_Nsurv0[df_Nsurv0$Nsurv == 0, ]$Nrepro[2] = 10
    # TEST
    expect_error(countData(df_Nsurv0))
})
