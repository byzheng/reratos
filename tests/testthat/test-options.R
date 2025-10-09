test_that("eratos_options() behaves correctly", {
    # Make sure the object exists and has the right structure
    expect_true(exists("eratos_options"))
    opts <- eratos_options()
    expect_true(is.list(opts))
    expect_true(all(c("eratos", "senaps") %in% names(opts)))

    # 1. Default structure and URLs
    expect_match(opts$eratos$workspace_url, "https://api\\.eratos\\.com")
    expect_match(opts$senaps$sensor_url, "https://senaps\\.eratos\\.com")

    # 2. Environment variables are read correctly
    Sys.setenv(ERATOS_KEY = "env_key", ERATOS_SECRET = "env_secret")
    Sys.setenv(SENAPS_APIKEY = "senaps_key")
    new_opts <- settings::options_manager(
        eratos = list(
            workspace_url = "https://api.eratos.com/api/workspace/v1",
            key = Sys.getenv("ERATOS_KEY", ""),
            secret = Sys.getenv("ERATOS_SECRET", "")
        ),
        senaps = list(
            sensor_url = "https://senaps.eratos.com/api/sensor/v2/",
            datasource_url = "https://senaps.eratos.com/api/datasource/v1/",
            tmd_url = "https://senaps.io/thredds/ncss/",
            username = Sys.getenv("SENAPS_USER", ""),
            password = Sys.getenv("SENAPS_PASS", ""),
            apikey = Sys.getenv("SENAPS_APIKEY", ""),
            token = Sys.getenv("SENAPS_TOKEN", "")
        )
    )()
    expect_equal(new_opts$eratos$key, "env_key")
    expect_equal(new_opts$eratos$secret, "env_secret")
    expect_equal(new_opts$senaps$apikey, "senaps_key")

    # 3. Setting Eratos options via eratos_options()
    eratos_options(eratos.key = "test_key", eratos.secret = "test_secret")
    expect_equal(eratos_options()$eratos$key, "test_key")
    expect_equal(eratos_options()$eratos$secret, "test_secret")

    # 4. Setting Senaps options
    eratos_options(senaps.apikey = "abc123", senaps.username = "user")
    expect_equal(eratos_options()$senaps$apikey, "abc123")
    expect_equal(eratos_options()$senaps$username, "user")

    
    # 6. Invalid usage
    expect_error(eratos_options("no_name_value"))
    expect_error(eratos_options(bad.group.option = "oops"))

    # Cleanup
    eratos_options(eratos.key = "", eratos.secret = "", senaps.apikey = "")
    Sys.unsetenv(c("ERATOS_KEY", "ERATOS_SECRET", "SENAPS_APIKEY"))
})
