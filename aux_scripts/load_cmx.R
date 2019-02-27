cmx <-
  readHMDweb(CNTRY = name_cou,
             item = "cMx_1x1",
             username = id[1],
             password = id[2]) %>%
  select(-OpenInterval)
