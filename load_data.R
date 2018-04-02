pop <-
  readHMDweb(CNTRY = name_cou,
             item = "Population",
             username = id[1],
             password = id[2]) %>%
  select(Year, Age, Female1, Male1, Total1) %>%
  rename(Female = Female1,
         Male = Male1,
         Total = Total1) %>%
  arrange(Year, Age)

cmx <-
  readHMDweb(CNTRY = name_cou,
             item = "cMx_1x1",
             username = id[1],
             password = id[2]) %>%
  select(-OpenInterval)
