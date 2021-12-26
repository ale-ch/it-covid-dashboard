source("https://raw.githubusercontent.com/ale-ch/it_covid_dashboard/main/dataset_nazionale.R")
source("https://raw.githubusercontent.com/ale-ch/it_covid_dashboard/main/dataset_regionale.R")

shared_names <- names(df_regioni)[which(names(df_regioni) %in% names(df))]

covid_italy <- full_join(df, df_regioni, by = shared_names)

