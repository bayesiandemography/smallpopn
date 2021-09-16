
library(dembase)
library(demogR)
library(dplyr)

labels_04 <- c("0", "1-4")
labels_cleaned <- c(paste(seq(0, 90, 5), seq(4, 94, 5), sep = "-"),
                    "95+")

Lx_female <- cdmltw(sex = "F")$nLx
Lx_male <- cdmltw(sex = "M")$nLx

Lx_female <- Lx_female %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    mutate(sex = "Female")

Lx_male <- Lx_male %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    mutate(sex = "Male")

Lx <- bind_rows(Lx_female, Lx_male) %>%
    rename(level = Var1, age = Var2) %>%
    mutate(age = cleanAgeGroup(age),
           age = if_else(age %in% labels_04, "0-4", age),
           age = factor(age, levels = labels_cleaned)) %>%
    mutate(level = as.integer(level)) %>%
    dtabs(Freq ~ age + sex + level)

save(Lx,
     file = "data/Lx.rda")
