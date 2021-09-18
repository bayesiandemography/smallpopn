    
library(dembase)
library(demogR)
library(dplyr)
library(tidyr)


labels_04 <- c("0", "1-4")
labels_cleaned <- c(paste(seq(0, 90, 5), seq(4, 94, 5), sep = "-"),
                    "95+")

mx_female <- (cdmltw(sex = "F")$nmx) %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    mutate(sex = "Female")

mx_male <- (cdmltw(sex = "M")$nmx) %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    mutate(sex = "Male")

mx <- bind_rows(mx_female, mx_male) %>%
    rename(level = Var1, age = Var2) %>%
    mutate(age = cleanAgeGroup(age)) %>%
    mutate(level = as.integer(level))

Lx_04_female <- (cdmltw(sex = "F")$nLx) %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    rename(level = Var1, age = Var2) %>%
    mutate(age = cleanAgeGroup(age)) %>%
    filter(age %in% labels_04) %>%
    mutate(sex = "Female")

Lx_04_male <- (cdmltw(sex = "M")$nLx) %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    rename(level = Var1, age = Var2) %>%
    mutate(age = cleanAgeGroup(age)) %>%
    filter(age %in% labels_04) %>%
    mutate(sex = "Male")

Lx_04 <- bind_rows(Lx_04_female, Lx_04_male) %>%
    mutate(level = as.integer(level)) %>%
    mutate(age = sub("-",  "", age),
           age = paste("Lx", age, sep = ".")) %>%
    pivot_wider(names_from = age, values_from = Freq) %>%
    mutate(level = as.integer(level))

mx_04 <- mx %>%
    filter(age %in% labels_04) %>%
    mutate(age = sub("-", "", age),
           age = paste("mx", age, sep = ".")) %>%
    pivot_wider(names_from = age, values_from = Freq) %>%
    left_join(Lx_04, by = c("level", "sex")) %>%
    mutate(Lower = ((4.5 * Lx.0 * mx.0 + 2 * Lx.14 * mx.14)
        / (4.5 * Lx.0 + 2 * Lx.14)),
           Upper = ((0.5 * Lx.0 * mx.0 + 3 * Lx.14 * mx.14)
        / (0.5 * Lx.0 + 3 * Lx.14))) %>%
    select(-Lx.0, -mx.0, -Lx.14, -mx.14) %>%
    pivot_longer(c(Lower, Upper), names_to = "triangle") %>%
    mutate(age = "0-4")

mx_05plus <- mx %>%
    filter(!(age %in% labels_04)) %>%
    rename(Lower = Freq) %>%
    mutate(Upper = Lower) %>%
    pivot_longer(c(Lower, Upper), names_to = "triangle")


mx <- bind_rows(mx_04, mx_05plus) %>%
    mutate(age = factor(age, levels = labels_cleaned)) %>%
    dtabs(value ~ age + triangle + sex + level)

save(mx,
     file = "data/mx.rda")
