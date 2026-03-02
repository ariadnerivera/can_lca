library(knitr)
library(MplusAutomation)
library(tidyverse)
library(haven)
library(here)
library(ggpubr)
library(psych)
library(corrplot)
library(glue)
library(gt)
library(reshape2)
library(cowplot)
library(tinytex)
library(rhdf5)
library(caret)

cannabispol <- read.csv(here(
  "data",
  "3_analytic",
  "cannabis_lca_analytic_2013_2024_updated.csv"))

cannabispol <- cannabispol[order(cannabispol$id), ] 

row.names(cannabispol) <- NULL

cannabispol <- cannabispol %>%
  rename(ID = id)

state <- subset(cannabispol, select = c("ID", "styr", "stname", "stabbr", "stfips", "year"))
state <- state %>% arrange(styr)


## Main results
#standard results 
main_sd <-  readModels(here("results", "main", "sd"), quiet = TRUE)


main_results <-  state |> inner_join(main_sd$c4_lca_standard.out$savedata, by = 'ID')

main_results <- subset(main_results, select = c("styr", "stabbr", "year", "C"))

head(main_results)

#Recoded
main_results$lca_main[main_results$C == 3] <- 0 #States without RML
main_results$lca_main[main_results$C == 1] <- 1 #Early legalization
main_results$lca_main[main_results$C == 2] <- 2 #More regulated than 1
main_results$lca_main[main_results$C == 4] <- 3 #More regulated than 2

#main_results$results <- "main"

## Multilevel modeling

multilevel <-  readModels(here("results", "s2", "ml"), quiet = TRUE)

multilevel_results <-  state |> inner_join(multilevel$c4_lca_multilevel.out$savedata,
                                by = 'ID')

multilevel_results <- subset(multilevel_results, select = c("styr", "stabbr", "year", "C"))

#Recoded
multilevel_results$lca_ml[multilevel_results$C == 1] <- 0 #States that havent legalized
multilevel_results$lca_ml[multilevel_results$C == 3] <- 1 #Early legalization
multilevel_results$lca_ml[multilevel_results$C == 4] <- 2 #More regulated than 1
multilevel_results$lca_ml[multilevel_results$C == 2] <- 3 #More regulated than 2



head(multilevel_results)

multilevel_results <- subset(multilevel_results, select = c("styr", "stabbr", "year", "lca_ml"))



#multilevel_results$results <- "multilevel"

## Without DC

lca_noDC_standard <-  readModels(here("results", "noDC", "sd"), quiet = TRUE)

state_noDC  <- subset(state, stabbr != "DC")

results_noDC <-  state |> inner_join(lca_noDC_standard$c4_lca_standard.out$savedata, by = 'ID')


table(results_noDC$C)


probabilities_noDC_sd <- lca_noDC_standard$c4_lca_standard.out$parameters$probability.scale



itemprobability_noDC_sd <- probabilities_noDC_sd %>%
  mutate(
    # map 3→1, 1→2, 2→3, 4→4 by releveling then relabeling
    LatentClassRecoded = factor(LatentClass, levels = c(1, 2, 3, 4), labels = 1:4)
  ) %>%
  select(param, category, est, LatentClassRecoded) %>%
  pivot_wider(
    names_from  = LatentClassRecoded,
    values_from = est,
    names_prefix = "Class ",
    names_expand = TRUE, names_sort = TRUE
  ) %>%
  select(param, category, `Class 1`, `Class 2`, `Class 3`, `Class 4`)


print(itemprobability_noDC_sd, n = 52)


#Recoded
results_noDC$lca_noDC[results_noDC$C == 3] <- 0 #States that havent legalized
results_noDC$lca_noDC[results_noDC$C == 1] <- 1 #Pre-commercial
results_noDC$lca_noDC[results_noDC$C == 4] <- 3 #Dispensary Access
results_noDC$lca_noDC[results_noDC$C == 2] <- 2 #Full Access

table(results_noDC$lca_noDC)

names(results_noDC)

results_noDC <- subset(results_noDC, select = c("styr", "stabbr", "year", "lca_noDC"))

##### no DC multilevel 

lca_noDC_multilevel <-  readModels(here("results", "noDC", "ml"), quiet = TRUE)


state_noDC  <- subset(state, stabbr != "DC")

lca4_noDC_ml <-  state_noDC |> inner_join(lca_noDC_multilevel$savedata,
                                          by = 'ID')

table(lca4_noDC_ml$C)

lca4_noDC_ml$lca_noDC_ml[lca4_noDC_ml$C == 3] <- 0 #States that havent legalized
lca4_noDC_ml$lca_noDC_ml[lca4_noDC_ml$C == 4] <- 1 #Early legalization
lca4_noDC_ml$lca_noDC_ml[lca4_noDC_ml$C == 1] <- 2 #More regulated than 1
lca4_noDC_ml$lca_noDC_ml[lca4_noDC_ml$C == 2] <- 3 #More regulated than 2


results_noDC_ml <- subset(lca4_noDC_ml, select = c("styr",  "stabbr", "year", "lca_noDC_ml"))



## Among RCL states 


results_rcl_all <-  readModels(here("results", "rcl"), quiet = TRUE)


results_RCL <- results_rcl_all$c4_lca_rcl.out$savedata


results_RCL <-  state |> inner_join(results_RCL,
                                    by = 'ID')

results_RCL$lca_rclonly[results_RCL$C == 3] <- 0 #States without RML
results_RCL$lca_rclonly[results_RCL$C == 4] <- 1 #Early legalization
results_RCL$lca_rclonly[results_RCL$C == 1] <- 2 #More regulated than 1
results_RCL$lca_rclonly[results_RCL$C == 2] <- 3 #More regulated than 2


results_RCLonly <- subset(results_RCL, select = c("styr", "stabbr", "year", "lca_rclonly"))



# Subset 2020-2024 combined
lca_2020_2024_sd <-  readModels(here("results", "2020_2024", "sd"), quiet = TRUE)

state_2020_2024 <- subset(state, year>2019)

lca4_2020_2024_state <-  state_2020_2024 |> inner_join(lca_2020_2024_sd$savedata, by = 'ID')

#Recoded
lca4_2020_2024_state$lca_2020_2024[lca4_2020_2024_state$C == 4] <- 0 #States that havent legalized
lca4_2020_2024_state$lca_2020_2024[lca4_2020_2024_state$C == 3] <- 1 #Early legalization
lca4_2020_2024_state$lca_2020_2024[lca4_2020_2024_state$C == 1] <- 2 #More regulated than 1
lca4_2020_2024_state$lca_2020_2024[lca4_2020_2024_state$C == 2] <- 3 #More regulated than 2

results_2020_2024 <- subset(lca4_2020_2024_state, select = c("styr", "stabbr", "year", "lca_2020_2024"))


#### Results 2020_2024 ML

lca_2020_2024_ml <-  readModels(here("results", "2020_2024", "ml"), quiet = TRUE)


state_2020_2024 <- subset(state, year>2019)

lca4_2020_2024_state <-  state_2020_2024 |> inner_join(lca_2020_2024_ml$savedata, by = 'ID')

#Recoded
lca4_2020_2024_state$lca_2020_2024_ml[lca4_2020_2024_state$C == 4] <- 0 #States that havent legalized
lca4_2020_2024_state$lca_2020_2024_ml[lca4_2020_2024_state$C == 3] <- 1 #Early legalization
lca4_2020_2024_state$lca_2020_2024_ml[lca4_2020_2024_state$C == 2] <- 2 #More regulated than 1
lca4_2020_2024_state$lca_2020_2024_ml[lca4_2020_2024_state$C == 1] <- 3 #More regulated than 2


results_2020_2024_ml <- subset(lca4_2020_2024_state, select = c("styr", "stabbr", "year", "lca_2020_2024_ml"))




# Results compiled annual 2020- 2024

results_yr <-  readModels(here("results", "yr"), quiet = TRUE)

results_yr_2020 <- results_yr$c4_lca_2020.out$savedata

#Recoded


results_yr_2021 <- results_yr$c4_lca_2021.out$savedata
results_yr_2022 <- results_yr$c4_lca_2022.out$savedata
results_yr_2023 <- results_yr$c4_lca_2023.out$savedata


results_yr_2024 <- results_yr$c4_lca_2024.out$savedata
table(results_yr_2024$C)

results_yr_2020_2024 <- rbind(results_yr_2020, results_yr_2021, results_yr_2022,
                              results_yr_2023, results_yr_2024)


results_yr_2020_2024 <-  state |> inner_join(results_yr_2020_2024,
                                             by = 'ID')

classes_2024 <- results_yr_2020_2024 %>%
  filter(year == 2024) %>%
  count(stabbr, C, .drop = FALSE) %>%
  pivot_wider(names_from = C, values_from = n, values_fill = 0)


print(classes_2024, n=52)

results_yr_2024 <- results_yr$c4_lca_2024.out$savedata
table(results_yr_2024$C)
results_yr_2024$C_recoded[results_yr_2024$C == 4] <- 0 #States without RML
results_yr_2024$C_recoded[results_yr_2024$C == 2] <- 1 #Early legalization
results_yr_2024$C_recoded[results_yr_2024$C == 3] <- 2 #More regulated than 1
results_yr_2024$C_recoded[results_yr_2024$C == 1] <- 3 #More regulated than 2


classes_2023 <- results_yr_2020_2024 %>%
  filter(year == 2023) %>%
  count(stabbr, C, .drop = FALSE) %>%
  pivot_wider(names_from = C, values_from = n, values_fill = 0)


table(results_yr_2023$C)
print(classes_2023, n=52)

results_yr_2023$C_recoded[results_yr_2023$C == 4] <- 0 #States without RML
results_yr_2023$C_recoded[results_yr_2023$C == 3] <- 1 #Early legalization
results_yr_2023$C_recoded[results_yr_2023$C == 2] <- 2 #More regulated than 1
results_yr_2023$C_recoded[results_yr_2023$C == 1] <- 3 #More regulated than 2


classes_2022 <- results_yr_2020_2024 %>%
  filter(year == 2022) %>%
  count(stabbr, C, .drop = FALSE) %>%
  pivot_wider(names_from = C, values_from = n, values_fill = 0)

table(results_yr_2022$C)
print(classes_2022, n=52)


results_yr_2022$C_recoded[results_yr_2022$C == 2] <- 0 #States without RML
results_yr_2022$C_recoded[results_yr_2022$C == 4] <- 1 #Early legalization
results_yr_2022$C_recoded[results_yr_2022$C == 3] <- 2 #More regulated than 1
results_yr_2022$C_recoded[results_yr_2022$C == 1] <- 3 #More regulated than 2

classes_2021 <- results_yr_2020_2024 %>%
  filter(year == 2021) %>%
  count(stabbr, C, .drop = FALSE) %>%
  pivot_wider(names_from = C, values_from = n, values_fill = 0)

table(results_yr_2021$C)
print(classes_2021, n=52)


results_yr_2021$C_recoded[results_yr_2021$C == 3] <- 0 #States without RML
results_yr_2021$C_recoded[results_yr_2021$C == 4] <- 1 #Early legalization
results_yr_2021$C_recoded[results_yr_2021$C == 2] <- 2 #More regulated than 1
results_yr_2021$C_recoded[results_yr_2021$C == 1] <- 3 #More regulated than 2

classes_2020 <- results_yr_2020_2024 %>%
  filter(year == 2020) %>%
  count(stabbr, C, .drop = FALSE) %>%
  pivot_wider(names_from = C, values_from = n, values_fill = 0)

table(results_yr_2020$C)
print(classes_2020, n=52)


results_yr_2020$C_recoded[results_yr_2020$C == 4] <- 0 #States without RML
results_yr_2020$C_recoded[results_yr_2020$C == 1] <- 1 #Early legalization
results_yr_2020$C_recoded[results_yr_2020$C == 2] <- 2 #More regulated than 1
results_yr_2020$C_recoded[results_yr_2020$C == 3] <- 3 #More regulated than 2

###########

rm(results_yr_2020_2024)
results_yr_2020_2024 <- rbind(results_yr_2020, results_yr_2021, results_yr_2022,
                              results_yr_2023, results_yr_2024)



results_yr_2020_2024 <-  state |> inner_join(results_yr_2020_2024,
                                             by = 'ID')


names(results_yr_2020_2024)

results_yr_2020_2024_subset <- subset(results_yr_2020_2024, select = c("styr", "stabbr", "year", "C_recoded")) |>
  rename(lca_annual_2020_2024 = C_recoded)


names(results_yr_2020_2024_subset)






###

results_compiled <- main_results  |>  inner_join(multilevel_results, by = c("styr", "stabbr", "year")) |>
  left_join(results_noDC, by = c("styr", "stabbr", "year")) |>
  left_join(results_noDC_ml, by = c("styr", "stabbr", "year")) |>
  left_join(results_RCLonly, by = c("styr", "stabbr", "year")) |>
  left_join(results_2020_2024, by = c("styr", "stabbr", "year")) |>
  left_join(results_2020_2024_ml, by = c("styr", "stabbr", "year")) |>
  left_join(results_yr_2020_2024_subset, by = c("styr", "stabbr", "year"))



head(results_compiled)

## Annual results 2020-2024



all_levels <- c("0", "1", "2", "3")
results_compiled$lca_main <- factor(results_compiled$lca_main, levels = all_levels)
results_compiled$lca_ml <- factor(results_compiled$lca_ml, levels = all_levels)


main_ml <- confusionMatrix(results_compiled$lca_main, results_compiled$lca_ml)

#no DC
results_compiled_noDC <- subset(results_compiled, stabbr !="DC")
results_compiled_noDC$lca_noDC <- factor(results_compiled_noDC$lca_noDC, levels = all_levels)
main_noDC <- confusionMatrix(results_compiled_noDC$lca_main, results_compiled_noDC$lca_noDC)

#no DC multilevel
results_compiled_noDC$lca_noDC_ml <- factor(results_compiled_noDC$lca_noDC_ml, levels = all_levels)
main_noDC_ml <- confusionMatrix(results_compiled_noDC$lca_main, results_compiled_noDC$lca_noDC_ml)

# 2020-2024 pooled
results_compiled_2020_2024 <- subset(results_compiled, year>2019)
results_compiled_2020_2024$lca_2020_2024 <- factor(results_compiled_2020_2024$lca_2020_2024, levels = all_levels)

main_2020_2024pooled <- confusionMatrix(results_compiled_2020_2024$lca_main, results_compiled_2020_2024$lca_2020_2024)

# 2020-2024 pooled multilevel

results_compiled_2020_2024$lca_2020_2024_ml <- factor(results_compiled_2020_2024$lca_2020_2024_ml, levels = all_levels)
main_2020_2024pooled_ml <- confusionMatrix(results_compiled_2020_2024$lca_main, results_compiled_2020_2024$lca_2020_2024_ml)

# 2020-2024 annual
results_compiled_2020_2024$lca_annual_2020_2024 <- factor(results_compiled_2020_2024$lca_annual_2020_2024, levels = all_levels)
main_2020_2024annual <- confusionMatrix(results_compiled_2020_2024$lca_main, results_compiled_2020_2024$lca_annual_2020_2024)

# RCL only
results_compiled_rclstates <- main_results  |>  inner_join(results_RCLonly, by = c("styr", "stabbr", "year"))
results_compiled_rclstates$lca_main <- factor(results_compiled_rclstates$lca_main, levels = all_levels)
results_compiled_rclstates$lca_rclonly <- factor(results_compiled_rclstates$lca_rclonly, levels = all_levels)


main_rclsates <- confusionMatrix(results_compiled_rclstates$lca_main, results_compiled_rclstates$lca_rclonly)


#########

# Function to extract summary from confusionMatrix
extract_cm_summary <- function(cm, comparison_name) {
  # Find misclassified pairs
  mat <- cm$table
  diag(mat) <- 0
  misclass_pairs <- which(mat > 0, arr.ind = TRUE)
  
  if (nrow(misclass_pairs) > 0) {
    confused <- paste(
      sapply(1:nrow(misclass_pairs), function(i) {
        paste0(rownames(mat)[misclass_pairs[i, 1]], "↔", 
               colnames(mat)[misclass_pairs[i, 2]], 
               " (n=", mat[misclass_pairs[i, 1], misclass_pairs[i, 2]], ")")
      }), collapse = "; "
    )
  } else {
    confused <- "None"
  }
  
  data.frame(
    Comparison = comparison_name,
    N = sum(cm$table),
    Accuracy = round(cm$overall["Accuracy"], 4),
    CI_lower = round(cm$overall["AccuracyLower"], 4),
    CI_upper = round(cm$overall["AccuracyUpper"], 4),
    Kappa = round(cm$overall["Kappa"], 4),
    Misclassified = sum(mat),
    Classes_Confused = confused,
    row.names = NULL
  )
}





summary_table <- bind_rows(
  extract_cm_summary(main_ml, "Multilevel estimation"),
  extract_cm_summary(main_noDC, "Excluding DC"),
  extract_cm_summary(main_noDC_ml, "Multilevel estimation, excluding DC"),
  extract_cm_summary(main_2020_2024pooled, "Restricted to 2020–2024, pooled"),
  extract_cm_summary(main_2020_2024pooled_ml, "Multilevel estimation, restricted to 2020–2024, pooled"),
  extract_cm_summary(main_2020_2024annual, "Restricted to 2020–2024, annual"),
  extract_cm_summary(main_rclsates, "RCL states only"),
  
)

summary_table



write.csv(summary_table, here("results", "lca4_no_DC_sd_gp_summary.csv"), row.names = FALSE)



confusionMatrix(results_compiled_noDC$lca_main, results_compiled_noDC$lca_noDC)

main_ml <- confusionMatrix(results_compiled$lca_main, results_compiled$lca_ml)


agree_pct <-  mean(results_compiled$lca_main == results_compiled$lca_ml)

agree_pct <- mean(df$classification1 == df$classification2) * 100


write.csv(results_yr_2020_2024_subset, here("results", "agreement", "annualLCA_2020_2024.csv"))










