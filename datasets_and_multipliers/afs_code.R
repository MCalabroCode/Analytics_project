library(tidyverse)
library(scales)
library(openxlsx)
library(dplyr)
library(rlang)
library(readxl)
library(tidyr)
library(stringr)
library(purrr)
library(broom)
library(ggplot2)
library(RColorBrewer)
library(ggh4x)


if (requireNamespace("rstudioapi", quietly = TRUE)) { 
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  path <- getwd()
  print(path)
} else {
  message("The rstudioapi package is not available.")
}

create_sector_mapping <- function() {
  sector_mapping <- c(
    "A01_02" = "Agriculture, hunting, forestry",
    "A03" = "Fishing and aquaculture",
    "B05_06" = "Mining and quarrying, energy producing products",
    "B07_08" = "Mining and quarrying, non-energy producing products",
    "B09" = "Mining support service activities",
    "C10T12" = "Food products, beverages and tobacco",
    "C13T15" = "Textiles, textile products, leather and footwear",
    "C16" = "Wood and products of wood and cork",
    "C17_18" = "Paper products and printing",
    "C19" = "Coke and refined petroleum products",
    "C20" = "Chemical and chemical products",
    "C21" = "Pharmaceuticals, medicinal chemical and botanical products",
    "C22" = "Rubber and plastics products",
    "C23" = "Other non-metallic mineral products",
    "C24" = "Basic metals",
    "C25" = "Fabricated metal products",
    "C26" = "Computer, electronic and optical equipment",
    "C27" = "Electrical equipment",
    "C28" = "Machinery and equipment, nec",
    "C29" = "Motor vehicles, trailers and semi-trailers",
    "C30" = "Other transport equipment",
    "C31T33" = "Manufacturing nec; repair and installation of machinery and equipment",
    "D" = "Electricity, gas, steam and air conditioning supply",
    "E" = "Water supply; sewerage, waste management and remediation activities",
    "F" = "Construction",
    "G" = "Wholesale and retail trade; repair of motor vehicles",
    "H49" = "Land transport and transport via pipelines",
    "H50" = "Water transport",
    "H51" = "Air transport",
    "H52" = "Warehousing and support activities for transportation",
    "H53" = "Postal and courier activities",
    "I" = "Accommodation and food service activities",
    "J58T60" = "Publishing, audiovisual and broadcasting activities",
    "J61" = "Telecommunications",
    "J62_63" = "IT and other information services",
    "K" = "Financial and insurance activities",
    "L" = "Real estate activities",
    "M" = "Professional, scientific and technical activities",
    "N" = "Administrative and support services",
    "O" = "Public administration and defence; compulsory social security",
    "P" = "Education",
    "Q" = "Human health and social work activities",
    "R" = "Arts, entertainment and recreation",
    "S" = "Other service activities",
    "T" = "Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use"
  )
  
  return(sector_mapping)
}

compute_output_multipliers <- function(file_path) {
  library(readr)
  library(dplyr)
  
  io_table <- read_csv(file_path)
  output_row <- io_table[46, 2:46]
  output_multipliers <- as.numeric(t(output_row))
  names(output_multipliers) <- colnames(io_table)[2:46]
  
  return(output_multipliers)
}

plot_multiplier_difference <- function(country_code, country_name) {
  dir_path <- file.path("Plots", "corrected_multipliers", country_code)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  
  file_2009 <- file.path("NATLEONTFT", paste0(country_code, "2009LEONTFT.csv"))
  file_2019 <- file.path("NATLEONTFT", paste0(country_code, "2019LEONTFT.csv"))
  
  mult_2009 <- compute_output_multipliers(file_2009)
  mult_2019 <- compute_output_multipliers(file_2019)
  
  df_2009 <- tibble(Sector = names(mult_2009), Multiplier = mult_2009)
  df_2019 <- tibble(Sector = names(mult_2019), Multiplier = mult_2019)
  df_diff <- left_join(
    tibble(Sector = names(mult_2019), Multiplier_2019 = mult_2019),
    tibble(Sector = names(mult_2009), Multiplier_2009 = mult_2009),
    by = "Sector"
  ) %>% mutate(Difference = Multiplier_2019 - Multiplier_2009)
  
  plot_base <- function(df, yvar, title) {
    ggplot(df, aes(x = reorder(Sector, !!sym(yvar)), y = !!sym(yvar), fill = !!sym(yvar) > 0)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02")) +
      scale_y_continuous(labels = comma_format(accuracy = 0.1)) +
      labs(
        title = title,
        x = NULL,
        y = "Output Multiplier"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 16, face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  }
  
  # Salvataggio grafici
  p2009 <- plot_base(df_2009, "Multiplier", paste("Output Multipliers by Sector —", country_name, "(2009)"))
  ggsave(file.path(dir_path, paste0(country_code, "_multiplier_2009.pdf")), p2009, width = 10, height = 8)
  
  p2019 <- plot_base(df_2019, "Multiplier", paste("Output Multipliers by Sector —", country_name, "(2019)"))
  ggsave(file.path(dir_path, paste0(country_code, "_multiplier_2019.pdf")), p2019, width = 10, height = 8)
  
  p_diff <- ggplot(df_diff, aes(x = reorder(Sector, Difference), y = Difference, fill = Difference > 0)) +
    geom_col(width = 0.7, show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "red")) +
    labs(
      title = paste("Difference in Output Multipliers by Sector —", country_name, "(2019 vs 2009)"),
      x = NULL,
      y = "Difference in Output Multiplier"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.text.y = element_text(size = 11, face = "bold"),
      axis.title.x = element_text(size = 16),
      axis.text.x = element_text(size = 16, face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  ggsave(file.path(dir_path, paste0(country_code, "_multiplier_diff.pdf")), p_diff, width = 10, height = 8)
  
  message(paste("Saved 3 plots for", country_name, "in", dir_path))
  
  # Dataset differenze
  df_combined <- df_diff %>%
    rename(Multiplier_Diff = Difference) %>%
    left_join(df_2009, by = "Sector", suffix = c("_2019", "_2009")) %>%
    dplyr::select(Sector, Multiplier_2009, Multiplier_2019, Multiplier_Diff)
  
  # Mapping settori
  sector_mapping <- create_sector_mapping()
  df_combined$Sector_Description <- sector_mapping[df_combined$Sector]
  
  numeric_cols <- sapply(df_combined, is.numeric)
  df_combined[numeric_cols] <- lapply(df_combined[numeric_cols], function(x) round(x, 4))
  
  wb <- createWorkbook()
  addWorksheet(wb, sheetName = country_name)
  header_style <- createStyle(textDecoration = "bold")
  writeData(wb, sheet = country_name, x = df_combined, headerStyle = header_style)
  
  row_range <- 2:(nrow(df_combined) + 1)
  conditionalFormatting(
    wb,
    sheet = country_name,
    cols = 4,
    rows = row_range,
    style = c("#F8696B", "white", "#63BE7B"),
    type = "colourScale"
  )
  
  # --- Aggiunta multipliers dal 2009 al 2019 ---
  multipliers_all_years <- list()
  
  for (yr in 2000:2019) {
    file_path <- file.path("NATLEONTFT", paste0(country_code, yr, "LEONTFT.csv"))
    if (file.exists(file_path)) {
      mult <- compute_output_multipliers(file_path)
      df_year <- tibble(Sector = names(mult), !!as.character(yr) := round(mult, 4))
      multipliers_all_years[[as.character(yr)]] <- df_year
    } else {
      warning(paste("File missing for", country_code, yr))
    }
  }
  
  df_all_years <- reduce(multipliers_all_years, full_join, by = "Sector")
  df_all_years$Sector_Description <- sector_mapping[df_all_years$Sector]
  
  addWorksheet(wb, sheetName = "Multipliers_2000_2019")
  writeData(wb, sheet = "Multipliers_2000_2019", x = df_all_years, headerStyle = header_style)
  
  # Salvataggio finale
  saveWorkbook(
    wb,
    file = file.path(dir_path, paste0(country_code, "_multipliers_summary.xlsx")),
    overwrite = TRUE
  )
}

# Lista paesi
countries <- list(
  SWE = "Sweden",
  POL = "Poland",
  FIN = "Finland",
  FRA = "France",
  PRT = "Portugal",
  LVA = "Latvia",
  EST = "Estonia",
  CHE = "Switzerland",
  NOR = "Norway",
  ITA = "Italy",
  CHN = "China",
  NZL = "New Zealand",
  DEU = "Germany",
  IND = "India"
)

# Esecuzione
for (code in names(countries)) {
  plot_multiplier_difference(code, countries[[code]])
}


#---------------------------------CO2 DATA--------------------------------------

files <- list.files(path = "WorldCarbonPricingDatabase/_dataset/data/CO2/national", 
                    pattern = "*.csv", full.names = TRUE)
data_list <- lapply(files, read.csv) 
names(data_list) <- gsub("/wcpd_co2_|.csv", "", files)

co2_data <- bind_rows(data_list, .id = "Country")

co2_data$Country <- NULL

co2_datafact <- co2_data
co2_datafact$jurisdiction <- as.factor(co2_datafact$jurisdiction)
co2_datafact$year <- as.factor(co2_datafact$year)
co2_datafact$ipcc_code <- as.factor(co2_datafact$ipcc_code)
co2_datafact$ets_curr_code <- as.factor(co2_datafact$ets_curr_code)
co2_datafact$tax_curr_code <- as.factor(co2_datafact$tax_curr_code)

str(co2_datafact)
glimpse(co2_datafact)
summary(co2_datafact)

sweden <- co2_data %>%
  filter(jurisdiction == "Sweden", year >= 2009, year <= 2019)

india <- co2_data %>%
  filter(jurisdiction == "India", year >= 2009, year <= 2019)

nwz <- co2_data %>%
  filter(jurisdiction == "New Zealand", year >= 2009, year <= 2019)

switz <- co2_data %>%
  filter(jurisdiction == "Switzerland", year >= 2009, year <= 2019)

#----------------------------GET CARBON TAXES DATA------------------------------

currency = "EUR"

jurisdictions_by_currency <- list(
  #EUR = c("Sweden", "Poland", "Finland", "France", "Portugal", "Latvia", "Estonia", "Switzerland",
  #"Norway", "Italy", "China", "New Zealand", "United States", "Germany"),
  EUR = c("Sweden", "Poland", "Finland", "France", "Portugal", "Latvia", "Estonia", "Switzerland",
          "Norway")
)


exchange_rates <- tibble::tibble(
  year = 1991:2022,
  nok_to_eur = c(8.00, 8.30, 8.40, 8.20, 8.10, 8.00, 8.20, 8.40, 8.50, 8.60, 8.50, 8.40, 8.30, 8.20, 8.00, 8.10, 8.20, 8.30, 8.40, 8.50, 8.60, 8.20, 8.00, 8.20, 8.30, 8.50, 8.60, 8.30, 8.20, 8.10, 8.10, 8.10),
  sek_to_eur = c(9.00, 8.90, 8.80, 8.70, 8.60, 8.50, 8.40, 8.30, 8.20, 8.10, 8.00, 7.90, 7.80, 7.80, 7.70, 7.60, 7.50, 7.40, 7.30, 7.40, 7.50, 7.60, 7.50, 7.50, 7.40, 7.30, 7.20, 7.10, 7.00, 7.10, 7.20, 7.30),
  chf_to_eur = c(1.50, 1.52, 1.53, 1.54, 1.56, 1.58, 1.59, 1.60, 1.61, 1.62, 
                 1.64, 1.65, 1.67, 1.69, 1.70, 1.71, 1.73, 1.75, 1.73, 1.75, 
                 1.23, 1.20, 1.23, 1.22, 1.07, 1.08, 1.08, 1.15, 1.11, 1.08, 
                 1.08, 1.03),
  pln_to_eur = c(4.00, 4.10, 4.20, 4.30, 4.40, 4.50, 4.60, 4.70, 4.80, 4.90, 
                 4.80, 4.70, 4.60, 4.50, 4.40, 4.30, 4.20, 4.10, 4.30, 4.40, 
                 4.20, 4.10, 4.00, 4.00, 4.10, 4.30, 4.30, 4.30, 4.30, 4.50, 
                 4.60, 4.60)
)

# Filter the dataset for the current currency's nations
tax_values <- co2_data %>% 
  filter(jurisdiction %in% jurisdictions_by_currency[["EUR"]]) %>%
  mutate(
    currency = currency  # Add the currency to the data for reference
  ) %>%
  group_by(jurisdiction, year) %>%
  summarise(mean_tax = mean(tax_rate_incl_ex_clcu, na.rm = TRUE)) 

jurisdiction_currency_map <- co2_data %>%
  group_by(jurisdiction) %>%
  summarise(
    tax_curr_code = first(tax_curr_code[!is.na(tax_curr_code)]),
    .groups = "drop"
  ) %>% 
  filter(jurisdiction %in% jurisdictions_by_currency[["EUR"]])


tax_data <- co2_data %>% 
  filter(jurisdiction %in% jurisdictions_by_currency[["EUR"]]) %>%
  mutate(
    currency = currency  # Add the currency to the data for reference
  ) %>%
  group_by(jurisdiction, year) %>%
  summarise(mean_tax = mean(tax_rate_incl_ex_clcu, na.rm = TRUE)) %>%
  left_join(exchange_rates, by = "year") %>%
  mutate(
    mean_tax = case_when(
      jurisdiction == "Norway" ~ mean_tax / nok_to_eur,
      jurisdiction == "Poland" ~ mean_tax / pln_to_eur,
      jurisdiction == "Sweden" ~ mean_tax / sek_to_eur,
      jurisdiction == "Switzerland" ~ mean_tax * chf_to_eur,
      jurisdiction == "Liechtenstein" ~ mean_tax * chf_to_eur,
      TRUE ~ mean_tax
    )
  )

tax_data_subset <- tax_data[, 1:3]
write.csv(tax_data_subset, "outputs/tax_data.csv", row.names = FALSE)

#-------------------------------GET ETS DATA------------------------------------

jurisdictions_by_currency <- list(
  EUR = c("Sweden", "Poland", "Finland", "France", "Portugal", "Latvia", "Estonia", "Switzerland",
  "Norway", "Italy", "China", "New Zealand", "United States", "Germany")
)

ets_values <- co2_data %>% 
  filter(jurisdiction %in% jurisdictions_by_currency[["EUR"]]) %>%
  mutate(
    currency = currency  # Add the currency to the data for reference
  ) %>%
  group_by(jurisdiction, year) %>%
  summarise(mean_tax = mean(ets_price, na.rm = TRUE))

jurisdiction_currency_map <- co2_data %>%
  group_by(jurisdiction) %>%
  summarise(
    ets_curr_code = first(tax_curr_code[!is.na(ets_curr_code)]),
    .groups = "drop"
  ) %>% 
  filter(jurisdiction %in% jurisdictions_by_currency[["EUR"]])

ets_data <- data.frame(year = ets_values$year[which(ets_values == "Estonia")], 
                       EUR = ets_values$mean_tax[which(ets_values == "Estonia")],
                       CHN = ets_values$mean_tax[which(ets_values == "China")],
                       US = ets_values$mean_tax[which(ets_values == "United States")],
                       NWZ = ets_values$mean_tax[which(ets_values == "New Zealand")],
                       CHE = ets_values$mean_tax[which(ets_values == "Switzerland")])

write.csv(ets_data, "outputs/ets_data.csv", row.names = FALSE)



#-----------------------ANALYSIS------------------------------------------------

tax_data <- read.csv("outputs/tax_data.csv")
ets_data <- read.csv("outputs/ets_data.csv")


# Regression

analizza_nazione_reg <- function(nazione, path_cartella, tax_data, tax = TRUE,
                                 perm = FALSE, n_perm = 1000) {
  
  # Costruisci percorso file
  file_path <- file.path(path_cartella)
  
  # Leggi il file csv (prendi colonne 1 e 2)
  df <- read.csv(file_path)[, c(1,2)]
  
  # Filtra dati di carbon tax per la nazione
  if(tax == TRUE){
    tax_nation <- tax_data %>%
      filter(jurisdiction == nazione, year >= 2000, year <= 2019) %>%
      dplyr::select(year, mean_tax)
  } else{
    nazione_sym <- sym(nazione)
    tax_nation <- tax_data %>%
      filter(year >= 2000, year <= 2019) %>%
      dplyr::select(year, !!nazione_sym) %>%
      rename(mean_tax = !!nazione_sym) 
  }
  
  # Merge dati
  df_merged <- merge(df, tax_nation, by.x = "Year", by.y = "year") %>%
    mutate(across(everything(), ~replace_na(.x, 0)))
  
  # Calcolo correlazioni standard
  correlazione <- cor(df_merged$mean_tax, df_merged$First.Order.Degree.β, use = "complete.obs")
  p_value_spe <- cor.test(df_merged$mean_tax, df_merged$First.Order.Degree.β, method = "spearman")$p.value
  p_value_ken <- cor.test(df_merged$mean_tax, df_merged$First.Order.Degree.β, method = "kendall")$p.value
  
  p_value_perm <- NA
  
  if (perm) {
    # Permutation test per Spearman
    n <- nrow(df_merged)
    obs_stat <- cor(df_merged$mean_tax, df_merged$First.Order.Degree.β, method = "spearman")
    perm_stats <- numeric(n_perm)
    
    set.seed(123) # per riproducibilità
    for (i in seq_len(n_perm)) {
      permuted <- sample(df_merged$First.Order.Degree.β, n, replace = FALSE)
      perm_stats[i] <- cor(df_merged$mean_tax, permuted, method = "spearman")
    }
    
    # Calcolo p-value permutazione (test a due code)
    p_value_perm <- mean(abs(perm_stats) >= abs(obs_stat))
  }
  
  correlazioni <- tibble(
    correlazione = correlazione,
    p_value_spe = p_value_spe,
    p_value_ken = p_value_ken,
    p_value_perm = p_value_perm
  )
  
  print(correlazioni)
  
  return(correlazioni)
}

suppressWarnings({
# ITALY ETS
print("Italy ETS")
result <- analizza_nazione_reg(nazione = "EUR", 
                               path_cartella = "outputs/GI_reg/Italy/regression_GI_results.csv",
                               tax_data = ets_data, tax = FALSE, perm = TRUE)

# FRANCE TAX
print("FRANCE TAX")
result <- analizza_nazione_reg(nazione = "France", 
                               path_cartella = "outputs/GI_reg/France/regression_GI_results.csv",
                               tax_data = tax_data, perm = TRUE)

# FRANCE ETS
print("FRANCE ETS")
result <- analizza_nazione_reg(nazione = "EUR", 
                               path_cartella = "outputs/GI_reg/France/regression_GI_results.csv",
                               tax_data = ets_data, tax = FALSE, perm = TRUE)

# GERMANY ETS
print("GERMANY ETS")
result <- analizza_nazione_reg(nazione = "EUR", 
                               path_cartella = "outputs/GI_reg/Germany/regression_GI_results.csv",
                               tax_data = ets_data, tax = FALSE, perm = TRUE)

# SWEDEN TAX
print("SWEDEN TAX")
result <- analizza_nazione_reg(nazione = "Sweden", 
                           path_cartella = "outputs/GI_reg/Sweden/regression_GI_results.csv",
                           tax_data = tax_data, perm = TRUE)
# SWEDEN ETS
print("SWEDEN ETS")
result <- analizza_nazione_reg(nazione = "EUR", 
                               path_cartella = "outputs/GI_reg/Sweden/regression_GI_results.csv",
                               tax_data = ets_data, tax = FALSE, perm = TRUE)

# POLONIA TAX
print("POLONIA TAX")
result <- analizza_nazione_reg(nazione = "Poland", 
                               path_cartella = "outputs/GI_reg/Poland/regression_GI_results.csv",
                               tax_data = tax_data, perm = TRUE)
# POLONIA ETS
print("POLONIA ETS")
result <- analizza_nazione_reg(nazione = "EUR", 
                               path_cartella = "outputs/GI_reg/Poland/regression_GI_results.csv",
                               tax_data = ets_data, tax = FALSE, perm = TRUE)

# Switzerland TAX
print("Switzerland TAX")
result <- analizza_nazione_reg(nazione = "Switzerland", 
                               path_cartella = "outputs/GI_reg/Switzerland/regression_GI_results.csv",
                               tax_data = tax_data, perm = TRUE)
# Switzerland ETS
print("Switzerland ETS")
result <- analizza_nazione_reg(nazione = "CHE", 
                               path_cartella = "outputs/GI_reg/Switzerland/regression_GI_results.csv",
                               tax_data = ets_data, tax = FALSE, perm = TRUE)

# Finland TAX
print("Finland TAX")
result <- analizza_nazione_reg(nazione = "Finland", 
                               path_cartella = "outputs/GI_reg/Finland/regression_GI_results.csv",
                               tax_data = tax_data, perm = TRUE)
# Finland ETS
print("Finland ETS")
result <- analizza_nazione_reg(nazione = "EUR", 
                               path_cartella = "outputs/GI_reg/Finland/regression_GI_results.csv",
                               tax_data = ets_data, tax = FALSE, perm = TRUE)

# Estonia TAX
print("Estonia TAX")
result <- analizza_nazione_reg(nazione = "Estonia", 
                               path_cartella = "outputs/GI_reg/Estonia/regression_GI_results.csv",
                               tax_data = tax_data, perm = TRUE)
# Estonia ETS
print("Estonia ETS")
result <- analizza_nazione_reg(nazione = "EUR", 
                               path_cartella = "outputs/GI_reg/Estonia/regression_GI_results.csv",
                               tax_data = ets_data, tax = FALSE, perm = TRUE)

# Portugal TAX
print("Portugal TAX")
result <- analizza_nazione_reg(nazione = "Portugal", 
                               path_cartella = "outputs/GI_reg/Portugal/regression_GI_results.csv",
                               tax_data = tax_data, perm = TRUE)
# Portugal ETS
print("Portugal ETS")
result <- analizza_nazione_reg(nazione = "EUR", 
                               path_cartella = "outputs/GI_reg/Portugal/regression_GI_results.csv",
                               tax_data = ets_data, tax = FALSE, perm = TRUE)

# Norway TAX
print("Norway TAX")
result <- analizza_nazione_reg(nazione = "Norway", 
                               path_cartella = "outputs/GI_reg/Norway/regression_GI_results.csv",
                               tax_data = tax_data, perm = TRUE)
# Norway ETS
print("Norway ETS")
result <- analizza_nazione_reg(nazione = "EUR", 
                               path_cartella = "outputs/GI_reg/Norway/regression_GI_results.csv",
                               tax_data = ets_data, tax = FALSE, perm = TRUE)

# Nuova Zelanda ETS
print("Nuova Zelanda ETS")
result <- analizza_nazione_reg(nazione = "NWZ", 
                               path_cartella = "outputs/GI_reg/New Zealand/regression_GI_results.csv",
                               tax_data = ets_data, tax = FALSE, perm = TRUE)

# Spain ETS
print("Spain ETS")
result <- analizza_nazione_reg(nazione = "EUR", 
                               path_cartella = "outputs/GI_reg/Spain/regression_GI_results.csv",
                               tax_data = ets_data, tax = FALSE, perm = TRUE)

# Germany ETS
print("Germany ETS")
result <- analizza_nazione_reg(nazione = "EUR", 
                               path_cartella = "outputs/GI_reg/Germany/regression_GI_results.csv",
                               tax_data = ets_data, tax = FALSE, perm = TRUE)
})


# Multipliers

analizza_nazione <- function(nazione, path_cartella, tax_data, tax = TRUE, perm = FALSE, n_perm = 1000) {
  
  # Settori rilevanti
  carbon_tax_related_sectors <- c(
    "B05_06", "B07_08", "C19", "C20", "C23", "C24",
    "D", "E", "F", "H49", "H50", "H51"
  )
  
  # Costruisci percorso file
  file_path <- file.path(path_cartella)
  
  # Leggi il secondo foglio
  df <- read_excel(file_path, sheet = 2)
  
  # Trasforma in formato lungo
  df_long <- df %>%
    filter(Sector %in% carbon_tax_related_sectors) %>%
    pivot_longer(
      cols = matches("^20\\d{2}$"),
      names_to = "Year",
      values_to = "Multiplier"
    ) %>%
    mutate(
      Multiplier = as.numeric(str_replace(Multiplier, ",", ".")),
      Year = as.integer(Year)
    )
  
  # Filtra dati di carbon tax per la nazione
  if(tax == TRUE){
    tax_nation <- tax_data %>%
      filter(jurisdiction == nazione, year >= 2000, year <= 2019) %>%
      dplyr::select(year, mean_tax)
    
  } else{
    nazione_sym <- sym(nazione)
    # Estrai colonna della nazione dalla tabella delle tasse
    tax_nation <- tax_data %>%
      filter(year >= 2000, year <= 2019) %>%
      dplyr::select(year, !!nazione_sym) %>%
      rename(mean_tax = !!nazione_sym) 
  }
  
  # Merge
  df_merged <- merge(df_long, tax_nation, by.x = "Year", by.y = "year")  %>%
    mutate(across(everything(), ~replace_na(.x, 0)))
  
  # Funzione per permutation test di Spearman (due code)
  perm_test_spearman <- function(x, y, n_perm) {
    obs_stat <- cor(x, y, method = "spearman")
    n <- length(x)
    perm_stats <- numeric(n_perm)
    set.seed(123) # per riproducibilità
    for (i in seq_len(n_perm)) {
      y_perm <- sample(y, n, replace = FALSE)
      perm_stats[i] <- cor(x, y_perm, method = "spearman")
    }
    p_perm <- mean(abs(perm_stats) >= abs(obs_stat))
    return(p_perm)
  }
  
  # Analisi delle correlazioni per settore con permutation test opzionale
  correlazioni <- df_merged %>%
    group_by(Sector_Description) %>%
    summarise(
      correlazione = cor(mean_tax, Multiplier, use = "complete.obs"),
      p_value_spe = cor.test(mean_tax, Multiplier, method = "spearman")$p.value,
      p_value_ken = cor.test(mean_tax, Multiplier, method = "kendall")$p.value,
      p_value_perm = if (perm) perm_test_spearman(mean_tax, Multiplier, n_perm) else NA_real_
    ) %>%
    arrange(desc(correlazione))
  
  print("Correlazioni per settore:")
  print(correlazioni)
  
  return(correlazioni)
}

suppressWarnings({

  # Sweden
print("SWEDEN TAX")
result <- analizza_nazione("Sweden", 
                           "Plots/corrected_multipliers/SWE/SWE_multipliers_summary.xlsx",
                           tax_data, perm = TRUE)

# Poland
print("Poland TAX")
result <- analizza_nazione("Poland", 
                           "Plots/corrected_multipliers/POL/POL_multipliers_summary.xlsx",
                           tax_data, perm = TRUE)

# Finland
print(" Finland TAX")
result <- analizza_nazione("Finland", 
                           "Plots/corrected_multipliers/FIN/FIN_multipliers_summary.xlsx",
                           tax_data, perm = TRUE)

# Portugal
print("Portugal TAX")
result <- analizza_nazione("Portugal", 
                           "Plots/corrected_multipliers/PRT/PRT_multipliers_summary.xlsx",
                           tax_data, perm = TRUE)

# Estonia
print("Estonia TAX")
result <- analizza_nazione("Estonia", 
                           "Plots/corrected_multipliers/EST/EST_multipliers_summary.xlsx",
                           tax_data, perm = TRUE)

# Switzerland
print("Switzerland TAX")
result <- analizza_nazione("Switzerland", 
                           "Plots/corrected_multipliers/CHE/CHE_multipliers_summary.xlsx",
                           tax_data, perm = TRUE)

# Norway
print("Norway TAX")
result <- analizza_nazione("Norway", 
                           "Plots/corrected_multipliers/NOR/NOR_multipliers_summary.xlsx",
                           tax_data, perm = TRUE)

# France
print("France TAX")
result <- analizza_nazione("France", 
                           "Plots/corrected_multipliers/FRA/FRA_multipliers_summary.xlsx",
                           tax_data, perm = TRUE)

print("SWEDEN ETS")
result <- analizza_nazione("EUR", 
                           "Plots/corrected_multipliers/SWE/SWE_multipliers_summary.xlsx",
                           ets_data, tax = FALSE, perm = TRUE)

print("Poland ETS")
result <- analizza_nazione("EUR", 
                           "Plots/corrected_multipliers/POL/POL_multipliers_summary.xlsx",
                           ets_data, tax = FALSE, perm = TRUE)

print(" Finland ETS")
result <- analizza_nazione("EUR", 
                           "Plots/corrected_multipliers/FIN/FIN_multipliers_summary.xlsx",
                           ets_data, tax = FALSE, perm = TRUE)



print("Portugal ETS")
result <- analizza_nazione("EUR", 
                           "Plots/corrected_multipliers/PRT/PRT_multipliers_summary.xlsx",
                           ets_data, tax = FALSE, perm = TRUE)


print("Estonia ETS")
result <- analizza_nazione("EUR", 
                           "Plots/corrected_multipliers/EST/EST_multipliers_summary.xlsx",
                           ets_data, tax = FALSE, perm = TRUE)

print("Switzerland ETS")
result <- analizza_nazione("CHE", 
                           "Plots/corrected_multipliers/CHE/CHE_multipliers_summary.xlsx",
                           ets_data, tax = FALSE, perm = TRUE)


print("Norway ETS")
result <- analizza_nazione("EUR", 
                           "Plots/corrected_multipliers/NOR/NOR_multipliers_summary.xlsx",
                           ets_data, tax = FALSE, perm = TRUE)

# Italy
print("Italy ETS")
result <- analizza_nazione("EUR", 
                           "Plots/corrected_multipliers/ITA/ITA_multipliers_summary.xlsx",
                           ets_data, tax = FALSE, perm = TRUE)

# France
print("France ETS")
result <- analizza_nazione("EUR", 
                           "Plots/corrected_multipliers/FRA/FRA_multipliers_summary.xlsx",
                           ets_data, tax = FALSE, perm = TRUE)

# Germany
print("Germany ETS")
result <- analizza_nazione("EUR", 
                           "Plots/corrected_multipliers/DEU/DEU_multipliers_summary.xlsx",
                           ets_data, tax = FALSE, perm = TRUE)


# New Zealand
print("New Zealand ETS")
result <- analizza_nazione("NWZ", 
                           "Plots/corrected_multipliers/NZL/NZL_multipliers_summary.xlsx",
                           ets_data, tax = FALSE, perm = TRUE)

})


tax_data <- read.csv("outputs/tax_data.csv")
str(tax_data)
ets_data <- read.csv("outputs/ets_data.csv")[, c(1,2,10,11)]
colnames(ets_data) <- c("year", "ets_europe", "ets_switzerland", "ets_new_zealand")
str(ets_data)
beta_data <- read.csv("outputs/beta_GI.csv")
str(beta_data)

mult_ita_data <- read_excel("Plots/corrected_multipliers/ITA/ITA_multipliers_summary.xlsx", sheet = 2)


library(dplyr)
library(tidyr)

# Function to prepare multiplier data for each country
prep_multiplier <- function(mult_data, country_name) {
  
  carbon_tax_related_sectors <- c(
    "B05_06", "B07_08", "C19", "C20", "C23", "C24",
    "D", "E", "F", "H49", "H50", "H51"
  )
  
  mult_data %>%
    filter(Sector %in% carbon_tax_related_sectors) %>%
    pivot_longer(
      cols = starts_with("20"),     # pivot all columns with years like 2000, 2001...
      names_to = "year",
      values_to = "multiplier"
    ) %>%
    mutate(
      year = as.integer(year),
      jurisdiction = country_name
    ) %>%
    rename(sector = Sector) %>%
    select(jurisdiction, sector, year, multiplier)
}

# Prepare multiplier data for each country
mult_est_long <- prep_multiplier(read_excel("Plots/corrected_multipliers/EST/EST_multipliers_summary.xlsx", 
                                            sheet = 2),
                                 "Estonia")
mult_fin_long <- prep_multiplier(read_excel("Plots/corrected_multipliers/FIN/FIN_multipliers_summary.xlsx", 
                                            sheet = 2),
                                 "Finland")
mult_fra_long <- prep_multiplier(read_excel("Plots/corrected_multipliers/FRA/FRA_multipliers_summary.xlsx", 
                                            sheet = 2),
                                 "France")
mult_nor_long <- prep_multiplier(read_excel("Plots/corrected_multipliers/NOR/NOR_multipliers_summary.xlsx", 
                                            sheet = 2),
                                 "Norway")
mult_pol_long <- prep_multiplier(read_excel("Plots/corrected_multipliers/POL/POL_multipliers_summary.xlsx", 
                                            sheet = 2),
                                 "Poland")
mult_por_long <- prep_multiplier(read_excel("Plots/corrected_multipliers/PRT/PRT_multipliers_summary.xlsx", 
                                            sheet = 2),
                                 "Portugal")
mult_swe_long <- prep_multiplier(read_excel("Plots/corrected_multipliers/SWE/SWE_multipliers_summary.xlsx", 
                                            sheet = 2),
                                 "Sweden")
mult_swi_long <- prep_multiplier(read_excel("Plots/corrected_multipliers/CHE/CHE_multipliers_summary.xlsx", 
                                            sheet = 2),
                                 "Switzerland")

mult_ita_long <- prep_multiplier(read_excel("Plots/corrected_multipliers/ITA/ITA_multipliers_summary.xlsx", 
                                            sheet = 2),
                                 "Italy")

mult_ger_long <- prep_multiplier(read_excel("Plots/corrected_multipliers/DEU/DEU_multipliers_summary.xlsx", 
                                            sheet = 2),
                                 "Germany")

mult_chi_long <- prep_multiplier(read_excel("Plots/corrected_multipliers/CHN/CHN_multipliers_summary.xlsx", 
                                            sheet = 2),
                                 "China")

mult_ind_long <- prep_multiplier(read_excel("Plots/corrected_multipliers/IND/IND_multipliers_summary.xlsx", 
                                            sheet = 2),
                                 "India")

# Combine all multiplier data
all_multipliers_long <- bind_rows(
  mult_est_long, mult_fin_long, mult_fra_long, mult_nor_long,
  mult_pol_long, mult_por_long, mult_swe_long, mult_swi_long,
  mult_ita_long, mult_ger_long, mult_chi_long, mult_ind_long
)

# Merge tax_data with combined multiplier data
merged_data <- tax_data %>%
  inner_join(all_multipliers_long, by = c("jurisdiction", "year")) %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

library(dplyr)

combined_data <- all_multipliers_long %>%
  left_join(tax_data, by = c("jurisdiction", "year")) %>%  # keep all rows in all_multipliers_long
  mutate(
    tax_presence = ifelse(!is.na(mean_tax) & mean_tax > 0, 1, 0)
  )



# Check the merged data
head(merged_data)

library(lme4)

model <- lmer(multiplier ~ mean_tax + (1|jurisdiction), data = merged_data)
summary(model)






