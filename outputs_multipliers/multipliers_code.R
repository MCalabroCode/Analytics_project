library(tidyverse)
library(scales)

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
  library(MASS)
  
  io_table <- read_csv(file_path)
  colnames(io_table)[1] <- "row_name"
  
  exclude_columns <- c("HFCE", "NPISH", "GGFC", "GFCF", "INVNT", "DPABR", "CONS_NONRES", "EXPO", "IMPO")
  exclude_rows <- c("TXS_IMP_FNL", "TXS_INT_FNL", "TTL_INT_FNL", "VALU", "OUTPUT")
  
  io_table_filtered <- io_table %>%
    filter(!row_name %in% exclude_rows) %>%
    dplyr::select(-any_of(intersect(exclude_columns, colnames(io_table))))
  
  all_sector_names <- colnames(io_table_filtered)[-1]
  
  io_matrix <- io_table_filtered %>%
    dplyr::select(-row_name) %>%
    as.matrix()
  
  non_zero_rows <- rowSums(io_matrix != 0) > 0
  non_zero_cols <- colSums(io_matrix != 0) > 0
  non_zero_all <- non_zero_rows & non_zero_cols
  io_matrix <- io_matrix[non_zero_all, non_zero_all, drop = FALSE]
  
  sector_names <- all_sector_names[non_zero_all]
  io_matrix <- t(io_matrix)
  
  total_output_all <- as.numeric(io_table[50, 2:46])
  total_output <- total_output_all[non_zero_all]
  total_output[total_output == 0] <- 1e-10
  
  A <- sweep(io_matrix, 2, total_output, FUN = "/")
  I <- diag(nrow(A))
  IA <- I - A
  cond_number <- kappa(IA)
  message(sprintf("Condizionamento della matrice (I - A): %.2e", cond_number))
  
  if (cond_number > 1e8) {
    message("Condizionamento elevato, uso ginv() per maggiore stabilità numerica")
    L <- ginv(IA)
  } else {
    L <- solve(IA)
  }
  
  output_multipliers <- colSums(L)
  names(output_multipliers) <- sector_names
  return(output_multipliers)
}



# Funzione aggiornata per generare e salvare i 3 plot (2009, 2019, differenza)
plot_multiplier_difference <- function(country_code, country_name) {
  dir_path <- file.path("Plots", "multipliers", country_code)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  
  file_2009 <- file.path("NATIOTTL", paste0(country_code, "2009ttl.csv"))
  file_2019 <- file.path("NATIOTTL", paste0(country_code, "2019ttl.csv"))
  
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
  
  # Plot 2009
  p2009 <- plot_base(df_2009, "Multiplier", paste("Output Multipliers by Sector —", country_name, "(2009)"))
  ggsave(file.path(dir_path, paste0(country_code, "_multiplier_2009.pdf")), p2009, width = 10, height = 8)
  
  # Plot 2019
  p2019 <- plot_base(df_2019, "Multiplier", paste("Output Multipliers by Sector —", country_name, "(2019)"))
  ggsave(file.path(dir_path, paste0(country_code, "_multiplier_2019.pdf")), p2019, width = 10, height = 8)
  
  # Plot differenza
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
  
  # Save the combined data
  df_combined <- df_diff %>%
    rename(Multiplier_Diff = Difference) %>%
    left_join(df_2009, by = "Sector", suffix = c("_2019", "_2009")) %>%
    dplyr::select(Sector, Multiplier_2009, Multiplier_2019, Multiplier_Diff)
  
  # Save to Excel instead of CSV
  library(openxlsx)
  
  # Crea mapping dei settori
  sector_mapping <- create_sector_mapping()
  
  # Aggiungi descrizione settore
  df_combined$Sector_Description <- sector_mapping[df_combined$Sector]
  
  # Arrotonda tutte le colonne numeriche a 4 cifre decimali
  numeric_cols <- sapply(df_combined, is.numeric)
  df_combined[numeric_cols] <- lapply(df_combined[numeric_cols], function(x) round(x, 4))
  
  # Crea workbook e foglio
  wb <- createWorkbook()
  addWorksheet(wb, sheetName = country_name)
  
  # Stile intestazione
  header_style <- createStyle(textDecoration = "bold")
  writeData(wb, sheet = country_name, x = df_combined, headerStyle = header_style)
  
  # Righe da formattare (senza intestazione)
  row_range <- 2:(nrow(df_combined) + 1)
  
  # Formattazione condizionale con scala semaforo ad alto contrasto
  conditionalFormatting(
    wb,
    sheet = country_name,
    cols = 4,  # "Multiplier_Diff"
    rows = row_range,
    style = c("#F8696B", "white", "#63BE7B"),  
    type = "colourScale"
  )
  
  # Salva file
  saveWorkbook(
    wb,
    file = file.path(dir_path, paste0(country_code, "_multipliers_summary.xlsx")),
    overwrite = TRUE
  )
}

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
  NZL = "New Zealand"
)

for (code in names(countries)) {
  plot_multiplier_difference(code, countries[[code]])
}



compute_output_multipliers <- function(file_path) {
  library(readr)
  library(dplyr)
  library(MASS)  # For ginv()
  
  # Load the table from CSV
  io_table <- read_csv(file_path)
  
  # Rename first column as "row_name"
  colnames(io_table)[1] <- "row_name"
  
  # Initial exclude lists
  exclude_columns <- c("HFCE", "NPISH", "GGFC", "GFCF", "INVNT", "DPABR", "CONS_NONRES", "EXPO", "IMPO")
  exclude_rows <- c("TXS_IMP_FNL", "TXS_INT_FNL", "TTL_INT_FNL", "VALU", "OUTPUT")
  
  # Step 1: Pre-filter table without removing sectors with A > 1 yet
  io_table_filtered <- io_table %>%
    filter(!row_name %in% exclude_rows) %>%
    dplyr::select(-any_of(intersect(exclude_columns, colnames(io_table))))
  
  # Extract sector names (columns except row_name)
  all_sector_names <- colnames(io_table_filtered)[-1]
  
  # Convert to numeric matrix without row_name
  io_matrix <- io_table_filtered %>%
    dplyr::select(-row_name) %>%
    as.matrix()
  
  # Remove rows and columns that are all zero
  non_zero_rows <- rowSums(io_matrix != 0) > 0
  non_zero_cols <- colSums(io_matrix != 0) > 0
  non_zero_all <- non_zero_rows & non_zero_cols
  io_matrix <- io_matrix[non_zero_all, non_zero_all, drop = FALSE]
  
  # Update sector names according to non-zero filtered matrix
  sector_names <- all_sector_names[non_zero_all]
  
  # Transpose: rows = sectors, cols = input providers
  io_matrix <- t(io_matrix)
  
  # Load total output (row 50 from original table), filtered accordingly
  total_output_all <- as.numeric(io_table[50, 2:46])
  total_output <- total_output_all[non_zero_all]
  total_output[total_output == 0] <- 1e-10
  
  # Compute technical coefficient matrix A
  A <- sweep(io_matrix, 2, total_output, FUN = "/")
  
  # Identify sectors with any coefficient > 1
  inds <- which(A > 1, arr.ind = TRUE)
  
  if(nrow(inds) > 0) {
    sectors_to_exclude <- rownames(inds)
    
    # Add these sectors to exclusion lists
    exclude_columns <- c(exclude_columns, sectors_to_exclude)
    exclude_rows <- c(exclude_rows, paste0("TTL_", sectors_to_exclude))
    
    message("Esclusi settori con coefficienti A > 1: ", paste(sectors_to_exclude, collapse = ", "))
    
    # Step 2: Re-filter io_table now excluding problematic sectors
    io_table_filtered <- io_table %>%
      filter(!row_name %in% exclude_rows) %>%
      dplyr::select(-any_of(intersect(exclude_columns, colnames(io_table))))
    
    # Update sector names again
    all_sector_names <- colnames(io_table_filtered)[-1]
    
    # Convert to numeric matrix again
    io_matrix <- io_table_filtered %>%
      dplyr::select(-row_name) %>%
      as.matrix()
    
    # Remove zero-only rows and columns again
    non_zero_rows <- rowSums(io_matrix != 0) > 0
    non_zero_cols <- colSums(io_matrix != 0) > 0
    non_zero_all <- non_zero_rows & non_zero_cols
    io_matrix <- io_matrix[non_zero_all, non_zero_all, drop = FALSE]
    
    # Final sector names
    sector_names <- all_sector_names[non_zero_all]
    
    # Transpose again
    io_matrix <- t(io_matrix)
    
    # Total output updated accordingly
    total_output_all <- as.numeric(io_table[50, setdiff(colnames(io_table)[2:46], sectors_to_exclude)])
    total_output <- total_output_all[non_zero_all]
    total_output[total_output == 0] <- 1e-10
    
    # Final technical coefficient matrix A
    A <- sweep(io_matrix, 2, total_output, FUN = "/")
    
  } else {
    message("Nessun coefficiente A > 1 rilevato, procedo senza escludere settori.")
  }
  
  # Leontief inverse matrix calculation
  I <- diag(nrow(A))
  IA <- I - A
  cond_number <- kappa(IA)
  message(sprintf("Condizionamento della matrice (I - A): %.2e", cond_number))
  
  # Use Moore-Penrose inverse if condition number too high
  if (cond_number > 1e8) {
    message("Condizionamento elevato, uso ginv() per maggiore stabilità numerica")
    L <- ginv(IA)
  } else {
    L <- solve(IA)
  }
  
  # Compute output multipliers
  output_multipliers <- colSums(L)
  names(output_multipliers) <- sector_names
  
  return(output_multipliers)
}

# Funzione aggiornata per generare e salvare i 3 plot (2009, 2019, differenza)
plot_multiplier_difference <- function(country_code, country_name) {
  dir_path <- file.path("Plots", "corrected_multipliers", country_code)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  
  file_2009 <- file.path("NATIOTTL", paste0(country_code, "2009ttl.csv"))
  file_2019 <- file.path("NATIOTTL", paste0(country_code, "2019ttl.csv"))
  
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
  
  # Plot 2009
  p2009 <- plot_base(df_2009, "Multiplier", paste("Output Multipliers by Sector —", country_name, "(2009)"))
  ggsave(file.path(dir_path, paste0(country_code, "_multiplier_2009.pdf")), p2009, width = 10, height = 8)
  
  # Plot 2019
  p2019 <- plot_base(df_2019, "Multiplier", paste("Output Multipliers by Sector —", country_name, "(2019)"))
  ggsave(file.path(dir_path, paste0(country_code, "_multiplier_2019.pdf")), p2019, width = 10, height = 8)
  
  # Plot differenza
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
  
  # Save the combined data
  df_combined <- df_diff %>%
    rename(Multiplier_Diff = Difference) %>%
    left_join(df_2009, by = "Sector", suffix = c("_2019", "_2009")) %>%
    dplyr::select(Sector, Multiplier_2009, Multiplier_2019, Multiplier_Diff)
  
  # Save to Excel instead of CSV
  library(openxlsx)
  
  # Crea mapping dei settori
  sector_mapping <- create_sector_mapping()
  
  # Aggiungi descrizione settore
  df_combined$Sector_Description <- sector_mapping[df_combined$Sector]
  
  # Arrotonda tutte le colonne numeriche a 4 cifre decimali
  numeric_cols <- sapply(df_combined, is.numeric)
  df_combined[numeric_cols] <- lapply(df_combined[numeric_cols], function(x) round(x, 4))
  
  # Crea workbook e foglio
  wb <- createWorkbook()
  addWorksheet(wb, sheetName = country_name)
  
  # Stile intestazione
  header_style <- createStyle(textDecoration = "bold")
  writeData(wb, sheet = country_name, x = df_combined, headerStyle = header_style)
  
  # Righe da formattare (senza intestazione)
  row_range <- 2:(nrow(df_combined) + 1)
  
  # Formattazione condizionale con scala semaforo ad alto contrasto
  conditionalFormatting(
    wb,
    sheet = country_name,
    cols = 4,  # "Multiplier_Diff"
    rows = row_range,
    style = c("#F8696B", "white", "#63BE7B"),  
    type = "colourScale"
  )
  
  # Salva file
  saveWorkbook(
    wb,
    file = file.path(dir_path, paste0(country_code, "_multipliers_summary.xlsx")),
    overwrite = TRUE
  )
  
  
}

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
  NZL = "New Zealand"
)

for (code in names(countries)) {
  plot_multiplier_difference(code, countries[[code]])
}



