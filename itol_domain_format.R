############
#RNAkhand###
############

# install packages if requires!
install.packages(c("readxl", "dplyr", "tidyr", "openxlsx", "stringr"))
######################################################
# ===============================
# 1. Load libraries
# ===============================
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(stringr)

# ===============================
# 2. Read input files
# ===============================
file <- "prac_dom.xlsx"        # Give your file name here 
#install.packages(c("readxl", "dplyr", "tidyr", "openxlsx", "stringr"))
######################################################
# ===============================
# 1. Load libraries
# ===============================
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(stringr)

# ===============================
# 2. Read input files
# ===============================
file <- "prac_dom.xlsx"
# InterPro combined table
domain_df <- read_excel(file, sheet=1, col_types = "text")

# Shape / color table
shape_df  <- read_excel(file, sheet=2, col_types = "text")


# ===============================
# 2. Force ALL domain columns to character
# ===============================
domain_df <- domain_df %>%
  mutate(
    across(-c(ID, Length), as.character)
  )

# ===============================
# 3. Pivot safely (NO type conflict)
# ===============================
domain_long <- domain_df %>%
  pivot_longer(
    cols = -c(ID, Length),
    names_to = c("InterPro_ID", "Field"),
    names_pattern = "(IPR\\d+)_(Start|End|Domain_Name)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from  = Field,
    values_from = value
  )

# ===============================
# 4. Join shape & color
# ===============================
shape_df <- shape_df %>%
  rename(
    Shape_iTOL  = Shape_iTOL,   # already correct
    Domain_Name = Name,         # <-- FIX
    Hex_Code    = `Hex Code`    # <-- FIX (space needs backticks)
  )

domain_long <- domain_long %>%
  left_join(shape_df, by = "Domain_Name")

# ===============================
# 5. Create iTOL string
# ===============================
domain_long <- domain_long %>%
  mutate(
    itol_string = ifelse(
      is.na(Start),
      NA,
      paste0(
        Shape_iTOL, "|",
        Start, "|",
        End, "|",
        Hex_Code, "|",
        Domain_Name
      )
    )
  )

# ===============================
# 6. Final wide format
# ===============================
final_df <- domain_long %>%
  select(ID, Length, InterPro_ID, itol_string) %>%
  pivot_wider(
    names_from = InterPro_ID,
    values_from = itol_string
  ) %>%
  arrange(ID)

# ===============================
# 7. Export Excel
# ===============================
write.xlsx(final_df, "iTOL_domains_final.xlsx", overwrite = TRUE)


# ===============================
# 8. Replace NA with empty strings and export clean comma-separated text file
# ===============================

# Replace NA with empty strings
final_df_noNA <- final_df %>%
  mutate(across(everything(), ~replace_na(., "")))

# Convert each row to a single string with commas, removing extra commas
lines <- apply(final_df_noNA, 1, function(x) {
  # keep only single commas between values, ignore empty strings at end
  paste(x[x != ""], collapse = ",")
})

# Write to text file
writeLines(lines, "iTOL_domains_final.txt")

