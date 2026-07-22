library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(tidyr)

isosources_file <- "isosources_grouped.txt"

normalize_iso <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("[\r\n\t]", " ") %>%
    str_squish() %>%
    str_to_lower()
}

infer_bodypart_host <- function(x) {
  x_norm <- normalize_iso(x)
  
  is_human <- str_detect(x_norm, regex(
    paste(
      c(
        "\\bhuman\\b", "\\bhomo sapiens\\b", "\\bpatient\\b", "\\bpatients\\b",
        "\\bperson\\b", "\\bpeople\\b", "\\bman\\b", "\\bwoman\\b",
        "\\bmale\\b", "\\bfemale\\b", "\\bchild\\b", "\\bchildren\\b",
        "\\binfant\\b", "\\bneonate\\b", "\\bnewborn\\b", "\\bpregnant\\b",
        "\\bmsm\\b", "\\brefugee\\b", "\\bcaretaker\\b", "\\bcare taker\\b",
        "\\bveterinarian\\b", "\\bsoldier\\b"
      ),
      collapse = "|"
    ),
    ignore_case = TRUE
  ))
  
  is_domestic_mammal <- str_detect(x_norm, regex(
    paste(
      c(
        "\\bcanine\\b", "\\bdog\\b", "\\bpuppy\\b", "\\bpet\\b",
        "\\bcat\\b", "\\bfeline\\b", "\\bfelis catus\\b",
        "\\bcanis lupus familiaris\\b", "\\bequine\\b", "\\bhorse\\b",
        "\\bstallion\\b", "\\bmare\\b", "\\brabbit\\b"
      ),
      collapse = "|"
    ),
    ignore_case = TRUE
  ))
  
  is_livestock_mammal <- str_detect(x_norm, regex(
    paste(
      c(
        "\\bswine\\b", "\\bpig\\b", "\\bpiglet\\b", "\\bsow\\b",
        "\\bcattle\\b", "\\bcow\\b", "\\bbovine\\b", "\\bcalf\\b",
        "\\bsteer\\b", "\\bbull\\b", "\\bdairy\\b", "\\bgoat\\b",
        "\\bsheep\\b", "\\blivestock\\b", "\\bmastitis\\b", "\\bteat\\b",
        "\\budder\\b"
      ),
      collapse = "|"
    ),
    ignore_case = TRUE
  )) | (
    str_detect(x_norm, "\\bmilk\\b") &
      !str_detect(x_norm, regex("mother.?s milk|breast milk|human milk", ignore_case = TRUE))
  )
  
  is_wild_mammal <- str_detect(x_norm, regex(
    paste(
      c(
        "\\bwild\\b", "\\bmouse\\b", "\\bmice\\b", "\\bmus musculus\\b",
        "\\bmonkey\\b", "\\bpanda\\b", "\\btiger\\b", "\\belephant\\b",
        "\\bboar\\b", "\\byak\\b", "\\bdeer\\b", "\\botter\\b",
        "\\bpangolin\\b", "\\bmink\\b", "\\bkangaroo\\b", "\\balpaca\\b"
      ),
      collapse = "|"
    ),
    ignore_case = TRUE
  ))
  
  is_bird <- str_detect(x_norm, regex(
    paste(
      c(
        "\\bbird\\b", "\\bchicken\\b", "\\bhen\\b", "\\bbroiler\\b",
        "\\bpoultry\\b", "\\bturkey\\b", "\\bduck\\b", "\\bgallus\\b",
        "\\bmeleagris\\b", "\\bchick\\b", "\\begg\\b", "\\bcloaca\\b"
      ),
      collapse = "|"
    ),
    ignore_case = TRUE
  ))
  
  is_invertebrate <- str_detect(x_norm, regex(
    paste(
      c(
        "\\binsect\\b", "\\bfly\\b", "\\bhoneybee\\b", "\\bbee\\b",
        "\\bant\\b", "\\btermite\\b", "\\bsilkworm\\b", "\\blarva\\b",
        "\\blarvae\\b", "\\bnematode\\b", "\\bcricket\\b", "\\bisopod\\b",
        "\\bisopoda\\b", "\\bmusca domestica\\b", "\\baedes\\b"
      ),
      collapse = "|"
    ),
    ignore_case = TRUE
  ))
  
  is_aquatic <- str_detect(x_norm, regex(
    paste(
      c(
        "\\bfish\\b", "\\bsalmon\\b", "\\bshrimp\\b", "\\bprawn\\b",
        "\\bclam\\b", "\\boyster\\b", "\\boysters\\b", "\\bcrab\\b",
        "\\bmussel\\b", "\\bmussels\\b", "\\bbivalve\\b", "\\btilapia\\b",
        "\\bmahi mahi\\b", "\\bsardine\\b", "\\bcoral\\b", "\\bseafood\\b"
      ),
      collapse = "|"
    ),
    ignore_case = TRUE
  ))
  
  is_reptile_amphibian <- str_detect(x_norm, regex(
    paste(
      c(
        "\\breptile\\b", "\\bamphibian\\b", "\\bturtle\\b", "\\bfrog\\b",
        "\\blizard\\b", "\\bsnake\\b", "\\bvaranus\\b", "\\blitoria\\b",
        "\\bnotaden\\b", "\\bpseudemoia\\b", "\\bcuora\\b", "\\bctenotus\\b",
        "\\bdiplodactylus\\b", "\\begernia\\b", "\\belseya\\b"
      ),
      collapse = "|"
    ),
    ignore_case = TRUE
  ))
  
  tibble(
    main_category = case_when(
      is_human ~ "Human",
      is_domestic_mammal | is_livestock_mammal | is_wild_mammal |
        is_bird | is_invertebrate | is_aquatic | is_reptile_amphibian ~ "Animal/Animalproduct",
      TRUE ~ "Human"
    ),
    subcategory = case_when(
      is_domestic_mammal | is_livestock_mammal | is_wild_mammal ~ "Mammals",
      is_bird ~ "Birds",
      is_invertebrate ~ "Invertebrates/Isopods",
      is_aquatic ~ "Aquatic organism",
      is_reptile_amphibian ~ "Reptiles/Amphibian",
      TRUE ~ NA_character_
    ),
    ecological_niche = case_when(
      is_domestic_mammal ~ "Domestic",
      is_livestock_mammal ~ "Livestock",
      is_wild_mammal ~ "Wild",
      TRUE ~ NA_character_
    )
  )
}

lines <- read_lines(isosources_file)

lookup <- tibble(line = lines) %>%
  mutate(line = str_trim(line)) %>%
  filter(line != "") %>%
  mutate(
    section = if_else(str_starts(line, '"'), str_remove_all(line, '^"|"$'), NA_character_)
  ) %>%
  fill(section, .direction = "down") %>%
  filter(!str_starts(line, '"')) %>%
  transmute(
    isolation_source = line,
    isolation_source_key = normalize_iso(line),
    section = case_when(
      str_detect(section, "^Body parts/fluids") ~ "Body parts/fluids",
      TRUE ~ section
    )
  ) %>%
  distinct(isolation_source_key, .keep_all = TRUE) %>%
  mutate(
    tmp = pmap(
      list(section, isolation_source),
      function(section, isolation_source) {
        if (section == "Human") {
          tibble(main_category = "Human", subcategory = NA_character_, ecological_niche = NA_character_)
        } else if (section %in% c("Hospital environment", "Farm environment", "Natural environment", "Food-production environment")) {
          tibble(main_category = "Environment", subcategory = section, ecological_niche = NA_character_)
        } else if (section == "Natural plants") {
          tibble(main_category = "Plants", subcategory = "Natural plants", ecological_niche = NA_character_)
        } else if (section == "Agricultural plants") {
          tibble(main_category = "Plants", subcategory = "Agricultural plants", ecological_niche = NA_character_)
        } else if (section == "Processed food") {
          tibble(main_category = "Miscellaneous", subcategory = "Processed food", ecological_niche = NA_character_)
        } else if (section %in% c("Unknown/Not specified")) {
          tibble(main_category = "Miscellaneous", subcategory = "Unknown", ecological_niche = NA_character_)
        } else if (section %in% c("Under-defined")) {
          tibble(main_category = "Miscellaneous", subcategory = "Underdefined", ecological_niche = NA_character_)
        } else if (section %in% c("Laboratory/In-vitro")) {
          tibble(main_category = "Miscellaneous", subcategory = "In-vitro/Lab-derived", ecological_niche = NA_character_)
        } else if (section == "Livestock") {
          if (str_detect(normalize_iso(isolation_source), regex(
            paste(c("\\bbird\\b", "\\bchicken\\b", "\\bhen\\b", "\\bbroiler\\b", "\\bpoultry\\b", "\\bturkey\\b", "\\bduck\\b", "\\bgallus\\b", "\\bmeleagris\\b", "\\bchick\\b", "\\begg\\b", "\\bcloaca\\b"), collapse = "|"),
            ignore_case = TRUE
          ))) {
            tibble(main_category = "Animal/Animalproduct", subcategory = "Birds", ecological_niche = NA_character_)
          } else {
            tibble(main_category = "Animal/Animalproduct", subcategory = "Mammals", ecological_niche = "Livestock")
          }
        } else if (section == "Domestic") {
          tibble(main_category = "Animal/Animalproduct", subcategory = "Mammals", ecological_niche = "Domestic")
        } else if (section == "Wild Mammals") {
          tibble(main_category = "Animal/Animalproduct", subcategory = "Mammals", ecological_niche = "Wild")
        } else if (section == "Birds") {
          tibble(main_category = "Animal/Animalproduct", subcategory = "Birds", ecological_niche = NA_character_)
        } else if (section == "Invertebrates/Isopods") {
          tibble(main_category = "Animal/Animalproduct", subcategory = "Invertebrates/Isopods", ecological_niche = NA_character_)
        } else if (section == "Aquatic Organisms") {
          tibble(main_category = "Animal/Animalproduct", subcategory = "Aquatic organism", ecological_niche = NA_character_)
        } else if (section == "Reptiles/Amphibian") {
          tibble(main_category = "Animal/Animalproduct", subcategory = "Reptiles/Amphibian", ecological_niche = NA_character_)
        } else if (section == "Body parts/fluids") {
          tibble(
            main_category = "Body parts/fluids",
            subcategory = "see host",
            ecological_niche = NA_character_
          )
        } else {
          tibble(main_category = "Miscellaneous", subcategory = "Underdefined", ecological_niche = NA_character_)
        }
      }
    )
  ) %>%
  unnest(tmp)

combined_metadata_isosources_grouped <- combined_metadata %>%
  mutate(
    isolation_source = coalesce(
      `NCBI_assemblyInfo.biosample.isolationSource`,
      `PW_Isolation source`
    ),
    isolation_source_key = normalize_iso(isolation_source)
  ) %>%
  left_join(
    lookup %>%
      select(isolation_source_key, main_category, subcategory, ecological_niche),
    by = "isolation_source_key"
  ) %>%
  mutate(
    main_category = case_when(
      is.na(isolation_source) ~ "Miscellaneous",
      TRUE ~ main_category
    ),
    subcategory = case_when(
      is.na(isolation_source) ~ "Unknown",
      TRUE ~ subcategory
    )
  )

combined_metadata_hostedit <- combined_metadata_isosources_grouped %>%
  mutate(
    host_subcategory = case_when(
      host_category == "Companion Animals" ~ "Domestic",
      host_category == "Livestock" ~ "Livestock",
      host_category == "Wild Mammals" ~ "Wild",
      host_category == "Rodents" ~ "Wild",
      host_category == "Birds" ~ "Birds",
      host_category == "Invertebrates" ~ "Invertebrates/Isopods",
      host_category == "Aquatic Organisms" ~ "Aquatic organism",
      host_category == "Reptiles/Amphibians" ~ "Reptiles/Amphibian",
      host_category == "Environmental Sample" ~ "see isolation source",
      host_category == "Plants" ~ "see isolation source",
      host_category == "Food" ~ "Processed food",
      host_category == "Human" ~ "Human",
      host_category == "Unknown/Not specified" ~ "Unknown",
      host_category == "Under-defined" ~ "Underdefined",
      TRUE ~ NA_character_
    ),
    host_main_category = case_when(
      host_category == "Human" ~ "Human",
      host_category %in% c(
        "Companion Animals", "Livestock", "Wild Mammals", "Rodents",
        "Birds", "Invertebrates", "Aquatic Organisms", "Reptiles/Amphibians"
      ) ~ "Animal/Animalproduct",
      host_category == "Environmental Sample" ~ "Environment",
      host_category == "Plants" ~ "Plants",
      host_category %in% c("Food", "Unknown/Not specified", "Under-defined") ~ "Miscellaneous",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-host_category)

combined_metadata_hostedit <- combined_metadata_hostedit %>%
  mutate(
    host_main_matches_isolation_main = case_when(
      is.na(host_main_category) | is.na(main_category) ~ FALSE,
      host_main_category == main_category ~ TRUE,
      TRUE ~ FALSE
    )
  )

combined_metadata_hostedit_stats <- combined_metadata_hostedit %>%
  group_by(host_main_category, main_category, subcategory, host_main_matches_isolation_main) %>%
  filter(host_main_matches_isolation_main == "FALSE") %>%
  summarise(n_entries = n())

combined_metadata_subgrouped <- combined_metadata_hostedit %>%
  mutate(
    final_main_categoy = case_when(
      isolation_source %in% c(
        "Bird",
        "Sturnus nigricollis",
        "Gull",
        "Migratory birds",
        "bird feces",
        "Black-collared Starling",
        "bubo",
        "domestic pigeon from market square",
        "duck",
        "Feces, Gull (Larus cachinnans)",
        "Feces, Gull (Larus spp.)",
        "Feces, Gull (Leucophaeus pipixcan)",
        "Malurus cyaneus",
        "Manorina melanocephala",
        "Dromaius novaehollandiae",
        "quail",
        "Red-breasted Parakeet",
        "Wild Bird",
        "crow",
        "Hill Myna",
        "Psittacus erithacus feces",
        "Eclectus roratus feces",
        "Fertilizer"
      ) ~ "Animal/Animalproduct",
      
      host_subcategory == "see isolation source" ~ main_category,
      
      host_main_matches_isolation_main == TRUE ~ main_category,
      
      subcategory == "Natural environment" &
        Host == "Ipomoea batatas" ~ "Plants",
      
      isolation_source == "Seal" &
        host_subcategory == "Wild" ~ host_main_category,
      
      isolation_source == "Milk waste container" &
        host_subcategory == "Human" ~ host_main_category,
      
      subcategory == "Processed food" &
        host_subcategory == "Livestock" ~ host_main_category,
      
      subcategory == "In-vitro/Lab-derived" &
        host_subcategory == "Wild" ~ host_main_category,
      
      subcategory == "Natural environment" &
        host_subcategory == "Domestic" ~ host_main_category,
      
      ecological_niche == "Livestock" &
        host_main_category == "Human" ~ host_main_category,
      
      isolation_source == "built" &
        host_main_category == "Miscellaneous" ~ host_main_category,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Hospital environment" &
        host_subcategory == "Wild" ~ main_category,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Food-production environment" &
        host_main_category == "Human" ~ main_category,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Natural environment" &
        host_subcategory %in% c("Aquatic organism", "Reptiles/Amphibian") ~ host_main_category,
      
      host_main_matches_isolation_main == FALSE &
        main_category == "Environment" &
        host_subcategory == "Livestock" ~ main_category,
      
      host_main_matches_isolation_main == FALSE &
        main_category == "Environment" &
        host_subcategory == "Invertebrates/Isopods" ~ main_category,
      
      host_main_matches_isolation_main == FALSE &
        main_category == "Body parts/fluids" &
        host_main_category == "Miscellaneous" ~ "Human",
      
      host_main_matches_isolation_main == FALSE &
        main_category == "Body parts/fluids" &
        host_main_category %in% c("Human", "Animal/Animalproduct") ~ host_main_category,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "In-vitro/Lab-derived" &
        host_main_category == "Human" ~ host_main_category,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Hospital environment" &
        host_main_category == "Human" ~ host_main_category,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Natural environment" &
        host_main_category == "Human" ~ host_main_category,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Farm environment" &
        host_main_category == "Animal/Animalproduct" ~ host_main_category,
      
      host_main_matches_isolation_main == FALSE &
        subcategory %in% c("Unknown", "Underdefined") &
        host_main_category != "Miscellaneous" ~ host_main_category,
      
      host_main_matches_isolation_main == FALSE &
        host_main_category == "Miscellaneous" &
        !is.na(main_category) &
        main_category != "Body parts/fluids" ~ main_category,
      
      TRUE ~ "MANUAL_CHECK"
    ),
    
    final_subcategory = case_when(
      isolation_source %in% c(
        "Bird",
        "Sturnus nigricollis",
        "Gull",
        "Migratory birds",
        "bird feces",
        "Black-collared Starling",
        "bubo",
        "domestic pigeon from market square",
        "duck",
        "Feces, Gull (Larus cachinnans)",
        "Feces, Gull (Larus spp.)",
        "Feces, Gull (Leucophaeus pipixcan)",
        "Malurus cyaneus",
        "Manorina melanocephala",
        "Dromaius novaehollandiae",
        "quail",
        "Red-breasted Parakeet",
        "Wild Bird",
        "crow",
        "Hill Myna",
        "Psittacus erithacus feces",
        "Eclectus roratus feces",
        "Fertilizer"
      ) ~ "Birds",
      
      host_subcategory == "see isolation source" ~ subcategory,
      
      host_main_matches_isolation_main == TRUE ~ subcategory,
      
      subcategory == "Natural environment" &
        Host == "Ipomoea batatas" ~ "Agricultural plants",
      
      isolation_source == "Seal" &
        host_subcategory == "Wild" ~ host_subcategory,
      
      isolation_source == "Milk waste container" &
        host_subcategory == "Human" ~ host_subcategory,
      
      subcategory == "Processed food" &
        host_subcategory == "Livestock" ~ host_subcategory,
      
      subcategory == "In-vitro/Lab-derived" &
        host_subcategory == "Wild" ~ host_subcategory,
      
      subcategory == "Natural environment" &
        host_subcategory == "Domestic" ~ host_subcategory,
      
      ecological_niche == "Livestock" &
        host_main_category == "Human" ~ host_subcategory,
      
      isolation_source == "built" &
        host_main_category == "Miscellaneous" ~ "Underdefined",
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Hospital environment" &
        host_subcategory == "Wild" ~ subcategory,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Food-production environment" &
        host_main_category == "Human" ~ subcategory,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Natural environment" &
        host_subcategory %in% c("Aquatic organism", "Reptiles/Amphibian") ~ host_subcategory,
      
      host_main_matches_isolation_main == FALSE &
        main_category == "Environment" &
        host_subcategory == "Livestock" ~ "Farm environment",
      
      host_main_matches_isolation_main == FALSE &
        main_category == "Environment" &
        host_subcategory == "Invertebrates/Isopods" ~ subcategory,
      
      host_main_matches_isolation_main == FALSE &
        main_category == "Body parts/fluids" &
        host_main_category == "Miscellaneous" ~ "Human",
      
      host_main_matches_isolation_main == FALSE &
        main_category == "Body parts/fluids" &
        host_main_category %in% c("Human", "Animal/Animalproduct") ~ host_subcategory,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "In-vitro/Lab-derived" &
        host_main_category == "Human" ~ host_subcategory,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Hospital environment" &
        host_main_category == "Human" ~ host_subcategory,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Natural environment" &
        host_main_category == "Human" ~ host_subcategory,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Farm environment" &
        host_main_category == "Animal/Animalproduct" ~ host_subcategory,
      
      host_main_matches_isolation_main == FALSE &
        subcategory %in% c("Unknown", "Underdefined") &
        host_main_category != "Miscellaneous" ~ host_subcategory,
      
      host_main_matches_isolation_main == FALSE &
        host_main_category == "Miscellaneous" &
        !is.na(main_category) &
        main_category != "Body parts/fluids" ~ subcategory,
      
      TRUE ~ "MANUAL_CHECK"
    ),
    
    final_ecological_nice = case_when(
      isolation_source %in% c(
        "Bird",
        "Sturnus nigricollis",
        "Gull",
        "Migratory birds",
        "bird feces",
        "Black-collared Starling",
        "bubo",
        "domestic pigeon from market square",
        "duck",
        "Feces, Gull (Larus cachinnans)",
        "Feces, Gull (Larus spp.)",
        "Feces, Gull (Leucophaeus pipixcan)",
        "Malurus cyaneus",
        "Manorina melanocephala",
        "Dromaius novaehollandiae",
        "quail",
        "Red-breasted Parakeet",
        "Wild Bird",
        "crow",
        "Hill Myna",
        "Psittacus erithacus feces",
        "Eclectus roratus feces",
        "Fertilizer"
      ) ~ NA_character_,
      
      host_subcategory == "see isolation source" ~ ecological_niche,
      
      host_main_matches_isolation_main == TRUE ~ ecological_niche,
      
      subcategory == "Natural environment" &
        Host == "Ipomoea batatas" ~ NA_character_,
      
      isolation_source == "Seal" &
        host_subcategory == "Wild" ~ host_subcategory,
      
      isolation_source == "Milk waste container" &
        host_subcategory == "Human" ~ NA_character_,
      
      subcategory == "Processed food" &
        host_subcategory == "Livestock" ~ host_subcategory,
      
      subcategory == "In-vitro/Lab-derived" &
        host_subcategory == "Wild" ~ host_subcategory,
      
      subcategory == "Natural environment" &
        host_subcategory == "Domestic" ~ host_subcategory,
      
      ecological_niche == "Livestock" &
        host_main_category == "Human" ~ NA_character_,
      
      isolation_source == "built" &
        host_main_category == "Miscellaneous" ~ NA_character_,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Hospital environment" &
        host_subcategory == "Wild" ~ ecological_niche,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Food-production environment" &
        host_main_category == "Human" ~ ecological_niche,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Natural environment" &
        host_subcategory %in% c("Aquatic organism", "Reptiles/Amphibian") ~ NA_character_,
      
      host_main_matches_isolation_main == FALSE &
        main_category == "Environment" &
        host_subcategory == "Livestock" ~ ecological_niche,
      
      host_main_matches_isolation_main == FALSE &
        main_category == "Environment" &
        host_subcategory == "Invertebrates/Isopods" ~ ecological_niche,
      
      host_main_matches_isolation_main == FALSE &
        main_category == "Body parts/fluids" &
        host_main_category == "Miscellaneous" ~ NA_character_,
      
      host_main_matches_isolation_main == FALSE &
        main_category == "Body parts/fluids" &
        host_main_category %in% c("Human", "Animal/Animalproduct") &
        host_subcategory %in% c("Domestic", "Livestock", "Wild") ~ host_subcategory,
      
      host_main_matches_isolation_main == FALSE &
        main_category == "Body parts/fluids" &
        host_main_category %in% c("Human", "Animal/Animalproduct") ~ NA_character_,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "In-vitro/Lab-derived" &
        host_main_category == "Human" ~ NA_character_,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Hospital environment" &
        host_main_category == "Human" ~ NA_character_,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Natural environment" &
        host_main_category == "Human" ~ NA_character_,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Farm environment" &
        host_main_category == "Animal/Animalproduct" &
        host_subcategory %in% c("Domestic", "Livestock", "Wild") ~ host_subcategory,
      
      host_main_matches_isolation_main == FALSE &
        subcategory == "Farm environment" &
        host_main_category == "Animal/Animalproduct" ~ NA_character_,
      
      host_main_matches_isolation_main == FALSE &
        subcategory %in% c("Unknown", "Underdefined") &
        host_main_category != "Miscellaneous" &
        host_subcategory %in% c("Domestic", "Livestock", "Wild") ~ host_subcategory,
      
      host_main_matches_isolation_main == FALSE &
        subcategory %in% c("Unknown", "Underdefined") &
        host_main_category != "Miscellaneous" ~ NA_character_,
      
      host_main_matches_isolation_main == FALSE &
        host_main_category == "Miscellaneous" &
        !is.na(main_category) &
        main_category != "Body parts/fluids" ~ ecological_niche,
      
      TRUE ~ "MANUAL_CHECK"
    )
  )

combined_metadata_subgrouped_edit <- combined_metadata_subgrouped %>%
  mutate(
    final_main_categoy = case_when(
      main_category == "Body parts/fluids" & Host == "Platanus x hispanica" ~ "Plants",
      main_category == "Body parts/fluids" & Host == "Plant" ~ "Plants",
      main_category == "Body parts/fluids" & Host == "Nilaparvata lugens" ~ "Animal/Animalproduct",
      main_category == "Body parts/fluids" & host_main_category == "Environment" ~ "Human",
      TRUE ~ final_main_categoy
    ),
    final_subcategory = case_when(
      main_category == "Body parts/fluids" & Host == "Platanus x hispanica" ~ "Natural plants",
      main_category == "Body parts/fluids" & Host == "Plant" ~ "Natural plants",
      main_category == "Body parts/fluids" & Host == "Nilaparvata lugens" ~ "Invertebrates/Isopods",
      main_category == "Body parts/fluids" & host_main_category == "Environment" ~ "Human",
      final_subcategory %in% c("Wild", "Domestic", "Livestock") ~ "Mammals",
      final_main_categoy == "Human" & is.na(final_subcategory) ~ "Human",
      TRUE ~ final_subcategory
    )
  )

subgroups_stats <- combined_metadata_subgrouped_edit_correction  %>%
  group_by(final_main_categoy,final_subcategory, final_ecological_nice) %>%
  summarise(n_entries = n())

#edit of the lisbon samples in the specific project PRJEB38289 and exclude the oldest sample (manually looked up)
combined_metadata_subgrouped_edit_correction <- combined_metadata_subgrouped_edit %>%
  mutate(
    final_main_categoy = case_when(
      `PW_Study accession` == "PRJEB38289" ~ "Human",
      TRUE ~ final_main_categoy
    ),
    final_subcategory = case_when(
      `PW_Study accession` == "PRJEB38289" ~ "Human",
      TRUE ~ final_subcategory
    )
  ) %>%
  filter(year != 1819)

to_check <- combined_metadata_subgrouped_edit_correction %>%
  filter(`PW_Study accession` == "PRJEB38289")

subgroups_stats <- combined_metadata_subgrouped_edit_correction  %>%
  group_by(final_main_categoy,final_subcategory, final_ecological_nice) %>%
  summarise(n_entries = n())

