# Load packages
library(dplyr)
library(readr)
library(stringr)

# Read the input file
df <- read_csv("species_hosts.csv")

# Define classification function
categorize_host <- function(host) {
  host <- tolower(trimws(host))
  
  if (is.na(host) || host %in% c("-", "not available", "unknown", "missing", "na", "not applicable", "restricted access", "not provided", "none", "Not collected", "not available: not collected", "not aplicable", "n/a", "not collected")) {
    return("Unknown/Not specified")
  } else if (str_detect(host, "homo sapiens|human|children|male|female|patient|homo|homosapiens|homos sapiens|omo sapiens|homp sapiens|hono sapiens|homo-sapien|hom sapiens|home sapiens|homo�sapiens|human being")) {
    return("Human")
  } else if (str_detect(host, "dog|cat|canine|feline|felis|felidae|rabbit|canis|canis lupus familiaris|equine|horse|equus caballus|equus")) {
    return("Companion Animals")
  } else if (str_detect(host, "poultry|broiler|chicken|beef|cow|pig|sheep|goat|calf|laying hen|buffalo|capra|bubalus|bovine|bos taurus|poultry|swine|porcine|yak|ovine|gallus gallus|bos indicus|holstein friesians|ovis aries|capra hircus|livestock|gallus gallus domesticus|sus domesticus|chickens|sus scrofa domesticus")) {
    return("Livestock")
  } else if (str_detect(host, "bat|deer|monkey|panda|wolf|fox|camel|elephant|kangaroo|callithrix|primate|oryctolagus cuniculus|opossum|sika|mink|roe buck|wild boar|sea mammal|fennec|alpine ibex|coypu|phoca largha|canis lupus|hedgehog|sus scrofa cristatus|vulpes zerda|phascolarctos cinereus|pan troglodytes verus|ailuropoda melanoleuca|sus scrofa|saimiri boliviensis|pteropus poliocephalus|phocarctos hookeri|phoca vitulina|rhesus macaques|panthera tigris amoyensis facel|caracal caracal|malayan pangolin|macaca fascicularis")) { #mammals
    return("Wild Mammals")
  } else if (str_detect(host, "rodent|rattus norvegicus|mus musculus|mus musculus c57bl/6j|mouse|rattus rattus")) {
    return("Rodents")
  } else if (str_detect(host, "bird|duck|goose|swan|crow|sparrow|avian|anser|columba|godwit|rook|branta|bubo|pica pica|peafowl|wild chukar|amazona aestiva|turkey|gull|corvus|wader|milvus milvus|curlew sandpiper|pandion haliaetus|haliaeetus albicilla|chroicocephalus ridibundus|milvus migrans|sus scrofa domesticus|ptyonoprogne rupestris|anas poecilorhyncha|quiscalus lugubris|larus michahellis|meleagris gallopavo|stercorarius antarcticus|dendrocygna viduata|magellanic penguin|circus aeruginosus|megadyptes antipodes|passer domesticus|cygnus olor|aratinga solstitialis|eclectus roratus|neotropic cormorant|larus michahellis")) { #birds
    return("Birds")
  } else if (str_detect(host, "fish|carp|tilapia|trout|salmon|mola|danio|cod|zebrafish|aulonocara|anabas|aphyocharax|rock cod|crab|callinectes|shrimp|bivalve|oyster|salmo salar|thunnus albacare|osphronemus goramy|labidochromis caeruleus|arapaima gigas|pangasius pangasius|mastacembelus|poecilia reticulata|penaeus vannamei|sparus aurata|osteoglossum bicirrhosum|mystus tengara|channa punctata|labeo rohita|cirrhinus mrigala|procambarus clarkii|penaeus sp.|eriocheir sinensis|sperata seenghala|magallana angulata|lamellidens marginalis|mytilus edulis|misgurnus anguillicaudatus|clarias magur|tegillarcagranosa|ophichthys cuchia|piaractus brachypomus")) { #aquas
    return("Aquatic Organisms")
  } else if (str_detect(host, "frog|lizard|turtle|salamander|reptile|amphibian|atelopus|clemmys|snake|anolis|lutra lutra|serpentes|pelodiscus sinensis|chelonia mydas|stigmochelys pardalis|ptyas mucosa|gopherus berlandieri|reptiles")) { #rep/amp
    return("Reptiles/Amphibians")
  } else if (str_detect(host, "mosquito|insect|fly|termite|bee|aphid|aedes|anopheles|bactrocera|drosophila|silkworm|worm|albitarsis|evuncifer|fruitfly|zophobas morio|flies|microtermes|microtermes sp.|grasshopper|phgllotreta striolaea|cricket|heterometopus palaeformis|musca domestica|cetonia aurata|trinervitermes sp.|macrotermes bellicosus|chlrocebus sabaeus|microcerotermes sp.|mastotermes darwiniensis|tenebrio molitor|periplaneta americana|zophobas atratus|zeugodacus cucurbitae|promirotermes sp.|macrotermes subhyalinus|macrotermes sp.|ancistrotermes cavithorax|spodoptera frugiperda|anopheline darlingi|ceratitis capitata|procubitermes undulans|rhodnius prolixus|curculio caryae|dryococelus australis|leptinotarsa decemlineata|hermetia illucens|nasonia giraulti|nasonia vitripennis|nasonia sp. vxg hybrid|nasonia vitripennis x nasonia giraulti hybrid|caenorhabditis elegans|gryllotalpa orientalis|oscheius sp. mcb|anastrepha ludens|costelytra giveni|apis mellifera|varroa destructor|nezara viridula|melipona interrupta|mexican fruit fly: anastrepha ludens|anastrepha ludens black pupae (bp) genetic sexing strain guatemala-10 (aka family 10);anastrepha ludens black pupae|anastrepha ludens")) { #insects
    return("Invertebrates")
  } else if (str_detect(host, "banana|lettuce|pepper|tomato|mulberry|maize|cabbage|chili|arabidopsis|sativa|cannabis|cantonese cabbage|seagrass|plant|bambusa|agave|brassica|sugar|grain|crop|peanut|arachis|fistulosum|costatum|oleracea|rapa |cucumber|strawberry|cucurbita pepo|tobacco|dragonfruit|turnip|psammochloa|morus alba|faba bean|gossypium|grapevine|zea mays|grass|platanus x hispanica|solanum nigrum|uvaria chamae|sorghum bicolor|pistia stratiotes|capsicum annuum cv. king arthur|coscinium fenestratum|vigna radiata l.|cakile maritima|musa sp.|hemionitis calomelanos|malus domestica 'gala'|nicotiana tabacum l yunyan 97|mimosa pudica|zea nicaraguensis|phoenix dactylifera|suaeda glauca|hemionitis calomelanos|musa acuminata|zingiber officinale|madhuca insignis|solanum lycopersicum l.|beta vulgaris|capsicum annuum|salacia chinensis|daucus carota|cucumis sativus|philodendron erubescens|jatropha curcas|sarracenia|coleus scutellarioides|indigofera argentea burm.f.|pontederia crassipes|sterculia sp.|oxytenanthera abyssinica|solanum lycopersicum|raphanus sativus|cenchrus macrourus|desmodium pulchellum|garcinia xanthochymus|helianthus tuberosus|oryza glaberrima|saccharum officinarum|petroselinum crispum|dioscorea villosa|lactuca sativa")) { #plants
    return("Plants")
  } else if (str_detect(host, "milk|food|pork|milk powder")) {
    return("Processed Food")
  } else if (str_detect(host, "wastewater|surface water|soil|environmental|river|lake|stream|sediment|raw water|water sample|nil|environment|water|envirnomental|enviromental|environment|environnment")) {
    return("Environmental Sample")
  } else if (str_detect(host, "clinical|biopsy|blood|tissue|concentrate|sample|serum|swab|intestinal|abscess|liver abscess|maoctra veneriformis|pneumoniae")) {
    return("Clinical/Medical Sample")
  } else if (str_detect(host, "wild animal|animal|in vitro|laboratory|ucc strain|host-associated|natural / free-living|bacteria|vertebrata|nist mixed microbial rm strain|scrub1a|scrub2|padding|hospital environment|hospital surface|lab|hosptial surface")) {
    return("Under-defined")
  } else if (str_detect(host, "plagiopyla sp. strain oystr")) {
    return("Protists")
  } else if (str_detect(host, "ustilago maydis")) {
    return("Fungi")
  } else if (str_detect(host, "scrippsiella trochoidea| klebsiella pneumoniae")) {
    return("Prokaryotic organisms")
  } else {
    return("ASSIGN ME")
  }
}


# Apply classification
df <- df %>%
  mutate(host_category = sapply(host, categorize_host))

# View results
head(df)

# Filter entries that still need manual assignment
assign_me_entries <- df %>%
  filter(host_category == "ASSIGN ME") %>%
  distinct(host, .keep_all = TRUE)

#manual reduction of low entry categories after looking up on Biosample
df <- df %>%
  mutate(
    host_category = case_when(
      host_category == "Fungi" ~ "Under-defined",
      host_category == "Protists" ~ "Aquatic Organisms",
      host_category == "Prokaryotic organisms" ~ "Aquatic Organisms",
      host_category == "Clinical/Medical Sample" &
        NCBI_assemblyInfo.biosample.accession == "SAMN26810713" ~ "Aquatic Organisms",
      host_category == "Clinical/Medical Sample" ~ "Human",
      TRUE ~ host_category
    )
  )
