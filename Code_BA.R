# loading packages
library(tidyverse)
library(lubridate)
library(quanteda)
library(text2vec)
library(conText)
library(qdapRegex)
library(udpipe)
library(SnowballC)
library(factoextra)
library(ggfortify)
library(ggthemes)
library(ggrepel)
library(flextable)
library(readxl)
library(writexl)

# # loading data
#  NATODebate <- read_csv("Daten/CombinedData.csv")
#
# # pre-processing
# NATODebate$Text <- NATODebate$Text %>%
#     rm_url() %>%
#     rm_bracket() %>%
#     str_replace_all(pattern = "[:punct:]", replacement = "") %>%
#     str_replace_all(pattern = "[:digit:]", replacement = "") %>%
#     str_to_lower() %>%
#     rm_nchar_words("1") %>%
#     str_replace_all(pattern = "finlands", replacement = "finland") %>%
#     str_replace_all(pattern = "swedens", replacement = "sweden") %>%
#     str_replace_all(pattern = "russias", replacement = "russia") %>%
#     str_replace_all(pattern = "irelands", replacement = "ireland") %>%
#     str_replace_all(pattern = "irish", replacement = "ireland") %>%
#     str_replace_all(pattern = "finnish", replacement = "finland") %>%
#     str_replace_all(pattern = "swedish", replacement = "sweden") %>%
#     str_replace_all(pattern = "natos", replacement = "nato") %>%
#     str_replace_all(pattern = "ukraines", replacement = "ukraine") %>%
#     str_replace_all(pattern = "join", replacement = "joining") %>%
#     str_squish()
#
# write_csv(NATODebate, file = "Daten/NATODebateProcessed.csv")

# check if enough words for local embedding
# NATOWords <- NATODebate %>%
#     mutate(Wordnumber = str_count(Text, pattern = " "))
#
# sum(NATOWords$Wordnumber, 2760) # over 1,000,000 is enough

# saveRDS(NATODebate, file = "NATODebate.RDS")

# NATODebate <- read_rds("NATODebate.RDS")

# ParlSpeeches <- NATODebate %>%
#      filter(ID >= 1953)
#
# saveRDS(ParlSpeeches, file = "ParlSpeeches.RDS")

ParlSpeeches <- read_rds("ParlSpeeches.RDS")

# Country-specific repetition only for NNS-Ratio-Plot

# IRLSpeeches <- ParlSpeeches %>%
#     filter(Country == "Ireland")
#
# SWESpeeches <- ParlSpeeches %>%
#     filter(Country == "Sweden")
#
# FINSpeeches <- ParlSpeeches %>%
#     filter(Country == "Finland")

SpeechesPerParty <- ParlSpeeches %>%
  group_by(Affiliation, Invasion) %>%
  tally()
#
# ParlSpeeches <- ParlSpeeches %>%
#     filter(Affiliation != "AON_IRL") # No entry of AON-Party before invasion

# create corpus-object
# NATOCorpus <- corpus(NATODebate,
#                      docid_field = "ID",
#                      text_field = "Text")

ParlCorpus <- corpus(ParlSpeeches,
  docid_field = "ID",
  text_field = "Text"
)

# Country-specific repetition only for NNS-Ratio-Plot

# IRLCorpus <- corpus(IRLSpeeches,
#                     docid_field = "ID",
#                     text_field = "Text")
# SWECorpus <- corpus(SWESpeeches,
#                     docid_field = "ID",
#                     text_field = "Text")
# FINCorpus <- corpus(FINSpeeches,
#                     docid_field = "ID",
#                     text_field = "Text")


# create initial tokens-object
# InitTokens <- tokens(NATOCorpus) %>%
#     tokens_remove(stopwords(language = "en")) %>%
#     tokens_remove(c("also", "however", "even", "us", "say", "said", "one"))

InitTokens <- tokens(ParlCorpus) %>%
  tokens_remove(stopwords(language = "en")) %>%
  tokens_remove(c(
    "also", "however", "even", "us", "say",
    "said", "one", "think", "mr", "like"
  ))

# Country-specific repetition only for NNS-Ratio-Plot

# InitTokens <- tokens(IRLCorpus) %>%
#     tokens_remove(stopwords(language = "en")) %>%
#     tokens_remove(c("also", "however", "even", "us", "say",
#                     "said", "one", "think", "mr", "like"))
# InitTokens <- tokens(SWECorpus) %>%
#     tokens_remove(stopwords(language = "en")) %>%
#     tokens_remove(c("also", "however", "even", "us", "say",
#                     "said", "one", "think", "mr", "like"))
# InitTokens <- tokens(FINCorpus) %>%
#     tokens_remove(stopwords(language = "en")) %>%
#     tokens_remove(c("also", "however", "even", "us", "say",
#                     "said", "one", "think", "mr", "like"))

# create Document Feature Matrix, reduce, and extract features
# Features <- dfm(InitTokens) %>%
#     dfm_trim(min_termfreq = 20, min_docfreq = 10) %>%
#     featnames()

Features <- dfm(InitTokens) %>%
  dfm_trim(min_termfreq = 10, min_docfreq = 5) %>%
  featnames()

# lower threshold for country-specific operations
# Features <- dfm(InitTokens) %>%
#     dfm_trim(min_termfreq = 5, min_docfreq = 3) %>%
#     featnames()

# delete non-overlapping vocabulary
InitTokens <- tokens_select(InitTokens, Features, padding = T)


########################################

# Create local GloVe embeddings and transformation matrix

# construct Feature Co-occurence Matrix for tokens object
TokensFCM <- fcm(InitTokens,
  context = "window",
  window = 6L,
  count = "frequency",
  tri = FALSE
) # if TRUE only returns upper triangle


# estimate local GloVe model
# GloVe <- GlobalVectors$new(rank = 300,
#                            x_max = 10,
#                            learning_rate = 0.05)
#
# wv_main <- GloVe$fit_transform(TokensFCM,
#                                n_iter = 20L,
#                                convergence_tol = 1e-3,
#                                n_threads = 8L)
#
# wv_context <- GloVe$components
# local_glove <- wv_main + t(wv_context)
# dim(local_glove)

# saveRDS(local_glove, file = "local_glove.RDS")

local_glove <- read_rds("local_glove.RDS")

# assess quality
# find_nns(local_glove["neutrality",],
#          pre_trained = local_glove,
#          N = 15,
#          candidates = Features)
# find_nns(local_glove["nonalignment",],
#          pre_trained = local_glove,
#          N = 15,
#          candidates = Features)
# find_nns(local_glove["nato",],
#          pre_trained = local_glove,
#          N = 15,
#          candidates = Features)

# estimate transformation matrix (needs corpus + embeddings)
# weighting depends on corpus size
# -> small corpus = "log", big corpus = numeric value e.g., 500
# see Khodak 2018 for more information
# local_transform <- compute_transform(TokensFCM,
#                                      pre_trained = local_glove,
#                                      weighting = "log")
#
# saveRDS(local_transform, file = "local_transform.RDS")

local_transform <- readRDS("local_transform.RDS")

#######################################

# create token-object of context
ParlTokens <- tokens_context(InitTokens,
  pattern = c("nato", "neutrality", "nonalignment"),
  window = 6L
)
# 3939 instances of "nato" found.
# 491 instances of "neutrality" found.
# 128 instances of "nonalignment" found.

# check
head(docvars(NATOTokens))

# create Document Feature Matrix of context
ParlDFM <- dfm(ParlTokens)
# check
ParlDFM[1:4, 1:8]

######################################

# build a Document Embedding Matrix
#   from 1) context document feature matrix
#        2) local embeddings
#        3) local transformation matrix


ParlDEM <- dem(ParlDFM,
  pre_trained = local_glove,
  transform = T,
  transform_matrix = local_transform
)
# check
head(ParlDEM@docvars) # @ not $-quanteda functions don't work on DEM
tail(ParlDEM@features)

#########################

# group specific embeddings - average within party
# PartyEmbed <- dem_group(ParlDEM,
#                         groups = interaction(ParlDEM@docvars$Affiliation,
#                                              ParlDEM@docvars$Invasion)
# )
# dim(PartyEmbed)

############################

# nearest neighbors to ALC group embeddings
# set.seed(123) # to regenerate same result when bootstrapping
# CountryNNS <- get_nns(ParlTokens,
#                            N = 5,
#                            groups = interaction(ParlDEM@docvars$Affiliation,
#                                                 ParlDEM@docvars$Invasion),
#                            candidates = ParlTokens,
#                            pre_trained = local_glove,
#                            transform = T,
#                            transform_matrix = local_transform,
#                            bootstrap = T,
#                            num_bootstraps = 100,
#                            as_list = F,
# )
#
# saveRDS(CountryNNS, file = "NNS_Party_Invasion.rds")

# NNS_Party_Invasion <- read_rds("NNS_Party_Invasion.rds")
#
# NNS_Party_Invasion <- NNS_Party_Invasion %>%
#     group_by(target) %>%
#     mutate(mean_cos = round(mean(value),digits = 3),
#            mean_std.error = round(mean(std.error), digits = 3)) %>%
#         ungroup() %>%
#     select(-value,-std.error) %>%
#     pivot_wider(names_from = rank, values_from = feature) %>%
#     separate(col = target, into = c("Party", "Rest"),
#              sep = "_")
#
#
# write_xlsx(NNS_Party_Invasion, path = "NNS_Party_Invasion.xlsx")

NNS_Party_Invasion <- read_excel("NNS_Party_Invasion.xlsx")

NNS_Party_Invasion <- NNS_Party_Invasion %>%
  unite(col = "Features", c(4:8), sep = ", ", remove = T) %>%
  rename(
    SE = mean_std.error,
    `Kosinus-Ähnlichkeit` = mean_cos,
    Partei = Party,
    Land = Country
  ) %>%
  mutate(Invasion = fct_recode(as_factor(Invasion),
    Ja = "Yes",
    Nein = "No"
  ))



flextable(NNS_Party_Invasion) %>%
  theme_vanilla() %>%
  save_as_docx(path = "NNS_Party_Invasion_Table.docx")

# cosine similarity between group embeddings and specific features
# set.seed(234)
# CountryCOS <- get_cos_sim(ParlTokens,
#             groups = interaction(ParlDEM@docvars$Affiliation,
#                                  ParlDEM@docvars$Invasion),
#             features = c("neutrality", "nonalignment", "nato"),
#             pre_trained = local_glove,
#             transform = T,
#             transform_matrix = local_transform,
#             bootstrap = T,
#             num_bootstraps = 100,
#             as_list = F,
# )
#
# saveRDS(CountryCOS, file = "COS_Party_Invasion.rds")

# COS_Party_Invasion <- read_rds("COS_Party_Invasion.rds")
#
# COS_Party_Invasion <- COS_Party_Invasion %>%
#     mutate(value = round(value, digits = 3),
#            std.error = round(std.error, digits = 3)) %>%
#     unite(col = "Values", c(3:4), sep = "   SE: ") %>%
#     separate(col = target, into = c("Party","Country"),
#              sep ="_") %>%
#     pivot_wider(names_from = feature, values_from = Values)
#
# write_xlsx(COS_Party_Invasion, path = "COS_Party_Invasion.xlsx")

COS_Party_Invasion <- read_excel("COS_Party_Invasion.xlsx")

COS_Party_Invasion <- COS_Party_Invasion %>%
  arrange(Party, Country) %>%
  rename(
    Partei = Party,
    Land = Country
  ) %>%
  mutate(Invasion = fct_recode(as_factor(Invasion),
    Ja = "Yes",
    Nein = "No"
  )) %>%
  separate(col = NATO, into = c("NATO", "1"), sep = " ") %>%
  separate(
    col = Neutrality, into = c("Neutrality", "2"),
    sep = " "
  ) %>%
  separate(
    col = Nonalignment, into = c("Nonalignment", "3"),
    sep = " "
  ) %>%
  select(-"1", -"2", -"3") %>%
  mutate(
    NATO = as.numeric(NATO),
    Neutrality = as.numeric(Neutrality),
    Nonalignment = as.numeric(Nonalignment)
  )



flextable(COS_Party_Invasion) %>%
  theme_vanilla() %>%
  autofit() %>%
  color(j = 4:6, color = "white") %>%
  bg(j = 4:6, bg = scales::col_numeric(
    palette = "viridis",
    domain = c(0, 0.8)
  )) %>%
  save_as_docx(path = "COS_Party_Invasion_Table.docx")


# cosine similarity ratio - binary groups argument is mandatory!
# inferences using a permutation test around the absolute deviation
# of the observed cosine similarity ratio from 1
# -> for each permutation the grouping variable is randomly shuffled
#    and absolute deviation of cosine similarity ratios from 1 is
#    computed - p.value is proportion of "permuted" deviations that
#    are larger than the observed deviation
# "shared" in output means that feature appeared in both groups'
# top (N) nearest neighbors

# set.seed(345)
# NNSRatio <- get_nns_ratio(ParlTokens,
#                                  N = 10,
#                                  groups = docvars(ParlTokens, "Invasion"),
#                                  numerator = "Yes",
#                                  candidates = ParlTokens,
#                                  pre_trained = local_glove,
#                                  transform = T,
#                                  transform_matrix = local_transform,
#                                  bootstrap = T,
#                                  permute = T,
#                                  num_bootstraps = 100,
#                                  num_permutations = 100)

# computed multiple times for country-specific words

# IRLNNSRatio <- NNSRatio
# saveRDS(IRLNNSRatio, file = "IRLNNSRatio.rds")
# SWENNSRatio <- NNSRatio
# saveRDS(SWENNSRatio, file = "SWENNSRatio.rds")
# FINNNSRatio <- NNSRatio
# saveRDS(FINNNSRatio, file = "FINNNSRatio.rds")

IRLNNSRatio <- read_rds("IRLNNSRatio.rds")
SWENNSRatio <- read_rds("SWENNSRatio.rds")
FINNNSRatio <- read_rds("FINNNSRatio.rds")

IRLNNSRatio <- IRLNNSRatio %>%
  mutate(Country = "Irland")
SWENNSRatio <- SWENNSRatio %>%
  mutate(Country = "Schweden")
FINNNSRatio <- FINNNSRatio %>%
  mutate(Country = "Finnland")

NNSRatio <- bind_rows(IRLNNSRatio, SWENNSRatio, FINNNSRatio)

NNSRatio <- NNSRatio %>%
  mutate(Rank = value - 1) %>%
  mutate(Abs = abs(Rank)) %>%
  mutate(Distance = rank(desc(Abs))) %>%
  rename(
    Invasion = group,
    Land = Country
  ) %>%
  mutate(Invasion = fct_recode(as_factor(Invasion),
    Ja = "Yes",
    Nein = "No",
    Beides = "shared"
  )) %>%
  mutate(feature = if_else(p.value < 0.01, paste0(feature, "*"), feature))


# plot of nns_ratio
# -> y-axis simple ranks the features according to distance

ggplot(NNSRatio) +
  aes(x = value, y = Distance, colour = Land) +
  geom_point(aes(shape = Invasion), size = 2.5) +
  geom_text_repel(aes(label = feature),
    size = 4,
    max.overlaps = 15,
    key_glyph = "rect"
  ) +
  ylab(label = "Rang geordnet nach größter absoluter Differenz") +
  xlab(label = "Kosinus-Ähnlichkeit im Verhältnis (Ja/Nein)") +
  scale_color_manual(
    values = c("#012f6b", "#169b62", "#dbb200"),
    name = "Land"
  ) +
  theme_gdocs(base_size = 14) +
  theme(
    legend.text = element_text(size = 14),
    axis.ticks.x = element_blank()
  ) +
  geom_vline(xintercept = 1, linetype = "dashed")

# nearest contexts
# set.seed(456)
# NCS <- get_ncs(ParlTokens,
#                            N = 3,
#                            groups = interaction(ParlDEM@docvars$Affiliation,
#                                     ParlDEM@docvars$Invasion),
#                            pre_trained = local_glove,
#                            transform = T,
#                            transform_matrix = local_transform,
#                            bootstrap = T,
#                            num_bootstraps = 100,
#                            as_list = F
# )
#
# NCS
#
# saveRDS(NCS, file = "NCS_Party_Invasion.rds")

# NCS_Party_Invasion <- read_rds("NCS_Party_Invasion.rds")
#
# NCS_Party_Invasion <- NCS_Party_Invasion %>%
#     group_by(target) %>%
#     mutate(Cosine = round(mean(value), digits = 3),
#            SE = round(mean(std.error), digits = 3)) %>%
#     select(-rank,-value,-std.error) %>%
#     separate(col = target, into = c("Party", "Country"),
#              sep = "_", remove = T)
#
# write_xlsx(NCS_Party_Invasion, path = "NCS_Party_Invasion.xlsx")

NCS_Party_Invasion <- read_excel("NCS_Party_Invasion.xlsx")

NCS_Party_Invasion <- NCS_Party_Invasion %>%
  pivot_wider(names_from = Rank, values_from = context) %>%
  unite(col = "Context", c(6:8), sep = "; ") %>%
  rename(
    Land = Country,
    Partei = Party,
    `Kosinus-Ähnlichkeit` = Cosine,
    Kontext = Context
  ) %>%
  mutate(Invasion = fct_recode(as_factor(Invasion),
    Ja = "Yes",
    Nein = "No"
  ))

flextable(NCS_Party_Invasion) %>%
  theme_vanilla() %>%
  autofit() %>%
  save_as_docx(path = "NCS_Party_Invasion_Table.docx")

######################

# Scaling Party Position Before/After Invasion with PCA

# pre-processing
# PartyEmbedDF <- as.matrix(PartyEmbed@x)
#
# C_SWE_NO <- PartyEmbedDF[seq(1,15600,by = 52)]
# FF_IRL_NO <- PartyEmbedDF[seq(2, 15600, by = 52)]
# FG_IRL_NO <-PartyEmbedDF[seq(3, 15600, by = 52)]
# GP_IRL_NO <-PartyEmbedDF[seq(4, 15600, by = 52)]
# Independent_IRL_NO <-PartyEmbedDF[seq(5, 15600, by = 52)]
# KD_FIN_NO <-PartyEmbedDF[seq(6, 15600, by = 52)]
# KD_SWE_NO <-PartyEmbedDF[seq(7, 15600, by = 52)]
# KESK_FIN_NO <-PartyEmbedDF[seq(8, 15600, by = 52)]
# KOK_FIN_NO <-PartyEmbedDF[seq(9, 15600, by = 52)]
# L_SWE_NO <-PartyEmbedDF[seq(10, 15600, by = 52)]
# LAB_IRL_NO <-PartyEmbedDF[seq(11, 15600, by = 52)]
# LIIK_FIN_NO <-PartyEmbedDF[seq(12, 15600, by = 52)]
# M_SWE_NO <-PartyEmbedDF[seq(13, 15600, by = 52)]
# MP_SWE_NO <-PartyEmbedDF[seq(14, 15600, by = 52)]
# PS_FIN_NO <-PartyEmbedDF[seq(15, 15600, by = 52)]
# RKP_FIN_NO <-PartyEmbedDF[seq(16, 15600, by = 52)]
# S_PBP_IRL_NO <-PartyEmbedDF[seq(17, 15600, by = 52)]
# S_SWE_NO <-PartyEmbedDF[seq(18, 15600, by = 52)]
# SD_IRL_NO <-PartyEmbedDF[seq(19, 15600, by = 52)]
# SD_SWE_NO <-PartyEmbedDF[seq(20, 15600, by = 52)]
# SDP_FIN_NO <-PartyEmbedDF[seq(21, 15600, by = 52)]
# SF_IRL_NO <-PartyEmbedDF[seq(22, 15600, by = 52)]
# V_SWE_NO <-PartyEmbedDF[seq(23, 15600, by = 52)]
# VAS_FIN_NO <-PartyEmbedDF[seq(24, 15600, by = 52)]
# VIHR_FIN_NO <-PartyEmbedDF[seq(25, 15600, by = 52)]
# VKK_FIN_NO <-PartyEmbedDF[seq(26, 15600, by = 52)]
# C_SWE_YES <-PartyEmbedDF[seq(27, 15600, by = 52)]
# FF_IRL_YES <- PartyEmbedDF[seq(28, 15600, by = 52)]
# FG_IRL_YES <- PartyEmbedDF[seq(29, 15600, by = 52)]
# GP_IRL_YES <- PartyEmbedDF[seq(30, 15600, by = 52)]
# Independent_IRL_YES <- PartyEmbedDF[seq(31, 15600, by = 52)]
# KD_FIN_YES <- PartyEmbedDF[seq(32, 15600, by = 52)]
# KD_SWE_YES <- PartyEmbedDF[seq(33, 15600, by = 52)]
# KESK_FIN_YES <- PartyEmbedDF[seq(34, 15600, by = 52)]
# KOK_FIN_YES <- PartyEmbedDF[seq(35, 15600, by = 52)]
# L_SWE_YES <- PartyEmbedDF[seq(36, 15600, by = 52)]
# LAB_IRL_YES <- PartyEmbedDF[seq(37, 15600, by = 52)]
# LIIK_FIN_YES <- PartyEmbedDF[seq(38, 15600, by = 52)]
# M_SWE_YES <- PartyEmbedDF[seq(39, 15600, by = 52)]
# MP_SWE_YES <- PartyEmbedDF[seq(40, 15600, by = 52)]
# PS_FIN_YES <- PartyEmbedDF[seq(41, 15600, by = 52)]
# RKP_FIN_YES <- PartyEmbedDF[seq(42, 15600, by = 52)]
# S_PBP_IRL_YES <- PartyEmbedDF[seq(43, 15600, by = 52)]
# S_SWE_YES <- PartyEmbedDF[seq(44, 15600, by = 52)]
# SD_IRL_YES <- PartyEmbedDF[seq(45, 15600, by = 52)]
# SD_SWE_YES <- PartyEmbedDF[seq(46, 15600, by = 52)]
# SDP_FIN_YES <- PartyEmbedDF[seq(47, 15600, by = 52)]
# SF_IRL_YES <- PartyEmbedDF[seq(48, 15600, by = 52)]
# V_SWE_YES <- PartyEmbedDF[seq(49, 15600, by = 52)]
# VAS_FIN_YES <- PartyEmbedDF[seq(50, 15600, by = 52)]
# VIHR_FIN_YES <- PartyEmbedDF[seq(51, 15600, by = 52)]
# VKK_FIN_YES <- PartyEmbedDF[seq(52, 15600, by = 52)]
#
#
# names(C_SWE_NO) <- c(1:300)
# names(C_SWE_YES) <- c(1:300)
# names(FF_IRL_NO) <- c(1:300)
# names(FF_IRL_YES)<- c(1:300)
# names(FG_IRL_NO) <- c(1:300)
# names(FG_IRL_YES) <- c(1:300)
# names(GP_IRL_NO) <- c(1:300)
# names(GP_IRL_YES) <- c(1:300)
# names(Independent_IRL_NO) <- c(1:300)
# names(Independent_IRL_YES) <- c(1:300)
# names(KD_FIN_NO) <- c(1:300)
# names(KD_FIN_YES) <- c(1:300)
# names(KD_SWE_NO) <- c(1:300)
# names(KD_SWE_YES) <- c(1:300)
# names(KESK_FIN_NO) <- c(1:300)
# names(KESK_FIN_YES) <- c(1:300)
# names(KOK_FIN_NO) <- c(1:300)
# names(KOK_FIN_YES) <- c(1:300)
# names(L_SWE_NO) <- c(1:300)
# names(L_SWE_YES) <- c(1:300)
# names(LAB_IRL_NO) <- c(1:300)
# names(LAB_IRL_YES) <- c(1:300)
# names(LIIK_FIN_NO) <- c(1:300)
# names(LIIK_FIN_YES) <- c(1:300)
# names(M_SWE_NO) <- c(1:300)
# names(M_SWE_YES) <- c(1:300)
# names(MP_SWE_NO) <- c(1:300)
# names(MP_SWE_YES) <- c(1:300)
# names(PS_FIN_NO) <- c(1:300)
# names(PS_FIN_YES) <- c(1:300)
# names(RKP_FIN_NO) <- c(1:300)
# names(RKP_FIN_YES) <- c(1:300)
# names(S_PBP_IRL_NO) <- c(1:300)
# names(S_PBP_IRL_YES) <- c(1:300)
# names(S_SWE_NO) <- c(1:300)
# names(S_SWE_YES) <- c(1:300)
# names(SD_IRL_NO) <- c(1:300)
# names(SD_IRL_YES) <- c(1:300)
# names(SD_SWE_NO) <- c(1:300)
# names(SD_SWE_YES) <- c(1:300)
# names(SDP_FIN_NO) <- c(1:300)
# names(SDP_FIN_YES) <- c(1:300)
# names(SF_IRL_NO) <- c(1:300)
# names(SF_IRL_YES) <- c(1:300)
# names(V_SWE_NO) <- c(1:300)
# names(V_SWE_YES) <- c(1:300)
# names(VAS_FIN_NO) <- c(1:300)
# names(VAS_FIN_YES) <- c(1:300)
# names(VIHR_FIN_NO) <- c(1:300)
# names(VIHR_FIN_YES) <- c(1:300)
# names(VKK_FIN_NO) <- c(1:300)
# names(VKK_FIN_YES) <- c(1:300)
#
#
# #check
# C_SWE_NO
#
# PartyEmbedPCADF <- bind_rows(C_SWE_NO,C_SWE_YES,FF_IRL_NO,FF_IRL_YES,
#                            FG_IRL_NO, FG_IRL_YES, GP_IRL_NO,GP_IRL_YES,
#                            Independent_IRL_NO, Independent_IRL_YES,
#                            KD_FIN_NO, KD_FIN_YES, KD_SWE_NO, KD_SWE_YES,
#                            KESK_FIN_NO, KESK_FIN_YES, KOK_FIN_NO, KOK_FIN_YES,
#                            L_SWE_NO, L_SWE_YES, LAB_IRL_NO, LAB_IRL_YES,
#                            LIIK_FIN_NO, LIIK_FIN_YES, M_SWE_NO, M_SWE_YES,
#                            MP_SWE_NO, MP_SWE_YES, PS_FIN_NO, PS_FIN_YES,
#                            RKP_FIN_NO, RKP_FIN_YES, S_PBP_IRL_NO, S_PBP_IRL_YES,
#                            S_SWE_NO, S_SWE_YES, SD_IRL_NO, SD_IRL_YES,
#                            SD_SWE_NO, SD_SWE_YES,
#                            SDP_FIN_NO, SDP_FIN_YES, SF_IRL_NO, SF_IRL_YES,
#                            V_SWE_NO, V_SWE_YES, VAS_FIN_NO, VAS_FIN_YES,
#                            VIHR_FIN_NO, VIHR_FIN_YES, VKK_FIN_NO, VKK_FIN_YES)
#
#
# PartyEmbedPCADF <- PartyEmbedPCADF %>%
#     mutate(Invasion = as_factor(c("No", "Yes", "No", "Yes", "No", "Yes",
#                                   "No", "Yes", "No", "Yes", "No", "Yes",
#                                   "No", "Yes", "No", "Yes", "No", "Yes",
#                                   "No", "Yes", "No", "Yes", "No", "Yes",
#                                   "No", "Yes", "No", "Yes", "No", "Yes",
#                                   "No", "Yes", "No", "Yes", "No", "Yes",
#                                   "No", "Yes", "No", "Yes", "No", "Yes",
#                                   "No", "Yes", "No", "Yes", "No", "Yes",
#                                   "No", "Yes", "No", "Yes"))) %>%
#     mutate(Country = as_factor(c("Sweden", "Sweden", "Ireland", "Ireland",
#                                  "Ireland", "Ireland", "Ireland", "Ireland",
#                                  "Ireland", "Ireland", "Finland", "Finland",
#                                  "Sweden", "Sweden", "Finland", "Finland",
#                                  "Finland", "Finland", "Sweden", "Sweden",
#                                  "Ireland", "Ireland", "Finland", "Finland",
#                                  "Sweden", "Sweden", "Sweden", "Sweden",
#                                  "Finland", "Finland", "Finland", "Finland",
#                                  "Ireland", "Ireland", "Sweden", "Sweden",
#                                  "Ireland", "Ireland", "Sweden", "Sweden",
#                                  "Finland", "Finland", "Ireland", "Ireland",
#                                  "Sweden", "Sweden", "Finland", "Finland",
#                                  "Finland", "Finland", "Finland", "Finland"))) %>%
#     mutate(Party = as_factor(c("C", "C", "FF", "FF",
#                                "FG", "FG", "GP", "GP",
#                                "Independent", "Independent",
#                                "KD", "KD", "KD", "KD",
#                                "KESK", "KESK", "KOK", "KOK",
#                                "L", "L", "LAB", "LAB",
#                                "LIIK", "LIIK", "M", "M",
#                                "MP", "MP", "PS", "PS",
#                                "RKP", "RKP", "S_PBP", "S_PBP",
#                                "S", "S", "SD", "SD",
#                                "SD", "SD", "SDP", "SDP",
#                                "SF", "SF", "V", "V",
#                                "VAS", "VAS", "VIHR", "VIHR",
#                                "VKK", "VKK")))
#
# saveRDS(PartyEmbedPCADF, file = "Party_Embed_PCA_DF.rds")



Party_Embed_PCA_DF <- read_rds("Party_Embed_PCA_DF.rds")


# filter at least 3 speeches before and after invasion
Party_Embed_PCA_DF <- Party_Embed_PCA_DF[-c(37:38), ]


Party_Embed_PCA_DF <- Party_Embed_PCA_DF %>%
  filter(Party != "GP") %>%
  filter(Party != "Independent") %>%
  filter(Party != "LAB") %>%
  filter(Party != "LIIK") %>%
  mutate(
    Invasion = fct_recode(as_factor(Invasion),
      Ja = "Yes",
      Nein = "No"
    ),
    Party = fct_recode(as_factor(Party),
      `S-PBP` = "S_PBP"
    )
  )

# create PCA Matrix
PartyMat <- as.matrix(Party_Embed_PCA_DF[, 1:300])


# PCA
results_pca <- prcomp(PartyMat, scale. = T, rank. = 5)

# scree plot
fviz_eig(results_pca)

# graph
autoplot(results_pca,
  data = Party_Embed_PCA_DF,
  shape = "Invasion", size = 2.9
) +
  geom_text_repel(
    label = Party_Embed_PCA_DF$Party,
    aes(color = Party_Embed_PCA_DF$Country),
    size = 4.5,
    max.overlaps = 15,
    key_glyph = "rect"
  ) +
  scale_color_manual(
    values = c("#dbb200", "#169b62", "#012f6b"),
    labels = c("Schweden", "Irland", "Finnland"),
    name = "Country"
  ) +
  theme_gdocs(base_size = 14) +
  theme(legend.text = element_text(size = 14))


# eigenvalues
eig_val <- get_eigenvalue(results_pca)
eig_val

# Results for individuals
results_ind <- get_pca_ind(results_pca)
results_ind$coord # Coordinates
results_ind$contrib # Contributions to the PCs
results_ind$cos2 # Quality of representation


# create speech sample for quotes

# set.seed(888)
# NoInvasionSample <- ParlSpeeches %>%
#     filter(Invasion == "No") %>%
#     filter(
#            Affiliation == "VAS_FIN" |
#                Affiliation == "PS_FIN" |
#                Affiliation == "SF_IRL"
#                ) %>%
#     group_by(Affiliation) %>%
#     slice_sample(n = 5)
#
#
# set.seed(999)
# InvasionSample <- ParlSpeeches %>%
#     filter(Invasion == "Yes") %>%
#     filter(
#                Affiliation == "VAS_FIN" |
#                Affiliation == "PS_FIN" |
#                Affiliation == "SF_IRL"
#     ) %>%
#     group_by(Affiliation) %>%
#     slice_sample(n = 5)
#
# SpeechSample <- bind_rows(NoInvasionSample, InvasionSample)
#
# write_xlsx(SpeechSample, path = "SpeechSample.xlsx")

# # merge with unedited texts
# SpeechSample <- read_excel("SpeechSample.xlsx")
# OrigSpeeches <- read_excel("Daten/Debates/Debates.xlsx")
#
# SpeechSampleText <- left_join(SpeechSample, OrigSpeeches, by = "ID")
#
# SpeechSampleText <- SpeechSampleText %>%
#     select(ID, Date.x, Country.x, Party,
#            Actor.x, Invasion, Text.y) %>%
#     rename(Date = Date.x,
#            Country = Country.x,
#            Actor = Actor.x,
#            Text = Text.y)
#
# write_xlsx(SpeechSampleText, path = "SpeechSampleText.xlsx")



# Public Opinion
PublicOpinion <- read_excel("PublicOpinion.xlsx")

PublicOpinion <- PublicOpinion %>%
  mutate(Datum = ymd(Date)) %>%
  rename(
    Ja = Yes,
    Nein = No,
    Unentschlossen = Indeterminate
  ) %>%
  pivot_longer(
    cols = c(3:5), names_to = "Meinung",
    values_to = "Prozent"
  ) %>%
  mutate(
    Country = fct_recode(as_factor(Country),
      Finnland = "Finland",
      Schweden = "Sweden",
      Irland = "Ireland"
    ),
    Meinung = as_factor(Meinung)
  )

# separate for plotting
PublicOpinionNoIreland <- PublicOpinion %>%
  filter(Country != "Irland")


# plotting Sweden + Finland
ggplot(PublicOpinionNoIreland, aes(
  x = Datum,
  y = Prozent,
  group = Meinung
)) +
  geom_line(aes(color = Meinung), size = 1.2) +
  facet_wrap(vars(Country), ncol = 1, scales = "free_y") +
  theme_gdocs(base_size = 14)
