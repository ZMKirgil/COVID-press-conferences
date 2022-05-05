library(tidytext)
library(tidyverse)
library(readxl)
library(car)
library(RColorBrewer)
library(ggpubr)

## Figure 2 - LIWC -----------------------------------------------------

LIWC_data <- read_excel("Data/01_LIWC-data-output.xlsx") %>%
  mutate(count = WC/1000,
         pol = as.factor(pol))

# Corpus size comparison
a <- ggplot(LIWC_data, aes(y = count, x = reorder(state, -count))) + 
  geom_bar(stat = "identity", color = "black", fill="grey49") +
  ggtitle("Corpus Size by US States") +
  xlab("Text Corpus") +
  ylab("Word Count") +
  theme_classic() 

# Analytic
b <- ggplot(LIWC_data, aes(y = Analytic, x = reorder(state, -Analytic))) + 
  geom_bar(stat = "identity", color = "black", fill="grey38") +
  ggtitle("Analytical Language Use by US States") +
  xlab("Text Corpus") +
  ylab("LIWC Analytic") +
  theme_classic()+
  geom_hline(yintercept=57.18, alpha=.6) +
  geom_hline(yintercept=93.59, linetype="dashed", alpha=.6) 

# Authentic
c <- ggplot(LIWC_data, aes(y = Authentic, x = reorder(state, -Authentic))) + 
  geom_bar(stat = "identity", color = "black", fill="grey25") +
  ggtitle("Authentic Language Use by US States") +
  xlab("Text Corpus") +
  ylab("LIWC Authentic") +
  theme_classic() +
  geom_hline(yintercept=36.35, alpha=.6) +
  geom_hline(yintercept=15.91, linetype="dashed", alpha=.6) 

# Tone
d <- ggplot(LIWC_data, aes(y = Tone, x = reorder(state, -Tone))) + 
  geom_bar(stat = "identity", color = "black", fill="grey18") +
  ggtitle("Tone by US States") +
  xlab("Text Corpus") +
  ylab("LIWC Tone") +
  theme_classic()+
  geom_hline(yintercept=59.49, alpha=.6) +
  geom_hline(yintercept=41.28, linetype="dashed", alpha=.6) 

# Summary plot
ggarrange(a, b, c, d , 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)



## Figure 3 - Closest words to collective intentionality -----------------------

load("03_CI_corpus_data.rda")

CI <- CI_full %>%
  select(c(2:5)) %>%
  group_by(state) %>%
  top_n(10, p_together) %>%
  mutate(word2 = as.factor(word2), #only for cross party plot
         pol = as.factor(pol),
         p_together = ifelse(pol == "Republican", p_together + 10, p_together - 10))

ggplot(CI, aes(x = reorder(word2, -p_together), y = p_together))+
  geom_bar(stat = "identity", position = "identity")+
  facet_grid(~state + pol) +
  coord_flip() +
  xlab("Words") +
  ylab("Closeness to collective intentionality") +
  ggtitle("Closest Words to Collective Intentionality") +
  theme_classic() +
  theme(plot.title = element_text(size=15, face="bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()  +
  aes(fill = as.factor(state)) +
  theme(legend.position = "none") + 
  theme(axis.text=element_text(size=8))

## Figure 4 - Over representation plot ---------------------------------

# Plot based on code by Johannes B. Gruber, https://www.r-bloggers.com/2019/02/smarter-wordclouds/

figure4 <- bind_rows(CI_CA, CI_FL, CI_MD, CI_ME, CI_NJ, CI_NV, CI_NY, 
                      CI_OH, CI_TX) %>%
  select(word2, pol) %>% 
  group_by(word2, pol) %>%
  count(word2, sort = TRUE) %>%
  ungroup() %>%
  group_by(pol) %>%
  mutate(count_pol = sum(n)) %>%
  ungroup() %>%
  mutate(total = sum(n),
         rel_freq = round(n/count_pol, 4)) %>%
  select(word2, total, pol, rel_freq) %>%
  spread(pol, rel_freq) %>%
  mutate(Overrepresentation = log((Republican+1)/(Democrat+1))) 

plot <- figure4 %>%
  ggplot(aes(
    x = Overrepresentation, y = 0,
    label = word2, size= Overrepresentation, colour = Overrepresentation)
  ) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 10) +
  labs(title="Overrepresentation in republican administrations") +
  theme_minimal() +
  scale_color_gradient(low = "red", high = "darkred")

plot











