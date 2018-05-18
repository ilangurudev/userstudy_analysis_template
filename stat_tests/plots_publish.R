library(ggplot2)
library(tidyverse)
library(stringr)
library(forcats)
library(RColorBrewer)

session_important <- read_csv("./data/key_info.csv", 
                              col_types = list(failed_attention = col_factor(c(0,1)))) %>%
                          arrange(mode) %>%
                            mutate(mode_paper = fct_inorder(mode_paper))
question_answer_info <- read_csv("./data/question_answers.csv")
sample_disclosure <- read_csv("./data/sample_disclosure.csv")
demographics <- read_csv("./data/demographics.csv")

summary_disclosure <- 
  session_important %>%
  group_by(mode,mode_paper) %>% 
  summarise(char_display = median(char_display)) %>% 
  ungroup() %>%
  mutate(disclosure_perc = char_display/max(char_display))


theme_pub <- function(){
  theme_minimal() +
    theme(axis.text=element_text(size=14),
          title=element_text(size=14),
          plot.title = element_text(hjust = 0.5))
}



#################################################################

session_important %>%
  ggplot() + geom_boxplot(aes(x = mode_paper, y = scr30_percent/100),
                          lwd = 0.85, fatten = 1, outlier.size = 2, fill = "darkslategray3", alpha = 0.9) +
  labs(title = "Score Vs Mode",
       x = "Mode",
       y = "Score in percent") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 17)) + 
  labs(title = "Scores in each mode") +
  theme_minimal() +
  theme(axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=17),
        title=element_text(size=18),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "darkgrey", size = 0.75)) +
  guides(fill=FALSE) 

ggsave("./plots/pb_score_boxplot.png", dpi = 500, width = 10, height = 8)

# filter(duration_main < 25) %>% 
#bar chart for disclosure percent
summary_disclosure %>%
  mutate(char_display  = char_display/max(char_display)) %>% 
  ggplot(aes(mode_paper, char_display)) + 
  geom_bar(stat = "identity", fill = "dodgerblue3", alpha = 0.7) +
  scale_y_continuous(breaks = seq(0,1,.25), labels = scales::percent) +
  labs(title = "Percentage of characters disclosed") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 17)) + 
  theme_minimal() +
  geom_text(aes(y = char_display + 0.06, label = paste0(ifelse(char_display %in% c(1,0),
                                      char_display * 100,
                                      format(round(char_display * 100, 0), nsmall = 0)),
                                      "%")), color = "gray27", size = 4) +
  theme(axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=14),
        title=element_text(size=14),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "darkgrey", size = 0.75))

ggsave("./plots/pb_disclosureValues_perc.png", dpi = 500, height = 4, width = 8)



question_answer_info %>%
  group_by(mode,id,grade) %>% 
  summarise_all(mean) %>%
  ungroup() %>%
  left_join((session_important %>%
               select(mode,mode_paper) %>%
               unique), by = "mode") %>% 
  ggplot(aes(fct_rev(factor(mode_paper)),confidence_recoded)) +
  geom_boxplot( aes(fill = mode_paper), alpha = 0.8) +
  coord_flip() +
  facet_wrap(~ifelse(grade, "Confidence for Correct Decisions", 
                     "Confidence for Wrong Decisions"),ncol = 1) +
  theme_minimal() +
  theme(axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=14),
        title=element_text(size=14),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey"),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 15),
        panel.spacing = unit(2, "lines")) +
  guides(fill = FALSE) +
  scale_fill_manual(values = c(rgb(86/255,235/255,211/255), rgb(21/255,81/255,38/255), 
                               rgb(126/255,190/255,248/255), 
                               rgb(99/255,161/255,34/255), rgb(48/255, 109/255, 153/255)))

ggsave("./plots/pb_conf_grade.png", dpi = 500, width = 7, height = 9)











##########################################################################

#bar chart for disclosure
summary_disclosure %>%
  ggplot(aes(mode_paper, char_display)) + 
  geom_bar(stat = "identity", fill = "deepskyblue4", alpha = 0.7) +
  scale_y_continuous(breaks = seq(0,2500,1000)) +
  geom_text(aes(label = char_display, y = char_display + 100)) +
  labs(title = "Median number of characters disclosed") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + 
  theme_minimal() +
  theme(axis.text=element_text(size=14),
        title=element_text(size=14),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave("./plots/pb_disclosureValues.png", dpi = 500)



session_important %>%
  ggplot() + geom_boxplot(aes(x = mode_paper, y = duration_main, fill = mode_paper),
                          lwd = 0.75, fatten = 1, outlier.size = 2, 
                          fill = "deepskyblue4", alpha = 0.7) +
  labs(title = "Score Vs Mode",
       x = "Mode",
       y = "Score in percent") +
  scale_y_continuous(limits = c(0,43), breaks = seq(0,42,10)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 17)) + 
  labs(title = "Duration in minutes for each mode") +
  theme_minimal() +
  theme(axis.text.x=element_text(size=17),
        axis.text.y=element_text(size=14),
        title=element_text(size=14),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(fill=FALSE) 

ggsave("./plots/pb_duration_boxplot.png", dpi = 500, width = 10, height = 9)



