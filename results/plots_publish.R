pacman::p_load(tidyverse, RColorBrewer)

#import 
session_important <- 
  read_csv("./data/key_info.csv", 
            col_types = list(failed_attention = col_factor(c(0,1)))) %>%
  arrange(mode) %>%
  mutate(mode_paper = fct_inorder(mode_paper))

question_answer_info <- read_csv("./data/question_answers.csv")
# sample_disclosure <- read_csv("./data/sample_disclosure.csv")
# demographics <- read_csv("./data/demographics.csv")

summary_disclosure <- 
  session_important %>%
  group_by(mode, mode_paper) %>% 
  summarise(char_display = median(char_display)) %>% 
  ungroup() %>%
  mutate(disclosure_perc = char_display/max(char_display))


theme_pub <- function(size_txt_x = 15, size_txt_y = 14, size_txt_title = 14){
  theme_minimal() +
    theme(axis.text.x=element_text(size = size_txt_x),
          axis.text.y=element_text(size = size_txt_y),
          title=element_text(size = size_txt_title),
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(color = "darkgrey", size = 0.75))
}

draw_boxplot <- function(df, var_x, var_y, title, 
                         fill = "darkslategray3", lwd = 0.85, fatten = 1, outlier.size = 2, alpha = 0.9,
                         label_wrap = 17,
                         size_x = 20, size_y = 17, size_title = 18){
  # browser()
  
  var_x <- enquo(var_x)
  var_y <- enquo(var_y)
  
  df %>%
    ggplot() + 
    geom_boxplot(aes_string(x = quo_name(var_x), y = quo_name(var_y)),
                 lwd = lwd, fatten = fatten, outlier.size = outlier.size, fill = fill, alpha = alpha) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = label_wrap)) + 
    labs(title = title) +
    theme_pub(size_x, size_y, size_title) +
    guides(fill=FALSE)
}

draw_barplot <- function(df, var_x, var_y, title, 
                         fill = "dodgerblue3", alpha = 0.7,
                         label_wrap = 17,
                         size_x = 20, size_y = 17, size_title = 18){
  # browser()
  # var_x
  var_x <- enquo(var_x)
  var_y <- enquo(var_y)
  
  df %>% 
    ggplot(aes_string(quo_name(var_x), quo_name(var_y))) + 
    geom_bar(stat = "identity", fill = fill, alpha = alpha) +
    labs(title = title) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = label_wrap)) + 
    theme_pub(size_x, size_y, size_title)
}


#################################################################

draw_boxplot(session_important, var_x = mode_paper, var_y = scr30_percent/100, title = "Scores in each mode") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1), breaks = seq(0,1,0.2))

draw_boxplot(session_important, var_x = "mode_paper", var_y = "scr30_percent/100", title = "Scores in each mode") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1), breaks = seq(0,1,0.2))

ggsave("./plots/pb_score_boxplot.png", dpi = 500, width = 10, height = 8)



summary_disclosure %>%
  mutate(char_display  = char_display/max(char_display)) %>% 
  draw_barplot(var_x = mode_paper, var_y = char_display, title = "Percentage of characters disclosed") + 
  scale_y_continuous(breaks = seq(0,1,.25), labels = scales::percent) + 
  geom_text(aes(y = char_display + 0.06, label = paste0(ifelse(char_display %in% c(1,0),
                                                               char_display * 100,
                                                               format(round(char_display * 100, 0), nsmall = 0)),
                                                        "%")), color = "gray27", size = 4) 

ggsave("./plots/pb_disclosureValues_perc.png", dpi = 500, height = 4, width = 8)


question_answer_info %>%
  group_by(mode,id,grade) %>% 
  summarise_all(mean) %>%
  ungroup() %>%
  left_join((session_important %>%
               dplyr::select(mode,mode_paper) %>%
               unique), by = "mode") %>% 
  ggplot(aes(fct_rev(factor(mode_paper)),confidence_recoded)) +
  geom_boxplot( aes(fill = mode_paper), alpha = 0.8) +
  coord_flip() +
  facet_wrap(~ifelse(grade, "Confidence for Correct Decisions", 
                     "Confidence for Wrong Decisions"),ncol = 1) +
  theme_pub() +
  theme(strip.text = element_text(size = 15),
        panel.spacing = unit(2, "lines")) +
  guides(fill = FALSE) +
  scale_fill_manual(values = c(rgb(86/255,235/255,211/255), rgb(21/255,81/255,38/255), 
                               rgb(126/255,190/255,248/255), 
                               rgb(99/255,161/255,34/255), rgb(48/255, 109/255, 153/255)))

ggsave("./plots/pb_conf_grade.png", dpi = 500, width = 7, height = 9)


##########################################################################

#bar chart for disclosure
summary_disclosure %>%
  draw_barplot(mode_paper, char_display, title = "Percentage of characters disclosed") + 
  scale_y_continuous(breaks = seq(0, 2500, 500)) + 
  geom_text(aes(y = char_display + 45, label = char_display), color = "gray27", size = 4) 

ggsave("./plots/pb_disclosureValues.png", dpi = 500)


draw_boxplot(session_important, mode_paper, duration_main, title = "Duration in minutes for each mode") +
  scale_y_continuous(limits = c(0,43), breaks = seq(0,42,10))

ggsave("./plots/pb_duration_boxplot.png", dpi = 500, width = 10, height = 9)



