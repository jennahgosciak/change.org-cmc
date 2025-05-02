library(tidyverse)
library(haven)
library(broom)
library(broom.helpers)    # Add empty reference categories to tidy model data frames
library(ggforce) 
library(survey)
library(marginaleffects)
library(scales)  

disagree_vals <- c("Somewhat Disagree", "Strongly disagree", "Neither agree nor disagree")
agree_vals <- c("Somewhat agree", "Strongly agree")
petitionA <- c("Q12", "Q39", "Q53")
petitionB <- c("Q15", "Q41", "Q59")
petitionC <- c("Q25", "Q45", "Q55")
petitionD <- c("Q24", "Q47", "Q61")

question1_vars <- c("var_P1a", "var_P2a", "var_P3a", "var_P4a")
question2_vars <- c("var_P1b", "var_P2b", "var_P3b", "var_P4b")

titles <- c("A" = "Stop Cyanide Fishing: Save the Humphead Wrasse",
            "B" = "Enforce Cyanide Fishing Bans and Protect the Humphead Wrasse",
            "C" = "No Fishing, Save the Parrotfish!",
            "D" = "The Parrotfish Should Live!")

process_petitions <- function(path_to_qualtrics_export) {
  df <- read_csv(path_to_qualtrics_export, skip=0) %>% 
    filter(`StartDate` != '{"ImportId":"startDate","timeZone":"America/Denver"}' &
             `StartDate` != "Start Date") %>% 
    mutate(across(c("RecordedDate", "StartDate", "EndDate"), ~ymd_hms(.))) %>% 
    filter(Status == "IP Address")  %>% 
    filter(StartDate >= ymd("2025-04-16")) %>% 
    mutate(pair_order = coalesce(FL_26_DO, FL_27_DO, FL_28_DO)) %>% 
    separate(pair_order, c("first_pair", "second_pair"), sep="\\|") %>% 
    mutate(pair1_order = coalesce(`PetitionPair1-AB_DO`, `PetitionPair2-AC_DO`, `PetitionPair3-A/D_DO`),
           pair2_order = coalesce(`PetitionPair1-CD_DO`, `PetitionPair2-BD_DO`, `PetitionPair3-B/C_DO`)) %>% 
    separate(pair1_order, c("var_P1", "var_P1a", "var_P1b", 
                            "var_P2", "var_P2a", "var_P2b", 
                            "var_selection1"), sep="\\|") %>%
    separate(pair2_order, c("var_P3", "var_P3a", "var_P3b", 
                            "var_P4", "var_P4a", "var_P4b", 
                            "var_selection2"), sep="\\|") %>% 
    mutate(across(question1_vars, ~str_c(., "_1"), .names = "{.col}_1")) %>% 
    mutate(across(question1_vars, ~str_c(., "_2"), .names = "{.col}_2")) %>% 
    mutate(across(question2_vars, ~str_c(., "_1"), .names = "{.col}_1")) %>% 
    mutate(across(question2_vars, ~str_c(., "_2"), .names = "{.col}_2")) %>% 
    rowwise() %>% 
    mutate(across(str_c(question1_vars, "_1"), ~get(.), 
                  .names="subject_matter_expert{str_extract(col, 'P[0-9]')}")) %>% 
    mutate(across(str_c(question1_vars, "_2"), ~get(.), 
                  .names="author_committed{str_extract(col, 'P[0-9]')}")) %>% 
    mutate(across(str_c(question2_vars, "_1"), ~get(.), 
                  .names="persuade_undecided{str_extract(col, 'P[0-9]')}")) %>% 
    mutate(across(str_c(question2_vars, "_2"), ~get(.), 
                  .names="motivate_reader{str_extract(col, 'P[0-9]')}")) %>% 
    ungroup() %>% 
    mutate(pass_attention = case_when(str_detect(Q5, "AI-generated drafting") &
                                        FL_3_DO == 'AI-Primed' ~ 1,
                                      !str_detect(Q8, "AI-generated drafting") &
                                        FL_3_DO == 'Control' ~ 1,
                                      TRUE ~ 0),
           AI_primed = if_else(FL_3_DO == "AI-Primed", 1, 0)) %>%
    rowwise() %>%
    mutate(selection1 = get(var_selection1),
           selection2 = get(var_selection2)) %>%
    ungroup() %>% 
    mutate(across(c("var_P1", "var_P2", "var_P3", "var_P4"), 
                  ~case_when(
                    . %in% petitionA ~ "A",
                    . %in% petitionB ~ "B",
                    . %in% petitionC ~ "C",
                    . %in% petitionD ~ "D"),
                  .names="petition{str_replace(col, 'var_', '')}")) %>% 
    pivot_longer(cols = c(str_c("petitionP", 1:4),
                          str_c("subject_matter_expert", str_c("P", 1:4)),
                          str_c("author_committed", str_c("P", 1:4)),
                          str_c("persuade_undecided", str_c("P", 1:4)),
                          str_c("motivate_reader", str_c("P", 1:4))), 
                 names_to=c("type", "petition_order"),
                 names_pattern=c("(.*)(P[0-9])"),
                 values_to="response") %>% 
    mutate(petition_order = str_extract(petition_order, "(?<=P)[0-9]") %>% 
             as.numeric()) %>% 
    pivot_wider(names_from = c("type"), values_from = c("response")) %>%
    mutate(selected_petition = if_else(petition_order %in% c(1,2),
                                       names(titles)[match(selection1, titles)]==petition,
                                       names(titles)[match(selection2, titles)]==petition) %>% 
             as.numeric()) %>% 
    mutate(long_petition = if_else(petition %in% c("B", "D"), 0, 1),
           good_title = if_else(petition %in% c("A", "B"), 1, 0 ))
}