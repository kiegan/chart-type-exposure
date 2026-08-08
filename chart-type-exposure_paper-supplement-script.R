# Header ------------------------------------------------------------------
### This script completes analyses on chart type exposure survey data for the 
### IEEE VIS 2026 full paper submission titled 
### "Exposure to Common Data Visualization Types Among the U.S. Adult Population"

# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(survey)
library(stringr)
library(foreign)
library(forcats)
library(ggview)
library(showtext)
library(systemfonts)
library(margins)


font_add_google("Roboto", "Roboto")
showtext_auto()
showtext_opts(dpi = 400)


# Load data and initial prep ---------------------------------------------------------------
rawdata <- read.csv("OmnibusW2_October2024_PreppedFile.csv")

rawdata <- rawdata |>
  unite("full_response_pattern", `DV1A`:`DV4D`, remove = F) |>
  ## calculate summary variables for each responder on the number of yes, no, and unsure responses
  mutate(number_yes = str_count(full_response_pattern, pattern = "Yes"), 
         number_unsure = str_count(full_response_pattern, pattern = "Unsure"), 
         number_no = str_count(full_response_pattern, pattern = "No"), 
         number_skip = str_count(full_response_pattern, pattern = "SKIPPED ON WEB")) |>
  ## set levels for demographic variables
  mutate(
    ## combine small race/ethnicity groups
    RACETH2 = ifelse(RACETHNICITY %in% c("Other, non-Hispanic", "2+, non-Hispanic"), 
                     "Other, non-Hispanic", RACETHNICITY),
    RACETH2 = factor(RACETH2,
                     #levels = c("1", "2", "4", "6", "Other, non-Hispanic"),
                     levels = c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic", "Asian-Pacific Islander, non-Hispanic", "Other, non-Hispanic"),
                     labels = c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic", "Asian-Pacific Islander, non-Hispanic", "Other, non-Hispanic")),
    AGE4 = factor(AGE4, levels = c("18-29", "30-44", "45-59", "60+")),
    GENDER = factor(GENDER, levels = c("Male", "Female")),
    INCOME4 = factor(INCOME4, levels = c("Less than $30,000", "$30,000 to under $60,000", 
                                         "$60,000 to under $100,000", "$100,000 or more")),
    EDUC5 = factor(EDUC5, levels = c("Less than HS", "HS graduate or equivalent", "Some college/ associates degree", 
                                      "Bachelor's degree", "Post grad study/professional degree")),
    METRO = factor(METRO, levels = c("Non-Metro Area", "Metro Area")),
    DV1A_bin = factor(ifelse(DV1A == "Yes", "Yes", "No or Unsure")), 
    DV1B_bin = factor(ifelse(DV1B == "Yes", "Yes", "No or Unsure")), 
    DV1C_bin = factor(ifelse(DV1C == "Yes", "Yes", "No or Unsure")), 
    DV1D_bin = factor(ifelse(DV1D == "Yes", "Yes", "No or Unsure")),
    DV2A_bin = factor(ifelse(DV2A == "Yes", "Yes", "No or Unsure")), 
    DV2B_bin = factor(ifelse(DV2B == "Yes", "Yes", "No or Unsure")), 
    DV2C_bin = factor(ifelse(DV2C == "Yes", "Yes", "No or Unsure")), 
    DV2D_bin = factor(ifelse(DV2D == "Yes", "Yes", "No or Unsure")),
    DV3A_bin = factor(ifelse(DV3A == "Yes", "Yes", "No or Unsure")), 
    DV3B_bin = factor(ifelse(DV3B == "Yes", "Yes", "No or Unsure")), 
    DV3C_bin = factor(ifelse(DV3C == "Yes", "Yes", "No or Unsure")), 
    DV3D_bin = factor(ifelse(DV3D == "Yes", "Yes", "No or Unsure")),
    DV4A_bin = factor(ifelse(DV4A == "Yes", "Yes", "No or Unsure")), 
    DV4B_bin = factor(ifelse(DV4B == "Yes", "Yes", "No or Unsure")), 
    DV4C_bin = factor(ifelse(DV4C == "Yes", "Yes", "No or Unsure")), 
    DV4D_bin = factor(ifelse(DV4D == "Yes", "Yes", "No or Unsure"))) |>
  ## remove respondents who skipped all 16 items
  filter(number_skip != 16)


### assign data viz types by question sub-ID
question_crosswalk <- data.frame(
  question = c("DV1A", "DV1B", "DV1C", "DV1D", 
               "DV2A", "DV2B", "DV2C", "DV2D", 
               "DV3A", "DV3B", "DV3C", "DV3D", 
               "DV4A", "DV4B", "DV4C", "DV4D"), 
  chart_type = c("Pie chart", "Scatterplot", "Line chart", "Bar chart", 
                 "Bubble chart", "Area chart", "Donut chart", "Diverging stacked bar chart", 
                 "Candlestick chart", "Stacked bar chart", "Dot plot with error bars", "Box and whisker plot", 
                 "Heat map", "Barbell chart", "Grouped bar chart", "Dot plot")
)


### define survey design for calculating survey estimates and completing modeling tasks while incorporating survey weights 
rawdata_design <- svydesign(ids=~1, 
                            #strata = NULL,
                            data = rawdata, 
                            weights = ~WEIGHT)


# Basic Summary Stats -----------------------------------------------------

#### median number of Yes responses
svymean(~number_yes, design = rawdata_design)
median(rawdata$number_yes)
#### number of respondents who reported yes to all 16 items
sum(rawdata$number_yes == 16)/nrow(rawdata)
#### number of respondents who said unsure to more than half
sum(rawdata$number_unsure > 8)/nrow(rawdata)
sum(rawdata$number_unsure > 8)
#### number of respondents who skipped any
sum(rawdata$number_skip > 0)/nrow(rawdata)
sum(rawdata$number_skip > 0)




# Table 2 -----------------------------------------------------------------
## Demographic representation of survey file

educ <- svymean(~EDUC5, design = rawdata_design) |>
  as.data.frame() |>
  tibble::rownames_to_column(var = "Demographic Group") |>
  mutate(Demographic = "Education Level", 
         Group = str_sub(`Demographic Group`, 6, -1)) |>
  left_join(rawdata |> group_by(EDUC5) |>
              summarise(n = n()) |>
              setNames(c("Group", "n")))

age <- svymean(~AGE4, design = rawdata_design) |>
  as.data.frame() |>
  tibble::rownames_to_column(var = "Demographic Group") |>
  mutate(Demographic = "Age Group", 
         Group = str_sub(`Demographic Group`, 5, -1)) |>
  left_join(rawdata |> group_by(AGE4) |>
              summarise(n = n()) |>
              setNames(c("Group", "n")))

gender <- svymean(~GENDER, design = rawdata_design) |>
  as.data.frame() |>
  tibble::rownames_to_column(var = "Demographic Group") |>
  mutate(Demographic = "Gender", 
         Group = str_sub(`Demographic Group`, 7, -1)) |>
  left_join(rawdata |> group_by(GENDER) |>
              summarise(n = n()) |>
              setNames(c("Group", "n")))

metro <- svymean(~METRO, design = rawdata_design) |>
  as.data.frame() |>
  tibble::rownames_to_column(var = "Demographic Group") |>
  mutate(Demographic = "Metro Area Residency", 
         Group = str_sub(`Demographic Group`, 6, -1)) |>
  left_join(rawdata |> group_by(METRO) |>
              summarise(n = n()) |>
              setNames(c("Group", "n")))

income <- svymean(~INCOME4, design = rawdata_design) |>
  as.data.frame() |>
  tibble::rownames_to_column(var = "Demographic Group") |>
  mutate(Demographic = "Income Level", 
         Group = str_sub(`Demographic Group`, 8, -1)) |>
  left_join(rawdata |> group_by(INCOME4) |>
              summarise(n = n()) |>
              setNames(c("Group", "n")))

raceth <- svymean(~RACETH2, design = rawdata_design) |>
  as.data.frame() |>
  tibble::rownames_to_column(var = "Demographic Group") |>
  mutate(Demographic = "Race/Ethnicity", 
         Group = str_sub(`Demographic Group`, 8, -1)) |>
  left_join(rawdata |> group_by(RACETH2) |>
              summarise(n = n()) |>
              setNames(c("Group", "n")))


bind_rows(educ, income, age, metro, gender, raceth) |>
  mutate(perc_se = paste0(format(round(mean*100, 1), digits = 2 ), 
                          "% (", format(round(SE*100, 2), digits =  3), ")")) |>
  select(Demographic, Group, `N Participants` = n, 
         `Weighted Percent of Completes (s.e.)` = perc_se ) 



# Figure 3 ----------------------------------------------------------------
### Number of charts seen

numresponses_overall <- rawdata |>
  pivot_longer(cols = c("number_yes", "number_unsure", "number_no"), 
               names_to = "which_response", values_to = "count") |>
  mutate(which_response = factor(which_response, 
                                 levels = c("number_yes", "number_unsure", "number_no"), 
                                 labels = c("Number of Charts Participant Has Seen", 
                                            "Number of Charts Participant Unsure About", 
                                            "Number of Charts Participant Has Not Seen"))) |>
  ggplot() + 
  geom_bar(aes(x = count, fill = which_response), show.legend = F) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        strip.background = element_blank(), 
        strip.text = element_text(hjust = -0.01, size = 12), 
        axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  facet_wrap(~which_response, ncol = 1, scales = "free_y") + 
  scale_y_continuous(n.breaks = 5,
                     expand = c(0,0)) +
  scale_fill_manual(values = c("#CA500A", "#ADA39D", "#4E79A7")) + 
  scale_x_continuous(breaks = 0:16,
                     labels = 0:16,
                     expand = c(0,0)) +
  labs(x = "", 
       y = "Number of Participants")

numresponses_overall + canvas(dpi = 400, height = 5, width  = 4.5, unit = "in", bg = "white")

ggsave(filename = "figures/response-distributions.png",
       plot = numresponses_overall, 
       height = 4, 
       width = 4.5, 
       unit = "in", 
       bg = "white", 
       dpi = 400
)


# Table 3 -----------------------------------------------------------------
## Logistic regression for number of charts with "Yes" responses

numyesmodel <- svyglm(
  cbind(number_yes, 16-number_yes) ~ EDUC5+INCOME4+AGE4+METRO+GENDER+RACETH2, 
  family = binomial,
  design = rawdata_design
) 

numyesames <- margins_summary(numyesmodel, design = rawdata_design) |>
  setNames(c("term", "AME", "SE", "z", "p", "lower", "upper"))

## Create a crosswalk for the terms used in the model
model_crosswalk <- data.frame(term = c("Intercept", unique(numyesames$term)), 
                              variable = c("Intercept",
                                           rep("Age group", 3), 
                                           
                                           rep("Education level", 4),
                                           "Gender",
                                           rep("Income level", 3), 
                                           "Metro", 
                                            
                                           rep("Race/ethnicity group", 4)), 
                              level = c("Intercept", 
                                        "30-44", 
                                        "45-59", 
                                        "60+", 
                                        "Bachelor's degree",
                                        "HS graduate or equivalent", 
                                        "Post grad study/professional degree",
                                        "Some college/associates degree",
                                        "Female", 
                                        "$100,000 or more",
                                        "$30,000 to under $60,000", 
                                        "$60,000 to under $100,000", 
                                         
                                        "Metro Area", 
                                        "Asian-Pacific Islander, non-Hispanic",
                                        "Black, non-Hispanic", 
                                        "Hispanic", 
                                        "Other, non-Hispanic"), 
                              order = c(1, 2, 3, 4, 7, 5, 8, 6, 9, 12, 10, 11, 13, 14, 15, 16, 17))



numyesmodel |> broom::tidy() |>
  left_join(numyesames)  |>
  left_join(model_crosswalk) |>
  mutate(variable = fct_reorder(variable, order)) |>
  mutate(AME_CI = paste0(round(AME, 2), " (", round(lower, 2), ", ", round(upper, 2), ")"), 
         estimate_se = paste0(round(estimate, 2), " (", round(std.error, 2), ")")) |>
  select(variable, level, estimate_se, p.value, AME_CI)



# Figure 4 ----------------------------------------------------------------
## Response distributions by chart type
### calculate survey means by chart type and clean up formatting
overall_answers <- data.frame()
for(ques in unique(question_crosswalk$question)) {
  
  new <- svymean(reformulate(ques), rawdata_design) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "question_response") %>% 
    mutate(question = str_sub(question_response, 1, 4), 
           response = str_sub(question_response, 5, -1L))
  
  overall_answers <- bind_rows(overall_answers, new)
  
}

### join on crosswalk
overall_answers <- overall_answers %>% 
  left_join(question_crosswalk) %>%
  filter(response != "REFUSED") %>%
  mutate(response = factor(response, levels = c("No", "SKIPPED ON WEB", "Unsure", "Yes"), 
                           labels = c("No", "Unsure", "Unsure", "Yes")))

### create order of highest to lowest "Yes" responses by chart type
order <- overall_answers %>% 
  filter(response == "Yes") %>% 
  arrange(desc(mean)) %>%
  mutate(order_by_yes = n():1) %>%
  select(question, order_by_yes)


#### visualize ####
figure4_data <- overall_answers %>% 
  left_join(question_crosswalk) %>% 
  left_join(order) 

figure4 <- figure4_data %>%
  ggplot(aes(y = fct_reorder(chart_type, order_by_yes), x = mean)) + 
  geom_col(aes(fill = response)) + 
  geom_text(aes(x = 0.03, label = paste0(format(round(mean*100, 1), nsmall = 1), "%")), 
            data = figure4_data %>% filter(response %in% c("Yes")), colour = "white", hjust = 0, 
            fontface = "bold", 
            size = 3) +
  scale_fill_manual(breaks = c("Yes", "Unsure", "No", "SKIPPED ON WEB", "REFUSED"), 
                    values = c("#CA500A", "#ADA39D", "#4E79A7", "#353435", "black"), name = "") + 
  theme_bw() + 
  theme(legend.position = "top", 
        legend.justification = "left",
        legend.margin = margin(t = 0, r = 0.1, b =-0.3, l = -0.2, unit = "cm"),
        plot.margin = margin(t = 0.1, l = 0.1, r = 0.5, b = 0, unit = "cm"),
        plot.title = element_text(size = 10),
        panel.border = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.x = element_text(size = 8), 
        legend.text = element_text(size = 8), 
        legend.key.height = unit(0.5, "cm"), 
        legend.key.width = unit(0.5, "cm"), 
        axis.ticks.x = element_blank()) + 
  labs(x = "Weighted % of responses", 
       y = "") +
  scale_x_continuous(labels = scales::percent, 
                     expand = c(0,0))

figure4 + canvas(dpi = 400, height = 4.5, width  = 4.5, unit = "in", bg = "white")

ggsave(
  filename = "figures/overall-familiarity.png", 
  plot = figure4,
  width = 4.5, 
  height = 4.5, 
  unit = "in", 
  bg = "white", 
  dpi = 400
)



# Figure 5 ----------------------------------------------------------------
## Percent "Yes" by chart type and educational attainment level

### calculate weighted Percent "Yes" and standard errors for each chart type by educational attainment level
educ_answers <- data.frame()
for(ques in unique(question_crosswalk$question)) {
  
  new <- svyby(formula = reformulate(ques), by = ~EDUC5, 
               design = rawdata_design, FUN = svymean) %>%
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "demogroup") %>% 
    mutate(demo = "Education (5-level)") %>%
    pivot_longer(-c("demogroup", "demo", "EDUC5"), names_to = "question_response", values_to = "value" ) %>%
    mutate(se = ifelse(str_detect(question_response, "se."), "se", "mean"), 
           question_response = str_replace(question_response, "se.", "")) %>%
    pivot_wider(names_from = "se", values_from = "value") %>%
    mutate(question = str_sub(question_response, 1, 4), 
           response = str_sub(question_response, 5, -1L))
  
  educ_answers <- bind_rows(educ_answers, new)
  
}

### add on question labels and high-to-low exposure level overall
plot_educ_data <- educ_answers %>% 
  left_join(question_crosswalk) %>% 
  left_join(order)%>%
  mutate(demogroup = factor(demogroup, levels = c("Less than HS", "HS graduate or equivalent", "Some college/ associates degree", "Bachelor's degree", "Post grad study/professional degree"))) %>%
  mutate(response = factor(response, levels = c("SKIPPED ON WEB", "No", "Unsure", "Yes")))

### Create visualization
plot_educ <- plot_educ_data |>
  filter(response == "Yes") |>
  ggplot() + 
  geom_point(aes(x = mean, y = fct_reorder(chart_type, order_by_yes), 
                 colour = demogroup), 
             size = 2) + 
  geom_ribbon(aes(xmin = mean - se, 
                  xmax = mean + se, 
                  y = fct_reorder(chart_type, order_by_yes),
                  group = demogroup,
                  fill = demogroup
  ), 
  orientation = "y", 
  alpha = 0.3) + 
  geom_line(aes(x = mean, y = fct_reorder(chart_type, order_by_yes), 
                colour = demogroup, group = demogroup), 
            orientation = "y", 
            lwd = 1.3) + 
  theme_bw() + 
  scale_fill_manual(values = c(
    "#4E79A7", "#59A14F","#CA500A",  "#B07AA1", "#76B7B2"
  ),
  guide = guide_legend(ncol = 2, direction= "horizontal"), 
  name = "") + 
  scale_colour_manual(values = c(
    "#4E79A7", "#59A14F",  "#CA500A","#B07AA1", "#76B7B2"
  ),
  guide = guide_legend(ncol = 2, direction = "horizontal"), 
  name = "") +
  scale_x_continuous(limits = c(0, 1),
                     labels = scales::label_percent(), 
                     expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) + 
  theme(panel.grid.minor.x = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour = "#D5D0CA"), 
        axis.ticks.x = element_line(colour = "#D5D0CA"), 
        axis.ticks.y = element_blank(), 
        legend.position = "top", 
        legend.justification = "left", 
        legend.title.position = "top",
        axis.title.x = element_text(size = 8), 
        legend.text = element_text(size = 8), 
        legend.key.height = unit(0.5, "cm"), 
        legend.key.width = unit(0.5, "cm"),
        legend.key.spacing.y = unit(0.05, "cm"),
        legend.margin = margin(t = -0.1, r = 0.1, b =-0.1, l = 0, unit = "cm"),
        plot.margin = margin(t = 0.1, l = 0.1, r = 0.5, b = 0, unit = "cm"),
  ) + 
  labs(x = "% Responding Yes (+/- s.e.)", 
       y = "") + 
  coord_cartesian(clip = "off")

plot_educ + canvas(dpi = 400, height = 6, width  = 6, unit = "in", bg = "white")

ggsave(
  filename = "figures/familiarity-educ.png", 
  plot = plot_educ3,
  width = 6, 
  height = 6, 
  unit = "in", 
  bg = "white", 
  dpi = 400
)


# Figure 6 ----------------------------------------------------------------
## Chart-specific logistic regression AME chart


### fit a model for each chart type and save out
charttype_ames <- data.frame()
for(ques in unique(question_crosswalk$question)) {
  
  myques <- paste0(ques, "_bin")  
  myformula <- reformulate(c("EDUC5", "INCOME4", "AGE4", "METRO", "GENDER", "RACETH2"), response = myques)
  model <- svyglm(
    formula = myformula,
    family = quasibinomial(),
    design = rawdata_design) |>
    margins_summary(design = rawdata_design) |>
    select(`term` = factor, `effect` = AME, `p_value` = p, lower, upper) |>
    mutate(question = ques)
  
  charttype_ames <- bind_rows(charttype_ames, model)
}



charttype_ames2 <- charttype_ames |>
  left_join(question_crosswalk) |>
  left_join(figure4_data |> filter(response == "Yes") |> select(mean, chart_type, order_by_yes) ) |>
  left_join(model_crosswalk) |>
  mutate(variable = fct_reorder(variable, order)) |>
  mutate(sig = ifelse(p_value < 0.05, T, F))

modelplotame1 <- charttype_ames2 |>
  mutate(
    sig = ifelse(p_value < 0.05, T, F)) |>
  ggplot() + 
  geom_tile(aes(x = fct_reorder(level, order), y = fct_reorder(chart_type, order_by_yes),
                colour = sig,
                fill = effect), 
            width = 0.8, height = 0.8, 
            lwd = 1.2) + 
  scale_fill_gradient2(low = "#ca500a", mid = "white", high = "#59A14F", 
                       name = "Average\nMarginal\nEffect") +
  scale_colour_manual(values = c("white", "black", "white", "black")) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 20, hjust = 1), 
        panel.border = element_rect(colour = "black", fill = NA), 
        strip.text.x = element_text(hjust = 0, face = "bold", size = 9), 
        panel.grid.major.x = element_blank()) +
  geom_text(aes(x = fct_reorder(level, order), y = fct_reorder(chart_type, order_by_yes), 
                label = round(effect, 2),
                colour = ifelse(abs(effect) > 0.3, "00", "01")), 
            show.legend = F) +
  guides(colour = "none") + 
  coord_cartesian(clip = "off") + 
  facet_grid(~variable, scales = "free_x", space = "free") +
  labs(x = "Variable Level", 
       y = "")
modelplotame1  + canvas(dpi = 400, height = 6, width  = 13, unit = "in", bg = "white")


ggsave(
  filename = "figures/full-model-results-ame.png", 
  plot = modelplotame1, 
  height = 6, 
  width = 13, 
  unit = "in", 
  bg = "white", 
  dpi = 400
)


# Figure 7 ----------------------------------------------------------------
## Conditional probability matrix visual

my_crosswalk <-
  full_join(question_crosswalk, order) |>
  mutate(xvar = chart_type)


conting_table <- table(rawdata$DV1A_bin, rawdata$DV1B_bin)

my_pairs <- expand.grid(conditionvar = paste0(unique(my_crosswalk$question), "_bin"), 
                        probvar = paste0(unique(my_crosswalk$question), "_bin"))
for(i in 1:nrow(my_pairs)){
  condprob <- prop.table(table(rawdata[,as.character(my_pairs$conditionvar[i])], rawdata[,as.character(my_pairs$probvar[i])]), margin = 1)["Yes", "Yes"]
  my_pairs$condprob[i] <- condprob
}

my_crosswalk <- my_crosswalk |>
  mutate(conditionvar = paste0(question, "_bin"))

condprobplot <- my_pairs |>
  left_join(my_crosswalk |> select(conditionvar, `chart_type_condition` = chart_type, order_by_yes)) |>
  left_join(my_crosswalk |> mutate(probvar = conditionvar, order_by_yes_y = order_by_yes) |> 
              select(probvar, `chart_type_prob` = chart_type, order_by_yes_y)) |>
  mutate(condprob = ifelse(condprob == 1, NA, condprob)) |>
  ggplot() + 
  geom_tile(aes(x = fct_reorder(chart_type_prob, order_by_yes_y, .desc = T), 
                y = fct_reorder(chart_type_condition, order_by_yes), 
                fill = condprob), 
            #width = 0.9, 
            #height = 0.9,
            show.legend = F) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        panel.grid.major = element_blank(), 
        panel.border = element_blank()) + 
  scale_fill_gradient2(high = "#59A14F", mid = "white", low = "#CA500A", midpoint= 0.5, na.value = "#D5D0CA") +
  geom_text(aes(x = chart_type_prob,y= chart_type_condition, label = round(condprob, 2), 
                colour = ifelse(condprob < .96, "white", "black")), 
            size = 3, 
            show.legend = F) +
  scale_colour_manual(values = c("white", "black")) + 
  labs(x = "Proportion responding 'Yes' to X axis chart given they responded 'Yes' to Y axis chart", y = "Responded 'Yes' to chart")

condprobplot + canvas(dpi = 400, bg = "white",  height = 7, width = 8, units = "in") 


ggsave(filename = "figures/chart-condprob.png", 
       plot = condprobplot,
       bg = "white", 
       dpi = 400, 
       height = 7, 
       width = 8, 
       units = "in")


