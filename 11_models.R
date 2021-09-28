
library(tidyverse)
## library(brms)
library(lme4)
library(modelr)
library(scales)

lda_output = read_csv("data/nov_tra_res.csv", col_types = cols())
lda_data   = read_csv("data/tidy_metadata.csv",   col_types = cols()) %>%
    right_join(lda_output, by = "doc_id") %>%
    mutate(Parti = ifelse(Parti == "RV", "R", Parti),
           Parti = ifelse(Parti == "KF", "K", Parti)) %>%
    mutate(Blok = factor(case_when(
        Parti %in% c("DF", "LA", "V", "K") ~ "Blå Blok",
        Parti %in% c("ALT", "EL", "R", "S", "SF") ~ "Rød Blok",
        TRUE ~ "Grønland/Færøerne"
        ), levels = c("Rød Blok", "Grønland/Færøerne", "Blå Blok")),
        Parti = factor(Parti, levels = unique(Parti))) %>%
    filter(## Blok != "Grønland/Færøerne"
        complete.cases(Parti))##  %>%
    ## mutate(is_minister = ifelse(is.na(Ministerpost), "No", "Yes"))


## GGPLOT customizations
my_theme = theme_classic() +
    theme(legend.position = c(.92,.25),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.title.align = 0,
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 22, hjust = .5),
          # plot.subtitle = element_text(size = 12, hjust = .3),
          axis.title = element_text(size = 18))
my_guides = guides(fill = guide_colorbar(title.position = "bottom"))



## parti_colour_scale = rep("#000000", length(unique(lda_data$Parti)))
## parti_colour_scale[1] = "#B41015"
## parti_colour_scale[2] = "#1D4267"
## parti_colour_scale[3] = "#ED77A9"
## parti_colour_scale[4] = "#B41015"
## parti_colour_scale[1] = "#B41015"
## parti_colour_scale[1] = "#B41015"
## parti_colour_scale[1] = "#B41015"


## parti_colour_scale = c("#B41015", "#1D4267", "#ED77A9", "#F37B00", "#7C2983", "#EEC600", "#00B4BF", "#8DB400", rep("#000000", 4), "#00892A")
## names(parti_colour_scale) = unique(lda_data$Parti)


###### TRANSIENCE

## models

lda_data = filter(lda_data, Period %in% c("2007-2011", "2011-2015", "2015-2019"))

trans_1 = lmer(transience ~ novelty + (1|Dato),
               data = lda_data, REML = FALSE)
trans_2 = update(trans_1, .~. + Parti)
trans_3 = update(trans_2, .~. + Parti:novelty)

## anova(trans_1, trans_2, trans_3)
## summary(trans_3)



## plot
                 
ranef_lines = expand.grid(novelty = modelr::seq_range(lda_data$novelty, 20),
            Dato = sample(unique(lda_data$Dato), 10),
            Parti = unique(lda_data$Parti)) %>%
    mutate(y = predict(trans_3, newdata = ., re.form = NA)) %>%
    left_join(distinct(lda_data, Parti, Blok)) %>%
    filter(Blok != "Grønland/Færøerne")

# transience vs novelty
ggplot(lda_output, aes(novelty, transience)) +
    geom_bin2d(bins = 100) +
    ## scale_fill_viridis_c(trans = "log", breaks = c(1,10,100,1000,10000),
    ##                      labels = trans_format("log10", math_format(expr = 10^.x, format = force))) +
    scale_fill_gradient(low = "white", high = "black",
                        trans = "log", breaks = c(1,10,100,1000,10000),
                        labels = trans_format("log10", math_format(expr = 10^.x, format = force))) +
    ## geom_smooth(method = "lm", colour = "black", linetype = "dashed",  alpha = 0) +
    geom_line(aes(y = y, group = Parti, colour = Parti), data = ranef_lines, size = 1) +
    ## geom_ribbon(aes(y = NULL, ymin = ymin, ymax = ymax, group = Parti, colour = NULL), data = ranef_lines, alpha = 1) +
    my_guides + my_theme +
    scale_color_manual(values = parti_colour_scale) + 
    coord_cartesian(xlim = c(.8, 5.2),
                    ylim = c(.8, 6)) +
    labs(x = "Novelty", y = "Transience", title = "Transience vs. Novelty", caption = "Data: Speeches in Folketinget 2009-2017") ## +
    ## facet_wrap(~Parti)

### resonance
reso_1 = lmer(resonance ~ novelty + (1|Dato),
               data = lda_data, REML = FALSE)
reso_2 = update(reso_1, .~. + Parti)
reso_3 = update(reso_2, .~. + Parti:novelty)

anova(reso_1, reso_2, reso_3)

summary(reso_3)


ranef_lines2 = expand.grid(novelty = modelr::seq_range(lda_data$novelty, 20),
            Dato = sample(unique(lda_data$Dato), 10),
            Parti = unique(lda_data$Parti)) %>%
    mutate(y = predict(reso_3, newdata = ., re.form = NA)) %>%
    left_join(distinct(lda_data, Parti, Blok)) %>%
    filter(Blok != "Grønland/Færøerne")

# resonance vs novelty
ggplot(lda_output, aes(novelty, resonance)) +
    geom_bin2d(bins = 50) +
    ## scale_fill_viridis_c(trans = "log", breaks = c(1,10,100,1000,10000),
    ##                      labels = trans_format("log10", math_format(expr = 10^.x, format = force))) +
    scale_fill_gradient(low = "white", high = "black",
                        trans = "log", breaks = c(1,10,100,1000,10000),
                        labels = trans_format("log10", math_format(expr = 10^.x, format = force))) +
    ## geom_smooth(method = "lm", colour = "black", linetype = "dashed",  alpha = 0) +
    geom_line(aes(y = y, group = Parti, colour = Parti), data = ranef_lines2, size = 2) +
    scale_color_manual(values = parti_colour_scale) + 
    ## geom_ribbon(aes(y = NULL, ymin = ymin, ymax = ymax, group = Parti, colour = NULL), data = ranef_lines, alpha = 1) +
    my_guides + my_theme +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    coord_cartesian(xlim = c(.8, 5.2),
                    ylim = c(-2.5, 2)) +
    labs(x = "Novelty", y = "Resonance", title = "Transience vs. Resonance", caption = "Data: Speeches in Folketinget 2009-2017")##  +




### BY PERIOD analysis

trans_4 = update(trans_1, .~. + Parti*novelty*Period)
## anova(trans_3, trans_4)
reso_4 = update(reso_1, .~. + Parti*novelty*Period)
## anova(reso_3, reso_4)



ranef_lines3 = distinct(lda_data, Parti, Period) %>%
    group_by(Parti, Period) %>%
    data_grid(novelty = seq_range(lda_data$novelty, 20)) %>%
    ungroup() %>%
    mutate(y = predict(trans_4, newdata = ., re.form = NA)) %>%
    left_join(distinct(lda_data, Parti, Blok)) %>%
    filter(Blok != "Grønland/Færøerne")


# transience vs novelty
ggplot(lda_output, aes(novelty, transience)) +
    geom_bin2d(bins = 100) +
    ## scale_fill_viridis_c(trans = "log", breaks = c(1,10,100,1000,10000),
    ##                      labels = trans_format("log10", math_format(expr = 10^.x, format = force))) +
    scale_fill_gradient(low = "white", high = "black",
                        trans = "log", breaks = c(1,10,100,1000,10000),
                        labels = trans_format("log10", math_format(expr = 10^.x, format = force))) +
    ## geom_smooth(method = "lm", colour = "black", linetype = "dashed",  alpha = 0) +
    geom_line(aes(y = y, group = Parti, colour = Parti), data = ranef_lines3, size = 1) +
    ## geom_ribbon(aes(y = NULL, ymin = ymin, ymax = ymax, group = Parti, colour = NULL), data = ranef_lines, alpha = 1) +
    my_guides + my_theme +
    scale_color_manual(values = parti_colour_scale) + 
    coord_cartesian(xlim = c(.8, 5.2),
                    ylim = c(.8, 6)) +
    labs(x = "Novelty", y = "Transience", title = "Transience vs. Novelty", caption = "Data: Speeches in Folketinget 2009-2017") +
    facet_wrap(~Period)



ranef_lines4 = distinct(lda_data, Parti, Period) %>%
    group_by(Parti, Period) %>%
    data_grid(novelty = seq_range(lda_data$novelty, 20)) %>%
    ungroup() %>%
    mutate(y = predict(reso_4, newdata = ., re.form = NA))##  %>%
    ## left_join(distinct(lda_data, Parti, Blok)) %>%
    ## filter(Blok != "Grønland/Færøerne")


# resonance vs novelty
ggplot(lda_output, aes(novelty, resonance)) +
    geom_bin2d(bins = 50) +
    ## scale_fill_viridis_c(trans = "log", breaks = c(1,10,100,1000,10000),
    ##                      labels = trans_format("log10", math_format(expr = 10^.x, format = force))) +
    scale_fill_gradient(low = "white", high = "black",
                        trans = "log", breaks = c(1,10,100,1000,10000)) +
    ## = trans_format("log10", math_format(expr = 10^.x, format = force))) +
    ## geom_smooth(method = "lm", colour = "black", linetype = "dashed",  alpha = 0) +
    geom_line(aes(y = y, group = Parti, colour = Parti), data = ranef_lines4, size = 2) +
    ## scale_color_manual(values = parti_colour_scale) + 
    ## geom_ribbon(aes(y = NULL, ymin = ymin, ymax = ymax, group = Parti, colour = NULL), data = ranef_lines, alpha = 1) +
    my_guides + my_theme +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    coord_cartesian(xlim = c(.8, 5.2),
                    ylim = c(-2.5, 2)) +
    labs(x = "Novelty", y = "Resonance", title = "Transience vs. Resonance", caption = "Data: Speeches in Folketinget 2009-2017") +
    facet_wrap(~Period)




##### MINISTER post
trans_5 = update(trans_3, .~. + is_minister*novelty)
reso_5 = update(reso_3, .~. + is_minister*novelty)
