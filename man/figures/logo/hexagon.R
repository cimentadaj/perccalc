library(viridis)
library(carData)
library(perccalc)
library(dplyr)
library(ggplot2)
library(hexSticker)
data("GSSvocab")

gss <- 
  as_tibble(GSSvocab) %>% 
  filter(year == '1978') %>% 
  mutate(weight = sample(1:3, size = nrow(.), replace = TRUE, prob = c(0.1, 0.5, 0.4))) %>% 
  select(ageGroup, vocab, weight)

gss <-
  gss %>%
  mutate(ageGroup = factor(ageGroup, ordered = TRUE))

p <-
  gss %>%
  perc_dist(ageGroup, vocab, weight) %>%
  mutate(ci_low = estimate - 1.96 * std.error,
         ci_hi = estimate + 1.96 * std.error,
         alpha = cumsum(estimate)) %>%
  ggplot(aes(percentile, estimate)) +
  geom_point(aes(color = alpha), size = 0.05) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_hi, color = alpha), size = 0.1) +
  scale_color_viridis_c() +
  geom_text(aes_(x=45, y=-0.5, angle=0), label="perc",
            color="#404788FF", size=16) +
  geom_text(aes_(x=68, y=-1.1, angle=-45), label="calc",
            color="#404788FF", size=16)

p <- p + theme_void() + theme_transparent() +  theme(legend.position = "none")

p

sticker_p <- sticker(p,
                     package="perccalc", 
                     s_x = 1, # horizontal position of subplot
                     s_y = 1.1, # vertical position of subplot
                     s_width = 1.7, # width of subplot
                     s_height = 1.2, # height of subplot
                     p_x = 1, # horizontal position of font
                     p_y = .55, # vertical position of font
                     spotlight = TRUE,
                     l_x = 1,
                     l_y = .43,
                     l_alpha = 2/3,
                     h_size = 1, # hexagon border size
                     h_fill = "white", # hexagon fill colour
                     h_color = "#404788FF",# hexagon border colour
                     filename= "logo_hex.png")

sticker_p
