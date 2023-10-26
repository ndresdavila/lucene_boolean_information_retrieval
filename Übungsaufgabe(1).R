#install.packages("gifski")
library(gganimate)
library(ggplot2)
library(dplyr)
library(gapminder)
library(ggthemes)
library(gifski)

gapminder
# gdpPercap: Pro-Kopf-BIP (x-Axis)
# lifeEXP: Lebenserwartung (y-Axis)
# pop: Einwohnerzahl (Größe der Punkte im Diagramm)

graph1 = gapminder %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, color=continent, size=pop)) +
  geom_point(alpha = 0.7, stroke = 0) +
  theme_fivethirtyeight() +
  scale_size(range=c(2,12), guide="none") +
  scale_x_log10() +
  labs(title = "Lebenserwartung vs Pro-Kopf-BIP nach Ländern",
       x = "Einkommen pro Person (BIP/Kopf)",
       y = "Lebenserwartung (Jahren)",
       color = "Kontinent") +
  theme(axis.title = element_text(),
  legend.text=element_text(size=10)) +
  scale_color_brewer(palette = "Set2")

graph1.animation = graph1 +
  transition_time(year) +
  labs(subtitle = "Jahr: {frame_time}") +
  shadow_wake(wake_length = 0.1)

animate(graph1.animation, height = 500, width = 800, fps = 30, duration = 10,
        end_pause = 60, res = 100)