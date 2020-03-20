library(tidyverse)

data <- read_delim(
  "coronavirus.politologue.com-pays-2020-03-20.csv",
  delim = ";"
  )

pays <- c(
  "France",
  "États-Unis",
  "Royaume-Uni",
  "Corée du Sud",
  "Japon",
  "Allemagne",
  "Espagne",
  "Italie",
  "Iran",
  "Chine"
)

cfr <- data %>%
  mutate(
    CFR = ifelse(Deces > 0, Deces / Infections, 0)
  ) %>%
  filter(Pays %in% pays & Date >= ymd("2020-02-28"))

ggplot(cfr, aes(x = Date, y = CFR, group = Pays, color = Pays)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Évolution du taux de mortalité du coronavirus par pays") +
  xlab("Date") +
  ylab("Taux de mortalité") +
  scale_color_discrete(name = "Pays") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(
    labels = scales::date_format("%d/%m/%Y"),
    date_breaks = "5 day"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Johns Hopkins University après correction par @Politologue_com (https://coronavirus.politologue.com) - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Évolution du taux de mortalité du coronavirus par pays - Mis à jour.png", width = 12, height = 6)

url_italian_data <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"

italian_tests_data <- read_csv(url(url_italian_data)) %>%
  mutate(Date = ymd(floor_date(ymd_hms(data), unit = "day"))) %>%
  group_by(Date) %>%
  summarize(
    Cases = sum(totale_casi),
    Tests = sum(tamponi)
  ) %>%
  mutate(
    New_Cases = Cases - lag(Cases, default = 0),
    New_Tests = Tests - lag(Tests, default = 0)
    ) %>%
  pivot_longer(
    -c(Date, Cases, Tests),
    names_to = "Type",
    values_to = "Number")

ggplot(italian_tests_data, aes(x = Date, y = Number, group = Type, color = Type)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Nombre de nouveaux tests effectués et nombre de nouveaux cas diagnostiqués chaque jour en Italie") +
  xlab("Date") +
  ylab("Number") +
  scale_color_discrete(
    name = "",
    labels = c(
      "Nouveaux cas diagnostiqués",
      "Nouveaux tests effectués"
    )
  ) +
  scale_x_date(
    labels = scales::date_format("%m/%d/%Y"),
    date_breaks = "1 day"
  ) +
  # scale_y_continuous(trans = "log10") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Gouvernement italien (https://github.com/pcm-dpc/COVID-19) - Graphique par Philippe Lemoine (@phl43)") +
  ggsave("Nombre de nouveaux tests effectués et nombre de nouveaux cas diagnostiqués chaque jour en Italie.png", width = 12, height = 6)

italian_icu_data <- read_csv(url(url_italian_data)) %>%
  mutate(Date = ymd(floor_date(ymd_hms(data), unit = "day"))) %>%
  group_by(Date) %>%
  summarize(
    ICU = sum(terapia_intensiva)
  )

ggplot(italian_icu_data, aes(x = Date, y = ICU)) +
  geom_line(size = 1, color = "steelblue") +
  theme_bw() +
  ggtitle("Nombre de personnes actuellement en soins intensifs en Italie") +
  xlab("Date") +
  ylab("Nombre") +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(
    labels = scales::date_format("%d/%m/%Y"),
    date_breaks = "5 day"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Gouvernement italien (https://github.com/pcm-dpc/COVID-19) - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Nombre de personnes actuellement en soins intensifs en Italie.png", width = 12, height = 6)