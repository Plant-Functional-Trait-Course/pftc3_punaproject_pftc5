# Plot theme

puna_colour = tribble(~site, ~treatment, ~colour,
                      "WAY", "C", "#E54D00",
                      "WAY", "B", "#650505",
                      "ACJ", "C", "#E58900",
                      "ACJ", "B", "#7F2929",
                      "ACJ", "NB", "#420426",
                      "PIL", "C", "#F4B462",
                      "PIL", "B", "#C06733",
                      "PIL", "BB", "#005385", #bonus colour - pick your own
                      "TRE", "C", "#7AAB76",
                      "TRE", "NB", "#841457",
                      "QUE", "C", "#2C9160", #added to get complete elvation gradient
                      "QUE", "B", "#DAC2A4",
                      "QUE", "NB", "#D15E7F",
                      "OCC", "C", "#033311")

puna_site_colour = puna_colour %>%
  filter(treatment == "C") %>%
  select(-treatment)

puna_treatment_colour = puna_colour %>%
  filter(site == "ACJ" |
         treatment == "BB") %>%
  select(-site)

#To capitalise labels
capitalise <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}



figure_theme <- theme_minimal() +
  theme(axis.text = element_text(size = rel(1)),
        axis.title= element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(0.9)),
        legend.title = element_text(size = rel(1)))

