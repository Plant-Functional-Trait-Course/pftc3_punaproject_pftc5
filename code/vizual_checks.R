library("vegan")
library("ggvegan")

index <- species_cover %>%
  group_by(year, month, site, elevation, treatment, plot_id) %>%
  summarise(richness = n(),
            diversity = diversity(cover),
            evenness = diversity/log(richness),
            sum_cover = sum(cover),
            graminoid_prop = sum(cover[functional_group == "Gramminoid"])/sum_cover,
            forb_prop = sum(cover[functional_group == "Forb"])/sum_cover,
            wood_prop = sum(cover[functional_group == "Woody"])/sum_cover)

index %>%
  filter(treatment %in% c("C", "B")) %>%
ggplot(aes(x = elevation, y = diversity, colour = treatment)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x") +
  facet_wrap(~ month) +
  theme_minimal()



trait_data_peru %>%
  mutate(value_trans = if_else(trait %in% c("dry_mass_g", "leaf_area_cm2", "plant_height_cm", "wet_mass_g"), log(value), value),
         trait = case_when(trait == "dry_mass_g" ~ "dry_mass_g_log",
                           trait == "wet_mass_g" ~ "wet_mass_g_log",
                           trait == "leaf_area_cm2" ~ "leaf_area_cm2_log",
                           trait == "plant_height_cm" ~ "plant_height_cm_log",
                           TRUE ~ trait)) %>%
  ggplot(aes(x = value_trans, fill = site)) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis_d(option = "plasma") +
  facet_wrap(~ trait, scales = "free") +
  theme_minimal()

# Check traits
trait_data_peru %>%
  filter(trait %in% c("wet_mass_g", "dry_mass_g")) %>%
  pivot_wider(names_from = trait, values_from = value) %>%
  ggplot(aes(x = log(wet_mass_g), y = log(dry_mass_g), colour = treatment)) +
  geom_point(colour = "lightblue", alpha = 0.5) +
  scale_colour_viridis_d(option = "plasma", end = 0.8) +
  facet_wrap(~ course) +
  theme_minimal()


trait_data_peru %>%
  filter(trait %in% c("leaf_area_cm2", "dry_mass_g")) %>%
  pivot_wider(names_from = trait, values_from = value) %>%
  ggplot(aes(x = log(leaf_area_cm2), y = log(dry_mass_g), colour = treatment)) +
  geom_point(alpha = 0.5) +
  scale_colour_viridis_d(option = "plasma", end = 0.8) +
  facet_wrap(~ course) +
  theme_minimal()

