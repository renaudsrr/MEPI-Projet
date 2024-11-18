library(ggplot2)
library(gganimate)
library(dplyr)
library(maps)
library(sf)
library(animation)
library(gifski)
library(geosphere)
library(gridExtra)

rm(list=ls())  
graphics.off() 
cat("\014")

setwd("C:/Users/User/Desktop/MASTER/M2/MEPI/PROJET_MEPI")
animation_dir = "C:/Users/User/Desktop/MASTER/M2/MEPI/PROJET_MEPI/animations/"

epidemy = read.csv("us-counties.csv", h = T)
head(epidemy)
str(epidemy)
colnames(epidemy)

###########################################################################
# Traitement data ---------------------------------------------------------
###########################################################################
# enlever lignes avec NA
epidemy = na.omit(epidemy)
colSums(is.na(epidemy))
epidemy$date = as.Date(epidemy$date, format = "%Y-%m-%d")

# enlever états en dehors map (outre mer + alaska)
map_state = map_data("state")
length(unique(epidemy$state))
length(unique(map_state$state))
setdiff(unique(epidemy$state), unique(map_state$state))
epidemy = epidemy %>% 
  filter(!state %in% c("Alaska", "American Samoa", "Guam", "Hawaii", 
                       "Northern Mariana Islands", "Puerto Rico", "Virgin Islands"))

###########################################################################
# Représentation graphique ------------------------------------------------
###########################################################################

map_state = map_data("state")
map_state = map_state %>% rename("state" = "region")
map_state$state = sort(tools::toTitleCase(map_state$state)) # ajouter maj + sort
epidemy = epidemy %>% arrange(epidemy$state)

# isoler chaque coord de chaque états
list_coord_state  = list()
for (i in 1:length(unique(epidemy$state))){
  state_i = unique(epidemy$state)[i]
  print(state_i)
  
  maps_by_state = map_state %>%
    filter(state == state_i)
  
  long_state_i = maps_by_state$long
  lat_state_i = maps_by_state$lat
  
  list_coord_state[[i]] = matrix(c(long_state_i,lat_state_i), ncol = 2)
}

# génerer le centroid de chaque état
state = unique(epidemy$state)
group = seq(from=1,to=49,by=1)
centroids_long = rep(0,49)
centroids_lat = rep(0,49)
state_centroids = data.frame(state,group,centroids_long,centroids_lat)

for (i in 1:length(list_coord_state)){
  centroid_result = centroid(list_coord_state[[i]])
  state_centroids$centroids_long[i] = centroid_result[1] # longitude  
  state_centroids$centroids_lat[i] = centroid_result[2] # latitude  
  
}

# afficher la carte
gg = ggplot(map_state, aes(long, lat, group = group)) +
  geom_polygon(fill = "grey", color = "black") +
  coord_fixed(1.3) +
  geom_text(data = state_centroids, aes(x = centroids_long, y = centroids_lat, label = state), 
            size = 2, color = "black") +   
  theme_void() # theme_minimal()
plot(gg)


###########################################################################
# Exploration dynamiques --------------------------------------------------
###########################################################################


### PAYS ------------------------------------------------------------------
### DYNAMIQUE CARTE
map_country = map_data("usa")
map_country = map_country %>% select(c(1,2,5))

# merge pour dynamique à l'échelle du pays
epidemy_country = epidemy %>%
  group_by(date) %>%
  summarize(total_cases = sum(cases, na.rm = TRUE),
            total_deaths = sum(deaths, na.rm = TRUE))%>%
  mutate(region="main")%>%
  arrange(date) %>% 
  mutate(new_cases = total_cases - lag(total_cases, default = 0)) %>% # Différence entre jours consécutifs
  mutate(new_deaths = total_deaths - lag(total_deaths, default = 0)) 

# pour intergration lat en long dans espidemy_country
epidemy_maps_country = left_join(epidemy_country,map_country,by="region")

# Animation
gg = ggplot(epidemy_maps_country, aes(long, lat, group = region)) +
  geom_polygon(aes(fill = new_cases), color = "black") +  
  
  scale_fill_gradientn(colors = c("blue", "yellow", "red"),
                       values = scales::rescale(c(0, 10000, max(epidemy_country$total_cases)))) + 

  labs(title = 'Total des Cas aux États-Unis - {frame_time}', 
       fill = 'Total Cases') +
  geom_text(aes(x = -125, y = 25, label = paste("Total Cases: ", total_cases), 
                group = 1), size = 6, color = "black", vjust = -1) +
  
  theme_void() +
  transition_time(date) +  
  ease_aes('linear')

nframes = 100  

animated_plot = animate(gg, 
                        nframes = nframes, 
                        fps = 10, 
                        width = 800, 
                        height = 600,
                        renderer = gifski_renderer(),
                        end_pause = 20,
                        rewind = FALSE)

# Sauvegarder l'animation au format GIF
date = Sys.Date()
anim_save(paste0(animation_dir, 'evolution_new_cases_country_', date, '.gif'))

### DYNAMIQUE GRAPHS
gg = ggplot(data = epidemy_country, aes(x = date)) +
  geom_point(aes(y = total_cases, col = "Total Cases")) +
  geom_point(aes(y = new_cases, col = "New Cases")) +
  geom_point(aes(y = total_deaths, col = "Total Deaths")) +
  geom_point(aes(y = new_deaths, col = "New Deaths")) +
  
  scale_color_manual(values = c("Total Cases" = "blue",
                                "New Cases" = "skyblue",
                                "Total Deaths" = "red",
                                "New Deaths" = "orange")) +
  
  # février 2021 : début vaccination
  geom_vline(xintercept = as.Date("2021-02-01"), color = "black", linetype = "dashed", linewidth = 1) +
  
  labs(title = "Évolution des cas et des décès",
       x = "Date", 
       y = "Nombre de cas et de décès (log)",
       legend = "Legend")+
  scale_y_log10()
plot(gg)



### ETAT -----------------------------------------------------------------------
### DYNAMIQUE CARTE
map_state = map_data("state")
map_state = map_state %>% select(-6) %>% rename("state"="region")
map_state$state = sort(tools::toTitleCase(map_state$state)) # ajouter maj + sort

# merge pour dynamique à l'échelle des états
epidemy_state = epidemy %>%
  group_by(date,state) %>%
  summarize(cases_by_state = sum(cases, na.rm = TRUE),
            deaths_by_state = sum(deaths, na.rm = TRUE), .groups = 'drop')%>%
  
  arrange(state, date) %>% 
  
  mutate(new_cases_by_state = pmax(cases_by_state - lag(cases_by_state, default = 0), 0)) %>%  # Différence entre jours consécutifs, mais valeurs minimales à 0
  mutate(new_deaths_by_state = pmax(deaths_by_state - lag(deaths_by_state, default = 0), 0))  # Idem pour les décès

# pour intergration lat en long dans espidemy_state
epidemy_maps_state = left_join(epidemy_state,map_state,by = "state")

### Animation new_cases_by_state
# gg = ggplot(epidemy_maps_state, aes(long, lat, group = state)) +
#   geom_polygon(data = epidemy_maps_state, aes(fill = new_cases_by_state), color = "black") +  
#   scale_fill_gradientn(colors = c("blue", "yellow", "red"),
#                        values = scales::rescale(c(0, 10000, max(epidemy_country$new_cases_by_state)))) + 
#   labs(title = 'Cas aux États-Unis par state - {frame_time}', 
#        fill = 'Total Cases') +
#   geom_text(data = epidemy_country ,aes(x = -125, y = 25, label = paste("Total Cases: ", total_cases), 
#                 group = 1), size = 6, color = "black", vjust = -1) +
#   geom_line(data = epidemy_country, aes(x=date, y= total_cases))+
#   
#   theme_void() +
#   transition_time(date) +  
#   ease_aes('linear')
# 
# nframes = 100  
# 
# animated_plot = animate(gg, 
#                         nframes = nframes, 
#                         fps = 10, 
#                         width = 800, 
#                         height = 600,
#                         renderer = gifski_renderer(),
#                         end_pause = 20,
#                         rewind = FALSE)

### Animation cases_by_state
gg = ggplot(epidemy_maps_state, aes(long, lat, group = state)) +
  geom_polygon(data = epidemy_maps_state, aes(fill = cases_by_state), color = "black") +  
  scale_fill_gradientn(colors = c("blue", "yellow", "red"),
                       values = scales::rescale(c(0, 10000, max(epidemy_country$cases_by_state)))) + 
  labs(title = 'Cas aux États-Unis par state - {frame_time}', 
       fill = 'Total Cases') +
  geom_text(data = epidemy_country ,aes(x = -110, y = 25, label = paste("Total Cases: ", total_cases), 
                                       group = 1), size = 6, color = "black", vjust = -1) +
  theme_void() +
  transition_time(date) +  
  ease_aes('linear')

nframes = 100  
animated_plot = animate(gg, 
                        nframes = nframes, 
                        fps = 10, 
                        width = 800, 
                        height = 600,
                        renderer = gifski_renderer(),
                        end_pause = 20,
                        rewind = FALSE)

# sauvegarde
date_time <- Sys.time()
date <- format(date_time, "%Y-%m-%d_%H-%M-%S")
anim_save(paste0(animation_dir, 'evolution_cases_state_', date, '.gif'))


### DYNAMIQUE GRAPHS
gg_cases = ggplot(data = epidemy_state, aes(x = date)) +
  geom_line(aes(y = cases_by_state, color = state)) +
  labs(title = "Évolution des cas par état",
       x = "Date", 
       y = "Nombre de cas ",
       color = "État") +  
  theme(legend.position = "none")

gg_new_cases = ggplot(data = epidemy_state, aes(x = date)) +
  geom_line(aes(y = new_cases_by_state, color = state)) +
  labs(title = "Nouveaux cas par état",
       x = "Date", 
       y = "Nouveaux cas",
       color = "État") +
  theme(legend.position = "none")

gg_deaths = ggplot(data = epidemy_state, aes(x = date)) +
  geom_line(aes(y = deaths_by_state, color = state)) +
  labs(title = "Évolution des décès par état",
       x = "Date", 
       y = "Nombre de décès",
       color = "État") +
  theme(legend.position = "none")

gg_new_deaths = ggplot(data = epidemy_state, aes(x = date)) +
  geom_line(aes(y = new_deaths_by_state, color = state)) +
  labs(title = "Nouveaux décès par état",
       x = "Date", 
       y = "Nouveaux décès",
       color = "État") +
  theme(legend.position = "none")

grid.arrange(gg_cases, gg_new_cases, gg_deaths, gg_new_deaths, ncol = 2)

### COUNTIES -------------------------------------------------------------------
### DYNAMIQUE CARTE
map_county = map_data("county")
map_county = map_county %>% rename("state"="region")
map_county = map_county %>% rename("county"="subregion")
map_county$state = tools::toTitleCase(map_county$state) # ajouter maj + sort
map_county$county = sort(tools::toTitleCase(map_county$county)) # ajouter maj + sort

# merge pour dynamique à l'échelle des états
epidemy_county = epidemy %>%
  group_by(date,county) %>%
  summarize(cases_by_county = sum(cases, na.rm = TRUE),
            deaths_by_county = sum(deaths, na.rm = TRUE))%>%
  
  arrange(county, date) %>% 
  
  mutate(new_cases_by_county = cases_by_county - lag(cases_by_county, default = 0)) %>% # Différence entre jours consécutifs
  mutate(new_deaths_by_county = deaths_by_county - lag(deaths_by_county, default = 0)) 

# pour intergration lat en long dans espidemy_state
epidemy_maps_county = left_join(epidemy_county,map_county,by="county")

# Animation
gg = ggplot(epidemy_maps_county, aes(long, lat, group = county)) +
  geom_polygon(aes(fill = new_cases_by_county), color = "black") +  
  
  scale_fill_gradientn(colors = c("blue", "yellow", "red"),
                       values = scales::rescale(c(0, 10000, max(epidemy_country$new_cases_by_county)))) + 
  
  labs(title = 'Cas aux États-Unis par county - {frame_time}', 
       fill = 'Total Cases') +
  geom_text(data = epidemy_country ,aes(x = -125, y = 25, label = paste("Total Cases: ", total_cases), 
                                       group = 1), size = 6, color = "black", vjust = -1) +
  
  theme_void() +
  transition_time(date) +  
  ease_aes('linear')

nframes = 100  
animated_plot = animate(gg, 
                        nframes = nframes, 
                        fps = 10, 
                        width = 800, 
                        height = 600,
                        renderer = gifski_renderer(),
                        end_pause = 20,
                        rewind = FALSE)

# sauvegarde
date = Sys.Date()
anim_save(paste0(animation_dir, 'evolution_cases_county_', date, '.gif'))

### DYNAMIQUE GRAPHS
gg_cases = ggplot(data = epidemy_county, aes(x = date)) +
  geom_line(aes(y = cases_by_county, color = county)) +
  labs(title = "Évolution des cas par county",
       x = "Date", 
       y = "Nombre de cas (log)",
       color = "État") +  
  scale_y_log10() +
  theme(legend.position = "none")

gg_new_cases = ggplot(data = epidemy_county, aes(x = date)) +
  geom_line(aes(y = new_cases_by_county, color = county)) +
  labs(title = "Nouveaux cas par county",
       x = "Date", 
       y = "Nouveaux cas (log)",
       color = "État") +
  scale_y_log10() +
  theme(legend.position = "none")

gg_deaths = ggplot(data = epidemy_county, aes(x = date)) +
  geom_line(aes(y = deaths_by_county, color = county)) +
  labs(title = "Évolution des décès par county",
       x = "Date", 
       y = "Nombre de décès (log)",
       color = "État") +
  scale_y_log10() +
  theme(legend.position = "none")

gg_new_deaths = ggplot(data = epidemy_county, aes(x = date)) +
  geom_line(aes(y = new_deaths_by_county, color = county)) +
  labs(title = "Nouveaux décès par county",
       x = "Date", 
       y = "Nouveaux décès (log)",
       color = "État") +
  scale_y_log10() +
  theme(legend.position = "none")

grid.arrange(gg_cases, gg_new_cases, gg_deaths, gg_new_deaths, ncol = 2)

################################################################################
# Creation model ---------------------------------------------------------------
################################################################################

# Réseux de contact avec modèles SEIR ou SEIRS ? (bibliographie)
# trouver matrice d'adjacence
contact_matrix = matrix()
# choix réseaux orienté pondéré ?
# statique ou dynamique (si dynamique, obligatoirement en tps discret ?)

# déduire métriques :
# - degré
# - degré pondéré
# - polarité
# - GSCC

# Modèle 
Paramètres globaux
T <- 50  # Durée de la simulation
num_populations <- 9
pop_sizes <- c(1000, 800, 1200, 600, 400, 700, 100, 50, 500)
beta <- 0.2
beta_inter <- 0.01
gamma <- 0.05
delta <- 0.01  # Taux de perte d'immunité (Rétablis devenant Susceptibles)


metapop_id = "05"

info_pop = read.csv(paste0("../TD_metapop_network_data/net_",metapop_id,"/","info_pop_aggregate.csv"))
network_static = read.csv(paste0("../TD_metapop_network_data/net_",metapop_id,"/","network_static.csv"))

num_populations = nrow(info_pop)
info_pop$simu_id = seq(1,num_populations)

# matrice de contacts alternative
contact_matrix <- matrix(0.0, nrow = num_populations, ncol = num_populations)
for (i in 1:num_populations) {
  for (j in 1:num_populations) {
    the_weight = network_static[(network_static$source_id == info_pop$node_id[info_pop$simu_id == i]) & (network_static$destination_id == info_pop$node_id[info_pop$simu_id == j]),]$weight
    if (length(the_weight) > 0) {
      contact_matrix[i,j] = the_weight
    }
  }
}

pop_sizes = info_pop$nb_indiv


# in_degree = table(network_static$destination_id)
# out_degree = table(network_static$source_id)



network_dynamic_weeks = read.csv(paste0("../TD_metapop_network_data/net_",metapop_id,"/","network_dynamic_weeks.csv"))

list_of_contact_matrix = list()
# matrice de contacts alternative
for (t in 1:max(network_dynamic_weeks$date)) {
  tmp_net = network_dynamic_weeks[network_dynamic_weeks$date == t,]
  contact_matrix <- matrix(0.0, nrow = num_populations, ncol = num_populations)
  for (i in 1:num_populations) {
    for (j in 1:num_populations) {
      the_weight = tmp_net[(tmp_net$source_id == info_pop$node_id[info_pop$simu_id == i]) & (tmp_net$destination_id == info_pop$node_id[info_pop$simu_id == j]),]$weight
      if (length(the_weight) > 0) {
        contact_matrix[i,j] = the_weight
      }
    }
  }
  list_of_contact_matrix[[t]] = contact_matrix
}



# Initialisation des vecteurs pour stocker les résultats pour chaque population
S <- matrix(0, nrow = num_populations, ncol = T)
I <- matrix(0, nrow = num_populations, ncol = T)
R <- matrix(0, nrow = num_populations, ncol = T)
N <- matrix(0, nrow = num_populations, ncol = T)
new_infections <- matrix(0, nrow = num_populations, ncol = T)
new_infections_from_ext <- matrix(0, nrow = num_populations, ncol = T)
new_recoveries <- matrix(0, nrow = num_populations, ncol = T)
new_lossimmunity <- matrix(0, nrow = num_populations, ncol = T)

# Initialisation des conditions initiales pour chaque population
for (i in 1:num_populations) {
  S[i, 1] <- pop_sizes[i]  # Une personne infectée au départ
  I[i, 1] <- 0
  R[i, 1] <- 0
  N[i, 1] <- S[i, 1] + I[i, 1] + R[i, 1]
}
# infection d'une population aléatoirement
pop_id = sample(1:num_populations, 1)
S[pop_id, 1] <- pop_sizes[pop_id] - 1  # Une personne infectée au départ
I[pop_id, 1] <- 1

# Simulation de l'épidémie dans la métapopulation
for (t in 2:T) {
  for (i in 1:num_populations) {
    # Dynamique dans la population i
    # Calcul des probabilités de transition
    prob_transmission <- 1 - exp(-beta * I[i, t-1] / N[i, t-1])
    prob_recuperation <- 1 - exp(-gamma)
    prob_lossimmunity <- 1 - exp(-delta)
    
    # Mise à jour des compartiments S, I et R
    new_infections[i, t] <- rbinom(1, S[i, t-1], prob_transmission)
    new_recoveries[i, t] <- rbinom(1, I[i, t-1], prob_recuperation)
    new_lossimmunity[i, t] <- rbinom(1, R[i, t-1], prob_lossimmunity)
    
    
    # Mettre à jour les compartiments S, I, R dans la population i
    S[i, t] <- S[i, t-1] + new_lossimmunity[i, t] - new_infections[i, t]
    I[i, t] <- I[i, t-1] + new_infections[i, t] - new_recoveries[i, t]
    R[i, t] <- R[i, t-1] + new_recoveries[i, t] - new_lossimmunity[i, t]
    
    # Simulation de l'étape de propagation dans chaque population
    for (j in 1:num_populations) {
      if (i != j) {
        
        # prob_transmission_from_j <- 1 - exp(-beta_inter * contact_matrix[i, j] * I[j, t-1] / N[i, t-1])
        prob_transmission_from_j <- 1 - exp(-beta_inter * list_of_contact_matrix[[t]][i, j] * I[j, t-1] / N[i, t-1])
        
        infections_from_j <- rbinom(1, S[i, t], prob_transmission_from_j)
        new_infections_from_ext[i, t] <- new_infections_from_ext[i, t] + infections_from_j
        S[i, t] <- S[i, t] - infections_from_j
        I[i, t] <- I[i, t] + infections_from_j
      }
    }
    
    N[i, t] <- S[i, t] + I[i, t] + R[i, t]
  }
}



library(ggplot2)
library(reshape2)

# Convertir la matrice en un data frame pour ggplot2
dynamics_df <- as.data.frame(t(I))
dynamics_df$Time <- 1:ncol(I)

# Réorganiser le data frame en format long
dynamics_long <- melt(dynamics_df, id.vars = "Time", variable.name = "Population", value.name = "Value")

# Tracer les dynamiques avec ggplot2
ggplot(dynamics_long, aes(x = Time, y = Value, group = Population)) +
  geom_line(color = "blue", alpha = 0.3) +
  labs(x = "Temps", y = "Dynamique épidémique", title = "Dynamique épidémique des différentes populations") +
  theme_minimal()


