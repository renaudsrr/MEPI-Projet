library(ggplot2)
library(gganimate)
library(dplyr)
library(maps)
library(sf)
library(animation)
library(gifski)
library(geosphere)
library(gridExtra)
library(cowplot)
library(grid)
library(reshape2)


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


## PAYS ------------------------------------------------------------------
## DYNAMIQUE CARTE
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
  mutate(new_deaths = total_deaths - lag(total_deaths, default = 0))%>%
  # ajout jours julien sur dataset de base pour comparer : 
  mutate("date_julian" = (as.numeric(date))-18281)


# pour intergration lat en long dans espidemy_country
# epidemy_maps_country = left_join(epidemy_country,map_country,by="region")
# 
# # Animation
# gg = ggplot(epidemy_maps_country, aes(long, lat, group = region)) +
#   geom_polygon(aes(fill = new_cases), color = "black") +
# 
#   scale_fill_gradientn(colors = c("blue", "yellow", "red"),
#                        values = scales::rescale(c(0, 10000, max(epidemy_country$total_cases)))) +
# 
#   labs(title = 'Total des Cas aux États-Unis - {frame_time}',
#        fill = 'Total Cases') +
#   geom_text(aes(x = -125, y = 25, label = paste("Total Cases: ", total_cases),
#                 group = 1), size = 6, color = "black", vjust = -1) +
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
# 
# # Sauvegarder l'animation au format GIF
# date = Sys.Date()
# anim_save(paste0(animation_dir, 'evolution_new_cases_country_', date, '.gif'))

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

anim_state = F
if (anim_state == T){
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
  
  ### attention temps d'éxécution long
  
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
  date_time = Sys.time()
  date = format(date_time, "%Y-%m-%d_%H-%M-%S")
  anim_save(paste0(animation_dir, 'evolution_cases_state_', date, '.gif'))
}

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
# map_county = map_data("county")
# map_county = map_county %>% rename("state"="region")
# map_county = map_county %>% rename("county"="subregion")
# map_county$state = tools::toTitleCase(map_county$state) # ajouter maj + sort
# map_county$county = sort(tools::toTitleCase(map_county$county)) # ajouter maj + sort
# 
# # merge pour dynamique à l'échelle des états
# epidemy_county = epidemy %>%
#   group_by(date, county) %>%
#   summarize(cases_by_county = sum(cases, na.rm = TRUE),
#             deaths_by_county = sum(deaths, na.rm = TRUE)) %>%
#   arrange(county, date) %>% 
#   mutate(new_cases_by_county = pmax(cases_by_county - lag(cases_by_county, default = 0), 0)) %>%  # Différence entre jours consécutifs, mais valeurs minimales à 0
#   mutate(new_deaths_by_county = pmax(deaths_by_county - lag(deaths_by_county, default = 0), 0)) 
# # pour intergration lat en long dans espidemy_state
# epidemy_maps_county = left_join(epidemy_county,map_county,by="county")
# 
# # Animation
# gg = ggplot(epidemy_maps_county, aes(long, lat, group = county)) +
#   geom_polygon(aes(fill = new_cases_by_county), color = "black") +  
#   
#   scale_fill_gradientn(colors = c("blue", "yellow", "red"),
#                        values = scales::rescale(c(0, 10000, max(epidemy_country$new_cases_by_county)))) + 
#   
#   labs(title = 'Cas aux États-Unis par county - {frame_time}', 
#        fill = 'Total Cases') +
#   geom_text(data = epidemy_country ,aes(x = -125, y = 25, label = paste("Total Cases: ", total_cases), 
#                                        group = 1), size = 6, color = "black", vjust = -1) +
#   
#   theme_void() +
#   transition_time(date) +  
#   ease_aes('linear')
# 
# nframes = 100  
# animated_plot = animate(gg, 
#                         nframes = nframes, 
#                         fps = 10, 
#                         width = 800, 
#                         height = 600,
#                         renderer = gifski_renderer(),
#                         end_pause = 20,
#                         rewind = FALSE)
# 
# # sauvegarde
# date = Sys.Date()
# anim_save(paste0(animation_dir, 'evolution_cases_county_', date, '.gif'))

### DYNAMIQUE GRAPHS => trop de county

################################################################################
# Creation model 1 POP TPS DISCRET ---------------------------------------------
################################################################################

# simplification dataset par semaines si besoin --------------------------------

epidemy_country_weeks = epidemy_country %>%
  mutate("date_julian" = (as.numeric(date))-18282)%>%
  mutate("weeks" = (date_julian)%/%7 + 1)%>%
  group_by(weeks)%>%
  summarise(
    total_cases = sum(total_cases),
    total_deaths = sum(total_deaths),
    new_cases = sum(new_cases),
    new_deaths = sum(new_deaths))

plot(x=epidemy_country_weeks$weeks,y=epidemy_country_weeks$new_cases)
lines(x=epidemy_country_weeks$weeks,y=epidemy_country_weeks$new_cases,type="l")

plot(x=epidemy_country$date,y=epidemy_country$new_cases)

epidemy_state_weeks = epidemy_state %>%
  mutate("date_julian" = (as.numeric(date))-18282)%>%
  mutate("weeks" = (date_julian)%/%7 + 1)%>%
  group_by(weeks,state)%>%
  summarise(
    cases_by_state = sum(cases_by_state),
    deaths_by_state = sum(deaths_by_state),
    new_cases_by_state = sum(new_cases_by_state),
    new_deaths_by_state = sum(new_deaths_by_state)) %>% 
  arrange(state)

epidemy_country = epidemy_country%>%
  mutate("date_julian" = (as.numeric(date))-18282)

# Creation data obs pourcomparer avec simulation -------------------------------
epidemy_country_obs = epidemy_country%>%
  select(c("date_julian","new_cases","new_deaths"))%>%
  rename("I_E_obs"= "new_cases",
         "D_obs" = "new_deaths",
         "date"= "date_julian")


### MODEL Deterministe ---------------------------------------------------------
################################################################################

SEIRDS_model_det = function(P0, X0, t_max) {
  
  # Compartiment
  S = numeric(t_max)
  E = numeric(t_max)
  I = numeric(t_max)
  R = numeric(t_max)
  D = numeric(t_max)
  N = numeric(t_max)
  
  S[1] = X0[2]
  E[1] = X0[3]
  I[1] = X0[4]
  R[1] = X0[5]
  D[1] = X0[6]
  
  N[1] = X0[1]
  t = 0
  
  beta = P0[1] 
  sigma = P0[2]
  mu = P0[3]
  gamma = P0[4]
  w = P0[5]
    
  # Initialisation historique
  history = data.frame(time = 0, S = S[1], E = E[1], I = I[1], R = R[1], D = D[1], N = N[1])
  
  for (t in 1:t_max) {
    
    new_E = beta * S[t] * I[t] / N[t]      # Transmission
    new_I = sigma * E[t]         # Progression des exposés vers infectieux
    new_R = gamma * I[t]         # Récupération des infectieux
    new_D = mu * I[t]            # Décès parmi les infectieux
    new_S = w * R[t]             # Perte d'immunité vers susceptibles

    # Simulation des transitions
    S[t + 1] = S[t] - new_E + new_S
    E[t + 1] = E[t] + new_E - new_I
    I[t + 1] = I[t] + new_I - new_R - new_D
    R[t + 1] = R[t] + new_R - new_S
    D[t + 1] = D[t] + new_D
    
    N[t + 1] = N[t] - new_D
    
    # Assurer que les populations ne deviennent pas négatives
    S[t + 1] = max(S[t + 1], 0)
    E[t + 1] = max(E[t + 1], 0)
    I[t + 1] = max(I[t + 1], 0)
    R[t + 1] = max(R[t + 1], 0)
    D[t + 1] = max(D[t + 1], 0)
    
    N[t + 1] = max(N[t + 1], 0)
    
    history = rbind(history, data.frame(time = t, S = S[t + 1], E = E[t + 1], I = I[t + 1], R = R[t + 1], D = D[t + 1], N = N[t + 1]))
  }
  
  return(history)
}


### Stochastique ---------------------------------------------------------------
################################################################################

# Fonction de simulation du modèle SIR stochastique à temps discret
SEIRDS_model_binomial = function(P0, X0, t_max) {
  
  # Compartiment
  S = numeric(t_max)
  E = numeric(t_max)
  I = numeric(t_max)
  R = numeric(t_max)
  D = numeric(t_max)
  N = numeric(t_max)
  
  S[1] = X0[2]
  E[1] = X0[3]
  I[1] = X0[4]
  R[1] = X0[5]
  D[1] = X0[6]
  
  N[1] = X0[1]
  t = 0
  
  beta = P0[1] 
  sigma = P0[2]
  mu = P0[3]
  gamma = P0[4]
  w = P0[5]
  
  # Initialisation historique
  history = data.frame(time = 0, S = S[1], E = E[1], I = I[1], R = R[1], D = D[1], N = N[1])
  
  # Boucle de simulation
  for (t in 1:t_max) {
    
    # Calcul des probabilités de transition
    p_beta = 1 - exp(-beta * I[t]/N[t]) 
    p_sigma = 1 - exp(-sigma)
    p_gamma = 1 - exp(-gamma)
    p_w = 1 - exp(-w) 
    p_mu = 1 - exp(-mu)
    
    # Simulation des transitions
    new_E = rbinom(1, S[t], p_beta)
    new_I = rbinom(1, E[t], p_sigma)
    new_R = rbinom(1, I[t], p_gamma)
    new_D = rbinom(1, I[t], p_mu)
    new_S = rbinom(1, R[t], p_w)
    
    # Simulation des transitions
    S[t + 1] = S[t] - new_E + new_S
    E[t + 1] = E[t] + new_E - new_I
    I[t + 1] = I[t] + new_I - new_R - new_D
    R[t + 1] = R[t] + new_R - new_S
    D[t + 1] = D[t] + new_D
    
    N[t + 1] = N[t] - new_D
    
    # Assurer que les populations ne deviennent pas négatives
    S[t + 1] = max(S[t + 1], 0)
    E[t + 1] = max(E[t + 1], 0)
    I[t + 1] = max(I[t + 1], 0)
    R[t + 1] = max(R[t + 1], 0)
    D[t + 1] = max(D[t + 1], 0)
    
    N[t + 1] = max(N[t + 1], 0)
    
    history = rbind(history, data.frame(time = t, S = S[t + 1], E = E[t + 1], I = I[t + 1], R = R[t + 1], D = D[t + 1], N = N[t + 1]))
  }
  
  return(history)
}

# optimisation paramètres deterministe => Vraissemblance -----------------------
################################################################################

# paramètres bibliographie -----------------------------------------------------

# model SEIRS covid estimation params (biblio) : 
# R0 = 2.5 − 3.5

# taux_transmission => beta = 0.35−0.7
# tps_incubation => 1/sigma = 4 − 6 jours => 0.17 − 0.25
# taux_mortalite => mu = 0.1 − 0.2
# taux_immunisation => gamma = 0.143 − 0.2
# tps_réinfection => 1/w = ...  => 0.00018 − 0.00055 

# Paramètres à otpimiser -------------------------------------------------------
N = 330000000        # Population totale
I0 = 10              # Nombre initial d'infectés
S0 = N - I0          # Nombre initial de susceptibles
E0 = 100             # Nombre initial d'exposés
R0 = 0               # Nombre initial de récupérés
D0 = 0               # Nombre initial de décédés
X0 = c(N=N,S0=S0,E0=E0,I0=I0,R0=R0,D0=D0) # param a estimer

beta = 0.3   # Taux de transmission
sigma = 0.3  # Taux d'incubation (1 / durée d'incubation)
mu = 0.005    # Taux de mortalité
gamma = 0.1  # Taux de immunité 
w = 0.2  # Taux de perte d'immunité (1 / durée d'infection)
P0 = c(beta = beta,sigma = sigma,mu = mu,gamma = gamma,w = w) # param a estimer

t_max = 844

# Maximisation Likehood --------------------------------------------------------
log_likelihood = function(X0,P0,t_max,epidemy_country_obs) {
  
  S0 = X0[2]
  E0 = X0[3]
  I0 = X0[4]
  R0 = X0[5]
  D0 = X0[6]
  
  I_E_obs = epidemy_country_obs$I_E_obs
  D_obs = epidemy_country_obs$D_obs
  
  beta = P0[1]
  sigma = P0[2]
  mu = P0[3]
  gamma = P0[4]
  w = P0[5]
  
  model_results = SEIRDS_model_det(P0, X0, t_max)
  model_results = model_results[-845,]
  
  I_E_sim = pmax(model_results$I,1e-10)
  D_sim = pmax(model_results$D,1e-10)
  
  # qq vérifs
  sum(is.na(I_E_sim)) 
  sum(is.na(D_sim))
  sum(I_E_sim<0)
  sum(D_sim<0)
  
  LLC = dpois(I_E_obs, I_E_sim, log = TRUE)
  LLD = dpois(D_obs, D_sim, log = TRUE)
  
  LLC[is.infinite(LLC)] = -1e10
  LLD[is.infinite(LLD)] = -1e10
  
  # print(LLC)
  # print(LLD)
  
  LLC = sum(LLC) # Likehood pour cas (Poisson)
  LLD = sum(LLD)  # Likehood pour deaths (Poisson)
  LL =  LLC + LLD     # Likehood combinée
  print(-LL)
  
  return(-LL)  # négatif pour minimisation
}

# theta0 = c(P0) 

# boucle optimisation 
optimisation_model = F
if (optimisation_model){
result_optim = optim(par = P0,
                     fn = log_likelihood,
                     X0 = X0,
                     t_max = t_max,
                     epidemy_country_obs = epidemy_country_obs,
                     method = "L-BFGS-B", 
                     lower = c(0, 0, 0, 0, 0), 
                     upper = c(1, 1, 1, 1, 1))

# Résultats des paramètres optimisés
optimized_params = result_optim$par
print(optimized_params)
}
# [...] a refaire en partie car problème avec -Inf dans liste vraissemblance

# optimisation paramètres stochastique -----------------------------------------
################################################################################

# Méthode ABC ou autre ---------------------------------------------------------
# [...] à faire en méthode sans vraissemblance


################################################################################
# éxécution simulation ---------------------------------------------------------
################################################################################

# Paramètres initiaux
N = 330000000        # Population totale
I0 = 0              # Nombre initial d'infectés
S0 = N - I0          # Nombre initial de susceptibles
E0 = 1             # Nombre initial d'exposés
R0 = 0               # Nombre initial de récupérés
D0 = 0               # Nombre initial de décédés
X0 = c(N=N,S0=S0,E0=E0,I0=I0,R0=R0,D0=D0) # param a estimer

beta = 0.3   # Taux de transmission
sigma = 0.3  # Taux d'incubation (1 / durée d'incubation)
mu = 0.009    # Taux de mortalité
gamma = 0.05  # Taux de immunité 
w = 0.1  # Taux de perte d'immunité (1 / durée d'infection)
P0 = c(beta = beta,sigma = sigma,mu = mu,gamma = gamma,w = w) # param a estimer

# Paramètre optimisés pour model det
# sigma = 0.4245536708,
# beta = 0.2081456769,
# gamma = 0.2911411862,
# w = 0.0933803593,
# mu = 0.0008998318

# Paramètre optimisés pour model stoch
# sigma = 
# beta = 
# gamma = 
# w = 
# mu = 

t_max = 844 # Nombre de jours pour la simulation

# Deterministe -----------------------------------------------------------------
set.seed(123) 
history_SEIRD_model_det = SEIRDS_model_det(P0, X0, t_max)
history_SEIRD_model_det = history_SEIRD_model_det[-845,]

history_melt_SEIRD_det = melt(history_SEIRD_model_det, 
                              id.vars = "time", 
                              variable.name = "Compartment", 
                              value.name = "Count")%>%
  filter(Compartment!="N")

# vérif
# plot(history_SEIRD_model_det$S~history_SEIRD_model_det$time)
# plot(history_SEIRD_model_det$E~history_SEIRD_model_det$time)
# plot(history_SEIRD_model_det$I~history_SEIRD_model_det$time)
# plot(history_SEIRD_model_det$R~history_SEIRD_model_det$time)
# plot(history_SEIRD_model_det$D~history_SEIRD_model_det$time)

ggplot(history_melt_SEIRD_det, aes(x = time, y = Count, color = Compartment)) +
  geom_step() +
  labs(title = "Modèle SEIRS Deterministe à Temps Discret",
       x = "Temps",
       y = "Nombre d'Individus") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red", "green","orange","black"))

# pour comparaison avec epidemy_country
epidemy_country_sim_det = history_SEIRD_model_det%>%
  select("time","I","E","D")%>%
  mutate("I_E_sim" = I+E)%>%
  mutate("D_sim" = pmax(D - lag(D, default = 0), 0)) %>% # équivalent de new_death
  rename("date"="time")%>%
  select(-c("I","E","D"))


# Stochastique -----------------------------------------------------------------
set.seed(123) 
history_SEIRD_binomial = SEIRDS_model_binomial(P0, X0, t_max)

history_melt_SEIRD_binomial = melt(history_SEIRD_binomial, 
                                   id.vars = "time", 
                                   variable.name = "Compartment", 
                                   value.name = "Count")%>%
  filter(Compartment!="N")

# vérif
# plot(history_SEIRD_binomial$S~history_SEIRD_binomial$time)
# plot(history_SEIRD_binomial$E~history_SEIRD_binomial$time)
# plot(history_SEIRD_binomial$I~history_SEIRD_binomial$time)
# plot(history_SEIRD_binomial$R~history_SEIRD_binomial$time)
# plot(history_SEIRD_binomial$D~history_SEIRD_binomial$time)

ggplot(history_melt_SEIRD_binomial, aes(x = time, y = Count, color = Compartment)) +
  geom_step() +
  labs(title = "Modèle SEIRS Stochastique à Temps Discret",
       x = "Temps",
       y = "Nombre d'Individus") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red", "green","orange","black"))

# pour comparaison avec epidemy_country
epidemy_country_sim_stoch = history_SEIRD_binomial%>%
  select("time","I","E","D")%>%
  mutate("I_E_sim" = I+E)%>%
  mutate("D_sim" = pmax(D - lag(D, default = 0), 0)) %>% # équivalent de new_death
  rename("date"="time")%>%
  select(-c("I","E","D"))

# boucle stochastique 
nb_simu = 10
list_results_I_E_sim = list()
list_result_D_sim = list()
df_stoch_E_I_sim = data.frame("date"=seq(0:844))
df_stoch_D_sim = data.frame("date"=seq(0:844))

for (i in 1:nb_simu){
  set.seed(sample(1:1300, 1))
  history_SEIRD_binomial = SEIRDS_model_binomial(P0, X0, t_max)
  epidemy_country_sim_stoch = history_SEIRD_binomial%>%
    select("time","I","E","D")%>%
    mutate("I_E_sim" = I+E)%>%
    mutate("D_sim" = pmax(D - lag(D, default = 0), 0)) %>% # équivalent de new_death
    rename("date"="time")%>%
    select(-c("I","E","D"))
  
  list_results_I_E_sim[[i]] = epidemy_country_sim_stoch$I_E_sim
  list_result_D_sim[[i]] = epidemy_country_sim_stoch$D_sim
  
  df_stoch_E_I_sim[i+1] = list_results_I_E_sim[[i]]
  df_stoch_D_sim[i+1] = list_result_D_sim[[i]]
}

gg = ggplot()+
  geom_line(data=df_stoch_E_I_sim, aes(x = date, y = V2),col="red",alpha=0.2)+
  geom_line(data=df_stoch_E_I_sim, aes(x = date, y = V3),col="red",alpha=0.2)+
  geom_line(data=df_stoch_E_I_sim, aes(x = date, y = V4),col="red",alpha=0.2)+
  geom_line(data=df_stoch_E_I_sim, aes(x = date, y = V5),col="red",alpha=0.2)+
  geom_line(data=df_stoch_E_I_sim, aes(x = date, y = V6),col="red",alpha=0.2)+
  geom_line(data=df_stoch_E_I_sim, aes(x = date, y = V7),col="red",alpha=0.2)+
  geom_line(data=df_stoch_E_I_sim, aes(x = date, y = V8),col="red",alpha=0.2)+
  geom_line(data=df_stoch_E_I_sim, aes(x = date, y = V9),col="red",alpha=0.2)+
  geom_line(data=df_stoch_E_I_sim, aes(x = date, y = V10),col="red",alpha=0.2)+
  geom_line(data=df_stoch_E_I_sim, aes(x = date, y = V11),col="red",alpha=0.2)
plot(gg)

################################################################################
# représentation graphique combinée --------------------------------------------
################################################################################

gg = ggplot()+
  scale_y_log10()+
  geom_point(data=epidemy_country_obs, aes(x = date, y = I_E_obs),col="black")+
  
  geom_point(data=df_stoch_E_I_sim, aes(x = date, y = V2),col="red",alpha=0.5)+
  geom_point(data=df_stoch_E_I_sim, aes(x = date, y = V3),col="red",alpha=0.5)+
  geom_point(data=df_stoch_E_I_sim, aes(x = date, y = V4),col="red",alpha=0.5)+
  geom_point(data=df_stoch_E_I_sim, aes(x = date, y = V5),col="red",alpha=0.5)+
  geom_point(data=df_stoch_E_I_sim, aes(x = date, y = V6),col="red",alpha=0.5)+
  geom_point(data=df_stoch_E_I_sim, aes(x = date, y = V7),col="red",alpha=0.5)+
  geom_point(data=df_stoch_E_I_sim, aes(x = date, y = V8),col="red",alpha=0.5)+
  geom_point(data=df_stoch_E_I_sim, aes(x = date, y = V9),col="red",alpha=0.5)+
  geom_point(data=df_stoch_E_I_sim, aes(x = date, y = V10),col="red",alpha=0.5)+
  geom_point(data=df_stoch_E_I_sim, aes(x = date, y = V11),col="red",alpha=0.5)+
  
  geom_point(data=epidemy_country_sim_det, aes(x = date, y = I_E_sim),col="blue")
  
plot(gg)
  


################################################################################
# MODEL METAPOP TPS DISCRET
################################################################################

### Traitement data ------------------------------------------------------------
state_names = unique(epidemy_state$state)
nb_state = length(state_names)
print(state_names)
print(nb_state)
t_max = 844

# https://www.kaggle.com/datasets/adnananam/usa-statewise-population-2020?resource=download
pop_size_data = read.csv("usa_statewise_population_2020.csv",h=T)

length(unique(pop_size_data$State))
setdiff(unique(pop_size_data$State),state_names)

pop_size = pop_size_data %>% 
  select(c("State","Population"))%>%
  filter(!(State %in% c("Alaska", "Hawaii", "Puerto Rico")))

vect_pop_size = c(pop_size$Population)

gg = ggplot(data = pop_size, aes(x = State, y = Population)) +
  geom_bar(stat = "identity") + 
  labs(title = "Population par État en 2020",
       x = "État",
       y = "Population")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels d'axe X
plot(gg)


### Deterministe ---------------------------------------------------------------
################################################################################

SEIRDS_model_metapop_det = function(X0, P0, contact_matrix, t_max){
    
  # Initialisation des vecteurs pour stocker les résultats pour chaque population
  S = matrix(0, nrow = nb_state, ncol = t_max)
  E = matrix(0, nrow = nb_state, ncol = t_max)
  I = matrix(0, nrow = nb_state, ncol = t_max)
  R = matrix(0, nrow = nb_state, ncol = t_max)
  D = matrix(0, nrow = nb_state, ncol = t_max)
  N = matrix(0, nrow = nb_state, ncol = t_max)
  
  beta = P0[1]
  beta_inter = P0[2]
  sigma = P0[3]
  mu = P0[4]
  gamma = P0[5]
  w = P0[6]
  
  new_E = matrix(0, nrow = nb_state, ncol = t_max)
  new_I = matrix(0, nrow = nb_state, ncol = t_max)
  new_I_from_ext = matrix(0, nrow = nb_state, ncol = t_max)
  new_R = matrix(0, nrow = nb_state, ncol = t_max)
  new_D = matrix(0, nrow = nb_state, ncol = t_max)
  new_S = matrix(0, nrow = nb_state, ncol = t_max)
  
  history = list(S = matrix(0, nrow = nb_state, ncol = t_max), 
                 E = matrix(0, nrow = nb_state, ncol = t_max), 
                 I = matrix(0, nrow = nb_state, ncol = t_max), 
                 R = matrix(0, nrow = nb_state, ncol = t_max), 
                 D = matrix(0, nrow = nb_state, ncol = t_max))
  
  # Initialisation des conditions initiales pour chaque population
  for (i in 1:nb_state) {
    S[i, 1] = vect_pop_size[i]  # Une personne infectée au départ
    E[i, 1] = 0
    I[i, 1] = 0
    R[i, 1] = 0
    D[i, 1] = 0
    N[i, 1] = S[i, 1] + I[i, 1] + E[i, 1] + R[i, 1] - D[i, 1]
  }
  
  # infection d'une population aléatoirement
  pop_id = sample(1:nb_state, 1)
  S[pop_id, 1] = vect_pop_size[pop_id] - X0[1]  # Une personne infectée au départ
  I[pop_id, 1] = X0[1]
  
  history$S[, 1] = S[, 1]
  history$E[, 1] = E[, 1]
  history$I[, 1] = I[, 1]
  history$R[, 1] = R[, 1]
  history$D[, 1] = D[, 1]
  
  # Simulation de l'épidémie dans la métapopulation
  for (t in 2:t_max){
    
    print(paste0("Generation tps : ", t))
    
    for (i in 1:nb_state) {
      
      # Dynamique dans la population i
      # Calcul des probabilités de transition
      prob_beta = 1 - exp(-beta* I[i, t-1]/N[i,t-1])
      prob_sigma = 1 - exp(-sigma)
      prob_gamma = 1 - exp(-gamma)
      prob_w = 1 - exp(-w) 
      prob_mu = 1 - exp(-mu)
      
      # Mise à jour des compartiments S, E, I, R et D
      new_E[i, t] = rbinom(1, S[i, t-1], prob_beta)
      new_I[i, t] = rbinom(1, E[i, t-1], prob_sigma)
      new_R[i, t] = rbinom(1, I[i, t-1], prob_gamma)
      new_D[i, t] = rbinom(1, I[i, t-1], prob_mu)
      new_S[i, t] = rbinom(1, R[i, t-1], prob_w)
      
      # Mettre à jour les compartiments S, E, I, R et D dans la population i
      # + S'assurer de valeurs positives avec max
      S[i, t] = max(0, S[i, t-1] - new_E[i, t] + new_S[i, t])
      E[i, t] = max(0, E[i, t-1] + new_E[i, t] - new_I[i, t])
      I[i, t] = max(0, I[i, t-1] + new_I[i, t] - new_R[i, t] - new_D[i, t])
      R[i, t] = max(0, R[i, t-1] + new_R[i, t] - new_S[i, t])
      D[i, t] = max(0, D[i, t-1] + new_D[i, t])
      
      # Simulation de l'étape de propagation dans chaque population
      for (j in 1:nb_state) {
        if (i != j) {
          
          prob_transmission_from_j = 1 - exp(-beta_inter * contact_matrix[i, j] * I[j, t-1] / N[i, t-1])
          infections_from_j = rbinom(1, S[i, t], prob_transmission_from_j)

          new_I_from_ext[i, t] = new_I_from_ext[i, t] + infections_from_j
          
          S[i, t] = S[i, t] - infections_from_j
          E[i, t] = E[i, t] + infections_from_j
        }
      }
      
      N[i, t] = S[i, t] + E[i, t] + I[i, t] + R[i, t] - D[i, t]
      
    }
    
    history$S[, t] = S[, t]
    history$E[, t] = E[, t]
    history$I[, t] = I[, t]
    history$R[, t] = R[, t]
    history$D[, t] = D[, t]
  }
  return(history)
}

# Stochastique------------------------------------------------------------------
################################################################################


### [...] à faire

################################################################################
# éxécution simulation ---------------------------------------------------------
################################################################################

# Paramètres initiaux
E0 = 1             # Nombre initial d'exposés
X0 = c(E0=E0) # param a estimer

beta = 0.03   # Taux de transmission intrapop
beta_inter = 0.01 # taux de transmission interpop <================== aussi sous forme de matrice !!!!!!!
sigma = 0.3  # Taux d'incubation (1 / durée d'incubation)
mu = 0.0001    # Taux de mortalité
gamma = 0.05  # Taux de immunité 
w = 0.1  # Taux de perte d'immunité (1 / durée d'infection)
P0 = c(beta = beta, beta_inter = beta_inter, sigma = sigma,mu = mu,gamma = gamma,w = w) # param a estimer

# Creation matrice de contact --------------------------------------------------
contact_matrix = matrix(0.3, nrow = nb_state, ncol = nb_state)  # Taux de migration entre population


# Deterministe -------------------------------------------------------------------

history_metapop_det = SEIRDS_model_metapop_det(X0, P0, contact_matrix, t_max)

history_metapop_det$S[is.na(history_metapop_det$S)] = 0
history_metapop_det$E[is.na(history_metapop_det$E)] = 0
history_metapop_det$I[is.na(history_metapop_det$I)] = 0
history_metapop_det$R[is.na(history_metapop_det$R)] = 0
history_metapop_det$D[is.na(history_metapop_det$D)] = 0




# graphiques ---------------------------------------------------------------------




# Test avec Alabama
pop = 40
S_test = history_metapop_det$S[pop,]
E_test = history_metapop_det$E[pop,]
I_test = history_metapop_det$I[pop,]
R_test = history_metapop_det$R[pop,]
D_test = history_metapop_det$D[pop,]
df_pop_unique = data.frame(S=S_test,E=E_test,I=I_test,R=R_test,D=D_test)

date_time = Sys.time()
date = format(date_time, "%Y-%m-%d_%H-%M-%S")

graph_dir = "C:/Users/User/Desktop/MASTER/M2/MEPI/PROJET_MEPI/graphs/"
png(paste0(graph_dir,"graphique_pop_",pop,"__",date,".png"), width = 2000, height = 2000, res = 150)
plot(x = 1:t_max, y = S_test,col="green",type="l",ylim=c(0,S_test[1]))
lines(x = 1:t_max, y = E_test,col="orange")
lines(x = 1:t_max, y = I_test,col="red")
lines(x = 1:t_max, y = R_test,col="blue")
lines(x = 1:t_max, y = D_test,col="black")
dev.off()

png(paste0(graph_dir,"graphique_metapop_",date,".png"), width = 2000, height = 2000, res = 150)
# Tracé des résultats pour chaque population
par(mfrow = c(7, 7))
for (i in 1:nb_state) {
  plot(1:t_max, history_metapop_det$S[i, ], 
       type = "l", 
       col = "blue", 
       xlab = "Temps", 
       ylab = "Susceptibles", 
       ylim = c(0, history_metapop_det$S[i,1]))
  lines(1:t_max, history_metapop_det$E[i, ], col = "skyblue")
  lines(1:t_max, history_metapop_det$I[i, ], col = "red")
  lines(1:t_max, history_metapop_det$R[i, ], col = "green")
  lines(1:t_max, history_metapop_det$D[i, ], col = "coral")
  # lines(1:t_max, new_I_from_ext[i, ], col = "yellow")
  title(paste("Population", i))
}

dev.off()

# Stochstique -------------------------------------------------------------------




