library(ggplot2)
library(gganimate)
library(dplyr)
library(maps)
library(sf)
library(gifski)
library(geosphere)
library(grid)
library(reshape2)
library(plotly)
library(ade4)
library(lubridate)
library(BRREWABC) # devtools::install_github("GaelBn/BRREWABC")
library(abcrf)

rm(list=ls()) 
graphics.off() 
cat("\014")

setwd("C:/Users/User/Desktop/MASTER/M2/MEPI/PROJET_MEPI/VF")
animation_dir = "C:/Users/User/Desktop/MASTER/M2/MEPI/PROJET_MEPI/animations/"
graph_dir = "C:/Users/User/Desktop/MASTER/M2/MEPI/PROJET_MEPI/graphs/"

epidemy = read.csv("us-counties.csv", h = T)
pop_size_data = read.csv("usa_statewise_population_2020.csv",h=T) # https://www.kaggle.com/datasets/adnananam/usa-statewise-population-2020?resource=download

###########################################################################
# Traitement data ---------------------------------------------------------
###########################################################################
epidemy = na.omit(epidemy) # enlever lignes avec NA
epidemy$date = as.Date(epidemy$date, format = "%Y-%m-%d") # en format date

# enlever états en dehors map (outre mer + alaska)
map_state = map_data("state")
# length(unique(epidemy$state))
# length(unique(map_state$region))
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
gg_usa = ggplot(map_state, aes(long, lat, group = group)) +
  geom_polygon(fill = "grey", color = "black") +
  coord_fixed(1.3) +
  geom_text(data = state_centroids, aes(x = centroids_long, y = centroids_lat, label = state), 
            size = 2.9, color = "black") +   
  theme_void() # theme_minimal()
plot(gg_usa)

###########################################################################
# Creations datasets country et state  ------------------------------------
###########################################################################

# simplification à l'échelle du pays ou des états 
# PAYS
epidemy_country = epidemy %>%
  group_by(date) %>%
  summarize(total_cases = sum(cases, na.rm = TRUE),
            total_deaths = sum(deaths, na.rm = TRUE))%>%
  arrange(date) %>%
  mutate(new_cases = pmax(total_cases - lag(total_cases, default = 0),0)) %>% # Différence entre jours consécutifs
  mutate(new_deaths = pmax(total_deaths - lag(total_deaths, default = 0),0))%>%
  mutate("date_julian" = (as.numeric(date))-18281) %>% # ajout jours julien sur dataset de base pour comparer avec simulations 
  mutate(weeks = (date_julian) %/% 7 + 1)
  
epidemy_country_weeks = epidemy_country %>%
  group_by(weeks) %>%
  summarise(
    total_cases = last(total_cases),   # Prendre le cumul au dernier jour de la semaine
    total_deaths = last(total_deaths), 
    new_cases = sum(new_cases),       
    new_deaths = sum(new_deaths))

# STATE
epidemy_state = epidemy %>%
  group_by(state,date) %>%
  summarize(cases_by_state = sum(cases, na.rm = TRUE),
            deaths_by_state = sum(deaths, na.rm = TRUE))%>%
  arrange(state,date) %>%
  mutate(new_cases_by_state = pmax(cases_by_state - lag(cases_by_state, default = 0), 0)) %>% 
  mutate(new_deaths_by_state = pmax(deaths_by_state - lag(deaths_by_state, default = 0), 0))%>%
  mutate("date_julian" = (as.numeric(date))-18281) %>% 
  mutate(weeks = (date_julian) %/% 7 + 1)

epidemy_state_weeks = epidemy_state %>%
  arrange(state, weeks) %>%
  group_by(state, weeks) %>%
  summarise(
    cases_by_state = last(cases_by_state),  
    deaths_by_state = last(deaths_by_state), 
    new_cases_by_state = sum(new_cases_by_state),       
    new_deaths_by_state = sum(new_deaths_by_state)
  )


# qq vérifs
sum(is.na(epidemy_country))
sum(is.na(epidemy_country_weeks))
sum(is.na(epidemy_state_weeks))
sum(is.na(epidemy_state_weeks))

# sum(epidemy_country$total_cases<0)
# sum(epidemy_country$total_deaths<0)
# sum(epidemy_country$new_cases<0)
# sum(epidemy_country$new_deaths<0)
# which(epidemy_country$new_cases < 0)
# which(epidemy_country$new_deaths < 0)
# epidemy_country[501,]
# epidemy_country[784,]

sum(epidemy_country_weeks<0)
sum(epidemy_state_weeks<0)
sum(epidemy_state_weeks<0)

plot(x = epidemy_country$weeks, y=epidemy_country$total_cases,type='l',log="y")
lines(x = epidemy_country_weeks$weeks, y=epidemy_country_weeks$total_cases,col='red',log="y")

plot(x = epidemy_state$weeks[epidemy_state$state == "California"], 
     y= epidemy_state$cases_by_state[epidemy_state$state == "California"],type='l')
lines(x = epidemy_state_weeks$weeks[epidemy_state_weeks$state == "California"], 
      y= epidemy_state_weeks$cases_by_state[epidemy_state_weeks$state == "California"],col="red")

# Creation data obs pourcomparer avec simulation -------------------------------
epidemy_country_weeks_obs = epidemy_country_weeks%>%
  select(c("weeks","new_cases","new_deaths"))%>%
  rename("I_obs"= "new_cases",
         "D_obs" = "new_deaths")

epidemy_state_weeks_obs = epidemy_state_weeks%>%
  select(c("weeks","state","new_cases_by_state","new_deaths_by_state"))%>%
  rename("I_obs"= "new_cases_by_state",
         "D_obs" = "new_deaths_by_state")

###########################################################################
# Exploration dynamiques --------------------------------------------------
###########################################################################

## 1. PAYS ------------------------------------------------------------------
### 1.1 DYNAMIQUE GRAPHS
gg = ggplot(data = epidemy_country, aes(x = date)) +
  geom_point(aes(y = total_cases, col = "Total Cases")) +
  geom_point(aes(y = new_cases, col = "New Cases")) +
  geom_point(aes(y = total_deaths, col = "Total Deaths")) +
  geom_point(aes(y = new_deaths, col = "New Deaths")) +
  
  scale_color_manual(values = c("Total Cases" = "blue",
                                "New Cases" = "skyblue",
                                "Total Deaths" = "red",
                                "New Deaths" = "orange")) +
  
  # # Vaccination
  # geom_vline(xintercept = as.Date("2021-11-30"), color = "black", linetype = "dashed", linewidth = 1) + # première vaccination
  # geom_vline(xintercept = as.Date("2021-08-23"), color = "black", linetype = "dashed", linewidth = 1) + # troisième vaccination
  # 
  # Variants
  geom_vline(xintercept = as.Date("2021-03-22"), color = "black", linewidth = 1) + # var init 
  geom_vline(xintercept = as.Date("2021-04-19"), color = "black", linewidth = 1) + # var ad
  geom_vline(xintercept = as.Date("2021-11-22"), color = "black", linewidth = 1) + # var omic
  
  labs(x = "Date", 
       y = "Nombre de cas et de décès (log)",
       colour = "")+
  scale_y_log10()+
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.text = element_text(size=20)
  ) 

plot(gg)



### 2. ETAT -----------------------------------------------------------------------
### 2. DYNAMIQUE CARTE

anim_state = F   ### attention temps d'éxécution long
if (anim_state == T){
  map_state = map_data("state")
  map_state = map_state %>% select(-6) %>% rename("state"="region")
  map_state$state = sort(tools::toTitleCase(map_state$state)) # ajouter maj + sort
  
  # pour intergration lat en long dans espidemy_state
  epidemy_maps_state = left_join(epidemy_state,map_state,by = "state")

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
state_names = unique(epidemy_state_weeks_obs$state)
nb_state = length(state_names)
pop_size = pop_size_data %>% 
  select(c("State","Population"))%>%
  filter(!(State %in% c("Alaska", "Hawaii", "Puerto Rico")))

vect_pop_size = c(pop_size$Population)
facteur_retrecissement_pop = 9
vect_pop_size = vect_pop_size%/%facteur_retrecissement_pop

# Proportion infectés
final_cases_by_state = epidemy_state_weeks_obs %>%
  group_by(state) %>%
  summarise(total_cases = sum(I_obs, na.rm = TRUE))%>%
  left_join(pop_size, by = c("state" = "State")) %>%
  mutate(percentage_infected = (total_cases / Population) * 100) 
# habitants
gg = ggplot(data = final_cases_by_state, aes(x = state)) +
  geom_bar(stat = "identity", aes(y = Population, fill = "Population totale")) + 
  geom_bar(stat = "identity", aes(y = total_cases, fill = "Cas totaux"), alpha = 0.8) +
  scale_fill_manual(values = c("Population totale" = "#4F4F4F", "Cas totaux" = "red")) +
  labs(title = "Population par État en 2020",
       x = "État",
       y = "Population",
       fill = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 20),
        legend.text = element_text(size=20))
plot(gg)


# Cas
fig_cas_obs = plot_ly()
for (state in unique(epidemy_state_weeks_obs$state)) {
  obs_data = epidemy_state_weeks_obs[epidemy_state_weeks_obs$state == state, ]
  fig_cas_obs = fig_cas_obs %>%
    add_trace(
      x = obs_data$weeks,  # Temps
      y = rep(state, nrow(obs_data)),  # State
      z = obs_data$I_obs,  # I observé
      type = 'scatter3d',
      mode = 'lines',  
      line = list(color = 'black', width = 3),
      name = paste0("Observé - ", state))}
# Ajouter le layout
fig_cas_obs = fig_cas_obs %>% layout(
  title = "Individus infectés par State",
  scene = list(
    xaxis = list(title = "Temps"),
    yaxis = list(
      title = "State",
      tickvals = 1:nb_state, 
      ticktext = state_names[state],  
      tickfont = list(size = 10)),
    zaxis = list(title = "Nombre d'individus infectés")))
fig_cas_obs

# Deaths
fig_deaths_obs = plot_ly()
for (state in unique(epidemy_state_weeks_obs$state)) {
  obs_data = epidemy_state_weeks_obs[epidemy_state_weeks_obs$state == state, ]
  fig_deaths_obs = fig_deaths_obs %>%
    add_trace(
      x = obs_data$weeks,  # Temps
      y = rep(state, nrow(obs_data)),  # State
      z = obs_data$D_obs,  # I + E observé
      type = 'scatter3d',
      mode = 'lines',  
      line = list(color = 'black', width = 3),
      name = paste0("Observé - ", state))}
# Ajouter le layout
fig_deaths_obs = fig_deaths_obs %>% layout(
  title = "Individus infectés par State",
  scene = list(
    xaxis = list(title = "Temps"),
    yaxis = list(
      title = "State",
      tickvals = 1:nb_state, 
      ticktext = state_names[state],  
      tickfont = list(size = 10)),
    zaxis = list(title = "Nombre d'individus infectés")))
fig_deaths_obs


# nb de mort anormal en Floride : 
which(epidemy_state_weeks_obs$D_obs > 10000)
epidemy_state_weeks_obs[1011,] 

################################################################################
# Creation model 1 POP TPS DISCRET ---------------------------------------------
################################################################################

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
    
    new_E = beta * S[t] * I[t]/N[t]     # Transmission
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

# Maximisation Likehood => ne marche pas bien dans ce cas ----------------------
log_likelihood = function(X0,P0,t_max,data_obs,model) {
  
  S0 = X0[2]
  E0 = X0[3]
  I0 = X0[4]
  R0 = X0[5]
  D0 = X0[6]
  
  I_obs = data_obs$I_obs
  # D_obs = data_obs$D_obs
  
  beta = P0[1]
  sigma = P0[2]
  mu = P0[3]
  gamma = P0[4]
  w = P0[5]
  
  model_results = model(P0, X0, t_max)

  I_sim = model_results$I
  I_sim[I_sim<0] = 0
  # D_sim = model_results$D
  
  epsilon = 1e-10  # Petite valeur pour éviter le log(0)
  
  LLC = dpois(I_obs, I_sim + epsilon, log = TRUE)
  # LLC = dpois(I_obs, lambda = I_sim + epsilon, log = TRUE)
  # LLC = dnorm(log(I_obs + epsilon), log(I_sim + epsilon), sd = 1, log = TRUE)
  
  # LLD = dpois(D_obs, D_sim, log = TRUE)
  
  LLC = sum(LLC) # Likehood pour cas (Poisson)
  # LLD = sum(LLD)  # Likehood pour deaths (Poisson)
  # LL =  LLC + LLD     # Likehood combinée
  # print(-LLC)
  # print(P0)
  
  return(-LLC)  # négatif pour minimisation
}


################################################################################
# éxécution simulation UNE POP / 1 PIC -----------------------------------------
################################################################################

# for (i in state_names){
#   one_state_test = i
#   I_E_one_state = epidemy_state_weeks_obs$I_obs[epidemy_state_weeks_obs$state==one_state_test]
#   weeks = seq(1:length(I_E_one_state))
#   df_one_state = data.frame(weeks, I_E_one_state)
#   
#   plot(x=df_one_state$weeks,y=df_one_state$I_E_one_state,main=paste0(i),type="l")}

# uniquement la première grosse vague
one_state_test = "California"
I_obs = epidemy_state_weeks_obs$I_obs[epidemy_state_weeks_obs$state==one_state_test]
weeks = seq(1:length(I_obs))
df_one_state = data.frame(weeks, I_obs)%>%
  slice(40:60)%>%
  mutate(weeks = seq(1:21))

plot(x=df_one_state$weeks,y=df_one_state$I_obs,main=one_state_test,type="l")
write.csv(df_one_state, file = "df_one_state.csv", row.names = FALSE)

# beta => Taux de transmission
# 1/sigma =>  Temps de latence *7 car weeks => plus on diminue ce param et plus la latence est longue 
# mu => Taux de mortalité
# gamma => Taux d'immunité 
# 1/w => Taux de perte d'immunité => plus on diminue ce param et plus le temps de retour à l'état sucesptibe sera long 

# opimisation vraisemblance
optimisation_model_LH = F
nb_LH = 1

if (optimisation_model_LH){
  for (i in 1:nb_LH){
    
    N = 39346023   
    I0 = 1000         
    S0 = N - I0     
    E0 = 0       
    R0 = 0          
    D0 = 0          
    X0 = c(N=N,S0=S0,E0=E0,I0=I0,R0=R0,D0=D0)
    
    beta = 2.49730767 # 0.1*sample(size = 1,1:10)
    sigma = 0.83501694 # 0.05*sample(size = 1,1:10)
    mu = 0.01023731 # 0.003*sample(size = 1,1:10)
    gamma = 0.95295171 # 0.08*sample(size = 1,1:10)
    w = 0.02097567 # 0.1*sample(size = 1,1:10)
    P0 = c(beta = beta,sigma = sigma,mu=mu,gamma = gamma,w=w)
    
    R_0 = beta/(mu+gamma)
    
    theta0 = c(P0)
    
    t_max = 21 # nb semaines
    
    result_optim = optim(par = theta0,
                         fn = log_likelihood,
                         X0 = X0,
                         t_max = t_max,
                         method = "L-BFGS-B",lower = c(0, 0, 0, 0, 0),  # pour éviter param négatifs => mais apporte pb 
                         data_obs = df_one_state,
                         model=SEIRDS_model_det) 
    
    # Résultats des paramètres optimisés
    optimized_params = result_optim$par
    
    print("param ininiaux : ")
    print(P0)
    print("param opti :")
    print(optimized_params)
    print(paste0("R0  : ",R_0))
    cat("\n")
      
    
    # Paramètres initiaux optimisés
    beta = optimized_params[1] # 
    sigma = optimized_params[2] # 
    mu = optimized_params[3] # 
    gamma =optimized_params[4] # 
    w = optimized_params[5] # 
    P0 = c(beta = beta,sigma = sigma,mu = mu,gamma = gamma,w = w) 
    
    history_SEIRD_model = SEIRDS_model_det(P0, X0, t_max)
    
    epidemy_country_sim = history_SEIRD_model%>%
      select("time","I","E","D")%>%
      mutate("I_sim" = I)%>%
      mutate("D_sim" = pmax(D - lag(D, default = 0), 0)) %>% # équivalent de new_death
      rename("weeks"="time")%>%
      select(-c("I","E","D"))
    
    gg = ggplot()+
      #scale_y_log10()+
      geom_point(data=df_one_state, aes(x = weeks, y = I_obs),col="black",alpha=0.7)+
      geom_line(data=epidemy_country_sim, aes(x = weeks, y = I_sim),col="red",alpha=0.7,lwd=2)+
      theme_minimal() +
      labs(title = "Comparaison model SEIRDS avec donneés observées",
           x = "Temps",
           y = "Nombre d'Individus",
           subtitle = paste0(
             "beta = ", round(beta,4), ", ",
             "sigma = ", round(sigma,4), ", ",
             "mu = ", round(mu,4), ", ",
             "gamma = ", round(gamma,4), ", ",
             "w = ", round(w,4))) 
    plot(gg)
}}

################################################################################
# éxécution simulation POP TOTALE => INFERENCE ABC REGRESSION ------------------
################################################################################

# Analyse epidemy ---------------------------------------------------------

#### Plot vaccination
plot(0:120,epidemy_country_weeks_obs$I_obs,type = "l", ylab = "Nouvelles infections", xlab = "Semaine",main = "Nombre de nouvelles infection hebdomadaires aux Etats-Unis")
abline(v = 46, col="red",lty = 1,lwd=0.1) # vaccination autour du 15 décembre, 46eme semaine
abline(v = 84, col="blue",lty = 1,lwd=0.1)
legend("topright",legend = c("1ere Vaccination","3ème vaccination"), col = c("red","blue"), lty = 1, lwd = 0.1)

#### Plot variants
plot(0:120,epidemy_country_weeks_obs$I_obs,type = "l", ylab = "Nouvelles infections", xlab = "Semaine",main = "Nombre de nouvelles infection hebdomadaires aux Etats-Unis")
abline(v = 62, col="red",lty = 1,lwd=0.1)
abline(v = 66, col="blue",lty = 1,lwd=0.1)
abline(v = 97, col="green",lty = 1,lwd=0.1)
legend("topright",legend = c("Alpha","Delta","Omicron"), col = c("red","blue","green"), lty = 1, lwd = 0.1)

# Decoupage selon les 3 variants --------------------------------------------

# Conditions initiales
N = 15000000        # Population totale des USA
I0 = 5           # Nombre initial d'infectés
S0 = N - I0         # Nombre initial de susceptibles
E0 = 0              # Nombre initial d'exposés
R0 = 0              # Nombre initial de récupérés
D0 = 0              # Nombre initial de décédés
X0 = c(N=N,S0=S0,E0=E0,I0=I0,R0=R0,D0=D0) 

# 1. phase initiale
params_init = c(beta = 2.511133271, 
                sigma = 0.909272728, 
                mu = 0.009603567, 
                gamma =0.915915544, 
                w = 0.054783550)
sim_init_opt = SEIRDS_model_det(params_init, X0, t_max = 62)

# 2. phase alpha/delta
params_ad = c(beta = 3.128401485, 
              sigma = 1.000444358, 
              mu = 0.008464926, 
              gamma = 0.890503547,  
              w = 0.037770080)
X0_ad = c(N= tail(sim_init_opt$N,1),
          S0=tail(sim_init_opt$S,1), # +tail(sim_init$R,1),
          E0=tail(sim_init_opt$E,1),
          I0=tail(sim_init_opt$I,1),
          R0=tail(sim_init_opt$R,1),
          D0=tail(sim_init_opt$D,1))
sim_ad_opt = SEIRDS_model_det(params_ad, X0_ad, t_max = 97-62)

# 3. phase omicron
params_omicron = c(beta = 3.832789639, 
                   sigma = 0.979351299, 
                   mu = 0.007425002, 
                   gamma = 0.784010527, 
                   w = 0.060581444)
X0_omicron = c(N= tail(sim_ad_opt$N,1),
               S0=tail(sim_ad_opt$S,1)+tail(sim_ad_opt$R,1),
               E0=tail(sim_ad_opt$E,1),
               I0=tail(sim_ad_opt$I,1),
               R0=0,
               D0=tail(sim_ad_opt$D,1))
sim_omicron_opt = SEIRDS_model_det(params_omicron, X0_omicron, t_max = 121- 97)

# 1.2.3 Simulation totale
sim_totale = rbind(sim_init_opt, sim_ad_opt, sim_omicron_opt)
sim_totale$time = 0:(length(sim_totale$time)-1)
plot(sim_totale$time, sim_totale$I, type = "l", col = "blue", xlab = "Semaine", ylab= "Nouvelles infections")
lines(0:120,epidemy_country_weeks_obs$I_obs,type = "l", ylab = "Nouvelles infections", xlab = "Semaine",main = "Nombre de nouvelles infection hebdomadaires aux Etats-Unis")
plot(0:120,epidemy_country_weeks_obs$I_obs,type = "l", ylab = "Nouvelles infections", xlab = "Semaine",main = "Nombre de nouvelles infection hebdomadaires aux Etats-Unis")

sim_init_test_opt = sim_init_opt %>%
  mutate("D_sim" = pmax(D - lag(D, default = 0), 0)) 
sim_ad_test_opt = sim_ad_opt %>%
  mutate("D_sim" = pmax(D - lag(D, default = 0), 0)) 
sim_omicron_test_opt = sim_omicron_opt %>%
  mutate("D_sim" = pmax(D - lag(D, default = 0), 0)) 

## AVEC PARAMS OPTI ---------------------------------------------------------
gg = ggplot(data=epidemy_country_weeks_obs, aes(x=weeks,y=I_obs))+
  geom_point(col="black")+
  geom_point(col="black")+
  geom_line(data = sim_init_opt, aes(x=c(1:63),y=I),col=2)+
  geom_line(data = sim_ad_opt, aes(x=c(63:98),y=I),col=3)+
  geom_line(data = sim_omicron_opt, aes(x=c(99:123),y=I),col=4)+
  labs(ylab = "Nouvelles infections", xlab = "Semaine", main = "Nombre de nouvelles infection hebdomadaires aux Etats-Unis",
       subtitle = paste0(
         "P0 : ",
         "beta = ", round(params_init[1], 4), ", ",
         "sigma = ", round(params_init[2], 4), ", ",
         "mu = ", round(params_init[3], 4), ", ",
         "gamma = ", round(params_init[4], 4), ", ",
         "w = ", round(params_init[5], 4), "\n",
         "P1 : ",
         "beta = ", round(params_ad[1], 4), ", ",
         "sigma = ", round(params_ad[2], 4), ", ",
         "mu = ", round(params_ad[3], 4), ", ",
         "gamma = ", round(params_ad[4], 4), ", ",
         "w = ", round(params_ad[5], 4), "\n",
         "P2 : ",
         "beta = ", round(params_omicron[1], 4), ", ",
         "sigma = ", round(params_omicron[2], 4), ", ",
         "mu = ", round(params_omicron[3], 4), ", ",
         "gamma = ", round(params_omicron[4], 4), ", ",
         "w = ", round(params_omicron[5], 4)
       )
  )+
    theme_minimal()
plot(gg)

# nombre simulation 
Ns = 1000

optimisation_model_ABC_Reg = T
if (optimisation_model_ABC_Reg){
  
  # Optimisation paramètre phase 1 ------------------------------------------
  
  t_mod = 62
  
  prior_distrib = list(
    beta = list(type = "uniform", min = 2, max = 5),
    sigma = list(type = "uniform", min = 0.8, max = 1.2),
    mu = list(type = "uniform", min = 0.007, max = 0.034),
    gamma = list(type = "uniform", min = 0.5, max = 1),
    w = list(type = "uniform", min = 0.03, max = 0.07)
  )
  
  # prior_distrib = list(
  #   beta = list(type = "uniform", min = 2.2, max = 2.9),
  #   sigma = list(type = "uniform", min = 0.8, max = 1.2),
  #   mu = list(type = "uniform", min = 0.007, max = 0.034),
  #   gamma = list(type = "uniform", min = 0.5, max = 1),
  #   w = list(type = "uniform", min = 0.03, max = 0.07)
  # )
  
  sample_from_prior = function(prior_distribution){
    sampled_params = sapply(prior_distribution, function(param){
      return(runif(1, min = param$min, max = param$max))
    })
    return(sampled_params)
  }
  
  params = sample_from_prior(prior_distrib)
  
  # tirage des paramètres
  params_list = replicate(Ns, sample_from_prior(prior_distrib), simplify = FALSE)
  
  # mise en forme
  params_df = do.call(rbind, params_list)
  colnames(params_df) = names(prior_distrib)
  
  print("phase 1")
  # simuler des données pour chaque lot de paramètre
  simulate_data = function(params, t_max){
    #X0 = initial_conditions(params)
    sim = SEIRDS_model_det(params, X0, t_max)
    return(sim)
  }
  
  simulations = lapply(params_list, function(params){
    simulate_data(params, t_max = t_mod)
  })
  
  ## Calcul des statistiques résumées # prend du temps exécution

  compute_summary_statistics = function(data){
    return(c(
      mean(data$I),
      sd(data$I),
      median(data$I),
      mean(data$D),
      sd(data$D)
    ))
  }
  
  summary_stats = do.call(rbind, lapply(simulations, compute_summary_statistics))
  
  # Statistique des données observées
  colnames(epidemy_country_weeks) = c("weeks","total_case","total_deaths","I","D")
  data_obs_summary = data.frame(X1 = compute_summary_statistics(epidemy_country_weeks[0:t_mod,])[1],
                                X2 = compute_summary_statistics(epidemy_country_weeks[0:t_mod,])[2],
                                X3 = compute_summary_statistics(epidemy_country_weeks[0:t_mod,])[3],
                                X4 = compute_summary_statistics(epidemy_country_weeks[0:t_mod,])[4],
                                X5 = compute_summary_statistics(epidemy_country_weeks[0:t_mod,])[5]
                                
  )
  
  # Dataframe des statistiques résumées et des paramètres 
  training_data = data.frame(summary_stats, params_df)
  training_data = na.omit(training_data)
  
  # Entrainement de la forêt aléatoire
  
  # BETA
  abc_beta = regAbcrf(beta ~ X1 + X2 
                      + X3 
                      + X4
                      + X5
                      , data = training_data, ntree = 500)
  predicted_beta = predict(abc_beta, data_obs_summary, training = training_data)
  
  # SIGMA
  abc_sigma = regAbcrf(sigma ~ X1 + X2 
                       + X3 
                       + X4
                       + X5
                       , data = training_data, ntree = 500)
  predicted_sigma = predict(abc_sigma, data_obs_summary, training = training_data)
  
  # MU
  abc_mu = regAbcrf(mu ~ X1 + X2 
                    + X3 
                    + X4
                    + X5
                    , data = training_data, ntree = 500)
  predicted_mu = predict(abc_mu, data_obs_summary, training = training_data)
  
  # GAMMA
  abc_gamma = regAbcrf(gamma ~ X1 + X2
                       + X3
                       + X4
                       + X5
                       , data = training_data, ntree = 500)
  predicted_gamma = predict(abc_gamma, data_obs_summary, training = training_data)
  
  # W
  abc_w = regAbcrf(w ~ X1 + X2 
                   + X3 
                   + X4
                   + X5
                   , data = training_data, ntree = 500)
  predicted_w = predict(abc_w, data_obs_summary, training = training_data)
  
  # paramètres opti
  P0 = c(predicted_beta$expectation,
         predicted_sigma$expectation,
         predicted_mu$expectation,
         predicted_gamma$expectation,
         predicted_w$expectation)
  
  
  # Plot
  history_SEIRDS_opt_init = SEIRDS_model_det(P0,X0,t_mod)
  history_melt_SEIRDS_opt = melt(history_SEIRDS_opt_init, 
                                 id.vars = "time", 
                                 variable.name = "Compartment", 
                                 value.name = "Count")%>%
    filter(Compartment %in% c("E", "I", "R", "D"))
  gg = ggplot(history_melt_SEIRDS_opt, aes(x = time, y = Count, color = Compartment)) +
    # scale_y_log10()+
    geom_step() +
    labs(title = "Modèle SEIRS Deterministe à Temps Discret",
         x = "Temps",
         y = "Nombre d'Individus",
    ) +
    theme_minimal() +
    scale_color_manual(values = c("red", "green","orange","black"))
  plot(gg)
  
  ################### COMPARAISON OBS <=> SIM
  epidemy_country_sim_init = history_SEIRDS_opt_init%>%
    select("time","I","E","D")%>%
    mutate("I_sim" = I)%>%
    mutate("D_sim" = pmax(D - lag(D, default = 0), 0)) %>% 
    rename("weeks"="time")%>%
    select(-c("I","E","D"))
  # cas
  plot(x = epidemy_country_weeks_obs$weeks,
       y =epidemy_country_weeks_obs$I_obs,type = "l")
  lines(x = epidemy_country_sim_init$weeks, 
       y = epidemy_country_sim_init$I_sim, type='l',col="red", 
       ylab = "Nouvelles infections", xlab = "Semaine", main = "Nombre de nouvelles infection hebdomadaires aux Etats-Unis")
  ################### --------------------------
  
  # préparation X0 phase 2
  sim_init = SEIRDS_model_det(P0, X0, t_max = 62)
  X0_ad = c(N= tail(sim_init$N,1),
            S0=tail(sim_init$S,1), #+tail(sim_init$R,1),
            E0=tail(sim_init$E,1),
            I0=tail(sim_init$I,1),
            R0=tail(sim_init$R,1),
            D0=tail(sim_init$D,1))
  
  # Optimisation paramètre phase 2 ------------------------------------------
  
  t_mod = 97-62
  
  prior_distrib = list(
    beta = list(type = "uniform", min = 2, max = 5),
    sigma = list(type = "uniform", min = 0.8, max = 1.2),
    mu = list(type = "uniform", min = 0.007, max = 0.034),
    gamma = list(type = "uniform", min = 0.5, max = 1),
    w = list(type = "uniform", min = 0.03, max = 0.07)
  )
  
  # prior_distrib = list(
  #   beta = list(type = "uniform", min = 1, max = 5),
  #   sigma = list(type = "uniform", min = 0.8, max = 1.001),
  #   mu = list(type = "uniform", min = 0.007, max = 0.034),
  #   gamma = list(type = "uniform", min = 0.5, max = 1),
  #   w = list(type = "uniform", min = 0.03, max = 0.04)
  # )
  
  sample_from_prior = function(prior_distribution){
    sampled_params = sapply(prior_distribution, function(param){
      return(runif(1, min = param$min, max = param$max))
    })
    return(sampled_params)
  }
  
  params = sample_from_prior(prior_distrib)
  
  # tirage des paramètres
  params_list = replicate(Ns, sample_from_prior(prior_distrib), simplify = FALSE)
  
  # mise en forme
  params_df = do.call(rbind, params_list)
  colnames(params_df) = names(prior_distrib)
  
  print("phase 2")
  # simuler des données pour chaque lot de paramètre
  simulate_data = function(params, t_max){
    #X0 = initial_conditions(params)
    sim = SEIRDS_model_det(params, X0_ad, t_max)
    return(sim)
  }
  
  simulations = lapply(params_list, function(params){
    simulate_data(params, t_max = t_mod)
  })
  
  ## Calcul des statistiques résumées

  compute_summary_statistics = function(data){
    return(c(
      mean(data$I),
      sd(data$I),
      median(data$I),
      mean(data$D),
      sd(data$D)
    ))
  }
  
  summary_stats = do.call(rbind, lapply(simulations, compute_summary_statistics))
  
  # Statistique des données observées
  colnames(epidemy_country_weeks) = c("weeks","total_case","total_deaths","I","D")
  data_obs_summary = data.frame(X1 = compute_summary_statistics(epidemy_country_weeks[61:96,])[1],
                                X2 = compute_summary_statistics(epidemy_country_weeks[61:96,])[2],
                                X3 = compute_summary_statistics(epidemy_country_weeks[61:96,])[3],
                                X4 = compute_summary_statistics(epidemy_country_weeks[61:96,])[4],
                                X5 = compute_summary_statistics(epidemy_country_weeks[61:96,])[5]
                                
  )
  
  # Dataframe des statistiques résumées et des paramètres 
  training_data = data.frame(summary_stats, params_df)
  training_data = na.omit(training_data)
  
  # Entrainement de la forêt aléatoire
  
  # BETA
  abc_beta = regAbcrf(beta ~ X1 + X2 
                      + X3 
                      + X4
                      + X5
                      , data = training_data, ntree = 500)
  predicted_beta = predict(abc_beta, data_obs_summary, training = training_data)
  
  
  # SIGMA
  abc_sigma = regAbcrf(sigma ~ X1 + X2 
                       + X3 
                       + X4
                       + X5
                       , data = training_data, ntree = 500)
  predicted_sigma = predict(abc_sigma, data_obs_summary, training = training_data)
  
  # MU
  abc_mu = regAbcrf(mu ~ X1 + X2 
                    + X3 
                    + X4
                    + X5
                    , data = training_data, ntree = 500)
  predicted_mu = predict(abc_mu, data_obs_summary, training = training_data)
  
  # GAMMA
  abc_gamma = regAbcrf(gamma ~ X1 + X2
                       + X3
                       + X4
                       + X5
                       , data = training_data, ntree = 500)
  predicted_gamma = predict(abc_gamma, data_obs_summary, training = training_data)
  
  # W
  abc_w = regAbcrf(w ~ X1 + X2 
                   + X3 
                   + X4
                   + X5
                   , data = training_data, ntree = 500)
  predicted_w = predict(abc_w, data_obs_summary, training = training_data)
  
  # paramètres opti
  P1 = c(predicted_beta$expectation,
         predicted_sigma$expectation,
         predicted_mu$expectation,
         predicted_gamma$expectation,
         predicted_w$expectation)
  
  
  # Plot
  history_SEIRDS_opt_ad = SEIRDS_model_det(P1,X0_ad,t_mod)
  history_melt_SEIRDS_opt = melt(history_SEIRDS_opt_ad, 
                                 id.vars = "time", 
                                 variable.name = "Compartment", 
                                 value.name = "Count")%>%
    filter(Compartment %in% c("E", "I", "R", "D"))
  gg = ggplot(history_melt_SEIRDS_opt, aes(x = time, y = Count, color = Compartment)) +
    # scale_y_log10()+
    geom_step() +
    labs(title = "Modèle SEIRS Deterministe à Temps Discret",
         x = "Temps",
         y = "Nombre d'Individus",
    ) +
    theme_minimal() +
    scale_color_manual(values = c("red", "green","orange","black"))
  plot(gg)
  
  ################### COMPARAISON OBS <=> SIM
  epidemy_country_sim_ad = history_SEIRDS_opt_ad%>%
    select("time","I","E","D")%>%
    mutate("I_sim" = I)%>%
    mutate("D_sim" = pmax(D - lag(D, default = 0), 0)) %>% 
    rename("weeks"="time")%>%
    select(-c("I","E","D"))
  # cas
  plot(x = epidemy_country_weeks_obs$weeks,
        y =epidemy_country_weeks_obs$I_obs,type = "l")
  lines(x = epidemy_country_weeks_obs$weeks[61:96], 
        y = epidemy_country_sim_ad$I_sim, type='l',col="red", 
        ylab = "Nouvelles infections", xlab = "Semaine", main = "Nombre de nouvelles infection hebdomadaires aux Etats-Unis")
  ################### --------------------------
  
  # préparation X0 phase 3
  sim_ad = SEIRDS_model_det(P1, X0_ad, t_max = 97-62)
  X0_omicron = c(N= tail(sim_init$N,1),
                 S0=tail(sim_init$S,1),#+tail(sim_init$R,1),
                 E0=tail(sim_init$E,1),
                 I0=tail(sim_init$I,1),
                 R0= tail(sim_init$R,1), # si perte immunité !! OU tail(sim_init$R,1),  
                 D0=tail(sim_init$D,1))
  
  # Optimisation paramètre phase 3 ------------------------------------------
  
  t_mod = 121-97
  
  prior_distrib = list(
    beta = list(type = "uniform", min = 2, max = 5),
    sigma = list(type = "uniform", min = 0.8, max = 1.2),
    mu = list(type = "uniform", min = 0.007, max = 0.034),
    gamma = list(type = "uniform", min = 0.5, max = 1),
    w = list(type = "uniform", min = 0.03, max = 0.07)
  )
  
  # prior_distrib = list(
  #   beta = list(type = "uniform", min = 3.7, max = 5),
  #   sigma = list(type = "uniform", min = 0.8, max = 1),
  #   mu = list(type = "uniform", min = 0.007, max = 0.034),
  #   gamma = list(type = "uniform", min = 0, max = 1),
  #   w = list(type = "uniform", min = 0.03, max = 0.07)
  # )
  
  sample_from_prior = function(prior_distribution){
    sampled_params = sapply(prior_distribution, function(param){
      return(runif(1, min = param$min, max = param$max))
    })
    return(sampled_params)
  }
  
  params = sample_from_prior(prior_distrib)
  
  # tirage des paramètres
  params_list = replicate(Ns, sample_from_prior(prior_distrib), simplify = FALSE)
  
  # mise en forme
  params_df = do.call(rbind, params_list)
  colnames(params_df) = names(prior_distrib)
  
  print("phase 3")
  # simuler des données pour chaque lot de paramètre
  simulate_data = function(params, t_max){
    #X0 = initial_conditions(params)
    sim = SEIRDS_model_det(params, X0_omicron, t_max)
    return(sim)
  }
  
  simulations = lapply(params_list, function(params){
    simulate_data(params, t_max = t_mod)
  })
  
  ## Calcul des statistiques résumées
  compute_summary_statistics = function(data){
    return(c(
      mean(data$I),
      sd(data$I),
      median(data$I),
      mean(data$D),
      sd(data$D)
    ))
  }
  
  summary_stats = do.call(rbind, lapply(simulations, compute_summary_statistics))
  
  # Statistique des données observées
  colnames(epidemy_country_weeks) = c("weeks","total_case","total_deaths","I","D")
  data_obs_summary = data.frame(X1 = compute_summary_statistics(epidemy_country_weeks[96:121,])[1],
                                X2 = compute_summary_statistics(epidemy_country_weeks[96:121,])[2],
                                X3 = compute_summary_statistics(epidemy_country_weeks[96:121,])[3],
                                X4 = compute_summary_statistics(epidemy_country_weeks[96:121,])[4],
                                X5 = compute_summary_statistics(epidemy_country_weeks[96:121,])[5]
                                
  )
  
  # Dataframe des statistiques résumées et des paramètres 
  training_data = data.frame(summary_stats, params_df)
  training_data = na.omit(training_data)
  
  # Entrainement de la forêt aléatoire
  
  # BETA
  abc_beta = regAbcrf(beta ~ X1 + X2 
                      + X3 
                      + X4
                      + X5
                      , data = training_data, ntree = 500)
  predicted_beta = predict(abc_beta, data_obs_summary, training = training_data)
  
  
  # SIGMA
  abc_sigma = regAbcrf(sigma ~ X1 + X2 
                       + X3 
                       + X4
                       + X5
                       , data = training_data, ntree = 500)
  predicted_sigma = predict(abc_sigma, data_obs_summary, training = training_data)
  
  # MU
  abc_mu = regAbcrf(mu ~ X1 + X2 
                    + X3 
                    + X4
                    + X5
                    , data = training_data, ntree = 500)
  predicted_mu = predict(abc_mu, data_obs_summary, training = training_data)
  
  # GAMMA
  abc_gamma = regAbcrf(gamma ~ X1 + X2
                       + X3
                       + X4
                       + X5
                       , data = training_data, ntree = 500)
  predicted_gamma = predict(abc_gamma, data_obs_summary, training = training_data)
  
  # W
  abc_w = regAbcrf(w ~ X1 + X2 
                   + X3 
                   + X4
                   + X5
                   , data = training_data, ntree = 500)
  predicted_w = predict(abc_w, data_obs_summary, training = training_data)
  
  # paramètres opti
  P2 = c(predicted_beta$expectation,
         predicted_sigma$expectation,
         predicted_mu$expectation,
         predicted_gamma$expectation,
         predicted_w$expectation)
  
  
  # Plot
  history_SEIRDS_opt_omicron = SEIRDS_model_det(P2,X0_omicron,t_mod)
  history_melt_SEIRDS_opt = melt(history_SEIRDS_opt_omicron, 
                                 id.vars = "time", 
                                 variable.name = "Compartment", 
                                 value.name = "Count")%>%
    filter(Compartment %in% c("E", "I", "R", "D"))
  gg = ggplot(history_melt_SEIRDS_opt, aes(x = time, y = Count, color = Compartment)) +
    # scale_y_log10()+
    geom_step() +
    labs(title = "Modèle SEIRS Deterministe à Temps Discret",
         x = "Temps",
         y = "Nombre d'Individus",
    ) +
    theme_minimal() +
    scale_color_manual(values = c("red", "green","orange","black"))
  plot(gg)
  
  ################### COMPARAISON OBS <=> SIM
  epidemy_country_sim_omicron = history_SEIRDS_opt_omicron%>%
    select("time","I","E","D")%>%
    mutate("I_sim" = I)%>%
    mutate("D_sim" = pmax(D - lag(D, default = 0), 0)) %>% 
    rename("weeks"="time")%>%
    select(-c("I","E","D"))
  # cas
  plot(x = epidemy_country_weeks_obs$weeks,
        y =epidemy_country_weeks_obs$I_obs,type = "l")
  lines(x = epidemy_country_weeks_obs$weeks[97:121], 
        y = epidemy_country_sim_omicron$I_sim, type='l',col="red", 
        ylab = "Nouvelles infections", xlab = "Semaine", main = "Nombre de nouvelles infection hebdomadaires aux Etats-Unis")
  ################### --------------------------
  
  
  ################### COMPARAISON TOTALE OBS <=> SIM #########################
  
  ### Sur param optimises recents ----------------------------------------------
  
  # cas
  gg = ggplot(data=epidemy_country_weeks_obs, aes(x=weeks,y=I_obs))+
    geom_point(col="black")+
    geom_line(col="black")+
    geom_line(data=epidemy_country_sim_init, aes(x=c(1:63),y=I_sim),col=2)+
    geom_line(data=epidemy_country_sim_ad, aes(x=c(63:98),y=I_sim),col=3)+
    geom_line(data=epidemy_country_sim_omicron, aes(x=c(98:122),y=I_sim),col=4)+
    labs(ylab = "Nouvelles infections", xlab = "Semaine", main = "Nombre de nouvelles infection hebdomadaires aux Etats-Unis",
         subtitle = paste0(
           "P0 : ",
           "beta = ", round(P0[1], 4), ", ",
           "sigma = ", round(P0[2], 4), ", ",
           "mu = ", round(P0[3], 4), ", ",
           "gamma = ", round(P0[4], 4), ", ",
           "w = ", round(P0[5], 4), "\n",
           "P1 : ",
           "beta = ", round(P1[1], 4), ", ",
           "sigma = ", round(P1[2], 4), ", ",
           "mu = ", round(P1[3], 4), ", ",
           "gamma = ", round(P1[4], 4), ", ",
           "w = ", round(P1[5], 4), "\n",
           "P2 : ",
           "beta = ", round(P2[1], 4), ", ",
           "sigma = ", round(P2[2], 4), ", ",
           "mu = ", round(P2[3], 4), ", ",
           "gamma = ", round(P2[4], 4), ", ",
           "w = ", round(P2[5], 4)
         )
    )+
    theme_minimal()  
  plot(gg)
  
}


################### --------------------------


################################################################################
# MODEL METAPOP TPS DISCRET
################################################################################

### Stochastique ---------------------------------------------------------------
################################################################################

metapop_start = T
if (metapop_start){

  SEIRDS_model_metapop_stoch = function(X0, P, contact_matrix, t_max){
    
    # Initialisation des vecteurs pour stocker les résultats pour chaque population
    S = matrix(0, nrow = nb_state, ncol = t_max)
    E = matrix(0, nrow = nb_state, ncol = t_max)
    I = matrix(0, nrow = nb_state, ncol = t_max)
    R = matrix(0, nrow = nb_state, ncol = t_max)
    D = matrix(0, nrow = nb_state, ncol = t_max)
    N = matrix(0, nrow = nb_state, ncol = t_max)
    
    beta = P[1]
    beta_inter = P[2]
    sigma = P[3]
    mu = P[4]
    gamma = P[5]
    w = P[6]
    
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
                   D = matrix(0, nrow = nb_state, ncol = t_max),
                   N = matrix(0, nrow = nb_state, ncol = t_max))
    
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
    S[pop_id, 1] = vect_pop_size[pop_id] - X0[[1]]  # Une personne infectée au départ
    I[pop_id, 1] = X0[[1]]
    
    history$S[, 1] = S[, 1]
    history$E[, 1] = E[, 1]
    history$I[, 1] = I[, 1]
    history$R[, 1] = R[, 1]
    history$D[, 1] = D[, 1]
    history$N[, 1] = N[, 1]
    
    # Simulation de l'épidémie dans la métapopulation
    for (t in 2:t_max){
      
      print(paste0("Generation tps : ", t))
      
      for (i in 1:nb_state) {
        
        # Dynamique dans la population i
        # Calcul des probabilités de transition
        prob_beta = 1 - exp(-beta* E[i, t-1]/N[i,t-1])
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
        S[i, t] = S[i, t-1] - new_E[i, t] + new_S[i, t]
        E[i, t] = E[i, t-1] + new_E[i, t] - new_I[i, t]
        I[i, t] = I[i, t-1] + new_I[i, t] - new_R[i, t] - new_D[i, t]
        R[i, t] = R[i, t-1] + new_R[i, t] - new_S[i, t]
        D[i, t] = D[i, t-1] + new_D[i, t]
        
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
      history$N[, t] = N[, t]
    }
    return(history)
  }
  
  ################################################################################
  # matrice de contact ---------------------------------------------------------
  ################################################################################
  
  # Creation matrice de contact 
  # plot(gg_usa)
  
  # matrice uniforme
  contact_matrix_uniform = matrix(0.3, nrow = nb_state, ncol = nb_state)  # Taux de migration entre population
  
  # matrice binaire
  map_clean = map_state[,c(5,1,2)]
  map_clean$state = as.factor(map_clean$state)
  
  hab.neig = neig(area = map_clean)
  summary(hab.neig)
  hab.mat = neig2mat(hab.neig)
  area.plot(map_clean, graph = hab.neig) # réseau de contact
  
  contact_matrix_binaire = as.matrix(hab.mat)
  # heatmap(contact_matrix_binaire,col = grey.colors(256), scale = "none",main="Bin")
  # matrice pondéree
  m_dist = matrix(0, nrow = 49, ncol = 49)
  
  for (i in 1:nrow(hab.mat)){
    for (j in 1:ncol(hab.mat)){
      if (hab.mat[i,j] == 1){
        m_dist[i,j] = distHaversine(c(state_centroids[i,3],state_centroids[i,4]),
                                    c(state_centroids[j,3],state_centroids[j,4]))
      }
      else{
        m_dist[i,j] = 0
      }
    }
  }
  
  contact_matrix_pond =  1 - m_dist/rowSums(m_dist)
  contact_matrix_pond[contact_matrix_pond==1]=0
  # heatmap(contact_matrix_pond,col = grey.colors(256),main="Pond")
  
  # choix matrice de contact : <========================================================================
  contact_matrix = contact_matrix_binaire # contact_matrix_binaire # contact_matrix_uniform
  
  # Paramètres initiaux -----------------------------------------------------------
  
  print(params_init)
  print(params_ad)
  print(params_omicron)
  
  beta_inter = 0.01
  P0 = c(beta = params_init[[1]], beta_inter = beta_inter, sigma = params_init[[2]],mu = params_init[[3]],gamma = params_init[[4]],w = params_init[[5]]) 
  P1 = c(beta = params_ad[[1]], beta_inter = beta_inter, sigma = params_ad[[2]],mu = params_ad[[3]],gamma = params_ad[[4]],w = params_ad[[5]]) 
  P2 = c(beta = params_omicron[[1]], beta_inter = beta_inter, sigma = params_omicron[[2]],mu = params_omicron[[3]],gamma = params_omicron[[4]],w = params_omicron[[5]]) 
  
  
  
  
  
  # Simulation Stochastique -------------------------------------------------------------------
  t_max1 = 61
  t_max2 = 35
  t_max3 = 25
  
  X0 = c(I0=10)
  history_metapop_stoch1 = SEIRDS_model_metapop_stoch(X0, P0, contact_matrix, t_max1)
  
  I_last_sim_1 = history_metapop_stoch1$I[, t_max1]  
  X0_ad = c(I0=median(I_last_sim_1))
  history_metapop_stoch2 = SEIRDS_model_metapop_stoch(X0_ad, P1, contact_matrix, t_max2)
  
  I_last_sim_2 = history_metapop_stoch2$I[, t_max2] 
  X0_omicron =  c(I0=median(I_last_sim_2))
  history_metapop_stoch3 = SEIRDS_model_metapop_stoch(X0_omicron, P2, contact_matrix, t_max3)
  
  history_metapop_stoch1$S[is.na(history_metapop_stoch1$S)] = 0
  history_metapop_stoch1$I[is.na(history_metapop_stoch1$I)] = 0
  history_metapop_stoch1$R[is.na(history_metapop_stoch1$R)] = 0
  history_metapop_stoch2$S[is.na(history_metapop_stoch2$S)] = 0
  history_metapop_stoch2$I[is.na(history_metapop_stoch2$I)] = 0
  history_metapop_stoch2$R[is.na(history_metapop_stoch2$R)] = 0
  history_metapop_stoch3$S[is.na(history_metapop_stoch3$S)] = 0
  history_metapop_stoch3$I[is.na(history_metapop_stoch3$I)] = 0
  history_metapop_stoch3$R[is.na(history_metapop_stoch3$R)] = 0
  
  # Graphiques ---------------------------------------------------------------------
  
  # Test avec pop_id
  pop = 1
  S_test1 = history_metapop_stoch1$S[pop,] ; S_test2 = history_metapop_stoch2$S[pop,] ; S_test3 = history_metapop_stoch3$S[pop,]
  I_test1 = history_metapop_stoch1$I[pop,] ; I_test2 = history_metapop_stoch2$I[pop,] ; I_test3 = history_metapop_stoch3$I[pop,]
  E_test1 = history_metapop_stoch1$E[pop,] ; E_test2 = history_metapop_stoch2$E[pop,] ; E_test3 = history_metapop_stoch3$E[pop,]
  R_test1 = history_metapop_stoch1$R[pop,] ; R_test2 = history_metapop_stoch2$R[pop,] ; R_test3 = history_metapop_stoch3$R[pop,]
  D_test1 = history_metapop_stoch1$D[pop,] ; D_test2 = history_metapop_stoch2$D[pop,] ; D_test3 = history_metapop_stoch3$D[pop,]
  N_test1 = history_metapop_stoch1$N[pop,] ; N_test2 = history_metapop_stoch2$N[pop,] ; N_test3 = history_metapop_stoch3$N[pop,]
  
  df_pop_unique1 = data.frame(weeks=c(1:t_max1),S=S_test1,E=E_test1,I=I_test1,R=R_test1,D=D_test1,N=N_test1)
  df_pop_unique2 = data.frame(weeks=c(1:t_max2),S=S_test2,E=E_test2,I=I_test2,R=R_test2,D=D_test2,N=N_test2)
  df_pop_unique3 = data.frame(weeks=c(1:t_max3),S=S_test3,E=E_test3,I=I_test3,R=R_test3,D=D_test3,N=N_test3)
  
  # date_time = Sys.time()
  # date = format(date_time, "%Y-%m-%d_%H-%M-%S")
  # 
  # # FIG 1 pop
  # png(paste0(graph_dir,"graphique_pop_",pop,"__",date,".png"), width = 2000, height = 2000, res = 150)
  # plot(x = 1:t_max, y = R_test,col="red",type="l",lwd=2)
  # lines(x = 1:t_max, y = E_test,col="orange",lwd=2)
  # lines(x = 1:t_max, y = I_test,col="blue",lwd=2)
  # legend("topright", legend = c("Infectés (I)","Latent (E)","Récupérés (R)"), col = c("red", "orange","blue"), lwd = 2, cex = 1.5)
  # dev.off()
  # 
  # # FIG ALL POP
  # png(paste0(graph_dir,"graphique_metapop_",date,".png"), width = 2000, height = 2000, res = 150)
  # par(mfrow = c(7, 7))
  # for (i in 1:nb_state) {
  #   plot(1:t_max, history_metapop_stoch$R[i, ], 
  #        type = "l", 
  #        col = "red", 
  #        xlab = "Temps", 
  #        ylab = "Susceptibles",
  #        lwd=2)
  #   lines(1:t_max, history_metapop_stoch$E[i, ], col = "orange")
  #   lines(1:t_max, history_metapop_stoch$I[i, ], col = "blue",lwd=2)
  #   # lines(1:t_max, new_I_from_ext[i, ], col = "yellow")
  #   title(paste("Population", i))
  # }
  # dev.off()
  
  ### plot 3D de chaque état ------------------------------------------------------------
  
  fig = plot_ly()
  
  # phase 1
  for (state in 1:nb_state) {
    fig = fig %>%
      add_trace(
        x = 1:t_max1,  
        y = rep(state_names[state], t_max1),  
        z = history_metapop_stoch1$I[state, ],  
        type = 'scatter3d',
        mode = 'lines',  
        line = list(color = 'red', width = 2, opacity = 0.5),
        name = paste0("Simulé - ", state_names[state])  
      )
  }
  # phase 2
  for (state in 1:nb_state) {
    fig = fig %>%
      add_trace(
        x = 62:96,  
        y = rep(state_names[state], t_max2),  
        z = history_metapop_stoch2$I[state, ],  
        type = 'scatter3d',
        mode = 'lines',  
        line = list(color = 'green', width = 2, opacity = 0.5),
        name = paste0("Simulé - ", state_names[state])  
      )
  }
  # phase 3
  for (state in 1:nb_state) {
    fig = fig %>%
      add_trace(
        x = 97:121,  
        y = rep(state_names[state], t_max3),  
        z = history_metapop_stoch3$I[state, ],  
        type = 'scatter3d',
        mode = 'lines',  
        line = list(color = 'blue', width = 2, opacity = 0.5),
        name = paste0("Simulé - ", state_names[state])  
      )
  }
  
  # Ajouter sur le plot les données obs
  add_obs_plot_3D = T
  if (add_obs_plot_3D) {
    for (state in unique(epidemy_state_weeks_obs$state)) {
      obs_data = epidemy_state_weeks_obs[epidemy_state_weeks_obs$state == state, ]
      fig = fig %>%
        add_trace(
          x = obs_data$weeks,  
          y = rep(state, nrow(obs_data)),  
          z = obs_data$I_obs, 
          type = 'scatter3d',
          mode = 'lines',  
          line = list(color = 'black', width = 3),
          name = paste0("Observé - ", state) 
        )
    }
  }
  
  fig = fig %>%
    layout(
      title = "Comparaison des individus infectés par état (Simulé vs Observé)",
      scene = list(
        xaxis = list(title = "Temps"),
        yaxis = list(
          title = "État",
          tickvals = 1:nb_state, 
          ticktext = state_names[nb_state], 
          tickfont = list(size = 10)
        ),
        zaxis = list(title = "Nombre d'individus infectés (I)")
      )
    )
  fig
  
}
