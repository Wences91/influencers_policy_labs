# Libraries
library(viridis)
library(mapproj)
library(ggplot2)
library(networkD3)
library(dplyr)

# Functions
erase_accents <- function(string){
  unwanted_array = list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                        'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                        'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='S', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                        'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ñ'='n',
                        'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y', 'ü'='u')
  
  string <- chartr(paste(names(unwanted_array), collapse=''),
                   paste(unwanted_array, collapse=''),
                   string)
  
  return(string)
}

geo_map <- function(map, data, x, y, title = 'Name'){
  # Give map data.frame and data data.frame indicating the name of map_id and scale
  ggplot() + 
    geom_polygon(data = map, aes(x = long, y = lat, group = group), fill='grey90') + 
    geom_map(data = data, map = map, aes(map_id = x, fill = y), colour='grey70', size = 0.20) +
    scale_fill_distiller(name = 'Scale log',limits=c(min(y), max(y)), palette = 'Spectral', guide=guide_colourbar(barwidth = 12, title.vjust=0.75)) +
    #scale_fill_viridis(option='D', name = 'Scale log',limits=c(min(y), max(y)), guide=guide_colourbar(barwidth = 12, title.vjust=0.75)) +
    coord_map('mercator', xlim=c(-180,180), ylim = c(-60,90))+
    labs(title = title) +
    theme_minimal() +
    theme(plot.title = element_text(family = 'Helvetica', face = 'bold', size = (15), hjust = 0.5),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          legend.position = 'bottom')
}


# Data
friends <- read.delim('data/friends.tsv',
                      quote = '',
                      header = TRUE,
                      stringsAsFactors = FALSE,
                      colClasses = 'character')


followers <- read.delim('data/followers.tsv',
                        quote = '',
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        colClasses = 'character')

glossary <- read.delim('data/data_labs.tsv',
                       quote = '',
                       header = TRUE,
                       stringsAsFactors = FALSE,
                       colClasses = c('id'='character'),
                       skipNul = TRUE)


# Stats
length(unique(friends$target))
length(unique(followers$source))


# There are missing data
all(friends$target %in% glossary$screen_name)
all(followers$source %in% glossary$screen_name)
sum(!(followers$source %in% glossary$screen_name))
sum(glossary$screen_name %in% followers$source)


# Reduce glossary data.frame by deleting NAs
sum(is.na(glossary$location))
glossary <- glossary[which(!(is.na(glossary$location))),]

# Reduce glossary by deleting no locations
sum(glossary$location == '')
glossary <- glossary[which(!(glossary$location == '')),]

sum(glossary$screen_name %in% c(friends$target, followers$source))
sum(glossary$screen_name %in% friends$target)
sum(glossary$screen_name %in% followers$source)


# Unify locations
glossary$location <- tolower(glossary$location)
glossary$location <- erase_accents(glossary$location)

# Unify in a new variable
locations <- glossary$location
locations <- unique(locations)

# Export locations
#write.table(locations, 'locations.txt',
#            row.names = FALSE,
#            col.names = FALSE,
#            quote = FALSE)

## Downloaded locations

# Load downloaded locations
good_locations <- read.delim('data/obtained.tsv', header = TRUE,
                             stringsAsFactors = FALSE,
                             sep = '\t',
                             quote = '')

bad_locations <- read.delim('data/erroneous.tsv', header = FALSE,
                            stringsAsFactors = FALSE,
                            sep = '\n',
                            quote = '')

# Reduce good location to countries
good_locations <- good_locations[,1:2]

# Check all locations
all(locations %in% c(good_locations$localizacion_original, bad_locations$V1))
sum(glossary$location %in% good_locations$localizacion_original)
sum(glossary$location %in% bad_locations$V1)


# Fix some errors
good_locations[c(17417, 45121, 46879, 59926), 'pais'] <- 'Philippines'
good_locations <- good_locations[which(good_locations$pais != ''),]
bad_locations <- bad_locations[which(!(bad_locations$V1 %in% c('philippines',
                                                               'federal capital territory, nig',
                                                               'republique democratique du con',
                                                               'manila, philippines',
                                                               'territorio de la capital feder',
                                                               'republica de filipinas',
                                                               'ciudad autónoma de buenos aire',
                                                               'manila',
                                                               'republic of the philippines',
                                                               'amman, hashemite kingdom of jo',
                                                               'riyadh, kingdom of saudi arabi',
                                                               'jersey, channel islands',
                                                               'riad, reino de arabia saudi',
                                                               'scotland, europe',
                                                               'bosnia y herzegovina',
                                                               'sarajevo',
                                                               'bosnia and herzegovina',
                                                               'united kingdon',
                                                               'abu dhabi, united arab emirate',
                                                               'punyab, भारत',
                                                               'الرياض, المملكة العربية السعود',
                                                               'nueva bombay, भारत',
                                                               'vododara, india',
                                                               'brussels, europe'))),]


good_locations <- rbind.data.frame(good_locations, 
                                   data.frame(localizacion_original = c('philippines',
                                                                         'federal capital territory, nig',
                                                                         'republique democratique du con',
                                                                         'manila, philippines',
                                                                         'territorio de la capital feder',
                                                                         'republica de filipinas',
                                                                         'ciudad autónoma de buenos aire',
                                                                         'manila',
                                                                         'republic of the philippines',
                                                                         'amman, hashemite kingdom of jo',
                                                                         'riyadh, kingdom of saudi arabi',
                                                                         'jersey, channel islands',
                                                                         'riad, reino de arabia saudi',
                                                                         'scotland, europe',
                                                                         'bosnia y herzegovina',
                                                                         'sarajevo',
                                                                         'bosnia and herzegovina',
                                                                         'united kingdon',
                                                                         'abu dhabi, united arab emirate',
                                                                         'punyab, भारत',
                                                                         'الرياض, المملكة العربية السعود',
                                                                         'nueva bombay, भारत',
                                                                         'vododara, india',
                                                                         'brussels, europe'),
                                              pais = c('Philippines',
                                                        'Nigeria',
                                                        'Congo',
                                                        'Philippines',
                                                        'Nigeria',
                                                        'Philippines',
                                                        'Argentina',
                                                        'Philippines',
                                                        'Philippines',
                                                        'Jordan',
                                                        'Saudi Arabia',
                                                        'Jersey',
                                                        'Saudi Arabia',
                                                        'Scotland',
                                                        'Bosnia and Herzegovina',
                                                        'Bosnia and Herzegovina',
                                                        'Bosnia and Herzegovina',
                                                        'United Kingdom',
                                                        'Saudi Arabia',
                                                        'India',
                                                        'Saudi Arabia',
                                                        'India',
                                                        'India',
                                                        'Belgium'),
                                              stringsAsFactors = FALSE), 
                                   stringsAsFactors = FALSE)


# Adapt countries names to R
map <- ggplot2::map_data('world')

wrongs_names <- unique(good_locations$pais)[which(!(unique(good_locations$pais) %in% map$region))]
good_names <- c('Netherlands', 'UK', 'USA', 'Trinidad', 'South Africa', 'Germany', 'Dominican Republic',
                'China', 'Ivory Coast', 'Taiwan', 'Democratic Republic of the Congo', 'Czech Republic', 'France', 'Democratic Republic of the Congo', 'Democratic Republic of the Congo', 'Netherlands',
                'Virgin Islands', 'Saint Vincent', 'Gambia', 'Barbuda', '', 'Timor-Leste', 'Swaziland', 'Bahamas',
                'Kazakhstan', '', 'China', 'UK', 'Jamaica', 'Spain', 'Sao Tome and Principe', 'South Georgia',
                'Saint Kitts', 'Saint Helena', 'Micronesia', '', 'Italy', 'Japan', '', '', 'Peru', 'Switzerland',
                'South Georgia', 'Belgium', 'Romania', 'Democratic Republic of the Congo', 'UK')

fix_names <- data.frame(wrongs_names, good_names,stringsAsFactors = FALSE)
map_names <- setNames(good_names, wrongs_names)

good_locations$pais[which(!(good_locations$pais %in% map$region))] <- map_names[good_locations$pais[which(!(good_locations$pais %in% map$region))]]
good_locations <- good_locations[which(good_locations$pais != ''),]

all(good_locations$pais %in% map$region)


# Stats
length(unique(glossary$location))
length(unique(friends$target))
dim(glossary[which(glossary$screen_name %in% friends$target),])
dim(glossary[which(glossary$screen_name %in% friends$target & glossary$location %in% good_locations$localizacion_original),])

length(unique(followers$source))
dim(glossary[which(glossary$screen_name %in% followers$source),])
dim(glossary[which(glossary$screen_name %in% followers$source & glossary$location %in% good_locations$localizacion_original),])
dim(glossary[which(glossary$screen_name %in% followers$source & glossary$location %in% bad_locations),])
sum(glossary$location %in% bad_locations)
sum(glossary$location %in% good_locations$localizacion_original)


# Change glossary locations into countries
map_names <- setNames(good_locations$pais, good_locations$localizacion_original)

# Copy glossary in a new variable
glossary_map <- glossary
glossary_map$location <- map_names[glossary_map$location]

# Remove NA
glossary_map <- glossary_map[which(!(is.na(glossary_map$location))),]
glossary_map$screen_name[which(!(glossary_map$screen_name %in% c(friends$target, followers$source)))]

# Merge friends and follower with locations
friends_map <- merge(data.frame(screen_name = unique(friends$target), stringsAsFactors = FALSE), glossary_map, by = 'screen_name')
followers_map <- merge(data.frame(screen_name = unique(followers$source), stringsAsFactors = FALSE), glossary_map, by = 'screen_name')

all(friends_map$screen_name %in% friends$target)
all(followers_map$screen_name %in% followers$source)

length(unique(friends_map$screen_name)) + length(unique(followers_map$screen_name))

# Maps
# General
friends_geo <- as.data.frame(table(friends_map$location), stringsAsFactors = FALSE)
followers_geo <- as.data.frame(table(followers_map$location), stringsAsFactors = FALSE)

# Means - SD
mean(table(friends$source))
mean(table(followers$target))
sd(table(friends$source))
sd(table(followers$target))


#png('friends_final.png', units="in", width=9, height=6, res=500)
geo_map(map, friends_geo, friends_geo$Var1, log10(friends_geo$Freq), 'Friends')
#dev.off()

#png('followers_final.png', units="in", width=9, height=6, res=500)
geo_map(map, followers_geo, followers_geo$Var1, log10(followers_geo$Freq), 'Followers')
#dev.off()

## By friend and follower type
# UK
# Merge friends and follower with locations
uk_accounts <- c('LBofBexley', 'Bromford', 'DataMillNorth', 'DFID_UK',
                 'GDSTeam', 'Justice_Digital', 'PDR_Online', 'TheSatoriLab',
                 'creativescots', 'silkteam', 'iLab_NI', 'PolicyLabUK',
                 'ylabwales', 'DCCStudio')

all(uk_accounts %in% friends$source)
all(uk_accounts %in% followers$target)

# Means - SD
mean(table(friends$source[which(friends$source %in% uk_accounts)]))
mean(table(followers$target[which(followers$target %in% uk_accounts)]))
sd(table(friends$source[which(friends$source %in% uk_accounts)]))
sd(table(followers$target[which(followers$target %in% uk_accounts)]))


####################################
## FOR UNIQUE USE THE FIRST LINES ##
####################################

friends_map_uk <- merge(data.frame(screen_name = unique(friends$target[which(friends$source %in% uk_accounts)]), stringsAsFactors = FALSE), glossary_map, by = 'screen_name')
#friends_map_uk <- merge(data.frame(screen_name = friends$target[which(friends$source %in% uk_accounts)], stringsAsFactors = FALSE), glossary_map, by = 'screen_name')

followers_map_uk <- merge(data.frame(screen_name = unique(followers$source[which(followers$target %in% uk_accounts)]), stringsAsFactors = FALSE), glossary_map, by = 'screen_name')
friends_geo_uk <- as.data.frame(table(friends_map_uk$location), stringsAsFactors = FALSE)
followers_geo_uk <- as.data.frame(table(followers_map_uk$location), stringsAsFactors = FALSE)

#png('friends_uk.png', units="in", width=9, height=6, res=500)
geo_map(map, friends_geo_uk, friends_geo_uk$Var1, log10(friends_geo_uk$Freq), 'Friends Eng')
#dev.off()

#png('followers_uk.png', units="in", width=9, height=6, res=500)
geo_map(map, followers_geo_uk, followers_geo_uk$Var1, log10(followers_geo_uk$Freq), 'Followers Eng')
#dev.off()

# Southern
# Merge friends and follower with locations
st_accounts <- c('BarcelonaLab', 'ciutatbeta', 'labo_demo',
               'CoBattipaglia', 'comantova',
               'LabX_govpt',
               'UNHCRInnovation',
               'Millenaire3', 'fabriqueH', '_DITP', 'IGNFrance', 'poleemploi_lab',
               'AlpesMaritimes', 'labo_GrandEst', 'DILA_officiel')
all(st_accounts %in% friends$source)
all(st_accounts %in% followers$target)

# Means
mean(table(friends$source[which(friends$source %in% st_accounts)]))
mean(table(followers$target[which(followers$target %in% st_accounts)]))
sd(table(friends$source[which(friends$source %in% st_accounts)]))
sd(table(followers$target[which(followers$target %in% st_accounts)]))

friends_map_st <- merge(data.frame(screen_name = unique(friends$target[which(friends$source %in% st_accounts)]), stringsAsFactors = FALSE), glossary_map, by = 'screen_name')
#friends_map_st <- merge(data.frame(screen_name = friends$target[which(friends$source %in% st_accounts)], stringsAsFactors = FALSE), glossary_map, by = 'screen_name')

followers_map_st <- merge(data.frame(screen_name = unique(followers$source[which(followers$target %in% st_accounts)]), stringsAsFactors = FALSE), glossary_map, by = 'screen_name')
friends_geo_st <- as.data.frame(table(friends_map_st$location), stringsAsFactors = FALSE)
followers_geo_st <- as.data.frame(table(followers_map_st$location), stringsAsFactors = FALSE)

#png('friends_st.png', units="in", width=9, height=6, res=500)
geo_map(map, friends_geo_st, friends_geo_st$Var1, log10(friends_geo_st$Freq), 'Friends south')
#dev.off()

#png('followers_st.png', units="in", width=9, height=6, res=500)
geo_map(map, followers_geo_st, followers_geo_st$Var1, log10(followers_geo_st$Freq), 'Followers south')
#dev.off()

# Nothern
# Merge friends and follower with locations
nt_accounts <- c('CityofOdense', 'roskildekommune', 'cphsolutionslab',
                 'ExperioLab', 'trafiklab', 'waag',
                 'Kennisland', 'LEFfuturecenter',
                 'Sitrafund')

all(nt_accounts %in% friends$source)
all(nt_accounts %in% followers$target)
length(unique(friends$target[which(friends$target %in% glossary_map$screen_name)]))
# Means
mean(table(friends$source[which(friends$source %in% nt_accounts)]))
mean(table(followers$target[which(followers$target %in% nt_accounts)]))
sd(table(friends$source[which(friends$source %in% nt_accounts)]))
sd(table(followers$target[which(followers$target %in% nt_accounts)]))

friends_map_nt <- merge(data.frame(screen_name = unique(friends$target[which(friends$source %in% nt_accounts)]), stringsAsFactors = FALSE), glossary_map, by = 'screen_name')
#friends_map_nt <- merge(data.frame(screen_name = friends$target[which(friends$source %in% nt_accounts)], stringsAsFactors = FALSE), glossary_map, by = 'screen_name')

followers_map_nt <- merge(data.frame(screen_name = unique(followers$source[which(followers$target %in% nt_accounts)]), stringsAsFactors = FALSE), glossary_map, by = 'screen_name')
friends_geo_nt <- as.data.frame(table(friends_map_nt$location), stringsAsFactors = FALSE)
followers_geo_nt <- as.data.frame(table(followers_map_nt$location), stringsAsFactors = FALSE)

#png('friends_nt.png', units="in", width=9, height=6, res=500)
geo_map(map, friends_geo_nt, friends_geo_nt$Var1, log10(friends_geo_nt$Freq), 'Friends north')
#dev.off()

#png('followers_nt.png', units="in", width=9, height=6, res=500)
geo_map(map, followers_geo_nt, followers_geo_nt$Var1, log10(followers_geo_nt$Freq), 'Followers north')
#dev.off()




# Sankey
# all and blocks percentages
friends_geo_uk$Freq2 <- 100*friends_geo_uk$Freq/sum(friends_geo_uk$Freq,friends_geo_st$Freq,friends_geo_nt$Freq)
friends_geo_st$Freq2 <- 100*friends_geo_st$Freq/sum(friends_geo_uk$Freq,friends_geo_st$Freq,friends_geo_nt$Freq)
friends_geo_nt$Freq2 <- 100*friends_geo_nt$Freq/sum(friends_geo_uk$Freq,friends_geo_st$Freq,friends_geo_nt$Freq)
friends_geo_uk$Freq3 <- 100*friends_geo_uk$Freq/sum(friends_geo_uk$Freq)
friends_geo_st$Freq3 <- 100*friends_geo_st$Freq/sum(friends_geo_st$Freq)
friends_geo_nt$Freq3 <- 100*friends_geo_nt$Freq/sum(friends_geo_nt$Freq)


# filter countries
friends_geo_uk2 <- friends_map_uk[which(friends_map_uk$location %in% friends_geo_uk$Var1[friends_geo_uk$Freq2>=0.2]), c('screen_name', 'location')]
friends_geo_st2 <- friends_map_st[which(friends_map_st$location %in% friends_geo_st$Var1[friends_geo_st$Freq2>=0.2]), c('screen_name', 'location')]
friends_geo_nt2 <- friends_map_nt[which(friends_map_nt$location %in% friends_geo_nt$Var1[friends_geo_nt$Freq2>=0.2]), c('screen_name', 'location')]

friends_geo_2per <- rbind.data.frame(friends_geo_uk2, friends_geo_st2, stringsAsFactors = F)
friends_geo_2per <- rbind.data.frame(friends_geo_2per, friends_geo_nt2, stringsAsFactors = F)
friends_geo_2per$count <- 1
friends_geo_2per <- friends_geo_2per %>%
  dplyr::group_by(screen_name, location) %>%
  dplyr::summarise(count=sum(count))


sankey_data <- cbind.data.frame(friends_geo_uk, Block='AS', stringsAsFactors=F)
sankey_data <- rbind.data.frame(sankey_data, cbind.data.frame(friends_geo_st, Block='SO', stringsAsFactors=F), stringsAsFactors=F)
sankey_data <- rbind.data.frame(sankey_data, cbind.data.frame(friends_geo_nt, Block='NO', stringsAsFactors=F), stringsAsFactors=F)
sankey_data <- sankey_data[which(sankey_data$Freq2>=.2),]

sum(sankey_data$Freq[which(sankey_data$Block=='AS')])
sum(sankey_data$Freq[which(sankey_data$Block=='SO')])
sum(sankey_data$Freq[which(sankey_data$Block=='NO')])


sapply(unique(sankey_data$Var1), function(x){
  sankey_data$Var1[which(sankey_data$Var1==x)] <<- paste0(x, ' (', round(max(sankey_data$Freq3[which(sankey_data$Var1==x)]),1), '% | ', round(sum(sankey_data$Freq2[which(sankey_data$Var1==x)]),1), '%)')
})


sankey_data$Block[which(sankey_data$Block=='AS')] <- paste0('AS (', round(sum(sankey_data$Freq3[which(sankey_data$Block=='AS')]),1), '% | ', round(sum(sankey_data$Freq2[which(sankey_data$Block=='AS')]),1), '%)')
sankey_data$Block[which(sankey_data$Block=='NO')] <- paste0('NO (', round(sum(sankey_data$Freq3[which(sankey_data$Block=='NO')]),1), '% | ', round(sum(sankey_data$Freq2[which(sankey_data$Block=='NO')]),1), '%)')
sankey_data$Block[which(sankey_data$Block=='SO')] <- paste0('SO (', round(sum(sankey_data$Freq3[which(sankey_data$Block=='SO')]),1), '% | ', round(sum(sankey_data$Freq2[which(sankey_data$Block=='SO')]),1), '%)')


# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=sankey_data$Block, 
  target=sankey_data$Var1, 
  value=sankey_data$Freq2
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

my_color <- 'd3.scaleOrdinal() .range(["#40e0d0", "#6897bb", "#DD4124", "#40e0d0", "#DD4124", "#40e0d0", "#6897bb", "#6897bb", "#fff68f", "#40e0d0", "#DD4124", "#fff68f", "#40e0d0", "#40e0d0", "#6897bb", "#6897bb", "#DD4124", "#DD4124", "#DD4124"])'

sankeyNetwork(Links = links, Nodes = nodes,
              Source = 'IDsource', Target = 'IDtarget',
              Value = 'value', NodeID = 'name', 
              colourScale=my_color,
              sinksRight=FALSE, LinkGroup='source', fontSize = 10)
