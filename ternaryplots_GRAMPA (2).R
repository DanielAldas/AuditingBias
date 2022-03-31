# Illustrate GRAMPA dataset in ternary plots

## Working directory
getwd()
setwd('C:/Users/danie/Documents/Tesis')

## packages
#install.packages("ggplot2")
#install.packages('ggtern')
library(ggplot2)
library(ggtern)

## Load dataset
pep2D_db <- read.csv('./Data/grampa_pep2d.csv', header=TRUE)
pep2D_db

#Subset
pep2D_sub <- pep2D_db[,4:7]
colnames(pep2D_sub) <- c('helix_H', 'sheet_E', 'coil_C', 'lgth')

#Basic ternary plot
TernBasic <- ggtern::ggtern(data = pep2D_sub,
                  aes(x = sheet_E,
                      y = helix_H, 
                      z = coil_C)) +  
  geom_point(size=0.8, colour='lightblue') +  
  #scale_color_viridis_c() +
  #scale_fill_gradient(low='green', high='red') +
  theme_dark() +
  theme_showarrows() +
  theme_anticlockwise() +
  ggtitle('Distribution')

TernBasic
ggsave('./TernBasic.png')

# Ternary plot with length as 4th dimension
lgth_vec <- pep2D_sub$lgth
TernLength <- ggtern::ggtern(data = pep2D_sub,
                            aes(x = sheet_E,
                                y = helix_H, 
                                z = coil_C)) + 
  geom_point(size=0.8, aes(colour=lgth_vec)) +  
  scale_color_viridis_c(lgth_vec) +
  theme_dark() +
  theme_showarrows() +
  theme_anticlockwise() +
  guides(fill = guide_colorbar(order=1)) +
  labs(title= "Length Distribution",
       fill = "Length", alpha="Length")

TernLength
ggsave('./Figures/TernLength.png')

# Ternary plot highlighting density distribution
TernDens <- ggtern::ggtern(data = pep2D_sub,
                            aes(x = sheet_E,
                                y = helix_H, 
                                z = coil_C))  + 
  stat_density_tern(geom='polygon', color='black',
                    n=500, expand = 1.1, h=0.1,
                    base='identity',
                    aes(fill   = ..level..),
                    na.rm = TRUE) +
  #geom_point(size=0.8, colour='grey', alpha=0.2) +
  theme_dark() +
  theme_showarrows() +
  theme_anticlockwise()

TernDens
#ggsave('./TernDens2.png')

# Regions
pep2D_sub.lims=data.frame(sheet_E = c(1,.20,.20), helix_H = c(.00,.80,.00), coil_C = c(.00,.00,.80))
pep2D_sub.lims2=data.frame(sheet_E = c(.00,.80,.00), helix_H = c(1,.20,.20), coil_C = c(.00,.00,.80))
pep2D_sub.lims3=data.frame(sheet_E = c(.00,.80,.00), helix_H = c(.00,.00,.80), coil_C = c(1,.20,.20))

TernDens_limited <- ggtern::ggtern(data = pep2D_sub,
                                   aes(x = sheet_E,
                                       y = helix_H,
                                       z = coil_C),
                                   aes(x,y,z)) +
  stat_density_tern(geom = 'polygon',
                    expand = 1, h=0.15,
                    base='identity',
                    aes(fill = ..level.., alpha = 0.5),
                    na.rm = TRUE) +
  geom_point(size=0.8, colour='darkgrey', alpha=0.8) +
  geom_mask() +
  geom_polygon(data=pep2D_sub.lims, color='blue', alpha=0, size=0.5) +
  geom_polygon(data=pep2D_sub.lims2, color='red', alpha=0, size=0.5) +
  geom_polygon(data=pep2D_sub.lims3, color='green', alpha=0, size=0.5) +
  scale_fill_continuous(low= 'darkolivegreen1', high='red', name='density', limits=c(0,25)) +
  #scale_fill_viridis(option = 'B', direction = -1) +
  theme_bw() +
  theme_showarrows() +
  theme_anticlockwise() +
  ggtitle('Density Map GRAMPA')

TernDens_limited

