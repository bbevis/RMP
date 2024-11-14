
###############################################
#
# Week 3 - Plotting
# Imai & Williams 3.3 - 3.9
#
###############################################

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
install.packages("viridis")
library(viridis)

afghan = read.csv('afghan.csv')

# Survey data on civilian victimisation where people were asked:
# Over the past year, have you or anyone in your family suffered harm due to actions
# of the Foreign Forces or the Taliban?

head(afghan)
glimpse(afghan)
colnames(afghan)

############################
# Data exploration
############################

# harmed by ISAF, harmed by Taliban (1 = yes, 0 = no)
afghan %>%
  select(violent.exp.ISAF, violent.exp.taliban) %>%
  head()

# How many people were harmed by either forces?
# Were any harmed by both?
afghan %>%
  filter(!is.na(violent.exp.ISAF)) %>%
  filter(!is.na(violent.exp.taliban)) %>%
  group_by(violent.exp.ISAF, violent.exp.taliban) %>%
  count()

afghan %>%
  filter(!is.na(violent.exp.ISAF)) %>%
  filter(!is.na(violent.exp.taliban)) %>%
  group_by(violent.exp.ISAF, violent.exp.taliban) %>%
  count()

# perhaps we can make this a bit neater
afghan %>%
  filter(!is.na(violent.exp.ISAF)) %>%
  filter(!is.na(violent.exp.taliban)) %>%
  group_by(violent.exp.ISAF, violent.exp.taliban) %>%
  count() %>%
  mutate(harmed_by = case_when(violent.exp.ISAF == 0 & violent.exp.taliban == 1 ~ "Taliban",
                               violent.exp.ISAF == 1 & violent.exp.taliban == 0 ~ "ISAF",
                               violent.exp.ISAF == 0 & violent.exp.taliban == 0 ~ "Neither",
                               violent.exp.ISAF == 1 & violent.exp.taliban == 1 ~ "Both"))


############################
# Bar plots
############################

# check out https://ggplot2.tidyverse.org/

# Basic charts...
afghan %>%
  filter(!is.na(violent.exp.ISAF)) %>%
  filter(!is.na(violent.exp.taliban)) %>%
  group_by(violent.exp.ISAF, violent.exp.taliban) %>%
  count() %>%
  mutate(harmed_by = case_when(violent.exp.ISAF == 0 & violent.exp.taliban == 1 ~ "Taliban",
                               violent.exp.ISAF == 1 & violent.exp.taliban == 0 ~ "ISAF",
                               violent.exp.ISAF == 0 & violent.exp.taliban == 0 ~ "Neither",
                               violent.exp.ISAF == 1 & violent.exp.taliban == 1 ~ "Both")) %>%
  ggplot(aes(x = harmed_by, y = n)) +
  geom_bar(stat = 'identity')

# lets make this prettier
afghan %>%
  filter(!is.na(violent.exp.ISAF)) %>%
  filter(!is.na(violent.exp.taliban)) %>%
  group_by(violent.exp.ISAF, violent.exp.taliban) %>%
  count() %>%
  mutate(harmed_by = case_when(violent.exp.ISAF == 0 & violent.exp.taliban == 1 ~ "Taliban", # create a single categorical variable
                               violent.exp.ISAF == 1 & violent.exp.taliban == 0 ~ "ISAF",
                               violent.exp.ISAF == 0 & violent.exp.taliban == 0 ~ "Neither",
                               violent.exp.ISAF == 1 & violent.exp.taliban == 1 ~ "Both")) %>%
  ggplot(aes(x = reorder(harmed_by, n), y = n, fill = harmed_by)) + # initial parameters for any ggplot must include variables to plot
  geom_bar(stat = 'identity') + # for bar plots, include stat = 'identity'. Not required for line or points plot
  theme_classic() + # creates a minimal look
  scale_fill_manual(values = c( "#F8766D", "grey", '#00BFC4', 'grey')) + # manually specify colours if you want
  coord_flip() + # swap y and x axis. Do this to make names easier to read
  theme(legend.position = 'none', # in this case, we don't need a legend
        axis.text=element_text(size=20), # Make sure texts and titles are legible
        axis.title=element_text(size=20, face="bold")) +
  labs(x="",y="Counts of harm") # Give the axes meaningful names

ggsave("./afghan_barplot.png",dpi=300,height=7,width=12) # Save the file to specific

# Multiple ways to plot data
afghan %>%
  pivot_longer(violent.exp.ISAF:violent.exp.taliban, # reshape the data
               names_to = "harming_group",
               values_to = "harm") %>%
  mutate(harm = as.factor(harm)) %>%
  group_by(harming_group, harm) %>%
  count() %>%
  mutate(harming_group = case_when(harming_group == 'violent.exp.ISAF'  ~ "ISAF", # create a single categorical variable
                                   harming_group == 'violent.exp.taliban' ~ "Taliban")) %>%
  ggplot(aes(x = harm, y = n, fill = harming_group)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_minimal() + # minimal theme
  scale_x_discrete(labels = c('No harm', 'Harm', 'No response')) +
  scale_fill_manual(values = c( "#85c1e9", '#1a5276')) + # google hex colours
  theme(legend.position = 'top',
        legend.title = element_blank(), # legend titles are not always needed
        legend.text = element_text(size=20),
        axis.text=element_text(size=20), # Make sure texts and titles are legible
        panel.grid.minor = element_blank(), # remove specific grid lines
        panel.grid.major.x = element_blank(),
        axis.title=element_text(size=20, face="bold")) +
    labs(x="",y="Counts of harm")

ggsave("./afghan_barplot_2.png",dpi=300,height=7,width=12) # Save the file to specific


# Now you know how to make charts pretty, lets see what other TYPES of charts we can make

############################
# Histograms - exploring distributions
############################

afghan %>%
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = .5) +
  scale_x_continuous(breaks = seq(20, 80, by = 10)) +
  theme_minimal()

############################
# Boxplots - showing distributions with means
############################

afghan %>%
  ggplot(aes(y = educ.years, x = province)) +
  geom_boxplot(aes(fill = province)) +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE, option = "B")
  # scale_fill_brewer(palette = "Dark2")

# For palette options, see: https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/

afghan %>%
  ggplot(aes(y = age)) +
  geom_boxplot(aes(color = province)) +
  theme_minimal() +
  facet_wrap(.~province) +
  scale_x_discrete(expand = c(0, 0.5)) +
  ylim(0, 100) +
  theme(legend.position = c(.8,.2),
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank())

############################
# point plots - a cleaner way of showing distribution and means
############################

afghan %>%
  group_by(province) %>%
  summarise(m = mean(age), # sumarising data first to get means and standard errors
            n = n(),
            se = sd(age) / sqrt(n)) %>%
  ggplot(aes(y = m, ymin = m - se, ymax = m + se, x = reorder(province, desc(m)), color = province)) +
  geom_errorbar(width = .2) +
  geom_point(aes(size = 3)) +
  theme_minimal() +
  coord_flip() +
  theme(legend.position = 'none') +
  scale_color_brewer(palette = "Dark2")

############################
# Scatter + line of best fit
############################

#simulate data
x <- c(1, 2, 3, 4, 5, 6, 7, 8)
y <- c(2, 5, 6, 7, 9, 12, 16, 19)

data = data_frame(x, y)

#create scatter plot of x vs. y
data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(color = 'darkgrey') +
  geom_smooth(stat = "smooth", method = "lm", color = 'darkgreen',fill="#69b3a2") +
  theme_minimal()

###################################
# Lets get creative
install.packages('patchwork')
library(patchwork)

# Create some data to represent attitudes life meaning by party
df=data_frame(
  labels=c(
    "Spirituality, faith and religion",
    "Freedom and independence",
    "Hobbies and recreation",
    "Physical and mental health",
    "COVID-19",
    "Pets",
    "Nature and the outdoors"),
  Dem=c(8,6,13,13,8,5,5),
  Rep=c(22,12,7,9,5,2,3)
)

df %>%
  pivot_longer(-labels) %>%
  ggplot(aes(x=value,y=labels)) +

  geom_line(aes(group=labels), color="#E7E7E7", linewidth=3.5) +
  # note that linewidth is a little larger than the point size
  # so that the line matches the height of the point. why is it
  # like that? i don't really know

  geom_point(aes(color=name), size=3) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="#989898"),
        axis.title = element_blank(),
        panel.grid = element_blank()
  ) +
  scale_color_manual(values=c("#436685", "#BF2F24"))+
  scale_x_continuous(labels = scales::percent_format(scale = 1))


df = df %>% # raw is just the generated data

  # compute the gap
  mutate(gap=Rep-Dem) %>%

  # find the maximum value by label
  group_by(labels) %>%
  mutate(max=max(Dem, Rep)) %>%
  ungroup() %>%

  # sort the labels by gap value
  # note that its absolute value of gap
  mutate(labels=forcats::fct_reorder(labels, abs(gap)))

# make into long format for easier plotting
df=df %>%
  pivot_longer(
    c(Dem,Rep)
  )

df

df %>%

  # the following 3 lines of code are the same
  ggplot(aes(x=value,y=labels)) +
  geom_line(aes(group=labels), color="#E7E7E7", linewidth=3.5) +
  geom_point(aes(color=name), size=3) +

  # but we want geom_text for the data callouts and the legend

  # data callout
  geom_text(aes(label=value, color=name),
            size=7,
            nudge_x=if_else(
              df$value==df$max, # if it's the larger value...
              .6,   # move it to the right of the point
              -.6), # otherwise, move it to the left of the point
            hjust=if_else(
              df$value==df$max, #if it's the larger value
              0, # left justify
              1),# otherwise, right justify
  )+

  # legend
  geom_text(aes(label=name, color=name),
            data=. %>% filter(gap==max(gap)),
            nudge_y =.5,
            fontface="bold",
            size=7)+

  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(color="black", size = 14),
        axis.text.x = element_text(color="#989898", size = 14),
        axis.title = element_blank(),
        panel.grid = element_blank(),
  ) +
  labs(x="%",y=NULL) +
  scale_color_manual(values=c("#436685", "#BF2F24")) +

  #extend the y-axis otherwise the legend is cut off
  coord_cartesian(ylim=c(1, 7.5)) +

  #display percentages with % appended
  scale_x_continuous(labels = scales::percent_format(scale = 1))

########################
# fancy plot 2

install.packages('ggstatsplot')
install.packages('palmerpenguins')
library(ggstatsplot)
library(palmerpenguins)

# data were collected and made available by Dr. Kristen Gorman and the Palmer Station, Antarctica LTER,
# a member of the Long Term Ecological Research Network.
# This dataset was popularized by Allison Horst in her R package palmerpenguins
# with the goal to offer an alternative to the iris dataset for data exploration and visualization.

data("penguins", package = "palmerpenguins")

penguins <- drop_na(penguins)

plt <- ggbetweenstats(
  data = penguins,
  x = species,
  y = bill_length_mm
)

# show the distribution of Bill length for the three species of penguins in the dataset (Adelie, Chinstrap, and Gentoo).
# The function ggbetweenstats in the ggstatsplot is a great fit for this goal. Let’s see how it works.
plt <- plt +
  # Add labels and title
  labs(
    x = "Penguins Species",
    y = "Bill Length",
    title = "Distribution of bill length across penguins species"
  ) +
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Roboto", size = 8, color = "black"),
    plot.title = element_text(
      family = "Lobster Two",
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Roboto",
      size = 15,
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  )

plt <- plt  +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )

plt
ggsave("./penguin.png",dpi=300,height=7,width=12)

###########################################
# Maps

# This visualization shows the Human Development Index (HDI) at the subregional level in Sao Paulo, Brazil’s largest city.
# The values follow the standard United Nations’s HDI: values are in the 0 to 1 range.
# The visualization combines a choropleth map with a bar chart using the patchwork package.

install.packages('ggthemes')
install.packages('patchwork')
install.packages('readr')
install.packages('sf')

library(ggthemes)
library(patchwork)
library(readr)
library(sf)

atlas <- readr::read_rds(
  "https://github.com/viniciusoike/restateinsight/raw/main/static/data/atlas_sp_hdi.rds"
)

# basic map
ggplot(atlas) +
  geom_sf(aes(fill = HDI), lwd = 0.05, color = "white")


pmap <- ggplot(atlas) +
  geom_sf(aes(fill = HDI), lwd = 0.05, color = "white") +
  scale_fill_fermenter(
    name = "",
    breaks = seq(0.65, 0.95, 0.05),
    direction = 1,
    palette = "YlGnBu"
  ) +
  labs(
    title = "HDI in Sao Paulo, BR (2010)",
    subtitle = "Microregion HDI in Sao Paulo",
    caption = "Source: Atlas Brasil"
  ) +
  theme_map() +
  theme(
    # Legend
    legend.position = "top",
    legend.justification = 0.5,
    legend.key.size = unit(1.25, "cm"),
    legend.key.width = unit(1.75, "cm"),
    legend.text = element_text(size = 12),
    legend.margin = margin(),
    # Increase size and horizontal alignment of the both the title and subtitle
    plot.title = element_text(size = 28, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

pmap

# create bar chart

# Calculate population share in each HDI group
pop_hdi <- atlas |>
  st_drop_geometry() |>
  mutate(
    group_hdi = findInterval(HDI, seq(0.65, 0.95, 0.05), left.open = FALSE),
    group_hdi = factor(group_hdi)
  ) |>
  group_by(group_hdi) |>
  summarise(score = sum(pop, na.rm = TRUE)) |>
  ungroup() |>
  mutate(share = score / sum(score) * 100) |>
  na.omit()

# Create a variable to store the position of the text label
pop_hdi <- pop_hdi |>
  mutate(
    y_text = if_else(group_hdi %in% c(0, 7), share + 3, share - 3),
    label = paste0(round(share, 1), "%")
  )

# Labels for the color legend
x_labels <- c(
  "0.650 or less", "0.650 to 0.699", "0.700 to 0.749", "0.750 to 0.799",
  "0.800 to 0.849", "0.850 to 0.899", "0.900 to 0.949", "0.950 or more"
)

pcol <- ggplot(pop_hdi, aes(group_hdi, share, fill = group_hdi)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  geom_text(
    aes(y = y_text, label = label, color = group_hdi),
    size = 4
  ) +
  coord_flip() +
  scale_x_discrete(labels = x_labels) +
  scale_fill_brewer(palette = "YlGnBu") +
  scale_color_manual(values = c(rep("black", 5), rep("white", 2), "black")) +
  guides(fill = "none", color = "none") +
  labs(
    title = "Population share by HDI group",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 14),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_blank()
  )

pcol

p_hdi_atlas <-
  pmap + inset_element(pcol, left = 0.50, bottom = 0.05, right = 1, top = 0.5)

p_hdi_atlas

