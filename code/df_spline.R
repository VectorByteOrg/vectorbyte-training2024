library(tidyverse)
# setup_x1.iso: an alternative setup to x1. Here we take x1 = iso_week
fx1.iso <- function(datetime){
  startdate <- min(datetime) # identify start week
  x1 <- as.numeric(stringr::str_sub(ISOweek::ISOweek(datetime),7, 8)) # find iso week #
  return(x1)
}

site_data <- readr::read_csv(paste0("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/",
                                    "main/NEON_Field_Site_Metadata_20220412.csv")) |> 
  dplyr::filter(ticks == 1)

site.info <- cbind.data.frame(site_data[,c("field_site_id","field_latitude", "field_longitude", "field_mean_elevation_m")],
                              site_data[,c("field_avg_grean_increase_doy", "field_avg_green_decrease_doy",
                                                 "field_avg_number_of_green_days", "field_avg_green_max_doy", "field_avg_green_min_doy")])


colnames(site.info) <- c("site","lat", "long", "alt", "g.inc", "g.dec", "g.avg", "g.max", "g.min")


sites_sum <- site.info
df <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/ticks/ticks-targets.csv.gz", guess_max = 1e1)
df <- df[, c("datetime", "site_id", "observation")]
colnames(df) <- c("datetime", "site", "y")

cutoff <- "2020-12-31"
sites_sum[,5:9] <- apply(sites_sum[,5:9], 2, function(x) as.numeric(unlist(x))/7)
loc <- unique(df$site)
n.site <- length(loc)

df <- df[df$datetime<=cutoff,]

X <- sites_sum[,c("g.inc","g.max","g.dec","g.min")]
X <- floor(X)
rownames(X) <- loc

df$iso <- fx1.iso(df$datetime)

Y <- matrix(NA,ncol = ncol(X), nrow=nrow(X))
rownames(Y) <- rownames(X)

for(i in 1:n.site){
  
  df.loc <- subset(df,df$site==loc[i])
  # y1l <- c(df.loc$y[which(df.loc$iso == X[i,1])])
  # y2l <- c(df.loc$y[which(df.loc$iso == X[i,2])])
  # y3l <- c(df.loc$y[which(df.loc$iso == X[i,3])])
  # y4l <- c(df.loc$y[which(df.loc$iso == X[i,4])])
  y1l <- c(df.loc$y[which(df.loc$iso == X[i,1])],df.loc$y[which(df.loc$iso == (X[i,1]+1))])
  y2l <- c(df.loc$y[which(df.loc$iso == X[i,2])],df.loc$y[which(df.loc$iso == (X[i,2]+1))])
  y3l <- c(df.loc$y[which(df.loc$iso == X[i,3])],df.loc$y[which(df.loc$iso == (X[i,3]+1))])
  y4l <- c(df.loc$y[which(df.loc$iso == X[i,4])],df.loc$y[which(df.loc$iso == (X[i,4]+1))])
  
  if(length(y1l) == 0)
    y1 <- 0
  else 
    y1 <- mean(y1l)
  
  if(length(y2l) == 0)
    y2 <- 0
  else 
    y2 <- mean(y2l)
  
  if(length(y3l) == 0)
    y3 <- 0
  else 
    y3 <- mean(y3l)
  
  if(length(y4l) == 0) 
    y4 <- 0
  else 
    y4 <- mean(y4l)
  
  Y[i,] <- c(y1,y2,y3,y4)
}

X1 <- rep(1, 9)
Y1 <- rep(0, 9)
X6 <- rep(53, 9)
Xs <- cbind(X1,X,X6)
Ys <- cbind(Y1,Y,Y1)

df_sp <- NULL
for(i in 1:n.site){
  
  X.site <- as.numeric(Xs[i,])
  Y.site <- as.numeric(Ys[i,])
  
  cubic.spline <- spline(X.site,Y.site,n=53, method = "natural")
  
  if(Y.site[5] == 0)
    cubic.spline$y[cubic.spline$x>X.site[5]] = 0
  if(Y.site[2] == 0)
    cubic.spline$y[cubic.spline$x<X.site[2]] = 0
  cubic.spline$y[cubic.spline$y<0] = 0 
  
  # plot(as.numeric(X.site),as.numeric(Y.site), main = loc[i], xlab = "week",
  #      ylab = "observation")
  # lines(cubic.spline$x,cubic.spline$y, type = 'l')

  dfs <- cbind.data.frame(site=rep(loc[i],53),iso = 1:53, ker = cubic.spline$y)
  df_sp <- rbind(df_sp,dfs)
}

site.info <- sites_sum[,1:4]

df_sp <- df_sp |> left_join(site.info,"site") 
df_green <- df_sp[, 1:3]

# write.csv(df_sp, '../../csv/df_sp.csv')
