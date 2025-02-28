# script to buffer wind/solar points and then analyse what change in LAI
# and forest cover pre and post construction

# LAI from point data
# which has spatial resolution of ~6-8km! So, not very high, points in essence already buffered
hist(windsolar$mean_LAI_0103) # mostly low LAI values
hist(windsolar$mean_LAI_2123) # mostly low still, some higher
hist(windsolar$difLAI) # mostly close to 0, some -ve but most +ve (so, increase in LAI from 0103 to 2123, does this make sense???)

# feel like comparing LAI pre and post development is too coarse
# especially given coarse spatial resolution
# SO, need to work out Hansen forest mapping
# can then compare pre- and post- development forest cover for wind/solar locations?

# may want to BUFFER POINTS to pick up area
# but have checked that all points are currently in the middle of the impact area
# which differs in magnitude for wind and solar - dif buffer sizes maybe?


# get spatial points
windsolarV <- sf::st_as_sf(windsolar, coords = c("lon", "lat"), remove = FALSE, crs = "EPSG:4326")
windsolarV


# TO DOs
#   - try buffering (prop to MW?) with Hansen forest cover mapping, from ~1998 (earliest start year) to most recent, to see change in cover
#   - possible to match years more tightly?? could filter??