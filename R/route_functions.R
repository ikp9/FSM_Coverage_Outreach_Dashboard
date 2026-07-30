haversine_km <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  dlon <- (lon2 - lon1) * rad
  dlat <- (lat2 - lat1) * rad
  a <- sin(dlat/2)^2 + cos(lat1*rad) * cos(lat2*rad) * sin(dlon/2)^2
  6371 * 2 * atan2(sqrt(a), sqrt(1-a))
}

nearest_neighbor_route <- function(stops, hub_lon, hub_lat, speed_kmh = 25,
                                   service_minutes_child = 4, setup_minutes = 30,
                                   workday_hours = 8, teams = 1) {
  if (nrow(stops) == 0) return(list(itinerary = stops, summary = data.frame()))
  stops <- stops[!is.na(stops$longitude) & !is.na(stops$latitude), , drop = FALSE]
  stops$team <- rep(seq_len(max(1, teams)), length.out = nrow(stops))
  all_routes <- list()
  for (tm in sort(unique(stops$team))) {
    remaining <- stops[stops$team == tm, , drop = FALSE]
    cur_lon <- hub_lon; cur_lat <- hub_lat; elapsed <- 0; seq_no <- 1
    route <- list()
    while (nrow(remaining) > 0) {
      d <- mapply(haversine_km, cur_lon, cur_lat, remaining$longitude, remaining$latitude)
      j <- which.min(d)
      x <- remaining[j, , drop = FALSE]
      travel_min <- d[j] / speed_kmh * 60
      service_min <- setup_minutes + service_minutes_child * x$children_not_utd
      elapsed <- elapsed + travel_min + service_min
      x$team <- tm; x$stop_sequence <- seq_no; x$leg_distance_km <- d[j]
      x$travel_minutes <- travel_min; x$service_minutes <- service_min
      x$cumulative_hours <- elapsed / 60
      x$day <- ceiling(x$cumulative_hours / workday_hours)
      route[[length(route)+1]] <- x
      cur_lon <- x$longitude; cur_lat <- x$latitude; seq_no <- seq_no + 1
      remaining <- remaining[-j, , drop = FALSE]
    }
    all_routes[[length(all_routes)+1]] <- do.call(rbind, route)
  }
  itinerary <- do.call(rbind, all_routes)
  itinerary <- itinerary[order(itinerary$team, itinerary$stop_sequence), ]
  summary <- data.frame(
    teams = length(unique(itinerary$team)),
    communities = nrow(itinerary),
    children_targeted = sum(itinerary$children_not_utd, na.rm = TRUE),
    total_distance_km = round(sum(itinerary$leg_distance_km, na.rm = TRUE), 1),
    total_travel_hours = round(sum(itinerary$travel_minutes, na.rm = TRUE)/60, 1),
    total_service_hours = round(sum(itinerary$service_minutes, na.rm = TRUE)/60, 1),
    maximum_route_days = max(itinerary$day, na.rm = TRUE)
  )
  list(itinerary = itinerary, summary = summary)
}
