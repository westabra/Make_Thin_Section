# Use 5_Configure_paths.cmd to choose a database without editing R code.
# Priority: PETRO_DB_PATH > database_path.txt > MTS_DATABASE below.
# This legacy fallback is R code; use forward slashes if editing it manually.
MTS_DATABASE <- "Make_Thin_Section_demo.sqlite"
MTS_LOCAL_PORT <- 3081L
# For LAN access, replace this with the serving computer's actual LAN IPv4 address.
# Example only: MTS_LAN_HOST <- "192.168.1.25"
MTS_LAN_HOST <- "127.0.0.1"
MTS_LAN_PORT <- 3080L
