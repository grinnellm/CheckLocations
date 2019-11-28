# Spawn a 32-bit R sesstion to run `Locations.R`
# system( paste0(Sys.getenv("R_HOME"), "/bin/i386/Rgui.exe"),
#         wait=FALSE, invisible=FALSE )
x <- Sys.getenv("R_HOME")
system( paste0("C:/Applications/R/R-3.6.1/bin/i386/Rgui.exe Locations.R"),
        wait=FALSE, invisible=FALSE )
