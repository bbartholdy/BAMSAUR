#'Function to launch the BAMSAUR shinyapp
#'
#'This function launches the shiny graphical user interface (GUI) for the BAMSAUR package.
#'@details The GUI is based on the functions available in the BAMSAUR package and has the same utility as the stand-alone functions. It is provided as a user-friendly alternative for less experienced R-users.
#'@importFrom shiny runApp
#'@example inst/runBAMex.R
#'@export runBAM
runBAM <- function(){
  runApp(system.file("BAMSAURapp", package = "BAMSAUR"))
}
