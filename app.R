# This file is just an entry point into the app. It technically creates a
# single file app, while still having almost all of the structure of a
# multi-file app. Its it still uses `ui.R` and `server.R`, just like in
# a standard two-file R Shiny app.

# A single entry point in the app's root directory is a requirement of
# deployment on Posit Connect Cloud.

print(Sys.getlocale("LC_ALL"))

shinyApp(ui, server) # nolint
# profvis::profvis(runApp(shinyApp(ui, server)))
