path = 'C:/Users/Bruin/Documents/GitHub/UI_STAT251-03_Spring_2024/docs/shiny_apps/Regression/'
outpath = 'C:/Users/Bruin/Documents/GitHub/UI_STAT251-03_Spring_2024/docs/shiny_apps/Regression/webfiles/'
shinylive::export(appdir = path, destdir = outpath)
httpuv::runStaticServer("C:/Users/Bruin/Documents/GitHub/UI_STAT251-03_Spring_2024/docs/shiny_apps/Regression/webfiles/")
