path = 'C:/Users/Bruin/Documents/GitHub/UI_STAT251-03_Spring_2024/docs/shiny_apps/categorical_tests'
outpath = 'C:/Users/Bruin/Documents/GitHub/UI_STAT251-03_Spring_2024/docs/shiny_apps/categorical_tests/webfiles/'
shinylive::export(appdir = path, destdir = outpath)
httpuv::runStaticServer("C:/Users/Bruin/Documents/GitHub/UI_STAT251-03_Spring_2024/docs/shiny_apps/categorical_tests/webfiles/")
