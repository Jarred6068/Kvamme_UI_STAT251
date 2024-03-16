path = 'C:/Users/Bruin/Documents/GitHub/UI_STAT251-03_Spring_2024/docs/shiny_apps/Convert_file_to_text/'
outpath = 'C:/Users/Bruin/Documents/GitHub/UI_STAT251-03_Spring_2024/docs/shiny_apps/Convert_file_to_text/webfiles/'
shinylive::export(appdir = path, destdir = outpath)
httpuv::runStaticServer("C:/Users/Bruin/Documents/GitHub/UI_STAT251-03_Spring_2024/docs/shiny_apps/Convert_file_to_text/webfiles/")
