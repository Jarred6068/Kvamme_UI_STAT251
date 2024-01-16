path = 'C:/Users/Bruin/Documents/GitHub/UI_STAT251-03_Spring_2024/docs/shiny_apps/Freq_table/'
outpath = 'C:/Users/Bruin/Documents/GitHub/UI_STAT251-03_Spring_2024/docs/shiny_apps/Freq_table/webfiles/'
shinylive::export(appdir = path, destdir = outpath)
httpuv::runStaticServer("C:/Users/Bruin/Documents/GitHub/UI_STAT251-03_Spring_2024/docs/shiny_apps/Freq_table/webfiles/")



