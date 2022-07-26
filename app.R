library(shiny)
library(ggplot2)
library(tidyverse)
library(gganimate)
library(transformr)
library(tmap)
library(data.table)
library(plotly)
library(sf)
library(sicegar)
library(glue)

if (RCurl::url.exists("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")) {
    df <- fread('https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv') %>% 
        filter(!is.na(as.numeric(totale_casi)))
}else{
    print("The URL does not exist. Check the repository")
}

if (RCurl::url.exists("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")) {
    df_reg <- fread('https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv')
}else{
    print("The URL does not exist. Check the repository")
}

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
    if(length(x) != 1) stop("'x' must be of length 1")
    22^floor(log10(x)) * nice[[which(x <= 22^floor(log10(x)) * nice)[[1]]]]
}


yesterday <- format(Sys.Date()-1,"%d-%b")
today <- format(Sys.Date(),"%d-%b")

### total cases country level
df %>% 
    group_by(data) %>% 
    summarise(tot=sum(totale_casi,na.rm=TRUE)) %>% 
    rownames_to_column() %>% 
    mutate(rowname=as.numeric(rowname)-1,
           data = format(as.Date(data)))-> df_tot
##### fitting exponential exp(intercept+coefficient2*x)
exponential.model <- lm(log(df_tot$tot[df_tot$tot != 0])~ as.numeric(df_tot$rowname[df_tot$tot != 0]))
exp_pred <- function(x){
    prediction <- exp(exponential.model$coefficients[1]+exponential.model$coefficients[2]*x)
    names(prediction) <- glue::glue("exp_{x}")
    return(prediction)
}
##### fitting polynomial ax^2+bx+c
poly_fit <- lm(df_tot$tot[df_tot$tot != 0] ~ poly(as.numeric(df_tot$rowname[df_tot$tot != 0]), 2, raw=TRUE))
a <- poly_fit$coefficients[3]
b <- poly_fit$coefficients[2]
c <- poly_fit$coefficients[1]
poly_pred <- function(x){
    predictions <- a*x*x+b*x+c
    names(predictions) <- glue::glue("poly_{x}")
    return(predictions)
}
##### fitting logistic regression
scaled_data <-   normalizeData(df_tot %>% 
                                   data.frame() %>% 
                                   rename("time" = 'rowname',
                                          'intensity'='tot') %>% 
                                   select(time,intensity) %>% 
                                   mutate(time=as.integer(time)))

fit_par <-   sigmoidalFitFunction(scaled_data,
                                  tryCounter = 10)




# Define UI for application that draws a histogram
ui <- fluidPage(theme = "my_theme.css",
                

    # Application title
    titlePanel("COVID-19 spreading and projections for Italy"),

    # Sidebar with a slider input for days of prediction
    # Select input from which select the province
             sidebarLayout(
                sidebarPanel(
                    selectizeInput("provincia",
                                "Select province of interest:",
                                choices = list('List of province'= c("Select province",unique(df$denominazione_provincia))),
                                selected = "Milano",
                                multiple = TRUE
                                ),
                    #Insert slider to 
                    sliderInput('day_proj',
                                '# Days to project',
                                min = 1,
                                max = 15,
                                value = 7,
                                step=1,
                                ticks=FALSE
                                ),
                    selectizeInput("regione",
                                   "Select region of interest:",
                                   choices = list('List of regions'= c("",unique(df_reg$denominazione_regione))),
                                   selected = "Lombardia",
                                   multiple = TRUE
                    ),
                    checkboxGroupInput("type_data",
                                       "Variable to visualize:",
                                       selected = "Deceduti",
                                       choiceNames =
                                           list('Deaths',
                                                'Hospitalized',
                                                'ICU'),
                                       choiceValues =
                                           list("Deceduti", 
                                                "Totale Ospedalizzati",
                                                "Terapia intensiva")),
                    selectizeInput(inputId="logscale",
                                 label="Y-axis scale",
                                 choices = list("Linear","Logarithmic"),
                                 selected = "Logarithmic"),
                    helpText(HTML('<p style="font-weight:1000;color:red;font-size:16px;">Province tab:</p> 
                                  Plot 1) Daily increase of positive case. 2) Total number of positive cases.')),
                    helpText(HTML('<p style="font-weight:1000;color:red;font-size:16px;">Region tab:</p> 
                             Plot 1) Daily increase of: Hospitalized, ICU and deaths by region.
                             Plot 2) Total number of Hospitalized, ICU and deaths by region')),    
                    helpText(HTML('<p style="font-weight:1000;color:red;font-size:16px;">Nation tab:</p> 
                             Plot 1) Cumulative number of infected people and projection up to 15 days for 3 model: Exponential,
                             2nd degree Polynomial and Logistic. Map 1) Distribution by province of the number of infected')),
                    helpText(HTML('<p style="font-weight:1000;color:red;font-size:16px;">Health Infrastructures tab:</p> 
                             Map 1) Ratio between the number of people in ICU and the total number of ICU beds at regional scale*.
                             Map 2) Number of people in deaths for each region.
                                  <p style="font-style:italic;color:green;font-size:12px;">*Data on the number of ICU beds is continously increasing,
                                  and are recovered from "unofficial" source (i.e. region.it, newspapers)</p>'))
                    
                    
                    
                    
                    
                ),
            
                # SPlit the main panel into 4 tabs: Province, Region, Country, Infrastructure
                mainPanel(
                  tabsetPanel(
                      tabPanel("Province",plotlyOutput("daily_increase"),plotlyOutput("totale_cont")),
                      tabPanel('Region',plotlyOutput("increase_reg"),plotlyOutput("totale_reg")),
                      tabPanel("Nation", plotlyOutput("proj_cont"), tmap::tmapOutput("map_proj")),
                      tabPanel("Health Infrastructure",tmap::tmapOutput("tot_posti"),tmap::tmapOutput("posti_surg"))
                  ),
                   tableOutput("pred")
                )
            )
)

# Define server fucntion
server <- function(input, output) {
    #=========#=========#=========#=========#=========#=========#=========#=========#
    #  Prediction table                                                             #
    #=========#=========#=========#=========#=========#=========#=========#=========#
    if (as.POSIXct(Sys.time()) > as.POSIXct(paste(Sys.Date(),"18:00:00"))) {
        #### Dalle 18 voglio passare alla previsione del giorno successivo 
        output$pred <-function(){
            data.frame("Today"= format(Sys.Date()+1,"%d-%b"),
                       "Exp"=exp_pred(df_tot$rowname[as.Date(df_tot$data) %in% c(Sys.Date())]+1) %>% 
                           round(0) %>% as.integer(),
                       "Poly"=poly_pred(df_tot$rowname[as.Date(df_tot$data) %in% c(Sys.Date())]+1) %>% 
                           round(0) %>% as.integer(),
                       "Yesterday"= format(Sys.Date(),"%d-%b"),
                       "Real"=ifelse(Sys.Date() %in% as.Date(df_tot$data), 
                                     df_tot$tot[as.Date(df_tot$data) %in% c(Sys.Date())],
                                     "Still waiting for updated data"),
                       "Exp"=exp_pred(df_tot$rowname[as.Date(df_tot$data) %in% c(Sys.Date())]) %>%
                           round(0) %>% as.integer(),
                       "Poly"=poly_pred(df_tot$rowname[as.Date(df_tot$data) %in% c(Sys.Date())]) %>% 
                           round(0) %>% as.integer(),
                       check.names = FALSE) %>% 
                                     knitr::kable(format = 'html',) %>% 
                kableExtra::kable_styling(bootstrap_options = c("bordered","striped","hover"),
                                          full_width = TRUE,
                                          position = 'center',font_size = 15) %>% 
                kableExtra::column_spec(column = c(1,4),bold=T,color = 'black',background = 'azure') %>% 
                kableExtra::column_spec(column = c(2,3),bold=F,color = 'red',background = 'azure') %>% 
                kableExtra::column_spec(column = 5,bold=F,color = 'green',background = 'azure') %>% 
                kableExtra::column_spec(column = c(6,7),bold=F,color = 'blue',background = 'azure') %>% 
                kableExtra::row_spec(0,bold=T,color = 'black',background = 'azure') 
    }
        }else{

        output$pred <- function(){
            data.frame("Today"=  format(Sys.Date(),"%d-%b"),
                       "Exp"=exp_pred(df_tot$rowname[as.Date(df_tot$data) %in% (Sys.Date()-1)]+1) %>% 
                           round(0) %>% as.integer(),
                       "Poly"=poly_pred(df_tot$rowname[as.Date(df_tot$data) %in% (Sys.Date()-1)]+1) %>% 
                           round(0) %>% as.integer(),
                       "Yesterday"=  format(Sys.Date()-1,"%d-%b"),
                       "Real"=df_tot$tot[as.Date(df_tot$data) == Sys.Date()-1],
                       "Exp"=exp_pred(df_tot$rowname[as.Date(df_tot$data) %in% (Sys.Date()-1)]) %>% 
                           round(0) %>% as.integer(),
                       "Poly"=poly_pred(df_tot$rowname[as.Date(df_tot$data) %in% (Sys.Date()-1)]) %>% 
                           round(0) %>% as.integer(),
                       check.names = FALSE) %>% 
                knitr::kable(format = 'html',) %>% 
                kableExtra::kable_styling(bootstrap_options = c("bordered","striped","hover"),
                                          full_width = TRUE,
                                          position = 'center',font_size = 15) %>% 
                kableExtra::column_spec(column = c(1,4),bold=T,color = 'black',background = 'azure') %>% 
                kableExtra::column_spec(column = c(2,3),bold=F,color = 'red',background = 'azure') %>% 
                kableExtra::column_spec(column = 5,bold=F,color = 'green',background = 'azure') %>% 
                kableExtra::column_spec(column = c(6,7),bold=F,color = 'blue',background = 'azure') %>% 
                kableExtra::row_spec(0,bold=T,color = 'black',background = 'azure') 
            
        }
    }
    
    #=========#=========#=========#=========#=========#=========#=========#=========#
    #     PROVINCE LEVEL                                                            #
    #=========#=========#=========#=========#=========#=========#=========#=========#
    
    # Define plot daily increas
    output$daily_increase <- renderPlotly({
         df %>% 
            arrange(denominazione_provincia) %>% 
            mutate(lagged = lag(totale_casi),
                   daily_increase = ifelse((totale_casi - lagged) < 0, 0, totale_casi-lagged )) %>% 
            filter(denominazione_provincia %in% input$provincia) %>% 
            ggplot(aes(x=as.Date(data),y=daily_increase,col=denominazione_provincia))+
            geom_point()+
            geom_line()+
            # scale_x_date(breaks = seq.Date(as.Date("2020-02-24"),Sys.Date(),by=2),
            #              labels = format(seq.Date(as.Date("2020-02-24"),Sys.Date(),by=2),"%d-%b"))+
            scale_colour_discrete("Provincia")+
            ylab('Aumento contagiati giornaliero')+
            xlab('')+
            theme_bw()+
            theme(panel.background = element_rect(fill='azure'),
                  axis.text = element_text(size = 12 ),
                  axis.title = element_text(size = 18))
    })
    # Define plot total count  
    output$totale_cont <- renderPlotly({
        df %>% 
            arrange(denominazione_provincia) %>% 
            filter(denominazione_provincia %in% input$provincia) %>% 
            rownames_to_column() %>% 
            ggplot(aes(x=as.Date(data),y=totale_casi,col=denominazione_provincia))+
            geom_point()+
            geom_line()+
            # scale_x_date(breaks = seq.Date(as.Date("2020-02-24"),Sys.Date(),by=2),
            #              labels = format(seq.Date(as.Date("2020-02-24"),Sys.Date(),by=2),"%d-%b"))+
            scale_colour_discrete("Provincia")+
            ylab('Totale contagiati')+
            xlab('')+
            theme_bw()+
            theme(panel.background = element_rect(fill='azure'),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 18))
    })

    #=========#=========#=========#=========#=========#=========#=========#=========#
    #     REGION LEVEL                                                              #
    #=========#=========#=========#=========#=========#=========#=========#=========#
    # Define plot daily increas
    output$increase_reg <- renderPlotly({
        df_reg %>% 
            arrange(denominazione_regione) %>% 
            mutate(lag_dec = lag(deceduti),
                   Deceduti = ifelse((deceduti - lag_dec) < 0, 0, deceduti-lag_dec ),
                   lag_icu = lag(terapia_intensiva),
                   `Terapia intensiva` = ifelse((terapia_intensiva - lag_icu) < 0, 0, terapia_intensiva-lag_icu),
                   lag_osp = lag(totale_ospedalizzati),
                   `Totale Ospedalizzati` = ifelse((totale_ospedalizzati-lag_osp) < 0, 0, totale_ospedalizzati-lag_osp)) %>% 
            select(data,denominazione_regione,Deceduti,`Terapia intensiva`,`Totale Ospedalizzati`) %>% 
            melt(id.vars=c("data","denominazione_regione")) %>% 
            filter(denominazione_regione %in% input$regione,
                   variable %in% input$type_data) %>%
            ggplot(aes(x=as.Date(data),y=value,
                       shape = denominazione_regione,
                       col=variable,
                       linetype=denominazione_regione))+
            geom_point(size = 3)+
            geom_line(size = 1.25)+
            # scale_x_date(breaks = seq.Date(as.Date("2020-02-24"),Sys.Date(),by=2),
            #              labels = format(seq.Date(as.Date("2020-02-24"),Sys.Date(),by=2),"%d-%b"))+
            scale_colour_discrete("")+
            scale_shape_discrete("")+
            scale_linetype_discrete("")+
            ylab('Aumento giornaliero')+
            xlab('')+
            theme_bw()+
            theme(panel.background = element_rect(fill='azure'),
                  axis.text = element_text(size = 12 ),
                  axis.title = element_text(size = 18)) -> gg_daily_Reg
        
        ggplotly(gg_daily_Reg)
    })
    # Define plot total count
    output$totale_reg <- renderPlotly({
        if (input$logscale == "Logarithmic") {
            df_reg %>%
                arrange(denominazione_regione) %>%
                filter(denominazione_regione %in% input$regione) %>%
                rownames_to_column() %>%
                select(data,denominazione_regione,deceduti,terapia_intensiva,totale_ospedalizzati) %>%
                rename('Deceduti'= 'deceduti',
                       'Totale Ospedalizzati'='totale_ospedalizzati',
                       'Terapia intensiva'='terapia_intensiva') %>% 
                melt(id.vars=c("data","denominazione_regione")) %>%
                filter(variable %in% input$type_data) %>% 
                ggplot(aes(x=as.Date(data),y=value,col=variable,shape = denominazione_regione))+
                geom_point(size=3)+
                geom_line()+
                scale_y_log10()+
                scale_colour_discrete("")+
                scale_shape("")+
                ylab('Numero di casi (log scale)')+
                xlab('')+
                theme_bw()+
                theme(panel.background = element_rect(fill='azure'),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 18)) -> ggreg
            ggplotly(ggreg)
        }else{
            df_reg %>%
                arrange(denominazione_regione) %>%
                filter(denominazione_regione %in% input$regione) %>%
                rownames_to_column() %>%
                select(data,denominazione_regione,deceduti,terapia_intensiva,totale_ospedalizzati) %>%
                rename('Deceduti'= 'deceduti',
                       'Totale Ospedalizzati'='totale_ospedalizzati',
                       'Terapia intensiva'='terapia_intensiva') %>% 
                melt(id.vars=c("data","denominazione_regione")) %>%
                filter(variable %in% input$type_data) %>% 
                ggplot(aes(x=as.Date(data),y=value,col=variable,shape = denominazione_regione))+
                geom_point(size=3)+
                geom_line()+
                scale_colour_discrete("")+
                scale_shape("")+
                ylab('Numero di casi')+
                xlab('')+
                theme_bw()+
                theme(panel.background = element_rect(fill='azure'),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 18)) -> ggreg
            ggplotly(ggreg)
        }
    })

    

    
    
    #=========#=========#=========#=========#=========#=========#=========#=========#
    ######     COUNTRY LEVEL                                           #######      #
    #=========#=========#=========#=========#=========#=========#=========#=========#
    # PRojection for entire country
    output$proj_cont <- renderPlotly({

       left_join(df_tot %>% 
                     rename("Reali"="tot") %>% 
                     rbind(.,data.frame(rowname=seq(max(df_tot$rowname)+1,length.out = input$day_proj-1),
                                        data=as.character(seq.Date(from=as.Date(df_tot$data[length(df_tot$rowname)])+1,by='days',length.out = input$day_proj-1)),
                                        Reali = NA)) ,
                 data.frame(rowname=seq(max(df_tot$rowname)-2,length.out = input$day_proj+2),
                            expo=exp_pred(seq(df_tot$rowname[length(df_tot$rowname)-2],length.out = input$day_proj+2))),
                 by=c("rowname")) %>% 
           left_join(.,
                     data.frame(rowname=seq(max(df_tot$rowname)-2,length.out = input$day_proj+2),
                                poly=poly_pred(seq(df_tot$rowname[length(df_tot$rowname)-2],length.out = input$day_proj+2)))) %>%
           left_join(.,
                     data.frame(rowname=seq(max(df_tot$rowname)-2,length.out = input$day_proj+2),
                                logistic=sigmoidalFitFormula(seq(max(df_tot$rowname)-2,length.out = input$day_proj+2),
                                                             maximum = fit_par$maximum_Estimate,
                                                             slopeParam = fit_par$slopeParam_Estimate,
                                                             midPoint = fit_par$midPoint_Estimate))) %>%     
           melt(id.vars=c("rowname",'data')) %>% 
           filter(!is.na(value)) %>% 
           mutate(value = round(value,0)) %>% 
           rename("Model" = "variable",
                   "Contagiati"="value")-> df_proj 
           ggplot(data=df_proj %>% mutate(data = as.Date(data)),
                  aes(x=data,y=Contagiati,col=Model))+
           geom_point()+
           geom_line()+
           scale_color_discrete('')+
           guides(alpha="none")+
           xlab("")+
           ylab("Totale Contagiati")+
           #ylim(0,sum(df$totale_casi[as.Date(df$data) == Sys.Date() | as.Date(df$data) == Sys.Date()-1], na.rm=TRUE) %>% roundUpNice())+
           ggtitle(glue("Proiezione fino al {format(max(as.Date(df_proj$data)),'%d-%B')}"))+
           theme_bw()+
           theme(panel.background = element_rect(fill='azure'),
                 axis.title = element_text(size = 12 )) -> gg1
       
       ggplotly(gg1)
    })
# There will be a map right now just the same plot
    output$map_proj <- tmap::renderTmap({
        shp_it <- st_read("gadm36_ITA_2.shp")
        tmap_mode("view")
        df %>% 
            filter(denominazione_provincia != 'In fase di definizione/aggiornamento',
                   !is.na(long)) %>% 
            group_by(data,denominazione_provincia,long,lat) %>% 
            summarise(tot=max(totale_casi,na.rm=T)) %>% 
            st_as_sf(coords=c('long','lat'),crs=4326) %>% 
            rownames_to_column() %>% 
            mutate(rowname=as.numeric(rowname)-1,
                   data_2 = as.Date(data)) %>% 
            filter(ifelse(Sys.Date() %in% data_2,
                          data_2 == Sys.Date(),
                          data_2 == Sys.Date()-1)) %>%
            st_cast(.,to="POINT") %>%  
            st_intersection(.,shp_it) %>% 
            st_set_geometry(NULL) %>%
            select(NAME_2,tot) %>% 
            left_join(shp_it,.) %>% 
            filter(!is.na(tot)) %>% 
            tm_shape()+tm_polygons(col=('tot'),
                                   palette="Reds",
                                   style='cont',
                                   title = 'N° Contagiati',
                                   n=5,
                                   popup.vars = c("Prov"="NAME_2",
                                                  "N° Cont." = "tot")
                                   )
        })
    #=========#=========#=========#=========#=========#=========#=========#=========#
    #     HEALTH INFRASTRUCTURES                                                    #
    #=========#=========#=========#=========#=========#=========#=========#=========#
    health <- fread("C_17_dataset_18_0_upFile.csv") 
    health$`Codice Regione` <- round(health$`Codice Regione`/10,0)
    icu_av <- fread('Icu.csv')
    shp_reg <- st_read("gadm36_ITA_1.shp")
    
    shp_reg$index <- 1:length(shp_reg$TYPE_1)
    #shp_reg
    
    st_intersection(shp_reg,
                    df_reg %>% 
                        st_as_sf(coords=c("long",'lat'),crs=4326) %>% 
                        filter(as.Date(data) == max(as.Date(data)))) %>% 
        dplyr::select(index:last_col()) %>% 
        st_join(shp_reg %>% dplyr::select(index),.,left=TRUE) %>% 
        left_join(.,health %>% 
                      dplyr::rename("codice_regione"=`Codice Regione`) %>% 
                      filter(!is.na(as.numeric(`Totale posti letto`))) %>% 
                      group_by(codice_regione) %>% 
                      dplyr::summarise(tot_posti=sum(as.numeric(`Totale posti letto`),na.rm=TRUE),
                                       tot_surg =sum(as.numeric(`Posti letto Day Surgery`),na.rm=TRUE),
                                       tot_surg =sum(as.numeric(`Posti letto Day Surgery`),na.rm=TRUE),
                                       tot_ord =sum(as.numeric(`Posti letto degenza ordinaria`),na.rm=TRUE)),
                  by="codice_regione") -> df_health
    
    output$tot_posti <- tmap::renderTmap({

        df_health %>% 
            left_join(.,icu_av[,2:5], by = 'codice_regione') %>% 
            mutate(r_osp_tot=terapia_intensiva/ICU) %>%
        tm_shape()+tm_polygons(col='r_osp_tot',
                               palette= c('blue','red'),
                               style='fixed',
                               title = 'Percentuale occupazione ospedali',
                               breaks = seq(0,1.25,0.25),
                               labels = paste0(seq(0,1.25,0.25)*100,'%'),
                               popup.vars = c("Regione"="denominazione_regione",
                                              "N° letti" = "tot_posti",
                                              "Tot. Ospedalizzati"= 'totale_ospedalizzati',
                                              "Terapia intensiva"='terapia_intensiva',
                                              'Posti ICU' = 'ICU',
                                              "% Occupazione ICU"='r_osp_tot'
                                              ))
        
    })
    
    output$posti_surg <- tmap::renderTmap({
        tm_shape(df_health)+tm_polygons(col='deceduti',
                                        palette="Blues",
                                        style='cont',
                                        title = 'Decessi',
                                        n=5,
                                        popup.vars = c("Regione"="denominazione_regione",
                                                       "N° letti" = "tot_posti",
                                                       "Tot. Ospedalizzati"= 'totale_ospedalizzati',
                                                       "Terapia intensiva"='terapia_intensiva'))
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
