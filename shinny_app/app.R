# The Following Shiny Application Creates the State Constitutions Initiative App
# The app alows users to view tables on a SQLite db and perform CRUD operations

# LOAD LIBRARIES ----------------------------------------------------------
library(shiny)
library(shinydashboard)
library(dplyr)
library(glue)
library(shinyauthr)
library(RSQLite)
library(DBI)
library(lubridate)
library(fresh)
library(shinyjs)
library(DT)
library(shinyalert)
library(plotly)



# DB CONNECTION -----------------------------------------------------------

# Connect to RSQLite DB
conn <- dbConnect(RSQLite::SQLite(), "data/constitutionsdb.db")


# DASHBOARD THEME ---------------------------------------------------------

# Create Theme for the App
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#020205" #Primary status and header color
  ),
  adminlte_sidebar(
    width = "400px", #Width of sidebar
    dark_bg = "#020205", #Sidebar background color
    dark_hover_bg = "#f87060",
    dark_color = "#666370"
  ),
  adminlte_global(
    content_bg = "#FBF8F5", #Background Color
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)



# MODALS AND BUTTONS ------------------------------------------------------

# Create Buttons to Modify or Delte a Section
create_btns <- function(x) {
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button" id="edit_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                   <button class="btn btn-default action-button btn-danger action_button" id="delete_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button></div>'
                     ))
}

# Modal pop up to add Sections
modal_dialog <- function(section_id, constitution_id, section_year, article_num,
                         section_num, part_num, section_topic,section_text, edit) {
  if (edit) {
    x <- "Submit Edits"
  } else {
    x <- "Add New Section"
  }
  shiny::modalDialog(
    title = "Edit Constitution",
    div("Warning: All inputs must be filled before submitting!"),
    div(
      class = "text-center",
      div(
        style = "display: inline-block;",
        shiny::textInput(
          inputId = "section_id_val",
          label = "Section Id",
          value = section_id,
          placeholder = "Input Section Id",
          width = "400px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::textInput(
          inputId = "constitution_id_val",
          label = "Constitution Id",
          value = constitution_id,
          placeholder = "Input Constitution Id",
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId = "section_year_val",
          label = "Amendment Year",
          value = section_year,
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId = "article_num_val",
          label = "Article Number",
          value = article_num,
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId = "section_num_val",
          label = "Section Number",
          value = section_num,
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId = "part_num_val",
          label = "Part Number",
          value = part_num,
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::textInput(
          inputId = "section_topic_val",
          label = "Section Topic",
          value = section_topic,
          placeholder = "Input Topic",
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::textInput(
          inputId = "section_text_val",
          label = "Section Text",
          value = section_text,
          placeholder = "Input Text",
          width = "400px"
        )
      )
    ),
    size = "m",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(
        inputId = "final_edit",
        label = x,
        icon = shiny::icon("edit"),
        class = "btn-info"
      ),
      shiny::actionButton(
        inputId = "dismiss_modal",
        label = "Close",
        class = "btn-danger"
      )
    )
  ) %>% shiny::showModal()
}


# Modal pop up to modify Sections
modal_dialog2 <- function(section_id2, constitution_id2, section_year2, article_num2,
                          section_num2, part_num2, section_topic2,section_text2, edit) {
  if (edit) {
    x <- "Submit Edits"
  } else {
    x <- "Add New Section"
  }
  shiny::modalDialog(
    title = "Edit Constitution",
    div("Warning: All inputs must be filled before submitting!"),
    div(
      class = "text-center",
      div(
        style = "display: inline-block;",
        shiny::textInput(
          inputId = "section_id_val2",
          label = "Section Id",
          value = section_id2,
          placeholder = "Input Section Id",
          width = "400px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::textInput(
          inputId = "constitution_id_val2",
          label = "Constitution Id",
          value = constitution_id2,
          placeholder = "Input Constitution Id",
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId = "section_year_val2",
          label = "Amendment Year",
          value = section_year2,
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId = "article_num_val2",
          label = "Article Number",
          value = article_num2,
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId = "section_num_val2",
          label = "Section Number",
          value = section_num2,
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId = "part_num_val2",
          label = "Part Number",
          value = part_num2,
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::textInput(
          inputId = "section_topic_val2",
          label = "Section Topic",
          value = section_topic2,
          placeholder = "Input Topic",
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::textInput(
          inputId = "section_text_val2",
          label = "Section Text",
          value = section_text2,
          placeholder = "Input Text",
          width = "400px"
        )
      )
    ),
    size = "m",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(
        inputId = "final_edit2",
        label = x,
        icon = shiny::icon("edit"),
        class = "btn-info"
      ),
      shiny::actionButton(
        inputId = "dismiss_modal",
        label = "Close",
        class = "btn-danger"
      )
    )
  ) %>% shiny::showModal()
}


# AUTHENTICATION FUNCTIONS ------------------------------------------------

# Authentication shinyauthr Functions taken from https://github.com/PaulC91/shinyauthr
# How many days should sessions last?
cookie_expiry <- 7

# This function must return a data.frame with columns user and sessionid.  Other columns are also okay
# and will be made available to the app after log in.
get_sessions_from_db <- function(conn = db, expiry = cookie_expiry) {
  dbReadTable(conn, "sessions") %>%
    mutate(login_time = ymd_hms(login_time)) %>%
    as_tibble() %>%
    filter(login_time > now() - days(expiry))
}

# This function must accept two parameters: user and sessionid. It will be called whenever the user
# successfully logs in with a password.
add_session_to_db <- function(user, sessionid, conn = db) {
  tibble(user = user, sessionid = sessionid, login_time = as.character(now())) %>%
    dbWriteTable(conn, "sessions", ., append = TRUE)
}

# Connect to 
db <- dbConnect(SQLite(), ":memory:")
dbCreateTable(db, "sessions", c(user = "TEXT", sessionid = "TEXT", login_time = "TEXT"))

# Usernames and Passwords for Demo. This will be changed in production and stored securely
user_base <- tibble(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"),
  password_hash = sapply(c("pass1", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("admin", "guest")
)







# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  title = "State Constitutions Initiative",
  dashboardHeader(
    title = tags$a(tags$img(src = "logo3.png", align = "left", height="95%")),
    tags$li(
      class = "dropdown",
      style = "padding: 8px;",
      shinyauthr::logoutUI("logout", 
                           icon=icon("fas fa-sign-out-alt"), 
                           class = 'btn-danger', 
                           style = "color:white; background-color: #020205; border-color: #020205")
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        icon("github"),
        href = "https://github.com/mparoca/SCI",
        title = "See the code on github"
      )
    )
  ),
  dashboardSidebar(
    collapsed = TRUE,
    div(textOutput("welcome"), style = "padding: 20px"), 
    sidebarMenu(
      
      menuItem("View Tables", tabName = "view_table", icon = icon("search")),
      menuItem("Explore Constitutions", tabName = "explore", icon = icon("chart-line")),
      menuItem("Update Entries", tabName = "insert_value", icon = icon("edit")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    shinyauthr::loginUI(
      "login", 
      cookie_expiry = cookie_expiry, 
      additional_ui = tagList(
        tags$p("Use passwords below to test the application", class = "text-center"),
        HTML(knitr::kable(user_base[, -3], format = "html", table.attr = "style='width:100%;color:#50A2A7;'"))
      ) 
      
    ),
    tabItems(
      tabItem(tabName = "view_table", uiOutput("tab1UI")),
      tabItem(tabName = "explore", uiOutput("tab2UI")),
      tabItem(tabName = "insert_value", uiOutput("tab5UI")),
      tabItem(tabName = "about", uiOutput("tab6UI"))
    ),
    
    uiOutput("testUI"),
    use_theme(mytheme), # <-- use the theme
    includeCSS("www/dark_mode.css"), 
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico"))
  )
)













# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  

# Authentication ----------------------------------------------------------

  # call login module supplying data frame, user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password_hash,
    sodium_hashed = TRUE,
    cookie_logins = TRUE,
    sessionid_col = sessionid,
    cookie_getter = get_sessions_from_db,
    cookie_setter = add_session_to_db,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  observe({
    if (credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  user_info <- reactive({
    credentials()$info
  })
  
  user_data <- reactive({
    req(credentials()$user_auth)
    
    if (user_info()$permissions == "admin") {
      dplyr::starwars[, 1:10]
    } else if (user_info()$permissions == "standard") {
      dplyr::storms[, 1:11]
    }
  })
  
  output$welcome <- renderText({
    req(credentials()$user_auth)
    
    glue("Welcome\n{user_info()$name} !")
  })
  

  
  
# View Tables Tab ---------------------------------------------------------

  # UI
  output$tab1UI <- renderUI({
    req(credentials()$user_auth)
    
    fluidRow(
      column(
        width = 12,
        tags$h2(glue("Your permission level is: {user_info()$permissions}.
                     You logged in at: {user_info()$login_time}.")),
        selectInput(inputId = "sel_table_1", label= "Select Table from Database:", choices = dbListTables(conn), selected ="constitutions")),
        box(width = 12, 
            status = "primary",
            title = h4(strong("Table Preview")),
            dataTableOutput(outputId = "sel_table_view")
              )
            )
  })

  # Output table  
  output$sel_table_view <- DT::renderDT(dbGetQuery(conn, statement = paste0('SELECT * from ',input$sel_table_1)),
                                      filter = 'bottom',
                                      options = list(autoWidth = FALSE, scrollX = TRUE,
                                                     initComplete = JS(
                                                       "function(settings, json) {",
                                                       "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                       "}")))  


  
  
  
  
# Update Entries Tab ------------------------------------------------------


# UI
output$tab5UI <- renderUI({
  req(credentials()$user_auth)
  
  if (user_info()$name =="guest")
    
  {
    fluidRow(
      column(
        width = 12,
        tags$h2(glue("Your permission level is: {user_info()$permissions}.
                     You logged in at: {user_info()$login_time}. You do not currently have access to this feature."))
      )
    )
    
  }
  
  else if (user_info()$name == "admin")
    
  {
    
    fluidPage(
      column(
        width = 12,
        tags$h2(glue("Your permission level is: {user_info()$permissions}.
                     You logged in at: {user_info()$login_time}.")),
        tags$h4(glue("Note: Because Dataset is not yet public, only sections and amendments
                     from a sample of states (Alabama, Alaska and Arizona) have been added to this Demo.
                     Remaining state documents will appear without sections.")),
        tags$h3(glue("")),
      selectInput(inputId = "sel_constitution", label= "Select Constitution:", choices = dbGetQuery(conn, 'SELECT constitution_id FROM constitutions'), selected ="Alabama1819"),
      div(
        class = "container",
        div(
          style = "margin-top: 0px; margin-left: 0px; margin-bottom: 5px;",
          shiny::actionButton(
            inputId = "add_section",
            label = "Add Section",
            icon = shiny::icon("plus"),
            class = "btn-success"
          )
        )
      ),
      box(width = 12, 
          status = "primary",
          DT::DTOutput(outputId = "dt_table")
      )),
      shiny::includeScript("script.js")

    )
    
  }
})
  
  

# ADD SECTION -------------------------------------------------------------

  df_sections <- reactive({
    df = dbGetQuery(conn, statement = paste0('SELECT * FROM sections WHERE constitution_id="',input$sel_constitution, '"'))
    rows = dbGetQuery(conn, statement = paste0('SELECT COUNT(*) FROM sections WHERE constitution_id="',input$sel_constitution, '"'))
    x = create_btns(1:rows$`COUNT(*)`[1])
    df <- df %>%
      dplyr::bind_cols(tibble("Modify" = x))
    df <- df[, c(12, 1:11)]
    return(df)
  })
  

  output$dt_table <- DT::renderDT(df_sections(),
    escape = F,
    rownames = FALSE,
    filter = 'bottom',
    options = list(autoWidth = TRUE, scrollX = TRUE,processing=FALSE, pageLength = 5,
                   columnDefs = list(list(width = '80px', targets = 0)),
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}"))
  )
  
  shiny::observeEvent(input$add_section, {
    modal_dialog(
      section_id = "", constitution_id = "", section_year= "", 
      article_num = "", section_num = "", part_num = "",
      section_topic = "", section_text = "",  edit = FALSE
    )
  })
  
  # After Click Run Query to Insert New Section and Update Audit Table
  shiny::observeEvent(input$final_edit, {
    
    dbExecute(conn, statement = 
    paste0('INSERT INTO sections (section_id, constitution_id, section_year, article_num, section_num, part_num, section_topic, 
          section_text, is_deleted, created_by, updated_by) 
          VALUES ("', input$section_id_val, '", "', input$constitution_id_val, '",', input$section_year_val, ', ', input$article_num_val, ',', input$section_num_val, ', ', input$part_num_val, ', "', input$section_topic_val, '", "', input$section_text_val, '", "0", "admin", "admin");'))

  })
  
  
  # Close Button. Remove Modal after click of close button
  shiny::observeEvent(input$dismiss_modal, {
    shiny::removeModal()
  })
  
  # After Click Success Message Will appear
  shiny::observeEvent(input$final_edit, {
    shinyalert("Success", paste0('Section Added!', 
                                 '\nNote: Refresh to see Change.', 
                                 '\nChange is logged into section_history audit table'), type = "success")
    #shiny::removeModal() # Don't remove modal, person might want to create another section
  })
 
  

# DELETE SECTION ----------------------------------------------------------
  
  #POP UP TO INPUT YEAR OF REPEAL
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "delete"))

    row <- df_sections()[strtoi(strsplit(input$current_id, "_")[[1]][2]), ]
    section_id_val2 <-row$section_id

    shinyalert(
      html = TRUE,
      text = tagList(
        numericInput("num", "Year of Repeal", "Insert Year"),
        paste0("Year of Repeal of ",section_id_val2, " is "),
        textOutput("year", inline = TRUE),
      ), 
      inputId="deletion"
    )

  })
 
  # After Click run Query to Delete Section and update audit table
observeEvent(c(input$deletion), {
  
  row2 <- df_sections()[strtoi(strsplit(input$current_id, "_")[[1]][2]), ]
  section_id_val3 <-row2$section_id
  
  dbExecute(conn, statement = paste0('UPDATE sections SET is_deleted=1, section_year = ', input$num,', updated_by="admin" where section_id="', section_id_val3, '";'))
  
  
    shinyalert("Success", paste0(section_id_val3, ' deleted! Year of repeal:', input$num, 
                                 "\nNote: Entry will not be removed from DB, but is_deleted
                                 column will take the value of 1. Refresh to see change.", 
                                 '\nChange is logged into section_history audit table') , type = "success")
  
  })
  
  # Show text of year of repeal
  output$year <- renderText({input$num})
  
  

# UPDATE SECTION ----------------------------------------------------------

  
  # MODAL POP UP TO UPDATE A CONSTITUTIONAL SECTION
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "edit"))
    row <- df_sections()[strtoi(strsplit(input$current_id, "_")[[1]][2]), ]
    modal_dialog2(
      
      section_id2 = row$section_id, constitution_id2 = row$constitution_id, section_year2 = row$section_year, 
      article_num2 = row$article_num, section_num2 = row$section_num, part_num2 = row$part_num,
      section_topic2 = row$section_topic, section_text2 = row$section_text, edit = TRUE
    )
  })

  
  # After Click Run Query to update section and audit table
  shiny::observeEvent(input$final_edit2, {
    row3 <- df_sections()[strtoi(strsplit(input$current_id, "_")[[1]][2]), ]
    
    dbExecute(conn, statement=
                paste0(
                  'UPDATE sections SET section_id="', input$section_id_val2, 
                  '", constitution_id ="', input$constitution_id_val2,
                  '", section_topic ="', input$section_topic_val2,
                  '", section_text ="', input$section_text_val2,
                  '", section_year =', input$section_year_val2,
                  ', article_num =', input$article_num_val2,
                  ', section_num =', input$section_num_val2,
                  ', part_num =', input$part_num_val2,
                  ', updated_by="admin" WHERE section_id="', row3$section_id, '"'))
    
    shinyalert("Success", paste0('Section ', row3$section_id,' Modified!', 
                                 '\nNote: Refresh to see Change in Section.', 
                                 '\nChange is logged into section_history audit table'), type = "success")
    shiny::removeModal()
  })
  
    

  


# Explore Constitutions Tab -----------------------------------------------

  #Explore Constitutions UI
  output$tab2UI <- renderUI({
    req(credentials()$user_auth)
    
    fluidRow(
      column(
        width = 12,
        tags$h2(glue("Your permission level is: {user_info()$permissions}.
                     You logged in at: {user_info()$login_time}.")),
        tags$h4(glue("Note: Because Dataset is not yet public, only sections and amendments
                     from a sample of states (Alabama, Alaska and Arizona) have been added to this Demo.
                     Remaining state documents will appear without sections.")),
        tags$h3(glue("")),
        selectInput(inputId = "sel_constitution2", label= "Select Constitution:", 
                    choices = dbGetQuery(conn, 'SELECT constitution_id FROM constitutions'), 
                    selected ="Alabama1901")),
      valueBoxOutput("YearBox"),
      valueBoxOutput("SectionsBox"),
      valueBoxOutput("AmendmentsBox"),
      box(plotlyOutput("plot1"), width = 12),
      box(plotlyOutput("plot2"), width = 12)
    
      #DT::DTOutput(outputId = "dt_counts")
      
      
    )
  })
  

# VALUE BOXES  
output$YearBox <- renderValueBox({
  year_adopted = dbGetQuery(conn, statement = paste0('SELECT year_of_adoption as year FROM constitutions WHERE constitution_id="',input$sel_constitution2, '"'))
    valueBox(
      year_adopted$year, "Year Adopted", icon = icon("calendar"),
      color = "yellow"
    )
  })
  
  output$SectionsBox <- renderValueBox({
    rows_sec = dbGetQuery(conn, statement = paste0('SELECT COUNT(*) FROM sections WHERE constitution_id="',input$sel_constitution2, '"'))  
    
    valueBox(
      rows_sec$`COUNT(*)`[1], "Sections", icon = icon("list", lib = "glyphicon"),
      color = "red"
    )
  })
  
  output$AmendmentsBox <- renderValueBox({
    year_adopted2 = dbGetQuery(conn, statement = paste0('SELECT year_of_adoption as year FROM constitutions WHERE constitution_id="',input$sel_constitution2, '"'))
    count_amendments = dbGetQuery(conn, statement = paste0('SELECT COUNT(*) as count FROM sections WHERE constitution_id="',input$sel_constitution2, '" AND section_year>', year_adopted2))
    
    valueBox(
      count_amendments$count, "Amendments", icon = icon("pencil", lib = "glyphicon"),
      color = "navy"
    )
  })
  
  
  
# PLOTLY OUTPUT  
  
  year_data <- reactive({
    year_adopted3 = dbGetQuery(conn, statement = paste0('SELECT year_of_adoption as year FROM constitutions WHERE constitution_id="',input$sel_constitution2, '"'))
    counts = dbGetQuery(conn, statement = paste0('SELECT section_year as count FROM sections WHERE constitution_id="',input$sel_constitution2, '" AND section_year>', year_adopted3))
    if (nrow(counts)==0){
      year=seq(year_adopted3$year, year_adopted3$year+20, by=1)
      Freq = rep(0, 21)
      year_df <- data.frame(year, Freq)
    }
    else {
      year = factor(counts$count,levels=c(year_adopted3$year:max(counts)+1))
      year = table(year)
      year_df <- as.data.frame(year)
    }
    return(year_df)
  })
  
  topic_data <-reactive({
    topic_count <- dbGetQuery(conn, statement = 
                                paste0('SELECT COUNT(*) as total_sections, section_topic as topic FROM sections WHERE constitution_id ="',input$sel_constitution2, '" GROUP BY section_topic ORDER BY total_sections DESC LIMIT 10'))
    topic_count$topic <- factor(topic_count$topic, levels = unique(topic_count$topic)[order(topic_count$total_sections, decreasing = TRUE)])
    topic_count <- as.data.frame(topic_count)
  })
  
  output$plot1 <-renderPlotly({ 
    
    plot_ly(year_data(), x = ~year, y = ~Freq, type = 'scatter', 
            mode = 'lines+markers', line = list(color = '#50A2A7', width = 3), 
            marker = list(color = '#666370', size = 8)) %>% 
      layout(
      title = "\nAmendments per Year\n",
      yaxis = list(rangemode = "nonnegative"), 
      showlegend = FALSE
      )
    
    })

  output$plot2 <-renderPlotly({ 
    
    plot_ly(topic_data(), x = ~topic, y = ~total_sections, type = 'bar', 
            text = ~total_sections, textposition = 'auto',
            marker = list(color = '#F87060',
                          line = list(color = '#50A2A7', width = 1.5))) %>% 
      layout(
        title = "\nTop 10 Topics in Document\n"
      )
    
  })
  
  

  
  
  

# About Tab ---------------------------------------------------------------
  output$tab6UI <- renderUI({
    req(credentials()$user_auth)
    
    fluidRow(
      column(
        width = 12,
        box(width = 12,
            status = "primary",
            #background="black",
            tags$h1(glue("About")),
            tags$h4(glue("This App helps State Constitutions Initiative researchers build a 
                         dataset of state constitutions and their amendments. 
                         The State Constitutions Initiative has the objective of creating a digital history of all 154 
                         adopted constitutions in the U.S. states and all of their amendments with the purpose of allowing researchers to track and visualize the evolution of constitutional text within states, as well as to compare across them. ")),
            tags$h2(glue("Constitutions Data")),
            tags$h4(glue("The U.S. Constitutions Dataset (Miller et al., N.D.) is an original dataset 
                         that contains information of 111 state constitutions from 40 states and all of 
                         their amendments adopted between 1776 and 2020. 
                         The Miller. et al. (N.D.) dataset was built with data from The Federal and 
                         State Constitutions, Colonial Charters, and the Organic Laws of the State, 
                         Territories, and Colonies; Now or Heretofore Forming the United States of 
                         America by Francis Newton Thorpe, 
                         The NBER/Maryland State Constitutions project and information from state 
                         government websites. ")),
            tags$h4(glue("Only a sample of the Data is displayed in this Demo.")),
            tags$h2(glue("Developer")),
            tags$h4(glue("Maria Aroca")),
            tags$a(href="https://www.linkedin.com/in/maria-paula-aroca-42a0a5166/", "LinkedIn"),
            tags$h2(glue("Code")),
            tags$h4(glue("For the Source Code and more information see GitHub:")),
            tags$a(href="https://github.com/mparoca/sci", "GitHub"),
            tags$h3(glue(""))
            )
      )
    )
  }
  )
  
  
}




# CREATE SHINY APP OBJECT -------------------------------------------------
shiny::shinyApp(ui, server)