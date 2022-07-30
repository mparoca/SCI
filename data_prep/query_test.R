library(DBI)
library(RSQLite)


# Connect to RSQLite DB
conn <- dbConnect(RSQLite::SQLite(), "data/constitutionsdb.db")

# List all the tables available in the database
dbListTables(conn)


# Select Table
dbGetQuery(conn, 
           'SELECT *
           FROM states')

# Select Table with limit
dbGetQuery(conn, 
           'SELECT *
           FROM sections_history
           LIMIT 10')

# Test to see if trigger is working for addition of section
dbExecute(conn, 
          'INSERT INTO sections (section_id, constitution_id, section_year, article_num, section_num, part_num, section_topic, 
          section_text, is_deleted, created_by, updated_by) 
          VALUES ("AL_9099_139_0_1901", "Alabama1901", "1957", "9099", "139", "0", "Local Government", 
          "The legislature may from time to time, by general or local laws, fix, alter and regulate the costs and charges of courts in Montgomery county, and the method of disbursement thereof.", "0", "admin", "admin");')

dbGetQuery(conn,
           'SELECT * 
           FROM sections_history 
           WHERE section_id="AL_9099_139_0_1901"')

# Test to see if Trigger is working for update of section
dbExecute(conn,
           'UPDATE sections 
           SET section_text="The clerk of the supreme court shall be appointed by the justices thereof; and the clerks of such inferior courts as may be established by law shall be selected in such manner as the legislature may provide.", 
           section_year = 1967,
           updated_by="admin" WHERE section_id="AL_6_164_0_1901"')

dbGetQuery(conn,
           'SELECT *
           FROM sections_history 
           WHERE column_name = "section_text"
           AND section_id="AL_6_164_0_1901"')


# Select Only Constitutions from Sections DB
dbGetQuery(conn, 'SELECT DISTINCT(constitution_id) FROM sections')


# Select Unique Constitutions
dbGetQuery(conn, 'SELECT constitution_id FROM constitutions')

# Select Sections From Constitutions
df_sections <-dbGetQuery(conn, 'SELECT * FROM sections WHERE constitution_id = "Alaska1959"')

# Count number of rows
sections_rows <-dbGetQuery(conn, 'SELECT COUNT(*) FROM sections WHERE constitution_id = "Alabama1861"')


year_amendment <-dbGetQuery(conn, 'SELECT year_of_adoption FROM constitutions WHERE constitution_id = "Alabama1861"')

# Count Total Amendments
dbGetQuery(conn, 'SELECT COUNT(*) as count FROM sections WHERE constitution_id = "Alabama1901" AND section_year>1901')

# Amendment years
counts <-dbGetQuery(conn, 'SELECT section_year as count FROM sections WHERE constitution_id = "Alabama1901" AND section_year>1901')
year<-factor(counts$count,levels=c(1901:max(counts)+1))
year<-table(year)
year_counts_df <- as.data.frame(year)

library(plotly)

fig <- plot_ly(year_counts_df, x = ~year, y = ~Freq, type = 'scatter', mode = 'lines')

fig

ggplot(year_counts_df, aes(x= year, y = Freq, group=1)) + 
  geom_line() 
#section_id, constitution_id, section_year, article_num, section_num, part_num, section_topic, 
#section_text


