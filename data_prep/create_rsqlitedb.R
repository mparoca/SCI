library(DBI)
library(RSQLite)

# Connect to MySQL workbench databases
constitutionsDB <- dbConnect(odbc::odbc(), 
                             .connection_string = "Driver={MySQL ODBC 8.0 Unicode Driver};", 
                             Server= "127.0.0.1", Database = "constitutionsdb", 
                             UID="root", PwD="root", Port=3306)

dbListTables(constitutionsDB)

# Import tables
states <- dbReadTable(constitutionsDB, "states")
constitutions <- dbReadTable(constitutionsDB, "constitutions")
sections <- dbReadTable(constitutionsDB, "sections")
sections_history <- dbReadTable(constitutionsDB, "sections_history")


# Create RSQLite database called constitutionsdb
conn <- dbConnect(RSQLite::SQLite(), "constitutionsdb.db")

# Write Tables
dbWriteTable(conn, "states", states)
dbWriteTable(conn, "constitutions", constitutions)
dbWriteTable(conn, "sections", sections)
dbWriteTable(conn, "sections_history", sections_history)

# MYSQL Triggers do not import properly because of syntax differences. Create Triggers in SQLlite
# CREATE INSERT TRIGGER TO UPDATE SECTION

#dbExecute(conn, 'DROP TRIGGER sections_update')

dbExecute(conn,
          'CREATE TRIGGER sections_update
          AFTER UPDATE ON sections 
          FOR EACH ROW
          BEGIN
            insert into sections_history(section_id, column_name, old_value, new_value, done_by) values(NEW.section_id,"constitution_id",OLD.constitution_id,NEW.constitution_id,NEW.updated_by);
            insert into sections_history(section_id, column_name, old_value, new_value, done_by) values(NEW.section_id,"section_year",OLD.section_year,NEW.section_year,NEW.updated_by);
            insert into sections_history(section_id, column_name, old_value, new_value, done_by) values(NEW.section_id,"article_num",OLD.article_num,NEW.article_num,NEW.updated_by);
            insert into sections_history(section_id, column_name, old_value, new_value, done_by) values(NEW.section_id,"section_num",OLD.section_num,NEW.section_num,NEW.updated_by);
            insert into sections_history(section_id, column_name, old_value, new_value, done_by) values(NEW.section_id,"part_num",OLD.part_num,NEW.part_num,NEW.updated_by);
            insert into sections_history(section_id, column_name, old_value, new_value, done_by) values(NEW.section_id,"section_topic",OLD.section_topic,NEW.section_topic,NEW.updated_by);
            insert into sections_history(section_id, column_name, old_value, new_value, done_by) values(NEW.section_id,"section_text",OLD.section_text,NEW.section_text,NEW.updated_by);
            insert into sections_history(section_id, column_name, old_value, new_value, done_by) values(NEW.section_id,"is_deleted",OLD.is_deleted,NEW.is_deleted,NEW.updated_by);
          END
          ')

# CREATE INSERT TRIGGER TO INSERT NEW SECTIONS TO A CONSTITUTION
dbExecute(conn, 
          'CREATE TRIGGER sections_create
          AFTER INSERT
          ON sections FOR EACH ROW
          BEGIN
            insert into sections_history(section_id, column_name, new_value, done_by) values(NEW.section_id,"constitution_id",NEW.constitution_id,NEW.created_by);
            insert into sections_history(section_id, column_name, new_value, done_by) values(NEW.section_id,"section_year",NEW.section_year,NEW.created_by);
            insert into sections_history(section_id, column_name, new_value, done_by) values(NEW.section_id,"article_num",NEW.article_num,NEW.created_by);
            insert into sections_history(section_id, column_name, new_value, done_by) values(NEW.section_id,"section_num",NEW.section_num,NEW.created_by);
            insert into sections_history(section_id, column_name, new_value, done_by) values(NEW.section_id,"part_num",NEW.part_num,NEW.created_by);
            insert into sections_history(section_id, column_name, new_value, done_by) values(NEW.section_id,"section_topic",NEW.section_topic,NEW.created_by);
            insert into sections_history(section_id, column_name, new_value, done_by) values(NEW.section_id,"section_text",NEW.section_text,NEW.created_by);
          END')

# DROP COLUMN
#dbExecute(conn, 'ALTER TABLE sections_history DROP COLUMN id')
dbExecute(conn, 'ALTER TABLE sections_history DROP COLUMN done_at')

dbExecute(conn, 'ALTER TABLE constitutions DROP COLUMN date_of_adoption')

# List all the tables available in the database
dbListTables(conn)



# EXAMPLE OF HOW TO ADD DATA FROM EXCEL FILES TO DB -----------------------

# Add some sections from excel file
library("readxl")

## Inserting New Sections from Excel

sections_df <- read_excel("C:/Users/User/Box/IU/DataViz/final_project/data/full_xlsx_files/Arizona1912_base.xlsx", sheet = "original")

sections_df$id<-paste0(sections_df$state, "_", as.integer(sections_df$article_num), "_", 
                       as.integer(sections_df$section_num), "_", as.integer(sections_df$part_num), "_", sections_df$year_constitution)

for(i in 1:nrow(sections_df)){
  dbExecute(conn, statement = 
              paste0('INSERT INTO sections (section_id, constitution_id, section_year, article_num, section_num, part_num, section_topic, 
          section_text, is_deleted, created_by, updated_by) 
          VALUES ("', sections_df$id[i], '", "', sections_df$constitution[i], '",', as.integer(sections_df$year_constitution[i]), ', ', as.integer(sections_df$article_num[i]), ',', as.integer(sections_df$section_num[i]), ', ', as.integer(sections_df$part_num[i]), ', "', sections_df$subject[i], '", "', sections_df$section_text[i], '", "0", "mparoca", "mparoca");'))
}


## Inserting Amendments from Excel
amendments_df <- read_excel("C:/Users/User/Box/IU/DataViz/final_project/data/full_xlsx_files/Alabama1901_base.xlsx", sheet = "amendments")

amendments_df$id<-paste0(amendments_df$state, "_", as.numeric(amendments_df$article_num), "_", 
                         as.numeric(amendments_df$section_num), "_", as.numeric(amendments_df$part_num), "_", amendments_df$year_constitution)

#amendments_df <-amendments_df[-c(3,5), ]

# DELETIONS
deletions<-amendments_df[amendments_df$deleted==1,]

for(i in 1:nrow(deletions)){
  
  dbExecute(conn, statement = paste0('UPDATE sections SET is_deleted=1, section_year = ', as.integer(deletions$year_amendment[i]),', updated_by="mparoca" where section_id="', deletions$id[i], '";'))
 
}

# UPDATES
updates <- amendments_df[amendments_df$modified==1,]

for(i in 1:nrow(updates)){
  dbExecute(conn, statement=
              paste0(
                'UPDATE sections SET section_text ="', updates$amendment_text[i],
                '", section_year =', as.integer(updates$year_amendment[i]),
                ', updated_by="mparoca" WHERE section_id="', updates$id[i], '"'))

}

# ADDITIONS
additions <- amendments_df[amendments_df$added==1,]

for(i in 1:nrow(additions)) {
  dbExecute(conn, statement = 
              paste0('INSERT INTO sections (section_id, constitution_id, section_year, article_num, section_num, part_num, section_topic, 
          section_text, is_deleted, created_by, updated_by) 
          VALUES ("', additions$id[i], '", "', additions$constitution[i], '",', as.integer(additions$year_amendment[i]), ', ', as.numeric(additions$article_num[i]), ',', as.numeric(additions$section_num[i]), ', ', as.numeric(additions$part_num[i]), ', "', additions$subject[i], '", "', additions$amendment_text[i], '", "0", "mparoca", "mparoca");'))
  
}
