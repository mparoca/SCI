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


# List all the tables available in the database
dbListTables(conn)