# readrules
Idiosyncratic (for now) data cleaning software that we have come to know as 'Loop 1' 

update -- 
by 'idiosyncratic' we mean that this was written for a specific purpose - cleaning and merging files from a large comparative project. 
It was not written with generalization in mind. You might even say it wasn't written with anyone in mind other than the immediate user and this particular project. 

That said, a lot of the content could be useful for other projects where cleaning data based on a data dictionary or performing complex merges would be useful. 

Note - the file structure has to be:
 - readrules in root directory 
 - a folder containing xls files in root directory called "Data Sheets .xls"
 - within this data folder, folders for each field site
 - within readrules is an 'ltab' that contains site and investigator initials. this should probably match the input data.
 - the root directory must contain a file called 'RULES' that is the data dictionary that underlies the checking/cleaning
 - NOTE: sheet names matter. an error about no PIDs in PID file could mean that a sheet has the wrong name or is unnamed. 

For use, simply find the 'automatated' part. Load all the functions above that, and run. 
Many parts of the code below where the functions are loaded contains potentially useful tangents and tools. 

