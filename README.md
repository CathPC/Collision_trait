# Collision_trait

24/01/2025 
In the collision project, we want to download the traits of the crushed species. However, some crushed individuals cannot be identified at species level. So the whole point of this project is to retrieve from several databases the traits for the identified species and to have a measure of the trait values for the individuals found in the genus state. To do this, we need to download all the known species of a genus and average their traits (in the case of non possible identification at the species level).


From the Bioshift project, Nico C has selected all the scripts and files needed to download the traits and regroup them into a list of species, which only works for the script pipeline using the "https://github.com/RS-eco/traitdata" packages.
He has modified the code to allow the traits to be downloaded when we have only identified the genus (and also when they are the genus and species names).
These are the main changes:
- They do not correct the list of species names when they are only the genus if the species_only option is false (do not add sp. but leave it blank).
- This makes it possible to download all the data for a genus in any database.
- He has created a new column merge_string to identify if information about a species is being downloaded because there is no information on the species names, or if the information is being downloaded because it was a species observation.
