<style>
body {
    color: black !important;
}
</style>
## Why search?

The iRISE-SOLES workflow uses the full text of publications to annotate articles by intervention evaluated and outcomes measured, among other tags. If you want to identify research in a particular population or by another measure which isn't covered by our tagging functionality, we recommend using search terms to identify these.

## How does searching work?

The search functionality within the SOLES app allows users to search the title, abstract, and keywords of indexed articles. Users enter search terms which are translated in the background into a specialised search patterns called <a href="https://cs.lmu.edu/~ray/notes/regex/" target="_blank">regular expressions</a>  which are used to search through the title, abstract, and keyword text fields and return studies with matches.

## How to search

Enter terms separated by commas into the box on this page and decide whether to combine with a boolean AND or OR operator. 

For example, searching with the **AND** button pressed `scientific, manuscript, feedback` will search for "scientific" AND "manuscript" AND "feedback". If you would like to combine boolean operators then you can use the <i class="fa fa-magnifying-glass-plus"></i> <strong>Advanced</strong> search tab.

Then click <i class="fa fa-magnifying-glass"></i> <strong>Search</strong> to run the search. The results will appear in the table at the bottom of this page.


## Wildcards and search syntax

To search for **reproducible**, **reproducibility**, or **reproducibly**, we can use regular expressions in R in a similar way to wildcards in other search engines.

In regular expression notation:  
- The character `.` matches any single character.  
- The character `*` matches the previous element **zero or more times**.  
- To match **one or more times**, you would use `+` instead.  

The pattern `reproducib.*` will match any string starting with `reproducib` and followed by zero or more characters, which includes:  
- reproducible
- reproducibility  
- reproducibly

## How to use filters

Before or after you run a search, you can press the <i class="fa fa-sliders"></i> <strong>Filter</strong> button to filter studies of interest.

## Download study metadata

You can download study metadata in various formats, using the <i class="fa fa-download"></i> <strong>Download</strong> button located on the table below.

## How to start again  

If you want to run a new search, it is advisable to press the <i class="fa fa-times-circle"></i><strong>Clear</strong> button to remove any filters and clear your search box. You can then run another search and apply different filters.