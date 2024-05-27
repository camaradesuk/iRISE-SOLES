<style>
body {
    color: black !important;
}
</style>
## Why search?

The iRISE-SOLES workflow uses the full text of publications to annotate articles by intervention evaluated and outcomes measured, among other tags. If you want to identify research in a particular in population or by another measure which isn't covered by our tagging functionality, we recommend using search terms to identify these.

## How does searching work?

The search functionality within the SOLES app allows users to search the title, abstract, and keywords of indexed articles. Users enter search terms which are translated in the background into a specialised search patterns called [regular expressions](https://cs.lmu.edu/~ray/notes/regex/) which are used to search through the title, abstract, and keyword text fields and return studies with matches.

## How to search

Enter terms separated by commas into the box on this page and decide whether to combine with a boolean AND or OR operator. Then click "search database" to run the search. The results will appear in the table at the bottom of this page.


## Wildcards and search syntax

To search for Parkinson's, Parkinsons, Parkinsonian gait, or Parkinsonism, we can use wildcards (\*) in a similar way to other search engines. In regular expression notation, the character "." matches any character and the character "\*" matches the previous character 1 or more times.

The example search: Parkinson.\* Will search for articles that mention Parkinson's, Parkinsons, Parkinsonian, or Parkinsonism in the title, abstract, or keywords

## How to use filters

Before or after you run a search, you can press the filter button (left hand side of the table of studies) to filter studies of interest. When you have selected your filters, press the apply button to obtain the relevant articles that match your filter(s).

How to start again If you want to run a new search, it is advisable to press the Reset search query button to remove any filters and clear your search box. You can then run another search and apply different filters.


