
# libzhuoer 0.3.0

## Breaking changes

1. introduce `cols_char()` to supersede `read_char_*()` 

   Formerly I write that wrapper function and try to handle various situations. When I want to fix a bug, I realise that the real world is too complicated. Actually the core point is to set default column type to character, which can be easily accomplished by `readr::cols(..., .default = readr::col_character())`.

1. remove unused function `initiate_dir()`

## New features

1. add or enrich example for every function

## Minor improvements and fixes

1. `gzip_integrity()` correct return `FALSE` for broken file now



# libzhuoer 0.2.0

## New features

1. improve documentation (mainly add pkgdown)



# libzhuoer 0.1.0

## New features

1. open multiple urls sequentially in browser
1. check integrity of gzip files in a directory
1. read delimited file assuming every column is character
1. helper function for assertthat