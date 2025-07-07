# My R Commenting & Indentation Style Guide

## Comment Hierarchy
- `#!` - Issues/bookmarks/important notes
- `#*` - Major sections (numbered: `#* 1:`, `#* 2:`, etc.)
- `#+` - Subsections (numbered: `#+ 1.1:`, `#+ 2.1:`, etc.)
- `#-` - Sub-subsections (numbered: `#- 1.1.1:`, `#- 2.1.1:`, etc.)
- `#_` - Check/verification steps (inline notes)

## Indentation Rules
- Code follows comment indentation level
- 2 spaces per hierarchy level
- Major sections: no indentation
- Subsections: 2 spaces
- Sub-subsections: 4 spaces
- Code blocks: match their comment level

## Example Structure
```r
#* 1: Major Section Title
  code_at_main_level <- value
  #+ 1.1: Subsection Title
    code_at_subsection_level <- value
    #- 1.1.1: Sub-subsection Title
      code_at_sub_subsection_level <- value
      #_Check something
        verification_code()
#* 2: Next Major Section
  more_code <- value
```

## Usage Notes
- Always number major sections sequentially
- Use descriptive titles after the colon
- Maintain consistent spacing
- Apply to ALL R code regardless of complexity

## For GitHub Copilot
When formatting R code, always apply this exact commenting structure and indentation pattern.
