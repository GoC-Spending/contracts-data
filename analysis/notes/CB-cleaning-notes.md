# Economic Object Codes
- Going to remove some columns from `contracts.csv` data to reduce file size
  - vendor_postal_code
  - description_fr
  - comments_fr
  - additional_comments_fr
  - vendor_postal_code
- Based on description col, economic object codes are all 4 digits --> some codes are not preceeded by '0'
- Normalising EOCs by adding 0 to front of codes that are only 3 digits --> adding 0 instead of removing because EOCs in descriptions are preceded by a 0
- In `contracts.csv` I changed `0049` to `0499` due to description reading "0499 OTHER PROFESSIONAL SERVCS"
- Extracting EOCs from rows that do not have entry in EOC category but have description
  - Hypothesis:
    - First try extracting anything that is 0+digits
    - If this fails check if any substrings match the text descriptions of EOCs
  - Extraction is successful for 0+digit numbers but some descriptions miss the zero and this does not catch the 4 digit EOCs --> matching descriptions sort of works but occasionally one part of description is cut off in `REVISED_economic_object_codes_to_category.csv` resulting in no match, so matching description should be final case
    - SOLUTION: if EOC is contained in description it is usually the first thing written --> check if first string is digit and then match digit to category
  - Observations: Even for rows that have an established EOC, occasionally that EOC doesn't match the one written in the description (ie row 2 of `contracts.csv`)
  - Matching text descriptions works, but... why do some EOCs have the same text description?

  


# OpenRefine Clustering Notes

## Proactive Disclosure Contracts

