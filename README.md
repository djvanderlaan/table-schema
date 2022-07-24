


Categorical fields
-------------------------------------------------------------------------------

Categorical fields are fields for which only specific set of values are allowed. 
Often these values have specific meaning/labels assigned. For example, the value
'1' might stand for 'Unmarried', '2' for 'Married, '3' for 'Divorced' and '4' 
for 'Widowed' in a field encoding marital status. In R these are usually stored 
in `factor' vectors. In the field schema a distinction is made in the type in 
which this data is stored, 'integer' in the example, and the fact that the 
field encodes a categorical variable. This is done by adding a `categories` field
to the field schema as in the example below:

```
{
  "name": "marstat",
  "title": "Marital status",
  "type" : "integer",
  "categories" : [
    {"value" = 1, label = "Unmarried"},
    {"value" = 2, label = "Married"},
    {"value" = 3, label = "Divorced"},
    {"value" = 4, label = "Widowed"}
  ]
}
```

Each element in the `categories` field should be an object with a `value` and 
`label` field. The `value` field should be of the same type as the field 
(integer in this case); the `label` field should be a string. 



TODO List
-------------------------------------------------------------------------------

- [x] CSV-writer
- [x] Documentation of functions
- [ ] Handle European CSV-format (';' and ',' vs ',' and '.') in `csv_read` and
  `csv_write`.
- [x] Handle date columns
- [x] Let `csv_write` also write to stdout
- [ ] Handle factor columns
- [x] Specify missing values at file level 
- [ ] Specify missing values at column level (not in standard)
- [ ] fst reader and writer (separate package?)
- [ ] feather reader and writer (separate package?)

