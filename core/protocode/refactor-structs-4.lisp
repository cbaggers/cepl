#|| To take back to master

- storing struct definitions is nice
- through-c could be good for avoiding consing
- need to type all of the map-c functions where possible
- row-major-aref-c should be added
- row-major-aref-c (or equivalent) can be used in all cases where
  you dont need the indices as a list
- in across-c make the list first and then mutate it
- map-c and friends should pull the first element and then mutate
  it for each iteration
- lets see if we can ditch the constructor and use some other function/macro
  to pour the data into an element of a c-array

||#
