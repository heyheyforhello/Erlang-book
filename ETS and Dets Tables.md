# ETS Table
ETS table stores tuples, with access to the elements given through a *key* field in the tuple. 
There are four kinds of ETS tables, as outlined in the following list:
- Set: Each key occur only once
- Ordered set: ordered set has the same property as the set, but its elements can be traversed following the lexicographical order on the keys.
- Bag: Bag allows multiple entries for the same key. The elements have to be distinct
- Duplicate bag: allows duplicate elements and keys.

## Creating Tables
A table is created by calling `ets:new/2`, the first parameter gives the name, and the second consists of a list of options. The function call `ets:new(myTable, Opts)` returns the *table identifier* used to referent the table.

Options include
- `set, order_set, bag, duplicate_bag`
- `{keypos, Pos}`: creates a table with the key in posision Pos
- `public, protected, private`: access permission of the table
- `named_table`: The name of the table is statically registered, and can be used to reference the table in ETS operations.

## Handling Table Elements
- Insert elements into a table using `ets:insert/2` and access them by their key using `ets:lookup/2`:
  ```erlang
  ets:insert(TabId, {alison, sweden}).
  ets:lookup(TabId, alison).
  ets:delete(TabId).
  
  ```