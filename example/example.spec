[
{ "tag": "const", "name": "BAR", "type": { "tag": ":int" }, "value": 14 },
{ "tag": "extern", "name": "SomeExtern", "type": { "tag": ":int" } },
{ "tag": "function", "name": "blah", "parameters": [{ "tag": "parameter", "name": "x", "type": { "tag": ":pointer", "type": { "tag": ":pointer", "type": { "tag": ":char" } } } }], "return-type": { "tag": ":void" } },
{ "tag": "extern", "name": "foo", "type": { "tag": ":pointer", "type": { "tag": ":char" } } },
{ "tag": "struct", "name": "my_point", "fields": [{ "tag": "field", "name": "x", "type": { "tag": ":int" } }, { "tag": "field", "name": "y", "type": { "tag": ":int" } }, { "tag": "field", "name": "odd_value", "type": { "tag": ":array", "type": { "tag": ":int" }, "size": 15 } }] },
{ "tag": "typedef", "name": "my_point_t", "type": { "tag": ":struct", "name": "my_point" } },
{ "tag": "typedef", "name": "anonymous_t", "type": { "tag": "struct", "name": "", "fields": [{ "tag": "field", "name": "a", "type": { "tag": ":int" } }, { "tag": "field", "name": "b", "type": { "tag": ":int" } }] } },
{ "tag": "union", "name": "my_union", "fields": [{ "tag": "field", "name": "c", "type": { "tag": ":char" } }, { "tag": "field", "name": "i", "type": { "tag": ":int" } }, { "tag": "field", "name": "d", "type": { "tag": ":double" } }] },
{ "tag": "enum", "name": "some_values", "fields": [{ "tag": "field", "name": "a_value", "value": 0 }, { "tag": "field", "name": "another_value", "value": 1 }, { "tag": "field", "name": "yet_another_value", "value": 2 }] },
{ "tag": "function", "name": "do_something", "parameters": [{ "tag": "parameter", "name": "p", "type": { "tag": ":pointer", "type": { "tag": "my_point_t" } } }, { "tag": "parameter", "name": "x", "type": { "tag": ":int" } }, { "tag": "parameter", "name": "y", "type": { "tag": ":int" } }], "return-type": { "tag": ":void" } }
]
