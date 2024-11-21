# https://qmacro.org/blog/posts/2022/05/19/json-object-values-into-csv-with-jq/
def onlyvalues: [ keys[] as $k | .[$k] ];

# needs a list of objects
def csv_with_header: [.[0] | keys_unsorted] + [.[] | onlyvalues] | .[] | @csv;
