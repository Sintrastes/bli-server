
module DataModel where

-- Note: We should probably include a prolog parser in the project
-- so that we can analyze datasources by parsing the prolog AST, and thus
-- we should have a datatype for this. Thus, the following definition
-- probably needs to be modified.

data SemanticAnnotation =
    TextAnnotation String
  | LogicAnnotation String

-- However, this should be the return type of all of our
-- parsers.