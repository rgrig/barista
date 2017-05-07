module Main where

-- TODO: At the end delete the unused ones.
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Typeable
import Data.Word
import OMap (OMap)
import Prelude hiding ((.))
import System.Environment
import System.IO
import System.IO.Unsafe -- TODO: remove
import Text.Printf
import qualified Data.ByteString as B
import qualified OMap

main = do
  xs <- getArgs
  sequence_ (map parse xs)

parse fn = do
  bc <- withFile fn ReadMode B.hGetContents
  let (t, mbs) = bytecode (B.unpack bc)
  printAsText t
  printf "\n"
  case mbs of
    Nothing -> printf "%s: parsing failed after %d bytes\n" fn (sizeOfAst t)
    Just (_ : _) -> printf "%s: parsing failed: trailing bytes\n" fn
    _ -> return ()

-- The grammar of Java bytecode starts here. {{{

integerOrder = bigEndian
lengthName x = x ++ "_count"

bytecode bs = let (_, t, mbs) = class_file "TOP" [] bs in (t, mbs)

class_file = struct
  [ u4 "magic" ?= 0xCAFEBABE
  , u2 "minor_version"
  , u2 "major_version"
  , u2 "constant_pool_count"
  -- FIXME: Long and Double constant pool entries 'use' 2 entries in the array
  , arrayG cp_info "constant_pool" (\v-> asInteger(v"constant_pool_count") - 1)
  , u2 "access_flags"
  , u2 "this_class" ~~> constant_pool
  , u2 "super_class" ~~> constant_pool
  , u2 "interfaces_count"
  , array u2 "interfaces" -- TODO: followers
  , u2 "fields_count"
  , array field_info "fields"
  , u2 "methods_count"
  , array method_info "methods"
  , u2 "attributes_count"
  , array attribute_info "attributes" ]

constant_pool v = v"constant_pool" ! (asInteger(v"") - 1)

cp_info = union
  [ constant_Class_info "constant_Class_info"
  , constant_Fieldref_info "constant_Fieldref_info"
  , constant_Methodref_info "constant_Methodref_info"
  , constant_InterfaceMethodref_info "constant_InterfaceMethodref_info"
  , constant_String_info "constant_String_info"
  , constant_Integer_info "constant_Integer_info"
  , constant_Float_info "constant_Float_info"
  , constant_Long_info "constant_Long_info"
  , constant_Double_info "constant_Double_info"
  , constant_NameAndType_info "constant_NameAndType_info"
  , constant_Utf8_info "constant_Utf8_info"
  , constant_MethodHandle_info "constant_MethodHandle_info"
  , constant_MethodType_info "constant_MethodType_info"
  , constant_InvokeDynamic_info "constant_InvokeDynamic_info" ]

constant_Class = 7
constant_Fieldref = 9
constant_Methodref = 10
constant_InterfaceMethodref = 11
constant_String = 8
constant_Integer = 3
constant_Float = 4
constant_Long = 5
constant_Double = 6
constant_NameAndType = 12
constant_Utf8 = 1
constant_MethodHandle = 15
constant_MethodType = 16
constant_InvokeDynamic = 18

constant_Class_info = struct
  [ u1 "tag" ?= constant_Class
  , u2 "name_index" ~~> constant_pool ]

constant_Fieldref_info = struct
  [ u1 "tag" ?= constant_Fieldref
  , u2 "class_index" ~~> constant_pool
  , u2 "name_and_type_index" ~~> constant_pool ]

constant_Methodref_info = struct
  [ u1 "tag" ?= constant_Methodref
  , u2 "class_index" ~~> constant_pool
  , u2 "name_and_type_index" ~~> constant_pool ]

constant_InterfaceMethodref_info = struct
  [ u1 "tag" ?= constant_InterfaceMethodref
  , u2 "class_index" ~~> constant_pool
  , u2 "name_and_type_index" ~~> constant_pool ]

constant_String_info = struct
  [ u1 "tag" ?= constant_String
  , u2 "string_index" ~~> constant_pool ]

constant_Integer_info = struct
  [ u1 "tag" ?= constant_Integer
  , u4 "bytes" ]

constant_Float_info = struct
  [ u1 "tag" ?= constant_Float
  , u4 "bytes" ]

constant_Long_info = struct
  [ u1 "tag" ?= constant_Long
  , u4 "high_bytes"
  , u4 "low_bytes" ]

constant_Double_info = struct
  [ u1 "tag" ?= constant_Double
  , u4 "high_bytes"
  , u4 "low_bytes" ]

constant_NameAndType_info = struct
  [ u1 "tag" ?= constant_NameAndType
  , u2 "name_index" ~~> constant_pool
  , u2 "descriptor_index" ~~> constant_pool ]

constant_Utf8_info = struct
  [ u1 "tag" ?= constant_Utf8
  , u2 "length"
  , arrayG u1 "bytes" (\v-> asInteger(v"length")) ]

constant_MethodHandle_info = struct
  [ u1 "tag" ?= constant_MethodHandle
  , u1 "reference_kind" ?? (\v->let x = asInteger(v"") in Just (1<=x && x<=9))
  , u2 "reference_index" ~~> constant_pool ]

constant_MethodType_info = struct
  [ u1 "tag" ?= constant_MethodType
  , u2 "descriptor_index" ~~> constant_pool ]

constant_InvokeDynamic_info = struct
  [ u1 "tag" ?= constant_InvokeDynamic
  , u2 "bootstrap_method_attr_index" -- TODO: follow
  , u2 "name_and_type_index" ~~> constant_pool ]

field_info = struct
  [ u2 "access_flags"
  , u2 "name_index" ~~> constant_pool
  , u2 "descriptor_index" ~~> constant_pool
  , u2 "attributes_count"
  , array attribute_info "attributes" ]

method_info = struct
  [ u2 "access_flags"
  , u2 "name_index" ~~> constant_pool
  , u2 "descriptor_index" ~~> constant_pool
  , u2 "attributes_count"
  , array attribute_info "attributes" ]

attribute_info = union
  [ constantValue_attribute "constantValue_attribute"
  , code_attribute "code_attribute"
-- TODO:  , stackMapTable_attribute "stackMapTable_attribute"
  , exceptions_attribute "exceptions_attribute"
  , innerClasses_attribute "innerClasses_attribute"
  , enclosingMethod_attribute "enclosingMethod_attribute"
  , synthetic_attribute "synthetic_attribute"
  , signature_attribute "signature_attribute"
  , sourceFile_attribute "sourceFile_attribute"
  , sourceDebugExtension_attribute "sourceDebugExtension_attribute"
  , lineNumberTable_attribute "lineNumberTable_attribute"
  , localVariableTable_attribute "localVariableTable_attribute"
  , localVariableTypeTable_attribute "localVariableTypeTable_attribute"
  , deprecated_attribute "deprecated_attribute"
  , unknown_attribute "unknown_attribute" ]

attribute_name_is n v = do
  c <- constant_pool v
  c <- c .? "constant_Utf8_info"
  Just (asString(c."bytes") == n)

constantValue_attribute = struct
  [ u2 "attribute_name_index" ?? attribute_name_is "ConstantValue"
  , u4 "attribute_length" ?= 2
  , u2 "constantvalue_index" ~~> constant_pool ]

code_attribute = struct
  [ u2 "attribute_name_index" ?? attribute_name_is "Code"
  , u4 "attribute_length"
  , u2 "max_stack"
  , u2 "max_locals"
  , u4 "code_length"
  , arrayG u1 "code" (\v->asInteger(v"code_length"))
  , u2 "exception_table_length"
  , arrayG exception "exception_table" (\v->asInteger(v"exception_table_length"))
  , u2 "attributes_count"
  , array attribute_info "attributes" ]

exception = struct
  [ u2 "start_pc"
  , u2 "end_pc"
  , u2 "handler_pc"
  , u2 "catch_type" ]

{- TODO: Now it's incomplete
stackMapTable_attribute = struct
  [ u2 "attribute_name_index" ?? attribute_name_is "StackMapTable"
  , u4 "attribute_length"
  , u2 "number_of_entries"
  , arrayG stack_map_frame "entries" (\v->asInteger(v"number_of_entries")) ]
-}

exceptions_attribute = struct
  [ u2 "attribute_name_index" ?? attribute_name_is "Exceptions"
  , u4 "attribute_length"
  , u2 "number_of_exceptions"
  , arrayG u2 "exception_index_table" (\v->asInteger(v"number_of_exceptions")) ]

innerClasses_attribute = struct
  [ u2 "attribute_name_index" ?? attribute_name_is "InnerClasses"
  , u4 "attribute_length"
  , u2 "number_of_classes"
  , arrayG inner_class_info "classes" (\v->asInteger(v"number_of_classes")) ]

inner_class_info = struct
  [ u2 "inner_class_info_index"
  , u2 "outer_class_info_index"
  , u2 "inner_name_index"
  , u2 "inner_class_access_flags" ]

enclosingMethod_attribute = struct
  [ u2 "attribute_name_index" ?? attribute_name_is "EnclosingMethod"
  , u4 "attribute_length"
  , u2 "class_index"
  , u2 "method_index" ]

synthetic_attribute = struct
  [ u2 "attribute_name_index" ?? attribute_name_is "Synthetic"
  , u4 "attribute_length" ]

signature_attribute = struct
  [ u2 "attribute_name_index" ?? attribute_name_is "Signature"
  , u4 "attribute_length"
  , u2 "signature_index" ]

sourceFile_attribute = struct
  [ u2 "attribute_name_index" ?? attribute_name_is "SourceFile"
  , u4 "attribute_length"
  , u2 "sourcefile_index" ]

sourceDebugExtension_attribute = struct
  [ u2 "attribute_name_index" ?? attribute_name_is "SourceDebugExtension"
  , u4 "attribute_length"
  , arrayG u1 "debug_extension" (\v->asInteger(v"attribute_length")) ]

lineNumberTable_attribute = struct
  [ u2 "attribute_name_index" ?? attribute_name_is "LineNumberTable"
  , u4 "attribute_length"
  , u2 "line_number_table_length"
  , arrayG line_number_info "line_number_table" (\v->asInteger(v"line_number_table_length")) ]

line_number_info = struct
  [ u2 "start_pc"
  , u2 "line_number" ]

localVariableTable_attribute = struct
  [ u2 "attribute_name_index" ?? attribute_name_is "LocalVariableTable"
  , u4 "attribute_length"
  , u2 "local_variable_table_length"
  , arrayG local_variable_info "local_variable_table" (\v->asInteger(v"local_variable_table_length")) ]

local_variable_info = struct
  [ u2 "start_pc"
  , u2 "length"
  , u2 "name_index"
  , u2 "descriptor_index"
  , u2 "index" ]

localVariableTypeTable_attribute = struct
  [ u2 "attribute_name_index" ?? attribute_name_is "LocalVariableTypeTable"
  , u4 "attribute_length"
  , u2 "local_variable_type_table_length"
  , arrayG local_variable_type_info "local_variable_type_table" (\v->asInteger(v"local_variable_type_table_length")) ]

local_variable_type_info = struct
  [ u2 "start_pc"
  , u2 "length"
  , u2 "name_index"
  , u2 "signature_index"
  , u2 "index" ]

deprecated_attribute = struct
  [ u2 "attribute_name_index" ?? attribute_name_is "Deprecated"
  , u4 "attribute_length" ]

unknown_attribute = struct
  [ u2 "attribute_name_index" ~~> constant_pool
  , u4 "attribute_length"
  , arrayG u1 "info" (\v-> asInteger(v"attribute_length")) ]

-- }}} The grammar of Java bytecode stops here.

-- Each grammar rule from above constructs an in-memory representation of the
-- bytecode being parsed.  Behind the scenes, arrays are structures whose field
-- names are "0", "1", and so on.

data Value = Struct (OMap String Ast) | Base [Word8]
data Ast = Ast Value [Follower]

-- The typical line in the grammar looks as follows.
--    typeName "fieldName"
-- This expression should evaluate to a parser.  Each parser consumes bytes and
-- builds an Ast, but doesn't (typically) need the field name.  The field name
-- is useful for the caller of the parser; so, parsers return a field name,
-- even if parsing fails.  When parsing everything finishes successfully there
-- are |Just []| bytes remaining; when parsing fails there is |Nothing|
-- remaining.

type Parser = [Ast] -> [Word8] -> (String, Ast, Maybe [Word8])

u' :: Int -> [Word8] -> (Ast, Maybe [Word8])
u' n bs
    | length cs < n   = (Ast (Base cs) [], Nothing)
    | otherwise       = (Ast (Base cs) [], Just ds)
  where (cs, ds) = splitAt n bs

u :: Int -> String -> Parser
u n field _ bs = makeParseResult field (u' n bs)

u1 = u 1
u2 = u 2
u4 = u 4

-- The empty string refers to the current Ast node; so, it can't be used as a
-- field name, just like the keyword "this" in Java.

makeParseResult field (t, mbs)
  | field /= ""   = (field, t, mbs)
  | otherwise     = error "INTERNAL: Field name can't be empty."

-- There are three kinds of parser combinators.
-- 1. The combinators |struct|, |union|, and |array| apply other parsers and
--    then combine the resulting values.
-- 2. The combinator |(??)| applies a semantic check to the Ast built so far.
-- 3. The combinator |(~~>)| simply stores a follower in the Ast.  A follower
--    is used during printing to cross-link various parts of the whole Ast.

-- Let's start with the checker combinator, because it illustrates how the Ast
-- position is used.  The position of the Ast node x is a list [x, p(x),
-- p(p(x)), ..., r], where p(x) is the parent of x, and r is the root of the
-- whole Ast.  The function |resolve| navigates from a position to another
-- position according to a field name.
--
-- The specialized checker combinator |(?=)| covers most uses, with a simpler
-- syntax.

type Resolver = String -> [Ast]

(??) :: Parser -> (Resolver -> Maybe Bool) -> Parser
(parse ?? check) ts bs =
  case parse ts bs of
    (r @ (f, t, Nothing)) -> r
    (f, t, Just bs) ->
      let { mbs = do
        ok <- check $ resolve (t : ts)
        if ok then Just bs else Nothing
      } in (f, t, mbs)

(?=) :: Parser -> Integer -> Parser
parse ?= value = parse ?? (\v -> Just (asInteger(v"") == value))

-- There should be no failed lookup if the grammar is correct.  (It would be
-- interesting to turn this runtime check into a type check.)

resolve :: [Ast] -> Resolver
resolve ts "" =
--  unsafePerformIO (putStrLn "resolve:this") `seq`
  ts
resolve (ts @ (Ast (Struct m) _ : ts')) field =
--  unsafePerformIO (putStrLn ("resolve " ++ field ++ " in struct")) `seq`
  maybe (resolve ts' field) (:ts) $ OMap.lookup field m
resolve (Ast (Base _) _: ts') field =
--  unsafePerformIO (putStrLn "ignoring Base") `seq`
  resolve ts' field
resolve [] field = badGrammar ("Can't resolve " ++ field ++ ".")

badGrammar :: String -> a
badGrammar m = error ("INTERNAL: bad grammar: " ++ m)

-- Followers provide cross-links in the Ast structure.  The combinator |(~~>)|
-- stores them in the Ast.

type Follower = Resolver -> Maybe [Ast]

(~~>) :: Parser -> Follower -> Parser
(parse ~~> follow) ts bs =
  case parse ts bs of
    (f, Ast v fs, mbs) -> (f, Ast v (follow : fs), mbs)

-- A structure is a essentially a map from field names to Ast-s.  Subparsers
-- have already been given the field names.

struct :: [Parser] -> String -> Parser
struct ps field ts bs = makeParseResult field (struct' ps ts bs)

struct' :: [Parser] -> [Ast] -> [Word8] -> (Ast, Maybe [Word8])
struct' ps ts bs =
  let ast m = Ast (Struct m) [] in
  let { pf acc parse = case acc of
    (m, Nothing) -> acc
    (m, Just bs) ->
      let (f, t, mbs) = parse ((ast m) : ts) bs in
      (OMap.insert f t m, mbs)
  } in
  let r = foldl pf (OMap.empty, Just bs) ps in
  mapFst ast r

-- A union tries all its members one by one, and returns the first that
-- succeeds.   Parsing fails only if all alternatives fail.  In that case, we
-- keep the one with the biggest Ast.

-- TODO: assert that ps isn't empty
union :: [Parser] -> String -> Parser
union [] _ _ _ = badGrammar "empty union"
union ps field ts bs =
  let { pick r1 r2 = case (r1, r2) of
    ((_,_,Nothing), (_,_,Just _)) -> r2
    ((_,_,Just _), (_,_,_)) -> r1
    ((_,t1,Nothing), (_,t2,Nothing)) ->
      if sizeOfAst t1 >= sizeOfAst t2 then r1 else r2
  } in
  let z = ("<empty union>", Ast (Base []) [], Nothing) in
  let (f, t, mbs) = foldl pick z [p ts bs | p <- ps] in
  makeParseResult field (Ast (Struct (OMap.singleton f t)) [], mbs)

sizeOfAst :: Ast -> Int
sizeOfAst (Ast (Base bs) _) = length bs
sizeOfAst (Ast (Struct m) _) = OMap.fold (\_ t sz -> sz + sizeOfAst t) 0 m

-- Arrays are very similar to structures.  The main difference is that their
-- length must be computed: It is unknown at the time when this Haskell program
-- is compiled.  The general version |arrayG| receives a function that computes
-- the array length, based on the content of the Ast so far.  The specialized
-- version |array| covers common cases.

arrayG :: (String -> Parser) -> String -> (Resolver -> Integer) -> Parser
arrayG element field size ts bs =
  let sz = size (resolve ts) in
  let r = struct' (map element $ map show [0..sz-1]) ts bs in
  makeParseResult field r

array :: (String -> Parser) -> String -> Parser
array element field = arrayG element field (\v -> asInteger(v(lengthName field)))

-- The operators |(.)|, |(.?)|, and |(!)| navigte the Ast.  There are two
-- failure modes: a bad input is signaled by |Nothing|, and a bad grammar is
-- signaled by |badGrammar|.  All three operators should be used to go down one
-- level from a |Struct|: |(.)| for structures, |(.?)| for unions, and |(!)|
-- for arrays.

(.) :: [Ast] -> String -> [Ast]
(ts @ (Ast (Struct m) _ : _)) . f =
  maybe (badGrammar ("no field named " ++ f)) (:ts) (OMap.lookup f m)
(Ast (Base _) _ : _) . _ = badGrammar "treating primitive as a struct"
[] . _ = badGrammar "treating an invalid position as a struct"

(.?) :: [Ast] -> String -> Maybe [Ast]
(ts @ (Ast (Struct m) _ : _)) .? f = fmap (:ts) (OMap.lookup f m)
(Ast (Base _) _ : _) .? _ = badGrammar "treating primitive as a union"
[] .? [] = badGrammar "treating an invalid position as a union"

(!) :: [Ast] -> Integer -> Maybe [Ast]
(ts @ (Ast (Struct m) _ : _)) ! i = ts .? show i
(Ast (Base _) _ : _) ! _ = badGrammar "trying to index a primitive"
[] ! _ = badGrammar "treating an invalid position as an array"

-- The converters |asInteger| and |asString| are the last part of the parser.

asBytes :: Ast -> [Word8]
asBytes (Ast (Struct m) _) = OMap.values m >>= asBytes
asBytes (Ast (Base bs) _) = bs

asString :: [Ast] -> String
asString (t : _) = map toEnum $ map fromEnum (asBytes t)
asString [] = badGrammar "trying to cast invalid position to string"

asInteger :: [Ast] -> Integer
asInteger (t : _) = integerOrder (asBytes t)
asInteger [] = badGrammar "trying to cast invalid position to integer"

bigEndian :: [Word8] -> Integer
bigEndian bs = foldl (\n d -> 2 ^ 8 * n + d) 0 $ map toInteger bs

littleEndian :: [Word8] -> Integer
littleEndian bs = bigEndian (reverse bs)

-- The printer begins here.  It is the third important part of this program,
-- after the grammar and the parsing combinators.

-- TODO: Follow followers.
printAsText' :: String -> [Ast] -> IO ()
printAsText' nl (ts @ (t @ (Ast (Struct m) _) : _ )) =
  let nl' = nl ++ "  " in
  let { b f t p =
    p >> printf "%s%s =" nl' (f :: String) >> printAsText' nl' (t : ts)
  } in
  do
    printf " {"
    OMap.fold b (return ()) m
    printf "%s}" nl
    when (looksLikeAString t) (printf " = \"%s\"" (asString ts))
printAsText' nl ((Ast (Base bs) _) : _) = do
  forM_ bs (printf " %02x")
  let n = integerOrder bs
  when (n < 1000) (printf " = dec %d" n)
printAsText' _ [] = error "INTERNAL: printAsText' called on an invalid position"

printAsText :: Ast -> IO ()
printAsText t = printAsText' "\n" [t]

looksLikeAString :: Ast -> Bool
looksLikeAString t = astLooksLikeAString 100 t >= 0

astLooksLikeAString (-1) _ = -1
astLooksLikeAString limit (Ast (Base bs) _) = bytesLookLikeAString limit bs
astLooksLikeAString limit (Ast (Struct m) _) =
  OMap.fold f limit m
  where
    f _ _ (-1) = -1
    f _ t limit = astLooksLikeAString limit t

bytesLookLikeAString (-1) _ = -1
bytesLookLikeAString limit [] = limit
bytesLookLikeAString limit (b : bs)
  | isPrint (toEnum (fromEnum b))   = bytesLookLikeAString (limit - 1) bs
  | otherwise   = -1

-- Some small utilities follow.

mapFst f (x, y) = (f x, y)
mapSnd f (x, y) = (x, f y)

-- TODO
--  - Introduce notation. For example bs, cs for bytes; vs for values, ...
--  - When parsing fails, it should say why.
--  - Review & fix, after it works well enough.
--  - Compile with all warnings turned on and fix.
--  - Add a check for attribute lengths.
