module Main where

-- TODO: At the end delete the unused ones.
import Control.Monad
import Data.Maybe
import Data.Typeable
import Data.Word
import OMap (OMap)
import Prelude hiding ((.), (!!))
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
  case bytecode (B.unpack bc) of
    Nothing -> printf "%s: parsing failed\n" fn
    Just (t, bs) -> do
      printf "%s\n" (toText t)
      let w = printf "%s: %d unexpected bytes at end of file\n" fn (length bs)
      when (bs /= []) w

-- The grammar of Java bytecode starts here. {{{

integerOrder = bigEndian
lengthName x = x ++ "_count"

bytecode bs = let (_, r) = class_file "TOP" [] bs in r

class_file = struct
  [ u4 "magic" ?= 0xCAFEBABE
  , u2 "minor_version"
  , u2 "major_version"
  , u2 "constant_pool_count"
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

constant_pool v = v"constant_pool" !! (asInteger(v"") - 1)

cp_info = union
  [ constant_Class_info
  , constant_Fieldref_info
  , constant_Methodref_info
  , constant_InterfaceMethodref_info
  , constant_String_info
  , constant_Integer_info
  , constant_Float_info
  , constant_Long_info
  , constant_Double_info
  , constant_NameAndType_info
  , constant_Utf8_info
  , constant_MethodHandle_info
  , constant_MethodType_info
  , constant_InvokeDynamic_info ]

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
  [ constantValue_attribute
  , code_attribute
--  , exceptions_attribute
--  , innerClasses_attribute
--  , enclosingMethod_attribute
--  , synthetic_attribute
--  , signature_attribute
--  , sourceFile_attribute
--  , sourceDebugExtension_attribute
--  , lineNumberTable_attribute
--  , localVariableTable_attribute
--  , localVariableTypeTable_attribute
--  , deprecated_attribute
  , unknown_attribute ]

constantValue_attribute = struct
  [ u2 "attribute_name_index"
      ?? (\v -> do
        c <- constant_pool v
        Just (asString(c."bytes") == "ConstantValue"))
  , u4 "attribute_length" ?= 2
  , u2 "constantvalue_index" ~~> constant_pool ]

code_attribute = struct
  [ u2 "attribute_name_index"
      ?? (\v-> do
        c <- constant_pool v
        Just (asString(c."bytes") == "Code"))
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
-- even if parsing fails.

type Parser = [Ast] -> [Word8] -> (String, Maybe (Ast, [Word8]))

u' :: Int -> [Word8] -> Maybe (Ast, [Word8])
u' n bs
    | length cs < n   = Nothing
    | otherwise       = Just (Ast (Base cs) [], ds)
  where (cs, ds) = splitAt n bs

u :: Int -> String -> Parser
u n field _ bs = makeParseResult field (u' n bs)

u1 = u 1
u2 = u 2
u4 = u 4

-- The empty string refers to the current Ast node; so, it can't be used as a
-- field name, just like the keyword "this" in Java.

makeParseResult field r
  | field /= ""   = (field, r)
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
  let { check' r = do
    (t, _) <- r
    ok <- check $ resolve (t : ts)
    if ok then r else Nothing
  } in mapSnd check' $ parse ts bs

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
   (mapSnd $ fmap $ mapFst $ \ (Ast v fs) -> Ast v (follow:fs)) $ parse ts bs

-- A union tries all its members one by one, and returns the first that
-- succeeds.   Parsing fails only if all alternatives fail.  Union fields do
-- not have names, unlike C; so, we use a dummy name.

-- TODO: Union must have field names.
union :: [String -> Parser] -> String -> Parser
union ps field ts bs =
  let rs = [r | p <- ps, let (_, r) = p "<unionField>" ts bs] in
  (field, head $ (filter isJust rs ++ [Nothing]))

-- A structure is a essentially a map from field names to Ast-s.  Subparsers
-- have already been given the field names.

struct :: [Parser] -> String -> Parser
struct ps field ts bs = makeParseResult field (struct' ps ts bs)

struct' :: [Parser] -> [Ast] -> [Word8] -> Maybe (Ast, [Word8])
struct' ps ts bs =
  let { pf acc parse = do   -- Process one Field
    (m, bs) <- acc
    let (f, r) = parse (Ast (Struct m) [] : ts) bs
    (t, bs) <- r
    unsafePerformIO (putStrLn ("parsed field " ++ f)) `seq` Just (OMap.insert f t m, bs)
  } in
  let r = foldl pf (Just (OMap.empty, bs)) ps in
  fmap (mapFst (\m -> Ast (Struct m) [])) r

-- Arrays are very similar to structures.  The main difference is that their
-- length must be computed: It is unknown at the time when this Haskell program
-- is compiled.  The general version |arrayG| receives a function that computes
-- the array length, based on the content of the Ast so far.  The specialized
-- version |array| covers common cases.

arrayG :: (String -> Parser) -> String -> (Resolver -> Integer) -> Parser
arrayG element field size ts bs =
  let sz = size (resolve ts) in
  let r = struct' (map element $ map show [0..sz-1]) ts bs in
  (field, r)

array :: (String -> Parser) -> String -> Parser
array element field = arrayG element field (\v -> asInteger(v(lengthName field)))

-- There operators |(.)| and |(!!)| are useful for navigating the Ast.  In a
-- way, |(.)| is the counterpart of |struct|, and |(!!)| is the counterpart of
-- |arrayG|.  As in the case of |resolve| there are two failure modes:
-- |Nothing| indicates an error in the file being parsed, |badGrammar|
-- indicates a bug in this parser.

(.) :: [Ast] -> String -> [Ast]
(ts @ (Ast (Struct m) _ : _)) . f =
  maybe (badGrammar ("no field named " ++ f)) (:ts) (OMap.lookup f m)
(Ast (Base _) _ : _) . _ = badGrammar ("trying to read field of primitive")
[] . _ = badGrammar "how did I get called with an invalid position?"

(!!) :: [Ast] -> Integer -> Maybe [Ast]
(ts @ (Ast (Struct m) _ : _)) !! i = fmap (:ts) (OMap.lookup (show i) m)
(Ast (Base _) _ : _) !! _ = badGrammar "trying to index a primitive"
[] !! _ = badGrammar "how did I get called with an invalid position?"

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

toText :: Ast -> String
toText _ = "AST" -- CONTINUE

-- Some small utilities follow.

mapFst f (x, y) = (f x, y)
mapSnd f (x, y) = (x, f y)

-- TODO
--  - Introduce notation. For example bs, cs for bytes; vs for values, ...
--  - When parsing fails, it should say why.
--  - Review & fix, after it works well enough.
