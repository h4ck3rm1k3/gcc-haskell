{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
--{-# LANGUAGE MultiParamTypeClasses#-}
-- from ~/.stack/indices/Hackage/packages/rdf4h/3.0.1/rdf4h-3.0.1.tar.gz!rdf4h-3.0.1/testsuite/tests/Data/RDF/GraphTestUtils.hs
module Lib
    ( someFunc,
--      parseRdf1
    ) where
--import Data.RDF.Types
--import Lib2
import LibData (load)
import Data.List
import Text.Read
import qualified Data.Text as T
-- import Data.RDF.Query
-- import Data.RDF.Namespace
-- --import Data.Attoparsec
-- import Text.RDF.RDF4H.NTriplesSerializer
-- import qualified Data.Text as T
-- --import Data.RDF.Types
-- import Text.RDF.RDF4H.NTriplesParser
-- --import Text.RDF.RDF4H.TurtleParser -- TurtleParserCustom
-- import Data.RDF.Graph.TList
-- import System.IO
-- import Data.Attoparsec.ByteString (parse,IResult(..))
--import Text.RDF.RDF4H.ParserUtils

--cb x = Foo2(x)
--processRdf2
--cb2 = (ProcessRdf cb)
--cb3 :: Result [Maybe Triple] -> IO b
--cb2 x =  1
--parseRdf1 :: FilePath -> IO b
--IO (Either ParseFailure (RDF a0))
--parseRdf1 = Lib2.parseFileAttoparsecCallback exampleTTLFile

import Rdf(Node(..))

class OwlOntology a where
  baseUrl :: a -> String

data GccOntologyData =
  GccOntology2()

  
instance OwlOntology GccOntologyData where
  baseUrl a = "https://h4ck3rm1k3.github.io/gogccintro/gcc/ontology/2017/05/20/gcc_compiler.owl#"

data MainRDF = MainRDF
  | Type -- "type"
  
data RDFS = RDFS()
data OWL = OWL()
data DC = DC()


instance OwlOntology MainRDF where
  baseUrl a = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

instance OwlOntology RDFS where
  baseUrl a = "http://www.w3.org/2000/01/rdf-schema#"

instance OwlOntology OWL where
  baseUrl a = "http://www.w3.org/2002/07/owl#"

instance OwlOntology DC where
  baseUrl a = "http://purl.org/dc/elements/1.1/"
  
  
-- (UNode "#type") (UNode "https://h4ck3rm1k3.github.io/gogccintro/gcc/ontology/2017/05/20/gcc_compiler.owl#string_cst")


data GccPredicates =
  StringNode
  | FunctionNameString(GccPredicates)

data GccTypes = 
  StringCst

data GccElements =
  Predicate(GccPredicates)
  | Types(GccTypes)
  
class OwlNodeClass a where
  nameString :: a -> String

-- get_pred p = case p of StringNode-> "strg"
--get_type t = case t of StringCst-> "string_cst"


instance OwlNodeClass MainRDF where
  nameString a =  case a of
                    Type -> "strg"
                    MainRDF ->  "rdf"
                    
  
instance OwlNodeClass GccElements where
--  nameString o a = "strg"
-- instance OwlNodeClass GccOntologyData GCCConsts where
  nameString a =
    case a of
      
      Predicate(p) -> case p of
                        StringNode -> "strg"
                        FunctionNameString(x) -> "strg"
      Types(t) -> case t of
                    StringCst-> "string_cst"
                   
               
  
list_of_types = [
  Predicate(StringNode),
  Types(StringCst)
      ]

--parseRdf1 :: IO (Either ParseFailure (RDF TList))
exampleTTLFile = "/home/mdupont/experiments/gcc-ontology/data/clean2.ttl"
--parseRdf1 = Lib2.parseFileAttoparsec exampleTTLFile
      --parsedRDF   = f fromEither :: IO (RDF TList)
--  f :: IO (Either ParseFailure (RDF a0))
--a0  instance Rdf TList -- Defined in ‘Data.RDF.Graph.TList’
  --2
--parseRdf = parseRdf1 
--someFunc :: IO ()
--dox x = --  IO (RDF TList)

process_list_item x = ""
-- foo x = case x of Triple a b c -> do
--                     case c of
--                       UNode c1 -> c1
--                       BNode b -> b
--                       BNodeGen c -> T.pack "x"
--                       LNode l -> case l of
--                         PlainL l2 -> l2
--                         PlainLL l2 l3 -> l3
--                         TypedL l3 l4 -> l4


-- strings = [
--   "strg: %-7s ",
--     "n%*s ",
--     "%*s ",
--     "%-16s ",
--     "%-4s: %s ",
--     "%-4s: %s ",
--     "-uid ",
--     "._80",    
--     "@%-6u "
-- ]


-- somesteps xt = do
--   let xt2 = map process_list_item xt
-- -- extract the predicates
--   let pred = map foo xt
  
--   let l6 = take 1 xt
--   let l7 = case l6 of
--              [x] -> x
--              --[] -> (Triple Nothing Nothing Nothing)
--              --(a:b:c) -> (Triple a b c)
             
--   --l6 :: [Triple]
--   let subject = case l6 of [Triple a b c] -> a
-- --  let subject_id = case subject of BNode(id1) -> id1
--   let pred = case l6 of [Triple a b c] -> b
--   let obj = case l6 of [Triple a b c] -> c
--   obj

someFunc = do
  let x = LibData.load
  let y = take 1 x
  --let s = show y
  putStrLn "ok"

-- extract_string i2 = do
-- --  i2 :: Node
--   case i2  of
--     --[] -> T.pack "Empty array"
--     --_ -> T.pack "other"
--     LNode (PlainL s) -> s
--     --UNode (url) -> url
--     --BNode b -> b
--     --UNode _:_:_ -> T.pack "UNode"
--     --BNode _:_ -> T.pack "BNode"
--     --BNodeGen _:_ -> T.pack "BNodeGen"
--     --BNodeGen b -> T.pack "Some Node"
--     --LNode (PlainLL a b) ->  b
                      
--extract_object x = (extract_string (objectOf x))

-- someFunc2 = do
--   x <- parseRdf1 -- remove the IO
--   let x4 = case x of Right x3 -> x3
--   let xt = triplesOf x4 -- all the triples Data.RDF.Types.triplesOf :: Data.RDF.Types.RDF rdfImpl

--   let f = somesteps xt
--   let x2 = show f
--   putStrLn x2
--  let triples = query graph (Just (unode eswcCommitteeURI)) (Just (unode heldByProp)) Nothing
  --show subject_id
  --x4 :: RDF TList  
  --let x2 = showGraph x
--  let x2 = case x of Right x3 -> x3
  
  --x4 :: RDF TList
  -- x2
  -- :: Either
  --      Data.RDF.Types.ParseFailure
  --      (Data.RDF.Types.RDF Data.RDF.Graph.TList.TList)
       
  putStrLn "hello"
  --putStrLn "someFunc" + rdf2
  --withFile "out.nt" WriteMode (\h -> hWriteRdf NTriplesSerializer h rdfGraph

--x <- parseRdf1 -- remove the IO

load_step3 x = do
  let x4 = case x of
             Right x3 -> x3
             -- Left erro
  x4

--triple :: Node -> t1 -> t -> [Char]

--load :: [[Char]]

--import Data.RDF.Query
--import Data.RDF.Namespace
--import Data.Attoparsec
--import Text.RDF.RDF4H.NTriplesSerializer


-- data Text
--   = Data.Text.Internal.Text {-# UNPACK #-}Data.Text.Array.Array
--                             {-# UNPACK #-}Int
--                             {-# UNPACK #-}Int
--Data.Text.Internal.Text :: Data.Text.Array.Array -> Int -> Int -> Text
  
--import Data.RDF.Types
--import Text.RDF.RDF4H.NTriplesParser
----import Text.RDF.RDF4H.TurtleParser -- TurtleParserCustom
--import Data.RDF.Graph.TList
--import System.IO
--import Data.Attoparsec.ByteString (parse,IResult(..))
--import Text.RDF.RDF4H.ParserUtils
--import qualified Data.Text as T
--import Data.String

--data NodeRef =
  
  




bu = "https://h4ck3rm1k3.github.io/gogccintro/gcc/ontology/2017/05/20/gcc_compiler.owl#"

triple2 a b c =
  case a of
--    LNode (x) -> "Bar"
    UNode (x) -> do
      let v2 = bu `isPrefixOf` x
      case v2 of
        True -> do
          let v3 = stripPrefix bu x
          case v3 of
            Just v4 -> do
              let v5 = readMaybe v4 :: Maybe Int
              case v5 of
                Just v6 -> NodeId(v6)

      --case x of [Char] ->
--      case x of UNode(y) ->
--                  "Foo"
          
   
                        

--instance Show a => Show (Node a) where
--      showsPrec _ x = showsNode x

-- --showGraph' gr = concatMap (\t -> show t ++ "\n") (expandTriples gr)
-- expandTriples :: (Rdf a) => RDF a -> Triples
-- expandTriples rdf = expandTriples' [] (baseUrl rdf) (prefixMappings rdf) (triplesOf rdf)

-- expandTriples' :: Triples -> Maybe BaseUrl -> PrefixMappings -> Triples -> Triples
-- expandTriples' acc _ _ [] = acc
-- expandTriples' acc baseURL prefixMaps (t:rest) = expandTriples' (normalize baseURL prefixMaps t : acc) baseURL prefixMaps rest
--   where normalize baseURL' prefixMaps' = absolutizeTriple baseURL' . expandTriple prefixMaps'

-- -- |Expand the triple with the prefix map.
-- expandTriple :: PrefixMappings -> Triple -> Triple
-- expandTriple pms t = triple (expandNode pms $ subjectOf t) (expandNode pms $ predicateOf t) (expandNode pms $ objectOf t)

-- -- |Expand the node with the prefix map.
-- -- Only UNodes are expanded, other kinds of nodes are returned as-is.
-- expandNode :: PrefixMappings -> Node -> Node
-- expandNode pms (UNode n) = unode $ expandURI pms n
-- expandNode _ n'          = n'

-- -- |Expand the URI with the prefix map.
-- -- Also expands "a" to "http://www.w3.org/1999/02/22-rdf-syntax-ns#type".
-- expandURI :: PrefixMappings -> T.Text -> T.Text
-- expandURI _ "a"  = T.append (NS.uriOf NS.rdf) "type"
-- expandURI pms' x = firstExpandedOrOriginal x $ catMaybes $ map (resourceTail x) (NS.toPMList pms')
--   where resourceTail :: T.Text -> (T.Text, T.Text) -> Maybe T.Text
--         resourceTail x' (p', u') = T.stripPrefix (T.append p' ":") x' >>= Just . T.append u'
--         firstExpandedOrOriginal :: a -> [a] -> a
--         firstExpandedOrOriginal orig' [] = orig'
--         firstExpandedOrOriginal _ (e:_)  = e

--load_step :: RDF TList
-- load_step = do
--   x <- parseRdf1 -- remove the IO
  
-- extract_list :: RDF TList
-- extract_list x = do
--   x :: Either ParseFailure (RDF TList)
--   let x4 = case x of Right x3 -> x3
--   x4 :: RDF TList
--   x4

    
-- load_store = do
--   x <- parseRdf1 -- remove the IO
--   let x4 = case x of Right x3 -> x3
--   --let xt = triplesOf x4 -- all the triples Data.RDF.Types.triplesOf :: Data.RDF.Types.RDF rdfImpl
--   x4

--current_database = load_store
--query_current_store = query current_database

--get_one_url x = case x of [UNode url] -> url

--string_constants = query_current_store Nothing (Just (UNode (T.pack "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))) (Just(UNode (T.pack "https://h4ck3rm1k3.github.io/gogccintro/gcc/ontology/2017/05/20/gcc_compiler.owl#string_cst")))
