{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
--{-# LANGUAGE MultiParamTypeClasses#-}
-- from ~/.stack/indices/Hackage/packages/rdf4h/3.0.1/rdf4h-3.0.1.tar.gz!rdf4h-3.0.1/testsuite/tests/Data/RDF/GraphTestUtils.hs
module Lib
    ( someFunc,
      triple2,
      triple3,
      triple4,
      matchFirst,
      canStrip
--      parseRdf1
    ) where
--import Data.RDF.Types
--import Lib2
import LibData (load)
import LibData2 (load)
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

import Rdf(Node(..), Triple(..), LValue(..))

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

exampleTTLFile = "/home/mdupont/experiments/gcc-ontology/data/clean2.ttl"

someFunc = do
  let x1 = LibData.load
  let x2 = LibData2.load
  --let x = merge x1 x1
  let y = take 1 x1
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
  

rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
bu = "https://h4ck3rm1k3.github.io/gogccintro/gcc/ontology/2017/05/20/gcc_compiler.owl#"
bu2 = "http://www.co-ode.org/ontologies/ont.owl#"
owl = "http://www.w3.org/2002/07/owl#"
triple2 t = case t of
  Triple a b c -> case a of
    UNode (x) -> do
      let v2 = bu `isPrefixOf` x
      case v2 of
        False -> do
          let v2 = bu2 `isPrefixOf` x
          let v3 = stripPrefix bu2 x
          case v3 of
            Just v4 -> v4
        True -> do
          let v3 = stripPrefix bu x
          case v3 of
            Just v4 -> v4
              --let v5 = readMaybe v4 :: Maybe Int
              --case v5 of
              --  Just v6 -> v6

canStrip b t = do
  let v2 = isPrefixOf b t
  case v2 of
    True -> do
      stripPrefix b t
    False -> Nothing
      

--matchFirst :: (Ord a) => [a] -> a
matchFirst t []  = Nothing
matchFirst t [x]  = canStrip x t
matchFirst t (x:xs)  = do  
  let v2 = canStrip x t
  case v2 of
    Just v3 -> v2
    Nothing -> matchFirst t xs
  
procNode c =
  case c of 
    UNode (x) -> do
      matchFirst x [ bu , bu2 , rdf, owl ]
    LNode (PlainL s) -> Just s

triple4 t = case t of
  Triple a b c -> [
    procNode a,
      procNode b,
          procNode c
                  ]

          
   
                        
