{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
--{-# LANGUAGE MultiParamTypeClasses#-}
-- from ~/.stack/indices/Hackage/packages/rdf4h/3.0.1/rdf4h-3.0.1.tar.gz!rdf4h-3.0.1/testsuite/tests/Data/RDF/GraphTestUtils.hs
module Lib
    ( someFunc,
--      triple2,
--      triple3,
      baseUrl,
      ontologies,
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
import qualified Data.Map.Strict as Map
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

--data SomeInstance = SomeInstance
class OwlOntology a where
  baseUrl :: a -> String
  make_instance  :: a -> String -> a
  

data GccOntologyData =  GccOntologyData
  | SomeGCCLiteral(String) 
  | SomeInstance(String)
  deriving Show

data GccOntologyData2 =  GccOntologyData2
                      | SomeInstance2(String)
  deriving Show

data StatementPart =
  Subject(Maybe Ontos)
  |Predicate(Maybe Ontos)
  |Object(Maybe Ontos)
  deriving Show

data Statement = Statement(StatementPart,StatementPart,StatementPart)
  deriving Show

data MainRDF = MainRDF
  | Type -- "type"
  | SomeInstance3(String)
  deriving Show

data RDFS = RDFS
  | SomeInstance4(String)
  deriving Show
data OWL = OWL
  | SomeInstance5(String)
  deriving Show
data DC = DC
    | SomeInstance6(String)
        deriving Show

data Ontos =  AGccOntologyData(GccOntologyData)
  | AGccOntologyData2( GccOntologyData2 )
  | AMainRDF(MainRDF)
  | ARDFS(RDFS) 
  | AOWL (OWL )
  | ADC (DC )
  deriving Show

data GccPredicates =
  StringNode
  | FunctionNameString(GccPredicates)
  deriving Show

data GccTypes = 
  StringCst
  deriving Show

data GccElements =
  APredicate(GccPredicates)
  | Types(GccTypes)
  deriving Show

instance OwlOntology GccOntologyData where
  baseUrl a = "https://h4ck3rm1k3.github.io/gogccintro/gcc/ontology/2017/05/20/gcc_compiler.owl#"
  make_instance a x = SomeInstance(x)

instance OwlOntology GccOntologyData2 where
  baseUrl a = "http://www.co-ode.org/ontologies/ont.owl#"
  make_instance a x = SomeInstance2(x)

instance OwlOntology MainRDF where
  baseUrl a = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  make_instance a x = SomeInstance3(x)  

instance OwlOntology RDFS where
  baseUrl a = "http://www.w3.org/2000/01/rdf-schema#"
  make_instance a x = SomeInstance4(x)
  
instance OwlOntology OWL where
  baseUrl a = "http://www.w3.org/2002/07/owl#"
  make_instance a x = SomeInstance5(x)
  
instance OwlOntology DC where
  baseUrl a = "http://purl.org/dc/elements/1.1/"
  make_instance a x = SomeInstance6(x)
  
instance OwlOntology Ontos where
  baseUrl a = case a of 
    AGccOntologyData(b) -> baseUrl b
    AGccOntologyData2( b ) -> baseUrl b
    AMainRDF(b) -> baseUrl b
    ARDFS(b)  -> baseUrl b
    AOWL (b ) -> baseUrl b
    ADC (b) -> baseUrl b

  make_instance a x = case a of 
     AGccOntologyData(b) -> AGccOntologyData(make_instance b x)
     AGccOntologyData2( b ) -> AGccOntologyData2(make_instance b x)
     AMainRDF(b) -> AMainRDF(make_instance b x)
     ARDFS(b)  -> ARDFS(make_instance b x)
     AOWL (b ) -> AOWL(make_instance b x)
     ADC (b) -> ADC(make_instance b x)
  
--dosomth x = baseUrl x
-- distribute f x  = case x of 
--     AGccOntologyData(b) -> f b
--     AGccOntologyData2( b ) -> f b
--     AMainRDF(b) -> f b
--     ARDFS(b)  -> f b
--     AOWL (b ) -> f b
--     ADC (b) -> f b

ontologies = [
  AGccOntologyData(GccOntologyData) ,
  AGccOntologyData2(GccOntologyData2 ),
  AMainRDF(MainRDF),
  ARDFS(RDFS) ,
  AOWL(OWL) ,
  ADC(DC) ]

makepair x = (baseUrl x,x)
pairlist = map makepair ontologies 
url_lookup = Map.fromList pairlist
url_list = map baseUrl ontologies 

-- (UNode "#type") (UNode "https://h4ck3rm1k3.github.io/gogccintro/gcc/ontology/2017/05/20/gcc_compiler.owl#string_cst")

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
      
      APredicate(p) -> case p of
                        StringNode -> "strg"
                        FunctionNameString(x) -> "strg"
      Types(t) -> case t of
                    StringCst-> "string_cst"
                                   
list_of_types = [
  APredicate(StringNode),
  Types(StringCst)
      ]

--exampleTTLFile = "/home/mdupont/experiments/gcc-ontology/data/clean2.ttl"

someFunc = do
  let x1 = LibData.load
  let x2 = LibData2.load
  let y = take 1 x1
  putStrLn "ok"

load_step3 x = do
  let x4 = case x of
             Right x3 -> x3
             Left x3 -> Nothing
  x4
  

-- rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
-- bu = "https://h4ck3rm1k3.github.io/gogccintro/gcc/ontology/2017/05/20/gcc_compiler.owl#"
-- bu2 = "http://www.co-ode.org/ontologies/ont.owl#"
-- owl = "http://www.w3.org/2002/07/owl#"


                  
-- triple2 t = case t of
--   Triple a b c -> case a of
--     NodeId _ -> "NodeId"
--     LNode _ -> "LNode"
--     UNode (x) -> do
--       let v2 = bu `isPrefixOf` x
--       case v2 of
--         False -> do
--           let v2 = bu2 `isPrefixOf` x
--           let v3 = stripPrefix bu2 x
--           case v3 of
--             Nothing -> "Nothing"
--             Just v4 -> v4
--         True -> do
--           let v3 = stripPrefix bu x
--           case v3 of
--             Nothing -> "Nothing"
--             Just v4 -> v4
--               --let v5 = readMaybe v4 :: Maybe Int
--               --case v5 of
--               --  Just v6 -> v6

--canStrip :: [Char] -> [Char] -> Maybe Ontos
canStrip b t = do
  let v2 = isPrefixOf b t
  case v2 of
    True -> do
      let v = stripPrefix b t
      let onto = url_lookup Map.! b
      --Just onto
      case v of
        Just vs -> do
          let i = make_instance onto vs
          Just (i)
              
          -- case onto of 
          --   AGccOntologyData(b) -> do
          --     let i = make_instance b vs
          --     Just (AGccOntologyData (i))
              
            --AGccOntologyData2( b ) -> Just AGccOntologyData2 make_instance b v
            --AMainRDF(b) -> make_instance b v
            --ARDFS(b)  -> make_instance b v
            --AOWL (b ) -> make_instance b v
            --ADC (b) -> make_instance b v

          --let i = make_instance onto vs
          --Just i
        
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
    NodeId (y) -> Nothing
    UNode (x) -> do
      matchFirst x url_list
            
    LNode (PlainL s) -> Just (AGccOntologyData(SomeGCCLiteral(s)))

triple4 t = case t of
  Triple a b c -> Statement(
    Subject(procNode a),
      Predicate(procNode b),
      Object(procNode c)
    )

          
   
                        
