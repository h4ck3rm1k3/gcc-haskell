{-# LANGUAGE DeriveGeneric #-}
-- from ~/.stack/indices/Hackage/packages/rdf4h/3.0.1/rdf4h-3.0.1.tar.gz!rdf4h-3.0.1/testsuite/tests/Data/RDF/GraphTestUtils.hs
module Lib
    ( someFunc,
      parseRdf1
    ) where
import Data.RDF.Types
import Lib2
import Data.RDF.Query
import Data.RDF.Namespace
--import Data.Attoparsec
import Text.RDF.RDF4H.NTriplesSerializer
import qualified Data.Text as T
import Data.RDF.Types
import Text.RDF.RDF4H.NTriplesParser
--import Text.RDF.RDF4H.TurtleParser -- TurtleParserCustom
import Data.RDF.Graph.TList
import System.IO
import Data.Attoparsec.ByteString (parse,IResult(..))
import Text.RDF.RDF4H.ParserUtils

exampleTTLFile = "/home/mdupont/experiments/gcc-ontology/data/clean2.ttl"
--exampleTTLFile = "/home/mdupont/experiments/gcc-ontology/data/params.ttl"

cb x = Foo2(x)
--processRdf2
--cb2 = (ProcessRdf cb)
--cb3 :: Result [Maybe Triple] -> IO b
--cb2 x =  1
--parseRdf1 :: FilePath -> IO b
--IO (Either ParseFailure (RDF a0))
--parseRdf1 = Lib2.parseFileAttoparsecCallback exampleTTLFile
parseRdf1 :: IO (Either ParseFailure (RDF TList))
parseRdf1 = Lib2.parseFileAttoparsec exampleTTLFile
      --parsedRDF   = f fromEither :: IO (RDF TList)
--  f :: IO (Either ParseFailure (RDF a0))
--a0  instance Rdf TList -- Defined in ‘Data.RDF.Graph.TList’
  --2
--parseRdf = parseRdf1 
--someFunc :: IO ()
--dox x = --  IO (RDF TList)
  
someFunc = do
  x <- parseRdf1 -- remove the IO
  let x4 = case x of Right x3 -> x3
  let xt = triplesOf x4 -- all the triples Data.RDF.Types.triplesOf :: Data.RDF.Types.RDF rdfImpl
  --xt :: Triples
  let l6 = take 1 xt
  --l6 :: [Triple]
  let subject = case l6 of [Triple a b c] -> a
  let subject_id = case subject of BNode(id1) -> id1
  let pred = case l6 of [Triple a b c] -> b
  let obj = case l6 of [Triple a b c] -> c
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
  --withFile "out.nt" WriteMode (\h -> hWriteRdf NTriplesSerializer h rdfGraph)
