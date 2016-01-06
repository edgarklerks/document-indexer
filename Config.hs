{-# LANGUAGE TupleSections, DeriveGeneric, DeriveDataTypeable, TemplateHaskell, GeneralizedNewtypeDeriving, FlexibleContexts, TypeOperators, FlexibleInstances #-}
module Config where

import qualified Data.HashMap.Lazy as L
import Text.Trifecta as T hiding (Lines)
import Data.Monoid
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Applicative
import Data.Foldable
import Text.Trifecta.Delta
import Data.Int
import Control.Lens
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Debug.Trace
import GHC.Generics
import Data.Typeable
import qualified Data.Convertible as C
import Data.Convertible.Instances

data ConfigType = I Integer
                | S String
                | D Double
                | B Bool
                | L [ConfigType]
          deriving (Show, Read, Typeable, Generic)


type Section = String
type Label = String

getSection :: Typeable a => Section -> Config -> Maybe [(Label, a)]
getSection s (Config c) = undefined -- L.lookup s c


getString :: Section -> Label -> Config -> Maybe String
getString = lookupValue

getDouble :: Section -> Label -> Config -> Maybe Double
getDouble s l c = (lookupValue s l c) <|> (fromInteger <$> lookupValue s l c)

getInteger :: Section -> Label -> Config -> Maybe Integer
getInteger s l c = (lookupValue s l c) <|> (truncate <$> (lookupValue s l c :: Maybe Double))

getBool :: Section -> Label -> Config -> Maybe Bool
getBool s l c = (lookupValue s l c) <|> (castInteger =<< lookupValue s l c)
                where castInteger :: Integer -> Maybe Bool
                      castInteger 1 = Just True
                      castInteger 0 = Just False
                      castInteger _ = Nothing

getList :: Typeable a => Section -> Label -> Config -> Maybe [a]
getList s l (Config c) = do
           sec <- L.lookup s c
           l <- L.lookup l sec
           case l of
             (L xs) -> sequence $ gconvert . GHC.Generics.from <$> xs
             (S xs) -> cast xs
             _ -> Nothing

lookupValue ::Typeable a => Section -> Label -> Config -> Maybe a
lookupValue s l (Config c) = L.lookup s c >>= L.lookup l >>= gconvert . GHC.Generics.from

class GConvert f where
   gconvert :: Typeable a => f -> Maybe a
   gtype :: f -> TypeRep


instance GConvert (c f) => GConvert (M1 i a c f) where
   gconvert (M1 x) = gconvert x
   gtype (M1 x) = gtype x

instance (GConvert (f p), GConvert (g p)) => GConvert ((:+:) f g p) where
   gconvert (L1 l) = gconvert l
   gconvert (R1 r) = gconvert r
   gtype (R1 r) = gtype r
   gtype (L1 l) = gtype l

instance (GConvert (f p), GConvert (g p)) => GConvert ((:*:) f g p) where
  gconvert = error "This library cannot work with produuct types"
  gtype = error "This library cannot work with product types"

instance Typeable c => GConvert (K1 i c p) where
   gconvert (K1 a) = cast a
   gtype (K1 a) = typeOf a


newtype Config = Config {
     unConfig :: L.HashMap Section (L.HashMap Label ConfigType)
   } deriving (Show, Read)

type NakedConfig = [(Lines, Delta)]


data ConfigState = ConfigState {
              currentSection :: Maybe Section,
              hmap :: L.HashMap Section (L.HashMap Label ConfigType),
              pos :: Delta
   }

newtype ConfigBuilder a = ConfigBuilder {
           unConfigBuilder :: StateT ConfigState (Either String) a
   } deriving (Functor, Monad, Applicative, MonadState ConfigState)


throw :: String -> ConfigBuilder a
throw s = ConfigBuilder $ StateT $ \p -> Left $ s <> " at " <> (deltaToLineCol $ pos p)

parseConfigFile = parseConfiguration configParser

parseConfiguration :: (Result NakedConfig -> Either String Config) -> FilePath -> IO (Either String Config)
parseConfiguration  pr  fp  = pr <$> parseFromFileEx configLine fp

configParser :: Result NakedConfig -> Either String Config
configParser (Failure e)  = Left (show e)
configParser (Success xs) = runConfigBuilder worker xs
                     where worker (VarDecl a) = addLabel a
                           worker (Section s) = addSection s
                           worker (Comment _) = return ()

addLabel :: (Label, ConfigType) -> ConfigBuilder ()
addLabel (l,p) = do
      m <- gets hmap
      s <- gets currentSection
      case s of
        Nothing -> throw $ "Label " <> l <> " must be defined in a section"
        Just s -> do
              let cm =  L.lookupDefault (error "cannot happen") s m
              when (L.member l cm) $ throw $ "Label " <> l <> " is already defined in the section " <> s
              modify (\v -> v {hmap =  L.adjust (L.insert l p) s (hmap v)})




addSection :: Section -> ConfigBuilder ()
addSection s = gets hmap >>= \x -> case L.member s x of
                                     True -> throw $ "Section " <> s <> " already exists."
                                     False -> modify (\x -> x { hmap = L.insert s mempty (hmap x), currentSection = Just s})

runConfigBuilder :: (Lines -> ConfigBuilder ()) -> NakedConfig -> Either String Config
runConfigBuilder x xs = fmap (Config . hmap) $ execStateT (unConfigBuilder $ walk x xs) $ ConfigState Nothing mempty mempty
                        where walk x ((p,d) :ps) = modify (\x -> x { pos = d}) >> x p >> walk x ps
                              walk x [] = return ()

deltaToLineCol :: Delta -> String
deltaToLineCol (Lines lines cols _ _) = show (lines, cols)
deltaToLineCol (Directed fp lines cols _ _ ) = B.unpack fp <> show (lines + 1, cols)
deltaToLineCol (Columns c b) = show c <> " characters"


{-- The configuration syntax  looks like:
    [section]
    test = "bla"
    p = 1
    s = [1,2,3,4]

   bnf:
  config = configLine*
  configLine = (sectiondecl | vardecl) '\n'
  sectiondecl = '[' id ']'
  vardecl = id '=' expr
  expr = string | integer | double | list | bool
  string = '"' (normal-characters | escaped-characters)* '"'
  integer = ('-')? [0-9]+
  double = ('-')? [0-9]+ '.' [0-9]*
  list = '[' expr(,expr)* ']'
  bool = "true" | "false"
--}

data Lines = Section Section
           | VarDecl (Label, ConfigType)
           | Comment String

configLine :: Parser NakedConfig
configLine = ( position >>= \x -> fmap (\t ->  (t, x)) $ sectiondecl <|> vardecl <|> comment <?> "****no newline allowed at end of file**** ") `sepBy` (char '\n' <* optional (many $ char '\n' <|> space ))

sectiondecl :: Parser Lines
sectiondecl = Section <$> between (char '[') (char ']')  iddecl

comment :: Parser Lines
comment = string "#" *> (Comment <$> many (satisfy (/='\n')))

vardecl :: Parser Lines
vardecl = fmap VarDecl  $ (,) <$> iddecl <*> (spaces *> char '=' *> spaces *> expr)

iddecl :: Parser Label
iddecl = (:) <$> letter <*> many alphaNum

expr :: Parser ConfigType
expr = (str  <?> "string, '\"' string characters '\"' where \" is escaped as \"\"\n\"This is a string: \"\".\" ")
    <|> (try dbl <?> "double, (-)[0-9]+\\.[0-9]+\n-12.001")
    <|> (int <?> "integer, (-)[0-9]+\n12345")
    <|> (list <?> "list, '[' expr* ']' separated by spaces\n[1 2 3 4]")
    <|> (bool <?> "boolean, true | false\n true" )

  where

minus :: Num a => Parser (a -> a)
minus = maybe id (const negate) <$> optional (char '-')

int :: Parser ConfigType
int = fmap I $ minus <*> (read <$> some digit)

dbl :: Parser ConfigType
dbl = fmap D $ minus <*> fmap read (mappend <$> some digit <*> ((:) <$> char '.' <*> some digit))
list :: Parser ConfigType
list = L <$> between (char '[' *> spaces) (char ']' ) (many $ expr <* spaces)

str :: Parser ConfigType
str = S <$> between (char '"') (char '"') (many $ normal_characters <|> escaped_characters)


normal_characters :: Parser Char
normal_characters = satisfy (/='"')

escaped_characters :: Parser Char
escaped_characters = string "\"\"" *> pure '"'


bool :: Parser ConfigType
bool = string "true" *> pure (B True) <|> string "false" *> pure (B False)
