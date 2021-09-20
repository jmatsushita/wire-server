module Wire.API.Routes.MultiTablePaging where

import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString as BS
import Data.Json.Util (fromBase64Text, toBase64Text)
import Data.Kind
import Data.Proxy
import Data.Range
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Text as Text
import GHC.TypeLits
import Imports

data GetMultiTablePageRequest (name :: Symbol) (tables :: Type) (max :: Nat) (def :: Nat) = GetMultiTablePageRequest
  { gmtprSize :: Range 1 max Int32,
    gmtprState :: Maybe (MultiTablePagingState name tables)
  }
  deriving stock (Show, Eq)

-- Not sure why deriving via doesn't work
instance
  (KnownNat max, KnownNat def, Within Int32 1 max, LTE 1 def, LTE def max, PagingTable tables, KnownSymbol name) =>
  ToJSON (GetMultiTablePageRequest name tables max def)
  where
  toJSON = schemaToJSON

instance
  (KnownNat max, KnownNat def, Within Int32 1 max, LTE 1 def, LTE def max, PagingTable tables, KnownSymbol name) =>
  FromJSON (GetMultiTablePageRequest name tables max def)
  where
  parseJSON = schemaParseJSON

instance
  (KnownNat max, KnownNat def, Within Int32 1 max, LTE 1 def, LTE def max, PagingTable tables, KnownSymbol name) =>
  S.ToSchema (GetMultiTablePageRequest name tables max def)
  where
  declareNamedSchema = schemaToSwagger

instance
  (KnownNat max, KnownNat def, Within Int32 1 max, LTE 1 def, LTE def max, PagingTable tables, KnownSymbol name) =>
  ToSchema (GetMultiTablePageRequest name tables max def)
  where
  schema =
    let addPagingStateDoc =
          description
            ?~ "optional, when not specified, the first page will be returned.\
               \Every returned page contains a paging_state, this should be supplied to retrieve the next page."
        addSizeDoc = description ?~ ("optional, must be <= " <> textFromNat @max <> ", defaults to " <> textFromNat @def <> ".")
     in objectWithDocModifier
          ("GetPaginated_" <> textFromSymbol @name)
          (description ?~ "A request to list some or all of a user's conversation ids, including remote ones")
          $ GetMultiTablePageRequest
            <$> gmtprSize .= (fieldWithDocModifier "size" addSizeDoc schema <|> pure (toRange (Proxy @def)))
            <*> gmtprState .= optFieldWithDocModifier "paging_state" Nothing addPagingStateDoc schema

textFromNat :: forall n. KnownNat n => Text
textFromNat = Text.pack . show . natVal $ Proxy @n

textFromSymbol :: forall s. KnownSymbol s => Text
textFromSymbol = Text.pack . symbolVal $ Proxy @s

data MultiTablePagingState (name :: Symbol) tables = MultiTablePagingState
  { mtpsTable :: tables,
    mtpsState :: Maybe ByteString
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema (MultiTablePagingState name tables)

class PagingTable t where
  -- Using 'Word8' because 256 tables ought to be enough.
  encodePagingTable :: t -> Word8
  decodePagingTable :: MonadFail m => Word8 -> m t

instance (PagingTable tables, KnownSymbol name) => ToSchema (MultiTablePagingState name tables) where
  schema =
    (toBase64Text . encodePagingState)
      .= parsedText (textFromSymbol @name <> "_PagingState") (parseConvesationPagingState <=< fromBase64Text)

encodePagingState :: PagingTable tables => MultiTablePagingState name tables -> ByteString
encodePagingState (MultiTablePagingState table state) =
  let encodedTable = encodePagingTable table
      encodedState = fromMaybe "" state
   in BS.cons encodedTable encodedState

parseConvesationPagingState :: PagingTable tables => ByteString -> Either String (MultiTablePagingState name tables)
parseConvesationPagingState = AB.parseOnly conversationPagingStateParser

conversationPagingStateParser :: PagingTable tables => AB.Parser (MultiTablePagingState name tables)
conversationPagingStateParser = do
  table <- AB.anyWord8 >>= decodePagingTable
  state <- (AB.endOfInput $> Nothing) <|> (Just <$> AB.takeByteString <* AB.endOfInput)
  pure $ MultiTablePagingState table state

data MultiTablePage (name :: Symbol) (resultsKey :: Symbol) (tables :: Type) a = MultiTablePage
  { mtpResults :: [a],
    mtpHasMore :: Bool,
    mtpPagingState :: MultiTablePagingState name tables
  }
  deriving stock (Eq, Show)

-- deriving via doesn't work for some reason
instance
  (KnownSymbol resultsKey, KnownSymbol name, ToSchema a, PagingTable tables) =>
  ToJSON (MultiTablePage name resultsKey tables a)
  where
  toJSON = schemaToJSON

instance
  (KnownSymbol resultsKey, KnownSymbol name, ToSchema a, PagingTable tables) =>
  FromJSON (MultiTablePage name resultsKey tables a)
  where
  parseJSON = schemaParseJSON

instance
  (KnownSymbol resultsKey, KnownSymbol name, ToSchema a, PagingTable tables) =>
  S.ToSchema (MultiTablePage name resultsKey tables a)
  where
  declareNamedSchema = schemaToSwagger

instance
  (KnownSymbol resultsKey, KnownSymbol name, ToSchema a, PagingTable tables) =>
  ToSchema (MultiTablePage name resultsKey tables a)
  where
  schema =
    object (textFromSymbol @name <> "_Page") $
      MultiTablePage
        <$> mtpResults .= field (textFromSymbol @resultsKey) (array schema)
        <*> mtpHasMore .= field "has_more" schema
        <*> mtpPagingState .= field "paging_state" schema

data LocalOrRemoteTable
  = PagingLocals
  | PagingRemotes
  deriving stock (Show, Eq)

instance PagingTable LocalOrRemoteTable where
  encodePagingTable PagingLocals = 0
  encodePagingTable PagingRemotes = 1

  decodePagingTable 0 = pure PagingLocals
  decodePagingTable 1 = pure PagingRemotes
  decodePagingTable x = fail $ "Expected 0 or 1 while parsing LocalOrRemoteTable, got: " <> show x
