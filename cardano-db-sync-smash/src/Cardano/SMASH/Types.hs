{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.SMASH.Types
  ( ApplicationUser (..)
  , ApplicationUsers (..)
  , stubbedApplicationUsers
  , User
  , UserValidity (..)
  , checkIfUserValid
  -- * Pool info
  , PoolIdent (..)
  , PoolIdBlockNumber (..)
  , PoolUrl (..)
  , PoolMetaHash (..)
  , bytestringToPoolMetaHash
  , TickerName (..)
  , UniqueTicker (..)
  , PolicyResult (..)
  -- * Wrapper
  , PoolName (..)
  , PoolDescription (..)
  , PoolTicker (..)
  , PoolHomepage (..)
  , PoolOfflineMetadata (..)
  , createPoolOfflineMetadata
  , examplePoolOfflineMetadata
  -- * Configuration
  , HealthStatus (..)
  , Configuration (..)
  , defaultConfiguration
  -- * API
  , ApiResult (..)
  -- * HTTP
  , SmashURL (..)
  , PoolFetchError (..)
  , TimeStringFormat (..)
  , PoolMetadataRaw (..)
  -- * Util
  , formatTimeToNormal
  , getUniqueTicker
  ) where

import           Cardano.Prelude

import           Control.Monad.Fail (fail)

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import           Data.Swagger (NamedSchema (..), ToParamSchema (..), ToSchema (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock.POSIX as Time
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)


import           Network.URI (URI, parseURI)
import           Servant (FromHttpApiData (..), MimeUnrender (..), OctetStream)

import           Cardano.Db
import           Cardano.SMASH.Db.Error
import           Cardano.SMASH.Db.Types
import           Cardano.Sync.Types (PoolMetaHash (..), PoolMetaHex (..))

-- | The Smash @URI@ containing remote filtering data.
newtype SmashURL
  = SmashURL { unSmashURL :: URI }
  deriving (Eq, Show, Generic)

instance ToJSON SmashURL where
    toJSON (SmashURL uri) =
        object
            [ "smashURL" .= (show uri :: Text)
            ]

instance FromJSON SmashURL where
    parseJSON = withObject "SmashURL" $ \o -> do
        uri <- o .: "smashURL"
        case parseURI uri of
            Nothing -> fail "Not a valid URI for SMASH server."
            Just uri' -> pure $ SmashURL uri'

instance ToSchema SmashURL where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "SmashURL") mempty

-- | The basic @Configuration@.
newtype Configuration = Configuration
  { cPortNumber :: Int
  } deriving (Eq, Show)

defaultConfiguration :: Configuration
defaultConfiguration = Configuration 3100

-- | A list of users with very original passwords.
stubbedApplicationUsers :: ApplicationUsers
stubbedApplicationUsers = ApplicationUsers [ApplicationUser "ksaric" "cirask"]

examplePoolOfflineMetadata :: PoolOfflineMetadata
examplePoolOfflineMetadata =
    PoolOfflineMetadata
        (PoolName "TestPool")
        (PoolDescription "This is a pool for testing")
        (PoolTicker "testp")
        (PoolHomepage "https://iohk.io")

data PolicyResult = PolicyResult
  { prSmashURL :: !SmashURL
  , prHealthStatus :: !HealthStatus
  , prDelistedPools :: ![PoolIdent]
  , prUniqueTickers :: ![UniqueTicker]
  } deriving (Eq, Show, Generic)

instance ToJSON PolicyResult where
    toJSON (PolicyResult smashURL healthStatus delistedPools uniqueTickers) =
        object
            [ "smashURL" .= toJSON smashURL
            , "healthStatus" .= toJSON healthStatus
            , "delistedPools" .= toJSON delistedPools
            , "uniqueTickers" .= toJSON uniqueTickers
            ]

instance ToParamSchema PoolMetaHex

instance ToSchema PolicyResult

data UniqueTicker = UniqueTicker
  { utName :: !TickerName
  , utHexHash :: !PoolMetaHex
  } deriving (Eq, Show, Generic)

getUniqueTicker :: UniqueTicker -> (TickerName, PoolMetaHex)
getUniqueTicker (UniqueTicker name hash) = (name, hash)

instance ToJSON UniqueTicker where
  toJSON (UniqueTicker name hex) =
    object
      [ "tickerName" .= unTickerName name
      , "poolMetadataHash" .= unPoolMetaHex hex
      ]

instance FromJSON UniqueTicker where
    parseJSON = withObject "UniqueTicker" $ \o ->
        UniqueTicker
            <$> o .: "tickerName"
            <*> fmap PoolMetaHex (o .: "poolMetadataHash")

instance ToSchema UniqueTicker

instance ToParamSchema TickerName

instance ToSchema TickerName

instance ToParamSchema PoolIdent

instance ToSchema PoolIdent

-- instance ToParamSchema PoolMetaHash

-- A data type we use to store user credentials.
data ApplicationUser = ApplicationUser
  { username :: !Text
  , password :: !Text
  } deriving (Eq, Show, Generic)

instance ToJSON ApplicationUser
instance FromJSON ApplicationUser

-- A list of users we use.
newtype ApplicationUsers
  = ApplicationUsers [ApplicationUser]
  deriving (Eq, Show, Generic)

instance ToJSON ApplicationUsers
instance FromJSON ApplicationUsers

-- | A user we'll grab from the database when we authenticate someone
newtype User
  = User { userName :: Text }
  deriving (Eq, Show)

-- | This we can leak.
data UserValidity
    = UserValid !User
    | UserInvalid
    deriving (Eq, Show)

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
checkIfUserValid :: ApplicationUsers -> ApplicationUser -> UserValidity
checkIfUserValid (ApplicationUsers applicationUsers) applicationUser@(ApplicationUser usernameText _) =
    if applicationUser `elem` applicationUsers
        then UserValid (User usernameText)
        else UserInvalid

instance FromHttpApiData TickerName where
    parseUrlPiece tickerName = validateTickerName tickerName

-- Currently deserializing from safe types, unwrapping and wrapping it up again.
-- The underlying DB representation is HEX.
instance FromHttpApiData PoolIdent where
    parseUrlPiece poolId = parsePoolId poolId

-- TODO(KS): Temporarily, validation!?
instance FromHttpApiData PoolMetaHash where
    parseUrlPiece txt = Right $ PoolMetaHash (Text.encodeUtf8 txt)
    --TODO: parse hex or bech32

newtype PoolMetadataRaw
  = PoolMetadataRaw { unPoolMetadata :: Text }
  deriving (Eq, Generic, Show)

instance ToSchema PoolMetadataRaw

newtype PoolName
  = PoolName { unPoolName :: Text }
  deriving (Eq, Show, Ord, Generic)

instance ToSchema PoolName

newtype PoolDescription
  = PoolDescription { unPoolDescription :: Text }
  deriving (Eq, Show, Ord, Generic)

instance ToSchema PoolDescription

newtype PoolTicker
  = PoolTicker { unPoolTicker :: Text }
  deriving (Eq, Show, Ord, Generic)

instance ToSchema PoolTicker

newtype PoolHomepage
  = PoolHomepage { unPoolHomepage :: Text }
  deriving (Eq, Show, Ord, Generic)

instance ToSchema PoolHomepage

-- | The bit of the pool data off the chain.
data PoolOfflineMetadata = PoolOfflineMetadata
  { pomName :: !PoolName
  , pomDescription :: !PoolDescription
  , pomTicker :: !PoolTicker
  , pomHomepage :: !PoolHomepage
  } deriving (Eq, Show, Ord, Generic)

-- | Smart constructor, just adding one more layer of indirection.
createPoolOfflineMetadata
    :: PoolName -> PoolDescription -> PoolTicker -> PoolHomepage
    -> PoolOfflineMetadata
createPoolOfflineMetadata = PoolOfflineMetadata

-- Required instances
instance FromJSON PoolOfflineMetadata where
  parseJSON = withObject "poolOfflineMetadata" $ \o ->
    PoolOfflineMetadata
      <$> fmap PoolName (parseName o)
      <*> fmap PoolDescription (parseDescription o)
      <*> fmap PoolTicker (parseTicker o)
      <*> fmap PoolHomepage (o .: "homepage")
    where
        -- Copied from https://github.com/input-output-hk/cardano-node/pull/1299

        -- | Parse and validate the stake pool metadata name from a JSON object.
        --
        -- If the name consists of more than 50 characters, the parser will fail.
        parseName :: Aeson.Object -> Aeson.Parser Text
        parseName obj = do
          name <- obj .: "name"
          if length name <= 50
            then pure name
            else fail $
                 "\"name\" must have at most 50 characters, but it has "
              <> show (length name)
              <> " characters."

        -- | Parse and validate the stake pool metadata description from a JSON
        -- object.
        --
        -- If the description consists of more than 255 characters, the parser will
        -- fail.
        parseDescription :: Aeson.Object -> Aeson.Parser Text
        parseDescription obj = do
          description <- obj .: "description"
          if length description <= 255
            then pure description
            else fail $
                 "\"description\" must have at most 255 characters, but it has "
              <> show (length description)
              <> " characters."

        -- | Parse and validate the stake pool ticker description from a JSON object.
        --
        -- If the ticker consists of less than 3 or more than 5 characters, the parser
        -- will fail.
        parseTicker :: Aeson.Object -> Aeson.Parser Text
        parseTicker obj = do
          ticker <- obj .: "ticker"
          let tickerLen = length ticker
          if tickerLen >= 3 && tickerLen <= 5
            then pure ticker
            else fail $
                 "\"ticker\" must have at least 3 and at most 5 "
              <> "characters, but it has "
              <> show (length ticker)
              <> " characters."

-- |We presume the validation is not required the other way around?
instance ToJSON PoolOfflineMetadata where
  toJSON (PoolOfflineMetadata name' description' ticker' homepage') =
    object
      [ "name" .= unPoolName name'
      , "description" .= unPoolDescription description'
      , "ticker" .= unPoolTicker ticker'
      , "homepage" .= unPoolHomepage homepage'
      ]

instance ToSchema PoolOfflineMetadata

instance MimeUnrender OctetStream PoolMetaHash where
  mimeUnrender _ = fmap PoolMetaHash . Base16.decode . LBS.toStrict

instance ToSchema PoolMetaHex

instance ToSchema DBFail where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "DBFail") mempty

-- Result wrapper.
newtype ApiResult err a
  = ApiResult (Either err a)
  deriving (Generic)

instance (ToSchema a, ToSchema err) => ToSchema (ApiResult err a)

instance (ToJSON err, ToJSON a) => ToJSON (ApiResult err a) where
  toJSON (ApiResult (Left dbFail))  = toJSON dbFail
  toJSON (ApiResult (Right result)) = toJSON result

  toEncoding (ApiResult (Left result))  = toEncoding result
  toEncoding (ApiResult (Right result)) = toEncoding result

data PoolIdBlockNumber = PoolIdBlockNumber !PoolIdent !Word64
    deriving (Eq, Show, Generic)

instance ToJSON PoolIdBlockNumber where
  toJSON (PoolIdBlockNumber poolId blockNumber) =
    object
      [ "poolId"      .= poolId
      , "blockNumber" .= blockNumber
      ]

instance FromJSON PoolIdBlockNumber where
  parseJSON = withObject "poolIdBlockNumber" $ \o ->
    PoolIdBlockNumber
      <$> o .: "poolId"
      <*> o .: "blockNumber"

instance ToSchema PoolIdBlockNumber

-- |Fetch error for the specific @PoolIdent@ and the @PoolMetaHash@.
data PoolFetchError = PoolFetchError !Time.POSIXTime !PoolIdent !PoolMetaHex !Text !Word
  deriving (Eq, Show, Generic)

instance ToJSON PoolFetchError where
  toJSON (PoolFetchError time poolId poolHex errorCause retryCount) =
    object
      [ "time" .= formatTimeToNormal time
      , "utcTime" .= (show time :: Text)
      , "poolId" .= unPoolIdent poolId
      , "poolHash" .= unPoolMetaHex poolHex
      , "cause" .= errorCause
      , "retryCount" .= retryCount
      ]

instance ToSchema PoolFetchError

formatTimeToNormal :: Time.POSIXTime -> Text
formatTimeToNormal = Text.pack . formatTime defaultTimeLocale "%d.%m.%Y. %T" . Time.posixSecondsToUTCTime

-- |Specific time string format.
newtype TimeStringFormat
  = TimeStringFormat { unTimeStringFormat :: UTCTime }
  deriving (Eq, Show, Generic)

instance FromHttpApiData TimeStringFormat where
    --parseQueryParam :: Text -> Either Text a
    parseQueryParam queryParam =
        let timeFormat = "%d.%m.%Y"

            --parsedTime :: UTCTime <- parseTimeM False defaultTimeLocale "%d.%m.%Y %T" "04.03.2010 16:05:21"
            parsedTime = parseTimeM False defaultTimeLocale timeFormat $ Text.unpack queryParam
        in  TimeStringFormat <$> parsedTime

-- Required for the above, error with newer GHC versions
instance MonadFail (Either Text) where
  fail = Left . Text.pack

instance ToParamSchema TimeStringFormat

-- |The data for returning the health check for SMASH.
data HealthStatus = HealthStatus
  { hsStatus :: !Text
  , hsVersion :: !Text
  } deriving (Eq, Show, Generic)

instance ToJSON HealthStatus where
  toJSON (HealthStatus status version) =
    object
      [ "status" .= status
      , "version" .= version
      ]

instance FromJSON HealthStatus where
    parseJSON = withObject "healthStatus" $ \o ->
        HealthStatus
          <$> o .: "status"
          <*> o .: "version"

instance ToSchema HealthStatus

instance FromHttpApiData PoolMetaHex where
  parseUrlPiece p = Right $ PoolMetaHex p


instance ToJSON PoolMetadataRaw where
  toJSON (PoolMetadataRaw txt) =
    object
      [ "poolMetadataRaw" .= txt
      ]

instance FromJSON PoolMetaHex where
  parseJSON = withObject "PoolMetaHex" $ \o ->
    PoolMetaHex <$> o .: "poolMetaHex"
