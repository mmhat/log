module Log.Backend.ElasticSearch.V1.Internal
  (ElasticSearchConfig(..)
  ,defaultElasticSearchConfig
  ,EsUsername(..)
  ,EsPassword(..))
where

import Database.V1.Bloodhound hiding (Status)
import Prelude
import qualified Data.Text as T

-- | Configuration for the Elasticsearch 'Logger'. See
-- <https://www.elastic.co/guide/en/elasticsearch/reference/current/glossary.html>
-- for the explanation of terms.
data ElasticSearchConfig = ElasticSearchConfig {
    esServer        :: !T.Text -- ^ Elasticsearch server address.
  , esIndex         :: !T.Text -- ^ Elasticsearch index name.
  , esShardCount    :: !Int    -- ^ Elasticsearch shard count for the named index.
  , esReplicaCount  :: !Int    -- ^ Elasticsearch replica count for the named index.
  , esMapping       :: !T.Text -- ^ Elasticsearch mapping name.
  , esLogin         :: Maybe (EsUsername, EsPassword) -- ^ Elasticsearch basic authentication username and password.
  , esLoginInsecure :: !Bool   -- ^ Allow basic authentication over non-TLS connections.
  } deriving (Eq, Show)

-- | Sensible defaults for 'ElasticSearchConfig'.
defaultElasticSearchConfig :: ElasticSearchConfig
defaultElasticSearchConfig = ElasticSearchConfig {
  esServer        = "http://localhost:9200",
  esIndex         = "logs",
  esShardCount    = 4,
  esReplicaCount  = 1,
  esMapping       = "log",
  esLogin         = Nothing,
  esLoginInsecure = False
  }
