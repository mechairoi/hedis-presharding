module Database.Redis.Sharding.ConsistentHash (
    shardingInfo
  , lookup
  , find
  , masterConn
  , slaveConns
) where
import Prelude hiding (lookup)
import Database.Redis (Connection)
import Data.HashRing (lookup, find, fromList, HashRing)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Hashable (Hashable(..))
import Data.Digest.Pure.SHA (sha1, integerDigest)

data Shard = Shard { shardName :: String
            , masterConn :: Connection
            , slaveConns :: [ Connection ] }

instance Eq Shard where
  a == b = (shardName a) == (shardName b)

instance Ord Shard where
  compare a b = (shardName a) `compare` (shardName b)

instance Hashable Shard where
  hashWithSalt n salt = fromIntegral $ integerDigest $ sha1 bs
    where bs = pack $ (show n) ++ (show $ shardName salt)

newtype ShardingInfo = ShardingInfo ( HashRing Shard )

shardingInfo :: Int -> [(String, Connection, [Connection])] -> ShardingInfo
shardingInfo n ns = ShardingInfo $ fromList n $ map shard ns
  where shard (name, cm, css) = Shard name cm css

