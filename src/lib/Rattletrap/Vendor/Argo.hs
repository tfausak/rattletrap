module Rattletrap.Vendor.Argo
  ( Argo.HasCodec(codec)
  , Argo.Internal.Codec.Codec.map
  , Argo.Internal.Codec.Codec.mapMaybe
  , Argo.Internal.Codec.Object.fromObjectCodec
  , Argo.Internal.Codec.Codec.project
  , Argo.Internal.Codec.Object.required
  , optional
  , Argo.Internal.Codec.Value.identified
  , withIdentifier
  , Argo.Internal.Schema.Identifier.Identifier(..)
  , toValue
  , Argo.Internal.Type.Permission.Permission(Allow, Forbid)
  , (Control.Applicative.<|>)
  , Argo.Internal.Codec.Array.fromArrayCodec
  , Argo.Internal.Codec.Array.element
  , Argo.Internal.Codec.Codec.schema
  , Argo.decode
  , Argo.encode
  , Argo.encodeWith
  , Argo.Indent(Spaces, Tab)
  , Argo.Member(..)
  , Argo.Name(..)
  , Argo.Value(..)
  ) where

import qualified Argo
import qualified Argo.Internal.Class.HasCodec
import qualified Argo.Internal.Codec.Array
import qualified Argo.Internal.Codec.Codec
import qualified Argo.Internal.Codec.Object
import qualified Argo.Internal.Codec.Value
import qualified Argo.Internal.Schema.Identifier
import qualified Argo.Internal.Type.Permission
import qualified Control.Applicative
import qualified Data.Text
import qualified Data.Typeable

optional
  :: Data.Typeable.Typeable a
  => Argo.Name
  -> Argo.Internal.Codec.Value.Value a
  -> Argo.Internal.Codec.Object.Object (Maybe a)
optional = Argo.Internal.Class.HasCodec.optionalNullable

toValue :: Argo.HasCodec a => a -> Argo.Value
toValue = Argo.Internal.Codec.Value.encodeWith Argo.codec

withIdentifier
  :: String
  -> Argo.Internal.Codec.Value.Value a
  -> Argo.Internal.Codec.Value.Value a
withIdentifier =
  Argo.Internal.Codec.Value.withIdentifier
    . Argo.Internal.Schema.Identifier.fromText
    . Data.Text.pack
