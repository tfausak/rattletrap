module Rattletrap.Vendor.Argo
  ( Argo.HasCodec(codec)
  , Argo.Codec.Codec.map
  , Argo.Codec.Codec.mapMaybe
  , Argo.Codec.Object.fromObjectCodec
  , Argo.Codec.Codec.project
  , Argo.Codec.Object.required
  , optional
  , Argo.Codec.Value.identified
  , withIdentifier
  , Argo.Schema.Identifier.Identifier(..)
  , toValue
  , Argo.Type.Permission.Permission(Allow, Forbid)
  , Data.String.fromString
  , (Control.Applicative.<|>)
  , Argo.Codec.Array.fromArrayCodec
  , Argo.Codec.Array.element
  , Argo.Codec.Codec.schema
  , Argo.decode
  , Argo.encode
  , Argo.encodeWith
  , Argo.Indent(Spaces, Tab)
  , Argo.Member(..)
  , Argo.Name(..)
  , Argo.Value(..)
  ) where

import qualified Argo
import qualified Argo.Class.HasCodec
import qualified Argo.Codec.Array
import qualified Argo.Codec.Codec
import qualified Argo.Codec.Object
import qualified Argo.Codec.Value
import qualified Argo.Schema.Identifier
import qualified Argo.Type.Permission
import qualified Control.Applicative
import qualified Data.String
import qualified Data.Text
import qualified Data.Typeable

optional
  :: Data.Typeable.Typeable a
  => Argo.Name
  -> Argo.Codec.Value.Value a
  -> Argo.Codec.Object.Object (Maybe a)
optional = Argo.Class.HasCodec.optionalNullable

toValue :: Argo.HasCodec a => a -> Argo.Value
toValue = Argo.Codec.Value.encodeWith Argo.codec

withIdentifier
  :: String -> Argo.Codec.Value.Value a -> Argo.Codec.Value.Value a
withIdentifier =
  Argo.Codec.Value.withIdentifier
    . Argo.Schema.Identifier.fromText
    . Data.Text.pack
