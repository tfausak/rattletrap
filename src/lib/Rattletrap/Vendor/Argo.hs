module Rattletrap.Vendor.Argo
  ( Argo.HasCodec(codec)
  , Argo.map
  , Argo.mapMaybe
  , Argo.fromObjectCodec
  , Argo.project
  , Argo.Internal.Codec.Object.required
  , optional
  , Argo.identified
  , withIdentifier
  , Argo.Identifier(..)
  , Argo.toValue
  , Argo.Permission(Allow, Forbid)
  , (Control.Applicative.<|>)
  , Argo.fromArrayCodec
  , Argo.Internal.Codec.Array.element
  , Argo.schema
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
import qualified Argo.Internal.Codec.Object
import qualified Argo.Internal.Codec.Value
import qualified Argo.Internal.Schema.Identifier
import qualified Control.Applicative
import qualified Data.Text
import qualified Data.Typeable

optional
  :: Data.Typeable.Typeable a
  => Argo.Name
  -> Argo.Internal.Codec.Value.Value a
  -> Argo.Internal.Codec.Object.Object (Maybe a)
optional = Argo.Internal.Class.HasCodec.optionalNullable

withIdentifier
  :: String
  -> Argo.Internal.Codec.Value.Value a
  -> Argo.Internal.Codec.Value.Value a
withIdentifier =
  Argo.Internal.Codec.Value.withIdentifier
    . Argo.Internal.Schema.Identifier.fromText
    . Data.Text.pack
