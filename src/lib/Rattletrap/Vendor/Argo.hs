module Rattletrap.Vendor.Argo
  ( Argo.HasCodec(codec)
  , Argo.map
  , Argo.mapMaybe
  , Argo.fromObjectCodec
  , Argo.required
  , Argo.optional
  , Argo.identified
  , Argo.withIdentifier
  , Argo.Identifier(..)
  , Argo.toValue
  , Argo.Permission(Allow, Forbid)
  , oneOf
  , Argo.fromArrayCodec
  , Argo.element
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
import qualified Control.Applicative as Applicative
import qualified Data.Foldable as Foldable

oneOf :: Foldable t => t (Argo.Codec a) -> Argo.Codec a
oneOf xs = case Foldable.toList xs of
  [] -> Applicative.empty
  x : ys -> foldr (Applicative.<|>) x ys
