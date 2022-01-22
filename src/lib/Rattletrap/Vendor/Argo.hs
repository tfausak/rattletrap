{- hlint ignore "Avoid restricted flags" -}
{-# OPTIONS_GHC -Wno-orphans #-}

{- hlint ignore "Avoid restricted extensions" -}
{-# LANGUAGE FlexibleInstances #-}

module Rattletrap.Vendor.Argo
    ( Argo.HasCodec(codec)
    , Argo.Codec.Codec.map
    , Argo.Codec.Codec.mapMaybe
    , Argo.Codec.Object.fromObjectCodec
    , Argo.Codec.Codec.project
    , Argo.Codec.Object.required
    , Argo.Codec.Object.optional
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
    , Argo.Value
    ) where

import qualified Argo
import qualified Argo.Codec.Array
import qualified Argo.Codec.Codec
import qualified Argo.Codec.Object
import qualified Argo.Type.Permission
import qualified Control.Applicative
import qualified Data.Map
import qualified Data.String
import qualified Data.Text

instance (Argo.HasCodec a, Argo.HasCodec b, Argo.HasCodec c, Argo.HasCodec d) => Argo.HasCodec (a, b, c, d) where
    codec = Argo.Codec.Array.fromArrayCodec Argo.Type.Permission.Forbid $ (,,,)
        <$> Argo.Codec.Codec.project (\ (x, _, _, _) -> x) (Argo.Codec.Array.element Argo.codec)
        <*> Argo.Codec.Codec.project (\ (_, x, _, _) -> x) (Argo.Codec.Array.element Argo.codec)
        <*> Argo.Codec.Codec.project (\ (_, _, x, _) -> x) (Argo.Codec.Array.element Argo.codec)
        <*> Argo.Codec.Codec.project (\ (_, _, _, x) -> x) (Argo.Codec.Array.element Argo.codec)

instance Argo.HasCodec a => Argo.HasCodec (Data.Map.Map Data.Text.Text a) where
    codec = Argo.Codec.Codec.map (Data.Map.mapKeys (\ (Argo.Name x) -> x)) (Data.Map.mapKeys Argo.Name) Argo.codec
