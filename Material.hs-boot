{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
module Material where

type role Material representational representational nominal representational
data Material g (v :: * -> *) a c
