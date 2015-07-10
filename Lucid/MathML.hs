{-# LANGUAGE OverloadedStrings #-}

-- | See: http://www.w3.org/TR/2014/REC-MathML3-20140410

module Lucid.MathML where

import Lucid.Base

-- | @mn@ element: Number
mn_ :: Term arg result => arg -> result
mn_ = term "mn"

-- | @mo@ element: Operator, Fence, Separator or Accent
mo_ :: Term arg result => arg -> result
mo_ = term "mo"

-- | @mrow@ element: Horizontally Group Sub-Expressions
mrow_ :: Term arg result => arg -> result
mrow_ = term "mrow"

