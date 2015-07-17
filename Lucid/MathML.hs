{-# LANGUAGE OverloadedStrings #-}

-- | See: http://www.w3.org/TR/MathML3/

module Lucid.MathML where

import Lucid.Base

-- | @math@ element: Top-Level Element
math_ :: Term arg result => arg -> result
math_ = term "math"

-- | @mn@ element: Number
mn_ :: Term arg result => arg -> result
mn_ = term "mn"

-- | @mo@ element: Operator, Fence, Separator or Accent
mo_ :: Term arg result => arg -> result
mo_ = term "mo"

-- | @mrow@ element: Horizontally Group Sub-Expressions
mrow_ :: Term arg result => arg -> result
mrow_ = term "mrow"

-- | @mpadded@ element: Adjust Space Around Content
mpadded_ :: Term arg result => arg -> result
mpadded_ = term "mpadded"

-- | @mfrac@ element: Fractions
mfrac_ :: Term arg result => arg -> result
mfrac_ = term "mfrac"

-- | Fractions attributes
[linethickness_, numalign_, denomalign_, bevelled_] = map makeAttribute ["linethickness", "numalign", "denomalign", "bevelled"]
