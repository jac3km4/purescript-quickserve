module Type.Symbol where
import Type.Data.Symbol (class Cons, SProxy(..))

class Remove (match :: Symbol) (sym :: Symbol) (out :: Symbol) | match sym -> out

class Match (match :: Symbol) (head :: Symbol) (tail :: Symbol) (out :: Symbol) | match head tail -> out

instance nilMatch :: Match c "" "" ""
else instance hitMatch :: (Remove c t out) => Match c c t out
else instance noMatch :: (Remove c t out, Cons h out outs) => Match c h t outs

instance nilRemove :: Remove c "" ""
else instance consRemove :: (Cons h t s, Match c h t out) => Remove c s out

-- | Remove all occurences of a character in a typelevel string
remove
  :: ∀ char sym out
   . Remove char sym out
  => SProxy char
  -> SProxy sym
  -> SProxy out
remove _ _ = SProxy :: SProxy out
