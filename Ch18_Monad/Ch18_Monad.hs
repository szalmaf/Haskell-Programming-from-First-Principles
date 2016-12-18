fmap' f xs = xs >>= return .f

-- fmap (+1) [1..3]
-- [1..3] >>= return . (+1)