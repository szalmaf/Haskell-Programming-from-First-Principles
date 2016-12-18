fmap' f xs = xs >>= return .f
