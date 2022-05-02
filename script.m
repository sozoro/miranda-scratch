primes :: [num]
primes = [ p | p <- [2..]; and (map ((~= 0) . (p mod)) [2..p^0.5]) ]

app :: (* -> **) -> * -> **
app f x = f x

span :: (* -> bool) -> [*] -> ([*], [*])
span p []     = ([], [])
span p (x:xs) = first (x:) $app span p xs, if p x
span p (x:xs) = ([], (x:xs))             , otherwise

curry :: ((*, **) -> ***) -> * -> ** -> ***
curry f a b = f (a, b)

uncurry :: (* -> ** -> ***) -> (*, **) -> ***
uncurry f (a, b) = f a b

flip :: (* -> ** -> ***) -> ** -> * -> ***
flip f b a = f a b

|| Bifunctor 2-tuple
first :: (* -> ***) -> (*, **) -> (***, **)
first f (a, b) = (f a, b)

second :: (** -> ***) -> (*, **) -> (*, ***)
second f (a, b) = (a, f b)

|| state type synonym
state * ** == * -> (*, **)

eval_state :: state * ** -> * -> **
eval_state f = snd . f

|| Functor state
map_state :: (* -> **) -> state *** * -> state *** **
map_state f g s = second f (g s)

|| Applicative state
pure_state :: * -> state ** *
pure_state a s = (s, a)

ap_state :: state * (** -> ***) -> state * ** -> state * ***
ap_state f g s = second h (g s')
                 where (s', h) = f s

liftA2_state :: (* -> ** -> ***) -> state **** * -> state **** ** -> state **** ***
liftA2_state f s = ap_state (map_state f s)

sequence_list_state :: [state * **] -> state * [**]
sequence_list_state = foldr go (pure_state [])
                      where go x xs = liftA2_state (:) x xs

|| Monad state
join_state :: state * (state * **) -> state * **
join_state f s = g s'
                 where (s', g) = f s

bind_state :: state * ** -> (** -> state * ***) -> state * ***
bind_state s f = join_state (map_state f s)

|| stdin with stdin_lines state
stdin_lines == [[char]]
stdin *     == state stdin_lines *

run_stdin :: stdin * -> *
run_stdin = ($eval_state (lines $-))

get_line :: stdin [char]
get_line (l:ls) = (ls, l)

put_str_ln :: [char] -> sys_message
put_str_ln = Stdout . (++ "\n")

prime_factors :: num -> [num]
prime_factors n = []    , if n <= 1
prime_factors n = go 2 n, otherwise
                  where
                  go m n = [n]               , if m > n ^ 0.5
                  go m n = m : go m (n div m), if n mod m = 0
                  go m n = go (m + 1) n      , otherwise

ans_pfs :: stdin [sys_message]
ans_pfs = map_state f get_line
          where
          f line = [Stdout "Enter an integer bigger than one: ", print_pfs_or_exit]
                   where
                   print_pfs_or_exit = print_pfs (numval line), if and (map digit line)
                   print_pfs_or_exit = Exit 0                 , otherwise
                   print_pfs n = put_str_ln str
                                 where
                                 pfs = prime_factors n
                                 str = show n ++ " <= 1"             , if pfs = []
                                 str = show n ++ " is a prime number", if pfs = [n]
                                 str = "Prime factors of " ++ show n
                                       ++ " are " ++ show pfs        , otherwise

main :: [sys_message]
main = put_str_ln "~~ Prime Factorization ~~"
       : concat $app run_stdin $app sequence_list_state $app repeat ans_pfs
