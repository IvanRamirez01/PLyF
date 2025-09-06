-- factorial_funcional.hs
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do
    let resultado = factorial 5
    putStrLn ("Factorial de 5: " ++ show resultado)
