--1.Promedio de 3 numeros
promedio :: Float -> Float -> Float -> Float
promedio x y z = (x+y+z)/3

--2.Determinar si el ultimo digito de un numero x es 3
ultimoDigito3 :: (Integral a)=> a -> Bool
ultimoDigito3 numero = (numero `rem` 10) == 3 

--3.Determinar si el numero x tiene 3 digitos
tiene3Digit ::Int ->Bool
tiene3Digit entero = if (entero>99) && (entero<1000) then True else False 

--4.Determinar si el numero x es un negativo
esNegativo :: Int -> Bool
esNegativo numero = if numero < 0 then True else False

--5.Sumar 2 digitos de un numero x, donde x es un numero de 2 digitos
suma2DigNum :: Int -> Int 
suma2DigNum x 
    | x > 9 && x < 100 = (x `mod` 10) + ((x `div` 10) `mod` 10)
    | x > 99 = error "El numero no tiene mas de 2 digitos"
    | x<9 = error "El numero es de un 1 digito"

--6.Determinar si los 2 digitos que componen al número x son numeros pares
--donde x es un numero de 2 digitos
par2DigNum:: Int->Bool
par2DigNum x 
    | (x>9 && x<100) = even (x `mod` 10) && even((x `div` 10) `mod` 10) 
    | x>99 = error "El numero tiene mas de 2 digitos "
    | x<9 = error "El numero es de un 1 digito"

--7.Determinar si el numero x es alguno de los primeros 8 primos
esPrimo :: Int -> Bool
esPrimo x = elem x [2,3,5,7,13,17,19]

--8. Determinar si el número x es alguno de los primeros 8 números primos, además si es un número par.
esPrimoPar :: Int -> Bool
esPrimoPar x = esPrimo x && even(x)

--9.Determinar si el numero x es multiplo del numero y
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = if (x `mod` y == 0 ) then True else False

--10.Determinar si los 2 digitos que componen al numero x son iguales
--donde x es un numero de 2 digitos
esIgual2DigNum:: Int->Bool
esIgual2DigNum x 
    | x>9 && x<100 = if((x `mod` 10)==((x `div` 10) `mod` 10)) then True else False
    | x>99 = error "El numero tiene más de 2 digitos "
    | x<10 = error "El numero tiene 1 digito "

--11.Determinar el numero mayor de 3 numeros x,y,z 
elMayorDe3:: Int->Int->Int->Int
elMayorDe3 a b c 
    | a>b && a>c = a
    | b>a && b>c = b
    | c>a && c>b = c

--12.Determinar si la suma entre el numero x y el numero y origina un numero par
sumaEsParNum :: Int->Int-> Bool
sumaEsParNum x y = if (((x+y) `mod` 2)==0) then True else False

--13.Suma de todos los digitos de los numeros x,y siendo x,y numero de 2 digitos
sumaDigNum2Dig:: Int->Int->Int
sumaDigNum2Dig x y
 | (x>9 && x<100) && (y>9 && y<100) = (x `mod` 10)+((x `div` 10) `mod` 10) + (y `mod`10)+((y `div` 10) `mod` 10)
 | (x<99 || y<99) || (y<9 && x<9) = error "Algún número no tiene dos dígitos "


--14. Suma de los 3 digitos que componen al numero x, donde x es un numero de 3 digitos
suma3DigNum:: Int->Int
suma3DigNum x
    | (x>99 && x<1000) = (x `mod` 10)+((x `div` 10) `mod` 10)+((x `div` 100) `mod` 10)
    | (x<100) = error "El numero no tiene tres digitos "
    | (x>999) = error "El numero no tiene tres digitos "


--15.Determinar si al menos dos de los 3 digitos de x son iguales, donde x es un numero de 3 digitos
sonIgualesNum3Dig:: Int->Bool
sonIgualesNum3Dig x
    | (x>99 && x<1000) = if((x `mod` 10)==((x `div` 10) `mod` 10)) then True else if (((x `mod` 10))==((x `div` 100) `mod` 10)) then True else if (((x `div` 10) `mod` 10)==((x `div` 100) `mod` 10)) then True else False
    | (x<100) = error "El numero no tiene tres digitos "
    | (x>999) = error "El numero no tiene tres digitos " 

--16.Determinar en que posicion esta el digito mayor del numero x, donde x es un numero de 3 digitos
numMayorNum3Dig :: Int->String
numMayorNum3Dig x
 |(x<100 || x>999) = error "El numero no tiene tres dígitos "
 | (x `mod` 10)>((x `div` 10) `mod` 10) && (x `mod` 10) > ((x `div` 100) `mod` 10) = "El mayor esta en posición 3"
 | ((x `div` 10) `mod` 10)>(x `mod` 10) && ((x `div` 10) `mod` 10)>((x `div` 100) `mod`10) = "El mayor esta en posición 2"
 | ((x `div` 100) `mod` 10)>(x `mod` 10) && ((x `div` 100) `mod` 10)> (x `mod` 10) = "El mayor esta en posición 1"

--17.Determina si xs es palidromo
palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == reverse xs

--18.La division de x/y mostrando un error cuando y es 0
safeDivision :: (Eq p, Fractional p) => p -> p -> p
safeDivision _ 0 = error"No es posible dividir entre 0"
safeDivision x y = x/y

--19.El resultado de la operacion logica disyuncion
xor :: Bool -> Bool -> Bool
xor x y = (x || y) && not (x && y)

--20.La distancia entre 2 puntos, siendo x,y cordenadas en un plano cartesiano
distancia2Num :: Floating a => (a, a) -> (a, a) -> a
distancia2Num (x1,y1) (x2,y2) = sqrt((x1 -x2)^2+(y1 -y2)^2)



