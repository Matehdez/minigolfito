module Library where
import PdePreludat
import Data.List (sort, filter)

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String, --nombre del jugador
  padre :: String, --nombre del padre
  habilidad :: Habilidad --habilidad del jugador
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number ,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

--un tiro de golf
data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between n m x = x `elem` [n .. m]

maximoSegun :: Ord a1 => (a2 -> a1) -> [a2] -> a2
maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

--También necesitaremos modelar los palos de golf que pueden usarse y 
--los obstáculos que deben enfrentar para ganar el juego.

---Pto1-----------------------------------------------------------------------------------
--Sabemos que cada palo genera un efecto diferente, 
--por lo tanto elegir el palo correcto puede ser la diferencia entre ganar o perder el torneo.
---a------------------------------
--Modelar los palos usados en el juego que a partir de una determinada habilidad 
--generan un tiro que se compone por velocidad, precisión y altura.

---PALOS
--El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.
--La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión.
--Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan un tiro de velocidad igual a la fuerza multiplicada por n, la precisión dividida por n y una altura de n-3 (con mínimo 0). 
--Modelarlos de la forma más genérica posible.

type Palo = (Habilidad -> Tiro)

putter :: Palo
putter unaHabilidad = UnTiro { 
    velocidad = 10, 
    precision =  precisionJugador unaHabilidad * 2, 
    altura = 0 
    }

madera :: Palo
madera unaHabilidad = UnTiro { 
    velocidad = 100, 
    precision =  precisionJugador unaHabilidad `div` 2, 
    altura = 5 
    }

hierro :: Number -> Palo
hierro n unaHabilidad = UnTiro { 
    velocidad = fuerzaJugador unaHabilidad *n, 
    precision =  precisionJugador unaHabilidad `div` n, 
    altura = n-3 `max` 0 
    }

---b------------------------------
-- Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego.
palos :: [Palo]
palos = [putter, madera] ++ map hierro [1..10]

---Pto2-----------------------------------------------------------------------------------
--Definir la función golpe que dados una persona y un palo, 
--obtiene el tiro resultante de usar ese palo con las habilidades de la persona.

--Ej:
--Por ejemplo si Bart usa un putter, se genera un tiro de velocidad = 10, precisión = 120 y altura = 0.

golpe :: Jugador -> Palo -> Tiro
golpe unJugador unPalo = unPalo (habilidad unJugador)

---Pto3-----------------------------------------------------------------------------------
--Lo que nos interesa de los distintos obstáculos es si un tiro puede superarlo, 
--y en el caso de poder superarlo, 
--cómo se ve afectado dicho tiro por el obstáculo. 
--En principio necesitamos representar los siguientes obstáculos:

--Se desea saber cómo queda un tiro luego de intentar superar un obstáculo, 
--teniendo en cuenta que en caso de no superarlo, se detiene, quedando con todos sus componentes en 0.

---a------------------------------
--Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, 
--independientemente de la velocidad del tiro. 
--Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a ser 100 y la altura 0.

data Obstaculo = UnObstaculo {
  sePuedeSuperar :: Tiro -> Bool ,
  efecto :: Tiro-> Tiro
}

{--obstaculoaSuperar :: (Tiro -> Bool) -> (Tiro -> Tiro) -> Tiro -> Tiro
obstaculoaSuperar podriaSuperarlo superoObstaculo  unTiro 
  |podriaSuperarlo unTiro = superoObstaculo unTiro
  |otherwise = tiroDetenido--}

intentaSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentaSuperarObstaculo obstaculo tiro 
  |sePuedeSuperar obstaculo tiro = efecto obstaculo tiro
  |otherwise = efecto obstaculo tiroDetenido

tiroDetenido :: Tiro
tiroDetenido = UnTiro 0 0 0

----------------

tunel :: Obstaculo
tunel = UnObstaculo podriaSuperarTunel efectoTunel  

podriaSuperarTunel :: Tiro -> Bool
podriaSuperarTunel unTiro = precision unTiro > 90 && altura unTiro == 0

efectoTunel :: Tiro -> Tiro
efectoTunel unTiro = UnTiro{ velocidad = velocidad unTiro *2   , precision= 100 ,altura= 0 }

---b------------------------------
--Una laguna es superada si la velocidad del tiro es mayor a 80 y 
--tiene una altura de entre 1 y 5 metros. 
--Luego de superar una laguna el tiro llega con la misma velocidad y precisión, 
--pero una altura equivalente a la altura original dividida por el largo de la laguna.

laguna :: Number -> Obstaculo
laguna n  = UnObstaculo podriaSuperarLaguna (efectoLaguna n)  

podriaSuperarLaguna :: Tiro -> Bool
podriaSuperarLaguna unTiro = velocidad unTiro > 80 && between 1 5 (altura unTiro)

efectoLaguna :: Number -> Tiro -> Tiro
efectoLaguna n unTiro = UnTiro{ velocidad = velocidad unTiro  , precision= precision unTiro ,altura= altura unTiro `div` n }

---c------------------------------

--Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo 
--con una precisión mayor a 95. Al superar el hoyo, el tiro se detiene, 
--quedando con todos sus componentes en 0.

hoyo :: Obstaculo
hoyo = UnObstaculo podriaSuperarHoyo efectoHoyo 

efectoHoyo :: Tiro -> Tiro
efectoHoyo unTiro = tiroDetenido

podriaSuperarHoyo :: Tiro -> Bool
podriaSuperarHoyo  = entraAlHoyo

entraAlHoyo :: Tiro -> Bool
entraAlHoyo unTiro = velocidadOptima (velocidad unTiro) && precision unTiro > 95 && altura unTiro == 0 

velocidadOptima :: Number -> Bool
velocidadOptima velocidad = velocidad>=5 && velocidad <= 20

---Pto4-----------------------------------------------------------------------------------
---a-------------------------
--Definir palosUtiles que dada una persona y un obstáculo, 
--permita determinar qué palos le sirven para superarlo.

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador unObstaculo = PdePreludat.filter (obstaculoConPalo unJugador unObstaculo) palos

--Tengo lista de palos, deberia verificar uno por uno si será util para superar el obstaculo

obstaculoConPalo :: Jugador -> Obstaculo-> Palo-> Bool
obstaculoConPalo unJugador unObstaculo unPalo  =  sePuedeSuperar unObstaculo (golpe unJugador unPalo) 

---b-------------------------
--Saber, a partir de un conjunto de obstáculos y un tiro, 
--cuántos obstáculos consecutivos se pueden superar.

{--Ej
--Por ejemplo, para un tiro de velocidad = 10, precisión = 95 y altura = 0, 
y una lista con dos túneles con rampita seguidos de un hoyo, 
el resultado sería 2 ya que la velocidad al salir del segundo túnel es de 40, 
por ende no supera el hoyo.--}

--BONUS: 
--resolver este problema sin recursividad, 
--teniendo en cuenta que existe una función

--takeWhile :: (a -> Bool) -> [a] -> [a] que podría ser de utilidad.
--Mientras se pueda superar el tiro (a -> Bool), agarro todos los obstaculos

obstaculosSuperados :: Tiro-> [Obstaculo] -> Number
obstaculosSuperados _ [] = 0
obstaculosSuperados tiro (x:xs) 
  | sePuedeSuperar x tiro = 1 + obstaculosSuperados (efecto x tiro) xs 
  |otherwise = 0


---c-------------------------
--Definir paloMasUtil que recibe una persona y una lista de obstáculos y 
--determina cuál es el palo que le permite superar más obstáculos con un solo tiro.

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos 
  = maximoSegun (flip obstaculosSuperados obstaculos.golpe jugador) palos

---Pto5-----------------------------------------------------------------------------------

--Dada una lista de tipo [(Jugador, Puntos)] que tiene la información 
--de cuántos puntos ganó cada niño al finalizar el torneo, 
--se pide retornar la lista de padres que pierden la apuesta por ser el “padre del niño que no ganó”. 
--Se dice que un niño ganó el torneo si tiene más puntos que los otros niños.

jugadorTorneo = fst
puntosJugador = snd

pierdenLaApuesta :: [(Jugador, Puntos)] -> [String]
pierdenLaApuesta puntosTorneo = (map (padre.jugadorTorneo) . PdePreludat.filter (not . gano puntosTorneo)) puntosTorneo

gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosTorneo puntosdeUnJugador 
  = (all ((< puntosJugador puntosdeUnJugador). puntosJugador). PdePreludat.filter (/= puntosdeUnJugador)) puntosTorneo