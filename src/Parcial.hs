module Parcial where
import Text.Show.Functions()


doble :: Float -> Float
doble = (*2)

data Perro = UnPerro{
raza :: String ,
juguetesFavoritos :: [String],
tiempo:: Float,
energia :: Float
                } deriving (Show, Eq)

data Actividad = UnaActividad{
    ejercicio :: (Perro -> Perro),
    duracion :: Float
}

data Guarderia = UnaGuarderia{
    nombre:: String,
    runtina:: [Actividad]
}

modificarEnergia :: Float -> (Float->Float->Float) -> Perro
modificarEnergia  unPerro unaFuncion alteracion = unPerro {energia= max 0.unaFuncion.alteracion.energia $ unPerro} 


jugar :: Perro -> Perro

jugar unPerro = modificarEnergia (energia unPerro) subtract 10 

ladrar :: Float -> Perro -> Perro

ladrar cantidadLadridos unPerro = modificarEnergia  unPerro subtract (cantidadLadridos / 2)

regalarJuguete ::  String -> Perro -> Perro

regalarJuguete unJuguete unPerro  = unPerro{juguetesFavoritos= ( unJuguete : (juguetesFavoritos unPerro))} 

razaExtravagante :: Perro -> Bool 
razaExtravagante (UnPerro "dalmata" _ _ _)  = True
razaExtravagante (UnPerro "pomerania" _ _ _) = True
razaExtravagante _ = False 

permaneceMinimo :: Perro -> Float -> Bool 
permaneceMinimo unPerro tiempoMinimo = tiempo unPerro >= tiempoMinimo

accesoSpa :: Perro -> Bool 
accesoSpa unPerro = (permaneceMinimo unPerro 50) || (razaExtravagante unPerro)

diaDeSpa ::  Perro -> Perro 
diaDeSpa unPerro 
  | accesoSpa unPerro =  regalarJuguete "peine de goma" unPerro{energia = 100} 
  | otherwise = unPerro

perderJuguete :: Perro -> Perro 

perderJuguete unPerro= unPerro{juguetesFavoritos = drop 1 (juguetesFavoritos unPerro)}

diaDeCampo :: Perro -> Perro 
diaDeCampo unPerro = perderJuguete((jugar unPerro)) 

zara :: Perro
zara= UnPerro "dalmata" ["pelota","mantita"] 90 80

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos= UnaGuarderia "pdePerritos" [UnaActividad jugar 30,
                                     UnaActividad (ladrar 18) 20,
                                     UnaActividad (regalarJuguete "pelota") 0,
                                     UnaActividad diaDeSpa 120,
                                     UnaActividad diaDeCampo 720]
                                     

-- B

tiempoTotalRutina :: Guarderia -> Float 

tiempoTotalRutina unaGuarderia = sum (map duracion(runtina unaGuarderia))

puedeAccederGuarderia :: Perro -> Guarderia -> Bool 
puedeAccederGuarderia unPerro unaGuarderia = permaneceMinimo unPerro (tiempoTotalRutina unaGuarderia)


cantidadJuguetesPerro :: Perro -> Int 
cantidadJuguetesPerro unPerro = length.juguetesFavoritos $ unPerro

esPerroResponsable :: Perro -> Bool 
esPerroResponsable unPerro = (<3).cantidadJuguetesPerro $ unPerro


