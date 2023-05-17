module Solucion where

-- Definiciones de tipos

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])


-- Funciones básicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us


-- Ejercicios --

-- Ejercicio 1:
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = proyectarNombres (usuarios red)
    
proyectarNombres ::  [Usuario] ->  [String]
proyectarNombres [] = []
proyectarNombres (u:us) | pertenece (nombreDeUsuario u) nombres = nombres
                        | otherwise = nombreDeUsuario u : nombres
                        where nombres = proyectarNombres us

-- Ejercicio 2:
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = amigosDeAux red u rels
               where rels = relaciones red 

amigosDeAux :: RedSocial -> Usuario -> [Relacion] -> [Usuario]
amigosDeAux red u [] = []
amigosDeAux red u (rel:rels) | idDeUsuario (fst rel) == idDeUsuario u = (snd rel : listaDeAmigos) ++ amigosDeAux red u rels
                             | idDeUsuario (snd rel) == idDeUsuario u = (fst rel : listaDeAmigos) ++ amigosDeAux red u rels
                             | otherwise = amigosDeAux red u rels 
                             where listaDeAmigos = []

-- Ejercicio 3:
cantidadDeAmigos :: RedSocial -> Usuario -> Integer
cantidadDeAmigos red u = longitud (amigosDe red u)

longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- Ejercicio 4:
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = usuarioConMasAmigosAux red us
                        where us = usuarios red

usuarioConMasAmigosAux :: RedSocial -> [Usuario] -> Usuario
usuarioConMasAmigosAux red [u] = u
usuarioConMasAmigosAux red (u1:u2:us) | cantidadDeAmigos red u1 >= cantidadDeAmigos red u2 = usuarioConMasAmigosAux red (u1:us)
           | otherwise = usuarioConMasAmigosAux red (u2:us)

-- Ejercicio 5:
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = estaRobertoCarlosAux red (u:us)
                      where (u:us) = usuarios red 

estaRobertoCarlosAux :: RedSocial -> [Usuario] -> Bool
estaRobertoCarlosAux red [] = False
estaRobertoCarlosAux red (u:us) | (cantidadDeAmigos red u) > 1000000 = True
                                | otherwise = estaRobertoCarlosAux red us

-- Ejercicio 6: 
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = publicacionesDeAux red u (pub:pubs)
                      where (pub:pubs) = publicaciones red

publicacionesDeAux :: RedSocial -> Usuario -> [Publicacion] -> [Publicacion]
publicacionesDeAux red u [] = []
publicacionesDeAux red u (pub:pubs) | (usuarioDePublicacion pub == u) && noHayPublicacionesRepetidas publicaciones = pub : publicaciones
                                  where publicaciones = []

-- Ejercicio 7:
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = publicacionesQueLeGustanAAux red u (pub:pubs)
                                where (pub:pubs) = publicaciones red

publicacionesQueLeGustanAAux :: RedSocial -> Usuario -> [Publicacion] -> [Publicacion]
publicacionesQueLeGustanAAux red u [] = []
publicacionesQueLeGustanAAux red u (pub:pubs) | pertenece u (likesDePublicacion pub) = pub : publicaciones
                                              | otherwise = publicacionesQueLeGustanAAux red u pubs
                                              where publicaciones = []

-- Ejercicio 8:
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = publicacionesQueLeGustanA red u1 == publicacionesQueLeGustanA red u2

-- Ejercicio 9:
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u1 = tieneUnSeguidorFielAux red u1 (u2:us) (pub:pubs)
                                where (u2:us) = usuarios red 
                                      (pub:pubs) = publicaciones red


tieneUnSeguidorFielAux :: RedSocial -> Usuario -> [Usuario] -> [Publicacion] -> Bool
tieneUnSeguidorFielAux red u1 _ [] = False
tieneUnSeguidorFielAux red u1 [] _ = False
tieneUnSeguidorFielAux red u1 (u2:us) [pub] | u1 /= u2 && usuarioDePublicacion pub == u1 && pertenece u2 (likesDePublicacion pub) = True
tieneUnSeguidorFielAux red u1 (u2:us) (pub:pubs) | u1 /= u2 && usuarioDePublicacion pub == u1 && pertenece u2 (likesDePublicacion pub) = tieneUnSeguidorFielAux red u1 (u2:us) pubs
                                                 | otherwise = tieneUnSeguidorFielAux red u1 us (pub:pubs)
                                                 where todasSusPubs = (pub:pubs)


-- Predicados --

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece e [] = False
pertenece e (x:xs) = e == x || pertenece e xs 

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos l1 l2 = (length l1 == length l2) && esIgual l1 l2 && esIgual l2 l1 -- verifica doble pertenencia

-- Auxiliar de mismosElementos
esIgual :: (Eq t) => [t] -> [t] -> Bool
esIgual [] [] = True
esIgual _ [] = False
esIgual [] _ = False
esIgual s t = pertenece (head s) t && (esIgual (tail s) t || tail s == [])

usuarioValido :: Usuario -> Bool
usuarioValido u = (idDeUsuario u) > 0 && (length (nombreDeUsuario u) > 0)

noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos us = sinRepetidos us 

publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas us pubs = (usuariosDePublicacionSonUsuariosDeRed us pubs) 
                                && (usuariosDeLikeDePublicacionSonUsuariosDeRed us pubs) 
                                && (noHayPublicacionesRepetidas pubs)

usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed us pubs = pertenece (usuarioDePublicacion (head pubs)) us 
                                                && usuariosDePublicacionSonUsuariosDeRed us (tail pubs)

usuariosDeLikeDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDeLikeDePublicacionSonUsuariosDeRed us [] = True
usuariosDeLikeDePublicacionSonUsuariosDeRed us pubs = usuariosLikeValidos us (likesDePublicacion (head pubs)) 
                                                      && usuariosDeLikeDePublicacionSonUsuariosDeRed us (tail pubs)

usuariosLikeValidos :: [Usuario] -> [Usuario] -> Bool
usuariosLikeValidos [] us = True
usuariosLikeValidos (user:usl) us = (pertenece user us) && (usuariosLikeValidos usl us)  

noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas [] = True
noHayPublicacionesRepetidas pubs = sinRepetidos pubs || not (hayPubsRepetidasMismoUsuario pubs)

-- Auxiliar 1 de noHayPublicacionesRepetidas
hayPubsRepetidasMismoUsuario :: [Publicacion] -> Bool
hayPubsRepetidasMismoUsuario [] = False
hayPubsRepetidasMismoUsuario [p] = False
hayPubsRepetidasMismoUsuario (p:ps) = hayPubsRepetidasNoConsecutivasMismoUsuario p ps || hayPubsRepetidasMismoUsuario ps

-- Auxiliar 2 de noHayPublicacionesRepetidas
hayPubsRepetidasNoConsecutivasMismoUsuario :: Publicacion -> [Publicacion] -> Bool
hayPubsRepetidasNoConsecutivasMismoUsuario _ [] = False
hayPubsRepetidasNoConsecutivasMismoUsuario p1 (p2:ps) = idDeUsuario (usuarioDePublicacion p1) == idDeUsuario (usuarioDePublicacion p2) 
                                                        || hayPubsRepetidasNoConsecutivasMismoUsuario p1 ps

sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed red (u:us) = pertenece u (usuarios red) && sonDeLaRed red us

terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon e [] = False
terminaCon e [x] = e == x
terminaCon e (x:xs) = e == head (reverso xs) 

-- Auxiliar de terminaCon
reverso :: [t] -> [t]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos [] = True
sinRepetidos (x:xs) = not(pertenece x xs) && sinRepetidos xs

