module Tests where

import Test.HUnit
import Solucion

run = runTestTT tests

tests = test [
    "Casos de prueba para testSuiteNombresDeUsuarios" ~: testSuiteNombresDeUsuarios,
    "Casos de prueba para testSuiteAmigosDe" ~: testSuiteAmigosDe,
    "Casos de prueba para testSuiteCantidadDeAmigos" ~: testSuiteCantidadDeAmigos,
    "Casos de prueba para testSuiteCantidadDeAmigos" ~: testSuiteUsuarioConMasAmigos,
    "Casos de prueba para testSuiteUsuarioConMasAmigos" ~: testSuiteUsuarioConMasAmigos   
    ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- Datos de prueba --

-- Usuarios:
usuario1 = (1, "Alice")
usuario2 = (2, "Bob")
usuario3 = (3, "Rob")
usuario4 = (4, "Rob")
usuario5 = (5, "Tod")
usuario6 = (6, "Lisa")

usuariosSinNombresRepetidos = [usuario1, usuario2, usuario3] -- para testar nombresDeUsuarios
usuariosConNombresRepetidos = [usuario1, usuario2, usuario3, usuario4] -- para testar nombresDeUsuarios

variosUsuarios = [usuario1, usuario2, usuario3, usuario4] -- para testar amigosDe

usuariosMismaCantAmigos = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6]

-- Relaciones:
relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3) 
relacion1_4 = (usuario1, usuario4)

relacion3_4 = (usuario4, usuario3)
relacion3_5 = (usuario3, usuario5)

relacion1_6 = (usuario6, usuario1)

sinAmistades = []

algunaRelacion = [relacion1_2] -- para testar nombresDeUsuarios

variasRelaciones = [relacion1_2, relacion1_3, relacion3_5] -- para testar amigosDe
relacionesNombresRepetidos = [relacion1_2, relacion1_3, relacion1_4] -- para testar amigosDe

relacionesMismaCantAmigos = [relacion3_4, relacion3_5, relacion1_6, relacion1_2, relacion1_3]

-- Publicaciones:
publicacion1 = (usuario1, "¡Hola! Esta es mi primera publicación.", [usuario2]) -- para testar amigosDe
publicacion2 = (usuario2, "Cualquier texto", [usuario3]) -- para testar amigosDe
publicacion3 = (usuario3, "Cualquier texto", [usuario2, usuario1]) -- para testar amigosDe

algunaPublicacion = [publicacion1] -- para testar nombresDeUsuarios

variasPublicaciones = [publicacion1, publicacion2, publicacion3] -- para testar amigosDe

-- Redes:
redUnicoUsuario = ([usuario1], algunaRelacion, algunaPublicacion) -- para testar nombresDeUsuarios
redSinNombresRepetidos = (usuariosSinNombresRepetidos, variasRelaciones, algunaPublicacion) -- para testar nombresDeUsuarios
redConNombresRepetidos = (usuariosConNombresRepetidos, relacionesNombresRepetidos, algunaPublicacion) -- para testar nombresDeUsuarios

red = (variosUsuarios, variasRelaciones, variasPublicaciones) -- para testar amigosDe

redMismaCantidadAmigos = (usuariosMismaCantAmigos, relacionesMismaCantAmigos, variasPublicaciones)

redSinAmigos = ([usuario6], sinAmistades, variasPublicaciones)

-- Tests --

testSuiteNombresDeUsuarios = test [ 
    "Caso 1: Red social con un solo usuario" ~: (nombresDeUsuarios redUnicoUsuario) ~?= ["Alice"],
    "Caso 3: Red social con usuarios sin nombres repetidos" ~: (nombresDeUsuarios redSinNombresRepetidos) ~?= ["Alice", "Bob", "Rob"],
    "Caso 2: Red social con usuarios con nombres repetidos" ~: (nombresDeUsuarios redConNombresRepetidos) ~?= ["Alice", "Bob", "Rob"]
    ]

testSuiteAmigosDe = test [
    "Caso 1: Usuario sin amigos" ~: (amigosDe red usuario4) ~?= [],
    "Caso 2: Usuario con un solo amigo" ~: (amigosDe red usuario5) ~?= [(3, "Rob")],
    "Caso 3: Usuario con amigos sin nombres repetidos" ~: (amigosDe redSinNombresRepetidos usuario1) ~?= [(2, "Bob"), (3, "Rob")],
    "Caso 4: Usuario con amigos con nombres repetidos" ~: (amigosDe redConNombresRepetidos usuario1) ~?= [(2, "Bob"), (3, "Rob"), (4, "Rob")]
    ]

testSuiteCantidadDeAmigos = test [
    "Caso 1: Tiene 0 amigos" ~: (cantidadDeAmigos red usuario4) ~?= 0,
    "Caso 2: Tiene 1 amigo" ~: (cantidadDeAmigos red usuario5) ~?= 1,
    "Caso 3: Tiene 2 amigos" ~: (cantidadDeAmigos red usuario1) ~?= 2
    ]

testSuiteUsuarioConMasAmigos = test [
    "Caso 1: El único usuario de red" ~: (usuarioConMasAmigos redSinAmigos) ~?= (6, "Lisa"),
    "Caso 2: Un solo usuario con más amigos" ~: (usuarioConMasAmigos red) ~?= (1, "Alice"),
    "Caso 3: Dos usuarios con la misma cantidad más alta de amigos" ~: (usuarioConMasAmigos redMismaCantidadAmigos) ~?= (1, "Alice")
    ]