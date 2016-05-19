Asistente para pruebas de lógica proposicional
==============================================
Este asistente verifica que demostraciones de lógica proposicional que siguen
el estilo del libro de Gries & Schneider sean correctas. En caso de serlas,
las imprime paso por paso y da un mensaje de éxito. En caso contrario da un
mensaje de error.

Modo de empleo
--------------
El usuario debe proveer los módulos Theorems.hs y los archivos con las pruebas
como se especifica en el enunciado del proyecto. Los import necesarios para
Theorems.hs son:

    import Operators
    import Variables
    
Y para las pruebas son:

    import ProofAssistant
    
En la carpeta ejemplos están las pruebas que se enviaron por el grupo de correo
de la asignatura, con los import hechos.
    
Para que la prueba funcione, los módulos Inference.hs, Operators.hs, 
Variables.hs y ProofAssistant.hs deben estar en el mismo directorio que la 
prueba y el módulo de teoremas. Para verificar una prueba use ghci:

    ghci <prueba.hs>
    
Y en el prompt de ghci escriba:

    verify
    
Para hacerlo más rápido, puede usar el script verificar que se envía en esta 
entrega:

    verificar <prueba.hs>
    
Sobre la implementación
-----------------------
En Operators.hs están definidos los tipos para las diferentes expresiones 
booleanas y los operadores. En Variables.hs, una variable por cada letra del 
abecedario. Inference.hs contiene todas las funciones puras necesarias para
hacer las inferencias. ProofAssistant.hs implementa las funciones que hacen
salida, y exporta todo lo que el usuario necesita para las pruebas.

Autores
-------
Alfredo Fanghella, carné 12-10967
Ricardo Mena, carné 12-10872

