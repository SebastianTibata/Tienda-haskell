type Product = (String, Double, Int) -- (Nombre, Precio, Cantidad)

type Inventory = [Product]

-- Función para agregar un producto
addProduct :: Inventory -> String -> Double -> Int -> Inventory
addProduct inventory name price quantity = (name, price, quantity) : inventory

-- Función para actualizar la cantidad de un producto
updateQuantity :: Inventory -> String -> Int -> Inventory
updateQuantity [] _ _ = []
updateQuantity ((n, p, q) : xs) name newQty
  | n == name = (n, p, newQty) : xs
  | otherwise = (n, p, q) : updateQuantity xs name newQty

-- Función para eliminar un producto
removeProduct :: Inventory -> String -> Inventory
removeProduct [] _ = []
removeProduct ((n, p, q) : xs) name
  | n == name = xs
  | otherwise = (n, p, q) : removeProduct xs name

-- Función para obtener un resumen del inventario
inventorySummary :: Inventory -> (Int, Double)
inventorySummary inventory =
  let totalQty = sum [q | (_, _, q) <- inventory]
      totalValue = sum [p * fromIntegral q | (_, p, q) <- inventory]
   in (totalQty, totalValue)

-- Función para buscar un producto por su nombre
searchProduct :: Inventory -> String -> Maybe (Double, Int)
searchProduct [] _ = Nothing
searchProduct ((n, p, q) : xs) name
  | n == name = Just (p, q)
  | otherwise = searchProduct xs name

-- Función para aplicar un descuento a todos los productos
applyDiscount :: Inventory -> Double -> Inventory
applyDiscount inventory discount =
  [(n, p * (1 - discount / 100), q) | (n, p, q) <- inventory]

-- Prueba en main
main :: IO ()
main = do
  let inventory = []
  let inventory1 = addProduct inventory "Manzanas" 0.5 100
  let inventory2 = addProduct inventory1 "Platanos" 0.3 150
  let inventory3 = updateQuantity inventory2 "Manzanas" 120
  let inventory4 = removeProduct inventory3 "Platanos"
  let (totalQty, totalValue) = inventorySummary inventory4

  putStrLn $ "Inventario Final: " ++ show inventory4
  putStrLn $ "Total de productos en stock: " ++ show totalQty
  putStrLn $ "Valor total del inventario: " ++ show totalValue

  -- Buscar un producto
  case searchProduct inventory4 "Pablo" of
    Just (price, qty) -> putStrLn $ "Manzanas - Precio: " ++ show price ++ ", Cantidad: " ++ show qty
    Nothing -> putStrLn "Producto no encontrado"

  -- Aplicar descuento del 10%
  let discountedInventory = applyDiscount inventory4 10
  putStrLn $ "Inventario con descuento: " ++ show discountedInventory
