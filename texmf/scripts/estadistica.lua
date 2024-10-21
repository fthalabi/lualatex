---@diagnostic disable: undefined-global
function Muestra_normal(media, desviacion)
  -- Box-Muller transform
  local u1 = math.random()
  local u2 = math.random()
  local z0 = math.sqrt(-2 * math.log(u1)) * math.cos(2 * math.pi * u2)
  -- Transform to desired media and standard desviacion
  return media + z0 * desviacion
end

function Generar_muestras(n, mean, deviation)
  math.randomseed(os.time())
  local list = {}
  for i = 1, n do
    repeat
      muestra = math.floor(Muestra_normal(mean, deviation))
    until muestra >= 0
    list[i] = muestra
  end
  return list
end

function Obtener_unicos(lista)
  local seen = {}
  local uniques = {}

  for _, value in pairs(lista) do
    if not seen[value] == true then
      table.insert(uniques, value)
      seen[value] = true
    end
  end
  table.sort(uniques)
  return uniques
end

function Frecuencias(lista)
  local list_count = {}
  local uniques = Obtener_unicos(lista)
  for _, v in pairs(uniques) do
    local count = 0
    for _, w in pairs(lista) do
      if w == v then
        count = count + 1
      end
    end
    table.insert(list_count, count)
  end
  return list_count
end

function Probabilidades(lista)
  local probs = {}
  local cantidad = Frecuencias(lista)
  for _, v in pairs(cantidad) do
    table.insert(probs, (v / #lista))
  end
  return probs
end

function Suma_acumulativa(lista)
  local acumulado = 0
  local agregado = {}
  for i, v in pairs(lista) do
    acumulado = acumulado + v
    table.insert(agregado, acumulado)
  end
  return agregado
end

function Imprimir_columnas(columnas)
  local k = 1
  while columnas[k] == nil and k < #columnas + 1 do
    k = k + 1
  end
  local tabla = ""
  for i = 1, #columnas[k] do
    local fila = {}
    for j = 1, #columnas do
      if columnas[j] == nil then
        table.insert(fila, "   ")
      else
        table.insert(fila, columnas[j][i])
      end
    end
    tabla = tabla .. table.concat(fila, " & ") .. " \\\\ "
  end
  return tabla
end

function Columnas_a_latex(columnas)
  local k = 1
  while columnas[k] == nil and k < #columnas + 1 do
    k = k + 1
  end
  for i = 1, #columnas[k] do
    local fila = {}
    for j = 1, #columnas do
      if columnas[j] == nil then
        table.insert(fila, "   ")
      else
        table.insert(fila, columnas[j][i])
      end
    end
    tex.print(table.concat(fila, " & ") .. " \\\\ ")
  end
end

function Escribir_archivo(archivo, texto)
  archivo = io.open(archivo, "w")
  archivo:write(texto)
  archivo:close()
end

function Csv(archivo, columnas)
  archivo = io.open(archivo, "w")
  if archivo ~= nil then
    local k = 1
    while columnas[k] == nil and k < #columnas + 1 do
      k = k + 1
    end
    for i = 1, #columnas[k] do
      local fila = {}
      for j = 1, #columnas do
        if columnas[j] == nil then
          table.insert(fila, "    ")
        else
          table.insert(fila, columnas[j][i])
        end
      end
      archivo:write(table.concat(fila, " , ") .. "\n")
    end
    archivo:close()
  else
    print("ups, no se pudo escribir el archivo.")
  end
end

function Media(lista)
  local total = 0
  for _, v in pairs(lista) do
    total = total + v
  end
  return total / Largo(lista)
end

function Largo(lista)
  local i = 0
  for _, v in pairs(lista) do
    i = i + 1
  end
  return i
end

function Maximo(lista)
  local max = lista[1]
  for _, v in pairs(lista) do
    if v > max then
      max = v
    end
  end
  return max
end

function Minimo(lista)
  local min = lista[1]
  for _, v in pairs(lista) do
    if v < min then
      min = v
    end
  end
  return min
end

function Rango(lista)
  local max = Maximo(lista)
  local min = Minimo(lista)
  return max - min
end

function Divisores(n)
  local div = {}
  for i = 1, n do
    if n % i == 0 then
      table.insert(div, i)
    end
  end
  return div
end

function Mas_cercano(lista, objetivo)
  local min_dif = Maximo(lista)
  local min_i = 0
  for i, v in pairs(lista) do
    if math.abs(objetivo - v) <= min_dif then
      min_dif = math.abs(objetivo - v)
      min_i = i
    end
  end
  return lista[min_i]
end

function Divisor_ideal(lista)
  local rango = Rango(lista)
  local divisores = Divisores(rango)
  local grupos = {}
  for _, v in pairs(divisores) do
    table.insert(grupos, math.floor(rango / v))
  end
  return Mas_cercano(grupos, 5)
end

function Cuantil(lista, F)
  local probabilidades = Probabilidades(lista)
  local p_acumulativa = Suma_acumulativa(probabilidades)
  local unicos = Obtener_unicos(lista)
  local i = 1
  while (p_acumulativa[i] < F) do
    i = i + 1
  end
  return unicos[i]
end
