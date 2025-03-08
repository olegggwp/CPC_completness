#!/bin/bash
OUTPUT="Combined.hs"

# Укажите порядок файлов вручную, например:
FILES=(
    "EvalTerm.hs"
    "LemmTests.hs"
    "OldX.hs"
    "PrintUtils.hs"
    "Reform.hs"
    "RTL.hs"
    "TrueChecker.hs"
    "DMain.hs"
    "Lemms.hs"            
    "Parser.hs"
    "ProofThings.hs"
    "Term.hs"
)

> "$OUTPUT"  # Очищаем выходной файл

for file in "${FILES[@]}"; do
  # Удаляем объявления модулей и внутренние импорты
  grep -v '^module ' "$file" | grep -v '^import ' >> "$OUTPUT"
  echo -e "\n\n-- === СОДЕРЖИМОЕ ФАЙЛА $file ===\n" >> "$OUTPUT"
done

# Оставляем только уникальные внешние импорты
echo -e "{-# LANGUAGE OverloadedStrings #-}\n" | cat - "$OUTPUT" > temp
mv temp "$OUTPUT"


#!/bin/bash

> imports.txt
# Находим все .hs файлы и выводим их импорты
find . -name "*.hs" -exec grep -h '^import' {} \; | \
  sort -u >> imports.txt



#!/bin/bash

# Проверяем существование исходного файла
if [ ! -f "ato.txt" ]; then
    echo "Ошибка: файл ato.txt не найден."
    exit 1
fi

# Создаем временный файл
temp_file=$(mktemp)

# Объединяем ato.txt и Combined.txt во временный файл
cat ato.txt Combined.hs > "$temp_file" && mv "$temp_file" Combined.hs

# Проверяем успешность выполнения операции
if [ $? -eq 0 ]; then
    echo "Содержимое ato.txt успешно добавлено в начало Combined.hs"
else
    echo "Ошибка при обработке файлов"
    exit 1
fi