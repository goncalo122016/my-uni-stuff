#!/bin/bash

# Java formatter script - formats Java files with style rules

if [ $# -eq 0 ]; then
    echo "Usage: $0 <directory>"
    echo "Example: $0 app/src/main/java"
    exit 1
fi

TARGET_DIR=$1

if [ ! -d "$TARGET_DIR" ]; then
    echo "Error: Directory '$TARGET_DIR' not found"
    exit 1
fi

echo "📝 Formatting Java files in: $TARGET_DIR"
echo ""

# Find all Java files and format them
COUNT=0
find "$TARGET_DIR" -name "*.java" -type f | while read file; do
    COUNT=$((COUNT + 1))
    echo "  ▸ $file"

    # Remove trailing whitespace
    sed -i 's/[[:space:]]*$//' "$file" 2>/dev/null || true

    # Ensure Unix line endings
    sed -i 's/\r$//' "$file" 2>/dev/null || true
done

echo ""
echo "✓ Formatting complete"
