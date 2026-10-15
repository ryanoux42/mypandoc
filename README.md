# 📄 MYPANDOC

## Description

**MYPANDOC** is a small re-implementation of **[Pandoc](https://pandoc.org/)**, the universal document converter, written in **Haskell**.

It reads a structured document (header + body) in **JSON** or **XML**, parses it into an internal representation, and writes it back as **JSON**, **XML** or **Markdown**.

All the parsing is done with **hand-written parser combinators** (no parsing library), built on top of `Functor`, `Applicative`, `Alternative` and `Monad` instances.

This project focuses on **functional programming**, **parser combinators**, **algebraic data types** and **document formats**.

---

## 🧩 Features

- Custom **parser combinator** library (`Parser.hs`)
- **JSON parser** (`JsonParser.hs`)
- **XML parser** with attributes and nested elements (`XMLParser.hs`)
- **Printers** for JSON, XML and Markdown (`PrintValues.hs`)
- Markdown output for **bold**, *italic*, `code`, code blocks, section titles and lists
- **Automatic input format detection** when `-e` is not given
- Output to **stdout** or to a **file**
- Error handling with exit code `84`

---

## ⚙️ Usage

### Prerequisites

- [Stack](https://docs.haskellstack.org/)

### Compilation

```bash
make
```

Other rules:
```bash
make re      # Rebuild everything
make clean   # Clean the stack build
make fclean  # Remove the binary
```

---

### Execution

```bash
./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]
```

| Option | Description |
|--------|-------------|
| `-i ifile` | Path to the file to convert |
| `-f oformat` | Output format: `xml`, `json` or `markdown` |
| `-o ofile` | Output file (stdout if omitted) |
| `-e iformat` | Input format: `xml` or `json` (auto-detected if omitted) |

#### Example:
```bash
./mypandoc -i syntaxe.json -f markdown
./mypandoc -i syntaxe.json -f xml -o syntaxe.xml
./mypandoc -i syntaxe.xml -e xml -f json
```

---

## 📝 Document Format

```json
{
    "header": {
        "title": "Syntaxe JSON",
        "author": "Fornes Leo",
        "date": "2024-01-01"
    },
    "body": {
        "paragraphe": "..."
    }
}
```

A document always contains a **header** (title, optional author and date) and a **body** made of paragraphs, sections, lists, code and formatted text.

---

## 🫂 Team

- Ryan Hercule
- Zacharie Rodde
- Jean-Garice Denoncin
