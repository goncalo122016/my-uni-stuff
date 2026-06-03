import re

def md_to_html(text):
    lines = text.split('\n')
    result = []
    i = 0

    while i < len(lines):
        line = lines[i]

        # --- Títulos ---
        heading_match = re.match(r'^(#{1,6}) (.+)$', line)
        if heading_match:
            level = len(heading_match.group(1))
            content = heading_match.group(2)
            result.append(f'<h{level}>{content}</h{level}>')
            i += 1
            continue

        # --- Listas não ordenadas ---
        if re.match(r'^- .+', line):
            result.append('<ul>')
            while i < len(lines) and re.match(r'^- .+', lines[i]):
                item = re.match(r'^- (.+)', lines[i]).group(1)
                result.append(f' <li>{item}</li>')
                i += 1
            result.append('</ul>')
            continue

        # --- Listas ordenadas ---
        if re.match(r'^\d+\. .+', line):
            result.append('<ol>')
            while i < len(lines) and re.match(r'^\d+\. .+', lines[i]):
                item = re.match(r'^\d+\. (.+)', lines[i]).group(1)
                result.append(f' <li>{item}</li>')
                i += 1
            result.append('</ol>')
            continue

        # Linha normal (processa ênfase inline)
        result.append(line)
        i += 1

    html = '\n'.join(result)

    # --- Negrito: **texto** (antes do itálico para não conflituar) ---
    # Não pode estar colado a alfanuméricos, não pode começar/terminar com espaço
    html = re.sub(r'(?<![a-zA-Z0-9])\*\*(?! )(.+?)(?<! )\*\*(?![a-zA-Z0-9])', r'<strong>\1</strong>', html)

    # --- Itálico: *texto* ---
    html = re.sub(r'(?<![a-zA-Z0-9])\*(?! )(.+?)(?<! )\*(?![a-zA-Z0-9])', r'<em>\1</em>', html)

    return html


def count_tags(html, tag):
    # Conta tags de abertura, ex: <em> ou <li>
    pattern = f'<{tag}>'
    return len(re.findall(pattern, html))


if __name__ == '__main__':
    with open('md2.md', 'r', encoding='utf-8') as f:
        md_text = f.read()

    html = md_to_html(md_text)

    # Opcional: guardar o HTML resultante
    with open('md2.html', 'w', encoding='utf-8') as f:
        f.write(html)

    em_count = count_tags(html, 'em')
    li_count = count_tags(html, 'li')

    print(f'Número de tags <em>: {em_count}')
    print(f'Número de tags <li>: {li_count}')
