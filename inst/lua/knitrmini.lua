-- knitrmini's pandoc filter: the prose of an R Markdown document as the LaTeX one would write by
-- hand. Math as $...$; figures and tables as knitrmini writes those of chunks (a figure
-- environment with \includegraphics; a booktabs tabular, in a table environment if it has a
-- caption); unnumbered headings without \addcontentsline; labels from pandoc's {#x} and bookdown's
-- (\#x) in captions.

local function raw(s)
  return pandoc.RawInline("latex", s)
end

-- Inlines as LaTeX (for pieces this filter writes itself).
local function tex(inlines)
  local out = pandoc.write(pandoc.Pandoc({ pandoc.Plain(inlines) }), "latex", { wrap_text = "wrap-preserve" })
  return (out:gsub("\n+$", ""))
end

-- A caption's LaTeX and the label written in it ((\#x), {#x}), if any.
local function caption_and_label(inlines, id)
  local text = tex(inlines)
  local label = id ~= "" and id or nil
  text = text:gsub("%s*\\{\\#([%w:_%.%-]+)\\}%s*$", function(l) label = label or l; return "" end)
  text = text:gsub("^%s*%(\\#([%w:_%.%-]+)%)%s*", function(l) label = label or l; return "" end)
  text = text:gsub("%s*%(\\#([%w:_%.%-]+)%)%s*$", function(l) label = label or l; return "" end)
  return text, label
end

-- A size as LaTeX: 50% is half the line width.
local function size(v)
  local pct = v:match("^([%d.]+)%%$")
  return pct and (tostring(tonumber(pct) / 100) .. "\\linewidth") or v
end

local function graphics(img)
  local opts = {}
  for _, key in ipairs({ "width", "height" }) do
    if img.attributes[key] then
      opts[#opts + 1] = key .. "=" .. size(img.attributes[key])
    end
  end
  return "\\includegraphics" .. (#opts > 0 and ("[" .. table.concat(opts, ",") .. "]") or "") .. "{" .. img.src .. "}"
end

function Math(m)
  if m.mathtype == "InlineMath" then
    return raw("$" .. m.text .. "$")
  end
end

function Image(img) -- (one in the text, not a figure)
  return raw(graphics(img))
end

function Figure(fig)
  local img
  pandoc.walk_block(fig, { Image = function(i) img = img or i end })
  if not img then
    return nil
  end
  local caption, label = caption_and_label(pandoc.utils.blocks_to_inlines(fig.caption.long), fig.identifier ~= "" and fig.identifier or img.identifier)
  local lines = { "\\begin{figure}", "\\centering", graphics(img) }
  if caption ~= "" then
    lines[#lines + 1] = "\\caption{" .. caption .. "}" .. (label and ("\\label{" .. label .. "}") or "")
  elseif label then
    lines[#lines + 1] = "\\label{" .. label .. "}"
  end
  lines[#lines + 1] = "\\end{figure}"
  return pandoc.RawBlock("latex", table.concat(lines, "\n"))
end

local ALIGN = { AlignLeft = "l", AlignRight = "r", AlignCenter = "c", AlignDefault = "l" }

function Table(t)
  local spec = {}
  for _, c in ipairs(t.colspecs) do
    spec[#spec + 1] = ALIGN[c[1]] or "l"
  end
  local function row(r)
    local cells = {}
    for _, cell in ipairs(r.cells) do
      cells[#cells + 1] = tex(pandoc.utils.blocks_to_inlines(cell.contents))
    end
    return table.concat(cells, " & ") .. " \\\\"
  end
  local lines = { "\\begin{tabular}{" .. table.concat(spec) .. "}", "\\toprule" }
  for _, r in ipairs(t.head.rows) do
    lines[#lines + 1] = row(r)
  end
  if #t.head.rows > 0 then
    lines[#lines + 1] = "\\midrule"
  end
  for _, body in ipairs(t.bodies) do
    for _, r in ipairs(body.body) do
      lines[#lines + 1] = row(r)
    end
  end
  lines[#lines + 1] = "\\bottomrule"
  lines[#lines + 1] = "\\end{tabular}"
  local caption, label = caption_and_label(pandoc.utils.blocks_to_inlines(t.caption.long), t.identifier or "")
  if caption ~= "" or label then
    table.insert(lines, 1, "\\begin{table}")
    table.insert(lines, 2, "\\centering")
    if caption ~= "" then
      table.insert(lines, 3, "\\caption{" .. caption .. "}" .. (label and ("\\label{" .. label .. "}") or ""))
    end
    lines[#lines + 1] = "\\end{table}"
  end
  return pandoc.RawBlock("latex", table.concat(lines, "\n"))
end

local SECTION = { "section", "subsection", "subsubsection", "paragraph", "subparagraph" }

function Header(h)
  if h.classes:includes("unnumbered") then
    local cmd = SECTION[h.level] or "subparagraph"
    local label = h.identifier ~= "" and ("\\label{" .. h.identifier .. "}") or ""
    return pandoc.RawBlock("latex", "\\" .. cmd .. "*{" .. tex(h.content) .. "}" .. label)
  end
end
