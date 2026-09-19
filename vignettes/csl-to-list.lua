 local function has_class(element, wanted)
    for _, class in ipairs(element.classes) do
      if class == wanted then
        return true
      end
    end
    return false
  end

  local function bibliography_to_list(div)
    if not has_class(div, "references") then
      return nil
    end

    local items = {}

    for _, block in ipairs(div.content) do
      if block.t == "Div" and has_class(block, "csl-entry") then
        local anchor = pandoc.Span(
          {},
          pandoc.Attr(block.identifier)
        )

        -- Preserve the reference ID so linked citations still work.
        if #block.content > 0 and
           (block.content[1].t == "Para" or block.content[1].t == "Plain") then
          block.content[1].content:insert(1, anchor)
        else
          block.content:insert(1, pandoc.Plain({anchor}))
        end

        table.insert(items, block.content)
      end
    end

    return pandoc.BulletList(items)
  end

  function Pandoc(doc)
    -- Generate citations and the bibliography before transforming it.
    doc = pandoc.utils.citeproc(doc)

    return doc:walk({
      Div = bibliography_to_list
    })
  end
