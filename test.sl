isSafe str lists := lists.any(\l -> l.isBlocked(str))

isBlocked str := do
  return @list.any(\term -> strpos(term, str) !== false)
  end

isUnsafe str := !isSafe(str)
