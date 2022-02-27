      WtInput.new({type: "text", onChange: (\e -> props.onChange(props.id, e.target.value))})

    numrows.times(\i -> generated.push(Expense.new({id: @props.id + i, onChange: @props.onChange})))


@@rows += 1
in js should call setState
