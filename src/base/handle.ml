type ('var, 'value) t = { var : 'var; mutable value : 'value option }

let var { var; _ } = var
