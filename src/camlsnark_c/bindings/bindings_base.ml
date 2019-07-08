open Core_kernel

let with_prefix prefix s = sprintf "%s_%s" prefix s

module type Prefix_intf = sig
  val prefix : string
end

module type Foreign_types = sig
  type 'a return

  type 'a result
end

module type Foreign_intf = sig
  type t

  val typ : t Ctypes.typ
end
