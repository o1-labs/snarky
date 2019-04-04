module T = struct
  type ('request, 'compute) provider =
    | Request of 'request
    | Compute of 'compute
    | Both of 'request * 'compute
end

include T

type ('request, 'compute) t = ('request, 'compute) provider
