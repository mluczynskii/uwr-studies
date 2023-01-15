import Pkg 
Pkg.add("Distributions")
using Distributions
using Printf

# vs - wartości funkcji w punktach z xs
function quotients(vs :: Vector{<:Real}, xs :: Vector{<:Real})
    n = length(xs)
    b = zeros(n)
    for k in 1:n
        b[k] = vs[k]
    end
    for j in 2:n
        for k in reverse(j:n)
            b[k] = (b[k] - b[k-1])/(xs[k] - xs[k-j+1]) 
        end
    end
    return b
end

# xs, ys - mnożone wielomiany
function poly_mult(xs :: Vector{<:Real}, ys :: Vector{<:Real})
    n = length(xs) + length(ys) - 1
    ws = zeros(n)
    for i in 1:length(xs)
        for j in 1:length(ys)
            ws[i+j-1] = ws[i+j-1] + xs[i] * ys[j]
        end
    end
    return ws 
end

# xs, ys - dodawane wielomiany
function poly_add(xs :: Vector{<:Real}, ys :: Vector{<:Real})
    n = max(length(xs), length(ys))
    ws = zeros(n)
    for i in 1:n 
        if i > length(xs)
            ws[i] = ys[i]
        elseif i > length(ys)
            ws[i] = xs[i]
        else
            ws[i] = xs[i] + ys[i]
        end 
    end
    while ws[end] == 0; pop!(ws); end
    return ws
end

# xs, vs - punkty węzłowe wraz z ich wartościami
function newton_interpolate(vs :: Vector{<:Real}, xs :: Vector{<:Real})
    n = length(xs)
    ws = zeros(n)
    b = quotients(vs, xs)
    ws[1] = b[1]
    V = [Vector{Float64}(undef, n) for _ in 1:n]
    V[1] = [1]
    for k in 2:n 
        V[k] = poly_mult(V[k-1], [-xs[k-1], 1])
        ws = poly_add(ws, b[k] * V[k])
    end
    while ws[end] == 0; pop!(ws); end 
    return ws
end

# xs - współczynniki wielomianu uporządkowane od najmniejszej potęgi
# x  - punkt dla którego liczymy wartość wielomianu
function horner(xs :: Vector{<:Real}, x)
    n = length(xs)
    b = xs[n]
    for k in reverse(1:(n-1))
        b = xs[k] + x * b
    end
    return b
end

# xs     - współczynniki wielomianu uporządkowane od najmniejszej potęgi
# [a, b] - granice całkowania (b > a)
function poly_integrate(xs :: Vector{<:Real}, a, b)
    n = length(xs)
    ws = zeros(n+1)
    for k in 1:n 
        ws[k+1] = xs[k] / k
    end
    return horner(ws, b) - horner(ws, a)  
end

function generate_points(a, b, n)
    xs = zeros(n)
    xs[1] = a; xs[n] = b;
    h = (b - a)/n
    for k in 1:(n-2)
        l = a + k*h; r = a + (k+1)*h 
        xs[k+1] = rand(Uniform(l, r))
    end 
    return xs
end

function generate_values(xs, f :: Function)
    n = length(xs)
    vs = zeros(n)
    for i in 1:n 
        x = xs[i]
        vs[i] = f(x)
    end
    return vs 
end

function print_poly(xs)
    n = length(xs)
    @printf("%0.4f", xs[1])
    for i in 2:n 
        if xs[i] >= 0; @printf("+"); end
        @printf("%0.4f*x^%d", xs[i], i-1)
    end
    @printf("\n")
end

# Pole trapezu
function P(x1, x2, v1, v2)
    p = (abs(v1) + abs(v2))*(x2 - x1)/2
    if v1 < 0 || v2 < 0; return -p; else return p; end
end

# Metoda trapezów
function trap(xs, vs) 
    res = zero(Float64)
    n = length(xs)
    for k in 1:(n-1) 
        x1 = xs[k]; x2 = xs[k+1]
        v1 = vs[k]; v2 = vs[k+1]
        if v1*v2 < 0
            d = x2 - x1 
            e = d * abs(v1)/(abs(v1) + abs(v2))
            res = res + P(x1, x1+e, v1, 0) + P(x1+e, x2, 0, v2)
        else 
            res = res + P(x1, x2, v1, v2)
        end
    end
    return res
end

poly(x) = 2x^3 - x^2 + 3x - 7
runge(x) = 1/(1+25x^2)
sinus(x) = sin(x)