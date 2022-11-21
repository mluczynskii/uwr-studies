include("./methods.jl")
using .Methods
using Printf

e = 0.0167086
T = 365.25636
t = 10
M = 2*pi*t/T
@show M
f(x) = x - e*sin(x) - M
df(x) = 1 - e*cos(x)
x0 = M + e*sin(M)/(1-sin(M+e)+sin(M))
Methods.newton(f, df, x0; 𝞮=1.0e-16, maxiter=10, print=false)
Methods.bisection(f, M, M + e; 𝞮=1.0e-16, maxiter=40, print=false)
Methods.iterative(0, e, M; 𝞮=1.0e-16, maxiter=16, print=false)

function solve_for_M(M, e)
    f(x) = x - e*sin(x) - M 
    df(x) = 1 - e*cos(x)
    x0 = M + e*sin(M)/(1-sin(M+e)+sin(M))
    x, iter = Methods.newton(f, df, x0; 𝞮=1.0e-16, maxiter=100, print=false)
    #x, iter = Methods.iterative(x0, e, M; 𝞮=1.0e-16, maxiter=100, print=false)
    #x, iter = Methods.bisection(f, M, M + e; 𝞮=1.0e-16, maxiter=100, print=false)
    return x, iter
end

# Funkcja licząca współrzędne na orbicie
function get_data(filename, e, a)
    file = open(filename, "w")
    for M in 0:0.01:(2*pi)
        x = solve_for_M(M, e)[1]
        l = a * (cos(x) - e)
        r = a * sqrt(1 - e^2) * sin(x)
        line = string(l) * " " * string(r) * "\n"
        write(file, line)
    end
    close(file)
end

names = ["Ziemia"]
E = [0.0167086]
A = [149598023]
for i in 1:1
    get_data(names[i] * ".txt", E[i], A[i])
end

e = 0.9
num_of_iter = 0
num_of_tests = 0
for M in 0:0.1:(2*pi)
    iter = solve_for_M(M, e)[2]
    global num_of_iter = num_of_iter + iter
    global num_of_tests = num_of_tests + 1
end
@printf("Number of iterations needed: %f\n", num_of_iter/num_of_tests)