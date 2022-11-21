module Methods
using Printf

function bisection(f::Function, l, r; 𝞮=1.0e-6, maxiter=16, print=false)
    iter = 0;
    𝞮a = one(Float64)
    m_old = zero(Float64)
    lval, rval = f(l), f(r)
    while true
        iter = iter + 1
        m = (l + r)/2
        mval = f(m)
        if m != 0; 𝞮a = abs(m - m_old)/abs(m) end
        if print; @printf("Iter: %d, x = %0.16f, 𝞮a = %0.2e, f(x) = %0.2e\n", iter, m, 𝞮a, f(m)); end
        check = lval * mval
        if check < 0
            r, rval = m, mval 
        else 
            l, lval = m, mval 
        end
        if 𝞮a <= 𝞮 || iter >= maxiter
            return m, iter
        end
        m_old = m
    end
end

function iterative(x, e, M; 𝞮=1.0e-6, maxiter=16, print=false)
    iter = 0;
    f(x) = x - e*sin(x) - M
    phi(xn) = e*sin(xn) + M
    𝞮a = one(x)
    while true
        iter = iter + 1
        x_old = x
        x = phi(x)
        if x != 0; 𝞮a = abs(x - x_old)/abs(x); end
        if print; @printf("Iter: %d, x = %0.16f, 𝞮a = %0.2e, f(x) = %0.2e\n", iter, x, 𝞮a, f(x)); end
        if 𝞮a <= 𝞮 || iter >= maxiter
            break
        end
    end
    return x, iter
end

function newton(f::Function, 𝜹f::Function, x; 𝞮=1.0e-6, maxiter=16, print=false)
    iter = 0
    xn = x
    𝞮a = one(xn)
    while true
        iter = iter + 1
        xnold = xn
        fval, 𝜹fval = f(xn), 𝜹f(xn)
        xn = xnold - fval/𝜹fval
        if xn != 0.0; 𝞮a = abs((xn - xnold)/xn); end;
        if print; @printf("iter: %d : xn=%.25f 𝞮a=%0.2e f(x)=%0.2e\n", iter, xn, 𝞮a, f(xn)); end;
        if 𝞮a <= 𝞮 || iter >= maxiter
            break
        end
    end
    return xn, iter
end

end
