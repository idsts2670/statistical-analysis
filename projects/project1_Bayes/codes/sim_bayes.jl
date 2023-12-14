using CSV
using DataFrames
using Distributions
using JLD2
import Random
using LinearAlgebra
Random.seed!(1234)


function prob_W_given_Wtilde(w_tilde,p,q)
    p_ = 1-p
    q_ = 1-q
    if w_tilde==1
        return p*q/(p*q + p_*q_)
    else
        return p*q_/(p*q_ + p_*q)
    end
end

function step1(y0,y1,y_tilde,w_tilde,mu0,mu1,sigma0,sigma1,H0,N,post_prob_W0,post_prob_W1,eps_y)
    W = zeros(Int64,N)
    prob = 0.5
    for i in 1:N
        p0 = post_prob_W0[i] * pdf(Laplace(y0[i],1/eps_y),y_tilde[i])
        p1 = post_prob_W1[i] * pdf(Laplace(y1[i],1/eps_y),y_tilde[i])
        prob = p1 / (p0+p1)
        W[i] = convert(Int64,rand(Bernoulli(prob),1)[1])
    end
    return W
end


function step2_MH(y,y_tilde,mu,sigma,eps_y)
    y_ = rand(TruncatedNormal(mu,sqrt(sigma),0,1),1)[1]
    log_u = log(rand(Uniform(0,1),1)[1])
    numer = logpdf(Laplace(y_, 1/eps_y),y_tilde)
    denom = logpdf(Laplace(y, 1/eps_y),y_tilde)
    if numer - denom >= log_u
        y = y_
    end
    return y
end

function step2(y0,y1,y_tilde,w,mu0,mu1,sigma0,sigma1,eps_y,H0,N)
    Y0 = zeros(Float64,N)
    Y1 = zeros(Float64,N)
    # flg = false
    for i in 1:N
        if w[i]==0
            Y0[i] = step2_MH(y0[i],y_tilde[i],mu0[H0[i]],sigma0[H0[i]],eps_y)
            Y1[i] = rand(TruncatedNormal(mu1[H0[i]],sqrt(sigma1[H0[i]]),0,1),1)[1] 
        else
            Y0[i] = rand(TruncatedNormal(mu0[H0[i]],sqrt(sigma0[H0[i]]),0,1),1)[1]
            Y1[i] = step2_MH(y1[i],y_tilde[i],mu1[H0[i]],sigma1[H0[i]],eps_y)
        end
    end
    return Y0, Y1
end


function step3(K,N,w,ws,y0,y1,mu0,sigma0,mu1,sigma1)
    ph = zeros(Float64, K)
    H = zeros(Int64, N)
    for i in 1:N
        for j in 1:K
            ph[j] = ws[j] * pdf(TruncatedNormal(mu0[j],sqrt(sigma0[j]),0,1),y0[i]) * pdf(TruncatedNormal(mu1[j],sqrt(sigma1[j]),0,1),y1[i])
        end
        ph = ph ./ sum(ph)
        H[i] = rand(Categorical(ph),1)[1]
    end
    return H
end

function step4(K::Int64,H::Array{Int64,1},alpha::Float64)
    Vs = ones(Float64, K)
    ws = ones(Float64, K)
    v_tmp = 1.0
    a = 0
    b = 0
    for j in 1:K-1
        a = sum(H.==j)
        b = sum(H.>j)
        Vs[j] = rand(Beta(1+a, alpha+b),1)[1]
        if j > 1
            v_tmp = v_tmp * (1-Vs[j-1])
        end
        ws[j] = Vs[j] * v_tmp
    end
    ws[K] = Vs[K] * v_tmp * (1-Vs[K-1])
    return ws,Vs
end

function step5(alpha::Float64,K::Int64,Vs::Array{Float64,1},H::Array{Int64,1})::Float64
    log_u = log(rand(Uniform(0,1),1)[1])
    step = 1.0^2
    alpha_ = rand(TruncatedNormal(alpha,step,0,Inf),1)[1]
    numer = logpdf(TruncatedNormal(alpha_,step,0,Inf),alpha)[1]
    denom = logpdf(TruncatedNormal(alpha,step,0,Inf),alpha_)[1]
    a = 0
    b = 0
    for j in 1:K#K-1
        a = sum(H.==j)
        b = sum(H.>j)
        numer += logpdf(Beta(1+a, alpha_+b),Vs[j])
        denom += logpdf(Beta(1+a, alpha+b),Vs[j])
    end
    if numer - denom >= log_u
        alpha = alpha_
    end
    return alpha
end

function step6_MH_mu(N,y,sigma,mu_prior,sigma_prior)
    if N==0
        mu_ = rand(TruncatedNormal(mu_prior,sqrt(sigma_prior),0,1),1)[1]
    else
        mm = (mu_prior*sigma+sigma_prior*sum(y))/(sigma+N*sigma_prior)
        ss = sigma_prior*sigma/(sigma+N*sigma_prior)
        mu_ = rand(TruncatedNormal(mm,sqrt(ss),0,1),1)[1]
    end
    return mu_
end

function step6_MH_sigma(N,y,mu,a_prior,b_prior)
    if N==0
        sigma_ = rand(InverseGamma(a_prior,b_prior),1)[1]
    else
        a_ = a_prior + N/2
        b_ = b_prior + 0.5*sum((y .- mu).^2)
        sigma_ = rand(InverseGamma(a_,b_),1)[1]
    end
    return sigma_
end

function step6(K,w,H0,N,mu0,mu1,sigma0,sigma1,y0,y1,mu_prior,sig_prior,a_prior,b_prior)
    for k in 1:K
        n0 = sum((H0.==k))# .& (w.==0))
        n1 = sum((H0.==k))# .& (w.==1))
        y0_ = y0[(H0.==k)]# .& (w.==0)]
        y1_ = y1[(H0.==k)]# .& (w.==1)]
        sigma0[k] = step6_MH_sigma(n0,y0_,mu0[k],a_prior,b_prior)
        sigma1[k] = step6_MH_sigma(n1,y1_,mu1[k],a_prior,b_prior)
        mu0[k] = step6_MH_mu(n0,y0_,sigma0[k],mu_prior,sig_prior)
        mu1[k] = step6_MH_mu(n1,y1_,sigma1[k],mu_prior,sig_prior)
    end
    return mu0,mu1,sigma0,sigma1
end



function main(eps_y,eps_z,n_sim)
    Random.seed!(1234)

    p = 0.5
    q = exp(eps_z) / (1+exp(eps_z))


    y_tilde = DataFrame(CSV.File(string("input/data_",eps_y,"_",eps_z,"_",n_sim,".csv"),header=true)).Y
    w_tilde = DataFrame(CSV.File(string("input/data_",eps_y,"_",eps_z,"_",n_sim,".csv"),header=true)).W
    N = size(y_tilde)[1]
    post_prob_W0 = zeros(N)
    post_prob_W1 = zeros(N)
    for i in 1:N
        post_prob_W1[i] = prob_W_given_Wtilde(w_tilde[i],p,q)
        post_prob_W0[i] = 1-post_prob_W1[i]
    end

    K = 10#trunc(Int64,sqrt(N))
    # initialization
    mu0 = repeat([0.5],K)#rand(TruncatedNormal(0.5,1,0,1),K)
    mu1 = repeat([0.5],K)#rand(TruncatedNormal(0.5,1,0,1),K)
    sigma0 = repeat([0.2^2],K)#rand(InverseGamma(1,1),K)
    sigma1 = repeat([0.2^2],K)#rand(InverseGamma(1,1),K)

    alpha0 = rand(Gamma(1.0,1.0),1)[1]
    Vs0 = rand(Beta(1,alpha0),K)
    ws0 = rand(Dirichlet(ones(K)),1)[:]
    H0 = rand(Categorical(ones(K) ./ K), N)

    w = rand(Bernoulli(0.5), N)
    y0 = repeat([0.5],N)
    y1 = repeat([0.5],N)
    
    # priors
    mu_prior = 0.5
    sig_prior = 3.0^2
    a_prior = 2.0
    b_prior = 0.2^2
    
    if eps_y==5 || eps_y==1.5
        iter = 20000
        thin = 10
        burnin= 20000
    else
        iter = 100000
        thin = 10
        burnin= 50000
    end
    bayes_est = Vector{Float64}()
    # normalizing constant 
    
    for it in 1:(burnin+iter)
        # Step 2
        y0, y1 = step2(y0,y1,y_tilde,w,mu0,mu1,sigma0,sigma1,eps_y,H0,N)

        # Step 1
        w = step1(y0,y1,y_tilde,w_tilde,mu0,mu1,sigma0,sigma1,H0,N,post_prob_W0,post_prob_W1,eps_y)        
        
        # step 3
        H0 = step3(K,N,w,ws0,y0,y1,mu0,sigma0,mu1,sigma1)
        
        # step 4
        ws0,Vs0 = step4(K,H0,alpha0)
        
        # Step 5
        alpha0 = step5(alpha0,K,Vs0,H0)
        
        # Step 6
        musigma = step6(K,w,H0,N,mu0,mu1,sigma0,sigma1,y0,y1,mu_prior,sig_prior,a_prior,b_prior)
        mu0 = musigma[1]
        mu1 = musigma[2]
        sigma0 = musigma[3]
        sigma1 = musigma[4]

        
        if it > burnin && (it-burnin)%thin==0
            # res = mean(y1 - y0)
            mu0_tmp = 0.0
            mu1_tmp = 0.0
            for k in 1:K
                mu0_tmp += ws0[k] * mean(TruncatedNormal(mu0[k],sqrt(sigma0[k]),0,1))
                mu1_tmp += ws0[k] * mean(TruncatedNormal(mu1[k],sqrt(sigma1[k]),0,1))
            end
            res = mu1_tmp - mu0_tmp
            push!(bayes_est, deepcopy(res))
        end
    end 
    return bayes_est
end

if contains(ARGS[1],".")
    eps_y = parse(Float64,ARGS[1])
    eps_z = parse(Float64,ARGS[2])
else
    eps_y = parse(Int64,ARGS[1])
    eps_z = parse(Int64,ARGS[2])
end
n_sim = parse(Int64,ARGS[3])
s = string(eps_y,"_",eps_z,"_",n_sim)

results = main(eps_y,eps_z,n_sim)
save_object(string("output/results_joint_",s,".jld2"), results)
