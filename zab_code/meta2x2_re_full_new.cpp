#include <TMB.hpp>  // Links in the TMB libraries

template<class Type>
  Type lchoose(Type n, Type k){
    Type ch=0;
    ch += lgamma(n+1)-lgamma(k+1)-lgamma(n-k+1);
    return ch;
  }

template<class Type>
  Type decchyper(Type psi_i, Type m1, Type y1, Type m0, Type z){
    using CppAD::Integer;
    Type opp=0;
    Type ned=0;
    Type dens;
    //vector<Type> minv(2);
    //vector<Type> maxv(2);
    //Type ymin=0;
    //Type ymax=0;

    //minv[0]=0;
    //minv[1]=z-m0;
    //maxv[0]=z;
    //maxv[1]=m1;

    opp += lchoose(m1,y1)+lchoose(m0,z-y1)+psi_i*y1;
    //ymin=0; //max(minv);
    //ymax=z; //min(maxv);
    for (int j=0; j <= z; j++){
      Type u=j;
      ned += exp(lchoose(m1,u))*exp(lchoose(m0,z-u))*exp(psi_i*u);
    }
    dens=opp-log(ned);
    return dens;
  }
template<class Type>
  Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(vm1);  // vector of number of obs in treatment group
  DATA_VECTOR(vy1);  // vector of number of success in treatment group
  DATA_VECTOR(vm0);  // vector of number of obs in control group
  DATA_VECTOR(vz);  // vector of total number of successes 
  // parameters
  PARAMETER_VECTOR(psis);  // Random effect
  PARAMETER(psi);  // Parameter value transmitted from R
  PARAMETER(tau2);
  
  int k=vm1.size();
  Type f = 0;  // Declare the "objective function" (neg. log. likelihood)
  for (int j=0; j < k; j++){
    Type m1=vm1(j);
    Type y1=vy1(j);
    Type m0=vm0(j);
    Type z=vz(j);
    f -= decchyper(psis(j),m1,y1,m0,z) + dnorm(psis(j),psi,sqrt(tau2),true);
  }
  return f;
}
