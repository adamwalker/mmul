#include <stdint.h>

void mulhilo(uint32_t x, uint32_t y, uint32_t *hi, uint32_t *lo){
    uint64_t res = (uint64_t) x * (uint64_t) y;
    *lo = res;
    *hi = res >> 32;
}

void big_mul(uint32_t *z, uint32_t *x, uint32_t *y){
    int i, j;

    for(i=0; i<128; i++){
        z[i] = 0;
    }

    for(i=0; i<64; i++){
        uint64_t carry = 0;
        for(j=0; j<64; j++){
            int idx = i + j;

            uint32_t lo, hi;
            mulhilo(x[i], y[j], &hi, &lo);

            uint64_t accum  = z[idx] + carry + lo;
            carry           = (accum >> 32) + hi;
            z[idx]          = accum;
        }
        z[i + 64] = carry;
    }
}

void big_mul_low(uint32_t *z, uint32_t *x, uint32_t *y){
    int i, j;

    for(i=0; i<64; i++){
        z[i] = 0;
    }

    for(i=0; i<64; i++){
        uint64_t carry = 0;
        for(j=0; j<64; j++){
            int idx = i + j;
            if(idx<64){
                uint32_t lo, hi;
                mulhilo(x[i], y[j], &hi, &lo);

                uint64_t accum  = z[idx] + carry + lo;
                carry           = (accum >> 32) + hi;
                z[idx]          = accum;
            }
        }
    }
}

uint32_t big_add(int count, uint32_t *res, uint32_t *x, uint32_t *y){
    int i;
    uint32_t carry = 0;
    for(i=0; i<count; i++){
        res[i]           = x[i] + y[i];
        uint32_t carry1  = res[i] < x[i];
        res[i]          += carry;
        uint32_t carry2  = res[i] < carry;
        carry            = carry1 | carry2;
    }
    return carry;
}

uint32_t big_sub(int count, uint32_t *res, uint32_t *x, uint32_t *y){
    int i;
    uint32_t carry = 0;
    for(i=0; i<count; i++){
        uint32_t temp    = x[i] - y[i];
        uint32_t carry1  = temp > x[i];
        res[i]           = temp - carry;
        uint32_t carry2  = res[i] > temp;
        carry            = carry1 | carry2;
    }
    return carry;
}

uint32_t leq(int count, uint32_t *x, uint32_t *y){
    int i;
    for(i=count - 1; i>=0; i--){
        if(x[i]>y[i]) return 0;
        if(x[i]<y[i]) return 1;
    }
    return 1;
}

void mmul(uint32_t *res, uint32_t *x, uint32_t *y, uint32_t *m, uint32_t *mprime){
    uint32_t t[128], tm[64], tmm[128], u[128];

    int i;

    big_mul(t, x, y);
    big_mul_low(tm, t, mprime);
    big_mul(tmm, tm, m);

    uint32_t ov = big_add(128, u, t, tmm);

    for(i=0; i<64; i++){
        res[i] = u[i+64];
    }

    if(ov>0 || leq(64, m, res)){
        big_sub(64, res, res, m);
    }
}

void modexp(uint32_t *res, uint32_t *base, uint32_t *exponent, uint32_t *m, uint32_t *m_prime, uint32_t *r_modp, uint32_t *r2_modp){
    int i, j;

    uint32_t base2[64];
    mmul(base2, base, r2_modp, m, m_prime);

    for(i=0; i<64; i++){
        res[i] = r_modp[i];
    }

    for(i=0; i<64; i++){
        uint32_t exp = exponent[i];
        for(j=0; j<32; j++){
            if(exp & 0x1){
                mmul(res, res, base2, m, m_prime);
            }
            mmul(base2, base2, base2, m, m_prime);
            exp >>= 1;
        }
    }

    uint32_t one[64];
    one[0] = 1;
    for(i=1; i<64; i++){
        one[i]=0;
    }
    
    mmul(res, res, one, m, m_prime);
}

