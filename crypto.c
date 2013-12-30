#include <stdint.h>
#include <stdio.h>

void mulhilo(uint32_t x, uint32_t y, uint32_t *hi, uint32_t *lo){
    uint64_t res = (uint64_t) x * (uint64_t) y;
    *lo = res;
    *hi = res >> 32;
}

//TODO, what about the carry at the end
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
        //TODO
        //if(i!=63) z[i + 65] = carry >> 32;
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
    uint32_t t[128], tm[128], tmm[128], u[128];

    int i;

    big_mul(t, x, y);
    big_mul(tm, t, mprime);
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
    //convert to montgomery domain. TODO: this isnt necessary for diffie hellman
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

uint32_t m[64] = {
    0x0C10E64F, 0x0AC4DFFE, 0x4E71B81C, 0xCF9DE538,                               
    0xFFA31F71, 0x7EF363E2, 0x6B8E75B9, 0xE3FB73C1, 0x4BA80A29, 0xC9B53DCF, 
    0x16E79763, 0x23F10B0E, 0x13042E9B, 0xC52172E4, 0xC928B2B9, 0xBE60E69C, 
    0xB9E587E8, 0x80CD86A1, 0x98C641A4, 0x315D75E1, 0x44328387, 0xCDF93ACC, 
    0xDC0A486D, 0x15987D9A, 0x1FD5A074, 0x7310F712, 0xDE31EFDC, 0x278273C7, 
    0x415D9330, 0x1602E714, 0xBC8985DB, 0x81286130, 0x70918836, 0xB3BF8A31, 
    0xB9C49708, 0x6A00E0A0, 0x8BBC27BE, 0xC6BA0B2C, 0xED34DBF6, 0xC9F98D11, 
    0xB6C12207, 0x7AD5B7D0, 0x55B7394B, 0xD91E8FEF, 0xEFDA4DF8, 0x9037C9ED, 
    0xAD6AC212, 0x6D3F8152, 0x1274A0A6, 0x1DE6B85A, 0x309C180E, 0xEB3D688A, 
    0x7BA1DF15, 0xAF9A3C40, 0xF95A56DB, 0xE6FA141D, 0xB61D0A75, 0xB54B1597, 
    0x683B9FD1, 0xA20D64E5, 0x9559C51F, 0xD660FAA7, 0x9123A9D0, 0xAD107E1E, 
};

uint32_t r2_modp[64] = {
    0x2ec2bd6b, 0x3bb48b69, 0x43836529, 0x44c734b8, 0xd8c671aa, 0x2b522529,
    0xfe7d0a78, 0x1b1c6bbc, 0xcf4ce640, 0xf35542d9, 0x7f8c859c, 0x3adb125b,
    0xe76dcc8f, 0x2fe74ebd, 0xf00edc9a, 0x782b7d91, 0xbf746299, 0x983d71a4,
    0xe0b8f780, 0xfcd65f0e, 0xa986611d, 0xe59df3e,  0x36bb3cfa, 0xeda913b2,
    0x25afae57, 0x96f6367,  0x4f1f69d4, 0x1f0dd87c, 0xf6c223c3, 0xf326176d,
    0x60c9e8a5, 0x342ea7d4, 0x703f2f11, 0x613c1887, 0x6054024f, 0x4d0b5ebc,
    0xd3b6694a, 0x4329764c, 0xd84e86c1, 0x8a9b8c76, 0xedc76b42, 0x36858fc9,
    0x647c0452, 0x3f462af2, 0x58ecb8d4, 0x35e47d0c, 0xeb59e616, 0x8c873d52,
    0x3aeab8d,  0x66b25557, 0xcd278a9d, 0x3bf43296, 0x6d86b7a2, 0x7d04e4c9,
    0x927ba630, 0x32a3d5f2, 0x16008064, 0xd552f36a, 0x59e84817, 0xf94dcf8d,
    0xc149dead, 0xcbaf2cd,  0xde5c8027, 0x9cd7f320
};

uint32_t r_modp[64] = {
    0xf3ef19b1, 0xf53b2001, 0xb18e47e3, 0x30621ac7, 0x5ce08e,   0x810c9c1d,
    0x94718a46, 0x1c048c3e, 0xb457f5d6, 0x364ac230, 0xe918689c, 0xdc0ef4f1,
    0xecfbd164, 0x3ade8d1b, 0x36d74d46, 0x419f1963, 0x461a7817, 0x7f32795e,
    0x6739be5b, 0xcea28a1e, 0xbbcd7c78, 0x3206c533, 0x23f5b792, 0xea678265,
    0xe02a5f8b, 0x8cef08ed, 0x21ce1023, 0xd87d8c38, 0xbea26ccf, 0xe9fd18eb,
    0x43767a24, 0x7ed79ecf, 0x8f6e77c9, 0x4c4075ce, 0x463b68f7, 0x95ff1f5f,
    0x7443d841, 0x3945f4d3, 0x12cb2409, 0x360672ee, 0x493eddf8, 0x852a482f,
    0xaa48c6b4, 0x26e17010, 0x1025b207, 0x6fc83612, 0x52953ded, 0x92c07ead,
    0xed8b5f59, 0xe21947a5, 0xcf63e7f1, 0x14c29775, 0x845e20ea, 0x5065c3bf,
    0x6a5a924,  0x1905ebe2, 0x49e2f58a, 0x4ab4ea68, 0x97c4602e, 0x5df29b1a,
    0x6aa63ae0, 0x299f0558, 0x6edc562f, 0x52ef81e1
};

uint32_t m_prime[64] = {
    0x52af8f51, 0xcab19312, 0x4d72ab2c, 0xab0d8d3,  0x1c310b8d, 0xed456389,
    0xd15055c8, 0x6b94f163, 0xd2eda983, 0x816337bd, 0x63733eee, 0xea09e89b,
    0x834448a2, 0x11786fdf, 0x969fb701, 0x315c7a9,  0x3a05e816, 0xd2b3269a,
    0x815aa6ee, 0x84679f17, 0xd2e50962, 0xd85713af, 0x4a319cd3, 0x7a9b0f29,
    0x1eeadfdc, 0xc2dc3cdf, 0xff866ce2, 0x9101214b, 0xe9a60d60, 0xf64c2321,
    0x6c62ccaa, 0x79517d70, 0xe2c2197a, 0x2b77d945, 0x94671797, 0x6f012989,
    0x3a190ce7, 0x1b0adb10, 0xe756d200, 0xaa8c6626, 0x7f49466,  0xba73edfa,
    0xbd0f7ca5, 0xffba8d95, 0x1edd58fd, 0x3bd88b24, 0x7b8d283,  0x73ba075b,
    0xd0499cef, 0x91573f97, 0x37384faa, 0xa6d6b6d5, 0x781722cb, 0xd0b70cce,
    0xe4167c3b, 0x7c558936, 0x9e6baf8,  0xfadcea61, 0xbce8ed35, 0xb482166c,
    0x7e0b8f8e, 0xa78046de, 0x82086f,   0x6348474e
};

//also the prime
uint32_t x[64] = {
    0x1E1A1597, 0xDB094AE9, 0xD7EF09CA, 0x693877FA,                                   
    0x6E11715F, 0x6116D227, 0xC198AF12, 0xA4B54330, 0xD7014103, 0x75F26375,     
    0x54E710C3, 0xC3A3960A, 0xBD0BE621, 0xDED4010A, 0x89962856, 0xC0B857F6,     
    0x71506026, 0xB3CA3F79, 0xE6B486F6, 0x1CCACB83, 0x14056425, 0x67E144E5,     
    0xA41825D9, 0xF6A167B5, 0x96524D8E, 0x3AD83477, 0x51BFA4AB, 0xF13C6D9A,     
    0x35488A0E, 0x2D525267, 0xCAA6B790, 0xB63ACAE1, 0x81B23F76, 0x4FDB70C5,     
    0x12307F5C, 0xBC39A0BF, 0xB1E59BB8, 0xB941F54E, 0xD45F9088, 0x6C5BFC11,     
    0x4275BF7B, 0x22E0B1EF, 0x5B4758C0, 0x91F9E672, 0x6BCF67ED, 0x5A8A9D30,     
    0x97517ABD, 0x209E0C64, 0x830E9A7C, 0x3BF4296D, 0x34096FAA, 0x16C3D911,     
    0x61B2AA30, 0xFAF7DF45, 0xD61957D4, 0xE00DF8F1, 0x435E3B00, 0x5D2CEED4,     
    0x660DD0F2, 0x8CEEF608, 0x65195999, 0xFFBBD19C, 0xB4B6663C, 0x87A8E61D
};

//also the generator
uint32_t y[64] = {
     0x6CC41659, 0x664B4C0F, 0xEF98C582, 0x5E2327CF, 
     0xD4795451, 0xD647D148, 0x90F00EF8, 0x2F630784, 0x1DB246C3, 0x184B523D,  
     0xCDC67EB6, 0xC7891428, 0x0DF92B52, 0x7FD02837, 0x64E0EC37, 0xB3353BBB,  
     0x57CD0915, 0xECD06E15, 0xDF016199, 0xB7D2BBD2, 0x052588B9, 0xC8484B1E,  
     0x13D3FE14, 0xDB2A3B73, 0xD182EA0A, 0xD052B985, 0xE83B9C80, 0xA4BD1BFF,  
     0xFB3F2E55, 0xDFC967C1, 0x767164E1, 0xB5045AF2, 0x6F2F9193, 0x1D14348F,  
     0x428EBC83, 0x64E67982, 0x82D6ED38, 0x8AC376D2, 0xAAB8A862, 0x777DE62A,  
     0xE9EC144B, 0xDDF463E5, 0xC77A57F2, 0x0196F931, 0x41000A65, 0xA55AE313,  
     0xC28CBB18, 0x901228F8, 0x7E8C6F62, 0xBC3773BF, 0x0C6B47B1, 0xBE3A6C1B,  
     0xAC0BB555, 0xFF4FED4A, 0x77BE463F, 0x10DBC150, 0x1A0BA125, 0x07F4793A,  
     0x21EF2054, 0x4CA7B18F, 0x60EDBD48, 0x2E775066, 0x73134D0B, 0x3FB32C9B
};

int main(){
    /*
    uint32_t res[64], res2[64], x_m[64], y_m[64], one[64];
    int i;

    //Convert to montgomery domain
    mmul(x_m, x, r2_modp, m, m_prime);
    mmul(y_m, y, r2_modp, m, m_prime);

    //Perform the multiplication
    mmul(res, x_m, y_m, m, m_prime);

    //Convert result back
    one[0] = 1;
    for(i=1; i<64; i++){
        one[i]=0;
    }
    
    mmul(res2, res, one, m, m_prime);

    for(i=0; i<64; i++){
        printf("%x, ", res2[i]);
    }
    */

    uint32_t res[64];
    modexp(res, x, y, m, m_prime, r_modp, r2_modp);

    int i;
    for(i=0; i<64; i++){
        printf("%x, ", res[i]);
    }
}

