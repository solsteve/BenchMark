#!/usr/bin/python3
# use:
#
# gcc -march=native -E -v - </dev/null 2>&1 | grep cc1
#
#

def incr( a, b ):
    c = []
    n = len(a)
    for i in range(n):
        c.append( a[i] + b[i] )
    return c

def bin2dec( b ):
    return 4*b[0] + 2*b[1] + b[2]


data={
    'PDP':{'id':[1,0,0],'make':'Intel','class':'Xeon','model':'E5540','freq':'2.53GHz','thread':'8', 'OPT':'-m_arch=nehalem -m_mmx -m_no-3dnow -m_sse -m_sse2 -m_sse3 -m_ssse3 -m_no-sse4a -m_cx16 -m_sahf -m_no-movbe -m_no-aes -m_no-sha -m_no-pclmul -m_popcnt -m_no-abm -m_no-lwp -m_no-fma -m_no-fma4 -m_no-xop -m_no-bmi -m_no-sgx -m_no-bmi2 -m_no-pconfig -m_no-wbnoinvd -m_no-tbm -m_no-avx -m_no-avx2 -m_sse4.2 -m_sse4.1 -m_no-lzcnt -m_no-rtm -m_no-hle -m_no-rdrnd -m_no-f16c -m_no-fsgsbase -m_no-rdseed -m_no-prfchw -m_no-adx -m_fxsr -m_no-xsave -m_no-xsaveopt -m_no-avx512f -m_no-avx512er -m_no-avx512cd -m_no-avx512pf -m_no-prefetchwt1 -m_no-clflushopt -m_no-xsavec -m_no-xsaves -m_no-avx512dq -m_no-avx512bw -m_no-avx512vl -m_no-avx512ifma -m_no-avx512vbmi -m_no-avx5124fmaps -m_no-avx5124vnniw -m_no-clwb -m_no-mwaitx -m_no-clzero -m_no-pku -m_no-rdpid -m_no-gfni -m_no-shstk -m_no-avx512vbmi2 -m_no-avx512vnni -m_no-vaes -m_no-vpclmulqdq -m_no-avx512bitalg -m_no-movdiri -m_no-movdir64b --param_l1-cache-size=32 --param_l1-cache-line-size=64 --param_l2-cache-size=8192 -m_tune=nehalem'},
    'VAX':{'id':[0,1,0],'make':'Intel(R)','class':'Core','model':'i7-3770K','freq':'3.50GHz','thread':'8', 'OPT':'-m_arch=znver1 -m_mmx -m_no-3dnow -m_sse -m_sse2 -m_sse3 -m_ssse3 -m_sse4a -m_cx16 -m_sahf -m_movbe -m_aes -m_sha -m_pclmul -m_popcnt -m_abm -m_no-lwp -m_fma -m_no-fma4 -m_no-xop -m_bmi -m_no-sgx -m_bmi2 -m_no-tbm -m_avx -m_avx2 -m_sse4.2 -m_sse4.1 -m_lzcnt -m_no-rtm -m_no-hle -m_rdrnd -m_f16c -m_fsgsbase -m_rdseed -m_prfchw -m_adx -m_fxsr -m_xsave -m_xsaveopt -m_no-avx512f -m_no-avx512er -m_no-avx512cd -m_no-avx512pf -m_no-prefetchwt1 -m_clflushopt -m_xsavec -m_xsaves -m_no-avx512dq -m_no-avx512bw -m_no-avx512vl -m_no-avx512ifma -m_no-avx512vbmi -m_no-avx5124fmaps -m_no-avx5124vnniw -m_no-clwb -m_mwaitx -m_clzero -m_no-pku -m_no-rdpid --param_l1-cache-size=32 --param_l1-cache-line-size=64 --param_l2-cache-size=512 -m_tune=znver1 -f_stack-protector-strong -W_format -W_format-security'},
'CDC':{'id':[0,0,1],'make':'AMD','class':'Ryzen','model':'Threadripper 1950X','freq':'4.0','thread':'32', 'OPT':'-m_arch=ivybridge -m_mmx -m_no-3dnow -m_sse -m_sse2 -m_sse3 -m_ssse3 -m_no-sse4a -m_cx16 -m_sahf -m_no-movbe -m_aes -m_no-sha -m_pclmul -m_popcnt -m_no-abm -m_no-lwp -m_no-fma -m_no-fma4 -m_no-xop -m_no-bmi -m_no-sgx -m_no-bmi2 -m_no-tbm -m_avx -m_no-avx2 -m_sse4.2 -m_sse4.1 -m_no-lzcnt -m_no-rtm -m_no-hle -m_rdrnd -m_f16c -m_fsgsbase -m_no-rdseed -m_no-prfchw -m_no-adx -m_fxsr -m_xsave -m_xsaveopt -m_no-avx512f -m_no-avx512er -m_no-avx512cd -m_no-avx512pf -m_no-prefetchwt1 -m_no-clflushopt -m_no-xsavec -m_no-xsaves -m_no-avx512dq -m_no-avx512bw -m_no-avx512vl -m_no-avx512ifma -m_no-avx512vbmi -m_no-avx5124fmaps -m_no-avx5124vnniw -m_no-clwb -m_no-mwaitx -m_no-clzero -m_no-pku -m_no-rdpid --param_l1-cache-size=32 --param_l1-cache-line-size=64 --param_l2-cache-size=8192 -m_tune=ivybridge -f_stack-protector-strong -W_format -W_format-security'}
}

flags={}
for x in data:
    op = data[x]['OPT'].split()
    for o in op:
        k = o.split('_')
        flags[k[0]] = {}

for x in data:
    op = data[x]['OPT'].split()
    for o in op:
        prt = o.split('_')
        key = prt[0]
        if ( 1 < len(prt) ):
            val = '_'.join(prt[1:])
        else:
            val = 'bolean'
        flags[key][val] = [0,0,0]


for x in data:
    op    = data[x]['OPT'].split()
    addto = data[x]['id']
    for o in op:
        prt = o.split('_')
        key = prt[0]
        if ( 1 < len(prt) ):
            val = '_'.join(prt[1:])
        else:
            val = 'bolean'

        test = flags[key][val]
        flags[key][val] = incr( test, addto )












for ii in (7,6,5,3,4,2,1):    
    for key in flags:
        group = flags[key]
        for op in group:
            id = bin2dec(group[op])
            if ( ii == id ):
                if ( '--' == key[:2] ):
                    print( '%d >> %s %s' % (id,key,op,) )
                else:
                    print( '%d >> %s%s' % (id, key,op,) )
    print('')

