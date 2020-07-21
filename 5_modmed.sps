* Encoding: UTF-8.

get file " **set file location** ".


*moderated mediation effect of C from ch to prom_jc.

process
y = posi_jc / m = WE / x = ch_str / w  = C /
cov = sex, age, edu, tenure /
boot = 5000 /
conf = 99.5 /
decimals = F10.3 /
jn = 1 /
model  = 7.


*moderated mediation effect of N from hi to prev_jc

process
y = nega_jc / m = WE / x = hi_str / w  = N /
cov = sex, age, edu, tenure /
boot = 5000 /
conf = 99.5 /
decimals = F10.3 /
jn = 1 /
model  = 7