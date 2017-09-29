sm = 1293.7
meat = 93.37 + 54.09
smV = sm - meat
ppl1=4
andr1 = smV/3/ppl1
part1 = smV/3/ppl1 + meat/3/(ppl1 - 1)
ppl2 = ppl1
andr2 = smV*2/3/ppl2
part2 = smV * 2/3/ppl2 + meat * 2/3/(ppl2 - 1)
toAng = 70/3
toAndr = 85/4

smK = part1*(ppl1 - 1) + andr1 + andr2 + part2 * (ppl2 - 1)
control = abs (sm - smK) < 0.01
