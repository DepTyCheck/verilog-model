-- Seed: 6038550840841163055,9372850727389630639



entity fegvg is
  port (zweeelk : inout time);
end fegvg;



architecture yqytwphhy of fegvg is
  
begin
  
end yqytwphhy;



entity xrcjoida is
  port (ajrpxeq : out time; j : in time; iazkph : buffer time; valbupgx : in time);
end xrcjoida;



architecture fvi of xrcjoida is
  
begin
  
end fvi;



entity nve is
  port (givnnzshd : buffer bit; xxgvsbxg : inout real; pcnanhfv : out time; vd : buffer real);
end nve;



architecture urlw of nve is
  
begin
  ptjxu : entity work.fegvg
    port map (zweeelk => pcnanhfv);
end urlw;



entity wz is
  port (vyqgcio : inout time; mkfravkffu : in time; qpmbyxf : linkage time; rueftjpgz : linkage real);
end wz;



architecture hgh of wz is
  signal xengybhe : time;
  signal yaaswfrfkz : time;
  signal kfccxwgg : time;
  signal tglwojg : time;
begin
  mblirdg : entity work.xrcjoida
    port map (ajrpxeq => tglwojg, j => kfccxwgg, iazkph => kfccxwgg, valbupgx => yaaswfrfkz);
  djgw : entity work.fegvg
    port map (zweeelk => vyqgcio);
  uuvfq : entity work.fegvg
    port map (zweeelk => xengybhe);
  q : entity work.fegvg
    port map (zweeelk => yaaswfrfkz);
end hgh;



-- Seed after: 15752018032999609497,9372850727389630639
