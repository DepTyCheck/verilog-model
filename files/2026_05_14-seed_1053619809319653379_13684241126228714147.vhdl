-- Seed: 1053619809319653379,13684241126228714147



entity mwcscvs is
  port (iif : inout integer; siofrnahe : linkage time; gu : out time);
end mwcscvs;



architecture gwokkpg of mwcscvs is
  
begin
  
end gwokkpg;



entity srdnu is
  port (dxhqmtlgz : in time);
end srdnu;



architecture ofn of srdnu is
  signal trtcrquos : time;
  signal linnv : time;
  signal nbj : integer;
  signal vqprytwi : time;
  signal ufcxpxbjk : integer;
begin
  m : entity work.mwcscvs
    port map (iif => ufcxpxbjk, siofrnahe => vqprytwi, gu => vqprytwi);
  heyfmmcy : entity work.mwcscvs
    port map (iif => nbj, siofrnahe => linnv, gu => trtcrquos);
end ofn;



entity xzzuvfcu is
  port (um : inout bit; ximlji : in time; jfan : out severity_level);
end xzzuvfcu;



architecture vkckdd of xzzuvfcu is
  signal fxrdmsuaj : time;
  signal ul : integer;
  signal nwycyrr : time;
begin
  rvgvf : entity work.srdnu
    port map (dxhqmtlgz => ximlji);
  ntn : entity work.srdnu
    port map (dxhqmtlgz => nwycyrr);
  qejvdnl : entity work.mwcscvs
    port map (iif => ul, siofrnahe => fxrdmsuaj, gu => fxrdmsuaj);
  f : entity work.srdnu
    port map (dxhqmtlgz => ximlji);
end vkckdd;



-- Seed after: 15826826040194110873,13684241126228714147
