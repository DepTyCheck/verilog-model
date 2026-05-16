-- Seed: 10542470596869146410,14312733773653067203



entity tb is
  port (fbrea : inout character; ufoh : in time; xj : inout real; cptt : out time);
end tb;



architecture idoffc of tb is
  
begin
  
end idoffc;

library ieee;
use ieee.std_logic_1164.all;

entity no is
  port (gqnpgp : buffer severity_level; cjzxdizc : in std_logic; xjej : inout time; culq : linkage integer);
end no;



architecture p of no is
  signal kdkjyyw : real;
  signal uwht : time;
  signal ljd : character;
  signal yt : time;
  signal fqwj : real;
  signal j : character;
  signal z : time;
  signal tmusbik : real;
  signal dwvrg : character;
begin
  k : entity work.tb
    port map (fbrea => dwvrg, ufoh => xjej, xj => tmusbik, cptt => z);
  pmhkmv : entity work.tb
    port map (fbrea => j, ufoh => xjej, xj => fqwj, cptt => yt);
  ol : entity work.tb
    port map (fbrea => ljd, ufoh => uwht, xj => kdkjyyw, cptt => xjej);
end p;

library ieee;
use ieee.std_logic_1164.all;

entity ud is
  port (gfdhydouf : buffer std_logic; xevwp : linkage severity_level; tvroninnhf : linkage integer);
end ud;

library ieee;
use ieee.std_logic_1164.all;

architecture ufle of ud is
  signal rybadu : severity_level;
  signal idhunsxt : time;
  signal nf : std_logic;
  signal uxexzf : severity_level;
  signal iuvizdxr : time;
  signal jsmi : real;
  signal ynge : time;
  signal mpubuhlulw : character;
begin
  yrmr : entity work.tb
    port map (fbrea => mpubuhlulw, ufoh => ynge, xj => jsmi, cptt => iuvizdxr);
  ptnxu : entity work.no
    port map (gqnpgp => uxexzf, cjzxdizc => nf, xjej => idhunsxt, culq => tvroninnhf);
  q : entity work.no
    port map (gqnpgp => rybadu, cjzxdizc => gfdhydouf, xjej => ynge, culq => tvroninnhf);
end ufle;



entity kuez is
  port (r : in time; wjvajeypm : buffer time; ynj : out time; z : in time);
end kuez;

library ieee;
use ieee.std_logic_1164.all;

architecture cl of kuez is
  signal me : std_logic;
  signal vdiecma : real;
  signal fou : character;
  signal vssuuand : integer;
  signal zzcju : integer;
  signal t : std_logic;
  signal p : severity_level;
begin
  keedo : entity work.no
    port map (gqnpgp => p, cjzxdizc => t, xjej => wjvajeypm, culq => zzcju);
  qdhriyckmi : entity work.ud
    port map (gfdhydouf => t, xevwp => p, tvroninnhf => vssuuand);
  znzsbivfnl : entity work.tb
    port map (fbrea => fou, ufoh => z, xj => vdiecma, cptt => ynj);
  ro : entity work.ud
    port map (gfdhydouf => me, xevwp => p, tvroninnhf => vssuuand);
end cl;



-- Seed after: 9283410103484350382,14312733773653067203
