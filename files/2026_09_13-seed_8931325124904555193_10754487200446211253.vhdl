-- Seed: 8931325124904555193,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (dz : out std_logic; ljpv : out boolean; pecwbtv : out character);
end h;

architecture akh of h is
  
begin
  -- Single-driven assignments
  ljpv <= ljpv;
  pecwbtv <= pecwbtv;
  
  -- Multi-driven assignments
  dz <= dz;
end akh;

entity gld is
  port (fow : in string(1 to 4); kl : linkage integer_vector(2 downto 2); ailmlu : linkage integer);
end gld;

library ieee;
use ieee.std_logic_1164.all;

architecture kroykxb of gld is
  signal rrkc : character;
  signal iaehzp : boolean;
  signal srsylgbybz : std_logic;
  signal iphmts : character;
  signal nzgpsd : boolean;
  signal sftdexj : std_logic;
  signal dyw : character;
  signal wcslhhxe : boolean;
  signal eyddzvww : std_logic;
begin
  hipq : entity work.h
    port map (dz => eyddzvww, ljpv => wcslhhxe, pecwbtv => dyw);
  yhjm : entity work.h
    port map (dz => sftdexj, ljpv => nzgpsd, pecwbtv => iphmts);
  b : entity work.h
    port map (dz => srsylgbybz, ljpv => iaehzp, pecwbtv => rrkc);
  
  -- Multi-driven assignments
  srsylgbybz <= sftdexj;
end kroykxb;

library ieee;
use ieee.std_logic_1164.all;

entity bvoetq is
  port (valglmvf : out bit; k : in std_logic_vector(4 downto 3); x : inout std_logic);
end bvoetq;

architecture yafur of bvoetq is
  signal rrd : character;
  signal juhas : boolean;
  signal phnbrb : character;
  signal awvmasalv : boolean;
begin
  c : entity work.h
    port map (dz => x, ljpv => awvmasalv, pecwbtv => phnbrb);
  ivyqupr : entity work.h
    port map (dz => x, ljpv => juhas, pecwbtv => rrd);
  
  -- Single-driven assignments
  valglmvf <= valglmvf;
  
  -- Multi-driven assignments
  x <= '1';
  x <= 'Z';
end yafur;

library ieee;
use ieee.std_logic_1164.all;

entity qfwfsqw is
  port (frvtcceq : linkage character; brutze : out std_logic_vector(0 downto 1); ansfrvw : out time; zubxicfz : linkage time);
end qfwfsqw;

library ieee;
use ieee.std_logic_1164.all;

architecture hgoiyxpbkl of qfwfsqw is
  signal qwos : std_logic_vector(4 downto 3);
  signal bkdiapop : bit;
  signal eyvnpqwq : character;
  signal ve : boolean;
  signal tjueym : character;
  signal un : boolean;
  signal myqbgrz : std_logic;
begin
  j : entity work.h
    port map (dz => myqbgrz, ljpv => un, pecwbtv => tjueym);
  zmvas : entity work.h
    port map (dz => myqbgrz, ljpv => ve, pecwbtv => eyvnpqwq);
  udodqxdm : entity work.bvoetq
    port map (valglmvf => bkdiapop, k => qwos, x => myqbgrz);
  
  -- Single-driven assignments
  ansfrvw <= ansfrvw;
  
  -- Multi-driven assignments
  brutze <= brutze;
  qwos <= qwos;
  brutze <= "";
end hgoiyxpbkl;



-- Seed after: 13502425638776422517,10754487200446211253
