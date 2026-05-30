-- Seed: 16021823385396214839,1630680796402093529



entity eineq is
  port (y : buffer integer);
end eineq;



architecture gphuecn of eineq is
  
begin
  
end gphuecn;

library ieee;
use ieee.std_logic_1164.all;

entity rk is
  port (fsvmtw : in real; vmritbv : linkage std_logic_vector(4 downto 3));
end rk;



architecture xdugnklk of rk is
  signal aljyos : integer;
  signal rm : integer;
  signal qd : integer;
  signal votswjx : integer;
begin
  ufbtcty : entity work.eineq
    port map (y => votswjx);
  tfmtdcqd : entity work.eineq
    port map (y => qd);
  vpdij : entity work.eineq
    port map (y => rm);
  ecnsnc : entity work.eineq
    port map (y => aljyos);
end xdugnklk;

library ieee;
use ieee.std_logic_1164.all;

entity tabrvyzzc is
  port (shkgauli : inout std_logic);
end tabrvyzzc;

library ieee;
use ieee.std_logic_1164.all;

architecture pidnr of tabrvyzzc is
  signal tjzqgwfbe : integer;
  signal ikesvkyx : std_logic_vector(4 downto 3);
  signal az : real;
begin
  mavazyj : entity work.rk
    port map (fsvmtw => az, vmritbv => ikesvkyx);
  ban : entity work.eineq
    port map (y => tjzqgwfbe);
end pidnr;



entity qwkzg is
  port (czmusytnu : buffer bit_vector(0 downto 2); xdak : linkage integer; bsinq : inout bit; vkrrqlm : in real);
end qwkzg;

library ieee;
use ieee.std_logic_1164.all;

architecture vwknlp of qwkzg is
  signal h : std_logic_vector(4 downto 3);
  signal brxzbi : real;
  signal ippf : std_logic;
  signal robep : integer;
begin
  m : entity work.eineq
    port map (y => robep);
  tvy : entity work.tabrvyzzc
    port map (shkgauli => ippf);
  pjofsdl : entity work.rk
    port map (fsvmtw => brxzbi, vmritbv => h);
end vwknlp;



-- Seed after: 14382114744674878075,1630680796402093529
