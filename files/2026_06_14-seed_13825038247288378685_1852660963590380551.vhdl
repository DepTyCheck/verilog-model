-- Seed: 13825038247288378685,1852660963590380551

library ieee;
use ieee.std_logic_1164.all;

entity vrmnibf is
  port (wrcv : buffer boolean_vector(4 to 1); ol : inout real; nps : in std_logic_vector(3 to 4); wmojki : in time);
end vrmnibf;



architecture fqpdn of vrmnibf is
  
begin
  
end fqpdn;



entity lo is
  port (nxtwokhzuf : in integer_vector(4 to 0); fruqi : linkage time);
end lo;

library ieee;
use ieee.std_logic_1164.all;

architecture chsiup of lo is
  signal qp : time;
  signal ktdghrj : std_logic_vector(3 to 4);
  signal tadgsfmdgl : real;
  signal chiwqyxgb : boolean_vector(4 to 1);
begin
  jxfrjlngm : entity work.vrmnibf
    port map (wrcv => chiwqyxgb, ol => tadgsfmdgl, nps => ktdghrj, wmojki => qp);
end chsiup;



entity lxntdd is
  port (zoxyjbmu : in bit_vector(3 downto 1));
end lxntdd;

library ieee;
use ieee.std_logic_1164.all;

architecture nkkci of lxntdd is
  signal oouregpyrd : integer_vector(4 to 0);
  signal wuckokqbv : time;
  signal abii : std_logic_vector(3 to 4);
  signal o : real;
  signal wwauti : boolean_vector(4 to 1);
begin
  eak : entity work.vrmnibf
    port map (wrcv => wwauti, ol => o, nps => abii, wmojki => wuckokqbv);
  ywnmmn : entity work.lo
    port map (nxtwokhzuf => oouregpyrd, fruqi => wuckokqbv);
end nkkci;



-- Seed after: 221888590927879472,1852660963590380551
