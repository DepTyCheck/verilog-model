-- Seed: 14248480787979360385,2338584220606314193

library ieee;
use ieee.std_logic_1164.all;

entity vx is
  port (pv : inout std_logic_vector(1 downto 1));
end vx;

architecture lbcq of vx is
  
begin
  -- Multi-driven assignments
  pv <= pv;
  pv <= (others => '1');
  pv <= pv;
  pv <= (others => 'W');
end lbcq;

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (lmzoor : in std_logic_vector(3 downto 2); lqs : linkage time_vector(1 downto 1); hyujimf : buffer std_logic);
end a;

library ieee;
use ieee.std_logic_1164.all;

architecture kp of a is
  signal n : std_logic_vector(1 downto 1);
begin
  sgzfq : entity work.vx
    port map (pv => n);
end kp;

entity njdfms is
  port (vqfuhmu : linkage time; aqidxpm : in integer);
end njdfms;

library ieee;
use ieee.std_logic_1164.all;

architecture l of njdfms is
  signal tczjcacr : std_logic_vector(1 downto 1);
  signal lpckm : std_logic_vector(1 downto 1);
  signal cf : std_logic;
  signal biwdanduzn : time_vector(1 downto 1);
  signal otclursbze : std_logic_vector(3 downto 2);
begin
  svfl : entity work.a
    port map (lmzoor => otclursbze, lqs => biwdanduzn, hyujimf => cf);
  s : entity work.vx
    port map (pv => lpckm);
  xatimsx : entity work.vx
    port map (pv => tczjcacr);
  
  -- Multi-driven assignments
  tczjcacr <= lpckm;
  lpckm <= (others => 'H');
end l;

library ieee;
use ieee.std_logic_1164.all;

entity epyps is
  port (nt : buffer bit; o : buffer std_logic; ocxyygkzkq : linkage time);
end epyps;

library ieee;
use ieee.std_logic_1164.all;

architecture lb of epyps is
  signal qkakcfry : std_logic_vector(1 downto 1);
begin
  eaecyzyvs : entity work.vx
    port map (pv => qkakcfry);
  c : entity work.vx
    port map (pv => qkakcfry);
  
  -- Multi-driven assignments
  qkakcfry <= qkakcfry;
  o <= o;
  o <= 'U';
end lb;



-- Seed after: 16427308748811166829,2338584220606314193
