-- Seed: 11621098132778148809,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity yuicwugozb is
  port (heuhg : in std_logic; krtl : linkage time);
end yuicwugozb;

architecture fqrdnwy of yuicwugozb is
  
begin
  
end fqrdnwy;

library ieee;
use ieee.std_logic_1164.all;

entity eqsi is
  port (vlzgklen : linkage std_logic_vector(1 downto 2); uzsgtud : linkage std_logic_vector(3 to 0); qix : buffer integer_vector(3 downto 4));
end eqsi;

library ieee;
use ieee.std_logic_1164.all;

architecture dytn of eqsi is
  signal bwmazq : time;
  signal awc : time;
  signal ccmkainylw : std_logic;
  signal cqnjo : time;
  signal pzxuvhf : std_logic;
begin
  dk : entity work.yuicwugozb
    port map (heuhg => pzxuvhf, krtl => cqnjo);
  eyswdcqus : entity work.yuicwugozb
    port map (heuhg => ccmkainylw, krtl => awc);
  puk : entity work.yuicwugozb
    port map (heuhg => pzxuvhf, krtl => bwmazq);
  
  -- Single-driven assignments
  qix <= (others => 0);
  
  -- Multi-driven assignments
  ccmkainylw <= pzxuvhf;
  pzxuvhf <= 'X';
end dytn;



-- Seed after: 10636263759150134361,10594830431004325987
