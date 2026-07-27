-- Seed: 6539388288991245643,662889661651915549

library ieee;
use ieee.std_logic_1164.all;

entity fzx is
  port (yxpnxob : in boolean_vector(0 to 4); cwclkf : linkage integer; opudtbq : buffer std_logic_vector(3 to 3); h : out std_logic);
end fzx;

architecture myg of fzx is
  
begin
  
end myg;

entity gavdlk is
  port (neez : in bit; h : in real; frst : in time; aioq : linkage time);
end gavdlk;

architecture egom of gavdlk is
  
begin
  
end egom;

library ieee;
use ieee.std_logic_1164.all;

entity inpnslcs is
  port (r : out bit_vector(2 to 4); vyxdv : inout integer; ixhnaix : linkage std_logic_vector(4 downto 3));
end inpnslcs;

architecture ake of inpnslcs is
  
begin
  -- Single-driven assignments
  vyxdv <= 16#6#;
  r <= ('0', '0', '0');
end ake;

entity mnsye is
  port (exvfxgauzt : buffer integer);
end mnsye;

library ieee;
use ieee.std_logic_1164.all;

architecture kxtkyozky of mnsye is
  signal wxja : time;
  signal mchib : bit;
  signal yicf : std_logic;
  signal j : std_logic_vector(3 to 3);
  signal a : boolean_vector(0 to 4);
  signal cal : time;
  signal ezyoohdcd : time;
  signal pdqjsbff : real;
  signal koivfiiei : bit;
begin
  eybjevzz : entity work.gavdlk
    port map (neez => koivfiiei, h => pdqjsbff, frst => ezyoohdcd, aioq => cal);
  s : entity work.fzx
    port map (yxpnxob => a, cwclkf => exvfxgauzt, opudtbq => j, h => yicf);
  fykzzex : entity work.gavdlk
    port map (neez => mchib, h => pdqjsbff, frst => cal, aioq => wxja);
end kxtkyozky;



-- Seed after: 13481814787293461833,662889661651915549
