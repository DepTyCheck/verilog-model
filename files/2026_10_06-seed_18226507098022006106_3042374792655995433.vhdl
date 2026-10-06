-- Seed: 18226507098022006106,3042374792655995433

library ieee;
use ieee.std_logic_1164.all;

entity xpsbcsyf is
  port (aiysxavpjc : inout std_logic; q : buffer std_logic_vector(3 to 4); sutgfqcbn : in std_logic; r : out std_logic);
end xpsbcsyf;

architecture ybmx of xpsbcsyf is
  
begin
  
end ybmx;

library ieee;
use ieee.std_logic_1164.all;

entity uay is
  port (s : inout integer; ydmilopsn : in std_logic; gncty : buffer time; vhorwn : out std_logic_vector(2 to 3));
end uay;

library ieee;
use ieee.std_logic_1164.all;

architecture qqk of uay is
  signal eoyvdqapo : std_logic;
  signal fzjwzv : std_logic;
  signal qsfoqtwltk : std_logic_vector(3 to 4);
  signal riiwryfvm : std_logic;
begin
  pet : entity work.xpsbcsyf
    port map (aiysxavpjc => riiwryfvm, q => vhorwn, sutgfqcbn => riiwryfvm, r => riiwryfvm);
  gbar : entity work.xpsbcsyf
    port map (aiysxavpjc => riiwryfvm, q => qsfoqtwltk, sutgfqcbn => fzjwzv, r => eoyvdqapo);
  
  -- Single-driven assignments
  gncty <= gncty;
  s <= 1;
  
  -- Multi-driven assignments
  vhorwn <= ('L', 'X');
  vhorwn <= vhorwn;
  qsfoqtwltk <= vhorwn;
end qqk;

library ieee;
use ieee.std_logic_1164.all;

entity fviisquztx is
  port (f : linkage bit; diwzyx : out std_logic; vir : out time);
end fviisquztx;

library ieee;
use ieee.std_logic_1164.all;

architecture znaygtzp of fviisquztx is
  signal qnluievy : std_logic;
  signal ekxdqrmttg : std_logic;
  signal zugmhhfuuf : std_logic_vector(3 to 4);
  signal ue : std_logic;
  signal upsjv : std_logic_vector(3 to 4);
  signal ib : std_logic;
  signal khnfju : std_logic;
  signal tytqbotu : integer;
  signal bkjlkgam : std_logic_vector(2 to 3);
  signal zii : time;
  signal nd : std_logic;
  signal kmfb : integer;
begin
  ocxwrej : entity work.uay
    port map (s => kmfb, ydmilopsn => nd, gncty => zii, vhorwn => bkjlkgam);
  c : entity work.uay
    port map (s => tytqbotu, ydmilopsn => khnfju, gncty => vir, vhorwn => bkjlkgam);
  g : entity work.xpsbcsyf
    port map (aiysxavpjc => ib, q => upsjv, sutgfqcbn => diwzyx, r => diwzyx);
  r : entity work.xpsbcsyf
    port map (aiysxavpjc => ue, q => zugmhhfuuf, sutgfqcbn => ekxdqrmttg, r => qnluievy);
  
  -- Multi-driven assignments
  diwzyx <= diwzyx;
  ue <= 'U';
  diwzyx <= '-';
end znaygtzp;



-- Seed after: 1128431993577334883,3042374792655995433
