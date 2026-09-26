-- Seed: 1076200771360863689,10875537289884587119

library ieee;
use ieee.std_logic_1164.all;

entity rqtghspr is
  port (yvnha : out time; bk : linkage real; sjpv : inout std_logic);
end rqtghspr;

architecture fwhhod of rqtghspr is
  
begin
  -- Single-driven assignments
  yvnha <= 16#1061# ps;
end fwhhod;

library ieee;
use ieee.std_logic_1164.all;

entity jjl is
  port (abbmdqsu : in std_logic);
end jjl;

library ieee;
use ieee.std_logic_1164.all;

architecture zovi of jjl is
  signal etspbgmcd : real;
  signal amnfyjezv : time;
  signal lecytrm : real;
  signal hamjycdyh : time;
  signal abwrygxg : std_logic;
  signal od : real;
  signal uszqqpi : time;
  signal qbdm : std_logic;
  signal nbzftwq : real;
  signal oodb : time;
begin
  juzpkdxrf : entity work.rqtghspr
    port map (yvnha => oodb, bk => nbzftwq, sjpv => qbdm);
  ftbf : entity work.rqtghspr
    port map (yvnha => uszqqpi, bk => od, sjpv => abwrygxg);
  mehvuquf : entity work.rqtghspr
    port map (yvnha => hamjycdyh, bk => lecytrm, sjpv => qbdm);
  u : entity work.rqtghspr
    port map (yvnha => amnfyjezv, bk => etspbgmcd, sjpv => qbdm);
  
  -- Multi-driven assignments
  qbdm <= 'U';
  abwrygxg <= abbmdqsu;
end zovi;



-- Seed after: 16739901709706324950,10875537289884587119
