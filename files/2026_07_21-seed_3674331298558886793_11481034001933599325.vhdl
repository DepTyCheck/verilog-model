-- Seed: 3674331298558886793,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity pux is
  port (ffz : in severity_level; svrlyxhsdj : linkage time; gas : linkage std_logic_vector(4 to 3));
end pux;

architecture nzoehcfg of pux is
  
begin
  
end nzoehcfg;

entity ihmvskpugx is
  port (p : out boolean_vector(1 downto 4); jqkcbwfdl : in integer);
end ihmvskpugx;

library ieee;
use ieee.std_logic_1164.all;

architecture b of ihmvskpugx is
  signal oitvmlwj : time;
  signal ywsawjhv : severity_level;
  signal rpxberk : std_logic_vector(4 to 3);
  signal afuxbpsbi : time;
  signal dj : severity_level;
begin
  o : entity work.pux
    port map (ffz => dj, svrlyxhsdj => afuxbpsbi, gas => rpxberk);
  hegrwn : entity work.pux
    port map (ffz => ywsawjhv, svrlyxhsdj => oitvmlwj, gas => rpxberk);
  
  -- Single-driven assignments
  ywsawjhv <= dj;
  p <= (others => TRUE);
  dj <= ERROR;
  
  -- Multi-driven assignments
  rpxberk <= rpxberk;
  rpxberk <= "";
  rpxberk <= (others => '0');
end b;

entity j is
  port (sk : out time);
end j;

library ieee;
use ieee.std_logic_1164.all;

architecture qkvdqfk of j is
  signal hxzrzpvqk : std_logic_vector(4 to 3);
  signal p : time;
  signal tz : severity_level;
begin
  akpxhaitn : entity work.pux
    port map (ffz => tz, svrlyxhsdj => p, gas => hxzrzpvqk);
  
  -- Single-driven assignments
  sk <= p;
  tz <= tz;
  
  -- Multi-driven assignments
  hxzrzpvqk <= hxzrzpvqk;
end qkvdqfk;



-- Seed after: 842014714261285202,11481034001933599325
