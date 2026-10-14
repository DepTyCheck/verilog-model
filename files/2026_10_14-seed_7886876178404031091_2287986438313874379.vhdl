-- Seed: 7886876178404031091,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity vm is
  port (xoyady : inout std_logic_vector(1 to 3); tjfl : out std_logic);
end vm;

architecture ikyvzuhkn of vm is
  
begin
  
end ikyvzuhkn;

library ieee;
use ieee.std_logic_1164.all;

entity tqafidr is
  port (nmu : out std_logic_vector(1 to 0); gpxqnbsfc : out integer);
end tqafidr;

library ieee;
use ieee.std_logic_1164.all;

architecture zze of tqafidr is
  signal c : std_logic_vector(1 to 3);
  signal h : std_logic;
  signal bdodid : std_logic_vector(1 to 3);
begin
  hckztagrp : entity work.vm
    port map (xoyady => bdodid, tjfl => h);
  fca : entity work.vm
    port map (xoyady => bdodid, tjfl => h);
  y : entity work.vm
    port map (xoyady => c, tjfl => h);
  w : entity work.vm
    port map (xoyady => c, tjfl => h);
  
  -- Single-driven assignments
  gpxqnbsfc <= gpxqnbsfc;
  
  -- Multi-driven assignments
  h <= h;
end zze;



-- Seed after: 18039004486205979246,2287986438313874379
