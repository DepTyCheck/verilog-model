-- Seed: 10046264205929485082,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity xhjmn is
  port (dpsypxm : inout integer; cqxqnavw : inout time; uyohdssg : out std_logic_vector(3 downto 2); oasw : out real_vector(0 downto 4));
end xhjmn;

architecture fcc of xhjmn is
  
begin
  
end fcc;

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (qyxw : inout std_logic; c : in std_logic_vector(2 to 4));
end s;

library ieee;
use ieee.std_logic_1164.all;

architecture sv of s is
  signal fhw : real_vector(0 downto 4);
  signal gftr : std_logic_vector(3 downto 2);
  signal fxpxsqm : time;
  signal yaxznvxdhh : integer;
begin
  l : entity work.xhjmn
    port map (dpsypxm => yaxznvxdhh, cqxqnavw => fxpxsqm, uyohdssg => gftr, oasw => fhw);
  
  -- Multi-driven assignments
  qyxw <= 'L';
  qyxw <= 'L';
end sv;



-- Seed after: 9209383876762224715,10557070023141912087
