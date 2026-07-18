-- Seed: 16242342812539331014,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (kvpk : linkage std_logic_vector(2 downto 2));
end z;

architecture fprmb of z is
  
begin
  
end fprmb;

library ieee;
use ieee.std_logic_1164.all;

entity envqlp is
  port (edjc : out std_logic_vector(0 to 0); b : buffer std_logic; hqcqffkw : inout real_vector(4 downto 1); cuzhmedfs : out real_vector(0 to 1));
end envqlp;

library ieee;
use ieee.std_logic_1164.all;

architecture lygblohbs of envqlp is
  signal gwjghlepas : std_logic_vector(2 downto 2);
  signal yhkglrtfl : std_logic_vector(2 downto 2);
  signal ypzyhl : std_logic_vector(2 downto 2);
begin
  ablz : entity work.z
    port map (kvpk => edjc);
  dwdplzkjk : entity work.z
    port map (kvpk => ypzyhl);
  ec : entity work.z
    port map (kvpk => yhkglrtfl);
  vgbbblch : entity work.z
    port map (kvpk => gwjghlepas);
  
  -- Single-driven assignments
  cuzhmedfs <= cuzhmedfs;
  hqcqffkw <= hqcqffkw;
  
  -- Multi-driven assignments
  gwjghlepas <= yhkglrtfl;
  ypzyhl <= edjc;
  gwjghlepas <= edjc;
  edjc <= "U";
end lygblohbs;



-- Seed after: 5018317146846082689,1112937151005418631
