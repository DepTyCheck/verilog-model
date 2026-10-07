-- Seed: 16928209468926915383,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity dvsoh is
  port (slkblk : inout std_logic_vector(3 to 0); thnniwqa : inout integer);
end dvsoh;

architecture olzphblzfi of dvsoh is
  
begin
  -- Single-driven assignments
  thnniwqa <= thnniwqa;
end olzphblzfi;

entity c is
  port (qz : buffer integer);
end c;

library ieee;
use ieee.std_logic_1164.all;

architecture p of c is
  signal zmfensc : std_logic_vector(3 to 0);
  signal felff : integer;
  signal lhr : std_logic_vector(3 to 0);
begin
  xmlso : entity work.dvsoh
    port map (slkblk => lhr, thnniwqa => felff);
  qy : entity work.dvsoh
    port map (slkblk => zmfensc, thnniwqa => qz);
  
  -- Multi-driven assignments
  lhr <= (others => '0');
end p;



-- Seed after: 3846892125677330710,5906004015519833893
