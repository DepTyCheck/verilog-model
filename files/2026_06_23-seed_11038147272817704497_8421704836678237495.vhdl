-- Seed: 11038147272817704497,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (nz : inout integer; pdxn : out std_logic_vector(3 to 4));
end d;

architecture yjxvple of d is
  
begin
  -- Single-driven assignments
  nz <= 2_3_1_1_2;
  
  -- Multi-driven assignments
  pdxn <= ('H', 'X');
end yjxvple;

entity belyzp is
  port (csxmaykl : buffer integer; yknhmnb : inout time_vector(4 downto 3));
end belyzp;

library ieee;
use ieee.std_logic_1164.all;

architecture hdvnw of belyzp is
  signal uztspf : std_logic_vector(3 to 4);
  signal oxosf : integer;
begin
  gpkth : entity work.d
    port map (nz => oxosf, pdxn => uztspf);
  qkvqh : entity work.d
    port map (nz => csxmaykl, pdxn => uztspf);
  
  -- Multi-driven assignments
  uztspf <= "X-";
  uztspf <= "L0";
  uztspf <= "-H";
end hdvnw;



-- Seed after: 9548578864047665011,8421704836678237495
