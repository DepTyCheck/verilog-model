-- Seed: 14459718184696232199,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity zwmpzrlb is
  port (rubsyrmq : out std_logic_vector(0 downto 0); dnbrcp : buffer integer; fakhn : buffer time; eqwbhpxie : in real_vector(4 downto 0));
end zwmpzrlb;

architecture s of zwmpzrlb is
  
begin
  -- Single-driven assignments
  fakhn <= 2#0001.1# fs;
  dnbrcp <= 8#7515#;
  
  -- Multi-driven assignments
  rubsyrmq <= "Z";
  rubsyrmq <= "L";
end s;

library ieee;
use ieee.std_logic_1164.all;

entity xar is
  port (jehtiz : in bit; z : buffer std_logic_vector(1 downto 4));
end xar;

library ieee;
use ieee.std_logic_1164.all;

architecture ua of xar is
  signal pp : real_vector(4 downto 0);
  signal ox : time;
  signal vnwo : integer;
  signal zcwtajo : std_logic_vector(0 downto 0);
begin
  ytway : entity work.zwmpzrlb
    port map (rubsyrmq => zcwtajo, dnbrcp => vnwo, fakhn => ox, eqwbhpxie => pp);
  
  -- Single-driven assignments
  pp <= (16#FBE.7_1#, 1_2_3.232, 4_3_2_3.344, 4343.44, 4_0.1_2_1);
  
  -- Multi-driven assignments
  z <= "";
end ua;



-- Seed after: 11452509199984425369,17924494779688682807
