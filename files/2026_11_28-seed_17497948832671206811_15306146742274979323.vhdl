-- Seed: 17497948832671206811,15306146742274979323

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (rs : in std_logic; chv : inout real_vector(2 to 1));
end a;

architecture twaafdc of a is
  
begin
  -- Single-driven assignments
  chv <= chv;
end twaafdc;

library ieee;
use ieee.std_logic_1164.all;

entity szqeo is
  port (c : inout std_logic_vector(1 downto 1));
end szqeo;

architecture zayixi of szqeo is
  
begin
  -- Multi-driven assignments
  c <= c;
  c <= c;
  c <= "H";
end zayixi;



-- Seed after: 2135181143627804171,15306146742274979323
