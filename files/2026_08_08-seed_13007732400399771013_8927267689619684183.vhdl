-- Seed: 13007732400399771013,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity xp is
  port (phd : in severity_level; tl : buffer std_logic_vector(1 downto 1); xs : out std_logic);
end xp;

architecture spfizfc of xp is
  
begin
  -- Multi-driven assignments
  tl <= tl;
  tl <= "U";
end spfizfc;



-- Seed after: 17381168594892621119,8927267689619684183
