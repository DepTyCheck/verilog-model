-- Seed: 16346338316976710129,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity nww is
  port (ztklafla : inout integer; cs : inout time; hszg : buffer std_logic_vector(4 downto 0); dfoxuydaxb : buffer integer);
end nww;

architecture f of nww is
  
begin
  -- Single-driven assignments
  dfoxuydaxb <= 16#A#;
  ztklafla <= 24;
  cs <= 8#65114.320# us;
  
  -- Multi-driven assignments
  hszg <= ('H', 'Z', 'U', '1', 'L');
  hszg <= ('-', 'H', 'H', '-', 'W');
  hszg <= "UXZ1-";
end f;



-- Seed after: 11841102573255591695,13694093582652240945
