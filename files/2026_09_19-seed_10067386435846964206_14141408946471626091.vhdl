-- Seed: 10067386435846964206,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (gk : out std_logic_vector(3 downto 1); lyi : buffer integer);
end d;

architecture x of d is
  
begin
  -- Single-driven assignments
  lyi <= 2#1_1_0#;
  
  -- Multi-driven assignments
  gk <= gk;
end x;



-- Seed after: 8551438871268167403,14141408946471626091
