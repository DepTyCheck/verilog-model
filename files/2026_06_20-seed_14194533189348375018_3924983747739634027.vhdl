-- Seed: 14194533189348375018,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity urijinbopj is
  port (jnezdyk : inout std_logic; d : inout std_logic_vector(2 to 3); ns : buffer boolean_vector(0 to 4));
end urijinbopj;

architecture pij of urijinbopj is
  
begin
  -- Single-driven assignments
  ns <= (FALSE, FALSE, TRUE, FALSE, TRUE);
  
  -- Multi-driven assignments
  jnezdyk <= 'W';
  jnezdyk <= '1';
  jnezdyk <= '0';
end pij;



-- Seed after: 609986722273525876,3924983747739634027
