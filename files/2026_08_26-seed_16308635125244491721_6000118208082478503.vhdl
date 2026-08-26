-- Seed: 16308635125244491721,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity mkx is
  port (rac : buffer std_logic_vector(1 downto 0));
end mkx;

architecture gcmmcng of mkx is
  
begin
  -- Multi-driven assignments
  rac <= "W1";
  rac <= rac;
end gcmmcng;



-- Seed after: 9516317285922810756,6000118208082478503
