-- Seed: 16419914724298085124,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity arrgc is
  port (x : in time; avshtm : buffer std_logic; q : out character; wxf : in std_logic_vector(4 downto 1));
end arrgc;

architecture cke of arrgc is
  
begin
  -- Single-driven assignments
  q <= 'j';
  
  -- Multi-driven assignments
  avshtm <= '1';
end cke;



-- Seed after: 1423054497352173613,13479070923501788437
