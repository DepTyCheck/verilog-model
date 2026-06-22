-- Seed: 17581144829495885861,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity pnvctextd is
  port (vckvpms : out bit; db : inout time; ot : out std_logic_vector(4 to 0));
end pnvctextd;

architecture gbluapr of pnvctextd is
  
begin
  -- Single-driven assignments
  db <= 1_2_4.0_0_2 fs;
  vckvpms <= '1';
  
  -- Multi-driven assignments
  ot <= (others => '0');
  ot <= (others => '0');
  ot <= (others => '0');
end gbluapr;



-- Seed after: 12584217681835463392,13479070923501788437
