-- Seed: 4363510864486880356,3042374792655995433

library ieee;
use ieee.std_logic_1164.all;

entity lmcefxpt is
  port (fsmsk : out std_logic; nl : in time_vector(3 to 1); qdot : out integer);
end lmcefxpt;

architecture vu of lmcefxpt is
  
begin
  -- Single-driven assignments
  qdot <= 3_3_1_0_2;
  
  -- Multi-driven assignments
  fsmsk <= fsmsk;
  fsmsk <= 'X';
  fsmsk <= '0';
end vu;



-- Seed after: 15044829362145203028,3042374792655995433
