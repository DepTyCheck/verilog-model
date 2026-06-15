-- Seed: 9522225726490753592,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity kj is
  port (o : inout std_logic; pyztb : out real_vector(2 to 2));
end kj;

architecture spdadeg of kj is
  
begin
  -- Single-driven assignments
  pyztb <= (others => 4_2_0_0_3.2_2_3);
  
  -- Multi-driven assignments
  o <= '1';
end spdadeg;



-- Seed after: 8636374307525081826,1834764876137802293
