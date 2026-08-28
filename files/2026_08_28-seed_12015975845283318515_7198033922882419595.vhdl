-- Seed: 12015975845283318515,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity qf is
  port (pnee : out real; blta : buffer std_logic_vector(2 downto 2));
end qf;

architecture lj of qf is
  
begin
  -- Single-driven assignments
  pnee <= 8#4_5.1#;
  
  -- Multi-driven assignments
  blta <= (others => 'U');
  blta <= "0";
end lj;



-- Seed after: 11284289953081489012,7198033922882419595
