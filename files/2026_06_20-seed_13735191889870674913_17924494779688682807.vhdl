-- Seed: 13735191889870674913,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity kbcspizpsm is
  port (fdbss : linkage real_vector(0 downto 2); qta : buffer std_logic_vector(2 to 4));
end kbcspizpsm;

architecture pnogr of kbcspizpsm is
  
begin
  -- Multi-driven assignments
  qta <= ('W', 'W', 'W');
  qta <= "L-1";
  qta <= "ULZ";
end pnogr;



-- Seed after: 18147566855780998527,17924494779688682807
