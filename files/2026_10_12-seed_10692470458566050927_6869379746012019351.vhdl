-- Seed: 10692470458566050927,6869379746012019351

library ieee;
use ieee.std_logic_1164.all;

entity kqurp is
  port (zcse : inout time; aftddcq : out std_logic_vector(4 downto 3); qkrr : in std_logic_vector(4 to 2));
end kqurp;

architecture pbv of kqurp is
  
begin
  -- Single-driven assignments
  zcse <= 2 ps;
  
  -- Multi-driven assignments
  aftddcq <= ('L', '0');
  aftddcq <= aftddcq;
end pbv;



-- Seed after: 8788529331720693618,6869379746012019351
