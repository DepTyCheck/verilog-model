-- Seed: 17665528324833766545,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity sk is
  port (mblqg : inout std_logic_vector(1 to 1); mxnabin : inout time);
end sk;

architecture byklp of sk is
  
begin
  -- Single-driven assignments
  mxnabin <= mxnabin;
  
  -- Multi-driven assignments
  mblqg <= "H";
  mblqg <= (others => 'W');
  mblqg <= mblqg;
end byklp;



-- Seed after: 3005546414910603226,10975587770517081373
