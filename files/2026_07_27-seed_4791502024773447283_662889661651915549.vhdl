-- Seed: 4791502024773447283,662889661651915549

library ieee;
use ieee.std_logic_1164.all;

entity dqyk is
  port (xk : in real; rhw : out std_logic_vector(1 to 3));
end dqyk;

architecture iom of dqyk is
  
begin
  -- Multi-driven assignments
  rhw <= rhw;
  rhw <= "U0-";
  rhw <= rhw;
  rhw <= "XWX";
end iom;



-- Seed after: 16719894750026778344,662889661651915549
