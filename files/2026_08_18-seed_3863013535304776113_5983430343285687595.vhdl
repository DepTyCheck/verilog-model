-- Seed: 3863013535304776113,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity dq is
  port (inihw : buffer std_logic_vector(0 to 4); mgpgrw : in integer);
end dq;

architecture buac of dq is
  
begin
  -- Multi-driven assignments
  inihw <= ('Z', 'H', 'U', 'W', 'L');
  inihw <= "LU1WX";
  inihw <= ('-', 'W', 'L', 'U', '-');
  inihw <= inihw;
end buac;



-- Seed after: 11437207758579338699,5983430343285687595
