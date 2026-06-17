-- Seed: 5313240573310443963,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (ousghwnmrn : linkage integer; jgjyd : out time; ahrknyvb : inout std_logic_vector(4 downto 3));
end f;

architecture tci of f is
  
begin
  -- Single-driven assignments
  jgjyd <= 2_2_1_3_1.1_4 ms;
  
  -- Multi-driven assignments
  ahrknyvb <= "L-";
  ahrknyvb <= "WH";
  ahrknyvb <= "UZ";
  ahrknyvb <= ('H', 'W');
end tci;

entity cgxegksei is
  port (uq : buffer time);
end cgxegksei;

library ieee;
use ieee.std_logic_1164.all;

architecture fl of cgxegksei is
  signal nt : std_logic_vector(4 downto 3);
  signal boi : integer;
begin
  wshtfu : entity work.f
    port map (ousghwnmrn => boi, jgjyd => uq, ahrknyvb => nt);
end fl;



-- Seed after: 13729904050278130244,10557070023141912087
