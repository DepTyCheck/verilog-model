-- Seed: 7934254805892806086,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity vuibr is
  port (a : buffer time; hw : buffer boolean; bget : buffer std_logic_vector(3 to 0); fdeb : out character);
end vuibr;

architecture hsatdb of vuibr is
  
begin
  -- Single-driven assignments
  fdeb <= fdeb;
  hw <= FALSE;
  a <= a;
  
  -- Multi-driven assignments
  bget <= "";
  bget <= (others => '0');
  bget <= "";
end hsatdb;



-- Seed after: 17928354754217799740,5805648483995786113
