-- Seed: 14607966081103175907,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity lndmtzbob is
  port (hgi : out std_logic; x : buffer integer; vs : buffer std_logic_vector(2 to 1));
end lndmtzbob;

architecture o of lndmtzbob is
  
begin
  -- Single-driven assignments
  x <= x;
  
  -- Multi-driven assignments
  hgi <= 'Z';
  vs <= "";
end o;



-- Seed after: 13278118098931929426,5805648483995786113
