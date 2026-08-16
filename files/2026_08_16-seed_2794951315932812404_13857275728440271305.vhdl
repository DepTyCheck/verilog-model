-- Seed: 2794951315932812404,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (desxcw : linkage std_logic; t : buffer std_logic; mesuvc : in real);
end o;

architecture rt of o is
  
begin
  -- Multi-driven assignments
  t <= 'L';
  t <= 'Z';
  t <= t;
  t <= '1';
end rt;



-- Seed after: 15944123321745344539,13857275728440271305
