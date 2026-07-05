-- Seed: 18408227684965555601,3181554006726329157

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity e is
  port (zokal : out std_logic; dp : inout floating_subtype_mirror; je : out real);
end e;

architecture os of e is
  
begin
  -- Multi-driven assignments
  zokal <= '1';
  zokal <= zokal;
  zokal <= 'Z';
end os;



-- Seed after: 13267663767179927957,3181554006726329157
