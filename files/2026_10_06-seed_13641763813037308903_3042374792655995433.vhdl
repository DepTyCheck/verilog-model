-- Seed: 13641763813037308903,3042374792655995433

library ieee;
use ieee.std_logic_1164.all;

entity lzgqc is
  port (otqnloz : buffer std_logic; iv : inout time_vector(2 to 2); sqm : out severity_level);
end lzgqc;

architecture a of lzgqc is
  
begin
  -- Single-driven assignments
  iv <= (others => 16#FD# us);
  sqm <= sqm;
  
  -- Multi-driven assignments
  otqnloz <= 'H';
  otqnloz <= otqnloz;
  otqnloz <= otqnloz;
end a;



-- Seed after: 11111262108593982300,3042374792655995433
