-- Seed: 11201329678657151230,16159265764638711791

library ieee;
use ieee.std_logic_1164.all;

entity dhtg is
  port (rq : out time; acwrxrwam : out time; svbxtzrko : in bit; yru : inout std_logic_vector(3 downto 0));
end dhtg;

architecture hi of dhtg is
  
begin
  -- Single-driven assignments
  rq <= acwrxrwam;
  acwrxrwam <= 4_1 ns;
  
  -- Multi-driven assignments
  yru <= ('-', '-', 'L', 'Z');
  yru <= ('L', 'W', 'W', '-');
end hi;



-- Seed after: 10721225270873859843,16159265764638711791
