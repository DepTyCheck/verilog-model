-- Seed: 14547390266960285992,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity tyturxd is
  port (zfivf : in severity_level; iv : out time_vector(2 to 1); wgacc : inout std_logic; izm : inout integer);
end tyturxd;

architecture uzxrvf of tyturxd is
  
begin
  -- Single-driven assignments
  iv <= (others => 0 ns);
  izm <= 16#7#;
  
  -- Multi-driven assignments
  wgacc <= 'L';
end uzxrvf;

entity ycuekrkiq is
  port (n : buffer real);
end ycuekrkiq;

library ieee;
use ieee.std_logic_1164.all;

architecture cbolj of ycuekrkiq is
  signal x : integer;
  signal wzaab : std_logic;
  signal hpaqrskxi : time_vector(2 to 1);
  signal jj : severity_level;
begin
  xuqhwdozv : entity work.tyturxd
    port map (zfivf => jj, iv => hpaqrskxi, wgacc => wzaab, izm => x);
  
  -- Single-driven assignments
  n <= 8#14331.7_1_0#;
  jj <= NOTE;
  
  -- Multi-driven assignments
  wzaab <= 'L';
  wzaab <= 'H';
  wzaab <= 'W';
end cbolj;



-- Seed after: 14347614160410391533,10557070023141912087
