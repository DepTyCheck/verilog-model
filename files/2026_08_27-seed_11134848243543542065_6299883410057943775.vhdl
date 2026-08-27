-- Seed: 11134848243543542065,6299883410057943775

entity cyfshcp is
  port (mvvjm : in integer_vector(3 downto 1); fru : in boolean_vector(0 downto 4); imrmmvc : inout bit_vector(2 downto 1));
end cyfshcp;

architecture a of cyfshcp is
  
begin
  -- Single-driven assignments
  imrmmvc <= ('0', '0');
end a;

library ieee;
use ieee.std_logic_1164.all;

entity eb is
  port (pxgnois : linkage std_logic; kvswnt : linkage severity_level; xntxw : in character);
end eb;

architecture vsofm of eb is
  signal obdpidlbf : bit_vector(2 downto 1);
  signal iukqsgevgq : boolean_vector(0 downto 4);
  signal vdqyx : integer_vector(3 downto 1);
begin
  jveww : entity work.cyfshcp
    port map (mvvjm => vdqyx, fru => iukqsgevgq, imrmmvc => obdpidlbf);
end vsofm;



-- Seed after: 4806435832267495954,6299883410057943775
