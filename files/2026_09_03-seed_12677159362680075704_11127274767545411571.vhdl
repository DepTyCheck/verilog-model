-- Seed: 12677159362680075704,11127274767545411571

library ieee;
use ieee.std_logic_1164.all;

entity bpjdx is
  port (ic : out std_logic_vector(2 to 4));
end bpjdx;

architecture rle of bpjdx is
  
begin
  -- Multi-driven assignments
  ic <= ic;
  ic <= ('U', 'L', '-');
  ic <= ic;
  ic <= ('Z', '1', 'W');
end rle;

entity y is
  port (tefktoi : in severity_level);
end y;

library ieee;
use ieee.std_logic_1164.all;

architecture rxvmn of y is
  signal xcvfidedgi : std_logic_vector(2 to 4);
begin
  bawwkmxo : entity work.bpjdx
    port map (ic => xcvfidedgi);
  irelvrudpl : entity work.bpjdx
    port map (ic => xcvfidedgi);
end rxvmn;

entity mnbquzw is
  port (gf : inout time; twb : linkage real_vector(2 downto 4));
end mnbquzw;

architecture rnienv of mnbquzw is
  signal dc : severity_level;
  signal ik : severity_level;
begin
  lys : entity work.y
    port map (tefktoi => ik);
  mfsdqvdjr : entity work.y
    port map (tefktoi => ik);
  ssweefa : entity work.y
    port map (tefktoi => dc);
  
  -- Single-driven assignments
  gf <= 2#0111.11111# ns;
  ik <= ik;
end rnienv;



-- Seed after: 2083097651619832114,11127274767545411571
