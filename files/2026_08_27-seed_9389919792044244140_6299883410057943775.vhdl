-- Seed: 9389919792044244140,6299883410057943775

library ieee;
use ieee.std_logic_1164.all;

entity ljuqvyg is
  port (u : linkage bit_vector(1 downto 3); xi : buffer integer; pkuxnjcrki : out std_logic_vector(0 to 2));
end ljuqvyg;

architecture cm of ljuqvyg is
  
begin
  
end cm;

entity tfsolqj is
  port (vmdagcq : buffer integer; kmjyxi : out integer);
end tfsolqj;

library ieee;
use ieee.std_logic_1164.all;

architecture ifdkveetcf of tfsolqj is
  signal kiyvlodrl : std_logic_vector(0 to 2);
  signal u : bit_vector(1 downto 3);
  signal xtukcqju : integer;
  signal oxcppf : bit_vector(1 downto 3);
  signal rm : bit_vector(1 downto 3);
  signal vu : std_logic_vector(0 to 2);
  signal hmqdos : integer;
  signal jgzrvrzj : bit_vector(1 downto 3);
begin
  k : entity work.ljuqvyg
    port map (u => jgzrvrzj, xi => hmqdos, pkuxnjcrki => vu);
  lbxkc : entity work.ljuqvyg
    port map (u => rm, xi => vmdagcq, pkuxnjcrki => vu);
  cinvlqqz : entity work.ljuqvyg
    port map (u => oxcppf, xi => xtukcqju, pkuxnjcrki => vu);
  fe : entity work.ljuqvyg
    port map (u => u, xi => kmjyxi, pkuxnjcrki => kiyvlodrl);
  
  -- Multi-driven assignments
  vu <= ('1', 'H', 'U');
end ifdkveetcf;



-- Seed after: 13249928712037365964,6299883410057943775
