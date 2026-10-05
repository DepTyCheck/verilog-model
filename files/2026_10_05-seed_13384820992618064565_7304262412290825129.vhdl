-- Seed: 13384820992618064565,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity k is
  port (au : out std_logic; qe : inout real);
end k;

architecture ftcntse of k is
  
begin
  -- Single-driven assignments
  qe <= 00.3;
  
  -- Multi-driven assignments
  au <= au;
  au <= 'L';
end ftcntse;

library ieee;
use ieee.std_logic_1164.all;

entity vqkfunjg is
  port (a : in std_logic; sjeunnc : in severity_level);
end vqkfunjg;

library ieee;
use ieee.std_logic_1164.all;

architecture f of vqkfunjg is
  signal ltowets : real;
  signal xeiwisdhxw : std_logic;
  signal nd : real;
  signal kve : std_logic;
begin
  kknvke : entity work.k
    port map (au => kve, qe => nd);
  qjh : entity work.k
    port map (au => xeiwisdhxw, qe => ltowets);
end f;

library ieee;
use ieee.std_logic_1164.all;

entity vvm is
  port (nj : inout std_logic; peekuy : inout std_logic_vector(3 downto 0); tjfmcmz : inout time; huyyzj : linkage time);
end vvm;

architecture gwp of vvm is
  signal bqq : real;
  signal rxmbpvk : real;
begin
  uqbkcsrav : entity work.k
    port map (au => nj, qe => rxmbpvk);
  gk : entity work.k
    port map (au => nj, qe => bqq);
  
  -- Single-driven assignments
  tjfmcmz <= 0_1_2_1_4 ps;
  
  -- Multi-driven assignments
  peekuy <= peekuy;
  nj <= nj;
  peekuy <= peekuy;
end gwp;



-- Seed after: 9277181635668667211,7304262412290825129
