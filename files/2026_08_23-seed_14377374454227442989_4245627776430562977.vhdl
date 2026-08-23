-- Seed: 14377374454227442989,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity plf is
  port (h : in real_vector(0 to 2); ren : linkage std_logic_vector(0 downto 2); aq : linkage std_logic_vector(2 downto 0); ufxpw : buffer real);
end plf;

architecture asobgks of plf is
  
begin
  -- Single-driven assignments
  ufxpw <= 0.323;
end asobgks;

library ieee;
use ieee.std_logic_1164.all;

entity yqwbpq is
  port (fhnmoe : inout std_logic_vector(4 downto 2));
end yqwbpq;

library ieee;
use ieee.std_logic_1164.all;

architecture wbrn of yqwbpq is
  signal ub : real;
  signal podzz : std_logic_vector(2 downto 0);
  signal rautuc : real;
  signal vq : std_logic_vector(0 downto 2);
  signal nxxu : real;
  signal itf : std_logic_vector(0 downto 2);
  signal uqm : real_vector(0 to 2);
  signal ryqtzgmwj : real;
  signal cz : std_logic_vector(0 downto 2);
  signal rjk : real_vector(0 to 2);
begin
  knxol : entity work.plf
    port map (h => rjk, ren => cz, aq => fhnmoe, ufxpw => ryqtzgmwj);
  z : entity work.plf
    port map (h => uqm, ren => itf, aq => fhnmoe, ufxpw => nxxu);
  emgwfmx : entity work.plf
    port map (h => uqm, ren => vq, aq => fhnmoe, ufxpw => rautuc);
  ne : entity work.plf
    port map (h => uqm, ren => vq, aq => podzz, ufxpw => ub);
  
  -- Single-driven assignments
  rjk <= uqm;
  uqm <= rjk;
  
  -- Multi-driven assignments
  itf <= vq;
  fhnmoe <= "X0L";
  podzz <= fhnmoe;
end wbrn;



-- Seed after: 5971025381026928566,4245627776430562977
