-- Seed: 15830961470260224720,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity atf is
  port (nrfhknro : in std_logic_vector(2 downto 1));
end atf;

architecture fqrldt of atf is
  
begin
  
end fqrldt;

entity mxppnf is
  port (rls : inout bit);
end mxppnf;

library ieee;
use ieee.std_logic_1164.all;

architecture fba of mxppnf is
  signal xmxhp : std_logic_vector(2 downto 1);
  signal skanmm : std_logic_vector(2 downto 1);
begin
  yovxjo : entity work.atf
    port map (nrfhknro => skanmm);
  ktdiuhsiv : entity work.atf
    port map (nrfhknro => xmxhp);
  
  -- Single-driven assignments
  rls <= rls;
  
  -- Multi-driven assignments
  xmxhp <= skanmm;
end fba;

library ieee;
use ieee.std_logic_1164.all;

entity kxxzygq is
  port (ndacdpt : out std_logic_vector(1 downto 4); hbwpzjid : inout time);
end kxxzygq;

library ieee;
use ieee.std_logic_1164.all;

architecture zqavhxscy of kxxzygq is
  signal mcrfyiszj : std_logic_vector(2 downto 1);
  signal fdinnnz : std_logic_vector(2 downto 1);
begin
  awoda : entity work.atf
    port map (nrfhknro => fdinnnz);
  kqblntt : entity work.atf
    port map (nrfhknro => mcrfyiszj);
  pvovhon : entity work.atf
    port map (nrfhknro => fdinnnz);
  
  -- Single-driven assignments
  hbwpzjid <= hbwpzjid;
  
  -- Multi-driven assignments
  ndacdpt <= "";
end zqavhxscy;



-- Seed after: 13382755014593969835,14141408946471626091
