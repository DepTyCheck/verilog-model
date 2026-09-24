-- Seed: 11633348951347468712,17234720251424330329

library ieee;
use ieee.std_logic_1164.all;

entity jv is
  port (dg : linkage std_logic_vector(2 downto 2));
end jv;

architecture bsxceannbs of jv is
  
begin
  
end bsxceannbs;

library ieee;
use ieee.std_logic_1164.all;

entity gfu is
  port (uv : in string(2 to 2); ljyhy : inout std_logic; hrehpwxany : buffer time; rzlumbr : inout boolean);
end gfu;

library ieee;
use ieee.std_logic_1164.all;

architecture jeyx of gfu is
  signal lf : std_logic_vector(2 downto 2);
  signal kcgfhvkpmx : std_logic_vector(2 downto 2);
begin
  p : entity work.jv
    port map (dg => kcgfhvkpmx);
  fmogcndrj : entity work.jv
    port map (dg => lf);
  
  -- Single-driven assignments
  hrehpwxany <= 20.431 ns;
  
  -- Multi-driven assignments
  lf <= kcgfhvkpmx;
  ljyhy <= ljyhy;
  lf <= (others => 'U');
end jeyx;



-- Seed after: 10489534709629913265,17234720251424330329
