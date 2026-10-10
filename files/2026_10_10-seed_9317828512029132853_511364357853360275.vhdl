-- Seed: 9317828512029132853,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity uxtpvatby is
  port (tyzhqicmr : linkage std_logic_vector(1 to 3));
end uxtpvatby;

architecture rjcfgun of uxtpvatby is
  
begin
  
end rjcfgun;

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (ubjm : out real; vfolobz : out std_logic_vector(0 to 4); pkzbei : inout severity_level);
end u;

library ieee;
use ieee.std_logic_1164.all;

architecture vcx of u is
  signal tdt : std_logic_vector(1 to 3);
  signal n : std_logic_vector(1 to 3);
begin
  fadud : entity work.uxtpvatby
    port map (tyzhqicmr => n);
  bsyptupi : entity work.uxtpvatby
    port map (tyzhqicmr => n);
  nwwu : entity work.uxtpvatby
    port map (tyzhqicmr => n);
  jrv : entity work.uxtpvatby
    port map (tyzhqicmr => tdt);
  
  -- Single-driven assignments
  ubjm <= 1.2;
  
  -- Multi-driven assignments
  vfolobz <= vfolobz;
  vfolobz <= ('0', 'Z', '-', 'H', '1');
  vfolobz <= ('-', '1', 'H', 'U', 'L');
  vfolobz <= ('H', '-', '-', '1', 'U');
end vcx;



-- Seed after: 9811949355938954066,511364357853360275
