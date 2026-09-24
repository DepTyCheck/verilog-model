-- Seed: 13825080008069374231,17234720251424330329

library ieee;
use ieee.std_logic_1164.all;

entity svk is
  port (mdit : inout std_logic; ytrzfxquxa : buffer string(5 to 3));
end svk;

architecture rqammwktg of svk is
  
begin
  -- Single-driven assignments
  ytrzfxquxa <= "";
  
  -- Multi-driven assignments
  mdit <= mdit;
  mdit <= mdit;
  mdit <= 'H';
end rqammwktg;

library ieee;
use ieee.std_logic_1164.all;

entity omk is
  port (ryaibvygz : in std_logic; ryilqhtyex : buffer real; hwpfkqcfr : in std_logic; l : out severity_level);
end omk;

library ieee;
use ieee.std_logic_1164.all;

architecture omgo of omk is
  signal uytf : string(5 to 3);
  signal vxya : std_logic;
  signal dqc : string(5 to 3);
  signal zuq : std_logic;
begin
  avc : entity work.svk
    port map (mdit => zuq, ytrzfxquxa => dqc);
  v : entity work.svk
    port map (mdit => vxya, ytrzfxquxa => uytf);
  
  -- Single-driven assignments
  l <= WARNING;
  ryilqhtyex <= 16#F7.8_B_D#;
  
  -- Multi-driven assignments
  vxya <= hwpfkqcfr;
  zuq <= vxya;
  vxya <= '1';
  zuq <= hwpfkqcfr;
end omgo;



-- Seed after: 7011027481916457733,17234720251424330329
