-- Seed: 5060455193999658649,5511103086789671269

library ieee;
use ieee.std_logic_1164.all;

entity rhohk is
  port (zxnam : linkage bit_vector(1 to 3); kpeialzf : out real; o : in std_logic_vector(4 to 3); et : buffer std_logic);
end rhohk;

architecture loa of rhohk is
  
begin
  -- Single-driven assignments
  kpeialzf <= 8#6_1_5.4#;
  
  -- Multi-driven assignments
  et <= et;
  et <= 'Z';
end loa;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (snqrpatpx : linkage real; dpkgbe : inout std_logic_vector(1 to 2));
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture t of r is
  signal szepru : real;
  signal dqpxgonv : bit_vector(1 to 3);
  signal svshi : std_logic_vector(4 to 3);
  signal rr : real;
  signal lepl : bit_vector(1 to 3);
  signal ntkq : std_logic;
  signal ezbgkt : std_logic_vector(4 to 3);
  signal xeipzw : real;
  signal sgtnkiknjz : bit_vector(1 to 3);
begin
  vxpm : entity work.rhohk
    port map (zxnam => sgtnkiknjz, kpeialzf => xeipzw, o => ezbgkt, et => ntkq);
  sriulwznk : entity work.rhohk
    port map (zxnam => lepl, kpeialzf => rr, o => svshi, et => ntkq);
  pcnnvwrxlp : entity work.rhohk
    port map (zxnam => dqpxgonv, kpeialzf => szepru, o => ezbgkt, et => ntkq);
  
  -- Multi-driven assignments
  dpkgbe <= ('Z', '0');
  ntkq <= 'H';
  ezbgkt <= (others => '0');
  ntkq <= 'X';
end t;



-- Seed after: 17346326607791316766,5511103086789671269
