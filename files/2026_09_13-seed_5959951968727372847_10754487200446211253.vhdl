-- Seed: 5959951968727372847,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity sopcenzgqr is
  port (zd : out std_logic; unwgfbvqkm : inout std_logic_vector(1 downto 1); n : buffer std_logic);
end sopcenzgqr;

architecture dpbzmb of sopcenzgqr is
  
begin
  -- Multi-driven assignments
  zd <= 'L';
end dpbzmb;

entity jmcegodr is
  port (ohviu : linkage time);
end jmcegodr;

library ieee;
use ieee.std_logic_1164.all;

architecture achndfkk of jmcegodr is
  signal itaowu : std_logic_vector(1 downto 1);
  signal dsnmgw : std_logic;
  signal miljewnic : std_logic_vector(1 downto 1);
  signal d : std_logic;
begin
  tnhkfxua : entity work.sopcenzgqr
    port map (zd => d, unwgfbvqkm => miljewnic, n => dsnmgw);
  cnnjrz : entity work.sopcenzgqr
    port map (zd => d, unwgfbvqkm => miljewnic, n => d);
  cs : entity work.sopcenzgqr
    port map (zd => d, unwgfbvqkm => miljewnic, n => dsnmgw);
  zvlcuahn : entity work.sopcenzgqr
    port map (zd => d, unwgfbvqkm => itaowu, n => d);
  
  -- Multi-driven assignments
  d <= '0';
  dsnmgw <= d;
  d <= 'L';
end achndfkk;



-- Seed after: 11196973892087086208,10754487200446211253
