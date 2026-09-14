-- Seed: 10059999424937160518,13196211255131729027

library ieee;
use ieee.std_logic_1164.all;

entity ehayau is
  port (lfejiq : out integer; jnzsm : inout integer; zysu : buffer time; etazwdzbb : linkage std_logic_vector(4 downto 4));
end ehayau;

architecture cxgkxua of ehayau is
  
begin
  
end cxgkxua;

library ieee;
use ieee.std_logic_1164.all;

entity lyaljf is
  port (qcyfw : out std_logic; gtin : in integer_vector(4 downto 3));
end lyaljf;

architecture fiewq of lyaljf is
  
begin
  
end fiewq;

entity zjxqsird is
  port (usegqbz : linkage time_vector(0 downto 3));
end zjxqsird;

architecture ngoukvinx of zjxqsird is
  
begin
  
end ngoukvinx;

entity cofgbl is
  port (madoglqd : in character; qvzweets : buffer integer; ixhtownepc : linkage boolean_vector(0 to 0); lfl : out real);
end cofgbl;

library ieee;
use ieee.std_logic_1164.all;

architecture zxv of cofgbl is
  signal dlibfdtch : time;
  signal hp : integer;
  signal jhllusjnd : time_vector(0 downto 3);
  signal tdwmkyod : std_logic_vector(4 downto 4);
  signal fjmeap : time;
  signal egummevi : integer;
  signal j : integer;
  signal jknbossj : time_vector(0 downto 3);
begin
  qvc : entity work.zjxqsird
    port map (usegqbz => jknbossj);
  mubifsmnj : entity work.ehayau
    port map (lfejiq => j, jnzsm => egummevi, zysu => fjmeap, etazwdzbb => tdwmkyod);
  gq : entity work.zjxqsird
    port map (usegqbz => jhllusjnd);
  usjqqom : entity work.ehayau
    port map (lfejiq => qvzweets, jnzsm => hp, zysu => dlibfdtch, etazwdzbb => tdwmkyod);
  
  -- Single-driven assignments
  lfl <= 2_4_1_2.3_2_0_4_0;
  
  -- Multi-driven assignments
  tdwmkyod <= "X";
end zxv;



-- Seed after: 1005822787316512612,13196211255131729027
