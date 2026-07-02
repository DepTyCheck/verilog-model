-- Seed: 12142139280560522810,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity dzgb is
  port (sptxzggfvc : buffer std_logic);
end dzgb;

architecture vcp of dzgb is
  
begin
  
end vcp;

entity korfrra is
  port (srarqqk : buffer time; ykdopxzoxe : buffer real; bbvmetqi : inout bit);
end korfrra;

library ieee;
use ieee.std_logic_1164.all;

architecture h of korfrra is
  signal xscqd : std_logic;
  signal mx : std_logic;
begin
  ukkl : entity work.dzgb
    port map (sptxzggfvc => mx);
  xfgxdzriis : entity work.dzgb
    port map (sptxzggfvc => xscqd);
  
  -- Single-driven assignments
  bbvmetqi <= '1';
  ykdopxzoxe <= 3.0;
  
  -- Multi-driven assignments
  mx <= 'X';
end h;

library ieee;
use ieee.std_logic_1164.all;

entity emjzuyyjz is
  port (uhafixmr : buffer std_logic_vector(4 to 1); lj : linkage integer; pqtrtjo : out string(3 to 2));
end emjzuyyjz;

library ieee;
use ieee.std_logic_1164.all;

architecture zfuvfb of emjzuyyjz is
  signal xzfnjbir : bit;
  signal ohltyryla : real;
  signal gq : time;
  signal kzoxi : std_logic;
  signal vjsowtql : std_logic;
  signal iziayy : std_logic;
begin
  mqjot : entity work.dzgb
    port map (sptxzggfvc => iziayy);
  sruujsggey : entity work.dzgb
    port map (sptxzggfvc => vjsowtql);
  btjkohnmf : entity work.dzgb
    port map (sptxzggfvc => kzoxi);
  zqzhagmix : entity work.korfrra
    port map (srarqqk => gq, ykdopxzoxe => ohltyryla, bbvmetqi => xzfnjbir);
  
  -- Single-driven assignments
  pqtrtjo <= (others => ' ');
  
  -- Multi-driven assignments
  uhafixmr <= (others => '0');
  iziayy <= 'L';
end zfuvfb;



-- Seed after: 355521590532308112,13694093582652240945
