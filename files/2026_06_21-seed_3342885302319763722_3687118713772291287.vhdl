-- Seed: 3342885302319763722,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity qovxhzmc is
  port (lkuy : inout real; y : inout std_logic_vector(1 downto 0));
end qovxhzmc;

architecture lzpzsw of qovxhzmc is
  
begin
  -- Single-driven assignments
  lkuy <= 1_3_0.2;
end lzpzsw;

entity vuxkeabvfw is
  port (qypqq : out time);
end vuxkeabvfw;

library ieee;
use ieee.std_logic_1164.all;

architecture vdhjvueuoz of vuxkeabvfw is
  signal hdrhr : real;
  signal ibtq : std_logic_vector(1 downto 0);
  signal ourqwl : real;
begin
  lzdgcqb : entity work.qovxhzmc
    port map (lkuy => ourqwl, y => ibtq);
  tf : entity work.qovxhzmc
    port map (lkuy => hdrhr, y => ibtq);
end vdhjvueuoz;

library ieee;
use ieee.std_logic_1164.all;

entity sextsoy is
  port (c : out std_logic_vector(2 to 4); hi : out integer; mwqomo : buffer severity_level; ziglkb : in time);
end sextsoy;

library ieee;
use ieee.std_logic_1164.all;

architecture igbal of sextsoy is
  signal pktnbbwko : std_logic_vector(1 downto 0);
  signal tz : real;
  signal vqeilk : std_logic_vector(1 downto 0);
  signal zmpq : real;
begin
  gr : entity work.qovxhzmc
    port map (lkuy => zmpq, y => vqeilk);
  wjdbhimg : entity work.qovxhzmc
    port map (lkuy => tz, y => pktnbbwko);
  
  -- Single-driven assignments
  mwqomo <= FAILURE;
  hi <= 1;
  
  -- Multi-driven assignments
  c <= ('L', '0', 'U');
  vqeilk <= "LH";
end igbal;

entity tngexjonb is
  port (oos : buffer string(3 downto 2));
end tngexjonb;

architecture gbtngunmtc of tngexjonb is
  signal kszc : time;
begin
  ef : entity work.vuxkeabvfw
    port map (qypqq => kszc);
end gbtngunmtc;



-- Seed after: 7717692257733196916,3687118713772291287
