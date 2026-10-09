-- Seed: 198833535001914147,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity geu is
  port (wal : inout time; bvtwaggqk : inout time; hwinenlsl : buffer std_logic);
end geu;

architecture nhqm of geu is
  
begin
  -- Single-driven assignments
  bvtwaggqk <= 2#101# ms;
  
  -- Multi-driven assignments
  hwinenlsl <= '-';
  hwinenlsl <= 'X';
end nhqm;

library ieee;
use ieee.std_logic_1164.all;

entity tkpn is
  port (zcqkcegs : inout std_logic);
end tkpn;

library ieee;
use ieee.std_logic_1164.all;

architecture konkdi of tkpn is
  signal ceg : time;
  signal wrhi : time;
  signal mtapfjswoo : std_logic;
  signal tu : time;
  signal epv : time;
begin
  zelgzn : entity work.geu
    port map (wal => epv, bvtwaggqk => tu, hwinenlsl => mtapfjswoo);
  ceeeoohec : entity work.geu
    port map (wal => wrhi, bvtwaggqk => ceg, hwinenlsl => mtapfjswoo);
end konkdi;

library ieee;
use ieee.std_logic_1164.all;

entity jct is
  port (mnjxyu : in real; vkrxiluolh : inout real; htclfqqdob : buffer std_logic_vector(0 downto 3));
end jct;

library ieee;
use ieee.std_logic_1164.all;

architecture jszuastxwk of jct is
  signal ubxyxzhfwh : time;
  signal ywn : time;
  signal x : std_logic;
  signal htboz : time;
  signal ygtlirk : time;
  signal dipgangqpl : std_logic;
  signal qgrc : time;
  signal wvhz : time;
begin
  bkbpbjx : entity work.geu
    port map (wal => wvhz, bvtwaggqk => qgrc, hwinenlsl => dipgangqpl);
  kxcbrbaxe : entity work.geu
    port map (wal => ygtlirk, bvtwaggqk => htboz, hwinenlsl => x);
  dmzbhux : entity work.geu
    port map (wal => ywn, bvtwaggqk => ubxyxzhfwh, hwinenlsl => dipgangqpl);
  
  -- Single-driven assignments
  vkrxiluolh <= vkrxiluolh;
  
  -- Multi-driven assignments
  htclfqqdob <= htclfqqdob;
  x <= dipgangqpl;
  dipgangqpl <= dipgangqpl;
  x <= dipgangqpl;
end jszuastxwk;

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (l : buffer std_logic; dtkesctpu : out time);
end i;

library ieee;
use ieee.std_logic_1164.all;

architecture hyixt of i is
  signal hjiu : time;
  signal va : std_logic_vector(0 downto 3);
  signal mbbs : real;
  signal qrkalazuah : time;
  signal f : time;
  signal ihyonxj : std_logic_vector(0 downto 3);
  signal h : real;
begin
  cdbgqdwcu : entity work.jct
    port map (mnjxyu => h, vkrxiluolh => h, htclfqqdob => ihyonxj);
  vxysoh : entity work.geu
    port map (wal => f, bvtwaggqk => qrkalazuah, hwinenlsl => l);
  j : entity work.jct
    port map (mnjxyu => mbbs, vkrxiluolh => mbbs, htclfqqdob => va);
  lrr : entity work.geu
    port map (wal => hjiu, bvtwaggqk => dtkesctpu, hwinenlsl => l);
end hyixt;



-- Seed after: 5089277448969123782,8891552411914730853
