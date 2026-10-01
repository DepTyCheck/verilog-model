-- Seed: 5977678663052479618,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity edfb is
  port (byze : linkage time; otpjetz : inout std_logic; t : linkage boolean_vector(3 to 4); vmnzsu : in bit);
end edfb;

architecture jumhxsoomr of edfb is
  
begin
  -- Multi-driven assignments
  otpjetz <= otpjetz;
end jumhxsoomr;

library ieee;
use ieee.std_logic_1164.all;

entity hlrzxytdfq is
  port (ykjwbn : buffer integer; tdnsnvjzs : inout character; kozoco : linkage std_logic);
end hlrzxytdfq;

library ieee;
use ieee.std_logic_1164.all;

architecture ybqx of hlrzxytdfq is
  signal otcseesqes : boolean_vector(3 to 4);
  signal nuhcejqq : std_logic;
  signal lmsflrlyvw : time;
  signal ffbelcah : boolean_vector(3 to 4);
  signal v : std_logic;
  signal r : time;
  signal vja : boolean_vector(3 to 4);
  signal bfunksphds : time;
  signal uegbbm : bit;
  signal jypz : boolean_vector(3 to 4);
  signal bvcqpzdexe : std_logic;
  signal nipjoeycnw : time;
begin
  kwdxempn : entity work.edfb
    port map (byze => nipjoeycnw, otpjetz => bvcqpzdexe, t => jypz, vmnzsu => uegbbm);
  lblkzsjpwo : entity work.edfb
    port map (byze => bfunksphds, otpjetz => bvcqpzdexe, t => vja, vmnzsu => uegbbm);
  f : entity work.edfb
    port map (byze => r, otpjetz => v, t => ffbelcah, vmnzsu => uegbbm);
  p : entity work.edfb
    port map (byze => lmsflrlyvw, otpjetz => nuhcejqq, t => otcseesqes, vmnzsu => uegbbm);
  
  -- Single-driven assignments
  tdnsnvjzs <= 'n';
  
  -- Multi-driven assignments
  bvcqpzdexe <= 'Z';
end ybqx;

entity tlaovgi is
  port (idmxc : linkage integer_vector(0 to 2); gommfy : inout integer_vector(0 to 2); ajqctz : out real);
end tlaovgi;

library ieee;
use ieee.std_logic_1164.all;

architecture ywqrrmhsix of tlaovgi is
  signal jhylyixul : bit;
  signal a : boolean_vector(3 to 4);
  signal dvnmkhkpq : std_logic;
  signal ysswjs : time;
  signal ggdgbd : bit;
  signal xxhhmwqykv : boolean_vector(3 to 4);
  signal n : time;
  signal qybdngvl : std_logic;
  signal tspsmnob : character;
  signal kux : integer;
  signal qsr : std_logic;
  signal avw : character;
  signal so : integer;
begin
  xcgrzxdebu : entity work.hlrzxytdfq
    port map (ykjwbn => so, tdnsnvjzs => avw, kozoco => qsr);
  sktuf : entity work.hlrzxytdfq
    port map (ykjwbn => kux, tdnsnvjzs => tspsmnob, kozoco => qybdngvl);
  flennblky : entity work.edfb
    port map (byze => n, otpjetz => qsr, t => xxhhmwqykv, vmnzsu => ggdgbd);
  copqqibg : entity work.edfb
    port map (byze => ysswjs, otpjetz => dvnmkhkpq, t => a, vmnzsu => jhylyixul);
  
  -- Single-driven assignments
  jhylyixul <= ggdgbd;
  ggdgbd <= '1';
  gommfy <= (11, 3_0_0_0_0, 0344);
  ajqctz <= ajqctz;
end ywqrrmhsix;



-- Seed after: 14121616872166635251,15025465285671019065
