-- Seed: 7945436106107203986,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity dhb is
  port (zdwwu : inout std_logic; uluppvcl : linkage integer_vector(3 downto 1); w : buffer std_logic);
end dhb;

architecture kn of dhb is
  
begin
  -- Multi-driven assignments
  w <= '1';
  w <= 'Z';
  w <= 'W';
end kn;

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (epgdhdxjof : in std_logic; ntpqfzy : in real; paqz : in time; ktfsm : out integer);
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture jrer of w is
  signal iijoweq : std_logic;
  signal adus : integer_vector(3 downto 1);
  signal zuaz : integer_vector(3 downto 1);
  signal iogxrbxlqs : std_logic;
  signal t : integer_vector(3 downto 1);
  signal jumwlxakft : std_logic;
begin
  vtd : entity work.dhb
    port map (zdwwu => jumwlxakft, uluppvcl => t, w => iogxrbxlqs);
  llhtkdafl : entity work.dhb
    port map (zdwwu => jumwlxakft, uluppvcl => zuaz, w => jumwlxakft);
  nitsj : entity work.dhb
    port map (zdwwu => iogxrbxlqs, uluppvcl => adus, w => iijoweq);
  
  -- Single-driven assignments
  ktfsm <= 1004;
  
  -- Multi-driven assignments
  jumwlxakft <= 'Z';
  jumwlxakft <= 'L';
  jumwlxakft <= 'H';
  jumwlxakft <= 'W';
end jrer;

entity clcxfpon is
  port (ztxyvixnus : inout time);
end clcxfpon;

library ieee;
use ieee.std_logic_1164.all;

architecture idtvof of clcxfpon is
  signal wwuwbzcp : integer_vector(3 downto 1);
  signal llq : integer;
  signal rwk : time;
  signal yis : real;
  signal qulx : integer_vector(3 downto 1);
  signal wk : std_logic;
begin
  dfrxtgvn : entity work.dhb
    port map (zdwwu => wk, uluppvcl => qulx, w => wk);
  kblpbjs : entity work.w
    port map (epgdhdxjof => wk, ntpqfzy => yis, paqz => rwk, ktfsm => llq);
  nu : entity work.dhb
    port map (zdwwu => wk, uluppvcl => wwuwbzcp, w => wk);
  
  -- Single-driven assignments
  yis <= 0.2_3_0_2_4;
  rwk <= 2#0_0_0_1_0# us;
  ztxyvixnus <= 16#110.1# ps;
  
  -- Multi-driven assignments
  wk <= 'U';
  wk <= 'H';
  wk <= '0';
end idtvof;

entity mmhynn is
  port (fxswaaba : in real);
end mmhynn;

library ieee;
use ieee.std_logic_1164.all;

architecture bfed of mmhynn is
  signal ws : std_logic;
  signal kibmyb : integer_vector(3 downto 1);
  signal ukqbxbzosm : std_logic;
begin
  oxdefruq : entity work.dhb
    port map (zdwwu => ukqbxbzosm, uluppvcl => kibmyb, w => ws);
  
  -- Multi-driven assignments
  ws <= 'L';
end bfed;



-- Seed after: 11783134678922330950,1834764876137802293
