-- Seed: 10623655864302197607,5444032936641997477

library ieee;
use ieee.std_logic_1164.all;

entity mc is
  port (ix : in integer; om : buffer std_logic_vector(4 downto 2); pxrfo : buffer character; uxemhthbly : linkage integer);
end mc;

architecture y of mc is
  
begin
  -- Single-driven assignments
  pxrfo <= 'p';
  
  -- Multi-driven assignments
  om <= ('X', 'W', '0');
  om <= om;
  om <= ('0', 'Z', '-');
end y;

library ieee;
use ieee.std_logic_1164.all;

entity fkv is
  port (og : buffer real_vector(4 downto 3); ecaibhvrz : inout std_logic_vector(3 downto 4));
end fkv;

library ieee;
use ieee.std_logic_1164.all;

architecture hyby of fkv is
  signal o : integer;
  signal efopcuusy : character;
  signal qedgutem : std_logic_vector(4 downto 2);
  signal vjmpc : integer;
begin
  bum : entity work.mc
    port map (ix => vjmpc, om => qedgutem, pxrfo => efopcuusy, uxemhthbly => o);
  
  -- Single-driven assignments
  vjmpc <= 2#000#;
  og <= (1_4.3313, 16#0D00.B_7_A_5#);
  
  -- Multi-driven assignments
  qedgutem <= "Z-W";
  ecaibhvrz <= ecaibhvrz;
end hyby;

entity zebcb is
  port (rjuhisb : in time; j : in time; hfzeancz : buffer time; tismw : linkage time);
end zebcb;

library ieee;
use ieee.std_logic_1164.all;

architecture vshhkwih of zebcb is
  signal siwvgg : integer;
  signal aizx : character;
  signal vbj : std_logic_vector(3 downto 4);
  signal geuixbppr : real_vector(4 downto 3);
  signal qgylciy : std_logic_vector(3 downto 4);
  signal txnygv : real_vector(4 downto 3);
  signal dzmif : integer;
  signal w : character;
  signal o : std_logic_vector(4 downto 2);
  signal h : integer;
begin
  eijx : entity work.mc
    port map (ix => h, om => o, pxrfo => w, uxemhthbly => dzmif);
  vyqxgudsr : entity work.fkv
    port map (og => txnygv, ecaibhvrz => qgylciy);
  urrsvhkbzu : entity work.fkv
    port map (og => geuixbppr, ecaibhvrz => vbj);
  fm : entity work.mc
    port map (ix => dzmif, om => o, pxrfo => aizx, uxemhthbly => siwvgg);
  
  -- Multi-driven assignments
  o <= ('L', '-', 'X');
end vshhkwih;



-- Seed after: 4053207822293032662,5444032936641997477
