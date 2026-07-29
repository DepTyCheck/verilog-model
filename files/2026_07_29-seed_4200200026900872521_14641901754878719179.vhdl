-- Seed: 4200200026900872521,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity qk is
  port (hlp : buffer integer; lmz : inout string(2 downto 3); xczc : in std_logic);
end qk;

architecture t of qk is
  
begin
  -- Single-driven assignments
  lmz <= lmz;
  hlp <= hlp;
end t;

entity ierkbwicb is
  port (au : in bit_vector(2 to 4); lxaqyir : in boolean_vector(3 downto 3));
end ierkbwicb;

library ieee;
use ieee.std_logic_1164.all;

architecture b of ierkbwicb is
  signal dzobbd : std_logic;
  signal zuhnrtgkkb : string(2 downto 3);
  signal udvcd : integer;
  signal wggyxxrmp : std_logic;
  signal grlxqzx : string(2 downto 3);
  signal giv : integer;
  signal pcwwl : std_logic;
  signal fsup : string(2 downto 3);
  signal fw : integer;
  signal jelus : std_logic;
  signal lkvysoo : string(2 downto 3);
  signal gtzpv : integer;
begin
  sdipt : entity work.qk
    port map (hlp => gtzpv, lmz => lkvysoo, xczc => jelus);
  arywqs : entity work.qk
    port map (hlp => fw, lmz => fsup, xczc => pcwwl);
  duy : entity work.qk
    port map (hlp => giv, lmz => grlxqzx, xczc => wggyxxrmp);
  wwnxoq : entity work.qk
    port map (hlp => udvcd, lmz => zuhnrtgkkb, xczc => dzobbd);
  
  -- Multi-driven assignments
  jelus <= jelus;
  jelus <= 'Z';
  jelus <= 'H';
end b;

library ieee;
use ieee.std_logic_1164.all;

entity hh is
  port (ts : in std_logic_vector(1 to 2); wtm : out std_logic);
end hh;

library ieee;
use ieee.std_logic_1164.all;

architecture hqbb of hh is
  signal evgotj : string(2 downto 3);
  signal vkaivjxxbw : integer;
  signal ouviwtakxm : string(2 downto 3);
  signal slvfk : integer;
  signal cpcsobubu : boolean_vector(3 downto 3);
  signal cw : bit_vector(2 to 4);
  signal eaaaylsro : std_logic;
  signal kxtqcqo : string(2 downto 3);
  signal zaoq : integer;
begin
  hbdbnrwpc : entity work.qk
    port map (hlp => zaoq, lmz => kxtqcqo, xczc => eaaaylsro);
  vehusb : entity work.ierkbwicb
    port map (au => cw, lxaqyir => cpcsobubu);
  iogzqrekmn : entity work.qk
    port map (hlp => slvfk, lmz => ouviwtakxm, xczc => wtm);
  snzg : entity work.qk
    port map (hlp => vkaivjxxbw, lmz => evgotj, xczc => wtm);
  
  -- Single-driven assignments
  cw <= cw;
  cpcsobubu <= (others => TRUE);
  
  -- Multi-driven assignments
  wtm <= wtm;
  eaaaylsro <= 'X';
  eaaaylsro <= '1';
end hqbb;



-- Seed after: 12357326149628104426,14641901754878719179
