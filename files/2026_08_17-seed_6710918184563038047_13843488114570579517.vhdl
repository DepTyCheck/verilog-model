-- Seed: 6710918184563038047,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity mijg is
  port (rnqjfotc : linkage std_logic; eep : buffer bit_vector(1 downto 1));
end mijg;

architecture ftetcr of mijg is
  
begin
  -- Single-driven assignments
  eep <= eep;
end ftetcr;

library ieee;
use ieee.std_logic_1164.all;

entity uqaaf is
  port (u : inout time; y : inout std_logic; f : linkage std_logic_vector(4 to 4); inirroc : in time);
end uqaaf;

library ieee;
use ieee.std_logic_1164.all;

architecture sf of uqaaf is
  signal rssevt : bit_vector(1 downto 1);
  signal wppr : std_logic;
begin
  m : entity work.mijg
    port map (rnqjfotc => wppr, eep => rssevt);
  
  -- Single-driven assignments
  u <= 3 sec;
end sf;

entity zxddscyiox is
  port (ns : linkage bit);
end zxddscyiox;

library ieee;
use ieee.std_logic_1164.all;

architecture mnmzwejzn of zxddscyiox is
  signal y : bit_vector(1 downto 1);
  signal ksjjico : bit_vector(1 downto 1);
  signal vlrrdz : std_logic;
  signal jajhcm : std_logic_vector(4 to 4);
  signal bzgr : std_logic;
  signal tbirqrl : time;
begin
  qsrwarwb : entity work.uqaaf
    port map (u => tbirqrl, y => bzgr, f => jajhcm, inirroc => tbirqrl);
  gdyliia : entity work.mijg
    port map (rnqjfotc => vlrrdz, eep => ksjjico);
  epylztk : entity work.mijg
    port map (rnqjfotc => bzgr, eep => y);
  
  -- Multi-driven assignments
  jajhcm <= (others => 'U');
  bzgr <= bzgr;
  bzgr <= 'W';
end mnmzwejzn;



-- Seed after: 9151643089228138376,13843488114570579517
