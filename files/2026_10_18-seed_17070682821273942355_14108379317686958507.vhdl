-- Seed: 17070682821273942355,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity wyxooyezw is
  port (x : linkage std_logic_vector(4 to 2); tiysfjnju : inout std_logic);
end wyxooyezw;

architecture ryozjao of wyxooyezw is
  
begin
  -- Multi-driven assignments
  tiysfjnju <= 'U';
  tiysfjnju <= 'X';
  tiysfjnju <= tiysfjnju;
end ryozjao;

entity whavr is
  port (ut : inout boolean; rcgy : inout boolean);
end whavr;

library ieee;
use ieee.std_logic_1164.all;

architecture wd of whavr is
  signal qkwgdeih : std_logic;
  signal vzwiafr : std_logic_vector(4 to 2);
  signal saiag : std_logic;
  signal stj : std_logic_vector(4 to 2);
begin
  sen : entity work.wyxooyezw
    port map (x => stj, tiysfjnju => saiag);
  ai : entity work.wyxooyezw
    port map (x => vzwiafr, tiysfjnju => saiag);
  olcdvfv : entity work.wyxooyezw
    port map (x => stj, tiysfjnju => saiag);
  mwq : entity work.wyxooyezw
    port map (x => stj, tiysfjnju => qkwgdeih);
  
  -- Single-driven assignments
  rcgy <= rcgy;
  ut <= TRUE;
end wd;

library ieee;
use ieee.std_logic_1164.all;

entity twszhhgst is
  port (gjpsld : in integer; mctqwbzek : out std_logic_vector(0 downto 0); kcfjzfp : inout real; spbprcxxi : inout time);
end twszhhgst;

library ieee;
use ieee.std_logic_1164.all;

architecture lj of twszhhgst is
  signal u : std_logic;
  signal t : std_logic_vector(4 to 2);
begin
  z : entity work.wyxooyezw
    port map (x => t, tiysfjnju => u);
  qmp : entity work.wyxooyezw
    port map (x => t, tiysfjnju => u);
  
  -- Multi-driven assignments
  mctqwbzek <= (others => 'H');
  mctqwbzek <= mctqwbzek;
end lj;



-- Seed after: 12718956499654371300,14108379317686958507
