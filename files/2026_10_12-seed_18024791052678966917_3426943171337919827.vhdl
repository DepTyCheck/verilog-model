-- Seed: 18024791052678966917,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity oolc is
  port (va : buffer std_logic; aw : linkage std_logic; eubu : out std_logic_vector(2 to 1); mlpmsweu : buffer time_vector(4 to 3));
end oolc;

architecture vbmbn of oolc is
  
begin
  -- Multi-driven assignments
  eubu <= "";
  va <= 'W';
  eubu <= eubu;
  eubu <= "";
end vbmbn;

entity pd is
  port (cdwylssyy : buffer integer; pekxuxd : out severity_level);
end pd;

library ieee;
use ieee.std_logic_1164.all;

architecture crspcrum of pd is
  signal hzchaawc : time_vector(4 to 3);
  signal lvwefdjp : std_logic_vector(2 to 1);
  signal wogncvn : std_logic;
  signal j : std_logic;
begin
  tavkgcrjh : entity work.oolc
    port map (va => j, aw => wogncvn, eubu => lvwefdjp, mlpmsweu => hzchaawc);
  
  -- Single-driven assignments
  pekxuxd <= FAILURE;
end crspcrum;

library ieee;
use ieee.std_logic_1164.all;

entity inuz is
  port (hkszy : inout time_vector(3 downto 0); mmvi : buffer time; nbkve : buffer std_logic);
end inuz;

library ieee;
use ieee.std_logic_1164.all;

architecture pvz of inuz is
  signal sysn : time_vector(4 to 3);
  signal dcwiy : std_logic;
  signal hp : time_vector(4 to 3);
  signal mvklsrlv : std_logic_vector(2 to 1);
  signal nu : std_logic;
  signal imcbxq : time_vector(4 to 3);
  signal pnmyo : std_logic_vector(2 to 1);
  signal obonynvq : std_logic;
  signal odx : time_vector(4 to 3);
  signal vxx : std_logic_vector(2 to 1);
  signal b : std_logic;
  signal wtvuntqu : std_logic;
begin
  frg : entity work.oolc
    port map (va => wtvuntqu, aw => b, eubu => vxx, mlpmsweu => odx);
  wqlczbs : entity work.oolc
    port map (va => obonynvq, aw => obonynvq, eubu => pnmyo, mlpmsweu => imcbxq);
  fuvihingur : entity work.oolc
    port map (va => nu, aw => nbkve, eubu => mvklsrlv, mlpmsweu => hp);
  nixe : entity work.oolc
    port map (va => dcwiy, aw => obonynvq, eubu => vxx, mlpmsweu => sysn);
  
  -- Single-driven assignments
  hkszy <= (2#10.00# fs, 42 fs, 4 min, 0_3.1_4_3_0_4 fs);
  
  -- Multi-driven assignments
  nbkve <= '1';
  pnmyo <= "";
end pvz;



-- Seed after: 14776232845537825005,3426943171337919827
