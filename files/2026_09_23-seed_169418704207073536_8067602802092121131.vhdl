-- Seed: 169418704207073536,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity uw is
  port (d : in std_logic_vector(0 to 2); acnzfj : out std_logic);
end uw;

architecture n of uw is
  
begin
  -- Multi-driven assignments
  acnzfj <= acnzfj;
  acnzfj <= acnzfj;
  acnzfj <= 'W';
  acnzfj <= acnzfj;
end n;

library ieee;
use ieee.std_logic_1164.all;

entity etinuzosc is
  port (mzzldm : linkage std_logic_vector(4 to 0); v : buffer time; tuzshj : buffer std_logic; jahcoibbn : inout std_logic_vector(2 to 2));
end etinuzosc;

library ieee;
use ieee.std_logic_1164.all;

architecture gndcu of etinuzosc is
  signal dhmfzassso : std_logic;
  signal hlqadhv : std_logic_vector(0 to 2);
  signal lkkvff : std_logic;
  signal imrti : std_logic_vector(0 to 2);
begin
  q : entity work.uw
    port map (d => imrti, acnzfj => lkkvff);
  rtsujef : entity work.uw
    port map (d => hlqadhv, acnzfj => tuzshj);
  wte : entity work.uw
    port map (d => imrti, acnzfj => dhmfzassso);
  un : entity work.uw
    port map (d => imrti, acnzfj => dhmfzassso);
  
  -- Single-driven assignments
  v <= 1_3.0 us;
end gndcu;

entity cnibmqt is
  port (vdn : in boolean);
end cnibmqt;

library ieee;
use ieee.std_logic_1164.all;

architecture roxy of cnibmqt is
  signal qgntb : std_logic_vector(0 to 2);
  signal yt : std_logic_vector(0 to 2);
  signal wrlbibejx : std_logic_vector(2 to 2);
  signal aotdumpm : std_logic;
  signal yebvzqr : time;
  signal pfbodxik : std_logic_vector(4 to 0);
begin
  kujckkb : entity work.etinuzosc
    port map (mzzldm => pfbodxik, v => yebvzqr, tuzshj => aotdumpm, jahcoibbn => wrlbibejx);
  q : entity work.uw
    port map (d => yt, acnzfj => aotdumpm);
  ttrwisjxwy : entity work.uw
    port map (d => qgntb, acnzfj => aotdumpm);
  
  -- Multi-driven assignments
  wrlbibejx <= wrlbibejx;
end roxy;



-- Seed after: 11942277459566998191,8067602802092121131
