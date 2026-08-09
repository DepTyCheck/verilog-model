-- Seed: 6660909677413916969,10871023049702252113

library ieee;
use ieee.std_logic_1164.all;

entity pwrw is
  port (ljk : in std_logic_vector(2 to 1));
end pwrw;

architecture obbubsral of pwrw is
  
begin
  
end obbubsral;

library ieee;
use ieee.std_logic_1164.all;

entity nlboxq is
  port (lzzjfmz : out time_vector(0 to 4); oorajvnv : out std_logic_vector(2 to 2));
end nlboxq;

architecture ywxjangofd of nlboxq is
  
begin
  -- Single-driven assignments
  lzzjfmz <= lzzjfmz;
  
  -- Multi-driven assignments
  oorajvnv <= "W";
  oorajvnv <= "U";
  oorajvnv <= oorajvnv;
  oorajvnv <= "Z";
end ywxjangofd;

library ieee;
use ieee.std_logic_1164.all;

entity ekeuuv is
  port (luvahnxnf : linkage std_logic_vector(4 to 4); ctfy : linkage time; qc : linkage real; xi : in time);
end ekeuuv;

library ieee;
use ieee.std_logic_1164.all;

architecture db of ekeuuv is
  signal qt : std_logic_vector(2 to 2);
  signal axyoix : time_vector(0 to 4);
  signal uoz : std_logic_vector(2 to 1);
begin
  cx : entity work.pwrw
    port map (ljk => uoz);
  yowhbaqh : entity work.pwrw
    port map (ljk => uoz);
  pq : entity work.nlboxq
    port map (lzzjfmz => axyoix, oorajvnv => qt);
end db;



-- Seed after: 15094865982503303384,10871023049702252113
