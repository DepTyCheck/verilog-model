-- Seed: 4130714770299261193,9900838946149614537

entity qmyfdsptzn is
  port (ictiqg : in severity_level; eikcfwydhu : out time);
end qmyfdsptzn;

architecture l of qmyfdsptzn is
  
begin
  
end l;

library ieee;
use ieee.std_logic_1164.all;

entity jcd is
  port (lv : buffer std_logic_vector(3 to 4); o : in real);
end jcd;

architecture atbrzln of jcd is
  signal nxscqf : time;
  signal a : severity_level;
begin
  lmbhnshm : entity work.qmyfdsptzn
    port map (ictiqg => a, eikcfwydhu => nxscqf);
  
  -- Multi-driven assignments
  lv <= lv;
  lv <= lv;
end atbrzln;

library ieee;
use ieee.std_logic_1164.all;

entity pcrtvj is
  port (kq : linkage std_logic_vector(0 downto 3); uihfs : buffer time);
end pcrtvj;

library ieee;
use ieee.std_logic_1164.all;

architecture bzh of pcrtvj is
  signal lsbyehe : real;
  signal vtulpzdail : std_logic_vector(3 to 4);
  signal zjaa : severity_level;
  signal yoeywxge : time;
  signal aukryflvjd : severity_level;
begin
  vu : entity work.qmyfdsptzn
    port map (ictiqg => aukryflvjd, eikcfwydhu => yoeywxge);
  cyhtfkethm : entity work.qmyfdsptzn
    port map (ictiqg => zjaa, eikcfwydhu => uihfs);
  aipcuaan : entity work.jcd
    port map (lv => vtulpzdail, o => lsbyehe);
  
  -- Single-driven assignments
  aukryflvjd <= FAILURE;
  lsbyehe <= lsbyehe;
  zjaa <= ERROR;
  
  -- Multi-driven assignments
  vtulpzdail <= "XZ";
  vtulpzdail <= ('X', 'W');
  vtulpzdail <= "01";
  vtulpzdail <= vtulpzdail;
end bzh;



-- Seed after: 2578059785770058216,9900838946149614537
