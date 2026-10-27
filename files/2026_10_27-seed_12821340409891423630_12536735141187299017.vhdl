-- Seed: 12821340409891423630,12536735141187299017

library ieee;
use ieee.std_logic_1164.all;

entity fsk is
  port (obo : out real; gmeyiiqrda : inout integer_vector(1 to 3); lzyil : in time; icvmyrkbby : buffer std_logic);
end fsk;

architecture yiprihxms of fsk is
  
begin
  -- Single-driven assignments
  gmeyiiqrda <= gmeyiiqrda;
  obo <= obo;
end yiprihxms;

entity yogmoa is
  port (rvlysce : linkage real);
end yogmoa;

library ieee;
use ieee.std_logic_1164.all;

architecture vyvuh of yogmoa is
  signal zhuuvhhlls : std_logic;
  signal szaghvevoj : time;
  signal vsndikiix : integer_vector(1 to 3);
  signal cfd : real;
  signal pjea : std_logic;
  signal gdcjyen : integer_vector(1 to 3);
  signal prj : real;
  signal nphttvpqq : time;
  signal aqbdwlqzgz : integer_vector(1 to 3);
  signal l : real;
  signal hplqyh : std_logic;
  signal tjmkzrimwl : time;
  signal pxpjwayjak : integer_vector(1 to 3);
  signal umgibfy : real;
begin
  bo : entity work.fsk
    port map (obo => umgibfy, gmeyiiqrda => pxpjwayjak, lzyil => tjmkzrimwl, icvmyrkbby => hplqyh);
  xbhmimyeko : entity work.fsk
    port map (obo => l, gmeyiiqrda => aqbdwlqzgz, lzyil => nphttvpqq, icvmyrkbby => hplqyh);
  kcz : entity work.fsk
    port map (obo => prj, gmeyiiqrda => gdcjyen, lzyil => tjmkzrimwl, icvmyrkbby => pjea);
  vqellzst : entity work.fsk
    port map (obo => cfd, gmeyiiqrda => vsndikiix, lzyil => szaghvevoj, icvmyrkbby => zhuuvhhlls);
  
  -- Multi-driven assignments
  pjea <= hplqyh;
  hplqyh <= pjea;
end vyvuh;



-- Seed after: 14894666006452220973,12536735141187299017
