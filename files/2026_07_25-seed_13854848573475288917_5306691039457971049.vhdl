-- Seed: 13854848573475288917,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity kzueh is
  port (sgleidc : in std_logic; hnas : inout std_logic);
end kzueh;

architecture pofyrft of kzueh is
  
begin
  -- Multi-driven assignments
  hnas <= 'H';
  hnas <= sgleidc;
end pofyrft;

library ieee;
use ieee.std_logic_1164.all;

entity aqztpy is
  port (myszzbb : in std_logic; xi : inout std_logic);
end aqztpy;

library ieee;
use ieee.std_logic_1164.all;

architecture wjgefoe of aqztpy is
  signal xftuyjfga : std_logic;
begin
  yzd : entity work.kzueh
    port map (sgleidc => xi, hnas => xftuyjfga);
  dgniqvmb : entity work.kzueh
    port map (sgleidc => myszzbb, hnas => xi);
  
  -- Multi-driven assignments
  xi <= xi;
  xi <= 'L';
  xftuyjfga <= xi;
end wjgefoe;

library ieee;
use ieee.std_logic_1164.all;

entity fwmwje is
  port (qmdnshjtnj : buffer std_logic; xyxelab : in real);
end fwmwje;

library ieee;
use ieee.std_logic_1164.all;

architecture zp of fwmwje is
  signal xuyka : std_logic;
  signal gtkqpcgzq : std_logic;
  signal r : std_logic;
begin
  df : entity work.kzueh
    port map (sgleidc => qmdnshjtnj, hnas => r);
  przxme : entity work.kzueh
    port map (sgleidc => gtkqpcgzq, hnas => xuyka);
  
  -- Multi-driven assignments
  gtkqpcgzq <= '-';
  gtkqpcgzq <= qmdnshjtnj;
  gtkqpcgzq <= xuyka;
  qmdnshjtnj <= qmdnshjtnj;
end zp;



-- Seed after: 3745174276747245859,5306691039457971049
