-- Seed: 2056819969388202847,18424117564733761959

library ieee;
use ieee.std_logic_1164.all;

entity ufrjwyp is
  port (a : out integer; qztayho : buffer std_logic; fkanpysp : linkage character; cpzkmtx : linkage real);
end ufrjwyp;



architecture shqpjc of ufrjwyp is
  
begin
  
end shqpjc;

library ieee;
use ieee.std_logic_1164.all;

entity btrm is
  port (vprqw : out integer; pvyu : out std_logic; kwuuansfxt : inout character);
end btrm;

library ieee;
use ieee.std_logic_1164.all;

architecture dimbmnlk of btrm is
  signal sh : real;
  signal pwip : character;
  signal aja : std_logic;
begin
  nbolo : entity work.ufrjwyp
    port map (a => vprqw, qztayho => aja, fkanpysp => pwip, cpzkmtx => sh);
end dimbmnlk;



entity elmy is
  port (iyop : in time);
end elmy;

library ieee;
use ieee.std_logic_1164.all;

architecture nij of elmy is
  signal j : integer;
  signal wwumbkysz : character;
  signal cteipmdo : std_logic;
  signal ivwlhppmd : integer;
  signal pdws : real;
  signal gxy : character;
  signal wix : std_logic;
  signal tbuua : integer;
begin
  yqz : entity work.ufrjwyp
    port map (a => tbuua, qztayho => wix, fkanpysp => gxy, cpzkmtx => pdws);
  vkgrgouc : entity work.ufrjwyp
    port map (a => ivwlhppmd, qztayho => cteipmdo, fkanpysp => wwumbkysz, cpzkmtx => pdws);
  zdmdyrz : entity work.btrm
    port map (vprqw => j, pvyu => wix, kwuuansfxt => gxy);
end nij;



entity uzpebztc is
  port (bg : inout integer);
end uzpebztc;

library ieee;
use ieee.std_logic_1164.all;

architecture yfnigx of uzpebztc is
  signal nofi : time;
  signal s : character;
  signal wp : std_logic;
begin
  npiifeoifv : entity work.btrm
    port map (vprqw => bg, pvyu => wp, kwuuansfxt => s);
  pvz : entity work.elmy
    port map (iyop => nofi);
end yfnigx;



-- Seed after: 16601108823091529924,18424117564733761959
