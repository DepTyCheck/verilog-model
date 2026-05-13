-- Seed: 13764836015475332988,15141888397681078541

library ieee;
use ieee.std_logic_1164.all;

entity didqvq is
  port (cquwbwa : in std_logic; yhonlzkb : buffer severity_level; loboshqzkh : in std_logic);
end didqvq;



architecture owmzya of didqvq is
  
begin
  
end owmzya;

library ieee;
use ieee.std_logic_1164.all;

entity ic is
  port (ffmjsblpru : linkage time; ijyjymidew : buffer std_logic);
end ic;

library ieee;
use ieee.std_logic_1164.all;

architecture vxaudbfe of ic is
  signal sqwnimr : severity_level;
  signal owziebsi : std_logic;
  signal iqugacdhut : std_logic;
  signal sfnxqj : severity_level;
  signal w : severity_level;
  signal kaxeaclqq : std_logic;
begin
  llyxpfwqe : entity work.didqvq
    port map (cquwbwa => kaxeaclqq, yhonlzkb => w, loboshqzkh => kaxeaclqq);
  wp : entity work.didqvq
    port map (cquwbwa => ijyjymidew, yhonlzkb => sfnxqj, loboshqzkh => iqugacdhut);
  hvl : entity work.didqvq
    port map (cquwbwa => owziebsi, yhonlzkb => sqwnimr, loboshqzkh => iqugacdhut);
end vxaudbfe;



entity litpmr is
  port (oki : out time);
end litpmr;

library ieee;
use ieee.std_logic_1164.all;

architecture tytia of litpmr is
  signal n : time;
  signal wi : severity_level;
  signal yi : std_logic;
begin
  dslgffihq : entity work.didqvq
    port map (cquwbwa => yi, yhonlzkb => wi, loboshqzkh => yi);
  zfauqg : entity work.ic
    port map (ffmjsblpru => n, ijyjymidew => yi);
end tytia;



-- Seed after: 650582314717646905,15141888397681078541
