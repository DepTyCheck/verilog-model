-- Seed: 16094340514224789664,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity urvp is
  port (plk : in severity_level; pofslz : in severity_level; r : out std_logic_vector(1 downto 0));
end urvp;

architecture lywfayplq of urvp is
  
begin
  -- Multi-driven assignments
  r <= "XW";
  r <= r;
end lywfayplq;

entity hmqyeuliii is
  port (xwsaw : in integer; tdvcennb : linkage severity_level);
end hmqyeuliii;

library ieee;
use ieee.std_logic_1164.all;

architecture kj of hmqyeuliii is
  signal ihksig : severity_level;
  signal lqglosv : severity_level;
  signal jzghyzwxys : std_logic_vector(1 downto 0);
  signal hhwzm : severity_level;
begin
  wcb : entity work.urvp
    port map (plk => hhwzm, pofslz => hhwzm, r => jzghyzwxys);
  m : entity work.urvp
    port map (plk => hhwzm, pofslz => lqglosv, r => jzghyzwxys);
  hckjyhj : entity work.urvp
    port map (plk => hhwzm, pofslz => ihksig, r => jzghyzwxys);
  
  -- Single-driven assignments
  hhwzm <= WARNING;
  lqglosv <= hhwzm;
  ihksig <= NOTE;
  
  -- Multi-driven assignments
  jzghyzwxys <= "ZX";
end kj;

library ieee;
use ieee.std_logic_1164.all;

entity dfopaxyw is
  port (ntulzcqgn : out time; nqwyqslq : buffer std_logic_vector(1 to 1));
end dfopaxyw;

library ieee;
use ieee.std_logic_1164.all;

architecture befg of dfopaxyw is
  signal tvpmtdaew : severity_level;
  signal cdxiqzdhxk : severity_level;
  signal ago : std_logic_vector(1 downto 0);
  signal skaekwm : severity_level;
  signal k : std_logic_vector(1 downto 0);
  signal owrfioeud : severity_level;
  signal didajqiv : severity_level;
begin
  rfpyzy : entity work.urvp
    port map (plk => didajqiv, pofslz => owrfioeud, r => k);
  jxgnjl : entity work.urvp
    port map (plk => owrfioeud, pofslz => skaekwm, r => k);
  fi : entity work.urvp
    port map (plk => didajqiv, pofslz => owrfioeud, r => ago);
  qpcl : entity work.urvp
    port map (plk => cdxiqzdhxk, pofslz => tvpmtdaew, r => k);
  
  -- Single-driven assignments
  tvpmtdaew <= cdxiqzdhxk;
  owrfioeud <= ERROR;
  cdxiqzdhxk <= WARNING;
  ntulzcqgn <= 13 ps;
  didajqiv <= WARNING;
  
  -- Multi-driven assignments
  ago <= "-X";
  nqwyqslq <= "X";
end befg;



-- Seed after: 4042275953407138130,7304262412290825129
