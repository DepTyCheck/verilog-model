-- Seed: 8586693405753975412,662889661651915549

library ieee;
use ieee.std_logic_1164.all;

entity bylvd is
  port (dzp : buffer integer_vector(0 to 0); ep : inout boolean; tzou : inout std_logic_vector(4 to 2));
end bylvd;

architecture mdxvrgil of bylvd is
  
begin
  -- Single-driven assignments
  dzp <= (others => 3_3_3_1);
  ep <= TRUE;
end mdxvrgil;

library ieee;
use ieee.std_logic_1164.all;

entity esefgrhnaq is
  port (dpuhe : inout std_logic_vector(3 to 0); rgaczbsdk : inout std_logic_vector(2 downto 1));
end esefgrhnaq;

library ieee;
use ieee.std_logic_1164.all;

architecture ohbbgiawsr of esefgrhnaq is
  signal xqin : boolean;
  signal ps : integer_vector(0 to 0);
  signal uss : boolean;
  signal cxaxuquoqn : integer_vector(0 to 0);
  signal ux : std_logic_vector(4 to 2);
  signal kf : boolean;
  signal dp : integer_vector(0 to 0);
  signal d : std_logic_vector(4 to 2);
  signal dhfo : boolean;
  signal vmrwdhgbw : integer_vector(0 to 0);
begin
  nozzztgdhe : entity work.bylvd
    port map (dzp => vmrwdhgbw, ep => dhfo, tzou => d);
  llfzg : entity work.bylvd
    port map (dzp => dp, ep => kf, tzou => ux);
  oovhn : entity work.bylvd
    port map (dzp => cxaxuquoqn, ep => uss, tzou => dpuhe);
  vuvwppli : entity work.bylvd
    port map (dzp => ps, ep => xqin, tzou => d);
  
  -- Multi-driven assignments
  d <= (others => '0');
  dpuhe <= dpuhe;
  dpuhe <= "";
  ux <= dpuhe;
end ohbbgiawsr;



-- Seed after: 9227855960518998035,662889661651915549
