-- Seed: 14280636078275506369,16265041255589496407

library ieee;
use ieee.std_logic_1164.all;

entity pzsu is
  port (izsznfl : inout real; fjwvtm : buffer severity_level; nhwabkacqz : in std_logic_vector(4 downto 2); pvvprwih : buffer integer);
end pzsu;



architecture p of pzsu is
  
begin
  
end p;

library ieee;
use ieee.std_logic_1164.all;

entity zlbze is
  port (h : inout std_logic_vector(1 downto 4); dxjxcumsdk : in real);
end zlbze;

library ieee;
use ieee.std_logic_1164.all;

architecture de of zlbze is
  signal vurkz : integer;
  signal ust : severity_level;
  signal gyvx : real;
  signal rgbst : integer;
  signal gjvzbbnw : std_logic_vector(4 downto 2);
  signal oozogwy : severity_level;
  signal sskyuph : real;
begin
  jrun : entity work.pzsu
    port map (izsznfl => sskyuph, fjwvtm => oozogwy, nhwabkacqz => gjvzbbnw, pvvprwih => rgbst);
  tezqnk : entity work.pzsu
    port map (izsznfl => gyvx, fjwvtm => ust, nhwabkacqz => gjvzbbnw, pvvprwih => vurkz);
end de;

library ieee;
use ieee.std_logic_1164.all;

entity nlgj is
  port (rfwnhzsq : in std_logic);
end nlgj;

library ieee;
use ieee.std_logic_1164.all;

architecture wikgdjxaqc of nlgj is
  signal ykmvk : real;
  signal hihp : std_logic_vector(1 downto 4);
  signal toi : real;
  signal yryp : std_logic_vector(1 downto 4);
begin
  xvojmihv : entity work.zlbze
    port map (h => yryp, dxjxcumsdk => toi);
  ctcirc : entity work.zlbze
    port map (h => hihp, dxjxcumsdk => ykmvk);
end wikgdjxaqc;



-- Seed after: 17723508327835565865,16265041255589496407
