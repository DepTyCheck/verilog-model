-- Seed: 8062631345375727539,6329330932550885447

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (rif : inout real; x : buffer std_logic; lbluch : out time);
end a;



architecture fevze of a is
  
begin
  
end fevze;

library ieee;
use ieee.std_logic_1164.all;

entity ix is
  port (s : in real; yabzubwwt : in boolean_vector(3 to 1); mn : in std_logic_vector(3 downto 1); bmghuyjn : in time);
end ix;



architecture bamtcxlwe of ix is
  
begin
  
end bamtcxlwe;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (omcmf : in std_logic);
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture kjuu of x is
  signal nftnwerzam : time;
  signal rmotbxrk : real;
  signal tkspyqu : time;
  signal dotnprguya : std_logic;
  signal xcbst : real;
begin
  orkprajndd : entity work.a
    port map (rif => xcbst, x => dotnprguya, lbluch => tkspyqu);
  oqwgbodvk : entity work.a
    port map (rif => rmotbxrk, x => dotnprguya, lbluch => nftnwerzam);
end kjuu;

library ieee;
use ieee.std_logic_1164.all;

entity jbzvmmzg is
  port (xia : inout time; edpz : inout integer_vector(2 to 2); so : out time; lka : in std_logic_vector(1 downto 0));
end jbzvmmzg;

library ieee;
use ieee.std_logic_1164.all;

architecture guqvafgid of jbzvmmzg is
  signal wfwmjaa : real;
  signal rguxg : std_logic_vector(3 downto 1);
  signal vuygy : boolean_vector(3 to 1);
  signal xoinck : std_logic;
  signal zbvyukmc : real;
begin
  xiwcem : entity work.a
    port map (rif => zbvyukmc, x => xoinck, lbluch => xia);
  thrgoc : entity work.x
    port map (omcmf => xoinck);
  llg : entity work.ix
    port map (s => zbvyukmc, yabzubwwt => vuygy, mn => rguxg, bmghuyjn => xia);
  awnnobbkg : entity work.a
    port map (rif => wfwmjaa, x => xoinck, lbluch => so);
end guqvafgid;



-- Seed after: 2102107879624057652,6329330932550885447
