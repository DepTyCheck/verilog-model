-- Seed: 4363907669296968384,11387579217500963635

library ieee;
use ieee.std_logic_1164.all;

entity pbiypoluls is
  port (xeesyrc : out std_logic_vector(2 downto 0); dxxtuxiz : buffer std_logic);
end pbiypoluls;



architecture dccal of pbiypoluls is
  
begin
  
end dccal;

library ieee;
use ieee.std_logic_1164.all;

entity hszcj is
  port (gltjffsvxd : buffer character; vguw : buffer integer; ayg : buffer std_logic_vector(3 downto 1));
end hszcj;

library ieee;
use ieee.std_logic_1164.all;

architecture ywasx of hszcj is
  signal fol : std_logic;
begin
  zxafruxzmz : entity work.pbiypoluls
    port map (xeesyrc => ayg, dxxtuxiz => fol);
end ywasx;

library ieee;
use ieee.std_logic_1164.all;

entity wdqjmrxzl is
  port (i : buffer boolean_vector(2 downto 1); pztdvqxms : out boolean_vector(2 downto 0); rgmqkqwiq : buffer std_logic_vector(4 to 3));
end wdqjmrxzl;

library ieee;
use ieee.std_logic_1164.all;

architecture elskbfjzux of wdqjmrxzl is
  signal flbmd : std_logic;
  signal bvfshddoby : std_logic_vector(2 downto 0);
  signal fogyctu : std_logic_vector(3 downto 1);
  signal aaokkzgwcn : integer;
  signal b : character;
begin
  opw : entity work.hszcj
    port map (gltjffsvxd => b, vguw => aaokkzgwcn, ayg => fogyctu);
  mppcydrmr : entity work.pbiypoluls
    port map (xeesyrc => bvfshddoby, dxxtuxiz => flbmd);
end elskbfjzux;



-- Seed after: 3709788100033630058,11387579217500963635
