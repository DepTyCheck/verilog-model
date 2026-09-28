-- Seed: 3259252343583856847,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity kbffb is
  port (w : in std_logic_vector(3 downto 4); flnfbyk : in std_logic_vector(1 downto 0); fmg : inout std_logic_vector(4 downto 2));
end kbffb;

architecture hpmqa of kbffb is
  
begin
  -- Multi-driven assignments
  fmg <= fmg;
  fmg <= fmg;
end hpmqa;

library ieee;
use ieee.std_logic_1164.all;

entity rohwrmt is
  port (ef : linkage std_logic_vector(0 to 3); leldgi : out std_logic);
end rohwrmt;

library ieee;
use ieee.std_logic_1164.all;

architecture t of rohwrmt is
  signal xt : std_logic_vector(1 downto 0);
  signal zlloqdlfxs : std_logic_vector(3 downto 4);
  signal yfjmng : std_logic_vector(3 downto 4);
  signal chetod : std_logic_vector(4 downto 2);
  signal pqza : std_logic_vector(1 downto 0);
  signal cpxejqmf : std_logic_vector(3 downto 4);
begin
  exiiqkeirf : entity work.kbffb
    port map (w => cpxejqmf, flnfbyk => pqza, fmg => chetod);
  tw : entity work.kbffb
    port map (w => yfjmng, flnfbyk => pqza, fmg => chetod);
  wvb : entity work.kbffb
    port map (w => zlloqdlfxs, flnfbyk => xt, fmg => chetod);
  wnoqcvmzz : entity work.kbffb
    port map (w => zlloqdlfxs, flnfbyk => pqza, fmg => chetod);
end t;

library ieee;
use ieee.std_logic_1164.all;

entity cloli is
  port (oowywa : inout std_logic; wndhmag : out boolean; wioysnm : in real; oyg : buffer character);
end cloli;

library ieee;
use ieee.std_logic_1164.all;

architecture fqr of cloli is
  signal fvkqkydyr : std_logic_vector(4 downto 2);
  signal fkeiflnz : std_logic_vector(1 downto 0);
  signal zvxrw : std_logic_vector(4 downto 2);
  signal nwsf : std_logic_vector(1 downto 0);
  signal ecr : std_logic_vector(4 downto 2);
  signal jbxqoxxq : std_logic_vector(1 downto 0);
  signal kyitfrrtvl : std_logic_vector(3 downto 4);
begin
  xkqjl : entity work.kbffb
    port map (w => kyitfrrtvl, flnfbyk => jbxqoxxq, fmg => ecr);
  cucbmpn : entity work.kbffb
    port map (w => kyitfrrtvl, flnfbyk => nwsf, fmg => zvxrw);
  o : entity work.kbffb
    port map (w => kyitfrrtvl, flnfbyk => jbxqoxxq, fmg => ecr);
  agvbeqf : entity work.kbffb
    port map (w => kyitfrrtvl, flnfbyk => fkeiflnz, fmg => fvkqkydyr);
  
  -- Single-driven assignments
  oyg <= oyg;
  
  -- Multi-driven assignments
  ecr <= ('0', 'Z', '1');
end fqr;



-- Seed after: 4664784182518088395,7311216359267151659
