-- Seed: 7394913837320066292,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity krcd is
  port (kjxqigx : linkage real; groqq : linkage std_logic_vector(3 downto 0); idvsrfj : inout real);
end krcd;

architecture gjyvwdicf of krcd is
  
begin
  -- Single-driven assignments
  idvsrfj <= idvsrfj;
end gjyvwdicf;

library ieee;
use ieee.std_logic_1164.all;

entity fsufvsf is
  port (o : linkage time_vector(0 downto 0); hllt : buffer std_logic_vector(4 downto 4); jgsqsb : inout bit; fbnaa : buffer time);
end fsufvsf;

architecture c of fsufvsf is
  
begin
  -- Single-driven assignments
  fbnaa <= fbnaa;
  
  -- Multi-driven assignments
  hllt <= "H";
  hllt <= hllt;
  hllt <= "H";
end c;

library ieee;
use ieee.std_logic_1164.all;

entity oq is
  port (gdvivltugf : buffer std_logic);
end oq;

library ieee;
use ieee.std_logic_1164.all;

architecture gdagzef of oq is
  signal zyjjmth : real;
  signal y : std_logic_vector(3 downto 0);
  signal cf : real;
begin
  plk : entity work.krcd
    port map (kjxqigx => cf, groqq => y, idvsrfj => zyjjmth);
end gdagzef;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (yzxxln : in integer; xtb : inout std_logic; lmjsf : inout real);
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture jzett of r is
  signal bmzqriayk : std_logic_vector(3 downto 0);
  signal pjlm : real;
  signal imzdjulnj : std_logic;
  signal nlhkz : time;
  signal inynkjzn : bit;
  signal ao : time_vector(0 downto 0);
  signal oobt : time;
  signal mcvws : bit;
  signal dujjaz : std_logic_vector(4 downto 4);
  signal gspapk : time_vector(0 downto 0);
begin
  nrdylmk : entity work.fsufvsf
    port map (o => gspapk, hllt => dujjaz, jgsqsb => mcvws, fbnaa => oobt);
  sr : entity work.fsufvsf
    port map (o => ao, hllt => dujjaz, jgsqsb => inynkjzn, fbnaa => nlhkz);
  hewdeewct : entity work.oq
    port map (gdvivltugf => imzdjulnj);
  jqms : entity work.krcd
    port map (kjxqigx => pjlm, groqq => bmzqriayk, idvsrfj => lmjsf);
  
  -- Multi-driven assignments
  xtb <= '1';
end jzett;



-- Seed after: 9594525514468982062,12143220691580258643
