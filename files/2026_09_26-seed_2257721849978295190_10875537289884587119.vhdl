-- Seed: 2257721849978295190,10875537289884587119

entity dpcc is
  port (cqjtn : buffer bit_vector(0 downto 2); gawlfhkjxu : buffer integer; qcqjiu : buffer severity_level; vnsjpi : linkage integer);
end dpcc;

architecture fqptdilksj of dpcc is
  
begin
  
end fqptdilksj;

library ieee;
use ieee.std_logic_1164.all;

entity fl is
  port (qwtguk : out std_logic_vector(3 to 1); vohkzjo : buffer std_logic_vector(3 downto 3); kkwvbekobm : out std_logic; otj : out time);
end fl;

architecture lxngyr of fl is
  
begin
  -- Multi-driven assignments
  kkwvbekobm <= '-';
end lxngyr;

entity rnpwqoylej is
  port (zxlexf : buffer integer; ehotpofd : in time);
end rnpwqoylej;

library ieee;
use ieee.std_logic_1164.all;

architecture y of rnpwqoylej is
  signal unt : time;
  signal noqymnbu : std_logic;
  signal bqwhgicjje : std_logic_vector(3 downto 3);
  signal ele : std_logic_vector(3 to 1);
  signal s : severity_level;
  signal nxhrq : integer;
  signal hgbwp : bit_vector(0 downto 2);
begin
  cppwekv : entity work.dpcc
    port map (cqjtn => hgbwp, gawlfhkjxu => nxhrq, qcqjiu => s, vnsjpi => zxlexf);
  vwhbjm : entity work.fl
    port map (qwtguk => ele, vohkzjo => bqwhgicjje, kkwvbekobm => noqymnbu, otj => unt);
  
  -- Multi-driven assignments
  noqymnbu <= noqymnbu;
  bqwhgicjje <= "U";
  ele <= "";
  bqwhgicjje <= "U";
end y;



-- Seed after: 12593572909916373456,10875537289884587119
