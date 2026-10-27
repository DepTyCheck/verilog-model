-- Seed: 12686646801324177371,12536735141187299017

library ieee;
use ieee.std_logic_1164.all;

entity yssjzts is
  port (mm : linkage std_logic);
end yssjzts;

architecture l of yssjzts is
  
begin
  
end l;

library ieee;
use ieee.std_logic_1164.all;

entity ust is
  port (ougasi : linkage std_logic; fp : in real; mim : in bit; dupvbfie : buffer std_logic);
end ust;

library ieee;
use ieee.std_logic_1164.all;

architecture vzheko of ust is
  signal teziosfpbx : std_logic;
  signal vnzultzo : std_logic;
begin
  wkqf : entity work.yssjzts
    port map (mm => vnzultzo);
  aqtvo : entity work.yssjzts
    port map (mm => dupvbfie);
  kwzjdggj : entity work.yssjzts
    port map (mm => teziosfpbx);
  
  -- Multi-driven assignments
  teziosfpbx <= dupvbfie;
  dupvbfie <= vnzultzo;
  vnzultzo <= 'Z';
  dupvbfie <= '1';
end vzheko;



-- Seed after: 15695987959836177940,12536735141187299017
