-- Seed: 3896028433254940447,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity lecje is
  port (qmulcsc : in real; m : inout std_logic_vector(1 downto 4); bedqcpm : inout std_logic);
end lecje;

architecture jgo of lecje is
  
begin
  -- Multi-driven assignments
  bedqcpm <= 'L';
  bedqcpm <= bedqcpm;
end jgo;

library ieee;
use ieee.std_logic_1164.all;

entity mibxjy is
  port (gecslw : inout std_logic_vector(4 downto 2); iuypep : inout time; o : inout std_logic);
end mibxjy;

library ieee;
use ieee.std_logic_1164.all;

architecture jxvz of mibxjy is
  signal ubayjvwva : std_logic;
  signal jhniafao : std_logic_vector(1 downto 4);
  signal iipd : real;
begin
  rsk : entity work.lecje
    port map (qmulcsc => iipd, m => jhniafao, bedqcpm => ubayjvwva);
  
  -- Single-driven assignments
  iuypep <= 16#8.4_1_8# ns;
  iipd <= iipd;
  
  -- Multi-driven assignments
  jhniafao <= jhniafao;
end jxvz;



-- Seed after: 6005563968951899280,8891552411914730853
