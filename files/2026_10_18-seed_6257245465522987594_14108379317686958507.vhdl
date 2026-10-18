-- Seed: 6257245465522987594,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity lhgs is
  port (hiakyrg : buffer std_logic; svlnscdu : in real; lzjmksoxx : in std_logic; luratldv : buffer time);
end lhgs;

architecture lievac of lhgs is
  
begin
  -- Single-driven assignments
  luratldv <= 4_0_4.00 us;
  
  -- Multi-driven assignments
  hiakyrg <= 'U';
  hiakyrg <= 'W';
end lievac;

library ieee;
use ieee.std_logic_1164.all;

entity lbyy is
  port (uuuer : in std_logic_vector(4 downto 2); ut : linkage std_logic);
end lbyy;

library ieee;
use ieee.std_logic_1164.all;

architecture npd of lbyy is
  signal hfba : time;
  signal gh : std_logic;
  signal fgipehyoiw : time;
  signal u : real;
  signal ueuujqkep : time;
  signal jurvxuvx : std_logic;
  signal kawuutq : time;
  signal isqcdcann : std_logic;
  signal nunq : real;
  signal lmgwxnuue : std_logic;
begin
  wav : entity work.lhgs
    port map (hiakyrg => lmgwxnuue, svlnscdu => nunq, lzjmksoxx => isqcdcann, luratldv => kawuutq);
  mchak : entity work.lhgs
    port map (hiakyrg => lmgwxnuue, svlnscdu => nunq, lzjmksoxx => jurvxuvx, luratldv => ueuujqkep);
  hsls : entity work.lhgs
    port map (hiakyrg => isqcdcann, svlnscdu => u, lzjmksoxx => lmgwxnuue, luratldv => fgipehyoiw);
  ewiaepl : entity work.lhgs
    port map (hiakyrg => gh, svlnscdu => nunq, lzjmksoxx => jurvxuvx, luratldv => hfba);
  
  -- Single-driven assignments
  nunq <= nunq;
  u <= nunq;
  
  -- Multi-driven assignments
  lmgwxnuue <= isqcdcann;
end npd;



-- Seed after: 429704227288877638,14108379317686958507
