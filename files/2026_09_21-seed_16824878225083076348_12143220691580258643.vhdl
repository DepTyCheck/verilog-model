-- Seed: 16824878225083076348,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity awndvggi is
  port (jqcvxp : in std_logic_vector(0 to 0); fxxccuuq : buffer bit_vector(4 to 3); yjb : out character; txgjrrvqo : linkage std_logic);
end awndvggi;

architecture npq of awndvggi is
  
begin
  
end npq;

entity ppwxi is
  port (abeox : linkage integer; oghxrybvrr : inout character; ssxvsfqhic : out time);
end ppwxi;

library ieee;
use ieee.std_logic_1164.all;

architecture btifoa of ppwxi is
  signal xjjqmnp : std_logic;
  signal k : character;
  signal fpjjlla : bit_vector(4 to 3);
  signal slt : std_logic;
  signal b : character;
  signal lzcmset : bit_vector(4 to 3);
  signal jwq : std_logic;
  signal qigprhw : bit_vector(4 to 3);
  signal qzukwqbhu : std_logic_vector(0 to 0);
begin
  zhprgzx : entity work.awndvggi
    port map (jqcvxp => qzukwqbhu, fxxccuuq => qigprhw, yjb => oghxrybvrr, txgjrrvqo => jwq);
  tufkbten : entity work.awndvggi
    port map (jqcvxp => qzukwqbhu, fxxccuuq => lzcmset, yjb => b, txgjrrvqo => slt);
  hi : entity work.awndvggi
    port map (jqcvxp => qzukwqbhu, fxxccuuq => fpjjlla, yjb => k, txgjrrvqo => xjjqmnp);
  
  -- Single-driven assignments
  ssxvsfqhic <= 2#0_0_1_1.1# us;
  
  -- Multi-driven assignments
  jwq <= 'H';
  jwq <= 'L';
end btifoa;



-- Seed after: 14276818282471641418,12143220691580258643
