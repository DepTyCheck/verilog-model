-- Seed: 7097297693750197291,6329330932550885447

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (pvqpn : in std_logic_vector(2 downto 4));
end n;



architecture cuhn of n is
  
begin
  
end cuhn;



entity tathvpanf is
  port (ba : inout bit_vector(1 downto 4));
end tathvpanf;

library ieee;
use ieee.std_logic_1164.all;

architecture bagft of tathvpanf is
  signal lhqivupp : std_logic_vector(2 downto 4);
  signal mg : std_logic_vector(2 downto 4);
  signal una : std_logic_vector(2 downto 4);
begin
  pexrlz : entity work.n
    port map (pvqpn => una);
  xddstav : entity work.n
    port map (pvqpn => mg);
  idtj : entity work.n
    port map (pvqpn => lhqivupp);
  yjbcm : entity work.n
    port map (pvqpn => lhqivupp);
end bagft;



-- Seed after: 2996847533046147976,6329330932550885447
