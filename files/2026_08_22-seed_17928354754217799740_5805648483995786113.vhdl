-- Seed: 17928354754217799740,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (chyjhqfex : linkage std_logic_vector(3 to 1));
end g;

architecture oli of g is
  
begin
  
end oli;

library ieee;
use ieee.std_logic_1164.all;

entity jozjxqzkv is
  port (voego : inout std_logic_vector(3 downto 0); wosjtgdsp : buffer std_logic);
end jozjxqzkv;

library ieee;
use ieee.std_logic_1164.all;

architecture daonlz of jozjxqzkv is
  signal entyirmeg : std_logic_vector(3 to 1);
begin
  psk : entity work.g
    port map (chyjhqfex => entyirmeg);
  kyeu : entity work.g
    port map (chyjhqfex => entyirmeg);
  t : entity work.g
    port map (chyjhqfex => entyirmeg);
  
  -- Multi-driven assignments
  entyirmeg <= entyirmeg;
end daonlz;



-- Seed after: 17999530185882177129,5805648483995786113
