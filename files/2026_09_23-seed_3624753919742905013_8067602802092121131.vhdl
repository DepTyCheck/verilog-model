-- Seed: 3624753919742905013,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (ipihle : buffer std_logic_vector(1 to 0); qsgkhdy : inout std_logic_vector(1 to 4); gvsselpofp : linkage time);
end g;

architecture i of g is
  
begin
  -- Multi-driven assignments
  ipihle <= (others => '0');
  qsgkhdy <= ('Z', 'H', 'H', '1');
  ipihle <= ipihle;
end i;

entity zy is
  port (kstiolqaa : out character);
end zy;

library ieee;
use ieee.std_logic_1164.all;

architecture akvxifcmkw of zy is
  signal iat : time;
  signal xluap : std_logic_vector(1 to 0);
  signal derjswvled : time;
  signal ybnzh : std_logic_vector(1 to 4);
  signal c : std_logic_vector(1 to 0);
begin
  rhsqx : entity work.g
    port map (ipihle => c, qsgkhdy => ybnzh, gvsselpofp => derjswvled);
  zw : entity work.g
    port map (ipihle => xluap, qsgkhdy => ybnzh, gvsselpofp => iat);
  
  -- Single-driven assignments
  kstiolqaa <= 'd';
  
  -- Multi-driven assignments
  c <= c;
  c <= "";
  ybnzh <= "ZWWX";
end akvxifcmkw;



-- Seed after: 15849533164952422863,8067602802092121131
