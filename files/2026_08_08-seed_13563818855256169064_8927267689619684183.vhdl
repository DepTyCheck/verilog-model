-- Seed: 13563818855256169064,8927267689619684183

entity bopjpbns is
  port (wbufhuvt : in integer; gw : out bit_vector(3 downto 2));
end bopjpbns;

architecture nepgpcjq of bopjpbns is
  
begin
  
end nepgpcjq;

library ieee;
use ieee.std_logic_1164.all;

entity jpcrej is
  port (drp : buffer std_logic_vector(3 downto 2); juncmawq : out std_logic_vector(2 to 3));
end jpcrej;

architecture pqjzllvqxh of jpcrej is
  
begin
  
end pqjzllvqxh;

library ieee;
use ieee.std_logic_1164.all;

entity rzsxtzi is
  port (khxtxpmwe : linkage character; gyrnzk : out std_logic);
end rzsxtzi;

library ieee;
use ieee.std_logic_1164.all;

architecture dpabmrwe of rzsxtzi is
  signal rmjtfi : bit_vector(3 downto 2);
  signal ctz : bit_vector(3 downto 2);
  signal hojygqyqi : integer;
  signal eaprd : std_logic_vector(2 to 3);
  signal oumuianyg : std_logic_vector(3 downto 2);
begin
  yv : entity work.jpcrej
    port map (drp => oumuianyg, juncmawq => eaprd);
  lrqbkwma : entity work.bopjpbns
    port map (wbufhuvt => hojygqyqi, gw => ctz);
  dd : entity work.bopjpbns
    port map (wbufhuvt => hojygqyqi, gw => rmjtfi);
  
  -- Single-driven assignments
  hojygqyqi <= 3;
  
  -- Multi-driven assignments
  gyrnzk <= gyrnzk;
  gyrnzk <= gyrnzk;
  oumuianyg <= ('W', 'W');
end dpabmrwe;



-- Seed after: 3075958333963181483,8927267689619684183
