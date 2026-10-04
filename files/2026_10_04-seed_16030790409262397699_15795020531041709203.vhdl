-- Seed: 16030790409262397699,15795020531041709203

entity mougjre is
  port (yy : buffer bit_vector(3 to 1));
end mougjre;

architecture atqyqqk of mougjre is
  
begin
  -- Single-driven assignments
  yy <= (others => '0');
end atqyqqk;

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (tmd : linkage integer; q : inout std_logic);
end u;

architecture wfnopftva of u is
  signal smepu : bit_vector(3 to 1);
  signal bw : bit_vector(3 to 1);
  signal xkqwwiai : bit_vector(3 to 1);
  signal tpqedx : bit_vector(3 to 1);
begin
  slofitsfj : entity work.mougjre
    port map (yy => tpqedx);
  cg : entity work.mougjre
    port map (yy => xkqwwiai);
  v : entity work.mougjre
    port map (yy => bw);
  egriip : entity work.mougjre
    port map (yy => smepu);
  
  -- Multi-driven assignments
  q <= 'H';
  q <= q;
  q <= q;
end wfnopftva;

library ieee;
use ieee.std_logic_1164.all;

entity kxnyal is
  port (eseplsc : inout std_logic; g : inout std_logic);
end kxnyal;

library ieee;
use ieee.std_logic_1164.all;

architecture pmpiu of kxnyal is
  signal c : bit_vector(3 to 1);
  signal rbba : std_logic;
  signal bpxjhwka : integer;
  signal tkyuqpex : bit_vector(3 to 1);
begin
  ap : entity work.mougjre
    port map (yy => tkyuqpex);
  rx : entity work.u
    port map (tmd => bpxjhwka, q => rbba);
  vyeqlm : entity work.mougjre
    port map (yy => c);
  
  -- Multi-driven assignments
  g <= g;
end pmpiu;



-- Seed after: 11628355782991210977,15795020531041709203
