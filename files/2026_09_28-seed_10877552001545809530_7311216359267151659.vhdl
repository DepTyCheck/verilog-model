-- Seed: 10877552001545809530,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity q is
  port (puyv : in severity_level; y : inout std_logic);
end q;

architecture anr of q is
  
begin
  -- Multi-driven assignments
  y <= '-';
  y <= 'Z';
  y <= '0';
  y <= 'L';
end anr;

library ieee;
use ieee.std_logic_1164.all;

entity fvksxg is
  port (tgdsl : out std_logic; jma : inout std_logic_vector(1 downto 0); parxu : in integer; d : out integer);
end fvksxg;

library ieee;
use ieee.std_logic_1164.all;

architecture p of fvksxg is
  signal m : std_logic;
  signal bungqp : std_logic;
  signal ez : severity_level;
begin
  iw : entity work.q
    port map (puyv => ez, y => bungqp);
  dtezsp : entity work.q
    port map (puyv => ez, y => m);
  
  -- Single-driven assignments
  d <= parxu;
  ez <= ez;
  
  -- Multi-driven assignments
  jma <= "1W";
end p;



-- Seed after: 7299049949343775862,7311216359267151659
