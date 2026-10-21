-- Seed: 4835546500802934959,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity wmhcdo is
  port (xvfeuaxncn : in std_logic_vector(4 downto 1); ev : inout std_logic_vector(1 downto 3); ywomat : buffer boolean_vector(3 to 3));
end wmhcdo;

architecture zpgp of wmhcdo is
  
begin
  -- Single-driven assignments
  ywomat <= (others => TRUE);
  
  -- Multi-driven assignments
  ev <= ev;
end zpgp;

library ieee;
use ieee.std_logic_1164.all;

entity ebxgff is
  port (okechjtei : in std_logic);
end ebxgff;

library ieee;
use ieee.std_logic_1164.all;

architecture ecgex of ebxgff is
  signal sben : boolean_vector(3 to 3);
  signal tbktakzwsn : std_logic_vector(4 downto 1);
  signal a : boolean_vector(3 to 3);
  signal daqreg : boolean_vector(3 to 3);
  signal bi : std_logic_vector(1 downto 3);
  signal ntre : std_logic_vector(4 downto 1);
  signal zaaqkbdmey : boolean_vector(3 to 3);
  signal pjmqfurn : std_logic_vector(1 downto 3);
  signal ekplwfnvdd : std_logic_vector(4 downto 1);
begin
  bjigfhknbl : entity work.wmhcdo
    port map (xvfeuaxncn => ekplwfnvdd, ev => pjmqfurn, ywomat => zaaqkbdmey);
  ofo : entity work.wmhcdo
    port map (xvfeuaxncn => ntre, ev => bi, ywomat => daqreg);
  djlidn : entity work.wmhcdo
    port map (xvfeuaxncn => ekplwfnvdd, ev => pjmqfurn, ywomat => a);
  fr : entity work.wmhcdo
    port map (xvfeuaxncn => tbktakzwsn, ev => pjmqfurn, ywomat => sben);
  
  -- Multi-driven assignments
  tbktakzwsn <= ekplwfnvdd;
end ecgex;

library ieee;
use ieee.std_logic_1164.all;

entity aynbr is
  port (jukwd : in real_vector(3 to 4); r : buffer std_logic_vector(3 downto 3));
end aynbr;

library ieee;
use ieee.std_logic_1164.all;

architecture qeqrmljw of aynbr is
  signal kag : std_logic;
  signal gg : boolean_vector(3 to 3);
  signal krjlsbjzu : std_logic_vector(1 downto 3);
  signal tnqrggmia : boolean_vector(3 to 3);
  signal zgztswye : std_logic_vector(1 downto 3);
  signal qugry : std_logic_vector(4 downto 1);
begin
  xpzoee : entity work.wmhcdo
    port map (xvfeuaxncn => qugry, ev => zgztswye, ywomat => tnqrggmia);
  vpygdvfvv : entity work.wmhcdo
    port map (xvfeuaxncn => qugry, ev => krjlsbjzu, ywomat => gg);
  zbgo : entity work.ebxgff
    port map (okechjtei => kag);
  g : entity work.ebxgff
    port map (okechjtei => kag);
  
  -- Multi-driven assignments
  kag <= kag;
  qugry <= qugry;
  kag <= 'X';
  krjlsbjzu <= (others => '0');
end qeqrmljw;



-- Seed after: 17973315905958676966,17219977430298397867
