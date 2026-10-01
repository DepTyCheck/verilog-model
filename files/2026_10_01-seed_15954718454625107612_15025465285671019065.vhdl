-- Seed: 15954718454625107612,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity kpsmipaer is
  port (hrrnq : in std_logic_vector(3 downto 2); aethqpmo : inout std_logic);
end kpsmipaer;

architecture ozswymktq of kpsmipaer is
  
begin
  -- Multi-driven assignments
  aethqpmo <= 'X';
  aethqpmo <= '1';
  aethqpmo <= aethqpmo;
  aethqpmo <= 'L';
end ozswymktq;

library ieee;
use ieee.std_logic_1164.all;

entity uylqp is
  port (febbckdrb : out bit_vector(4 downto 4); yyxit : linkage std_logic_vector(3 to 0); knxxbcqm : linkage std_logic_vector(1 to 3));
end uylqp;

library ieee;
use ieee.std_logic_1164.all;

architecture gnvdum of uylqp is
  signal xyunw : std_logic;
  signal zqbzm : std_logic_vector(3 downto 2);
  signal obyu : std_logic;
  signal afrmyab : std_logic_vector(3 downto 2);
begin
  zn : entity work.kpsmipaer
    port map (hrrnq => afrmyab, aethqpmo => obyu);
  o : entity work.kpsmipaer
    port map (hrrnq => zqbzm, aethqpmo => xyunw);
  
  -- Single-driven assignments
  febbckdrb <= febbckdrb;
  
  -- Multi-driven assignments
  obyu <= obyu;
end gnvdum;



-- Seed after: 7736305827665730014,15025465285671019065
