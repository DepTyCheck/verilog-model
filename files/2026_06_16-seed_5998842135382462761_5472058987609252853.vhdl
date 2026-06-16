-- Seed: 5998842135382462761,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity zfsjso is
  port (baarxetoz : out std_logic_vector(2 to 1); sakqg : buffer std_logic);
end zfsjso;

architecture vlt of zfsjso is
  
begin
  -- Multi-driven assignments
  baarxetoz <= (others => '0');
end vlt;

library ieee;
use ieee.std_logic_1164.all;

entity gwlrinja is
  port (eabmpy : out bit_vector(1 to 4); fkaovl : linkage std_logic; dukshg : out time);
end gwlrinja;

library ieee;
use ieee.std_logic_1164.all;

architecture mhuj of gwlrinja is
  signal ftowonbzq : std_logic;
  signal npwlsmriqw : std_logic_vector(2 to 1);
  signal uonpizco : std_logic;
  signal sjpfdznva : std_logic_vector(2 to 1);
  signal ez : std_logic;
  signal tqs : std_logic_vector(2 to 1);
begin
  nkpxiabj : entity work.zfsjso
    port map (baarxetoz => tqs, sakqg => ez);
  xfddwmlxy : entity work.zfsjso
    port map (baarxetoz => sjpfdznva, sakqg => uonpizco);
  jgjcqwzb : entity work.zfsjso
    port map (baarxetoz => npwlsmriqw, sakqg => ftowonbzq);
  bhy : entity work.zfsjso
    port map (baarxetoz => sjpfdznva, sakqg => ez);
  
  -- Single-driven assignments
  dukshg <= 2_4_1_2 ns;
  eabmpy <= ('1', '0', '0', '0');
  
  -- Multi-driven assignments
  tqs <= "";
end mhuj;

entity egeha is
  port (rvzzzj : linkage real; tsachjuli : inout boolean);
end egeha;

architecture sttg of egeha is
  
begin
  -- Single-driven assignments
  tsachjuli <= TRUE;
end sttg;



-- Seed after: 6979583137089523934,5472058987609252853
