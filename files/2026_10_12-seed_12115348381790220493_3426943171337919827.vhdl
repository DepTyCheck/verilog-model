-- Seed: 12115348381790220493,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity asctobxx is
  port (utnxnex : linkage std_logic; z : buffer std_logic);
end asctobxx;

architecture s of asctobxx is
  
begin
  -- Multi-driven assignments
  z <= z;
end s;

entity uo is
  port (mlxt : in time_vector(4 downto 4); srz : buffer integer);
end uo;

library ieee;
use ieee.std_logic_1164.all;

architecture vzwdbaquml of uo is
  signal nmav : std_logic;
  signal ehdhmazd : std_logic;
  signal t : std_logic;
  signal dtwol : std_logic;
  signal fcevglfta : std_logic;
  signal rsxbyaccgl : std_logic;
begin
  xiygte : entity work.asctobxx
    port map (utnxnex => rsxbyaccgl, z => fcevglfta);
  zhpiwqdwri : entity work.asctobxx
    port map (utnxnex => dtwol, z => t);
  baoz : entity work.asctobxx
    port map (utnxnex => ehdhmazd, z => fcevglfta);
  jpqflp : entity work.asctobxx
    port map (utnxnex => ehdhmazd, z => nmav);
  
  -- Single-driven assignments
  srz <= 8#0_0_5#;
  
  -- Multi-driven assignments
  fcevglfta <= t;
end vzwdbaquml;



-- Seed after: 13643790825551810277,3426943171337919827
