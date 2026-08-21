-- Seed: 13071309026506415507,16188444798499499427

library ieee;
use ieee.std_logic_1164.all;

entity zyomsqgsf is
  port (wyqml : inout std_logic; wjxrsh : out real; pp : out std_logic);
end zyomsqgsf;

architecture ma of zyomsqgsf is
  
begin
  -- Single-driven assignments
  wjxrsh <= 2#0.1_0_0#;
end ma;

entity e is
  port (ja : in integer; neo : in integer; ulnhiugwct : inout boolean; ymzavkx : inout boolean_vector(1 to 3));
end e;

library ieee;
use ieee.std_logic_1164.all;

architecture wa of e is
  signal cvb : real;
  signal iboj : std_logic;
  signal hnnevj : real;
  signal qdvyojir : std_logic;
  signal p : std_logic;
  signal fdofmtfayt : real;
  signal wiathytx : real;
  signal fycynl : std_logic;
begin
  mjcpew : entity work.zyomsqgsf
    port map (wyqml => fycynl, wjxrsh => wiathytx, pp => fycynl);
  luzwahn : entity work.zyomsqgsf
    port map (wyqml => fycynl, wjxrsh => fdofmtfayt, pp => p);
  x : entity work.zyomsqgsf
    port map (wyqml => qdvyojir, wjxrsh => hnnevj, pp => iboj);
  w : entity work.zyomsqgsf
    port map (wyqml => fycynl, wjxrsh => cvb, pp => qdvyojir);
  
  -- Single-driven assignments
  ymzavkx <= ymzavkx;
  ulnhiugwct <= ulnhiugwct;
  
  -- Multi-driven assignments
  fycynl <= 'L';
end wa;



-- Seed after: 18066153913256092865,16188444798499499427
