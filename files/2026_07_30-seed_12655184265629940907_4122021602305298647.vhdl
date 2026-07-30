-- Seed: 12655184265629940907,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity gncac is
  port (cykxf : inout integer; myzrwjpoma : buffer boolean_vector(2 to 3); jgujber : buffer std_logic);
end gncac;

architecture iytgwjajrt of gncac is
  
begin
  -- Single-driven assignments
  myzrwjpoma <= (FALSE, TRUE);
  cykxf <= 0433;
  
  -- Multi-driven assignments
  jgujber <= 'L';
end iytgwjajrt;

library ieee;
use ieee.std_logic_1164.all;

entity trtkqkll is
  port (fv : inout std_logic_vector(1 to 2));
end trtkqkll;

architecture jcjjgj of trtkqkll is
  
begin
  -- Multi-driven assignments
  fv <= ('X', '0');
end jcjjgj;

library ieee;
use ieee.std_logic_1164.all;

entity coxsfoao is
  port (qvc : buffer time_vector(3 downto 2); dlmwovvici : in std_logic; qulbrqk : buffer std_logic);
end coxsfoao;

architecture uxthqqnd of coxsfoao is
  signal vlgmquseii : boolean_vector(2 to 3);
  signal gcgnhzieo : integer;
begin
  awxftmvo : entity work.gncac
    port map (cykxf => gcgnhzieo, myzrwjpoma => vlgmquseii, jgujber => qulbrqk);
  
  -- Single-driven assignments
  qvc <= qvc;
  
  -- Multi-driven assignments
  qulbrqk <= dlmwovvici;
  qulbrqk <= 'L';
  qulbrqk <= 'Z';
end uxthqqnd;

library ieee;
use ieee.std_logic_1164.all;

entity zjgxq is
  port (vglzdh : buffer character; utrxsw : inout boolean_vector(4 to 2); jt : in severity_level; myu : linkage std_logic_vector(4 to 3));
end zjgxq;

library ieee;
use ieee.std_logic_1164.all;

architecture nvj of zjgxq is
  signal nbvb : std_logic_vector(1 to 2);
begin
  lcifxbji : entity work.trtkqkll
    port map (fv => nbvb);
  
  -- Single-driven assignments
  utrxsw <= (others => TRUE);
  vglzdh <= vglzdh;
  
  -- Multi-driven assignments
  nbvb <= "0-";
  nbvb <= nbvb;
end nvj;



-- Seed after: 18415663722256723440,4122021602305298647
