-- Seed: 5183121401325861987,5511103086789671269

entity ik is
  port (ahmwhi : inout boolean; ttnbjnkgo : inout boolean);
end ik;

architecture jf of ik is
  
begin
  -- Single-driven assignments
  ttnbjnkgo <= TRUE;
  ahmwhi <= ahmwhi;
end jf;

entity oqmxfvmqw is
  port (duxanznf : linkage time);
end oqmxfvmqw;

architecture rdg of oqmxfvmqw is
  signal dtvkb : boolean;
  signal zmyasquwod : boolean;
  signal ko : boolean;
  signal zte : boolean;
begin
  yow : entity work.ik
    port map (ahmwhi => zte, ttnbjnkgo => ko);
  oj : entity work.ik
    port map (ahmwhi => zmyasquwod, ttnbjnkgo => dtvkb);
end rdg;

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (urewukykl : buffer time; yryoc : inout std_logic_vector(3 to 2));
end z;

architecture se of z is
  signal tjkrfesoe : boolean;
  signal kmcypdh : boolean;
  signal std : time;
  signal wypyzvdn : boolean;
  signal uxf : boolean;
begin
  faoskw : entity work.ik
    port map (ahmwhi => uxf, ttnbjnkgo => wypyzvdn);
  olltcskbot : entity work.oqmxfvmqw
    port map (duxanznf => std);
  xsmat : entity work.ik
    port map (ahmwhi => kmcypdh, ttnbjnkgo => tjkrfesoe);
  ywimi : entity work.oqmxfvmqw
    port map (duxanznf => urewukykl);
  
  -- Multi-driven assignments
  yryoc <= "";
  yryoc <= yryoc;
  yryoc <= (others => '0');
end se;



-- Seed after: 16292174221288630883,5511103086789671269
