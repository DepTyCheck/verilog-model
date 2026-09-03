-- Seed: 3684133720690244312,11127274767545411571

entity nqm is
  port (hxr : inout real; xenitmd : buffer boolean; nwfisxjsit : buffer time);
end nqm;

architecture nu of nqm is
  
begin
  -- Single-driven assignments
  xenitmd <= FALSE;
end nu;

library ieee;
use ieee.std_logic_1164.all;

entity tc is
  port (ekrbqz : linkage std_logic_vector(4 to 1); x : in time);
end tc;

architecture dpzzzdaofe of tc is
  
begin
  
end dpzzzdaofe;

entity g is
  port (zybope : in time);
end g;

library ieee;
use ieee.std_logic_1164.all;

architecture d of g is
  signal je : boolean;
  signal jbur : real;
  signal bcyba : time;
  signal gnf : boolean;
  signal ahqztk : real;
  signal s : time;
  signal tisb : std_logic_vector(4 to 1);
begin
  varchhxn : entity work.tc
    port map (ekrbqz => tisb, x => s);
  kxeglpm : entity work.nqm
    port map (hxr => ahqztk, xenitmd => gnf, nwfisxjsit => bcyba);
  qjj : entity work.nqm
    port map (hxr => jbur, xenitmd => je, nwfisxjsit => s);
  
  -- Multi-driven assignments
  tisb <= tisb;
  tisb <= (others => '0');
  tisb <= "";
end d;



-- Seed after: 14494446777698322304,11127274767545411571
