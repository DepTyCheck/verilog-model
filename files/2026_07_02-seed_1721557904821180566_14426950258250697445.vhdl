-- Seed: 1721557904821180566,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity mpjdk is
  port (qse : in std_logic; cmfpxguu : inout value_mirror; ltvg : in std_logic);
end mpjdk;

architecture qlq of mpjdk is
  
begin
  
end qlq;

use std.reflection.all;

entity yrvtg is
  port (dworx : in integer; tmegqiw : inout integer_value_mirror);
end yrvtg;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture nxpgeczyf of yrvtg is
  shared variable dqyr : value_mirror;
  signal zcozwgomj : std_logic;
  shared variable qmghnekgu : value_mirror;
  signal oomubn : std_logic;
begin
  sp : entity work.mpjdk
    port map (qse => oomubn, cmfpxguu => qmghnekgu, ltvg => oomubn);
  dvxqh : entity work.mpjdk
    port map (qse => zcozwgomj, cmfpxguu => dqyr, ltvg => oomubn);
  
  -- Multi-driven assignments
  oomubn <= oomubn;
  oomubn <= 'L';
end nxpgeczyf;



-- Seed after: 11961217166343109987,14426950258250697445
