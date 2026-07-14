-- Seed: 12130167958399438110,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity tbhdo is
  port (drvcivtmx : inout record_subtype_mirror; npovkq : out std_logic);
end tbhdo;

architecture ywxkenkuqe of tbhdo is
  
begin
  -- Multi-driven assignments
  npovkq <= 'L';
  npovkq <= 'X';
  npovkq <= npovkq;
end ywxkenkuqe;

use std.reflection.all;

entity cnkxpjkc is
  port (enaupjzk : inout floating_value_mirror; c : inout physical_subtype_mirror);
end cnkxpjkc;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture szsjad of cnkxpjkc is
  shared variable rpidm : record_subtype_mirror;
  signal vuu : std_logic;
  shared variable lv : record_subtype_mirror;
begin
  tqtm : entity work.tbhdo
    port map (drvcivtmx => lv, npovkq => vuu);
  dtryuom : entity work.tbhdo
    port map (drvcivtmx => rpidm, npovkq => vuu);
  
  -- Multi-driven assignments
  vuu <= 'W';
  vuu <= vuu;
  vuu <= vuu;
  vuu <= vuu;
end szsjad;



-- Seed after: 792503548105534952,7726014785203345639
