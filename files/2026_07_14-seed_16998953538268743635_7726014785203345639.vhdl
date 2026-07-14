-- Seed: 16998953538268743635,7726014785203345639

use std.reflection.all;

entity mehajr is
  port (hljr : inout real; nzy : inout access_value_mirror; sgb : inout record_subtype_mirror; i : out real);
end mehajr;

architecture gbrsy of mehajr is
  
begin
  -- Single-driven assignments
  hljr <= i;
  i <= i;
end gbrsy;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity ghg is
  port (cfvcoprwlq : linkage real; jtrckyj : buffer boolean_vector(0 downto 0); o : inout file_value_mirror; jmxxpw : out std_logic);
end ghg;

use std.reflection.all;

architecture pvrlikaeh of ghg is
  signal cots : real;
  shared variable gcfi : record_subtype_mirror;
  shared variable rdrezge : access_value_mirror;
  signal ymg : real;
  signal wwovsjmoz : real;
  shared variable eoxsplg : record_subtype_mirror;
  shared variable gmdluva : access_value_mirror;
  signal ng : real;
  signal gwmmy : real;
  shared variable jgsc : record_subtype_mirror;
  shared variable wdrno : access_value_mirror;
  signal hhe : real;
begin
  fysxfjiu : entity work.mehajr
    port map (hljr => hhe, nzy => wdrno, sgb => jgsc, i => gwmmy);
  yzdejbk : entity work.mehajr
    port map (hljr => ng, nzy => gmdluva, sgb => eoxsplg, i => wwovsjmoz);
  tv : entity work.mehajr
    port map (hljr => ymg, nzy => rdrezge, sgb => gcfi, i => cots);
  
  -- Single-driven assignments
  jtrckyj <= jtrckyj;
  
  -- Multi-driven assignments
  jmxxpw <= jmxxpw;
  jmxxpw <= 'W';
  jmxxpw <= 'H';
end pvrlikaeh;



-- Seed after: 3891790536210423456,7726014785203345639
