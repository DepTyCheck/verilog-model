-- Seed: 2167252319584938331,14426950258250697445

use std.reflection.all;

entity frlxzxtltg is
  port (mwjaijymzc : inout enumeration_value_mirror; fvwfzsqxa : out time);
end frlxzxtltg;

architecture v of frlxzxtltg is
  
begin
  -- Single-driven assignments
  fvwfzsqxa <= 4_4_4.4 ns;
end v;

library ieee;
use ieee.std_logic_1164.all;

entity mzdnmr is
  port (jdmnyezha : linkage integer; lungpoebzm : inout std_logic_vector(1 downto 1));
end mzdnmr;

use std.reflection.all;

architecture fctdqyyowa of mzdnmr is
  signal jdzy : time;
  shared variable i : enumeration_value_mirror;
begin
  nemece : entity work.frlxzxtltg
    port map (mwjaijymzc => i, fvwfzsqxa => jdzy);
  
  -- Multi-driven assignments
  lungpoebzm <= (others => 'H');
  lungpoebzm <= lungpoebzm;
  lungpoebzm <= lungpoebzm;
end fctdqyyowa;

entity lfr is
  port (lj : out character);
end lfr;

architecture nwmhedl of lfr is
  
begin
  -- Single-driven assignments
  lj <= 'p';
end nwmhedl;

entity lcdkgz is
  port (tckwckgs : linkage character);
end lcdkgz;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture yi of lcdkgz is
  signal qgeygespo : time;
  shared variable prkjv : enumeration_value_mirror;
  signal wuyee : std_logic_vector(1 downto 1);
  signal wrccxd : integer;
  signal ynpeff : time;
  shared variable kleqka : enumeration_value_mirror;
begin
  frcxsr : entity work.frlxzxtltg
    port map (mwjaijymzc => kleqka, fvwfzsqxa => ynpeff);
  tv : entity work.mzdnmr
    port map (jdmnyezha => wrccxd, lungpoebzm => wuyee);
  eonevecmzt : entity work.frlxzxtltg
    port map (mwjaijymzc => prkjv, fvwfzsqxa => qgeygespo);
end yi;



-- Seed after: 5003123820318172018,14426950258250697445
