-- Seed: 7531213109444528107,2158184632809654795

use std.reflection.all;

entity wctn is
  port (ximrfo : out real_vector(3 to 1); ikie : inout time; osvalsrzh : inout physical_value_mirror);
end wctn;

architecture kmnipjyve of wctn is
  
begin
  
end kmnipjyve;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity c is
  port (trzvtm : inout array_value_mirror; hkom : linkage std_logic; linu : in std_logic_vector(4 to 0); mmbxuu : inout value_mirror);
end c;

use std.reflection.all;

architecture wv of c is
  shared variable psqjj : physical_value_mirror;
  signal bjulpj : time;
  signal mdhmekm : real_vector(3 to 1);
  shared variable fojlvopm : physical_value_mirror;
  signal mxn : time;
  signal trmphfqkv : real_vector(3 to 1);
begin
  xcbhbex : entity work.wctn
    port map (ximrfo => trmphfqkv, ikie => mxn, osvalsrzh => fojlvopm);
  xzx : entity work.wctn
    port map (ximrfo => mdhmekm, ikie => bjulpj, osvalsrzh => psqjj);
end wv;

entity bbzsxuploj is
  port (cbeoakp : out time; kkqllalx : inout severity_level; hwfxquqlpp : buffer string(2 downto 5); hhfmk : inout time);
end bbzsxuploj;

use std.reflection.all;

architecture gyofusm of bbzsxuploj is
  shared variable kjtzv : physical_value_mirror;
  signal b : real_vector(3 to 1);
begin
  dsnahp : entity work.wctn
    port map (ximrfo => b, ikie => hhfmk, osvalsrzh => kjtzv);
end gyofusm;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity s is
  port (domcba : inout array_value_mirror; efn : inout file_subtype_mirror; gzcymgfi : buffer std_logic);
end s;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture ckbigo of s is
  shared variable gijsixd : value_mirror;
  signal yyp : std_logic_vector(4 to 0);
  shared variable wvjw : value_mirror;
  signal wteaxkvuj : std_logic_vector(4 to 0);
  signal nwzatpuag : std_logic;
  shared variable dfkxk : array_value_mirror;
  shared variable bvkrpbs : value_mirror;
  signal cw : std_logic_vector(4 to 0);
  signal ipswj : std_logic;
  shared variable itakft : array_value_mirror;
begin
  gtycpz : entity work.c
    port map (trzvtm => itakft, hkom => ipswj, linu => cw, mmbxuu => bvkrpbs);
  odbzkz : entity work.c
    port map (trzvtm => dfkxk, hkom => nwzatpuag, linu => wteaxkvuj, mmbxuu => wvjw);
  obhauycj : entity work.c
    port map (trzvtm => domcba, hkom => gzcymgfi, linu => yyp, mmbxuu => gijsixd);
  
  -- Multi-driven assignments
  yyp <= yyp;
end ckbigo;



-- Seed after: 14407422446008495587,2158184632809654795
