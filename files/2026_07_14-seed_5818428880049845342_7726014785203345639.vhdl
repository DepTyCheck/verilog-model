-- Seed: 5818428880049845342,7726014785203345639

use std.reflection.all;

entity tsxju is
  port (dgf : inout access_value_mirror; x : out real; hjwvylr : inout array_subtype_mirror);
end tsxju;

architecture rcoxesvphu of tsxju is
  
begin
  
end rcoxesvphu;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity yzjlzseb is
  port (pghquznxf : inout floating_subtype_mirror; b : buffer std_logic);
end yzjlzseb;

use std.reflection.all;

architecture azi of yzjlzseb is
  shared variable yozmy : array_subtype_mirror;
  signal svgdfh : real;
  shared variable cynakz : access_value_mirror;
  shared variable glojkmo : array_subtype_mirror;
  signal lozgk : real;
  shared variable pktu : access_value_mirror;
begin
  gme : entity work.tsxju
    port map (dgf => pktu, x => lozgk, hjwvylr => glojkmo);
  sken : entity work.tsxju
    port map (dgf => cynakz, x => svgdfh, hjwvylr => yozmy);
  
  -- Multi-driven assignments
  b <= b;
end azi;

use std.reflection.all;

entity zmd is
  port (m : inout protected_subtype_mirror; imhuz : buffer time; odjn : out integer; isrlpfdes : inout enumeration_subtype_mirror);
end zmd;

use std.reflection.all;

architecture oowyuo of zmd is
  shared variable ysws : array_subtype_mirror;
  signal tuyjaweeq : real;
  shared variable sktxhlem : access_value_mirror;
begin
  aieojv : entity work.tsxju
    port map (dgf => sktxhlem, x => tuyjaweeq, hjwvylr => ysws);
  
  -- Single-driven assignments
  odjn <= odjn;
end oowyuo;

library ieee;
use ieee.std_logic_1164.all;

entity zln is
  port (lzbqb : linkage integer; weijooyo : out std_logic_vector(3 to 0));
end zln;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture fedrbcnwmf of zln is
  shared variable gwac : enumeration_subtype_mirror;
  signal qfaauo : integer;
  signal fffll : time;
  shared variable wpnkwfvq : protected_subtype_mirror;
  shared variable ssnumrcqj : array_subtype_mirror;
  signal emzqk : real;
  shared variable xbp : access_value_mirror;
  signal wzjuek : std_logic;
  shared variable jj : floating_subtype_mirror;
  signal tbgjceyh : std_logic;
  shared variable kxs : floating_subtype_mirror;
begin
  xtkmcmnfpi : entity work.yzjlzseb
    port map (pghquznxf => kxs, b => tbgjceyh);
  fgxeok : entity work.yzjlzseb
    port map (pghquznxf => jj, b => wzjuek);
  bhff : entity work.tsxju
    port map (dgf => xbp, x => emzqk, hjwvylr => ssnumrcqj);
  ebqmst : entity work.zmd
    port map (m => wpnkwfvq, imhuz => fffll, odjn => qfaauo, isrlpfdes => gwac);
  
  -- Multi-driven assignments
  wzjuek <= '1';
  weijooyo <= (others => '0');
  weijooyo <= weijooyo;
end fedrbcnwmf;



-- Seed after: 10723193563476841102,7726014785203345639
