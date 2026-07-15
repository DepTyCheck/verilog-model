-- Seed: 11972021618049080812,2983771601630957889

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity bdxpondnd is
  port ( ris : in string(2 downto 3)
  ; yooxyp : linkage std_logic
  ; variable k : inout protected_value_mirror_pt
  ; variable uvuggn : inout integer_value_mirror_pt
  );
end bdxpondnd;

architecture rvklu of bdxpondnd is
  
begin
  
end rvklu;

use std.reflection.all;

entity ja is
  port (nuwhhbjxn : out time; ctgnv : out integer; x : out integer; variable elddkw : inout array_subtype_mirror_pt);
end ja;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture gwl of ja is
  shared variable sfdqkvdev : integer_value_mirror_pt;
  shared variable nui : protected_value_mirror_pt;
  signal tkudcouzef : std_logic;
  shared variable rbnitekxd : integer_value_mirror_pt;
  shared variable nhkjrmwwuu : protected_value_mirror_pt;
  signal eziai : std_logic;
  shared variable f : integer_value_mirror_pt;
  shared variable txqip : protected_value_mirror_pt;
  signal y : std_logic;
  shared variable i : integer_value_mirror_pt;
  shared variable q : protected_value_mirror_pt;
  signal z : std_logic;
  signal tindbklww : string(2 downto 3);
begin
  sqmxrmgfo : entity work.bdxpondnd
    port map (ris => tindbklww, yooxyp => z, k => q, uvuggn => i);
  llcp : entity work.bdxpondnd
    port map (ris => tindbklww, yooxyp => y, k => txqip, uvuggn => f);
  drnv : entity work.bdxpondnd
    port map (ris => tindbklww, yooxyp => eziai, k => nhkjrmwwuu, uvuggn => rbnitekxd);
  vsvtbkoq : entity work.bdxpondnd
    port map (ris => tindbklww, yooxyp => tkudcouzef, k => nui, uvuggn => sfdqkvdev);
  
  -- Multi-driven assignments
  z <= z;
  tkudcouzef <= z;
  eziai <= 'Z';
end gwl;

use std.reflection.all;

entity dzptgwokae is
  port (variable tnfnledn : inout value_mirror_pt);
end dzptgwokae;

architecture isgnivs of dzptgwokae is
  
begin
  
end isgnivs;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity nuvq is
  port ( variable slkhdefma : inout enumeration_subtype_mirror_pt
  ; variable ibugeztbzw : inout enumeration_value_mirror_pt
  ; variable iel : inout floating_value_mirror_pt
  ; otdwshyl : out std_logic_vector(2 to 1)
  );
end nuvq;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture nbqa of nuvq is
  shared variable pd : value_mirror_pt;
  shared variable bcfjz : integer_value_mirror_pt;
  shared variable exdky : protected_value_mirror_pt;
  signal emid : std_logic;
  signal jtbpjzukf : string(2 downto 3);
  shared variable g : array_subtype_mirror_pt;
  signal zzvl : integer;
  signal u : integer;
  signal xfoln : time;
begin
  wmjr : entity work.ja
    port map (nuwhhbjxn => xfoln, ctgnv => u, x => zzvl, elddkw => g);
  zoflafekuk : entity work.bdxpondnd
    port map (ris => jtbpjzukf, yooxyp => emid, k => exdky, uvuggn => bcfjz);
  x : entity work.dzptgwokae
    port map (tnfnledn => pd);
  
  -- Single-driven assignments
  jtbpjzukf <= jtbpjzukf;
  
  -- Multi-driven assignments
  otdwshyl <= (others => '0');
  otdwshyl <= (others => '0');
end nbqa;



-- Seed after: 5604513135531240039,2983771601630957889
