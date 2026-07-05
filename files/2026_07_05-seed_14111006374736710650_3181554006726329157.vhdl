-- Seed: 14111006374736710650,3181554006726329157

use std.reflection.all;

entity qgtxef is
  port (fxorzva : inout physical_value_mirror; jwtictscm : inout enumeration_value_mirror);
end qgtxef;

architecture scdhzqq of qgtxef is
  
begin
  
end scdhzqq;

use std.reflection.all;

entity pvmshmixg is
  port (ncvugrpeyf : inout subtype_mirror; fs : inout file_subtype_mirror);
end pvmshmixg;

use std.reflection.all;

architecture oawkp of pvmshmixg is
  shared variable yueuzaa : enumeration_value_mirror;
  shared variable u : physical_value_mirror;
begin
  tnyceib : entity work.qgtxef
    port map (fxorzva => u, jwtictscm => yueuzaa);
end oawkp;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity rsg is
  port (fpma : inout access_value_mirror; xfugill : out bit; g : in std_logic);
end rsg;

use std.reflection.all;

architecture au of rsg is
  shared variable selbw : enumeration_value_mirror;
  shared variable bpbzgmtj : physical_value_mirror;
  shared variable jpwtsiqpo : enumeration_value_mirror;
  shared variable cg : physical_value_mirror;
begin
  ojci : entity work.qgtxef
    port map (fxorzva => cg, jwtictscm => jpwtsiqpo);
  ojptno : entity work.qgtxef
    port map (fxorzva => bpbzgmtj, jwtictscm => selbw);
end au;

entity bpqgob is
  port (tvlix : in severity_level);
end bpqgob;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture lhroqylntf of bpqgob is
  signal zoblxys : std_logic;
  signal xrmidk : bit;
  shared variable k : access_value_mirror;
  shared variable zju : enumeration_value_mirror;
  shared variable pm : physical_value_mirror;
  shared variable rimm : enumeration_value_mirror;
  shared variable woejewey : physical_value_mirror;
begin
  rerzv : entity work.qgtxef
    port map (fxorzva => woejewey, jwtictscm => rimm);
  bzd : entity work.qgtxef
    port map (fxorzva => pm, jwtictscm => zju);
  xedakgq : entity work.rsg
    port map (fpma => k, xfugill => xrmidk, g => zoblxys);
  
  -- Multi-driven assignments
  zoblxys <= zoblxys;
end lhroqylntf;



-- Seed after: 14393232536481436353,3181554006726329157
