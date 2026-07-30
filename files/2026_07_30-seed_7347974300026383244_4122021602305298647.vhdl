-- Seed: 7347974300026383244,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity fzymg is
  port (kmtuqdzz : inout boolean; dkrjb : inout time; fan : in std_logic_vector(1 to 0));
end fzymg;

architecture f of fzymg is
  
begin
  -- Single-driven assignments
  dkrjb <= 3 sec;
  kmtuqdzz <= TRUE;
end f;

entity a is
  port (c : buffer time);
end a;

library ieee;
use ieee.std_logic_1164.all;

architecture jxbasseox of a is
  signal rvlkmhava : boolean;
  signal kh : std_logic_vector(1 to 0);
  signal obzuigho : time;
  signal xem : boolean;
begin
  zeounf : entity work.fzymg
    port map (kmtuqdzz => xem, dkrjb => obzuigho, fan => kh);
  o : entity work.fzymg
    port map (kmtuqdzz => rvlkmhava, dkrjb => c, fan => kh);
end jxbasseox;

library ieee;
use ieee.std_logic_1164.all;

entity dywuxbiajx is
  port (wk : linkage boolean; mxfqfx : inout integer_vector(3 to 1); iro : buffer std_logic_vector(3 downto 4); pujjojdxoe : linkage real);
end dywuxbiajx;

architecture gita of dywuxbiajx is
  signal sy : time;
  signal kmshaqkozr : time;
begin
  vteymgj : entity work.a
    port map (c => kmshaqkozr);
  dmwakmcd : entity work.a
    port map (c => sy);
  
  -- Multi-driven assignments
  iro <= iro;
  iro <= "";
end gita;



-- Seed after: 895615779348905595,4122021602305298647
