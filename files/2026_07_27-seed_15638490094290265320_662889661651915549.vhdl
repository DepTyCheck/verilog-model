-- Seed: 15638490094290265320,662889661651915549

library ieee;
use ieee.std_logic_1164.all;

entity gdrvoak is
  port (mx : in boolean; xy : buffer bit; qoimsztetg : inout std_logic_vector(0 to 0); hfigzj : buffer time);
end gdrvoak;

architecture ygvgtiz of gdrvoak is
  
begin
  -- Multi-driven assignments
  qoimsztetg <= "W";
  qoimsztetg <= qoimsztetg;
end ygvgtiz;

entity mfjjzpvpmg is
  port (bpld : buffer real; lz : in severity_level; z : buffer time);
end mfjjzpvpmg;

library ieee;
use ieee.std_logic_1164.all;

architecture pnxnr of mfjjzpvpmg is
  signal kqq : std_logic_vector(0 to 0);
  signal ogia : bit;
  signal gxevuhswjw : time;
  signal ghx : std_logic_vector(0 to 0);
  signal xktk : bit;
  signal ag : boolean;
  signal oojch : time;
  signal w : std_logic_vector(0 to 0);
  signal n : bit;
  signal znpxwqz : boolean;
begin
  flpgjcum : entity work.gdrvoak
    port map (mx => znpxwqz, xy => n, qoimsztetg => w, hfigzj => oojch);
  qgiiq : entity work.gdrvoak
    port map (mx => ag, xy => xktk, qoimsztetg => ghx, hfigzj => gxevuhswjw);
  snhcsqu : entity work.gdrvoak
    port map (mx => znpxwqz, xy => ogia, qoimsztetg => kqq, hfigzj => z);
  
  -- Single-driven assignments
  znpxwqz <= znpxwqz;
  bpld <= 2412.4_3_4;
  
  -- Multi-driven assignments
  w <= (others => 'U');
end pnxnr;



-- Seed after: 14313092717712128283,662889661651915549
