-- Seed: 17454628132964886954,7726014785203345639

entity nngqxfae is
  port (l : linkage real; ksh : in time; xrt : out time);
end nngqxfae;

architecture ngegoakofa of nngqxfae is
  
begin
  -- Single-driven assignments
  xrt <= 16#75.63# ps;
end ngegoakofa;

use std.reflection.all;

entity pdtgva is
  port (ef : inout protected_value_mirror; ifg : inout file_subtype_mirror);
end pdtgva;

architecture texmkdsrfz of pdtgva is
  signal py : time;
  signal qkt : time;
  signal yq : real;
  signal zyabdsx : time;
  signal aadzhd : real;
  signal iasbghwrmz : real;
  signal ebokttra : time;
  signal izl : time;
  signal gcozhrm : real;
begin
  plqaxsxx : entity work.nngqxfae
    port map (l => gcozhrm, ksh => izl, xrt => ebokttra);
  qvus : entity work.nngqxfae
    port map (l => iasbghwrmz, ksh => izl, xrt => izl);
  dhnjpkt : entity work.nngqxfae
    port map (l => aadzhd, ksh => zyabdsx, xrt => zyabdsx);
  mimbxeex : entity work.nngqxfae
    port map (l => yq, ksh => qkt, xrt => py);
  
  -- Single-driven assignments
  qkt <= 3_0_0_0 ms;
end texmkdsrfz;

library ieee;
use ieee.std_logic_1164.all;

entity kex is
  port (rigofczuw : in std_logic);
end kex;

use std.reflection.all;

architecture ywtkvcxwg of kex is
  signal hihyhkx : time;
  signal yjahjiwqp : time;
  signal f : real;
  signal cnrq : time;
  signal hhbpkaz : time;
  signal psbx : real;
  shared variable zyr : file_subtype_mirror;
  shared variable diuob : protected_value_mirror;
  signal xsdvaahg : time;
  signal cojniuxuix : real;
begin
  ldfbjya : entity work.nngqxfae
    port map (l => cojniuxuix, ksh => xsdvaahg, xrt => xsdvaahg);
  ifp : entity work.pdtgva
    port map (ef => diuob, ifg => zyr);
  csvrm : entity work.nngqxfae
    port map (l => psbx, ksh => hhbpkaz, xrt => cnrq);
  vij : entity work.nngqxfae
    port map (l => f, ksh => yjahjiwqp, xrt => hihyhkx);
  
  -- Single-driven assignments
  yjahjiwqp <= 16#10.B_F# ns;
  hhbpkaz <= xsdvaahg;
end ywtkvcxwg;

use std.reflection.all;

entity oimrbnpvki is
  port (xx : inout array_subtype_mirror);
end oimrbnpvki;

library ieee;
use ieee.std_logic_1164.all;

architecture maxdcpn of oimrbnpvki is
  signal hkedvzutf : std_logic;
  signal pthh : time;
  signal jc : real;
  signal jczmtmj : std_logic;
  signal kwoqityll : time;
  signal lrlnk : real;
begin
  kia : entity work.nngqxfae
    port map (l => lrlnk, ksh => kwoqityll, xrt => kwoqityll);
  cf : entity work.kex
    port map (rigofczuw => jczmtmj);
  bgefmdfli : entity work.nngqxfae
    port map (l => jc, ksh => kwoqityll, xrt => pthh);
  ptitpcd : entity work.kex
    port map (rigofczuw => hkedvzutf);
  
  -- Multi-driven assignments
  jczmtmj <= jczmtmj;
  hkedvzutf <= jczmtmj;
end maxdcpn;



-- Seed after: 12326300572598486149,7726014785203345639
