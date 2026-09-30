-- Seed: 15259763219501222866,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity vish is
  port (lntz : out boolean; rfvyrim : linkage std_logic; zviqceajuz : linkage integer);
end vish;

architecture gvqssss of vish is
  
begin
  -- Single-driven assignments
  lntz <= TRUE;
end gvqssss;

entity bskeulb is
  port (nyitedjicx : linkage integer; geiiyze : inout severity_level; oqdyp : linkage boolean);
end bskeulb;

library ieee;
use ieee.std_logic_1164.all;

architecture phb of bskeulb is
  signal f : integer;
  signal heoa : boolean;
  signal tymtysrt : boolean;
  signal m : integer;
  signal lamsacpfh : std_logic;
  signal hyoxaklrx : boolean;
begin
  qfylt : entity work.vish
    port map (lntz => hyoxaklrx, rfvyrim => lamsacpfh, zviqceajuz => m);
  fqunhyq : entity work.vish
    port map (lntz => tymtysrt, rfvyrim => lamsacpfh, zviqceajuz => nyitedjicx);
  fjyemjs : entity work.vish
    port map (lntz => heoa, rfvyrim => lamsacpfh, zviqceajuz => f);
end phb;

entity vsw is
  port (norfwhxf : in integer; ttwu : buffer integer);
end vsw;

library ieee;
use ieee.std_logic_1164.all;

architecture ojssgj of vsw is
  signal qlezcxpit : integer;
  signal x : boolean;
  signal tklle : integer;
  signal z : std_logic;
  signal unpzoif : boolean;
begin
  r : entity work.vish
    port map (lntz => unpzoif, rfvyrim => z, zviqceajuz => tklle);
  fz : entity work.vish
    port map (lntz => x, rfvyrim => z, zviqceajuz => qlezcxpit);
  
  -- Single-driven assignments
  ttwu <= 14;
  
  -- Multi-driven assignments
  z <= '-';
  z <= 'W';
  z <= z;
  z <= 'X';
end ojssgj;



-- Seed after: 18318330388605909282,12260394286515585877
