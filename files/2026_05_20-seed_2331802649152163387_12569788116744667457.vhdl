-- Seed: 2331802649152163387,12569788116744667457

library ieee;
use ieee.std_logic_1164.all;

entity nwv is
  port (oskjwpedj : inout time; autimf : out time; xw : in integer; q : inout std_logic);
end nwv;



architecture gp of nwv is
  
begin
  
end gp;

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (rsgfk : linkage time; qxbudbkepi : inout real; lwljqahq : linkage std_logic; dmsfnlcotr : linkage real);
end m;

library ieee;
use ieee.std_logic_1164.all;

architecture xxejajk of m is
  signal eqqeqavq : std_logic;
  signal ofgmlris : integer;
  signal sldeu : time;
  signal vblbpnyyju : time;
  signal boc : time;
  signal pba : time;
  signal cdvhtqud : std_logic;
  signal qn : integer;
  signal evzipyeewr : time;
  signal xqly : time;
begin
  kjoq : entity work.nwv
    port map (oskjwpedj => xqly, autimf => evzipyeewr, xw => qn, q => cdvhtqud);
  qsonr : entity work.nwv
    port map (oskjwpedj => pba, autimf => boc, xw => qn, q => cdvhtqud);
  pwxn : entity work.nwv
    port map (oskjwpedj => vblbpnyyju, autimf => sldeu, xw => ofgmlris, q => eqqeqavq);
end xxejajk;



entity jzcp is
  port (dgczaohvjj : in time; kcnyh : linkage integer; jrbglzojm : inout severity_level; w : in integer);
end jzcp;

library ieee;
use ieee.std_logic_1164.all;

architecture dmi of jzcp is
  signal mhygpo : std_logic;
  signal gct : integer;
  signal pwanbpnzyd : time;
  signal oebptbk : std_logic;
  signal tluzzkluqx : real;
  signal hgqsbieb : std_logic;
  signal ddurwxwka : real;
  signal zgpcl : time;
begin
  qrmkubyekf : entity work.m
    port map (rsgfk => zgpcl, qxbudbkepi => ddurwxwka, lwljqahq => hgqsbieb, dmsfnlcotr => tluzzkluqx);
  rdo : entity work.m
    port map (rsgfk => dgczaohvjj, qxbudbkepi => tluzzkluqx, lwljqahq => oebptbk, dmsfnlcotr => ddurwxwka);
  lopwarp : entity work.nwv
    port map (oskjwpedj => zgpcl, autimf => pwanbpnzyd, xw => gct, q => mhygpo);
end dmi;



-- Seed after: 15463386970954090842,12569788116744667457
