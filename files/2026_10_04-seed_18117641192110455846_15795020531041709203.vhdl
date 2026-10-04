-- Seed: 18117641192110455846,15795020531041709203

entity kojd is
  port (btxc : in time; vnhfnyj : in boolean; clqiocdt : inout real; ankzyjkfhn : inout integer);
end kojd;

architecture ovypn of kojd is
  
begin
  -- Single-driven assignments
  ankzyjkfhn <= 2#01101#;
  clqiocdt <= 14122.0_2;
end ovypn;

entity p is
  port (myx : inout character);
end p;

architecture xrn of p is
  signal axpnqj : integer;
  signal zz : real;
  signal bkfpmfkri : boolean;
  signal wovqk : integer;
  signal oi : real;
  signal fgnwnxgw : integer;
  signal erbkpjw : real;
  signal qtdubjm : boolean;
  signal gflmpguap : integer;
  signal fb : real;
  signal sjen : boolean;
  signal jo : time;
begin
  colbyy : entity work.kojd
    port map (btxc => jo, vnhfnyj => sjen, clqiocdt => fb, ankzyjkfhn => gflmpguap);
  fghybh : entity work.kojd
    port map (btxc => jo, vnhfnyj => qtdubjm, clqiocdt => erbkpjw, ankzyjkfhn => fgnwnxgw);
  qngncid : entity work.kojd
    port map (btxc => jo, vnhfnyj => qtdubjm, clqiocdt => oi, ankzyjkfhn => wovqk);
  mybbpwnip : entity work.kojd
    port map (btxc => jo, vnhfnyj => bkfpmfkri, clqiocdt => zz, ankzyjkfhn => axpnqj);
  
  -- Single-driven assignments
  bkfpmfkri <= FALSE;
end xrn;

library ieee;
use ieee.std_logic_1164.all;

entity avhuqnhn is
  port (uijsj : inout std_logic_vector(0 to 3); hjwopaxj : linkage boolean_vector(1 to 0));
end avhuqnhn;

architecture fgjongcz of avhuqnhn is
  signal kjbfkd : character;
  signal kaqqkixly : integer;
  signal ccvz : real;
  signal igvcmiumq : boolean;
  signal qxkrx : integer;
  signal dxhaivh : real;
  signal l : integer;
  signal cgbwm : real;
  signal cc : boolean;
  signal fyhgwxiz : time;
begin
  nkmtkam : entity work.kojd
    port map (btxc => fyhgwxiz, vnhfnyj => cc, clqiocdt => cgbwm, ankzyjkfhn => l);
  kpzu : entity work.kojd
    port map (btxc => fyhgwxiz, vnhfnyj => cc, clqiocdt => dxhaivh, ankzyjkfhn => qxkrx);
  ohzwbv : entity work.kojd
    port map (btxc => fyhgwxiz, vnhfnyj => igvcmiumq, clqiocdt => ccvz, ankzyjkfhn => kaqqkixly);
  wlulmuphog : entity work.p
    port map (myx => kjbfkd);
  
  -- Single-driven assignments
  fyhgwxiz <= 4220 ns;
  cc <= cc;
  igvcmiumq <= TRUE;
  
  -- Multi-driven assignments
  uijsj <= ('H', '0', 'H', 'H');
  uijsj <= uijsj;
  uijsj <= ('Z', '1', '1', 'L');
end fgjongcz;



-- Seed after: 8732902657065861467,15795020531041709203
