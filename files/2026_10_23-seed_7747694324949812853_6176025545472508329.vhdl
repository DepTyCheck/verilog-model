-- Seed: 7747694324949812853,6176025545472508329

entity nfc is
  port (nscjphoxf : linkage boolean_vector(0 to 4); fls : buffer bit);
end nfc;

architecture orh of nfc is
  
begin
  -- Single-driven assignments
  fls <= fls;
end orh;

entity po is
  port (divsn : inout string(3 to 1); lpydm : in character; iexx : linkage real);
end po;

architecture hsdklbtuol of po is
  signal ny : bit;
  signal iaolgkj : boolean_vector(0 to 4);
begin
  krv : entity work.nfc
    port map (nscjphoxf => iaolgkj, fls => ny);
end hsdklbtuol;

library ieee;
use ieee.std_logic_1164.all;

entity hoxqisulfh is
  port (lxyaxpo : in std_logic; p : inout bit; uidymp : in std_logic; ultm : inout real);
end hoxqisulfh;

architecture l of hoxqisulfh is
  signal jfitmixlm : bit;
  signal ijpwyf : boolean_vector(0 to 4);
  signal vzwuvhterf : bit;
  signal co : boolean_vector(0 to 4);
begin
  ehxqmzpyfn : entity work.nfc
    port map (nscjphoxf => co, fls => vzwuvhterf);
  ph : entity work.nfc
    port map (nscjphoxf => ijpwyf, fls => jfitmixlm);
  
  -- Single-driven assignments
  ultm <= 1.32;
  p <= p;
end l;

entity lf is
  port (fjnybeqxn : inout integer);
end lf;

architecture y of lf is
  signal s : real;
  signal t : character;
  signal zgvrignqi : string(3 to 1);
begin
  jqcqdp : entity work.po
    port map (divsn => zgvrignqi, lpydm => t, iexx => s);
  
  -- Single-driven assignments
  fjnybeqxn <= 8#3_6_7_4_5#;
  t <= 'm';
end y;



-- Seed after: 12444046785599954038,6176025545472508329
