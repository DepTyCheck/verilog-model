-- Seed: 4338815620204913500,3108530264173481209

entity kqb is
  port (oltgivrh : in real; izciwlact : out character);
end kqb;

architecture rh of kqb is
  
begin
  -- Single-driven assignments
  izciwlact <= 'w';
end rh;

entity vfqq is
  port (uftbai : in bit_vector(1 downto 0); gaisvo : in real_vector(2 downto 3); wfpqunvx : buffer real; vktqs : buffer boolean);
end vfqq;

architecture mjasfrz of vfqq is
  
begin
  -- Single-driven assignments
  wfpqunvx <= 42.1_2;
  vktqs <= TRUE;
end mjasfrz;

library ieee;
use ieee.std_logic_1164.all;

entity ghib is
  port ( wllrtzypvn : buffer integer_vector(2 downto 1)
  ; jivtiy : in integer
  ; lcpnxbszqh : linkage std_logic_vector(1 to 4)
  ; vqozkb : inout real_vector(0 to 4)
  );
end ghib;

architecture ge of ghib is
  signal qlwikpvc : character;
  signal j : real;
  signal awnyyody : character;
  signal mw : real;
  signal te : boolean;
  signal lv : real;
  signal zbxrdc : real_vector(2 downto 3);
  signal evgvuaikh : bit_vector(1 downto 0);
begin
  rb : entity work.vfqq
    port map (uftbai => evgvuaikh, gaisvo => zbxrdc, wfpqunvx => lv, vktqs => te);
  huj : entity work.kqb
    port map (oltgivrh => mw, izciwlact => awnyyody);
  mnj : entity work.kqb
    port map (oltgivrh => j, izciwlact => qlwikpvc);
end ge;



-- Seed after: 1490419674056439605,3108530264173481209
