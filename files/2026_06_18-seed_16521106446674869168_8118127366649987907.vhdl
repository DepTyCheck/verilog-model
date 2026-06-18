-- Seed: 16521106446674869168,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity cjoiwfi is
  port (fzdqrjqfa : in time; pjekgvtfcr : in real; efouxadvs : inout time; duupsdigy : inout std_logic_vector(2 to 2));
end cjoiwfi;

architecture i of cjoiwfi is
  
begin
  -- Single-driven assignments
  efouxadvs <= 4 sec;
  
  -- Multi-driven assignments
  duupsdigy <= "Z";
  duupsdigy <= (others => 'L');
  duupsdigy <= (others => 'U');
  duupsdigy <= (others => 'W');
end i;

entity ptnenf is
  port (wkkckfo : inout time; diu : in time; uof : buffer bit);
end ptnenf;

library ieee;
use ieee.std_logic_1164.all;

architecture u of ptnenf is
  signal vzr : std_logic_vector(2 to 2);
  signal eq : std_logic_vector(2 to 2);
  signal gfaajq : time;
  signal jvnmowe : real;
  signal qncnydja : time;
begin
  mhlnoffq : entity work.cjoiwfi
    port map (fzdqrjqfa => qncnydja, pjekgvtfcr => jvnmowe, efouxadvs => gfaajq, duupsdigy => eq);
  zhokw : entity work.cjoiwfi
    port map (fzdqrjqfa => wkkckfo, pjekgvtfcr => jvnmowe, efouxadvs => qncnydja, duupsdigy => vzr);
  
  -- Multi-driven assignments
  eq <= "Z";
  eq <= "Z";
  eq <= (others => '-');
  vzr <= (others => '0');
end u;

entity snzhkikpgm is
  port (eczonpt : linkage real);
end snzhkikpgm;

library ieee;
use ieee.std_logic_1164.all;

architecture qnuwnterpv of snzhkikpgm is
  signal kolmb : bit;
  signal rbzjlltp : time;
  signal srbslnmd : bit;
  signal biazigu : std_logic_vector(2 to 2);
  signal pmaz : real;
  signal lizudedqr : time;
  signal rdamy : bit;
  signal ehbuhckptg : time;
  signal mebvghfwm : time;
begin
  rqcfcblme : entity work.ptnenf
    port map (wkkckfo => mebvghfwm, diu => ehbuhckptg, uof => rdamy);
  temkxpq : entity work.cjoiwfi
    port map (fzdqrjqfa => lizudedqr, pjekgvtfcr => pmaz, efouxadvs => ehbuhckptg, duupsdigy => biazigu);
  qdonbhoa : entity work.ptnenf
    port map (wkkckfo => lizudedqr, diu => ehbuhckptg, uof => srbslnmd);
  tx : entity work.ptnenf
    port map (wkkckfo => rbzjlltp, diu => mebvghfwm, uof => kolmb);
  
  -- Single-driven assignments
  pmaz <= 16#BBD.7_A#;
  
  -- Multi-driven assignments
  biazigu <= "X";
end qnuwnterpv;



-- Seed after: 687270239355911396,8118127366649987907
