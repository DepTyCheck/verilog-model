-- Seed: 15225566837551511481,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity yja is
  port (kholnlfl : out std_logic_vector(0 to 0); mdq : inout time; m : out bit_vector(3 to 1));
end yja;

architecture thj of yja is
  
begin
  -- Multi-driven assignments
  kholnlfl <= (others => 'W');
  kholnlfl <= "L";
  kholnlfl <= kholnlfl;
  kholnlfl <= kholnlfl;
end thj;

entity kvgebsmy is
  port (qg : out time);
end kvgebsmy;

library ieee;
use ieee.std_logic_1164.all;

architecture ilvz of kvgebsmy is
  signal ujrkm : bit_vector(3 to 1);
  signal rtcbkbxuz : bit_vector(3 to 1);
  signal veeadxuohm : time;
  signal ademzun : std_logic_vector(0 to 0);
begin
  weycmgl : entity work.yja
    port map (kholnlfl => ademzun, mdq => veeadxuohm, m => rtcbkbxuz);
  rfvsero : entity work.yja
    port map (kholnlfl => ademzun, mdq => qg, m => ujrkm);
  
  -- Multi-driven assignments
  ademzun <= (others => '-');
end ilvz;



-- Seed after: 18027328318181607893,2230106469645304029
