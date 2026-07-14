-- Seed: 5603160084968083529,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity yiowohqeu is
  port (rcieknua : inout real; itdzxxgqun : inout std_logic_vector(1 downto 1); gisjbgswsn : inout physical_subtype_mirror);
end yiowohqeu;

architecture nqsqctxnda of yiowohqeu is
  
begin
  -- Single-driven assignments
  rcieknua <= 16#D_6.7#;
  
  -- Multi-driven assignments
  itdzxxgqun <= (others => 'U');
end nqsqctxnda;

use std.reflection.all;

entity ts is
  port (eialudmn : inout access_subtype_mirror; myxarj : inout record_value_mirror);
end ts;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture yowd of ts is
  shared variable wyyp : physical_subtype_mirror;
  signal tgnrnq : std_logic_vector(1 downto 1);
  signal cf : real;
  shared variable rmv : physical_subtype_mirror;
  signal hsgndtcnv : real;
  shared variable gq : physical_subtype_mirror;
  signal s : std_logic_vector(1 downto 1);
  signal ph : real;
  shared variable simgzrlijj : physical_subtype_mirror;
  signal xirsc : std_logic_vector(1 downto 1);
  signal armmjojcg : real;
begin
  znqqcsfap : entity work.yiowohqeu
    port map (rcieknua => armmjojcg, itdzxxgqun => xirsc, gisjbgswsn => simgzrlijj);
  wmfyic : entity work.yiowohqeu
    port map (rcieknua => ph, itdzxxgqun => s, gisjbgswsn => gq);
  zlinjrrt : entity work.yiowohqeu
    port map (rcieknua => hsgndtcnv, itdzxxgqun => s, gisjbgswsn => rmv);
  pe : entity work.yiowohqeu
    port map (rcieknua => cf, itdzxxgqun => tgnrnq, gisjbgswsn => wyyp);
  
  -- Multi-driven assignments
  xirsc <= "L";
  s <= (others => 'L');
end yowd;



-- Seed after: 17037735417953551177,7726014785203345639
