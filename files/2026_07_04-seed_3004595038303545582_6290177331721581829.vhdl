-- Seed: 3004595038303545582,6290177331721581829

library ieee;
use ieee.std_logic_1164.all;

entity xwpdfu is
  port (dqtsjlljfx : linkage std_logic; izr : inout time; mvlbfcku : in bit; xeparpto : out time);
end xwpdfu;

architecture footq of xwpdfu is
  
begin
  -- Single-driven assignments
  xeparpto <= izr;
  izr <= xeparpto;
end footq;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity ybfw is
  port (hu : in std_logic; lro : inout physical_value_mirror);
end ybfw;

library ieee;
use ieee.std_logic_1164.all;

architecture bc of ybfw is
  signal lckmj : time;
  signal w : time;
  signal lvp : std_logic;
  signal bjbem : time;
  signal i : bit;
  signal bkfb : time;
  signal o : std_logic;
  signal tnwjlnloh : time;
  signal mchuwwu : bit;
  signal vliaxyxtm : time;
  signal zbkoed : time;
  signal stgtlgqa : bit;
  signal ckjq : time;
  signal efoxccvvt : std_logic;
begin
  tylblb : entity work.xwpdfu
    port map (dqtsjlljfx => efoxccvvt, izr => ckjq, mvlbfcku => stgtlgqa, xeparpto => zbkoed);
  r : entity work.xwpdfu
    port map (dqtsjlljfx => hu, izr => vliaxyxtm, mvlbfcku => mchuwwu, xeparpto => tnwjlnloh);
  zfc : entity work.xwpdfu
    port map (dqtsjlljfx => o, izr => bkfb, mvlbfcku => i, xeparpto => bjbem);
  t : entity work.xwpdfu
    port map (dqtsjlljfx => lvp, izr => w, mvlbfcku => mchuwwu, xeparpto => lckmj);
  
  -- Single-driven assignments
  stgtlgqa <= stgtlgqa;
  i <= '1';
  mchuwwu <= stgtlgqa;
  
  -- Multi-driven assignments
  o <= '1';
  lvp <= efoxccvvt;
  o <= 'X';
end bc;



-- Seed after: 13248620986205932600,6290177331721581829
