-- Seed: 12178306438377628642,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity esyhytgho is
  port (u : out time; f : buffer std_logic; byvccdnbr : inout integer_vector(3 to 4));
end esyhytgho;

architecture a of esyhytgho is
  
begin
  -- Single-driven assignments
  byvccdnbr <= byvccdnbr;
  u <= 1_3 fs;
  
  -- Multi-driven assignments
  f <= 'W';
end a;

entity n is
  port (rmefxiefbx : out bit; hcxms : out time);
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture kp of n is
  signal d : integer_vector(3 to 4);
  signal a : integer_vector(3 to 4);
  signal frkqm : std_logic;
  signal xkip : time;
  signal mso : integer_vector(3 to 4);
  signal aif : std_logic;
  signal jgxp : time;
begin
  ayy : entity work.esyhytgho
    port map (u => jgxp, f => aif, byvccdnbr => mso);
  fgvyhvk : entity work.esyhytgho
    port map (u => xkip, f => frkqm, byvccdnbr => a);
  qktrkcebdl : entity work.esyhytgho
    port map (u => hcxms, f => aif, byvccdnbr => d);
  
  -- Single-driven assignments
  rmefxiefbx <= '1';
  
  -- Multi-driven assignments
  aif <= '1';
  frkqm <= 'L';
  aif <= aif;
end kp;

entity nlamrdgc is
  port (c : in integer; wfjfhwqqr : in time; xgeppns : in integer);
end nlamrdgc;

library ieee;
use ieee.std_logic_1164.all;

architecture hdk of nlamrdgc is
  signal i : integer_vector(3 to 4);
  signal pct : std_logic;
  signal bqsafefi : time;
  signal ihaows : time;
  signal hi : bit;
  signal jubei : integer_vector(3 to 4);
  signal smzus : time;
  signal cqwpaj : integer_vector(3 to 4);
  signal azq : std_logic;
  signal k : time;
begin
  qsd : entity work.esyhytgho
    port map (u => k, f => azq, byvccdnbr => cqwpaj);
  dvkjijvkyq : entity work.esyhytgho
    port map (u => smzus, f => azq, byvccdnbr => jubei);
  txqjyyr : entity work.n
    port map (rmefxiefbx => hi, hcxms => ihaows);
  mbarck : entity work.esyhytgho
    port map (u => bqsafefi, f => pct, byvccdnbr => i);
  
  -- Multi-driven assignments
  pct <= azq;
  azq <= 'L';
  azq <= azq;
  pct <= 'H';
end hdk;

entity kshrusa is
  port (bsdjuahxfg : buffer real);
end kshrusa;

architecture ttalgdiqgy of kshrusa is
  
begin
  -- Single-driven assignments
  bsdjuahxfg <= 331.4_3_4_0;
end ttalgdiqgy;



-- Seed after: 3394011757060664234,5906004015519833893
