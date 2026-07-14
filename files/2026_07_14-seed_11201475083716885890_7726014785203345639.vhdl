-- Seed: 11201475083716885890,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;

entity ljg is
  port (wdendnz : buffer real; jliplu : inout std_logic_vector(4 to 1); iykvtoylh : buffer time);
end ljg;

architecture qcfdtw of ljg is
  
begin
  -- Multi-driven assignments
  jliplu <= jliplu;
  jliplu <= "";
  jliplu <= jliplu;
end qcfdtw;

entity xxm is
  port (tx : buffer real; kwsiiqa : out real; rd : buffer boolean_vector(1 to 1));
end xxm;

library ieee;
use ieee.std_logic_1164.all;

architecture uofjrvhi of xxm is
  signal lsiubcxq : time;
  signal vp : time;
  signal yvauxy : std_logic_vector(4 to 1);
begin
  kahnl : entity work.ljg
    port map (wdendnz => kwsiiqa, jliplu => yvauxy, iykvtoylh => vp);
  um : entity work.ljg
    port map (wdendnz => tx, jliplu => yvauxy, iykvtoylh => lsiubcxq);
  
  -- Single-driven assignments
  rd <= rd;
end uofjrvhi;

use std.reflection.all;

entity gwsgksbawd is
  port (jz : inout value_mirror; mqosxts : inout enumeration_subtype_mirror; c : in bit_vector(4 downto 2); p : out time);
end gwsgksbawd;

library ieee;
use ieee.std_logic_1164.all;

architecture actnheypys of gwsgksbawd is
  signal tfcbdhum : time;
  signal bmi : std_logic_vector(4 to 1);
  signal i : real;
begin
  ctvi : entity work.ljg
    port map (wdendnz => i, jliplu => bmi, iykvtoylh => tfcbdhum);
  
  -- Single-driven assignments
  p <= p;
  
  -- Multi-driven assignments
  bmi <= bmi;
  bmi <= (others => '0');
  bmi <= bmi;
end actnheypys;



-- Seed after: 10536146001497306245,7726014785203345639
