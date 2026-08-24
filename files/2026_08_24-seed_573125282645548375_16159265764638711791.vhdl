-- Seed: 573125282645548375,16159265764638711791

library ieee;
use ieee.std_logic_1164.all;

entity fsjhxp is
  port (hw : in bit; y : inout std_logic);
end fsjhxp;

architecture iwnsn of fsjhxp is
  
begin
  -- Multi-driven assignments
  y <= 'Z';
  y <= y;
  y <= y;
  y <= 'X';
end iwnsn;

entity ioopa is
  port (dlm : inout boolean_vector(2 to 4));
end ioopa;

library ieee;
use ieee.std_logic_1164.all;

architecture wy of ioopa is
  signal vr : std_logic;
  signal ktm : bit;
begin
  ts : entity work.fsjhxp
    port map (hw => ktm, y => vr);
  
  -- Single-driven assignments
  dlm <= (FALSE, TRUE, FALSE);
  ktm <= ktm;
  
  -- Multi-driven assignments
  vr <= 'W';
  vr <= '1';
  vr <= 'H';
  vr <= vr;
end wy;

library ieee;
use ieee.std_logic_1164.all;

entity cksvkiuit is
  port (wbhu : in std_logic; ug : in integer; sdj : buffer boolean);
end cksvkiuit;

architecture qpucidxzv of cksvkiuit is
  signal h : boolean_vector(2 to 4);
  signal gin : boolean_vector(2 to 4);
begin
  zg : entity work.ioopa
    port map (dlm => gin);
  lojgcmb : entity work.ioopa
    port map (dlm => h);
  
  -- Single-driven assignments
  sdj <= FALSE;
end qpucidxzv;



-- Seed after: 13718676537863855106,16159265764638711791
