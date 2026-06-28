-- Seed: 3241805964023544629,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity ztvntczqf is
  port (xmylryjepb : inout integer; al : inout std_logic_vector(0 to 4); jmvry : in std_logic_vector(1 downto 3));
end ztvntczqf;

architecture namt of ztvntczqf is
  
begin
  -- Single-driven assignments
  xmylryjepb <= 2#1#;
  
  -- Multi-driven assignments
  al <= ('0', '-', 'L', 'Z', '-');
  al <= ('X', 'H', '0', 'L', '-');
end namt;

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (rdvnpy : buffer boolean_vector(2 downto 0); uzd : out character; osogn : linkage std_logic_vector(2 to 0); abddltrinb : linkage std_logic);
end i;

library ieee;
use ieee.std_logic_1164.all;

architecture ijkbqwtgr of i is
  signal ahjmkri : std_logic_vector(1 downto 3);
  signal opctw : std_logic_vector(0 to 4);
  signal leisu : integer;
begin
  fzia : entity work.ztvntczqf
    port map (xmylryjepb => leisu, al => opctw, jmvry => ahjmkri);
  
  -- Single-driven assignments
  uzd <= 'd';
  rdvnpy <= (TRUE, TRUE, FALSE);
  
  -- Multi-driven assignments
  opctw <= ('X', 'H', '1', 'W', 'Z');
  opctw <= ('U', 'W', 'U', 'W', 'W');
  opctw <= ('0', '-', 'L', 'L', 'L');
end ijkbqwtgr;



-- Seed after: 9726188172970273425,6697892553037813751
