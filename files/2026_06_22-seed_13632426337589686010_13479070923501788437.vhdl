-- Seed: 13632426337589686010,13479070923501788437

entity h is
  port (rxtrntk : in integer; rdaxenczm : in boolean_vector(3 downto 2));
end h;

architecture ofdvjbsww of h is
  
begin
  
end ofdvjbsww;

entity bkbybtkf is
  port (cka : out integer);
end bkbybtkf;

architecture evmzft of bkbybtkf is
  signal uokhqwbver : boolean_vector(3 downto 2);
  signal cjsfqq : integer;
  signal c : boolean_vector(3 downto 2);
  signal zfezvkm : boolean_vector(3 downto 2);
begin
  lmcwp : entity work.h
    port map (rxtrntk => cka, rdaxenczm => zfezvkm);
  inzfhoiel : entity work.h
    port map (rxtrntk => cka, rdaxenczm => zfezvkm);
  cbadif : entity work.h
    port map (rxtrntk => cka, rdaxenczm => c);
  gxmfvohyu : entity work.h
    port map (rxtrntk => cjsfqq, rdaxenczm => uokhqwbver);
  
  -- Single-driven assignments
  cka <= 1_4_3;
  cjsfqq <= 2#111#;
  c <= (TRUE, TRUE);
  uokhqwbver <= (TRUE, TRUE);
  zfezvkm <= (FALSE, FALSE);
end evmzft;

library ieee;
use ieee.std_logic_1164.all;

entity ilqtf is
  port (peiprudvqb : linkage real; gpyhhmegin : linkage real; svkjtmhuj : buffer std_logic; oqpenhueb : buffer std_logic);
end ilqtf;

architecture ra of ilqtf is
  signal hemxdpswr : integer;
  signal j : boolean_vector(3 downto 2);
  signal u : integer;
begin
  tmglsu : entity work.h
    port map (rxtrntk => u, rdaxenczm => j);
  lish : entity work.h
    port map (rxtrntk => hemxdpswr, rdaxenczm => j);
  
  -- Single-driven assignments
  hemxdpswr <= 8#2_5_5_5#;
  u <= 0_2;
end ra;

entity huhgnii is
  port (ezhta : buffer real; usyvbhzb : inout integer; ed : in boolean_vector(0 downto 4); im : linkage time);
end huhgnii;

library ieee;
use ieee.std_logic_1164.all;

architecture itlbzxonc of huhgnii is
  signal j : boolean_vector(3 downto 2);
  signal fuxzodxvd : integer;
  signal hryrv : std_logic;
  signal bfxlqvox : std_logic;
  signal vq : real;
  signal idisskajfs : real;
begin
  xcmrr : entity work.ilqtf
    port map (peiprudvqb => idisskajfs, gpyhhmegin => vq, svkjtmhuj => bfxlqvox, oqpenhueb => hryrv);
  phxywx : entity work.h
    port map (rxtrntk => fuxzodxvd, rdaxenczm => j);
  
  -- Single-driven assignments
  usyvbhzb <= 1_0_0_4_2;
  j <= (FALSE, FALSE);
  fuxzodxvd <= 0_3_2_3;
  ezhta <= 0_1_1_0_3.0_4_4_4;
  
  -- Multi-driven assignments
  bfxlqvox <= 'U';
  bfxlqvox <= 'L';
  bfxlqvox <= '0';
  bfxlqvox <= 'L';
end itlbzxonc;



-- Seed after: 16390091968677668903,13479070923501788437
