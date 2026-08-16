-- Seed: 9660137671352928574,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity nej is
  port ( klswplnwld : buffer real_vector(3 to 2)
  ; idiij : out boolean_vector(0 downto 0)
  ; ptksnfpcho : linkage std_logic_vector(4 downto 2)
  ; wzcxg : inout std_logic
  );
end nej;

architecture vvgt of nej is
  
begin
  -- Multi-driven assignments
  wzcxg <= wzcxg;
  wzcxg <= 'Z';
  wzcxg <= '-';
end vvgt;

entity n is
  port (ir : buffer severity_level);
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture lqlmp of n is
  signal oftbyqvaja : std_logic;
  signal q : std_logic_vector(4 downto 2);
  signal ac : boolean_vector(0 downto 0);
  signal kzuf : real_vector(3 to 2);
  signal cefwiq : boolean_vector(0 downto 0);
  signal d : real_vector(3 to 2);
  signal teyrhdcdha : std_logic;
  signal trxoipqgzi : boolean_vector(0 downto 0);
  signal avqcur : real_vector(3 to 2);
  signal xgwnfceg : std_logic;
  signal ikfm : std_logic_vector(4 downto 2);
  signal jp : boolean_vector(0 downto 0);
  signal vfyqnag : real_vector(3 to 2);
begin
  vdivgo : entity work.nej
    port map (klswplnwld => vfyqnag, idiij => jp, ptksnfpcho => ikfm, wzcxg => xgwnfceg);
  moyd : entity work.nej
    port map (klswplnwld => avqcur, idiij => trxoipqgzi, ptksnfpcho => ikfm, wzcxg => teyrhdcdha);
  iczlnd : entity work.nej
    port map (klswplnwld => d, idiij => cefwiq, ptksnfpcho => ikfm, wzcxg => xgwnfceg);
  vri : entity work.nej
    port map (klswplnwld => kzuf, idiij => ac, ptksnfpcho => q, wzcxg => oftbyqvaja);
  
  -- Single-driven assignments
  ir <= ir;
  
  -- Multi-driven assignments
  ikfm <= ikfm;
end lqlmp;



-- Seed after: 7980054712510087437,13857275728440271305
